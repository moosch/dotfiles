-- Turns topic tables into buffer lines plus highlight spans.
--
-- Nothing here touches a window or a buffer: the output is a plain list of
-- strings and a plain list of { line, col, end_col, hl } spans, both 0-based
-- the way nvim_buf_set_extmark wants them. That is what makes the content
-- usable from someone else's window as well as from c_docs.ui.

local topics = require("c_docs.topics")

local M = {}

-- Defined with default = true so a colorscheme can override them, and
-- reasserted on ColorScheme because :hi clear drops them. init.lua applies the
-- colorscheme after every require, so without this they would never survive
-- startup.
M.groups = {
  CDocsTitle = "Title",
  CDocsHeading = "Function",
  CDocsKey = "Special",
  CDocsType = "Type",
  CDocsHint = "Comment",
}

local function define_highlights()
  for group, link in pairs(M.groups) do
    vim.api.nvim_set_hl(0, group, { link = link, default = true })
  end
end

define_highlights()
vim.api.nvim_create_autocmd("ColorScheme", {
  desc = "Keep c_docs highlight groups defined",
  group = vim.api.nvim_create_augroup("c-docs-hl", { clear = true }),
  callback = define_highlights,
})

local INDENT = "  "
local BODY_INDENT = "    "
local GUTTER = "  "

-- The column a cell lands in decides how it is coloured: the left column is the
-- thing you are looking up, the middle is its type or value, the rest is prose.
local COLUMN_HL = { "CDocsKey", "CDocsType" }

--- Append a span, skipping empty ranges so the extmark call never gets col ==
--- end_col (nvim tolerates it, but it clutters the namespace for nothing).
local function add(spans, line, col, end_col, hl)
  if hl and end_col > col then
    spans[#spans + 1] = { line = line, col = col, end_col = end_col, hl = hl }
  end
end

local function pad_to(s, width)
  return s .. string.rep(" ", width - vim.fn.strdisplaywidth(s))
end

--- Widths of every column but the last, which is left ragged.
local function column_widths(rows)
  local widths = {}
  for _, row in ipairs(rows) do
    for i = 1, #row - 1 do
      local w = vim.fn.strdisplaywidth(row[i])
      if w > (widths[i] or 0) then
        widths[i] = w
      end
    end
  end
  return widths
end

--- One aligned row. Returns the built line; pushes its spans onto `spans`.
--- `indent` leads the line, so the span columns have to start past it.
local function row_line(row, widths, lnum, spans, indent)
  local parts, col = {}, #indent
  for i, cell in ipairs(row) do
    local padded = (i < #row) and pad_to(cell, widths[i] or 0) or cell
    add(spans, lnum, col, col + #cell, COLUMN_HL[i])
    parts[#parts + 1] = padded
    col = col + #padded + #GUTTER
  end
  -- Trailing pad from an empty last cell would show up as invisible width, so
  -- the line is trimmed; every span sits left of the trim.
  local line = (indent .. table.concat(parts, GUTTER)):gsub("%s+$", "")
  return line
end

local function is_blank_row(row)
  for _, cell in ipairs(row) do
    if cell ~= "" then
      return false
    end
  end
  return true
end

local function push_hint(lines, spans, hint)
  lines[#lines + 1] = ""
  lines[#lines + 1] = INDENT .. hint
  add(spans, #lines - 1, 0, #lines[#lines], "CDocsHint")
end

--- The topic list.
--- @return string[] lines, table[] spans, string[] topic_ids indexed by display row
function M.index()
  local lines, spans, ids = {}, {}, {}

  local rows = {}
  for i, topic in ipairs(topics.list) do
    rows[#rows + 1] = { tostring(i), topic.title, topic.summary or "" }
  end
  local widths = column_widths(rows)

  for i, row in ipairs(rows) do
    lines[#lines + 1] = row_line(row, widths, #lines, spans, INDENT)
    ids[i] = topics.list[i].id
  end

  push_hint(lines, spans, "<CR> open   1-9 jump   q close")
  return lines, spans, ids
end

--- One topic.
--- @return string[]|nil lines, table[]|nil spans
function M.topic(id)
  local topic = topics.by_id[id]
  if not topic then
    return nil
  end

  local lines, spans = {}, {}

  lines[#lines + 1] = INDENT .. topic.title
  add(spans, 0, #INDENT, #lines[1], "CDocsTitle")

  for _, section in ipairs(topic.sections) do
    lines[#lines + 1] = ""

    if section.heading then
      lines[#lines + 1] = INDENT .. section.heading
      add(spans, #lines - 1, #INDENT, #lines[#lines], "CDocsHeading")
    end

    if section.rows then
      local widths = column_widths(section.rows)
      for _, row in ipairs(section.rows) do
        if is_blank_row(row) then
          lines[#lines + 1] = ""
        else
          lines[#lines + 1] = row_line(row, widths, #lines, spans, BODY_INDENT)
        end
      end
    end

    for _, text in ipairs(section.text or {}) do
      lines[#lines + 1] = text == "" and "" or (BODY_INDENT .. text)
    end
  end

  push_hint(lines, spans, "<BS> back   q close")
  return lines, spans
end

return M
