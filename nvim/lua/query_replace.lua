-- Emacs-style query-replace (M-%) and query-replace-regexp (C-M-%).
--
-- Runs from the cursor to the end of the buffer, prompting at each match with
-- the Emacs key set rather than vim's :s///c set:
--
--   y SPC  replace and advance      n DEL  skip
--   !      replace all remaining    .      replace this one and quit
--   ,      replace and pause        ^      back to the previous match
--   u      undo the last replace    q ESC  quit
--   ?      toggle the key panel
--
-- The key panel is listed under the prompt which-key style, growing the command
-- area for the duration and restoring it on exit.

local M = {}

local ns = vim.api.nvim_create_namespace("query_replace")

--- Turn a user string into a very-nomagic pattern so it matches literally.
local function literal_pattern(s)
  return "\\V" .. vim.fn.escape(s, "\\")
end

-- Defined with default = true so a colorscheme can override them, and
-- reasserted on ColorScheme because :hi clear drops them.
local function define_highlights()
  vim.api.nvim_set_hl(0, "QueryReplaceMatch", { link = "Search", default = true })
  vim.api.nvim_set_hl(0, "QueryReplaceKey", { link = "Special", default = true })

  -- The current match has to out-shout the others. IncSearch is the natural
  -- link, but some colorschemes (this config's included) give it a foreground
  -- only, which would read as weaker than Search's solid background. Fall back
  -- to reverse video, which stands out against any theme.
  local inc = vim.api.nvim_get_hl(0, { name = "IncSearch", link = false })
  if inc.bg or inc.reverse then
    vim.api.nvim_set_hl(0, "QueryReplaceCurrent", { link = "IncSearch", default = true })
  else
    vim.api.nvim_set_hl(0, "QueryReplaceCurrent", { reverse = true, bold = true, default = true })
  end
end

define_highlights()
vim.api.nvim_create_autocmd("ColorScheme", {
  desc = "Keep query-replace highlight groups defined",
  group = vim.api.nvim_create_augroup("query-replace-hl", { clear = true }),
  callback = define_highlights,
})

local function clear_highlight(buf)
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
end

--- Paint every match in the given row range, with the one being asked about
--- picked out in its own colour. Restricted to the viewport by default: matches
--- off screen cannot be seen, so scanning the whole buffer each keypress would
--- be wasted work on a large file.
local function highlight_matches(buf, pattern, crow, cscol, cecol, top, bot)
  clear_highlight(buf)

  top = top or (vim.fn.line("w0") - 1)
  bot = bot or (vim.fn.line("w$") - 1)
  -- w0/w$ describe the viewport as of the last redraw, so after a jump they can
  -- be stale; make sure the match being asked about is always in range.
  top, bot = math.min(top, crow), math.max(bot, crow)
  local last = vim.api.nvim_buf_line_count(buf) - 1
  top, bot = math.max(top, 0), math.min(bot, last)

  for r = top, bot do
    local line = vim.api.nvim_buf_get_lines(buf, r, r + 1, false)[1] or ""
    local from = 0
    while from <= #line do
      local m = vim.fn.matchstrpos(line:sub(from + 1), pattern)
      if m[2] < 0 or m[1] == "" then
        break
      end
      local scol, ecol = from + m[2], from + m[3]
      local current = (r == crow and scol == cscol and ecol == cecol)
      vim.api.nvim_buf_set_extmark(buf, ns, r, scol, {
        end_row = r,
        end_col = ecol,
        hl_group = current and "QueryReplaceCurrent" or "QueryReplaceMatch",
        priority = current and 4097 or 4096,
      })
      from = ecol
    end
  end
end

--- Locate the first match at or after (row, col), both 0-indexed.
--- Returns start row/col and end col (end exclusive), or nil.
---
--- This deliberately avoids search()/searchpos(): those tile matches from the
--- start of the line, so scanning from mid-line skips a match that overlaps an
--- earlier tile ("aaaa" from column 2 reports column 3, not 2). matchstrpos
--- against the line tail always yields the true leftmost match at or after col.
--- The tradeoff is that patterns spanning a line break are not matched.
local function next_match(buf, pattern, row, col)
  local last = vim.api.nvim_buf_line_count(buf) - 1
  for r = row, last do
    local line = vim.api.nvim_buf_get_lines(buf, r, r + 1, false)[1] or ""
    local from = (r == row) and col or 0
    if from <= #line then
      local m = vim.fn.matchstrpos(line:sub(from + 1), pattern)
      if m[2] >= 0 and m[1] ~= "" then
        return r, from + m[2], from + m[3]
      end
    end
  end
  return nil
end

--- The replacement for one match, with regex backreferences expanded.
local function replacement_for(buf, pattern, replace, row, scol, ecol)
  local matched = vim.api.nvim_buf_get_text(buf, row, scol, row, ecol, {})[1]
  return vim.fn.substitute(matched, pattern, replace, "")
end

local function echo(msg, hl)
  vim.api.nvim_echo({ { msg, hl or "Question" } }, false, {})
end

--- Whether the key panel starts expanded. ? toggles it mid-run.
M.show_help = true

local KEYS = {
  { "y SPC", "replace" },
  { "n DEL", "skip" },
  { "!", "all remaining" },
  { ".", "replace, quit" },
  { ",", "replace, pause" },
  { "^", "previous" },
  { "u", "undo last" },
  { "q ESC", "quit" },
  { "?", "toggle help" },
}

--- Lay the key list out in as many columns as the window is wide enough for,
--- which-key style. Returns a list of lines, each a list of echo chunks.
local function help_lines(width)
  local kw, dw = 0, 0
  for _, e in ipairs(KEYS) do
    kw = math.max(kw, #e[1])
    dw = math.max(dw, #e[2])
  end

  local cell = kw + 1 + dw + 3 -- key, gap, description, gutter
  local cols = math.max(1, math.floor(width / cell))
  local rows = math.ceil(#KEYS / cols)

  local lines = {}
  for r = 1, rows do
    local chunks = {}
    for c = 0, cols - 1 do
      local e = KEYS[r + c * rows]
      if e then
        table.insert(chunks, { string.format("%" .. kw .. "s", e[1]), "QueryReplaceKey" })
        table.insert(chunks, { " " .. string.format("%-" .. dw .. "s", e[2]) .. "   ", "Comment" })
      end
    end
    table.insert(lines, chunks)
  end
  return lines
end

M.help_lines = help_lines -- exposed for tests

--- Echo the prompt with the key panel below it, sizing the command area to fit.
--- Sized against `baseline` (the cmdheight we started with) so toggling the
--- panel off shrinks it back, but never below what the user had.
local function render_prompt(message, baseline, hl)
  local chunks = { { message, hl or "Question" } }
  local rows = 1

  if M.show_help then
    local lines = help_lines(vim.o.columns)
    -- never eat more than half the screen
    local room = math.max(1, math.floor(vim.o.lines / 2) - 1)
    for i = 1, math.min(#lines, room) do
      table.insert(chunks, { "\n" })
      vim.list_extend(chunks, lines[i])
      rows = rows + 1
    end
  end

  local target = math.max(rows, baseline or 1)
  if vim.o.cmdheight ~= target then
    vim.o.cmdheight = target
  end
  vim.api.nvim_echo(chunks, false, {})
  return rows
end

--- Indirected so tests can drive the prompt loop without a terminal.
function M.read_key()
  return vim.fn.getcharstr()
end

function M.run(from, to, opts)
  opts = opts or {}
  if from == nil or from == "" then
    return
  end

  local pattern = opts.regexp and from or literal_pattern(from)
  -- a literal search means a literal replacement too: & ~ \ are magic to substitute()
  local replace = opts.regexp and to or vim.fn.escape(to, "\\&~")

  local buf = vim.api.nvim_get_current_buf()
  local replaced, all = 0, false
  local history = {} -- for u: {row, scol, ecol_after, old_text}
  local visited = {} -- for ^: scan positions of prior prompts

  -- Scan position is tracked explicitly; the window cursor is only moved so the
  -- user can see the match, and may be clamped to the line, so it is not state.
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  row = row - 1

  -- The key panel grows the command area; this has to go back exactly as it was
  -- however we leave, including on an unexpected error, so the loop is pcall'd.
  local saved_cmdheight = vim.o.cmdheight

  local loop_ok, loop_err = pcall(function()
  while true do
    local srow, scol, ecol = next_match(buf, pattern, row, col)
    if not srow then
      break
    end
    row, col = srow, scol

    local new_text = replacement_for(buf, pattern, replace, srow, scol, ecol)
    local old_text = vim.api.nvim_buf_get_text(buf, srow, scol, srow, ecol, {})[1]

    local function do_replace()
      vim.api.nvim_buf_set_text(buf, srow, scol, srow, ecol, { new_text })
      table.insert(history, { srow, scol, scol + #new_text, old_text })
      replaced = replaced + 1
      row, col = srow, scol + #new_text
    end

    if all then
      do_replace()
    else
      -- move, then redraw so w0/w$ reflect where we actually landed, then paint
      vim.api.nvim_win_set_cursor(0, { srow + 1, scol })
      vim.cmd("redraw")
      highlight_matches(buf, pattern, srow, scol, ecol)
      vim.cmd("redraw")
      render_prompt(("Query replacing %s with %s:"):format(from, to), saved_cmdheight)

      local ok, key = pcall(M.read_key)
      if not ok then
        key = "\27"
      end

      if key == "y" or key == " " then
        do_replace()
      elseif key == "n" or key == "\8" or key == "\127" then
        table.insert(visited, { srow, scol })
        row, col = srow, ecol -- step past this match so it is not offered again
      elseif key == "!" then
        all = true
        do_replace()
      elseif key == "." then
        do_replace()
        break
      elseif key == "," then
        do_replace()
        clear_highlight(buf)
        vim.cmd("redraw")
        render_prompt("Replaced. Any key to continue, q to quit.", saved_cmdheight)
        local _, k = pcall(M.read_key)
        if k == "q" or k == "\27" then
          break
        end
      elseif key == "^" then
        local prev = table.remove(visited)
        if prev then
          row, col = prev[1], prev[2]
        else
          echo("No previous match", "WarningMsg")
        end
      elseif key == "u" then
        local last = table.remove(history)
        if last then
          local hrow, hscol, hecol, hold = unpack(last)
          vim.api.nvim_buf_set_text(buf, hrow, hscol, hrow, hecol, { hold })
          row, col = hrow, hscol
          replaced = replaced - 1
        else
          echo("Nothing to undo", "WarningMsg")
        end
      elseif key == "?" then
        M.show_help = not M.show_help
        row, col = srow, scol -- re-offer this same match with the panel toggled
      elseif key == "q" or key == "\27" or key == "\r" then
        break
      end
    end
  end
  end)

  clear_highlight(buf)
  vim.o.cmdheight = saved_cmdheight
  vim.cmd("redraw")

  if not loop_ok then
    error(loop_err)
  end

  vim.api.nvim_echo({ { ("Replaced %d occurrence%s"):format(replaced, replaced == 1 and "" or "s") } }, false, {})
  return replaced
end

--- Prompt for both strings, then run.
function M.prompt(opts)
  opts = opts or {}
  local label = opts.regexp and "Query replace regexp: " or "Query replace: "
  vim.ui.input({ prompt = label }, function(from)
    if not from or from == "" then
      return
    end
    vim.ui.input({ prompt = ("Query replace %s with: "):format(from) }, function(to)
      if to == nil then
        return
      end
      M.run(from, to, opts)
    end)
  end)
end

vim.api.nvim_create_user_command("QueryReplace", function()
  M.prompt({})
end, { desc = "Emacs-style query-replace from the cursor" })

vim.api.nvim_create_user_command("QueryReplaceRegexp", function()
  M.prompt({ regexp = true })
end, { desc = "Emacs-style query-replace-regexp from the cursor" })

return M
