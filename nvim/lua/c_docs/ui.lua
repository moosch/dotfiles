-- The bundled float: one buffer and one window, re-rendered in place as you
-- move between the topic list and a topic. Opening a topic never stacks a
-- second window, so <BS> always lands back where you started.
--
-- This is the replaceable half of c_docs. Everything it draws comes from
-- c_docs.render, which knows nothing about windows.

local render = require("c_docs.render")
local topics = require("c_docs.topics")

local M = {}

local ns = vim.api.nvim_create_namespace("c_docs")
local augroup = vim.api.nvim_create_augroup("c-docs-win", { clear = true })

local state = {
  buf = nil,
  win = nil,
  view = "index",
  topic_id = nil,
  index_row = 1,
  ids = {},
}

local function is_open()
  return state.win ~= nil and vim.api.nvim_win_is_valid(state.win)
end

--- Fit the window to the content, clamped to 80% of the editor and centred.
local function geometry(lines)
  local longest = 0
  for _, line in ipairs(lines) do
    longest = math.max(longest, vim.fn.strdisplaywidth(line))
  end

  local width = math.max(math.min(longest + 2, math.floor(vim.o.columns * 0.8)), 30)
  local height = math.max(math.min(#lines, math.floor(vim.o.lines * 0.8)), 1)

  return {
    relative = "editor",
    width = width,
    height = height,
    row = math.max(math.floor((vim.o.lines - height) / 2) - 1, 0),
    col = math.max(math.floor((vim.o.columns - width) / 2), 0),
  }
end

local function apply(lines, spans, title, cursor_row)
  vim.bo[state.buf].modifiable = true
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, lines)
  vim.bo[state.buf].modifiable = false

  vim.api.nvim_buf_clear_namespace(state.buf, ns, 0, -1)
  for _, span in ipairs(spans) do
    vim.api.nvim_buf_set_extmark(state.buf, ns, span.line, span.col, {
      end_col = span.end_col,
      hl_group = span.hl,
    })
  end

  local config = geometry(lines)
  config.border = "rounded"
  config.title = " " .. title .. " "
  config.title_pos = "center"
  vim.api.nvim_win_set_config(state.win, config)

  vim.api.nvim_win_set_cursor(state.win, { math.min(cursor_row, #lines), 0 })
end

local function show_index()
  local lines, spans, ids = render.index()
  state.view = "index"
  state.topic_id = nil
  state.ids = ids
  apply(lines, spans, "C Docs", math.min(state.index_row, #ids > 0 and #ids or 1))
end

local function show_topic(id)
  local lines, spans = render.topic(id)
  if not lines then
    vim.notify("c_docs: no such topic: " .. tostring(id), vim.log.levels.WARN)
    return
  end
  -- Remember where the list was left so <BS> puts the cursor back on the row
  -- the topic was chosen from.
  if state.view == "index" and is_open() then
    state.index_row = vim.api.nvim_win_get_cursor(state.win)[1]
  end
  state.view = "topic"
  state.topic_id = id
  apply(lines, spans, topics.by_id[id].title, 1)
end

local function select_under_cursor()
  local id = state.ids[vim.api.nvim_win_get_cursor(state.win)[1]]
  if id then
    show_topic(id)
  end
end

local function set_keymaps()
  local function map(lhs, fn, desc)
    vim.keymap.set("n", lhs, fn, {
      buffer = state.buf,
      nowait = true,
      silent = true,
      desc = "C Docs: " .. desc,
    })
  end

  map("<CR>", function()
    if state.view == "index" then
      select_under_cursor()
    end
  end, "Open topic under cursor")

  for i = 1, math.min(#state.ids, 9) do
    local id = state.ids[i]
    map(tostring(i), function()
      if state.view == "index" then
        show_topic(id)
      end
    end, "Open topic " .. i)
  end

  local function back()
    if state.view == "topic" then
      show_index()
    end
  end
  map("<BS>", back, "Back to the topic list")
  map("-", back, "Back to the topic list")
  map("<Left>", back, "Back to the topic list")

  map("q", M.close, "Close")
  map("<Esc>", M.close, "Close")
end

local function create_window()
  state.buf = vim.api.nvim_create_buf(false, true)
  vim.bo[state.buf].buftype = "nofile"
  vim.bo[state.buf].bufhidden = "wipe"
  vim.bo[state.buf].swapfile = false
  vim.bo[state.buf].filetype = "cdocs"

  state.win = vim.api.nvim_open_win(state.buf, true, {
    relative = "editor",
    width = 40,
    height = 1,
    row = 1,
    col = 1,
    style = "minimal",
    border = "rounded",
    title = " C Docs ",
    title_pos = "center",
  })

  local wo = vim.wo[state.win]
  wo.wrap = false
  wo.cursorline = true
  wo.number = false
  wo.relativenumber = false
  wo.signcolumn = "no"

  vim.api.nvim_clear_autocmds({ group = augroup })
  vim.api.nvim_create_autocmd("VimResized", {
    desc = "Re-centre the C Docs float",
    group = augroup,
    callback = function()
      if not is_open() then
        return true
      end
      local lines = vim.api.nvim_buf_get_lines(state.buf, 0, -1, false)
      local config = geometry(lines)
      config.border = "rounded"
      vim.api.nvim_win_set_config(state.win, config)
    end,
  })
  vim.api.nvim_create_autocmd("WinClosed", {
    desc = "Forget the C Docs window once it is gone",
    group = augroup,
    pattern = tostring(state.win),
    callback = function()
      state.win = nil
      state.buf = nil
    end,
  })
end

--- Open at the topic list, or refocus and re-render if already open.
function M.open()
  if not is_open() then
    create_window()
  else
    vim.api.nvim_set_current_win(state.win)
  end
  state.ids = select(3, render.index())
  set_keymaps()
  show_index()
end

--- Open straight to a topic.
function M.open_topic(id)
  if not topics.by_id[id] then
    vim.notify("c_docs: no such topic: " .. tostring(id), vim.log.levels.WARN)
    return
  end
  M.open()
  show_topic(id)
end

function M.close()
  if is_open() then
    vim.api.nvim_win_close(state.win, true)
  end
  state.win = nil
  state.buf = nil
  state.view = "index"
end

function M.is_open()
  return is_open()
end

function M.toggle()
  if is_open() then
    M.close()
  else
    M.open()
  end
end

return M
