-- Per-picker history of what was typed into the search box when the picker
-- closed. Unlike snacks' built-in history, which records every intermediate
-- query while typing, only the final non-empty text is kept (max 10, newest
-- first, duplicates collapsed). <C-Up> / <C-Down> in the picker input step
-- through it.
--
--   Snacks.picker.grep(require("search_history").opts("grep", { ... }))

local M = {}

local MAX = 10

---@type table<string, string[]>
local lists = {}

local function list(source)
  lists[source] = lists[source] or {}
  return lists[source]
end

local function push(source, text)
  local l = list(source)
  for i, v in ipairs(l) do
    if v == text then
      table.remove(l, i)
      break
    end
  end
  table.insert(l, 1, text)
  if #l > MAX then
    table.remove(l)
  end
end

local function show(picker, text)
  picker.input:set(text, text)
end

-- Move through the history. `step` is +1 for older, -1 for newer; position 0 is
-- whatever was being typed before navigation started.
local function move(picker, source, step)
  local l = list(source)
  local state = picker._search_history
  if not state then
    state = { idx = 0, draft = picker.input:get() }
    picker._search_history = state
  end
  local idx = math.min(math.max(state.idx + step, 0), #l)
  if idx == state.idx then
    return
  end
  state.idx = idx
  show(picker, idx == 0 and state.draft or l[idx])
end

---@param source string  history bucket, e.g. "grep" or "files"
---@param opts? table  picker opts to extend
function M.opts(source, opts)
  opts = opts or {}
  local user_on_close = opts.on_close

  opts.on_close = function(picker)
    local text = vim.trim(picker.input:get())
    if text ~= "" then
      push(source, text)
    end
    if user_on_close then
      user_on_close(picker)
    end
  end

  opts.actions = vim.tbl_extend("force", opts.actions or {}, {
    search_history_older = function(picker)
      move(picker, source, 1)
    end,
    search_history_newer = function(picker)
      move(picker, source, -1)
    end,
  })

  opts.win = vim.tbl_deep_extend("force", opts.win or {}, {
    input = {
      keys = {
        ["<C-Up>"] = { "search_history_older", mode = { "i", "n" } },
        ["<C-Down>"] = { "search_history_newer", mode = { "i", "n" } },
      },
    },
  })

  return opts
end

return M
