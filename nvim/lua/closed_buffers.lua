-- Remembers the last few buffers closed with <leader>w so <leader>W can reopen
-- them, most recently closed first. Repeating <leader>W walks further back.

local M = {}

local MAX = 10

---@type { path: string, pos: integer[] }[]
local stack = {}

local function push(path, pos)
  for i, entry in ipairs(stack) do
    if entry.path == path then
      table.remove(stack, i)
      break
    end
  end
  table.insert(stack, { path = path, pos = pos })
  if #stack > MAX then
    table.remove(stack, 1)
  end
end

-- Close the current buffer without breaking the window layout, remembering it.
function M.close()
  local buf = vim.api.nvim_get_current_buf()
  local path = vim.api.nvim_buf_get_name(buf)
  local is_file = path ~= "" and vim.bo[buf].buftype == ""
  local pos = vim.api.nvim_win_get_cursor(0)

  vim.cmd.bp()
  local ok = pcall(vim.cmd.bd, "#")
  if ok and is_file then
    push(path, pos)
  end
end

-- Reopen the most recently closed buffer.
function M.reopen()
  local entry = table.remove(stack)
  if not entry then
    vim.notify("No recently closed buffers", vim.log.levels.INFO)
    return
  end
  vim.cmd.edit(vim.fn.fnameescape(entry.path))
  pcall(vim.api.nvim_win_set_cursor, 0, entry.pos)
end

return M
