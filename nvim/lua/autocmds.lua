vim.api.nvim_create_autocmd("VimEnter", {
  desc = "Open Neotree floating on startup",
  callback = function()
    local open_neotree = true
    for i = 1, vim.fn.argc() do
      local arg = vim.fn.argv(i - 1)
      if vim.fn.isdirectory(arg) == 0 then
        open_neotree = false
        break
      end
    end
    if open_neotree then
      vim.cmd("Neotree float")
    end
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking text",
  group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
  callback = function()
    vim.hl.on_yank()
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Enable treesitter highlighting",
  group = vim.api.nvim_create_augroup("treesitter-highlight", { clear = true }),
  callback = function(args)
    pcall(vim.treesitter.start, args.buf)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "C indent: 4 spaces",
  group = vim.api.nvim_create_augroup("indent-c", { clear = true }),
  pattern = { "c", "cpp" },
  callback = function()
    vim.bo.expandtab = true
    vim.bo.tabstop = 4
    vim.bo.shiftwidth = 4
  end,
})

-- Neovim ignores a swap file whose owning process is dead, but never deletes
-- it, so orphans from uncleanly-killed sessions pile up in ~/.local/state and
-- re-prompt forever. These two helpers identify an orphan so we can drop it.

-- The owning process id lives at byte offset 24 of the swap's block-zero
-- header (b0_id[2] + b0_version[10] + b0_page_size[4] + b0_mtime[4] + b0_ino[4]).
local function swap_owner_pid(swapname)
  local fd = vim.uv.fs_open(swapname, "r", 438)
  if not fd then
    return nil
  end
  local data = vim.uv.fs_read(fd, 4, 24)
  vim.uv.fs_close(fd)
  if type(data) ~= "string" or #data < 4 then
    return nil
  end
  local b1, b2, b3, b4 = data:byte(1, 4)
  return b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
end

local function process_alive(pid)
  if type(pid) ~= "number" or pid <= 0 then
    return false
  end
  return vim.uv.fs_stat("/proc/" .. pid) ~= nil
end

vim.api.nvim_create_autocmd("SwapExists", {
  desc = "Drop stale swap files instead of prompting",
  group = vim.api.nvim_create_augroup("stale-swap", { clear = true }),
  callback = function(args)
    local swapname = vim.v.swapname
    -- swap newer than the file may hold unsaved work: leave the prompt
    if vim.fn.getftime(args.file) < vim.fn.getftime(swapname) then
      return
    end
    -- still open in another session: leave the prompt
    if process_alive(swap_owner_pid(swapname)) then
      return
    end
    vim.fn.delete(swapname)
    vim.v.swapchoice = "e"
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  desc = "Go indent: real tabs, width 4",
  group = vim.api.nvim_create_augroup("indent-go", { clear = true }),
  pattern = { "go" },
  callback = function()
    vim.bo.expandtab = false
    vim.bo.tabstop = 4
    vim.bo.shiftwidth = 4
  end,
})
