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
