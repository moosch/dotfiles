vim.o.number = true
vim.o.relativenumber = false

vim.o.undofile = true
vim.o.breakindent = true

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.updatetime = 250
vim.o.timeoutlen = 300

vim.o.splitright = true
vim.o.splitbelow = true

vim.schedule(function()
  vim.o.clipboard = "unnamedplus"
end)

vim.o.cursorline = true
vim.o.signcolumn = "yes"
vim.o.termguicolors = true
vim.o.confirm = true
vim.o.scrolloff = 10
vim.o.inccommand = "split"
vim.o.showmode = false

vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2

vim.o.foldmethod = "expr"
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.o.foldlevel = 99
