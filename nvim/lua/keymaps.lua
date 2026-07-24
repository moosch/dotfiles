vim.api.nvim_create_user_command("Notify", function(opts)
  vim.notify(opts.args)
end, { nargs = "+" })

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

vim.keymap.set("n", "U", "<C-r>", { desc = "Redo" })

vim.keymap.set("n", "<leader>-", "<C-w>s", { desc = "Split horizontal" })
vim.keymap.set("n", "<leader>|", "<C-w>v", { desc = "Split vertical" })

vim.keymap.set("n", "<leader><left>", "<C-w>h", { desc = "Focus left" })
vim.keymap.set("n", "<leader><right>", "<C-w>l", { desc = "Focus right" })
vim.keymap.set("n", "<leader><up>", "<C-w>k", { desc = "Focus up" })
vim.keymap.set("n", "<leader><down>", "<C-w>j", { desc = "Focus down" })

vim.keymap.set("n", "<leader>w", function()
  vim.cmd.bp()
  vim.cmd.bd("#")
end, { desc = "Close buffer without breaking layout" })

vim.keymap.set("n", "<leader><", "<C-o>", { desc = "Jump back" })
vim.keymap.set("n", "<leader>>", "<C-i>", { desc = "Jump forward" })

vim.keymap.set("v", "<S-Down>", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set("v", "<S-Up>", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
