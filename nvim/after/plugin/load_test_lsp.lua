local client = vim.lsp.start_client {
  -- The path to binary.
  name = "funlsp",
  cmd = { "/home/ryan/moosch/lsp-in-go/main" },
}

if not client then
  vim.notify "Client didn't work"
  return
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    vim.lsp.buf_attach_client(0, client)
  end,
})
