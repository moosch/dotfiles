-- Custom Go-based markdown LSP ("funlsp"). Only present on machines where it
-- has been built, so skip quietly elsewhere instead of erroring at startup.
local candidates = {
  vim.fn.expand("~/moosch/lsp-in-go/main"),
  "/home/ryan/moosch/lsp-in-go/main",
}

local bin
for _, path in ipairs(candidates) do
  if vim.fn.executable(path) == 1 then
    bin = path
    break
  end
end

if not bin then
  return
end

local client = vim.lsp.start_client({
  name = "funlsp",
  cmd = { bin },
})

if not client then
  vim.notify("funlsp: failed to start client", vim.log.levels.WARN)
  return
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    vim.lsp.buf_attach_client(0, client)
  end,
})
