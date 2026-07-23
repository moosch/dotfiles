for _, path in ipairs({ "~/.local/bin", "~/go/bin", "~/Software/node/bin" }) do
  vim.env.PATH = vim.fn.expand(path) .. ":" .. vim.env.PATH
end

local function tsserver_path()
  local handle = io.popen("which tsserver 2>/dev/null")
  if not handle then
    return nil
  end
  local result = handle:read("*a")
  handle:close()
  result = result:gsub("%s+$", "")
  if result == "" then
    return nil
  end
  local f = io.popen("sed -n 's|.*cmd-shim-target=\\(.*\\)/bin/tsserver|\\1|p' " .. result)
  if not f then
    return nil
  end
  local dir = f:read("*a"):gsub("%s+$", "")
  f:close()
  return dir ~= "" and dir .. "/bin/tsserver" or nil
end

vim.lsp.config("*", {
  capabilities = vim.lsp.protocol.make_client_capabilities(),
})

vim.lsp.config("gopls", {
  cmd = { "gopls" },
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  settings = {
    gopls = {
      completeFunctionCalls = true,
      usePlaceholders = true,
    },
  },
})

vim.lsp.config("clangd", {
  cmd = { "clangd", "--fallback-style=c89" },
  filetypes = { "c", "cpp", "objc", "objcpp" },
})

vim.lsp.config("dexter", {
  cmd = { "dexter", "lsp" },
  filetypes = { "elixir", "eelixir", "heex" },
})

vim.lsp.config("zls", {
  cmd = { "zls" },
  filetypes = { "zig" },
})

vim.lsp.config("terraformls", {
  cmd = { "terraform-ls", "serve" },
  filetypes = { "terraform", "tf", "hcl" },
})

local ts_cmd = { "typescript-language-server", "--stdio" }
local ts_path = tsserver_path()
if ts_path then
  table.insert(ts_cmd, "--tsserver-path")
  table.insert(ts_cmd, ts_path)
end

vim.lsp.config("ts_ls", {
  cmd = ts_cmd,
  filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact", "tsx", "jsx" },
})

vim.lsp.enable({ "gopls", "clangd", "dexter", "zls", "terraformls", "ts_ls" })

vim.api.nvim_create_autocmd("LspAttach", {
  desc = "LSP keybindings",
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    local client = assert(vim.lsp.get_client_by_id(event.data.client_id))
    vim.lsp.completion.enable(true, client.id, event.buf)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    map("gd", vim.lsp.buf.definition, "Go to Definition")
    map("gD", vim.lsp.buf.declaration, "Go to Declaration")
    map("gr", vim.lsp.buf.references, "References")
    map("gI", vim.lsp.buf.implementation, "Go to Implementation")
    map("gy", vim.lsp.buf.type_definition, "Go to Type Definition")
    map("K", vim.lsp.buf.hover, "Hover Documentation")
    map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
    map("<leader>rn", vim.lsp.buf.rename, "Rename")

    vim.keymap.set("i", "<C-y>", function()
      vim.lsp.completion.accept()
    end, { buffer = event.buf, desc = "LSP: Accept completion" })
  end,
})
