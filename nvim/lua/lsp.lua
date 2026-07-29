for _, path in ipairs({ "~/.local/bin", "/usr/local/bin", "~/go/bin" }) do
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
  capabilities = require("blink.cmp").get_lsp_capabilities(),
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

vim.lsp.config("elmls", {
  cmd = { "elm-language-server" },
  filetypes = { "elm" },
  root_markers = { "elm.json", ".git" },
})

vim.lsp.enable({ "gopls", "clangd", "dexter", "zls", "terraformls", "ts_ls", "elmls" })

local severity = { "● Error", "◆ Warning", "● Hint", "● Info" }

local function Diag()
  local diags = vim.diagnostic.get(0, { lnum = vim.fn.line(".") - 1 })
  if #diags == 0 then
    return nil
  end
  local lines = {}
  for _, d in ipairs(diags) do
    local icon = severity[d.severity] or "●"
    local msg = d.message:gsub("\n", " "):sub(1, 200)
    table.insert(lines, icon .. " " .. msg)
  end
  return lines
end

local function Hover()
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ bufnr = bufnr })
  if #clients == 0 then
    return
  end
  local params = vim.lsp.util.make_position_params(0, clients[1].offset_encoding)
  local diag_lines = Diag()

  vim.lsp.buf_request(bufnr, "textDocument/hover", params, function(err, result, ctx)
    local hover_lines = {}
    if result and result.contents then
      hover_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
    end

    if not diag_lines and #hover_lines == 0 then
      return
    end

    local combined = {}
    if diag_lines then
      for _, line in ipairs(diag_lines) do
        table.insert(combined, line)
      end
      if #hover_lines > 0 then
        table.insert(combined, "")
      end
    end
    for _, line in ipairs(hover_lines) do
      table.insert(combined, line)
    end

    local trimmed = {}
    for _, line in ipairs(combined) do
      if line ~= "" then
        table.insert(trimmed, line)
      end
    end

    local fbuf, fwin = vim.lsp.util.open_floating_preview(trimmed, "markdown", {
      border = "rounded",
      focusable = true,
    })
    vim.keymap.set("n", "q", "<Cmd>close<CR>", { buffer = fbuf, silent = true })
    vim.keymap.set("n", "<Esc>", "<Cmd>close<CR>", { buffer = fbuf, silent = true })
    vim.api.nvim_win_set_option(fwin, "wrap", true)
    vim.api.nvim_set_current_win(fwin)
  end)
end

vim.api.nvim_create_autocmd("LspAttach", {
  desc = "LSP keybindings",
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    local client = assert(vim.lsp.get_client_by_id(event.data.client_id))
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    map("gd", vim.lsp.buf.definition, "Go to Definition")
    map("gD", vim.lsp.buf.declaration, "Go to Declaration")
    map("gr", vim.lsp.buf.references, "References")
    map("gI", vim.lsp.buf.implementation, "Go to Implementation")
    map("gy", vim.lsp.buf.type_definition, "Go to Type Definition")
    map("K", Hover, "Hover Documentation")
    map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
    map("<leader>rn", vim.lsp.buf.rename, "Rename")

    if client.name == "gopls" then
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = event.buf,
        callback = function()
          vim.lsp.buf.format({ async = false })
        end,
      })
    end
  end,
})
