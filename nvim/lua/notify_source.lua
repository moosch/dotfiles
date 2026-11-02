-- Attribute every vim.notify() message to its originating source (plugin,
-- config file, or Neovim core) by walking the Lua call stack, and stash it in
-- opts.title so snacks.nvim's notifier shows it in the toast and in the
-- history (<leader>n).

local M = {}

-- Frames that are notify plumbing, not the real caller we want to credit.
local function is_plumbing(src)
  return src:match("notify_source%.lua$") -- this module (wrapper + this fn)
    or src:match("_editor%.lua$") -- vim.notify / vim.notify_once shims
    or src:match("/vim/shared%.lua$")
end

local function source_label()
  local cfg = vim.fn.stdpath("config")
  for i = 2, 25 do
    local info = debug.getinfo(i, "S")
    if not info then
      break
    end
    local src = info.source
    if src and src:sub(1, 1) == "@" then
      src = src:sub(2)
      if not is_plumbing(src) then
        -- A lazy.nvim-managed plugin.
        local plugin = src:match("/lazy/([^/]+)/")
        if plugin then
          return plugin
        end

        -- Your own config.
        if src:sub(1, #cfg) == cfg then
          local rel = src:sub(#cfg + 2):gsub("%.lua$", ""):gsub("/", ".")
          return "config: " .. rel
        end

        -- Neovim core / runtime lua module.
        local vmod = src:match("/lua/vim/(.+)%.lua$")
        if vmod then
          return "nvim: vim." .. vmod:gsub("/", ".")
        end

        -- Fallback: just the file basename.
        return vim.fn.fnamemodify(src, ":t:r")
      end
    end
  end
  return nil
end

-- Wrap whatever vim.notify currently is, forwarding to it with an injected
-- title. Idempotent: never double-wraps our own wrapper.
local function wrap()
  if vim.notify == M._wrapper then
    return
  end
  local real = vim.notify
  local wrapper = function(msg, level, opts)
    opts = opts or {}
    if opts.title == nil then
      local label = source_label()
      if label then
        opts.title = label
      end
    end
    return real(msg, level, opts)
  end
  M._wrapper = wrapper
  vim.notify = wrapper
end

function M.setup()
  wrap()
  -- Re-wrap if a plugin replaces vim.notify after us (e.g. a notifier that
  -- installs itself later in the startup sequence).
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = wrap,
  })
end

return M
