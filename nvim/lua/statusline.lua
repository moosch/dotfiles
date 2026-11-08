local M = {}

-- Directory nvim was opened in; fallback when no .git root is found.
local launch_cwd = vim.uv.cwd()

local function project_root(path)
  local start = (path ~= "" and vim.fs.dirname(path)) or launch_cwd
  return vim.fs.root(start, ".git") or launch_cwd
end

local function git_branch(root)
  local f = io.open(root .. "/.git/HEAD", "r")
  if not f then
    return nil
  end
  local head = f:read("*l") or ""
  f:close()
  return head:match("^ref: refs/heads/(.+)$") or head:sub(1, 7)
end

local function diagnostics(buf)
  local count = vim.diagnostic.count(buf)
  local S = vim.diagnostic.severity
  local parts = {}
  local levels = {
    { S.ERROR, "E", "StatusLineError" },
    { S.WARN, "W", "StatusLineWarn" },
    { S.INFO, "I", "StatusLineInfo" },
    { S.HINT, "H", "StatusLineHint" },
  }
  for _, d in ipairs(levels) do
    local n = count[d[1]]
    if n and n > 0 then
      parts[#parts + 1] = "%#" .. d[3] .. "#" .. d[2] .. n .. "%*"
    end
  end
  return #parts > 0 and (table.concat(parts, " ") .. "  ") or ""
end

local function display_path(buf)
  local name = vim.api.nvim_buf_get_name(buf)
  if name == "" then
    return "[No Name]"
  end
  if vim.bo[buf].buftype ~= "" then
    return vim.bo[buf].filetype ~= "" and vim.bo[buf].filetype or name
  end
  local root = project_root(name)
  local rel = vim.fs.relpath(root, name)
  return rel or vim.fn.fnamemodify(name, ":~")
end

function M.render()
  local win = vim.g.statusline_winid
  if type(win) ~= "number" or not vim.api.nvim_win_is_valid(win) then
    win = 0
  end
  local buf = vim.api.nvim_win_get_buf(win)
  local name = vim.api.nvim_buf_get_name(buf)
  local root = project_root(name)
  local project = vim.fs.basename(root)

  local path = display_path(buf):gsub("%%", "%%%%")
  local flags = (vim.bo[buf].modified and " [+]" or "") .. (vim.bo[buf].readonly and " [RO]" or "")
  local branch = git_branch(root)
  branch = branch and (" (" .. branch:gsub("%%", "%%%%") .. ")") or ""
  return table.concat({
    " ", (project:gsub("%%", "%%%%")), branch, " | ", path, flags,
    "%=",
    diagnostics(buf),
    "%y  %l:%c  %p%% ",
  })
end

-- Diagnostic colours on top of the StatusLine background.
local function define_highlights()
  local sl = vim.api.nvim_get_hl(0, { name = "StatusLine", link = false })
  local map = {
    StatusLineError = "DiagnosticError",
    StatusLineWarn = "DiagnosticWarn",
    StatusLineInfo = "DiagnosticInfo",
    StatusLineHint = "DiagnosticHint",
  }
  for group, src in pairs(map) do
    local fg = vim.api.nvim_get_hl(0, { name = src, link = false }).fg
    vim.api.nvim_set_hl(0, group, { fg = fg, bg = sl.bg, bold = true })
  end
end

function M.setup()
  define_highlights()
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("statusline-hl", { clear = true }),
    callback = define_highlights,
  })
  vim.o.laststatus = 3 -- one global statusline, always visible
  vim.o.statusline = "%!v:lua.require'statusline'.render()"
end

return M
