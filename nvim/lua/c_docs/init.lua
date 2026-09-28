-- C reference docs in a popup.
--
-- :CDocs opens a float listing the reference topics. <CR> on a topic replaces
-- the window contents with it, <BS> goes back to the list, q or <Esc> closes.
-- :CDocs {topic} opens one directly.
--
-- The content layer is public and window-free, so the bundled float is not the
-- only way to read it:
--
--   local lines = require("c_docs").lines("printf")
--   Snacks.win({ text = lines, border = "rounded", enter = true })
--
-- No keymap is set here; bind M.toggle from lua/keymaps.lua.

local render = require("c_docs.render")
local topics = require("c_docs.topics")
local ui = require("c_docs.ui")

local M = {}

M.open = ui.open
M.open_topic = ui.open_topic
M.close = ui.close
M.toggle = ui.toggle
M.is_open = ui.is_open

--- The topics in list order.
--- @return table[] { id, title, summary }
function M.topics()
  local out = {}
  for i, topic in ipairs(topics.list) do
    out[i] = { id = topic.id, title = topic.title, summary = topic.summary }
  end
  return out
end

--- Rendered content for your own window. A nil id gives the topic list.
--- @return string[]|nil lines, table[]|nil highlight spans
function M.lines(id)
  if id == nil then
    return render.index()
  end
  return render.topic(id)
end

--- The highlight groups the spans refer to, mapped to what they link to by
--- default, for a consumer that wants to restyle them.
function M.highlight_groups()
  return vim.deepcopy(render.groups)
end

vim.api.nvim_create_user_command("CDocs", function(opts)
  if opts.args ~= "" then
    M.open_topic(opts.args)
  else
    M.open()
  end
end, {
  nargs = "?",
  complete = function(arg_lead)
    return vim.tbl_filter(function(id)
      return id:find(arg_lead, 1, true) == 1
    end, topics.ids())
  end,
  desc = "C reference docs popup",
})

return M
