-- The topic registry. Order here is the order in the list view, so it is an
-- array rather than a map. Adding a topic is one new file plus one line below.

local M = {}

M.list = {
  require("c_docs.topics.printf"),
  require("c_docs.topics.types"),
}

M.by_id = {}
for _, topic in ipairs(M.list) do
  M.by_id[topic.id] = topic
end

--- Ordered ids, for command completion.
function M.ids()
  local ids = {}
  for _, topic in ipairs(M.list) do
    ids[#ids + 1] = topic.id
  end
  return ids
end

return M
