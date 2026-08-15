-- build the table 1..100000 then sum it -- allocation + traversal.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local data = {}
for i = 1, 100000 do data[i] = i end
bench("sum", function()
  local s = 0
  for i = 1, #data do s = s + data[i] end
  return s
end)
