-- fixed string built once; the timed work is a linear rolling-hash scan.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local HMOD = 1000000007
local parts = {}
for i = 0, 19999 do parts[i + 1] = string.char(32 + (7 * i) % 95) end
local data = table.concat(parts)
bench("strscan", function()
  local h = 0
  for i = 1, #data do h = (h * 31 + data:byte(i)) % HMOD end
  return h
end)
