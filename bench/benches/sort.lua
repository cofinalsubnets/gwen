-- sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local N = 5000
bench("sort", function()
  local x = 1
  local data = {}
  for i = 1, N do x = (16807 * x) % 2147483647; data[i] = x end
  table.sort(data)
  local h = 0
  for i = 1, N do h = (h * 31 + data[i]) % 1000000007 end
  return h
end)
