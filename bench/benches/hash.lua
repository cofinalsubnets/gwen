-- mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
-- keys are sparse (stride 97) so they land in the table's hash part, not the
-- contiguous array part.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local N = 10000
bench("hash", function()
  local h = {}
  for i = 0, N - 1 do h[97 * i + 1] = i end
  local a = 0
  for i = 0, N - 1 do a = a + h[97 * i + 1] end
  for i = 0, N - 1 do local k = 97 * i + 1; h[k] = h[k] + 1 end
  for i = 0, N - 1 do a = a + h[97 * i + 1] end
  return a
end)
