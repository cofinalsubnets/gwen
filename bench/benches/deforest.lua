-- sum of squares of the odd numbers in [0, N) -- a map/filter/fold list pipeline.
-- lua has no built-in map/filter, so the idiom is a single loop over the range (no
-- intermediate tables) -- this is the hand-written loop love's deforestation produces.
-- checksum = 4891344686 (< 2^53, exact in a double).
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
bench("deforest", function()
  local s = 0
  for i = 0, 19999 do
    if i % 2 == 1 then s = s + (i * i) % 1000003 end
  end
  return s
end)
