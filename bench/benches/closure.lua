package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local function twice(f) return function(x) return f(f(x)) end end
local function adder(i) return function(x) return x + i end end
local N, M = 100000, 1000000007
bench("closure", function()
  local acc = 0
  for i = 0, N - 1 do acc = (acc * 31 + twice(adder(i))(i)) % M end
  return acc
end)
