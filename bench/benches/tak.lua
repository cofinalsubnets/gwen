-- the takeuchi function -- deep non-tail recursion, no allocation.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local function tak(x, y, z)
  if y < x then return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y)) else return z end
end
bench("tak", function() return tak(22, 12, 6) end)
