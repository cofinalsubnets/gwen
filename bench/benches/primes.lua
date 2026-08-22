-- count primes below 30000 by trial division; checksum = pi(30000) = 3245.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local function is_prime(n)
  local d = 2
  while d * d <= n do
    if n % d == 0 then return false end
    d = d + 1
  end
  return true
end
bench("primes", function()
  local c = 0
  for n = 2, 29999 do if is_prime(n) then c = c + 1 end end
  return c
end)
