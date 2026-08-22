-- naive recursive fibonacci -- function-call and integer-arithmetic stress.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local function fib(n) if n < 2 then return n else return fib(n - 1) + fib(n - 2) end end
bench("fib", function() return fib(30) end)
