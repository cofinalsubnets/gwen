-- build an N-char string by repeated single-char concatenation, then hash it.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local HMOD = 1000000007
local N = 4000
bench("strcat", function()
  local s = ""
  for i = 0, N - 1 do s = s .. string.char(48 + i % 10) end
  local h = 0
  for i = 1, #s do h = (h * 31 + s:byte(i)) % HMOD end
  return h
end)
