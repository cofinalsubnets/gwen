-- binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local function mk(d) if d == 0 then return nil else return { mk(d - 1), mk(d - 1) } end end
local function ck(t) if t == nil then return 0 else return 1 + ck(t[1]) + ck(t[2]) end end
bench("tree", function() return ck(mk(16)) end)
