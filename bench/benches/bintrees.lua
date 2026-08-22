-- bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
-- GC-throughput / long-lived-survival workload. build a stretch tree of depth
-- max+1, hold a long-lived tree of depth max alive across the run, then for each
-- depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
-- node counts. a leaf is nil and counts 0.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local function mk(d) if d < 1 then return nil else return { mk(d - 1), mk(d - 1) } end end
local function ck(t) if t == nil then return 0 else return 1 + ck(t[1]) + ck(t[2]) end end
local function bt_run(mn, mx)
  local stretch = ck(mk(mx + 1))
  local long = mk(mx) -- LONG-LIVED -- survives the loop below
  local total = 0
  for d = mn, mx, 2 do
    local n = 2 ^ (mx - d + mn)
    local s = 0
    for _ = 1, n do s = s + ck(mk(d)) end
    total = total + s
  end
  return stretch + ck(long) + total
end
bench("bintrees", function() return bt_run(4, 14) end)
