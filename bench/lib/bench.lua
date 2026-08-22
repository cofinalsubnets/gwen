-- lua benchmark harness -- mirrors bench/bench.l.
-- bench(name, work) auto-scales the repetition count (doubling until the run
-- clears MIN_MS), then prints one line matching the other harnesses:
--     <name> <lang> <reps> <ms> <checksum>
-- work is a nullary function returning a deterministic checksum. os.clock() is
-- CPU time, which is fine for these single-threaded CPU-bound loops. the
-- language label comes from BENCH_LANG (so lua and luajit share these files).
local MIN_MS = 200.0
local LANG = os.getenv("BENCH_LANG") or "lua"

local function bench(name, work)
  local reps = 1
  while true do
    local t0 = os.clock()
    local chk
    for _ = 1, reps do chk = work() end
    local ms = (os.clock() - t0) * 1000.0
    if ms >= MIN_MS then
      print(string.format("%s %s %d %.3f %s", name, LANG, reps, ms, tostring(chk)))
      break
    end
    reps = reps * 2
  end
end

return bench
