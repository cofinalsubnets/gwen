# julia benchmark harness -- mirrors bench/bench.l and lib/bench.py.
# bench(name, work) auto-scales the repetition count (doubling until the run
# clears MIN_MS), then prints one line matching the other harnesses:
#     <name> <lang> <reps> <ms> <checksum>
# work is a nullary closure returning a deterministic checksum. BENCH_LANG sets
# the column label, default "julia". A single untimed warm-up call triggers
# Julia's JIT compilation up front, so the timed loop measures steady state
# (the per-function compile is excluded, like every other harness excludes
# interpreter/JIT startup).
using Printf

const MIN_MS = 200.0
const LANG = get(ENV, "BENCH_LANG", "julia")

function bench(name, work)
    work()                       # warm up: force JIT compilation before timing
    # Opaque (::Any) handle: calling through it is a dynamic dispatch the
    # optimizer can't see into, so it can't hoist or constant-fold a pure
    # `work` out of the timing loop (which would spin the rep-doubling forever).
    w = Base.inferencebarrier(work)
    reps = 1
    while true
        t0 = time_ns()
        chk = nothing
        for _ in 1:reps
            chk = w()
        end
        ms = (time_ns() - t0) / 1.0e6
        if ms >= MIN_MS
            println("$name $LANG $reps $(@sprintf("%.3f", ms)) $chk")
            return
        end
        reps *= 2
    end
end
