# python benchmark harness -- mirrors bench/bench.l.
# bench(name, work) auto-scales the repetition count (doubling until the run
# clears MIN_MS), then prints one line matching the other harnesses:
#     <name> <lang> <reps> <ms> <checksum>
# the same file serves cpython, pypy, and hy (which imports it); BENCH_LANG sets
# the label so the columns stay distinct, default "python".
import os, time

MIN_MS = 200.0
LANG = os.environ.get("BENCH_LANG", "python")

def bench(name, work):
    reps = 1
    while True:
        t0 = time.perf_counter()
        chk = None
        for _ in range(reps):
            chk = work()
        ms = (time.perf_counter() - t0) * 1000.0
        if ms >= MIN_MS:
            break
        reps *= 2
    print(f"{name} {LANG} {reps} {ms:.3f} {chk}")
