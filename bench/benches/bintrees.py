import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
# GC-throughput / long-lived-survival workload. build a stretch tree of depth
# max+1, hold a long-lived tree of depth max alive across the run, then for each
# depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
# node counts. a leaf is None and counts 0.
def mk(d):
    return None if d < 1 else (mk(d - 1), mk(d - 1))

def ck(t):
    return 0 if t is None else 1 + ck(t[0]) + ck(t[1])

def bt_run(mn, mx):
    stretch = ck(mk(mx + 1))
    long = mk(mx)                       # LONG-LIVED -- survives the loop below
    total = 0
    for d in range(mn, mx + 1, 2):
        n = 1 << (mx - d + mn)
        s = 0
        for _ in range(n):
            s += ck(mk(d))
        total += s
    return stretch + ck(long) + total

bench("bintrees", lambda: bt_run(4, 14))
