import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
def mk(d):
    return None if d == 0 else (mk(d - 1), mk(d - 1))

def ck(t):
    return 0 if t is None else 1 + ck(t[0]) + ck(t[1])

bench("tree", lambda: ck(mk(16)))
