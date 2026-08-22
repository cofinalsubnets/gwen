import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# mutable hash-table throughput (see bench/benches/hash.l). checksum = N*N.
N = 10000

def work():
    h = {}
    for i in range(N):
        h[97 * i + 1] = i
    a = 0
    for i in range(N):
        a += h[97 * i + 1]
    for i in range(N):
        k = 97 * i + 1
        h[k] = h[k] + 1
    for i in range(N):
        a += h[97 * i + 1]
    return a

bench("hash", work)
