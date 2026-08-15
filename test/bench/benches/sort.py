import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
N = 5000

def work():
    x = 1
    data = []
    for _ in range(N):
        x = (16807 * x) % 2147483647
        data.append(x)
    data.sort()
    h = 0
    for v in data:
        h = (h * 31 + v) % 1000000007
    return h

bench("sort", work)
