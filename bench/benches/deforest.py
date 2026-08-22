import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# sum of squares of the odd numbers in [0, N) -- a map/filter/fold pipeline.
# map/filter are lazy in py3 (no intermediate lists), but each element still pays
# the per-call lambda overhead -- the interpreter counterpart to love's fused loop.
# checksum = 4891344686 (< 2^53).
def work():
    return sum(map(lambda x: (x * x) % 1000003, filter(lambda x: x % 2 == 1, range(20000))))

bench("deforest", work)
