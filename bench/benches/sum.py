import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# build the list 1..100000 then sum it.
bench("sum", lambda: sum(list(range(1, 100001))))
