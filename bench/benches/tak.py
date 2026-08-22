import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

sys.setrecursionlimit(100000)

def tak(x, y, z):
    if y < x:
        return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
    return z

bench("tak", lambda: tak(22, 12, 6))
