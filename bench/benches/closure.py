import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench
twice = lambda f: lambda x: f(f(x))
adder = lambda i: lambda x: x + i
N, M = 100000, 1000000007
def work():
    acc = 0
    for i in range(N):
        acc = (acc * 31 + twice(adder(i))(i)) % M
    return acc
bench("closure", work)
