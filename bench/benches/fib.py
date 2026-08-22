import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

sys.setrecursionlimit(100000)

def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)

bench("fib", lambda: fib(30))
