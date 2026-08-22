import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

def is_prime(n):
    d = 2
    while d * d <= n:
        if n % d == 0:
            return False
        d += 1
    return True

def count(lo, hi):
    c = 0
    for n in range(lo, hi):
        if is_prime(n):
            c += 1
    return c

bench("primes", lambda: count(2, 30000))
