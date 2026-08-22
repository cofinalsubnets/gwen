import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# Bell numbers in base 36 (see bench/benches/bell.l). Fresh memo dicts per rep.
# checksum = total characters across all rendered lines.
DIGITS = "0123456789abcdefghijklmnopqrstuvwxyz"
BASE = len(DIGITS)

def bell_run(limit):
    facts, bells = {}, {}

    def fact(n):
        if n in facts:
            return facts[n]
        x, m = 1, n
        while m > 1:
            x *= m
            m -= 1
        facts[n] = x
        return x

    def choose(n, k):
        return fact(n) // (fact(k) * fact(n - k))

    def bell(n):
        if n in bells:
            return bells[n]
        r = 1 if n < 2 else sum(choose(n - 1, k) * bell(k) for k in range(n))
        bells[n] = r
        return r

    def show(n):
        s = ""
        while n > 0:
            s = DIGITS[n % BASE] + s
            n //= BASE
        return s

    total, i = 0, 0
    while True:
        b = show(bell(i))
        if len(b) > limit:
            return total
        total += len(b)
        i += 1

bench("bell", lambda: bell_run(280))
