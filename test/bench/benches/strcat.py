import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

HMOD = 1000000007
N = 4000

# build an N-char string by repeated single-char concatenation, then hash it.
def work():
    s = ""
    for i in range(N):
        s += chr(48 + i % 10)
    h = 0
    for ch in s:
        h = (h * 31 + ord(ch)) % HMOD
    return h

bench("strcat", work)
