import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

HMOD = 1000000007

# fixed string built once; the timed work is a linear rolling-hash scan.
data = "".join(chr(32 + (7 * i) % 95) for i in range(20000))

def work():
    h = 0
    for ch in data:
        h = (h * 31 + ord(ch)) % HMOD
    return h

bench("strscan", work)
