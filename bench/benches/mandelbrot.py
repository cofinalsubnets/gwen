import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "lib"))
from bench import bench

# mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
# bench/benches/mandelbrot.l). Same op order as the reference
# (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W computed first.
MAXIT = 128
W = 128
H = 128

def pix(cr, ci):
    zr = 0.0
    zi = 0.0
    it = 0
    while it < MAXIT and zr * zr + zi * zi <= 4.0:
        nzr = zr * zr - zi * zi + cr
        zi = 2.0 * zr * zi + ci
        zr = nzr
        it += 1
    return it

def work():
    s = 0
    for py in range(H):
        ci = -1.5 + py * (3.0 / H)
        for px in range(W):
            cr = -2.0 + px * (3.0 / W)
            s += pix(cr, ci)
    return s

bench("mandelbrot", work)
