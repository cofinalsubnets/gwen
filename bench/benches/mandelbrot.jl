include(joinpath(@__DIR__, "..", "lib", "bench.jl"))

# mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
# bench/benches/mandelbrot.l). Same op order as the reference
# (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W computed first.
const MAXIT = 128
const W = 128
const H = 128

function pix(cr, ci)
    zr = 0.0
    zi = 0.0
    it = 0
    while it < MAXIT && zr * zr + zi * zi <= 4.0
        nzr = zr * zr - zi * zi + cr
        zi = 2.0 * zr * zi + ci
        zr = nzr
        it += 1
    end
    return it
end

function mandel_work()
    s = 0
    for py in 0:H-1
        ci = -1.5 + py * (3.0 / H)
        for px in 0:W-1
            cr = -2.0 + px * (3.0 / W)
            s += pix(cr, ci)
        end
    end
    return s
end

bench("mandelbrot", mandel_work)
