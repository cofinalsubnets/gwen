include!("../lib/bench.rs");

// mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
// bench/benches/mandelbrot.l). IEEE-double arithmetic in the same op order as
// the reference (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W
// computed before the multiply. checksum = sum of the per-pixel escape counts.
const MAXIT: i64 = 128;
const W: i64 = 128;
const H: i64 = 128;

fn pix(cr: f64, ci: f64) -> i64 {
    let mut zr = 0.0f64;
    let mut zi = 0.0f64;
    let mut it = 0i64;
    while it < MAXIT && zr * zr + zi * zi <= 4.0 {
        let nzr = zr * zr - zi * zi + cr;
        zi = 2.0 * zr * zi + ci;
        zr = nzr;
        it += 1;
    }
    it
}

fn main() {
    bench("mandelbrot", || {
        let mut s: i64 = 0;
        for py in 0..H {
            let ci = -1.5 + py as f64 * (3.0 / H as f64);
            for px in 0..W {
                let cr = -2.0 + px as f64 * (3.0 / W as f64);
                s += pix(cr, ci);
            }
        }
        s
    });
}
