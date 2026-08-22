package main

// mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
// bench/benches/mandelbrot.l). IEEE-double arithmetic in the same op order as
// the reference (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W
// computed before the multiply. checksum = sum of the per-pixel escape counts.
const (
	maxit = 128
	W     = 128
	H     = 128
)

func pix(cr, ci float64) int64 {
	zr := 0.0
	zi := 0.0
	var it int64
	for it < maxit && zr*zr+zi*zi <= 4.0 {
		nzr := zr*zr - zi*zi + cr
		zi = 2.0*zr*zi + ci
		zr = nzr
		it++
	}
	return it
}

func main() {
	bench("mandelbrot", func() int64 {
		var s int64
		for py := 0; py < H; py++ {
			ci := -1.5 + float64(py)*(3.0/H)
			for px := 0; px < W; px++ {
				cr := -2.0 + float64(px)*(3.0/W)
				s += pix(cr, ci)
			}
		}
		return s
	})
}
