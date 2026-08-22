// mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
// bench/benches/mandelbrot.l). Same op order as the reference
// (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W computed first.
const { bench } = require("../lib/bench");
const MAXIT = 128, W = 128, H = 128;
function pix(cr, ci) {
  let zr = 0.0, zi = 0.0, it = 0;
  while (it < MAXIT && zr * zr + zi * zi <= 4.0) {
    const nzr = zr * zr - zi * zi + cr;
    zi = 2.0 * zr * zi + ci;
    zr = nzr;
    it++;
  }
  return it;
}
function work() {
  let s = 0;
  for (let py = 0; py < H; py++) {
    const ci = -1.5 + py * (3.0 / H);
    for (let px = 0; px < W; px++) {
      const cr = -2.0 + px * (3.0 / W);
      s += pix(cr, ci);
    }
  }
  return s;
}
bench("mandelbrot", work);
