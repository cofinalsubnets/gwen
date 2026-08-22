import Bench

-- mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
-- bench/benches/mandelbrot.l). IEEE-double arithmetic in the same op order as
-- the reference (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W
-- computed before the multiply. checksum = sum of the per-pixel escape counts.
def MAXIT : Nat := 128
def W : Nat := 128
def H : Nat := 128

@[inline] def pix (cr ci : Float) : Nat := Id.run do
  let mut zr := 0.0
  let mut zi := 0.0
  let mut it : Nat := 0
  for _ in [0:MAXIT] do
    if zr * zr + zi * zi > 4.0 then break
    let nzr := zr * zr - zi * zi + cr
    zi := 2.0 * zr * zi + ci
    zr := nzr
    it := it + 1
  return it

-- `z` is the harness's opaque 0 (as 0.0); adding it to `ci` keeps the grid from
-- folding to a compile-time constant while staying bit-identical (lib/Bench.lean).
def main : IO Unit := bench "mandelbrot" (fun z =>
  let zf := Float.ofNat z
  Id.run do
    let mut s : Int := 0
    for py in [0:H] do
      let ci := -1.5 + Float.ofNat py * (3.0 / Float.ofNat H) + zf
      for px in [0:W] do
        let cr := -2.0 + Float.ofNat px * (3.0 / Float.ofNat W)
        s := s + Int.ofNat (pix cr ci)
    return s)
