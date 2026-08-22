-- mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
-- bench/benches/mandelbrot.l). Same op order as the reference
-- (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W computed first.
package.path = (arg[0]:match("(.*/)") or "./") .. "../lib/?.lua;" .. package.path
local bench = require("bench")
local MAXIT, W, H = 128, 128, 128
local function pix(cr, ci)
  local zr, zi, it = 0.0, 0.0, 0
  while it < MAXIT and zr * zr + zi * zi <= 4.0 do
    local nzr = zr * zr - zi * zi + cr
    zi = 2.0 * zr * zi + ci
    zr = nzr
    it = it + 1
  end
  return it
end
bench("mandelbrot", function()
  local s = 0
  for py = 0, H - 1 do
    local ci = -1.5 + py * (3.0 / H)
    for px = 0, W - 1 do
      local cr = -2.0 + px * (3.0 / W)
      s = s + pix(cr, ci)
    end
  end
  return s
end)
