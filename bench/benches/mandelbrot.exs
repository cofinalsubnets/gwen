Code.require_file("../lib/bench.exs", __DIR__)

# mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (see
# bench/benches/mandelbrot.l). Same op order as the reference
# (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci), the grid step 3.0/W computed first.
defmodule Mandelbrot do
  @maxit 128
  @w 128
  @h 128

  def pix(cr, ci), do: pix(cr, ci, 0.0, 0.0, 0)
  def pix(cr, ci, zr, zi, it) do
    if it < @maxit and zr * zr + zi * zi <= 4.0 do
      pix(cr, ci, zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, it + 1)
    else
      it
    end
  end

  def run do
    Enum.reduce(0..(@h - 1), 0, fn py, s ->
      ci = -1.5 + py * (3.0 / @h)
      Enum.reduce(0..(@w - 1), s, fn px, s ->
        cr = -2.0 + px * (3.0 / @w)
        s + pix(cr, ci)
      end)
    end)
  end
end

Bench.run("mandelbrot", fn -> Mandelbrot.run() end)
