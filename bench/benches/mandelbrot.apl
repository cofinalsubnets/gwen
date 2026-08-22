⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5], same
⍝ grid step (3.0/W) and op order as the reference, vectorised over the whole
⍝ grid: m = (|z|^2 <= 4) gates the per-cell iteration count, escaped cells are
⍝ frozen to (3,0) so |z| can't overflow. checksum = 424578.
∇ z←work dummy;W;H;MAXIT;cr;ci;zr;zi;it;m;tzr;k
  W←128 ⋄ H←128 ⋄ MAXIT←128
  cr←(H⍴0)∘.+(-2.0)+(3.0÷W)×⍳W
  ci←((-1.5)+(3.0÷H)×⍳H)∘.+(W⍴0)
  zr←(H W)⍴0.0 ⋄ zi←(H W)⍴0.0 ⋄ it←(H W)⍴0
  :For k :In ⍳MAXIT
    m←4≥(zr×zr)+(zi×zi)
    it←it+m
    tzr←((zr×zr)-(zi×zi))+cr
    zi←(2×zr×zi)+ci
    zr←tzr
    zr←(m×zr)+(~m)×3
    zi←m×zi
  :EndFor
  z←+/,it
∇

_←work bench.Run 'mandelbrot'
