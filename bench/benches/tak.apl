⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ the Takeuchi function tak(22,12,6). checksum = 7. ⍵ is the 3-vector (x y z).
tak←{(x y z)←⍵ ⋄ y≥x:z ⋄ ∇(∇(x-1)y z)(∇(y-1)z x)(∇(z-1)x y)}
work←{tak 22 12 6}

_←work bench.Run 'tak'
