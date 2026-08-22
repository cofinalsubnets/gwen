⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ fixed 20000-byte string built once; the timed work is a linear rolling-hash
⍝ scan (h ← (h*31 + byte) mod 1e9+7). checksum = 219660688.
data←32+95|7×⍳20000

∇ z←work dummy;h;c
  h←0 ⋄ :For c :In data ⋄ h←1000000007|c+h×31 ⋄ :EndFor
  z←h
∇

_←work bench.Run 'strscan'
