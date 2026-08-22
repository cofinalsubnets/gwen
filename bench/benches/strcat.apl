⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ build an N-byte string by repeated single-byte concatenation, then hash it.
⍝ checksum = 222329890.
∇ z←work dummy;N;s;i;h;c
  N←4000 ⋄ s←⍬
  :For i :In ⍳N ⋄ s,←48+10|i ⋄ :EndFor
  h←0 ⋄ :For c :In s ⋄ h←1000000007|c+h×31 ⋄ :EndFor
  z←h
∇

_←work bench.Run 'strcat'
