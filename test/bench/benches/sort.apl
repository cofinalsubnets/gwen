⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ sort N pseudo-random ints (MINSTD LCG), order-dependent rolling-hash checksum.
⍝ checksum = 899994547.
∇ z←work dummy;N;x;data;h;i;v
  N←5000 ⋄ x←1 ⋄ data←⍬
  :For i :In ⍳N ⋄ x←2147483647|16807×x ⋄ data,←x ⋄ :EndFor
  data←data[⍋data]
  h←0 ⋄ :For v :In data ⋄ h←1000000007|v+h×31 ⋄ :EndFor
  z←h
∇

_←work bench.Run 'sort'
