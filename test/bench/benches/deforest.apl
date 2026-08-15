⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ sum of (x*x mod 1000003) over the odd x in [0,20000). checksum = 4891344686.
work←{d←⍳20000 ⋄ o←d/⍨1=2|d ⋄ +/1000003|o*2}

_←work bench.Run 'deforest'
