⎕IO←1 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ build the vector 1..100000 then sum it. checksum = 5000050000.
work←{+/⍳100000}

_←work bench.Run 'sum'
