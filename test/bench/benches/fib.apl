⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ naive recursive fib(30). checksum = 832040.
fib←{⍵<2:⍵ ⋄ (∇⍵-1)+∇⍵-2}
work←{fib 30}

_←work bench.Run 'fib'
