⎕IO←0 ⋄ ⎕PP←17
⎕FIX'file://lib/bench.apl'

⍝ count the primes in [2,30000) -- a sieve of Eratosthenes (the array-idiomatic
⍝ counterpart of the reference trial division; same count). checksum = 3245.
∇ z←work dummy;n;isp;p
  n←30000 ⋄ isp←n⍴1 ⋄ isp[0 1]←0 ⋄ p←2
  :While (p×p)<n
    :If isp[p] ⋄ isp[(p*2)+p×⍳⌈(n-p*2)÷p]←0 ⋄ :EndIf
    p←p+1
  :EndWhile
  z←+/isp
∇

_←work bench.Run 'primes'
