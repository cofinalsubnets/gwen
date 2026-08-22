; count primes below 30000 by trial division -- integer arithmetic + branching.
; checksum = pi(30000) = 3245.
(: (prime? n) ((: (go d) (? (< n (* d d)) 1 (? (= 0 (% n d)) 0 (go (+ d 1))))) 2)
   (count lo hi) ((: (go n c) (? (< n hi) (go (+ n 1) (? (prime? n) (+ c 1) c)) c)) lo 0)
 (bench "primes" (\ _ (count 2 30000))))
