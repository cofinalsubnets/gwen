; strcat: build an N-char string by repeated single-char concatenation (+),
; then fold a polynomial rolling hash over it. exercises + (each step
; allocates a fresh string, so the build is O(n^2)) and the peep/pin read path.
; the hash is taken mod a prime so it stays < 2^31 and every language -- lua
; included (64-bit ints, no bignums) -- agrees on the checksum.
(: hmod 1000000007
   (strhash s) (: n (tally s)
     (go i h) (? (< i n) (go (+ i 1) (% (+ (* h 31) (peep s i 0)) hmod)) h) (go 0 0))
   (strbuild n) (: (go i s) (? (< i n) (go (+ i 1) (+ s (string (+ 48 (% i 10))))) s) (go 0 ""))
   strcat-n 4000
 (bench "strcat" (\ _ (strhash (strbuild strcat-n)))))
