; strscan: fold a polynomial rolling hash over a fixed 20000-char string built
; once, outside the timed loop. the read counterpart to strcat's allocating
; build -- it exercises only the peep/pin scan path (a linear pass over the
; bytes). printable bytes 32..126 via (32 + 7*i mod 95); the data is built as a
; charlist then realized with `string` (one O(n) allocation, not O(n^2) +).
; the hash stays < 2^31, so the checksum is lua-safe and matches every language.
(: hmod 1000000007
   (strhash s) (: n (tally s)
     (go i h) (? (< i n) (go (+ i 1) (% (+ (* h 31) (peep s i 0)) hmod)) h) (go 0 0))
   (mkdata n) (string ((: (go i) (? (< i n) (link (+ 32 (% (* 7 i) 95)) (go (+ i 1))))) 0))
   data (mkdata 20000)
 (bench "strscan" (\ _ (strhash data))))
