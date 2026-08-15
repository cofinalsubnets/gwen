; closure -- higher-order stress, idiomatic love. `(2 (+ i))` is church-2 of the section (+i) =
; (+i)∘(+i); applied to i it is 3i (apply "add i" twice). The compiler flattens/inlines the
; composition -- fair, every language's optimizer does the same to its own closures -- so what runs is
; the rolling-hash loop, which loopclose CAN'T collapse to a formula: a real 100000-iteration loop,
; glazed native. (A plain sum would close to O(1) and measure nothing.) checksum = the hash.
(: hmod 1000000007
   (hof i acc) (? (< i 100000) (hof (+ i 1) (% (+ (* acc 31) ((2 (+ i)) i)) hmod)) acc)
 (bench "closure" (\ _ (hof 0 0))))
