; merge sort N pseudo-random ints (a MINSTD LCG, double-safe so every language
; produces the identical sequence), then an order-dependent rolling hash of the
; sorted list as the checksum (verifies the ordering, not just the multiset).
; l uses the C `sort` (total order); other dialects use their built-in sort.
(: (lcg x)        (% (* 16807 x) 2147483647)
   (genl x n)     (? (< n 1) 0 (: nx (lcg x) (link nx (genl nx (- n 1)))))
   (hsh acc l)    (? (atom? l) acc (hsh (% (+ (* acc 31) (cap l)) 1000000007) (cup l)))
   (sort-run n)   (hsh 0 (sort (genl 1 n)))
 (bench "sort" (\ _ (sort-run 5000))))
