; bintrees -- the benchmark-game binary-trees: a GC-throughput / long-lived-survival workload.
; build a "stretch" tree of depth max+1, hold a LONG-LIVED tree of depth max alive across the run,
; then for each depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their node
; counts. the long-lived tree survives every short-lived one -- sustained collector churn + tenuring,
; the allocation loop our single-build `tree` leaves out (mk now conses NATIVE, so this also scales
; the cons lane). checksum = stretch + long-count + the depth sums (= 1600174 at depths 4..14).
(: (mk d) (? (< d 1) 0 (link (mk (- d 1)) (mk (- d 1))))
   (ck t) (? (two? t) (+ 1 (+ (ck (cap t)) (ck (cup t)))) 0)
   (bt-run mn mx)
    (: stretch (ck (mk (+ mx 1)))                                       ; depth max+1, checked then dropped
       long (mk mx)                                                     ; LONG-LIVED -- survives the loop below
       (depths d acc) (? (> d mx) acc
          (: n (<< 1 (- (+ mx mn) d))                                   ; 2^(max-d+min) trees at this depth
             (inner i s) (? (< i n) (inner (+ i 1) (+ s (ck (mk d)))) s)
             (depths (+ d 2) (+ acc (inner 0 0)))))
       (+ stretch (+ (ck long) (depths mn 0))))
 (bench "bintrees" (\ _ (bt-run 4 14))))
