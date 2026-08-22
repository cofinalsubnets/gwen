; binary-trees allocation/GC stress: build a perfect binary tree of depth D
; (2^D-1 internal nodes, leaves are nil) then traverse counting nodes. checksum
; = node count = 2^D-1. measures small-aggregate allocation + collector churn.
(: (mk d) (? (< d 1) 0 (link (mk (- d 1)) (mk (- d 1))))
   (ck t) (? (two? t) (+ 1 (+ (ck (cap t)) (ck (cup t)))) 0)
 (bench "tree" (\ _ (ck (mk 16)))))
