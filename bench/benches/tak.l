; the takeuchi function -- deep non-tail recursion, no allocation.
(: (tak x y z) (? (< y x) (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)) z)
 (bench "tak" (\ _ (tak 22 12 6))))
