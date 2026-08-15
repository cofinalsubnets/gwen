; the takeuchi function -- deep non-tail recursion, no allocation.
(load "lib/bench.ss")
(define (tak x y z)
  (if (< y x) (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)) z))
(bench "tak" (lambda () (tak 22 12 6)))
