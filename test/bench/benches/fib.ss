; naive recursive fibonacci -- function-call and integer-arithmetic stress.
(load "lib/bench.ss")
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(bench "fib" (lambda () (fib 30)))
