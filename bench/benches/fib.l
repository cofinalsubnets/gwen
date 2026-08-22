; naive recursive fibonacci -- function-call and integer-arithmetic stress.
(: (fib n) (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))
 (bench "fib" (\ _ (fib 30))))
