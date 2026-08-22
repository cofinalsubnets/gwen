; closure / higher-order stress (see bench/benches/closure.l): twice(adder(i))(i)=3i, rolling hash.
(load "lib/bench.ss")
(define N 100000)
(define M 1000000007)
(define (twice f) (lambda (x) (f (f x))))
(define (adder i) (lambda (x) (+ x i)))
(bench "closure"
  (lambda ()
    (let loop ((i 0) (acc 0))
      (if (< i N) (loop (+ i 1) (modulo (+ (* acc 31) ((twice (adder i)) i)) M)) acc))))
