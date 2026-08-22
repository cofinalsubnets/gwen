; count primes below 30000 by trial division; checksum = pi(30000) = 3245.
(load "lib/bench.ss")
(define (prime? n)
  (let loop ((d 2))
    (cond ((> (* d d) n) #t) ((= 0 (remainder n d)) #f) (else (loop (+ d 1))))))
(define (count lo hi)
  (let loop ((n lo) (c 0)) (if (< n hi) (loop (+ n 1) (if (prime? n) (+ c 1) c)) c)))
(bench "primes" (lambda () (count 2 30000)))
