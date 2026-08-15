;;; count primes below 30000 by trial division; checksum = pi(30000) = 3245.
(load "lib/bench.lisp")
(defun prime-p (n)
  (loop for d from 2 while (<= (* d d) n)
        do (when (zerop (mod n d)) (return-from prime-p nil)))
  t)
(defun count-primes (lo hi) (loop for n from lo below hi count (prime-p n)))
(bench "primes" (lambda () (count-primes 2 30000)))
