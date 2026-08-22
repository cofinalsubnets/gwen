;;; naive recursive fibonacci -- function-call and integer-arithmetic stress.
(load "lib/bench.lisp")
(declaim (ftype (function (fixnum) fixnum) fib))
(defun fib (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type fixnum n))
  (if (< n 2) n (the fixnum (+ (fib (- n 1)) (fib (- n 2))))))
(bench "fib" (lambda () (fib 30)))
