;;; binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
(load "lib/bench.lisp")
(defun mk (d) (if (= d 0) nil (cons (mk (- d 1)) (mk (- d 1)))))
(defun ck (x) (if (consp x) (+ 1 (ck (car x)) (ck (cdr x))) 0))
(bench "tree" (lambda () (ck (mk 16))))
