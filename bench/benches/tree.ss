; binary-trees allocation/GC stress (see bench/benches/tree.l). checksum = 2^D-1.
(load "lib/bench.ss")
(define (mk d) (if (= d 0) '() (cons (mk (- d 1)) (mk (- d 1)))))
(define (ck t) (if (pair? t) (+ 1 (ck (car t)) (ck (cdr t))) 0))
(bench "tree" (lambda () (ck (mk 16))))
