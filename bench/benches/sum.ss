; build the list 1..100000 then fold-sum it -- allocation + traversal.
(load "lib/bench.ss")
(define (iota1 n) (let loop ((i n) (a '())) (if (< i 1) a (loop (- i 1) (cons i a)))))
(bench "sum" (lambda () (fold-left + 0 (iota1 100000))))
