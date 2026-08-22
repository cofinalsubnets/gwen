; map/filter/fold list pipeline: sum the squares of the odd numbers in [0, N).
; the range + intermediate lists are built INSIDE the timed work (the allocation
; love's deforestation fuses away). checksum = 4891344686.
(load "lib/bench.ss")
(define (sq x) (modulo (* x x) 1000003))
(bench "deforest" (lambda ()
  (fold-left + 0 (map sq (filter odd?
    (let loop ((i 19999) (a '())) (if (< i 0) a (loop (- i 1) (cons i a)))))))))
