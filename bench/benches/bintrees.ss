; bintrees -- the benchmark-game binary-trees (see bench/benches/bintrees.l): a
; GC-throughput / long-lived-survival workload. build a stretch tree of depth
; max+1, hold a long-lived tree of depth max alive across the run, then for each
; depth d in min..max step 2 build 2^(max-d+min) short-lived trees and sum their
; node counts. a leaf is () and counts 0.
(load "lib/bench.ss")
(define (mk d) (if (< d 1) '() (cons (mk (- d 1)) (mk (- d 1)))))
(define (ck t) (if (pair? t) (+ 1 (ck (car t)) (ck (cdr t))) 0))
(define (bt-run mn mx)
  (let ((stretch (ck (mk (+ mx 1))))
        (long (mk mx)))                 ; LONG-LIVED -- survives the loop below
    (let loop ((d mn) (total 0))
      (if (> d mx)
          (+ stretch (ck long) total)
          (let ((n (expt 2 (- (+ mx mn) d))))
            (let inner ((i 0) (s 0))
              (if (< i n)
                  (inner (+ i 1) (+ s (ck (mk d))))
                  (loop (+ d 2) (+ total s)))))))))
(bench "bintrees" (lambda () (bt-run 4 14)))
