;;; binary-trees -- GC-throughput / long-lived-survival (see bench/benches/bintrees.l):
;;; a stretch tree of depth max+1, a long-lived tree of depth max held across the run,
;;; then for each depth d in min..max step 2 build 2^(max-d+min) short-lived trees and
;;; sum their node counts. cons cells, a leaf is nil and counts 0. checksum = 1600174.
(load "lib/bench.lisp")
(defun mk (d) (if (< d 1) nil (cons (mk (- d 1)) (mk (- d 1)))))
(defun ck (x) (if (null x) 0 (+ 1 (ck (car x)) (ck (cdr x)))))
(defun bt-run (mn mx)
  (let ((stretch (ck (mk (+ mx 1))))
        (long (mk mx))                          ; LONG-LIVED -- survives the loop below
        (total 0))
    (loop for d from mn to mx by 2 do
      (let ((n (ash 1 (+ (- mx d) mn))) (s 0))
        (dotimes (i n) (incf s (ck (mk d))))
        (incf total s)))
    (+ stretch (ck long) total)))
(bench "bintrees" (lambda () (bt-run 4 14)))
