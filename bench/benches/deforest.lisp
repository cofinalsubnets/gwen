;;; map/filter/fold list pipeline: sum the squares of the odd numbers in [0, N).
;;; the range + intermediate lists are built INSIDE the timed work. checksum = 4891344686.
(load "lib/bench.lisp")
(bench "deforest"
       (lambda () (reduce #'+ (mapcar (lambda (x) (mod (* x x) 1000003))
                                      (remove-if-not #'oddp (loop for i from 0 below 20000 collect i))))))
