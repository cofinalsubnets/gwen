;;; build the list 1..100000 then fold-sum it -- allocation + traversal.
(load "lib/bench.lisp")
(bench "sum" (lambda () (reduce #'+ (loop for i from 1 to 100000 collect i))))
