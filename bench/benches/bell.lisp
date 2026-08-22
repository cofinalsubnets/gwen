;;; Bell numbers in base 36 (see bench/benches/bell.l) -- a bignum-tower stress.
;;; fresh memo tables per rep; checksum = total characters across all lines.
(load "lib/bench.lisp")
(defparameter *digits* "0123456789abcdefghijklmnopqrstuvwxyz")
(defparameter *base* (length *digits*))
(defun bell-run (limit)
  (let ((facts (make-hash-table)) (bells (make-hash-table)))
    (labels ((fact (n)
               (or (gethash n facts)
                   (setf (gethash n facts)
                         (let ((x 1)) (loop for m from n above 1 do (setf x (* x m))) x))))
             (choose (n k) (/ (fact n) (* (fact k) (fact (- n k)))))
             (bell (n)
               (or (gethash n bells)
                   (setf (gethash n bells)
                         (if (< n 2) 1
                             (loop for k below n sum (* (choose (- n 1) k) (bell k)))))))
             (show (n)
               (let ((s "")) (loop while (> n 0) do
                 (setf s (concatenate 'string
                                      (string (char *digits* (mod n *base*))) s)
                       n (floor n *base*)))
                 s)))
      (loop for i from 0
            for b = (show (bell i))
            while (<= (length b) limit)
            sum (length b)))))
(bench "bell" (lambda () (bell-run 280)))
