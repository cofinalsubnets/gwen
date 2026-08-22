;;; build an N-char string by repeated single-char concatenation, then hash it.
(load "lib/bench.lisp")
(defparameter *hmod* 1000000007)
(defparameter *n* 4000)
(bench "strcat"
       (lambda ()
         (let ((s ""))
           (dotimes (i *n*)
             (setf s (concatenate 'string s (string (code-char (+ 48 (mod i 10)))))))
           (let ((h 0))
             (loop for ch across s do (setf h (mod (+ (* h 31) (char-code ch)) *hmod*)))
             h))))
