;;; fixed string built once; the timed work is a linear rolling-hash scan.
(load "lib/bench.lisp")
(defparameter *hmod* 1000000007)
(defparameter *data*
  (let ((s (make-string 20000)))
    (dotimes (i 20000)
      (setf (char s i) (code-char (+ 32 (mod (* 7 i) 95)))))
    s))
(bench "strscan"
       (lambda ()
         (let ((h 0))
           (loop for ch across *data* do (setf h (mod (+ (* h 31) (char-code ch)) *hmod*)))
           h)))
