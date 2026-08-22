; fixed string built once; the timed work is a linear rolling-hash scan.
(load "lib/bench.ss")
(define hmod 1000000007)
(define data
  (let ((s (make-string 20000)))
    (let loop ((i 0))
      (when (< i 20000)
        (string-set! s i (integer->char (+ 32 (modulo (* 7 i) 95))))
        (loop (+ i 1))))
    s))
(bench "strscan"
  (lambda ()
    (let hloop ((j 0) (h 0))
      (if (< j (string-length data))
          (hloop (+ j 1) (modulo (+ (* h 31) (char->integer (string-ref data j))) hmod))
          h))))
