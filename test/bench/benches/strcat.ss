; build an N-char string by repeated single-char concatenation, then hash it.
(load "lib/bench.ss")
(define hmod 1000000007)
(define n 4000)
(bench "strcat"
  (lambda ()
    (let loop ((i 0) (s ""))
      (if (< i n)
          (loop (+ i 1) (string-append s (string (integer->char (+ 48 (modulo i 10))))))
          (let hloop ((j 0) (h 0))
            (if (< j (string-length s))
                (hloop (+ j 1) (modulo (+ (* h 31) (char->integer (string-ref s j))) hmod))
                h))))))
