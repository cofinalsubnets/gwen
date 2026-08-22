; chez scheme benchmark harness -- mirrors bench/bench.l.
; (bench name work) auto-scales the repetition count (doubling until the run
; clears min-ms), then prints one line matching the other harnesses:
;     <name> <lang> <reps> <ms> <checksum>
; work is a nullary thunk returning a deterministic checksum. the language label
; comes from BENCH_LANG (so chez and petite can share these files), default "chez".
(define bench-min-ms 200)
(define bench-lang (or (getenv "BENCH_LANG") "chez"))

(define (bench name work)
  (let loop ((reps 1))
    (let* ((t0 (real-time))
           (chk (let rep ((i 0) (c #f)) (if (< i reps) (rep (+ i 1) (work)) c)))
           (ms (- (real-time) t0)))
      (if (>= ms bench-min-ms)
          (begin (printf "~a ~a ~a ~a ~a\n" name bench-lang reps ms chk)
                 (flush-output-port))
          (loop (* 2 reps))))))
