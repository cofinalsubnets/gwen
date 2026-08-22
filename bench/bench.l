; love benchmark harness -- shared timing, reporting and list helpers.
; concatenated ahead of each bench/benches/*.g file by bench/Makefile, so the
; bindings below leak into global scope (a top-level `:` with no trailing body
; defines globals). every bench file ends in one (bench name work) call.
;
; `bench` auto-scales the repetition count: it runs `work` an increasing power
; of two times until the wall time clears bench-min-ms, then reports the chosen
; reps and the total ms so a per-iteration time is reps-independent. one line is
; printed per bench, matching the python/node harnesses:
;     <name> love <reps> <ms> <checksum>
; the lang label comes from BENCH_LANG (run.sh exports it), default "love" -- the
; SAME contract as lib/bench.* (bench.py/.lisp/...); it MUST equal the column key
; in bench/Makefile's lang lists or report.awk drops the column.
; work is a unary thunk (called as (work ()), since love has no nullary calls -- the
; do-nothing arg is the UNIT, not 0)
; and must return a deterministic checksum the other languages reproduce.
(: (iota1 n) (map (+ 1) (jot n))                                     ; 1..n (iota is prel now)
   bench-lang (|| (getenv "BENCH_LANG") "love")                         ; column key; matches lib/bench.* default-to-own-name
   bench-min-ms 200                                                   ; timing floor
   (bench-run work reps)                                              ; -> (ms . chk)
    (: t0  (clock 0)
       chk ((: (go i a) (? (< i reps) (go (+ i 1) (work ())) a)) 0 0)
     (link (- (clock 0) t0) chk))
   (bench-loop work reps)                                             ; double until >= floor
    (: r (bench-run work reps)
       (? (< (cap r) bench-min-ms) (bench-loop work (* 2 reps)) (link reps r)))
   (bench name work)
    (: r    (bench-loop work 1)
       reps (cap r) ms (caup r) chk (cuup r)
     (: _ (puts name)       _ (putc 32)
        _ (puts bench-lang) _ (putc 32)
        _ (putx reps)   _ (putc 32)
        _ (putx ms)     _ (putc 32)
        _ (putx chk)    _ (putc 10) 0)))
