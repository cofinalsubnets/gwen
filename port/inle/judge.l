; judge.l -- the dock's local VERIFIER. patch.l ASKS the model for a change;
; judge.l SCORES what the
; model writes back: given a candidate love program, it grades a LADDER of signals and
; returns one verdict + two scalar lenses. this is the reward oracle an RL loop
; hangs off AND the filter a differential fuzzer hangs off -- the same object, read
; two ways (the polarities are opposite on purpose).
;
;   out/host/love -l port/inle/judge.l -e "(judge-e \"(puts (show (+ 1 2)))\")"
;
; the ladder, cheapest first:
;   parses  -- the reader accepts every form (in-process, via `sound`; no eval)
;   runs    -- host `love -e` completes with no scare / missing-nom / overlay clash
;   hung    -- ... or it ran past a wall-clock cap (an infinite loop -- a real bug)
;   agree   -- host and love0 (the self-hosted bootstrap) print the SAME output
; the run/hang/agree rungs execute the candidate in a SUBPROCESS under `timeout`, so
; a hang or crash can't take the judge down -- the isolation the in-process reader
; can't give. host-vs-love0 divergence is the gold the memory bugs were made of
; (kernel-tco1-gc, glaze-intparam-guard were both target divergences).
;
; NB: relative binary paths -> run from the repo root. one `-e` form per candidate
; (a `:` block counts as one). pure love + the `hark`/`timeout` shell-out inle already
; uses for curl -- no new nif.

(: ;; -- rung 1: parse, in-process. reads every form with `sound`; `ok` is 0 the
   ;; moment the reader hits an unfinished shape (a returned port), else 1 at eof. --
   (judge-parse s)
      (: (go x n) (: r (sound x)
                    (? (two? r)      (go (cup r) (+ n 1))
                       (id? r 'torn)  (link n 0)           ; incomplete -> (count . 0)
                       (link n 1)))                       ; clean end   -> (count . 1)
         (go s 0))

   ;; -- rungs 2-4: run one target under a wall-clock cap -> (status . stdout).
   ;; host_run captures STDOUT only (stderr is inherited), but the exit STATUS is
   ;; the honest classifier: 0 clean, 1 a terminal scare, 124 timeout(1)'s hang
   ;; kill, >=128 a signal kill (segfault/abort -- the best fuzzer find). a
   ;; missing-nom / a silent reader stop both exit 0 (soft) -- parse is caught at
   ;; rung 1 in-process; a missing-nom's `;; missing` face rides stderr, uncaught
   ;; here (a temp-file + `2>&1` variant can add it -- deferred, it's soft). --
   (judge-run bin prog secs) (hark (L "timeout" (show secs) bin "-e" prog))   ; (show 5)="5"; (string 5) is the BYTE 5

   ;; -- the judge: parse in-process, run host + love0 isolated, classify by status,
   ;; diff, tally. --
   (judge prog)
      (: pr (judge-parse prog) forms (cap pr) parses (cup pr)
         cap-s 5                                          ; the wall-clock cap (seconds)
         rh (judge-run "out/host/love"  prog cap-s) sh (cap rh) oh (cup rh)
         r0 (judge-run "out/host/love0" prog cap-s) s0 (cap r0) o0 (cup r0)
         hung   (? (= sh 124) 1 (= s0 124) 1 0)           ; either target timed out
         crash  (? (>= sh 128) 1 (>= s0 128) 1 0)         ; killed by a signal (segv/abort)
         scare  (? (&& (! (= sh 0)) (&& (! (= sh 124)) (< sh 128))) 1 0)  ; host exited nonzero, not hang/signal
         clean  (? (= sh 0) 1 0)                          ; host ran to a 0 exit
         agree  (? hung 0 (? (&& (= sh s0) (= oh o0)) 1 0))   ; same status AND same stdout (0 if hung)
         v (tablet 0)
         _ (pin v 'parses parses) _ (pin v 'forms forms)  _ (pin v 'hung hung)
         _ (pin v 'crash crash)   _ (pin v 'scare scare)  _ (pin v 'clean clean)
         _ (pin v 'agree agree)   _ (pin v 'status sh)    _ (pin v 'out oh)
         v)

   ;; -- two LENSES over one verdict, opposite polarities:
   ;; reward -- an RL signal: HIGH = a correct, portable program.
   ;; bug    -- a fuzzer signal: HIGH = an interesting divergence / crash to open. --
   (judge-reward v) (+ (peep v 'parses 0)
                       (+ (* 2 (peep v 'clean 0))
                          (+ (peep v 'agree 0)
                             (+ (* -4 (peep v 'hung 0))
                                (* -3 (peep v 'crash 0))))))
   (judge-diverge? v) (? (&& (! (peep v 'hung 0)) (! (peep v 'agree 0))) 1 0)   ; ran both, disagreed
   (judge-bug v)      (+ (* 6 (peep v 'crash 0))          ; a signal kill -- open it first
                         (+ (* 5 (peep v 'hung 0))
                            (+ (* 4 (judge-diverge? v))
                               (* 2 (peep v 'scare 0)))))

   ;; -- a compact human+machine line; returns the verdict for chaining. --
   (judge-report v)
      (: _ (puts "parses=") _ (puts (show (peep v 'parses 0)))
         _ (puts " forms=") _ (puts (show (peep v 'forms 0)))
         _ (puts " clean=") _ (puts (show (peep v 'clean 0)))
         _ (puts " agree=") _ (puts (show (peep v 'agree 0)))
         _ (puts " hung=")  _ (puts (show (peep v 'hung 0)))
         _ (puts " crash=") _ (puts (show (peep v 'crash 0)))
         _ (puts " | reward=") _ (puts (show (judge-reward v)))
         _ (puts " bug=")      _ (puts (show (judge-bug v)))
         _ (putc 10) v)
   (judge-e prog) (judge-report (judge prog)))            ; the one-call entry: run + print + return
