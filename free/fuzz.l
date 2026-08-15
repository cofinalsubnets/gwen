; fuzz.l -- the dock's generative differential fuzzer: the judge's first CLIENT.
; the loop the memory keeps circling -- propose a candidate, JUDGE it, keep the
; interesting ones -- with a LOCAL proposer (a random love-program generator).
;
;   out/host/love -l free/judge.l -l free/fuzz.l -e "(fuzz 40 1)"
;
; the generator emits a DETERMINISTIC subset -- arithmetic, application, church
; numerals, lists -- NEVER clock/rand/twirl, so a host-vs-love0 disagreement is a REAL
; bug, never nondeterminism. each candidate E is wrapped `(puts (show E))` so its
; VALUE is the observable the judge diffs across targets. apcap + the judge's
; `timeout` fence the church-exponent towers (a blow-up lands as scare/hung, caught).
;
; interesting = judge-bug > 0: a crash, a hang, a host!=love0 divergence, or a scare.

(: ;; -- a reproducible RNG in a slot (wheel once; no state threading) --
   rng-slot (tablet 0)
   (rng-init n) (pin rng-slot 0 (wheel n))
   (rng-next n) (: r (random (peep rng-slot 0 (wheel 1)) n) _ (pin rng-slot 0 (cup r)) (cap r))
   (jnth l i) (? (= i 0) (cap l) (jnth (cup l) (- i 1)))  ; nth is absent here -- roll our own
   (pick l) (jnth l (rng-next (tally l)))                ; a random element of a list

   ;; -- the generator: build love DATA, depth-bounded --
   ops '(+ - * / // % < = <= > >= |)
   ;; NB: unit params (u) -- a NULLARY define `(f) body` is `(\ body)` = QUOTE, so
   ;; `f` would be the DATA `body`, never a callable. fire each with the unit `()`.
   (gen-op u)   (pick ops)
   (gen-num u)  (rng-next 8)                             ; a small church exponent / literal
   (gen-atom u) (: k (rng-next 11)                       ; a terminal
                   (? (< k 9) k                          ; 0..8  : a small int
                      (= k 9) ()                         ; the zero point
                      "ab"))                             ; a string
   (gen d)                                               ; -> a love expression (data)
      (? (! d) (gen-atom ())
         (: k (rng-next 6)
            (? (= k 0) (gen-atom ())
               (= k 1) (L (gen-op ())  (gen (- d 1)) (gen (- d 1)))   ; (op a b) -- dyadic
               (= k 2) (L (gen-num ()) (gen (- d 1)))                 ; (n x)    -- church apply
               (= k 3) (L (gen (- d 1)) (gen (- d 1)))                ; (f x)    -- application
               (= k 4) (L 'L (gen (- d 1)) (gen (- d 1)))             ; (L a b)  -- a list
               (L (gen-op ()) (gen (- d 1)) (gen (- d 1)) (gen (- d 1))))))  ; (op a b c) -- church-EXP tower

   (candidate d) (show (L 'puts (L 'show (gen d))))       ; wrap so the VALUE is printed, -> a source string

   ;; -- the loop: explore n candidates, keep the bug>0 ones --
   (fuzz-go i n finds)
      (? (= i n) finds
         (: c (candidate 3) v (judge c) bug (judge-bug v)
            (? (< 0 bug)
               (: _ (puts "  [bug ") _ (puts (show bug)) _ (puts "] ") _ (puts c) _ (putc 10)
                  _ (puts "         ") _ (judge-report v)
                  (fuzz-go (+ i 1) n (link (link c v) finds)))
               (fuzz-go (+ i 1) n finds))))
   (fuzz n s)
      (: _ (rng-init s)
         _ (puts "-- fuzzing ") _ (puts (show n)) _ (puts " candidates (seed ") _ (puts (show s)) _ (puts ") --") _ (putc 10)
         finds (fuzz-go 0 n ())
         _ (puts "-- ") _ (puts (show (tally finds))) _ (puts " interesting / ") _ (puts (show n)) _ (putc 10)
         finds))
