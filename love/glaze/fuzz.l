; fuzz -- the glaze's DIFFERENTIAL fuzz. builds closures at random in the shapes the
; natjit creation hook recognizes, applies each, prints the answer. the gate
; (test/test.mk test_glazefuzz) runs this file TWICE against the same binary -- once
; plain, once under LOVE_NO_GLAZE=1 -- and requires the two stdouts to match byte for
; byte. the glaze is only ever allowed to be FASTER, never different.
;
; why a fuzz and not more asserts: the lanes are recognizer + codegen PAIRS, and the
; failure mode when they disagree is not a crash, it is a plausible number. cggir's `?`
; chain ends in a silent () -- an operand it cannot emit compiles to NO CODE at all and
; leaves whatever the accumulator last held -- so a lane that admits one shape too many
; answers something reasonable-looking forever. that is unreadable in review and invisible
; to a test corpus that only ever feeds the lanes what they like. so feed them what they
; do not: the leaf pools below deliberately carry strings, noms, lists and gems, which
; the integer lanes cannot hold and must therefore DECLINE.
;
; the three things that make this fuzz honest, each of which cost a false negative first:
;
;  * FULL APPLICATION. a partially-applied closure's `show` differs between the two runs
;    by design -- the native cell keeps value[-1]=s, the SURFACE src, while the bytecode
;    twin shows its post-cprop body -- and that is transparency working, not a wrong
;    answer. every shape below is applied to exactly its arity, so results are VALUES.
;  * SHAPES THE RECOGNIZERS ACTUALLY MATCH. the call-out and counted-loop lanes want the
;    APPLIED named-let `((: (go i a) ..) 0 a0)`; the un-applied `(: (go i a) .. (go 0 0))`
;    drops past them to the group lane and those recognizers never see it. probe with
;    ((from 'glaze) 'calloutinfo) &c before believing a shape covers a lane.
;  * A HOISTED HARNESS. `upto` below is deliberately NOT a nested single-binding `:` --
;    that is the very shape the 2026-07-29 group-lane miscompile ate, so a harness written
;    the natural way miscompiles ITSELF on the binary it is meant to indict and the fuzz
;    silently runs zero cases while reporting success.
;
; the trailing `fires=` line is the gate's proof of work: it is how many closures the hook
; actually native-backed, and the recipe requires it to be nonzero in the glazed run and
; zero in the other. a green diff over nothing is not evidence.
;
; validated against the pre-fix binary (the natjit group lane gating on jgir-ok? alone,
; with no grpfix): 215 of 3000 cases differed, in exactly the five group shapes.
(: ; globals the call-out lane can resolve -- two arithmetic, two that hand back a value
   ; the integer lane cannot hold, so the lane must decline rather than emit nothing
   (gfz0 i) (i + 1)
   (gfz1 i) (i * 2)
   (gfz2 i) (? (i < 2) " " i)
   (gfz3 i) (? (i < 2) '(1 2) i)
   _ (rng-set (wheel 20260729))                    ; fixed seed: the two runs must generate the SAME forms
   (gnth l i) (? (i < 1) (cap l) (gnth (cup l) (i - 1)))
   (pick l) (gnth l (rand (tally l)))
   ; ⚠ HOISTED (see the header) -- never a nested single-binding `:`
   (uptogo i acc) (? (i < 0) acc (uptogo (i - 1) (link i acc)))
   (upto n) (uptogo (n - 1) ())
   ; the leaf pools. `bleaf` is the hostile one: k/acc are frame params where they are
   ; bound and FREE elsewhere (a free read is a helpless scare, which rides to stderr and
   ; answers the zero point -- stdout stays comparable), and the rest are the values the
   ; integer lanes have no register shape for.
   ileaf (\ _ (pick '(x y 0 1 2 3 -1 7)))
   bleaf (\ _ (pick '(k acc 0 1 2 " " "Q" 'nm '(1 2) 1.5 "" 3)))
   fleaf (\ _ (pick '(x 0.5 1.5 2.0 3 1)))
   iop   (\ _ (pick '(+ - * < > = // %)))
   fop   (\ _ (pick '(+ - *)))
   gfn   (\ _ (pick '(gfz0 gfz1 gfz2 gfz3)))
   ; a small expression of depth d over a leaf generator and an operator generator
   (xpr d lf op) (? (d < 1) (lf 0)
                    (: r (rand 4)
                       (? (r = 0) (lf 0)
                          (r = 1) `((op 0) (xpr (d - 1) lf op) (xpr (d - 1) lf op))
                          (r = 2) `('? `('< (xpr (d - 1) lf op) 3) (xpr (d - 1) lf op) (xpr (d - 1) lf op))
                          `('+ (xpr (d - 1) lf op) (lf 0)))))
   ; thirteen shapes, one per lane the hook offers. the lane each one lands in was PROBED
   ; (((from 'glaze) 'loopinfo) &c), not assumed -- 9 reaches calloutinfo with every earlier lane
   ; declining, 10 reaches calloutinfo-frame, 11 reaches loopinfo.
   (shape i)
    (: r (% i 13)
       (? (r = 0) `('\ 'x (xpr 2 ileaf iop))                                            ; leaf
          (r = 1) `('\ 'x 'y (xpr 2 ileaf iop))                                         ; n-ary leaf
          (r = 2) `('\ 'x (xpr 2 fleaf fop))                                            ; float-RESULT leaf (qualfr)
          (r = 3) `('\ 'n `(': `('go 'k 'acc) `('? '(< k 1) 'acc `('go '(- k 1) (xpr 2 bleaf iop))) '(go n 0)))
          (r = 4) `('\ 'n `(': `('go 'k 'acc) `('? '(< k 1) 'acc `('go '(- k 1) (xpr 2 bleaf iop))) `('go 'n (bleaf 0))))
          (r = 5) `('\ 'n `(': `('go 'k) `('? '(< k 1) (bleaf 0) `('+ (bleaf 0) `('go '(- k 1)))) '(go n)))
          (r = 6) `('\ 'n `(': `('evn 'k) `('? '(< k 1) (bleaf 0) `('odd '(- k 1)))     ; a MUTUALLY-recursive pair: the group lane proper
                                `('odd 'k) `('? '(< k 1) (bleaf 0) `('+ (bleaf 0) `('evn '(- k 1))))
                                '(evn n)))
          (r = 7) `('\ 'n `(': `('go 'i 'a 'b) `('? '(< i n) `('go '(+ i 1) 'b (xpr 1 bleaf iop)) 'a) '(go 0 0 1)))   ; 3-var loop
          (r = 8) `('\ 'n `(': `('go 'k 'acc) `('? '(< k 1) 'acc `('go '(- k 1) `('+ 'acc (xpr 1 bleaf iop)))) '(go n 0)))
          (r = 9) `('\ 'n `(`(': `('go 'i 'a) `('? '(< i n) `('go '(+ i 1) `('+ 'a `((gfn 0) 'i))) 'a)) 0 0))   ; CALL-OUT over a resolvable global
          (r = 10) `('\ 'f 'n `(`(': `('go 'i 'a) `('? '(< i n) `('go '(+ i 1) `('+ 'a '(f i))) 'a)) 0 0))      ; CALL-OUT with F a frame param
          (r = 11) `('\ 'n `(`(': `('go 'i 'a) `('? '(< i n) `('go '(+ i 1) `('+ 'a (xpr 1 bleaf iop))) 'a)) 0 0))   ; counted loop (loopinfo)
          `('\ 'x `('? '(< x 1) (bleaf 0) (xpr 2 bleaf iop)))))                         ; branchy fall-through
   ; apply FULLY, per shape: two ints for the n-ary leaf, (F n) for the frame call-out, else one
   (try f i a) (: r (% i 13) (? (r = 1) (f a a) (r = 10) (f gfz0 a) (f a)))
   n0 (peep fires () 0)
   (run i) (: s (shape i)  f (ev s)
              (puts ((show i) + " " + (show s) + " => "
                     + (show (try f i 0)) + " | " + (show (try f i 1)) + " | " + (show (try f i 4)) + "\n")))
   _ (each (upto 3000) run)
   _ (puts ("fires=" + (show ((peep fires () 0) - n0)) + "\n")))
