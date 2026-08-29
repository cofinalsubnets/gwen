; fuzz -- the glaze's differential fuzz: closures built at random in the shapes the
; natjit creation hook recognizes, each applied, the answer printed. the gate
; (test/test.mk test_glazefuzz) runs this file twice against the same binary, plain and
; under LOVE_NO_GLAZE=1, and requires the two stdouts to match byte for byte -- the glaze
; may be faster, never different. the failure it hunts is not a crash but a plausible
; number: a lane that admits one shape too many emits no code for the operand it cannot
; hold and answers whatever the accumulator last held, so the leaf pools carry strings,
; noms, lists and gems the integer lanes must decline.
;
; every shape is applied to exactly its arity, so results are values: a partial's `show`
; differs between the runs by design (the native cell keeps the surface src, the
; bytecode twin its post-cprop body). the call-out and counted-loop lanes want the
; applied named-let `((: (go i a) ..) 0 a0)`; the un-applied form drops to the group
; lane -- probe ((from 'glaze) 'calloutinfo) &c before believing a shape covers a lane.
; the trailing `fires=` line is the proof of work: nonzero in the glazed run, zero in
; the other, or the diff is over nothing.

; the free name is the instrument (bleaf), and file mode's help quits on every scare:
; a missing read answers the zero point and the case runs on, everything else stops the file
(: fzhelp (heard ())
   _ (hear (\ a b (id? a 'missing ? () (fzhelp a b)))))

(: (gfz0 i) (i + 1)                                ; globals the call-out lane can resolve: two arithmetic,
   (gfz1 i) (i * 2)                                ; two handing back a value the integer lane cannot hold
   (gfz2 i) (i < 2 ? " " i)
   (gfz3 i) (i < 2 ? '(1 2) i)
   _ (rng-set (wheel 20260729))                    ; fixed seed: the two runs must generate the same forms
   (pick l) (l (rand #l))
   ; the leaf pools. bleaf is the hostile one: k/acc are frame params where they are bound and free
   ; elsewhere (a free read scares to stderr and answers the zero point, stdout stays comparable)
   ileaf (_ \ pick '(x y 0 1 2 3 -1 7))
   bleaf (_ \ pick '(k acc 0 1 2 " " "Q" 'nm '(1 2) 1.5 "" 3))
   fleaf (_ \ pick '(x 0.5 1.5 2.0 3 1))
   iop   (_ \ pick '(+ - * < > = // %))
   fop   (_ \ pick '(+ - *))
   gfn   (_ \ pick '(gfz0 gfz1 gfz2 gfz3))
   (xpr d lf op) (? (d < 1) (lf 0)                 ; an expression of depth d over a leaf and an operator generator
                    (rand 4 @ 0 (lf 0)
                              1 [(op 0) (xpr (d - 1) lf op) (xpr (d - 1) lf op)]
                              2 ['? ['< (xpr (d - 1) lf op) 3] (xpr (d - 1) lf op) (xpr (d - 1) lf op)]
                              ['+ (xpr (d - 1) lf op) (lf 0)]))
   ; thirteen shapes, one per lane the hook offers, each lane probed not assumed: 9 reaches
   ; calloutinfo with every earlier lane declining, 10 calloutinfo-frame, 11 loopinfo
   (shape i)
    (i % 13 @ 0 ['\ 'x (xpr 2 ileaf iop)]                                             ; leaf
               1 ['\ 'x 'y (xpr 2 ileaf iop)]                                          ; n-ary leaf
               2 ['\ 'x (xpr 2 fleaf fop)]                                             ; float-result leaf (qualfr)
               3 ['\ 'n [': ['go 'k 'acc] ['? '(< k 1) 'acc ['go '(- k 1) (xpr 2 bleaf iop)]] '(go n 0)]]
               4 ['\ 'n [': ['go 'k 'acc] ['? '(< k 1) 'acc ['go '(- k 1) (xpr 2 bleaf iop)]] ['go 'n (bleaf 0)]]]
               5 ['\ 'n [': ['go 'k] ['? '(< k 1) (bleaf 0) ['+ (bleaf 0) ['go '(- k 1)]]] '(go n)]]
               6 ['\ 'n [': ['evn 'k] ['? '(< k 1) (bleaf 0) ['odd '(- k 1)]]          ; a mutually recursive pair: the group lane proper
                            ['odd 'k] ['? '(< k 1) (bleaf 0) ['+ (bleaf 0) ['evn '(- k 1)]]]
                            '(evn n)]]
               7 ['\ 'n [': ['go 'i 'a 'b] ['? '(< i n) ['go '(+ i 1) 'b (xpr 1 bleaf iop)] 'a] '(go 0 0 1)]]        ; 3-var loop
               8 ['\ 'n [': ['go 'k 'acc] ['? '(< k 1) 'acc ['go '(- k 1) ['+ 'acc (xpr 1 bleaf iop)]]] '(go n 0)]]
               9 ['\ 'n [[': ['go 'i 'a] ['? '(< i n) ['go '(+ i 1) ['+ 'a [(gfn 0) 'i]]] 'a]] 0 0]]           ; call-out over a resolvable global
               10 ['\ 'f 'n [[': ['go 'i 'a] ['? '(< i n) ['go '(+ i 1) ['+ 'a '(f i)]] 'a]] 0 0]]             ; call-out with f a frame param
               11 ['\ 'n [[': ['go 'i 'a] ['? '(< i n) ['go '(+ i 1) ['+ 'a (xpr 1 bleaf iop)]] 'a]] 0 0]]          ; counted loop (loopinfo)
               ['\ 'x ['? '(< x 1) (bleaf 0) (xpr 2 bleaf iop)]])                                  ; branchy fall-through
   (try f i a) (i % 13 @ 1 (f a a) 10 (f gfz0 a) (f a))   ; apply fully: two ints for the n-ary leaf, (f n) for the frame call-out, else one
   n0 (peep fires () 0)
   (run i) (: s (shape i)  f (ev s)
              (puts (show i + " " + show s + " => " + show (try f i 0) + " | " + show (try f i 1) + " | " + show (try f i 4) + "\n")))
   _ (each ^3000 run)
   _ (puts ("fires=" + show (peep fires () 0 - n0) + "\n")))
