; tools/clay-g2.l -- G2, doc/clay.md's CONVERSION gate: does the C a generator lays
; mean what the C it replaces meant?
;
; parse two whole translation units and compare the named top-level DEFINITIONS as
; trees. parsing whole is what makes it exact: both sides meet the same cpp, the same
; typedefs and the same macro expansions, so a difference that survives is a
; difference in what the C MEANS and never in how it was spelled. that is the one
; comparison a byte diff cannot make -- the generator says `A(l)`, prints `A(l)`, and
; cpp turns both into the same tree whatever the macro happens to be today.
;
; ⚠ it runs ONCE, at a conversion, against the file the region came out of -- the
; original is deleted afterwards and there is nothing left to compare to. what stands
; afterwards is the regeneration-drift cmp in test_clay, and the corpus.
;
; run inside a woken mooncc image, which is where the front end lives:
;   love --wake mooncc.image -l tools/clay-g2.l \
;     -e '(clay-g2 ".love-orig.c" "love.c" (\ ("salpha" "shash")))'
; answers 1 and prints a report; quits 1 on any difference.
(:
   nl (string 10)
   (g2-el l i) (? (i < 1) <l (g2-el >l (i - 1)))
   (g2-say s) (puts (s + nl))

   ; the front end without cc-parse's udie: a refusal has to come back as a value, so
   ; that "the file did not parse" reads differently from "the trees differ".
   (g2-front src)
    (: r (uread src)
       (? (! (two? r)) 'noread
          (: lx (clex ((deftext `((archdef 'x64))) + <>r))
             (? (! (okv? lx)) 'lexfail
                (: pp (cpp <>lx (\ n s (incload () () (cdir src) n s)) src)
                   (? (! (okv? pp)) 'cppfail
                      (: ps (cparse-t <>pp 'x64)
                         (? (okv? ps) (g2-el (caup ps) 1) 'parsefail))))))))

   ; the DEFINITION, never the prototype that announces it: the two wear one name, and
   ; comparing the wrong one would pass on a signature while the body drifted.
   (g2-find tops nm)
    (: (go l) (? (! (two? l)) ()
                 (: e <l (? (&& (id? <e 'fn) (: w (g2-el e 1) (&& (string? w) (w = nm))))
                            `(1 e) (go >l))))
       (go tops))

   (clay-g2 old new names)
    (: a (g2-front old)
       b (g2-front new)
       _ (? (two? a) 0 (: _ (g2-say ("g2: " + old + ": " + (show a))) (quit 1)))
       _ (? (two? b) 0 (: _ (g2-say ("g2: " + new + ": " + (show b))) (quit 1)))
       (one nm)
        (: fa (g2-find a nm) fb (g2-find b nm)
           (? (! (two? fa)) (: _ (g2-say ("g2: MISSING from " + old + ": " + nm)) 0)
              (! (two? fb)) (: _ (g2-say ("g2: MISSING from " + new + ": " + nm)) 0)
              ((g2-el fa 1) = (g2-el fb 1)) 1
              (: _ (g2-say ("g2: DIFFERS: " + nm)) 0)))
       oks (foldl (+) 0 (map one names))
       _ (g2-say ("g2: " + (show oks) + " / " + (show (tally names))
                  + " definitions identical as trees"))
       ; ⚠ A VACUOUS PASS IS A FAIL: a front end that answered nothing reports 0/0,
       ; and 0/0 is not agreement -- it is a gate that measured nothing.
       (? (&& (oks = (tally names)) (0 < oks))
          (: _ (g2-say "g2: pass") 1)
          (: _ (g2-say "g2: FAIL") (quit 1)))))
