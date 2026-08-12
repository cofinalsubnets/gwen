; test/uuspllaw.l -- the SPLICE LICENSE, proved of the terms test/uusplgen.l
; GENERATED from doc/proto/spl.l (loaded just before this by sort order; the
; stack machine in test/uuhom.l, kernel in test/uu.l). the model pins these
; laws by ASSERT at one demo env; here each is a THEOREM forall states -- and
; the flip pair is the reason the observation carries a TRACE: substitution
; under an effectful arg read twice keeps the VALUE equal and doubles the
; trace, the by-name breach R alone cannot convict. the license ev.l's inliner
; would ship under, stated before any inliner exists. sl-cross is the
; cross-machine theorem: the stack machine's y+1 under the binding splice
; meets the register machine's de5bck (test/uuhomgen.l) at the same value,
; forall states of BOTH machines -- doc/hom.md's "one design, two machines"
; as one law. exports to Rocq + Lean with the corpus.
; guard: substrate + generated terms must land in GLOB first; fail by name if not.
(assert (member? 'vof (names ())) (two? (peep GLOB 'srun 0)) (two? (peep GLOB 'sp0call 0)))

; --- the license: the splice observes as the call, value AND trace, forall states ---
(assert
 (defq 'sl-lic-0
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (stos (srun sp0bind A (smk S 0 L E T 0)))
                 (stos (srun sp0call A (smk S 0 L E T 0)))))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (stos (srun sp0bind A (smk S 0 L E T 0))))))))))
 (defq 'sl-lic-1
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (stos (srun sp1bind A (smk S 0 L E T 0)))
                 (stos (srun sp1call A (smk S 0 L E T 0)))))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (stos (srun sp1bind A (smk S 0 L E T 0))))))))))
 (defq 'sl-lic-1t
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strn (srun sp1bind A (smk S 0 L E T 0)))
                 (strn (srun sp1call A (smk S 0 L E T 0)))))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath 1)))))))
 (defq 'sl-lic-1e
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strat 0 (srun sp1bind A (smk S 0 L E T 0))) (A 0)))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (A 0)))))))))

; --- order: the arg's effect precedes the body's, call and splice alike ---
(assert
 (defq 'sl-ord-bind
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strat 0 (srun sp3bind A (smk S 0 L E T 0))) (A 0)))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (A 0))))))))
 (defq 'sl-ord-bind2
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strat 1 (srun sp3bind A (smk S 0 L E T 0))) 5))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath 5)))))))
 (defq 'sl-ord-call
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strat 0 (srun sp3call A (smk S 0 L E T 0))) (A 0)))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (A 0)))))))))

; --- the flip: substitution under an effectful arg read twice -- the VALUE
; --- stays equal (R cannot convict) and the TRACE doubles (the trace can) ---
(assert
 (defq 'sl-flip-value
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (stos (srun sp2subst A (smk S 0 L E T 0)))
                 (stos (srun sp2bind A (smk S 0 L E T 0)))))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (stos (srun sp2bind A (smk S 0 L E T 0))))))))))
 (defq 'sl-flip-bind
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strn (srun sp2bind A (smk S 0 L E T 0))) 1))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath 1)))))))
 (defq 'sl-flip-subst
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strn (srun sp2subst A (smk S 0 L E T 0))) 2))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath 2))))))))

; --- the substitution license, the pure face: an EFFECT-FREE arg substitutes --
; --- value and trace as the call (the lane ev.l's cprop ships for pure args;
; --- the impure args take the binding splice, sl-lic) ---
(assert
 (defq 'sl-sub-0
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (stos (srun sp0subst A (smk S 0 L E T 0)))
                 (stos (srun sp0call A (smk S 0 L E T 0)))))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (stos (srun sp0subst A (smk S 0 L E T 0))))))))))
 (defq 'sl-sub-0t
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strn (srun sp0subst A (smk S 0 L E T 0)))
                 (strn (srun sp0call A (smk S 0 L E T 0)))))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath (strn (srun sp0subst A (smk S 0 L E T 0)))))))))))

; --- the cross-machine theorem: one source (y + 1), two machines, one value --
; --- the stack machine's binding splice meets the register machine's de5bck
; --- forall states of both ---
(assert
 (defq 'sl-cross
   '(pi v nat (pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (pi R hst (pi F hst (pi K hst
        (paths nat
          (stos (srun sp0bind (hupd 0 v A) (smk S 0 L E T 0)))
          (mrg (drun de5bck (mmk (hupd 9 v R) F K 0)) 0)))))))))))
   '(lam v (lam A (lam S (lam L (lam E (lam T (lam R (lam F (lam K
      (idpath (stos (srun sp0bind (hupd 0 v A) (smk S 0 L E T 0)))))))))))))))
