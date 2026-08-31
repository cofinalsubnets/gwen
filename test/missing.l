; missing -- a nom not in the book (the global table). reading one is a
; call for help: with a global help installed the read raises
; (help 'missing nom) and the help's result becomes the value at the site;
; with nothing heard it reads the zero point -- the mint at serial 0: nameless, false,
; and a unit (absence absorbs application). the read happens where the code says it does: a closure
; captures its free globals at creation, so a missing read in a lambda body fires
; the condition at the define, not the call.

; welp is the floor handler: install it and a missing read answers the zero point with
; no warning -- which is why every read below sits in a welp window. Outside one the
; corpus hears test-strict (00-init.l) and a missing read ends the run, as in file mode.
(hear welp)
(assert
 (id? (:) an-never-defined)                      ; the floor handler: the zero point
 !an-never-defined                               ; ... so a missing name is false
 !(nom? an-never-defined))                       ; ... the zero point (absence): not a nom -- anonymous and empty
(hear 0)

; a name bound to 0 is in the book -- not missing, no condition: under an
; armed help the read still answers its own 0 (presence beats value; the
; value no longer tells bound from missing apart -- the condition lane does)
(: an-zero 0)
(hear (\ a b 'BOOM))
(assert (0 = an-zero) !(id? 'BOOM an-zero) !(id? (:) an-zero))
(hear 0)

; the help hears (help 'missing nom) and its result is the value at the site
(hear (\ a b (? (id? a 'missing) ['missing-was b] 0)))
(assert (: r an-ghost
           (&& (two? r) (id? 'missing-was (cap r)) (id? 'an-ghost (caup r)))))
(hear 0)

; the read of a lambda's free global happens at closure creation (the runtime
; compiler captures it) or as a live site in the body (the bootstrap lane);
; with the help installed across both moments, both lanes answer the help.
; note the discipline: love0 runs this corpus twice in one scope, so a name
; asserted missing must never be defined later -- the second pass would fold it.
(hear (\ a b (? (id? a 'missing) 42 0)))
(: (an-g _) an-phantom)
(assert (42 = (an-g 0)))
(hear 0)

; one form, two scopes -- the split law: eager reads see let*, closures see
; letrec*. a bare read before its name's pin is the outer binding (the book, a
; param, an enclosing let) -- so a binding may rebuild on what it shadows:
; (: s (trim s)) under a param s reads the param. a read under A \ is the letrec
; cell, filled by the binder's pin; run early it is the missing condition with
; the binding-site nom as payload: a call for help, the zero point where nothing hears.
(hear welp)               ; the bare lane is now a book read when nothing binds it -- welp keeps the corpus quiet
(: an-outer 99)
(assert
 (99 = (: a an-outer an-outer 2 a))              ; the bare pre-pin read is the book's an-outer
 (id? (:) (: a an-local an-local 2 a))            ; ... and of nothing, the zero point
 (2 = (: a an-local an-local 2 an-local))        ; the body reads the local, post-pin
 (id? (:) (: f (\ _ an-cell) v (f 0) an-cell 5 v))  ; the closure's read is the cell: the point pre-pin
 (5 = (: f (\ _ an-cell) v (f 0) an-cell 5 (f 0)))) ; ... and the value, once pinned
(hear 0)
; at every depth means nested too, and a shadowed name is the sharp case: the
; outer binding answers exactly the eager pre-pin reads -- bare in a def -- and
; never a closure's, which belong to the inner cell. cprop's splice lanes
; (beta/bspl) bsub-rename a :'s binders whole-form, letrec-wise, so an entangled
; : -- a bare pre-pin sibling read -- declines the splice (entg?) and the call
; runs un-inlined; the two shapes below are the ones that told the lanes apart.
(: an-outer2 42)
(assert
 (43 = (: q 1 (: an-outer2 (an-outer2 + 1) an-outer2)))      ; shadowed constant, nested: the outer 42 + 1
 (43 = ((\ p (: p (p + 1) p)) 42))                           ; ..and a shadowed lambda param, alike
 (id? (:) (: an-o3 99 (: f (\ _ an-o3) r (f 0) an-o3 42 r))) ; ..the closure's read is the cell, outer in scope
 (2 = (: b 1 b (b + 1) b))                                   ; a rebind within one chain still
 (43 = (: c 42 d (c + 1) d)))                                ;   reads the previous value, and folds

; the help hears the cell miss too; the payload is the binding-site nom
(hear (\ a b (? (&& (id? a 'missing) (nom? b) ("an-fwd" = (string b))) 'cell-nom 0)))
(assert (id? 'cell-nom (: g (\ _ an-fwd) q (g 0) an-fwd 2 q)))
(hear 0)

(hear (\ a b (? (id? a 'missing) 'fired 0)))
(assert (id? (:) ()) (id? (?) ()) (id? (\) ()))
(hear 0)

; the conditioned read itself is the compiler's (boxfix's letrec cells) and its nom
; is mopped at birth, so there is no user-facing call to assert here. what a reader
; wants from a tablet is presence, and peep answers that totally: a fresh (mint 0)
; is unique, so no stored value can pose as it.
(assert (: t {1 0} s (mint 0)
           (&& !(id? s (peep t 1 s))               ; a pinned 0 is present, not absent
               (id? s (peep t 9 s))                ; an absent key is absent
               (id? (:) (peep t 9 ())))))          ; ... and the caller may still name absence ()

(hear test-strict)   ; the empty slot is this file's subject; the corpus resumes strict
