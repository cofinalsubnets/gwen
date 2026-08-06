; missing -- a nom not in the book (the global table). reading one is a
; CALL FOR HELP: with a global help installed the read raises
; (help 'missing nom) and the help's result becomes the value at the site;
; helpless it reads the ZERO POINT -- the mint at serial 0: nameless, false,
; and a unit (absence absorbs application). the read happens where the code says it does: a closure
; captures its free globals at creation, so a missing read in a lambda body fires
; the condition at the define, not the call.

; welp is the FLOOR HANDLER: install it and a missing read answers the zero point with
; no warning. (Truly helpless -- no help at all -- it still answers the zero point, but
; SURFACES ";; missing nom" on err to catch the silent const-1 trap; we don't trigger that.)
(hear welp)
(assert
 (id? (:) an-never-defined)                      ; the floor handler: the zero point
 !an-never-defined                               ; ... so a missing name is false
 !(nom? an-never-defined))                       ; ... the zero point (absence): NOT a nom -- anonymous and empty
(hear 0)

; a name BOUND to 0 is in the book -- not missing, no condition: under an
; armed help the read still answers its own 0 (presence beats value; the
; value no longer tells bound from missing apart -- the condition lane does)
(: an-zero 0)
(hear (\ a b 'BOOM))
(assert (0 = an-zero) !(id? 'BOOM an-zero) !(id? (:) an-zero))
(hear 0)

; the help hears (help 'missing nom) and its result is the value at the site
(hear (\ a b (? (id? a 'missing) `('missing-was b) 0)))
(assert (: r an-ghost
           (&& (two? r) (id? 'missing-was (cap r)) (id? 'an-ghost (caup r)))))
(hear 0)

; the read of a lambda's free global happens at closure creation (the runtime
; compiler captures it) or as a live site in the body (the bootstrap lane);
; with the help installed across both moments, both lanes answer the help.
; NOTE the discipline: love0 runs this corpus TWICE in one scope, so a name
; asserted missing must NEVER be defined later -- the second pass would fold it.
(hear (\ a b (? (id? a 'missing) 42 0)))
(: (an-g _) an-phantom)
(assert (42 = (an-g 0)))
(hear 0)

; ONE SCOPE -- a `:` binds every name over the whole form. reading a binding
; before its value is pinned is the SAME condition, with the binding site's
; nom (a named point) as payload: a call for help, the zero point helpless. the two old silent
; lanes -- a direct forward read escaping to the book, a boxed pre-fill read
; of 0 -- are gone: one law at every depth, local or book.
(: an-outer 99)
(assert
 (id? (:) (: a an-outer an-outer 2 a))            ; no silent capture of the book's an-outer
 (id? (:) (: a an-local an-local 2 a))            ; ... nor of nothing: the point helpless
 (2 = (: a an-local an-local 2 an-local))        ; the body reads the local, post-pin
 (id? (:) (: f (\ _ an-cell) v (f 0) an-cell 5 v))  ; the boxed lane reads the same point
 (5 = (: f (\ _ an-cell) v (f 0) an-cell 5 (f 0)))) ; ... and the cell, once pinned

; the help hears the cell miss too; the payload is the binding-site nom
(hear (\ a b (? (&& (id? a 'missing) (nom? b) ("an-fwd" = (string b))) 'cell-nom 0)))
(assert (id? 'cell-nom (: q an-fwd an-fwd 2 q)))
(hear 0)

(hear (\ a b (? (id? a 'missing) 'fired 0)))
(assert (id? (:) ()) (id? (?) ()) (id? (\) ()))
(hear 0)

; the conditioned read itself is the COMPILER's (boxfix's letrec cells) and its nom
; is mopped at birth, so there is no user-facing call to assert here. what a reader
; wants from a tablet is presence, and peep answers that totally: a fresh (mint 0)
; is unique, so no stored value can pose as it.
(assert (: t #(1 0) s (mint 0)
           (&& !(id? s (peep t 1 s))               ; a pinned 0 is PRESENT, not absent
               (id? s (peep t 9 s))                ; an absent key is absent
               (id? (:) (peep t 9 ())))))          ; ... and the caller may still name absence ()
