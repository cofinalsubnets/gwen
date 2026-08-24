; inl.l -- standalone spec of the KNOWN-LAMBDA INLINER (ev.l's cprop beta-reduction lane).
;
; A curried higher-order recurrence like the closure bench's ((twice (adder i)) i) -- where
; twice/adder are statically-known non-recursive lambdas -- beta-reduces at compile time to the
; first-order arithmetic (+ (+ i i) i) = 3i. That collapse (a) drops the per-iteration closure
; allocation (~2.5x interpreted) and (b) leaves a first-order integer loop the glaze takes NATIVE
; (the bench's dehof pass + autogroup; closure 5.25 -> 0.07 ms/it).
;
; SOUNDNESS, the subtle part: `=` on closures is NOT bridged by beta (the spec) -- a beta-produced
; closure (\ x (+ x 5)) is not = the capturing closure (\ x (+ x n))[n=5]. So a beta result that is
; itself a lambda must never ESCAPE to a value position where =/id?/show could observe it. We thread
; an operator-position flag `op`: commit a lambda beta-result only when CONSUMED (op=1, an enclosing
; application's head); at a VALUE position (op=0) REVERT to the un-inlined source. So ((twice (adder
; i)) i) -> 3i (a value, fine) but ((\ n ..) 5) stays a capturing closure (it escapes).
;
; This spec is the SUBSTITUTION lane -- args must all be pure (`allp`), or the inline declines.
; ev.l's cprop carries a second lane this spec predates: the BINDING SPLICE (bspl). a saturated
; call with impure args substitutes the pure ones, substitutes the LAST effectful one too when
; its param is read exactly once, unconditionally, in an otherwise-pure body (the call then
; vanishes with no let), and binds each remaining effectful arg ONCE to a fresh nom, in call
; order, ahead of the body. Its license is a theorem set, not a heuristic: test/uuspllaw.l
; proves splice == call as value AND effect trace forall machine states (sl-lic/sl-ord for the
; bind, sl-sub/-1 for the two substitution faces), and the flip -- substituting an effectful
; arg read twice keeps the value equal and doubles the trace (sl-flip), which is why a
; read-many effectful form is bound, never substituted. Same escape law: a lambda body at a
; value position declines. Only the LAST effectful arg may substitute: the moved effect runs
; inside the body, after every binding -- any effectful arg after it would be overtaken
; (sl-ord); and a read under \ or ? runs zero or many times, so those decline too.
;
; lp/lambp/w/macros are MOPPED post-egg, so this spec re-supplies them locally; inside feel/cprop the
; real ones are live. Mirrors love/ev.l's feel block. `make` does not run this; it is the why.

(: macros (tablet 0)                                     ; the real macro table lives in the compiler
   (lambp x) (? (two? x) (&& (id? '\ (cap x)) (two? (cuup x))))
   (lp x) (: a (cup x) (? (atom? (cup a)) (link 0 (cap a)) (link (init a) (last a))))   ; (\ p.. body) -> (params . body)
   (w v x bnd) (? (nom? x) (&& (id? x v) (! (member? v bnd)))   ; does v occur free in x? (\ shadows)
                  (atom? x) 0
                  (id? (cap x) '\) (? (atom? (cuup x)) 0 (: r (lp x) (w v (last x) (cat (cap r) bnd))))
                  (any (\ e (w v e bnd)) x))
   opval '(+ - * // % = < <= > >= ! nil?)                ; the pure-op whitelist (ev.l carries the nifs)
   (inop k s) (member? k s)

   ; --- inlinability gates ---
   (smpl? x) (? (atom? x) 1 (id? (cap x) ':) 0           ; bsub-safe: no : / macro head
                (id? (cap x) '\) (? (atom? (cuup x)) 1 (smpl? (last x)))
                (nom? (cap x)) (? (peep macros (cap x) 0) 0 (all smpl? x))
                (all smpl? x))
   (fvs x bs) (? (nom? x) (? (|| (id? x ()) (member? x bs)) () (list x))
                 (atom? x) ()
                 (id? (cap x) '\) (? (atom? (cuup x)) () (: r (lp x) (fvs (last x) (cat (cap r) bs))))
                 (foldl (\ a e (cat a (fvs e bs))) () x))
   (closedl? lam) (all (\ v (! (! (book v)))) (fvs lam ()))    ; free vars all global
   (pbody? x bnd) (? (atom? x) 1                         ; PURE body: every call head a pure op or a param
                     (id? (cap x) '?) (all (\ e (pbody? e bnd)) (cup x))
                     (id? (cap x) '\) (? (atom? (cuup x)) 1 (: r (lp x) (pbody? (last x) (cat (cap r) bnd))))
                     (nom? (cap x)) (&& (|| (inop (cap x) opval) (member? (cap x) bnd)) (all (\ e (pbody? e bnd)) (cup x)))
                     (all (\ e (pbody? e bnd)) x))
   (inlinable? nm lam) (&& (lambp lam) (smpl? lam) (closedl? lam) (pbody? lam ()) (! (w nm lam 0)))

   ; --- capture-avoiding substitution + beta ---
   (rmkeys ps env) (filter (\ p (! (member? (cap p) ps))) env)
   (bsub env x) (? (nom? x) (: p (assoc x env) (? (two? p) (cup p) x))
                   (atom? x) x
                   (id? (cap x) '\) (? (atom? (cuup x)) x
                      (: r (lp x) ps (cap r) ren (map (\ p (link p (nom 0))) ps)   ; rename binders fresh -> no capture
                         (link '\ (cat (map cup ren) (list (bsub (cat ren (rmkeys ps env)) (last x)))))))
                   (map (\ e (bsub env e)) x))
   (beta lam as) (: r (lp lam) ps (cap r) np (tally ps)
                    (? (< (tally as) np) (link lam as)
                       (: bd2 (bsub (zip ps (take np as)) (last lam)) rest (drop np as)
                          (? (two? rest) (link bd2 rest) bd2))))

   ; --- purity of an arg (effect-free -> duplicable/droppable when inlined) ---
   (allp as se) (? (! (two? as)) 1 (? (purep (cap as) se) (allp (cup as) se) 0))
   (purep a se) (? (atom? a) 1 (id? (cap a) '\) 1
                   (id? (cap a) '?) (allp (cup a) se)
                   (nom? (cap a)) (: e (assoc (cap a) se)
                      (? (&& (two? e) (lambp (cup e))) (allp (cup a) se) (inop (cap a) opval) (allp (cup a) se) 0))
                   0)

   ; --- cprop: thread a known-lambda env `se` + the op flag; beta-reduce known heads, fuel-bounded ---
   (resolve h se) (: e (assoc h se) (? (two? e) (cup e) h))
   (cprop x se fl op) (?
     (atom? x) (? (nom? x) (resolve x se) x)
     (id? (cap x) '\) (? (atom? (cuup x)) x (: r (lp x) (link '\ (cat (cap r) (list (cprop (last x) se fl 0))))))
     (: as (map (\ e (cprop e se fl 0)) (cup x))          ; ARGS are values -> op 0
        h (cap x)
        hr (? (nom? h) (resolve h se) (cprop h se fl 1))   ; HEAD is consumed -> op 1
        (? (&& (< 0 fl) (lambp hr) (allp as se))
             (: red (cprop (beta hr as) se (- fl 1) op)
                (? (|| op (! (lambp red))) red (link h as)))   ; escaping lambda -> REVERT (keep the closure)
           (link h as))))
   (inline x) (cprop x () 64 0)

   ; --- demonstration: the closure-bench HOFs collapse; an escaping closure is preserved ---
   se [(link 'twice '(\ f (\ x (f (f x))))) (link 'adder '(\ i (\ x (+ x i))))]
   (cp x) (cprop x se 64 0)
 (: _ (assert (= '(+ (+ i i) i) (cp '((twice (adder i)) i))))   ; HOFs collapse to 3i (a value)
    _ (assert (= '((\ n (\ x (+ x n))) 5) (cp '((\ n (\ x (+ x n))) 5))))   ; escapes -> NOT beta'd (capturing closure kept)
    (say out "inl.l: known-lambda inliner spec ok\n")))
