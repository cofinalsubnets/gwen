; layer-2 spike: a standalone online PARTIAL EVALUATOR (`mix`) -- the first Futamura projection in
; miniature, done two ways. mix walks a source expr under an env that splits each variable STATIC (a
; known value) or DYNAMIC (a residual variable), folds the static parts, and RESIDUALIZES the dynamic
; ones into fresh source. specialize the recursive `power` to a static exponent and the recursion
; evaporates into a straight-line product; specialize the tiny INTERPRETER `ev1` to a static program
; and the dispatch+navigation evaporate into the program's straight-line arithmetic -- the 1st
; Futamura projection proper (interp + program -> compiled residual).
;
; this is the all-static folding the egg already does (feels/hgen in love/ev.l), generalized to PARTIAL
; staticness -- the difference between constant-folding and partial evaluation. it is also where the
; interpreter-specialization PRIZE actually lives: doc/misc/proto/interp-spec.l measures a 6.5x gap between
; interpreting a program and running its straight-line residual, and shows the IN-COMPILER unfold
; (love/ev.l cprop) captures ~none of it. the offline mix below DOES: it specializes ev1 to P down to
; the exact `hand` residual, which then runs ~8x faster than interpreting P (measured, host). the
; companion is doc/misc/proto/kinds.l: kinds.l is the binding-time ANALYSIS half (a static<dyn fixpoint
; reuses that same engine), mix is the SPECIALIZATION half it would feed.
;
; over a naive unroller it adds the things that make it a real specializer:
;   * a GENERAL definition table (functions read from `defs`, not inlined by bespoke code)
;   * STATIC DATA folding: quote, and the structural ops (cap/cup/caup/cuup/id?) folded over a static
;     AST, so an interpreter can be driven THROUGH a known program; a MULTI-CLAUSE cond whose static
;     tests pick a branch (this is what collapses the interpreter's dispatch)
;   * a TERMINATION guard: a call whose static signature is already in progress is residualized
;     instead of unfolded forever -- static-driven recursion unrolls, dynamic-driven stops clean
;   * an apcap FUEL bound (a one-shot call-cc escape; the real apcap raises (scare 'apcap k))
;
; NB the house nil-trap: end-of-list is a SPINE test (two?), never (! l) -- `!` reads the NET,
; and a list of falsy cells like '(0 0) nets to nothing, so (! '(0 0)) is TRUE. and pe/pcondm/
; pe-call are MUTUALLY RECURSIVE, so they share one body-less `:` letrec (boxfix resolves the
; forward edges); a plain sequence of top-level defines would freeze a forward `pe` as missing.

(:
 (nul? l) (! (two? l))                          ; end-of-list = spine test, not the net

 ; --- pe-values: (s v) static, (r c) residual source ---
 (stat v) ['s v]
 (dyn c)  ['r c]
 (stat? p) ('s = (cap p))
 (pval p)  (caup p)
 (qv v)    ['\ v]                               ; quote a static datum so re-eval/residual sees DATA, not a call
 (reify p) (: v (caup p) (? (&& (stat? p) (! (constellation? v))) (qv v) v))   ; non-number static -> quoted
 (statval? p v) (&& (stat? p) (v = (pval p)))

 ; --- little list helpers ---
 (inset? k s) (? (nul? s) 0 (? (k = (cap s)) 1 (inset? k (cup s))))
 (alls l)     (? (nul? l) 1 (? (cap l) (alls (cup l)) 0))
 (allstat apv)(alls (map stat? apv))
 (sig apv)    (map (\ p (? (stat? p) (pval p) '_)) apv)        ; static signature (dynamic slots -> _)
 (zipenv ps apv) (? (nul? ps) () (link [(cap ps) (cap apv)] (zipenv (cup ps) (cup apv))))

 ; --- env: name -> pe-value; an unbound name is a residual variable (dynamic) ---
 (look env k)
   (? (nul? env)            (dyn k)
      (k = (cap (cap env)))  (caup (cap env))
                             (look (cup env) k))

 ; --- the function definition table: (name params body). closed recursive source. ---
 defs [
  ['power '(n b) '(? (! n) 1 (* b (power (- n 1) b)))]         ; b^n
  ['down  '(b)   '(? (! b) 0 (down (- b 1)))]                  ; recursion driven by DYNAMIC b
  ['poly  '(x)   '(+ (* 3 (power 2 x)) (+ (* 2 x) 1))]         ; 3x^2 + 2x + 1, calls power
  ['ev1   '(nd x) '(? (id? (cap nd) 'lit) (caup nd)             ; a tiny AST interpreter: (lit n)(var)(add ..)(mul ..)
                      (id? (cap nd) 'var) x                     ;   nd STATIC (the program), x DYNAMIC (the input)
                      (id? (cap nd) 'add) (+ (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                      (id? (cap nd) 'mul) (* (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                      0)]]
 (deflook nm)      (deflook-go nm defs)
 (deflook-go nm l) (? (nul? l) () (? (nm = (cap (cap l))) (cap l) (deflook-go nm (cup l))))

 ; --- apcap fuel: charged per unfold; exhausted -> a one-shot escape (the real apcap scares) ---
 pe-fuel (tablet 0)
 (tick u) (: n (peep pe-fuel 'n 0)              ; u is a dummy: a 0-arg (tick) is a VALUE, not a fn
             (? (! n) ((peep pe-fuel 'esc 0) ['APCAP (peep pe-fuel 'cap 0)])
                      (pin pe-fuel 'n (- n 1))))

 ; --- primitives: arithmetic + structural AST navigation. fold when all args static (quoting each,
 ;     so a static LIST/SYM folds as DATA), else residualize (unit-aware on the binary * and +). ---
 (prim? h) (inset? h ['* '- '+ '! 'cap 'cup 'caap 'caup 'cuap 'cuup 'id?])
 (simp1 h a b)
   (? (&& (h = '*) (statval? a 1)) b
      (&& (h = '*) (statval? b 1)) a
      (&& (h = '+) (statval? a 0)) b
      (&& (h = '+) (statval? b 0)) a
                                  (dyn [h (reify a) (reify b)]))
 (pe-prim h apv)
   (? (allstat apv)     (stat (ev (link h (map qv (map pval apv)))))   ; quote args -> fold over data
      (2 = (tally apv)) (simp1 h (cap apv) (caup apv))                  ; binary: residualize, drop units
                        (dyn (link h (map reify apv))))

 ; --- cond (multi-clause): walk (t1 r1 t2 r2 .. else). a static test keeps/drops its clause (this is
 ;     what lets static recursion terminate AND collapses the interpreter's dispatch); the first
 ;     DYNAMIC test residualizes the whole tail back into source. ---
 (pcondm cs env seen)
   (? (nul? (cup cs)) (pe (cap cs) env seen)                           ; lone else
      (: cv (pe (cap cs) env seen)
         (? (! (stat? cv)) (dyn (link '? (resid-cl cv (caup cs) (cuup cs) env seen)))
            (! (pval cv))  (pcondm (cuup cs) env seen)                 ; static false -> drop clause
                           (pe (caup cs) env seen))))                  ; static true  -> take this result
 (resid-cl cv then rest env seen)
   (link (reify cv) (link (reify (pe then env seen)) (resid-tl rest env seen)))
 (resid-tl cs env seen)
   (? (nul? (cup cs)) [(reify (pe (cap cs) env seen))]                 ; trailing else
      (: cv (pe (cap cs) env seen)
         (? (! (stat? cv)) (resid-cl cv (caup cs) (cuup cs) env seen)
            (! (pval cv))  (resid-tl (cuup cs) env seen)
                           [(reify (pe (caup cs) env seen))])))        ; static true -> becomes the else

 ; --- a defined-function call: unfold, unless this static instance is already in progress ---
 (pe-call def apv seen)
   (: nm (cap def) ps (caup def) bd (caup (cup def))
      key (link nm (sig apv))
      (? (inset? key seen)  (dyn (link nm (map reify apv)))     ; no static progress -> residual call
         (: _ (tick 0)
            (pe bd (zipenv ps apv) (link key seen)))))

 ; --- the partial evaluator ---
 (pe x env seen)
   (? (constellation? x) (stat x)
      (nom? x)           (look env x)
      (atom? x)          (stat x)
      (: h (cap x) a (cup x)
         (? (&& (h = '\) (nul? (cup a))) (stat (cap a))               ; quote -> a static datum
            (h = '?)  (pcondm a env seen)
            (prim? h) (pe-prim h (map (\ e (pe e env seen)) a))
            (: d (deflook h)
               (? d (pe-call d (map (\ e (pe e env seen)) a) seen)
                    (dyn (link h (map reify (map (\ e (pe e env seen)) a)))))))))

 ; --- entry points ---
 (mixf expr env fuel)
   (call-cc (\ esc (: _ (pin pe-fuel 'n fuel) _ (pin pe-fuel 'cap fuel) _ (pin pe-fuel 'esc esc)
                      (pe expr env ()))))
 (mix expr env)       (mixf expr env (apcap 0))
 (envb nv) [['n (stat nv)] ['b (dyn 'b)]]                     ; n static, b dynamic

 ; --- residual source -> a compiled unary lambda, for the equivalence checks ---
 (resid expr env v) ((ev ['\ v (reify (mix expr env))]))      ; () outer: ev returns the closure

 ; ============================ self-checks ============================
 ; the first Futamura projection, take one: the residual of `power` specialized to a static exponent
 ; computes the SAME function as the original program -- straight-line, no recursion left.
 _ (assert ('(* b (* b b)) = (reify (mix '(power n b) (envb 3)))) "power n=3 unrolls")
 _ (assert (125 = ((resid '(power n b) (envb 3) 'b) 5))         "residual(power,3)(5) == 5^3")
 _ (assert (32  = ((resid '(power n b) (envb 5) 'b) 2))         "residual(power,5)(2) == 2^5")
 _ (assert ('b  = (reify (mix '(power n b) (envb 1))))          "power n=1 -> b (unit simp)")
 _ (assert (1   = (reify (mix '(power n b) (envb 0))))          "power n=0 -> 1")
 _ (assert (8   = (pval (mix '(power n b) [['n (stat 3)] ['b (stat 2)]]))) "both static folds")
 ; a nested call unrolls inside a larger expression:
 _ (assert (57  = ((resid '(poly x) [['x (dyn 'x)]] 'x) 4)) "residual(poly)(4) == 3*16+8+1")
 ; recursion driven by a DYNAMIC value cannot unroll -> the guard leaves it intact:
 _ (assert ('(? (! b) 0 (down (- b 1))) = (reify (mix '(down b) [['b (dyn 'b)]]))) "down stays residual")
 ; the apcap fuel bounds a huge static unfold instead of building a million-node residual:
 _ (assert ('APCAP = (cap (mixf '(power n b) (envb 1000) 8)))   "apcap guard fires")

 ; the first Futamura projection, take two -- the real one: specialize the INTERPRETER ev1 to a static
 ; program P (the Horner form of 2x^3+3x^2+5x+7). the dispatch and the AST navigation fold away; what
 ; is left is P's straight-line arithmetic -- the exact `hand` residual interp-spec.l hand-wrote, which
 ; runs ~8x faster than interpreting P (measured, host). this is the win the in-compiler unfold missed.
 P '(add (mul (add (mul (add (mul (lit 2) (var)) (lit 3)) (var)) (lit 5)) (var)) (lit 7))
 envP [['nd (stat P)] ['x (dyn 'x)]]
 _ (assert ('(+ (* (+ (* (+ (* 2 x) 3) x) 5) x) 7) = (reify (mix '(ev1 nd x) envP)))
           "ev1 specialized to P == the straight-line Horner residual")
 ; the residual computes the SAME function as the interpreter, across a spread of inputs:
 (ev1 nd x) (? (id? (cap nd) 'lit) (caup nd)
               (id? (cap nd) 'var) x
               (id? (cap nd) 'add) (+ (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
               (id? (cap nd) 'mul) (* (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
               0)
 ev1P (resid '(ev1 nd x) envP 'x)
 _ (assert (alls (map (\ v (= (ev1P v) (ev1 P v))) '(0 1 2 5 9 12 100))) "residual matches interp on every input")
 (. ";; doc/misc/proto/mix.l: partial evaluator self-checks pass (power unroll + interpreter specialization)\n"))
