; --- differential oracle: ev's reduction vs a reference normalizer ---
; The companion to proof/rocq/spec.v's reduction layer (shift/subst/step). spec.v proves
; that calculus consistent; this checks the BINARY against an INDEPENDENT reference
; implementation of the same de Bruijn reduction, over thousands of random terms --
; the conformance witness the proof alone can't give. ("the math is sound" + "the
; binary computes the math" = not slop.)
;
; The reference is a direct transcription of spec.v's shift/subst, evaluated under
; love's OWN strategy: N-ARY closures (nested lambdas collapse to one arity-n closure)
; with CBV, weak (no reduction under a binder) and SATURATING (a beta step fires only
; when all n args are present; an under-applied closure is held unreduced -- exactly
; as ev shows ((B A) stays (B A) until the 2nd arg lands).
;
; Terms are random CLOSED AFFINE lambda terms (each bound var used <=1 => strongly
; normalizing, so both ev and the reference terminate). Comparison is EXTENSIONAL:
; both results are applied to distinct marker symbols (a symbol applied is const-1,
; so no church-numeral blowup), grounding away the only legitimate difference -- love
; holding a partial application lazily vs the reference materializing the equal
; closure. This is representation-independent and robust at every term size; a
; deliberately wrong subst index (a mutation test) drops it well below 100%, so it
; discriminates real reduction bugs. The free-variable SHIFT path of subst is
; unreachable here (weak CBV on closed terms only ever fires a closed function, so
; free vars are never renumbered) -- that arithmetic is covered by spec.v, not ev.
(: ; de Bruijn terms as tagged chains: (0 n) Var | (1 b) Lam | (2 f a) App
 (dvar n) `(0 n)   (dlam b) `(1 b)   (dapp f a) `(2 f a)
 (dtag t) (cap t)   (dn t) (caup t)   (dbody t) (caup t)
 (dfun t) (caup t)  (darg t) (cauup t)

 ; shift d c t -- bump free vars (index >= c) by d   (spec.v: shift)
 (tshift d c t)
   (? (= (dtag t) 0) (: k (dn t) (? (< k c) (dvar k) (dvar (+ k d))))
      (= (dtag t) 1) (dlam (tshift d (+ c 1) (dbody t)))
      (dapp (tshift d c (dfun t)) (tshift d c (darg t))))
 ; subst j s t -- replace var j by s, renumber survivors   (spec.v: subst)
 (tsubst j s t)
   (? (= (dtag t) 0) (: k (dn t) (? (= k j) s (? (< k j) (dvar k) (dvar (- k 1)))))
      (= (dtag t) 1) (dlam (tsubst (+ j 1) (tshift 1 0 s) (dbody t)))
      (dapp (tsubst j s (dfun t)) (tsubst j s (darg t))))

 ; CBV / n-ary / weak / saturating evaluation -- love's strategy
 (darity t) (? (= (dtag t) 1) (+ 1 (darity (dbody t))) 0)
 (rebuild t args) (? !args t (rebuild (dapp t (cap args)) (cup args)))
 (apply-n t args n) (? (< n 1) t (apply-n (tsubst 0 (cap args) (dbody t)) (cup args) (- n 1)))
 (evs fuel t args)
   (? (< fuel 1) (rebuild t args)
      (= (dtag t) 2) (: a (evs (- fuel 1) (darg t) 0) (evs (- fuel 1) (dfun t) (link a args)))
      (= (dtag t) 1)
        (: n (darity t) k (tally args)
           (? (< k 1) t
              (< k n) (rebuild t args)
              (evs (- fuel 1) (apply-n t (take n args) n) (drop n args))))
      (rebuild t args))
 (teval t) (evs 4000 t 0)

 ; render de Bruijn -> love \-expression (binder names = de Bruijn LEVELS, capture-free)
 lam-sym (cap '(\ x))
 (vname i) (intern (+ "v" (show i)))
 (render d t)
   (? (= (dtag t) 0) (vname (- (- d 1) (dn t)))
      (= (dtag t) 1) `(lam-sym (vname d) (render (+ d 1) (dbody t)))
      `((render d (dfun t)) (render d (darg t))))

 ; random CLOSED AFFINE term: partition splits a scope so f/a never share a var
 (partition sc st)
   (? !sc `(0 0 st)
      (: r (random st 2) rest (partition (cup sc) (cup r))
         (? (= (cap r) 0)
            `((link (cap sc) (cap rest)) (caup rest) (cauup rest))
            `((cap rest) (link (cap sc) (caup rest)) (cauup rest)))))
 tI (dlam (dvar 0))
 (gen g sc st)
   (: r (random st 10) pick (cap r) st1 (cup r)
      (? (< g 1) (? !sc `(tI 0 st1) `((dvar (cap sc)) (cup sc) st1))
         (&& !!sc (< pick 4)) `((dvar (cap sc)) (cup sc) st1)
         (< pick 7)
           (: bsc (link 0 (map (+ 1) sc))
              res (gen (- g 1) bsc st1) b (cap res) lo (caup res) st2 (cauup res)
              lo' (map (\ x (- x 1)) (filter (\ x (< 0 x)) lo))
              `((dlam b) lo' st2))
           (: pr (partition sc st1) sc1 (cap pr) sc2 (caup pr) st2 (cauup pr)
              rf (gen (- g 1) sc1 st2) f (cap rf) lof (caup rf) st3 (cauup rf)
              ra (gen (- g 1) (cat lof sc2) st3) a (cap ra) st4 (cauup ra)
              `((dapp f a) (caup ra) st4))))

 ; EXTENSIONAL agreement: apply both results to marker symbols, then compare
 (ground v) (v 'm1 'm2 'm3 'm4 'm5)
 (oracle-ok n sd g)
   (: (go n st good)
        (? (< n 1) good
           (: res (gen g 0 st) t (cap res) st' (cauup res)
              ok (= (ground (ev (render 0 t))) (ground (ev (render 0 (teval t)))))
              (go (- n 1) st' (+ good (? ok 1 0)))))
      (= n (go n (wheel sd) 0)))

 ; spec.v's reduction theorems, as binary twins (beta_id / beta_const / nested / eta)
 (assert
  (5 = ((\ x x) 5))  (1 = ((\ _ 1) 9))  (5 = ((\ x x) ((\ x x) 5)))
  !((\ x (cap x)) = cap)
  ; the fuzzed differential: every random closed affine term agrees with ev
  (oracle-ok 300 1 4) (oracle-ok 300 2 5) (oracle-ok 200 3 6)))
