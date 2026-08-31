; kanren -- the machinery lives in love/kanren.l, a registered module (unify + its
; ufail/ufail? contract and the macro-emitted var/s_plus/s_star/===/=/= stay global;
; walk/est/query/streams/rel ride the registry -- (from 'kanren) is the curried
; accessor). this file is the assert suite + the relational-algebra demo. one
; body-having `:` -- the accessor binds and `unord_eq` (an unordered list equality;
; search order is not specified) stay local, per the corpus law (and the registry
; retired the old re-sweep-mark poisoning hazard outright).
(: K (from 'kanren)
   est (K 'est)  query (K 'query)  rel (K 'rel)  sX (K 'sX)  slist (K 'slist)
   smap (K 'smap)  sno (K 'sno)  stake (K 'stake)  walk (K 'walk)  s_app (K 's_app)
   dif (K 'dif)  absent (K 'absent)  apt (K 'apt)  ifte (K 'ifte)  sole (K 'sole)
   naf (K 'naf)  tie (K 'tie)  untie (K 'untie)  scount (K 'scount)  empty_dict (K 'empty_dict)
   sun (K 'sun)
   (unord_eq a b) (&& (= (sat a) (sat b)) (all (flip member? a) b))

 _ (assert
 (unord_eq '(1 2 3) '(2 1 3))
 (unord_eq '(2 1 3) (stake 3 (sX 1 (sX 2 (sX 3 sno)))))
 (unord_eq '(6 4 2) (slist (smap (* 2) (sX 1 (sX 2 (sX 3 sno))))))
 ; a stream is a colist: () empty, (x >< s) mature, a thunk immature. so a finite
 ; realized one is just its list, and `sX`/`sno` are `link`/`()` under other names.
 (= '(1 2 3) (sX 1 (sX 2 (sX 3 sno))))
 (= 3 (scount (sX 1 (sX 2 (sX 3 sno)))))
 (: s1 (sX 1 (sX 2 (sX 3 sno)))
    s2 (smap (+ 3) s1)
  (unord_eq '(1 2 3 4 5 6) (slist (s_plus s1 s2))))
 ; the two disjunctions differ at a delay, which is the only place fairness can be
 ; spent or kept: s_plus swaps there (so the other side runs while this one thinks),
 ; s_app just waits for the left to finish.
 (: d (\ _ '(1 2))
  (&& (= '(3 4 1 2) (slist (s_plus d '(3 4))))
      (= '(1 2 3 4) (slist (s_app d '(3 4))))))
 ; --- unification + reification through `query` ---
 (= '((a 1)) (query 0 '(a) (\\\ a (est a 1))))
 (id? () (query 0 '(a) (\\\ a (&&& (est a 1) (est a 2)))))              ; 1 and 2 don't unify -> no answer
 (= '((a (1 2 (3 (4))) b (3 (4)) c 4))
    (query 0 '(a b c) (\\\ a b c (&&& (est a [1 2 b])
                                      (est b [3 [c]])
                                      (est c 4)))))
 (unord_eq '((a 7 b 8 c 9) (a 4 b 5 c 6) (a 1 b 2 c 3))
  (query 0 '(a b c) (\\\ a b c (||| (&&& (est a 1) (est b 2) (est c 3))
                                    (&&& (est a 4) (est b 5) (est c 6))
                                    (&&& (est a 7) (est b 8) (est c 9))))))
 ; --- an infinite stream, fairly enumerated via zz-delay ---
 (unord_eq '((a 0) (a 1) (a 2) (a 3) (a 4) (a 5) (a 6) (a 7) (a 8) (a 9))
  (query 10 '(a) (\\\ a
   (: (never s) ((||| (\ _ sno) (zz never)) s)
      (at_least n) (||| (est a n) (zz (at_least (+ n 1))))
    (||| never (est a 0) (at_least 1))))))
 ; --- a nom is data wherever it is not A VAR'S tail. a variable is (label . nom),
 ;     so the test is on the tail: a nom in any other position -- an atom under a
 ;     binder, one a relation minted -- is an ordinary term and unifies as itself.
 (: n (nom 0) v (var 'v)
    r (unify empty_dict ['app n v] ['app n 'y])
  (&& (nil? (ufail? r)) (= 'y (walk r v))))
 ; --- disequality: the constraint store rides beside `unify`, order-free ---
 (= '((a 2)) (query 0 '(a) (\\\ a (&&& (dif a 1) (est a 2)))))
 (id? () (query 0 '(a) (\\\ a (&&& (dif a 1) (est a 1)))))              ; the constraint fires later
 (id? () (query 0 '(a) (\\\ a (&&& (est a 1) (dif a 1)))))              ; ..and the other order too
 (= '((a 1)) (query 0 '(a) (\\\ a (&&& (est a 1) (dif a 2)))))          ; can never be equal -> nothing to watch
 (id? () (query 0 '(a b) (\\\ a b (&&& (dif a b) (est a b)))))         ; two holes told apart, then joined
 ; a disequality over a pair is one constraint, not two: it fails only when both halves land
 (: g (\ a b (&&& (dif [a b] [1 2]) (est a 1)))
  (&& (= '((a 1 b 3)) (query 0 '(a b) (\\\ a b (&&& (g a b) (est b 3)))))
      (id? () (query 0 '(a b) (\\\ a b (&&& (g a b) (est b 2)))))))
 ; --- absento: x occurs nowhere in t, now or ever ---
 (= '((t (1 2))) (query 0 '(t) (\\\ t (&&& (absent 'x t) (est t '(1 2))))))
 (id? () (query 0 '(t) (\\\ t (&&& (absent 'x t) (est t '(1 x))))))
 (id? () (query 0 '(t) (\\\ t (&&& (est t '(1 (x))) (absent 'x t)))))   ; and it looks deep
 ; --- a type constraint over any predicate: (apt nom? x) is symbolo ---
 (= '((a foo)) (query 0 '(a) (\\\ a (&&& (apt nom? a) (est a 'foo)))))
 (id? () (query 0 '(a) (\\\ a (&&& (apt nom? a) (est a 1)))))
 ; --- the extra-logical three: soft cut, commit, negation as failure ---
 (= '((a 1)) (query 0 '(a) (\\\ a (ifte (est a 1) (\ st (sun st)) (est a 2)))))   ; the test held: else is dropped
 (= '((a 2)) (query 0 '(a) (\\\ a (ifte (&&& (est a 1) (est a 3)) (\ st (sun st)) (est a 2)))))
 (= 1 (tally (query 0 '(a) (\\\ a (sole (||| (est a 1) (est a 2)))))))            ; commit to the first answer
 (= '((a 1)) (query 0 '(a) (\\\ a (&&& (est a 1) (naf (est a 2))))))              ; a=1, and 1 is not 2
 (id? () (query 0 '(a) (\\\ a (&&& (est a 1) (naf (est a 1))))))                  ; ..but it is 1
 ; --- ties: a binder is a love lambda, so alpha-equivalence is the language's ---
 (= (tie (\ x [x 'y])) (tie (\ z [z 'y])))                    ; alpha-equal binders are `=`, and so unify
 (nil? (= (tie (\ x [x 'y])) (tie (\ x [x 'w]))))
 (= '((b y)) (query 0 '(b) (\\\ b (est (tie (\ x [x b])) (tie (\ x [x 'y]))))))   ; unify under the binder
 (id? () (query 0 '(b) (\\\ b (&&& (est (tie (\ x [x b])) (tie (\ x [x 'y])))     ; ..and the store still holds
                                   (dif b 'y)))))
 ; the bound atom reifies as a1, a2, .. in first-sight order -- the answer reads
 (= '((b (tie a1 (a1 a1)))) (query 0 '(b) (\\\ b (est b (tie (\ x [x x]))))))
 ; --- coinductive: no occurs check, so a self-referential binding is a rational
 ;     tree, and walk* reifies it as a mu-type without looping (A = (prod elem A)) ---
 (= '(prod elem (mu A))
    (caup (cap (query 0 '(A) (\\\ A (est A ['prod 'elem A])))))))

 ; --- demo: relations read as algebra. `===` unifies, `=/=` separates; + is or, *
 ; is and. so a relation is a polynomial in unifications -- facts sum,
 ; sub-relations multiply; a cnf formula is literally a product of sums. ---
 _ (: ; a plant cladogram -- `diverges` is a sum of (a===ancestor * b===clade) facts:
   (diverges a b) (((a === 'spermatophytes) * (b === 'angiosperms))   ; seed plants -> flowering plants
                 + ((a === 'angiosperms)    * (b === 'monocots))      ; angiosperms split into
                 + ((a === 'angiosperms)    * (b === 'eudicots)))     ;   monocots | eudicots
   (descends a c) (rel (\\\ b (load ((diverges a b) * (diverges b c)))))   ; c descends from a, two splits down
   (boolean x) ((x === 0) + (x === 1))
   (assert
    (unord_eq '((k monocots) (k eudicots)) (query 0 '(k) (\\\ k (load (diverges 'angiosperms k)))))     ; daughter clades of angiosperms (OR)
    (unord_eq '((g monocots) (g eudicots)) (query 0 '(g) (\\\ g (load (descends 'spermatophytes g)))))  ; two splits below seed plants
    ; a clade is not its own daughter: =/= is the ring's separation, infix like ===
    (unord_eq '((k eudicots)) (query 0 '(k) (\\\ k (load ((diverges 'angiosperms k) * (k =/= 'monocots))))))
    ; SAT: a cnf formula (x|y)&(!x|z) is literally a product of sums of literals
    (: sat (query 0 '(x y z) (\\\ x y z (load
              ((boolean x) * (boolean y) * (boolean z)
                * ((x === 1) + (y === 1))
                * ((x === 0) + (z === 1))))))
       _ (member? '(x 0 y 1 z 1) sat)        ; a model
       (nil? (member? '(x 0 y 0 z 1) sat)))))  ; x=0,y=0 violates (x|y) -> not a model
 0)  ; the closing 0 = the outer form's body: nothing leaks
