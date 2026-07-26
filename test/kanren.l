; kanren -- the machinery lives in love/kanren.l, a REGISTERED MODULE (unify + its
; ufail/ufail? contract and the macro-emitted var/s_plus/s_star/=== stay global;
; walk/est/query/streams/rel ride the registry -- (from 'kanren) is the curried
; accessor). this file is the assert suite + the relational-algebra demo. ONE
; body-having `:` -- the accessor binds and `unord_eq` (an unordered list equality;
; search order is not specified) stay LOCAL, per the corpus law (and the registry
; retired the old re-sweep-mark poisoning hazard outright).
(: K (from 'kanren)
   est (K 'est)  query (K 'query)  rel (K 'rel)  sX (K 'sX)  slist (K 'slist)
   smap (K 'smap)  sno (K 'sno)  stake (K 'stake)  walk (K 'walk)
   (unord_eq a b) (&& (= (sat a) (sat b)) (all (flip member? a) b))

 _ (assert
 (unord_eq '(1 2 3) '(2 1 3))
 (unord_eq '(2 1 3) (stake 3 (sX 1 (sX 2 (sX 3 sno)))))
 (unord_eq '(6 4 2) (slist (smap (* 2) (sX 1 (sX 2 (sX 3 sno))))))
 (: s1 (sX 1 (sX 2 (sX 3 sno)))
    s2 (smap (+ 3) s1)
  (unord_eq '(1 2 3 4 5 6) (slist (s_plus s1 s2))))
 ; --- unification + reification through `query` ---
 (= '((a 1)) (query 0 '(a) (\\ a (est a 1))))
 (id? () (query 0 '(a) (\\ a (et (est a 1) (est a 2)))))               ; 1 and 2 don't unify -> no answer
 (= '((a (1 2 (3 (4))) b (3 (4)) c 4))
    (query 0 '(a b c) (\\ a b c (et (est a `(1 2 b))
                                    (est b `(3 `(c)))
                                    (est c 4)))))
 (unord_eq '((a 7 b 8 c 9) (a 4 b 5 c 6) (a 1 b 2 c 3))
  (query 0 '(a b c) (\\ a b c (vel (et (est a 1) (est b 2) (est c 3))
                                   (et (est a 4) (est b 5) (est c 6))
                                   (et (est a 7) (est b 8) (est c 9))))))
 ; --- an infinite stream, fairly enumerated via zz-delay ---
 (unord_eq '((a 0) (a 1) (a 2) (a 3) (a 4) (a 5) (a 6) (a 7) (a 8) (a 9))
  (query 10 '(a) (\\ a
   (: (never s) ((vel (\ _ sno) (zz never)) s)
      (at_least n) (vel (est a n) (zz (at_least (+ n 1))))
    (vel never (est a 0) (at_least 1))))))
 ; --- the ring: + is OR, * is AND over goal-coins (load unwraps for query) ---
 (unord_eq '((a 1) (a 2)) (query 0 '(a) (\\ a (load (+ (rel (est a 1)) (rel (est a 2)))))))   ; a=1 OR a=2
 (= '((a 1 b 2)) (query 0 '(a b) (\\ a b (load (* (rel (est a 1)) (rel (est b 2)))))))         ; a=1 AND b=2
 ; --- coinductive: no occurs check, so a self-referential binding is a rational
 ;     tree; walk* reifies it as a mu-type without looping (A = (prod elem A)) ---
 (= '(prod elem (mu A))
    (caup (cap (query 0 '(A) (\\ A (est A (L 'prod 'elem A))))))))

 ; --- DEMO: relations read as algebra. `===` (in prel, infix at two) unifies; +
 ; is OR, * is AND. so a relation is a polynomial in unifications -- facts SUM,
 ; sub-relations MULTIPLY; a CNF formula is literally a product of sums. ---
 _ (: ; a plant cladogram -- `diverges` is a SUM of (a===ancestor * b===clade) facts:
   (diverges a b) (((a === 'spermatophytes) * (b === 'angiosperms))   ; seed plants -> flowering plants
                 + ((a === 'angiosperms)    * (b === 'monocots))      ; angiosperms split into
                 + ((a === 'angiosperms)    * (b === 'eudicots)))     ;   monocots | eudicots
   (descends a c) (rel (\\ b (load ((diverges a b) * (diverges b c)))))   ; c descends from a, two splits down
   (boolean x) ((x === 0) + (x === 1))
   (assert
    (unord_eq '((k monocots) (k eudicots)) (query 0 '(k) (\\ k (load (diverges 'angiosperms k)))))     ; daughter clades of angiosperms (OR)
    (unord_eq '((g monocots) (g eudicots)) (query 0 '(g) (\\ g (load (descends 'spermatophytes g)))))  ; two splits below seed plants
    ; SAT: a CNF formula (x|y)&(!x|z) is literally a PRODUCT of SUMS of literals
    (: sat (query 0 '(x y z) (\\ x y z (load
              ((boolean x) * (boolean y) * (boolean z)
                * ((x === 1) + (y === 1))
                * ((x === 0) + (z === 1))))))
       _ (member? '(x 0 y 1 z 1) sat)        ; a model
       (nil? (member? '(x 0 y 0 z 1) sat)))))  ; x=0,y=0 violates (x|y) -> not a model
 0)  ; the closing 0 = the outer form's body: nothing leaks
