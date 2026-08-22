; test/uukind.l -- GENERATED from doc/proto/kinds.l by tools/kinds2uu.l. Do
; not edit; refresh with `make uukind`. The abstract kinds-lattice JOIN (and
; its num? guard) compiled into uu over a nat-coded carrier, kind = its rank
; (str the branch off the spine). test/uukindlaw.l proves the semilattice laws
; of THESE terms at corpus time -- a law-breaking kinds.l edit fails a check.
; Substrate: test/uu.l (natgtb / nateqb / andb / bool_rect).
(assert
 (defn 'numq
   '(pi k nat bool)
   '(lam k (let r k (andb (natgtb r 0) (natgtb 5 r)))))
 (defn 'kjoin
   '(pi a nat (pi b nat nat))
   '(lam a (lam b (bool_rect (lam gx nat) b (bool_rect (lam gx nat) a (bool_rect (lam gx nat) a (bool_rect (lam gx nat) (bool_rect (lam gx nat) b a (natgtb b a)) 5 (andb (numq a) (numq b))) (nateqb a b)) (nateqb b 0)) (nateqb a 0))))))
(: uk-codes '(0 1 2 3 4 5 6))
