; test/uuwm.l -- GENERATED from crew/lux/core.l by tools/uuwmgen.l. Do not
; edit; refresh with `make uuwm`. The window-manager moves, compiled from
; the SOURCE the lux runs (kind-directed by crew/lux/sigs.l) -- so the laws
; in test/uuwmlaw.l are proved of the implementation itself; an invariant-
; breaking core.l edit fails a PROOF at `make test`. Substrate: test/uu.l.
(assert
 (defn 'zrow
   '(pi z stk nlist)
   '(lam z (lapp (lrev (zup z)) (ncons (zfoc z) (zdn z)))))
 (defn 'zrev
   '(pi z stk stk)
   '(lam z (zmk (zfoc z) (zdn z) (zup z))))
 (defn 'zfocup
   '(pi z stk stk)
   '(lam z (let up (zup z) (nat_rect (lam gq stk) (let all (lrev (ncons (zfoc z) (zdn z))) (zmk (hd0 all) (tl0 all) nnil)) (lam gk (lam gr (zmk (hd0 up) (tl0 up) (ncons (zfoc z) (zdn z))))) (pr1 up)))))
 (defn 'zfocdn
   '(pi z stk stk)
   '(lam z (zrev (zfocup (zrev z)))))
 (defn 'zswapup
   '(pi z stk stk)
   '(lam z (let up (zup z) (nat_rect (lam gq stk) (zmk (zfoc z) (lrev (zdn z)) nnil) (lam gk (lam gr (zmk (zfoc z) (tl0 up) (ncons (hd0 up) (zdn z))))) (pr1 up)))))
 (defn 'zswapdn
   '(pi z stk stk)
   '(lam z (zrev (zswapup (zrev z)))))
 (defn 'zmaster
   '(pi z stk stk)
   '(lam z (zmk (zfoc z) nnil (lapp (lrev (zup z)) (zdn z)))))
 (defn 'zins
   '(pi w nat (pi z stk stk))
   '(lam w (lam z (zmk w (zup z) (ncons (zfoc z) (zdn z))))))
 (defn 'znub
   '(pi w nat (pi l nlist nlist))
   '(lam w (lam l (nfilt w l))))
 (defn 'zdel
   '(pi w nat (pi z stk mstk))
   '(lam w (lam z (let f (zfoc z) (let up (znub w (zup z)) (let dn (znub w (zdn z)) (bool_rect (lam gx mstk) (nat_rect (lam gq mstk) (nat_rect (lam gq mstk) (ii1 tt) (lam gk (lam gr (ii2 (zmk (hd0 up) (tl0 up) nnil)))) (pr1 up)) (lam gk (lam gr (ii2 (zmk (hd0 dn) up (tl0 dn))))) (pr1 dn)) (ii2 (zmk f up dn)) (nateqb f w)))))))))
