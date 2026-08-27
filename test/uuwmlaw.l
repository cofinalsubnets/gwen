; test/uuwmlaw.l -- the LAWS of the window-manager moves, over the ops that
; test/uuwm.l GENERATED from crew/lux/core.l (this file loads after it by sort
; order; the substrate + kernel live in test/uu.l). This is the stage-5 lock:
; the theorems below are checked against terms COMPILED FROM THE SOURCE, so a
; core.l edit that breaks an invariant fails a PROOF right here, on every
; target, at `make test`. zsane is the specification (a row with no
; duplicates); zinsane is Swierstra's insert invariant.

; --- the specification: sanity is a duplicate-free row ---
(assert
 (defn zsane (pi z stk bool) (lam z (nodupb (zrow z)))))

; --- the moves COMPUTE: rows, counts, focus motion (the wrap included),
; --- the master reorder, the swaps, and all four delete faces ---
(assert
 (4 = (vof 0 '(zcount (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil))))))
 (5 = (vof 0 '(zcount (zins 4 (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))))
 ((vof 0 'true)  = (vof 0 '(zsane (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil))))))
 ((vof 0 'false) = (vof 0 '(zsane (zmk 5 (ncons 2 nnil) (ncons 5 nnil)))))
 ((vof 0 'true)  = (vof 0 '(memb 2 (zrow (zmk 5 (ncons 2 nnil) (ncons 7 nnil))))))
 ; focus motion never loses a window: the row is a fixed carousel
 ((vof 0 '(zrow (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))
  = (vof 0 '(zrow (zfocup (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))))
 (2 = (vof 0 '(zfoc (zfocup (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))))
 (9 = (vof 0 '(zfoc (zfocup (zmk 5 nnil (ncons 7 (ncons 9 nnil)))))))   ; the wrap
 (7 = (vof 0 '(zfoc (zfocdn (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))))
 ; zmaster reorders by design: the focus takes the head of the row
 ((vof 0 '(ncons 5 (ncons 2 (ncons 7 nnil))))
  = (vof 0 '(zrow (zmaster (zmk 5 (ncons 2 nnil) (ncons 7 nnil))))))
 ; the delete faces: a bystander, the focus (dn heir, then up heir), the last one out
 ((vof 0 '(zdel 7 (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))
  = (vof 0 '(ii2 (zmk 5 (ncons 2 nnil) (ncons 9 nnil)))))
 ((vof 0 '(zdel 5 (zmk 5 (ncons 2 nnil) (ncons 7 (ncons 9 nnil)))))
  = (vof 0 '(ii2 (zmk 7 (ncons 2 nnil) (ncons 9 nnil)))))
 ((vof 0 '(zdel 5 (zmk 5 (ncons 2 nnil) nnil)))
  = (vof 0 '(ii2 (zmk 2 nnil nnil))))
 ((vof 0 '(zdel 5 (zmk 5 nnil nnil))) = (vof 0 '(ii1 tt)))
 ; the swaps: the focused window trades places, or rotates to the bottom
 ((vof 0 '(zrow (zswapup (zmk 5 (ncons 2 nnil) (ncons 7 nnil)))))
  = (vof 0 '(ncons 5 (ncons 2 (ncons 7 nnil)))))
 ((vof 0 '(zrow (zswapup (zmk 5 nnil (ncons 7 nnil)))))
  = (vof 0 '(ncons 7 (ncons 5 nnil))))
 ((vof 0 '(zfoc (zswapdn (zmk 5 (ncons 2 nnil) (ncons 7 nnil))))) = 5))

; --- the theorems, PROVED OF THE COMPILED TERMS. zrevrev is sigma-eta;
; --- zfocins is computation; zcountins is natplusnsm riding maponpaths
; --- (add (succ n) m unfolds to succ (add n m) even on a neutral n);
; --- zinsane -- Swierstra's insert invariant -- is nodupmid instantiated
; --- at the row's split, since zrow (zins w z) IS lapp (lrev up)
; --- (ncons w (ncons f dn)) by computation ---
(assert
 (defn zrevrev (pi z stk (paths stk (zrev (zrev z)) z))
   (lam z (idpath z)))
 (defn zfocins (pi w nat (pi z stk (paths nat (zfoc (zins w z)) w)))
   (lam w (lam z (idpath w))))
 (defq zcountins
   (pi w nat (pi z stk (paths nat (zcount (zins w z)) (succ (zcount z)))))
   (lam w (lam z
      (maponpaths nat nat (lam q (succ q))
        (add (nlen (zup z)) (succ (nlen (zdn z))))
        (succ (add (nlen (zup z)) (nlen (zdn z))))
        (natplusnsm (nlen (zup z)) (nlen (zdn z)))))))
 (defq zinsane
   (pi w nat (pi z stk (pi hm (paths bool (memb w (zrow z)) false)
      (pi hs (paths bool (zsane z) true)
       (paths bool (zsane (zins w z)) true)))))
   (lam w (lam z (nodupmid w (ncons (zfoc z) (zdn z)) (lrev (zup z)))))))
