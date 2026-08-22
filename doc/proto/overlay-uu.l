; ============================================================================
; overlay x uu -- the overlay's DEPENDENT-index check backed by uu's REAL normalizer.
; Run concatenated with the uu core (which leaks vof/GLOB/etc.):
;     cat test/uu.l doc/proto/overlay-uu.l | out/host/love
; The vec domain's `okr` (result-acceptable predicate) normalizes each vec's length
; INDEX with uu's `(vof () idx)` -- uu's genuine NbE over its nat_rect-defined `add`
; (NOT a toy Peano adder). So `vconcat` of a length-2 and a length-1 vector checks
; against a length-3 signature because UU computes 2+1=3, and a length-9 claim is
; flagged. This is the overlay's `norm`/`okr` slot connected to uu (memory
; pluggable-types, the Phase-2 "dial up to uu" rung).
(: uunorm (\ d (? (&& (two? d) (id? (cap d) 'vec) (= 3 (tally d)))
                  (L 'vec (caup d) (vof () (cap (cup (cup d)))))    ; length index -> uu normal form (a charm)
                  d))
   vdom {'dice '(meters) 'ctors '(pair vec succ zero add)
         'okr    (\ bd decl (= (uunorm bd) (uunorm decl)))         ; result acceptable iff indices uu-CONVERTIBLE
         'join   (\ a b (? (= a b) a 'top)) 'litdie (\ e 'top) 'tr (\ op a b 'top)}
   _ (overlay-off ())
   _ (ev '(: (mk2 x) 0 (mk1 x) 0 (vconcat u v) 0))                  ; runtime stubs (leak), so bodies don't warn
   _ (overlay-set vdom '((mk2 (x) (vec meters (succ (succ zero))))  ; length-2 vector
                         (mk1 (x) (vec meters (succ zero)))         ; length-1
                         (vconcat ((vec a m) (vec a n)) (vec a (add m n)))
                         (len3 (x) (vec meters (succ (succ (succ zero)))))                                     ; claims 3  (correct: uu says 2+1=3)
                         (len9 (x) (vec meters (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))))) ; claims 9  (wrong)
   _ (puts "-- len3 = vconcat (mk2 x)(mk1 x), sig claims 3 -- uu normalizes 2+1=3 [ok, silent]\n")
   _ (ev '(: (len3 x) (vconcat (mk2 x) (mk1 x))))
   _ (puts "-- len9 = same body, sig claims 9 -- uu normalizes 2+1=3 != 9 [FLAGGED]\n")
   _ (ev '(: (len9 x) (vconcat (mk2 x) (mk1 x))))
   _ (puts "-- (uu's real nat_rect-based add did the dependent computation)\n") 0)
