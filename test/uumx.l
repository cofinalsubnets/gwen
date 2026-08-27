; test/uumx.l -- GENERATED from src/mx.l by tools/mx2uu.l. Do not edit;
; refresh with `make uumx`. love's `+` and `*` dispatch matrices as uu terms:
; a kind is its index in mx.l's enum roster (which IS the dispatch lattice), a
; lane its index in appearance order, a band its class id under row+column
; equality across BOTH matrices -- the partition tools/mx2coq.l derives too, off
; the same datum, so the Rocq model and this one cannot disagree about a band.
; test/uumxlaw.l then proves the laws OF THIS TABLE -- the same love datum
; love.c's mx.h is laid from -- so a dispatch edit that breaks the band quotient,
; or the mirror symmetry, fails a PROOF at `make test`. Substrate: test/uu.l.

(assert (member? 'vof (names ())))

; --- the codes ---
(assert
 (defn mxKMint nat 0)
 (defn mxKNom nat 1)
 (defn mxKCharm nat 2)
 (defn mxKSun nat 3)
 (defn mxKGem nat 4)
 (defn mxKTwin nat 5)
 (defn mxKBig nat 6)
 (defn mxKTrayZ nat 7)
 (defn mxKTrayR nat 8)
 (defn mxKTrayC nat 9)
 (defn mxKTrayO nat 10)
 (defn mxKString nat 11)
 (defn mxKChain nat 12)
 (defn mxKTablet nat 13)
 (defn mxKHot nat 14)
 (defn mxLbin_unit nat 0)
 (defn mxLadd_string nat 1)
 (defn mxLbin_a nat 2)
 (defn mxLadd_seq nat 3)
 (defn mxLaddh nat 4)
 (defn mxLbin_b nat 5)
 (defn mxLaddn nat 6)
 (defn mxL0 nat 7)
 (defn mxLmul_rep nat 8)
 (defn mxLmulh nat 9)
 (defn mxLmuln nat 10)
 (defn mxLmul_cart nat 11)
 (defn mxnk nat 15)
 (defn mxnl nat 12)
 (defn mxnb nat 5))

; --- mlist: nlist's shape one type up, a list of ROWS. the two walkers below
; --- take memb's form -- one nat_rect over the length, the index carried down
; --- -- and a grid keeps both of them short, which is the whole reason the
; --- square is not one flat list: npred is O(index) under the eager NbE ---
(assert
 (defn mvec (pi n nat UU)
   (lam n (nat_rect (lam q UU) unit (lam k (lam r (total2 h nlist r))) n)))
 (defn mlist UU (total2 n nat (mvec n)))
 (defn mnil mlist (tpair 0 tt))
 (defn mcons (pi x nlist (pi l mlist mlist))
   (lam x (lam l (tpair (succ (pr1 l)) (tpair x (pr2 l))))))
 (defn mxnth (pi i nat (pi l nlist nat))
   (lam i (lam l ((nat_rect (lam q (pi v (nvec q) (pi j nat nat)))
                     (lam v (lam j 0))
                     (lam k (lam IH (lam v (lam j (bool_rect (lam x nat)
                              (pr1 v)
                              (IH (pr2 v) (npred j))
                              (niszero j))))))
                     (pr1 l))
                  (pr2 l) i))))
 (defn mxrow (pi i nat (pi l mlist nlist))
   (lam i (lam l ((nat_rect (lam q (pi v (mvec q) (pi j nat nlist)))
                     (lam v (lam j nnil))
                     (lam k (lam IH (lam v (lam j (bool_rect (lam x nlist)
                              (pr1 v)
                              (IH (pr2 v) (npred j))
                              (niszero j))))))
                     (pr1 l))
                  (pr2 l) i))))
 (2 = (vof 0 '(mxnth 1 (ncons 5 (ncons 2 (ncons 9 nnil))))))
 (0 = (vof 0 '(mxnth 7 (ncons 5 nnil))))                        ; 0 off the end
 (9 = (vof 0 '(mxnth 0 (mxrow 1 (mcons (ncons 5 nnil) (mcons (ncons 9 nnil) mnil)))))))

; --- THE SQUARES, one row per kind in enum order ---
(assert
 ; `+`: 15 kinds square, 225 cells
 (defn mxaddgrid mlist
   ; KMint
   (mcons (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 nnil)))))))))))))))
   ; KNom
   (mcons (ncons 0 (ncons 1 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 1 (ncons 3 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KCharm
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KSun
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KGem
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KTwin
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KBig
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KTrayZ
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KTrayR
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KTrayC
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KTrayO
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 6 (ncons 5 (ncons 5 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KString
   (mcons (ncons 0 (ncons 1 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 1 (ncons 3 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KChain
   (mcons (ncons 0 (ncons 3 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 3 (ncons 3 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KTablet
   (mcons (ncons 0 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 nnil)))))))))))))))
   ; KHot
   (mcons (ncons 0 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 (ncons 4 nnil)))))))))))))))
    mnil))))))))))))))))

 ; `*`: 15 kinds square, 225 cells
 (defn mxmulgrid mlist
   ; KMint
   (mcons (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 nnil)))))))))))))))
   ; KNom
   (mcons (ncons 7 (ncons 7 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 7 (ncons 7 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KCharm
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KSun
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KGem
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KTwin
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KBig
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KTrayZ
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KTrayR
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KTrayC
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KTrayO
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 10 (ncons 8 (ncons 8 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KString
   (mcons (ncons 7 (ncons 7 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 7 (ncons 7 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KChain
   (mcons (ncons 7 (ncons 7 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 8 (ncons 7 (ncons 11 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KTablet
   (mcons (ncons 7 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 nnil)))))))))))))))
   ; KHot
   (mcons (ncons 7 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 (ncons 9 nnil)))))))))))))))
    mnil))))))))))))))))

 ; the band partition, DERIVED by row+column equality over both squares
 (defn mxbandtbl nlist
   (ncons 0 (ncons 1 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 2 (ncons 1 (ncons 3 (ncons 4 (ncons 4 nnil))))))))))))))))

 ; mirror: the argument swap's action on lanes, in code order
 (defn mxmirtbl nlist
   (ncons 0 (ncons 1 (ncons 5 (ncons 3 (ncons 4 (ncons 2 (ncons 6 (ncons 7 (ncons 8 (ncons 9 (ncons 10 (ncons 11 nnil)))))))))))))

 ; `+` on the band quotient, 25 cells
 (defn mxaddbgrid mlist
   ; band 0 -- KMint
   (mcons (ncons 0 (ncons 0 (ncons 0 (ncons 0 (ncons 0 nnil)))))
   ; band 1 -- KNom
   (mcons (ncons 0 (ncons 1 (ncons 2 (ncons 3 (ncons 4 nnil)))))
   ; band 2 -- KCharm
   (mcons (ncons 0 (ncons 5 (ncons 6 (ncons 5 (ncons 4 nnil)))))
   ; band 3 -- KChain
   (mcons (ncons 0 (ncons 3 (ncons 2 (ncons 3 (ncons 4 nnil)))))
   ; band 4 -- KTablet
   (mcons (ncons 0 (ncons 4 (ncons 4 (ncons 4 (ncons 4 nnil)))))
    mnil))))))

 ; `*` on the band quotient, 25 cells
 (defn mxmulbgrid mlist
   ; band 0 -- KMint
   (mcons (ncons 7 (ncons 7 (ncons 7 (ncons 7 (ncons 7 nnil)))))
   ; band 1 -- KNom
   (mcons (ncons 7 (ncons 7 (ncons 8 (ncons 7 (ncons 9 nnil)))))
   ; band 2 -- KCharm
   (mcons (ncons 7 (ncons 8 (ncons 10 (ncons 8 (ncons 9 nnil)))))
   ; band 3 -- KChain
   (mcons (ncons 7 (ncons 7 (ncons 8 (ncons 11 (ncons 9 nnil)))))
   ; band 4 -- KTablet
   (mcons (ncons 7 (ncons 9 (ncons 9 (ncons 9 (ncons 9 nnil)))))
    mnil))))))

 ; --- and the six lookups they carry ---
 (defn mxadd (pi a nat (pi b nat nat)) (lam a (lam b (mxnth b (mxrow a mxaddgrid)))))
 (defn mxmul (pi a nat (pi b nat nat)) (lam a (lam b (mxnth b (mxrow a mxmulgrid)))))
 (defn mxband (pi k nat nat) (lam k (mxnth k mxbandtbl)))
 (defn mxmirror (pi l nat nat) (lam l (mxnth l mxmirtbl)))
 (defn mxaddband (pi x nat (pi y nat nat))
   (lam x (lam y (mxnth y (mxrow x mxaddbgrid)))))
 (defn mxmulband (pi x nat (pi y nat nat))
   (lam x (lam y (mxnth y (mxrow x mxmulbgrid))))))

; --- the bounded forall the theorems close by: every kind, then the square ---
(assert
 (defn mxall (pi n nat (pi f (pi k nat bool) bool))
   (lam n (lam f (nat_rect (lam q bool) true (lam k (lam r (andb (f k) r))) n))))
 (defn mxsq (pi f (pi a nat (pi b nat bool)) bool)
   (lam f (mxall mxnk (lam a (mxall mxnk (lam b (f a b))))))))
