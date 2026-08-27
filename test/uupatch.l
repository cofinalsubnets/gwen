; test/uupatch.l -- the PATCH GROUPOID's named-slot AND positional slices as uu
; proof TERMS: the generated leg of the patch arc. test/patch.l's first two
; forms state the laws as executable asserts; test/proof/rocq/patch.v hand-proved
; them as scaffolding and PROMISED this file (its provenance note): the laws
; re-proven inside uu's own kernel, then emitted to Rocq AND Lean by
; tools/uu2coq.l + tools/uu2lean.l -- two independent kernels re-certifying the
; same terms, the de Bruijn criterion the rest of the proof corpus already
; rides. Substrate: test/uu.l + test/uulay.l's list vocabulary (this file
; loads after both by sort order).
;
; the model mirrors the .l's first form name for name: a tree is a total map
; (key -> value, nats -- the .v's closure of "a missing slot reads 0"), a patch
; (chg k a b) one slot-change carrying its context, commute the partial swap.
; where patch.v case-split on Nat.eqb with tactics, the terms here case on
; isdeceqnat -- decidable equality as a COPROD, so every branch carries the
; path evidence a bool would have dropped; refusal is the unit summand. all
; equality of trees is pointwise (no funext anywhere), exactly the .v's rule.

; --- the model: patch, tree, apply, validity, inverse, commute ---
(assert
 (defn patch UU (total2 k nat (total2 a nat nat)))
 (defn chg (pi k nat (pi a nat (pi b nat patch)))
   (lam k (lam a (lam b (tpair k (tpair a b))))))
 (defn pk (pi p patch nat) (lam p (pr1 p)))
 (defn pa (pi p patch nat) (lam p (pr1 (pr2 p))))
 (defn pb (pi p patch nat) (lam p (pr2 (pr2 p))))
 (defn tree UU (pi k nat nat))
 (defn apon (pi t tree (pi p patch tree))
   (lam t (lam p (lam q (coprod_rect (lam c nat)
                           (lam e (pb p))
                           (lam ne (t q))
                           (isdeceqnat q (pk p)))))))
 (defn valid (pi t tree (pi p patch bool))
   (lam t (lam p (coprod_rect (lam c bool)
                    (lam e true)
                    (lam ne false)
                    (isdeceqnat (t (pk p)) (pa p))))))
 (defn invert (pi p patch patch)
   (lam p (chg (pk p) (pb p) (pa p))))
 (defn ppair UU (total2 x patch patch))
 (defn commute (pi p patch (pi q patch (coprod ppair unit)))
   (lam p (lam q (coprod_rect (lam c (coprod ppair unit))
                    (lam e (ii2 tt))
                    (lam ne (ii1 (tpair q p)))
                    (isdeceqnat (pk p) (pk q))))))
 ; the model COMPUTES: apply writes its slot and only its slot; invert is its
 ; own inverse; independent keys swap, a shared key refuses
 (9 = (vof 0 '(apon (lam k 7) (chg 3 7 9) 3)))
 (7 = (vof 0 '(apon (lam k 7) (chg 3 7 9) 5)))
 ((vof 0 '(chg 3 7 9)) = (vof 0 '(invert (invert (chg 3 7 9)))))
 (id? (vof 0 'true)  (vof 0 '(valid (lam k 5) (chg 3 5 9))))
 (id? (vof 0 'false) (vof 0 '(valid (lam k 5) (chg 3 4 9))))
 ((vof 0 '(commute (chg 3 0 1) (chg 5 0 2)))
  = (vof 0 '(ii1 (tpair (chg 5 0 2) (chg 3 0 1)))))
 ((vof 0 '(commute (chg 3 0 1) (chg 3 1 2))) = (vof 0 '(ii2 tt))))

; --- (L1) commute is INVOLUTIVE. cmside is the coprod no-confusion family (a
; --- recorded refusal is never a success); the ii1 lane rides maponpaths of
; --- "re-commute the carried pair", so the involution IS the algebra: commute
; --- q p computes straight to ii1 (p,q) once the keys are known distinct ---
(assert
 (defn cmside (pi c (coprod ppair unit) UU)
   (lam c (coprod_rect (lam x UU) (lam x empty) (lam x unit) c)))
 (defq commute-involutive
   (pi p patch (pi q patch (pi q2 patch (pi p2 patch
      (pi h (paths (coprod ppair unit) (commute p q) (ii1 (tpair q2 p2)))
        (paths (coprod ppair unit) (commute q2 p2) (ii1 (tpair p q))))))))
   (lam p (lam q (lam q2 (lam p2
      ((coprod_rect
         (lam c (pi h (paths (coprod ppair unit)
                        (coprod_rect (lam c2 (coprod ppair unit))
                          (lam e2 (ii2 tt))
                          (lam ne2 (ii1 (tpair q p)))
                          c)
                        (ii1 (tpair q2 p2)))
                  (paths (coprod ppair unit) (commute q2 p2) (ii1 (tpair p q)))))
         (lam e (lam h (fromempty
            (paths (coprod ppair unit) (commute q2 p2) (ii1 (tpair p q)))
            (transportf (coprod ppair unit) cmside (ii2 tt) (ii1 (tpair q2 p2)) h tt))))
         (lam ne (lam h
            (pathscomp0 (coprod ppair unit)
              (commute q2 p2) (commute q p) (ii1 (tpair p q))
              (pathsinv0 (coprod ppair unit) (commute q p) (commute q2 p2)
                (maponpaths (coprod ppair unit) (coprod ppair unit)
                  (lam c (coprod_rect (lam x (coprod ppair unit))
                           (lam z (commute (pr1 z) (pr2 z)))
                           (lam u (ii2 tt))
                           c))
                  (ii1 (tpair q p)) (ii1 (tpair q2 p2)) h))
              ((coprod_rect
                 (lam c (paths (coprod ppair unit)
                          (coprod_rect (lam c2 (coprod ppair unit))
                            (lam e2 (ii2 tt))
                            (lam ne2 (ii1 (tpair p q)))
                            c)
                          (ii1 (tpair p q))))
                 (lam e2 (fromempty
                    (paths (coprod ppair unit) (ii2 tt) (ii1 (tpair p q)))
                    (ne (pathsinv0 nat (pk q) (pk p) e2))))
                 ; the ascriptions pin tpair's implicit family and the reduced
                 ; refl type: Lean's elaborator won't iota-reduce the stuck rec
                 ; in the expected type while a metavariable is live (the
                 ; based-J seam, uu2lean's note); Rocq needed nothing
                 (lam ne2 (the (paths (coprod ppair unit)
                                 (ii1 (tpair p q)) (ii1 (tpair p q)))
                               (idpath (ii1 (the ppair (tpair p q))))))
                 (isdeceqnat (pk q) (pk p)))))))
         (isdeceqnat (pk p) (pk q))))))))))

; --- (L2) the INVERSE round-trips IN CONTEXT: where the patch is valid, apply
; --- then unpull restores every slot -- pointwise, quantifying the key like
; --- the .v does; validity opened by ONE convoy on its own decision, the main
; --- case abstracting BOTH apon layers' shared scrutinee at once ---
(assert
 (defq invert-roundtrip
   (pi t tree (pi p patch (pi hv (paths bool (valid t p) true)
      (pi q nat (paths nat (apon (apon t p) (invert p) q) (t q))))))
   (lam t (lam p
      ((coprod_rect
         (lam c (pi hv (paths bool
                         (coprod_rect (lam c2 bool) (lam e2 true) (lam ne2 false) c)
                         true)
                  (pi q nat (paths nat (apon (apon t p) (invert p) q) (t q)))))
         (lam e (lam hv (lam q
           ((coprod_rect
              (lam c (paths nat
                       (coprod_rect (lam c2 nat)
                         (lam e2 (pa p))
                         (lam ne2 (coprod_rect (lam c3 nat)
                                    (lam e3 (pb p))
                                    (lam ne3 (t q))
                                    c))
                         c)
                       (t q)))
              (lam e1 (pathscomp0 nat (pa p) (t (pk p)) (t q)
                        (pathsinv0 nat (t (pk p)) (pa p) e)
                        (maponpaths nat nat t (pk p) q (pathsinv0 nat q (pk p) e1))))
              (lam ne1 (idpath (t q)))
              (isdeceqnat q (pk p)))))))
         (lam ne (lam hv (fromempty
            (pi q nat (paths nat (apon (apon t p) (invert p) q) (t q)))
            (nopathsfalsetotrue hv))))
         (isdeceqnat (t (pk p)) (pa p)))))))
 ; and it computes: apply then unpull reads the old value back
 (7 = (vof 0 '(apon (apon (lam k 7) (chg 3 7 9)) (invert (chg 3 7 9)) 3))))

; --- (L3a) independent patches are ORDER-FREE (the semantic heart -- what makes
; --- merge and cherry-pick sound). apon-comm is the raw form off the key
; --- disequality; commute-sound ties it to the primitive: a successful commute
; --- WITNESSES order-freedom. the double convoy cases each decision once,
; --- abstracting all its occurrences on both sides ---
(assert
 (defq apon-comm
   (pi p patch (pi q patch (pi ne (pi e (paths nat (pk p) (pk q)) empty)
      (pi t tree (pi r nat
        (paths nat (apon (apon t p) q r) (apon (apon t q) p r)))))))
   (lam p (lam q (lam ne (lam t (lam r
      ((coprod_rect
         (lam c (paths nat
                  (coprod_rect (lam c2 nat)
                    (lam e2 (pb q))
                    (lam ne2 (coprod_rect (lam c3 nat)
                               (lam e3 (pb p))
                               (lam ne3 (t r))
                               (isdeceqnat r (pk p))))
                    c)
                  (coprod_rect (lam c2 nat)
                    (lam e2 (pb p))
                    (lam ne2 (coprod_rect (lam c3 nat)
                               (lam e3 (pb q))
                               (lam ne3 (t r))
                               c))
                    (isdeceqnat r (pk p)))))
         (lam e1
           ((coprod_rect
              (lam c (paths nat (pb q)
                       (coprod_rect (lam c2 nat)
                         (lam e2 (pb p))
                         (lam ne2 (pb q))
                         c)))
              (lam e2 (fromempty (paths nat (pb q) (pb p))
                        (ne (pathscomp0 nat (pk p) r (pk q)
                              (pathsinv0 nat r (pk p) e2)
                              e1))))
              (lam ne2 (idpath (pb q)))
              (isdeceqnat r (pk p)))))
         (lam ne1
           ((coprod_rect
              (lam c (paths nat
                       (coprod_rect (lam c2 nat) (lam e2 (pb p)) (lam ne2 (t r)) c)
                       (coprod_rect (lam c2 nat) (lam e2 (pb p)) (lam ne2 (t r)) c)))
              (lam e2 (idpath (pb p)))
              (lam ne2 (idpath (t r)))
              (isdeceqnat r (pk p)))))
         (isdeceqnat r (pk q))))))))))
 (defq commute-sound
   (pi p patch (pi q patch
      (pi h (paths (coprod ppair unit) (commute p q) (ii1 (tpair q p)))
        (pi t tree (pi r nat
          (paths nat (apon (apon t p) q r) (apon (apon t q) p r)))))))
   (lam p (lam q
      ((coprod_rect
         (lam c (pi h (paths (coprod ppair unit)
                        (coprod_rect (lam c2 (coprod ppair unit))
                          (lam e2 (ii2 tt))
                          (lam ne2 (ii1 (tpair q p)))
                          c)
                        (ii1 (tpair q p)))
                  (pi t tree (pi r nat
                    (paths nat (apon (apon t p) q r) (apon (apon t q) p r))))))
         (lam e (lam h (fromempty
            (pi t tree (pi r nat
              (paths nat (apon (apon t p) q r) (apon (apon t q) p r))))
            (transportf (coprod ppair unit) cmside (ii2 tt) (ii1 (tpair q p)) h tt))))
         (lam ne (lam h (apon-comm p q ne)))
         (isdeceqnat (pk p) (pk q)))))))
 ; both pull orders land the same tree, at the touched keys and elsewhere
 (1 = (vof 0 '(apon (apon (lam k 0) (chg 3 0 1)) (chg 5 0 2) 3)))
 (1 = (vof 0 '(apon (apon (lam k 0) (chg 5 0 2)) (chg 3 0 1) 3)))
 (2 = (vof 0 '(apon (apon (lam k 0) (chg 3 0 1)) (chg 5 0 2) 5)))
 (2 = (vof 0 '(apon (apon (lam k 0) (chg 5 0 2)) (chg 3 0 1) 5))))

; --- (L3b) a FRONTIER is stable under an adjacent swap of independent patches.
; --- patch lists are UniMath-style vectors at patch (a plist's length is pr1,
; --- like nlist); applall is the .l's left fold. the lift is two congruence
; --- lemmas (one apon, then the whole fold -- patch.v's apon_ext /
; --- fold_apon_ext) and one induction down the prefix; the swap at the head is
; --- exactly apon-comm under the fold. meaning is order-free, so the
; --- (unordered) patch set is a sound version identity -- the reproducibility
; --- tie, now machine-checked in three kernels ---
(assert
 (defn pvec (pi n nat UU)
   (lam n (nat_rect (lam q UU) unit (lam k (lam r (total2 h patch r))) n)))
 (defn plist UU (total2 n nat (pvec n)))
 (defn pnil plist (tpair 0 tt))
 (defn pcons (pi x patch (pi l plist plist))
   (lam x (lam l (tpair (succ (pr1 l)) (tpair x (pr2 l))))))
 (defn papp (pi a plist (pi b plist plist))
   (lam a (lam b ((nat_rect (lam q (pi v (pvec q) plist))
                     (lam v b)
                     (lam k (lam IH (lam v (pcons (pr1 v) (IH (pr2 v))))))
                     (pr1 a))
                   (pr2 a)))))
 (defn applall (pi l plist (pi t tree tree))
   (lam l (lam t ((nat_rect (lam q (pi v (pvec q) (pi t2 tree tree)))
                     (lam v (lam t2 t2))
                     (lam k (lam IH (lam v (lam t2 (IH (pr2 v) (apon t2 (pr1 v)))))))
                     (pr1 l))
                   (pr2 l) t))))
 (defq apon-ext
   (pi t1 tree (pi t2 tree (pi p patch
      (pi hp (pi k nat (paths nat (t1 k) (t2 k)))
        (pi r nat (paths nat (apon t1 p r) (apon t2 p r)))))))
   (lam t1 (lam t2 (lam p (lam hp (lam r
      ((coprod_rect
         (lam c (paths nat
                  (coprod_rect (lam c2 nat) (lam e2 (pb p)) (lam ne2 (t1 r)) c)
                  (coprod_rect (lam c2 nat) (lam e2 (pb p)) (lam ne2 (t2 r)) c)))
         (lam e (idpath (pb p)))
         (lam ne (hp r))
         (isdeceqnat r (pk p))))))))))
 (defq applall-ext
   (pi l plist (pi t1 tree (pi t2 tree
      (pi hp (pi k nat (paths nat (t1 k) (t2 k)))
        (pi r nat (paths nat (applall l t1 r) (applall l t2 r)))))))
   (lam l ((nat_rect
       (lam q (pi v (pvec q) (pi t1 tree (pi t2 tree
                (pi hp (pi k nat (paths nat (t1 k) (t2 k)))
                  (pi r nat (paths nat (applall (tpair q v) t1 r)
                                       (applall (tpair q v) t2 r))))))))
       (lam v (lam t1 (lam t2 (lam hp (lam r (hp r))))))
       (lam k (lam IH (lam v (lam t1 (lam t2 (lam hp
         (IH (pr2 v) (apon t1 (pr1 v)) (apon t2 (pr1 v))
             (apon-ext t1 t2 (pr1 v) hp))))))))
       (pr1 l))
     (pr2 l))))
 (defq applall-swap
   (pi p patch (pi q patch (pi ne (pi e (paths nat (pk p) (pk q)) empty)
      (pi pre plist (pi post plist (pi t tree (pi r nat
        (paths nat
          (applall (papp pre (pcons p (pcons q post))) t r)
          (applall (papp pre (pcons q (pcons p post))) t r)))))))))
   (lam p (lam q (lam ne (lam pre (lam post
      ((nat_rect
         (lam n (pi v (pvec n) (pi t tree (pi r nat
           (paths nat
             (applall (papp (tpair n v) (pcons p (pcons q post))) t r)
             (applall (papp (tpair n v) (pcons q (pcons p post))) t r))))))
         (lam v (lam t (applall-ext post
                  (apon (apon t p) q)
                  (apon (apon t q) p)
                  (apon-comm p q ne t))))
         (lam k (lam IH (lam v (lam t
           (IH (pr2 v) (apon t (pr1 v)))))))
         (pr1 pre))
       (pr2 pre))))))))
 ; the fold COMPUTES, and the frontier agrees either order
 (2 = (vof 0 '(applall (pcons (chg 3 0 1) (pcons (chg 5 0 2) pnil)) (lam k 0) 5)))
 ((vof 0 '(applall (pcons (chg 3 0 1) (pcons (chg 5 0 2) pnil)) (lam k 0) 3))
  = (vof 0 '(applall (pcons (chg 5 0 2) (pcons (chg 3 0 1) pnil)) (lam k 0) 3))))

; ============================================================================
; the POSITIONAL slice: hunks over a SEQUENCE, where commute genuinely
; RE-AIMS -- test/patch.l second form, patch.v hunk slice, migrated. the
; kit: plus-cancellation as bool/nat EQUATIONS (succ-succ steps are
; definitional), the take/drop/append fusion laws, then the model and the
; four laws, each intermediate SEALED as its own defq (a path-valued let
; poisons every later check in its scope -- the eager-NbE let law).
; ============================================================================
; --- the POSITIONAL slice needs an index algebra (plus-cancellation through
; --- the order and the truncated minus) and a take/drop/append fusion kit.
; --- every succ-succ step of natgtb and natminus is DEFINITIONAL, so the
; --- cancellation laws are one induction each, steps landing on their IH ---
(assert
 (defq natlehnplusnm
   (pi a nat (pi b nat (paths bool (natgtb a (add a b)) false)))
   (lam a (lam b (nat_rect (lam q (paths bool (natgtb q (add q b)) false))
      (idpath false)
      (lam k (lam IH IH))
      a))))
 (defq natlehmplusnm
   (pi a nat (pi b nat (paths bool (natgtb a (add b a)) false)))
   (lam a (lam b (nat_rect (lam q (paths bool (natgtb a (add q a)) false))
      (isreflnatleh a)
      (lam k (lam IH (natlehsucc a (add k a) IH)))
      b))))
 (defq natgtbplusl
   (pi c nat (pi a nat (pi b nat (paths bool (natgtb (add c a) (add c b)) (natgtb a b)))))
   (lam c (lam a (lam b (nat_rect (lam q (paths bool (natgtb (add q a) (add q b)) (natgtb a b)))
      (idpath (natgtb a b))
      (lam k (lam IH IH))
      c)))))
 (defq natgtbplusr
   (pi a nat (pi b nat (pi c nat (paths bool (natgtb (add a c) (add b c)) (natgtb a b)))))
   (lam a (lam b (lam c
      (pathscomp0 bool (natgtb (add a c) (add b c)) (natgtb (add c a) (add c b)) (natgtb a b)
        (pathscomp0 bool (natgtb (add a c) (add b c)) (natgtb (add c a) (add b c)) (natgtb (add c a) (add c b))
          (maponpaths nat bool (lam w (natgtb w (add b c))) (add a c) (add c a) (natpluscomm a c))
          (maponpaths nat bool (lam w (natgtb (add c a) w)) (add b c) (add c b) (natpluscomm b c)))
        (natgtbplusl c a b))))))
 (defq natpluscancelminus
   (pi a nat (pi b nat (paths nat (natminus (add a b) b) a)))
   (lam a (lam b (nat_rect (lam q (paths nat (natminus (add a q) q) a))
      (pathscomp0 nat (natminus (add a 0) 0) (add a 0) a (natminusn0 (add a 0)) (natplusr0 a))
      (lam k (lam IH
        (pathscomp0 nat (natminus (add a (succ k)) (succ k)) (natminus (add (succ a) k) (succ k)) a
          (maponpaths nat nat (lam w (natminus w (succ k))) (add a (succ k)) (add (succ a) k) (natplusnsm a k))
          IH)))
      b))))
 (defq natpluslcancelminus
   (pi c nat (pi x nat (paths nat (natminus (add c x) c) x)))
   (lam c (lam x
      (pathscomp0 nat (natminus (add c x) c) (natminus (add x c) c) x
        (maponpaths nat nat (lam w (natminus w c)) (add c x) (add x c) (natpluscomm c x))
        (natpluscancelminus x c)))))
 (defq natplusswap
   (pi a nat (pi b nat (pi c nat (paths nat (add (add a b) c) (add (add a c) b)))))
   (lam a (lam b (lam c
      (pathscomp0 nat (add (add a b) c) (add a (add b c)) (add (add a c) b)
        (natplusassoc a b c)
        (pathscomp0 nat (add a (add b c)) (add a (add c b)) (add (add a c) b)
          (maponpaths nat nat (lam w (add a w)) (add b c) (add c b) (natpluscomm b c))
          (pathsinv0 nat (add (add a c) b) (add a (add c b)) (natplusassoc a c b))))))))
 ; the computed face
 (id? (vof 0 'false) (vof 0 '(natgtb 4 (add 4 3))))
 (id? (vof 0 '5) (vof 0 '(natminus (add 5 3) 3))))

; --- the list kit: how take, drop and append fuse. all at nlist, uulay.l's
; --- vocabulary (ltake/ldrop/lapp/lapptakedrop/ldroplen/lnil0 load before
; --- this file by sort order); every induction walks the vector like uulay's ---
(assert
 (defq ldropnil
   (pi k nat (paths nlist (ldrop k nnil) nnil))
   (lam k (nat_rect (lam q (paths nlist (ldrop q nnil) nnil))
      (idpath nnil)
      (lam j (lam IH (idpath nnil)))
      k)))
 (defq lapplen
   (pi a nlist (pi b nlist (paths nat (nlen (lapp a b)) (add (nlen a) (nlen b)))))
   (lam a (lam b ((nat_rect
      (lam q (pi v (nvec q) (paths nat (nlen (lapp (tpair q v) b)) (add q (nlen b)))))
      (lam v (idpath (nlen b)))
      (lam k (lam IH (lam v
        (maponpaths nat nat (lam w (succ w))
          (nlen (lapp (tpair k (pr2 v)) b)) (add k (nlen b)) (IH (pr2 v))))))
      (pr1 a))
     (pr2 a)))))
 (defq lappassoc
   (pi a nlist (pi b nlist (pi c nlist (paths nlist (lapp (lapp a b) c) (lapp a (lapp b c))))))
   (lam a (lam b (lam c ((nat_rect
      (lam q (pi v (nvec q) (paths nlist (lapp (lapp (tpair q v) b) c) (lapp (tpair q v) (lapp b c)))))
      (lam v (idpath (lapp b c)))
      (lam k (lam IH (lam v
        (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
          (lapp (lapp (tpair k (pr2 v)) b) c) (lapp (tpair k (pr2 v)) (lapp b c)) (IH (pr2 v))))))
      (pr1 a))
     (pr2 a))))))
 (defq ltakelenapp
   (pi a nlist (pi b nlist (paths nlist (ltake (nlen a) (lapp a b)) a)))
   (lam a (lam b ((nat_rect
      (lam q (pi v (nvec q) (paths nlist (ltake q (lapp (tpair q v) b)) (tpair q v))))
      (lam v (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v)))
      (lam k (lam IH (lam v
        (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
          (ltake k (lapp (tpair k (pr2 v)) b)) (tpair k (pr2 v)) (IH (pr2 v))))))
      (pr1 a))
     (pr2 a)))))
 (defq ldroplenapp
   (pi a nlist (pi b nlist (paths nlist (ldrop (nlen a) (lapp a b)) b)))
   (lam a (lam b ((nat_rect
      (lam q (pi v (nvec q) (paths nlist (ldrop q (lapp (tpair q v) b)) b)))
      (lam v (idpath b))
      (lam k (lam IH (lam v (IH (pr2 v)))))
      (pr1 a))
     (pr2 a)))))
 (defq ldropplusapp
   (pi a nlist (pi m nat (pi b nlist (paths nlist (ldrop (add (nlen a) m) (lapp a b)) (ldrop m b)))))
   (lam a (lam m (lam b ((nat_rect
      (lam q (pi v (nvec q) (paths nlist (ldrop (add q m) (lapp (tpair q v) b)) (ldrop m b))))
      (lam v (idpath (ldrop m b)))
      (lam k (lam IH (lam v (IH (pr2 v)))))
      (pr1 a))
     (pr2 a))))))
 (defq ltakeplusapp
   (pi a nlist (pi m nat (pi b nlist (paths nlist (ltake (add (nlen a) m) (lapp a b)) (lapp a (ltake m b))))))
   (lam a (lam m (lam b ((nat_rect
      (lam q (pi v (nvec q) (paths nlist (ltake (add q m) (lapp (tpair q v) b)) (lapp (tpair q v) (ltake m b)))))
      (lam v (idpath (ltake m b)))
      (lam k (lam IH (lam v
        (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
          (ltake (add k m) (lapp (tpair k (pr2 v)) b)) (lapp (tpair k (pr2 v)) (ltake m b)) (IH (pr2 v))))))
      (pr1 a))
     (pr2 a))))))
 (defq ltakeprefix
   (pi j nat (pi a nlist (pi b nlist (pi h (paths bool (natgtb j (nlen a)) false)
      (paths nlist (ltake j (lapp a b)) (ltake j a))))))
   (lam j ((nat_rect
      (lam q (pi a nlist (pi b nlist (pi h (paths bool (natgtb q (nlen a)) false)
               (paths nlist (ltake q (lapp a b)) (ltake q a))))))
      (lam a (lam b (lam h (idpath nnil))))
      (lam j2 (lam IH
        (lam a (lam b ((nat_rect
           (lam r (pi v (nvec r) (pi h (paths bool (natgtb (succ j2) r) false)
                    (paths nlist (ltake (succ j2) (lapp (tpair r v) b)) (ltake (succ j2) (tpair r v))))))
           (lam v (lam h (fromempty
              (paths nlist (ltake (succ j2) (lapp (tpair 0 v) b)) (ltake (succ j2) (tpair 0 v)))
              (nopathstruetofalse h))))
           (lam k (lam u (lam v (lam h
              (maponpaths nlist nlist (lam t (ncons (pr1 v) t))
                (ltake j2 (lapp (tpair k (pr2 v)) b)) (ltake j2 (tpair k (pr2 v)))
                (IH (tpair k (pr2 v)) b h))))))
           (pr1 a))
          (pr2 a))))))
      j))))
 (defq ldropprefix
   (pi j nat (pi a nlist (pi b nlist (pi h (paths bool (natgtb j (nlen a)) false)
      (paths nlist (ldrop j (lapp a b)) (lapp (ldrop j a) b))))))
   (lam j ((nat_rect
      (lam q (pi a nlist (pi b nlist (pi h (paths bool (natgtb q (nlen a)) false)
               (paths nlist (ldrop q (lapp a b)) (lapp (ldrop q a) b))))))
      (lam a (lam b (lam h (idpath (lapp a b)))))
      (lam j2 (lam IH
        (lam a (lam b ((nat_rect
           (lam r (pi v (nvec r) (pi h (paths bool (natgtb (succ j2) r) false)
                    (paths nlist (ldrop (succ j2) (lapp (tpair r v) b)) (lapp (ldrop (succ j2) (tpair r v)) b)))))
           (lam v (lam h (fromempty
              (paths nlist (ldrop (succ j2) (lapp (tpair 0 v) b)) (lapp (ldrop (succ j2) (tpair 0 v)) b))
              (nopathstruetofalse h))))
           (lam k (lam u (lam v (lam h
              (IH (tpair k (pr2 v)) b h)))))
           (pr1 a))
          (pr2 a))))))
      j))))
 (defq ldropdrop
   (pi i nat (pi c nat (pi s nlist (paths nlist (ldrop c (ldrop i s)) (ldrop (add i c) s)))))
   (lam i (lam c ((nat_rect
      (lam q (pi s nlist (paths nlist (ldrop c (ldrop q s)) (ldrop (add q c) s))))
      (lam s (idpath (ldrop c s)))
      (lam i2 (lam IH
        (lam s ((nat_rect
           (lam r (pi v (nvec r) (paths nlist (ldrop c (ldrop (succ i2) (tpair r v))) (ldrop (succ (add i2 c)) (tpair r v)))))
           (lam v (ldropnil c))
           (lam k (lam u (lam v (IH (tpair k (pr2 v))))))
           (pr1 s))
          (pr2 s)))))
      i)))))
 (defq ltakelen
   (pi i nat (pi s nlist (pi h (paths bool (natgtb i (nlen s)) false)
      (paths nat (nlen (ltake i s)) i))))
   (lam i ((nat_rect
      (lam q (pi s nlist (pi h (paths bool (natgtb q (nlen s)) false)
               (paths nat (nlen (ltake q s)) q))))
      (lam s (lam h (idpath 0)))
      (lam i2 (lam IH
        (lam s ((nat_rect
           (lam r (pi v (nvec r) (pi h (paths bool (natgtb (succ i2) r) false)
                    (paths nat (nlen (ltake (succ i2) (tpair r v))) (succ i2)))))
           (lam v (lam h (fromempty
              (paths nat (nlen (ltake (succ i2) (tpair 0 v))) (succ i2))
              (nopathstruetofalse h))))
           (lam k (lam u (lam v (lam h
              (maponpaths nat nat (lam w (succ w))
                (nlen (ltake i2 (tpair k (pr2 v)))) i2
                (IH (tpair k (pr2 v)) h))))))
           (pr1 s))
          (pr2 s)))))
      i))))
 ; the fused faces compute
 ((vof 0 '(ncons 7 (ncons 9 nnil)))
  = (vof 0 '(ltake (nlen (ncons 7 (ncons 9 nnil))) (lapp (ncons 7 (ncons 9 nnil)) (ncons 4 nnil)))))
 ((vof 0 '(ncons 4 nnil))
  = (vof 0 '(ldrop 2 (lapp (ncons 7 (ncons 9 nnil)) (ncons 4 nnil))))))

; --- the POSITIONAL model: a state is an nlist, a hunk (mkh i o n) aims o->n
; --- at index i; splice is take/append/drop, so hap's first layer is
; --- DEFINITIONAL over the seams. commute's tests ride natgthorleh -- the
; --- dichotomy carries the bool-path evidence into every branch, the same
; --- convoy the named-slot slice built on isdeceqnat. refusal (overlap OR
; --- touch: strict, the darcs seam) is the unit summand ---
(assert
 (defn hunk UU (total2 i nat (total2 o nlist nlist)))
 (defn mkh (pi i nat (pi o nlist (pi n nlist hunk)))
   (lam i (lam o (lam n (tpair i (tpair o n))))))
 (defn hi (pi p hunk nat) (lam p (pr1 p)))
 (defn ho (pi p hunk nlist) (lam p (pr1 (pr2 p))))
 (defn hn (pi p hunk nlist) (lam p (pr2 (pr2 p))))
 (defn splice (pi s nlist (pi i nat (pi c nat (pi n nlist nlist))))
   (lam s (lam i (lam c (lam n (lapp (ltake i s) (lapp n (ldrop (add i c) s))))))))
 (defn hap (pi s nlist (pi p hunk nlist))
   (lam s (lam p (splice s (hi p) (nlen (ho p)) (hn p)))))
 (defn hinv (pi p hunk hunk)
   (lam p (mkh (hi p) (hn p) (ho p))))
 (defn hpair UU (total2 x hunk hunk))
 (defn hcommute (pi p hunk (pi q hunk (coprod hpair unit)))
   (lam p (lam q
      (coprod_rect (lam c (coprod hpair unit))
        (lam g (ii1 (tpair (mkh (natminus (add (hi q) (nlen (ho p))) (nlen (hn p))) (ho q) (hn q)) p)))
        (lam g (coprod_rect (lam c2 (coprod hpair unit))
                 (lam g2 (ii1 (tpair q (mkh (natminus (add (hi p) (nlen (hn q))) (nlen (ho q))) (ho p) (hn p)))))
                 (lam g2 (ii2 tt))
                 (natgthorleh (hi p) (add (hi q) (nlen (ho q))))))
        (natgthorleh (hi q) (add (hi p) (nlen (hn p))))))))
 ; the model COMPUTES: splice replaces the middle, hap lays a hunk, commute
 ; re-aims past unequal lengths and refuses a touching seam
 ((vof 0 '(ncons 1 (ncons 7 (ncons 8 (ncons 3 nnil)))))
  = (vof 0 '(splice (ncons 1 (ncons 2 (ncons 3 nnil))) 1 1 (ncons 7 (ncons 8 nnil)))))
 ((vof 0 '(ncons 7 (ncons 8 (ncons 2 (ncons 3 nnil)))))
  = (vof 0 '(hap (ncons 1 (ncons 2 (ncons 3 nnil))) (mkh 0 (ncons 1 nnil) (ncons 7 (ncons 8 nnil))))))
 ((vof 0 '(hcommute (mkh 0 (ncons 1 nnil) (ncons 7 (ncons 8 nnil))) (mkh 3 (ncons 4 nnil) nnil)))
  = (vof 0 '(ii1 (tpair (mkh 2 (ncons 4 nnil) nnil) (mkh 0 (ncons 1 nnil) (ncons 7 (ncons 8 nnil)))))))
 ((vof 0 '(hcommute (mkh 0 (ncons 1 nnil) (ncons 7 (ncons 8 nnil))) (mkh 2 (ncons 3 nnil) nnil)))
  = (vof 0 '(ii2 tt))))

; --- (L1) hcommute is INVOLUTIVE, with the genuine index re-aim: the slid
; --- index slides home. the carried pair rides the same maponpaths trick as
; --- the named-slot slice; the arithmetic is the natgtbplusr cancellation
; --- through natminusplusnmm's recomposition, once per branch ---
(assert
 (defn hcmside (pi c (coprod hpair unit) UU)
   (lam c (coprod_rect (lam x UU) (lam x empty) (lam x unit) c)))
 (defq hcommute-involutive
   (pi p hunk (pi q hunk (pi q2 hunk (pi p2 hunk
      (pi h (paths (coprod hpair unit) (hcommute p q) (ii1 (tpair q2 p2)))
        (paths (coprod hpair unit) (hcommute q2 p2) (ii1 (tpair p q))))))))
   (lam p (lam q (lam q2 (lam p2
      (coprod_rect
        (lam c (pi h (paths (coprod hpair unit)
                 (coprod_rect (lam cc (coprod hpair unit))
                   (lam g9 (ii1 (tpair (mkh (natminus (add (hi q) (nlen (ho p))) (nlen (hn p))) (ho q) (hn q)) p)))
                   (lam g9 (coprod_rect (lam cd (coprod hpair unit))
                            (lam g8 (ii1 (tpair q (mkh (natminus (add (hi p) (nlen (hn q))) (nlen (ho q))) (ho p) (hn p)))))
                            (lam g8 (ii2 tt))
                            (natgthorleh (hi p) (add (hi q) (nlen (ho q))))))
                   c)
                 (ii1 (tpair q2 p2)))
               (paths (coprod hpair unit) (hcommute q2 p2) (ii1 (tpair p q)))))
        (lam g (lam h
          (let j2 (natminus (add (hi q) (nlen (ho p))) (nlen (hn p)))
          (let rr (natminusplusnmm (add (hi q) (nlen (ho p))) (nlen (hn p))
                    (istransnatleh (nlen (hn p)) (add (hi p) (nlen (hn p))) (add (hi q) (nlen (ho p)))
                      (natlehmplusnm (nlen (hn p)) (hi p))
                      (istransnatleh (add (hi p) (nlen (hn p))) (hi q) (add (hi q) (nlen (ho p)))
                        (natlthtoleh (add (hi p) (nlen (hn p))) (hi q) g)
                        (natlehnplusnm (hi q) (nlen (ho p))))))
          (let bfact (pathscomp0 bool
                       (natgtb j2 (add (hi p) (nlen (ho p))))
                       (natgtb (add j2 (nlen (hn p))) (add (add (hi p) (nlen (ho p))) (nlen (hn p))))
                       true
                       (pathsinv0 bool
                         (natgtb (add j2 (nlen (hn p))) (add (add (hi p) (nlen (ho p))) (nlen (hn p))))
                         (natgtb j2 (add (hi p) (nlen (ho p))))
                         (natgtbplusr j2 (add (hi p) (nlen (ho p))) (nlen (hn p))))
                       (pathscomp0 bool
                         (natgtb (add j2 (nlen (hn p))) (add (add (hi p) (nlen (ho p))) (nlen (hn p))))
                         (natgtb (add j2 (nlen (hn p))) (add (add (hi p) (nlen (hn p))) (nlen (ho p))))
                         true
                         (maponpaths nat bool (lam w (natgtb (add j2 (nlen (hn p))) w))
                           (add (add (hi p) (nlen (ho p))) (nlen (hn p))) (add (add (hi p) (nlen (hn p))) (nlen (ho p)))
                           (natplusswap (hi p) (nlen (ho p)) (nlen (hn p))))
                         (pathscomp0 bool
                           (natgtb (add j2 (nlen (hn p))) (add (add (hi p) (nlen (hn p))) (nlen (ho p))))
                           (natgtb (add (hi q) (nlen (ho p))) (add (add (hi p) (nlen (hn p))) (nlen (ho p))))
                           true
                           (maponpaths nat bool (lam w (natgtb w (add (add (hi p) (nlen (hn p))) (nlen (ho p)))))
                             (add j2 (nlen (hn p))) (add (hi q) (nlen (ho p))) rr)
                           (pathscomp0 bool
                             (natgtb (add (hi q) (nlen (ho p))) (add (add (hi p) (nlen (hn p))) (nlen (ho p))))
                             (natgtb (hi q) (add (hi p) (nlen (hn p))))
                             true
                             (natgtbplusr (hi q) (add (hi p) (nlen (hn p))) (nlen (ho p)))
                             g))))
          (let afact (istransnatleh (hi p) j2 (add j2 (nlen (hn q)))
                       (istransnatleh (hi p) (add (hi p) (nlen (ho p))) j2
                         (natlehnplusnm (hi p) (nlen (ho p)))
                         (natlthtoleh (add (hi p) (nlen (ho p))) j2 bfact))
                       (natlehnplusnm j2 (nlen (hn q))))
          (let cfact (pathscomp0 nat
                       (natminus (add j2 (nlen (hn p))) (nlen (ho p)))
                       (natminus (add (hi q) (nlen (ho p))) (nlen (ho p)))
                       (hi q)
                       (maponpaths nat nat (lam w (natminus w (nlen (ho p))))
                         (add j2 (nlen (hn p))) (add (hi q) (nlen (ho p))) rr)
                       (natpluscancelminus (hi q) (nlen (ho p))))
          (pathscomp0 (coprod hpair unit)
            (hcommute q2 p2)
            (hcommute (mkh j2 (ho q) (hn q)) p)
            (ii1 (tpair p q))
            (pathsinv0 (coprod hpair unit)
              (hcommute (mkh j2 (ho q) (hn q)) p)
              (hcommute q2 p2)
              (maponpaths (coprod hpair unit) (coprod hpair unit)
                (lam cz (coprod_rect (lam cy (coprod hpair unit)) (lam z (hcommute (pr1 z) (pr2 z))) (lam uz (ii2 tt)) cz))
                (ii1 (tpair (mkh j2 (ho q) (hn q)) p)) (ii1 (tpair q2 p2)) h))
            (coprod_rect
              (lam c3 (paths (coprod hpair unit)
                        (coprod_rect (lam cc (coprod hpair unit))
                          (lam g7 (ii1 (tpair (mkh (natminus (add (hi p) (nlen (ho q))) (nlen (hn q))) (ho p) (hn p)) (mkh j2 (ho q) (hn q)))))
                          (lam g7 (coprod_rect (lam c4 (coprod hpair unit))
                                   (lam g6 (ii1 (tpair p (mkh (natminus (add j2 (nlen (hn p))) (nlen (ho p))) (ho q) (hn q)))))
                                   (lam g6 (ii2 tt))
                                   (natgthorleh j2 (add (hi p) (nlen (ho p))))))
                          c3)
                        (ii1 (tpair p q))))
              (lam g7 (fromempty
                (paths (coprod hpair unit)
                  (ii1 (tpair (mkh (natminus (add (hi p) (nlen (ho q))) (nlen (hn q))) (ho p) (hn p)) (mkh j2 (ho q) (hn q))))
                  (ii1 (tpair p q)))
                (natlehtonegnatgth (hi p) (add j2 (nlen (hn q))) afact g7)))
              (lam g7
                (coprod_rect
                  (lam c4 (paths (coprod hpair unit)
                            (coprod_rect (lam cc (coprod hpair unit))
                              (lam g6 (ii1 (tpair p (mkh (natminus (add j2 (nlen (hn p))) (nlen (ho p))) (ho q) (hn q)))))
                              (lam g6 (ii2 tt))
                              c4)
                            (ii1 (tpair p q))))
                  (lam g6 (maponpaths nat (coprod hpair unit)
                            (lam w (ii1 (tpair p (mkh w (ho q) (hn q)))))
                            (natminus (add j2 (nlen (hn p))) (nlen (ho p))) (hi q) cfact))
                  (lam g6 (fromempty
                    (paths (coprod hpair unit) (ii2 tt) (ii1 (tpair p q)))
                    (natlehtonegnatgth j2 (add (hi p) (nlen (ho p))) g6 bfact)))
                  (natgthorleh j2 (add (hi p) (nlen (ho p))))))
              (natgthorleh (hi p) (add j2 (nlen (hn q)))))))))))))
        (lam g (lam h
          ((coprod_rect
             (lam c2 (pi h2 (paths (coprod hpair unit)
                       (coprod_rect (lam cc (coprod hpair unit))
                         (lam g8 (ii1 (tpair q (mkh (natminus (add (hi p) (nlen (hn q))) (nlen (ho q))) (ho p) (hn p)))))
                         (lam g8 (ii2 tt))
                         c2)
                       (ii1 (tpair q2 p2)))
                     (paths (coprod hpair unit) (hcommute q2 p2) (ii1 (tpair p q)))))
             (lam g2 (lam h2
               (let i2 (natminus (add (hi p) (nlen (hn q))) (nlen (ho q)))
               (let rr2 (natminusplusnmm (add (hi p) (nlen (hn q))) (nlen (ho q))
                          (istransnatleh (nlen (ho q)) (add (hi q) (nlen (ho q))) (add (hi p) (nlen (hn q)))
                            (natlehmplusnm (nlen (ho q)) (hi q))
                            (istransnatleh (add (hi q) (nlen (ho q))) (hi p) (add (hi p) (nlen (hn q)))
                              (natlthtoleh (add (hi q) (nlen (ho q))) (hi p) g2)
                              (natlehnplusnm (hi p) (nlen (hn q))))))
               (let b2fact (pathscomp0 bool
                             (natgtb i2 (add (hi q) (nlen (hn q))))
                             (natgtb (add i2 (nlen (ho q))) (add (add (hi q) (nlen (hn q))) (nlen (ho q))))
                             true
                             (pathsinv0 bool
                               (natgtb (add i2 (nlen (ho q))) (add (add (hi q) (nlen (hn q))) (nlen (ho q))))
                               (natgtb i2 (add (hi q) (nlen (hn q))))
                               (natgtbplusr i2 (add (hi q) (nlen (hn q))) (nlen (ho q))))
                             (pathscomp0 bool
                               (natgtb (add i2 (nlen (ho q))) (add (add (hi q) (nlen (hn q))) (nlen (ho q))))
                               (natgtb (add i2 (nlen (ho q))) (add (add (hi q) (nlen (ho q))) (nlen (hn q))))
                               true
                               (maponpaths nat bool (lam w (natgtb (add i2 (nlen (ho q))) w))
                                 (add (add (hi q) (nlen (hn q))) (nlen (ho q))) (add (add (hi q) (nlen (ho q))) (nlen (hn q)))
                                 (natplusswap (hi q) (nlen (hn q)) (nlen (ho q))))
                               (pathscomp0 bool
                                 (natgtb (add i2 (nlen (ho q))) (add (add (hi q) (nlen (ho q))) (nlen (hn q))))
                                 (natgtb (add (hi p) (nlen (hn q))) (add (add (hi q) (nlen (ho q))) (nlen (hn q))))
                                 true
                                 (maponpaths nat bool (lam w (natgtb w (add (add (hi q) (nlen (ho q))) (nlen (hn q)))))
                                   (add i2 (nlen (ho q))) (add (hi p) (nlen (hn q))) rr2)
                                 (pathscomp0 bool
                                   (natgtb (add (hi p) (nlen (hn q))) (add (add (hi q) (nlen (ho q))) (nlen (hn q))))
                                   (natgtb (hi p) (add (hi q) (nlen (ho q))))
                                   true
                                   (natgtbplusr (hi p) (add (hi q) (nlen (ho q))) (nlen (hn q)))
                                   g2))))
               (let c2fact (pathscomp0 nat
                             (natminus (add i2 (nlen (ho q))) (nlen (hn q)))
                             (natminus (add (hi p) (nlen (hn q))) (nlen (hn q)))
                             (hi p)
                             (maponpaths nat nat (lam w (natminus w (nlen (hn q))))
                               (add i2 (nlen (ho q))) (add (hi p) (nlen (hn q))) rr2)
                             (natpluscancelminus (hi p) (nlen (hn q))))
               (pathscomp0 (coprod hpair unit)
                 (hcommute q2 p2)
                 (hcommute q (mkh i2 (ho p) (hn p)))
                 (ii1 (tpair p q))
                 (pathsinv0 (coprod hpair unit)
                   (hcommute q (mkh i2 (ho p) (hn p)))
                   (hcommute q2 p2)
                   (maponpaths (coprod hpair unit) (coprod hpair unit)
                     (lam cz (coprod_rect (lam cy (coprod hpair unit)) (lam z (hcommute (pr1 z) (pr2 z))) (lam uz (ii2 tt)) cz))
                     (ii1 (tpair q (mkh i2 (ho p) (hn p)))) (ii1 (tpair q2 p2)) h2))
                 (coprod_rect
                   (lam c3 (paths (coprod hpair unit)
                             (coprod_rect (lam cc (coprod hpair unit))
                               (lam g7 (ii1 (tpair (mkh (natminus (add i2 (nlen (ho q))) (nlen (hn q))) (ho p) (hn p)) q)))
                               (lam g7 (coprod_rect (lam c4 (coprod hpair unit))
                                        (lam g6 (ii1 (tpair (mkh i2 (ho p) (hn p)) (mkh (natminus (add (hi q) (nlen (hn p))) (nlen (ho p))) (ho q) (hn q)))))
                                        (lam g6 (ii2 tt))
                                        (natgthorleh (hi q) (add i2 (nlen (ho p))))))
                               c3)
                             (ii1 (tpair p q))))
                   (lam g7 (maponpaths nat (coprod hpair unit)
                             (lam w (ii1 (tpair (mkh w (ho p) (hn p)) q)))
                             (natminus (add i2 (nlen (ho q))) (nlen (hn q))) (hi p) c2fact))
                   (lam g7 (fromempty
                     (paths (coprod hpair unit)
                       (coprod_rect (lam cc (coprod hpair unit))
                         (lam g6 (ii1 (tpair (mkh i2 (ho p) (hn p)) (mkh (natminus (add (hi q) (nlen (hn p))) (nlen (ho p))) (ho q) (hn q)))))
                         (lam g6 (ii2 tt))
                         (natgthorleh (hi q) (add i2 (nlen (ho p)))))
                       (ii1 (tpair p q)))
                     (natlehtonegnatgth i2 (add (hi q) (nlen (hn q))) g7 b2fact)))
                   (natgthorleh i2 (add (hi q) (nlen (hn q))))))))))))
             (lam g2 (lam h2 (fromempty
               (paths (coprod hpair unit) (hcommute q2 p2) (ii1 (tpair p q)))
               (transportf (coprod hpair unit) hcmside (ii2 tt) (ii1 (tpair q2 p2)) h2 tt))))
             (natgthorleh (hi p) (add (hi q) (nlen (ho q)))))
           h)))
        (natgthorleh (hi q) (add (hi p) (nlen (hn p)))))))))))

; --- (L2) the INVERSE round-trips IN CONTEXT: in range (the bound is what
; --- keeps an out-of-range insert honest -- its inverse would aim past the
; --- end) and matching, apply-then-unpull is the identity. splice is
; --- DEFINITIONALLY take/append/drop, so the whole proof is the seams
; --- decomposition s = A ++ o ++ W laid twice through the fusion kit ---
(assert
 (defq hinv-roundtrip
   (pi s nlist (pi p hunk
      (pi hb (paths bool (natgtb (add (hi p) (nlen (ho p))) (nlen s)) false)
        (pi hc (paths nlist (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p))
          (paths nlist (hap (hap s p) (hinv p)) s)))))
   (lam s (lam p (lam hb (lam hc
      (let A (ltake (hi p) s)
      (let W (ldrop (add (hi p) (nlen (ho p))) s)
      (let HA (ltakelen (hi p) s
                (istransnatleh (hi p) (add (hi p) (nlen (ho p))) (nlen s)
                  (natlehnplusnm (hi p) (nlen (ho p))) hb))
      (let Hs (pathscomp0 nlist s (lapp A (ldrop (hi p) s)) (lapp A (lapp (ho p) W))
                (pathsinv0 nlist (lapp A (ldrop (hi p) s)) s (lapptakedrop (hi p) s))
                (maponpaths nlist nlist (lam t (lapp A t)) (ldrop (hi p) s) (lapp (ho p) W)
                  (pathscomp0 nlist (ldrop (hi p) s)
                    (lapp (ltake (nlen (ho p)) (ldrop (hi p) s)) (ldrop (nlen (ho p)) (ldrop (hi p) s)))
                    (lapp (ho p) W)
                    (pathsinv0 nlist
                      (lapp (ltake (nlen (ho p)) (ldrop (hi p) s)) (ldrop (nlen (ho p)) (ldrop (hi p) s)))
                      (ldrop (hi p) s)
                      (lapptakedrop (nlen (ho p)) (ldrop (hi p) s)))
                    (pathscomp0 nlist
                      (lapp (ltake (nlen (ho p)) (ldrop (hi p) s)) (ldrop (nlen (ho p)) (ldrop (hi p) s)))
                      (lapp (ho p) (ldrop (nlen (ho p)) (ldrop (hi p) s)))
                      (lapp (ho p) W)
                      (maponpaths nlist nlist (lam t (lapp t (ldrop (nlen (ho p)) (ldrop (hi p) s))))
                        (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p) hc)
                      (maponpaths nlist nlist (lam t (lapp (ho p) t))
                        (ldrop (nlen (ho p)) (ldrop (hi p) s)) W (ldropdrop (hi p) (nlen (ho p)) s))))))
      (let T1 (pathscomp0 nlist
                (ltake (hi p) (lapp A (lapp (hn p) W)))
                (ltake (nlen A) (lapp A (lapp (hn p) W)))
                A
                (maponpaths nat nlist (lam w (ltake w (lapp A (lapp (hn p) W))))
                  (hi p) (nlen A) (pathsinv0 nat (nlen A) (hi p) HA))
                (ltakelenapp A (lapp (hn p) W)))
      (let T2 (pathscomp0 nlist
                (ldrop (add (hi p) (nlen (hn p))) (lapp A (lapp (hn p) W)))
                (ldrop (add (nlen A) (nlen (hn p))) (lapp A (lapp (hn p) W)))
                W
                (maponpaths nat nlist (lam w (ldrop (add w (nlen (hn p))) (lapp A (lapp (hn p) W))))
                  (hi p) (nlen A) (pathsinv0 nat (nlen A) (hi p) HA))
                (pathscomp0 nlist
                  (ldrop (add (nlen A) (nlen (hn p))) (lapp A (lapp (hn p) W)))
                  (ldrop (nlen (hn p)) (lapp (hn p) W))
                  W
                  (ldropplusapp A (nlen (hn p)) (lapp (hn p) W))
                  (ldroplenapp (hn p) W)))
      (pathscomp0 nlist
        (hap (hap s p) (hinv p))
        (lapp A (lapp (ho p) W))
        s
        (pathscomp0 nlist
          (lapp (ltake (hi p) (lapp A (lapp (hn p) W)))
                (lapp (ho p) (ldrop (add (hi p) (nlen (hn p))) (lapp A (lapp (hn p) W)))))
          (lapp A (lapp (ho p) (ldrop (add (hi p) (nlen (hn p))) (lapp A (lapp (hn p) W)))))
          (lapp A (lapp (ho p) W))
          (maponpaths nlist nlist
            (lam t (lapp t (lapp (ho p) (ldrop (add (hi p) (nlen (hn p))) (lapp A (lapp (hn p) W))))))
            (ltake (hi p) (lapp A (lapp (hn p) W))) A T1)
          (maponpaths nlist nlist (lam t (lapp A (lapp (ho p) t)))
            (ldrop (add (hi p) (nlen (hn p))) (lapp A (lapp (hn p) W))) W T2))
        (pathsinv0 nlist s (lapp A (lapp (ho p) W)) Hs)))))))))))))
 ; and it computes: lay a hunk, unpull it, the state is home
 ((vof 0 '(ncons 1 (ncons 2 (ncons 3 nnil))))
  = (vof 0 '(hap (hap (ncons 1 (ncons 2 (ncons 3 nnil))) (mkh 1 (ncons 2 nnil) (ncons 7 (ncons 8 nnil))))
                 (hinv (mkh 1 (ncons 2 nnil) (ncons 7 (ncons 8 nnil))))))))

; --- (L3a) the machinery, SEALED RUNG BY RUNG: a path-valued let poisons every
; --- later check in its scope (the eager NbE re-walks it superadditively --
; --- exactly why defq seals), so each intermediate path is its OWN defq and
; --- every use is a stuck neutral. seams is the one decomposition law all the
; --- slices lean on: a valid (i, o) window splits s as A ++ o ++ W ---
(assert
 (defq seams
   (pi s nlist (pi i nat (pi o nlist
      (pi hb (paths bool (natgtb (add i (nlen o)) (nlen s)) false)
      (pi hc (paths nlist (ltake (nlen o) (ldrop i s)) o)
        (paths nlist s (lapp (ltake i s) (lapp o (ldrop (add i (nlen o)) s)))))))))
   (lam s (lam i (lam o (lam hb (lam hc
      (pathscomp0 nlist s (lapp (ltake i s) (ldrop i s)) (lapp (ltake i s) (lapp o (ldrop (add i (nlen o)) s)))
        (pathsinv0 nlist (lapp (ltake i s) (ldrop i s)) s (lapptakedrop i s))
        (maponpaths nlist nlist (lam t (lapp (ltake i s) t)) (ldrop i s) (lapp o (ldrop (add i (nlen o)) s))
          (pathscomp0 nlist (ldrop i s)
            (lapp (ltake (nlen o) (ldrop i s)) (ldrop (nlen o) (ldrop i s)))
            (lapp o (ldrop (add i (nlen o)) s))
            (pathsinv0 nlist
              (lapp (ltake (nlen o) (ldrop i s)) (ldrop (nlen o) (ldrop i s)))
              (ldrop i s)
              (lapptakedrop (nlen o) (ldrop i s)))
            (pathscomp0 nlist
              (lapp (ltake (nlen o) (ldrop i s)) (ldrop (nlen o) (ldrop i s)))
              (lapp o (ldrop (nlen o) (ldrop i s)))
              (lapp o (ldrop (add i (nlen o)) s))
              (maponpaths nlist nlist (lam t (lapp t (ldrop (nlen o) (ldrop i s))))
                (ltake (nlen o) (ldrop i s)) o hc)
              (maponpaths nlist nlist (lam t (lapp o t))
                (ldrop (nlen o) (ldrop i s)) (ldrop (add i (nlen o)) s) (ldropdrop i (nlen o) s))))))))))))
 ; the AFTER branch's index ledger: with i + |n1| < j and the window in range,
 ; j sits |A| + (|n1| + kk) in from the patched state's seams (kk the gap)
 (defq after-j
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths nat j (add (nlen (ltake i s)) (add (nlen n1) (natminus j (add i (nlen n1)))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam g (lam hb1
      (let A (ltake i s)
      (let kk (natminus j (add i (nlen n1)))
      (pathscomp0 nat j (add i (add (nlen n1) kk)) (add (nlen A) (add (nlen n1) kk))
        (pathsinv0 nat (add i (add (nlen n1) kk)) j
          (pathscomp0 nat (add i (add (nlen n1) kk)) (add (add i (nlen n1)) kk) j
            (pathsinv0 nat (add (add i (nlen n1)) kk) (add i (add (nlen n1) kk))
              (natplusassoc i (nlen n1) kk))
            (pathscomp0 nat (add (add i (nlen n1)) kk) (add kk (add i (nlen n1))) j
              (natpluscomm (add i (nlen n1)) kk)
              (natminusplusnmm j (add i (nlen n1)) (natlthtoleh (add i (nlen n1)) j g)))))
        (maponpaths nat nat (lam w (add w (add (nlen n1) kk))) i (nlen A)
          (pathsinv0 nat (nlen A) i
            (ltakelen i s
              (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1)))))))))))))))
 ; dropping j off the patched state is dropping the gap off W
 (defq after-drop
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths nlist
          (ldrop j (lapp (ltake i s) (lapp n1 (ldrop (add i (nlen o1)) s))))
          (ldrop (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam g (lam hb1
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (pathscomp0 nlist
        (ldrop j (lapp A (lapp n1 W)))
        (ldrop (add (nlen A) (add (nlen n1) kk)) (lapp A (lapp n1 W)))
        (ldrop kk W)
        (maponpaths nat nlist (lam w (ldrop w (lapp A (lapp n1 W))))
          j (add (nlen A) (add (nlen n1) kk)) (after-j s i o1 n1 j g hb1))
        (pathscomp0 nlist
          (ldrop (add (nlen A) (add (nlen n1) kk)) (lapp A (lapp n1 W)))
          (ldrop (add (nlen n1) kk) (lapp n1 W))
          (ldrop kk W)
          (ldropplusapp A (add (nlen n1) kk) (lapp n1 W))
          (ldropplusapp n1 kk W))))))))))))))
 ; q's context, re-read at the gap
 (defq after-c2
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ltake (nlen o2) (ldrop (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s)))
          o2))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam g (lam hb1 (lam hc2
      (pathscomp0 nlist
        (ltake (nlen o2) (ldrop (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s)))
        (ltake (nlen o2) (ldrop j (lapp (ltake i s) (lapp n1 (ldrop (add i (nlen o1)) s)))))
        o2
        (maponpaths nlist nlist (lam t (ltake (nlen o2) t))
          (ldrop (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))
          (ldrop j (lapp (ltake i s) (lapp n1 (ldrop (add i (nlen o1)) s))))
          (pathsinv0 nlist
            (ldrop j (lapp (ltake i s) (lapp n1 (ldrop (add i (nlen o1)) s))))
            (ldrop (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))
            (after-drop s i o1 n1 j g hb1)))
        hc2)))))))))))
 ; q's window, in range past the gap
 (defq after-bk
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
        (paths bool
          (natgtb (add (natminus j (add i (nlen n1))) (nlen o2)) (nlen (ldrop (add i (nlen o1)) s)))
          false))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam g (lam hb1 (lam hb2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let HA (ltakelen i s
                (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1))
      (pathscomp0 bool
        (natgtb (add kk (nlen o2)) (nlen W))
        (natgtb (add (add i (nlen n1)) (add kk (nlen o2))) (add (add i (nlen n1)) (nlen W)))
        false
        (pathsinv0 bool
          (natgtb (add (add i (nlen n1)) (add kk (nlen o2))) (add (add i (nlen n1)) (nlen W)))
          (natgtb (add kk (nlen o2)) (nlen W))
          (natgtbplusl (add i (nlen n1)) (add kk (nlen o2)) (nlen W)))
        (pathscomp0 bool
          (natgtb (add (add i (nlen n1)) (add kk (nlen o2))) (add (add i (nlen n1)) (nlen W)))
          (natgtb (add j (nlen o2)) (add (add i (nlen n1)) (nlen W)))
          false
          (maponpaths nat bool (lam w (natgtb w (add (add i (nlen n1)) (nlen W))))
            (add (add i (nlen n1)) (add kk (nlen o2))) (add j (nlen o2))
            (pathscomp0 nat
              (add (add i (nlen n1)) (add kk (nlen o2)))
              (add (add (add i (nlen n1)) kk) (nlen o2))
              (add j (nlen o2))
              (pathsinv0 nat
                (add (add (add i (nlen n1)) kk) (nlen o2))
                (add (add i (nlen n1)) (add kk (nlen o2)))
                (natplusassoc (add i (nlen n1)) kk (nlen o2)))
              (maponpaths nat nat (lam w (add w (nlen o2)))
                (add (add i (nlen n1)) kk) j
                (pathscomp0 nat (add (add i (nlen n1)) kk) (add kk (add i (nlen n1))) j
                  (natpluscomm (add i (nlen n1)) kk)
                  (natminusplusnmm j (add i (nlen n1)) (natlthtoleh (add i (nlen n1)) j g))))))
          (pathscomp0 bool
            (natgtb (add j (nlen o2)) (add (add i (nlen n1)) (nlen W)))
            (natgtb (add j (nlen o2)) (nlen (lapp A (lapp n1 W))))
            false
            (maponpaths nat bool (lam w (natgtb (add j (nlen o2)) w))
              (add (add i (nlen n1)) (nlen W)) (nlen (lapp A (lapp n1 W)))
              (pathscomp0 nat
                (add (add i (nlen n1)) (nlen W))
                (add i (add (nlen n1) (nlen W)))
                (nlen (lapp A (lapp n1 W)))
                (natplusassoc i (nlen n1) (nlen W))
                (pathscomp0 nat
                  (add i (add (nlen n1) (nlen W)))
                  (add (nlen A) (add (nlen n1) (nlen W)))
                  (nlen (lapp A (lapp n1 W)))
                  (maponpaths nat nat (lam w (add w (add (nlen n1) (nlen W)))) i (nlen A)
                    (pathsinv0 nat (nlen A) i HA))
                  (pathsinv0 nat (nlen (lapp A (lapp n1 W))) (add (nlen A) (add (nlen n1) (nlen W)))
                    (pathscomp0 nat
                      (nlen (lapp A (lapp n1 W)))
                      (add (nlen A) (nlen (lapp n1 W)))
                      (add (nlen A) (add (nlen n1) (nlen W)))
                      (lapplen A (lapp n1 W))
                      (maponpaths nat nat (lam w (add (nlen A) w))
                        (nlen (lapp n1 W)) (add (nlen n1) (nlen W)) (lapplen n1 W)))))))
            hb2))))))))))))))))))

; --- the re-aimed index, located on the seams; then the four take/drop reads
; --- (the ORIGINAL order's two, the RE-AIMED order's two), the re-aimed
; --- state's normal form, and the assembly ---
(assert
 (defq after-j2v
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths nat
          (natminus (add j (nlen o1)) (nlen n1))
          (add (nlen (ltake i s)) (add (nlen o1) (natminus j (add i (nlen n1)))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam g (lam hb1
      (let A (ltake i s)
      (let kk (natminus j (add i (nlen n1)))
      (pathscomp0 nat
        (natminus (add j (nlen o1)) (nlen n1))
        (natminus (add (add (nlen A) (add (nlen o1) kk)) (nlen n1)) (nlen n1))
        (add (nlen A) (add (nlen o1) kk))
        (maponpaths nat nat (lam w (natminus w (nlen n1)))
          (add j (nlen o1)) (add (add (nlen A) (add (nlen o1) kk)) (nlen n1))
          (pathsinv0 nat (add (add (nlen A) (add (nlen o1) kk)) (nlen n1)) (add j (nlen o1))
            (pathscomp0 nat
              (add (add (nlen A) (add (nlen o1) kk)) (nlen n1))
              (add (add (add (nlen A) (nlen o1)) kk) (nlen n1))
              (add j (nlen o1))
              (maponpaths nat nat (lam w (add w (nlen n1)))
                (add (nlen A) (add (nlen o1) kk)) (add (add (nlen A) (nlen o1)) kk)
                (pathsinv0 nat (add (add (nlen A) (nlen o1)) kk) (add (nlen A) (add (nlen o1) kk))
                  (natplusassoc (nlen A) (nlen o1) kk)))
              (pathscomp0 nat
                (add (add (add (nlen A) (nlen o1)) kk) (nlen n1))
                (add (add (add (nlen A) (nlen o1)) (nlen n1)) kk)
                (add j (nlen o1))
                (natplusswap (add (nlen A) (nlen o1)) kk (nlen n1))
                (pathscomp0 nat
                  (add (add (add (nlen A) (nlen o1)) (nlen n1)) kk)
                  (add (add (add (nlen A) (nlen n1)) (nlen o1)) kk)
                  (add j (nlen o1))
                  (maponpaths nat nat (lam w (add w kk))
                    (add (add (nlen A) (nlen o1)) (nlen n1)) (add (add (nlen A) (nlen n1)) (nlen o1))
                    (natplusswap (nlen A) (nlen o1) (nlen n1)))
                  (pathscomp0 nat
                    (add (add (add (nlen A) (nlen n1)) (nlen o1)) kk)
                    (add (add (add (nlen A) (nlen n1)) kk) (nlen o1))
                    (add j (nlen o1))
                    (natplusswap (add (nlen A) (nlen n1)) (nlen o1) kk)
                    (pathscomp0 nat
                      (add (add (add (nlen A) (nlen n1)) kk) (nlen o1))
                      (add (add (nlen A) (add (nlen n1) kk)) (nlen o1))
                      (add j (nlen o1))
                      (maponpaths nat nat (lam w (add w (nlen o1)))
                        (add (add (nlen A) (nlen n1)) kk) (add (nlen A) (add (nlen n1) kk))
                        (natplusassoc (nlen A) (nlen n1) kk))
                      (maponpaths nat nat (lam w (add w (nlen o1)))
                        (add (nlen A) (add (nlen n1) kk)) j
                        (pathsinv0 nat j (add (nlen A) (add (nlen n1) kk))
                          (after-j s i o1 n1 j g hb1))))))))))
        (natpluscancelminus (add (nlen A) (add (nlen o1) kk)) (nlen n1)))))))))))))
 (defq after-u1
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ltake (natminus (add j (nlen o1)) (nlen n1)) s)
          (lapp (ltake i s) (lapp o1 (ltake (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam g (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let M (ltake kk W)
      (let Z (ldrop (add kk (nlen o2)) W)
      (pathscomp0 nlist
        (ltake (natminus (add j (nlen o1)) (nlen n1)) s)
        (ltake (add (nlen A) (add (nlen o1) kk)) s)
        (lapp A (lapp o1 M))
        (maponpaths nat nlist (lam w (ltake w s))
          (natminus (add j (nlen o1)) (nlen n1)) (add (nlen A) (add (nlen o1) kk))
          (after-j2v s i o1 n1 j g hb1))
        (pathscomp0 nlist
          (ltake (add (nlen A) (add (nlen o1) kk)) s)
          (ltake (add (nlen A) (add (nlen o1) kk)) (lapp A (lapp o1 (lapp M (lapp o2 Z)))))
          (lapp A (lapp o1 M))
          (maponpaths nlist nlist (lam t (ltake (add (nlen A) (add (nlen o1) kk)) t))
            s (lapp A (lapp o1 (lapp M (lapp o2 Z))))
            (pathscomp0 nlist s (lapp A (lapp o1 W)) (lapp A (lapp o1 (lapp M (lapp o2 Z))))
              (seams s i o1 hb1 hc1)
              (maponpaths nlist nlist (lam t (lapp A (lapp o1 t))) W (lapp M (lapp o2 Z))
                (seams W kk o2
                  (after-bk s i o1 n1 j o2 g hb1 hb2)
                  (after-c2 s i o1 n1 j o2 g hb1 hc2)))))
          (pathscomp0 nlist
            (ltake (add (nlen A) (add (nlen o1) kk)) (lapp A (lapp o1 (lapp M (lapp o2 Z)))))
            (lapp A (ltake (add (nlen o1) kk) (lapp o1 (lapp M (lapp o2 Z)))))
            (lapp A (lapp o1 M))
            (ltakeplusapp A (add (nlen o1) kk) (lapp o1 (lapp M (lapp o2 Z))))
            (pathscomp0 nlist
              (lapp A (ltake (add (nlen o1) kk) (lapp o1 (lapp M (lapp o2 Z)))))
              (lapp A (lapp o1 (ltake kk (lapp M (lapp o2 Z)))))
              (lapp A (lapp o1 M))
              (maponpaths nlist nlist (lam t (lapp A t))
                (ltake (add (nlen o1) kk) (lapp o1 (lapp M (lapp o2 Z))))
                (lapp o1 (ltake kk (lapp M (lapp o2 Z))))
                (ltakeplusapp o1 kk (lapp M (lapp o2 Z))))
              (pathscomp0 nlist
                (lapp A (lapp o1 (ltake kk (lapp M (lapp o2 Z)))))
                (lapp A (lapp o1 (ltake (nlen M) (lapp M (lapp o2 Z)))))
                (lapp A (lapp o1 M))
                (maponpaths nat nlist (lam w (lapp A (lapp o1 (ltake w (lapp M (lapp o2 Z))))))
                  kk (nlen M)
                  (pathsinv0 nat (nlen M) kk
                    (ltakelen kk W
                      (istransnatleh kk (add kk (nlen o2)) (nlen W) (natlehnplusnm kk (nlen o2))
                        (after-bk s i o1 n1 j o2 g hb1 hb2)))))
                (maponpaths nlist nlist (lam t (lapp A (lapp o1 t)))
                  (ltake (nlen M) (lapp M (lapp o2 Z))) M
                  (ltakelenapp M (lapp o2 Z)))))))))))))))))))))))))
 (defq after-u2
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ldrop (add (natminus (add j (nlen o1)) (nlen n1)) (nlen o2)) s)
          (ldrop (add (natminus j (add i (nlen n1))) (nlen o2)) (ldrop (add i (nlen o1)) s))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam g (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let M (ltake kk W)
      (let Z (ldrop (add kk (nlen o2)) W)
      (pathscomp0 nlist
        (ldrop (add (natminus (add j (nlen o1)) (nlen n1)) (nlen o2)) s)
        (ldrop (add (nlen A) (add (nlen o1) (add kk (nlen o2)))) s)
        Z
        (maponpaths nat nlist (lam w (ldrop w s))
          (add (natminus (add j (nlen o1)) (nlen n1)) (nlen o2))
          (add (nlen A) (add (nlen o1) (add kk (nlen o2))))
          (pathscomp0 nat
            (add (natminus (add j (nlen o1)) (nlen n1)) (nlen o2))
            (add (add (nlen A) (add (nlen o1) kk)) (nlen o2))
            (add (nlen A) (add (nlen o1) (add kk (nlen o2))))
            (maponpaths nat nat (lam w (add w (nlen o2)))
              (natminus (add j (nlen o1)) (nlen n1)) (add (nlen A) (add (nlen o1) kk))
              (after-j2v s i o1 n1 j g hb1))
            (pathscomp0 nat
              (add (add (nlen A) (add (nlen o1) kk)) (nlen o2))
              (add (nlen A) (add (add (nlen o1) kk) (nlen o2)))
              (add (nlen A) (add (nlen o1) (add kk (nlen o2))))
              (natplusassoc (nlen A) (add (nlen o1) kk) (nlen o2))
              (maponpaths nat nat (lam w (add (nlen A) w))
                (add (add (nlen o1) kk) (nlen o2)) (add (nlen o1) (add kk (nlen o2)))
                (natplusassoc (nlen o1) kk (nlen o2))))))
        (pathscomp0 nlist
          (ldrop (add (nlen A) (add (nlen o1) (add kk (nlen o2)))) s)
          (ldrop (add (nlen A) (add (nlen o1) (add kk (nlen o2)))) (lapp A (lapp o1 (lapp M (lapp o2 Z)))))
          Z
          (maponpaths nlist nlist (lam t (ldrop (add (nlen A) (add (nlen o1) (add kk (nlen o2)))) t))
            s (lapp A (lapp o1 (lapp M (lapp o2 Z))))
            (pathscomp0 nlist s (lapp A (lapp o1 W)) (lapp A (lapp o1 (lapp M (lapp o2 Z))))
              (seams s i o1 hb1 hc1)
              (maponpaths nlist nlist (lam t (lapp A (lapp o1 t))) W (lapp M (lapp o2 Z))
                (seams W kk o2
                  (after-bk s i o1 n1 j o2 g hb1 hb2)
                  (after-c2 s i o1 n1 j o2 g hb1 hc2)))))
          (pathscomp0 nlist
            (ldrop (add (nlen A) (add (nlen o1) (add kk (nlen o2)))) (lapp A (lapp o1 (lapp M (lapp o2 Z)))))
            (ldrop (add (nlen o1) (add kk (nlen o2))) (lapp o1 (lapp M (lapp o2 Z))))
            Z
            (ldropplusapp A (add (nlen o1) (add kk (nlen o2))) (lapp o1 (lapp M (lapp o2 Z))))
            (pathscomp0 nlist
              (ldrop (add (nlen o1) (add kk (nlen o2))) (lapp o1 (lapp M (lapp o2 Z))))
              (ldrop (add kk (nlen o2)) (lapp M (lapp o2 Z)))
              Z
              (ldropplusapp o1 (add kk (nlen o2)) (lapp M (lapp o2 Z)))
              (pathscomp0 nlist
                (ldrop (add kk (nlen o2)) (lapp M (lapp o2 Z)))
                (ldrop (add (nlen M) (nlen o2)) (lapp M (lapp o2 Z)))
                Z
                (maponpaths nat nlist (lam w (ldrop (add w (nlen o2)) (lapp M (lapp o2 Z))))
                  kk (nlen M)
                  (pathsinv0 nat (nlen M) kk
                    (ltakelen kk W
                      (istransnatleh kk (add kk (nlen o2)) (nlen W) (natlehnplusnm kk (nlen o2))
                        (after-bk s i o1 n1 j o2 g hb1 hb2)))))
                (pathscomp0 nlist
                  (ldrop (add (nlen M) (nlen o2)) (lapp M (lapp o2 Z)))
                  (ldrop (nlen o2) (lapp o2 Z))
                  Z
                  (ldropplusapp M (nlen o2) (lapp o2 Z))
                  (ldroplenapp o2 Z))))))))))))))))))))))))
 (defq after-l1p
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths nlist
          (ltake j (lapp (ltake i s) (lapp n1 (ldrop (add i (nlen o1)) s))))
          (lapp (ltake i s) (lapp n1 (ltake (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam g (lam hb1
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (pathscomp0 nlist
        (ltake j (lapp A (lapp n1 W)))
        (ltake (add (nlen A) (add (nlen n1) kk)) (lapp A (lapp n1 W)))
        (lapp A (lapp n1 (ltake kk W)))
        (maponpaths nat nlist (lam w (ltake w (lapp A (lapp n1 W))))
          j (add (nlen A) (add (nlen n1) kk)) (after-j s i o1 n1 j g hb1))
        (pathscomp0 nlist
          (ltake (add (nlen A) (add (nlen n1) kk)) (lapp A (lapp n1 W)))
          (lapp A (ltake (add (nlen n1) kk) (lapp n1 W)))
          (lapp A (lapp n1 (ltake kk W)))
          (ltakeplusapp A (add (nlen n1) kk) (lapp n1 W))
          (maponpaths nlist nlist (lam t (lapp A t))
            (ltake (add (nlen n1) kk) (lapp n1 W)) (lapp n1 (ltake kk W))
            (ltakeplusapp n1 kk W)))))))))))))))
 (defq after-l2p
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths nlist
          (ldrop (add j (nlen o2)) (lapp (ltake i s) (lapp n1 (ldrop (add i (nlen o1)) s))))
          (ldrop (add (natminus j (add i (nlen n1))) (nlen o2)) (ldrop (add i (nlen o1)) s)))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam g (lam hb1
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (pathscomp0 nlist
        (ldrop (add j (nlen o2)) (lapp A (lapp n1 W)))
        (ldrop (add (nlen A) (add (nlen n1) (add kk (nlen o2)))) (lapp A (lapp n1 W)))
        (ldrop (add kk (nlen o2)) W)
        (maponpaths nat nlist (lam w (ldrop w (lapp A (lapp n1 W))))
          (add j (nlen o2))
          (add (nlen A) (add (nlen n1) (add kk (nlen o2))))
          (pathscomp0 nat
            (add j (nlen o2))
            (add (add (nlen A) (add (nlen n1) kk)) (nlen o2))
            (add (nlen A) (add (nlen n1) (add kk (nlen o2))))
            (maponpaths nat nat (lam w (add w (nlen o2)))
              j (add (nlen A) (add (nlen n1) kk)) (after-j s i o1 n1 j g hb1))
            (pathscomp0 nat
              (add (add (nlen A) (add (nlen n1) kk)) (nlen o2))
              (add (nlen A) (add (add (nlen n1) kk) (nlen o2)))
              (add (nlen A) (add (nlen n1) (add kk (nlen o2))))
              (natplusassoc (nlen A) (add (nlen n1) kk) (nlen o2))
              (maponpaths nat nat (lam w (add (nlen A) w))
                (add (add (nlen n1) kk) (nlen o2)) (add (nlen n1) (add kk (nlen o2)))
                (natplusassoc (nlen n1) kk (nlen o2))))))
        (pathscomp0 nlist
          (ldrop (add (nlen A) (add (nlen n1) (add kk (nlen o2)))) (lapp A (lapp n1 W)))
          (ldrop (add (nlen n1) (add kk (nlen o2))) (lapp n1 W))
          (ldrop (add kk (nlen o2)) W)
          (ldropplusapp A (add (nlen n1) (add kk (nlen o2))) (lapp n1 W))
          (ldropplusapp n1 (add kk (nlen o2)) W))))))))))))))))

; --- the re-aimed state's normal form, its two reads at p's window, and the
; --- assembly: both orders land A ++ n1 ++ M ++ n2 ++ Z ---
(assert
 (defq after-nf2
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (splice s (natminus (add j (nlen o1)) (nlen n1)) (nlen o2) n2)
          (lapp (ltake i s)
                (lapp o1 (lapp (ltake (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))
                              (lapp n2 (ldrop (add (natminus j (add i (nlen n1))) (nlen o2)) (ldrop (add i (nlen o1)) s)))))))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let M (ltake kk W)
      (let Z (ldrop (add kk (nlen o2)) W)
      (let j2 (natminus (add j (nlen o1)) (nlen n1))
      (pathscomp0 nlist
        (splice s j2 (nlen o2) n2)
        (lapp (lapp A (lapp o1 M)) (lapp n2 Z))
        (lapp A (lapp o1 (lapp M (lapp n2 Z))))
        (pathscomp0 nlist
          (lapp (ltake j2 s) (lapp n2 (ldrop (add j2 (nlen o2)) s)))
          (lapp (lapp A (lapp o1 M)) (lapp n2 (ldrop (add j2 (nlen o2)) s)))
          (lapp (lapp A (lapp o1 M)) (lapp n2 Z))
          (maponpaths nlist nlist (lam t (lapp t (lapp n2 (ldrop (add j2 (nlen o2)) s))))
            (ltake j2 s) (lapp A (lapp o1 M))
            (after-u1 s i o1 n1 j o2 g hb1 hc1 hb2 hc2))
          (maponpaths nlist nlist (lam t (lapp (lapp A (lapp o1 M)) (lapp n2 t)))
            (ldrop (add j2 (nlen o2)) s) Z
            (after-u2 s i o1 n1 j o2 g hb1 hc1 hb2 hc2)))
        (pathscomp0 nlist
          (lapp (lapp A (lapp o1 M)) (lapp n2 Z))
          (lapp A (lapp (lapp o1 M) (lapp n2 Z)))
          (lapp A (lapp o1 (lapp M (lapp n2 Z))))
          (lappassoc A (lapp o1 M) (lapp n2 Z))
          (maponpaths nlist nlist (lam t (lapp A t))
            (lapp (lapp o1 M) (lapp n2 Z)) (lapp o1 (lapp M (lapp n2 Z)))
            (lappassoc o1 M (lapp n2 Z))))))))))))))))))))))))
 (defq after-v1
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ltake i (splice s (natminus (add j (nlen o1)) (nlen n1)) (nlen o2) n2))
          (ltake i s))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let M (ltake kk W)
      (let Z (ldrop (add kk (nlen o2)) W)
      (let j2 (natminus (add j (nlen o1)) (nlen n1))
      (pathscomp0 nlist
        (ltake i (splice s j2 (nlen o2) n2))
        (ltake (nlen A) (splice s j2 (nlen o2) n2))
        A
        (maponpaths nat nlist (lam w (ltake w (splice s j2 (nlen o2) n2)))
          i (nlen A)
          (pathsinv0 nat (nlen A) i
            (ltakelen i s
              (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1))))
        (pathscomp0 nlist
          (ltake (nlen A) (splice s j2 (nlen o2) n2))
          (ltake (nlen A) (lapp A (lapp o1 (lapp M (lapp n2 Z)))))
          A
          (maponpaths nlist nlist (lam t (ltake (nlen A) t))
            (splice s j2 (nlen o2) n2) (lapp A (lapp o1 (lapp M (lapp n2 Z))))
            (after-nf2 s i o1 n1 j o2 n2 g hb1 hc1 hb2 hc2))
          (ltakelenapp A (lapp o1 (lapp M (lapp n2 Z)))))))))))))))))))))))))
 (defq after-v2
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ldrop (add i (nlen o1)) (splice s (natminus (add j (nlen o1)) (nlen n1)) (nlen o2) n2))
          (lapp (ltake (natminus j (add i (nlen n1))) (ldrop (add i (nlen o1)) s))
                (lapp n2 (ldrop (add (natminus j (add i (nlen n1))) (nlen o2)) (ldrop (add i (nlen o1)) s)))))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let M (ltake kk W)
      (let Z (ldrop (add kk (nlen o2)) W)
      (let j2 (natminus (add j (nlen o1)) (nlen n1))
      (pathscomp0 nlist
        (ldrop (add i (nlen o1)) (splice s j2 (nlen o2) n2))
        (ldrop (add (nlen A) (nlen o1)) (splice s j2 (nlen o2) n2))
        (lapp M (lapp n2 Z))
        (maponpaths nat nlist (lam w (ldrop (add w (nlen o1)) (splice s j2 (nlen o2) n2)))
          i (nlen A)
          (pathsinv0 nat (nlen A) i
            (ltakelen i s
              (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1))))
        (pathscomp0 nlist
          (ldrop (add (nlen A) (nlen o1)) (splice s j2 (nlen o2) n2))
          (ldrop (add (nlen A) (nlen o1)) (lapp A (lapp o1 (lapp M (lapp n2 Z)))))
          (lapp M (lapp n2 Z))
          (maponpaths nlist nlist (lam t (ldrop (add (nlen A) (nlen o1)) t))
            (splice s j2 (nlen o2) n2) (lapp A (lapp o1 (lapp M (lapp n2 Z))))
            (after-nf2 s i o1 n1 j o2 n2 g hb1 hc1 hb2 hc2))
          (pathscomp0 nlist
            (ldrop (add (nlen A) (nlen o1)) (lapp A (lapp o1 (lapp M (lapp n2 Z)))))
            (ldrop (nlen o1) (lapp o1 (lapp M (lapp n2 Z))))
            (lapp M (lapp n2 Z))
            (ldropplusapp A (nlen o1) (lapp o1 (lapp M (lapp n2 Z))))
            (ldroplenapp o1 (lapp M (lapp n2 Z)))))))))))))))))))))))))
 (defq splice-after-comm
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g (paths bool (natgtb j (add i (nlen n1))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hb2 (paths bool (natgtb (add j (nlen o2)) (nlen (splice s i (nlen o1) n1))) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (splice (splice s i (nlen o1) n1) j (nlen o2) n2)
          (splice (splice s (natminus (add j (nlen o1)) (nlen n1)) (nlen o2) n2) i (nlen o1) n1))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let kk (natminus j (add i (nlen n1)))
      (let M (ltake kk W)
      (let Z (ldrop (add kk (nlen o2)) W)
      (let j2 (natminus (add j (nlen o1)) (nlen n1))
      (pathscomp0 nlist
        (splice (splice s i (nlen o1) n1) j (nlen o2) n2)
        (lapp A (lapp n1 (lapp M (lapp n2 Z))))
        (splice (splice s j2 (nlen o2) n2) i (nlen o1) n1)
        (pathscomp0 nlist
          (splice (splice s i (nlen o1) n1) j (nlen o2) n2)
          (lapp (lapp A (lapp n1 M)) (lapp n2 Z))
          (lapp A (lapp n1 (lapp M (lapp n2 Z))))
          (pathscomp0 nlist
            (lapp (ltake j (lapp A (lapp n1 W))) (lapp n2 (ldrop (add j (nlen o2)) (lapp A (lapp n1 W)))))
            (lapp (lapp A (lapp n1 M)) (lapp n2 (ldrop (add j (nlen o2)) (lapp A (lapp n1 W)))))
            (lapp (lapp A (lapp n1 M)) (lapp n2 Z))
            (maponpaths nlist nlist
              (lam t (lapp t (lapp n2 (ldrop (add j (nlen o2)) (lapp A (lapp n1 W))))))
              (ltake j (lapp A (lapp n1 W))) (lapp A (lapp n1 M))
              (after-l1p s i o1 n1 j g hb1))
            (maponpaths nlist nlist (lam t (lapp (lapp A (lapp n1 M)) (lapp n2 t)))
              (ldrop (add j (nlen o2)) (lapp A (lapp n1 W))) Z
              (after-l2p s i o1 n1 j o2 g hb1)))
          (pathscomp0 nlist
            (lapp (lapp A (lapp n1 M)) (lapp n2 Z))
            (lapp A (lapp (lapp n1 M) (lapp n2 Z)))
            (lapp A (lapp n1 (lapp M (lapp n2 Z))))
            (lappassoc A (lapp n1 M) (lapp n2 Z))
            (maponpaths nlist nlist (lam t (lapp A t))
              (lapp (lapp n1 M) (lapp n2 Z)) (lapp n1 (lapp M (lapp n2 Z)))
              (lappassoc n1 M (lapp n2 Z)))))
        (pathsinv0 nlist
          (splice (splice s j2 (nlen o2) n2) i (nlen o1) n1)
          (lapp A (lapp n1 (lapp M (lapp n2 Z))))
          (pathscomp0 nlist
            (lapp (ltake i (splice s j2 (nlen o2) n2)) (lapp n1 (ldrop (add i (nlen o1)) (splice s j2 (nlen o2) n2))))
            (lapp A (lapp n1 (ldrop (add i (nlen o1)) (splice s j2 (nlen o2) n2))))
            (lapp A (lapp n1 (lapp M (lapp n2 Z))))
            (maponpaths nlist nlist
              (lam t (lapp t (lapp n1 (ldrop (add i (nlen o1)) (splice s j2 (nlen o2) n2)))))
              (ltake i (splice s j2 (nlen o2) n2)) A
              (after-v1 s i o1 n1 j o2 n2 g hb1 hc1 hb2 hc2))
            (maponpaths nlist nlist (lam t (lapp A (lapp n1 t)))
              (ldrop (add i (nlen o1)) (splice s j2 (nlen o2) n2)) (lapp M (lapp n2 Z))
              (after-v2 s i o1 n1 j o2 n2 g hb1 hc1 hb2 hc2)))))))))))))))))))))))))

; --- the BEFORE branch: q aims strictly ahead of p, so p slides FORWARD by
; --- q's delta. the state decomposes B ++ o2 ++ C ++ o1 ++ W; the same sealed
; --- ladder, with A's own seams carrying the front half ---
(assert
 (defq before-leh-ja
   (pi s nlist (pi i nat (pi o1 nlist (pi j nat (pi o2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths bool (natgtb j (nlen (ltake i s))) false))))))))
   (lam s (lam i (lam o1 (lam j (lam o2 (lam g2 (lam hb1
      (pathscomp0 bool
        (natgtb j (nlen (ltake i s)))
        (natgtb j i)
        false
        (maponpaths nat bool (lam w (natgtb j w)) (nlen (ltake i s)) i
          (ltakelen i s
            (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1)))
        (istransnatleh j (add j (nlen o2)) i
          (natlehnplusnm j (nlen o2))
          (natlthtoleh (add j (nlen o2)) i g2)))))))))))
 (defq before-leh-jo
   (pi s nlist (pi i nat (pi o1 nlist (pi j nat (pi o2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths bool (natgtb (add j (nlen o2)) (nlen (ltake i s))) false))))))))
   (lam s (lam i (lam o1 (lam j (lam o2 (lam g2 (lam hb1
      (pathscomp0 bool
        (natgtb (add j (nlen o2)) (nlen (ltake i s)))
        (natgtb (add j (nlen o2)) i)
        false
        (maponpaths nat bool (lam w (natgtb (add j (nlen o2)) w)) (nlen (ltake i s)) i
          (ltakelen i s
            (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1)))
        (natlthtoleh (add j (nlen o2)) i g2))))))))))
 ; q's context re-read on A's front: peel the appended tail twice
 (defq before-hca
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist (ltake (nlen o2) (ldrop j (ltake i s))) o2))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam g2 (lam hb1 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (pathscomp0 nlist
        (ltake (nlen o2) (ldrop j A))
        (ltake (nlen o2) (lapp (ldrop j A) (lapp n1 W)))
        o2
        (pathsinv0 nlist
          (ltake (nlen o2) (lapp (ldrop j A) (lapp n1 W)))
          (ltake (nlen o2) (ldrop j A))
          (ltakeprefix (nlen o2) (ldrop j A) (lapp n1 W)
            (pathscomp0 bool
              (natgtb (nlen o2) (nlen (ldrop j A)))
              (natgtb (nlen o2) (natminus i j))
              false
              (maponpaths nat bool (lam w (natgtb (nlen o2) w))
                (nlen (ldrop j A)) (natminus i j)
                (pathscomp0 nat (nlen (ldrop j A)) (natminus (nlen A) j) (natminus i j)
                  (ldroplen j A)
                  (maponpaths nat nat (lam w (natminus w j)) (nlen A) i
                    (ltakelen i s
                      (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1)))))
              (pathscomp0 bool
                (natgtb (nlen o2) (natminus i j))
                (natgtb (natminus (add j (nlen o2)) j) (natminus i j))
                false
                (maponpaths nat bool (lam w (natgtb w (natminus i j)))
                  (nlen o2) (natminus (add j (nlen o2)) j)
                  (pathsinv0 nat (natminus (add j (nlen o2)) j) (nlen o2)
                    (natpluslcancelminus j (nlen o2))))
                (natlehandminusl (add j (nlen o2)) i j
                  (natlthtoleh (add j (nlen o2)) i g2))))))
        (pathscomp0 nlist
          (ltake (nlen o2) (lapp (ldrop j A) (lapp n1 W)))
          (ltake (nlen o2) (ldrop j (lapp A (lapp n1 W))))
          o2
          (maponpaths nlist nlist (lam t (ltake (nlen o2) t))
            (lapp (ldrop j A) (lapp n1 W))
            (ldrop j (lapp A (lapp n1 W)))
            (pathsinv0 nlist
              (ldrop j (lapp A (lapp n1 W)))
              (lapp (ldrop j A) (lapp n1 W))
              (ldropprefix j A (lapp n1 W)
                (before-leh-ja s i o1 j o2 g2 hb1))))
          hc2))))))))))))))
)

; --- the slid index, ALL NAT (tower lengths named by the guards' arithmetic,
; --- so conv never re-walks a take/drop tower); then the re-aimed state's
; --- normal form, its two reads, and the assembly ---
(assert
 (defq before-i2v
   (pi i nat (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
        (paths nat
          (natminus (add i (nlen n2)) (nlen o2))
          (add j (add (nlen n2) (natminus i (add j (nlen o2)))))))))))
   (lam i (lam j (lam o2 (lam n2 (lam g2
      (let cc (natminus i (add j (nlen o2)))
      (pathscomp0 nat
        (natminus (add i (nlen n2)) (nlen o2))
        (natminus (add (add j (add (nlen n2) cc)) (nlen o2)) (nlen o2))
        (add j (add (nlen n2) cc))
        (maponpaths nat nat (lam w (natminus w (nlen o2)))
          (add i (nlen n2)) (add (add j (add (nlen n2) cc)) (nlen o2))
          (pathsinv0 nat (add (add j (add (nlen n2) cc)) (nlen o2)) (add i (nlen n2))
            (pathscomp0 nat
              (add (add j (add (nlen n2) cc)) (nlen o2))
              (add (add (add j (nlen n2)) cc) (nlen o2))
              (add i (nlen n2))
              (maponpaths nat nat (lam w (add w (nlen o2)))
                (add j (add (nlen n2) cc)) (add (add j (nlen n2)) cc)
                (pathsinv0 nat (add (add j (nlen n2)) cc) (add j (add (nlen n2) cc))
                  (natplusassoc j (nlen n2) cc)))
              (pathscomp0 nat
                (add (add (add j (nlen n2)) cc) (nlen o2))
                (add (add (add j (nlen n2)) (nlen o2)) cc)
                (add i (nlen n2))
                (natplusswap (add j (nlen n2)) cc (nlen o2))
                (pathscomp0 nat
                  (add (add (add j (nlen n2)) (nlen o2)) cc)
                  (add (add (add j (nlen o2)) (nlen n2)) cc)
                  (add i (nlen n2))
                  (maponpaths nat nat (lam w (add w cc))
                    (add (add j (nlen n2)) (nlen o2)) (add (add j (nlen o2)) (nlen n2))
                    (natplusswap j (nlen n2) (nlen o2)))
                  (pathscomp0 nat
                    (add (add (add j (nlen o2)) (nlen n2)) cc)
                    (add (add (add j (nlen o2)) cc) (nlen n2))
                    (add i (nlen n2))
                    (natplusswap (add j (nlen o2)) (nlen n2) cc)
                    (maponpaths nat nat (lam w (add w (nlen n2)))
                      (add (add j (nlen o2)) cc) i
                      (pathscomp0 nat (add (add j (nlen o2)) cc) (add cc (add j (nlen o2))) i
                        (natpluscomm (add j (nlen o2)) cc)
                        (natminusplusnmm i (add j (nlen o2))
                          (natlthtoleh (add j (nlen o2)) i g2))))))))))
        (natpluscancelminus (add j (add (nlen n2) cc)) (nlen o2))))))))))
 (defq before-nfs2
   (pi s nlist (pi i nat (pi o1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
        (paths nlist
          (splice s j (nlen o2) n2)
          (lapp (ltake j (ltake i s))
                (lapp n2 (lapp (ldrop (add j (nlen o2)) (ltake i s))
                              (lapp o1 (ldrop (add i (nlen o1)) s)))))))))))))))
   (lam s (lam i (lam o1 (lam j (lam o2 (lam n2 (lam g2 (lam hb1 (lam hc1
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let B (ltake j A)
      (let C (ldrop (add j (nlen o2)) A)
      (pathscomp0 nlist
        (lapp (ltake j s) (lapp n2 (ldrop (add j (nlen o2)) s)))
        (lapp B (lapp n2 (ldrop (add j (nlen o2)) s)))
        (lapp B (lapp n2 (lapp C (lapp o1 W))))
        (maponpaths nlist nlist (lam t (lapp t (lapp n2 (ldrop (add j (nlen o2)) s))))
          (ltake j s) B
          (pathscomp0 nlist (ltake j s) (ltake j (lapp A (lapp o1 W))) B
            (maponpaths nlist nlist (lam t (ltake j t)) s (lapp A (lapp o1 W))
              (seams s i o1 hb1 hc1))
            (ltakeprefix j A (lapp o1 W)
              (before-leh-ja s i o1 j o2 g2 hb1))))
        (maponpaths nlist nlist (lam t (lapp B (lapp n2 t)))
          (ldrop (add j (nlen o2)) s) (lapp C (lapp o1 W))
          (pathscomp0 nlist
            (ldrop (add j (nlen o2)) s)
            (ldrop (add j (nlen o2)) (lapp A (lapp o1 W)))
            (lapp C (lapp o1 W))
            (maponpaths nlist nlist (lam t (ldrop (add j (nlen o2)) t)) s (lapp A (lapp o1 W))
              (seams s i o1 hb1 hc1))
            (ldropprefix (add j (nlen o2)) A (lapp o1 W)
              (before-leh-jo s i o1 j o2 g2 hb1)))))))))))))))))))
 (defq before-clen
   (pi s nlist (pi i nat (pi o1 nlist (pi j nat (pi o2 nlist
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
        (paths nat
          (nlen (ldrop (add j (nlen o2)) (ltake i s)))
          (natminus i (add j (nlen o2))))))))))
   (lam s (lam i (lam o1 (lam j (lam o2 (lam hb1
      (pathscomp0 nat
        (nlen (ldrop (add j (nlen o2)) (ltake i s)))
        (natminus (nlen (ltake i s)) (add j (nlen o2)))
        (natminus i (add j (nlen o2)))
        (ldroplen (add j (nlen o2)) (ltake i s))
        (maponpaths nat nat (lam w (natminus w (add j (nlen o2)))) (nlen (ltake i s)) i
          (ltakelen i s
            (istransnatleh i (add i (nlen o1)) (nlen s) (natlehnplusnm i (nlen o1)) hb1)))))))))))
 (defq before-v1
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ltake (natminus (add i (nlen n2)) (nlen o2)) (splice s j (nlen o2) n2))
          (lapp (ltake j (ltake i s))
                (lapp n2 (ldrop (add j (nlen o2)) (ltake i s))))))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g2 (lam hb1 (lam hc1 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let B (ltake j A)
      (let C (ldrop (add j (nlen o2)) A)
      (let cc (natminus i (add j (nlen o2)))
      (pathscomp0 nlist
        (ltake (natminus (add i (nlen n2)) (nlen o2)) (splice s j (nlen o2) n2))
        (ltake (add j (add (nlen n2) cc)) (splice s j (nlen o2) n2))
        (lapp B (lapp n2 C))
        (maponpaths nat nlist (lam w (ltake w (splice s j (nlen o2) n2)))
          (natminus (add i (nlen n2)) (nlen o2)) (add j (add (nlen n2) cc))
          (before-i2v i j o2 n2 g2))
        (pathscomp0 nlist
          (ltake (add j (add (nlen n2) cc)) (splice s j (nlen o2) n2))
          (ltake (add (nlen B) (add (nlen n2) (nlen C))) (splice s j (nlen o2) n2))
          (lapp B (lapp n2 C))
          (pathscomp0 nlist
            (ltake (add j (add (nlen n2) cc)) (splice s j (nlen o2) n2))
            (ltake (add (nlen B) (add (nlen n2) cc)) (splice s j (nlen o2) n2))
            (ltake (add (nlen B) (add (nlen n2) (nlen C))) (splice s j (nlen o2) n2))
            (maponpaths nat nlist (lam w (ltake (add w (add (nlen n2) cc)) (splice s j (nlen o2) n2)))
              j (nlen B)
              (pathsinv0 nat (nlen B) j
                (ltakelen j A (before-leh-ja s i o1 j o2 g2 hb1))))
            (maponpaths nat nlist (lam w (ltake (add (nlen B) (add (nlen n2) w)) (splice s j (nlen o2) n2)))
              cc (nlen C)
              (pathsinv0 nat (nlen C) cc
                (before-clen s i o1 j o2 hb1))))
          (pathscomp0 nlist
            (ltake (add (nlen B) (add (nlen n2) (nlen C))) (splice s j (nlen o2) n2))
            (ltake (add (nlen B) (add (nlen n2) (nlen C))) (lapp B (lapp n2 (lapp C (lapp o1 W)))))
            (lapp B (lapp n2 C))
            (maponpaths nlist nlist (lam t (ltake (add (nlen B) (add (nlen n2) (nlen C))) t))
              (splice s j (nlen o2) n2) (lapp B (lapp n2 (lapp C (lapp o1 W))))
              (before-nfs2 s i o1 j o2 n2 g2 hb1 hc1))
            (pathscomp0 nlist
              (ltake (add (nlen B) (add (nlen n2) (nlen C))) (lapp B (lapp n2 (lapp C (lapp o1 W)))))
              (lapp B (ltake (add (nlen n2) (nlen C)) (lapp n2 (lapp C (lapp o1 W)))))
              (lapp B (lapp n2 C))
              (ltakeplusapp B (add (nlen n2) (nlen C)) (lapp n2 (lapp C (lapp o1 W))))
              (pathscomp0 nlist
                (lapp B (ltake (add (nlen n2) (nlen C)) (lapp n2 (lapp C (lapp o1 W)))))
                (lapp B (lapp n2 (ltake (nlen C) (lapp C (lapp o1 W)))))
                (lapp B (lapp n2 C))
                (maponpaths nlist nlist (lam t (lapp B t))
                  (ltake (add (nlen n2) (nlen C)) (lapp n2 (lapp C (lapp o1 W))))
                  (lapp n2 (ltake (nlen C) (lapp C (lapp o1 W))))
                  (ltakeplusapp n2 (nlen C) (lapp C (lapp o1 W))))
                (maponpaths nlist nlist (lam t (lapp B (lapp n2 t)))
                  (ltake (nlen C) (lapp C (lapp o1 W))) C
                  (ltakelenapp C (lapp o1 W)))))))))))))))))))))))))
 (defq before-v2
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (ldrop (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1)) (splice s j (nlen o2) n2))
          (ldrop (add i (nlen o1)) s)))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g2 (lam hb1 (lam hc1 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let B (ltake j A)
      (let C (ldrop (add j (nlen o2)) A)
      (let cc (natminus i (add j (nlen o2)))
      (pathscomp0 nlist
        (ldrop (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1)) (splice s j (nlen o2) n2))
        (ldrop (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1)))) (splice s j (nlen o2) n2))
        W
        (maponpaths nat nlist (lam w (ldrop w (splice s j (nlen o2) n2)))
          (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1))
          (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1))))
          (pathscomp0 nat
            (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1))
            (add (add j (add (nlen n2) cc)) (nlen o1))
            (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1))))
            (maponpaths nat nat (lam w (add w (nlen o1)))
              (natminus (add i (nlen n2)) (nlen o2)) (add j (add (nlen n2) cc))
              (before-i2v i j o2 n2 g2))
            (pathscomp0 nat
              (add (add j (add (nlen n2) cc)) (nlen o1))
              (add j (add (add (nlen n2) cc) (nlen o1)))
              (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1))))
              (natplusassoc j (add (nlen n2) cc) (nlen o1))
              (pathscomp0 nat
                (add j (add (add (nlen n2) cc) (nlen o1)))
                (add j (add (nlen n2) (add cc (nlen o1))))
                (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1))))
                (maponpaths nat nat (lam w (add j w))
                  (add (add (nlen n2) cc) (nlen o1)) (add (nlen n2) (add cc (nlen o1)))
                  (natplusassoc (nlen n2) cc (nlen o1)))
                (pathscomp0 nat
                  (add j (add (nlen n2) (add cc (nlen o1))))
                  (add (nlen B) (add (nlen n2) (add cc (nlen o1))))
                  (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1))))
                  (maponpaths nat nat (lam w (add w (add (nlen n2) (add cc (nlen o1)))))
                    j (nlen B)
                    (pathsinv0 nat (nlen B) j
                      (ltakelen j A (before-leh-ja s i o1 j o2 g2 hb1))))
                  (maponpaths nat nat (lam w (add (nlen B) (add (nlen n2) (add w (nlen o1)))))
                    cc (nlen C)
                    (pathsinv0 nat (nlen C) cc
                      (before-clen s i o1 j o2 hb1))))))))
        (pathscomp0 nlist
          (ldrop (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1)))) (splice s j (nlen o2) n2))
          (ldrop (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1)))) (lapp B (lapp n2 (lapp C (lapp o1 W)))))
          W
          (maponpaths nlist nlist (lam t (ldrop (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1)))) t))
            (splice s j (nlen o2) n2) (lapp B (lapp n2 (lapp C (lapp o1 W))))
            (before-nfs2 s i o1 j o2 n2 g2 hb1 hc1))
          (pathscomp0 nlist
            (ldrop (add (nlen B) (add (nlen n2) (add (nlen C) (nlen o1)))) (lapp B (lapp n2 (lapp C (lapp o1 W)))))
            (ldrop (add (nlen n2) (add (nlen C) (nlen o1))) (lapp n2 (lapp C (lapp o1 W))))
            W
            (ldropplusapp B (add (nlen n2) (add (nlen C) (nlen o1))) (lapp n2 (lapp C (lapp o1 W))))
            (pathscomp0 nlist
              (ldrop (add (nlen n2) (add (nlen C) (nlen o1))) (lapp n2 (lapp C (lapp o1 W))))
              (ldrop (add (nlen C) (nlen o1)) (lapp C (lapp o1 W)))
              W
              (ldropplusapp n2 (add (nlen C) (nlen o1)) (lapp C (lapp o1 W)))
              (pathscomp0 nlist
                (ldrop (add (nlen C) (nlen o1)) (lapp C (lapp o1 W)))
                (ldrop (nlen o1) (lapp o1 W))
                W
                (ldropplusapp C (nlen o1) (lapp o1 W))
                (ldroplenapp o1 W)))))))))))))))))))))))
 (defq splice-before-comm
   (pi s nlist (pi i nat (pi o1 nlist (pi n1 nlist (pi j nat (pi o2 nlist (pi n2 nlist
      (pi g2 (paths bool (natgtb i (add j (nlen o2))) true)
      (pi hb1 (paths bool (natgtb (add i (nlen o1)) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen o1) (ldrop i s)) o1)
      (pi hc2 (paths nlist (ltake (nlen o2) (ldrop j (splice s i (nlen o1) n1))) o2)
        (paths nlist
          (splice (splice s i (nlen o1) n1) j (nlen o2) n2)
          (splice (splice s j (nlen o2) n2) (natminus (add i (nlen n2)) (nlen o2)) (nlen o1) n1)))))))))))))
   (lam s (lam i (lam o1 (lam n1 (lam j (lam o2 (lam n2 (lam g2 (lam hb1 (lam hc1 (lam hc2
      (let A (ltake i s)
      (let W (ldrop (add i (nlen o1)) s)
      (let B (ltake j A)
      (let C (ldrop (add j (nlen o2)) A)
      (pathscomp0 nlist
        (splice (splice s i (nlen o1) n1) j (nlen o2) n2)
        (lapp B (lapp n2 (lapp C (lapp n1 W))))
        (splice (splice s j (nlen o2) n2) (natminus (add i (nlen n2)) (nlen o2)) (nlen o1) n1)
        (pathscomp0 nlist
          (lapp (ltake j (lapp A (lapp n1 W))) (lapp n2 (ldrop (add j (nlen o2)) (lapp A (lapp n1 W)))))
          (lapp B (lapp n2 (ldrop (add j (nlen o2)) (lapp A (lapp n1 W)))))
          (lapp B (lapp n2 (lapp C (lapp n1 W))))
          (maponpaths nlist nlist
            (lam t (lapp t (lapp n2 (ldrop (add j (nlen o2)) (lapp A (lapp n1 W))))))
            (ltake j (lapp A (lapp n1 W))) B
            (ltakeprefix j A (lapp n1 W)
              (before-leh-ja s i o1 j o2 g2 hb1)))
          (maponpaths nlist nlist (lam t (lapp B (lapp n2 t)))
            (ldrop (add j (nlen o2)) (lapp A (lapp n1 W))) (lapp C (lapp n1 W))
            (ldropprefix (add j (nlen o2)) A (lapp n1 W)
              (before-leh-jo s i o1 j o2 g2 hb1))))
        (pathsinv0 nlist
          (splice (splice s j (nlen o2) n2) (natminus (add i (nlen n2)) (nlen o2)) (nlen o1) n1)
          (lapp B (lapp n2 (lapp C (lapp n1 W))))
          (pathscomp0 nlist
            (splice (splice s j (nlen o2) n2) (natminus (add i (nlen n2)) (nlen o2)) (nlen o1) n1)
            (lapp (lapp B (lapp n2 C)) (lapp n1 W))
            (lapp B (lapp n2 (lapp C (lapp n1 W))))
            (pathscomp0 nlist
              (lapp (ltake (natminus (add i (nlen n2)) (nlen o2)) (splice s j (nlen o2) n2))
                    (lapp n1 (ldrop (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1)) (splice s j (nlen o2) n2))))
              (lapp (lapp B (lapp n2 C))
                    (lapp n1 (ldrop (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1)) (splice s j (nlen o2) n2))))
              (lapp (lapp B (lapp n2 C)) (lapp n1 W))
              (maponpaths nlist nlist
                (lam t (lapp t (lapp n1 (ldrop (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1)) (splice s j (nlen o2) n2)))))
                (ltake (natminus (add i (nlen n2)) (nlen o2)) (splice s j (nlen o2) n2))
                (lapp B (lapp n2 C))
                (before-v1 s i o1 n1 j o2 n2 g2 hb1 hc1 hc2))
              (maponpaths nlist nlist (lam t (lapp (lapp B (lapp n2 C)) (lapp n1 t)))
                (ldrop (add (natminus (add i (nlen n2)) (nlen o2)) (nlen o1)) (splice s j (nlen o2) n2))
                W
                (before-v2 s i o1 n1 j o2 n2 g2 hb1 hc1 hc2)))
            (pathscomp0 nlist
              (lapp (lapp B (lapp n2 C)) (lapp n1 W))
              (lapp B (lapp (lapp n2 C) (lapp n1 W)))
              (lapp B (lapp n2 (lapp C (lapp n1 W))))
              (lappassoc B (lapp n2 C) (lapp n1 W))
              (maponpaths nlist nlist (lam t (lapp B t))
                (lapp (lapp n2 C) (lapp n1 W)) (lapp n2 (lapp C (lapp n1 W)))
                (lappassoc n2 C (lapp n1 W))))))))))))))))))))))))

; --- (L3a) tied to the primitive: a successful hcommute WITNESSES order-
; --- freedom, in the honest contexts (p belongs on s, q belongs after p).
; --- the convoy reads the branch off natgthorleh's evidence and hands the
; --- carried pair through the same maponpaths as L1; each branch closes by
; --- its splice lemma. then (L3b): states are DATA, so the frontier is one
; --- maponpaths -- the fold's tail rides the two-patch agreement ---
(assert
 (defn hvec (pi n nat UU)
   (lam n (nat_rect (lam q UU) unit (lam k (lam r (total2 x hunk r))) n)))
 (defn hlist UU (total2 n nat (hvec n)))
 (defn hnil hlist (tpair 0 tt))
 (defn hcons (pi x hunk (pi l hlist hlist))
   (lam x (lam l (tpair (succ (pr1 l)) (tpair x (pr2 l))))))
 (defn happl (pi l hlist (pi s nlist nlist))
   (lam l (lam s ((nat_rect (lam q (pi v (hvec q) (pi s2 nlist nlist)))
                     (lam v (lam s2 s2))
                     (lam k (lam IH (lam v (lam s2 (IH (pr2 v) (hap s2 (pr1 v)))))))
                     (pr1 l))
                   (pr2 l) s))))
 ; the fold COMPUTES: two hunks land left to right
 ((vof 0 '(happl (hcons (mkh 0 (ncons 1 nnil) (ncons 7 (ncons 8 nnil)))
                        (hcons (mkh 3 (ncons 3 nnil) (ncons 9 nnil)) hnil))
                 (ncons 1 (ncons 2 (ncons 3 (ncons 4 nnil))))))
  = (vof 0 '(ncons 7 (ncons 8 (ncons 2 (ncons 9 (ncons 4 nnil))))))))
(assert
 (defq hcommute-sound
   (pi
     p
     hunk
     (pi
       q
       hunk
       (pi
         q2
         hunk
         (pi
           p2
           hunk
           (pi
             s
             nlist
             (pi
               h
               (paths (coprod hpair unit) (hcommute p q) (ii1 (tpair q2 p2)))
               (pi
                 hb1
                 (paths bool (natgtb (add (hi p) (nlen (ho p))) (nlen s)) false)
                 (pi
                   hc1
                   (paths nlist (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p))
                   (pi
                     hb2
                     (paths bool (natgtb (add (hi q) (nlen (ho q))) (nlen (hap s p))) false)
                     (pi
                       hc2
                       (paths nlist (ltake (nlen (ho q)) (ldrop (hi q) (hap s p))) (ho q))
                       (paths nlist (hap (hap s p) q) (hap (hap s q2) p2))))))))))))
   (lam
     p
     (lam
       q
       (lam
         q2
         (lam
           p2
           (lam
             s
             (coprod_rect
               (lam
                 c
                 (pi
                   h
                   (paths
                     (coprod hpair unit)
                     (coprod_rect
                       (lam cc (coprod hpair unit))
                       (lam
                         g9
                         (ii1
                           (tpair
                             (mkh (natminus (add (hi q) (nlen (ho p))) (nlen (hn p))) (ho q) (hn q))
                             p)))
                       (lam
                         g9
                         (coprod_rect
                           (lam cc (coprod hpair unit))
                           (lam
                             g8
                             (ii1
                               (tpair
                                 q
                                 (mkh
                                   (natminus (add (hi p) (nlen (hn q))) (nlen (ho q)))
                                   (ho p)
                                   (hn p)))))
                           (lam g8 (ii2 tt))
                           (natgthorleh (hi p) (add (hi q) (nlen (ho q))))))
                       c)
                     (ii1 (tpair q2 p2)))
                   (pi
                     hb1
                     (paths bool (natgtb (add (hi p) (nlen (ho p))) (nlen s)) false)
                     (pi
                       hc1
                       (paths nlist (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p))
                       (pi
                         hb2
                         (paths bool (natgtb (add (hi q) (nlen (ho q))) (nlen (hap s p))) false)
                         (pi
                           hc2
                           (paths nlist (ltake (nlen (ho q)) (ldrop (hi q) (hap s p))) (ho q))
                           (paths nlist (hap (hap s p) q) (hap (hap s q2) p2))))))))
               (lam
                 g
                 (lam
                   h
                   (lam
                     hb1
                     (lam
                       hc1
                       (lam
                         hb2
                         (lam
                           hc2
                           (pathscomp0
                             nlist
                             (hap (hap s p) q)
                             (hap
                               (hap
                                 s
                                 (mkh
                                   (natminus (add (hi q) (nlen (ho p))) (nlen (hn p)))
                                   (ho q)
                                   (hn q)))
                               p)
                             (hap (hap s q2) p2)
                             (splice-after-comm
                               s
                               (hi p)
                               (ho p)
                               (hn p)
                               (hi q)
                               (ho q)
                               (hn q)
                               g
                               hb1
                               hc1
                               hb2
                               hc2)
                             (maponpaths
                               (coprod hpair unit)
                               nlist
                               (lam
                                 cz
                                 (coprod_rect
                                   (lam cy nlist)
                                   (lam z (hap (hap s (pr1 z)) (pr2 z)))
                                   (lam uz nnil)
                                   cz))
                               (ii1
                                 (tpair
                                   (mkh
                                     (natminus (add (hi q) (nlen (ho p))) (nlen (hn p)))
                                     (ho q)
                                     (hn q))
                                   p))
                               (ii1 (tpair q2 p2))
                               h))))))))
               (lam
                 g
                 (lam
                   h
                   ((coprod_rect
                       (lam
                         c2
                         (pi
                           h2
                           (paths
                             (coprod hpair unit)
                             (coprod_rect
                               (lam cc (coprod hpair unit))
                               (lam
                                 g8
                                 (ii1
                                   (tpair
                                     q
                                     (mkh
                                       (natminus (add (hi p) (nlen (hn q))) (nlen (ho q)))
                                       (ho p)
                                       (hn p)))))
                               (lam g8 (ii2 tt))
                               c2)
                             (ii1 (tpair q2 p2)))
                           (pi
                             hb1
                             (paths bool (natgtb (add (hi p) (nlen (ho p))) (nlen s)) false)
                             (pi
                               hc1
                               (paths nlist (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p))
                               (pi
                                 hb2
                                 (paths
                                   bool
                                   (natgtb (add (hi q) (nlen (ho q))) (nlen (hap s p)))
                                   false)
                                 (pi
                                   hc2
                                   (paths
                                     nlist
                                     (ltake (nlen (ho q)) (ldrop (hi q) (hap s p)))
                                     (ho q))
                                   (paths nlist (hap (hap s p) q) (hap (hap s q2) p2))))))))
                       (lam
                         g2
                         (lam
                           h2
                           (lam
                             hb1
                             (lam
                               hc1
                               (lam
                                 hb2
                                 (lam
                                   hc2
                                   (pathscomp0
                                     nlist
                                     (hap (hap s p) q)
                                     (hap
                                       (hap s q)
                                       (mkh
                                         (natminus (add (hi p) (nlen (hn q))) (nlen (ho q)))
                                         (ho p)
                                         (hn p)))
                                     (hap (hap s q2) p2)
                                     (splice-before-comm
                                       s
                                       (hi p)
                                       (ho p)
                                       (hn p)
                                       (hi q)
                                       (ho q)
                                       (hn q)
                                       g2
                                       hb1
                                       hc1
                                       hc2)
                                     (maponpaths
                                       (coprod hpair unit)
                                       nlist
                                       (lam
                                         cz
                                         (coprod_rect
                                           (lam cy nlist)
                                           (lam z (hap (hap s (pr1 z)) (pr2 z)))
                                           (lam uz nnil)
                                           cz))
                                       (ii1
                                         (tpair
                                           q
                                           (mkh
                                             (natminus (add (hi p) (nlen (hn q))) (nlen (ho q)))
                                             (ho p)
                                             (hn p))))
                                       (ii1 (tpair q2 p2))
                                       h2))))))))
                       (lam
                         g2
                         (lam
                           h2
                           (fromempty
                             (pi
                               hb1
                               (paths bool (natgtb (add (hi p) (nlen (ho p))) (nlen s)) false)
                               (pi
                                 hc1
                                 (paths nlist (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p))
                                 (pi
                                   hb2
                                   (paths
                                     bool
                                     (natgtb (add (hi q) (nlen (ho q))) (nlen (hap s p)))
                                     false)
                                   (pi
                                     hc2
                                     (paths
                                       nlist
                                       (ltake (nlen (ho q)) (ldrop (hi q) (hap s p)))
                                       (ho q))
                                     (paths nlist (hap (hap s p) q) (hap (hap s q2) p2))))))
                             (transportf
                               (coprod hpair unit)
                               hcmside
                               (ii2 tt)
                               (ii1 (tpair q2 p2))
                               h2
                               tt))))
                       (natgthorleh (hi p) (add (hi q) (nlen (ho q)))))
                     h)))
               (natgthorleh (hi q) (add (hi p) (nlen (hn p))))))))))))
(assert
 (defq hfold-swap
   (pi p hunk (pi q hunk (pi q2 hunk (pi p2 hunk (pi s nlist (pi post hlist
      (pi h (paths (coprod hpair unit) (hcommute p q) (ii1 (tpair q2 p2)))
      (pi hb1 (paths bool (natgtb (add (hi p) (nlen (ho p))) (nlen s)) false)
      (pi hc1 (paths nlist (ltake (nlen (ho p)) (ldrop (hi p) s)) (ho p))
      (pi hb2 (paths bool (natgtb (add (hi q) (nlen (ho q))) (nlen (hap s p))) false)
      (pi hc2 (paths nlist (ltake (nlen (ho q)) (ldrop (hi q) (hap s p))) (ho q))
        (paths nlist
          (happl (hcons p (hcons q post)) s)
          (happl (hcons q2 (hcons p2 post)) s)))))))))))))
   (lam p (lam q (lam q2 (lam p2 (lam s (lam post (lam h (lam hb1 (lam hc1 (lam hb2 (lam hc2
      (maponpaths nlist nlist (lam t (happl post t))
        (hap (hap s p) q) (hap (hap s q2) p2)
        (hcommute-sound p q q2 p2 s h hb1 hc1 hb2 hc2)))))))))))))))
(:
 ; --- the L3a witness COMPUTES: the after-pair agrees in both orders ---
 (heq a b) (? (&& (atom? a) (atom? b)) (id? a b)
              (|| (atom? a) (atom? b)) 0
              (&& (heq (cap a) (cap b)) (heq (cup a) (cup b))))
 s0 '(ncons 1 (ncons 2 (ncons 3 (ncons 4 nnil))))
 p0 '(mkh 0 (ncons 1 nnil) (ncons 7 (ncons 8 nnil)))
 q0 '(mkh 3 (ncons 3 nnil) (ncons 9 nnil))
 q0p '(mkh 2 (ncons 3 nnil) (ncons 9 nnil))
 _ (assert
    ((vof 0 ['hap ['hap s0 p0] q0]) = (vof 0 ['hap ['hap s0 q0p] p0]))
    ((vof 0 ['hcommute p0 q0]) = (vof 0 ['ii1 ['tpair q0p p0]])))
 0)

; ============================================================================
; the ELEMENT TREE: every nat IS a rose tree. labels are nats and kid lists
; are finite, so trees are counted by the cantor pairing -- a node is
; (npair label kids-code), a kid list codes by folding npair under succ, and
; the ZERO-labelled leaf is 0 itself. etree = nat buys the whole nat kit at
; tree grain for free (isdeceq, isaset, natgtb order for canonical forms),
; kids come back as a plain nlist (the list kit rides), and the swal/clash/
; metavariable slices get their element grain. the door: doc/misc/proto/patch.l's
; rose-tree walks, queued behind this type.
; ============================================================================
; --- the cantor pairing. npair climbs the triangle; unpair WALKS the
; --- diagonal (a duo-valued nat_rect: at (0 b) jump to (succ b, 0), else
; --- step (npred a, succ b) -- una/unb the two step components, niszero/
; --- npred the case machinery, so every branch is definitional). ---
(assert
 (defn tri
   (pi n nat nat)
   (lam n (nat_rect (lam q nat) 0 (lam k (lam r (add (succ k) r))) n)))
 (defn npair
   (pi a nat (pi b nat nat))
   (lam a (lam b (add (tri (add a b)) b))))
 (defn duo
   UU
   (total2 a nat nat))
 (defn una
   (pi x nat (pi y nat nat))
   (lam x (lam y (bool_rect (lam c nat) (succ y) (npred x) (niszero x)))))
 (defn unb
   (pi x nat (pi y nat nat))
   (lam x (lam y (bool_rect (lam c nat) 0 (succ y) (niszero x)))))
 (defn unpair
   (pi n nat duo)
   (lam n
      (nat_rect (lam q duo) (tpair 0 0)
        (lam k (lam u (tpair (una (pr1 u) (pr2 u)) (unb (pr1 u) (pr2 u)))))
        n)))
 (defn upa
   (pi n nat nat)
   (lam n (pr1 (unpair n))))
 (defn upb
   (pi n nat nat)
   (lam n (pr2 (unpair n))))
 ; the pairing computes: the diagonal walk inverts the triangle climb
 (id? (vof 0 '0) (vof 0 '(npair 0 0)))
 (id? (vof 0 '1) (vof 0 '(npair 1 0)))
 (id? (vof 0 '2) (vof 0 '(npair 0 1)))
 (id? (vof 0 '4) (vof 0 '(upa (npair 4 2))))
 (id? (vof 0 '2) (vof 0 '(upb (npair 4 2)))))

; --- the walk inverts the climb, one diagonal step at a time. npairhead:
; --- the (0 b) corner jumps to the next diagonal's head. npairdiag: an
; --- interior step slides down it. npairstep fuses them through una/unb
; --- (each branch definitional), npairunpair closes by one induction. ---
(assert
 (defq npairhead
   (pi b nat (paths nat (npair (succ b) 0) (succ (npair 0 b))))
   (lam b
      (maponpaths nat nat (lam w (succ w)) (add (add (add b 0) (tri (add b 0))) 0)
        (add (tri b) b)
        (pathscomp0 nat (add (add (add b 0) (tri (add b 0))) 0) (add (add b 0) (tri (add b 0)))
          (add (tri b) b)
          (natplusr0 (add (add b 0) (tri (add b 0))))
          (pathscomp0 nat (add (add b 0) (tri (add b 0))) (add b (tri b)) (add (tri b) b)
            (maponpaths nat nat (lam w (add w (tri w))) (add b 0) b (natplusr0 b))
            (natpluscomm b (tri b)))))))
 (defq npairdiag
   (pi a nat (pi b nat (paths nat (npair a (succ b)) (succ (npair (succ a) b)))))
   (lam a
      (lam b
        (pathscomp0 nat (npair a (succ b)) (add (tri (add (succ a) b)) (succ b))
          (succ (npair (succ a) b))
          (maponpaths nat nat (lam w (add (tri w) (succ b))) (add a (succ b)) (add (succ a) b)
            (natplusnsm a b))
          (maponpaths nat nat (lam w (succ w)) (add (add (add a b) (tri (add a b))) (succ b))
            (add (succ (add (add a b) (tri (add a b)))) b)
            (natplusnsm (add (add a b) (tri (add a b))) b))))))
 (defq npairstep
   (pi x nat (pi y nat (paths nat (npair (una x y) (unb x y)) (succ (npair x y)))))
   (lam x
      (lam y
        (nat_rect (lam q (paths nat (npair (una q y) (unb q y)) (succ (npair q y))))
          (npairhead y)
          (lam k (lam IH (npairdiag k y)))
          x))))
 (defq npairunpair
   (pi n nat (paths nat (npair (upa n) (upb n)) n))
   (lam n
      (nat_rect (lam q (paths nat (npair (upa q) (upb q)) q)) (idpath 0)
        (lam k
          (lam IH
            (pathscomp0 nat (npair (una (upa k) (upb k)) (unb (upa k) (upb k)))
              (succ (npair (upa k) (upb k)))
              (succ k)
              (npairstep (upa k) (upb k))
              (maponpaths nat nat (lam w (succ w)) (npair (upa k) (upb k)) k IH))))
        n)))
 ; the round trip computes both ways on closed nats
 (id? (vof 0 '9) (vof 0 '(npair (upa 9) (upb 9)))))

; --- the beta ladder: unpair undoes npair. the head/diag steps re-read the
; --- walk one rung down through the una/unb components (each close is
; --- definitional -- una 0 y is succ y, una (succ a) y is a); unpairbeta
; --- rides the double induction (diagonal d outside, the walk down it
; --- inside), the equation convoys carrying a+b = d exactly like
; --- isdeceqnat's; both projections travel together in one total2. ---
(assert
 (defq unpbheadu
   (pi d nat
      (pi ha (paths nat (upa (npair 0 d)) 0)
        (pi hb (paths nat (upb (npair 0 d)) d) (paths nat (upa (npair (succ d) 0)) (succ d)))))
   (lam d
      (lam ha
        (lam hb
          (pathscomp0 nat (upa (npair (succ d) 0)) (upa (succ (npair 0 d))) (succ d)
            (maponpaths nat nat (lam w (upa w)) (npair (succ d) 0) (succ (npair 0 d))
              (npairhead d))
            (pathscomp0 nat (una (upa (npair 0 d)) (upb (npair 0 d))) (una 0 (upb (npair 0 d)))
              (succ d)
              (maponpaths nat nat (lam w (una w (upb (npair 0 d)))) (upa (npair 0 d)) 0 ha)
              (maponpaths nat nat (lam w (una 0 w)) (upb (npair 0 d)) d hb)))))))
 (defq unpbheadb
   (pi d nat (pi ha (paths nat (upa (npair 0 d)) 0) (paths nat (upb (npair (succ d) 0)) 0)))
   (lam d
      (lam ha
        (pathscomp0 nat (upb (npair (succ d) 0)) (upb (succ (npair 0 d))) 0
          (maponpaths nat nat (lam w (upb w)) (npair (succ d) 0) (succ (npair 0 d))
            (npairhead d))
          (maponpaths nat nat (lam w (unb w (upb (npair 0 d)))) (upa (npair 0 d)) 0 ha)))))
 (defq unpbdiagu
   (pi a nat
      (pi b nat
        (pi ha (paths nat (upa (npair (succ a) b)) (succ a))
          (paths nat (upa (npair a (succ b))) a))))
   (lam a
      (lam b
        (lam ha
          (pathscomp0 nat (upa (npair a (succ b))) (upa (succ (npair (succ a) b))) a
            (maponpaths nat nat (lam w (upa w)) (npair a (succ b)) (succ (npair (succ a) b))
              (npairdiag a b))
            (maponpaths nat nat (lam w (una w (upb (npair (succ a) b))))
              (upa (npair (succ a) b))
              (succ a)
              ha))))))
 (defq unpbdiagb
   (pi a nat
      (pi b nat
        (pi ha (paths nat (upa (npair (succ a) b)) (succ a))
          (pi hb (paths nat (upb (npair (succ a) b)) b)
            (paths nat (upb (npair a (succ b))) (succ b))))))
   (lam a
      (lam b
        (lam ha
          (lam hb
            (pathscomp0 nat (upb (npair a (succ b))) (upb (succ (npair (succ a) b))) (succ b)
              (maponpaths nat nat (lam w (upb w)) (npair a (succ b)) (succ (npair (succ a) b))
                (npairdiag a b))
              (pathscomp0 nat (unb (upa (npair (succ a) b)) (upb (npair (succ a) b)))
                (succ (upb (npair (succ a) b)))
                (succ b)
                (maponpaths nat nat (lam w (unb w (upb (npair (succ a) b))))
                  (upa (npair (succ a) b))
                  (succ a)
                  ha)
                (maponpaths nat nat (lam w (succ w)) (upb (npair (succ a) b)) b hb))))))))
 (defq unpairbeta
   (pi d nat
      (pi a nat
        (pi b nat
          (pi e (paths nat (add a b) d)
            (total2 zz (paths nat (upa (npair a b)) a) (paths nat (upb (npair a b)) b))))))
   (lam d
      (nat_rect
        (lam dq
          (pi a nat
            (pi b nat
              (pi e (paths nat (add a b) dq)
                (total2 zz (paths nat (upa (npair a b)) a) (paths nat (upb (npair a b)) b))))))
        (lam a
          (nat_rect
            (lam aq
              (pi b nat
                (pi e (paths nat (add aq b) 0)
                  (total2 zz (paths nat (upa (npair aq b)) aq) (paths nat (upb (npair aq b)) b)))))
            (lam b
              (lam e
                (transportf nat
                  (lam w
                    (total2 zz (paths nat (upa (npair 0 w)) 0) (paths nat (upb (npair 0 w)) w)))
                  0
                  b
                  (pathsinv0 nat b 0 e)
                  (tpair (idpath 0) (idpath 0)))))
            (lam k
              (lam IH
                (lam b
                  (lam e
                    (fromempty
                      (total2 zz (paths nat (upa (npair (succ k) b)) (succ k))
                        (paths nat (upb (npair (succ k) b)) b))
                      (negpathssx0 (add k b) e))))))
            a))
        (lam d2
          (lam IHd
            (lam a
              (lam b
                (lam e
                  ((nat_rect
                     (lam bq
                       (pi a2 nat
                         (pi e2 (paths nat (add a2 bq) (succ d2))
                           (total2 zz (paths nat (upa (npair a2 bq)) a2)
                             (paths nat (upb (npair a2 bq)) bq)))))
                     (lam a2
                       (lam e2
                         (transportf nat
                           (lam w
                             (total2 zz (paths nat (upa (npair w 0)) w)
                               (paths nat (upb (npair w 0)) 0)))
                           (succ d2)
                           a2
                           (pathsinv0 nat a2 (succ d2)
                             (pathscomp0 nat a2 (add a2 0) (succ d2)
                               (pathsinv0 nat (add a2 0) a2 (natplusr0 a2))
                               e2))
                           (tpair
                             (unpbheadu d2 (pr1 (IHd 0 d2 (idpath d2)))
                               (pr2 (IHd 0 d2 (idpath d2))))
                             (unpbheadb d2 (pr1 (IHd 0 d2 (idpath d2))))))))
                     (lam b2
                       (lam IHW
                         (lam a2
                           (lam e2
                             (tpair
                               (unpbdiagu a2 b2
                                 (pr1
                                   (IHW (succ a2)
                                     (pathscomp0 nat (add (succ a2) b2) (add a2 (succ b2))
                                       (succ d2)
                                       (pathsinv0 nat (add a2 (succ b2)) (add (succ a2) b2)
                                         (natplusnsm a2 b2))
                                       e2))))
                               (unpbdiagb a2 b2
                                 (pr1
                                   (IHW (succ a2)
                                     (pathscomp0 nat (add (succ a2) b2) (add a2 (succ b2))
                                       (succ d2)
                                       (pathsinv0 nat (add a2 (succ b2)) (add (succ a2) b2)
                                         (natplusnsm a2 b2))
                                       e2)))
                                 (pr2
                                   (IHW (succ a2)
                                     (pathscomp0 nat (add (succ a2) b2) (add a2 (succ b2))
                                       (succ d2)
                                       (pathsinv0 nat (add a2 (succ b2)) (add (succ a2) b2)
                                         (natplusnsm a2 b2))
                                       e2)))))))))
                     b)
                    a
                    e))))))
        d)))
 (defq upabeta
   (pi a nat (pi b nat (paths nat (upa (npair a b)) a)))
   (lam a (lam b (pr1 (unpairbeta (add a b) a b (idpath (add a b)))))))
 (defq upbbeta
   (pi a nat (pi b nat (paths nat (upb (npair a b)) b)))
   (lam a (lam b (pr2 (unpairbeta (add a b) a b (idpath (add a b)))))))
 (defq npairgeb
   (pi a nat (pi b nat (paths bool (natgtb b (npair a b)) false)))
   (lam a (lam b (natlehmplusnm b (tri (add a b))))))
 (defq upble
   (pi n nat (paths bool (natgtb (upb n) n) false))
   (lam n
      (transportf nat (lam w (paths bool (natgtb (upb n) w) false)) (npair (upa n) (upb n)) n
        (npairunpair n)
        (npairgeb (upa n) (upb n))))))

; --- a kid list as one nat: nil is 0, cons is succ of the pair, and ldec
; --- walks it back on FUEL -- the code bounds its own decode (a kid's code
; --- rides inside the pair, so upb never grows: upble + istransnatleh feed
; --- the guard convoys), the house fuel pattern at nateqb's one-call-per-
; --- level shape. round trips sealed both ways. ---
(assert
 (defn lcode
   (pi l nlist nat)
   (lam l
      ((nat_rect (lam q (pi v (nvec q) nat)) (lam v 0)
         (lam k (lam IH (lam v (succ (npair (pr1 v) (IH (pr2 v)))))))
         (pr1 l))
        (pr2 l))))
 (defn ldec
   (pi f nat (pi m nat nlist))
   (lam f
      (nat_rect (lam q (pi m nat nlist)) (lam m nnil)
        (lam f2
          (lam IH
            (lam m
              (bool_rect (lam c nlist) nnil (ncons (upa (npred m)) (IH (upb (npred m))))
                (niszero m)))))
        f)))
 (defq ldeccode
   (pi l nlist
      (pi f nat
        (pi h (paths bool (natgtb (lcode l) f) false) (paths nlist (ldec f (lcode l)) l))))
   (lam l
      ((nat_rect
         (lam q
           (pi v (nvec q)
             (pi f nat
               (pi h (paths bool (natgtb (lcode (tpair q v)) f) false)
                 (paths nlist (ldec f (lcode (tpair q v))) (tpair q v))))))
         (lam v
           (lam f
             (nat_rect
               (lam fq
                 (pi h (paths bool (natgtb (lcode (tpair 0 v)) fq) false)
                   (paths nlist (ldec fq (lcode (tpair 0 v))) (tpair 0 v))))
               (lam h (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v)))
               (lam f2 (lam IHf (lam h (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v)))))
               f)))
         (lam k
           (lam IH
             (lam v
               (lam f
                 (nat_rect
                   (lam fq
                     (pi h (paths bool (natgtb (lcode (tpair (succ k) v)) fq) false)
                       (paths nlist (ldec fq (lcode (tpair (succ k) v))) (tpair (succ k) v))))
                   (lam h
                     (fromempty
                       (paths nlist (ldec 0 (lcode (tpair (succ k) v))) (tpair (succ k) v))
                       (nopathstruetofalse h)))
                   (lam f2
                     (lam IHf
                       (lam h
                         (pathscomp0 nlist
                           (ncons (upa (npair (pr1 v) (lcode (tpair k (pr2 v)))))
                             (ldec f2 (upb (npair (pr1 v) (lcode (tpair k (pr2 v)))))))
                           (ncons (pr1 v)
                             (ldec f2 (upb (npair (pr1 v) (lcode (tpair k (pr2 v)))))))
                           (tpair (succ k) v)
                           (maponpaths nat nlist
                             (lam w
                               (ncons w
                                 (ldec f2 (upb (npair (pr1 v) (lcode (tpair k (pr2 v))))))))
                             (upa (npair (pr1 v) (lcode (tpair k (pr2 v)))))
                             (pr1 v)
                             (upabeta (pr1 v) (lcode (tpair k (pr2 v)))))
                           (pathscomp0 nlist
                             (ncons (pr1 v)
                               (ldec f2 (upb (npair (pr1 v) (lcode (tpair k (pr2 v)))))))
                             (ncons (pr1 v) (ldec f2 (lcode (tpair k (pr2 v)))))
                             (tpair (succ k) v)
                             (maponpaths nat nlist (lam w (ncons (pr1 v) (ldec f2 w)))
                               (upb (npair (pr1 v) (lcode (tpair k (pr2 v)))))
                               (lcode (tpair k (pr2 v)))
                               (upbbeta (pr1 v) (lcode (tpair k (pr2 v)))))
                             (maponpaths nlist nlist (lam w (ncons (pr1 v) w))
                               (ldec f2 (lcode (tpair k (pr2 v))))
                               (tpair k (pr2 v))
                               (IH (pr2 v) f2
                                 (istransnatleh (lcode (tpair k (pr2 v)))
                                   (npair (pr1 v) (lcode (tpair k (pr2 v))))
                                   f2
                                   (npairgeb (pr1 v) (lcode (tpair k (pr2 v))))
                                   h))))))))
                   f)))))
         (pr1 l))
        (pr2 l))))
 (defq lcodedec
   (pi f nat (pi m nat (pi h (paths bool (natgtb m f) false) (paths nat (lcode (ldec f m)) m))))
   (lam f
      (nat_rect
        (lam fq
          (pi m nat (pi h (paths bool (natgtb m fq) false) (paths nat (lcode (ldec fq m)) m))))
        (lam m
          (nat_rect
            (lam mq (pi h (paths bool (natgtb mq 0) false) (paths nat (lcode (ldec 0 mq)) mq)))
            (lam h (idpath 0))
            (lam m2
              (lam IHm
                (lam h
                  (fromempty (paths nat (lcode (ldec 0 (succ m2))) (succ m2))
                    (nopathstruetofalse h)))))
            m))
        (lam f2
          (lam IHf
            (lam m
              (nat_rect
                (lam mq
                  (pi h (paths bool (natgtb mq (succ f2)) false)
                    (paths nat (lcode (ldec (succ f2) mq)) mq)))
                (lam h (idpath 0))
                (lam m2
                  (lam IHm
                    (lam h
                      (maponpaths nat nat (lam w (succ w))
                        (npair (upa m2) (lcode (ldec f2 (upb m2))))
                        m2
                        (pathscomp0 nat (npair (upa m2) (lcode (ldec f2 (upb m2))))
                          (npair (upa m2) (upb m2))
                          m2
                          (maponpaths nat nat (lam w (npair (upa m2) w))
                            (lcode (ldec f2 (upb m2)))
                            (upb m2)
                            (IHf (upb m2) (istransnatleh (upb m2) m2 f2 (upble m2) h)))
                          (npairunpair m2))))))
                m))))
        f)))
 ; the code and its walk agree on a closed list
 (id? (vof 0 '(lcode (ncons 1 (ncons 2 nnil)))) (vof 0 '20))
 ((vof 0 '(ncons 1 (ncons 2 nnil))) = (vof 0 '(ldec 20 20))))

; --- the tree itself. enode pairs a label with its kids' code; elabel and
; --- ekids read a node back (ekids fuelled by the code it decodes). beta
; --- both ways and eta close from the two round trips, and nat's decidable
; --- equality IS the tree's -- the swal guards' Aeq, for free. ---
(assert
 (defn etree
   UU
   nat)
 (defn enode
   (pi l nat (pi ks nlist etree))
   (lam l (lam ks (npair l (lcode ks)))))
 (defn elabel
   (pi t etree nat)
   (lam t (upa t)))
 (defn ekids
   (pi t etree nlist)
   (lam t (ldec (upb t) (upb t))))
 (defq elabelbeta
   (pi l nat (pi ks nlist (paths nat (elabel (enode l ks)) l)))
   (lam l (lam ks (upabeta l (lcode ks)))))
 (defq ekidsbeta
   (pi l nat (pi ks nlist (paths nlist (ekids (enode l ks)) ks)))
   (lam l
      (lam ks
        (pathscomp0 nlist (ldec (upb (npair l (lcode ks))) (upb (npair l (lcode ks))))
          (ldec (lcode ks) (lcode ks))
          ks
          (maponpaths nat nlist (lam w (ldec w w)) (upb (npair l (lcode ks))) (lcode ks)
            (upbbeta l (lcode ks)))
          (ldeccode ks (lcode ks) (isreflnatleh (lcode ks)))))))
 (defq etreeeta
   (pi t etree (paths etree (enode (elabel t) (ekids t)) t))
   (lam t
      (pathscomp0 nat (npair (upa t) (lcode (ldec (upb t) (upb t)))) (npair (upa t) (upb t)) t
        (maponpaths nat nat (lam w (npair (upa t) w)) (lcode (ldec (upb t) (upb t))) (upb t)
          (lcodedec (upb t) (upb t) (isreflnatleh (upb t))))
        (npairunpair t))))
 (defn isdeceqetree
   (pi x etree (pi y etree (coprod (paths etree x y) (pi e (paths etree x y) empty))))
   isdeceqnat)
 ; the tree computes: a leaf is its label's triangle, the zero leaf IS 0,
 ; and eta reads ANY nat as a node -- 5 is (enode 0 (ncons 1 nnil))
 (id? (vof 0 '28) (vof 0 '(enode 7 nnil)))
 (id? (vof 0 '0) (vof 0 '(enode 0 nnil)))
 (id? (vof 0 '2) (vof 0 '(enode 0 (ncons (enode 0 nnil) nnil))))
 (id? (vof 0 '5) (vof 0 '(elabel (enode 5 (ncons 1 (ncons 2 nnil))))))
 ((vof 0 '(ncons 1 (ncons 2 nnil))) = (vof 0 '(ekids (enode 5 (ncons 1 (ncons 2 nnil))))))
 (id? (vof 0 '0) (vof 0 '(elabel 5)))
 ((vof 0 '(ncons 1 nnil)) = (vof 0 '(ekids 5)))
 (id? (vof 0 '5) (vof 0 '(enode (elabel 5) (ekids 5)))))


; ============================================================================
; the OCCURRENCE/REPLACE kit: patch.v's swallow primitives at element grain
; (elements are nats, so etree codes ride). occ counts, ffind aims at the
; first hit, lay replaces one seat, lget reads one back -- all TOTAL (out of
; range lay is the list, lget the zero point), so the option lanes flatten
; to natgtb guards. the decision is nateqb; every lemma is one walk down
; the list with the seat case inside, the decision split by uulay's
; dependent bool_rect (lgetffind convoys the decision's own idpath in, so
; the true branch REMEMBERS what it decided). the swal migration's floor.
; ============================================================================
; --- nateqb closes its own diagonal, and a true verdict is an identification
; --- (the corpus had symm; refl + soundness complete the little theory) ---
(assert
 (defq nateqbrefl
   (pi x nat (paths bool (nateqb x x) true))
   (lam x (nat_rect (lam q (paths bool (nateqb q q) true)) (idpath true) (lam k (lam IH IH)) x)))
 (defq nateqbtopaths
   (pi x nat (pi y nat (pi e (paths bool (nateqb x y) true) (paths nat x y))))
   (lam x
      (nat_rect (lam q (pi y nat (pi e (paths bool (nateqb q y) true) (paths nat q y))))
        (lam y
          (nat_rect (lam j (pi e (paths bool (nateqb 0 j) true) (paths nat 0 j)))
            (lam e (idpath 0))
            (lam j (lam r (lam e (fromempty (paths nat 0 (succ j)) (nopathsfalsetotrue e)))))
            y))
        (lam k
          (lam IH
            (lam y
              (nat_rect
                (lam j (pi e (paths bool (nateqb (succ k) j) true) (paths nat (succ k) j)))
                (lam e (fromempty (paths nat (succ k) 0) (nopathsfalsetotrue e)))
                (lam j (lam r (lam e (maponpaths nat nat (lam w (succ w)) k j (IH j e)))))
                y))))
        x))))

; --- the four primitives: the occ head contribution rides OUTSIDE the fold
; --- (one recursive call per level, nateqb's eager-NbE shape); lay and lget
; --- carry the seat as a pi so it decrements alongside the walk ---
(assert
 (defn occ
   (pi x nat (pi l nlist nat))
   (lam x
      (lam l
        ((nat_rect (lam q (pi v (nvec q) nat)) (lam v 0)
           (lam m
             (lam IH (lam v (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x)) (IH (pr2 v))))))
           (pr1 l))
          (pr2 l)))))
 (defn ffind
   (pi x nat (pi l nlist nat))
   (lam x
      (lam l
        ((nat_rect (lam q (pi v (nvec q) nat)) (lam v 0)
           (lam m
             (lam IH (lam v (bool_rect (lam c nat) 0 (succ (IH (pr2 v))) (nateqb (pr1 v) x)))))
           (pr1 l))
          (pr2 l)))))
 (defn lay
   (pi l nlist (pi k nat (pi w nat nlist)))
   (lam l
      (lam k
        (lam w
          ((nat_rect (lam q (pi v (nvec q) (pi k2 nat nlist))) (lam v (lam k2 nnil))
             (lam m
               (lam IH
                 (lam v
                   (lam k2
                     (bool_rect (lam c nlist) (ncons w (tpair m (pr2 v)))
                       (ncons (pr1 v) (IH (pr2 v) (npred k2)))
                       (niszero k2))))))
             (pr1 l))
            (pr2 l)
            k)))))
 (defn lget
   (pi k nat (pi l nlist nat))
   (lam k
      (lam l
        ((nat_rect (lam q (pi v (nvec q) (pi k2 nat nat))) (lam v (lam k2 0))
           (lam m
             (lam IH
               (lam v
                 (lam k2 (bool_rect (lam c nat) (pr1 v) (IH (pr2 v) (npred k2)) (niszero k2))))))
           (pr1 l))
          (pr2 l)
          k))))
 ; the primitives compute, edges included (an out-of-range lay rides through)
 (id? (vof 0 '2) (vof 0 '(occ 7 (ncons 7 (ncons 3 (ncons 7 nnil))))))
 (id? (vof 0 '1) (vof 0 '(ffind 3 (ncons 7 (ncons 3 (ncons 7 nnil))))))
 ((vof 0 '(ncons 7 (ncons 9 (ncons 5 nnil)))) = (vof 0 '(lay (ncons 7 (ncons 3 (ncons 5 nnil))) 1 9)))
 ((vof 0 '(ncons 7 nnil)) = (vof 0 '(lay (ncons 7 nnil) 5 9)))
 (id? (vof 0 '5) (vof 0 '(lget 2 (ncons 7 (ncons 3 (ncons 5 nnil))))))
 ; at tree grain: count a subtree among a node's kids
 (id? (vof 0 '1) (vof 0 '(occ (enode 0 nnil) (ekids (enode 1 (ncons (enode 0 nnil) nnil)))))))

; --- the lemma kit, patch.v's seven mirrored. laylen and laylay are pure
; --- walks; layget subsumes lay_id with NO hypothesis (out of range both
; --- sides are the list, sigma-eta closes the cons); the occ movers carry
; --- the seat guard + the nateqb verdicts patch.v's Aeq destructs carried ---
(assert
 (defq laylen
   (pi l nlist (pi k nat (pi w nat (paths nat (nlen (lay l k w)) (nlen l)))))
   (lam l
      (lam k
        (lam w
          ((nat_rect
             (lam q (pi v (nvec q) (pi k2 nat (paths nat (nlen (lay (tpair q v) k2 w)) q))))
             (lam v (lam k2 (idpath 0)))
             (lam m
               (lam IH
                 (lam v
                   (lam k2
                     (nat_rect
                       (lam kq (paths nat (nlen (lay (tpair (succ m) v) kq w)) (succ m)))
                       (idpath (succ m))
                       (lam k3
                         (lam r
                           (maponpaths nat nat (lam z (succ z))
                             (nlen (lay (tpair m (pr2 v)) k3 w))
                             m
                             (IH (pr2 v) k3))))
                       k2)))))
             (pr1 l))
            (pr2 l)
            k)))))
 (defq occffind
   (pi x nat
      (pi l nlist
        (pi h (paths bool (niszero (occ x l)) false)
          (paths bool (natgtb (nlen l) (ffind x l)) true))))
   (lam x
      (lam l
        ((nat_rect
           (lam q
             (pi v (nvec q)
               (pi h (paths bool (niszero (occ x (tpair q v))) false)
                 (paths bool (natgtb q (ffind x (tpair q v))) true))))
           (lam v
             (lam h
               (fromempty (paths bool (natgtb 0 (ffind x (tpair 0 v))) true)
                 (nopathstruetofalse h))))
           (lam m
             (lam IH
               (lam v
                 (bool_rect
                   (lam c
                     (pi h
                       (paths bool
                         (niszero (add (bool_rect (lam c2 nat) 1 0 c) (occ x (tpair m (pr2 v)))))
                         false)
                       (paths bool
                         (natgtb (succ m)
                           (bool_rect (lam c2 nat) 0 (succ (ffind x (tpair m (pr2 v)))) c))
                         true)))
                   (lam h (idpath true))
                   (lam h (IH (pr2 v) h))
                   (nateqb (pr1 v) x)))))
           (pr1 l))
          (pr2 l)))))
 (defq lgetffind
   (pi x nat
      (pi l nlist
        (pi h (paths bool (niszero (occ x l)) false) (paths nat (lget (ffind x l) l) x))))
   (lam x
      (lam l
        ((nat_rect
           (lam q
             (pi v (nvec q)
               (pi h (paths bool (niszero (occ x (tpair q v))) false)
                 (paths nat (lget (ffind x (tpair q v)) (tpair q v)) x))))
           (lam v
             (lam h
               (fromempty (paths nat (lget (ffind x (tpair 0 v)) (tpair 0 v)) x)
                 (nopathstruetofalse h))))
           (lam m
             (lam IH
               (lam v
                 ((bool_rect
                    (lam c
                      (pi hc (paths bool (nateqb (pr1 v) x) c)
                        (pi h
                          (paths bool
                            (niszero
                              (add (bool_rect (lam c2 nat) 1 0 c) (occ x (tpair m (pr2 v)))))
                            false)
                          (paths nat
                            (lget
                              (bool_rect (lam c2 nat) 0 (succ (ffind x (tpair m (pr2 v)))) c)
                              (tpair (succ m) v))
                            x))))
                    (lam hc (lam h (nateqbtopaths (pr1 v) x hc)))
                    (lam hc (lam h (IH (pr2 v) h)))
                    (nateqb (pr1 v) x))
                   (idpath (nateqb (pr1 v) x))))))
           (pr1 l))
          (pr2 l)))))
 (defq lgetlay
   (pi l nlist
      (pi k nat
        (pi w nat
          (pi h (paths bool (natgtb (nlen l) k) true) (paths nat (lget k (lay l k w)) w)))))
   (lam l
      (lam k
        (lam w
          ((nat_rect
             (lam q
               (pi v (nvec q)
                 (pi k2 nat
                   (pi h (paths bool (natgtb q k2) true)
                     (paths nat (lget k2 (lay (tpair q v) k2 w)) w)))))
             (lam v
               (lam k2
                 (lam h
                   (fromempty (paths nat (lget k2 (lay (tpair 0 v) k2 w)) w)
                     (nopathsfalsetotrue h)))))
             (lam m
               (lam IH
                 (lam v
                   (lam k2
                     (nat_rect
                       (lam kq
                         (pi h (paths bool (natgtb (succ m) kq) true)
                           (paths nat (lget kq (lay (tpair (succ m) v) kq w)) w)))
                       (lam h (idpath w))
                       (lam k3 (lam r (lam h (IH (pr2 v) k3 h))))
                       k2)))))
             (pr1 l))
            (pr2 l)
            k)))))
 (defq laylay
   (pi l nlist (pi k nat (pi u nat (pi w nat (paths nlist (lay (lay l k u) k w) (lay l k w))))))
   (lam l
      (lam k
        (lam u
          (lam w
            ((nat_rect
               (lam q
                 (pi v (nvec q)
                   (pi k2 nat
                     (paths nlist (lay (lay (tpair q v) k2 u) k2 w) (lay (tpair q v) k2 w)))))
               (lam v (lam k2 (idpath nnil)))
               (lam m
                 (lam IH
                   (lam v
                     (lam k2
                       (nat_rect
                         (lam kq
                           (paths nlist (lay (lay (tpair (succ m) v) kq u) kq w)
                             (lay (tpair (succ m) v) kq w)))
                         (idpath (ncons w (tpair m (pr2 v))))
                         (lam k3
                           (lam r
                             (maponpaths nlist nlist (lam z (ncons (pr1 v) z))
                               (lay (lay (tpair m (pr2 v)) k3 u) k3 w)
                               (lay (tpair m (pr2 v)) k3 w)
                               (IH (pr2 v) k3))))
                         k2)))))
               (pr1 l))
              (pr2 l)
              k))))))
 (defq layget
   (pi l nlist (pi k nat (paths nlist (lay l k (lget k l)) l)))
   (lam l
      (lam k
        ((nat_rect
           (lam q
             (pi v (nvec q)
               (pi k2 nat (paths nlist (lay (tpair q v) k2 (lget k2 (tpair q v))) (tpair q v)))))
           (lam v (lam k2 (pathsinv0 nlist (tpair 0 v) nnil (lnil0 v))))
           (lam m
             (lam IH
               (lam v
                 (lam k2
                   (nat_rect
                     (lam kq
                       (paths nlist (lay (tpair (succ m) v) kq (lget kq (tpair (succ m) v)))
                         (tpair (succ m) v)))
                     (idpath (tpair (succ m) v))
                     (lam k3
                       (lam r
                         (maponpaths nlist nlist (lam z (ncons (pr1 v) z))
                           (lay (tpair m (pr2 v)) k3 (lget k3 (tpair m (pr2 v))))
                           (tpair m (pr2 v))
                           (IH (pr2 v) k3))))
                     k2)))))
           (pr1 l))
          (pr2 l)
          k))))
 (defq occlayin
   (pi l nlist
      (pi k nat
        (pi x nat
          (pi h (paths bool (natgtb (nlen l) k) true)
            (pi hne (paths bool (nateqb (lget k l) x) false)
              (paths nat (occ x (lay l k x)) (succ (occ x l))))))))
   (lam l
      (lam k
        (lam x
          ((nat_rect
             (lam q
               (pi v (nvec q)
                 (pi k2 nat
                   (pi h (paths bool (natgtb q k2) true)
                     (pi hne (paths bool (nateqb (lget k2 (tpair q v)) x) false)
                       (paths nat (occ x (lay (tpair q v) k2 x)) (succ (occ x (tpair q v)))))))))
             (lam v
               (lam k2
                 (lam h
                   (lam hne
                     (fromempty
                       (paths nat (occ x (lay (tpair 0 v) k2 x)) (succ (occ x (tpair 0 v))))
                       (nopathsfalsetotrue h))))))
             (lam m
               (lam IH
                 (lam v
                   (lam k2
                     (nat_rect
                       (lam kq
                         (pi h (paths bool (natgtb (succ m) kq) true)
                           (pi hne (paths bool (nateqb (lget kq (tpair (succ m) v)) x) false)
                             (paths nat (occ x (lay (tpair (succ m) v) kq x))
                               (succ (occ x (tpair (succ m) v)))))))
                       (lam h
                         (lam hne
                           (pathscomp0 nat (occ x (ncons x (tpair m (pr2 v))))
                             (succ (occ x (tpair m (pr2 v))))
                             (succ (occ x (tpair (succ m) v)))
                             (maponpaths bool nat
                               (lam c
                                 (add (bool_rect (lam c2 nat) 1 0 c) (occ x (tpair m (pr2 v)))))
                               (nateqb x x)
                               true
                               (nateqbrefl x))
                             (pathsinv0 nat (succ (occ x (tpair (succ m) v)))
                               (succ (occ x (tpair m (pr2 v))))
                               (maponpaths bool nat
                                 (lam c
                                   (succ
                                     (add (bool_rect (lam c2 nat) 1 0 c)
                                       (occ x (tpair m (pr2 v))))))
                                 (nateqb (pr1 v) x)
                                 false
                                 hne)))))
                       (lam k3
                         (lam r
                           (lam h
                             (lam hne
                               (pathscomp0 nat
                                 (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                   (occ x (lay (tpair m (pr2 v)) k3 x)))
                                 (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                   (succ (occ x (tpair m (pr2 v)))))
                                 (succ
                                   (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                     (occ x (tpair m (pr2 v)))))
                                 (maponpaths nat nat
                                   (lam z
                                     (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x)) z))
                                   (occ x (lay (tpair m (pr2 v)) k3 x))
                                   (succ (occ x (tpair m (pr2 v))))
                                   (IH (pr2 v) k3 h hne))
                                 (natplusnsm (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                   (occ x (tpair m (pr2 v)))))))))
                       k2)))))
             (pr1 l))
            (pr2 l)
            k)))))
 (defq occlayout
   (pi l nlist
      (pi k nat
        (pi x nat
          (pi w nat
            (pi h (paths bool (natgtb (nlen l) k) true)
              (pi he (paths nat (lget k l) x)
                (pi hne (paths bool (nateqb w x) false)
                  (paths nat (occ x l) (succ (occ x (lay l k w)))))))))))
   (lam l
      (lam k
        (lam x
          (lam w
            ((nat_rect
               (lam q
                 (pi v (nvec q)
                   (pi k2 nat
                     (pi h (paths bool (natgtb q k2) true)
                       (pi he (paths nat (lget k2 (tpair q v)) x)
                         (pi hne (paths bool (nateqb w x) false)
                           (paths nat (occ x (tpair q v)) (succ (occ x (lay (tpair q v) k2 w))))))))))
               (lam v
                 (lam k2
                   (lam h
                     (lam he
                       (lam hne
                         (fromempty
                           (paths nat (occ x (tpair 0 v)) (succ (occ x (lay (tpair 0 v) k2 w))))
                           (nopathsfalsetotrue h)))))))
               (lam m
                 (lam IH
                   (lam v
                     (lam k2
                       (nat_rect
                         (lam kq
                           (pi h (paths bool (natgtb (succ m) kq) true)
                             (pi he (paths nat (lget kq (tpair (succ m) v)) x)
                               (pi hne (paths bool (nateqb w x) false)
                                 (paths nat (occ x (tpair (succ m) v))
                                   (succ (occ x (lay (tpair (succ m) v) kq w))))))))
                         (lam h
                           (lam he
                             (lam hne
                               (pathscomp0 nat (occ x (tpair (succ m) v))
                                 (add (bool_rect (lam c2 nat) 1 0 (nateqb x x))
                                   (occ x (tpair m (pr2 v))))
                                 (succ (occ x (ncons w (tpair m (pr2 v)))))
                                 (maponpaths nat nat
                                   (lam z
                                     (add (bool_rect (lam c2 nat) 1 0 (nateqb z x))
                                       (occ x (tpair m (pr2 v)))))
                                   (pr1 v)
                                   x
                                   he)
                                 (pathscomp0 nat
                                   (add (bool_rect (lam c2 nat) 1 0 (nateqb x x))
                                     (occ x (tpair m (pr2 v))))
                                   (succ (occ x (tpair m (pr2 v))))
                                   (succ (occ x (ncons w (tpair m (pr2 v)))))
                                   (maponpaths bool nat
                                     (lam c
                                       (add (bool_rect (lam c2 nat) 1 0 c)
                                         (occ x (tpair m (pr2 v)))))
                                     (nateqb x x)
                                     true
                                     (nateqbrefl x))
                                   (pathsinv0 nat (succ (occ x (ncons w (tpair m (pr2 v)))))
                                     (succ (occ x (tpair m (pr2 v))))
                                     (maponpaths bool nat
                                       (lam c
                                         (succ
                                           (add (bool_rect (lam c2 nat) 1 0 c)
                                             (occ x (tpair m (pr2 v))))))
                                       (nateqb w x)
                                       false
                                       hne)))))))
                         (lam k3
                           (lam r
                             (lam h
                               (lam he
                                 (lam hne
                                   (pathscomp0 nat
                                     (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                       (occ x (tpair m (pr2 v))))
                                     (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                       (succ (occ x (lay (tpair m (pr2 v)) k3 w))))
                                     (succ
                                       (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                         (occ x (lay (tpair m (pr2 v)) k3 w))))
                                     (maponpaths nat nat
                                       (lam z
                                         (add (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x)) z))
                                       (occ x (tpair m (pr2 v)))
                                       (succ (occ x (lay (tpair m (pr2 v)) k3 w)))
                                       (IH (pr2 v) k3 h he hne))
                                     (natplusnsm (bool_rect (lam c2 nat) 1 0 (nateqb (pr1 v) x))
                                       (occ x (lay (tpair m (pr2 v)) k3 w)))))))))
                         k2)))))
               (pr1 l))
              (pr2 l)
              k))))))
 (defq ffindlay
   (pi l nlist
      (pi k nat
        (pi x nat
          (pi h0 (paths nat (occ x l) 0)
            (pi h (paths bool (natgtb (nlen l) k) true) (paths nat (ffind x (lay l k x)) k))))))
   (lam l
      (lam k
        (lam x
          ((nat_rect
             (lam q
               (pi v (nvec q)
                 (pi k2 nat
                   (pi h0 (paths nat (occ x (tpair q v)) 0)
                     (pi h (paths bool (natgtb q k2) true)
                       (paths nat (ffind x (lay (tpair q v) k2 x)) k2))))))
             (lam v
               (lam k2
                 (lam h0
                   (lam h
                     (fromempty (paths nat (ffind x (lay (tpair 0 v) k2 x)) k2)
                       (nopathsfalsetotrue h))))))
             (lam m
               (lam IH
                 (lam v
                   (lam k2
                     (nat_rect
                       (lam kq
                         (pi h0 (paths nat (occ x (tpair (succ m) v)) 0)
                           (pi h (paths bool (natgtb (succ m) kq) true)
                             (paths nat (ffind x (lay (tpair (succ m) v) kq x)) kq))))
                       (lam h0
                         (lam h
                           (maponpaths bool nat
                             (lam c
                               (bool_rect (lam c2 nat) 0 (succ (ffind x (tpair m (pr2 v)))) c))
                             (nateqb x x)
                             true
                             (nateqbrefl x))))
                       (lam k3
                         (lam r
                           (lam h0
                             (lam h
                               ((bool_rect
                                  (lam c
                                    (pi h0
                                      (paths nat
                                        (add (bool_rect (lam c2 nat) 1 0 c)
                                          (occ x (tpair m (pr2 v))))
                                        0)
                                      (paths nat
                                        (bool_rect (lam c2 nat) 0
                                          (succ (ffind x (lay (tpair m (pr2 v)) k3 x)))
                                          c)
                                        (succ k3))))
                                  (lam h0
                                    (fromempty (paths nat 0 (succ k3))
                                      (negpathssx0 (occ x (tpair m (pr2 v))) h0)))
                                  (lam h0
                                    (maponpaths nat nat (lam z (succ z))
                                      (ffind x (lay (tpair m (pr2 v)) k3 x))
                                      k3
                                      (IH (pr2 v) k3 h0 h)))
                                  (nateqb (pr1 v) x))
                                 h0)))))
                       k2)))))
             (pr1 l))
            (pr2 l)
            k)))))
 ; the kit computes end to end: an absent element laid in is found at its seat
 (id? (vof 0 '1) (vof 0 '(ffind 9 (lay (ncons 7 (ncons 3 nnil)) 1 9))))
 (id? (vof 0 '1) (vof 0 '(occ 9 (lay (ncons 7 (ncons 3 nnil)) 1 9))))
 ((vof 0 '(ncons 7 (ncons 3 nnil))) = (vof 0 '(lay (lay (ncons 7 (ncons 3 nnil)) 1 9) 1 3))))


; ============================================================================
; the SWALLOW commute: patch.v's sixth slice at uu. a shallow patch is a
; HUNK (hinv is its inverse); the deep patch mke j a b rewrites one
; element at seat j. swal re-aims a deep-first pair: undo the deep edit
; inside BOTH shallow sides, re-aim it at the survivor's seat -- guarded
; by the four occurrence counts that make the rewrite canonical, all
; seven verdicts folded into ONE andb tower so the involution cases a
; single bool. the L1 law: swal through the groupoid inverse comes home.
; ============================================================================
; --- two nat facts the re-aim arithmetic leans on ---
(assert
 (defq natlehplusminus
   (pi i nat
      (pi j nat (pi h (paths bool (natgtb i j) false) (paths nat (add i (natminus j i)) j))))
   (lam i
      (nat_rect
        (lam q
          (pi j nat (pi h (paths bool (natgtb q j) false) (paths nat (add q (natminus j q)) j))))
        (lam j (lam h (natminusn0 j)))
        (lam k
          (lam IH
            (lam j
              (nat_rect
                (lam r
                  (pi h (paths bool (natgtb (succ k) r) false)
                    (paths nat (add (succ k) (natminus r (succ k))) r)))
                (lam h
                  (fromempty (paths nat (add (succ k) (natminus 0 (succ k))) 0)
                    (nopathstruetofalse h)))
                (lam r2
                  (lam rIH
                    (lam h
                      (maponpaths nat nat (lam z (succ z)) (add k (natminus r2 k)) r2 (IH r2 h)))))
                j))))
        i)))
 (defq natminusltl
   (pi i nat
      (pi j nat
        (pi n nat
          (pi h1 (paths bool (natgtb i j) false)
            (pi h2 (paths bool (natgtb (add i n) j) true)
              (paths bool (natgtb n (natminus j i)) true))))))
   (lam i
      (nat_rect
        (lam q
          (pi j nat
            (pi n nat
              (pi h1 (paths bool (natgtb q j) false)
                (pi h2 (paths bool (natgtb (add q n) j) true)
                  (paths bool (natgtb n (natminus j q)) true))))))
        (lam j
          (lam n
            (lam h1
              (lam h2
                (transportf nat (lam z (paths bool (natgtb n z) true)) j (natminus j 0)
                  (pathsinv0 nat (natminus j 0) j (natminusn0 j))
                  h2)))))
        (lam k
          (lam IH
            (lam j
              (nat_rect
                (lam r
                  (pi n nat
                    (pi h1 (paths bool (natgtb (succ k) r) false)
                      (pi h2 (paths bool (natgtb (add (succ k) n) r) true)
                        (paths bool (natgtb n (natminus r (succ k))) true)))))
                (lam n
                  (lam h1
                    (lam h2
                      (fromempty (paths bool (natgtb n (natminus 0 (succ k))) true)
                        (nopathstruetofalse h1)))))
                (lam r2 (lam rIH (lam n (lam h1 (lam h2 (IH r2 n h1 h2))))))
                j))))
        i))))

; --- the model: the deep patch, its inverse, the result pair, and swal
; --- itself -- one bool_rect on the folded guard, the payload re-aiming
; --- the deep edit at the survivor's seat (ffind under the four counts) ---
(assert
 (defn epatch
   UU
   (total2 j nat (total2 a nat nat)))
 (defn mke
   (pi j nat (pi a nat (pi b nat epatch)))
   (lam j (lam a (lam b (tpair j (tpair a b))))))
 (defn ej
   (pi p epatch nat)
   (lam p (pr1 p)))
 (defn ea
   (pi p epatch nat)
   (lam p (pr1 (pr2 p))))
 (defn eb
   (pi p epatch nat)
   (lam p (pr2 (pr2 p))))
 (defn einv
   (pi p epatch epatch)
   (lam p (mke (ej p) (eb p) (ea p))))
 (defn spair
   UU
   (total2 x hunk epatch))
 (defn scmside
   (pi c (coprod spair unit) UU)
   (lam c (coprod_rect (lam x UU) (lam x empty) (lam x unit) c)))
 (defn swal
   (pi p epatch (pi s hunk (coprod spair unit)))
   (lam p
      (lam s
        (bool_rect (lam c (coprod spair unit))
          (ii1
            (tpair
              (mkh (hi s) (lay (ho s) (natminus (ej p) (hi s)) (ea p))
                (lay (hn s) (ffind (eb p) (hn s)) (ea p)))
              (mke (add (hi s) (ffind (eb p) (hn s))) (ea p) (eb p))))
          (ii2 tt)
          (andb (negb (natgtb (hi s) (ej p)))
            (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
              (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                (andb (nateqb (occ (eb p) (ho s)) 1)
                  (andb (nateqb (occ (ea p) (ho s)) 0)
                    (andb (nateqb (occ (eb p) (hn s)) 1) (nateqb (occ (ea p) (hn s)) 0)))))))))))
 ; swal computes: the guarded re-aim fires, and a breach refuses
 ((vof 0 '(swal (mke 1 9 2) (mkh 0 (ncons 7 (ncons 2 nnil)) (ncons 2 (ncons 8 nnil)))))
  = (vof 0 '(ii1 (tpair (mkh 0 (ncons 7 (ncons 9 nnil)) (ncons 9 (ncons 8 nnil))) (mke 0 9 2)))))
 ((vof 0 '(swal (mke 5 9 2) (mkh 0 (ncons 7 (ncons 2 nnil)) (ncons 2 (ncons 8 nnil)))))
  = (vof 0 '(ii2 tt)))
 ; the round trip computes: invert both results, swal again, come home
 ((vof 0 '(swal (einv (mke 0 9 2)) (hinv (mkh 0 (ncons 7 (ncons 9 nnil)) (ncons 9 (ncons 8 nnil))))))
  = (vof 0 '(ii1 (tpair (hinv (mkh 0 (ncons 7 (ncons 2 nnil)) (ncons 2 (ncons 8 nnil)))) (einv (mke 1 9 2)))))))

; --- the core, at components: given the seven verdicts (as paths) + a/b
; --- distinct, the inverted pair swal's home. each forward guard is one
; --- kit lemma (occlayin/occlayout move the counts, lgetlay reads the
; --- laid seat, ffindlay pins the survivor), the payload ties by laylay +
; --- layget + the minus cancellations, and one maponpaths over the folded
; --- guard fires the tower. sealed as its own defq (the eager-NbE law) ---
(assert
 (defq swal-f1
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool (negb (natgtb i (add i (ffind b N)))) true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (maponpaths bool bool (lam c (negb c))
                                  (natgtb i (add i (ffind b N)))
                                  false
                                  (natlehnplusnm i (ffind b N))))))))))))))))))
 (defq swal-f2
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool
                                  (natgtb (add i (nlen (lay N (ffind b N) a)))
                                    (add i (ffind b N)))
                                  true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 bool
                                  (natgtb (add i (nlen (lay N (ffind b N) a)))
                                    (add i (ffind b N)))
                                  (natgtb (add i (nlen N)) (add i (ffind b N)))
                                  true
                                  (maponpaths nat bool
                                    (lam z (natgtb (add i z) (add i (ffind b N))))
                                    (nlen (lay N (ffind b N) a))
                                    (nlen N)
                                    (laylen N (ffind b N) a))
                                  (pathscomp0 bool (natgtb (add i (nlen N)) (add i (ffind b N)))
                                    (natgtb (nlen N) (ffind b N))
                                    true
                                    (natgtbplusl i (nlen N) (ffind b N))
                                    (occffind b N
                                      (maponpaths nat bool (lam z (niszero z)) (occ b N) 1 e6)))))))))))))))))))
 (defq swal-f3
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool
                                  (nateqb
                                    (lget (natminus (add i (ffind b N)) i) (lay N (ffind b N) a))
                                    a)
                                  true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 bool
                                  (nateqb
                                    (lget (natminus (add i (ffind b N)) i) (lay N (ffind b N) a))
                                    a)
                                  (nateqb a a)
                                  true
                                  (maponpaths nat bool (lam z (nateqb z a))
                                    (lget (natminus (add i (ffind b N)) i) (lay N (ffind b N) a))
                                    a
                                    (pathscomp0 nat
                                      (lget (natminus (add i (ffind b N)) i)
                                        (lay N (ffind b N) a))
                                      (lget (ffind b N) (lay N (ffind b N) a))
                                      a
                                      (maponpaths nat nat (lam z (lget z (lay N (ffind b N) a)))
                                        (natminus (add i (ffind b N)) i)
                                        (ffind b N)
                                        (natpluslcancelminus i (ffind b N)))
                                      (lgetlay N (ffind b N) a
                                        (occffind b N
                                          (maponpaths nat bool (lam z (niszero z)) (occ b N) 1
                                            e6)))))
                                  (nateqbrefl a)))))))))))))))))
 (defq swal-f4
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool (nateqb (occ a (lay N (ffind b N) a)) 1) true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (maponpaths nat bool (lam z (nateqb z 1))
                                  (occ a (lay N (ffind b N) a))
                                  1
                                  (pathscomp0 nat (occ a (lay N (ffind b N) a)) (succ (occ a N))
                                    1
                                    (occlayin N (ffind b N) a
                                      (occffind b N
                                        (maponpaths nat bool (lam z (niszero z)) (occ b N) 1 e6))
                                      (pathscomp0 bool (nateqb (lget (ffind b N) N) a)
                                        (nateqb b a)
                                        false
                                        (maponpaths nat bool (lam z (nateqb z a))
                                          (lget (ffind b N) N)
                                          b
                                          (lgetffind b N
                                            (maponpaths nat bool (lam z (niszero z)) (occ b N) 1
                                              e6)))
                                        (pathscomp0 bool (nateqb b a) (nateqb a b) false
                                          (nateqbsymm b a)
                                          e8)))
                                    (maponpaths nat nat (lam z (succ z)) (occ a N) 0 e7))))))))))))))))))
 (defq swal-f5
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool (nateqb (occ b (lay N (ffind b N) a)) 0) true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (maponpaths nat bool (lam z (nateqb z 0))
                                  (occ b (lay N (ffind b N) a))
                                  0
                                  (pathsinv0 nat 0 (occ b (lay N (ffind b N) a))
                                    (maponpaths nat nat (lam z (npred z)) 1
                                      (succ (occ b (lay N (ffind b N) a)))
                                      (pathscomp0 nat 1 (occ b N)
                                        (succ (occ b (lay N (ffind b N) a)))
                                        (pathsinv0 nat (occ b N) 1 e6)
                                        (occlayout N (ffind b N) b a
                                          (occffind b N
                                            (maponpaths nat bool (lam z (niszero z)) (occ b N) 1
                                              e6))
                                          (lgetffind b N
                                            (maponpaths nat bool (lam z (niszero z)) (occ b N) 1
                                              e6))
                                          e8))))))))))))))))))))
 (defq swal-f6
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool (nateqb (occ a (lay O (natminus j i) a)) 1) true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (maponpaths nat bool (lam z (nateqb z 1))
                                  (occ a (lay O (natminus j i) a))
                                  1
                                  (pathscomp0 nat (occ a (lay O (natminus j i) a))
                                    (succ (occ a O))
                                    1
                                    (occlayin O (natminus j i) a
                                      (natminusltl i j (nlen O) e1 e2)
                                      (pathscomp0 bool (nateqb (lget (natminus j i) O) a)
                                        (nateqb b a)
                                        false
                                        (maponpaths nat bool (lam z (nateqb z a))
                                          (lget (natminus j i) O)
                                          b
                                          e3)
                                        (pathscomp0 bool (nateqb b a) (nateqb a b) false
                                          (nateqbsymm b a)
                                          e8)))
                                    (maponpaths nat nat (lam z (succ z)) (occ a O) 0 e5))))))))))))))))))
 (defq swal-f7
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool (nateqb (occ b (lay O (natminus j i) a)) 0) true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (maponpaths nat bool (lam z (nateqb z 0))
                                  (occ b (lay O (natminus j i) a))
                                  0
                                  (pathsinv0 nat 0 (occ b (lay O (natminus j i) a))
                                    (maponpaths nat nat (lam z (npred z)) 1
                                      (succ (occ b (lay O (natminus j i) a)))
                                      (pathscomp0 nat 1 (occ b O)
                                        (succ (occ b (lay O (natminus j i) a)))
                                        (pathsinv0 nat (occ b O) 1 e4)
                                        (occlayout O (natminus j i) b a
                                          (natminusltl i j (nlen O) e1 e2)
                                          e3
                                          e8))))))))))))))))))))
 (defq swal-fg
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths bool
                                  (andb (negb (natgtb i (add i (ffind b N))))
                                    (andb
                                      (natgtb (add i (nlen (lay N (ffind b N) a)))
                                        (add i (ffind b N)))
                                      (andb
                                        (nateqb
                                          (lget (natminus (add i (ffind b N)) i)
                                            (lay N (ffind b N) a))
                                          a)
                                        (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                          (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0)))))))
                                  true)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 bool
                                  (andb (negb (natgtb i (add i (ffind b N))))
                                    (andb
                                      (natgtb (add i (nlen (lay N (ffind b N) a)))
                                        (add i (ffind b N)))
                                      (andb
                                        (nateqb
                                          (lget (natminus (add i (ffind b N)) i)
                                            (lay N (ffind b N) a))
                                          a)
                                        (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                          (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0)))))))
                                  (andb
                                    (natgtb (add i (nlen (lay N (ffind b N) a)))
                                      (add i (ffind b N)))
                                    (andb
                                      (nateqb
                                        (lget (natminus (add i (ffind b N)) i)
                                          (lay N (ffind b N) a))
                                        a)
                                      (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                        (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                          (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                            (nateqb (occ b (lay O (natminus j i) a)) 0))))))
                                  true
                                  (maponpaths bool bool
                                    (lam c
                                      (andb c
                                        (andb
                                          (natgtb (add i (nlen (lay N (ffind b N) a)))
                                            (add i (ffind b N)))
                                          (andb
                                            (nateqb
                                              (lget (natminus (add i (ffind b N)) i)
                                                (lay N (ffind b N) a))
                                              a)
                                            (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                              (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                                (andb
                                                  (nateqb (occ a (lay O (natminus j i) a)) 1)
                                                  (nateqb (occ b (lay O (natminus j i) a)) 0))))))))
                                    (negb (natgtb i (add i (ffind b N))))
                                    true
                                    (swal-f1 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                  (pathscomp0 bool
                                    (andb
                                      (natgtb (add i (nlen (lay N (ffind b N) a)))
                                        (add i (ffind b N)))
                                      (andb
                                        (nateqb
                                          (lget (natminus (add i (ffind b N)) i)
                                            (lay N (ffind b N) a))
                                          a)
                                        (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                          (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0))))))
                                    (andb
                                      (nateqb
                                        (lget (natminus (add i (ffind b N)) i)
                                          (lay N (ffind b N) a))
                                        a)
                                      (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                        (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                          (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                            (nateqb (occ b (lay O (natminus j i) a)) 0)))))
                                    true
                                    (maponpaths bool bool
                                      (lam c
                                        (andb c
                                          (andb
                                            (nateqb
                                              (lget (natminus (add i (ffind b N)) i)
                                                (lay N (ffind b N) a))
                                              a)
                                            (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                              (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                                (andb
                                                  (nateqb (occ a (lay O (natminus j i) a)) 1)
                                                  (nateqb (occ b (lay O (natminus j i) a)) 0)))))))
                                      (natgtb (add i (nlen (lay N (ffind b N) a)))
                                        (add i (ffind b N)))
                                      true
                                      (swal-f2 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                    (pathscomp0 bool
                                      (andb
                                        (nateqb
                                          (lget (natminus (add i (ffind b N)) i)
                                            (lay N (ffind b N) a))
                                          a)
                                        (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                          (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0)))))
                                      (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                        (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                          (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                            (nateqb (occ b (lay O (natminus j i) a)) 0))))
                                      true
                                      (maponpaths bool bool
                                        (lam c
                                          (andb c
                                            (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                              (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                                (andb
                                                  (nateqb (occ a (lay O (natminus j i) a)) 1)
                                                  (nateqb (occ b (lay O (natminus j i) a)) 0))))))
                                        (nateqb
                                          (lget (natminus (add i (ffind b N)) i)
                                            (lay N (ffind b N) a))
                                          a)
                                        true
                                        (swal-f3 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                      (pathscomp0 bool
                                        (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                          (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0))))
                                        (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                          (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                            (nateqb (occ b (lay O (natminus j i) a)) 0)))
                                        true
                                        (maponpaths bool bool
                                          (lam c
                                            (andb c
                                              (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                                (andb
                                                  (nateqb (occ a (lay O (natminus j i) a)) 1)
                                                  (nateqb (occ b (lay O (natminus j i) a)) 0)))))
                                          (nateqb (occ a (lay N (ffind b N) a)) 1)
                                          true
                                          (swal-f4 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                        (pathscomp0 bool
                                          (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0)))
                                          (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                            (nateqb (occ b (lay O (natminus j i) a)) 0))
                                          true
                                          (maponpaths bool bool
                                            (lam c
                                              (andb c
                                                (andb
                                                  (nateqb (occ a (lay O (natminus j i) a)) 1)
                                                  (nateqb (occ b (lay O (natminus j i) a)) 0))))
                                            (nateqb (occ b (lay N (ffind b N) a)) 0)
                                            true
                                            (swal-f5 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                          (pathscomp0 bool
                                            (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              (nateqb (occ b (lay O (natminus j i) a)) 0))
                                            (nateqb (occ b (lay O (natminus j i) a)) 0)
                                            true
                                            (maponpaths bool bool
                                              (lam c
                                                (andb c
                                                  (nateqb (occ b (lay O (natminus j i) a)) 0)))
                                              (nateqb (occ a (lay O (natminus j i) a)) 1)
                                              true
                                              (swal-f6 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                            (swal-f7 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))))))))))))))))))))))
 (defq swal-w1
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths nlist
                                  (lay (lay N (ffind b N) a) (natminus (add i (ffind b N)) i) b)
                                  N)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 nlist
                                  (lay (lay N (ffind b N) a) (natminus (add i (ffind b N)) i) b)
                                  (lay (lay N (ffind b N) a) (ffind b N) b)
                                  N
                                  (maponpaths nat nlist (lam z (lay (lay N (ffind b N) a) z b))
                                    (natminus (add i (ffind b N)) i)
                                    (ffind b N)
                                    (natpluslcancelminus i (ffind b N)))
                                  (pathscomp0 nlist (lay (lay N (ffind b N) a) (ffind b N) b)
                                    (lay N (ffind b N) b)
                                    N
                                    (laylay N (ffind b N) a b)
                                    (pathscomp0 nlist (lay N (ffind b N) b)
                                      (lay N (ffind b N) (lget (ffind b N) N))
                                      N
                                      (maponpaths nat nlist (lam z (lay N (ffind b N) z)) b
                                        (lget (ffind b N) N)
                                        (pathsinv0 nat (lget (ffind b N) N) b
                                          (lgetffind b N
                                            (maponpaths nat bool (lam z (niszero z)) (occ b N) 1
                                              e6))))
                                      (layget N (ffind b N))))))))))))))))))))
 (defq swal-w2
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths nlist
                                  (lay (lay O (natminus j i) a)
                                    (ffind a (lay O (natminus j i) a))
                                    b)
                                  O)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 nlist
                                  (lay (lay O (natminus j i) a)
                                    (ffind a (lay O (natminus j i) a))
                                    b)
                                  (lay (lay O (natminus j i) a) (natminus j i) b)
                                  O
                                  (maponpaths nat nlist
                                    (lam z (lay (lay O (natminus j i) a) z b))
                                    (ffind a (lay O (natminus j i) a))
                                    (natminus j i)
                                    (ffindlay O (natminus j i) a e5
                                      (natminusltl i j (nlen O) e1 e2)))
                                  (pathscomp0 nlist
                                    (lay (lay O (natminus j i) a) (natminus j i) b)
                                    (lay O (natminus j i) b)
                                    O
                                    (laylay O (natminus j i) a b)
                                    (pathscomp0 nlist (lay O (natminus j i) b)
                                      (lay O (natminus j i) (lget (natminus j i) O))
                                      O
                                      (maponpaths nat nlist (lam z (lay O (natminus j i) z)) b
                                        (lget (natminus j i) O)
                                        (pathsinv0 nat (lget (natminus j i) O) b e3))
                                      (layget O (natminus j i))))))))))))))))))))
 (defq swal-seat
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths nat (add i (ffind a (lay O (natminus j i) a))) j)))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 nat (add i (ffind a (lay O (natminus j i) a)))
                                  (add i (natminus j i))
                                  j
                                  (maponpaths nat nat (lam z (add i z))
                                    (ffind a (lay O (natminus j i) a))
                                    (natminus j i)
                                    (ffindlay O (natminus j i) a e5
                                      (natminusltl i j (nlen O) e1 e2)))
                                  (natlehplusminus i j e1)))))))))))))))))
 (defq swal-pay
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths spair
                                  (tpair
                                    (mkh i
                                      (lay (lay N (ffind b N) a)
                                        (natminus (add i (ffind b N)) i)
                                        b)
                                      (lay (lay O (natminus j i) a)
                                        (ffind a (lay O (natminus j i) a))
                                        b))
                                    (mke (add i (ffind a (lay O (natminus j i) a))) b a))
                                  (tpair (mkh i N O) (mke j b a)))))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 spair
                                  (tpair
                                    (mkh i
                                      (lay (lay N (ffind b N) a)
                                        (natminus (add i (ffind b N)) i)
                                        b)
                                      (lay (lay O (natminus j i) a)
                                        (ffind a (lay O (natminus j i) a))
                                        b))
                                    (mke (add i (ffind a (lay O (natminus j i) a))) b a))
                                  (tpair (mkh i N O)
                                    (mke (add i (ffind a (lay O (natminus j i) a))) b a))
                                  (tpair (mkh i N O) (mke j b a))
                                  (maponpaths hunk spair
                                    (lam z
                                      (tpair z
                                        (mke (add i (ffind a (lay O (natminus j i) a))) b a)))
                                    (mkh i
                                      (lay (lay N (ffind b N) a)
                                        (natminus (add i (ffind b N)) i)
                                        b)
                                      (lay (lay O (natminus j i) a)
                                        (ffind a (lay O (natminus j i) a))
                                        b))
                                    (mkh i N O)
                                    (pathscomp0 hunk
                                      (mkh i
                                        (lay (lay N (ffind b N) a)
                                          (natminus (add i (ffind b N)) i)
                                          b)
                                        (lay (lay O (natminus j i) a)
                                          (ffind a (lay O (natminus j i) a))
                                          b))
                                      (mkh i N
                                        (lay (lay O (natminus j i) a)
                                          (ffind a (lay O (natminus j i) a))
                                          b))
                                      (mkh i N O)
                                      (maponpaths nlist hunk
                                        (lam z
                                          (mkh i z
                                            (lay (lay O (natminus j i) a)
                                              (ffind a (lay O (natminus j i) a))
                                              b)))
                                        (lay (lay N (ffind b N) a)
                                          (natminus (add i (ffind b N)) i)
                                          b)
                                        N
                                        (swal-w1 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                      (maponpaths nlist hunk (lam z (mkh i N z))
                                        (lay (lay O (natminus j i) a)
                                          (ffind a (lay O (natminus j i) a))
                                          b)
                                        O
                                        (swal-w2 i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))))
                                  (maponpaths epatch spair (lam z (tpair (mkh i N O) z))
                                    (mke (add i (ffind a (lay O (natminus j i) a))) b a)
                                    (mke j b a)
                                    (maponpaths nat epatch (lam z (mke z b a))
                                      (add i (ffind a (lay O (natminus j i) a)))
                                      j
                                      (swal-seat i O N j a b e1 e2 e3 e4 e5 e6 e7 e8)))))))))))))))))))
 (defq swal-core
   (pi i nat
      (pi O nlist
        (pi N nlist
          (pi j nat
            (pi a nat
              (pi b nat
                (pi e1 (paths bool (natgtb i j) false)
                  (pi e2 (paths bool (natgtb (add i (nlen O)) j) true)
                    (pi e3 (paths nat (lget (natminus j i) O) b)
                      (pi e4 (paths nat (occ b O) 1)
                        (pi e5 (paths nat (occ a O) 0)
                          (pi e6 (paths nat (occ b N) 1)
                            (pi e7 (paths nat (occ a N) 0)
                              (pi e8 (paths bool (nateqb a b) false)
                                (paths (coprod spair unit)
                                  (swal (mke (add i (ffind b N)) b a)
                                    (mkh i (lay N (ffind b N) a) (lay O (natminus j i) a)))
                                  (ii1 (tpair (mkh i N O) (mke j b a))))))))))))))))))
   (lam i
      (lam O
        (lam N
          (lam j
            (lam a
              (lam b
                (lam e1
                  (lam e2
                    (lam e3
                      (lam e4
                        (lam e5
                          (lam e6
                            (lam e7
                              (lam e8
                                (pathscomp0 (coprod spair unit)
                                  (swal (mke (add i (ffind b N)) b a)
                                    (mkh i (lay N (ffind b N) a) (lay O (natminus j i) a)))
                                  (ii1
                                    (tpair
                                      (mkh i
                                        (lay (lay N (ffind b N) a)
                                          (natminus (add i (ffind b N)) i)
                                          b)
                                        (lay (lay O (natminus j i) a)
                                          (ffind a (lay O (natminus j i) a))
                                          b))
                                      (mke (add i (ffind a (lay O (natminus j i) a))) b a)))
                                  (ii1 (tpair (mkh i N O) (mke j b a)))
                                  (maponpaths bool (coprod spair unit)
                                    (lam c
                                      (bool_rect (lam c2 (coprod spair unit))
                                        (ii1
                                          (tpair
                                            (mkh i
                                              (lay (lay N (ffind b N) a)
                                                (natminus (add i (ffind b N)) i)
                                                b)
                                              (lay (lay O (natminus j i) a)
                                                (ffind a (lay O (natminus j i) a))
                                                b))
                                            (mke (add i (ffind a (lay O (natminus j i) a))) b a)))
                                        (ii2 tt)
                                        c))
                                    (andb (negb (natgtb i (add i (ffind b N))))
                                      (andb
                                        (natgtb (add i (nlen (lay N (ffind b N) a)))
                                          (add i (ffind b N)))
                                        (andb
                                          (nateqb
                                            (lget (natminus (add i (ffind b N)) i)
                                              (lay N (ffind b N) a))
                                            a)
                                          (andb (nateqb (occ a (lay N (ffind b N) a)) 1)
                                            (andb (nateqb (occ b (lay N (ffind b N) a)) 0)
                                              (andb (nateqb (occ a (lay O (natminus j i) a)) 1)
                                                (nateqb (occ b (lay O (natminus j i) a)) 0)))))))
                                    true
                                    (swal-fg i O N j a b e1 e2 e3 e4 e5 e6 e7 e8))
                                  (maponpaths spair (coprod spair unit) (lam z (ii1 z))
                                    (tpair
                                      (mkh i
                                        (lay (lay N (ffind b N) a)
                                          (natminus (add i (ffind b N)) i)
                                          b)
                                        (lay (lay O (natminus j i) a)
                                          (ffind a (lay O (natminus j i) a))
                                          b))
                                      (mke (add i (ffind a (lay O (natminus j i) a))) b a))
                                    (tpair (mkh i N O) (mke j b a))
                                    (swal-pay i O N j a b e1 e2 e3 e4 e5 e6 e7 e8)))))))))))))))))))

; --- (L1) the swallow commute is INVOLUTIVE through the groupoid inverse:
; --- invert both results, swal again, and the originals come home. one
; --- dependent bool_rect on the folded guard (an idpath convoy remembers
; --- the verdict; the refusing branch discriminates through scmside),
; --- andbtruel/r unstack the seven verdicts, a/b distinct falls out of
; --- the counts (1 = 0 is succ-on-zero absurd), ii1 injects through a
; --- coprod_rect projection, and ONE transport carries the goal from the
; --- computed pair to the hypothesis' -- sigma-eta does the records ---
(assert
 (defq swal-involutive
   (pi p epatch
      (pi s hunk
        (pi sq hunk
          (pi dq epatch
            (pi h (paths (coprod spair unit) (swal p s) (ii1 (tpair sq dq)))
              (paths (coprod spair unit) (swal (einv dq) (hinv sq))
                (ii1 (tpair (hinv s) (einv p)))))))))
   (lam p
      (lam s
        (lam sq
          (lam dq
            (lam h
              ((bool_rect
                 (lam c
                   (pi hc
                     (paths bool
                       (andb (negb (natgtb (hi s) (ej p)))
                         (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                           (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                             (andb (nateqb (occ (eb p) (ho s)) 1)
                               (andb (nateqb (occ (ea p) (ho s)) 0)
                                 (andb (nateqb (occ (eb p) (hn s)) 1)
                                   (nateqb (occ (ea p) (hn s)) 0)))))))
                       c)
                     (pi h2
                       (paths (coprod spair unit)
                         (bool_rect (lam c2 (coprod spair unit))
                           (ii1
                             (tpair
                               (mkh (hi s) (lay (ho s) (natminus (ej p) (hi s)) (ea p))
                                 (lay (hn s) (ffind (eb p) (hn s)) (ea p)))
                               (mke (add (hi s) (ffind (eb p) (hn s))) (ea p) (eb p))))
                           (ii2 tt)
                           c)
                         (ii1 (tpair sq dq)))
                       (paths (coprod spair unit) (swal (einv dq) (hinv sq))
                         (ii1 (tpair (hinv s) (einv p)))))))
                 (lam hc
                   (lam h2
                     (transportf spair
                       (lam u
                         (paths (coprod spair unit) (swal (einv (pr2 u)) (hinv (pr1 u)))
                           (ii1 (tpair (hinv s) (einv p)))))
                       (tpair
                         (mkh (hi s) (lay (ho s) (natminus (ej p) (hi s)) (ea p))
                           (lay (hn s) (ffind (eb p) (hn s)) (ea p)))
                         (mke (add (hi s) (ffind (eb p) (hn s))) (ea p) (eb p)))
                       (tpair sq dq)
                       (maponpaths (coprod spair unit) spair
                         (lam c
                           (coprod_rect (lam c2 spair) (lam s2 s2)
                             (lam u2
                               (tpair
                                 (mkh (hi s) (lay (ho s) (natminus (ej p) (hi s)) (ea p))
                                   (lay (hn s) (ffind (eb p) (hn s)) (ea p)))
                                 (mke (add (hi s) (ffind (eb p) (hn s))) (ea p) (eb p))))
                             c))
                         (ii1
                           (tpair
                             (mkh (hi s) (lay (ho s) (natminus (ej p) (hi s)) (ea p))
                               (lay (hn s) (ffind (eb p) (hn s)) (ea p)))
                             (mke (add (hi s) (ffind (eb p) (hn s))) (ea p) (eb p))))
                         (ii1 (tpair sq dq))
                         h2)
                       (swal-core (hi s) (ho s) (hn s) (ej p) (ea p) (eb p)
                         (negbtrue (natgtb (hi s) (ej p))
                           (andbtruel (negb (natgtb (hi s) (ej p)))
                             (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                               (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                 (andb (nateqb (occ (eb p) (ho s)) 1)
                                   (andb (nateqb (occ (ea p) (ho s)) 0)
                                     (andb (nateqb (occ (eb p) (hn s)) 1)
                                       (nateqb (occ (ea p) (hn s)) 0))))))
                             hc))
                         (andbtruel (natgtb (add (hi s) (nlen (ho s))) (ej p))
                           (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                             (andb (nateqb (occ (eb p) (ho s)) 1)
                               (andb (nateqb (occ (ea p) (ho s)) 0)
                                 (andb (nateqb (occ (eb p) (hn s)) 1)
                                   (nateqb (occ (ea p) (hn s)) 0)))))
                           (andbtruer (negb (natgtb (hi s) (ej p)))
                             (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                               (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                 (andb (nateqb (occ (eb p) (ho s)) 1)
                                   (andb (nateqb (occ (ea p) (ho s)) 0)
                                     (andb (nateqb (occ (eb p) (hn s)) 1)
                                       (nateqb (occ (ea p) (hn s)) 0))))))
                             hc))
                         (nateqbtopaths (lget (natminus (ej p) (hi s)) (ho s)) (eb p)
                           (andbtruel (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                             (andb (nateqb (occ (eb p) (ho s)) 1)
                               (andb (nateqb (occ (ea p) (ho s)) 0)
                                 (andb (nateqb (occ (eb p) (hn s)) 1)
                                   (nateqb (occ (ea p) (hn s)) 0))))
                             (andbtruer (natgtb (add (hi s) (nlen (ho s))) (ej p))
                               (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                 (andb (nateqb (occ (eb p) (ho s)) 1)
                                   (andb (nateqb (occ (ea p) (ho s)) 0)
                                     (andb (nateqb (occ (eb p) (hn s)) 1)
                                       (nateqb (occ (ea p) (hn s)) 0)))))
                               (andbtruer (negb (natgtb (hi s) (ej p)))
                                 (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                   (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                     (andb (nateqb (occ (eb p) (ho s)) 1)
                                       (andb (nateqb (occ (ea p) (ho s)) 0)
                                         (andb (nateqb (occ (eb p) (hn s)) 1)
                                           (nateqb (occ (ea p) (hn s)) 0))))))
                                 hc))))
                         (nateqbtopaths (occ (eb p) (ho s)) 1
                           (andbtruel (nateqb (occ (eb p) (ho s)) 1)
                             (andb (nateqb (occ (ea p) (ho s)) 0)
                               (andb (nateqb (occ (eb p) (hn s)) 1)
                                 (nateqb (occ (ea p) (hn s)) 0)))
                             (andbtruer (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                               (andb (nateqb (occ (eb p) (ho s)) 1)
                                 (andb (nateqb (occ (ea p) (ho s)) 0)
                                   (andb (nateqb (occ (eb p) (hn s)) 1)
                                     (nateqb (occ (ea p) (hn s)) 0))))
                               (andbtruer (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                 (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                   (andb (nateqb (occ (eb p) (ho s)) 1)
                                     (andb (nateqb (occ (ea p) (ho s)) 0)
                                       (andb (nateqb (occ (eb p) (hn s)) 1)
                                         (nateqb (occ (ea p) (hn s)) 0)))))
                                 (andbtruer (negb (natgtb (hi s) (ej p)))
                                   (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                     (andb
                                       (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                       (andb (nateqb (occ (eb p) (ho s)) 1)
                                         (andb (nateqb (occ (ea p) (ho s)) 0)
                                           (andb (nateqb (occ (eb p) (hn s)) 1)
                                             (nateqb (occ (ea p) (hn s)) 0))))))
                                   hc)))))
                         (nateqbtopaths (occ (ea p) (ho s)) 0
                           (andbtruel (nateqb (occ (ea p) (ho s)) 0)
                             (andb (nateqb (occ (eb p) (hn s)) 1) (nateqb (occ (ea p) (hn s)) 0))
                             (andbtruer (nateqb (occ (eb p) (ho s)) 1)
                               (andb (nateqb (occ (ea p) (ho s)) 0)
                                 (andb (nateqb (occ (eb p) (hn s)) 1)
                                   (nateqb (occ (ea p) (hn s)) 0)))
                               (andbtruer (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                 (andb (nateqb (occ (eb p) (ho s)) 1)
                                   (andb (nateqb (occ (ea p) (ho s)) 0)
                                     (andb (nateqb (occ (eb p) (hn s)) 1)
                                       (nateqb (occ (ea p) (hn s)) 0))))
                                 (andbtruer (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                   (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                     (andb (nateqb (occ (eb p) (ho s)) 1)
                                       (andb (nateqb (occ (ea p) (ho s)) 0)
                                         (andb (nateqb (occ (eb p) (hn s)) 1)
                                           (nateqb (occ (ea p) (hn s)) 0)))))
                                   (andbtruer (negb (natgtb (hi s) (ej p)))
                                     (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                       (andb
                                         (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                         (andb (nateqb (occ (eb p) (ho s)) 1)
                                           (andb (nateqb (occ (ea p) (ho s)) 0)
                                             (andb (nateqb (occ (eb p) (hn s)) 1)
                                               (nateqb (occ (ea p) (hn s)) 0))))))
                                     hc))))))
                         (nateqbtopaths (occ (eb p) (hn s)) 1
                           (andbtruel (nateqb (occ (eb p) (hn s)) 1)
                             (nateqb (occ (ea p) (hn s)) 0)
                             (andbtruer (nateqb (occ (ea p) (ho s)) 0)
                               (andb (nateqb (occ (eb p) (hn s)) 1)
                                 (nateqb (occ (ea p) (hn s)) 0))
                               (andbtruer (nateqb (occ (eb p) (ho s)) 1)
                                 (andb (nateqb (occ (ea p) (ho s)) 0)
                                   (andb (nateqb (occ (eb p) (hn s)) 1)
                                     (nateqb (occ (ea p) (hn s)) 0)))
                                 (andbtruer
                                   (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                   (andb (nateqb (occ (eb p) (ho s)) 1)
                                     (andb (nateqb (occ (ea p) (ho s)) 0)
                                       (andb (nateqb (occ (eb p) (hn s)) 1)
                                         (nateqb (occ (ea p) (hn s)) 0))))
                                   (andbtruer (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                     (andb
                                       (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                       (andb (nateqb (occ (eb p) (ho s)) 1)
                                         (andb (nateqb (occ (ea p) (ho s)) 0)
                                           (andb (nateqb (occ (eb p) (hn s)) 1)
                                             (nateqb (occ (ea p) (hn s)) 0)))))
                                     (andbtruer (negb (natgtb (hi s) (ej p)))
                                       (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                         (andb
                                           (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                           (andb (nateqb (occ (eb p) (ho s)) 1)
                                             (andb (nateqb (occ (ea p) (ho s)) 0)
                                               (andb (nateqb (occ (eb p) (hn s)) 1)
                                                 (nateqb (occ (ea p) (hn s)) 0))))))
                                       hc)))))))
                         (nateqbtopaths (occ (ea p) (hn s)) 0
                           (andbtruer (nateqb (occ (eb p) (hn s)) 1)
                             (nateqb (occ (ea p) (hn s)) 0)
                             (andbtruer (nateqb (occ (ea p) (ho s)) 0)
                               (andb (nateqb (occ (eb p) (hn s)) 1)
                                 (nateqb (occ (ea p) (hn s)) 0))
                               (andbtruer (nateqb (occ (eb p) (ho s)) 1)
                                 (andb (nateqb (occ (ea p) (ho s)) 0)
                                   (andb (nateqb (occ (eb p) (hn s)) 1)
                                     (nateqb (occ (ea p) (hn s)) 0)))
                                 (andbtruer
                                   (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                   (andb (nateqb (occ (eb p) (ho s)) 1)
                                     (andb (nateqb (occ (ea p) (ho s)) 0)
                                       (andb (nateqb (occ (eb p) (hn s)) 1)
                                         (nateqb (occ (ea p) (hn s)) 0))))
                                   (andbtruer (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                     (andb
                                       (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                       (andb (nateqb (occ (eb p) (ho s)) 1)
                                         (andb (nateqb (occ (ea p) (ho s)) 0)
                                           (andb (nateqb (occ (eb p) (hn s)) 1)
                                             (nateqb (occ (ea p) (hn s)) 0)))))
                                     (andbtruer (negb (natgtb (hi s) (ej p)))
                                       (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                         (andb
                                           (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                                           (andb (nateqb (occ (eb p) (ho s)) 1)
                                             (andb (nateqb (occ (ea p) (ho s)) 0)
                                               (andb (nateqb (occ (eb p) (hn s)) 1)
                                                 (nateqb (occ (ea p) (hn s)) 0))))))
                                       hc)))))))
                         ((bool_rect
                            (lam c
                              (pi hcc (paths bool (nateqb (ea p) (eb p)) c)
                                (paths bool (nateqb (ea p) (eb p)) false)))
                            (lam hcc
                              (fromempty (paths bool (nateqb (ea p) (eb p)) false)
                                (negpathssx0 0
                                  (pathscomp0 nat 1 (occ (eb p) (ho s)) 0
                                    (pathsinv0 nat (occ (eb p) (ho s)) 1
                                      (nateqbtopaths (occ (eb p) (ho s)) 1
                                        (andbtruel (nateqb (occ (eb p) (ho s)) 1)
                                          (andb (nateqb (occ (ea p) (ho s)) 0)
                                            (andb (nateqb (occ (eb p) (hn s)) 1)
                                              (nateqb (occ (ea p) (hn s)) 0)))
                                          (andbtruer
                                            (nateqb (lget (natminus (ej p) (hi s)) (ho s))
                                              (eb p))
                                            (andb (nateqb (occ (eb p) (ho s)) 1)
                                              (andb (nateqb (occ (ea p) (ho s)) 0)
                                                (andb (nateqb (occ (eb p) (hn s)) 1)
                                                  (nateqb (occ (ea p) (hn s)) 0))))
                                            (andbtruer
                                              (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                              (andb
                                                (nateqb (lget (natminus (ej p) (hi s)) (ho s))
                                                  (eb p))
                                                (andb (nateqb (occ (eb p) (ho s)) 1)
                                                  (andb (nateqb (occ (ea p) (ho s)) 0)
                                                    (andb (nateqb (occ (eb p) (hn s)) 1)
                                                      (nateqb (occ (ea p) (hn s)) 0)))))
                                              (andbtruer (negb (natgtb (hi s) (ej p)))
                                                (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                                  (andb
                                                    (nateqb
                                                      (lget (natminus (ej p) (hi s)) (ho s))
                                                      (eb p))
                                                    (andb (nateqb (occ (eb p) (ho s)) 1)
                                                      (andb (nateqb (occ (ea p) (ho s)) 0)
                                                        (andb (nateqb (occ (eb p) (hn s)) 1)
                                                          (nateqb (occ (ea p) (hn s)) 0))))))
                                                hc))))))
                                    (pathscomp0 nat (occ (eb p) (ho s)) (occ (ea p) (ho s)) 0
                                      (maponpaths nat nat (lam z (occ z (ho s))) (eb p) (ea p)
                                        (pathsinv0 nat (ea p) (eb p)
                                          (nateqbtopaths (ea p) (eb p) hcc)))
                                      (nateqbtopaths (occ (ea p) (ho s)) 0
                                        (andbtruel (nateqb (occ (ea p) (ho s)) 0)
                                          (andb (nateqb (occ (eb p) (hn s)) 1)
                                            (nateqb (occ (ea p) (hn s)) 0))
                                          (andbtruer (nateqb (occ (eb p) (ho s)) 1)
                                            (andb (nateqb (occ (ea p) (ho s)) 0)
                                              (andb (nateqb (occ (eb p) (hn s)) 1)
                                                (nateqb (occ (ea p) (hn s)) 0)))
                                            (andbtruer
                                              (nateqb (lget (natminus (ej p) (hi s)) (ho s))
                                                (eb p))
                                              (andb (nateqb (occ (eb p) (ho s)) 1)
                                                (andb (nateqb (occ (ea p) (ho s)) 0)
                                                  (andb (nateqb (occ (eb p) (hn s)) 1)
                                                    (nateqb (occ (ea p) (hn s)) 0))))
                                              (andbtruer
                                                (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                                (andb
                                                  (nateqb (lget (natminus (ej p) (hi s)) (ho s))
                                                    (eb p))
                                                  (andb (nateqb (occ (eb p) (ho s)) 1)
                                                    (andb (nateqb (occ (ea p) (ho s)) 0)
                                                      (andb (nateqb (occ (eb p) (hn s)) 1)
                                                        (nateqb (occ (ea p) (hn s)) 0)))))
                                                (andbtruer (negb (natgtb (hi s) (ej p)))
                                                  (andb
                                                    (natgtb (add (hi s) (nlen (ho s))) (ej p))
                                                    (andb
                                                      (nateqb
                                                        (lget (natminus (ej p) (hi s)) (ho s))
                                                        (eb p))
                                                      (andb (nateqb (occ (eb p) (ho s)) 1)
                                                        (andb (nateqb (occ (ea p) (ho s)) 0)
                                                          (andb (nateqb (occ (eb p) (hn s)) 1)
                                                            (nateqb (occ (ea p) (hn s)) 0))))))
                                                  hc)))))))))))
                            (lam hcc hcc)
                            (nateqb (ea p) (eb p)))
                           (idpath (nateqb (ea p) (eb p))))))))
                 (lam hc
                   (lam h2
                     (fromempty
                       (paths (coprod spair unit) (swal (einv dq) (hinv sq))
                         (ii1 (tpair (hinv s) (einv p))))
                       (transportf (coprod spair unit) scmside (ii2 tt) (ii1 (tpair sq dq)) h2
                         tt))))
                 (andb (negb (natgtb (hi s) (ej p)))
                   (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                     (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                       (andb (nateqb (occ (eb p) (ho s)) 1)
                         (andb (nateqb (occ (ea p) (ho s)) 0)
                           (andb (nateqb (occ (eb p) (hn s)) 1) (nateqb (occ (ea p) (hn s)) 0))))))))
                (idpath
                  (andb (negb (natgtb (hi s) (ej p)))
                    (andb (natgtb (add (hi s) (nlen (ho s))) (ej p))
                      (andb (nateqb (lget (natminus (ej p) (hi s)) (ho s)) (eb p))
                        (andb (nateqb (occ (eb p) (ho s)) 1)
                          (andb (nateqb (occ (ea p) (ho s)) 0)
                            (andb (nateqb (occ (eb p) (hn s)) 1) (nateqb (occ (ea p) (hn s)) 0))))))))
                h))))))))


; ============================================================================
; the CLASH-SEGMENTS slice: patch.v's seventh form at uu -- two co-valid
; hunks collapse the union HULL of their spans to ONE element, the BUBBLE:
; the hull segment plus each rival rendered as the hull with its hunk laid
; inside, the rival set cins-canonical by content hash. the three laws:
; creation forgets arrival order (M1 at cell grain), padding commutes with
; rendering (the widening law), and the merged length reads off the hull
; alone (the bubble law -- downstream seats determinate before anyone
; resolves). hash stays a pi-bound function; the demos ride lcode.
; ============================================================================
; --- the order tail: asymmetry, gtb-to-noteq, trichotomy, and the mixed
; --- transitivity the head ordering needs -- each a convoy or one triple
; --- rect on the existing istransnatgth/isantisymmnatleh floor ---
(assert
 (defq natgthtoneg
   (pi a nat (pi b nat (pi g (paths bool (natgtb a b) true) (paths bool (natgtb b a) false))))
   (lam a
      (lam b
        (lam g
          ((bool_rect
             (lam c (pi hc (paths bool (natgtb b a) c) (paths bool (natgtb b a) false)))
             (lam hc
               (fromempty (paths bool (natgtb b a) false)
                 (nopathstruetofalse
                   (pathscomp0 bool true (natgtb a a) false
                     (pathsinv0 bool (natgtb a a) true (istransnatgth a b a g hc))
                     (isreflnatleh a)))))
             (lam hc hc)
             (natgtb b a))
            (idpath (natgtb b a)))))))
 (defq natgthtoneq
   (pi a nat (pi b nat (pi g (paths bool (natgtb a b) true) (paths bool (nateqb a b) false))))
   (lam a
      (lam b
        (lam g
          ((bool_rect
             (lam c (pi hc (paths bool (nateqb a b) c) (paths bool (nateqb a b) false)))
             (lam hc
               (fromempty (paths bool (nateqb a b) false)
                 (nopathstruetofalse
                   (pathscomp0 bool true (natgtb a b) false (pathsinv0 bool (natgtb a b) true g)
                     (transportf nat (lam z (paths bool (natgtb a z) false)) a b
                       (nateqbtopaths a b hc)
                       (isreflnatleh a))))))
             (lam hc hc)
             (nateqb a b))
            (idpath (nateqb a b)))))))
 (defq nattrichot
   (pi x nat
      (pi y nat
        (pi hne (paths bool (nateqb x y) false)
          (pi hg (paths bool (natgtb y x) false) (paths bool (natgtb x y) true)))))
   (lam x
      (lam y
        (lam hne
          (lam hg
            ((bool_rect
               (lam c (pi hc (paths bool (natgtb x y) c) (paths bool (natgtb x y) true)))
               (lam hc hc)
               (lam hc
                 (fromempty (paths bool (natgtb x y) true)
                   (nopathstruetofalse
                     (pathscomp0 bool true (nateqb x y) false
                       (pathscomp0 bool true (nateqb x x) (nateqb x y)
                         (pathsinv0 bool (nateqb x x) true (nateqbrefl x))
                         (maponpaths nat bool (lam z (nateqb x z)) x y
                           (isantisymmnatleh x y hc hg)))
                       hne))))
               (natgtb x y))
              (idpath (natgtb x y))))))))
 (defq natlehgthtrans
   (pi h nat
      (pi x nat
        (pi y nat
          (pi h1 (paths bool (natgtb h x) false)
            (pi h2 (paths bool (natgtb h y) true) (paths bool (natgtb x y) true))))))
   (lam h
      (nat_rect
        (lam q
          (pi x nat
            (pi y nat
              (pi h1 (paths bool (natgtb q x) false)
                (pi h2 (paths bool (natgtb q y) true) (paths bool (natgtb x y) true))))))
        (lam x
          (lam y (lam h1 (lam h2 (fromempty (paths bool (natgtb x y) true) (negnatgth0n y h2))))))
        (lam k
          (lam IH
            (lam x
              (nat_rect
                (lam r
                  (pi y nat
                    (pi h1 (paths bool (natgtb (succ k) r) false)
                      (pi h2 (paths bool (natgtb (succ k) y) true)
                        (paths bool (natgtb r y) true)))))
                (lam y
                  (lam h1
                    (lam h2 (fromempty (paths bool (natgtb 0 y) true) (nopathstruetofalse h1)))))
                (lam x2
                  (lam xIH
                    (lam y
                      (nat_rect
                        (lam s
                          (pi h1 (paths bool (natgtb (succ k) (succ x2)) false)
                            (pi h2 (paths bool (natgtb (succ k) s) true)
                              (paths bool (natgtb (succ x2) s) true))))
                        (lam h1 (lam h2 (natgthsn0 x2)))
                        (lam y2 (lam yIH (lam h1 (lam h2 (IH x2 y2 h1 h2)))))
                        y))))
                x))))
        h))))

; --- min and max, recursive (succ-succ definitional), with comm and the
; --- two bounds the hull arithmetic reads; natminusswap re-associates a
; --- double truncated-minus through the two cancellations ---
(assert
 (defn natmin
   (pi a nat (pi b nat nat))
   (lam a
      (nat_rect (lam q (pi b nat nat)) (lam b 0)
        (lam k (lam IH (lam b (bool_rect (lam c nat) 0 (succ (IH (npred b))) (niszero b)))))
        a)))
 (defn natmax
   (pi a nat (pi b nat nat))
   (lam a
      (nat_rect (lam q (pi b nat nat)) (lam b b)
        (lam k
          (lam IH (lam b (bool_rect (lam c nat) (succ k) (succ (IH (npred b))) (niszero b)))))
        a)))
 (defq natmincomm
   (pi a nat (pi b nat (paths nat (natmin a b) (natmin b a))))
   (lam a
      (nat_rect (lam q (pi b nat (paths nat (natmin q b) (natmin b q))))
        (lam b
          (nat_rect (lam r (paths nat (natmin 0 r) (natmin r 0))) (idpath 0)
            (lam j (lam r (idpath 0)))
            b))
        (lam k
          (lam IH
            (lam b
              (nat_rect (lam r (paths nat (natmin (succ k) r) (natmin r (succ k)))) (idpath 0)
                (lam j
                  (lam r (maponpaths nat nat (lam z (succ z)) (natmin k j) (natmin j k) (IH j))))
                b))))
        a)))
 (defq natmaxcomm
   (pi a nat (pi b nat (paths nat (natmax a b) (natmax b a))))
   (lam a
      (nat_rect (lam q (pi b nat (paths nat (natmax q b) (natmax b q))))
        (lam b
          (nat_rect (lam r (paths nat (natmax 0 r) (natmax r 0))) (idpath 0)
            (lam j (lam r (idpath (succ j))))
            b))
        (lam k
          (lam IH
            (lam b
              (nat_rect (lam r (paths nat (natmax (succ k) r) (natmax r (succ k))))
                (idpath (succ k))
                (lam j
                  (lam r (maponpaths nat nat (lam z (succ z)) (natmax k j) (natmax j k) (IH j))))
                b))))
        a)))
 (defq natminlel
   (pi a nat (pi b nat (paths bool (natgtb (natmin a b) a) false)))
   (lam a
      (nat_rect (lam q (pi b nat (paths bool (natgtb (natmin q b) q) false)))
        (lam b
          (nat_rect (lam r (paths bool (natgtb (natmin 0 r) 0) false)) (idpath false)
            (lam j (lam r (idpath false)))
            b))
        (lam k
          (lam IH
            (lam b
              (nat_rect (lam r (paths bool (natgtb (natmin (succ k) r) (succ k)) false))
                (idpath false)
                (lam j (lam r (IH j)))
                b))))
        a)))
 (defq natmaxgel
   (pi a nat (pi b nat (paths bool (natgtb a (natmax a b)) false)))
   (lam a
      (nat_rect (lam q (pi b nat (paths bool (natgtb q (natmax q b)) false)))
        (lam b
          (nat_rect (lam r (paths bool (natgtb 0 (natmax 0 r)) false)) (idpath false)
            (lam j (lam r (idpath false)))
            b))
        (lam k
          (lam IH
            (lam b
              (nat_rect (lam r (paths bool (natgtb (succ k) (natmax (succ k) r)) false))
                (isreflnatleh (succ k))
                (lam j (lam r (IH j)))
                b))))
        a)))
 (defq natminusswap
   (pi l nat
      (pi u nat
        (pi m nat
          (pi h1 (paths bool (natgtb l u) false)
            (pi h2 (paths bool (natgtb u m) false)
              (paths nat (add l (natminus m u)) (natminus m (natminus u l))))))))
   (lam l
      (lam u
        (lam m
          (lam h1
            (lam h2
              (pathsinv0 nat (natminus m (natminus u l)) (add l (natminus m u))
                (pathscomp0 nat (natminus m (natminus u l))
                  (natminus (add u (natminus m u)) (natminus u l))
                  (add l (natminus m u))
                  (maponpaths nat nat (lam z (natminus z (natminus u l))) m
                    (add u (natminus m u))
                    (pathsinv0 nat (add u (natminus m u)) m (natlehplusminus u m h2)))
                  (pathscomp0 nat (natminus (add u (natminus m u)) (natminus u l))
                    (natminus (add (add l (natminus u l)) (natminus m u)) (natminus u l))
                    (add l (natminus m u))
                    (maponpaths nat nat (lam z (natminus (add z (natminus m u)) (natminus u l)))
                      u
                      (add l (natminus u l))
                      (pathsinv0 nat (add l (natminus u l)) u (natlehplusminus l u h1)))
                    (pathscomp0 nat
                      (natminus (add (add l (natminus u l)) (natminus m u)) (natminus u l))
                      (natminus (add (add l (natminus m u)) (natminus u l)) (natminus u l))
                      (add l (natminus m u))
                      (maponpaths nat nat (lam z (natminus z (natminus u l)))
                        (add (add l (natminus u l)) (natminus m u))
                        (add (add l (natminus m u)) (natminus u l))
                        (natplusswap l (natminus u l) (natminus m u)))
                      (natpluscancelminus (add l (natminus m u)) (natminus u l)))))))))))))

; --- the canonical rival set: sorted insert with dedup. the three sealed
; --- reduction rungs (dedup / new head / skip) make every comm/absorb leaf
; --- a short chain; comm's case tree rides the order tail exactly where
; --- patch.v's destructs rode lia ---
(assert
 (defn cins
   (pi x nat (pi l nlist nlist))
   (lam x
      (lam l
        ((nat_rect (lam q (pi v (nvec q) nlist)) (lam v (ncons x nnil))
           (lam m
             (lam IH
               (lam v
                 (bool_rect (lam c nlist) (tpair (succ m) v)
                   (bool_rect (lam c nlist) (ncons x (tpair (succ m) v))
                     (ncons (pr1 v) (IH (pr2 v)))
                     (natgtb (pr1 v) x))
                   (nateqb x (pr1 v))))))
           (pr1 l))
          (pr2 l)))))
 (defq cinsdedup
   (pi x nat
      (pi h nat
        (pi t nlist
          (pi e (paths bool (nateqb x h) true) (paths nlist (cins x (ncons h t)) (ncons h t))))))
   (lam x
      (lam h
        (lam t
          (lam e
            (maponpaths bool nlist
              (lam c
                (bool_rect (lam c2 nlist) (ncons h t)
                  (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t))
                    (natgtb h x))
                  c))
              (nateqb x h)
              true
              e))))))
 (defq cinshead
   (pi x nat
      (pi h nat
        (pi t nlist
          (pi e1 (paths bool (nateqb x h) false)
            (pi e2 (paths bool (natgtb h x) true)
              (paths nlist (cins x (ncons h t)) (ncons x (ncons h t))))))))
   (lam x
      (lam h
        (lam t
          (lam e1
            (lam e2
              (pathscomp0 nlist (cins x (ncons h t))
                (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t))
                  (natgtb h x))
                (ncons x (ncons h t))
                (maponpaths bool nlist
                  (lam c
                    (bool_rect (lam c2 nlist) (ncons h t)
                      (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t))
                        (natgtb h x))
                      c))
                  (nateqb x h)
                  false
                  e1)
                (maponpaths bool nlist
                  (lam c (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t)) c))
                  (natgtb h x)
                  true
                  e2))))))))
 (defq cinsskip
   (pi x nat
      (pi h nat
        (pi t nlist
          (pi e1 (paths bool (nateqb x h) false)
            (pi e2 (paths bool (natgtb h x) false)
              (paths nlist (cins x (ncons h t)) (ncons h (cins x t))))))))
   (lam x
      (lam h
        (lam t
          (lam e1
            (lam e2
              (pathscomp0 nlist (cins x (ncons h t))
                (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t))
                  (natgtb h x))
                (ncons h (cins x t))
                (maponpaths bool nlist
                  (lam c
                    (bool_rect (lam c2 nlist) (ncons h t)
                      (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t))
                        (natgtb h x))
                      c))
                  (nateqb x h)
                  false
                  e1)
                (maponpaths bool nlist
                  (lam c (bool_rect (lam c3 nlist) (ncons x (ncons h t)) (ncons h (cins x t)) c))
                  (natgtb h x)
                  false
                  e2))))))))
 (defq cinsabsorb
   (pi x nat (pi l nlist (paths nlist (cins x (cins x l)) (cins x l))))
   (lam x
      (lam l
        ((nat_rect
           (lam q
             (pi v (nvec q) (paths nlist (cins x (cins x (tpair q v))) (cins x (tpair q v)))))
           (lam v (cinsdedup x x nnil (nateqbrefl x)))
           (lam m
             (lam IH
               (lam v
                 ((bool_rect
                    (lam c
                      (pi he (paths bool (nateqb x (pr1 v)) c)
                        (paths nlist (cins x (cins x (tpair (succ m) v)))
                          (cins x (tpair (succ m) v)))))
                    (lam he
                      (maponpaths nlist nlist (lam z (cins x z))
                        (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                        (ncons (pr1 v) (tpair m (pr2 v)))
                        (cinsdedup x (pr1 v) (tpair m (pr2 v)) he)))
                    (lam he
                      ((bool_rect
                         (lam c
                           (pi hg (paths bool (natgtb (pr1 v) x) c)
                             (paths nlist (cins x (cins x (tpair (succ m) v)))
                               (cins x (tpair (succ m) v)))))
                         (lam hg
                           (pathscomp0 nlist (cins x (cins x (tpair (succ m) v)))
                             (cins x (ncons x (tpair (succ m) v)))
                             (cins x (tpair (succ m) v))
                             (maponpaths nlist nlist (lam z (cins x z))
                               (cins x (tpair (succ m) v))
                               (ncons x (tpair (succ m) v))
                               (cinshead x (pr1 v) (tpair m (pr2 v)) he hg))
                             (pathscomp0 nlist (cins x (ncons x (tpair (succ m) v)))
                               (ncons x (tpair (succ m) v))
                               (cins x (tpair (succ m) v))
                               (cinsdedup x x (tpair (succ m) v) (nateqbrefl x))
                               (pathsinv0 nlist (cins x (tpair (succ m) v))
                                 (ncons x (tpair (succ m) v))
                                 (cinshead x (pr1 v) (tpair m (pr2 v)) he hg)))))
                         (lam hg
                           (pathscomp0 nlist (cins x (cins x (tpair (succ m) v)))
                             (cins x (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                             (cins x (tpair (succ m) v))
                             (maponpaths nlist nlist (lam z (cins x z))
                               (cins x (tpair (succ m) v))
                               (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                               (cinsskip x (pr1 v) (tpair m (pr2 v)) he hg))
                             (pathscomp0 nlist
                               (cins x (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                               (ncons (pr1 v) (cins x (cins x (tpair m (pr2 v)))))
                               (cins x (tpair (succ m) v))
                               (cinsskip x (pr1 v) (cins x (tpair m (pr2 v))) he hg)
                               (pathscomp0 nlist
                                 (ncons (pr1 v) (cins x (cins x (tpair m (pr2 v)))))
                                 (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                 (cins x (tpair (succ m) v))
                                 (maponpaths nlist nlist (lam z (ncons (pr1 v) z))
                                   (cins x (cins x (tpair m (pr2 v))))
                                   (cins x (tpair m (pr2 v)))
                                   (IH (pr2 v)))
                                 (pathsinv0 nlist (cins x (tpair (succ m) v))
                                   (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                   (cinsskip x (pr1 v) (tpair m (pr2 v)) he hg))))))
                         (natgtb (pr1 v) x))
                        (idpath (natgtb (pr1 v) x))))
                    (nateqb x (pr1 v)))
                   (idpath (nateqb x (pr1 v)))))))
           (pr1 l))
          (pr2 l)))))
 (defq cinscomm
   (pi x nat (pi y nat (pi l nlist (paths nlist (cins x (cins y l)) (cins y (cins x l))))))
   (lam x
      (lam y
        (lam l
          ((nat_rect
             (lam q
               (pi v (nvec q)
                 (paths nlist (cins x (cins y (tpair q v))) (cins y (cins x (tpair q v))))))
             (lam v
               ((bool_rect
                  (lam c
                    (pi hxy (paths bool (nateqb x y) c)
                      (paths nlist (cins x (cins y (tpair 0 v))) (cins y (cins x (tpair 0 v))))))
                  (lam hxy
                    (transportf nat
                      (lam z
                        (paths nlist (cins x (cins z (tpair 0 v))) (cins z (cins x (tpair 0 v)))))
                      x
                      y
                      (nateqbtopaths x y hxy)
                      (idpath (cins x (cins x (tpair 0 v))))))
                  (lam hxy
                    ((bool_rect
                       (lam c
                         (pi hyx (paths bool (natgtb y x) c)
                           (paths nlist (cins x (cins y (tpair 0 v)))
                             (cins y (cins x (tpair 0 v))))))
                       (lam hyx
                         (pathscomp0 nlist (cins x (cins y (tpair 0 v)))
                           (ncons x (ncons y nnil))
                           (cins y (cins x (tpair 0 v)))
                           (cinshead x y nnil hxy hyx)
                           (pathsinv0 nlist (cins y (ncons x nnil)) (ncons x (cins y nnil))
                             (cinsskip y x nnil
                               (pathscomp0 bool (nateqb y x) (nateqb x y) false (nateqbsymm y x)
                                 hxy)
                               (natgthtoneg y x hyx)))))
                       (lam hyx
                         (pathscomp0 nlist (cins x (cins y (tpair 0 v))) (ncons y (cins x nnil))
                           (cins y (cins x (tpair 0 v)))
                           (cinsskip x y nnil hxy hyx)
                           (pathsinv0 nlist (cins y (ncons x nnil)) (ncons y (ncons x nnil))
                             (cinshead y x nnil
                               (pathscomp0 bool (nateqb y x) (nateqb x y) false (nateqbsymm y x)
                                 hxy)
                               (nattrichot x y hxy hyx)))))
                       (natgtb y x))
                      (idpath (natgtb y x))))
                  (nateqb x y))
                 (idpath (nateqb x y))))
             (lam m
               (lam IH
                 (lam v
                   ((bool_rect
                      (lam c
                        (pi hyh (paths bool (nateqb y (pr1 v)) c)
                          (paths nlist (cins x (cins y (tpair (succ m) v)))
                            (cins y (cins x (tpair (succ m) v))))))
                      (lam hyh
                        (transportf nat
                          (lam z
                            (paths nlist (cins x (cins z (tpair (succ m) v)))
                              (cins z (cins x (tpair (succ m) v)))))
                          (pr1 v)
                          y
                          (pathsinv0 nat y (pr1 v) (nateqbtopaths y (pr1 v) hyh))
                          ((bool_rect
                             (lam c
                               (pi hxh (paths bool (nateqb x (pr1 v)) c)
                                 (paths nlist (cins x (cins (pr1 v) (tpair (succ m) v)))
                                   (cins (pr1 v) (cins x (tpair (succ m) v))))))
                             (lam hxh
                               (transportf nat
                                 (lam w
                                   (paths nlist (cins w (cins (pr1 v) (tpair (succ m) v)))
                                     (cins (pr1 v) (cins w (tpair (succ m) v)))))
                                 (pr1 v)
                                 x
                                 (pathsinv0 nat x (pr1 v) (nateqbtopaths x (pr1 v) hxh))
                                 (idpath (cins (pr1 v) (cins (pr1 v) (tpair (succ m) v))))))
                             (lam hxh
                               ((bool_rect
                                  (lam c
                                    (pi hhx (paths bool (natgtb (pr1 v) x) c)
                                      (paths nlist (cins x (cins (pr1 v) (tpair (succ m) v)))
                                        (cins (pr1 v) (cins x (tpair (succ m) v))))))
                                  (lam hhx
                                    (pathscomp0 nlist (cins x (cins (pr1 v) (tpair (succ m) v)))
                                      (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                      (cins (pr1 v) (cins x (tpair (succ m) v)))
                                      (maponpaths nlist nlist (lam z (cins x z))
                                        (cins (pr1 v) (tpair (succ m) v))
                                        (ncons (pr1 v) (tpair m (pr2 v)))
                                        (cinsdedup (pr1 v) (pr1 v) (tpair m (pr2 v))
                                          (nateqbrefl (pr1 v))))
                                      (pathscomp0 nlist
                                        (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                        (ncons x (ncons (pr1 v) (tpair m (pr2 v))))
                                        (cins (pr1 v) (cins x (tpair (succ m) v)))
                                        (cinshead x (pr1 v) (tpair m (pr2 v)) hxh hhx)
                                        (pathscomp0 nlist
                                          (ncons x (ncons (pr1 v) (tpair m (pr2 v))))
                                          (ncons x
                                            (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v)))))
                                          (cins (pr1 v) (cins x (tpair (succ m) v)))
                                          (pathsinv0 nlist
                                            (ncons x
                                              (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v)))))
                                            (ncons x (ncons (pr1 v) (tpair m (pr2 v))))
                                            (maponpaths nlist nlist (lam z (ncons x z))
                                              (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v))))
                                              (ncons (pr1 v) (tpair m (pr2 v)))
                                              (cinsdedup (pr1 v) (pr1 v) (tpair m (pr2 v))
                                                (nateqbrefl (pr1 v)))))
                                          (pathscomp0 nlist
                                            (ncons x
                                              (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v)))))
                                            (cins (pr1 v)
                                              (ncons x (ncons (pr1 v) (tpair m (pr2 v)))))
                                            (cins (pr1 v) (cins x (tpair (succ m) v)))
                                            (pathsinv0 nlist
                                              (cins (pr1 v)
                                                (ncons x (ncons (pr1 v) (tpair m (pr2 v)))))
                                              (ncons x
                                                (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v)))))
                                              (cinsskip (pr1 v) x
                                                (ncons (pr1 v) (tpair m (pr2 v)))
                                                (pathscomp0 bool (nateqb (pr1 v) x)
                                                  (nateqb x (pr1 v))
                                                  false
                                                  (nateqbsymm (pr1 v) x)
                                                  hxh)
                                                (natgthtoneg (pr1 v) x hhx)))
                                            (pathsinv0 nlist
                                              (cins (pr1 v) (cins x (tpair (succ m) v)))
                                              (cins (pr1 v)
                                                (ncons x (ncons (pr1 v) (tpair m (pr2 v)))))
                                              (maponpaths nlist nlist (lam z (cins (pr1 v) z))
                                                (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                                (ncons x (ncons (pr1 v) (tpair m (pr2 v))))
                                                (cinshead x (pr1 v) (tpair m (pr2 v)) hxh hhx))))))))
                                  (lam hhx
                                    (pathscomp0 nlist (cins x (cins (pr1 v) (tpair (succ m) v)))
                                      (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                      (cins (pr1 v) (cins x (tpair (succ m) v)))
                                      (maponpaths nlist nlist (lam z (cins x z))
                                        (cins (pr1 v) (tpair (succ m) v))
                                        (ncons (pr1 v) (tpair m (pr2 v)))
                                        (cinsdedup (pr1 v) (pr1 v) (tpair m (pr2 v))
                                          (nateqbrefl (pr1 v))))
                                      (pathscomp0 nlist
                                        (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                        (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                        (cins (pr1 v) (cins x (tpair (succ m) v)))
                                        (cinsskip x (pr1 v) (tpair m (pr2 v)) hxh hhx)
                                        (pathscomp0 nlist
                                          (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                          (cins (pr1 v)
                                            (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                          (cins (pr1 v) (cins x (tpair (succ m) v)))
                                          (pathsinv0 nlist
                                            (cins (pr1 v)
                                              (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                            (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                            (cinsdedup (pr1 v) (pr1 v)
                                              (cins x (tpair m (pr2 v)))
                                              (nateqbrefl (pr1 v))))
                                          (pathsinv0 nlist
                                            (cins (pr1 v) (cins x (tpair (succ m) v)))
                                            (cins (pr1 v)
                                              (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                            (maponpaths nlist nlist (lam z (cins (pr1 v) z))
                                              (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                              (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                              (cinsskip x (pr1 v) (tpair m (pr2 v)) hxh hhx)))))))
                                  (natgtb (pr1 v) x))
                                 (idpath (natgtb (pr1 v) x))))
                             (nateqb x (pr1 v)))
                            (idpath (nateqb x (pr1 v))))))
                      (lam hyh
                        ((bool_rect
                           (lam c
                             (pi hxh (paths bool (nateqb x (pr1 v)) c)
                               (paths nlist (cins x (cins y (tpair (succ m) v)))
                                 (cins y (cins x (tpair (succ m) v))))))
                           (lam hxh
                             (transportf nat
                               (lam w
                                 (paths nlist (cins w (cins y (tpair (succ m) v)))
                                   (cins y (cins w (tpair (succ m) v)))))
                               (pr1 v)
                               x
                               (pathsinv0 nat x (pr1 v) (nateqbtopaths x (pr1 v) hxh))
                               ((bool_rect
                                  (lam c
                                    (pi hhy (paths bool (natgtb (pr1 v) y) c)
                                      (paths nlist (cins (pr1 v) (cins y (tpair (succ m) v)))
                                        (cins y (cins (pr1 v) (tpair (succ m) v))))))
                                  (lam hhy
                                    (pathscomp0 nlist (cins (pr1 v) (cins y (tpair (succ m) v)))
                                      (cins (pr1 v) (ncons y (ncons (pr1 v) (tpair m (pr2 v)))))
                                      (cins y (cins (pr1 v) (tpair (succ m) v)))
                                      (maponpaths nlist nlist (lam z (cins (pr1 v) z))
                                        (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                        (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                        (cinshead y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                      (pathscomp0 nlist
                                        (cins (pr1 v)
                                          (ncons y (ncons (pr1 v) (tpair m (pr2 v)))))
                                        (ncons y
                                          (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v)))))
                                        (cins y (cins (pr1 v) (tpair (succ m) v)))
                                        (cinsskip (pr1 v) y (ncons (pr1 v) (tpair m (pr2 v)))
                                          (pathscomp0 bool (nateqb (pr1 v) y) (nateqb y (pr1 v))
                                            false
                                            (nateqbsymm (pr1 v) y)
                                            hyh)
                                          (natgthtoneg (pr1 v) y hhy))
                                        (pathscomp0 nlist
                                          (ncons y
                                            (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v)))))
                                          (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                          (cins y (cins (pr1 v) (tpair (succ m) v)))
                                          (maponpaths nlist nlist (lam z (ncons y z))
                                            (cins (pr1 v) (ncons (pr1 v) (tpair m (pr2 v))))
                                            (ncons (pr1 v) (tpair m (pr2 v)))
                                            (cinsdedup (pr1 v) (pr1 v) (tpair m (pr2 v))
                                              (nateqbrefl (pr1 v))))
                                          (pathscomp0 nlist
                                            (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                            (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                            (cins y (cins (pr1 v) (tpair (succ m) v)))
                                            (pathsinv0 nlist
                                              (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                              (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                              (cinshead y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                            (pathsinv0 nlist
                                              (cins y (cins (pr1 v) (tpair (succ m) v)))
                                              (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                              (maponpaths nlist nlist (lam z (cins y z))
                                                (cins (pr1 v) (tpair (succ m) v))
                                                (ncons (pr1 v) (tpair m (pr2 v)))
                                                (cinsdedup (pr1 v) (pr1 v) (tpair m (pr2 v))
                                                  (nateqbrefl (pr1 v))))))))))
                                  (lam hhy
                                    (pathscomp0 nlist (cins (pr1 v) (cins y (tpair (succ m) v)))
                                      (cins (pr1 v) (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                      (cins y (cins (pr1 v) (tpair (succ m) v)))
                                      (maponpaths nlist nlist (lam z (cins (pr1 v) z))
                                        (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                        (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                        (cinsskip y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                      (pathscomp0 nlist
                                        (cins (pr1 v) (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                        (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                        (cins y (cins (pr1 v) (tpair (succ m) v)))
                                        (cinsdedup (pr1 v) (pr1 v) (cins y (tpair m (pr2 v)))
                                          (nateqbrefl (pr1 v)))
                                        (pathscomp0 nlist
                                          (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                          (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                          (cins y (cins (pr1 v) (tpair (succ m) v)))
                                          (pathsinv0 nlist
                                            (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                            (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                            (cinsskip y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                          (pathsinv0 nlist
                                            (cins y (cins (pr1 v) (tpair (succ m) v)))
                                            (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                            (maponpaths nlist nlist (lam z (cins y z))
                                              (cins (pr1 v) (tpair (succ m) v))
                                              (ncons (pr1 v) (tpair m (pr2 v)))
                                              (cinsdedup (pr1 v) (pr1 v) (tpair m (pr2 v))
                                                (nateqbrefl (pr1 v)))))))))
                                  (natgtb (pr1 v) y))
                                 (idpath (natgtb (pr1 v) y)))))
                           (lam hxh
                             ((bool_rect
                                (lam c
                                  (pi hhy (paths bool (natgtb (pr1 v) y) c)
                                    (paths nlist (cins x (cins y (tpair (succ m) v)))
                                      (cins y (cins x (tpair (succ m) v))))))
                                (lam hhy
                                  ((bool_rect
                                     (lam c
                                       (pi hhx (paths bool (natgtb (pr1 v) x) c)
                                         (paths nlist (cins x (cins y (tpair (succ m) v)))
                                           (cins y (cins x (tpair (succ m) v))))))
                                     (lam hhx
                                       ((bool_rect
                                          (lam c
                                            (pi hxy (paths bool (nateqb x y) c)
                                              (paths nlist (cins x (cins y (tpair (succ m) v)))
                                                (cins y (cins x (tpair (succ m) v))))))
                                          (lam hxy
                                            (transportf nat
                                              (lam w
                                                (paths nlist
                                                  (cins w (cins y (tpair (succ m) v)))
                                                  (cins y (cins w (tpair (succ m) v)))))
                                              y
                                              x
                                              (pathsinv0 nat x y (nateqbtopaths x y hxy))
                                              (idpath (cins y (cins y (tpair (succ m) v))))))
                                          (lam hxy
                                            ((bool_rect
                                               (lam c
                                                 (pi hyx (paths bool (natgtb y x) c)
                                                   (paths nlist
                                                     (cins x (cins y (tpair (succ m) v)))
                                                     (cins y (cins x (tpair (succ m) v))))))
                                               (lam hyx
                                                 (pathscomp0 nlist
                                                   (cins x (cins y (tpair (succ m) v)))
                                                   (cins x
                                                     (ncons y (ncons (pr1 v) (tpair m (pr2 v)))))
                                                   (cins y (cins x (tpair (succ m) v)))
                                                   (maponpaths nlist nlist (lam z (cins x z))
                                                     (cins y (tpair (succ m) v))
                                                     (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                                     (cinshead y (pr1 v) (tpair m (pr2 v)) hyh
                                                       hhy))
                                                   (pathscomp0 nlist
                                                     (cins x
                                                       (ncons y
                                                         (ncons (pr1 v) (tpair m (pr2 v)))))
                                                     (ncons x
                                                       (ncons y
                                                         (ncons (pr1 v) (tpair m (pr2 v)))))
                                                     (cins y (cins x (tpair (succ m) v)))
                                                     (cinshead x y
                                                       (ncons (pr1 v) (tpair m (pr2 v)))
                                                       hxy
                                                       hyx)
                                                     (pathscomp0 nlist
                                                       (ncons x
                                                         (ncons y
                                                           (ncons (pr1 v) (tpair m (pr2 v)))))
                                                       (ncons x
                                                         (cins y
                                                           (ncons (pr1 v) (tpair m (pr2 v)))))
                                                       (cins y (cins x (tpair (succ m) v)))
                                                       (pathsinv0 nlist
                                                         (ncons x
                                                           (cins y
                                                             (ncons (pr1 v) (tpair m (pr2 v)))))
                                                         (ncons x
                                                           (ncons y
                                                             (ncons (pr1 v) (tpair m (pr2 v)))))
                                                         (maponpaths nlist nlist
                                                           (lam z (ncons x z))
                                                           (cins y
                                                             (ncons (pr1 v) (tpair m (pr2 v))))
                                                           (ncons y
                                                             (ncons (pr1 v) (tpair m (pr2 v))))
                                                           (cinshead y (pr1 v) (tpair m (pr2 v))
                                                             hyh
                                                             hhy)))
                                                       (pathscomp0 nlist
                                                         (ncons x
                                                           (cins y
                                                             (ncons (pr1 v) (tpair m (pr2 v)))))
                                                         (cins y
                                                           (ncons x
                                                             (ncons (pr1 v) (tpair m (pr2 v)))))
                                                         (cins y (cins x (tpair (succ m) v)))
                                                         (pathsinv0 nlist
                                                           (cins y
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v)))))
                                                           (ncons x
                                                             (cins y
                                                               (ncons (pr1 v) (tpair m (pr2 v)))))
                                                           (cinsskip y x
                                                             (ncons (pr1 v) (tpair m (pr2 v)))
                                                             (pathscomp0 bool (nateqb y x)
                                                               (nateqb x y)
                                                               false
                                                               (nateqbsymm y x)
                                                               hxy)
                                                             (natgthtoneg y x hyx)))
                                                         (pathsinv0 nlist
                                                           (cins y (cins x (tpair (succ m) v)))
                                                           (cins y
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v)))))
                                                           (maponpaths nlist nlist
                                                             (lam z (cins y z))
                                                             (cins x (tpair (succ m) v))
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v))))
                                                             (cinshead x (pr1 v)
                                                               (tpair m (pr2 v))
                                                               hxh
                                                               hhx))))))))
                                               (lam hyx
                                                 (pathscomp0 nlist
                                                   (cins x (cins y (tpair (succ m) v)))
                                                   (cins x
                                                     (ncons y (ncons (pr1 v) (tpair m (pr2 v)))))
                                                   (cins y (cins x (tpair (succ m) v)))
                                                   (maponpaths nlist nlist (lam z (cins x z))
                                                     (cins y (tpair (succ m) v))
                                                     (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                                     (cinshead y (pr1 v) (tpair m (pr2 v)) hyh
                                                       hhy))
                                                   (pathscomp0 nlist
                                                     (cins x
                                                       (ncons y
                                                         (ncons (pr1 v) (tpair m (pr2 v)))))
                                                     (ncons y
                                                       (cins x (ncons (pr1 v) (tpair m (pr2 v)))))
                                                     (cins y (cins x (tpair (succ m) v)))
                                                     (cinsskip x y
                                                       (ncons (pr1 v) (tpair m (pr2 v)))
                                                       hxy
                                                       hyx)
                                                     (pathscomp0 nlist
                                                       (ncons y
                                                         (cins x
                                                           (ncons (pr1 v) (tpair m (pr2 v)))))
                                                       (ncons y
                                                         (ncons x
                                                           (ncons (pr1 v) (tpair m (pr2 v)))))
                                                       (cins y (cins x (tpair (succ m) v)))
                                                       (maponpaths nlist nlist
                                                         (lam z (ncons y z))
                                                         (cins x
                                                           (ncons (pr1 v) (tpair m (pr2 v))))
                                                         (ncons x
                                                           (ncons (pr1 v) (tpair m (pr2 v))))
                                                         (cinshead x (pr1 v) (tpair m (pr2 v))
                                                           hxh
                                                           hhx))
                                                       (pathscomp0 nlist
                                                         (ncons y
                                                           (ncons x
                                                             (ncons (pr1 v) (tpair m (pr2 v)))))
                                                         (cins y
                                                           (ncons x
                                                             (ncons (pr1 v) (tpair m (pr2 v)))))
                                                         (cins y (cins x (tpair (succ m) v)))
                                                         (pathsinv0 nlist
                                                           (cins y
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v)))))
                                                           (ncons y
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v)))))
                                                           (cinshead y x
                                                             (ncons (pr1 v) (tpair m (pr2 v)))
                                                             (pathscomp0 bool (nateqb y x)
                                                               (nateqb x y)
                                                               false
                                                               (nateqbsymm y x)
                                                               hxy)
                                                             (nattrichot x y hxy hyx)))
                                                         (pathsinv0 nlist
                                                           (cins y (cins x (tpair (succ m) v)))
                                                           (cins y
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v)))))
                                                           (maponpaths nlist nlist
                                                             (lam z (cins y z))
                                                             (cins x (tpair (succ m) v))
                                                             (ncons x
                                                               (ncons (pr1 v) (tpair m (pr2 v))))
                                                             (cinshead x (pr1 v)
                                                               (tpair m (pr2 v))
                                                               hxh
                                                               hhx))))))))
                                               (natgtb y x))
                                              (idpath (natgtb y x))))
                                          (nateqb x y))
                                         (idpath (nateqb x y))))
                                     (lam hhx
                                       (pathscomp0 nlist (cins x (cins y (tpair (succ m) v)))
                                         (cins x (ncons y (ncons (pr1 v) (tpair m (pr2 v)))))
                                         (cins y (cins x (tpair (succ m) v)))
                                         (maponpaths nlist nlist (lam z (cins x z))
                                           (cins y (tpair (succ m) v))
                                           (ncons y (ncons (pr1 v) (tpair m (pr2 v))))
                                           (cinshead y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                         (pathscomp0 nlist
                                           (cins x (ncons y (ncons (pr1 v) (tpair m (pr2 v)))))
                                           (ncons y (cins x (ncons (pr1 v) (tpair m (pr2 v)))))
                                           (cins y (cins x (tpair (succ m) v)))
                                           (cinsskip x y (ncons (pr1 v) (tpair m (pr2 v)))
                                             (natgthtoneq x y
                                               (natlehgthtrans (pr1 v) x y hhx hhy))
                                             (natgthtoneg x y
                                               (natlehgthtrans (pr1 v) x y hhx hhy)))
                                           (pathscomp0 nlist
                                             (ncons y (cins x (ncons (pr1 v) (tpair m (pr2 v)))))
                                             (ncons y (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                             (cins y (cins x (tpair (succ m) v)))
                                             (maponpaths nlist nlist (lam z (ncons y z))
                                               (cins x (ncons (pr1 v) (tpair m (pr2 v))))
                                               (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                               (cinsskip x (pr1 v) (tpair m (pr2 v)) hxh hhx))
                                             (pathscomp0 nlist
                                               (ncons y
                                                 (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                               (cins y
                                                 (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                               (cins y (cins x (tpair (succ m) v)))
                                               (pathsinv0 nlist
                                                 (cins y
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                                 (ncons y
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                                 (cinshead y (pr1 v) (cins x (tpair m (pr2 v)))
                                                   hyh
                                                   hhy))
                                               (pathsinv0 nlist
                                                 (cins y (cins x (tpair (succ m) v)))
                                                 (cins y
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                                 (maponpaths nlist nlist (lam z (cins y z))
                                                   (cins x (tpair (succ m) v))
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                                   (cinsskip x (pr1 v) (tpair m (pr2 v)) hxh hhx))))))))
                                     (natgtb (pr1 v) x))
                                    (idpath (natgtb (pr1 v) x))))
                                (lam hhy
                                  ((bool_rect
                                     (lam c
                                       (pi hhx (paths bool (natgtb (pr1 v) x) c)
                                         (paths nlist (cins x (cins y (tpair (succ m) v)))
                                           (cins y (cins x (tpair (succ m) v))))))
                                     (lam hhx
                                       (pathscomp0 nlist (cins x (cins y (tpair (succ m) v)))
                                         (cins x (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                         (cins y (cins x (tpair (succ m) v)))
                                         (maponpaths nlist nlist (lam z (cins x z))
                                           (cins y (tpair (succ m) v))
                                           (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                           (cinsskip y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                         (pathscomp0 nlist
                                           (cins x (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                           (ncons x (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                           (cins y (cins x (tpair (succ m) v)))
                                           (cinshead x (pr1 v) (cins y (tpair m (pr2 v))) hxh
                                             hhx)
                                           (pathscomp0 nlist
                                             (ncons x (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                             (ncons x (cins y (ncons (pr1 v) (tpair m (pr2 v)))))
                                             (cins y (cins x (tpair (succ m) v)))
                                             (pathsinv0 nlist
                                               (ncons x
                                                 (cins y (ncons (pr1 v) (tpair m (pr2 v)))))
                                               (ncons x
                                                 (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                               (maponpaths nlist nlist (lam z (ncons x z))
                                                 (cins y (ncons (pr1 v) (tpair m (pr2 v))))
                                                 (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                                 (cinsskip y (pr1 v) (tpair m (pr2 v)) hyh hhy)))
                                             (pathscomp0 nlist
                                               (ncons x
                                                 (cins y (ncons (pr1 v) (tpair m (pr2 v)))))
                                               (cins y
                                                 (ncons x (ncons (pr1 v) (tpair m (pr2 v)))))
                                               (cins y (cins x (tpair (succ m) v)))
                                               (pathsinv0 nlist
                                                 (cins y
                                                   (ncons x (ncons (pr1 v) (tpair m (pr2 v)))))
                                                 (ncons x
                                                   (cins y (ncons (pr1 v) (tpair m (pr2 v)))))
                                                 (cinsskip y x (ncons (pr1 v) (tpair m (pr2 v)))
                                                   (natgthtoneq y x
                                                     (natlehgthtrans (pr1 v) y x hhy hhx))
                                                   (natgthtoneg y x
                                                     (natlehgthtrans (pr1 v) y x hhy hhx))))
                                               (pathsinv0 nlist
                                                 (cins y (cins x (tpair (succ m) v)))
                                                 (cins y
                                                   (ncons x (ncons (pr1 v) (tpair m (pr2 v)))))
                                                 (maponpaths nlist nlist (lam z (cins y z))
                                                   (cins x (tpair (succ m) v))
                                                   (ncons x (ncons (pr1 v) (tpair m (pr2 v))))
                                                   (cinshead x (pr1 v) (tpair m (pr2 v)) hxh hhx))))))))
                                     (lam hhx
                                       (pathscomp0 nlist (cins x (cins y (tpair (succ m) v)))
                                         (cins x (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                         (cins y (cins x (tpair (succ m) v)))
                                         (maponpaths nlist nlist (lam z (cins x z))
                                           (cins y (tpair (succ m) v))
                                           (ncons (pr1 v) (cins y (tpair m (pr2 v))))
                                           (cinsskip y (pr1 v) (tpair m (pr2 v)) hyh hhy))
                                         (pathscomp0 nlist
                                           (cins x (ncons (pr1 v) (cins y (tpair m (pr2 v)))))
                                           (ncons (pr1 v) (cins x (cins y (tpair m (pr2 v)))))
                                           (cins y (cins x (tpair (succ m) v)))
                                           (cinsskip x (pr1 v) (cins y (tpair m (pr2 v))) hxh
                                             hhx)
                                           (pathscomp0 nlist
                                             (ncons (pr1 v) (cins x (cins y (tpair m (pr2 v)))))
                                             (ncons (pr1 v) (cins y (cins x (tpair m (pr2 v)))))
                                             (cins y (cins x (tpair (succ m) v)))
                                             (maponpaths nlist nlist (lam z (ncons (pr1 v) z))
                                               (cins x (cins y (tpair m (pr2 v))))
                                               (cins y (cins x (tpair m (pr2 v))))
                                               (IH (pr2 v)))
                                             (pathscomp0 nlist
                                               (ncons (pr1 v)
                                                 (cins y (cins x (tpair m (pr2 v)))))
                                               (cins y
                                                 (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                               (cins y (cins x (tpair (succ m) v)))
                                               (pathsinv0 nlist
                                                 (cins y
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                                 (ncons (pr1 v)
                                                   (cins y (cins x (tpair m (pr2 v)))))
                                                 (cinsskip y (pr1 v) (cins x (tpair m (pr2 v)))
                                                   hyh
                                                   hhy))
                                               (pathsinv0 nlist
                                                 (cins y (cins x (tpair (succ m) v)))
                                                 (cins y
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v)))))
                                                 (maponpaths nlist nlist (lam z (cins y z))
                                                   (cins x (tpair (succ m) v))
                                                   (ncons (pr1 v) (cins x (tpair m (pr2 v))))
                                                   (cinsskip x (pr1 v) (tpair m (pr2 v)) hxh hhx))))))))
                                     (natgtb (pr1 v) x))
                                    (idpath (natgtb (pr1 v) x))))
                                (natgtb (pr1 v) y))
                               (idpath (natgtb (pr1 v) y))))
                           (nateqb x (pr1 v)))
                          (idpath (nateqb x (pr1 v)))))
                      (nateqb y (pr1 v)))
                     (idpath (nateqb y (pr1 v)))))))
             (pr1 l))
            (pr2 l))))))
 ; the canonical set computes: sorted, deduped, order-free
 ((vof 0 '(ncons 2 (ncons 5 (ncons 9 nnil)))) = (vof 0 '(cins 5 (ncons 2 (ncons 9 nnil)))))
 ((vof 0 '(cins 5 (cins 9 nnil))) = (vof 0 '(cins 9 (cins 5 nnil))))
 ((vof 0 '(cins 5 (cins 5 (ncons 2 nnil)))) = (vof 0 '(cins 5 (ncons 2 nnil)))))

; --- the bubble model: sseg cuts the hull, rend lays a rival inside it,
; --- scell packs hull base + the cins-canonical rival set. hash rides as
; --- a pi-bound function -- lcode instantiates it in the demos ---
(assert
 (defn sseg
   (pi s nlist (pi i nat (pi n nat nlist)))
   (lam s (lam i (lam n (ltake n (ldrop i s))))))
 (defn ulo
   (pi p hunk (pi q hunk nat))
   (lam p (lam q (natmin (hi p) (hi q)))))
 (defn uhi
   (pi p hunk (pi q hunk nat))
   (lam p (lam q (natmax (add (hi p) (nlen (ho p))) (add (hi q) (nlen (ho q)))))))
 (defn rend
   (pi s nlist (pi lo nat (pi hh nat (pi r hunk nlist))))
   (lam s
      (lam lo
        (lam hh
          (lam r (splice (sseg s lo (natminus hh lo)) (natminus (hi r) lo) (nlen (ho r)) (hn r)))))))
 (defn cell
   UU
   (total2 x nlist nlist))
 (defn scell
   (pi hash (pi l nlist nat) (pi s nlist (pi p hunk (pi q hunk cell))))
   (lam hash
      (lam s
        (lam p
          (lam q
            (tpair (sseg s (ulo p q) (natminus (uhi p q) (ulo p q)))
              (cins (hash (rend s (ulo p q) (uhi p q) p))
                (cins (hash (rend s (ulo p q) (uhi p q) q)) nnil))))))))
 (defq scell-lo
   (pi hash (pi l nlist nat)
      (pi s nlist
        (pi p hunk
          (pi q hunk
            (paths cell
              (tpair (sseg s (ulo p q) (natminus (uhi p q) (ulo p q)))
                (cins (hash (rend s (ulo p q) (uhi p q) p))
                  (cins (hash (rend s (ulo p q) (uhi p q) q)) nnil)))
              (tpair (sseg s (ulo q p) (natminus (uhi p q) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi p q) p))
                  (cins (hash (rend s (ulo q p) (uhi p q) q)) nnil))))))))
   (lam hash
      (lam s
        (lam p
          (lam q
            (maponpaths nat cell
              (lam z
                (tpair (sseg s z (natminus (uhi p q) z))
                  (cins (hash (rend s z (uhi p q) p)) (cins (hash (rend s z (uhi p q) q)) nnil))))
              (ulo p q)
              (ulo q p)
              (natmincomm (hi p) (hi q))))))))
 (defq scell-hi
   (pi hash (pi l nlist nat)
      (pi s nlist
        (pi p hunk
          (pi q hunk
            (paths cell
              (tpair (sseg s (ulo q p) (natminus (uhi p q) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi p q) p))
                  (cins (hash (rend s (ulo q p) (uhi p q) q)) nnil)))
              (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi q p) p))
                  (cins (hash (rend s (ulo q p) (uhi q p) q)) nnil))))))))
   (lam hash
      (lam s
        (lam p
          (lam q
            (maponpaths nat cell
              (lam z
                (tpair (sseg s (ulo q p) (natminus z (ulo q p)))
                  (cins (hash (rend s (ulo q p) z p)) (cins (hash (rend s (ulo q p) z q)) nnil))))
              (uhi p q)
              (uhi q p)
              (natmaxcomm (add (hi p) (nlen (ho p))) (add (hi q) (nlen (ho q))))))))))
 (defq scell-riv
   (pi hash (pi l nlist nat)
      (pi s nlist
        (pi p hunk
          (pi q hunk
            (paths cell
              (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi q p) p))
                  (cins (hash (rend s (ulo q p) (uhi q p) q)) nnil)))
              (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi q p) q))
                  (cins (hash (rend s (ulo q p) (uhi q p) p)) nnil))))))))
   (lam hash
      (lam s
        (lam p
          (lam q
            (maponpaths nlist cell
              (lam z (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p))) z))
              (cins (hash (rend s (ulo q p) (uhi q p) p))
                (cins (hash (rend s (ulo q p) (uhi q p) q)) nnil))
              (cins (hash (rend s (ulo q p) (uhi q p) q))
                (cins (hash (rend s (ulo q p) (uhi q p) p)) nnil))
              (cinscomm (hash (rend s (ulo q p) (uhi q p) p))
                (hash (rend s (ulo q p) (uhi q p) q))
                nnil)))))))
 (defq scell-comm
   (pi hash (pi l nlist nat)
      (pi s nlist (pi p hunk (pi q hunk (paths cell (scell hash s p q) (scell hash s q p))))))
   (lam hash
      (lam s
        (lam p
          (lam q
            (pathscomp0 cell
              (tpair (sseg s (ulo p q) (natminus (uhi p q) (ulo p q)))
                (cins (hash (rend s (ulo p q) (uhi p q) p))
                  (cins (hash (rend s (ulo p q) (uhi p q) q)) nnil)))
              (tpair (sseg s (ulo q p) (natminus (uhi p q) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi p q) p))
                  (cins (hash (rend s (ulo q p) (uhi p q) q)) nnil)))
              (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p)))
                (cins (hash (rend s (ulo q p) (uhi q p) q))
                  (cins (hash (rend s (ulo q p) (uhi q p) p)) nnil)))
              (scell-lo hash s p q)
              (pathscomp0 cell
                (tpair (sseg s (ulo q p) (natminus (uhi p q) (ulo q p)))
                  (cins (hash (rend s (ulo q p) (uhi p q) p))
                    (cins (hash (rend s (ulo q p) (uhi p q) q)) nnil)))
                (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p)))
                  (cins (hash (rend s (ulo q p) (uhi q p) p))
                    (cins (hash (rend s (ulo q p) (uhi q p) q)) nnil)))
                (tpair (sseg s (ulo q p) (natminus (uhi q p) (ulo q p)))
                  (cins (hash (rend s (ulo q p) (uhi q p) q))
                    (cins (hash (rend s (ulo q p) (uhi q p) p)) nnil)))
                (scell-hi hash s p q)
                (scell-riv hash s p q))))))))
 ; the bubble forgets arrival order, computed at lcode hash (tiny values:
 ; a unary lcode tower grows QUADRATICALLY in the rendered content)
 ((vof 0 '(scell lcode (ncons 1 (ncons 2 nnil)) (mkh 0 (ncons 1 nnil) (ncons 2 nnil)) (mkh 1 (ncons 2 nnil) nnil)))
  = (vof 0 '(scell lcode (ncons 1 (ncons 2 nnil)) (mkh 1 (ncons 2 nnil) nnil) (mkh 0 (ncons 1 nnil) (ncons 2 nnil))))))

; --- the WIDENING law: padding commutes with rendering (a rival over a
; --- narrow hull, padded up when the bubble widens, IS the rival over the
; --- wide hull) -- pure take/drop/append fusion. the BUBBLE law: the
; --- conflicted hull contracts to ONE seat, so the merged length reads
; --- off the hull alone -- every downstream seat determinate before
; --- anyone resolves ---
(assert
 (defq splice-pad
   (pi pl nlist
      (pi pr nlist
        (pi b nlist
          (pi n nlist
            (pi ii nat
              (pi c nat
                (pi h (paths bool (natgtb (add ii c) (nlen b)) false)
                  (paths nlist (lapp pl (lapp (splice b ii c n) pr))
                    (splice (lapp pl (lapp b pr)) (add (nlen pl) ii) c n)))))))))
   (lam pl
      (lam pr
        (lam b
          (lam n
            (lam ii
              (lam c
                (lam h
                  (pathscomp0 nlist (lapp pl (lapp (splice b ii c n) pr))
                    (lapp pl (lapp (ltake ii b) (lapp (lapp n (ldrop (add ii c) b)) pr)))
                    (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                      (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                    (maponpaths nlist nlist (lam z (lapp pl z)) (lapp (splice b ii c n) pr)
                      (lapp (ltake ii b) (lapp (lapp n (ldrop (add ii c) b)) pr))
                      (lappassoc (ltake ii b) (lapp n (ldrop (add ii c) b)) pr))
                    (pathscomp0 nlist
                      (lapp pl (lapp (ltake ii b) (lapp (lapp n (ldrop (add ii c) b)) pr)))
                      (lapp pl (lapp (ltake ii b) (lapp n (lapp (ldrop (add ii c) b) pr))))
                      (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                        (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                      (maponpaths nlist nlist (lam z (lapp pl (lapp (ltake ii b) z)))
                        (lapp (lapp n (ldrop (add ii c) b)) pr)
                        (lapp n (lapp (ldrop (add ii c) b) pr))
                        (lappassoc n (ldrop (add ii c) b) pr))
                      (pathscomp0 nlist
                        (lapp pl (lapp (ltake ii b) (lapp n (lapp (ldrop (add ii c) b) pr))))
                        (lapp (lapp pl (ltake ii b)) (lapp n (lapp (ldrop (add ii c) b) pr)))
                        (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                          (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                        (pathsinv0 nlist
                          (lapp (lapp pl (ltake ii b)) (lapp n (lapp (ldrop (add ii c) b) pr)))
                          (lapp pl (lapp (ltake ii b) (lapp n (lapp (ldrop (add ii c) b) pr))))
                          (lappassoc pl (ltake ii b) (lapp n (lapp (ldrop (add ii c) b) pr))))
                        (pathscomp0 nlist
                          (lapp (lapp pl (ltake ii b)) (lapp n (lapp (ldrop (add ii c) b) pr)))
                          (lapp (lapp pl (ltake ii b)) (lapp n (ldrop (add ii c) (lapp b pr))))
                          (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                            (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                          (pathsinv0 nlist
                            (lapp (lapp pl (ltake ii b)) (lapp n (ldrop (add ii c) (lapp b pr))))
                            (lapp (lapp pl (ltake ii b)) (lapp n (lapp (ldrop (add ii c) b) pr)))
                            (maponpaths nlist nlist
                              (lam z (lapp (lapp pl (ltake ii b)) (lapp n z)))
                              (ldrop (add ii c) (lapp b pr))
                              (lapp (ldrop (add ii c) b) pr)
                              (ldropprefix (add ii c) b pr h)))
                          (pathscomp0 nlist
                            (lapp (lapp pl (ltake ii b)) (lapp n (ldrop (add ii c) (lapp b pr))))
                            (lapp (lapp pl (ltake ii b))
                              (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                            (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                              (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                            (pathsinv0 nlist
                              (lapp (lapp pl (ltake ii b))
                                (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                              (lapp (lapp pl (ltake ii b))
                                (lapp n (ldrop (add ii c) (lapp b pr))))
                              (pathscomp0 nlist
                                (lapp (lapp pl (ltake ii b))
                                  (lapp n
                                    (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                                (lapp (lapp pl (ltake ii b))
                                  (lapp n
                                    (ldrop (add (nlen pl) (add ii c)) (lapp pl (lapp b pr)))))
                                (lapp (lapp pl (ltake ii b))
                                  (lapp n (ldrop (add ii c) (lapp b pr))))
                                (maponpaths nat nlist
                                  (lam z
                                    (lapp (lapp pl (ltake ii b))
                                      (lapp n (ldrop z (lapp pl (lapp b pr))))))
                                  (add (add (nlen pl) ii) c)
                                  (add (nlen pl) (add ii c))
                                  (natplusassoc (nlen pl) ii c))
                                (maponpaths nlist nlist
                                  (lam z (lapp (lapp pl (ltake ii b)) (lapp n z)))
                                  (ldrop (add (nlen pl) (add ii c)) (lapp pl (lapp b pr)))
                                  (ldrop (add ii c) (lapp b pr))
                                  (ldropplusapp pl (add ii c) (lapp b pr)))))
                            (pathscomp0 nlist
                              (lapp (lapp pl (ltake ii b))
                                (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                              (lapp (lapp pl (ltake ii (lapp b pr)))
                                (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                              (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                                (lapp n (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                              (pathsinv0 nlist
                                (lapp (lapp pl (ltake ii (lapp b pr)))
                                  (lapp n
                                    (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                                (lapp (lapp pl (ltake ii b))
                                  (lapp n
                                    (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                                (maponpaths nlist nlist
                                  (lam z
                                    (lapp (lapp pl z)
                                      (lapp n
                                        (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr))))))
                                  (ltake ii (lapp b pr))
                                  (ltake ii b)
                                  (ltakeprefix ii b pr
                                    (istransnatleh ii (add ii c) (nlen b) (natlehnplusnm ii c) h))))
                              (pathsinv0 nlist
                                (lapp (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                                  (lapp n
                                    (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                                (lapp (lapp pl (ltake ii (lapp b pr)))
                                  (lapp n
                                    (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr)))))
                                (maponpaths nlist nlist
                                  (lam z
                                    (lapp z
                                      (lapp n
                                        (ldrop (add (add (nlen pl) ii) c) (lapp pl (lapp b pr))))))
                                  (ltake (add (nlen pl) ii) (lapp pl (lapp b pr)))
                                  (lapp pl (ltake ii (lapp b pr)))
                                  (ltakeplusapp pl ii (lapp b pr))))))))))))))))))
 (defq bubble-len
   (pi s nlist
      (pi p hunk
        (pi q hunk
          (pi x nat
            (pi h (paths bool (natgtb (uhi p q) (nlen s)) false)
              (paths nat
                (nlen (splice s (ulo p q) (natminus (uhi p q) (ulo p q)) (ncons x nnil)))
                (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))))))))
   (lam s
      (lam p
        (lam q
          (lam x
            (lam h
              (pathscomp0 nat
                (nlen (splice s (ulo p q) (natminus (uhi p q) (ulo p q)) (ncons x nnil)))
                (add (nlen (ltake (ulo p q) s))
                  (nlen
                    (lapp (ncons x nnil)
                      (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s))))
                (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))
                (lapplen (ltake (ulo p q) s)
                  (lapp (ncons x nnil) (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s)))
                (pathscomp0 nat
                  (add (nlen (ltake (ulo p q) s))
                    (nlen
                      (lapp (ncons x nnil)
                        (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s))))
                  (add (ulo p q)
                    (nlen
                      (lapp (ncons x nnil)
                        (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s))))
                  (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))
                  (maponpaths nat nat
                    (lam z
                      (add z
                        (nlen
                          (lapp (ncons x nnil)
                            (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s)))))
                    (nlen (ltake (ulo p q) s))
                    (ulo p q)
                    (ltakelen (ulo p q) s
                      (istransnatleh (ulo p q) (uhi p q) (nlen s)
                        (istransnatleh (ulo p q) (add (hi p) (nlen (ho p))) (uhi p q)
                          (istransnatleh (ulo p q) (hi p) (add (hi p) (nlen (ho p)))
                            (natminlel (hi p) (hi q))
                            (natlehnplusnm (hi p) (nlen (ho p))))
                          (natmaxgel (add (hi p) (nlen (ho p))) (add (hi q) (nlen (ho q)))))
                        h)))
                  (pathscomp0 nat
                    (add (ulo p q)
                      (nlen
                        (lapp (ncons x nnil)
                          (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s))))
                    (add (ulo p q)
                      (succ (nlen (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s))))
                    (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))
                    (maponpaths nat nat (lam z (add (ulo p q) z))
                      (nlen
                        (lapp (ncons x nnil)
                          (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s)))
                      (succ (nlen (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s)))
                      (lapplen (ncons x nnil)
                        (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s)))
                    (pathscomp0 nat
                      (add (ulo p q)
                        (succ (nlen (ldrop (add (ulo p q) (natminus (uhi p q) (ulo p q))) s))))
                      (add (ulo p q) (succ (nlen (ldrop (uhi p q) s))))
                      (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))
                      (maponpaths nat nat (lam z (add (ulo p q) (succ (nlen (ldrop z s)))))
                        (add (ulo p q) (natminus (uhi p q) (ulo p q)))
                        (uhi p q)
                        (natlehplusminus (ulo p q) (uhi p q)
                          (istransnatleh (ulo p q) (add (hi p) (nlen (ho p))) (uhi p q)
                            (istransnatleh (ulo p q) (hi p) (add (hi p) (nlen (ho p)))
                              (natminlel (hi p) (hi q))
                              (natlehnplusnm (hi p) (nlen (ho p))))
                            (natmaxgel (add (hi p) (nlen (ho p))) (add (hi q) (nlen (ho q)))))))
                      (pathscomp0 nat (add (ulo p q) (succ (nlen (ldrop (uhi p q) s))))
                        (add (ulo p q) (succ (natminus (nlen s) (uhi p q))))
                        (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))
                        (maponpaths nat nat (lam z (add (ulo p q) (succ z)))
                          (nlen (ldrop (uhi p q) s))
                          (natminus (nlen s) (uhi p q))
                          (ldroplen (uhi p q) s))
                        (pathscomp0 nat (add (ulo p q) (succ (natminus (nlen s) (uhi p q))))
                          (succ (add (ulo p q) (natminus (nlen s) (uhi p q))))
                          (succ (natminus (nlen s) (natminus (uhi p q) (ulo p q))))
                          (natplusnsm (ulo p q) (natminus (nlen s) (uhi p q)))
                          (maponpaths nat nat (lam z (succ z))
                            (add (ulo p q) (natminus (nlen s) (uhi p q)))
                            (natminus (nlen s) (natminus (uhi p q) (ulo p q)))
                            (natminusswap (ulo p q) (uhi p q) (nlen s)
                              (istransnatleh (ulo p q) (add (hi p) (nlen (ho p))) (uhi p q)
                                (istransnatleh (ulo p q) (hi p) (add (hi p) (nlen (ho p)))
                                  (natminlel (hi p) (hi q))
                                  (natlehnplusnm (hi p) (nlen (ho p))))
                                (natmaxgel (add (hi p) (nlen (ho p))) (add (hi q) (nlen (ho q)))))
                              h))))))))))))))
 ; the bubble contracts the hull to one seat: length off the hull alone
 (id? (vof 0 '2) (vof 0 '(nlen (splice (ncons 1 (ncons 2 (ncons 3 (ncons 4 nnil)))) 0 3 (ncons 9 nnil)))))
 ; and padding commutes with rendering
 ((vof 0 '(lapp (ncons 7 nnil) (lapp (splice (ncons 1 (ncons 2 (ncons 3 nnil))) 1 1 (ncons 5 nnil)) (ncons 8 nnil))))
  = (vof 0 '(splice (lapp (ncons 7 nnil) (lapp (ncons 1 (ncons 2 (ncons 3 nnil))) (ncons 8 nnil))) 2 1 (ncons 5 nnil)))))


; ============================================================================
; the METAVARIABLE slice: matching is sound, the re-aim swaps home --
; patch.v's eighth form, its two genuinely new halves. a PATTERN is a
; depth-indexed family (tm d): atoms and metavariables at the leaves, cells
; one rung down, so nat_rect on the depth IS structural induction. a GROUND
; term rides at CODE grain -- one nat under the cantor coding (the etree
; currency: tag 0 atom, 1 metavariable, 2 cell), so a ground is an ELEMENT:
; it can sit in a state, be hashed, be laid. the substitution is a duo-list
; threaded through a coprod option; teq collapses to nateqb at code grain,
; so patch.v's teq_eq IS nateqbtopaths, already home. matching binds a
; fresh metavariable, checks a bound one (a non-linear pattern binds
; consistently or refuses), and MATCHING IS SOUND: a pattern instantiated
; with its own bindings SPELLS the ground code (what valid/apon lean on).
; the commute half: the schematic swallow re-aims a deep patch from the
; variable's olds seat to its news seat by prefix surgery -- involutive at
; address grain, two ldroplenapp reads.
; ============================================================================
; --- the pattern family and its coding: leaves live at EVERY depth (the
; --- left summand), cells one rung down, so any two patterns share a depth;
; --- tcode lays a pattern onto the element grain the states speak ---
(assert
 (defn tmleaf
   UU
   (coprod nat nat))
 (defn tm
   (pi d nat UU)
   (lam d (nat_rect (lam q UU) tmleaf (lam k (lam r (coprod tmleaf (total2 h r r)))) d)))
 (defn latom
   (pi n nat tmleaf)
   (lam n (ii1 n)))
 (defn lmv
   (pi x nat tmleaf)
   (lam x (ii2 x)))
 (defn matom
   (pi d nat (pi n nat (tm (succ d))))
   (lam d (lam n (ii1 (ii1 n)))))
 (defn mmv
   (pi d nat (pi x nat (tm (succ d))))
   (lam d (lam x (ii1 (ii2 x)))))
 (defn mcell
   (pi d nat (pi a (tm d) (pi b (tm d) (tm (succ d)))))
   (lam d (lam a (lam b (ii2 (tpair a b))))))
 (defn tcode
   (pi d nat (pi p (tm d) nat))
   (lam d
      (nat_rect (lam q (pi p (tm q) nat))
        (lam p (coprod_rect (lam c nat) (lam n (npair 0 n)) (lam x (npair 1 x)) p))
        (lam k
          (lam IH
            (lam p
              (coprod_rect (lam c nat)
                (lam w (coprod_rect (lam c nat) (lam n (npair 0 n)) (lam x (npair 1 x)) w))
                (lam w (npair 2 (npair (IH (pr1 w)) (IH (pr2 w)))))
                p))))
        d)))
 ; the coding computes, and a leaf codes the same at every depth
 (id? (vof 0 '2) (vof 0 '(tcode 0 (latom 1))))
 (id? (vof 0 '1) (vof 0 '(tcode 0 (lmv 0))))
 (id? (vof 0 '(tcode 0 (latom 1))) (vof 0 '(tcode 1 (matom 0 1))))
 (id? (vof 0 '(tcode 0 (lmv 3))) (vof 0 '(tcode 1 (mmv 0 3))))
 (id? (vof 0 '52) (vof 0 '(tcode 1 (mcell 0 (latom 1) (lmv 0))))))

; --- the substitution: a duo-list (key, code), first hit wins -- the walk
; --- is occ's shape at duo grain; sqside is the option's no-confusion
; --- family (a miss is never a hit) ---
(assert
 (defn svec
   (pi n nat UU)
   (lam n (nat_rect (lam q UU) unit (lam k (lam r (total2 h duo r))) n)))
 (defn slist
   UU
   (total2 n nat (svec n)))
 (defn snil
   slist
   (tpair 0 tt))
 (defn scons
   (pi e duo (pi s slist slist))
   (lam e (lam s (tpair (succ (pr1 s)) (tpair e (pr2 s))))))
 (defn sasq
   (pi v nat (pi s slist (coprod nat unit)))
   (lam v
      (lam s
        ((nat_rect (lam q (pi w (svec q) (coprod nat unit))) (lam w (ii2 tt))
           (lam k
             (lam IH
               (lam w
                 (bool_rect (lam c (coprod nat unit)) (ii1 (pr2 (pr1 w))) (IH (pr2 w))
                   (nateqb (pr1 (pr1 w)) v)))))
           (pr1 s))
          (pr2 s)))))
 (defn sqside
   (pi c (coprod nat unit) UU)
   (lam c (coprod_rect (lam x UU) (lam x empty) (lam x unit) c)))
 ; the lookup computes: a hit, a miss, and the shadowing head
 ((vof 0 '(ii1 7)) = (vof 0 '(sasq 1 (scons (tpair 1 7) snil))))
 ((vof 0 '(ii2 tt)) = (vof 0 '(sasq 3 (scons (tpair 1 7) snil))))
 ((vof 0 '(ii1 5)) = (vof 0 '(sasq 1 (scons (tpair 1 5) (scons (tpair 1 7) snil))))))

; --- matching and instantiation: the leaf lane is shared by every depth
; --- (an atom compares codes, a bound metavariable CHECKS, a fresh one
; --- BINDS), the cell lane is guarded by the ground's tag -- upa/upb read
; --- the ground's kids, and npairunpair will reassemble them; mvside is
; --- the result option's no-confusion family ---
(assert
 (defn mleaf
   (pi p tmleaf (pi g nat (pi s slist (coprod slist unit))))
   (lam p
      (lam g
        (lam s
          (coprod_rect (lam c (coprod slist unit))
            (lam n
              (bool_rect (lam c2 (coprod slist unit)) (ii1 s) (ii2 tt) (nateqb (npair 0 n) g)))
            (lam x
              (coprod_rect (lam c2 (coprod slist unit))
                (lam t (bool_rect (lam c3 (coprod slist unit)) (ii1 s) (ii2 tt) (nateqb t g)))
                (lam u (ii1 (scons (tpair x g) s)))
                (sasq x s)))
            p)))))
 (defn mvside
   (pi c (coprod slist unit) UU)
   (lam c (coprod_rect (lam x UU) (lam x empty) (lam x unit) c)))
 (defn mtch
   (pi d nat (pi p (tm d) (pi g nat (pi s slist (coprod slist unit)))))
   (lam d
      (nat_rect (lam q (pi p (tm q) (pi g nat (pi s slist (coprod slist unit)))))
        (lam p (lam g (lam s (mleaf p g s))))
        (lam k
          (lam IH
            (lam p
              (lam g
                (lam s
                  (coprod_rect (lam c (coprod slist unit)) (lam w (mleaf w g s))
                    (lam w
                      (bool_rect (lam c2 (coprod slist unit))
                        (coprod_rect (lam c3 (coprod slist unit))
                          (lam so (IH (pr2 w) (upb (upb g)) so))
                          (lam u (ii2 tt))
                          (IH (pr1 w) (upa (upb g)) s))
                        (ii2 tt)
                        (nateqb (upa g) 2)))
                    p))))))
        d)))
 (defn ileaf
   (pi p tmleaf (pi s slist nat))
   (lam p
      (lam s
        (coprod_rect (lam c nat) (lam n (npair 0 n))
          (lam x (coprod_rect (lam c2 nat) (lam t t) (lam u (npair 1 x)) (sasq x s)))
          p))))
 (defn inst
   (pi d nat (pi p (tm d) (pi s slist nat)))
   (lam d
      (nat_rect (lam q (pi p (tm q) (pi s slist nat))) (lam p (lam s (ileaf p s)))
        (lam k
          (lam IH
            (lam p
              (lam s
                (coprod_rect (lam c nat) (lam w (ileaf w s))
                  (lam w (npair 2 (npair (IH (pr1 w) s) (IH (pr2 w) s))))
                  p)))))
        d)))
 ; matching computes: a bind + an atom check, the non-linear pattern
 ; consistent then breached, and a wrong-shape ground refused
 ((vof 0 '(mtch 1 (mcell 0 (lmv 0) (latom 0)) (tcode 1 (mcell 0 (latom 1) (latom 0))) snil))
  = (vof 0 '(ii1 (scons (tpair 0 (tcode 0 (latom 1))) snil))))
 ((vof 0 '(mtch 1 (mcell 0 (lmv 0) (lmv 0)) (tcode 1 (mcell 0 (latom 1) (latom 1))) snil))
  = (vof 0 '(ii1 (scons (tpair 0 (tcode 0 (latom 1))) snil))))
 ((vof 0 '(mtch 1 (mcell 0 (lmv 0) (lmv 0)) (tcode 1 (mcell 0 (latom 1) (latom 0))) snil))
  = (vof 0 '(ii2 tt)))
 ((vof 0 '(mtch 1 (mcell 0 (lmv 0) (latom 0)) (tcode 0 (latom 1)) snil)) = (vof 0 '(ii2 tt)))
 ; instantiation computes: soundness by eye, and a free metavariable rides
 (id? (vof 0 '(inst 1 (mcell 0 (lmv 0) (lmv 0)) (scons (tpair 0 (tcode 0 (latom 1))) snil)))
      (vof 0 '(tcode 1 (mcell 0 (latom 1) (latom 1)))))
 (id? (vof 0 '(tcode 0 (lmv 4))) (vof 0 '(inst 0 (lmv 4) snil))))

; --- bindings only GROW: what a match step bound stays bound, unchanged
; --- (patch.v's mtch_mono). the leaf rung carries the verdict convoys --
; --- an atom or a checked metavariable hands the substitution through
; --- untouched, and a fresh bind's shadowing cons reads through nateqb's
; --- decision (agreeing keys land in sqside's empty lane); the cell rung
; --- threads the two IHs through the remembered intermediate ---
(assert
 (defq mleaf-mono
   (pi p tmleaf
      (pi g nat
        (pi s slist
          (pi sf slist
            (pi e (paths (coprod slist unit) (mleaf p g s) (ii1 sf))
              (pi v nat
                (pi t nat
                  (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                    (paths (coprod nat unit) (sasq v sf) (ii1 t))))))))))
   (lam p
      (lam g
        (lam s
          (lam sf
            (coprod_rect
              (lam c
                (pi e (paths (coprod slist unit) (mleaf c g s) (ii1 sf))
                  (pi v nat
                    (pi t nat
                      (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                        (paths (coprod nat unit) (sasq v sf) (ii1 t)))))))
              (lam n
                ((bool_rect
                   (lam cb
                     (pi hv (paths bool (nateqb (npair 0 n) g) cb)
                       (pi e
                         (paths (coprod slist unit)
                           (bool_rect (lam c3 (coprod slist unit)) (ii1 s) (ii2 tt) cb)
                           (ii1 sf))
                         (pi v nat
                           (pi t nat
                             (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                               (paths (coprod nat unit) (sasq v sf) (ii1 t))))))))
                   (lam hv
                     (lam e
                       (lam v
                         (lam t
                           (lam ha
                             (transportf slist
                               (lam y (paths (coprod nat unit) (sasq v y) (ii1 t)))
                               s
                               sf
                               (maponpaths (coprod slist unit) slist
                                 (lam c9 (coprod_rect (lam c8 slist) (lam y9 y9) (lam u9 s) c9))
                                 (ii1 s)
                                 (ii1 sf)
                                 e)
                               ha))))))
                   (lam hv
                     (lam e
                       (lam v
                         (lam t
                           (lam ha
                             (fromempty (paths (coprod nat unit) (sasq v sf) (ii1 t))
                               (transportf (coprod slist unit) mvside (ii2 tt) (ii1 sf) e tt)))))))
                   (nateqb (npair 0 n) g))
                  (idpath (nateqb (npair 0 n) g))))
              (lam x
                ((coprod_rect
                   (lam cc
                     (pi hc (paths (coprod nat unit) (sasq x s) cc)
                       (pi e
                         (paths (coprod slist unit)
                           (coprod_rect (lam c3 (coprod slist unit))
                             (lam ti
                               (bool_rect (lam c4 (coprod slist unit)) (ii1 s) (ii2 tt)
                                 (nateqb ti g)))
                             (lam ui (ii1 (scons (tpair x g) s)))
                             cc)
                           (ii1 sf))
                         (pi v nat
                           (pi t nat
                             (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                               (paths (coprod nat unit) (sasq v sf) (ii1 t))))))))
                   (lam t0
                     (lam hc
                       ((bool_rect
                          (lam cb
                            (pi hv (paths bool (nateqb t0 g) cb)
                              (pi e
                                (paths (coprod slist unit)
                                  (bool_rect (lam c3 (coprod slist unit)) (ii1 s) (ii2 tt) cb)
                                  (ii1 sf))
                                (pi v nat
                                  (pi t nat
                                    (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                                      (paths (coprod nat unit) (sasq v sf) (ii1 t))))))))
                          (lam hv
                            (lam e
                              (lam v
                                (lam t
                                  (lam ha
                                    (transportf slist
                                      (lam y (paths (coprod nat unit) (sasq v y) (ii1 t)))
                                      s
                                      sf
                                      (maponpaths (coprod slist unit) slist
                                        (lam c9
                                          (coprod_rect (lam c8 slist) (lam y9 y9) (lam u9 s) c9))
                                        (ii1 s)
                                        (ii1 sf)
                                        e)
                                      ha))))))
                          (lam hv
                            (lam e
                              (lam v
                                (lam t
                                  (lam ha
                                    (fromempty (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                      (transportf (coprod slist unit) mvside (ii2 tt) (ii1 sf) e
                                        tt)))))))
                          (nateqb t0 g))
                         (idpath (nateqb t0 g)))))
                   (lam u
                     (lam hc
                       (lam e
                         (lam v
                           (lam t
                             (lam ha
                               (transportf slist
                                 (lam y (paths (coprod nat unit) (sasq v y) (ii1 t)))
                                 (scons (tpair x g) s)
                                 sf
                                 (maponpaths (coprod slist unit) slist
                                   (lam c9
                                     (coprod_rect (lam c8 slist) (lam y9 y9)
                                       (lam u9 (scons (tpair x g) s))
                                       c9))
                                   (ii1 (scons (tpair x g) s))
                                   (ii1 sf)
                                   e)
                                 ((bool_rect
                                    (lam cb
                                      (pi hv (paths bool (nateqb x v) cb)
                                        (paths (coprod nat unit)
                                          (bool_rect (lam c4 (coprod nat unit)) (ii1 g)
                                            (sasq v s)
                                            cb)
                                          (ii1 t))))
                                    (lam hv
                                      (fromempty (paths (coprod nat unit) (ii1 g) (ii1 t))
                                        (transportf (coprod nat unit) sqside (ii2 u) (ii1 t)
                                          (pathscomp0 (coprod nat unit) (ii2 u) (sasq x s)
                                            (ii1 t)
                                            (pathsinv0 (coprod nat unit) (sasq x s) (ii2 u) hc)
                                            (transportf nat
                                              (lam y
                                                (paths (coprod nat unit) (sasq y s) (ii1 t)))
                                              v
                                              x
                                              (pathsinv0 nat x v (nateqbtopaths x v hv))
                                              ha))
                                          u)))
                                    (lam hv ha)
                                    (nateqb x v))
                                   (idpath (nateqb x v))))))))))
                   (sasq x s))
                  (idpath (sasq x s))))
              p))))))
 (defq mtch-mono
   (pi d nat
      (pi p (tm d)
        (pi g nat
          (pi s slist
            (pi sf slist
              (pi e (paths (coprod slist unit) (mtch d p g s) (ii1 sf))
                (pi v nat
                  (pi t nat
                    (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                      (paths (coprod nat unit) (sasq v sf) (ii1 t)))))))))))
   (lam d
      (nat_rect
        (lam q
          (pi p (tm q)
            (pi g nat
              (pi s slist
                (pi sf slist
                  (pi e (paths (coprod slist unit) (mtch q p g s) (ii1 sf))
                    (pi v nat
                      (pi t nat
                        (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                          (paths (coprod nat unit) (sasq v sf) (ii1 t)))))))))))
        (lam p
          (lam g
            (lam s (lam sf (lam e (lam v (lam t (lam ha (mleaf-mono p g s sf e v t ha)))))))))
        (lam k
          (lam IH
            (lam p
              (lam g
                (lam s
                  (lam sf
                    (coprod_rect
                      (lam c
                        (pi e (paths (coprod slist unit) (mtch (succ k) c g s) (ii1 sf))
                          (pi v nat
                            (pi t nat
                              (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                                (paths (coprod nat unit) (sasq v sf) (ii1 t)))))))
                      (lam w (lam e (lam v (lam t (lam ha (mleaf-mono w g s sf e v t ha))))))
                      (lam w
                        ((bool_rect
                           (lam cb
                             (pi hg (paths bool (nateqb (upa g) 2) cb)
                               (pi e
                                 (paths (coprod slist unit)
                                   (bool_rect (lam c2 (coprod slist unit))
                                     (coprod_rect (lam c3 (coprod slist unit))
                                       (lam so (mtch k (pr2 w) (upb (upb g)) so))
                                       (lam u (ii2 tt))
                                       (mtch k (pr1 w) (upa (upb g)) s))
                                     (ii2 tt)
                                     cb)
                                   (ii1 sf))
                                 (pi v nat
                                   (pi t nat
                                     (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                                       (paths (coprod nat unit) (sasq v sf) (ii1 t))))))))
                           (lam hg
                             ((coprod_rect
                                (lam cc
                                  (pi hc
                                    (paths (coprod slist unit) (mtch k (pr1 w) (upa (upb g)) s)
                                      cc)
                                    (pi e
                                      (paths (coprod slist unit)
                                        (coprod_rect (lam c3 (coprod slist unit))
                                          (lam so (mtch k (pr2 w) (upb (upb g)) so))
                                          (lam u (ii2 tt))
                                          cc)
                                        (ii1 sf))
                                      (pi v nat
                                        (pi t nat
                                          (pi ha (paths (coprod nat unit) (sasq v s) (ii1 t))
                                            (paths (coprod nat unit) (sasq v sf) (ii1 t))))))))
                                (lam so
                                  (lam hc
                                    (lam e
                                      (lam v
                                        (lam t
                                          (lam ha
                                            (IH (pr2 w) (upb (upb g)) so sf e v t
                                              (IH (pr1 w) (upa (upb g)) s so hc v t ha))))))))
                                (lam u
                                  (lam hc
                                    (lam e
                                      (lam v
                                        (lam t
                                          (lam ha
                                            (fromempty
                                              (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                              (transportf (coprod slist unit) mvside (ii2 tt)
                                                (ii1 sf)
                                                e
                                                tt))))))))
                                (mtch k (pr1 w) (upa (upb g)) s))
                               (idpath (mtch k (pr1 w) (upa (upb g)) s))))
                           (lam hg
                             (lam e
                               (lam v
                                 (lam t
                                   (lam ha
                                     (fromempty (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                       (transportf (coprod slist unit) mvside (ii2 tt) (ii1 sf)
                                         e
                                         tt)))))))
                           (nateqb (upa g) 2))
                          (idpath (nateqb (upa g) 2))))
                      p)))))))
        d))))

; --- MATCHING IS SOUND, under any extension of the bindings it produced
; --- (patch.v's mtch_sound_ext -- the strengthening that lets the cell
; --- case ride mtch-mono). the leaf rung: an atom's verdict IS the goal,
; --- a checked metavariable reads its binding through the extension, a
; --- fresh bind reads its own cons head (nateqbrefl fires the decision);
; --- the cell rung reassembles the ground from its kids -- the two IHs,
; --- npairunpair twice, and the tag verdict slid home. then the headline
; --- at the empty substitution ---
(assert
 (defq mleaf-sound
   (pi p tmleaf
      (pi g nat
        (pi s slist
          (pi sf slist
            (pi e (paths (coprod slist unit) (mleaf p g s) (ii1 sf))
              (pi s2 slist
                (pi X
                  (pi v nat
                    (pi t nat
                      (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                        (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                  (paths nat (ileaf p s2) g))))))))
   (lam p
      (lam g
        (lam s
          (lam sf
            (coprod_rect
              (lam c
                (pi e (paths (coprod slist unit) (mleaf c g s) (ii1 sf))
                  (pi s2 slist
                    (pi X
                      (pi v nat
                        (pi t nat
                          (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                            (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                      (paths nat (ileaf c s2) g)))))
              (lam n
                ((bool_rect
                   (lam cb
                     (pi hv (paths bool (nateqb (npair 0 n) g) cb)
                       (pi e
                         (paths (coprod slist unit)
                           (bool_rect (lam c3 (coprod slist unit)) (ii1 s) (ii2 tt) cb)
                           (ii1 sf))
                         (pi s2 slist
                           (pi X
                             (pi v nat
                               (pi t nat
                                 (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                   (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                             (paths nat (ileaf (ii1 n) s2) g))))))
                   (lam hv (lam e (lam s2 (lam X (nateqbtopaths (npair 0 n) g hv)))))
                   (lam hv
                     (lam e
                       (lam s2
                         (lam X
                           (fromempty (paths nat (ileaf (ii1 n) s2) g)
                             (transportf (coprod slist unit) mvside (ii2 tt) (ii1 sf) e tt))))))
                   (nateqb (npair 0 n) g))
                  (idpath (nateqb (npair 0 n) g))))
              (lam x
                ((coprod_rect
                   (lam cc
                     (pi hc (paths (coprod nat unit) (sasq x s) cc)
                       (pi e
                         (paths (coprod slist unit)
                           (coprod_rect (lam c3 (coprod slist unit))
                             (lam ti
                               (bool_rect (lam c4 (coprod slist unit)) (ii1 s) (ii2 tt)
                                 (nateqb ti g)))
                             (lam ui (ii1 (scons (tpair x g) s)))
                             cc)
                           (ii1 sf))
                         (pi s2 slist
                           (pi X
                             (pi v nat
                               (pi t nat
                                 (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                   (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                             (paths nat (ileaf (ii2 x) s2) g))))))
                   (lam t0
                     (lam hc
                       ((bool_rect
                          (lam cb
                            (pi hv (paths bool (nateqb t0 g) cb)
                              (pi e
                                (paths (coprod slist unit)
                                  (bool_rect (lam c3 (coprod slist unit)) (ii1 s) (ii2 tt) cb)
                                  (ii1 sf))
                                (pi s2 slist
                                  (pi X
                                    (pi v nat
                                      (pi t nat
                                        (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                          (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                                    (paths nat (ileaf (ii2 x) s2) g))))))
                          (lam hv
                            (lam e
                              (lam s2
                                (lam X
                                  (pathscomp0 nat (ileaf (ii2 x) s2) t0 g
                                    (maponpaths (coprod nat unit) nat
                                      (lam c9
                                        (coprod_rect (lam c8 nat) (lam ti ti)
                                          (lam ui (npair 1 x))
                                          c9))
                                      (sasq x s2)
                                      (ii1 t0)
                                      (X x t0
                                        (transportf slist
                                          (lam y (paths (coprod nat unit) (sasq x y) (ii1 t0)))
                                          s
                                          sf
                                          (maponpaths (coprod slist unit) slist
                                            (lam c9
                                              (coprod_rect (lam c8 slist) (lam y9 y9) (lam u9 s)
                                                c9))
                                            (ii1 s)
                                            (ii1 sf)
                                            e)
                                          hc)))
                                    (nateqbtopaths t0 g hv))))))
                          (lam hv
                            (lam e
                              (lam s2
                                (lam X
                                  (fromempty (paths nat (ileaf (ii2 x) s2) g)
                                    (transportf (coprod slist unit) mvside (ii2 tt) (ii1 sf) e
                                      tt))))))
                          (nateqb t0 g))
                         (idpath (nateqb t0 g)))))
                   (lam u
                     (lam hc
                       (lam e
                         (lam s2
                           (lam X
                             (maponpaths (coprod nat unit) nat
                               (lam c9
                                 (coprod_rect (lam c8 nat) (lam ti ti) (lam ui (npair 1 x)) c9))
                               (sasq x s2)
                               (ii1 g)
                               (X x g
                                 (transportf slist
                                   (lam y (paths (coprod nat unit) (sasq x y) (ii1 g)))
                                   (scons (tpair x g) s)
                                   sf
                                   (maponpaths (coprod slist unit) slist
                                     (lam c9
                                       (coprod_rect (lam c8 slist) (lam y9 y9)
                                         (lam u9 (scons (tpair x g) s))
                                         c9))
                                     (ii1 (scons (tpair x g) s))
                                     (ii1 sf)
                                     e)
                                   (maponpaths bool (coprod nat unit)
                                     (lam c9
                                       (bool_rect (lam c8 (coprod nat unit)) (ii1 g) (sasq x s)
                                         c9))
                                     (nateqb x x)
                                     true
                                     (nateqbrefl x))))))))))
                   (sasq x s))
                  (idpath (sasq x s))))
              p))))))
 (defq mtch-sound-ext
   (pi d nat
      (pi p (tm d)
        (pi g nat
          (pi s slist
            (pi sf slist
              (pi e (paths (coprod slist unit) (mtch d p g s) (ii1 sf))
                (pi s2 slist
                  (pi X
                    (pi v nat
                      (pi t nat
                        (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                          (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                    (paths nat (inst d p s2) g)))))))))
   (lam d
      (nat_rect
        (lam q
          (pi p (tm q)
            (pi g nat
              (pi s slist
                (pi sf slist
                  (pi e (paths (coprod slist unit) (mtch q p g s) (ii1 sf))
                    (pi s2 slist
                      (pi X
                        (pi v nat
                          (pi t nat
                            (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                              (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                        (paths nat (inst q p s2) g)))))))))
        (lam p (lam g (lam s (lam sf (lam e (lam s2 (lam X (mleaf-sound p g s sf e s2 X))))))))
        (lam k
          (lam IH
            (lam p
              (lam g
                (lam s
                  (lam sf
                    (coprod_rect
                      (lam c
                        (pi e (paths (coprod slist unit) (mtch (succ k) c g s) (ii1 sf))
                          (pi s2 slist
                            (pi X
                              (pi v nat
                                (pi t nat
                                  (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                    (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                              (paths nat (inst (succ k) c s2) g)))))
                      (lam w (lam e (lam s2 (lam X (mleaf-sound w g s sf e s2 X)))))
                      (lam w
                        ((bool_rect
                           (lam cb
                             (pi hg (paths bool (nateqb (upa g) 2) cb)
                               (pi e
                                 (paths (coprod slist unit)
                                   (bool_rect (lam c2 (coprod slist unit))
                                     (coprod_rect (lam c3 (coprod slist unit))
                                       (lam so (mtch k (pr2 w) (upb (upb g)) so))
                                       (lam u (ii2 tt))
                                       (mtch k (pr1 w) (upa (upb g)) s))
                                     (ii2 tt)
                                     cb)
                                   (ii1 sf))
                                 (pi s2 slist
                                   (pi X
                                     (pi v nat
                                       (pi t nat
                                         (pi h3 (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                           (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                                     (paths nat (inst (succ k) (ii2 w) s2) g))))))
                           (lam hg
                             ((coprod_rect
                                (lam cc
                                  (pi hc
                                    (paths (coprod slist unit) (mtch k (pr1 w) (upa (upb g)) s)
                                      cc)
                                    (pi e
                                      (paths (coprod slist unit)
                                        (coprod_rect (lam c3 (coprod slist unit))
                                          (lam so (mtch k (pr2 w) (upb (upb g)) so))
                                          (lam u (ii2 tt))
                                          cc)
                                        (ii1 sf))
                                      (pi s2 slist
                                        (pi X
                                          (pi v nat
                                            (pi t nat
                                              (pi h3
                                                (paths (coprod nat unit) (sasq v sf) (ii1 t))
                                                (paths (coprod nat unit) (sasq v s2) (ii1 t)))))
                                          (paths nat (inst (succ k) (ii2 w) s2) g))))))
                                (lam so
                                  (lam hc
                                    (lam e
                                      (lam s2
                                        (lam X
                                          (pathscomp0 nat
                                            (npair 2
                                              (npair (inst k (pr1 w) s2) (inst k (pr2 w) s2)))
                                            (npair 2 (npair (upa (upb g)) (inst k (pr2 w) s2)))
                                            g
                                            (maponpaths nat nat
                                              (lam y (npair 2 (npair y (inst k (pr2 w) s2))))
                                              (inst k (pr1 w) s2)
                                              (upa (upb g))
                                              (IH (pr1 w) (upa (upb g)) s so hc s2
                                                (lam v
                                                  (lam t
                                                    (lam h3
                                                      (X v t
                                                        (mtch-mono k (pr2 w) (upb (upb g)) so sf
                                                          e
                                                          v
                                                          t
                                                          h3)))))))
                                            (pathscomp0 nat
                                              (npair 2 (npair (upa (upb g)) (inst k (pr2 w) s2)))
                                              (npair 2 (npair (upa (upb g)) (upb (upb g))))
                                              g
                                              (maponpaths nat nat
                                                (lam y (npair 2 (npair (upa (upb g)) y)))
                                                (inst k (pr2 w) s2)
                                                (upb (upb g))
                                                (IH (pr2 w) (upb (upb g)) so sf e s2 X))
                                              (pathscomp0 nat
                                                (npair 2 (npair (upa (upb g)) (upb (upb g))))
                                                (npair 2 (upb g))
                                                g
                                                (maponpaths nat nat (lam y (npair 2 y))
                                                  (npair (upa (upb g)) (upb (upb g)))
                                                  (upb g)
                                                  (npairunpair (upb g)))
                                                (pathscomp0 nat (npair 2 (upb g))
                                                  (npair (upa g) (upb g))
                                                  g
                                                  (maponpaths nat nat (lam y (npair y (upb g)))
                                                    2
                                                    (upa g)
                                                    (pathsinv0 nat (upa g) 2
                                                      (nateqbtopaths (upa g) 2 hg)))
                                                  (npairunpair g))))))))))
                                (lam u
                                  (lam hc
                                    (lam e
                                      (lam s2
                                        (lam X
                                          (fromempty (paths nat (inst (succ k) (ii2 w) s2) g)
                                            (transportf (coprod slist unit) mvside (ii2 tt)
                                              (ii1 sf)
                                              e
                                              tt)))))))
                                (mtch k (pr1 w) (upa (upb g)) s))
                               (idpath (mtch k (pr1 w) (upa (upb g)) s))))
                           (lam hg
                             (lam e
                               (lam s2
                                 (lam X
                                   (fromempty (paths nat (inst (succ k) (ii2 w) s2) g)
                                     (transportf (coprod slist unit) mvside (ii2 tt) (ii1 sf) e
                                       tt))))))
                           (nateqb (upa g) 2))
                          (idpath (nateqb (upa g) 2))))
                      p)))))))
        d)))
 (defq mtch-sound
   (pi d nat
      (pi p (tm d)
        (pi g nat
          (pi sf slist
            (pi e (paths (coprod slist unit) (mtch d p g snil) (ii1 sf))
              (paths nat (inst d p sf) g))))))
   (lam d
      (lam p
        (lam g (lam sf (lam e (mtch-sound-ext d p g snil sf e sf (lam v (lam t (lam h3 h3))))))))))
 ; the headline in the small: match, instantiate, come home
 (id? (vof 0 '(inst 1 (mcell 0 (lmv 0) (latom 0)) (scons (tpair 0 (tcode 0 (latom 1))) snil)))
      (vof 0 '(tcode 1 (mcell 0 (latom 1) (latom 0))))))

; --- the re-aim at address grain: the schematic swallow moves a deep
; --- patch's path from the variable's olds seat to its news seat by
; --- prefix surgery, the rule riding unchanged; swapping the seats back
; --- is home -- L1 for the metavariable lane, two ldroplenapp reads ---
(assert
 (defn reaim
   (pi a nlist (pi b nlist (pi p nlist nlist)))
   (lam a (lam b (lam p (lapp b (ldrop (nlen a) p))))))
 (defq reaim-involutive
   (pi a nlist
      (pi b nlist (pi w nlist (paths nlist (reaim b a (reaim a b (lapp a w))) (lapp a w)))))
   (lam a
      (lam b
        (lam w
          (pathscomp0 nlist (lapp a (ldrop (nlen b) (lapp b (ldrop (nlen a) (lapp a w)))))
            (lapp a (ldrop (nlen b) (lapp b w)))
            (lapp a w)
            (maponpaths nlist nlist (lam y (lapp a (ldrop (nlen b) (lapp b y))))
              (ldrop (nlen a) (lapp a w))
              w
              (ldroplenapp a w))
            (maponpaths nlist nlist (lam y (lapp a y)) (ldrop (nlen b) (lapp b w)) w
              (ldroplenapp b w)))))))
 ; the re-aim computes both ways: out through the olds seat, home again
 ((vof 0 '(reaim (ncons 1 nnil) (ncons 5 (ncons 6 nnil)) (ncons 1 (ncons 9 nnil))))
  = (vof 0 '(ncons 5 (ncons 6 (ncons 9 nnil)))))
 ((vof 0 '(reaim (ncons 5 (ncons 6 nnil)) (ncons 1 nnil)
            (reaim (ncons 1 nnil) (ncons 5 (ncons 6 nnil)) (ncons 1 (ncons 9 nnil)))))
  = (vof 0 '(ncons 1 (ncons 9 nnil)))))


; ============================================================================
; the STORE/REPO slice: a version IS its patch set, and the repo is keyed
; on that identity -- patch.v's identity slice (fid_swap / fid_dup) lands,
; then the store patch.v only promised. fid folds a frontier's hashes
; through cins, the SAME canonical set that carries a clash's rivals, so
; order-freedom IS cins-commutativity and dup-freedom IS cins-absorption.
; the key is lcode of the canonical list -- and lcode has a proven decoder
; (ldeccode), so the digest is INJECTIVE: patch.v's "a real digest when
; the store lands" parenthetical lands as a theorem (lcode-inj /
; repo-faithful), collision-freedom a law, not engineering. the repo
; speaks PRESENTATIONS: sget hashes canonically before it looks, so the
; store cannot even ask about replay order -- stored under one
; presentation, found under any other spelling the same set (repo-found;
; repo-swap / repo-dup the adjacent-swap and stutter corollaries), and
; distinct sets stay distinct.
; ============================================================================
; --- the canonical identity: fold the frontier through cins; an adjacent
; --- swap and a stutter vanish before the fold even lands (H1 / H2) ---
(assert
 (defn foldc
   (pi hs nlist (pi acc nlist nlist))
   (lam hs
      (lam acc
        ((nat_rect (lam q (pi v (nvec q) (pi s nlist nlist))) (lam v (lam s s))
           (lam k (lam IH (lam v (lam s (IH (pr2 v) (cins (pr1 v) s))))))
           (pr1 hs))
          (pr2 hs)
          acc))))
 (defn fid
   (pi hs nlist nlist)
   (lam hs (foldc hs nnil)))
 (defq foldcapp
   (pi a nlist
      (pi b nlist (pi s nlist (paths nlist (foldc (lapp a b) s) (foldc b (foldc a s))))))
   (lam a
      (lam b
        ((nat_rect
           (lam q
             (pi v (nvec q)
               (pi s nlist
                 (paths nlist (foldc (lapp (tpair q v) b) s) (foldc b (foldc (tpair q v) s))))))
           (lam v (lam s (idpath (foldc b s))))
           (lam k (lam IH (lam v (lam s (IH (pr2 v) (cins (pr1 v) s))))))
           (pr1 a))
          (pr2 a)))))
 (defq fid-swap
   (pi pre nlist
      (pi x nat
        (pi y nat
          (pi post nlist
            (paths nlist (fid (lapp pre (ncons x (ncons y post))))
              (fid (lapp pre (ncons y (ncons x post)))))))))
   (lam pre
      (lam x
        (lam y
          (lam post
            (pathscomp0 nlist (fid (lapp pre (ncons x (ncons y post))))
              (foldc post (cins y (cins x (foldc pre nnil))))
              (fid (lapp pre (ncons y (ncons x post))))
              (foldcapp pre (ncons x (ncons y post)) nnil)
              (pathscomp0 nlist (foldc post (cins y (cins x (foldc pre nnil))))
                (foldc post (cins x (cins y (foldc pre nnil))))
                (fid (lapp pre (ncons y (ncons x post))))
                (maponpaths nlist nlist (lam w (foldc post w))
                  (cins y (cins x (foldc pre nnil)))
                  (cins x (cins y (foldc pre nnil)))
                  (cinscomm y x (foldc pre nnil)))
                (pathsinv0 nlist (fid (lapp pre (ncons y (ncons x post))))
                  (foldc post (cins x (cins y (foldc pre nnil))))
                  (foldcapp pre (ncons y (ncons x post)) nnil)))))))))
 (defq fid-dup
   (pi pre nlist
      (pi x nat
        (pi post nlist
          (paths nlist (fid (lapp pre (ncons x (ncons x post)))) (fid (lapp pre (ncons x post)))))))
   (lam pre
      (lam x
        (lam post
          (pathscomp0 nlist (fid (lapp pre (ncons x (ncons x post))))
            (foldc post (cins x (cins x (foldc pre nnil))))
            (fid (lapp pre (ncons x post)))
            (foldcapp pre (ncons x (ncons x post)) nnil)
            (pathscomp0 nlist (foldc post (cins x (cins x (foldc pre nnil))))
              (foldc post (cins x (foldc pre nnil)))
              (fid (lapp pre (ncons x post)))
              (maponpaths nlist nlist (lam w (foldc post w)) (cins x (cins x (foldc pre nnil)))
                (cins x (foldc pre nnil))
                (cinsabsorb x (foldc pre nnil)))
              (pathsinv0 nlist (fid (lapp pre (ncons x post)))
                (foldc post (cins x (foldc pre nnil)))
                (foldcapp pre (ncons x post) nnil))))))))
 ; the canonical identity computes: order and repetition forgotten
 ((vof 0 '(fid (ncons 2 (ncons 1 (ncons 2 nnil))))) = (vof 0 '(ncons 1 (ncons 2 nnil)))))

; --- the digest is faithful: natgtb closes its own diagonal, so a code
; --- decodes on its own fuel -- lcode is injective, the real digest ---
(assert
 (defq natgtbnn
   (pi n nat (paths bool (natgtb n n) false))
   (lam n
      (nat_rect (lam q (paths bool (natgtb q q) false)) (idpath false) (lam k (lam IH IH)) n)))
 (defq lcode-inj
   (pi x nlist (pi y nlist (pi e (paths nat (lcode x) (lcode y)) (paths nlist x y))))
   (lam x
      (lam y
        (lam e
          (pathscomp0 nlist x (ldec (lcode x) (lcode x)) y
            (pathsinv0 nlist (ldec (lcode x) (lcode x)) x
              (ldeccode x (lcode x) (natgtbnn (lcode x))))
            (pathscomp0 nlist (ldec (lcode x) (lcode x)) (ldec (lcode x) (lcode y)) y
              (maponpaths nat nlist (lam m (ldec (lcode x) m)) (lcode x) (lcode y) e)
              (ldeccode y (lcode x)
                (transportf nat (lam w (paths bool (natgtb w (lcode x)) false)) (lcode x)
                  (lcode y)
                  e
                  (natgtbnn (lcode x)))))))))))

; --- the repo: a store from digests, put at raw-key grain, get through the
; --- identity -- a version stored once is found under every presentation
; --- of its set, and a miss rides through to the older book ---
(assert
 (defn store
   UU
   (pi k nat (coprod nat unit)))
 (defn repo0
   store
   (lam q (ii2 tt)))
 (defn skey
   (pi hs nlist nat)
   (lam hs (lcode (fid hs))))
 (defn sput
   (pi r store (pi k nat (pi u nat store)))
   (lam r
      (lam k (lam u (lam q (bool_rect (lam c (coprod nat unit)) (ii1 u) (r q) (nateqb q k)))))))
 (defn sget
   (pi r store (pi hs nlist (coprod nat unit)))
   (lam r (lam hs (r (skey hs)))))
 (defq repo-found
   (pi r store
      (pi u nat
        (pi k1 nlist
          (pi k2 nlist
            (pi e (paths nlist (fid k1) (fid k2))
              (paths (coprod nat unit) (sget (sput r (skey k1) u) k2) (ii1 u)))))))
   (lam r
      (lam u
        (lam k1
          (lam k2
            (lam e
              (maponpaths bool (coprod nat unit)
                (lam c (bool_rect (lam c2 (coprod nat unit)) (ii1 u) (r (skey k2)) c))
                (nateqb (skey k2) (skey k1))
                true
                (transportf nat (lam w (paths bool (nateqb w (skey k1)) true)) (skey k1)
                  (skey k2)
                  (maponpaths nlist nat (lam w2 (lcode w2)) (fid k1) (fid k2) e)
                  (nateqbrefl (skey k1))))))))))
 (defq repo-miss
   (pi r store
      (pi u nat
        (pi k1 nat
          (pi k2 nlist
            (pi h (paths bool (nateqb (skey k2) k1) false)
              (paths (coprod nat unit) (sget (sput r k1 u) k2) (sget r k2)))))))
   (lam r
      (lam u
        (lam k1
          (lam k2
            (lam h
              (maponpaths bool (coprod nat unit)
                (lam c (bool_rect (lam c2 (coprod nat unit)) (ii1 u) (r (skey k2)) c))
                (nateqb (skey k2) k1)
                false
                h)))))))
 (defq repo-swap
   (pi r store
      (pi u nat
        (pi pre nlist
          (pi x nat
            (pi y nat
              (pi post nlist
                (paths (coprod nat unit)
                  (sget (sput r (skey (lapp pre (ncons x (ncons y post)))) u)
                    (lapp pre (ncons y (ncons x post))))
                  (ii1 u))))))))
   (lam r
      (lam u
        (lam pre
          (lam x
            (lam y
              (lam post
                (repo-found r u (lapp pre (ncons x (ncons y post)))
                  (lapp pre (ncons y (ncons x post)))
                  (fid-swap pre x y post)))))))))
 (defq repo-dup
   (pi r store
      (pi u nat
        (pi pre nlist
          (pi x nat
            (pi post nlist
              (paths (coprod nat unit)
                (sget (sput r (skey (lapp pre (ncons x (ncons x post)))) u)
                  (lapp pre (ncons x post)))
                (ii1 u)))))))
   (lam r
      (lam u
        (lam pre
          (lam x
            (lam post
              (repo-found r u (lapp pre (ncons x (ncons x post))) (lapp pre (ncons x post))
                (fid-dup pre x post))))))))
 (defq repo-faithful
   (pi k1 nlist
      (pi k2 nlist (pi e (paths nat (skey k1) (skey k2)) (paths nlist (fid k1) (fid k2)))))
   (lam k1 (lam k2 (lam e (lcode-inj (fid k1) (fid k2) e)))))
 ; the repo in the small: stored under one replay, found under its swap
 ; and its stutter; a different frontier rides through to the empty repo
 ((vof 0 '(sget (sput repo0 (skey (ncons 2 (ncons 1 nnil))) 7) (ncons 1 (ncons 2 nnil))))
  = (vof 0 '(ii1 7)))
 ((vof 0 '(sget (sput repo0 (skey (ncons 2 (ncons 1 nnil))) 7)
           (ncons 1 (ncons 2 (ncons 1 nnil)))))
  = (vof 0 '(ii1 7)))
 ((vof 0 '(sget (sput repo0 (skey (ncons 2 (ncons 1 nnil))) 7) (ncons 3 nnil)))
  = (vof 0 '(ii2 tt))))

; ============================================================================
; the CRITICAL-PAIRS slice (patch.v's ninth), the uu leg: kid surgery at code
; grain. a cell is a cantor code (gcell a b), gka/gkb read the kids back, and
; gsub/gput read/write a tree ADDRESS (an nlist of directions, 0 the a-kid).
; the four laws mirror patch.v's RuleCommute kernel: get-put (gsubput), the
; two disturbance mirrors (gsubputab/gsubputba: a put on one side of the
; diverging seat leaves the other side's read alone), and the commute
; (gputcomm: diverging puts land the same tree, NO index re-aim). rw/rw2 and
; cp_earned stay Rocq-tier: option/mtch bookkeeping over these four.
; ladder style: each induction is a nat_rect over the address length whose
; step rungs are SEALED defqs taking the IH as a hypothesis; the commute's
; cell-swap rungs are ABSTRACT over the payloads (P Q A B ride as pis, so
; the towers appear once at the instantiation site, not in every statement).
; closed demos ride lof -- vof pays the unary grind on cell codes, lof does
; not (24ms vs 5s), and test/uu.l's differential battery ties the two.
; ============================================================================
(assert
 ; the cell code carries its tag in the SECOND seat -- (npair pair 2), not
 ; (npair 2 pair) -- so the charm never meets add/tri's unfolding loop and a
 ; cell at OPEN args stays one small stuck node instead of a succ tower that
 ; duplicates the core (the difference is ~10x on this whole slice's check)
 (defn gka (pi g nat nat) (lam g (upa (upa g))))
 (defn gkb (pi g nat nat) (lam g (upb (upa g))))
 (defn gcell (pi a nat (pi b nat nat)) (lam a (lam b (npair (npair a b) 2))))
 (defq gkabeta
   (pi a nat (pi b nat (paths nat (gka (gcell a b)) a)))
   (lam a
      (lam b
        (pathscomp0 nat (upa (upa (npair (npair a b) 2))) (upa (npair a b)) a
          (maponpaths nat nat (lam w (upa w)) (upa (npair (npair a b) 2)) (npair a b)
            (upabeta (npair a b) 2))
          (upabeta a b)))))
 (defq gkbbeta
   (pi a nat (pi b nat (paths nat (gkb (gcell a b)) b)))
   (lam a
      (lam b
        (pathscomp0 nat (upb (upa (npair (npair a b) 2))) (upb (npair a b)) b
          (maponpaths nat nat (lam w (upb w)) (upa (npair (npair a b) 2)) (npair a b)
            (upabeta (npair a b) 2))
          (upbbeta a b)))))
 (id? (vof 0 '3) (vof 0 '(gka (gcell 3 1))))
 (id? (vof 0 '1) (vof 0 '(gkb (gcell 3 1)))))

(assert
 (defn gsub
   (pi p nlist (pi g nat nat))
   (lam p
      (lam g
        ((nat_rect (lam q (pi v (nvec q) (pi g2 nat nat)))
           (lam v (lam g2 g2))
           (lam k
             (lam IH
               (lam v
                 (lam g2
                   (IH (pr2 v)
                     (bool_rect (lam c nat) (gka g2) (gkb g2) (niszero (pr1 v))))))))
           (pr1 p))
          (pr2 p)
          g))))
 (defn gput
   (pi p nlist (pi g nat (pi h nat nat)))
   (lam p
      (lam g
        (lam h
          ((nat_rect (lam q (pi v (nvec q) (pi g2 nat nat)))
             (lam v (lam g2 h))
             (lam k
               (lam IH
                 (lam v
                   (lam g2
                     (bool_rect (lam c nat)
                       (gcell (IH (pr2 v) (gka g2)) (gkb g2))
                       (gcell (gka g2) (IH (pr2 v) (gkb g2)))
                       (niszero (pr1 v)))))))
             (pr1 p))
            (pr2 p)
            g)))))
 1)

(assert
 (defq gsubput-stepa
   (pi k nat
      (pi vv (nvec k)
        (pi g nat
          (pi h nat
            (pi IH (paths nat (gsub (tpair k vv) (gput (tpair k vv) (gka g) h)) h)
              (paths nat
                (gsub (tpair k vv) (gka (gcell (gput (tpair k vv) (gka g) h) (gkb g))))
                h))))))
   (lam k
      (lam vv
        (lam g
          (lam h
            (lam IH
              (pathscomp0 nat
                (gsub (tpair k vv) (gka (gcell (gput (tpair k vv) (gka g) h) (gkb g))))
                (gsub (tpair k vv) (gput (tpair k vv) (gka g) h))
                h
                (maponpaths nat nat (lam z (gsub (tpair k vv) z))
                  (gka (gcell (gput (tpair k vv) (gka g) h) (gkb g)))
                  (gput (tpair k vv) (gka g) h)
                  (gkabeta (gput (tpair k vv) (gka g) h) (gkb g)))
                IH)))))))
 (defq gsubput-stepb
   (pi k nat
      (pi vv (nvec k)
        (pi g nat
          (pi h nat
            (pi IH (paths nat (gsub (tpair k vv) (gput (tpair k vv) (gkb g) h)) h)
              (paths nat
                (gsub (tpair k vv) (gkb (gcell (gka g) (gput (tpair k vv) (gkb g) h))))
                h))))))
   (lam k
      (lam vv
        (lam g
          (lam h
            (lam IH
              (pathscomp0 nat
                (gsub (tpair k vv) (gkb (gcell (gka g) (gput (tpair k vv) (gkb g) h))))
                (gsub (tpair k vv) (gput (tpair k vv) (gkb g) h))
                h
                (maponpaths nat nat (lam z (gsub (tpair k vv) z))
                  (gkb (gcell (gka g) (gput (tpair k vv) (gkb g) h)))
                  (gput (tpair k vv) (gkb g) h)
                  (gkbbeta (gka g) (gput (tpair k vv) (gkb g) h)))
                IH)))))))
 (defq gsubput
   (pi p nlist (pi g nat (pi h nat (paths nat (gsub p (gput p g h)) h))))
   (lam p
      ((nat_rect
         (lam q
           (pi v (nvec q)
             (pi g nat
               (pi h nat (paths nat (gsub (tpair q v) (gput (tpair q v) g h)) h)))))
         (lam v (lam g (lam h (idpath h))))
         (lam k
           (lam IH
             (lam v
               (lam g
                 (lam h
                   (bool_rect
                     (lam cb
                       (paths nat
                         (gsub (tpair k (pr2 v))
                           (bool_rect (lam c nat)
                             (gka
                               (bool_rect (lam c nat)
                                 (gcell (gput (tpair k (pr2 v)) (gka g) h) (gkb g))
                                 (gcell (gka g) (gput (tpair k (pr2 v)) (gkb g) h))
                                 cb))
                             (gkb
                               (bool_rect (lam c nat)
                                 (gcell (gput (tpair k (pr2 v)) (gka g) h) (gkb g))
                                 (gcell (gka g) (gput (tpair k (pr2 v)) (gkb g) h))
                                 cb))
                             cb))
                         h))
                     (gsubput-stepa k (pr2 v) g h (IH (pr2 v) (gka g) h))
                     (gsubput-stepb k (pr2 v) g h (IH (pr2 v) (gkb g) h))
                     (niszero (pr1 v))))))))
         (pr1 p))
        (pr2 p))))
 1)

(assert
 (defq gsubputab-stepa
   (pi k nat
      (pi vv (nvec k)
        (pi ra nlist
          (pi rb nlist
            (pi g nat
              (pi h nat
                (pi IH (paths nat
                         (gsub (lapp (tpair k vv) (ncons 1 rb))
                           (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h))
                         (gsub (lapp (tpair k vv) (ncons 1 rb)) (gka g)))
                  (paths nat
                    (gsub (lapp (tpair k vv) (ncons 1 rb))
                      (gka (gcell (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h) (gkb g))))
                    (gsub (lapp (tpair k vv) (ncons 1 rb)) (gka g))))))))))
   (lam k
      (lam vv
        (lam ra
          (lam rb
            (lam g
              (lam h
                (lam IH
                  (pathscomp0 nat
                    (gsub (lapp (tpair k vv) (ncons 1 rb))
                      (gka (gcell (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h) (gkb g))))
                    (gsub (lapp (tpair k vv) (ncons 1 rb))
                      (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h))
                    (gsub (lapp (tpair k vv) (ncons 1 rb)) (gka g))
                    (maponpaths nat nat
                      (lam z (gsub (lapp (tpair k vv) (ncons 1 rb)) z))
                      (gka (gcell (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h) (gkb g)))
                      (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h)
                      (gkabeta (gput (lapp (tpair k vv) (ncons 0 ra)) (gka g) h) (gkb g)))
                    IH)))))))))
 (defq gsubputab-stepb
   (pi k nat
      (pi vv (nvec k)
        (pi ra nlist
          (pi rb nlist
            (pi g nat
              (pi h nat
                (pi IH (paths nat
                         (gsub (lapp (tpair k vv) (ncons 1 rb))
                           (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h))
                         (gsub (lapp (tpair k vv) (ncons 1 rb)) (gkb g)))
                  (paths nat
                    (gsub (lapp (tpair k vv) (ncons 1 rb))
                      (gkb (gcell (gka g) (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h))))
                    (gsub (lapp (tpair k vv) (ncons 1 rb)) (gkb g))))))))))
   (lam k
      (lam vv
        (lam ra
          (lam rb
            (lam g
              (lam h
                (lam IH
                  (pathscomp0 nat
                    (gsub (lapp (tpair k vv) (ncons 1 rb))
                      (gkb (gcell (gka g) (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h))))
                    (gsub (lapp (tpair k vv) (ncons 1 rb))
                      (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h))
                    (gsub (lapp (tpair k vv) (ncons 1 rb)) (gkb g))
                    (maponpaths nat nat
                      (lam z (gsub (lapp (tpair k vv) (ncons 1 rb)) z))
                      (gkb (gcell (gka g) (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h)))
                      (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h)
                      (gkbbeta (gka g) (gput (lapp (tpair k vv) (ncons 0 ra)) (gkb g) h)))
                    IH)))))))))
 (defq gsubputab
   (pi w nlist
      (pi ra nlist
        (pi rb nlist
          (pi g nat
            (pi h nat
              (paths nat
                (gsub (lapp w (ncons 1 rb)) (gput (lapp w (ncons 0 ra)) g h))
                (gsub (lapp w (ncons 1 rb)) g)))))))
   (lam w
      (lam ra
        (lam rb
          ((nat_rect
             (lam q
               (pi v (nvec q)
                 (pi g nat
                   (pi h nat
                     (paths nat
                       (gsub (lapp (tpair q v) (ncons 1 rb))
                         (gput (lapp (tpair q v) (ncons 0 ra)) g h))
                       (gsub (lapp (tpair q v) (ncons 1 rb)) g))))))
             (lam v
               (lam g
                 (lam h
                   (maponpaths nat nat (lam z (gsub rb z))
                     (gkb (gcell (gput ra (gka g) h) (gkb g)))
                     (gkb g)
                     (gkbbeta (gput ra (gka g) h) (gkb g))))))
             (lam k
               (lam IH
                 (lam v
                   (lam g
                     (lam h
                       (bool_rect
                         (lam cb
                           (paths nat
                             (gsub (lapp (tpair k (pr2 v)) (ncons 1 rb))
                               (bool_rect (lam c nat)
                                 (gka
                                   (bool_rect (lam c nat)
                                     (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) h) (gkb g))
                                     (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) h))
                                     cb))
                                 (gkb
                                   (bool_rect (lam c nat)
                                     (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) h) (gkb g))
                                     (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) h))
                                     cb))
                                 cb))
                             (gsub (lapp (tpair k (pr2 v)) (ncons 1 rb))
                               (bool_rect (lam c nat) (gka g) (gkb g) cb))))
                         (gsubputab-stepa k (pr2 v) ra rb g h (IH (pr2 v) (gka g) h))
                         (gsubputab-stepb k (pr2 v) ra rb g h (IH (pr2 v) (gkb g) h))
                         (niszero (pr1 v))))))))
             (pr1 w))
            (pr2 w))))))
 1)

(assert
 (defq gsubputba-stepa
   (pi k nat
      (pi vv (nvec k)
        (pi ra nlist
          (pi rb nlist
            (pi g nat
              (pi h nat
                (pi IH (paths nat
                         (gsub (lapp (tpair k vv) (ncons 0 ra))
                           (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h))
                         (gsub (lapp (tpair k vv) (ncons 0 ra)) (gka g)))
                  (paths nat
                    (gsub (lapp (tpair k vv) (ncons 0 ra))
                      (gka (gcell (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h) (gkb g))))
                    (gsub (lapp (tpair k vv) (ncons 0 ra)) (gka g))))))))))
   (lam k
      (lam vv
        (lam ra
          (lam rb
            (lam g
              (lam h
                (lam IH
                  (pathscomp0 nat
                    (gsub (lapp (tpair k vv) (ncons 0 ra))
                      (gka (gcell (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h) (gkb g))))
                    (gsub (lapp (tpair k vv) (ncons 0 ra))
                      (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h))
                    (gsub (lapp (tpair k vv) (ncons 0 ra)) (gka g))
                    (maponpaths nat nat
                      (lam z (gsub (lapp (tpair k vv) (ncons 0 ra)) z))
                      (gka (gcell (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h) (gkb g)))
                      (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h)
                      (gkabeta (gput (lapp (tpair k vv) (ncons 1 rb)) (gka g) h) (gkb g)))
                    IH)))))))))
 (defq gsubputba-stepb
   (pi k nat
      (pi vv (nvec k)
        (pi ra nlist
          (pi rb nlist
            (pi g nat
              (pi h nat
                (pi IH (paths nat
                         (gsub (lapp (tpair k vv) (ncons 0 ra))
                           (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h))
                         (gsub (lapp (tpair k vv) (ncons 0 ra)) (gkb g)))
                  (paths nat
                    (gsub (lapp (tpair k vv) (ncons 0 ra))
                      (gkb (gcell (gka g) (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h))))
                    (gsub (lapp (tpair k vv) (ncons 0 ra)) (gkb g))))))))))
   (lam k
      (lam vv
        (lam ra
          (lam rb
            (lam g
              (lam h
                (lam IH
                  (pathscomp0 nat
                    (gsub (lapp (tpair k vv) (ncons 0 ra))
                      (gkb (gcell (gka g) (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h))))
                    (gsub (lapp (tpair k vv) (ncons 0 ra))
                      (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h))
                    (gsub (lapp (tpair k vv) (ncons 0 ra)) (gkb g))
                    (maponpaths nat nat
                      (lam z (gsub (lapp (tpair k vv) (ncons 0 ra)) z))
                      (gkb (gcell (gka g) (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h)))
                      (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h)
                      (gkbbeta (gka g) (gput (lapp (tpair k vv) (ncons 1 rb)) (gkb g) h)))
                    IH)))))))))
 (defq gsubputba
   (pi w nlist
      (pi ra nlist
        (pi rb nlist
          (pi g nat
            (pi h nat
              (paths nat
                (gsub (lapp w (ncons 0 ra)) (gput (lapp w (ncons 1 rb)) g h))
                (gsub (lapp w (ncons 0 ra)) g)))))))
   (lam w
      (lam ra
        (lam rb
          ((nat_rect
             (lam q
               (pi v (nvec q)
                 (pi g nat
                   (pi h nat
                     (paths nat
                       (gsub (lapp (tpair q v) (ncons 0 ra))
                         (gput (lapp (tpair q v) (ncons 1 rb)) g h))
                       (gsub (lapp (tpair q v) (ncons 0 ra)) g))))))
             (lam v
               (lam g
                 (lam h
                   (maponpaths nat nat (lam z (gsub ra z))
                     (gka (gcell (gka g) (gput rb (gkb g) h)))
                     (gka g)
                     (gkabeta (gka g) (gput rb (gkb g) h))))))
             (lam k
               (lam IH
                 (lam v
                   (lam g
                     (lam h
                       (bool_rect
                         (lam cb
                           (paths nat
                             (gsub (lapp (tpair k (pr2 v)) (ncons 0 ra))
                               (bool_rect (lam c nat)
                                 (gka
                                   (bool_rect (lam c nat)
                                     (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) h) (gkb g))
                                     (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) h))
                                     cb))
                                 (gkb
                                   (bool_rect (lam c nat)
                                     (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) h) (gkb g))
                                     (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) h))
                                     cb))
                                 cb))
                             (gsub (lapp (tpair k (pr2 v)) (ncons 0 ra))
                               (bool_rect (lam c nat) (gka g) (gkb g) cb))))
                         (gsubputba-stepa k (pr2 v) ra rb g h (IH (pr2 v) (gka g) h))
                         (gsubputba-stepb k (pr2 v) ra rb g h (IH (pr2 v) (gkb g) h))
                         (niszero (pr1 v))))))))
             (pr1 w))
            (pr2 w))))))
 1)

(assert
 (defq gputcomm-cellz
   (pi A nat (pi B nat (pi ra nlist (pi rb nlist (pi x nat (pi y nat
      (paths nat (gcell (gka (gcell (gput ra A x) B)) (gput rb (gkb (gcell (gput ra A x) B)) y)) (gcell (gput ra (gka (gcell A (gput rb B y))) x) (gkb (gcell A (gput rb B y)))))))))))
   (lam A (lam B (lam ra (lam rb (lam x (lam y
      (pathscomp0 nat (gcell (gka (gcell (gput ra A x) B)) (gput rb (gkb (gcell (gput ra A x) B)) y)) (gcell (gput ra A x) (gput rb B y)) (gcell (gput ra (gka (gcell A (gput rb B y))) x) (gkb (gcell A (gput rb B y))))
        (pathscomp0 nat (gcell (gka (gcell (gput ra A x) B)) (gput rb (gkb (gcell (gput ra A x) B)) y)) (gcell (gput ra A x) (gput rb (gkb (gcell (gput ra A x) B)) y)) (gcell (gput ra A x) (gput rb B y)) (maponpaths nat nat (lam z (gcell z (gput rb (gkb (gcell (gput ra A x) B)) y))) (gka (gcell (gput ra A x) B)) (gput ra A x) (gkabeta (gput ra A x) B)) (maponpaths nat nat (lam z (gcell (gput ra A x) (gput rb z y))) (gkb (gcell (gput ra A x) B)) B (gkbbeta (gput ra A x) B)))
        (pathsinv0 nat (gcell (gput ra (gka (gcell A (gput rb B y))) x) (gkb (gcell A (gput rb B y)))) (gcell (gput ra A x) (gput rb B y)) (pathscomp0 nat (gcell (gput ra (gka (gcell A (gput rb B y))) x) (gkb (gcell A (gput rb B y)))) (gcell (gput ra A x) (gkb (gcell A (gput rb B y)))) (gcell (gput ra A x) (gput rb B y)) (maponpaths nat nat (lam z (gcell (gput ra z x) (gkb (gcell A (gput rb B y))))) (gka (gcell A (gput rb B y))) A (gkabeta A (gput rb B y))) (maponpaths nat nat (lam z (gcell (gput ra A x) z)) (gkb (gcell A (gput rb B y))) (gput rb B y) (gkbbeta A (gput rb B y)))))))))))))
 (defq gputcomm-cella
   (pi P nat (pi Q nat (pi B nat (pi la nlist (pi lb nlist (pi x nat (pi y nat
      (pi IH (paths nat (gput lb P y) (gput la Q x))
        (paths nat (gcell (gput lb (gka (gcell P B)) y) (gkb (gcell P B))) (gcell (gput la (gka (gcell Q B)) x) (gkb (gcell Q B))))))))))))
   (lam P (lam Q (lam B (lam la (lam lb (lam x (lam y (lam IH
      (pathscomp0 nat (gcell (gput lb (gka (gcell P B)) y) (gkb (gcell P B))) (gcell (gput la Q x) B) (gcell (gput la (gka (gcell Q B)) x) (gkb (gcell Q B)))
        (pathscomp0 nat (gcell (gput lb (gka (gcell P B)) y) (gkb (gcell P B))) (gcell (gput lb P y) B) (gcell (gput la Q x) B)
          (pathscomp0 nat (gcell (gput lb (gka (gcell P B)) y) (gkb (gcell P B))) (gcell (gput lb P y) (gkb (gcell P B))) (gcell (gput lb P y) B) (maponpaths nat nat (lam z (gcell (gput lb z y) (gkb (gcell P B)))) (gka (gcell P B)) P (gkabeta P B)) (maponpaths nat nat (lam z (gcell (gput lb P y) z)) (gkb (gcell P B)) B (gkbbeta P B)))
          (maponpaths nat nat (lam z (gcell z B)) (gput lb P y) (gput la Q x) IH))
        (pathsinv0 nat (gcell (gput la (gka (gcell Q B)) x) (gkb (gcell Q B))) (gcell (gput la Q x) B) (pathscomp0 nat (gcell (gput la (gka (gcell Q B)) x) (gkb (gcell Q B))) (gcell (gput la Q x) (gkb (gcell Q B))) (gcell (gput la Q x) B) (maponpaths nat nat (lam z (gcell (gput la z x) (gkb (gcell Q B)))) (gka (gcell Q B)) Q (gkabeta Q B)) (maponpaths nat nat (lam z (gcell (gput la Q x) z)) (gkb (gcell Q B)) B (gkbbeta Q B))))))))))))))
 (defq gputcomm-cellb
   (pi A nat (pi P nat (pi Q nat (pi la nlist (pi lb nlist (pi x nat (pi y nat
      (pi IH (paths nat (gput lb P y) (gput la Q x))
        (paths nat (gcell (gka (gcell A P)) (gput lb (gkb (gcell A P)) y)) (gcell (gka (gcell A Q)) (gput la (gkb (gcell A Q)) x)))))))))))
   (lam A (lam P (lam Q (lam la (lam lb (lam x (lam y (lam IH
      (pathscomp0 nat (gcell (gka (gcell A P)) (gput lb (gkb (gcell A P)) y)) (gcell A (gput la Q x)) (gcell (gka (gcell A Q)) (gput la (gkb (gcell A Q)) x))
        (pathscomp0 nat (gcell (gka (gcell A P)) (gput lb (gkb (gcell A P)) y)) (gcell A (gput lb P y)) (gcell A (gput la Q x))
          (pathscomp0 nat (gcell (gka (gcell A P)) (gput lb (gkb (gcell A P)) y)) (gcell A (gput lb (gkb (gcell A P)) y)) (gcell A (gput lb P y)) (maponpaths nat nat (lam z (gcell z (gput lb (gkb (gcell A P)) y))) (gka (gcell A P)) A (gkabeta A P)) (maponpaths nat nat (lam z (gcell A (gput lb z y))) (gkb (gcell A P)) P (gkbbeta A P)))
          (maponpaths nat nat (lam z (gcell A z)) (gput lb P y) (gput la Q x) IH))
        (pathsinv0 nat (gcell (gka (gcell A Q)) (gput la (gkb (gcell A Q)) x)) (gcell A (gput la Q x)) (pathscomp0 nat (gcell (gka (gcell A Q)) (gput la (gkb (gcell A Q)) x)) (gcell A (gput la (gkb (gcell A Q)) x)) (gcell A (gput la Q x)) (maponpaths nat nat (lam z (gcell z (gput la (gkb (gcell A Q)) x))) (gka (gcell A Q)) A (gkabeta A Q)) (maponpaths nat nat (lam z (gcell A (gput la z x))) (gkb (gcell A Q)) Q (gkbbeta A Q))))))))))))))
 (defq gputcomm
   (pi w nlist (pi ra nlist (pi rb nlist (pi g nat (pi x nat (pi y nat
      (paths nat (gput (lapp w (ncons 1 rb)) (gput (lapp w (ncons 0 ra)) g x) y)
                 (gput (lapp w (ncons 0 ra)) (gput (lapp w (ncons 1 rb)) g y) x))))))))
   (lam w (lam ra (lam rb
      ((nat_rect
         (lam q (pi v (nvec q) (pi g nat (pi x nat (pi y nat
           (paths nat (gput (lapp (tpair q v) (ncons 1 rb)) (gput (lapp (tpair q v) (ncons 0 ra)) g x) y)
                      (gput (lapp (tpair q v) (ncons 0 ra)) (gput (lapp (tpair q v) (ncons 1 rb)) g y) x)))))))
         (lam v (lam g (lam x (lam y (gputcomm-cellz (gka g) (gkb g) ra rb x y)))))
         (lam k (lam IH (lam v (lam g (lam x (lam y
           (bool_rect
             (lam cb (paths nat
  (bool_rect (lam c nat)
    (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) x) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) x)) cb)) y) (gkb (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) x) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) x)) cb)))
    (gcell (gka (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) x) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) x)) cb)) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) x) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) x)) cb)) y))
    cb)
  (bool_rect (lam c nat)
    (gcell (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) y) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) y)) cb)) x) (gkb (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) y) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) y)) cb)))
    (gcell (gka (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) y) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) y)) cb)) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb (bool_rect (lam c nat) (gcell (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) y) (gkb g)) (gcell (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) y)) cb)) x))
    cb)))
             (gputcomm-cella (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gka g) x) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gka g) y) (gkb g)
               (lapp (tpair k (pr2 v)) (ncons 0 ra)) (lapp (tpair k (pr2 v)) (ncons 1 rb)) x y (IH (pr2 v) (gka g) x y))
             (gputcomm-cellb (gka g) (gput (lapp (tpair k (pr2 v)) (ncons 0 ra)) (gkb g) x) (gput (lapp (tpair k (pr2 v)) (ncons 1 rb)) (gkb g) y)
               (lapp (tpair k (pr2 v)) (ncons 0 ra)) (lapp (tpair k (pr2 v)) (ncons 1 rb)) x y (IH (pr2 v) (gkb g) x y))
             (niszero (pr1 v)))))))))
         (pr1 w))
        (pr2 w))))))
 1)

(assert
 ; the slice in the small, at lof speed: the write lands where the read
 ; looks, the diverging orders land one tree, the other side's read rides
 ((lof '(gsub (ncons 1 (ncons 0 nnil)) (gput (ncons 1 (ncons 0 nnil)) (gcell 0 (gcell 1 0)) 2))) = 2)
 ((lof '(gput (ncons 1 nnil) (gput (ncons 0 nnil) (gcell 3 4) 8) 9))
  = (lof '(gput (ncons 0 nnil) (gput (ncons 1 nnil) (gcell 3 4) 9) 8)))
 ((lof '(gsub (ncons 1 nnil) (gput (ncons 0 nnil) (gcell 3 4) 8))) = 4))
