; test/uuval.l -- love's own values, modelled in uu: the measure tower of
; claude.md's lambda equations ($x = ceil (re (net x)), ?x = ($x > 0)) built as
; a stack OF retractions, so the laws test/law.l can only refute become theorems
; (tools/law2uu.l compiles them off law.l's own rows into test/uuvallaw.l -- one
; spelling, both lanes). the value universe is a binary tree of measures -- love's
; net sums over a compound's parts and reads nothing else, so a leaf is its
; measure and no gem's value (hence no ieee) enters the model.
;
; the tower, each rung a section/retraction pair whose round trip is definitional:
;
;   bool  --nbit-->  nat  --nzed-->  zed  --zrea-->  rea  --rmsr-->  msr  --vnum--> val
;         <-natbit-       <--zsat--       <-rceil--       <--mre---        <--vnet--
;
; a real is a half-integer (z, plus a bit for +1/2): the smallest carrier closed
; under addition on which ceil is not the identity, which is all the tower's shape
; asks of it -- 2.5 ceils to 3 and -2.5 to -2 here, exactly as love answers.
;
; a leaf is not quite its measure: () and 0 net the same, read nil the same, and
; are still two values, so the leaf carries a tag (atm) and = reads it. that one
; bit is what a whole layer of the model needed and nothing else in it uses.
; the equality itself is an equivalence here -- reflexive, and transitive off the
; reflection of each rung's decidable equality into a path -- which is what lets
; a link's two limbs be compared through the heights neither of them spells.
; and beside it sits the pinned carrier, the same trees with the index tied to
; the height, where the spelling is unique and `paths` is the equality. the two
; are bridged both ways: veq answers true exactly when the two values pin to the
; same one, so love's = is a decision procedure for paths and every law can be
; read either way. the laws below are given in both.
; above the tower sits the charm ceiling, the only rung with a width -- zsat is
; the retraction and stays unbounded, zsatc is what the word can hold -- and
; beside it the colours, the sign structure ?x = ($x > 0) actually rests on.
; the laws here are about idempotence and factorisation, so a wrong zadd cannot
; fake one; the algebra of + and * is test/uuvalband.l's, and what gates the
; arithmetic in both is the differential in test/uuvaldiff.l, which runs love's
; own saturate beside this model's vsat over test/law.l's corpus.
;
; substrate: test/uu.l (the kernel + nat vocabulary; this file loads after it by
; sort order). guard: fail by name if the kernel is not up.
(assert (member? 'vof (names ())))

; --- zed: the integers, ceil's codomain. ii1 n is n, ii2 n is -(succ n) ---
(assert
 (defn zed UU (coprod nat nat))
 (defn nzed (pi n nat zed) (lam n (ii1 n)))                 ; the section: a nat is an integer
 (defn zneg (pi n nat zed) (lam n (ii2 n)))                 ; -(n+1)
 (defn zzero zed (ii1 0))
 (defn zsucc (pi z zed zed)
   (lam z (coprod_rect (lam w zed)
             (lam n (ii1 (succ n)))
             (lam n (nat_rect (lam q zed) (ii1 0) (lam k (lam r (ii2 k))) n))
             z)))
 ; the truncation to nat -- max(0,.) -- and its round trip
 (defn zsat (pi z zed nat)
   (lam z (coprod_rect (lam w nat) (lam n n) (lam n 0) z)))
 ; ..and the charm ceiling above it. a charm is the word minus its tag bits, so
 ; a net that will not fit lands on the edge rather than wrapping or widening
 ; (love.c's ai_saturate). the width is a parameter and the laws below are
 ; stated against chmax, holding at any width -- only the literal moves; a small
 ; one keeps the ceiling in reach of a demo. zsat is the tower's retraction and
 ; stays unbounded: the round trip is a fact about the measure, the ceiling a
 ; fact about the word it is read into, and only the second has a width.
 ; the step cases on b rather than folding it: conv is eager, so a nat_rect
 ; over b would evaluate the step at every level below it and cost b^a
 (defn nmin (pi a nat (pi b nat nat))
   (lam a (nat_rect (lam q (pi b nat nat))
             (lam b 0)
             (lam j (lam IH (lam b (bool_rect (lam x nat) 0 (succ (IH (npred b)))
                                      (niszero b)))))
             a)))
 (defn chmax nat 7)
 (defn zsatc (pi z zed nat) (lam z (nmin chmax (zsat z))))
 (defn zpred (pi z zed zed)
   (lam z (coprod_rect (lam w zed)
             (lam n (nat_rect (lam q zed) (ii2 0) (lam k (lam r (ii1 k))) n))
             (lam n (ii2 (succ n)))
             z)))
 ; addition by iteration -- b counts the steps, each one zsucc or zpred. the
 ; truncated-subtraction spelling computes the same sums and proves nothing:
 ; every law of + below reads off these four definitional equations, and
 ; natminus has none to give.
 (defn zaddn (pi a zed (pi n nat zed))
   (lam a (lam n (nat_rect (lam q zed) a (lam k (lam r (zsucc r))) n))))
 (defn zsubn (pi a zed (pi n nat zed))
   (lam a (lam n (nat_rect (lam q zed) a (lam k (lam r (zpred r))) n))))
 (defn zadd (pi a zed (pi b zed zed))
   (lam a (lam b (coprod_rect (lam w zed)
                    (lam n (zaddn a n))
                    (lam n (zsubn a (succ n)))
                    b))))
 (= (vof 0 '(ii1 5)) (vof 0 '(zadd (ii1 2) (ii1 3))))
 (= (vof 0 '(ii2 0)) (vof 0 '(zadd (ii1 2) (ii2 2))))     ;  2 + (-3) = -1
 (= (vof 0 '(ii1 1)) (vof 0 '(zadd (ii2 1) (ii1 3))))     ; -2 +   3  =  1
 (= (vof 0 '(ii2 3)) (vof 0 '(zadd (ii2 1) (ii2 1))))     ; -2 + (-2) = -4
 (= (vof 0 '(ii1 0)) (vof 0 '(zsucc (ii2 0)))))

; --- rea: a real is a half-integer, z + (h ? 1/2 : 0). ceil is the carry ---
(assert
 (defn rea UU (total2 z zed bool))
 (defn rmk (pi z zed (pi h bool rea)) (lam z (lam h (tpair z h))))
 (defn zrea (pi z zed rea) (lam z (tpair z false)))         ; the section: an integer is a real
 (defn rceil (pi r rea zed)
   (lam r (bool_rect (lam x zed) (zsucc (pr1 r)) (pr1 r) (pr2 r))))
 (defn rzero rea (zrea zzero))
 (defn radd (pi a rea (pi b rea rea))
   (lam a (lam b (bool_rect (lam x rea)
      (tpair (zsucc (zadd (pr1 a) (pr1 b))) false)           ; two halves carry
      (tpair (zadd (pr1 a) (pr1 b)) (orb (pr2 a) (pr2 b)))
      (andb (pr2 a) (pr2 b))))))
 (= (vof 0 '(ii1 3)) (vof 0 '(rceil (rmk (ii1 2) true))))  ; ceil  2.5 =  3
 (= (vof 0 '(ii2 1)) (vof 0 '(rceil (rmk (ii2 2) true))))  ; ceil -2.5 = -2
 (= (vof 0 '(rmk (ii1 3) false)) (vof 0 '(radd (rmk (ii1 1) true) (rmk (ii1 1) true)))))

; --- msr: the measure is complex, and re is the rung love's ceil reads ---
(assert
 (defn msr UU (total2 r rea rea))
 (defn mre (pi m msr rea) (lam m (pr1 m)))
 (defn rmsr (pi r rea msr) (lam r (tpair r rzero)))         ; the section: a real is a measure
 (defn mzero msr (rmsr rzero))
 (defn madd (pi a msr (pi b msr msr))
   (lam a (lam b (tpair (radd (pr1 a) (pr1 b)) (radd (pr2 a) (pr2 b)))))))

; --- atm: what a leaf carries. () and 0 net the same and both read nil, and
; --- they are still not one value -- love keeps "" and () and 0 apart, and
; --- (= () 0) is false -- so a leaf carries a tag and not just its measure ---
(assert
 (defn atm UU (coprod unit msr))
 (defn aunit atm (ii1 tt))                                  ; ()
 (defn anum (pi m msr atm) (lam m (ii2 m)))                 ; a number is its measure
 (defn anet (pi a atm msr)
   (lam a (coprod_rect (lam w msr) (lam u mzero) (lam m m) a))))

; --- val: a value is a binary tree of measures, depth-indexed (nlist's trick).
; --- at every level a node is a leaf (ii1, carrying its measure) or a pair of
; --- the level below (ii2) -- love's link. so the depth is a height bound and
; --- not a shape: a leaf spells the same at every depth, which is what makes a
; --- node's head unambiguous and lets cap/cup/two? read it without recursing.
; --- net folds the tree ---
(assert
 (defn vtre (pi d nat UU)
   (lam d (nat_rect (lam q UU) atm (lam k (lam T (coprod atm (total2 x T T)))) d)))
 ; the two limbs of a link, named. bare pr1/pr2 would do it here, but the Rocq
 ; export elaborates a sum_rect's branches before it unifies the scrutinee's
 ; type, so the pair's family is still a metavariable when pr2 asks for it and
 ; the unification goes higher-order. a named projection pins it from its own
 ; argument type instead, which is first-order and always solvable.
 (defn vfst (pi k nat (pi p (total2 x (vtre k) (vtre k)) (vtre k))) (lam k (lam p (pr1 p))))
 (defn vsnd (pi k nat (pi p (total2 x (vtre k) (vtre k)) (vtre k))) (lam k (lam p (pr2 p))))
 (defn netd (pi d nat (pi t (vtre d) msr))
   (lam d (nat_rect (lam q (pi t (vtre q) msr))
             (lam t (anet t))
             (lam k (lam IH (lam t (coprod_rect (lam w msr)
                                      (lam m (anet m))
                                      (lam p (madd (IH (vfst k p)) (IH (vsnd k p))))
                                      t))))
             d)))
 (defn val UU (total2 d nat (vtre d)))
 (defn vnum (pi m msr val) (lam m (tpair 0 (anum m))))     ; a number, at height 0
 (defn vnilv val (tpair 0 aunit))                           ; ..and () beside it
 (defn vnet (pi v val msr) (lam v (netd (pr1 v) (pr2 v))))
 ; the sections up the tower, worn as values
 (defn rval (pi r rea val) (lam r (vnum (rmsr r))))
 (defn zval (pi z zed val) (lam z (rval (zrea z))))
 (defn nval (pi n nat val) (lam n (zval (nzed n))))
 (defn nbit (pi b bool nat) (lam b (bool_rect (lam x nat) 1 0 b)))
 (defn bval (pi b bool val) (lam b (nval (nbit b))))
 ; --- and love's words, each the composite down from val ---
 (defn natbit (pi n nat bool) (lam n (negb (niszero n))))   ; the bit of a saturated measure
 (defn vre   (pi v val rea)  (lam v (mre (vnet v))))
 (defn vceil (pi v val zed)  (lam v (rceil (vre v))))       ; love's ceil
 (defn vsat  (pi v val nat)  (lam v (zsat (vceil v))))      ; love's saturate, $x
 (defn vsatc (pi v val nat)  (lam v (zsatc (vceil v))))     ; ..as it lands in a charm
 (defn vbit  (pi v val bool) (lam v (natbit (vsat v))))     ; love's bit, ?x
 (defn vnil  (pi v val bool) (lam v (negb (vbit v))))       ; love's nil?, !x
 ; --- the colours, read off the sign of the measure's real part. green is the
 ; --- kept band (what $ keeps), red the clamped half below the floor, blue the
 ; --- floor itself. green and blue are dual and not disjoint: 0 is green by
 ; --- sign and blue by measure, the overlap where green meets nothing ---
 (defn zgreen (pi z zed bool)
   (lam z (coprod_rect (lam w bool) (lam n true) (lam n false) z)))
 (defn zblue (pi z zed bool)
   (lam z (coprod_rect (lam w bool) (lam n (niszero n)) (lam n false) z)))
 (defn vgreen (pi v val bool) (lam v (zgreen (pr1 (vre v)))))
 (defn vred   (pi v val bool) (lam v (negb (vgreen v))))
 ; ..a half-integer is the floor only with its half bit down
 (defn vblue  (pi v val bool) (lam v (andb (zblue (pr1 (vre v))) (negb (pr2 (vre v)))))))

; --- the retraction ladder: each rung's round trip, four of the five by idpath ---
(assert
 (defn uv-retract (pi A UU (pi B UU (pi s (pi a A B) (pi r (pi b B A) UU))))
   (lam A (lam B (lam s (lam r (pi a A (paths A (r (s a)) a)))))))
 (defq uv-net-sect  (uv-retract msr val vnum vnet)  (lam m (idpath m)))
 (defq uv-re-sect   (uv-retract rea msr rmsr mre)   (lam r (idpath r)))
 (defq uv-ceil-sect (uv-retract zed rea zrea rceil) (lam z (idpath z)))
 (defq uv-sat-sect  (uv-retract nat zed nzed zsat)  (lam n (idpath n)))
 (defq uv-bit-sect  (uv-retract bool nat nbit natbit)
   (lam b (bool_rect (lam x (paths bool (natbit (nbit x)) x))
            (idpath true) (idpath false) b))))

; --- the charm ceiling, the one rung with a width. $ is non-negative by its
; --- type; what is left to state is that it keeps a net that fits, clamps the
; --- negative half to 0, and lands on the edge past chmax. stated hypothesis-
; --- free -- (nmin chmax n) is a net that fits and (add chmax n) one that does
; --- not -- so nothing rests on a decision procedure for the bound ---
(assert
 (defq uv-nmin-idem (pi a nat (pi b nat (paths nat (nmin a (nmin a b)) (nmin a b))))
   (lam a (nat_rect (lam q (pi b nat (paths nat (nmin q (nmin q b)) (nmin q b))))
             (lam b (idpath 0))
             (lam j (lam IH (lam b
                (nat_rect (lam r (paths nat (nmin (succ j) (nmin (succ j) r))
                                             (nmin (succ j) r)))
                   (idpath 0)
                   (lam k (lam s (maponpaths nat nat (lam z (succ z))
                                    (nmin j (nmin j k)) (nmin j k) (IH k))))
                   b))))
             a)))
 (defq uv-nmin-add (pi a nat (pi n nat (paths nat (nmin a (add a n)) a)))
   (lam a (nat_rect (lam q (pi n nat (paths nat (nmin q (add q n)) q)))
             (lam n (idpath 0))
             (lam j (lam IH (lam n (maponpaths nat nat (lam z (succ z))
                                      (nmin j (add j n)) j (IH n)))))
             a)))
 ; $ keeps what fits
 (defq uv-satc-keeps
   (pi n nat (paths nat (zsatc (nzed (nmin chmax n))) (nmin chmax n)))
   (lam n (uv-nmin-idem chmax n)))
 ; ..lands on the edge past it
 (defq uv-satc-ceils (pi n nat (paths nat (zsatc (nzed (add chmax n))) chmax))
   (lam n (uv-nmin-add chmax n)))
 ; ..and takes the whole negative half to 0
 (defq uv-satc-clamps (pi n nat (paths nat (zsatc (zneg n)) 0))
   (lam n (idpath 0)))
 ; the agreement: below the ceiling the clamped $ is the tower's
 (defq uv-satc-below (pi v val (paths nat (vsatc v) (nmin chmax (vsat v))))
   (lam v (idpath (nmin chmax (vsat v))))))

; --- net is a homomorphism: a leaf nets its measure at whatever height it sits
; --- at, and a link sums ---
(assert
 (defq uv-net-leaf
   (pi d nat (pi m atm (paths msr (netd (succ d) (ii1 m)) (anet m))))
   (lam d (lam m (idpath (anet m)))))
 (defq uv-net-link
   (pi d nat (pi a (vtre d) (pi b (vtre d)
     (paths msr (netd (succ d) (ii2 (tpair a b))) (madd (netd d a) (netd d b))))))
   (lam d (lam a (lam b (idpath (madd (netd d a) (netd d b))))))))

; --- structural equality. the depth is a height bound, so one value has many
; --- spellings -- a leaf sits at any depth, a link pads its shallow limb -- and
; --- `paths` is finer than love's `=`. veq compares what a node observes: is it
; --- a pair, its cap, its cup, its measure. the recursion is on the left value's
; --- index alone and the right rides along, read one observation at a time --
; --- which is what lets three of these compose, where a fuel derived from a pair
; --- of heights gives the three sides of a transitivity three different ones.
; --- first the measure's own equality, and the andb plumbing the proofs lean on ---
(assert
 (defn beqb (pi a bool (pi b bool bool))
   (lam a (lam b (bool_rect (lam x bool) b (negb b) a))))
 (defn zeqb (pi a zed (pi b zed bool))
   (lam a (lam b (coprod_rect (lam w bool)
                    (lam p (coprod_rect (lam w bool) (lam q (nateqb p q)) (lam q false) b))
                    (lam p (coprod_rect (lam w bool) (lam q false) (lam q (nateqb p q)) b))
                    a))))
 (defn reqb (pi a rea (pi b rea bool))
   (lam a (lam b (andb (zeqb (pr1 a) (pr1 b)) (beqb (pr2 a) (pr2 b))))))
 (defn meqb (pi a msr (pi b msr bool))
   (lam a (lam b (andb (reqb (pr1 a) (pr1 b)) (reqb (pr2 a) (pr2 b))))))
 ; the leaf's equality reads the tag first: () meets only (), and that is the
 ; one place love's = is finer than the measure it would otherwise be read by
 (defn aeqb (pi a atm (pi b atm bool))
   (lam a (lam b (coprod_rect (lam w bool)
                    (lam u (coprod_rect (lam w bool) (lam v true) (lam m false) b))
                    (lam m (coprod_rect (lam w bool) (lam v false) (lam n (meqb m n)) b))
                    a))))
 (defq uv-andb2
   (pi a bool (pi b bool (pi p (paths bool a true) (pi q (paths bool b true)
      (paths bool (andb a b) true)))))
   (lam a (lam b (lam p (lam q
      (pathscomp0 bool (andb a b) (andb true b) true
         (maponpaths bool bool (lam z (andb z b)) a true p)
         (maponpaths bool bool (lam z (andb true z)) b true q)))))))
 (defq uv-orbl (pi a bool (pi b bool (pi p (paths bool a true) (paths bool (orb a b) true))))
   (lam a (lam b (lam p (maponpaths bool bool (lam z (orb z b)) a true p)))))
 (defq uv-orbr (pi a bool (pi b bool (pi q (paths bool b true) (paths bool (orb a b) true))))
   (lam a (lam b (lam q (bool_rect (lam x (paths bool (orb x b) true)) (idpath true) q a)))))
 ; reflexivity, rung by rung. nateqb (succ k) (succ k) is nateqb k k, so the
 ; induction step is its own hypothesis
 (defq uv-nateqb-refl (pi n nat (paths bool (nateqb n n) true))
   (lam n (nat_rect (lam q (paths bool (nateqb q q) true))
             (idpath true) (lam k (lam r r)) n)))
 (defq uv-beqb-refl (pi b bool (paths bool (beqb b b) true))
   (lam b (bool_rect (lam x (paths bool (beqb x x) true)) (idpath true) (idpath true) b)))
 (defq uv-zeqb-refl (pi z zed (paths bool (zeqb z z) true))
   (lam z (coprod_rect (lam w (paths bool (zeqb w w) true))
             (lam p (uv-nateqb-refl p)) (lam p (uv-nateqb-refl p)) z)))
 (defq uv-reqb-refl (pi r rea (paths bool (reqb r r) true))
   (lam r (uv-andb2 (zeqb (pr1 r) (pr1 r)) (beqb (pr2 r) (pr2 r))
             (uv-zeqb-refl (pr1 r)) (uv-beqb-refl (pr2 r)))))
 (defq uv-meqb-refl (pi m msr (paths bool (meqb m m) true))
   (lam m (uv-andb2 (reqb (pr1 m) (pr1 m)) (reqb (pr2 m) (pr2 m))
             (uv-reqb-refl (pr1 m)) (uv-reqb-refl (pr2 m)))))
 (defq uv-aeqb-refl (pi a atm (paths bool (aeqb a a) true))
   (lam a (coprod_rect (lam w (paths bool (aeqb w w) true))
             (lam u (idpath true)) (lam m (uv-meqb-refl m)) a))))

; --- ..and each of them reflects: an equality that answers true hands back the
; --- path. reflexivity alone would give a preorder; this is what makes = an
; --- equivalence, and every rung has to give it up before the tree's can ---
(assert
 (defq uv-pairpath
   (pi A UU (pi B UU (pi a1 A (pi a2 A (pi b1 B (pi b2 B
      (pi p (paths A a1 a2) (pi q (paths B b1 b2)
         (paths (total2 x A B) (tpair a1 b1) (tpair a2 b2))))))))))
   (lam A (lam B (lam a1 (lam a2 (lam b1 (lam b2 (lam p (lam q
      (pathscomp0 (total2 x A B) (tpair a1 b1) (tpair a2 b1) (tpair a2 b2)
         (maponpaths A (total2 x A B) (lam w (tpair w b1)) a1 a2 p)
         (maponpaths B (total2 x A B) (lam w (tpair a2 w)) b1 b2 q)))))))))))
 (defq uv-nateqb-paths
   (pi n nat (pi m nat (pi h (paths bool (nateqb n m) true) (paths nat n m))))
   (lam n (nat_rect
             (lam q (pi m nat (pi h (paths bool (nateqb q m) true) (paths nat q m))))
             (lam m (nat_rect (lam r (pi h (paths bool (nateqb 0 r) true) (paths nat 0 r)))
                       (lam h (idpath 0))
                       (lam k (lam s (lam h (fromempty (paths nat 0 (succ k))
                                               (nopathsfalsetotrue h)))))
                       m))
             (lam j (lam IH (lam m
                (nat_rect (lam r (pi h (paths bool (nateqb (succ j) r) true)
                                    (paths nat (succ j) r)))
                   (lam h (fromempty (paths nat (succ j) 0) (nopathsfalsetotrue h)))
                   (lam k (lam s (lam h (maponpaths nat nat (lam x (succ x)) j k (IH k h)))))
                   m))))
             n)))
 (defq uv-beqb-paths
   (pi a bool (pi b bool (pi h (paths bool (beqb a b) true) (paths bool a b))))
   (lam a (lam b (bool_rect (lam x (pi h (paths bool (beqb x b) true) (paths bool x b)))
                    (lam h (pathsinv0 bool b true h))
                    (lam h ((bool_rect (lam y (pi g (paths bool (negb y) true)
                                                 (paths bool false y)))
                               (lam g (fromempty (paths bool false true)
                                         (nopathsfalsetotrue g)))
                               (lam g (idpath false))
                               b) h))
                    a))))
 (defq uv-zeqb-paths
   (pi a zed (pi b zed (pi h (paths bool (zeqb a b) true) (paths zed a b))))
   (lam a (lam b
      (coprod_rect (lam v (pi h (paths bool (zeqb v b) true) (paths zed v b)))
         (lam p (coprod_rect
                   (lam v (pi h (paths bool (zeqb (ii1 p) v) true) (paths zed (ii1 p) v)))
                   (lam q (lam h (maponpaths nat zed (lam x (ii1 x)) p q
                                    (uv-nateqb-paths p q h))))
                   (lam q (lam h (fromempty (paths zed (ii1 p) (ii2 q))
                                    (nopathsfalsetotrue h))))
                   b))
         (lam p (coprod_rect
                   (lam v (pi h (paths bool (zeqb (ii2 p) v) true) (paths zed (ii2 p) v)))
                   (lam q (lam h (fromempty (paths zed (ii2 p) (ii1 q))
                                    (nopathsfalsetotrue h))))
                   (lam q (lam h (maponpaths nat zed (lam x (ii2 x)) p q
                                    (uv-nateqb-paths p q h))))
                   b))
         a))))
 (defq uv-reqb-paths
   (pi a rea (pi b rea (pi h (paths bool (reqb a b) true) (paths rea a b))))
   (lam a (lam b (lam h
      (uv-pairpath zed bool (pr1 a) (pr1 b) (pr2 a) (pr2 b)
         (uv-zeqb-paths (pr1 a) (pr1 b)
            (andbtruel (zeqb (pr1 a) (pr1 b)) (beqb (pr2 a) (pr2 b)) h))
         (uv-beqb-paths (pr2 a) (pr2 b)
            (andbtruer (zeqb (pr1 a) (pr1 b)) (beqb (pr2 a) (pr2 b)) h)))))))
 (defq uv-meqb-paths
   (pi a msr (pi b msr (pi h (paths bool (meqb a b) true) (paths msr a b))))
   (lam a (lam b (lam h
      (uv-pairpath rea rea (pr1 a) (pr1 b) (pr2 a) (pr2 b)
         (uv-reqb-paths (pr1 a) (pr1 b)
            (andbtruel (reqb (pr1 a) (pr1 b)) (reqb (pr2 a) (pr2 b)) h))
         (uv-reqb-paths (pr2 a) (pr2 b)
            (andbtruer (reqb (pr1 a) (pr1 b)) (reqb (pr2 a) (pr2 b)) h)))))))
 (defq uv-aeqb-paths
   (pi a atm (pi b atm (pi h (paths bool (aeqb a b) true) (paths atm a b))))
   (lam a (lam b
      (coprod_rect (lam v (pi h (paths bool (aeqb v b) true) (paths atm v b)))
         (lam u (coprod_rect
                   (lam v (pi h (paths bool (aeqb (ii1 u) v) true) (paths atm (ii1 u) v)))
                   (lam x (lam h (maponpaths unit atm (lam y (ii1 y)) u x
                                    (pathscomp0 unit u tt x (uu-unitpath u)
                                       (pathsinv0 unit x tt (uu-unitpath x))))))
                   (lam m (lam h (fromempty (paths atm (ii1 u) (ii2 m))
                                    (nopathsfalsetotrue h))))
                   b))
         (lam m (coprod_rect
                   (lam v (pi h (paths bool (aeqb (ii2 m) v) true) (paths atm (ii2 m) v)))
                   (lam x (lam h (fromempty (paths atm (ii2 m) (ii1 x))
                                    (nopathsfalsetotrue h))))
                   (lam n (lam h (maponpaths msr atm (lam y (ii2 y)) m n
                                    (uv-meqb-paths m n h))))
                   b))
         a))))
 ; a path back into an answer, and the two composed: the leaf's equality is an
 ; equivalence, which is the whole of what the tree's transitivity leans on
 (defq uv-aeqb-of
   (pi a atm (pi b atm (pi p (paths atm a b) (paths bool (aeqb a b) true))))
   (lam a (lam b (lam p
      (pathscomp0 bool (aeqb a b) (aeqb a a) true
         (pathsinv0 bool (aeqb a a) (aeqb a b)
            (maponpaths atm bool (lam x (aeqb a x)) a b p))
         (uv-aeqb-refl a))))))
 (defq uv-aeqb-trans
   (pi a atm (pi b atm (pi c atm (pi h1 (paths bool (aeqb a b) true)
      (pi h2 (paths bool (aeqb b c) true) (paths bool (aeqb a c) true))))))
   (lam a (lam b (lam c (lam h1 (lam h2
      (uv-aeqb-of a c (pathscomp0 atm a b c (uv-aeqb-paths a b h1)
                                            (uv-aeqb-paths b c h2))))))))))

; --- the four things a node shows: whether it is a pair, its cap, its cup, and
; --- the measure it carries when it is not. none of them recurse -- a leaf is
; --- ii1 at every depth, so the head is read in one step ---
(assert
 (defn vtwod (pi d nat (pi t (vtre d) bool))
   (lam d (nat_rect (lam q (pi t (vtre q) bool))
             (lam t false)
             (lam k (lam IH (lam t (coprod_rect (lam w bool) (lam m false) (lam p true) t))))
             d)))
 (defn vleafd (pi d nat (pi t (vtre d) atm))
   (lam d (nat_rect (lam q (pi t (vtre q) atm))
             (lam t t)
             (lam k (lam IH (lam t (coprod_rect (lam w atm) (lam m m) (lam p aunit) t))))
             d)))
 (defn vcapd (pi d nat (pi t (vtre d) val))
   (lam d (nat_rect (lam q (pi t (vtre q) val))
             (lam t (tpair 0 t))                            ; an atom is its own cap
             (lam k (lam IH (lam t (coprod_rect (lam w val)
                                      (lam m (tpair 0 m)) (lam p (tpair k (vfst k p))) t))))
             d)))
 (defn vcupd (pi d nat (pi t (vtre d) val))
   (lam d (nat_rect (lam q (pi t (vtre q) val))
             (lam t vnilv)                                  ; ..and its cup is ()
             (lam k (lam IH (lam t (coprod_rect (lam w val)
                                      (lam m vnilv) (lam p (tpair k (vsnd k p))) t))))
             d)))
 (defn vtwo  (pi v val bool) (lam v (vtwod (pr1 v) (pr2 v))))
 (defn vleaf (pi v val atm)  (lam v (vleafd (pr1 v) (pr2 v))))
 (defn vcap  (pi v val val)  (lam v (vcapd (pr1 v) (pr2 v))))
 (defn vcup  (pi v val val)  (lam v (vcupd (pr1 v) (pr2 v))))
 ; a leaf answers the right value's head and its atom; a pair asks the right for
 ; a cap and a cup and descends. the index falls by one each step and the right
 ; value never has to be indexed at all
 (defn veqd (pi d nat (pi t (vtre d) (pi w val bool)))
   (lam d (nat_rect (lam q (pi t (vtre q) (pi w val bool)))
             (lam t (lam w (andb (negb (vtwo w)) (aeqb t (vleaf w)))))
             (lam k (lam IH (lam t (lam w
                (coprod_rect (lam z bool)
                   (lam a (andb (negb (vtwo w)) (aeqb a (vleaf w))))
                   (lam p (andb (vtwo w) (andb (IH (vfst k p) (vcap w))
                                               (IH (vsnd k p) (vcup w)))))
                   t)))))
             d)))
 (defn veq (pi u val (pi w val bool)) (lam u (lam w (veqd (pr1 u) (pr2 u) w)))))

; --- link, and the padding it costs. two values meet at the taller height, so
; --- the shallow limb is raised -- and both limbs are raised structurally, the
; --- recursion following the tree and never stacking levels on top of a neutral
; --- depth. that is what mx2 buys: (mx2 0 b) is b and (mx2 (succ j) 0) is
; --- (succ j) definitionally, so a pad lands on one index from either side.
; --- `add` cannot do it -- (add j 0) is stuck on a neutral j ---
(assert
 (defn mx2 (pi a nat (pi b nat nat))
   (lam a (nat_rect (lam q (pi b nat nat))
             (lam b b)
             (lam j (lam IH (lam b (nat_rect (lam r nat)
                                      (succ j) (lam k (lam s (succ (IH k)))) b))))
             a)))
 (defn leafat (pi d nat (pi m atm (vtre d)))                  ; a leaf, at any height
   (lam d (nat_rect (lam q (pi m atm (vtre q)))
             (lam m m) (lam k (lam IH (lam m (ii1 m)))) d)))
 (defn padl (pi a nat (pi b nat (pi t (vtre a) (vtre (mx2 a b)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre q) (vtre (mx2 q b)))))
             (lam b (lam m (leafat b m)))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre (succ j)) (vtre (mx2 (succ j) r))))
                   (lam t t)
                   (lam k (lam s (lam t
                      (coprod_rect (lam w (vtre (succ (mx2 j k))))
                         (lam m (ii1 m))
                         (lam p (ii2 (tpair (IH k (vfst j p)) (IH k (vsnd j p)))))
                         t))))
                   b))))
             a)))
 (defn padr (pi a nat (pi b nat (pi t (vtre b) (vtre (mx2 a b)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre b) (vtre (mx2 q b)))))
             (lam b (lam t t))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r) (vtre (mx2 (succ j) r))))
                   (lam m (leafat (succ j) m))
                   (lam k (lam s (lam t
                      (coprod_rect (lam w (vtre (succ (mx2 j k))))
                         (lam m (ii1 m))
                         (lam p (ii2 (tpair (IH k (vfst k p)) (IH k (vsnd k p)))))
                         t))))
                   b))))
             a)))
 (defn vlink (pi u val (pi w val val))
   (lam u (lam w (tpair (succ (mx2 (pr1 u) (pr1 w)))
                        (ii2 (tpair (padl (pr1 u) (pr1 w) (pr2 u))
                                    (padr (pr1 u) (pr1 w) (pr2 w)))))))))

; --- veq is reflexive: at a leaf the right value shows no head and the same
; --- atom, at a pair it shows both limbs one level down ---
(assert
 (defq uv-veq-refl (pi v val (paths bool (veq v v) true))
   (lam v ((nat_rect (lam d (pi t (vtre d) (paths bool (veqd d t (tpair d t)) true)))
             (lam t (uv-aeqb-refl t))
             (lam k (lam IH (lam t
                (coprod_rect
                   (lam z (paths bool (veqd (succ k) z (tpair (succ k) z)) true))
                   (lam a (uv-aeqb-refl a))
                   (lam p (uv-andb2 (veqd k (vfst k p) (tpair k (vfst k p)))
                                    (veqd k (vsnd k p) (tpair k (vsnd k p)))
                                    (IH (vfst k p)) (IH (vsnd k p))))
                   t))))
             (pr1 v)) (pr2 v)))))

; --- ..and transitive, which reflexivity alone would not give: with the fuel
; --- gone the three sides of a composition recurse on their own left index and
; --- the middle value is just a value, so the induction is the plain one. what
; --- it costs is that the middle's shape has to be read -- veqd only reduces on
; --- a head -- so each arm cases on the middle's index and tree, and the two
; --- arms where the middle shows a head the left does not are ex falso ---
(assert
 ; the leaf case, shared by the base and by the step's leaf arm: the middle must
 ; show no head, and its atom carries the equality across
 (defq uv-leaf-trans
   (pi a atm (pi e nat (pi s (vtre e) (pi w val
      (pi h1 (paths bool (andb (negb (vtwo (tpair e s))) (aeqb a (vleaf (tpair e s)))) true)
      (pi h2 (paths bool (veqd e s w) true)
         (paths bool (andb (negb (vtwo w)) (aeqb a (vleaf w))) true)))))))
   (lam a (lam e (nat_rect
      (lam qe (pi s (vtre qe) (pi w val
         (pi h1 (paths bool
                   (andb (negb (vtwo (tpair qe s))) (aeqb a (vleaf (tpair qe s)))) true)
         (pi h2 (paths bool (veqd qe s w) true)
            (paths bool (andb (negb (vtwo w)) (aeqb a (vleaf w))) true))))))
      (lam s (lam w (lam h1 (lam h2
         (uv-andb2 (negb (vtwo w)) (aeqb a (vleaf w))
            (andbtruel (negb (vtwo w)) (aeqb s (vleaf w)) h2)
            (uv-aeqb-trans a s (vleaf w) h1
               (andbtruer (negb (vtwo w)) (aeqb s (vleaf w)) h2)))))))
      (lam j (lam IHe (lam s
         (coprod_rect
            (lam z (pi w val
               (pi h1 (paths bool (andb (negb (vtwo (tpair (succ j) z)))
                                        (aeqb a (vleaf (tpair (succ j) z)))) true)
               (pi h2 (paths bool (veqd (succ j) z w) true)
                  (paths bool (andb (negb (vtwo w)) (aeqb a (vleaf w))) true)))))
            (lam x (lam w (lam h1 (lam h2
               (uv-andb2 (negb (vtwo w)) (aeqb a (vleaf w))
                  (andbtruel (negb (vtwo w)) (aeqb x (vleaf w)) h2)
                  (uv-aeqb-trans a x (vleaf w) h1
                     (andbtruer (negb (vtwo w)) (aeqb x (vleaf w)) h2)))))))
            (lam pp (lam w (lam h1 (lam h2
               (fromempty (paths bool (andb (negb (vtwo w)) (aeqb a (vleaf w))) true)
                  (nopathsfalsetotrue h1))))))
            s))))
      e))))
 (defq uv-veqd-trans
   (pi d nat (pi t (vtre d) (pi e nat (pi s (vtre e) (pi w val
      (pi h1 (paths bool (veqd d t (tpair e s)) true)
      (pi h2 (paths bool (veqd e s w) true)
         (paths bool (veqd d t w) true))))))))
   (lam d (nat_rect
      (lam qd (pi t (vtre qd) (pi e nat (pi s (vtre e) (pi w val
         (pi h1 (paths bool (veqd qd t (tpair e s)) true)
         (pi h2 (paths bool (veqd e s w) true)
            (paths bool (veqd qd t w) true))))))))
      (lam t (uv-leaf-trans t))
      (lam k (lam IH (lam t
         (coprod_rect
            (lam z (pi e nat (pi s (vtre e) (pi w val
               (pi h1 (paths bool (veqd (succ k) z (tpair e s)) true)
               (pi h2 (paths bool (veqd e s w) true)
                  (paths bool (veqd (succ k) z w) true)))))))
            (lam x (uv-leaf-trans x))
            (lam p (lam e (nat_rect
               (lam qe (pi s (vtre qe) (pi w val
                  (pi h1 (paths bool (veqd (succ k) (ii2 p) (tpair qe s)) true)
                  (pi h2 (paths bool (veqd qe s w) true)
                     (paths bool (veqd (succ k) (ii2 p) w) true))))))
               ; the middle is a leaf where the left is a pair: nothing to carry
               (lam s (lam w (lam h1 (lam h2
                  (fromempty (paths bool (veqd (succ k) (ii2 p) w) true)
                     (nopathsfalsetotrue h1))))))
               (lam j (lam IHe (lam s
                  (coprod_rect
                     (lam z (pi w val
                        (pi h1 (paths bool
                                  (veqd (succ k) (ii2 p) (tpair (succ j) z)) true)
                        (pi h2 (paths bool (veqd (succ j) z w) true)
                           (paths bool (veqd (succ k) (ii2 p) w) true)))))
                     (lam x (lam w (lam h1 (lam h2
                        (fromempty (paths bool (veqd (succ k) (ii2 p) w) true)
                           (nopathsfalsetotrue h1))))))
                     ; all three are pairs: the cap and the cup compose, each by
                     ; the hypothesis at one level down
                     (lam q (lam w (lam h1 (lam h2
                        (uv-andb2 (vtwo w)
                           (andb (veqd k (vfst k p) (vcap w))
                                 (veqd k (vsnd k p) (vcup w)))
                           (andbtruel (vtwo w)
                              (andb (veqd j (vfst j q) (vcap w))
                                    (veqd j (vsnd j q) (vcup w))) h2)
                           (uv-andb2 (veqd k (vfst k p) (vcap w))
                                     (veqd k (vsnd k p) (vcup w))
                              (IH (vfst k p) j (vfst j q) (vcap w)
                                 (andbtruel (veqd k (vfst k p) (tpair j (vfst j q)))
                                            (veqd k (vsnd k p) (tpair j (vsnd j q))) h1)
                                 (andbtruel (veqd j (vfst j q) (vcap w))
                                            (veqd j (vsnd j q) (vcup w))
                                    (andbtruer (vtwo w)
                                       (andb (veqd j (vfst j q) (vcap w))
                                             (veqd j (vsnd j q) (vcup w))) h2)))
                              (IH (vsnd k p) j (vsnd j q) (vcup w)
                                 (andbtruer (veqd k (vfst k p) (tpair j (vfst j q)))
                                            (veqd k (vsnd k p) (tpair j (vsnd j q))) h1)
                                 (andbtruer (veqd j (vfst j q) (vcap w))
                                            (veqd j (vsnd j q) (vcup w))
                                    (andbtruer (vtwo w)
                                       (andb (veqd j (vfst j q) (vcap w))
                                             (veqd j (vsnd j q) (vcup w))) h2)))))))))
                     s))))
               e)))
            t))))
      d)))
 (defq uv-veq-trans
   (pi u val (pi v val (pi w val (pi h1 (paths bool (veq u v) true)
      (pi h2 (paths bool (veq v w) true) (paths bool (veq u w) true))))))
   (lam u (lam v (lam w (uv-veqd-trans (pr1 u) (pr2 u) (pr1 v) (pr2 v) w))))))

; --- the padding is invisible. this is the theorem the depth index owes: a
; --- value and its padded self answer veq alike, so the height a link meets at
; --- is bookkeeping and nothing observable rests on it. the induction is on the
; --- pad's own index a -- what makes mx2 and padl reduce -- and the case on b
; --- and on the tree is what makes veqd reduce beside it. both argument orders,
; --- since a law names its two sides in its own order ---
(assert
 (defq uv-padl-fwd
   (pi a nat (pi b nat (pi t (vtre a)
      (paths bool (veqd a t (tpair (mx2 a b) (padl a b t))) true))))
   (lam a (nat_rect
              (lam q (pi b nat (pi t (vtre q)
                        (paths bool (veqd q t (tpair (mx2 q b) (padl q b t))) true))))
              (lam b (lam t
                 (nat_rect (lam r (paths bool
                              (veqd 0 t (tpair (mx2 0 r) (padl 0 r t))) true))
                    (uv-aeqb-refl t)
                    (lam k (lam s (uv-aeqb-refl t)))
                    b)))
              (lam j (lam IH (lam b
                 (nat_rect (lam r (pi t (vtre (succ j))
                              (paths bool (veqd (succ j) t
                                 (tpair (mx2 (succ j) r) (padl (succ j) r t))) true)))
                    (lam t (uv-veq-refl (tpair (succ j) t)))
                    (lam k (lam s (lam t
                       (coprod_rect (lam w (paths bool
                                      (veqd (succ j) w
                                        (tpair (succ (mx2 j k)) (padl (succ j) (succ k) w)))
                                      true))
                          (lam mm (uv-aeqb-refl mm))
                          (lam p (uv-andb2
                                    (veqd j (vfst j p) (tpair (mx2 j k) (padl j k (vfst j p))))
                                    (veqd j (vsnd j p) (tpair (mx2 j k) (padl j k (vsnd j p))))
                                    (IH k (vfst j p)) (IH k (vsnd j p))))
                          t))))
                    b))))
              a)))
 (defq uv-padl-bwd
   (pi a nat (pi b nat (pi t (vtre a)
      (paths bool (veqd (mx2 a b) (padl a b t) (tpair a t)) true))))
   (lam a (nat_rect
              (lam q (pi b nat (pi t (vtre q)
                        (paths bool (veqd (mx2 q b) (padl q b t) (tpair q t)) true))))
              (lam b (lam t
                 (nat_rect (lam r (paths bool
                              (veqd (mx2 0 r) (padl 0 r t) (tpair 0 t)) true))
                    (uv-aeqb-refl t)
                    (lam k (lam s (uv-aeqb-refl t)))
                    b)))
              (lam j (lam IH (lam b
                 (nat_rect (lam r (pi t (vtre (succ j))
                              (paths bool (veqd (mx2 (succ j) r) (padl (succ j) r t)
                                                (tpair (succ j) t)) true)))
                    (lam t (uv-veq-refl (tpair (succ j) t)))
                    (lam k (lam s (lam t
                       (coprod_rect (lam w (paths bool
                                      (veqd (succ (mx2 j k)) (padl (succ j) (succ k) w)
                                            (tpair (succ j) w))
                                      true))
                          (lam mm (uv-aeqb-refl mm))
                          (lam p (uv-andb2
                                    (veqd (mx2 j k) (padl j k (vfst j p)) (tpair j (vfst j p)))
                                    (veqd (mx2 j k) (padl j k (vsnd j p)) (tpair j (vsnd j p)))
                                    (IH k (vfst j p)) (IH k (vsnd j p))))
                          t))))
                    b))))
              a)))
 ; the right pad's a=0 corner is where mx2 earns its keep: (mx2 0 b) is b and
 ; (padr 0 b t) is t, so the whole corner is one reflexivity
 (defq uv-padr-fwd
   (pi a nat (pi b nat (pi t (vtre b)
      (paths bool (veqd b t (tpair (mx2 a b) (padr a b t))) true))))
   (lam a (nat_rect
             (lam q (pi b nat (pi t (vtre b)
                       (paths bool (veqd b t (tpair (mx2 q b) (padr q b t))) true))))
             (lam b (lam t (uv-veq-refl (tpair b t))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r)
                             (paths bool (veqd r t
                                (tpair (mx2 (succ j) r) (padr (succ j) r t))) true)))
                   (lam t (uv-aeqb-refl t))
                   (lam k (lam s (lam t
                      (coprod_rect (lam w (paths bool
                                     (veqd (succ k) w
                                       (tpair (succ (mx2 j k)) (padr (succ j) (succ k) w)))
                                     true))
                         (lam mm (uv-aeqb-refl mm))
                         (lam p (uv-andb2
                                   (veqd k (vfst k p) (tpair (mx2 j k) (padr j k (vfst k p))))
                                   (veqd k (vsnd k p) (tpair (mx2 j k) (padr j k (vsnd k p))))
                                   (IH k (vfst k p)) (IH k (vsnd k p))))
                         t))))
                   b))))
             a)))
 (defq uv-padr-bwd
   (pi a nat (pi b nat (pi t (vtre b)
      (paths bool (veqd (mx2 a b) (padr a b t) (tpair b t)) true))))
   (lam a (nat_rect
             (lam q (pi b nat (pi t (vtre b)
                       (paths bool (veqd (mx2 q b) (padr q b t) (tpair b t)) true))))
             (lam b (lam t (uv-veq-refl (tpair b t))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r)
                             (paths bool (veqd (mx2 (succ j) r) (padr (succ j) r t)
                                               (tpair r t)) true)))
                   (lam t (uv-aeqb-refl t))
                   (lam k (lam s (lam t
                      (coprod_rect (lam w (paths bool
                                     (veqd (succ (mx2 j k)) (padr (succ j) (succ k) w)
                                           (tpair (succ k) w))
                                     true))
                         (lam mm (uv-aeqb-refl mm))
                         (lam p (uv-andb2
                                   (veqd (mx2 j k) (padr j k (vfst k p)) (tpair k (vfst k p)))
                                   (veqd (mx2 j k) (padr j k (vsnd k p)) (tpair k (vsnd k p)))
                                   (IH k (vfst k p)) (IH k (vsnd k p))))
                         t))))
                   b))))
             a))))

; --- the congruence, which is what transitivity was for: two links whose limbs
; --- answer alike answer alike. each limb meets the other at a height neither
; --- spells, so the proof goes down through the pad, across by the hypothesis,
; --- and back up the other pad -- three steps that only compose because veq
; --- composes ---
(assert
 (defq uv-veq-of (pi u val (pi w val (pi p (paths val u w) (paths bool (veq u w) true))))
   (lam u (lam w (lam p
      (pathscomp0 bool (veq u w) (veq u u) true
         (pathsinv0 bool (veq u u) (veq u w)
            (maponpaths val bool (lam x (veq u x)) u w p))
         (uv-veq-refl u))))))
 (defq uv-veq-link
   (pi a val (pi b val (pi c val (pi d val
      (pi ha (paths bool (veq a c) true)
      (pi hb (paths bool (veq b d) true)
         (paths bool (veq (vlink a b) (vlink c d)) true)))))))
   (lam a (lam b (lam c (lam d (lam ha (lam hb
      (uv-andb2 (vtwo (vlink c d))
         (andb (veqd (mx2 (pr1 a) (pr1 b)) (padl (pr1 a) (pr1 b) (pr2 a))
                     (vcap (vlink c d)))
               (veqd (mx2 (pr1 a) (pr1 b)) (padr (pr1 a) (pr1 b) (pr2 b))
                     (vcup (vlink c d))))
         (idpath true)
         (uv-andb2
            (veqd (mx2 (pr1 a) (pr1 b)) (padl (pr1 a) (pr1 b) (pr2 a)) (vcap (vlink c d)))
            (veqd (mx2 (pr1 a) (pr1 b)) (padr (pr1 a) (pr1 b) (pr2 b)) (vcup (vlink c d)))
            (uv-veqd-trans (mx2 (pr1 a) (pr1 b)) (padl (pr1 a) (pr1 b) (pr2 a))
               (pr1 c) (pr2 c)
               (tpair (mx2 (pr1 c) (pr1 d)) (padl (pr1 c) (pr1 d) (pr2 c)))
               (uv-veqd-trans (mx2 (pr1 a) (pr1 b)) (padl (pr1 a) (pr1 b) (pr2 a))
                  (pr1 a) (pr2 a) c (uv-padl-bwd (pr1 a) (pr1 b) (pr2 a)) ha)
               (uv-padl-fwd (pr1 c) (pr1 d) (pr2 c)))
            (uv-veqd-trans (mx2 (pr1 a) (pr1 b)) (padr (pr1 a) (pr1 b) (pr2 b))
               (pr1 d) (pr2 d)
               (tpair (mx2 (pr1 c) (pr1 d)) (padr (pr1 c) (pr1 d) (pr2 d)))
               (uv-veqd-trans (mx2 (pr1 a) (pr1 b)) (padr (pr1 a) (pr1 b) (pr2 b))
                  (pr1 b) (pr2 b) d (uv-padr-bwd (pr1 a) (pr1 b) (pr2 b)) hb)
               (uv-padr-fwd (pr1 c) (pr1 d) (pr2 d)))))))))))))

; --- the pinned carrier, and the bridge to veq. everything above reads the
; --- depth as a bound, which is why the equality is veq and not paths: one
; --- value has many spellings and paths tells them apart. pin the index to the
; --- height a tree actually has and the spelling is unique -- within one index
; --- it always was, a node showing a leaf tag or a pair with nothing to choose,
; --- so the whole multiplicity was the choice of d. what that buys is `paths`
; --- as the equality, and paths is a congruence for every function for free.
; --- the tightness is a path in nat, hence a proposition, hence invisible to
; --- equality; the tree family, mx2 and both pads carry over unchanged ---
(assert
 (defn pht (pi d nat (pi t (vtre d) nat))
   (lam d (nat_rect (lam q (pi t (vtre q) nat))
             (lam t 0)
             (lam k (lam IH (lam t (coprod_rect (lam w nat)
                                      (lam a 0)
                                      (lam p (succ (mx2 (IH (vfst k p)) (IH (vsnd k p)))))
                                      t))))
             d)))
 (defn ptight (pi p val UU) (lam p (paths nat (pht (pr1 p) (pr2 p)) (pr1 p))))
 (defn pval UU (total2 p val (ptight p)))
 (defn pleaf (pi a atm pval) (lam a (tpair (tpair 0 a) (idpath 0))))
 (defn pnum (pi m msr pval) (lam m (pleaf (anum m))))
 (defn pnilv pval (pleaf aunit))
 (defn pnet (pi x pval msr) (lam x (vnet (pr1 x)))))

; --- what the pin costs: the pad must not move the height. two inductions with
; --- the pads' own shape, and a two-argument congruence for the max ---
(assert
 (defq uv-mx2-cong
   (pi a nat (pi a2 nat (pi b nat (pi b2 nat
      (pi p (paths nat a a2) (pi q (paths nat b b2)
         (paths nat (mx2 a b) (mx2 a2 b2))))))))
   (lam a (lam a2 (lam b (lam b2 (lam p (lam q
      (pathscomp0 nat (mx2 a b) (mx2 a2 b) (mx2 a2 b2)
         (maponpaths nat nat (lam w (mx2 w b)) a a2 p)
         (maponpaths nat nat (lam w (mx2 a2 w)) b b2 q)))))))))
 (defq uv-pht-leafat (pi d nat (pi m atm (paths nat (pht d (leafat d m)) 0)))
   (lam d (nat_rect (lam q (pi m atm (paths nat (pht q (leafat q m)) 0)))
             (lam m (idpath 0))
             (lam k (lam IH (lam m (idpath 0))))
             d)))
 (defq uv-pht-padl
   (pi a nat (pi b nat (pi t (vtre a)
      (paths nat (pht (mx2 a b) (padl a b t)) (pht a t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre q)
                              (paths nat (pht (mx2 q b) (padl q b t)) (pht q t)))))
             (lam b (lam m (uv-pht-leafat b m)))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre (succ j))
                             (paths nat (pht (mx2 (succ j) r) (padl (succ j) r t))
                                        (pht (succ j) t))))
                   (lam t (idpath (pht (succ j) t)))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths nat (pht (succ (mx2 j k)) (padl (succ j) (succ k) w))
                                           (pht (succ j) w)))
                         (lam m (idpath 0))
                         (lam p ((maponpaths nat nat (lam x (succ x))
                                   (mx2 (pht (mx2 j k) (padl j k (vfst j p)))
                                        (pht (mx2 j k) (padl j k (vsnd j p))))
                                   (mx2 (pht j (vfst j p)) (pht j (vsnd j p))))
                                 (uv-mx2-cong
                                    (pht (mx2 j k) (padl j k (vfst j p))) (pht j (vfst j p))
                                    (pht (mx2 j k) (padl j k (vsnd j p))) (pht j (vsnd j p))
                                    (IH k (vfst j p)) (IH k (vsnd j p)))))
                         t))))
                   b))))
             a)))
 (defq uv-pht-padr
   (pi a nat (pi b nat (pi t (vtre b)
      (paths nat (pht (mx2 a b) (padr a b t)) (pht b t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre b)
                              (paths nat (pht (mx2 q b) (padr q b t)) (pht b t)))))
             (lam b (lam t (idpath (pht b t))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r)
                             (paths nat (pht (mx2 (succ j) r) (padr (succ j) r t))
                                        (pht r t))))
                   (lam m (uv-pht-leafat (succ j) m))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths nat (pht (succ (mx2 j k)) (padr (succ j) (succ k) w))
                                           (pht (succ k) w)))
                         (lam m (idpath 0))
                         (lam p ((maponpaths nat nat (lam x (succ x))
                                   (mx2 (pht (mx2 j k) (padr j k (vfst k p)))
                                        (pht (mx2 j k) (padr j k (vsnd k p))))
                                   (mx2 (pht k (vfst k p)) (pht k (vsnd k p))))
                                 (uv-mx2-cong
                                    (pht (mx2 j k) (padr j k (vfst k p))) (pht k (vfst k p))
                                    (pht (mx2 j k) (padr j k (vsnd k p))) (pht k (vsnd k p))
                                    (IH k (vfst k p)) (IH k (vsnd k p)))))
                         t))))
                   b))))
             a))))

; --- and so the link, carrying its own tightness. this is the same vlink one
; --- level down -- the pinned link is a wrapper, not a second construction ---
(assert
 (defq uv-ptight-link
   (pi a nat (pi b nat (pi t (vtre a) (pi s (vtre b)
      (pi h1 (paths nat (pht a t) a) (pi h2 (paths nat (pht b s) b)
         (paths nat (pht (succ (mx2 a b)) (ii2 (tpair (padl a b t) (padr a b s))))
                    (succ (mx2 a b)))))))))
   (lam a (lam b (lam t (lam s (lam h1 (lam h2
      ((maponpaths nat nat (lam x (succ x))
          (mx2 (pht (mx2 a b) (padl a b t)) (pht (mx2 a b) (padr a b s)))
          (mx2 a b))
       (uv-mx2-cong (pht (mx2 a b) (padl a b t)) a
                    (pht (mx2 a b) (padr a b s)) b
                    (pathscomp0 nat (pht (mx2 a b) (padl a b t)) (pht a t) a
                       (uv-pht-padl a b t) h1)
                    (pathscomp0 nat (pht (mx2 a b) (padr a b s)) (pht b s) b
                       (uv-pht-padr a b s) h2))))))))))
 (defn plink (pi u pval (pi w pval pval))
   (lam u (lam w (tpair (vlink (pr1 u) (pr1 w))
                        (uv-ptight-link (pr1 (pr1 u)) (pr1 (pr1 w))
                                        (pr2 (pr1 u)) (pr2 (pr1 w))
                                        (pr2 u) (pr2 w)))))))

; --- the tightness is a prop, so two pinned values agree as soon as their trees
; --- do -- and that is what makes paths the right equality here. the payoff is
; --- the next two: uv-veq-link is thirty lines down through one pad, across by
; --- the hypothesis and back up the other, standing on transitivity; here the
; --- congruence is two maponpaths, and the same shape gives it for net, and for
; --- any function at all ---
(assert
 (defq uv-propirr (pi X UU (pi i (isaprop X) (pi x X (pi y X (paths X x y)))))
   (lam X (lam i (lam x (lam y (pr1 (i x y)))))))
 (defq uv-ptightirr
   (pi p val (pi h1 (ptight p) (pi h2 (ptight p) (paths (ptight p) h1 h2))))
   (lam p (lam h1 (lam h2
      (uv-propirr (paths nat (pht (pr1 p) (pr2 p)) (pr1 p))
         (isasetnat (pht (pr1 p) (pr2 p)) (pr1 p)) h1 h2)))))
 (defq uv-pvalpath
   (pi u pval (pi w pval (pi e (paths val (pr1 u) (pr1 w)) (paths pval u w))))
   (lam u (lam w (lam e
      (total2_paths_f val ptight u w e
         (uv-ptightirr (pr1 w)
            (transportf val ptight (pr1 u) (pr1 w) e (pr2 u))
            (pr2 w)))))))
 (defq uv-plink-cong
   (pi u pval (pi u2 pval (pi w pval (pi w2 pval
      (pi p (paths pval u u2) (pi q (paths pval w w2)
         (paths pval (plink u w) (plink u2 w2))))))))
   (lam u (lam u2 (lam w (lam w2 (lam p (lam q
      (pathscomp0 pval (plink u w) (plink u2 w) (plink u2 w2)
         (maponpaths pval pval (lam x (plink x w)) u u2 p)
         (maponpaths pval pval (lam x (plink u2 x)) w w2 q)))))))))
 (defq uv-pnet-cong
   (pi u pval (pi w pval (pi p (paths pval u w) (paths msr (pnet u) (pnet w)))))
   (lam u (lam w (lam p (maponpaths pval msr pnet u w p))))))

; --- pin: read a value at its own height. the recursion is through plink
; --- itself, so nothing here has to tighten a tree by hand ---
(assert
 (defn ppin (pi d nat (pi t (vtre d) pval))
   (lam d (nat_rect (lam q (pi t (vtre q) pval))
             (lam t (pleaf t))
             (lam k (lam IH (lam t (coprod_rect (lam w pval)
                                      (lam m (pleaf m))
                                      (lam p (plink (IH (vfst k p)) (IH (vsnd k p))))
                                      t))))
             d)))
 (defn pin (pi v val pval) (lam v (ppin (pr1 v) (pr2 v))))
 (defq uv-negbtrue (pi b bool (pi h (paths bool (negb b) true) (paths bool b false)))
   (lam b (bool_rect (lam x (pi h (paths bool (negb x) true) (paths bool x false)))
             (lam h (fromempty (paths bool true false) (nopathsfalsetotrue h)))
             (lam h (idpath false))
             b)))
 ; a value showing no head pins to its atom, and one showing a head pins to the
 ; link of its limbs -- both by computation, which is what pin is for
 (defq uv-ppin-leaf
   (pi w val (pi h (paths bool (vtwo w) false) (paths pval (pin w) (pleaf (vleaf w)))))
   (lam w ((nat_rect
              (lam q (pi t (vtre q) (pi h (paths bool (vtwo (tpair q t)) false)
                        (paths pval (pin (tpair q t)) (pleaf (vleaf (tpair q t)))))))
              (lam t (lam h (idpath (pleaf t))))
              (lam k (lam IH (lam t
                 (coprod_rect
                    (lam z (pi h (paths bool (vtwo (tpair (succ k) z)) false)
                              (paths pval (pin (tpair (succ k) z))
                                          (pleaf (vleaf (tpair (succ k) z))))))
                    (lam m (lam h (idpath (pleaf m))))
                    (lam p (lam h (fromempty
                              (paths pval (pin (tpair (succ k) (ii2 p)))
                                          (pleaf (vleaf (tpair (succ k) (ii2 p)))))
                              (nopathsfalsetotrue (pathsinv0 bool true false h)))))
                    t))))
              (pr1 w)) (pr2 w))))
 (defq uv-ppin-two
   (pi w val (pi h (paths bool (vtwo w) true)
      (paths pval (pin w) (plink (pin (vcap w)) (pin (vcup w))))))
   (lam w ((nat_rect
              (lam q (pi t (vtre q) (pi h (paths bool (vtwo (tpair q t)) true)
                        (paths pval (pin (tpair q t))
                           (plink (pin (vcap (tpair q t))) (pin (vcup (tpair q t))))))))
              (lam t (lam h (fromempty
                        (paths pval (pin (tpair 0 t))
                           (plink (pin (vcap (tpair 0 t))) (pin (vcup (tpair 0 t)))))
                        (nopathsfalsetotrue h))))
              (lam k (lam IH (lam t
                 (coprod_rect
                    (lam z (pi h (paths bool (vtwo (tpair (succ k) z)) true)
                              (paths pval (pin (tpair (succ k) z))
                                 (plink (pin (vcap (tpair (succ k) z)))
                                        (pin (vcup (tpair (succ k) z)))))))
                    (lam m (lam h (fromempty
                              (paths pval (pin (tpair (succ k) (ii1 m)))
                                 (plink (pin (vcap (tpair (succ k) (ii1 m))))
                                        (pin (vcup (tpair (succ k) (ii1 m))))))
                              (nopathsfalsetotrue h))))
                    (lam p (lam h (idpath (plink (ppin k (vfst k p)) (ppin k (vsnd k p))))))
                    t))))
              (pr1 w)) (pr2 w)))))

; --- the bridge. veq was defined, not derived: nothing above says it is the
; --- right relation. this says it -- love's = on values answers true exactly
; --- when the two pin to the same value -- so veq is a decision procedure for
; --- paths on the pinned carrier, sound and complete, and every law stated with
; --- one can be stated with the other ---
(assert
 (defq uv-leaf-pin
   (pi a atm (pi w val (pi h (paths bool (andb (negb (vtwo w)) (aeqb a (vleaf w))) true)
      (paths pval (pleaf a) (pin w)))))
   (lam a (lam w (lam h
      (pathscomp0 pval (pleaf a) (pleaf (vleaf w)) (pin w)
         (maponpaths atm pval pleaf a (vleaf w)
            (uv-aeqb-paths a (vleaf w)
               (andbtruer (negb (vtwo w)) (aeqb a (vleaf w)) h)))
         (pathsinv0 pval (pin w) (pleaf (vleaf w))
            (uv-ppin-leaf w
               (uv-negbtrue (vtwo w)
                  (andbtruel (negb (vtwo w)) (aeqb a (vleaf w)) h)))))))))
 (defq uv-veqd-pin
   (pi d nat (pi t (vtre d) (pi w val (pi h (paths bool (veqd d t w) true)
      (paths pval (ppin d t) (pin w))))))
   (lam d (nat_rect
             (lam q (pi t (vtre q) (pi w val (pi h (paths bool (veqd q t w) true)
                       (paths pval (ppin q t) (pin w))))))
             (lam t (uv-leaf-pin t))
             (lam k (lam IH (lam t
                (coprod_rect
                   (lam z (pi w val (pi h (paths bool (veqd (succ k) z w) true)
                             (paths pval (ppin (succ k) z) (pin w)))))
                   (lam a (uv-leaf-pin a))
                   (lam p (lam w (lam h
                      (pathscomp0 pval
                         (plink (ppin k (vfst k p)) (ppin k (vsnd k p)))
                         (plink (pin (vcap w)) (pin (vcup w)))
                         (pin w)
                         (uv-plink-cong (ppin k (vfst k p)) (pin (vcap w))
                                        (ppin k (vsnd k p)) (pin (vcup w))
                            (IH (vfst k p) (vcap w)
                               (andbtruel (veqd k (vfst k p) (vcap w))
                                          (veqd k (vsnd k p) (vcup w))
                                  (andbtruer (vtwo w)
                                     (andb (veqd k (vfst k p) (vcap w))
                                           (veqd k (vsnd k p) (vcup w))) h)))
                            (IH (vsnd k p) (vcup w)
                               (andbtruer (veqd k (vfst k p) (vcap w))
                                          (veqd k (vsnd k p) (vcup w))
                                  (andbtruer (vtwo w)
                                     (andb (veqd k (vfst k p) (vcap w))
                                           (veqd k (vsnd k p) (vcup w))) h))))
                         (pathsinv0 pval (pin w) (plink (pin (vcap w)) (pin (vcup w)))
                            (uv-ppin-two w
                               (andbtruel (vtwo w)
                                  (andb (veqd k (vfst k p) (vcap w))
                                        (veqd k (vsnd k p) (vcup w))) h)))))))
                   t))))
             d)))
 ; ..and back. a value and its normal form answer veq alike -- both ways round,
 ; since a law names its two sides in its own order -- and then the path between
 ; two normal forms carries across by veq's own transitivity
 (defq uv-pinback-fwd
   (pi d nat (pi t (vtre d) (paths bool (veqd d t (pr1 (ppin d t))) true)))
   (lam d (nat_rect
             (lam q (pi t (vtre q) (paths bool (veqd q t (pr1 (ppin q t))) true)))
             (lam t (uv-aeqb-refl t))
             (lam k (lam IH (lam t
                (coprod_rect
                   (lam z (paths bool (veqd (succ k) z (pr1 (ppin (succ k) z))) true))
                   (lam m (uv-aeqb-refl m))
                   (lam p (uv-andb2
                             (veqd k (vfst k p)
                                (vcap (vlink (pr1 (ppin k (vfst k p)))
                                             (pr1 (ppin k (vsnd k p))))))
                             (veqd k (vsnd k p)
                                (vcup (vlink (pr1 (ppin k (vfst k p)))
                                             (pr1 (ppin k (vsnd k p))))))
                             (uv-veqd-trans k (vfst k p)
                                (pr1 (pr1 (ppin k (vfst k p))))
                                (pr2 (pr1 (ppin k (vfst k p))))
                                (vcap (vlink (pr1 (ppin k (vfst k p)))
                                             (pr1 (ppin k (vsnd k p)))))
                                (IH (vfst k p))
                                (uv-padl-fwd (pr1 (pr1 (ppin k (vfst k p))))
                                             (pr1 (pr1 (ppin k (vsnd k p))))
                                             (pr2 (pr1 (ppin k (vfst k p))))))
                             (uv-veqd-trans k (vsnd k p)
                                (pr1 (pr1 (ppin k (vsnd k p))))
                                (pr2 (pr1 (ppin k (vsnd k p))))
                                (vcup (vlink (pr1 (ppin k (vfst k p)))
                                             (pr1 (ppin k (vsnd k p)))))
                                (IH (vsnd k p))
                                (uv-padr-fwd (pr1 (pr1 (ppin k (vfst k p))))
                                             (pr1 (pr1 (ppin k (vsnd k p))))
                                             (pr2 (pr1 (ppin k (vsnd k p))))))))
                   t))))
             d)))
 (defq uv-pinback-bwd
   (pi d nat (pi t (vtre d)
      (paths bool (veqd (pr1 (pr1 (ppin d t))) (pr2 (pr1 (ppin d t))) (tpair d t)) true)))
   (lam d (nat_rect
             (lam q (pi t (vtre q)
                (paths bool (veqd (pr1 (pr1 (ppin q t))) (pr2 (pr1 (ppin q t)))
                                  (tpair q t)) true)))
             (lam t (uv-aeqb-refl t))
             (lam k (lam IH (lam t
                (coprod_rect
                   (lam z (paths bool (veqd (pr1 (pr1 (ppin (succ k) z)))
                                            (pr2 (pr1 (ppin (succ k) z)))
                                            (tpair (succ k) z)) true))
                   (lam m (uv-aeqb-refl m))
                   (lam p (uv-andb2
                             (veqd (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                        (pr1 (pr1 (ppin k (vsnd k p)))))
                                   (padl (pr1 (pr1 (ppin k (vfst k p))))
                                         (pr1 (pr1 (ppin k (vsnd k p))))
                                         (pr2 (pr1 (ppin k (vfst k p)))))
                                   (tpair k (vfst k p)))
                             (veqd (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                        (pr1 (pr1 (ppin k (vsnd k p)))))
                                   (padr (pr1 (pr1 (ppin k (vfst k p))))
                                         (pr1 (pr1 (ppin k (vsnd k p))))
                                         (pr2 (pr1 (ppin k (vsnd k p)))))
                                   (tpair k (vsnd k p)))
                             (uv-veqd-trans
                                (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                     (pr1 (pr1 (ppin k (vsnd k p)))))
                                (padl (pr1 (pr1 (ppin k (vfst k p))))
                                      (pr1 (pr1 (ppin k (vsnd k p))))
                                      (pr2 (pr1 (ppin k (vfst k p)))))
                                (pr1 (pr1 (ppin k (vfst k p))))
                                (pr2 (pr1 (ppin k (vfst k p))))
                                (tpair k (vfst k p))
                                (uv-padl-bwd (pr1 (pr1 (ppin k (vfst k p))))
                                             (pr1 (pr1 (ppin k (vsnd k p))))
                                             (pr2 (pr1 (ppin k (vfst k p)))))
                                (IH (vfst k p)))
                             (uv-veqd-trans
                                (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                     (pr1 (pr1 (ppin k (vsnd k p)))))
                                (padr (pr1 (pr1 (ppin k (vfst k p))))
                                      (pr1 (pr1 (ppin k (vsnd k p))))
                                      (pr2 (pr1 (ppin k (vsnd k p)))))
                                (pr1 (pr1 (ppin k (vsnd k p))))
                                (pr2 (pr1 (ppin k (vsnd k p))))
                                (tpair k (vsnd k p))
                                (uv-padr-bwd (pr1 (pr1 (ppin k (vfst k p))))
                                             (pr1 (pr1 (ppin k (vsnd k p))))
                                             (pr2 (pr1 (ppin k (vsnd k p)))))
                                (IH (vsnd k p)))))
                   t))))
             d)))
 (defq uv-veq-pin (pi v val (paths bool (veq v (pr1 (pin v))) true))
   (lam v (uv-pinback-fwd (pr1 v) (pr2 v))))
 (defq uv-pin-veq (pi v val (paths bool (veq (pr1 (pin v)) v) true))
   (lam v (uv-pinback-bwd (pr1 v) (pr2 v))))
 ; sound: what veq accepts, paths accepts
 (defq uv-veq-paths
   (pi u val (pi w val (pi h (paths bool (veq u w) true) (paths pval (pin u) (pin w)))))
   (lam u (lam w (uv-veqd-pin (pr1 u) (pr2 u) w))))
 ; complete: what paths accepts, veq accepts
 (defq uv-paths-veq
   (pi u val (pi w val (pi h (paths pval (pin u) (pin w)) (paths bool (veq u w) true))))
   (lam u (lam w (lam h
      (uv-veq-trans u (pr1 (pin u)) w
         (uv-veq-pin u)
         (uv-veq-trans (pr1 (pin u)) (pr1 (pin w)) w
            (uv-veq-of (pr1 (pin u)) (pr1 (pin w))
               (maponpaths pval val (lam x (pr1 x)) (pin u) (pin w) h))
            (uv-pin-veq w))))))))

; --- ..and the colours over it. the first three are the sign's own shape; the
; --- fourth is claude.md's own lambda equation read as a colour -- truth is
; --- positive green, above the blue floor -- and it is the one that needs the
; --- half bit, since a measure at 0 with its half up already ceils to 1 ---
(assert
 (defq uv-blue-green (pi v val (paths bool (orb (negb (vblue v)) (vgreen v)) true))
   (lam v (coprod_rect (lam w (paths bool
                          (orb (negb (andb (zblue w) (negb (pr2 (vre v))))) (zgreen w)) true))
             (lam n (uv-orbr (negb (andb (zblue (ii1 n)) (negb (pr2 (vre v)))))
                             (zgreen (ii1 n)) (idpath true)))
             (lam n (idpath true))
             (pr1 (vre v)))))
 (defq uv-green-or-red (pi v val (paths bool (orb (vgreen v) (vred v)) true))
   (lam v (bool_rect (lam x (paths bool (orb x (negb x)) true))
             (idpath true) (idpath true) (vgreen v))))
 (defq uv-green-red-apart (pi v val (paths bool (negb (andb (vgreen v) (vred v))) true))
   (lam v (bool_rect (lam x (paths bool (negb (andb x (negb x))) true))
             (idpath true) (idpath true) (vgreen v))))
 ; the claim, as one bool of the two things a real is made of
 (defn vcolq (pi z zed (pi h bool bool))
   (lam z (lam h (beqb (natbit (zsat (rceil (rmk z h))))
                       (andb (zgreen z) (negb (andb (zblue z) (negb h))))))))
 (defq uv-colq (pi z zed (pi h bool (paths bool (vcolq z h) true)))
   (lam z (coprod_rect (lam w (pi h bool (paths bool (vcolq w h) true)))
             (lam n (nat_rect (lam q (pi h bool (paths bool (vcolq (ii1 q) h) true)))
                       (lam h (bool_rect (lam x (paths bool (vcolq (ii1 0) x) true))
                                 (idpath true) (idpath true) h))
                       (lam k (lam s (lam h
                          (bool_rect (lam x (paths bool (vcolq (ii1 (succ k)) x) true))
                             (idpath true) (idpath true) h))))
                       n))
             ; the negative half splits too: zsucc (ii2 n) is stuck on a neutral n,
             ; and it is exactly what a half bit under the floor asks for
             (lam n (nat_rect (lam q (pi h bool (paths bool (vcolq (ii2 q) h) true)))
                       (lam h (bool_rect (lam x (paths bool (vcolq (ii2 0) x) true))
                                 (idpath true) (idpath true) h))
                       (lam k (lam s (lam h
                          (bool_rect (lam x (paths bool (vcolq (ii2 (succ k)) x) true))
                             (idpath true) (idpath true) h))))
                       n))
             z)))
 (defq uv-truth-positive-green
   (pi v val (paths bool (beqb (vbit v) (andb (vgreen v) (negb (vblue v)))) true))
   (lam v (uv-colq (pr1 (vre v)) (pr2 (vre v))))))

; --- and the laws the equality unlocks. each is stated as test/law.l states it,
; --- and tools/law2uu.l picks these up by NAME -- so if a row moves and the
; --- obligation with it, the lemma stops applying and the gate says so ---
(assert
 (defq uv-cap-total-d
   (pi d nat (pi t (vtre d)
      (paths bool (orb (vtwod d t) (veq (tpair d t) (vcapd d t))) true)))
   (lam d (nat_rect (lam q (pi t (vtre q)
                     (paths bool (orb (vtwod q t) (veq (tpair q t) (vcapd q t))) true)))
             (lam t (uv-veq-refl (tpair 0 t)))
             (lam k (lam IH (lam t
                (coprod_rect (lam w (paths bool
                               (orb (vtwod (succ k) w)
                                    (veq (tpair (succ k) w) (vcapd (succ k) w))) true))
                   (lam m (uv-aeqb-refl m))
                   (lam p (idpath true))
                   t))))
             d)))
 (defq uv-cap-total (pi v val (paths bool (orb (vtwo v) (veq v (vcap v))) true))
   (lam v (uv-cap-total-d (pr1 v) (pr2 v))))
 (defq uv-cup-total-d
   (pi d nat (pi t (vtre d)
      (paths bool (orb (vtwod d t) (veq vnilv (vcupd d t))) true)))
   (lam d (nat_rect (lam q (pi t (vtre q)
                     (paths bool (orb (vtwod q t) (veq vnilv (vcupd q t))) true)))
             (lam t (uv-veq-refl vnilv))
             (lam k (lam IH (lam t
                (coprod_rect (lam w (paths bool
                               (orb (vtwod (succ k) w) (veq vnilv (vcupd (succ k) w))) true))
                   (lam m (uv-veq-refl vnilv))
                   (lam p (idpath true))
                   t))))
             d)))
 (defq uv-cup-total (pi v val (paths bool (orb (vtwo v) (veq vnilv (vcup v))) true))
   (lam v (uv-cup-total-d (pr1 v) (pr2 v))))
 (defq uv-link-back-d
   (pi d nat (pi t (vtre d)
      (paths bool (orb (negb (vtwod d t))
                       (veq (tpair d t) (vlink (vcapd d t) (vcupd d t)))) true)))
   (lam d (nat_rect (lam q (pi t (vtre q)
                     (paths bool (orb (negb (vtwod q t))
                                      (veq (tpair q t) (vlink (vcapd q t) (vcupd q t)))) true)))
             (lam t (idpath true))
             (lam k (lam IH (lam t
                (coprod_rect (lam w (paths bool
                               (orb (negb (vtwod (succ k) w))
                                    (veq (tpair (succ k) w)
                                         (vlink (vcapd (succ k) w) (vcupd (succ k) w)))) true))
                   (lam m (idpath true))
                   (lam p (uv-andb2
                             (veqd k (vfst k p) (tpair (mx2 k k) (padl k k (vfst k p))))
                             (veqd k (vsnd k p) (tpair (mx2 k k) (padr k k (vsnd k p))))
                             (uv-padl-fwd k k (vfst k p))
                             (uv-padr-fwd k k (vsnd k p))))
                   t))))
             d)))
 (defq uv-link-back
   (pi v val (paths bool (orb (negb (vtwo v)) (veq v (vlink (vcap v) (vcup v)))) true))
   (lam v (uv-link-back-d (pr1 v) (pr2 v))))
 ; link-apart takes no induction at all: cap and cup of (link v v) are the two
 ; pads of v, and the padding lemmas answer both
 (defq uv-link-apart
   (pi v val (paths bool (andb (veq (vcap (vlink v v)) v) (veq (vcup (vlink v v)) v)) true))
   (lam v (uv-andb2
             (veq (tpair (mx2 (pr1 v) (pr1 v)) (padl (pr1 v) (pr1 v) (pr2 v)))
                  (tpair (pr1 v) (pr2 v)))
             (veq (tpair (mx2 (pr1 v) (pr1 v)) (padr (pr1 v) (pr1 v) (pr2 v)))
                  (tpair (pr1 v) (pr2 v)))
             (uv-padl-bwd (pr1 v) (pr1 v) (pr2 v))
             (uv-padr-bwd (pr1 v) (pr1 v) (pr2 v)))))
 ; id? is finer than =, never the converse: where id? holds, = does
 (defq uv-idq-finer (pi v val (paths bool (veq v v) true)) uv-veq-refl)
 ; ..and = is total, answering the charm 1 on every value
 (defq uv-eq-total (pi v val (paths nat (nbit (veq v v)) 1))
   (lam v (maponpaths bool nat (lam z (nbit z)) (veq v v) true (uv-veq-refl v)))))
; --- pin is a homomorphism, which is what lets a law cross the bridge without
; --- being re-proved: the pinned link of two pinned values is the pin of their
; --- link, the two leaf constructors pin on the nose, and the net is untouched
; --- -- so pinning is a change of spelling and not of meaning ---
(assert
 (defq uv-madd2
   (pi a msr (pi a2 msr (pi b msr (pi b2 msr
      (pi p (paths msr a a2) (pi q (paths msr b b2)
         (paths msr (madd a b) (madd a2 b2))))))))
   (lam a (lam a2 (lam b (lam b2 (lam p (lam q
      (pathscomp0 msr (madd a b) (madd a2 b) (madd a2 b2)
         (maponpaths msr msr (lam w (madd w b)) a a2 p)
         (maponpaths msr msr (lam w (madd a2 w)) b b2 q)))))))))
 (defq uv-netd-padl
   (pi a nat (pi b nat (pi t (vtre a)
      (paths msr (netd (mx2 a b) (padl a b t)) (netd a t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre q)
                              (paths msr (netd (mx2 q b) (padl q b t)) (netd q t)))))
             (lam b (lam t
                (nat_rect (lam r (paths msr (netd (mx2 0 r) (padl 0 r t)) (netd 0 t)))
                   (idpath (anet t))
                   (lam k (lam s (idpath (anet t))))
                   b)))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre (succ j))
                             (paths msr (netd (mx2 (succ j) r) (padl (succ j) r t))
                                        (netd (succ j) t))))
                   (lam t (idpath (netd (succ j) t)))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths msr (netd (succ (mx2 j k)) (padl (succ j) (succ k) w))
                                           (netd (succ j) w)))
                         (lam m (idpath (anet m)))
                         (lam p (uv-madd2
                                   (netd (mx2 j k) (padl j k (vfst j p))) (netd j (vfst j p))
                                   (netd (mx2 j k) (padl j k (vsnd j p))) (netd j (vsnd j p))
                                   (IH k (vfst j p)) (IH k (vsnd j p))))
                         t))))
                   b))))
             a)))
 (defq uv-netd-padr
   (pi a nat (pi b nat (pi t (vtre b)
      (paths msr (netd (mx2 a b) (padr a b t)) (netd b t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre b)
                              (paths msr (netd (mx2 q b) (padr q b t)) (netd b t)))))
             (lam b (lam t (idpath (netd b t))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r)
                             (paths msr (netd (mx2 (succ j) r) (padr (succ j) r t))
                                        (netd r t))))
                   (lam t (idpath (anet t)))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths msr (netd (succ (mx2 j k)) (padr (succ j) (succ k) w))
                                           (netd (succ k) w)))
                         (lam m (idpath (anet m)))
                         (lam p (uv-madd2
                                   (netd (mx2 j k) (padr j k (vfst k p))) (netd k (vfst k p))
                                   (netd (mx2 j k) (padr j k (vsnd k p))) (netd k (vsnd k p))
                                   (IH k (vfst k p)) (IH k (vsnd k p))))
                         t))))
                   b))))
             a)))
 (defq uv-ppin-padl
   (pi a nat (pi b nat (pi t (vtre a)
      (paths pval (ppin (mx2 a b) (padl a b t)) (ppin a t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre q)
                              (paths pval (ppin (mx2 q b) (padl q b t)) (ppin q t)))))
             (lam b (lam t
                (nat_rect (lam r (paths pval (ppin (mx2 0 r) (padl 0 r t)) (ppin 0 t)))
                   (idpath (pleaf t))
                   (lam k (lam s (idpath (pleaf t))))
                   b)))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre (succ j))
                             (paths pval (ppin (mx2 (succ j) r) (padl (succ j) r t))
                                         (ppin (succ j) t))))
                   (lam t (idpath (ppin (succ j) t)))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths pval
                                   (ppin (succ (mx2 j k)) (padl (succ j) (succ k) w))
                                   (ppin (succ j) w)))
                         (lam m (idpath (pleaf m)))
                         (lam p (uv-plink-cong
                                   (ppin (mx2 j k) (padl j k (vfst j p))) (ppin j (vfst j p))
                                   (ppin (mx2 j k) (padl j k (vsnd j p))) (ppin j (vsnd j p))
                                   (IH k (vfst j p)) (IH k (vsnd j p))))
                         t))))
                   b))))
             a)))
 (defq uv-ppin-padr
   (pi a nat (pi b nat (pi t (vtre b)
      (paths pval (ppin (mx2 a b) (padr a b t)) (ppin b t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (vtre b)
                              (paths pval (ppin (mx2 q b) (padr q b t)) (ppin b t)))))
             (lam b (lam t (idpath (ppin b t))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r)
                             (paths pval (ppin (mx2 (succ j) r) (padr (succ j) r t))
                                         (ppin r t))))
                   (lam t (idpath (pleaf t)))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths pval
                                   (ppin (succ (mx2 j k)) (padr (succ j) (succ k) w))
                                   (ppin (succ k) w)))
                         (lam m (idpath (pleaf m)))
                         (lam p (uv-plink-cong
                                   (ppin (mx2 j k) (padr j k (vfst k p))) (ppin k (vfst k p))
                                   (ppin (mx2 j k) (padr j k (vsnd k p))) (ppin k (vsnd k p))
                                   (IH k (vfst k p)) (IH k (vsnd k p))))
                         t))))
                   b))))
             a)))
 (defq uv-pin-link
   (pi a val (pi b val (paths pval (pin (vlink a b)) (plink (pin a) (pin b)))))
   (lam a (lam b (uv-plink-cong
      (ppin (mx2 (pr1 a) (pr1 b)) (padl (pr1 a) (pr1 b) (pr2 a))) (pin a)
      (ppin (mx2 (pr1 a) (pr1 b)) (padr (pr1 a) (pr1 b) (pr2 b))) (pin b)
      (uv-ppin-padl (pr1 a) (pr1 b) (pr2 a))
      (uv-ppin-padr (pr1 a) (pr1 b) (pr2 b))))))
 (defq uv-pin-num (pi m msr (paths pval (pin (vnum m)) (pnum m))) (lam m (idpath (pnum m))))
 (defq uv-pin-nil (paths pval (pin vnilv) pnilv) (idpath pnilv))
 (defq uv-pin-net (pi v val (paths msr (pnet (pin v)) (vnet v)))
   (lam v ((nat_rect (lam q (pi t (vtre q) (paths msr (pnet (pin (tpair q t))) (netd q t))))
             (lam t (idpath (anet t)))
             (lam k (lam IH (lam t
                (coprod_rect (lam w (paths msr (pnet (pin (tpair (succ k) w)))
                                               (netd (succ k) w)))
                   (lam m (idpath (anet m)))
                   (lam p (uv-madd2
                             (netd (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                        (pr1 (pr1 (ppin k (vsnd k p)))))
                                   (padl (pr1 (pr1 (ppin k (vfst k p))))
                                         (pr1 (pr1 (ppin k (vsnd k p))))
                                         (pr2 (pr1 (ppin k (vfst k p))))))
                             (netd k (vfst k p))
                             (netd (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                        (pr1 (pr1 (ppin k (vsnd k p)))))
                                   (padr (pr1 (pr1 (ppin k (vfst k p))))
                                         (pr1 (pr1 (ppin k (vsnd k p))))
                                         (pr2 (pr1 (ppin k (vsnd k p))))))
                             (netd k (vsnd k p))
                             (pathscomp0 msr
                                (netd (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                           (pr1 (pr1 (ppin k (vsnd k p)))))
                                      (padl (pr1 (pr1 (ppin k (vfst k p))))
                                            (pr1 (pr1 (ppin k (vsnd k p))))
                                            (pr2 (pr1 (ppin k (vfst k p))))))
                                (pnet (ppin k (vfst k p)))
                                (netd k (vfst k p))
                                (uv-netd-padl (pr1 (pr1 (ppin k (vfst k p))))
                                              (pr1 (pr1 (ppin k (vsnd k p))))
                                              (pr2 (pr1 (ppin k (vfst k p)))))
                                (IH (vfst k p)))
                             (pathscomp0 msr
                                (netd (mx2 (pr1 (pr1 (ppin k (vfst k p))))
                                           (pr1 (pr1 (ppin k (vsnd k p)))))
                                      (padr (pr1 (pr1 (ppin k (vfst k p))))
                                            (pr1 (pr1 (ppin k (vsnd k p))))
                                            (pr2 (pr1 (ppin k (vsnd k p))))))
                                (pnet (ppin k (vsnd k p)))
                                (netd k (vsnd k p))
                                (uv-netd-padr (pr1 (pr1 (ppin k (vfst k p))))
                                              (pr1 (pr1 (ppin k (vsnd k p))))
                                              (pr2 (pr1 (ppin k (vsnd k p)))))
                                (IH (vsnd k p)))))
                   t))))
             (pr1 v)) (pr2 v)))))

; --- and the six laws again, over paths. each is the bridge applied to the veq
; --- law above it -- one line, because the bridge is where the work went. the
; --- shapes change: an orb that carried a side condition becomes the side
; --- condition itself, and totality becomes the statement that veq decides the
; --- equality. that is what a law reads like once its equality is an equality
; --- and not a decision procedure ---
(assert
 (defq uv-orbfalse
   (pi a bool (pi b bool (pi h (paths bool a false) (pi g (paths bool (orb a b) true)
      (paths bool b true)))))
   (lam a (lam b (lam h (lam g
      (pathscomp0 bool b (orb a b) true
         (pathsinv0 bool (orb a b) b (maponpaths bool bool (lam z (orb z b)) a false h))
         g))))))
 ; an atom is its own cap
 (defq uv-pcap-total
   (pi v val (pi h (paths bool (vtwo v) false) (paths pval (pin v) (pin (vcap v)))))
   (lam v (lam h (uv-veq-paths v (vcap v)
      (uv-orbfalse (vtwo v) (veq v (vcap v)) h (uv-cap-total v))))))
 ; ..and its cup is ()
 (defq uv-pcup-total
   (pi v val (pi h (paths bool (vtwo v) false) (paths pval pnilv (pin (vcup v)))))
   (lam v (lam h (uv-veq-paths vnilv (vcup v)
      (uv-orbfalse (vtwo v) (veq vnilv (vcup v)) h (uv-cup-total v))))))
 ; a pair is the link of what it shows
 (defq uv-plink-back
   (pi v val (pi h (paths bool (vtwo v) true)
      (paths pval (pin v) (plink (pin (vcap v)) (pin (vcup v))))))
   (lam v (lam h (uv-ppin-two v h))))
 ; ..and a link shows back what it was given, both limbs
 (defq uv-plink-cap (pi v val (paths pval (pin (vcap (vlink v v))) (pin v)))
   (lam v (uv-veq-paths (vcap (vlink v v)) v
      (andbtruel (veq (vcap (vlink v v)) v) (veq (vcup (vlink v v)) v)
         (uv-link-apart v)))))
 (defq uv-plink-cup (pi v val (paths pval (pin (vcup (vlink v v))) (pin v)))
   (lam v (uv-veq-paths (vcup (vlink v v)) v
      (andbtruer (veq (vcap (vlink v v)) v) (veq (vcup (vlink v v)) v)
         (uv-link-apart v)))))
 ; id? is finer than =, never the converse: where id? holds, = does -- and over
 ; paths that is reflexivity, with nothing left to decide
 (defq uv-pidq-finer (pi x pval (paths pval x x)) (lam x (idpath x)))
 ; ..and = is total: it answers the charm 1 on every pair the paths agree on,
 ; which is the completeness half of the bridge worn as a law
 (defq uv-peq-total
   (pi u val (pi w val (pi h (paths pval (pin u) (pin w)) (paths nat (nbit (veq u w)) 1))))
   (lam u (lam w (lam h (maponpaths bool nat (lam z (nbit z)) (veq u w) true
                           (uv-paths-veq u w h)))))))

; --- claude.md's laws themselves are not spelled here: tools/law2uu.l compiles
; --- them straight off test/law.l's rows into test/uuvallaw.l, so each law has one
; --- spelling and both lanes read it. sat-green and bit-bool need no proof at
; --- all: they are the types -- vsat lands in nat and vbit in bool, so "a
; --- non-negative charm" and "0 or 1" are read off the ladder rather than
; --- checked. the demos: the tower computes ---
(assert
 (= (vof 0 '(ii1 3))  (vof 0 '(vceil (rval (rmk (ii1 2) true)))))   ; ceil 2.5 = 3
 (= (vof 0 '3)        (vof 0 '(vsat  (rval (rmk (ii1 2) true)))))   ; $2.5 = 3
 (= (vof 0 '0)        (vof 0 '(vsat  (zval (zneg 4)))))             ; $-5 = 0
 (= (vof 0 'false)    (vof 0 '(vbit  (zval (zneg 4)))))             ; ?-5 = 0
 (= (vof 0 'true)     (vof 0 '(vnil  (zval (zneg 4)))))             ; !-5 = 1
 (= (vof 0 'true)     (vof 0 '(vbit  (nval 2))))
 ; a link: (1 2) nets 3, and the pair sits one level up
 (= (vof 0 '3)
      (vof 0 '(vsat (tpair 1 (ii2 (tpair (anum (rmsr (zrea (nzed 1))))
                                         (anum (rmsr (zrea (nzed 2))))))))))
 ; (-2 1) nets -1, so it saturates to 0 and reads nil -- the sign is the whole story
 (= (vof 0 '0)
      (vof 0 '(vsat (tpair 1 (ii2 (tpair (anum (rmsr (zrea (zneg 1))))
                                         (anum (rmsr (zrea (nzed 1))))))))))
 ; a twin: (3 ~ 4) ceils to 3 -- re drops the imaginary rung
 (= (vof 0 '(ii1 3))
      (vof 0 '(vceil (vnum (tpair (zrea (nzed 3)) (zrea (nzed 4)))))))
 ; --- and veq discriminates, which is what keeps its laws from being vacuous:
 ; --- a constant-true equality would prove every one of them ---
 (= (vof 0 'true)  (vof 0 '(veq (nval 2) (nval 2))))
 (= (vof 0 'false) (vof 0 '(veq (nval 2) (nval 3))))
 (= (vof 0 'true)  (vof 0 '(vtwo (vlink (nval 1) (nval 2)))))
 (= (vof 0 'false) (vof 0 '(vtwo (nval 1))))
 (= (vof 0 'false) (vof 0 '(veq (vlink (nval 1) (nval 2)) (nval 1))))
 (= (vof 0 'false) (vof 0 '(veq (vlink (nval 1) (nval 2)) (vlink (nval 2) (nval 1)))))
 ; cap and cup invert a link, and the padding a link costs is not observable
 (= (vof 0 'true)  (vof 0 '(veq (vcap (vlink (nval 1) (nval 2))) (nval 1))))
 (= (vof 0 'true)  (vof 0 '(veq (vcup (vlink (nval 1) (nval 2))) (nval 2))))
 (= (vof 0 'true)  (vof 0 '(veq (vlink (vlink (nval 1) (nval 2)) (nval 3))
                                (vlink (vlink (nval 1) (nval 2)) (nval 3)))))
 ; an atom is its own cap; its cup is ()
 (= (vof 0 'true)  (vof 0 '(veq (vcap (nval 7)) (nval 7))))
 (= (vof 0 'true)  (vof 0 '(veq (vcup (nval 7)) vnilv)))
 ; --- and veq composes: a value, its padded self, and the same value read back
 ; --- out of a deeper link are three spellings, and the chain closes ---
 (= (vof 0 'true)
      (vof 0 '(veq (vcap (vlink (vcap (vlink (nval 1) (nval 2))) (nval 3))) (nval 1))))
 (= (vof 0 'true)
      (vof 0 '(veq (vlink (vcap (vlink (nval 1) (nval 2))) (nval 3))
                   (vlink (nval 1) (nval 3)))))
 ; ..and the congruence discriminates: one limb agreeing is not enough
 (= (vof 0 'false)
      (vof 0 '(veq (vlink (nval 1) (nval 2)) (vlink (nval 1) (nval 3)))))
 ; --- and the pinned carrier: the index a value carries is the height it has,
 ; --- where the unpinned one carries whatever height it was met at ---
 (= (vof 0 '0) (vof 0 '(pr1 (pr1 (pin (nval 7))))))
 (= (vof 0 '1) (vof 0 '(pr1 (pr1 (pin (vlink (nval 1) (nval 2)))))))
 (= (vof 0 '2) (vof 0 '(pr1 (pr1 (pin (vlink (vlink (nval 1) (nval 2)) (nval 3)))))))
 ; the cup of that link is the number 3, met at height 1 and living at 0
 (= (vof 0 '1) (vof 0 '(pr1 (vcup (vlink (vlink (nval 1) (nval 2)) (nval 3))))))
 (= (vof 0 '0)
      (vof 0 '(pr1 (pr1 (pin (vcup (vlink (vlink (nval 1) (nval 2)) (nval 3))))))))
 ; ..and the two spellings net the same, which is what makes pin a change of
 ; spelling and not of meaning
 (= (vof 0 '(vnet (nval 3)))
      (vof 0 '(pnet (pin (vcup (vlink (vlink (nval 1) (nval 2)) (nval 3)))))))
 ; the bridge, computed: what veq accepts the pinned values agree on
 (= (vof 0 'true) (vof 0 '(veq (vcup (vlink (vlink (nval 1) (nval 2)) (nval 3))) (nval 3))))
 (= (vof 0 '(pin (nval 3)))
      (vof 0 '(pin (vcup (vlink (vlink (nval 1) (nval 2)) (nval 3)))))))

; --- the planted faults: a proof term that type-checks whatever it says proves
; --- nothing, so each load-bearing claim gets its false twin, and the kernel
; --- must refuse that one. every negative takes its positive (test/uu.l's rule:
; --- a term rejected for the wrong reason is not evidence). ---
(hear (\ p e (: h (peep HELPC () 0) (? h (h p e) 0))))
(assert
 ; ceil is not the identity on the integer part -- the rung that makes the
 ; tower more than five copies of one retraction
 (id? 'accepted (rejects (\ z (defn uv-ok-ceil
    (paths zed (rceil (rmk (ii1 2) true)) (ii1 3)) (idpath (ii1 3))))))
 (id? 'rejected (rejects (\ z (defn uv-bad-ceil
    (paths zed (rceil (rmk (ii1 2) true)) (ii1 2)) (idpath (ii1 2))))))
 ; the clamp bites at 0, and takes the whole negative half with it
 (id? 'accepted (rejects (\ z (defn uv-ok-clamp (paths nat (zsat (zneg 4)) 0) (idpath 0)))))
 (id? 'rejected (rejects (\ z (defn uv-bad-clamp (paths nat (zsat (zneg 4)) 5) (idpath 5)))))
 ; net sums over a link; it does not read the head alone
 (id? 'rejected (rejects (\ z (defq uv-bad-link
    (pi d nat (pi a (vtre d) (pi b (vtre d)
      (paths msr (netd (succ d) (ii2 (tpair a b))) (netd d a)))))
    (lam d (lam a (lam b (idpath (netd d a)))))))))
 ; and saturate is idempotent, not idempotent-up-to-one
 (id? 'rejected (rejects (\ z (defq uv-bad-satidem
    (pi v val (paths nat (vsat (nval (succ (vsat v)))) (vsat v)))
    (lam v (idpath (vsat v)))))))
 ; the tag on the leaf: () and 0 net the same and are not the same value
 (id? 'accepted (rejects (\ z (defn uv-ok-tag
    (paths bool (veq vnilv (nval 0)) false) (idpath false)))))
 (id? 'rejected (rejects (\ z (defn uv-bad-tag
    (paths bool (veq vnilv (nval 0)) true) (idpath true)))))
 ; ..and it is only the tag: the measure cannot tell them apart
 (id? 'rejected (rejects (\ z (defn uv-bad-tagnet
    (paths bool (meqb (vnet vnilv) (vnet (nval 0))) false) (idpath false)))))
 ; the ceiling holds a net that will not fit at the edge
 (id? 'accepted (rejects (\ z (defn uv-ok-ceiling
    (paths nat (zsatc (nzed (add chmax 3))) chmax) (idpath chmax)))))
 (id? 'rejected (rejects (\ z (defn uv-bad-ceiling
    (paths nat (zsatc (nzed (add chmax 3))) (add chmax 3)) (idpath (add chmax 3))))))
 ; and truth is green above the floor, not green alone -- 0 is both
 (id? 'rejected (rejects (\ z (defq uv-bad-green
    (pi v val (paths bool (beqb (vbit v) (vgreen v)) true))
    (lam v (uv-colq (pr1 (vre v)) (pr2 (vre v))))))))
 ; transitivity takes both sides -- the left one alone says nothing about the right
 (id? 'accepted (rejects (\ z (defq uv-ok-trans
    (pi u val (pi v val (pi w val (pi h1 (paths bool (veq u v) true)
       (pi h2 (paths bool (veq v w) true) (paths bool (veq u w) true))))))
    uv-veq-trans))))
 (id? 'rejected (rejects (\ z (defq uv-bad-trans
    (pi u val (pi v val (pi w val (pi h1 (paths bool (veq u v) true)
       (paths bool (veq u w) true)))))
    (lam u (lam v (lam w (lam h1 (uv-veq-refl u)))))))))
 ; ..and the congruence takes both limbs: a link is not read by its cap alone
 (id? 'rejected (rejects (\ z (defq uv-bad-link-cong
    (pi a val (pi b val (pi c val (pi d val (pi ha (paths bool (veq a c) true)
       (paths bool (veq (vlink a b) (vlink c d)) true))))))
    (lam a (lam b (lam c (lam d (lam ha (uv-veq-refl (vlink a b)))))))))))
 ; the pin does not move the net: a normaliser that did would be worthless, and
 ; every law crossing the bridge would be crossing to a different carrier
 (id? 'accepted (rejects (\ z (defq uv-ok-pinnet
    (pi v val (paths msr (pnet (pin v)) (vnet v))) uv-pin-net))))
 (id? 'rejected (rejects (\ z (defq uv-bad-pinnet
    (pi v val (paths msr (pnet (pin v)) mzero)) uv-pin-net))))
 ; ..and the bridge goes both ways: soundness alone leaves veq free to accept
 ; more than paths does, which is exactly the thing a decision procedure owes
 (id? 'accepted (rejects (\ z (defq uv-ok-bridge
    (pi u val (pi w val (pi h (paths pval (pin u) (pin w)) (paths bool (veq u w) true))))
    uv-paths-veq))))
 (id? 'rejected (rejects (\ z (defq uv-bad-bridge
    (pi u val (pi w val (paths bool (veq u w) true)))
    (lam u (lam w (uv-veq-refl u)))))))
 ; and a leaf spelled at height 1 is a legal VALUE and not a legal pinned one --
 ; the many-spellings defect, turned into a type error
 (id? 'accepted (rejects (\ z (defn uv-ok-pin pval
    (tpair (tpair 0 (anum (rmsr (zrea (nzed 7))))) (idpath 0))))))
 (id? 'rejected (rejects (\ z (defn uv-bad-pin pval
    (tpair (tpair 1 (ii1 (anum (rmsr (zrea (nzed 7)))))) (idpath 1)))))))
(hear test-strict)
