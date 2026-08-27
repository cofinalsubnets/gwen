; test/uuval.l -- LOVE'S OWN VALUES, modelled in uu: the measure tower of
; CLAUDE.md's lambda equations ($x = ceil (re (net x)), ?x = ($x > 0)) built as
; a STACK OF RETRACTIONS, so the laws test/law.l can only refute become theorems
; (tools/law2uu.l compiles them off law.l's own rows into test/uuvallaw.l -- one
; spelling, both lanes). the value universe is a binary tree of measures -- love's
; net sums over a compound's parts and reads nothing else, so a leaf IS its
; measure and no gem's value (hence no ieee) enters the model.
;
; the tower, each rung a section/retraction pair whose round trip is DEFINITIONAL:
;
;   bool  --nbit-->  nat  --nzed-->  zed  --zrea-->  rea  --rmsr-->  msr  --vnum--> val
;         <-natbit-       <--zsat--       <-rceil--       <--mre---        <--vnet--
;
; a real is a HALF-INTEGER (z, plus a bit for +1/2): the smallest carrier closed
; under addition on which ceil is not the identity, which is all the tower's shape
; asks of it -- 2.5 ceils to 3 and -2.5 to -2 here, exactly as love answers.
; the ARITHMETIC (zadd/radd/madd) carries no theorem: every law below is about
; idempotence and factorisation, so a wrong zadd cannot fake one. what gates the
; arithmetic instead is the differential in test/uuvaldiff.l, which runs love's own
; saturate beside this model's vsat over test/law.l's corpus.
;
; substrate: test/uu.l (the kernel + nat vocabulary; this file loads after it by
; sort order). guard: fail by name if the kernel is not up.
(assert (member? 'vof (names ())))

; --- zed: the integers, ceil's codomain. ii1 n is n, ii2 n is -(succ n) ---
(assert
 (defn zed UU (coprod nat nat))
 (defn nzed (pi n nat zed) (lam n (ii1 n)))                 ; the section: a nat IS an integer
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
 (defn zadd (pi a zed (pi b zed zed))
   (lam a (lam b (coprod_rect (lam w zed)
      (lam p (coprod_rect (lam w zed)
                (lam q (ii1 (add p q)))
                (lam q (bool_rect (lam x zed)
                          (ii1 (natminus p (succ q)))
                          (ii2 (natminus q p))
                          (natgtb p q)))
                b))
      (lam p (coprod_rect (lam w zed)
                (lam q (bool_rect (lam x zed)
                          (ii1 (natminus q (succ p)))
                          (ii2 (natminus p q))
                          (natgtb q p)))
                (lam q (ii2 (succ (add p q))))
                b))
      a))))
 (= (vof 0 '(ii1 5)) (vof 0 '(zadd (ii1 2) (ii1 3))))
 (= (vof 0 '(ii2 0)) (vof 0 '(zadd (ii1 2) (ii2 2))))     ;  2 + (-3) = -1
 (= (vof 0 '(ii1 1)) (vof 0 '(zadd (ii2 1) (ii1 3))))     ; -2 +   3  =  1
 (= (vof 0 '(ii2 3)) (vof 0 '(zadd (ii2 1) (ii2 1))))     ; -2 + (-2) = -4
 (= (vof 0 '(ii1 0)) (vof 0 '(zsucc (ii2 0)))))

; --- rea: a real is a half-integer, z + (h ? 1/2 : 0). ceil is the carry ---
(assert
 (defn rea UU (total2 z zed bool))
 (defn rmk (pi z zed (pi h bool rea)) (lam z (lam h (tpair z h))))
 (defn zrea (pi z zed rea) (lam z (tpair z false)))         ; the section: an integer IS a real
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

; --- msr: the measure is COMPLEX, and re is the rung love's ceil reads ---
(assert
 (defn msr UU (total2 r rea rea))
 (defn mre (pi m msr rea) (lam m (pr1 m)))
 (defn rmsr (pi r rea msr) (lam r (tpair r rzero)))         ; the section: a real IS a measure
 (defn mzero msr (rmsr rzero))
 (defn madd (pi a msr (pi b msr msr))
   (lam a (lam b (tpair (radd (pr1 a) (pr1 b)) (radd (pr2 a) (pr2 b)))))))

; --- val: a value is a binary tree of measures, depth-indexed (nlist's trick).
; --- at every level a node is a LEAF (ii1, carrying its measure) or a PAIR of
; --- the level below (ii2) -- love's link. so the depth is a HEIGHT BOUND and
; --- not a shape: a leaf spells the same at every depth, which is what makes a
; --- node's head unambiguous and lets cap/cup/two? read it without recursing.
; --- net folds the tree ---
(assert
 (defn vtre (pi d nat UU)
   (lam d (nat_rect (lam q UU) msr (lam k (lam T (coprod msr (total2 x T T)))) d)))
 ; the two limbs of a link, NAMED. bare pr1/pr2 would do it here, but the Rocq
 ; export elaborates a sum_rect's branches before it unifies the scrutinee's
 ; type, so the pair's family is still a metavariable when pr2 asks for it and
 ; the unification goes higher-order. a named projection pins it from its own
 ; argument type instead, which is first-order and always solvable.
 (defn vfst (pi k nat (pi p (total2 x (vtre k) (vtre k)) (vtre k))) (lam k (lam p (pr1 p))))
 (defn vsnd (pi k nat (pi p (total2 x (vtre k) (vtre k)) (vtre k))) (lam k (lam p (pr2 p))))
 (defn netd (pi d nat (pi t (vtre d) msr))
   (lam d (nat_rect (lam q (pi t (vtre q) msr))
             (lam t t)
             (lam k (lam IH (lam t (coprod_rect (lam w msr)
                                      (lam m m)
                                      (lam p (madd (IH (vfst k p)) (IH (vsnd k p))))
                                      t))))
             d)))
 (defn val UU (total2 d nat (vtre d)))
 (defn vnum (pi m msr val) (lam m (tpair 0 m)))             ; an atom IS its measure
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
 (defn vbit  (pi v val bool) (lam v (natbit (vsat v))))     ; love's bit, ?x
 (defn vnil  (pi v val bool) (lam v (negb (vbit v)))))      ; love's nil?, !x

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

; --- net is a HOMOMORPHISM: a leaf nets its measure at whatever height it sits
; --- at, and a link sums ---
(assert
 (defq uv-net-leaf
   (pi d nat (pi m msr (paths msr (netd (succ d) (ii1 m)) m)))
   (lam d (lam m (idpath m))))
 (defq uv-net-link
   (pi d nat (pi a (vtre d) (pi b (vtre d)
     (paths msr (netd (succ d) (ii2 (tpair a b))) (madd (netd d a) (netd d b))))))
   (lam d (lam a (lam b (idpath (madd (netd d a) (netd d b))))))))

; --- STRUCTURAL EQUALITY. the depth is a height BOUND, so one value has many
; --- spellings -- a leaf sits at any depth, a link pads its shallow limb -- and
; --- `paths` is finer than love's `=`. veq compares what a node OBSERVES (is it
; --- a pair, its cap, its cup, its measure) down a FUEL, which is ONE recursion
; --- where a walk over two depth-indexed trees would be two. first the measure's
; --- own equality, and the andb/orb plumbing every proof below leans on ---
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
 ; reflexivity, rung by rung. nateqb (succ k) (succ k) IS nateqb k k, so the
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
             (uv-reqb-refl (pr1 m)) (uv-reqb-refl (pr2 m))))))

; --- the four things a node shows: whether it is a pair, its cap, its cup, and
; --- the measure it carries when it is not. none of them recurse -- a leaf is
; --- ii1 at every depth, so the head is read in one step ---
(assert
 (defn vtwod (pi d nat (pi t (vtre d) bool))
   (lam d (nat_rect (lam q (pi t (vtre q) bool))
             (lam t false)
             (lam k (lam IH (lam t (coprod_rect (lam w bool) (lam m false) (lam p true) t))))
             d)))
 (defn vleafd (pi d nat (pi t (vtre d) msr))
   (lam d (nat_rect (lam q (pi t (vtre q) msr))
             (lam t t)
             (lam k (lam IH (lam t (coprod_rect (lam w msr) (lam m m) (lam p mzero) t))))
             d)))
 (defn vcapd (pi d nat (pi t (vtre d) val))
   (lam d (nat_rect (lam q (pi t (vtre q) val))
             (lam t (tpair 0 t))                            ; an atom is its own cap
             (lam k (lam IH (lam t (coprod_rect (lam w val)
                                      (lam m (tpair 0 m)) (lam p (tpair k (vfst k p))) t))))
             d)))
 (defn vcupd (pi d nat (pi t (vtre d) val))
   (lam d (nat_rect (lam q (pi t (vtre q) val))
             (lam t (vnum mzero))                           ; ..and its cup is ()
             (lam k (lam IH (lam t (coprod_rect (lam w val)
                                      (lam m (vnum mzero)) (lam p (tpair k (vsnd k p))) t))))
             d)))
 (defn vtwo  (pi v val bool) (lam v (vtwod (pr1 v) (pr2 v))))
 (defn vleaf (pi v val msr)  (lam v (vleafd (pr1 v) (pr2 v))))
 (defn vcap  (pi v val val)  (lam v (vcapd (pr1 v) (pr2 v))))
 (defn vcup  (pi v val val)  (lam v (vcupd (pr1 v) (pr2 v))))
 (defn vnilv val (vnum mzero))
 (defn veqf (pi n nat (pi u val (pi w val bool)))
   (lam n (nat_rect (lam q (pi u val (pi w val bool)))
             (lam u (lam w true))
             (lam k (lam IH (lam u (lam w
                (bool_rect (lam x bool)
                   (andb (vtwo w) (andb (IH (vcap u) (vcap w)) (IH (vcup u) (vcup w))))
                   (andb (negb (vtwo w)) (meqb (vleaf u) (vleaf w)))
                   (vtwo u))))))
             n)))
 ; the fuel two values need is bounded by their heights: a step drops both by at
 ; least one, and the succ is what keeps two leaves from going unread
 (defn veq (pi u val (pi w val bool))
   (lam u (lam w (veqf (succ (add (pr1 u) (pr1 w))) u w)))))

; --- LINK, and the padding it costs. two values meet at the taller height, so
; --- the shallow limb is raised -- and BOTH limbs are raised STRUCTURALLY, the
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
 (defn leafat (pi d nat (pi m msr (vtre d)))                  ; a leaf, at any height
   (lam d (nat_rect (lam q (pi m msr (vtre q)))
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

; --- veq is REFLEXIVE at every fuel: the head test abstracts to one bool, and
; --- both arms answer it ---
(assert
 (defq uv-veqf-refl (pi n nat (pi v val (paths bool (veqf n v v) true)))
   (lam n (nat_rect (lam q (pi v val (paths bool (veqf q v v) true)))
             (lam v (idpath true))
             (lam k (lam IH (lam v
                (bool_rect (lam x (paths bool
                             (bool_rect (lam y bool)
                                (andb x (andb (veqf k (vcap v) (vcap v))
                                              (veqf k (vcup v) (vcup v))))
                                (andb (negb x) (meqb (vleaf v) (vleaf v)))
                                x)
                             true))
                   (uv-andb2 (veqf k (vcap v) (vcap v)) (veqf k (vcup v) (vcup v))
                             (IH (vcap v)) (IH (vcup v)))
                   (uv-meqb-refl (vleaf v))
                   (vtwo v)))))
             n)))
 (defq uv-veq-refl (pi v val (paths bool (veq v v) true))
   (lam v (uv-veqf-refl (succ (add (pr1 v) (pr1 v))) v))))


; --- THE PADDING IS INVISIBLE. this is the theorem the depth index owes: a
; --- value and its padded self answer veq alike, so the height a link meets at
; --- is bookkeeping and nothing observable rests on it. one induction on the
; --- FUEL, then on the two heights, then on the tree -- and both argument
; --- orders, since a law names its two sides in its own order ---
(assert
 (defq uv-padl-fwd
   (pi n nat (pi a nat (pi b nat (pi t (vtre a)
      (paths bool (veqf n (tpair a t) (tpair (mx2 a b) (padl a b t))) true)))))
   (lam n (nat_rect (lam q (pi a nat (pi b nat (pi t (vtre a)
                      (paths bool (veqf q (tpair a t) (tpair (mx2 a b) (padl a b t))) true)))))
             (lam a (lam b (lam t (idpath true))))
             (lam m (lam IHn (lam a
                (nat_rect (lam qa (pi b nat (pi t (vtre qa)
                             (paths bool (veqf (succ m) (tpair qa t)
                                           (tpair (mx2 qa b) (padl qa b t))) true))))
                   (lam b (lam t
                      (nat_rect (lam r (paths bool (veqf (succ m) (tpair 0 t)
                                          (tpair (mx2 0 r) (padl 0 r t))) true))
                         (uv-veqf-refl (succ m) (tpair 0 t))
                         (lam k (lam s (uv-meqb-refl t)))
                         b)))
                   (lam j (lam IHa (lam b
                      (nat_rect (lam r (pi t (vtre (succ j))
                                   (paths bool (veqf (succ m) (tpair (succ j) t)
                                     (tpair (mx2 (succ j) r) (padl (succ j) r t))) true)))
                         (lam t (uv-veqf-refl (succ m) (tpair (succ j) t)))
                         (lam k (lam s (lam t
                            (coprod_rect (lam w (paths bool
                                           (veqf (succ m) (tpair (succ j) w)
                                             (tpair (succ (mx2 j k))
                                                    (padl (succ j) (succ k) w))) true))
                               (lam mm (uv-meqb-refl mm))
                               (lam p (uv-andb2
                                         (veqf m (tpair j (vfst j p))
                                               (tpair (mx2 j k) (padl j k (vfst j p))))
                                         (veqf m (tpair j (vsnd j p))
                                               (tpair (mx2 j k) (padl j k (vsnd j p))))
                                         (IHn j k (vfst j p)) (IHn j k (vsnd j p))))
                               t))))
                         b))))
                   a))))
             n)))
 (defq uv-padl-bwd
   (pi n nat (pi a nat (pi b nat (pi t (vtre a)
      (paths bool (veqf n (tpair (mx2 a b) (padl a b t)) (tpair a t)) true)))))
   (lam n (nat_rect (lam q (pi a nat (pi b nat (pi t (vtre a)
                      (paths bool (veqf q (tpair (mx2 a b) (padl a b t)) (tpair a t)) true)))))
             (lam a (lam b (lam t (idpath true))))
             (lam m (lam IHn (lam a
                (nat_rect (lam qa (pi b nat (pi t (vtre qa)
                             (paths bool (veqf (succ m) (tpair (mx2 qa b) (padl qa b t))
                                           (tpair qa t)) true))))
                   (lam b (lam t
                      (nat_rect (lam r (paths bool (veqf (succ m)
                                          (tpair (mx2 0 r) (padl 0 r t)) (tpair 0 t)) true))
                         (uv-veqf-refl (succ m) (tpair 0 t))
                         (lam k (lam s (uv-meqb-refl t)))
                         b)))
                   (lam j (lam IHa (lam b
                      (nat_rect (lam r (pi t (vtre (succ j))
                                   (paths bool (veqf (succ m)
                                     (tpair (mx2 (succ j) r) (padl (succ j) r t))
                                     (tpair (succ j) t)) true)))
                         (lam t (uv-veqf-refl (succ m) (tpair (succ j) t)))
                         (lam k (lam s (lam t
                            (coprod_rect (lam w (paths bool
                                           (veqf (succ m)
                                             (tpair (succ (mx2 j k))
                                                    (padl (succ j) (succ k) w))
                                             (tpair (succ j) w)) true))
                               (lam mm (uv-meqb-refl mm))
                               (lam p (uv-andb2
                                         (veqf m (tpair (mx2 j k) (padl j k (vfst j p)))
                                               (tpair j (vfst j p)))
                                         (veqf m (tpair (mx2 j k) (padl j k (vsnd j p)))
                                               (tpair j (vsnd j p)))
                                         (IHn j k (vfst j p)) (IHn j k (vsnd j p))))
                               t))))
                         b))))
                   a))))
             n)))
 (defq uv-padr-fwd
   (pi n nat (pi a nat (pi b nat (pi t (vtre b)
      (paths bool (veqf n (tpair b t) (tpair (mx2 a b) (padr a b t))) true)))))
   (lam n (nat_rect (lam q (pi a nat (pi b nat (pi t (vtre b)
                      (paths bool (veqf q (tpair b t) (tpair (mx2 a b) (padr a b t))) true)))))
             (lam a (lam b (lam t (idpath true))))
             (lam m (lam IHn (lam a
                (nat_rect (lam qa (pi b nat (pi t (vtre b)
                             (paths bool (veqf (succ m) (tpair b t)
                                           (tpair (mx2 qa b) (padr qa b t))) true))))
                   (lam b (lam t (uv-veqf-refl (succ m) (tpair b t))))
                   (lam j (lam IHa (lam b
                      (nat_rect (lam r (pi t (vtre r)
                                   (paths bool (veqf (succ m) (tpair r t)
                                     (tpair (mx2 (succ j) r) (padr (succ j) r t))) true)))
                         (lam t (uv-meqb-refl t))
                         (lam k (lam s (lam t
                            (coprod_rect (lam w (paths bool
                                           (veqf (succ m) (tpair (succ k) w)
                                             (tpair (succ (mx2 j k))
                                                    (padr (succ j) (succ k) w))) true))
                               (lam mm (uv-meqb-refl mm))
                               (lam p (uv-andb2
                                         (veqf m (tpair k (vfst k p))
                                               (tpair (mx2 j k) (padr j k (vfst k p))))
                                         (veqf m (tpair k (vsnd k p))
                                               (tpair (mx2 j k) (padr j k (vsnd k p))))
                                         (IHn j k (vfst k p)) (IHn j k (vsnd k p))))
                               t))))
                         b))))
                   a))))
             n)))
 (defq uv-padr-bwd
   (pi n nat (pi a nat (pi b nat (pi t (vtre b)
      (paths bool (veqf n (tpair (mx2 a b) (padr a b t)) (tpair b t)) true)))))
   (lam n (nat_rect (lam q (pi a nat (pi b nat (pi t (vtre b)
                      (paths bool (veqf q (tpair (mx2 a b) (padr a b t)) (tpair b t)) true)))))
             (lam a (lam b (lam t (idpath true))))
             (lam m (lam IHn (lam a
                (nat_rect (lam qa (pi b nat (pi t (vtre b)
                             (paths bool (veqf (succ m) (tpair (mx2 qa b) (padr qa b t))
                                           (tpair b t)) true))))
                   (lam b (lam t (uv-veqf-refl (succ m) (tpair b t))))
                   (lam j (lam IHa (lam b
                      (nat_rect (lam r (pi t (vtre r)
                                   (paths bool (veqf (succ m)
                                     (tpair (mx2 (succ j) r) (padr (succ j) r t))
                                     (tpair r t)) true)))
                         (lam t (uv-meqb-refl t))
                         (lam k (lam s (lam t
                            (coprod_rect (lam w (paths bool
                                           (veqf (succ m)
                                             (tpair (succ (mx2 j k))
                                                    (padr (succ j) (succ k) w))
                                             (tpair (succ k) w)) true))
                               (lam mm (uv-meqb-refl mm))
                               (lam p (uv-andb2
                                         (veqf m (tpair (mx2 j k) (padr j k (vfst k p)))
                                               (tpair k (vfst k p)))
                                         (veqf m (tpair (mx2 j k) (padr j k (vsnd k p)))
                                               (tpair k (vsnd k p)))
                                         (IHn j k (vfst k p)) (IHn j k (vsnd k p))))
                               t))))
                         b))))
                   a))))
             n))))

; --- and the laws the equality unlocks. each is stated as test/law.l states it,
; --- and tools/law2uu.l picks these up BY NAME -- so if a row moves and the
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
                   (lam m (uv-meqb-refl m))
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
                             (veqf (succ (add k (succ (mx2 k k))))
                                   (tpair k (vfst k p)) (tpair (mx2 k k) (padl k k (vfst k p))))
                             (veqf (succ (add k (succ (mx2 k k))))
                                   (tpair k (vsnd k p)) (tpair (mx2 k k) (padr k k (vsnd k p))))
                             (uv-padl-fwd (succ (add k (succ (mx2 k k)))) k k (vfst k p))
                             (uv-padr-fwd (succ (add k (succ (mx2 k k)))) k k (vsnd k p))))
                   t))))
             d)))
 (defq uv-link-back
   (pi v val (paths bool (orb (negb (vtwo v)) (veq v (vlink (vcap v) (vcup v)))) true))
   (lam v (uv-link-back-d (pr1 v) (pr2 v))))
 ; link-apart takes no induction at all: cap and cup of (link v v) ARE the two
 ; pads of v, and the padding lemmas answer both
 (defq uv-link-apart
   (pi v val (paths bool (andb (veq (vcap (vlink v v)) v) (veq (vcup (vlink v v)) v)) true))
   (lam v (uv-andb2
             (veq (tpair (mx2 (pr1 v) (pr1 v)) (padl (pr1 v) (pr1 v) (pr2 v)))
                  (tpair (pr1 v) (pr2 v)))
             (veq (tpair (mx2 (pr1 v) (pr1 v)) (padr (pr1 v) (pr1 v) (pr2 v)))
                  (tpair (pr1 v) (pr2 v)))
             (uv-padl-bwd (succ (add (mx2 (pr1 v) (pr1 v)) (pr1 v))) (pr1 v) (pr1 v) (pr2 v))
             (uv-padr-bwd (succ (add (mx2 (pr1 v) (pr1 v)) (pr1 v))) (pr1 v) (pr1 v) (pr2 v)))))
 ; id? is finer than =, never the converse: where id? holds, = does
 (defq uv-idq-finer (pi v val (paths bool (veq v v) true)) uv-veq-refl)
 ; ..and = is total, answering the charm 1 on every value
 (defq uv-eq-total (pi v val (paths nat (nbit (veq v v)) 1))
   (lam v (maponpaths bool nat (lam z (nbit z)) (veq v v) true (uv-veq-refl v)))))
; --- CLAUDE.md's laws themselves are not spelled here: tools/law2uu.l compiles
; --- them straight off test/law.l's rows into test/uuvallaw.l, so each law has ONE
; --- spelling and both lanes read it. sat-green and bit-bool need no proof at
; --- all: they are the TYPES -- vsat lands in nat and vbit in bool, so "a
; --- non-negative charm" and "0 or 1" are read off the ladder rather than
; --- checked. the demos: the tower COMPUTES ---
(assert
 (= (vof 0 '(ii1 3))  (vof 0 '(vceil (rval (rmk (ii1 2) true)))))   ; ceil 2.5 = 3
 (= (vof 0 '3)        (vof 0 '(vsat  (rval (rmk (ii1 2) true)))))   ; $2.5 = 3
 (= (vof 0 '0)        (vof 0 '(vsat  (zval (zneg 4)))))             ; $-5 = 0
 (= (vof 0 'false)    (vof 0 '(vbit  (zval (zneg 4)))))             ; ?-5 = 0
 (= (vof 0 'true)     (vof 0 '(vnil  (zval (zneg 4)))))             ; !-5 = 1
 (= (vof 0 'true)     (vof 0 '(vbit  (nval 2))))
 ; a link: (1 2) nets 3, and the pair sits one level up
 (= (vof 0 '3)
      (vof 0 '(vsat (tpair 1 (ii2 (tpair (rmsr (zrea (nzed 1))) (rmsr (zrea (nzed 2)))))))))
 ; (-2 1) nets -1, so it saturates to 0 and reads NIL -- the sign is the whole story
 (= (vof 0 '0)
      (vof 0 '(vsat (tpair 1 (ii2 (tpair (rmsr (zrea (zneg 1))) (rmsr (zrea (nzed 1)))))))))
 ; a twin: (3 ~ 4) ceils to 3 -- re drops the imaginary rung
 (= (vof 0 '(ii1 3))
      (vof 0 '(vceil (vnum (tpair (zrea (nzed 3)) (zrea (nzed 4)))))))
 ; --- and veq DISCRIMINATES, which is what keeps its laws from being vacuous:
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
 (= (vof 0 'true)  (vof 0 '(veq (vcup (nval 7)) vnilv))))

; --- the planted faults: a proof term that type-checks WHATEVER it says proves
; --- nothing, so each load-bearing claim gets its false twin, and the kernel
; --- must refuse that one. every negative takes its positive (test/uu.l's rule:
; --- a term rejected for the wrong reason is not evidence). ---
(hear (\ p e (: h (peep HELPC () 0) (? h (h p e) 0))))
(assert
 ; ceil is NOT the identity on the integer part -- the rung that makes the
 ; tower more than five copies of one retraction
 (id? 'accepted (rejects (\ z (defn uv-ok-ceil
    (paths zed (rceil (rmk (ii1 2) true)) (ii1 3)) (idpath (ii1 3))))))
 (id? 'rejected (rejects (\ z (defn uv-bad-ceil
    (paths zed (rceil (rmk (ii1 2) true)) (ii1 2)) (idpath (ii1 2))))))
 ; the clamp bites at 0, and takes the whole negative half with it
 (id? 'accepted (rejects (\ z (defn uv-ok-clamp (paths nat (zsat (zneg 4)) 0) (idpath 0)))))
 (id? 'rejected (rejects (\ z (defn uv-bad-clamp (paths nat (zsat (zneg 4)) 5) (idpath 5)))))
 ; net SUMS over a link; it does not read the head alone
 (id? 'rejected (rejects (\ z (defq uv-bad-link
    (pi d nat (pi a (vtre d) (pi b (vtre d)
      (paths msr (netd (succ d) (ii2 (tpair a b))) (netd d a)))))
    (lam d (lam a (lam b (idpath (netd d a)))))))))
 ; and saturate is idempotent, not idempotent-up-to-one
 (id? 'rejected (rejects (\ z (defq uv-bad-satidem
    (pi v val (paths nat (vsat (nval (succ (vsat v)))) (vsat v)))
    (lam v (idpath (vsat v))))))))
(hear test-strict)
