; test/uuvalband.l -- the band algebra over test/uuval.l's values: love's `+`
; and `*` themselves, where uuval models only what net reads and test/uumx.l
; only which lane a pair of kinds dispatches to. the tree's notions of band meet
; here: this carrier has the three shapes a value can be -- () and a number and
; a link -- and each is one of mx.l's fifteen kinds, so the cell vadd takes is
; the lane mx.l's matrix dispatches to, checked over all nine pairs. the three
; vocabularies cut at different depths and disagree about nothing: kinds.l's
; lattice ranks the numeric tier inside the one band mx.l's roster gives it.
;
; the laws are the ones test/proof/rocq/spec.v proves of `gval` -- the unit on
; either side of +, the zero that annihilates * on either side, the degeneration
; of a mixed band under + (a number against a chain arrives as that band's unit,
; which is what buys associativity over the whole carrier with no side
; conditions), the |count| action of a number on a chain, and the cartesian
; product that makes chain*chain a semiring rather than a monoid -- restated
; here over the finer carrier and proved in uu rather than in Rocq alone.
;
; what + needs that uuval did not: the integers really associate. that is the
; first block, and it is why uuval respells zadd by iteration -- the four
; definitional equations below are the whole proof, and the truncated-
; subtraction spelling has none of them. the chain band needs one more thing --
; that append associates -- and that needs the pad to be invisible to append and
; veq to compose, which is why uuval's equality is an equivalence. every law
; here is given twice: once over veq, and once over paths on uuval's pinned
; carrier, the second by the bridge and one line each.
;
; substrate: test/uu.l and test/uuval.l (both load before this by sort order).
(assert (member? 'vof (names ())) (two? (peep GLOB 'vlink 0)))

; --- zsucc and zpred are inverse, each way. the two nat cases are where the
; --- sign crosses: zpred (ii1 0) is -1 and zsucc (ii2 0) is 0 ---
(assert
 (defq uv-zsucc-pred (pi z zed (paths zed (zsucc (zpred z)) z))
   (lam z (coprod_rect (lam w (paths zed (zsucc (zpred w)) w))
             (lam n (nat_rect (lam q (paths zed (zsucc (zpred (ii1 q))) (ii1 q)))
                       (idpath (ii1 0))
                       (lam k (lam r (idpath (ii1 (succ k)))))
                       n))
             (lam n (idpath (ii2 n)))
             z)))
 (defq uv-zpred-succ (pi z zed (paths zed (zpred (zsucc z)) z))
   (lam z (coprod_rect (lam w (paths zed (zpred (zsucc w)) w))
             (lam n (idpath (ii1 n)))
             (lam n (nat_rect (lam q (paths zed (zpred (zsucc (ii2 q))) (ii2 q)))
                       (idpath (ii2 0))
                       (lam k (lam r (idpath (ii2 (succ k)))))
                       n))
             z))))

; --- a step on the right argument of + comes out front. no induction: each arm
; --- is one definitional equation, or one of the two inverses above ---
(assert
 (defq uv-zadd-succ
   (pi a zed (pi b zed (paths zed (zadd a (zsucc b)) (zsucc (zadd a b)))))
   (lam a (lam b
      (coprod_rect (lam w (paths zed (zadd a (zsucc w)) (zsucc (zadd a w))))
         (lam n (idpath (zsucc (zadd a (ii1 n)))))
         (lam n (nat_rect (lam q (paths zed (zadd a (zsucc (ii2 q))) (zsucc (zadd a (ii2 q)))))
                   (pathsinv0 zed (zsucc (zpred a)) a (uv-zsucc-pred a))
                   (lam k (lam r (pathsinv0 zed (zsucc (zpred (zadd a (ii2 k))))
                                                (zadd a (ii2 k))
                                    (uv-zsucc-pred (zadd a (ii2 k))))))
                   n))
         b))))
 (defq uv-zadd-pred
   (pi a zed (pi b zed (paths zed (zadd a (zpred b)) (zpred (zadd a b)))))
   (lam a (lam b
      (coprod_rect (lam w (paths zed (zadd a (zpred w)) (zpred (zadd a w))))
         (lam n (nat_rect (lam q (paths zed (zadd a (zpred (ii1 q))) (zpred (zadd a (ii1 q)))))
                   (idpath (zpred a))
                   (lam k (lam r (pathsinv0 zed (zpred (zsucc (zadd a (ii1 k))))
                                                (zadd a (ii1 k))
                                    (uv-zpred-succ (zadd a (ii1 k))))))
                   n))
         (lam n (idpath (zpred (zadd a (ii2 n)))))
         b)))))

; --- ..and so + associates. the induction is on the right argument alone: each
; --- step peels one zsucc or zpred, the lemma above carries it across a, and
; --- the hypothesis closes the rest ---
(assert
 (defq uv-zadd-assoc
   (pi a zed (pi b zed (pi c zed (paths zed (zadd (zadd a b) c) (zadd a (zadd b c))))))
   (lam a (lam b (lam c
      (coprod_rect (lam w (paths zed (zadd (zadd a b) w) (zadd a (zadd b w))))
         (lam n (nat_rect
                   (lam q (paths zed (zadd (zadd a b) (ii1 q)) (zadd a (zadd b (ii1 q)))))
                   (idpath (zadd a b))
                   (lam k (lam IH (pathscomp0 zed
                             (zsucc (zadd (zadd a b) (ii1 k)))
                             (zsucc (zadd a (zadd b (ii1 k))))
                             (zadd a (zsucc (zadd b (ii1 k))))
                             (maponpaths zed zed (lam x (zsucc x))
                                (zadd (zadd a b) (ii1 k)) (zadd a (zadd b (ii1 k))) IH)
                             (pathsinv0 zed (zadd a (zsucc (zadd b (ii1 k))))
                                            (zsucc (zadd a (zadd b (ii1 k))))
                                (uv-zadd-succ a (zadd b (ii1 k)))))))
                   n))
         (lam n (nat_rect
                   (lam q (paths zed (zadd (zadd a b) (ii2 q)) (zadd a (zadd b (ii2 q)))))
                   (pathsinv0 zed (zadd a (zpred b)) (zpred (zadd a b)) (uv-zadd-pred a b))
                   (lam k (lam IH (pathscomp0 zed
                             (zpred (zadd (zadd a b) (ii2 k)))
                             (zpred (zadd a (zadd b (ii2 k))))
                             (zadd a (zpred (zadd b (ii2 k))))
                             (maponpaths zed zed (lam x (zpred x))
                                (zadd (zadd a b) (ii2 k)) (zadd a (zadd b (ii2 k))) IH)
                             (pathsinv0 zed (zadd a (zpred (zadd b (ii2 k))))
                                            (zpred (zadd a (zadd b (ii2 k))))
                                (uv-zadd-pred a (zadd b (ii2 k)))))))
                   n))
         c)))))
 ; ..and 0 is its unit on both sides. the right is definitional; the left takes
 ; the same induction the associativity did
 (defq uv-zadd-runit (pi a zed (paths zed (zadd a zzero) a)) (lam a (idpath a)))
 (defq uv-zadd-lunit (pi b zed (paths zed (zadd zzero b) b))
   (lam b (coprod_rect (lam w (paths zed (zadd zzero w) w))
             (lam n (nat_rect (lam q (paths zed (zadd zzero (ii1 q)) (ii1 q)))
                       (idpath (ii1 0))
                       (lam k (lam IH (maponpaths zed zed (lam x (zsucc x))
                                         (zadd zzero (ii1 k)) (ii1 k) IH)))
                       n))
             (lam n (nat_rect (lam q (paths zed (zadd zzero (ii2 q)) (ii2 q)))
                       (idpath (ii2 0))
                       (lam k (lam IH (maponpaths zed zed (lam x (zpred x))
                                         (zadd zzero (ii2 k)) (ii2 k) IH)))
                       n))
             b))))

; --- a step on the left argument comes out front too. this is the one the half
; --- bit's carry needs: two halves make a whole, and the whole must travel ---
(assert
 (defq uv-zadd-lsucc
   (pi a zed (pi b zed (paths zed (zadd (zsucc a) b) (zsucc (zadd a b)))))
   (lam a (lam b
      (coprod_rect (lam w (paths zed (zadd (zsucc a) w) (zsucc (zadd a w))))
         (lam n (nat_rect (lam q (paths zed (zadd (zsucc a) (ii1 q)) (zsucc (zadd a (ii1 q)))))
                   (idpath (zsucc a))
                   (lam k (lam IH (maponpaths zed zed (lam x (zsucc x))
                                     (zadd (zsucc a) (ii1 k)) (zsucc (zadd a (ii1 k))) IH)))
                   n))
         (lam n (nat_rect (lam q (paths zed (zadd (zsucc a) (ii2 q)) (zsucc (zadd a (ii2 q)))))
                   (pathscomp0 zed (zpred (zsucc a)) a (zsucc (zpred a))
                      (uv-zpred-succ a)
                      (pathsinv0 zed (zsucc (zpred a)) a (uv-zsucc-pred a)))
                   (lam k (lam IH
                      (pathscomp0 zed
                         (zpred (zadd (zsucc a) (ii2 k)))
                         (zadd a (ii2 k))
                         (zsucc (zpred (zadd a (ii2 k))))
                         (pathscomp0 zed
                            (zpred (zadd (zsucc a) (ii2 k)))
                            (zpred (zsucc (zadd a (ii2 k))))
                            (zadd a (ii2 k))
                            (maponpaths zed zed (lam x (zpred x))
                               (zadd (zsucc a) (ii2 k)) (zsucc (zadd a (ii2 k))) IH)
                            (uv-zpred-succ (zadd a (ii2 k))))
                         (pathsinv0 zed (zsucc (zpred (zadd a (ii2 k)))) (zadd a (ii2 k))
                            (uv-zsucc-pred (zadd a (ii2 k)))))))
                   n))
         b)))))

; --- the three integer shapes the carry asks for, named once so the eight cases
; --- below read as the case analysis they are ---
(assert
 (defq uv-zsl
   (pi p zed (pi q zed (pi r zed
      (paths zed (zsucc (zadd (zadd p q) r)) (zadd p (zsucc (zadd q r)))))))
   (lam p (lam q (lam r
      (pathscomp0 zed
         (zsucc (zadd (zadd p q) r))
         (zsucc (zadd p (zadd q r)))
         (zadd p (zsucc (zadd q r)))
         (maponpaths zed zed (lam x (zsucc x))
            (zadd (zadd p q) r) (zadd p (zadd q r)) (uv-zadd-assoc p q r))
         (pathsinv0 zed (zadd p (zsucc (zadd q r))) (zsucc (zadd p (zadd q r)))
            (uv-zadd-succ p (zadd q r))))))))
 (defq uv-zls
   (pi p zed (pi q zed (pi r zed
      (paths zed (zadd (zsucc (zadd p q)) r) (zsucc (zadd p (zadd q r)))))))
   (lam p (lam q (lam r
      (pathscomp0 zed
         (zadd (zsucc (zadd p q)) r)
         (zsucc (zadd (zadd p q) r))
         (zsucc (zadd p (zadd q r)))
         (uv-zadd-lsucc (zadd p q) r)
         (maponpaths zed zed (lam x (zsucc x))
            (zadd (zadd p q) r) (zadd p (zadd q r)) (uv-zadd-assoc p q r)))))))
 (defq uv-zlr
   (pi p zed (pi q zed (pi r zed
      (paths zed (zadd (zsucc (zadd p q)) r) (zadd p (zsucc (zadd q r)))))))
   (lam p (lam q (lam r
      (pathscomp0 zed
         (zadd (zsucc (zadd p q)) r)
         (zsucc (zadd p (zadd q r)))
         (zadd p (zsucc (zadd q r)))
         (uv-zls p q r)
         (pathsinv0 zed (zadd p (zsucc (zadd q r))) (zsucc (zadd p (zadd q r)))
            (uv-zadd-succ p (zadd q r)))))))))

; --- ..and the half-integers associate. eight cases on the three half bits: the
; --- carry fires wherever two of them meet, and each firing is one of the three
; --- shapes above ---
(assert
 (defn uv-rac (pi p zed (pi x bool (pi q zed (pi y bool (pi r zed (pi z bool UU))))))
   (lam p (lam x (lam q (lam y (lam r (lam z
      (paths rea (radd (radd (rmk p x) (rmk q y)) (rmk r z))
                 (radd (rmk p x) (radd (rmk q y) (rmk r z)))))))))))
 (defq uv-radd-assoc-c
   (pi p zed (pi x bool (pi q zed (pi y bool (pi r zed (pi z bool (uv-rac p x q y r z)))))))
   (lam p (lam x (lam q (lam y (lam r (lam z
      (bool_rect (lam X (uv-rac p X q y r z))
         ; x = true
         (bool_rect (lam Y (uv-rac p true q Y r z))
            ; x,y = true,true -- the carry has already fired on the left
            (bool_rect (lam Z (uv-rac p true q true r Z))
               (uv-pairpath zed bool (zadd (zsucc (zadd p q)) r) (zadd p (zsucc (zadd q r)))
                  true true (uv-zlr p q r) (idpath true))
               (uv-pairpath zed bool (zadd (zsucc (zadd p q)) r) (zsucc (zadd p (zadd q r)))
                  false false (uv-zls p q r) (idpath false))
               z)
            (bool_rect (lam Z (uv-rac p true q false r Z))
               (uv-pairpath zed bool (zsucc (zadd (zadd p q) r)) (zsucc (zadd p (zadd q r)))
                  false false
                  (maponpaths zed zed (lam w (zsucc w))
                     (zadd (zadd p q) r) (zadd p (zadd q r)) (uv-zadd-assoc p q r))
                  (idpath false))
               (uv-pairpath zed bool (zadd (zadd p q) r) (zadd p (zadd q r))
                  true true (uv-zadd-assoc p q r) (idpath true))
               z)
            y)
         ; x = false
         (bool_rect (lam Y (uv-rac p false q Y r z))
            (bool_rect (lam Z (uv-rac p false q true r Z))
               (uv-pairpath zed bool (zsucc (zadd (zadd p q) r)) (zadd p (zsucc (zadd q r)))
                  false false (uv-zsl p q r) (idpath false))
               (uv-pairpath zed bool (zadd (zadd p q) r) (zadd p (zadd q r))
                  true true (uv-zadd-assoc p q r) (idpath true))
               z)
            (bool_rect (lam Z (uv-rac p false q false r Z))
               (uv-pairpath zed bool (zadd (zadd p q) r) (zadd p (zadd q r))
                  true true (uv-zadd-assoc p q r) (idpath true))
               (uv-pairpath zed bool (zadd (zadd p q) r) (zadd p (zadd q r))
                  false false (uv-zadd-assoc p q r) (idpath false))
               z)
            y)
         x))))))))
 (defq uv-radd-assoc
   (pi a rea (pi b rea (pi c rea (paths rea (radd (radd a b) c) (radd a (radd b c))))))
   (lam a (lam b (lam c
      (uv-radd-assoc-c (pr1 a) (pr2 a) (pr1 b) (pr2 b) (pr1 c) (pr2 c))))))
 ; ..and so do the measures, component by component
 (defq uv-madd-assoc
   (pi a msr (pi b msr (pi c msr (paths msr (madd (madd a b) c) (madd a (madd b c))))))
   (lam a (lam b (lam c
      (uv-pairpath rea rea
         (radd (radd (pr1 a) (pr1 b)) (pr1 c)) (radd (pr1 a) (radd (pr1 b) (pr1 c)))
         (radd (radd (pr2 a) (pr2 b)) (pr2 c)) (radd (pr2 a) (radd (pr2 b) (pr2 c)))
         (uv-radd-assoc (pr1 a) (pr1 b) (pr1 c))
         (uv-radd-assoc (pr2 a) (pr2 b) (pr2 c)))))))
 ; ..and 0 is a unit on both sides of both. the half bit is what needs the case:
 ; (orb h false) is stuck on a neutral h where (orb false h) is not
 (defq uv-radd-runit (pi a rea (paths rea (radd a rzero) a))
   (lam a (bool_rect (lam X (paths rea (radd (rmk (pr1 a) X) rzero) (rmk (pr1 a) X)))
             (idpath (rmk (pr1 a) true)) (idpath (rmk (pr1 a) false)) (pr2 a))))
 (defq uv-radd-lunit (pi a rea (paths rea (radd rzero a) a))
   (lam a (uv-pairpath zed bool (zadd zzero (pr1 a)) (pr1 a) (pr2 a) (pr2 a)
             (uv-zadd-lunit (pr1 a)) (idpath (pr2 a)))))
 (defq uv-madd-runit (pi a msr (paths msr (madd a mzero) a))
   (lam a (uv-pairpath rea rea (radd (pr1 a) rzero) (pr1 a) (radd (pr2 a) rzero) (pr2 a)
             (uv-radd-runit (pr1 a)) (uv-radd-runit (pr2 a)))))
 (defq uv-madd-lunit (pi a msr (paths msr (madd mzero a) a))
   (lam a (uv-pairpath rea rea (radd rzero (pr1 a)) (pr1 a) (radd rzero (pr2 a)) (pr2 a)
             (uv-radd-lunit (pr1 a)) (uv-radd-lunit (pr2 a))))))

; --- the band. a value has three shapes and love's dispatch has fifteen kinds;
; --- these are the three the model carries, and the codes are named against
; --- src/mx.l's own so the two notions cannot drift. () is the mint band, a
; --- number the numeric band, a link the chain band ---
(assert
 (defn bUnit nat 0)
 (defn bNum  nat 1)
 (defn bSeq  nat 2)
 (defn aband (pi a atm nat)
   (lam a (coprod_rect (lam w nat) (lam u bUnit) (lam m bNum) a)))
 (defn vband (pi v val nat)
   (lam v (bool_rect (lam x nat) bSeq (aband (vleaf v)) (vtwo v))))
 (= (vof 0 '0) (vof 0 '(vband vnilv)))
 (= (vof 0 '1) (vof 0 '(vband (nval 3))))
 (= (vof 0 '2) (vof 0 '(vband (vlink (nval 1) (nval 2))))))

; --- + over the whole carrier, cell for cell as love answers: the unit on
; --- either side, numbers add, chains append, and a mixed band degenerates --
; --- the number arrives as the chain's unit and the chain answers whole, which
; --- is what buys associativity with no side conditions. append walks the left
; --- spine and replaces its tail whatever the tail is: love answers (1 3) for
; --- ((link 1 2) + '(3)), and so does this ---
(assert
 (defn vappd (pi d nat (pi t (vtre d) (pi w val val)))
   (lam d (nat_rect (lam q (pi t (vtre q) (pi w val val)))
             (lam t (lam w w))
             (lam k (lam IH (lam t (lam w
                (coprod_rect (lam z val)
                   (lam m w)
                   (lam p (vlink (tpair k (vfst k p)) (IH (vsnd k p) w)))
                   t)))))
             d)))
 (defn vapp (pi u val (pi w val val)) (lam u (lam w (vappd (pr1 u) (pr2 u) w))))
 ; the lane a pair of bands takes, in mx.l's own vocabulary
 (defn vlane (pi i nat (pi j nat nat))
   (lam i (lam j (bool_rect (lam x nat)
                    mxLbin_unit
                    (bool_rect (lam y nat)
                       mxLbin_unit
                       (bool_rect (lam z nat)
                          (bool_rect (lam w nat) mxLaddn mxLbin_a (nateqb i bNum))
                          (bool_rect (lam w nat) mxLbin_b mxLadd_seq (nateqb i bNum))
                          (nateqb j bNum))
                       (nateqb j bUnit))
                    (nateqb i bUnit)))))
 (defn vadd (pi u val (pi w val val))
   (lam u (lam w (bool_rect (lam x val)
                    w
                    (bool_rect (lam y val)
                       u
                       (bool_rect (lam z val)
                          (bool_rect (lam q val)
                             (vnum (madd (vnet u) (vnet w)))       ; num + num
                             u                                     ; seq + num degenerates
                             (nateqb (vband u) bNum))
                          (bool_rect (lam q val)
                             w                                     ; num + seq degenerates
                             (vapp u w)                            ; seq + seq appends
                             (nateqb (vband u) bNum))
                          (nateqb (vband w) bNum))
                       (nateqb (vband w) bUnit))
                    (nateqb (vband u) bUnit)))))
 ; the corpus love answers, answered here
 (= (vof 0 'true) (vof 0 '(veq (vadd vnilv (nval 3)) (nval 3))))
 (= (vof 0 'true) (vof 0 '(veq (vadd (nval 3) vnilv) (nval 3))))
 (= (vof 0 'true) (vof 0 '(veq (vadd (nval 2) (nval 3)) (nval 5))))
 (= (vof 0 'true) (vof 0 '(veq (vadd (nval 5) (vlink (nval 1) (nval 2)))
                               (vlink (nval 1) (nval 2)))))
 (= (vof 0 'true) (vof 0 '(veq (vadd (vlink (nval 1) (nval 2)) (nval 5))
                               (vlink (nval 1) (nval 2)))))
 ; '(1) + '(2) = '(1 2): the left spine ends in (), and the right takes its place
 (= (vof 0 'true) (vof 0 '(veq (vadd (vlink (nval 1) vnilv) (vlink (nval 2) vnilv))
                               (vlink (nval 1) (vlink (nval 2) vnilv)))))
 ; ..and a dotted tail is replaced, not kept -- love answers (1 3) too
 (= (vof 0 'true) (vof 0 '(veq (vadd (vlink (nval 1) (nval 2)) (vlink (nval 3) vnilv))
                               (vlink (nval 1) (vlink (nval 3) vnilv))))))

; --- append associates, modulo veq. this is the law the depth index made hard:
; --- a link meets its limbs at a height neither spells, so appending on the left
; --- pads what appending on the right does not. the pad is invisible to append
; --- for the same reason it is invisible to veq -- one induction each -- and the
; --- three-way composition is what veq's transitivity is for ---
(assert
 (defq uv-vapp-padl
   (pi a nat (pi b nat (pi t (vtre a) (pi w val
      (paths bool (veq (vappd (mx2 a b) (padl a b t) w) (vappd a t w)) true)))))
   (lam a (nat_rect
             (lam q (pi b nat (pi t (vtre q) (pi w val
                       (paths bool (veq (vappd (mx2 q b) (padl q b t) w)
                                        (vappd q t w)) true)))))
             (lam b (lam t (lam w
                (nat_rect (lam r (paths bool
                             (veq (vappd (mx2 0 r) (padl 0 r t) w) (vappd 0 t w)) true))
                   (uv-veq-refl w)
                   (lam k (lam sk (uv-veq-refl w)))
                   b))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre (succ j)) (pi w val
                             (paths bool (veq (vappd (mx2 (succ j) r) (padl (succ j) r t) w)
                                              (vappd (succ j) t w)) true))))
                   (lam t (lam w (uv-veq-refl (vappd (succ j) t w))))
                   (lam k (lam sk (lam t (lam w
                      (coprod_rect (lam z (paths bool
                                     (veq (vappd (succ (mx2 j k)) (padl (succ j) (succ k) z) w)
                                          (vappd (succ j) z w)) true))
                         (lam m (uv-veq-refl w))
                         (lam pp (uv-veq-link
                                    (tpair (mx2 j k) (padl j k (vfst j pp)))
                                    (vappd (mx2 j k) (padl j k (vsnd j pp)) w)
                                    (tpair j (vfst j pp))
                                    (vappd j (vsnd j pp) w)
                                    (uv-padl-bwd j k (vfst j pp))
                                    (IH k (vsnd j pp) w)))
                         t)))))
                   b))))
             a)))
 (defq uv-vapp-padr
   (pi a nat (pi b nat (pi t (vtre b) (pi w val
      (paths bool (veq (vappd (mx2 a b) (padr a b t) w) (vappd b t w)) true)))))
   (lam a (nat_rect
             (lam q (pi b nat (pi t (vtre b) (pi w val
                       (paths bool (veq (vappd (mx2 q b) (padr q b t) w)
                                        (vappd b t w)) true)))))
             (lam b (lam t (lam w (uv-veq-refl (vappd b t w)))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (vtre r) (pi w val
                             (paths bool (veq (vappd (mx2 (succ j) r) (padr (succ j) r t) w)
                                              (vappd r t w)) true))))
                   (lam t (lam w (uv-veq-refl w)))
                   (lam k (lam sk (lam t (lam w
                      (coprod_rect (lam z (paths bool
                                     (veq (vappd (succ (mx2 j k)) (padr (succ j) (succ k) z) w)
                                          (vappd (succ k) z w)) true))
                         (lam m (uv-veq-refl w))
                         (lam pp (uv-veq-link
                                    (tpair (mx2 j k) (padr j k (vfst k pp)))
                                    (vappd (mx2 j k) (padr j k (vsnd k pp)) w)
                                    (tpair k (vfst k pp))
                                    (vappd k (vsnd k pp) w)
                                    (uv-padr-bwd j k (vfst k pp))
                                    (IH k (vsnd k pp) w)))
                         t)))))
                   b))))
             a)))
 ; ..and so append associates: the left spine is walked once either way, and what
 ; differs is only the height the middle result was built at
 (defq uv-vapp-assoc
   (pi d nat (pi t (vtre d) (pi v val (pi w val
      (paths bool (veq (vapp (vappd d t v) w) (vappd d t (vapp v w))) true)))))
   (lam d (nat_rect
             (lam q (pi t (vtre q) (pi v val (pi w val
                       (paths bool (veq (vapp (vappd q t v) w)
                                        (vappd q t (vapp v w))) true)))))
             (lam t (lam v (lam w (uv-veq-refl (vapp v w)))))
             (lam k (lam IH (lam t
                (coprod_rect (lam z (pi v val (pi w val
                               (paths bool (veq (vapp (vappd (succ k) z v) w)
                                                (vappd (succ k) z (vapp v w))) true))))
                   (lam m (lam v (lam w (uv-veq-refl (vapp v w)))))
                   (lam p (lam v (lam w
                      (uv-veq-link
                         (tpair (mx2 k (pr1 (vappd k (vsnd k p) v)))
                                (padl k (pr1 (vappd k (vsnd k p) v)) (vfst k p)))
                         (vappd (mx2 k (pr1 (vappd k (vsnd k p) v)))
                                (padr k (pr1 (vappd k (vsnd k p) v))
                                      (pr2 (vappd k (vsnd k p) v))) w)
                         (tpair k (vfst k p))
                         (vappd k (vsnd k p) (vapp v w))
                         (uv-padl-bwd k (pr1 (vappd k (vsnd k p) v)) (vfst k p))
                         (uv-veq-trans
                            (vappd (mx2 k (pr1 (vappd k (vsnd k p) v)))
                                   (padr k (pr1 (vappd k (vsnd k p) v))
                                         (pr2 (vappd k (vsnd k p) v))) w)
                            (vapp (vappd k (vsnd k p) v) w)
                            (vappd k (vsnd k p) (vapp v w))
                            (uv-vapp-padr k (pr1 (vappd k (vsnd k p) v))
                                            (pr2 (vappd k (vsnd k p) v)) w)
                            (IH (vsnd k p) v w))))))
                   t))))
             d))))

; --- the bridge: the model's three bands are three of mx.l's kinds, and the cell
; --- vadd takes is the lane mx.l's `+` matrix dispatches to. nine pairs, checked
; --- against the table love.c's mx.h is laid from, so a dispatch edit that moved
; --- a lane out from under this model fails a proof here ---
(assert
 (defn mxkof (pi i nat nat)
   (lam i (bool_rect (lam x nat) mxKMint
             (bool_rect (lam y nat) mxKCharm mxKChain (nateqb i bNum))
             (nateqb i bUnit))))
 (defq uv-band-is-mx
   (paths bool
     (mxall 3 (lam i (mxall 3 (lam j (nateqb (mxadd (mxkof i) (mxkof j)) (vlane i j))))))
     true)
   (idpath true))
 ; the model's number is a numeric kind, so the tier lattice test/uukindlaw.l
 ; proves the semilattice laws of ranks within this one band, and refines it
 (defq uv-bnum-is-numeric (paths nat (mxband (mxkof bNum)) (mxband mxKGem))
   (idpath (mxband mxKGem)))
 ; ..and the three kinds are distinct kinds, so the nine pairs are nine cells
 (defq uv-bands-apart
   (paths bool (andb (negb (nateqb (mxkof bUnit) (mxkof bNum)))
                     (andb (negb (nateqb (mxkof bNum) (mxkof bSeq)))
                           (negb (nateqb (mxkof bUnit) (mxkof bSeq)))))
     true)
   (idpath true)))

; --- the laws + owes, cell by cell. each is stated over the shape the cell is
; --- about rather than over its band code: a code is a nat and a neutral one
; --- tells conv nothing, where a tree with a head reduces the whole dispatch.
; --- so all but the right unit are one idpath, and that one is a case analysis
; --- with no induction in it ---
(assert
 (defq uv-vadd-lunit (pi v val (paths val (vadd vnilv v) v)) (lam v (idpath v)))
 ; the right unit is a veq law and not a paths one: () + () answers the () the
 ; right operand spells, which sits at height 0 where the left one may not
 (defq uv-vadd-runit (pi v val (paths bool (veq (vadd v vnilv) v) true))
   (lam v ((nat_rect
              (lam d (pi t (vtre d) (paths bool (veq (vadd (tpair d t) vnilv) (tpair d t))
                                                true)))
              (lam t (coprod_rect
                        (lam w (paths bool (veq (vadd (tpair 0 w) vnilv) (tpair 0 w)) true))
                        (lam u (unit_rect
                                  (lam y (paths bool (veq (vadd (tpair 0 (ii1 y)) vnilv)
                                                          (tpair 0 (ii1 y))) true))
                                  (uv-veq-refl vnilv) u))
                        (lam m (uv-veq-refl (tpair 0 (ii2 m))))
                        t))
              (lam k (lam IH (lam t
                 (coprod_rect
                    (lam w (paths bool (veq (vadd (tpair (succ k) w) vnilv)
                                            (tpair (succ k) w)) true))
                    (lam a (coprod_rect
                              (lam y (paths bool (veq (vadd (tpair (succ k) (ii1 y)) vnilv)
                                                      (tpair (succ k) (ii1 y))) true))
                              (lam u (unit_rect
                                        (lam y (paths bool
                                                  (veq (vadd (tpair (succ k) (ii1 (ii1 y)))
                                                             vnilv)
                                                       (tpair (succ k) (ii1 (ii1 y)))) true))
                                        (idpath true) u))
                              (lam m (uv-veq-refl (tpair (succ k) (ii1 (ii2 m)))))
                              a))
                    (lam p (uv-veq-refl (tpair (succ k) (ii2 p))))
                    t))))
              (pr1 v)) (pr2 v))))
 ; numbers add, by the measure's own monoid
 (defq uv-vadd-num (pi a msr (pi b msr (paths val (vadd (vnum a) (vnum b)) (vnum (madd a b)))))
   (lam a (lam b (idpath (vnum (madd a b))))))
 (defq uv-vadd-num-assoc
   (pi a msr (pi b msr (pi c msr
      (paths val (vadd (vadd (vnum a) (vnum b)) (vnum c))
                 (vadd (vnum a) (vadd (vnum b) (vnum c)))))))
   (lam a (lam b (lam c
      (maponpaths msr val (lam w (vnum w))
         (madd (madd a b) c) (madd a (madd b c)) (uv-madd-assoc a b c))))))
 ; a mixed band degenerates: the chain answers whole and the number is gone
 (defq uv-vadd-degen-l
   (pi m msr (pi a val (pi b val (paths val (vadd (vnum m) (vlink a b)) (vlink a b)))))
   (lam m (lam a (lam b (idpath (vlink a b))))))
 (defq uv-vadd-degen-r
   (pi m msr (pi a val (pi b val (paths val (vadd (vlink a b) (vnum m)) (vlink a b)))))
   (lam m (lam a (lam b (idpath (vlink a b))))))
 ; ..and the net can see it go: this is the whole reason + associates over the
 ; carrier with no side conditions, and the whole reason it is not a hom
 (defq uv-vadd-degen-net
   (pi m msr (pi a val (pi b val
      (paths msr (vnet (vadd (vnum m) (vlink a b))) (vnet (vlink a b))))))
   (lam m (lam a (lam b (idpath (vnet (vlink a b)))))))
 ; chains append
 (defq uv-vadd-seq
   (pi a val (pi b val (pi c val (pi d val
      (paths val (vadd (vlink a b) (vlink c d)) (vapp (vlink a b) (vlink c d)))))))
   (lam a (lam b (lam c (lam d (idpath (vapp (vlink a b) (vlink c d)))))))))

; --- and so + associates, cell by cell over the three shapes a value has. the
; --- carrier is a sigma over a depth-indexed tree and not a three-constructor
; --- inductive, so there is no destruct to write one forall with: what stands in
; --- for it is the twenty-seven cells, which is the same content spec.v's
; --- `destruct x, y, z` covers. twenty-five of them are one reflexivity -- both
; --- sides reduce to the same value, which is exactly what the degeneration
; --- buys -- and the two that carry mathematics are the numeric cell (the
; --- measure's own monoid) and the chain cell (append) ---
(assert
 (defn uv-aa (pi u val (pi v val (pi w val UU)))
   (lam u (lam v (lam w
      (paths bool (veq (vadd (vadd u v) w) (vadd u (vadd v w))) true)))))
 (defq uv-aa-uuu
   (uv-aa vnilv vnilv vnilv)
   (uv-veq-refl vnilv))
 (defq uv-aa-uun
   (pi c msr (uv-aa vnilv vnilv (vnum c)))
   (lam c (uv-veq-refl (vnum c))))
 (defq uv-aa-uus
   (pi e val (pi f val (uv-aa vnilv vnilv (vlink e f))))
   (lam e (lam f (uv-veq-refl (vlink e f)))))
 (defq uv-aa-unu
   (pi b msr (uv-aa vnilv (vnum b) vnilv))
   (lam b (uv-veq-refl (vnum b))))
 (defq uv-aa-unn
   (pi b msr (pi c msr (uv-aa vnilv (vnum b) (vnum c))))
   (lam b (lam c (uv-veq-refl (vnum (madd b c))))))
 (defq uv-aa-uns
   (pi b msr (pi e val (pi f val (uv-aa vnilv (vnum b) (vlink e f)))))
   (lam b (lam e (lam f (uv-veq-refl (vlink e f))))))
 (defq uv-aa-usu
   (pi c val (pi d val (uv-aa vnilv (vlink c d) vnilv)))
   (lam c (lam d (uv-veq-refl (vlink c d)))))
 (defq uv-aa-usn
   (pi c val (pi d val (pi e msr (uv-aa vnilv (vlink c d) (vnum e)))))
   (lam c (lam d (lam e (uv-veq-refl (vlink c d))))))
 (defq uv-aa-uss
   (pi c val (pi d val (pi e val (pi f val (uv-aa vnilv (vlink c d) (vlink e f))))))
   (lam c (lam d (lam e (lam f (uv-veq-refl (vapp (vlink c d) (vlink e f))))))))
 (defq uv-aa-nuu
   (pi a msr (uv-aa (vnum a) vnilv vnilv))
   (lam a (uv-veq-refl (vnum a))))
 (defq uv-aa-nun
   (pi a msr (pi c msr (uv-aa (vnum a) vnilv (vnum c))))
   (lam a (lam c (uv-veq-refl (vnum (madd a c))))))
 (defq uv-aa-nus
   (pi a msr (pi e val (pi f val (uv-aa (vnum a) vnilv (vlink e f)))))
   (lam a (lam e (lam f (uv-veq-refl (vlink e f))))))
 (defq uv-aa-nnu
   (pi a msr (pi b msr (uv-aa (vnum a) (vnum b) vnilv)))
   (lam a (lam b (uv-veq-refl (vnum (madd a b))))))
 (defq uv-aa-nnn
   (pi a msr (pi b msr (pi c msr (uv-aa (vnum a) (vnum b) (vnum c)))))
   (lam a (lam b (lam c
      (uv-veq-of (vnum (madd (madd a b) c)) (vnum (madd a (madd b c)))
         (maponpaths msr val (lam x (vnum x))
            (madd (madd a b) c) (madd a (madd b c)) (uv-madd-assoc a b c)))))))
 (defq uv-aa-nns
   (pi a msr (pi b msr (pi e val (pi f val (uv-aa (vnum a) (vnum b) (vlink e f))))))
   (lam a (lam b (lam e (lam f (uv-veq-refl (vlink e f)))))))
 (defq uv-aa-nsu
   (pi a msr (pi c val (pi d val (uv-aa (vnum a) (vlink c d) vnilv))))
   (lam a (lam c (lam d (uv-veq-refl (vlink c d))))))
 (defq uv-aa-nsn
   (pi a msr (pi c val (pi d val (pi e msr (uv-aa (vnum a) (vlink c d) (vnum e))))))
   (lam a (lam c (lam d (lam e (uv-veq-refl (vlink c d)))))))
 (defq uv-aa-nss
   (pi a msr (pi c val (pi d val (pi e val (pi f val (uv-aa (vnum a) (vlink c d) (vlink e f)))))))
   (lam a (lam c (lam d (lam e (lam f (uv-veq-refl (vapp (vlink c d) (vlink e f)))))))))
 (defq uv-aa-suu
   (pi a val (pi b val (uv-aa (vlink a b) vnilv vnilv)))
   (lam a (lam b (uv-veq-refl (vlink a b)))))
 (defq uv-aa-sun
   (pi a val (pi b val (pi e msr (uv-aa (vlink a b) vnilv (vnum e)))))
   (lam a (lam b (lam e (uv-veq-refl (vlink a b))))))
 (defq uv-aa-sus
   (pi a val (pi b val (pi e val (pi f val (uv-aa (vlink a b) vnilv (vlink e f))))))
   (lam a (lam b (lam e (lam f (uv-veq-refl (vapp (vlink a b) (vlink e f))))))))
 (defq uv-aa-snu
   (pi a val (pi b val (pi c msr (uv-aa (vlink a b) (vnum c) vnilv))))
   (lam a (lam b (lam c (uv-veq-refl (vlink a b))))))
 (defq uv-aa-snn
   (pi a val (pi b val (pi c msr (pi e msr (uv-aa (vlink a b) (vnum c) (vnum e))))))
   (lam a (lam b (lam c (lam e (uv-veq-refl (vlink a b)))))))
 (defq uv-aa-sns
   (pi a val (pi b val (pi c msr (pi e val (pi f val (uv-aa (vlink a b) (vnum c) (vlink e f)))))))
   (lam a (lam b (lam c (lam e (lam f (uv-veq-refl (vapp (vlink a b) (vlink e f)))))))))
 (defq uv-aa-ssu
   (pi a val (pi b val (pi c val (pi d val (uv-aa (vlink a b) (vlink c d) vnilv)))))
   (lam a (lam b (lam c (lam d (uv-veq-refl (vapp (vlink a b) (vlink c d))))))))
 (defq uv-aa-ssn
   (pi a val (pi b val (pi c val (pi d val (pi e msr (uv-aa (vlink a b) (vlink c d) (vnum e)))))))
   (lam a (lam b (lam c (lam d (lam e (uv-veq-refl (vapp (vlink a b) (vlink c d)))))))))
 (defq uv-aa-sss
   (pi a val (pi b val (pi c val (pi d val (pi e val (pi f val (uv-aa (vlink a b) (vlink c d) (vlink e f))))))))
   (lam a (lam b (lam c (lam d (lam e (lam f
      (uv-vapp-assoc (pr1 (vlink a b)) (pr2 (vlink a b)) (vlink c d) (vlink e f))))))))))

; --- and over paths. every cell above is a veq law; the bridge in uuval.l turns
; --- each one into a paths law on the pinned carrier in a single line, because
; --- the work is in the bridge and not in the cells. this is the restatement:
; --- + associates over the whole carrier as an equality, with veq left standing
; --- as the decision procedure the bridge proves it to be.
; --- each cell crosses at its own shape, for the same reason the cells are
; --- shaped at all: a generic crossing quantified over three neutral values
; --- carries the whole unreduced vadd dispatch, with pin unfolded inside every
; --- one of its leaves, and conv pays for that twice ---
(assert
 (defn uv-paa (pi u val (pi v val (pi w val UU)))
   (lam u (lam v (lam w
      (paths pval (pin (vadd (vadd u v) w)) (pin (vadd u (vadd v w))))))))
 (defq uv-paa-uuu
   (uv-paa vnilv vnilv vnilv)
   (uv-veq-paths (vadd (vadd vnilv vnilv) vnilv) (vadd vnilv (vadd vnilv vnilv))
      uv-aa-uuu))
 (defq uv-paa-uun
   (pi c msr (uv-paa vnilv vnilv (vnum c)))
   (lam c (uv-veq-paths (vadd (vadd vnilv vnilv) (vnum c)) (vadd vnilv (vadd vnilv (vnum c)))
      (uv-aa-uun c))))
 (defq uv-paa-uus
   (pi e val (pi f val (uv-paa vnilv vnilv (vlink e f))))
   (lam e (lam f (uv-veq-paths (vadd (vadd vnilv vnilv) (vlink e f)) (vadd vnilv (vadd vnilv (vlink e f)))
      (uv-aa-uus e f)))))
 (defq uv-paa-unu
   (pi b msr (uv-paa vnilv (vnum b) vnilv))
   (lam b (uv-veq-paths (vadd (vadd vnilv (vnum b)) vnilv) (vadd vnilv (vadd (vnum b) vnilv))
      (uv-aa-unu b))))
 (defq uv-paa-unn
   (pi b msr (pi c msr (uv-paa vnilv (vnum b) (vnum c))))
   (lam b (lam c (uv-veq-paths (vadd (vadd vnilv (vnum b)) (vnum c)) (vadd vnilv (vadd (vnum b) (vnum c)))
      (uv-aa-unn b c)))))
 (defq uv-paa-uns
   (pi b msr (pi e val (pi f val (uv-paa vnilv (vnum b) (vlink e f)))))
   (lam b (lam e (lam f (uv-veq-paths (vadd (vadd vnilv (vnum b)) (vlink e f)) (vadd vnilv (vadd (vnum b) (vlink e f)))
      (uv-aa-uns b e f))))))
 (defq uv-paa-usu
   (pi c val (pi d val (uv-paa vnilv (vlink c d) vnilv)))
   (lam c (lam d (uv-veq-paths (vadd (vadd vnilv (vlink c d)) vnilv) (vadd vnilv (vadd (vlink c d) vnilv))
      (uv-aa-usu c d)))))
 (defq uv-paa-usn
   (pi c val (pi d val (pi e msr (uv-paa vnilv (vlink c d) (vnum e)))))
   (lam c (lam d (lam e (uv-veq-paths (vadd (vadd vnilv (vlink c d)) (vnum e)) (vadd vnilv (vadd (vlink c d) (vnum e)))
      (uv-aa-usn c d e))))))
 (defq uv-paa-uss
   (pi c val (pi d val (pi e val (pi f val (uv-paa vnilv (vlink c d) (vlink e f))))))
   (lam c (lam d (lam e (lam f (uv-veq-paths (vadd (vadd vnilv (vlink c d)) (vlink e f)) (vadd vnilv (vadd (vlink c d) (vlink e f)))
      (uv-aa-uss c d e f)))))))
 (defq uv-paa-nuu
   (pi a msr (uv-paa (vnum a) vnilv vnilv))
   (lam a (uv-veq-paths (vadd (vadd (vnum a) vnilv) vnilv) (vadd (vnum a) (vadd vnilv vnilv))
      (uv-aa-nuu a))))
 (defq uv-paa-nun
   (pi a msr (pi c msr (uv-paa (vnum a) vnilv (vnum c))))
   (lam a (lam c (uv-veq-paths (vadd (vadd (vnum a) vnilv) (vnum c)) (vadd (vnum a) (vadd vnilv (vnum c)))
      (uv-aa-nun a c)))))
 (defq uv-paa-nus
   (pi a msr (pi e val (pi f val (uv-paa (vnum a) vnilv (vlink e f)))))
   (lam a (lam e (lam f (uv-veq-paths (vadd (vadd (vnum a) vnilv) (vlink e f)) (vadd (vnum a) (vadd vnilv (vlink e f)))
      (uv-aa-nus a e f))))))
 (defq uv-paa-nnu
   (pi a msr (pi b msr (uv-paa (vnum a) (vnum b) vnilv)))
   (lam a (lam b (uv-veq-paths (vadd (vadd (vnum a) (vnum b)) vnilv) (vadd (vnum a) (vadd (vnum b) vnilv))
      (uv-aa-nnu a b)))))
 (defq uv-paa-nnn
   (pi a msr (pi b msr (pi c msr (uv-paa (vnum a) (vnum b) (vnum c)))))
   (lam a (lam b (lam c (uv-veq-paths (vadd (vadd (vnum a) (vnum b)) (vnum c)) (vadd (vnum a) (vadd (vnum b) (vnum c)))
      (uv-aa-nnn a b c))))))
 (defq uv-paa-nns
   (pi a msr (pi b msr (pi e val (pi f val (uv-paa (vnum a) (vnum b) (vlink e f))))))
   (lam a (lam b (lam e (lam f (uv-veq-paths (vadd (vadd (vnum a) (vnum b)) (vlink e f)) (vadd (vnum a) (vadd (vnum b) (vlink e f)))
      (uv-aa-nns a b e f)))))))
 (defq uv-paa-nsu
   (pi a msr (pi c val (pi d val (uv-paa (vnum a) (vlink c d) vnilv))))
   (lam a (lam c (lam d (uv-veq-paths (vadd (vadd (vnum a) (vlink c d)) vnilv) (vadd (vnum a) (vadd (vlink c d) vnilv))
      (uv-aa-nsu a c d))))))
 (defq uv-paa-nsn
   (pi a msr (pi c val (pi d val (pi e msr (uv-paa (vnum a) (vlink c d) (vnum e))))))
   (lam a (lam c (lam d (lam e (uv-veq-paths (vadd (vadd (vnum a) (vlink c d)) (vnum e)) (vadd (vnum a) (vadd (vlink c d) (vnum e)))
      (uv-aa-nsn a c d e)))))))
 (defq uv-paa-nss
   (pi a msr (pi c val (pi d val (pi e val (pi f val (uv-paa (vnum a) (vlink c d) (vlink e f)))))))
   (lam a (lam c (lam d (lam e (lam f (uv-veq-paths (vadd (vadd (vnum a) (vlink c d)) (vlink e f)) (vadd (vnum a) (vadd (vlink c d) (vlink e f)))
      (uv-aa-nss a c d e f))))))))
 (defq uv-paa-suu
   (pi a val (pi b val (uv-paa (vlink a b) vnilv vnilv)))
   (lam a (lam b (uv-veq-paths (vadd (vadd (vlink a b) vnilv) vnilv) (vadd (vlink a b) (vadd vnilv vnilv))
      (uv-aa-suu a b)))))
 (defq uv-paa-sun
   (pi a val (pi b val (pi e msr (uv-paa (vlink a b) vnilv (vnum e)))))
   (lam a (lam b (lam e (uv-veq-paths (vadd (vadd (vlink a b) vnilv) (vnum e)) (vadd (vlink a b) (vadd vnilv (vnum e)))
      (uv-aa-sun a b e))))))
 (defq uv-paa-sus
   (pi a val (pi b val (pi e val (pi f val (uv-paa (vlink a b) vnilv (vlink e f))))))
   (lam a (lam b (lam e (lam f (uv-veq-paths (vadd (vadd (vlink a b) vnilv) (vlink e f)) (vadd (vlink a b) (vadd vnilv (vlink e f)))
      (uv-aa-sus a b e f)))))))
 (defq uv-paa-snu
   (pi a val (pi b val (pi c msr (uv-paa (vlink a b) (vnum c) vnilv))))
   (lam a (lam b (lam c (uv-veq-paths (vadd (vadd (vlink a b) (vnum c)) vnilv) (vadd (vlink a b) (vadd (vnum c) vnilv))
      (uv-aa-snu a b c))))))
 (defq uv-paa-snn
   (pi a val (pi b val (pi c msr (pi e msr (uv-paa (vlink a b) (vnum c) (vnum e))))))
   (lam a (lam b (lam c (lam e (uv-veq-paths (vadd (vadd (vlink a b) (vnum c)) (vnum e)) (vadd (vlink a b) (vadd (vnum c) (vnum e)))
      (uv-aa-snn a b c e)))))))
 (defq uv-paa-sns
   (pi a val (pi b val (pi c msr (pi e val (pi f val (uv-paa (vlink a b) (vnum c) (vlink e f)))))))
   (lam a (lam b (lam c (lam e (lam f (uv-veq-paths (vadd (vadd (vlink a b) (vnum c)) (vlink e f)) (vadd (vlink a b) (vadd (vnum c) (vlink e f)))
      (uv-aa-sns a b c e f))))))))
 (defq uv-paa-ssu
   (pi a val (pi b val (pi c val (pi d val (uv-paa (vlink a b) (vlink c d) vnilv)))))
   (lam a (lam b (lam c (lam d (uv-veq-paths (vadd (vadd (vlink a b) (vlink c d)) vnilv) (vadd (vlink a b) (vadd (vlink c d) vnilv))
      (uv-aa-ssu a b c d)))))))
 (defq uv-paa-ssn
   (pi a val (pi b val (pi c val (pi d val (pi e msr (uv-paa (vlink a b) (vlink c d) (vnum e)))))))
   (lam a (lam b (lam c (lam d (lam e (uv-veq-paths (vadd (vadd (vlink a b) (vlink c d)) (vnum e)) (vadd (vlink a b) (vadd (vlink c d) (vnum e)))
      (uv-aa-ssn a b c d e))))))))
 (defq uv-paa-sss
   (pi a val (pi b val (pi c val (pi d val (pi e val (pi f val (uv-paa (vlink a b) (vlink c d) (vlink e f))))))))
   (lam a (lam b (lam c (lam d (lam e (lam f (uv-veq-paths (vadd (vadd (vlink a b) (vlink c d)) (vlink e f)) (vadd (vlink a b) (vadd (vlink c d) (vlink e f)))
      (uv-aa-sss a b c d e f)))))))))
 ; the right unit and append's own law cross the same way. the right unit was a
 ; veq law because () + () answers the right operand's () -- over paths on the
 ; pinned carrier that distinction is gone, both spellings pinning to pnilv
 (defq uv-pvadd-runit (pi v val (paths pval (pin (vadd v vnilv)) (pin v)))
   (lam v (uv-veq-paths (vadd v vnilv) v (uv-vadd-runit v))))
 (defq uv-pvapp-assoc
   (pi d nat (pi t (vtre d) (pi v val (pi w val
      (paths pval (pin (vapp (vappd d t v) w)) (pin (vappd d t (vapp v w))))))))
   (lam d (lam t (lam v (lam w (uv-veq-paths (vapp (vappd d t v) w)
                                             (vappd d t (vapp v w))
                                  (uv-vapp-assoc d t v w))))))))

; --- * over the same carrier. the halves are closed under + and not under *
; --- (a half times a half is a quarter), so the multiplicative band is modelled
; --- at the integer rung: on a whole measure this is love's `*` cell for cell,
; --- and off it -- a measure carrying a half -- `*` leaves the carrier and the
; --- model does not follow. an exact `*` everywhere wants the dyadic rung, and
; --- that is a different carrier, not a lemma about this one ---
(assert
 (defn znegof (pi n nat zed)
   (lam n (nat_rect (lam q zed) (ii1 0) (lam k (lam r (ii2 k))) n)))
 (defn zabs (pi z zed nat)
   (lam z (coprod_rect (lam w nat) (lam n n) (lam n (succ n)) z)))
 (defn zmul (pi a zed (pi b zed zed))
   (lam a (lam b (coprod_rect (lam w zed)
      (lam p (coprod_rect (lam w zed)
                (lam q (ii1 (mul p q)))
                (lam q (znegof (mul p (succ q))))
                b))
      (lam p (coprod_rect (lam w zed)
                (lam q (znegof (mul (succ p) q)))
                (lam q (ii1 (mul (succ p) (succ q))))
                b))
      a))))
 (defn mzed (pi m msr zed) (lam m (pr1 (pr1 m))))          ; the integer rung of the real part
 (defn mmulz (pi a msr (pi b msr msr))
   (lam a (lam b (rmsr (zrea (zmul (mzed a) (mzed b)))))))
 (defn vcount (pi v val nat) (lam v (zabs (mzed (vnet v)))))  ; |count| -- love repeats by it
 (= (vof 0 '(ii1 6))  (vof 0 '(zmul (ii1 2) (ii1 3))))
 (= (vof 0 '(ii2 5))  (vof 0 '(zmul (ii2 1) (ii1 3))))     ; -2 * 3 = -6
 (= (vof 0 '(ii1 6))  (vof 0 '(zmul (ii2 1) (ii2 2))))     ; -2 * -3 = 6
 (= (vof 0 '(ii1 0))  (vof 0 '(zmul (ii1 0) (ii2 2))))
 (= (vof 0 '2)        (vof 0 '(vcount (zval (ii2 1))))))   ; |-2| = 2

; --- the two chain cells: a number repeats a chain |count| times, and two
; --- chains take the cartesian -- each pair a 2-list, which is why the carrier
; --- nests, and what makes this lane a semiring rather than a monoid ---
(assert
 (defn vrep (pi n nat (pi c val val))
   (lam n (lam c (nat_rect (lam q val) vnilv (lam k (lam r (vapp c r))) n))))
 (defn vrowd (pi d nat (pi t (vtre d) (pi x val val)))
   (lam d (nat_rect (lam q (pi t (vtre q) (pi x val val)))
             (lam t (lam x vnilv))
             (lam k (lam IH (lam t (lam x
                (coprod_rect (lam z val)
                   (lam m vnilv)
                   (lam p (vlink (vlink x (vlink (tpair k (vfst k p)) vnilv))
                                 (IH (vsnd k p) x)))
                   t)))))
             d)))
 (defn vrow (pi b val (pi x val val)) (lam b (lam x (vrowd (pr1 b) (pr2 b) x))))
 (defn vcartd (pi d nat (pi t (vtre d) (pi b val val)))
   (lam d (nat_rect (lam q (pi t (vtre q) (pi b val val)))
             (lam t (lam b vnilv))
             (lam k (lam IH (lam t (lam b
                (coprod_rect (lam z val)
                   (lam m vnilv)
                   (lam p (vapp (vrow b (tpair k (vfst k p))) (IH (vsnd k p) b)))
                   t)))))
             d)))
 (defn vcart (pi u val (pi w val val)) (lam u (lam w (vcartd (pr1 u) (pr2 u) w))))
 (defn vmul (pi u val (pi w val val))
   (lam u (lam w (bool_rect (lam x val)
                    vnilv                                    ; () * x = ()
                    (bool_rect (lam y val)
                       vnilv                                 ; x * () = ()
                       (bool_rect (lam z val)
                          (bool_rect (lam q val)
                             (vnum (mmulz (vnet u) (vnet w))) ; num * num
                             (vrep (vcount w) u)              ; seq * num repeats
                             (nateqb (vband u) bNum))
                          (bool_rect (lam q val)
                             (vrep (vcount u) w)              ; num * seq repeats
                             (vcart u w)                      ; seq * seq: the cartesian
                             (nateqb (vband u) bNum))
                          (nateqb (vband w) bNum))
                       (nateqb (vband w) bUnit))
                    (nateqb (vband u) bUnit))))))

; --- the laws * owes. the zero annihilates from either side -- and unlike +'s
; --- unit it needs no case on the shape, since both arms of the outer test
; --- already answer () ---
(assert
 (defq uv-vmul-lzero (pi v val (paths val (vmul vnilv v) vnilv)) (lam v (idpath vnilv)))
 (defq uv-vmul-rzero (pi v val (paths val (vmul v vnilv) vnilv))
   (lam v (bool_rect (lam X (paths val (bool_rect (lam x val) vnilv vnilv X) vnilv))
             (idpath vnilv) (idpath vnilv) (nateqb (vband v) bUnit))))
 (defq uv-vmul-num
   (pi a msr (pi b msr (paths val (vmul (vnum a) (vnum b)) (vnum (mmulz a b)))))
   (lam a (lam b (idpath (vnum (mmulz a b))))))
 (defq uv-vmul-rep-l
   (pi m msr (pi a val (pi b val
      (paths val (vmul (vnum m) (vlink a b)) (vrep (vcount (vnum m)) (vlink a b))))))
   (lam m (lam a (lam b (idpath (vrep (vcount (vnum m)) (vlink a b)))))))
 (defq uv-vmul-rep-r
   (pi m msr (pi a val (pi b val
      (paths val (vmul (vlink a b) (vnum m)) (vrep (vcount (vnum m)) (vlink a b))))))
   (lam m (lam a (lam b (idpath (vrep (vcount (vnum m)) (vlink a b)))))))
 (defq uv-vmul-cart
   (pi a val (pi b val (pi c val (pi d val
      (paths val (vmul (vlink a b) (vlink c d)) (vcart (vlink a b) (vlink c d)))))))
   (lam a (lam b (lam c (lam d (idpath (vcart (vlink a b) (vlink c d))))))))
 ; 0 copies is (): the number zero annihilates a chain too, and it is not the
 ; same value as () doing it -- () and 0 stay apart (uv-nil-ne-zero) where the
 ; string band, which this carrier has no room for, is where spec.v's gval keeps
 ; the two answers apart as well
 (defq uv-vmul-zero-count
   (pi a val (pi b val (paths val (vmul (nval 0) (vlink a b)) vnilv)))
   (lam a (lam b (idpath vnilv)))))

; --- the demos: * computes, and the cartesian is the shape love answers ---
(assert
 (= (vof 0 'true) (vof 0 '(veq (vmul (nval 2) (nval 3)) (nval 6))))
 (= (vof 0 'true) (vof 0 '(veq (vmul (zval (ii2 1)) (nval 3)) (zval (ii2 5)))))
 (= (vof 0 'true) (vof 0 '(veq (vmul vnilv (vlink (nval 1) vnilv)) vnilv)))
 (= (vof 0 'true) (vof 0 '(veq (vmul (nval 0) (vlink (nval 1) vnilv)) vnilv)))
 ; 2 * '(1) = '(1 1)
 (= (vof 0 'true) (vof 0 '(veq (vmul (nval 2) (vlink (nval 1) vnilv))
                               (vlink (nval 1) (vlink (nval 1) vnilv)))))
 ; ..and |count| is the absolute value: -2 repeats twice, as love does
 (= (vof 0 'true) (vof 0 '(veq (vmul (zval (ii2 1)) (vlink (nval 1) vnilv))
                               (vlink (nval 1) (vlink (nval 1) vnilv)))))
 ; '(1) * '(2 3) = ((1 2) (1 3))
 (= (vof 0 'true)
      (vof 0 '(veq (vmul (vlink (nval 1) vnilv) (vlink (nval 2) (vlink (nval 3) vnilv)))
                   (vlink (vlink (nval 1) (vlink (nval 2) vnilv))
                          (vlink (vlink (nval 1) (vlink (nval 3) vnilv)) vnilv)))))
 ; and * discriminates, so none of the above is a constant
 (= (vof 0 'false) (vof 0 '(veq (vmul (nval 2) (nval 3)) (nval 5))))
 (= (vof 0 'false) (vof 0 '(veq (vmul (nval 2) (vlink (nval 1) vnilv))
                                (vlink (nval 1) vnilv)))))

; --- and + associating, computed. the append is the interesting one: the two
; --- bracketings build the same chain at different heights, which is the whole
; --- of what the law says and the whole of what veq is for ---
(assert
 (= (vof 0 'true)
      (vof 0 '(veq (vadd (vadd (vlink (nval 1) vnilv) (vlink (nval 2) vnilv))
                         (vlink (nval 3) vnilv))
                   (vadd (vlink (nval 1) vnilv)
                         (vadd (vlink (nval 2) vnilv) (vlink (nval 3) vnilv))))))
 ; ..and the two bracketings really do differ as trees: veq sees past the height
 (= (vof 0 'false)
      (vof 0 '(veq (vadd (vadd (vlink (nval 1) vnilv) (vlink (nval 2) vnilv))
                         (vlink (nval 3) vnilv))
                   (vlink (nval 1) vnilv))))
 ; append is not commutative, so the associativity above is not a constant
 (= (vof 0 'false)
      (vof 0 '(veq (vapp (vlink (nval 1) vnilv) (vlink (nval 2) vnilv))
                   (vapp (vlink (nval 2) vnilv) (vlink (nval 1) vnilv)))))
 ; the mixed cells associate by degeneration: both sides answer the chain
 (= (vof 0 'true)
      (vof 0 '(veq (vadd (vadd (nval 5) (vlink (nval 1) vnilv)) (nval 7))
                   (vadd (nval 5) (vadd (vlink (nval 1) vnilv) (nval 7))))))
 (= (vof 0 'true)
      (vof 0 '(veq (vadd (vadd (nval 5) (vlink (nval 1) vnilv)) (nval 7))
                   (vlink (nval 1) vnilv)))))

; --- what is not proved here, named rather than left to be noticed. + is done:
; --- every cell associates, chain+chain included. * is not -- its numeric cell
; --- sits at the integer rung (see above), the |count| action wants smul's
; --- homomorphism over that rung, and the cartesian associates only up to the
; --- canonical reassociation, which is a statement about order that veq is too
; --- coarse to make. spec.v holds gtimes_assoc_seq1/2/3 and mul-reassoc holds
; --- the order seam meanwhile, and the differential in test/uuvaldiff.l runs
; --- every ordered pair of a twelve-value corpus through both operations
; --- against love itself.

; --- the planted faults: a proof term that type-checks whatever it says proves
; --- nothing, so each load-bearing claim gets its false twin and the kernel must
; --- refuse that one. every negative takes its positive. ---
(hear (\ p e (: h (peep HELPC () 0) (? h (h p e) 0))))
(assert
 ; the integers really do associate -- and zsucc really does travel
 (id? 'accepted (rejects (\ z (defq uv-ok-zsucc
    (pi a zed (paths zed (zadd a (zsucc zzero)) (zsucc a)))
    (lam a (idpath (zsucc a)))))))
 (id? 'rejected (rejects (\ z (defq uv-bad-zsucc
    (pi a zed (paths zed (zadd a (zsucc zzero)) a))
    (lam a (idpath a))))))
 ; the degeneration drops the number; it does not keep it
 (id? 'accepted (rejects (\ z (defq uv-ok-degen
    (pi m msr (pi a val (pi b val (paths val (vadd (vnum m) (vlink a b)) (vlink a b)))))
    (lam m (lam a (lam b (idpath (vlink a b)))))))))
 (id? 'rejected (rejects (\ z (defq uv-bad-degen
    (pi m msr (pi a val (pi b val (paths val (vadd (vnum m) (vlink a b)) (vnum m)))))
    (lam m (lam a (lam b (idpath (vnum m)))))))))
 ; () is the unit of + and the zero of *, not the unit of both
 (id? 'rejected (rejects (\ z (defq uv-bad-mulunit
    (pi v val (paths val (vmul vnilv v) v)) (lam v (idpath v))))))
 ; the sign of a product is not the sign of its magnitude
 (id? 'accepted (rejects (\ z (defn uv-ok-zmul
    (paths zed (zmul (ii2 1) (ii1 3)) (ii2 5)) (idpath (ii2 5))))))
 (id? 'rejected (rejects (\ z (defn uv-bad-zmul
    (paths zed (zmul (ii2 1) (ii1 3)) (ii1 6)) (idpath (ii1 6))))))
 ; ..and the bridge names the lane mx.l names: a chain against a number keeps
 ; the chain, which is bin_a with the chain on the left
 (id? 'accepted (rejects (\ z (defn uv-ok-lane
    (paths nat (mxadd mxKChain mxKCharm) mxLbin_a) (idpath mxLbin_a)))))
 (id? 'rejected (rejects (\ z (defn uv-bad-lane
    (paths nat (mxadd mxKChain mxKCharm) mxLbin_b) (idpath mxLbin_b)))))
 ; append associates and does not commute -- a law that held of both would be
 ; holding of nothing
 (id? 'rejected (rejects (\ z (defq uv-bad-app-comm
    (pi d nat (pi t (vtre d) (pi v val
       (paths bool (veq (vappd d t v) (vapp v (tpair d t))) true))))
    (lam d (lam t (lam v (uv-veq-refl (vappd d t v)))))))))
 ; ..and the pad really is invisible to it: an append onto a padded spine is the
 ; append onto the spine, which is the step the associativity leans on
 (id? 'accepted (rejects (\ z (defq uv-ok-app-pad
    (pi a nat (pi b nat (pi t (vtre a) (pi w val
       (paths bool (veq (vappd (mx2 a b) (padl a b t) w) (vappd a t w)) true)))))
    uv-vapp-padl)))))
(hear test-strict)
