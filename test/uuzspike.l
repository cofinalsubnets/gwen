; test/uuzspike.l -- SPIKE, priced and disposable: the EXACT-HEIGHT carrier.
; uuval indexes a value by a height BOUND, so one value has many spellings, the
; equality is veq rather than paths, and the apparatus that costs -- reflection,
; transitivity, the four pad lemmas, the link congruence -- is 385 of that file's
; 1019 lines. this file asks what the same carrier costs with the index PINNED to
; the height: is conv still fast, and is the two-limb congruence really two
; maponpaths. it builds the family, the link, and the congruence, and nothing else.
;
; the plan for this said course-of-values recursion. it does not need it: within
; ONE index the spelling is ALREADY unique -- a node either shows a leaf tag or a
; pair, and the recursion is forced -- so the whole multiplicity is the choice of
; d. pinning d is a PROOF carried beside the tree, not a new type family.

(assert
 ; the leaf's payload is beside the question here; a nat stands in for the measure
 (defn zatm UU (coprod unit nat))
 (defn zanet (pi a zatm nat) (lam a (coprod_rect (lam w nat) (lam u 0) (lam n n) a)))
 (defn ztre (pi d nat UU)
   (lam d (nat_rect (lam q UU) zatm (lam k (lam T (coprod zatm (total2 x T T)))) d)))
 (defn zfst (pi k nat (pi p (total2 x (ztre k) (ztre k)) (ztre k))) (lam k (lam p (pr1 p))))
 (defn zsnd (pi k nat (pi p (total2 x (ztre k) (ztre k)) (ztre k))) (lam k (lam p (pr2 p))))
 (defn znetd (pi d nat (pi t (ztre d) nat))
   (lam d (nat_rect (lam q (pi t (ztre q) nat))
             (lam t (zanet t))
             (lam k (lam IH (lam t (coprod_rect (lam w nat)
                                      (lam m (zanet m))
                                      (lam p (add (IH (zfst k p)) (IH (zsnd k p))))
                                      t))))
             d)))
 ; max, verbatim off uuval: both corners reduce definitionally
 (defn zmx2 (pi a nat (pi b nat nat))
   (lam a (nat_rect (lam q (pi b nat nat))
             (lam b b)
             (lam j (lam IH (lam b (nat_rect (lam r nat)
                                      (succ j) (lam k (lam s (succ (IH k)))) b))))
             a)))
 ; --- THE ONE NEW DEFINITION: the height a tree actually has ---
 (defn zht (pi d nat (pi t (ztre d) nat))
   (lam d (nat_rect (lam q (pi t (ztre q) nat))
             (lam t 0)
             (lam k (lam IH (lam t (coprod_rect (lam w nat)
                                      (lam a 0)
                                      (lam p (succ (zmx2 (IH (zfst k p)) (IH (zsnd k p)))))
                                      t))))
             d)))
 ; ..and the carrier that pins the index to it. the tightness is a path in nat,
 ; hence a proposition, hence invisible to equality -- that is the whole trick
 (defn zbare UU (total2 d nat (ztre d)))
 (defn ztight (pi p zbare UU) (lam p (paths nat (zht (pr1 p) (pr2 p)) (pr1 p))))
 (defn zval UU (total2 p zbare (ztight p)))
 (defn znum (pi n nat zval) (lam n (tpair (tpair 0 (ii2 n)) (idpath 0))))
 (defn znilv zval (tpair (tpair 0 (ii1 tt)) (idpath 0)))
 (defn znet (pi v zval nat) (lam v (znetd (pr1 (pr1 v)) (pr2 (pr1 v))))))

; --- the pads, verbatim off uuval: a link still meets at the taller height ---
(assert
 (defn zleafat (pi d nat (pi m zatm (ztre d)))
   (lam d (nat_rect (lam q (pi m zatm (ztre q)))
             (lam m m) (lam k (lam IH (lam m (ii1 m)))) d)))
 (defn zpadl (pi a nat (pi b nat (pi t (ztre a) (ztre (zmx2 a b)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (ztre q) (ztre (zmx2 q b)))))
             (lam b (lam m (zleafat b m)))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (ztre (succ j)) (ztre (zmx2 (succ j) r))))
                   (lam t t)
                   (lam k (lam s (lam t
                      (coprod_rect (lam w (ztre (succ (zmx2 j k))))
                         (lam m (ii1 m))
                         (lam p (ii2 (tpair (IH k (zfst j p)) (IH k (zsnd j p)))))
                         t))))
                   b))))
             a)))
 (defn zpadr (pi a nat (pi b nat (pi t (ztre b) (ztre (zmx2 a b)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (ztre b) (ztre (zmx2 q b)))))
             (lam b (lam t t))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (ztre r) (ztre (zmx2 (succ j) r))))
                   (lam m (zleafat (succ j) m))
                   (lam k (lam s (lam t
                      (coprod_rect (lam w (ztre (succ (zmx2 j k))))
                         (lam m (ii1 m))
                         (lam p (ii2 (tpair (IH k (zfst k p)) (IH k (zsnd k p)))))
                         t))))
                   b))))
             a))))

; --- what the pin costs: the pad must not move the height. two inductions,
; --- the same shape the pads have, and a two-argument congruence for max ---
(assert
 (defq zs-mx2-cong
   (pi a nat (pi a2 nat (pi b nat (pi b2 nat
      (pi p (paths nat a a2) (pi q (paths nat b b2)
         (paths nat (zmx2 a b) (zmx2 a2 b2))))))))
   (lam a (lam a2 (lam b (lam b2 (lam p (lam q
      (pathscomp0 nat (zmx2 a b) (zmx2 a2 b) (zmx2 a2 b2)
         (maponpaths nat nat (lam w (zmx2 w b)) a a2 p)
         (maponpaths nat nat (lam w (zmx2 a2 w)) b b2 q)))))))))
 (defq zs-ht-leafat (pi d nat (pi m zatm (paths nat (zht d (zleafat d m)) 0)))
   (lam d (nat_rect (lam q (pi m zatm (paths nat (zht q (zleafat q m)) 0)))
             (lam m (idpath 0))
             (lam k (lam IH (lam m (idpath 0))))
             d)))
 (defq zs-ht-padl
   (pi a nat (pi b nat (pi t (ztre a)
      (paths nat (zht (zmx2 a b) (zpadl a b t)) (zht a t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (ztre q)
                              (paths nat (zht (zmx2 q b) (zpadl q b t)) (zht q t)))))
             (lam b (lam m (zs-ht-leafat b m)))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (ztre (succ j))
                             (paths nat (zht (zmx2 (succ j) r) (zpadl (succ j) r t))
                                        (zht (succ j) t))))
                   (lam t (idpath (zht (succ j) t)))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths nat (zht (succ (zmx2 j k)) (zpadl (succ j) (succ k) w))
                                           (zht (succ j) w)))
                         (lam m (idpath 0))
                         (lam p ((maponpaths nat nat (lam x (succ x))
                                   (zmx2 (zht (zmx2 j k) (zpadl j k (zfst j p)))
                                         (zht (zmx2 j k) (zpadl j k (zsnd j p))))
                                   (zmx2 (zht j (zfst j p)) (zht j (zsnd j p))))
                                 (zs-mx2-cong
                                    (zht (zmx2 j k) (zpadl j k (zfst j p))) (zht j (zfst j p))
                                    (zht (zmx2 j k) (zpadl j k (zsnd j p))) (zht j (zsnd j p))
                                    (IH k (zfst j p)) (IH k (zsnd j p)))))
                         t))))
                   b))))
             a)))
 (defq zs-ht-padr
   (pi a nat (pi b nat (pi t (ztre b)
      (paths nat (zht (zmx2 a b) (zpadr a b t)) (zht b t)))))
   (lam a (nat_rect (lam q (pi b nat (pi t (ztre b)
                              (paths nat (zht (zmx2 q b) (zpadr q b t)) (zht b t)))))
             (lam b (lam t (idpath (zht b t))))
             (lam j (lam IH (lam b
                (nat_rect (lam r (pi t (ztre r)
                             (paths nat (zht (zmx2 (succ j) r) (zpadr (succ j) r t))
                                        (zht r t))))
                   (lam m (zs-ht-leafat (succ j) m))
                   (lam k (lam s (lam t
                      (coprod_rect
                         (lam w (paths nat (zht (succ (zmx2 j k)) (zpadr (succ j) (succ k) w))
                                           (zht (succ k) w)))
                         (lam m (idpath 0))
                         (lam p ((maponpaths nat nat (lam x (succ x))
                                   (zmx2 (zht (zmx2 j k) (zpadr j k (zfst k p)))
                                         (zht (zmx2 j k) (zpadr j k (zsnd k p))))
                                   (zmx2 (zht k (zfst k p)) (zht k (zsnd k p))))
                                 (zs-mx2-cong
                                    (zht (zmx2 j k) (zpadr j k (zfst k p))) (zht k (zfst k p))
                                    (zht (zmx2 j k) (zpadr j k (zsnd k p))) (zht k (zsnd k p))
                                    (IH k (zfst k p)) (IH k (zsnd k p)))))
                         t))))
                   b))))
             a))))

; --- and so the link, carrying its own tightness ---
(assert
 (defq zs-tight-link
   (pi a nat (pi b nat (pi t (ztre a) (pi s (ztre b)
      (pi ht (paths nat (zht a t) a) (pi hs (paths nat (zht b s) b)
         (paths nat (zht (succ (zmx2 a b)) (ii2 (tpair (zpadl a b t) (zpadr a b s))))
                    (succ (zmx2 a b)))))))))
   (lam a (lam b (lam t (lam s (lam ht (lam hs
      ((maponpaths nat nat (lam x (succ x))
          (zmx2 (zht (zmx2 a b) (zpadl a b t)) (zht (zmx2 a b) (zpadr a b s)))
          (zmx2 a b))
       (zs-mx2-cong (zht (zmx2 a b) (zpadl a b t)) a
                    (zht (zmx2 a b) (zpadr a b s)) b
                    (pathscomp0 nat (zht (zmx2 a b) (zpadl a b t)) (zht a t) a
                       (zs-ht-padl a b t) ht)
                    (pathscomp0 nat (zht (zmx2 a b) (zpadr a b s)) (zht b s) b
                       (zs-ht-padr a b s) hs))))))))))
 (defn zlink (pi u zval (pi w zval zval))
   (lam u (lam w
      (tpair (tpair (succ (zmx2 (pr1 (pr1 u)) (pr1 (pr1 w))))
                    (ii2 (tpair (zpadl (pr1 (pr1 u)) (pr1 (pr1 w)) (pr2 (pr1 u)))
                                (zpadr (pr1 (pr1 u)) (pr1 (pr1 w)) (pr2 (pr1 w))))))
             (zs-tight-link (pr1 (pr1 u)) (pr1 (pr1 w)) (pr2 (pr1 u)) (pr2 (pr1 w))
                            (pr2 u) (pr2 w)))))))

; --- THE PAYOFF. uuval's uv-veq-link is thirty lines down through one pad,
; --- across by the hypothesis and back up the other, and it only composes
; --- because veq was made transitive first. here it is two maponpaths ---
(assert
 (defq zs-link-cong
   (pi u zval (pi u2 zval (pi w zval (pi w2 zval
      (pi p (paths zval u u2) (pi q (paths zval w w2)
         (paths zval (zlink u w) (zlink u2 w2))))))))
   (lam u (lam u2 (lam w (lam w2 (lam p (lam q
      (pathscomp0 zval (zlink u w) (zlink u2 w) (zlink u2 w2)
         (maponpaths zval zval (lam x (zlink x w)) u u2 p)
         (maponpaths zval zval (lam x (zlink u2 x)) w w2 q)))))))))
 ; ..and it is free for EVERY function, not just the link: net too
 (defq zs-net-cong
   (pi u zval (pi w zval (pi p (paths zval u w) (paths nat (znet u) (znet w)))))
   (lam u (lam w (lam p (maponpaths zval nat znet u w p))))))

; --- the other half of the trick: the tightness is a PROP, so two values agree
; --- as soon as their trees do. this is what makes paths the right equality ---
(assert
 (defq zs-propirr (pi X UU (pi i (isaprop X) (pi x X (pi y X (paths X x y)))))
   (lam X (lam i (lam x (lam y (pr1 (i x y)))))))
 (defq zs-tightirr
   (pi p zbare (pi h1 (ztight p) (pi h2 (ztight p) (paths (ztight p) h1 h2))))
   (lam p (lam h1 (lam h2
      (zs-propirr (paths nat (zht (pr1 p) (pr2 p)) (pr1 p))
         (isasetnat (zht (pr1 p) (pr2 p)) (pr1 p)) h1 h2)))))
 (defq zs-valpath
   (pi u zval (pi w zval (pi e (paths zbare (pr1 u) (pr1 w)) (paths zval u w))))
   (lam u (lam w (lam e
      (total2_paths_f zbare ztight u w e
         (zs-tightirr (pr1 w)
            (transportf zbare ztight (pr1 u) (pr1 w) e (pr2 u))
            (pr2 w))))))))

; --- and it COMPUTES: the index of a built value is its height, on the nose ---
(assert
 (= 0 (vof 0 '(pr1 (pr1 (znum 7)))))
 (= 1 (vof 0 '(pr1 (pr1 (zlink (znum 1) (znum 2))))))
 ; a lopsided link takes the taller side and no more -- the bound is gone
 (= 2 (vof 0 '(pr1 (pr1 (zlink (zlink (znum 1) (znum 2)) (znum 3))))))
 (= 2 (vof 0 '(pr1 (pr1 (zlink (znum 3) (zlink (znum 1) (znum 2)))))))
 (= 3 (vof 0 '(pr1 (pr1 (zlink (zlink (zlink (znum 1) (znum 2)) (znum 3)) (znum 4))))))
 ; ..and the fold still reads the tree
 (= 10 (vof 0 '(znet (zlink (zlink (zlink (znum 1) (znum 2)) (znum 3)) (znum 4)))))
 (= 0 (vof 0 '(znet znilv)))
 ; the height a value reports IS the index it carries, by its own proof
 (= 3 (vof 0 '(zht (pr1 (pr1 (zlink (zlink (zlink (znum 1) (znum 2)) (znum 3)) (znum 4))))
                   (pr2 (pr1 (zlink (zlink (zlink (znum 1) (znum 2)) (znum 3)) (znum 4))))))))

; --- planted faults. the one that matters is the FIRST: a leaf spelled at
; --- index 1 is a legal value of uuval's carrier and is REFUSED here, which is
; --- the many-spellings defect turned into a type error ---
(hear (\ p e (: h (peep HELPC () 0) (? h (h p e) 0))))
(assert
 (id? 'accepted (rejects (\ z (defn zs-ok-leaf zval (tpair (tpair 0 (ii2 7)) (idpath 0))))))
 (id? 'rejected (rejects (\ z (defn zs-bad-leaf zval (tpair (tpair 1 (ii1 (ii2 7))) (idpath 1))))))
 ; ..and the index a link reports is the max, not the sum: the pad is not a level
 (id? 'accepted (rejects (\ z (defn zs-ok-idx
    (paths nat (pr1 (pr1 (zlink (zlink (znum 1) (znum 2)) (znum 3)))) 2) (idpath 2)))))
 (id? 'rejected (rejects (\ z (defn zs-bad-idx
    (paths nat (pr1 (pr1 (zlink (zlink (znum 1) (znum 2)) (znum 3)))) 3) (idpath 3)))))
 ; the congruence takes BOTH limbs -- one alone is not enough, here either
 (id? 'rejected (rejects (\ z (defq zs-bad-cong
    (pi u zval (pi u2 zval (pi w zval (pi w2 zval
       (pi p (paths zval u u2) (paths zval (zlink u w) (zlink u2 w2)))))))
    (lam u (lam u2 (lam w (lam w2 (lam p (idpath (zlink u w)))))))))))
 ; and the pad really does not move the height -- the lemma the pin rests on
 (id? 'accepted (rejects (\ z (defq zs-ok-padht
    (pi a nat (pi b nat (pi t (ztre a)
       (paths nat (zht (zmx2 a b) (zpadl a b t)) (zht a t)))))
    zs-ht-padl))))
 (id? 'rejected (rejects (\ z (defq zs-bad-padht
    (pi a nat (pi b nat (pi t (ztre a)
       (paths nat (zht (zmx2 a b) (zpadl a b t)) (zmx2 a b)))))
    zs-ht-padl)))))
