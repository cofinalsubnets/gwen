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
; --- ii1 lifts a tree one level, ii2 pairs two -- love's link. net folds it ---
(assert
 (defn vtre (pi d nat UU)
   (lam d (nat_rect (lam q UU) msr (lam k (lam T (coprod T (total2 x T T)))) d)))
 (defn netd (pi d nat (pi t (vtre d) msr))
   (lam d (nat_rect (lam q (pi t (vtre q) msr))
             (lam t t)
             (lam k (lam IH (lam t (coprod_rect (lam w msr)
                                      (lam u (IH u))
                                      (lam p (madd (IH (pr1 p)) (IH (pr2 p))))
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

; --- net is a HOMOMORPHISM: it ignores a lift and it sums over a link ---
(assert
 (defq uv-net-lift
   (pi d nat (pi t (vtre d) (paths msr (netd (succ d) (ii1 t)) (netd d t))))
   (lam d (lam t (idpath (netd d t)))))
 (defq uv-net-link
   (pi d nat (pi a (vtre d) (pi b (vtre d)
     (paths msr (netd (succ d) (ii2 (tpair a b))) (madd (netd d a) (netd d b))))))
   (lam d (lam a (lam b (idpath (madd (netd d a) (netd d b))))))))

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
      (vof 0 '(vceil (vnum (tpair (zrea (nzed 3)) (zrea (nzed 4))))))))

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
