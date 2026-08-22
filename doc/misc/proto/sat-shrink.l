; sat-shrink.l -- the all-UIP learnt-SHRINKING experiment: built, measured, KEPT OUT.
; (the sequel to doc/misc/proto/sat-watcher-vectors.l: an honest negative result, preserved.)
;
; cadical's search-side ablation on the factored-php8 encoding named shrink its
; single biggest feature (+61% conflicts without it). replicated here as a twin
; phase (fconf5 = fconf3 + phase 3c between minimize and the reason bump): per
; decision-level block of >= 2 learnt literals, walk that level's trail segment
; resolving reasons; if the block converges to the level's UIP without pulling in
; any unseen outside-level literal, the whole block is replaced by that one lit.
;
; MEASURED (2026-07-02, conflict counts via the twin, native bcp/dec):
;   uuf150-01: 3652 -> 3397   (-7%)
;   uuf100-01:  548 ->  493   (-10%)
;   php8 (fbva level-1): 1547 -> 1779   (+15% -- WORSE)
; verdict: in OUR stack (reason-side bumping already in) the marginal value is a
; single-digit win on random UNSAT and a regression on factored pigeonhole --
; nothing like the bump's 2.6x, and not worth a kernel phase + per-conflict cost.
; cadical's +61% is shrink's value inside ITS ensemble (chrono, stable phases, its
; restart policy), not a transferable constant. the stronger variant (recursively
; minimizing the outside-level pulls instead of failing) is the open follow-up.
;
; run: cat crew/sat/sat.l crew/sat/flat.l doc/misc/proto/sat-shrink.l - <<< "(: _ ... (pin s 'conf fconf5) ...)"
   (: (fconf5 fx ar wn confl)
    (: nv (getw fx 3) lvo (lvo-of nv) rso (rso-of nv) who (who-of nv)
       vao (vao-of nv) aco (aco-of nv) pho (pho-of nv) seo (seo-of nv)
       dl (getw fx 2)
       (? (= dl 0) -1
        (: cid0 (getw fx 4)
           czero (\ _ ((: (go v) (? (> v nv) 0 (: _ (putw fx (+ seo v) 0) (go (+ v 1))))) 0))
           _ (czero ())
           abs1 (\ cs skipv counter widx                    ; absorb clause at cs -> (counter . widx)
            ((: (go j c w)
                (? (> j (+ cs (getw ar cs))) (link c w)
                   (: el (getw ar j) v (>> el 1)
                      (? (= v skipv) (go (+ j 1) c w)
                         (! (= 0 (getw fx (+ seo v)))) (go (+ j 1) c w)
                         (= 0 (getw fx (+ lvo v))) (go (+ j 1) c w)
                         (: _ (putw fx (+ seo v) 1)
                            _ (putw fx (+ aco v) (+ (getw fx (+ aco v)) (getw fx 6)))   ; bump
                            (? (= (getw fx (+ lvo v)) dl) (go (+ j 1) (+ c 1) w)
                               (: _ (putw ar w el) (go (+ j 1) c (+ w 1)))))))))
             (+ cs 1) counter widx))
           a0 (abs1 confl 0 0 (+ cid0 2))
           r ((: (walk counter widx t)
                 (: np ((: (fs i) (? (= 0 (getw fx (+ seo (>> (getw fx (+ 8 i)) 1)))) (fs (- i 1)) i)) t)
                    el (getw fx (+ 8 np)) v (>> el 1)
                    _ (putw fx (+ seo v) 0) c (- counter 1)
                    (? (= c 0) (link el widx)
                       (: a (abs1 (- (getw fx (+ rso v)) 1) v c widx)
                          (walk (cap a) (cup a) (- np 1))))))
              (cap a0) (cup a0) (- (getw fx 0) 1))
           uip (cap r) wend (cup r)
           _ (putw ar (+ cid0 1) (^ uip 1))
           wr ((: (mn rd wr)                                ; minimize, compacting kept lits down
                  (? (>= rd wend) wr
                     (: el (getw ar rd) v (>> el 1) rv (getw fx (+ rso v))
                        red (? (= rv 0) 0
                               ((: (ck j) (? (> j (+ (- rv 1) (getw ar (- rv 1)))) 1
                                             (: v2 (>> (getw ar j) 1)
                                                (? (= v2 v) (ck (+ j 1))
                                                   (= 0 (getw fx (+ lvo v2))) (ck (+ j 1))
                                                   (! (= 0 (getw fx (+ seo v2)))) (ck (+ j 1))
                                                   0)))) (+ rv 0)))     ; j starts at (rv-1)+1 = rv
                        (? (= red 1) (mn (+ rd 1) wr)
                           (: _ (putw ar wr el) (mn (+ rd 1) (+ wr 1)))))))
               (+ cid0 2) (+ cid0 2))
           wr (: (shrink1 L wr)
              (: sm (tablet 0)
                 cnt ((: (go j c) (? (>= j wr) c
                        (: v (>> (getw ar j) 1)
                           (? (= L (getw fx (+ lvo v))) (: _ (pin sm v 1) (go (+ j 1) (+ c 1)))
                              (go (+ j 1) c)))))
                      (+ cid0 2) 0)
                 (? (< cnt 2) wr
                    (: hi ((: (fh t) (? (> (getw fx (+ lvo (>> (getw fx (+ 8 t)) 1))) L) (fh (- t 1)) t))
                           (- (getw fx 0) 1))
                       el ((: (walk t open)
                             (: e0 (getw fx (+ 8 t)) v (>> e0 1)
                                (? (! (= L (getw fx (+ lvo v)))) 0
                                   (= 0 (peep sm v 0)) (walk (- t 1) open)
                                   (= open 1) e0
                                   (: rv (getw fx (+ rso v))
                                      (? (= rv 0) 0
                                         (: fresh ((: (go2 k acc)
                                                      (? (> k (+ (- rv 1) (getw ar (- rv 1)))) acc
                                                         (: e (getw ar k) u (>> e 1) Lu (getw fx (+ lvo u))
                                                            (? (= u v) (go2 (+ k 1) acc)
                                                               (= Lu 0) (go2 (+ k 1) acc)
                                                               (= Lu L) (? (= 0 (peep sm u 0))
                                                                           (: _ (pin sm u 1) (go2 (+ k 1) (+ acc 1)))
                                                                           (go2 (+ k 1) acc))
                                                               (! (= 0 (getw fx (+ seo u)))) (go2 (+ k 1) acc)
                                                               -1))))
                                                   rv 0)
                                            (? (= fresh -1) 0
                                               (walk (- t 1) (+ open (- fresh 1))))))))))
                           hi cnt)
                       (? (= el 0) wr
                          (: nw ((: (cp j w) (? (>= j wr) w
                                    (: lt (getw ar j) v2 (>> lt 1)
                                       (? (= L (getw fx (+ lvo v2))) (cp (+ j 1) w)
                                          (: _ (putw ar w lt) (cp (+ j 1) (+ w 1)))))))
                                 (+ cid0 2) (+ cid0 2))
                             _ (putw ar nw (^ el 1))
                             _ (putw fx (+ seo (>> el 1)) 1)
                             (+ nw 1))))))
                 lvls ((: (go j acc) (? (>= j wr) acc
                          (: L (getw fx (+ lvo (>> (getw ar j) 1)))
                             (go (+ j 1) (? (member? L acc) acc (link L acc))))))
                       (+ cid0 2) ())
                 ((: (sa ls w) (? (atom? ls) w (sa (cup ls) (shrink1 (cap ls) w)))) lvls wr))
           ; reason-side bump (cadical's bumpreason): every UNSEEN var in a learnt
           ; literal's reason gets an activity bump -- focuses VSIDS one step past the
           ; learnt, the decisive lever on factored/tree instances (php8 ~4000 -> ~1500
           ; conflicts). seen still holds the analysis marks here (czero follows).
           _ ((: (rb j) (? (>= j wr) 0
                  (: v (>> (getw ar j) 1) rv (getw fx (+ rso v))
                     _ (? (= rv 0) 0
                          ((: (go2 k) (? (> k (+ (- rv 1) (getw ar (- rv 1)))) 0
                               (: v2 (>> (getw ar k) 1)
                                  _ (? (= 0 (getw fx (+ seo v2)))
                                       (putw fx (+ aco v2) (+ (getw fx (+ aco v2)) (getw fx 6))) 0)
                                  (go2 (+ k 1))))) rv))
                     (rb (+ j 1))))) (+ cid0 1))
           _ (czero ())
           gb ((: (go j glue bj)                            ; glue + backjump level over the kept tail
                  (? (>= j wr) (link glue bj)
                     (: L (getw fx (+ lvo (>> (getw ar j) 1)))
                        bj2 (? (> L bj) L bj)
                        (? (= 0 (getw fx (+ seo L)))
                           (: _ (putw fx (+ seo L) 1) (go (+ j 1) (+ glue 1) bj2))
                           (go (+ j 1) glue bj2)))))
               (+ cid0 2) (: _ (putw fx (+ seo dl) 1) 1) 0)
           glue (cap gb) bj (cup gb)
           nt ((: (go t)                                    ; backjump: pop above bj, save phase
                  (? (= t 0) 0
                     (: el (getw fx (+ 8 (- t 1))) v (>> el 1)
                        (? (> (getw fx (+ lvo v)) bj)
                           (: _ (putw fx (+ pho v) (+ 1 (& el 1)))
                              _ (putw fx (+ vao v) 0) _ (putw fx (+ lvo v) 0) _ (putw fx (+ rso v) 0)
                              (go (- t 1)))
                           t))))
               (getw fx 0))
           _ (putw fx 0 nt) _ (putw fx 1 nt) _ (putw fx 2 bj)
           size (- wr (+ cid0 1))
           _ (putw ar cid0 size) _ (putw fx 4 wr)
           _ (? (< size 2) 0
                (: mx ((: (go j b) (? (>= j wr) b                       ; reorder: max-level tail lit to slot 2
                              (go (+ j 1) (? (> (getw fx (+ lvo (>> (getw ar j) 1)))
                                               (getw fx (+ lvo (>> (getw ar b) 1)))) j b))))
                       (+ cid0 3) (+ cid0 2))
                   a (getw ar (+ cid0 2)) b (getw ar mx)
                   _ (putw ar (+ cid0 2) b) _ (putw ar mx a)
                   l0 (getw ar (+ cid0 1)) l1 (getw ar (+ cid0 2)) wnu (getw fx 5)
                   _ (putw wn wnu cid0) _ (putw wn (+ wnu 1) (getw fx (+ who l0))) _ (putw fx (+ who l0) wnu)
                   _ (putw wn (+ wnu 2) cid0) _ (putw wn (+ wnu 3) (getw fx (+ who l1))) _ (putw fx (+ who l1) (+ wnu 2))
                   (putw fx 5 (+ wnu 4))))
           _ (fasg fx (getw ar (+ cid0 1)) (+ cid0 1))
           (| (<< cid0 6) (? (> glue 63) 63 glue)))))
   )
