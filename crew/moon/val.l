; crew/moon/val.l -- the VALUE layer (moon-ssa rung 0): cfg, escape census,
; per-def chains and interval liveness over final forms. an instrument today,
; the allocator's substrate later; touches no codegen. the python twin is
; doc/misc/proto/ssagap/ (two implementations, one answer -- differ.sh).
; the determinism law: every list a v-fn answers is sorted, and no answer may
; depend on tablet iteration order (joins are unions, keys sort before walks).
; a frame op that misses the base+const shape reads everything -- the floor:
; every count downstream is conservative, never flattering.
(module 'moon
(:
   voff 4294967296              ; offset shift: encoded keys stay positive for // and %

   ; -- op families: width in bytes, 0 = not in the family --
   (vldw x) (x @ 'ld 8 'ld4 4 'ldu4 4 'ld2 2 'ldu2 2 'ld1 1 'ldu1 1 0)
   (vstw x) (x @ 'st 8 'st4 4 'st2 2 'st1 1 0)
   (vsiw x) (x @ 'si 8 'si4 4 'si2 2 'si1 1 0)
   (valu? x) (member? x '(add sub and or xor imul))
   (vbase? x) (x = 'sp || x = 'r4)
   (vsbit x) (x = 'sp ? 0 1)
   (vspc b) (b = 0 ? 'sp 'r4)
   ; ops that neither read nor write frame memory
   (vpure? x) (member? x '(ret br label li mov cmp test push pop set la ud2
                           add sub and or xor imul shl shr sar ror4
                           sx4 zx4 sx2 zx2 sx1 zx1 not neg))

   ; -- sorted-int-list sets --
   (vun a b) (? !(two? a) b !(two? b) a
                (<a < <b) (<a >< vun >a b)
                (<b < <a) (<b >< vun a >b)
                (<a >< vun >a >b))
   (vsans a b) (? !(two? a) () !(two? b) a
                  (<a < <b) (<a >< vsans >a b)
                  (<b < <a) (vsans a >b)
                  (vsans >a >b))
   (vboth a b) (? (!(two? a) || !(two? b)) ()
                  (<a < <b) (vboth >a b)
                  (<b < <a) (vboth a >b)
                  (<a >< vboth >a >b))
   (vapp a b) (? !(two? a) b (<a >< vapp >a b))
   (vall? x) (x = 'all)
   (vsz x) (? (vall? x) -1 (tally x))
   (vuq l) (? !(two? l) () (: r (vuq >l) (? (two? r && <l = <r) r (<l >< r))))
   (vany p l) (? !(two? l) 0 (p <l) 1 (vany p >l))
   (vkeep p l) (? !(two? l) () (p <l) (<l >< vkeep p >l) (vkeep p >l))
   (vtake n l) (? ((n < 1) || !(two? l)) () (<l >< vtake (n - 1) >l))
   (vat k l) (? (k < 1) <l (vat (k - 1) >l))

   ; -- indexing --
   (vtab l) (: t {} (go l i) (? !(two? l) t (: _ (pin t i <l) (go >l (i + 1)))) (go l 0))
   (vdot? s) (46 = (show s) 0)
   (vlabs bd) (: t {}
                 (go l i) (? !(two? l) t
                             (: f <l
                                _ (? (two? f && <f = 'label) (pin t <>f i) 0)
                                (go >l (i + 1))))
                 (go bd 0))

   ; -- a TU's forms into functions: a non-dot label opens one, align is glue --
   (vfns fs)
    (: (shut nm bd acc) (nm = () ? acc ([nm (rev bd)] >< acc))
       (go l nm bd acc)
        (? !(two? l) (rev (shut nm bd acc))
           (: f <l
              (? (two? f && <f = 'label && !(vdot? <>f)) (go >l <>f () (shut nm bd acc))
                 (two? f && <f = 'align) (go >l nm bd acc)
                 (nm = ()) (go >l nm bd acc)
                 (go >l nm (f >< bd) acc))))
       (go fs () () ()))

   ; -- escape census: spaces whose address leaks --
   (vesc bd)
    (: t {}
       (mark l) (? !(two? l) 0 (: _ (? (vbase? <l) (pin t <l 1) 0) (mark >l)))
       (go l)
        (? !(two? l) t
           (: f <l
              _ (? !(two? f) 0
                   (: op <f
                      (? (member? op '(lea lean leax)) (mark >f)
                         (0 < vldw op || 0 < vstw op || 0 < vsiw op
                          || member? op '(ldsd stsd cmpm cmprm ldss stss cvtsi2sd movqxr movqrx)) 0
                         (valu? op && <>f = 'sp) 0
                         (mark >f))))
              (go >l)))
       (go bd))

   ; -- back-edge spans and the loop weight 8^min(depth,3) --
   (vspans bd)
    (: lt (vlabs bd)
       (go l i acc)
        (? !(two? l) (rev acc)
           (: f <l
              j (? !(two? f) -1
                   (<f = 'jmp) (peep lt <>f -1)
                   (<f = 'br) (peep lt <>>f -1)
                   -1)
              (go >l (i + 1) (? (0 <= j && j < i) ([j i] >< acc) acc))))
       (go bd 0 ()))
   (vwt sp i)
    (: (dep l d) (? !(two? l) d
                    (: s <l (dep >l (? (<s <= i && i <= <>s) (d + 1) d))))
       d (dep sp 0)
       ((? (3 < d) 3 d) 8))

   ; -- successors per index --
   (vsucc bd)
    (: lt (vlabs bd) n (tally bd) t {}
       (go l i)
        (? !(two? l) t
           (: f <l
              nx (? ((i + 1) < n) [(i + 1)] ())
              ss (? !(two? f) nx
                    (: op <f
                       (? (op = 'jmp) (: j (peep lt <>f -1) (? (j < 0) () [j]))
                          (op = 'br) (: j (peep lt <>>f -1) (? (j < 0) nx (j >< nx)))
                          (member? op '(ret jmpr ud2)) ()
                          nx)))
              _ (pin t i ss)
              (go >l (i + 1))))
       (go bd 0))

   ; -- forward sp cursor: reach from entry, first visit wins (the oracle's walk) --
   (vcur bt n succ)
    (: t {}
       (go wk)
        (? !(two? wk) t
           (: p <wk i <p c <>p
              (? (n <= i || charm? (peep t i ())) (go >wk)
                 (: _ (pin t i c)
                    f (peep bt i ())
                    c2 (? !(two? f) c
                          (: op <f
                             (? (valu? op && <>f = 'sp && <>>f = 'sp && charm? <>>>f)
                                 (op = 'add ? (c + <>>>f) (c - <>>>f))
                                (op = 'push) (c - 8)
                                (op = 'pop) (c + 8)
                                c)))
                    (go (vapp (rev (map (\ j [j c2]) (peep succ i ()))) >wk))))))
       (go [[0 0]]))

   ; -- one form's frame bytes: [reads writes all?], keys (off*2 + space bit) --
   (vbytes b o w)
    (: sb (vsbit b)
       (go k acc) (? (k < 0) acc (go (k - 1) (((o + k) * 2 + sb) >< acc)))
       (go (w - 1) ()))
   (vrw f c lt)
    (? !(two? f) [() () 0]
       (: op <f
          lw (vldw op) sw (vstw op) iw (vsiw op)
          (? (0 < lw) ((vbase? <>>f && charm? <>>>f) ? [(vbytes <>>f (c + <>>>f) lw) () 0] [() () 1])
             (member? op '(ldsd ldss)) ((vbase? <>>f && charm? <>>>f) ? [(vbytes <>>f (c + <>>>f) 8) () 0] [() () 1])
             (0 < sw) ((vbase? <>f && charm? <>>f) ? [() (vbytes <>f (c + <>>f) sw) 0] [() () 1])
             (0 < iw) ((vbase? <>f && charm? <>>f) ? [() (vbytes <>f (c + <>>f) iw) 0] [() () 1])
             (member? op '(stsd stss)) ((vbase? <>f && charm? <>>f) ? [() (vbytes <>f (c + <>>f) 8) 0] [() () 1])
             (member? op '(cmpm cmprm call callr sys raw jmpr)) [() () 1]
             (op = 'jmp) ((0 <= peep lt <>f -1) ? [() () 0] [() () 1])
             (vpure? op) [() () 0]
             [() () 1])))

   ; -- backward byte liveness to the fixpoint. () nothing, 'all the top.
   ; monotone, so a set's tally is the change detector; the least fixpoint is
   ; sweep-order-free.
   (vlive bt n succ cur lt escn)
    (: lv {}
       rwt (: t {}
              (go i) (? (n <= i) t
                        (: _ (pin t i (vrw (peep bt i ()) (peep cur i 0) lt))
                           (go (i + 1))))
              (go 0))
       (out ss seed)
        (? (vall? seed) 'all
           !(two? ss) seed
           (: s (peep lv <ss ())
              (? (vall? s) 'all (out >ss (vun seed s)))))
       (sweep chg i)
        (? (i < 0) chg
           (: f (peep bt i ())
              o0 (? ((two? f && member? <f '(ret jmpr sys ud2)) && escn) 'all ())
              ou (out (peep succ i ()) o0)
              rw (peep rwt i ())
              nin (? ((0 < <>>rw) || vall? ou) 'all (vun (vsans ou <>rw) <rw))
              osz (vsz (peep lv i ()))
              _ (pin lv i nin)
              (sweep (? (osz = vsz nin) chg 1) (i - 1))))
       (fix _) (? (0 < sweep 0 (n - 1)) (fix 0) lv)
       (fix 0))

   ; -- the census walk (LINEAR cursor, the oracle's promo model): touch table
   ; ((off+voff)*2+sb)*16+w -> (idx op) list newest-first, unpromotable cells,
   ; call indices. 'skip when an unmodeled base use turns the fn conservative.
   (vmb? f) (: (go l) (? !(two? l) 0 (vbase? <l) 1 (go >l)) (go >f))
   (vtk sb o w) ((((o + voff) * 2 + sb) * 16) + w)
   (vdek k) (: w (k % 16) r (k // 16) [(r % 2) ((r // 2) - voff) w])
   (vtouch bd)
    (: tt {} bad {}
       (go l i c ci)
        (? !(two? l) [tt bad (rev ci)]
           (: f <l
              (? !(two? f) (go >l (i + 1) c ci)
                 (: op <f
                    lw (vldw op) sw (vstw op) iw (vsiw op)
                    (? (valu? op && <>f = 'sp && <>>f = 'sp && charm? <>>>f)
                        (go >l (i + 1) (op = 'add ? (c + <>>>f) (c - <>>>f)) ci)
                       (op = 'push) (go >l (i + 1) (c - 8) ci)
                       (op = 'pop) (go >l (i + 1) (c + 8) ci)
                       (0 < lw && vbase? <>>f && charm? <>>>f)
                        (: k (vtk (vsbit <>>f) (c + <>>>f) lw)
                           _ (pin tt k ([i op] >< peep tt k ()))
                           (go >l (i + 1) c ci))
                       (0 < sw && vbase? <>f && charm? <>>f)
                        (: k (vtk (vsbit <>f) (c + <>>f) sw)
                           _ (pin tt k ([i op] >< peep tt k ()))
                           (go >l (i + 1) c ci))
                       (0 < iw && vbase? <>f && charm? <>>f)
                        (: k (vtk (vsbit <>f) (c + <>>f) iw)
                           _ (pin tt k ([i op] >< peep tt k ()))
                           (go >l (i + 1) c ci))
                       (member? op '(ldsd ldss))
                        (: _ (? (vbase? <>>f && charm? <>>>f)
                                (pin bad ((c + <>>>f + voff) * 2 + vsbit <>>f) 1) 0)
                           (go >l (i + 1) c ci))
                       (member? op '(stsd stss))
                        (: _ (? (vbase? <>f && charm? <>>f)
                                (pin bad ((c + <>>f + voff) * 2 + vsbit <>f) 1) 0)
                           (go >l (i + 1) c ci))
                       (vmb? f && !(member? op '(label jmp br ret call li mov cmp test la))) 'skip
                       (go >l (i + 1) c
                           (? (member? op '(call callr sys)) (i >< ci) ci)))))))
       (go bd 0 0 ()))

   ; -- cells: the touches grouped 8-aligned per space, with the census flags
   ; and the def/use split the chains read. rows sort by (base, space).
   ; each: [sb base full across si nar multi nt wsum defs uses]
   (vflr8 o) (: r (o % 8) (o - (r < 0 ? (r + 8) r)))
   (vcls tch spans esc)
    (? (tch = 'skip) ()
       (: tt <tch bad <>tch ci <>>tch
          ct {}
          _ (: (go l)
               (? !(two? l) 0
                  (: k <l d (vdek k) sb <d off <>d w <>>d
                     ck (((vflr8 off) + voff) * 2 + sb)
                     _ (pin ct ck ([off w (rev (peep tt k ()))] >< peep ct ck ()))
                     (go >l)))
               (go (sort (keys tt))))
          (row l acc)
           (? !(two? l) (rev acc)
              (: ck <l
                 sb (ck % 2) base ((ck // 2) - voff)
                 es (rev (peep ct ck ()))
                 (badc? l2) (? !(two? l2) 0
                                (peep bad ((<<l2 + voff) * 2 + sb) 0) 1
                                (badc? >l2))
                 (? (peep esc (vspc sb) 0) (row >l acc)
                    (badc? es) (row >l acc)
                    (: allts (sort (: (g l2 a) (? !(two? l2) a
                                                  (: e <l2 (g >l2 (vapp (map (\ t <t) <>>e) a))))
                                      (g es ())))
                       offs (vuq (sort (map (\ e <e) es)))
                       ws (vuq (sort (map (\ e <>e) es)))
                       sifed (vany (\ e (vany (\ t (0 < vsiw <>t)) <>>e)) es)
                       full ((ws = '(8)) && (tally offs = 1) && !sifed)
                       nar ((tally offs = 1) && !full && (tally ws = 1))
                       multi ((1 < tally offs) ? 1 0)
                       lo <allts
                       hi (: (g l2) (? !(two? >l2) <l2 (g >l2)) (g allts))
                       across (vany (\ cx (lo < cx && cx < hi)) ci)
                       wsum (: (g l2 a) (? !(two? l2) a (g >l2 (a + vwt spans <l2))) (g allts 0))
                       dfs (sort (: (g l2 a)
                                    (? !(two? l2) a
                                       (: e <l2
                                          (g >l2 (vapp (map (\ t <t)
                                                            (vkeep (\ t (0 < vstw <>t || 0 < vsiw <>t)) <>>e))
                                                       a))))
                                    (g es ())))
                       uss (sort (: (g l2 a)
                                    (? !(two? l2) a
                                       (: e <l2
                                          (g >l2 (vapp (map (\ t <t) (vkeep (\ t (0 < vldw <>t)) <>>e))
                                                       a))))
                                    (g es ())))
                       (row >l ([sb base (full ? 1 0) (across ? 1 0) (sifed ? 1 0)
                                 (nar ? 1 0) multi (tally allts) wsum dfs uss] >< acc))))))
          (row (sort (keys ct)) ())))

   ; -- the whole layer over one body --
   (vctx bd)
    (: t {}
       bt (vtab bd) n (tally bd) lt (vlabs bd)
       succ (vsucc bd) esc (vesc bd) spans (vspans bd)
       cur (vcur bt n succ)
       escn (0 < tally (keys esc))
       lv (vlive bt n succ cur lt escn)
       tch (vtouch bd)
       cls (vcls tch spans esc)
       _ (pin t 'bt bt) _ (pin t 'n n) _ (pin t 'lt lt) _ (pin t 'succ succ)
       _ (pin t 'esc esc) _ (pin t 'spans spans) _ (pin t 'cur cur)
       _ (pin t 'lv lv) _ (pin t 'ci (tch = 'skip ? () <>>tch)) _ (pin t 'cls cls)
       t)

   ; -- the census rows: [sb base full across si nar multi nt wsum] --
   (vcells ctx) (map (vtake 9) (ctx 'cls))

   ; -- dead stores: a write no path reads. [i op weight near-call] --
   (vdead ctx)
    (: bt (ctx 'bt) n (ctx 'n) succ (ctx 'succ) cur (ctx 'cur)
       lt (ctx 'lt) lv (ctx 'lv) spans (ctx 'spans)
       (near i) (: (go k) (? ((n <= k) || ((i + 5) < k)) 0
                             (: f (peep bt k ())
                                (? (two? f && member? <f '(call callr)) 1 (go (k + 1)))))
                   (go (i + 1)))
       (out ss seed)
        (? (vall? seed) 'all
           !(two? ss) seed
           (: s (peep lv <ss ())
              (? (vall? s) 'all (out >ss (vun seed s)))))
       (row i acc)
        (? (i < 0) acc
           (: f (peep bt i ())
              rw (vrw f (peep cur i 0) lt)
              wr <>rw
              (? !(two? wr) (row (i - 1) acc)
                 (: ou (out (peep succ i ()) ())
                    (? (vall? ou) (row (i - 1) acc)
                       (two? (vboth wr ou)) (row (i - 1) acc)
                       (row (i - 1) ([i <f (vwt spans i) (near i)] >< acc)))))))
       (row (n - 1) ()))

   ; -- reaching defs at cell grain: rt i -> the defs (a sorted subset of dfs)
   ; reaching into i, over the index window [lo, hi] (a repack hull is such a
   ; window: no path leaves it and returns unseen). forward fixpoint; unions.
   (vreach lo hi succ dfs)
    (: rt {}
       dt (foldl (\ t d (pin t d 1)) {} dfs)
       (step chg i)
        (? (hi < i) chg
           (: rout (? (peep dt i 0) [i] (peep rt i ()))
              c2 (: (push l ch)
                    (? !(two? l) ch
                       (: j <l
                          (? ((j < lo) || (hi < j)) (push >l ch)
                             (: old (peep rt j ()) nu (vun old rout)
                                _ (pin rt j nu)
                                (push >l (tally nu = tally old ? ch 1))))))
                    (push (peep succ i ()) chg))
              (step c2 (i + 1))))
       (fix _) (? (0 < step 0 lo) (fix 0) rt)
       (fix 0))

   ; -- chain grouping: defs merge when one load's reaching set joins them (the
   ; phi seam). [((defs uses)..) nodefs], chains keyed ascending by first def.
   (vgroup rt dfs uss)
    (: uf {}
       (find x) (: p (peep uf x x) (? (p = x) x (find p)))
       (uni a b) (: ra (find a) rb (find b)
                    (? (ra = rb) 0 (ra < rb) (pin uf rb ra) (pin uf ra rb)))
       _ (: (g l2) (? !(two? l2) 0
                      (: u <l2 rin (peep rt u ())
                         _ (? (two? rin)
                              (: (g2 m) (? !(two? m) 0 (: _ (uni <rin <m) (g2 >m)))
                                 (g2 >rin))
                              0)
                         (g >l2)))
            (g uss))
       gt {}
       _ (: (g l2) (? !(two? l2) 0
                      (: d2 <l2 r2 (find d2)
                         _ (pin gt r2 (vapp (peep gt r2 ()) [d2]))
                         (g >l2)))
            (g dfs))
       ut {}
       nod (: (g l2 a) (? !(two? l2) (rev a)
                          (: u <l2 rin (peep rt u ())
                             (? !(two? rin) (g >l2 (u >< a))
                                (: r2 (find <rin)
                                   _ (pin ut r2 (vapp (peep ut r2 ()) [u]))
                                   (g >l2 a)))))
              (g uss ()))
       [(map (\ r2 [(peep gt r2 ()) (peep ut r2 ())]) (sort (keys gt))) nod])

   ; -- one value's boolean liveness at cell grain over [lo, hi]: kills at the
   ; stores, uses at the loads. answers i -> live out of i.
   (vclive lo hi succ kills uses)
    (: lo9 {}
       ut (foldl (\ t u (pin t u 1)) {} uses)
       kt (foldl (\ t k (pin t k 1)) {} kills)
       (lin i) (? (peep ut i 0) 1 (peep kt i 0) 0 (peep lo9 i 0))
       (sweep chg i)
        (? (i < lo) chg
           (: o (: (go ss) (? !(two? ss) 0 (0 < lin <ss) 1 (go >ss)) (go (peep succ i ())))
              old (peep lo9 i 0)
              _ (pin lo9 i o)
              (sweep (? (o = old) chg 1) (i - 1))))
       (fix _) (? (0 < sweep 0 hi) (fix 0) lo9)
       (fix 0))

   ; -- per-def chains: each cell's stores grouped by the loads that join them,
   ; and the call-crossing verdict per chain, not per hull. [sb base defs uses cross]
   (vchains ctx)
    (: n (ctx 'n) succ (ctx 'succ) lv (ctx 'lv) ci (ctx 'ci)
       (out ss seed)
        (? (vall? seed) 'all
           !(two? ss) seed
           (: s (peep lv <ss ())
              (? (vall? s) 'all (out >ss (vun seed s)))))
       (cell1 cl acc)
        (: sb <cl base <>cl
           dfs (vat 9 cl) uss (vat 10 cl)
           rt (vreach 0 (n - 1) succ dfs)
           gn (vgroup rt dfs uss)
           nod <>gn
           cbytes (vbytes (vspc sb) base 8)
           (celllive? cx) (: o (out (peep succ cx ()) ())
                             (? (vall? o) 1 (two? (vboth o cbytes)) 1 0))
           (cross ds) (vany (\ cx (two? (vboth (peep rt cx ()) ds) && celllive? cx)) ci)
           rows (map (\ gp [sb base <gp <>gp (cross <gp ? 1 0)]) <gn)
           (vapp acc (vapp rows (? (two? nod) [[sb base () nod 0]] ()))))
       (go l acc) (? !(two? l) acc (go >l (cell1 <l acc)))
       (go (ctx 'cls) ()))

   ; -- the differential rows for one TU: C census, D dead, H chains, per fn --
   (vgauge tu fs)
    (: (fn1 nb acc)
        (: nm <nb bd <>nb
           ctx (vctx bd)
           cs (map (\ r (vapp ['C tu nm] (vtake 9 r))) (ctx 'cls))
           dd (map (\ r (vapp ['D tu nm] r)) (vdead ctx))
           hs (map (\ r (vapp ['H tu nm] r)) (vchains ctx))
           (vapp acc (vapp cs (vapp dd hs))))
       (go l acc) (? !(two? l) acc (go >l (fn1 <l acc)))
       (go (vfns fs) ()))
)) ; module 'moon
