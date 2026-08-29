; gvn.tpl.l -- the GVN + LICM oracle over one TU's FINAL forms (sed @FILE@ per file,
; one process per TU, like irdump). what an SSA-based pass would still find in
; shipped code: G rows are computations whose value already sits in a register
; (value numbers to a fixpoint over the CFG, a join keeps only what every edge
; agrees on), L rows are loop-invariant computations left inside their innermost
; loop. every count is a floor: an op the tables do not model invalidates what
; it touches, and a loop is its linear back-edge span.
;   (G tu fn cat i wt)  cat: alu addr load       wt: 8^min(depth,3)
;   (L tu fn cat i wt)  cat: li alu addr load
(: _ (use 'moon)
   is9 ["out/host/" "./" "src/" "out/lib/" "crew/moon/lib/math/" "crew/moon/include/"]
   src "@FILE@"
   lin9 0                      ; 1: the LINEAR mode -- every label starts empty (no joins), the in-block floor
   comps '(add sub and or xor imul shl shr sar rol ror ror4 neg not sx1 sx2 sx4 zx1 zx2 zx4)
   addrs '(lea leax)
   comm '(add and or xor imul)
   lds '(ld ld1 ld2 ld4 ldu1 ldu2 ldu4)
   sts '(st st1 st2 st4 si si1 si2 si4 stx stxb stsd stss)
   bars '(call callr raw sys)
   (cat-of op) (? (member? op lds) 'load (member? op addrs) 'addr 'alu)
   (ctl? op) (member? op '(label jmp br ret jmpr ud2 align))   ; rdsp answers bar for these; they touch no register
   (nil? v) (= v ())
   (vk rg r) (? (charm? r) (show r)
                (: v (peep rg r ()) (? (nil? v) ("in:" + show r) v)))
   ; the value key a form computes, or () for one that computes nothing nameable.
   ; sf: frame slots by r4 offset, sc: stack cells by entry-relative address
   (vkey f rg sf sc mem c)
    (: op <f
       (? (op = 'li) ("k" + show <>f)
          (op = 'la) ("la:" + show <>>f)
          (op = 'mov) (vk rg <>>f)
          (member? op '(add sub and or xor imul))
           (: a (vk rg <>>f) b (vk rg <>>>f)
              (? (member? op comm && (b < a)) (show op + "(" + b + "," + a + ")")
                 (show op + "(" + a + "," + b + ")")))
          (member? op '(shl shr sar rol ror ror4)) (show op + "(" + vk rg <>f + "," + show <>>f + ")")
          (member? op '(neg not sx1 sx2 sx4 zx1 zx2 zx4)) (show op + "(" + vk rg <>f + ")")
          (op = 'lea) ("lea(" + vk rg <>>f + "," + show <>>>f + ")")
          (op = 'leax) ("leax(" + vk rg <>>f + "," + vk rg <>>>f + "," + show (f 4) + "," + show (f 5) + ")")
          (member? op lds)
           (: b <>>f off <>>>f
              kn (? (op != 'ld) ()
                    (b = 'r4) (peep sf off ())
                    (b = 'sp) (peep sc (c + off) ())
                    ())
              ad (? (b = 'r4) ("s" + show off)
                    (b = 'sp) ("c" + show (c + off))
                    (vk rg b + ":" + show off))
              (? !(nil? kn) kn (show op + "(" + ad + "@" + mem + ")")))
          ()))
   (copyt t) (foldl (\ t2 k (pin t2 k (t k))) {} (keys t))
   (joint ta tb) (foldl (\ t2 k (: va (peep ta k ()) vb (peep tb k ())
                                  (? (!(nil? va) && va = vb) (pin t2 k va) t2)))
                        {} (keys ta))
   ; a store drops every slot within a word of its offset (a narrow or unaligned
   ; overlap), then a full-word store records its value
   (drop-near t off) (foldl (\ t2 k (? ((off - 7) <= k && k <= (off + 7)) t2 (pin t2 k (t k))))
                            {} (keys t))
   ; one form onto the state -> [rg sf sc mem]; a fresh tablet where a clear is needed
   (step f i rg sf sc mem c)
    (: op <f
       rd (rdsp f)
       fresh ("f" + show i)
       (? (ctl? op) [rg sf sc mem]
          (member? op bars || id? rd 'bar) [{} {} {} ("m" + show i)]
          (member? op '(li la mov) || member? op comps || member? op addrs || member? op lds)
           (: _ (pin rg <>f (vkey f rg sf sc mem c)) [rg sf sc mem])
          (op = 'push) (: _ (pin sc (c - 8) (vk rg <>f)) [rg sf sc mem])
          (op = 'pop) (: v (peep sc c ()) _ (pin rg <>f (nil? v ? fresh v)) [rg sf sc mem])
          (member? op sts)
           (: b <>f off <>>f
              (? (member? op '(stx stxb)) [rg sf sc ("m" + show i)]
                 (b = 'r4) (: sf2 (drop-near sf off)
                              _ (? (op = 'st) (pin sf2 off (vk rg <>>>f))
                                   (op = 'si) (pin sf2 off ("k" + show <>>>f)) 0)
                              [rg sf2 sc mem])
                 (b = 'sp) (: sc2 (drop-near sc (c + off))
                              _ (? (op = 'st) (pin sc2 (c + off) (vk rg <>>>f))
                                   (op = 'si) (pin sc2 (c + off) ("k" + show <>>>f)) 0)
                              [rg sf sc2 mem])
                 [rg {} {} ("m" + show i)]))     ; a pointer store: the frame may alias it
          (: _ (foldl (\ _ d (pin rg d (fresh + ":" + show d))) 0 <>rd)
             [rg sf sc mem])))
   ; -- the fixpoint: label -> in-state, agree-or-drop joins, until nothing moves --
   (gvn tu nm bd)
    (: bt (vtab bd) n (tally bd) lab (vlabs bd) succ (vsucc bd)
       cur (vcur bt n succ) spans (vspans bd)
       st9 {} rows {}
       (merge l rg sf sc mem acc)
        (: old (peep st9 l ())
           (? lin9 (? (nil? old) (: _ (pin st9 l [{} {} {} ("mj" + show l)]) (l . acc)) acc)
              (nil? old) (: _ (pin st9 l [(copyt rg) (copyt sf) (copyt sc) mem]) (l . acc))
              (: rg2 (joint <old rg) sf2 (joint <>old sf) sc2 (joint <>>old sc)
                 mem2 (? (= (old 3) mem) mem ("mj" + show l))
                 ch (!(= (tally (keys rg2)) (tally (keys <old))) ||
                     !(= (tally (keys sf2)) (tally (keys <>old))) ||
                     !(= (tally (keys sc2)) (tally (keys <>>old))) ||
                     !(= mem2 (old 3)))
                 _ (pin st9 l [rg2 sf2 sc2 mem2])
                 (? ch (l . acc) acc))))
       ; walk a block from i (its label already behind us) -> the labels whose state moved;
       ; rep: record the redundant computations instead
       (walk i s0 rg sf sc mem acc rep)
        (? (n <= i) acc
           (: f (peep bt i ()) c (peep cur i 0)
              op (two? f ? <f ())
              (? (op = 'label) (? (= i s0) (walk (i + 1) s0 rg sf sc mem acc rep)
                                  rep acc
                                  (merge i rg sf sc mem acc))
                 (op = 'jmp) (: j (peep lab <>f -1)
                                (? (rep || j < 0) acc (merge j rg sf sc mem acc)))
                 (op = 'br) (: j (peep lab <>>f -1)
                               acc2 (? (rep || j < 0) acc (merge j rg sf sc mem acc))
                               (walk (i + 1) s0 rg sf sc mem acc2 rep))
                 (member? op '(ret jmpr ud2)) acc
                 (: _ (? (rep && (member? op comps || member? op addrs || member? op lds))
                         (: k (vkey f rg sf sc mem c)
                            held (vany (\ r (= (peep rg r ()) k)) (keys rg))
                            (? held (pin rows i [(cat-of op) (vwt spans i)]) 0))
                         0)
                    s2 (step f i rg sf sc mem c)
                    (walk (i + 1) s0 <s2 <>s2 <>>s2 (s2 3) acc rep)))))
       (from l rep)
        (: s (? (= l 0) (peep st9 0 ()) (peep st9 l ()))
           (? (nil? s) ()
              (walk l l (copyt <s) (copyt <>s) (copyt <>>s) (s 3) () rep)))
       _ (pin st9 0 [{} {} {} "m0"])
       (fix wk) (? !(two? wk) 0 (fix (vapp (from <wk ()) >wk)))
       _ (fix [0])
       _ (foldl (\ _ l (from l 1)) 0 (0 . sort (map (\ nm2 (lab nm2)) (keys lab))))
       (foldl (\ _ i (: r (rows i)
                        _ (putx ['G tu nm <r i <>r])
                        (putc 10)))
              0 (sort (keys rows))))
   ; -- LICM, in SSA's view: a form is invariant when every register it reads is --
   ; the same value each trip: no def of it anywhere in the span, or its reaching def
   ; in the span (the last one before it) is itself invariant. register reuse is
   ; the physical form's artifact, so the def side is not a condition. a load also
   ; wants a span with no store and no bar. a form counts under its innermost span.
   (licm tu nm bd)
    (: bt (vtab bd) n (tally bd) spans (vspans bd)
       (inner i) (foldl (\ best s (? (<s <= i && i <= <>s &&
                                      (nil? best || (<>s - <s) < (<>best - <best))) s best))
                        () spans)
       (rdsp2 f) (? !(two? f) 'bar (ctl? <f) [() () ()] (rdsp f))
       (span s)
        (: dc {}                                  ; reg -> defs in the span; 'bar / 'st flags
           _ ((: (cen i) (? (<>s < i) 0
                           (: f (peep bt i ()) rd (rdsp2 f)
                              _ (? (two? f && member? <f sts) (pin dc 'st 1) 0)
                              _ (? (id? rd 'bar) (pin dc 'bar 1)
                                   (foldl (\ _ d (pin dc d (1 + peep dc d 0))) 0 <>rd))
                              (cen (i + 1))))
                 cen) <s)
           stored (peep dc 'st 0)
           ldef {} inv {}
           (go i)
            (? (<>s < i) 0
               (: f (peep bt i ()) op (two? f ? <f ()) rd (rdsp2 f)
                  (rin? r) (: p (peep ldef r ())
                              (? (nil? p) (0 = peep dc r 0) (peep inv p 0)))
                  ok (!(peep dc 'bar 0) && !(id? rd 'bar) &&
                      (member? op '(li la) || member? op comps || member? op addrs || member? op lds) &&
                      !(vany (\ r !(rin? r)) <rd) &&
                      (!(member? op lds) || !stored))
                  _ (? ok (pin inv i 1) 0)
                  _ (? (ok && (= (inner i) s))
                       (: _ (putx ['L tu nm (member? op '(li la) ? 'li (cat-of op)) i (vwt spans i)])
                          (putc 10))
                       0)
                  _ (? (id? rd 'bar) 0 (foldl (\ _ d (pin ldef d i)) 0 <>rd))
                  (go (i + 1))))
           (go <s))
       (foldl (\ _ s (span s)) 0 spans))
   pr (cc-parse src {'is is9 'ds '("ai_tco=1" "AiHaveVersionH") 'tgt 'amd64 'os 'linux 'vb () 'dia 'moon})
   _ (? !(okv? pr) (puts ("!! parsefail " + src + "\n"))
        (@ pr [_ ast stag sigs weaks sfns svars sects alias aligns ainl noinl consts rstrs]
           (: ob (cgen-obj ast stag sigs weaks [sfns svars sects alias aligns ainl noinl consts rstrs ()] 'amd64)
              (? !(two? ob) (puts ("!! genfail " + src + "\n"))
                 (: _ (puts ("== " + src + "\n"))
                    _ (foldl (\ _ fn (: _ (gvn src <fn <>fn) (licm src <fn <>fn))) 0 (vfns <>ob))
                    0)))))
   0)
