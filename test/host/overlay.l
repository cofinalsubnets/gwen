; --- test/host/overlay.l: the lux type overlay + the baked uu book, end to end ---
; NOT in the test/*.l corpus (the overlay + uu ride the HOST binary's post-egg
; layers; love0/kernel/wasm never load them). Three legs:
;   1. crew/lux/sigs.l checks CLEAN over the real core.l + layout.l sources
;      (read as DATA, checked pure with (overlay 'ov-check1) -- no install,
;      no eval, observe-only squared), and a deliberately wounded def is
;      flagged by name.
;   2. the gradual laws: an unknown arg rides into a typed seat; a partial
;      application reads open, not clashing (the curried language's law).
;   3. the uu book is baked and COMPUTES: (uu 'defn) type-checks a nat_rect
;      adder, (uu 'vof) normalizes 2+1=3 -- no concat, a bare binary.
;   4. the MODEL vs the IMPLEMENTATION: test/uu.l's verified StackSet mirror
;      (its zins/zrow/zcount) computes the SAME answers as crew/lux/core.l's,
;      on the same stacks, through an encode -> vof -> decode round trip.
;   5. the seeded fuzz: the whole op vocabulary walked at random through both
;      worlds, compared after every step, the proved invariant watched live.
;   6. the TRANSLATOR (tools/wm2uu.l, stage 3): core.l's ops read as data,
;      kind-directed into uu terms, kernel-certified, and fuzzed against the
;      implementation they were translated from -- then (stage 4) tied to the
;      hand-proved model by idpath BRIDGES: compiled-from-source == proved,
;      certified by conversion, so every model theorem transfers for free.
;
;   cat test/00-init.l test/host/overlay.l | out/host/love

(reads (open "crew/lux/sigs.l" "r"))
(reads (open "crew/lux/core.l" "r"))

(:    chk1 (overlay 'ov-check)
   ; ⚠ OPFIX FIRST, THEN UNGLUE: `forms` reads the SURFACE, the domain checker reads
   ; the core shapes -- and a glued sigil factors to a VALUE, which no checker matches
   ; by name. the two passes are the one door between them (tools/uuwmgen.l too).
   (fix1 f) (unglue (opfix f))
   (sweep path) (foldl (\ a f (cat a (chk1 lux-dom lux-sigs (fix1 f)))) () (forms (open path "r")))

 ; leg 1: the shipped sources check clean...
 _ (assert (! (sweep "crew/lux/core.l"))
           (! (sweep "crew/lux/layout.l")))
 ; ...and a KNOWN kind in the wrong seat is caught: zins wants (window zipper),
 ; the wound hands it a tag and a sheaf
 hurt (chk1 lux-dom (link '(oops (sheaf tag) top) lux-sigs)
            '(: (oops s t) (zins (wstagof s t) s)))
 _ (assert (two? hurt) (id? 'oops (cap (cap hurt))))

 ; leg 2: the gradual laws
 sg2 (link '(g1 (rect) window) (link '(g2 (window) top) lux-sigs))
 _ (assert (! (chk1 lux-dom sg2 '(: (g1 r) (zfoc (cap r)))))       ; unknown arg: consistent
           (! (chk1 lux-dom sg2 '(: (g2 w) (zins w))))             ; partial app: open, not a clash
           (two? (chk1 lux-dom sg2 '(: (g1 r) (zfoc r)))))         ; a RECT in the zipper seat: flagged

 ; leg 3: uu, baked -- define, certify, compute
 _ ((uu 'defn) 'boot-add '(pi m nat (pi n nat nat))
               '(lam m (lam n (nat_rect (lam q nat) n (lam k (lam r (succ r))) m))))
 _ (assert (= 3 ((uu 'vof) () '(boot-add 2 1)))
           (= 5 ((uu 'vof) () '(boot-add 2 3)))))

; the kernel refuses through the help cell -- install the escape (test/uu.l's
; own tail move; the kernel span rightly bakes NO global help), probe, uninstall
(hear (\ p e (: h (peep (uu 'HELPC) () 0) (h ? (h p e) 0))))
(assert (id? 'rejected ((uu 'rejects) (z \ (uu 'defn) 'boot-bad 'nat '(lam x x)))))
(hear 0)

; leg 4: load the whole uu corpus (kernel + the StackSet model + its theorems,
; every defn re-certified right here), then run the MODEL against core.l's
; implementation on concrete stacks -- encode, vof, decode, compare
(reads (open "test/uu.l" "r"))
(reads (open "test/uulay.l" "r"))
(reads (open "test/uuwm.l" "r"))
(reads (open "test/uuwmlaw.l" "r"))
(: (encl l) (two? l ? ['ncons (cap l) (encl (cup l))] 'nnil)
   (encz z) ['zmk (cap z) (encl (caup z)) (encl (caup (cup z)))]
   (decl v) (: (go vv acc) (two? vv ? (go (vv 2) (link (vv 1) acc)) (rev acc))
              (go (v 2) ()))
   (decz v) (zmk (v 1) (decl ((v 2) 1)) (decl ((v 2) 2)))
 z0 (zmk 5 [2] [7 9])
 z1 (zmk 5 () [7 9])
 _ (assert
    (= (zrow z0) (decl (vof 0 ['zrow (encz z0)])))
    (= (zrow (zins 4 z0)) (decl (vof 0 ['zrow ['zins 4 (encz z0)]])))
    (= #(zrow (zins 4 z0)) (vof 0 ['zcount ['zins 4 (encz z0)]]))
    ; focus motion, both faces: the plain step and the wrap
    (= (cap (zfocup z0)) (vof 0 ['zfoc ['zfocup (encz z0)]]))
    (= (zrow (zfocup z0)) (decl (vof 0 ['zrow ['zfocup (encz z0)]])))
    (= (cap (zfocup z1)) (vof 0 ['zfoc ['zfocup (encz z1)]]))
    (= (cap (zfocdn z0)) (vof 0 ['zfoc ['zfocdn (encz z0)]]))
    (= (zrow (zmaster z0)) (decl (vof 0 ['zrow ['zmaster (encz z0)]])))
    ; delete, through the model's Maybe (ii2 carries the survivor)
    (= (zdel 5 z0) (decz ((vof 0 ['zdel 5 (encz z0)]) 1)))
    (= (zswapup z0) (decz (vof 0 ['zswapup (encz z0)])))))

; leg 5: the seeded fuzz -- the whole op vocabulary, random walks through
; BOTH worlds, the zippers compared after EVERY step, and the PROVED
; invariant watched live: fresh ids keep the row duplicate-free, so the
; model's zsane must answer true at every tenth stack. one op per step
; runs on the MODEL side too (the previous CORE state encodes, the model
; moves, the result decodes) -- so a divergence names its op and its step.
(: (rnd s) (% (+ (* s 1103515245) 12345) 2147483648)
   (encl l) (two? l ? ['ncons (cap l) (encl (cup l))] 'nnil)
   (encz z) ['zmk (cap z) (encl (caup z)) (encl (caup (cup z)))]
   (decl v) (: (go vv acc) (two? vv ? (go (vv 2) (link (vv 1) acc)) (rev acc))
              (go (v 2) ()))
   (decz v) (zmk (v 1) (decl ((v 2) 1)) (decl ((v 2) 2)))
   (mstep r w t) (? (= r 0) (vof 0 ['zins w t])
                    (= r 1) (vof 0 ['zfocup t])
                    (= r 2) (vof 0 ['zfocdn t])
                    (= r 3) (vof 0 ['zswapup t])
                    (= r 4) (vof 0 ['zswapdn t])
                    (= r 5) (vof 0 ['zmaster t])
                    ((vof 0 ['zdel w t]) 1))
   (cstep r w z) (? (= r 0) (zins w z)
                    (= r 1) (zfocup z)
                    (= r 2) (zfocdn z)
                    (= r 3) (zswapup z)
                    (= r 4) (zswapdn z)
                    (= r 5) (zmaster z)
                    (zdel w z))
   (fuzz i s z nid)
    (? (< i 1) 1
       (: row (zrow z)
          r (: r0 (% s 7) ((= r0 6 ? (< #row 2) 0) ? 1 r0))   ; no deletes below 2
          s2 (rnd s)
          w (? (= r 0) nid (= r 6) (row (% s2 #row)) 0)
          zc (cstep r w z)
          zm (decz (mstep r w (encz z)))
          (? (! (= zc zm)) (: _ (. [(\ fuzz-diverged) i r w zc zm]) 0)
             (: _ (? (= 0 (% i 10))
                     (? (= (vof 0 'true) (vof 0 ['zsane (encz zc)])) 0
                        (. [(\ fuzz-insane) i zc]))
                     0)
                (fuzz (- i 1) (rnd s2) zc (= r 0 ? (+ nid 1) nid))))))
 _ (assert (fuzz 150 20260705 (zmk 0 () ()) 1))
 (puts "overlay leg 5: ok"))

; leg 6 (stage 3 of the translator ladder): tools/wm2uu.l reads core.l's ops
; AS DATA, kind-directed by the sig table, and emits uu terms -- which the
; kernel then defn-CERTIFIES right here (⚠ dependency order: certify in the
; order gens answers), and which must answer the fuzz exactly as core.l
; does. the translated terms live in the model's vocabulary, so stage 4's
; idpath bridges can tie gen-* to the proved ops.
(reads (open "tools/wm2uu.l" "r"))
(:    cforms (map (f \ unglue (opfix f)) (forms (open "crew/lux/core.l" "r")))   ; the surface factored to the core, as above
   gens (wm2uu cforms lux-sigs)
 _ (assert (= 10 #gens))
 _ (map (g \ defn (cap g) (g 1) (g 2)) gens)   ; the kernel certifies each, in order
 _ (puts "overlay leg 6: translated + certified ")

 ; stage 4: the idpath bridges. for every op, the theorem
 ;   paths RES (gen-op args..) (op args..)
 ; proved by (idpath (op args..)) -- which CHECKS if and only if the
 ; translator's output is CONVERTIBLE with the hand-proved model. the kernel
 ; itself certifies compiled-from-source == proved, and every theorem about
 ; the model transfers to the generated terms for free.
 amap '((zrow agree-zrow) (zrev agree-zrev)
        (zfocup agree-zfocup) (zfocdn agree-zfocdn) (zswapup agree-zswapup)
        (zswapdn agree-zswapdn) (zmaster agree-zmaster) (zins agree-zins)
        (znub agree-znub) (zdel agree-zdel))
 (mname nm) (id? nm 'znub ? 'nfilt nm)                 ; znub's proved twin is nfilt
 _ (map (nm \ (: sg (w2-sigof lux-sigs nm)
                 ps (cup (cap (w2-find nm cforms)))
                 ks (cap (cup sg))
                 res (w2-mty (w2-rk nm (w2-nthl sg 2)))
                 (defn (caup (assoc nm amap))
                   (w2-pi ps ks ['paths res (link (w2-gen nm) ps) (link (mname nm) ps)])
                   (w2-lams ps ['idpath (link (mname nm) ps)]))))
        w2-want)
 (puts "overlay stage 4: 10 idpath bridges -- the compiled ops ARE the proved model"))

; the same seeded walk, the model side now stepping through the TRANSLATED ops
(: (rnd s) (% (+ (* s 1103515245) 12345) 2147483648)
   (encl l) (two? l ? ['ncons (cap l) (encl (cup l))] 'nnil)
   (encz z) ['zmk (cap z) (encl (caup z)) (encl (caup (cup z)))]
   (decl v) (: (go vv acc) (two? vv ? (go (vv 2) (link (vv 1) acc)) (rev acc))
              (go (v 2) ()))
   (decz v) (zmk (v 1) (decl ((v 2) 1)) (decl ((v 2) 2)))
   (mstep r w t) (? (= r 0) (vof 0 ['gen-zins w t])
                    (= r 1) (vof 0 ['gen-zfocup t])
                    (= r 2) (vof 0 ['gen-zfocdn t])
                    (= r 3) (vof 0 ['gen-zswapup t])
                    (= r 4) (vof 0 ['gen-zswapdn t])
                    (= r 5) (vof 0 ['gen-zmaster t])
                    ((vof 0 ['gen-zdel w t]) 1))
   (cstep r w z) (? (= r 0) (zins w z)
                    (= r 1) (zfocup z)
                    (= r 2) (zfocdn z)
                    (= r 3) (zswapup z)
                    (= r 4) (zswapdn z)
                    (= r 5) (zmaster z)
                    (zdel w z))
   (fuzz i s z nid)
    (? (< i 1) 1
       (: row (zrow z)
          r (: r0 (% s 7) ((= r0 6 ? (< #row 2) 0) ? 1 r0))
          s2 (rnd s)
          w (? (= r 0) nid (= r 6) (row (% s2 #row)) 0)
          zc (cstep r w z)
          zm (decz (mstep r w (encz z)))
          (? (! (= zc zm)) (: _ (. [(\ genfuzz-diverged) i r w zc zm]) 0)
             (fuzz (- i 1) (rnd s2) zc (= r 0 ? (+ nid 1) nid)))))
 _ (assert (fuzz 100 424242 (zmk 0 () ()) 1))
 ; and the accessors/row through the translated lane, spot-checked
 z0 (zmk 5 [2] [7 9])
 _ (assert
    (= (zrow z0) (decl (vof 0 ['gen-zrow (encz z0)])))
    (= (cap (zrev z0)) (vof 0 ['zfoc ['gen-zrev (encz z0)]]))
    (= (znub 7 (zrow z0)) (decl (vof 0 ['gen-znub 7 ['gen-zrow (encz z0)]]))))
 (puts "overlay: ok"))

; leg 7 (the layout rung): test/uulay.l's tiling-ledger model vs the REAL
; crew/lux/layout.l rt-tile -- for seeded random rows and master counts, the
; window column of the actual placements, the model's rtwins, and the row
; itself must be ONE list (rtwinsrow, watched live against the implementation
; the model mirrors). geometry stays law.l's business; this leg pins the ledger.
(reads (open "crew/lux/layout.l" "r"))
(: (rnd s) (% (+ (* s 1103515245) 12345) 2147483648)
   (encl l) (two? l ? ['ncons (cap l) (encl (cup l))] 'nnil)
   (decl v) (: (go vv acc) (two? vv ? (go (vv 2) (link (vv 1) acc)) (rev acc))
              (go (v 2) ()))
   (wincol ps) (two? ps ? (link (cap (cap ps)) (wincol (cup ps))) ())
   (iota n b) (< n 1 ? () (link b (iota (- n 1) (+ b 1))))
   (walk i s)
    (? (< i 1) 1
       (: n (% s 7) nm (% (rnd s) 5) s2 (rnd (rnd s))
          ws (iota n (+ 10 i))
          z (zfrom ws)
          core (wincol (cap (lay-tile (resizable-tall nm 0.03 0.5) '(0 0 800 600) z)))
          model (decl (vof 0 ['rtwins nm 'nnil 0 (encl ws)]))
          (? (! (&& (= core ws) (= model ws))) (: _ (. [(\ ledger-diverged) i nm ws core model]) 0)
             (walk (- i 1) s2))))
 _ (assert (walk 40 20260706))
 _ (test_fin ())
 (puts "overlay leg 7: the tiling ledger holds, model and metal"))
