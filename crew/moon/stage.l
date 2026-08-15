; crew/moon/stage.l -- the PIPELINE'S TYPES: gen.l's stage contracts as an overlay
; domain + sig table, checked STATICALLY by the moon gate. gen.l is read as DATA
; (sound), never evaled, and nothing here rides any image -- the sigs live in this
; file and the gate's process alone, so the baked mooncc pays zero bytes and zero
; time (the ov-slot hook stays unarmed everywhere).
;
; a die is the STAGE a form list is at. a cleanup pass is stage-PRESERVING ((a) a,
; polymorphic -- its order among its peers is free), a staging pass ADVANCES through
; a concrete die -- so the post-choice chain types in exactly one order, and a
; recomposition is a clash naming its site. a NEW PASS slots in by declaring a sig
; here: the input die says what must already be true, the result die says what the
; next pass may assume. the checker is gradual: a local name reads bot and binds
; nothing, so the enforcement is sharp exactly at the pass-to-pass seams.
; (content of the forms is cskeep's charge at emission; this leg types the WIRING.)

(: ovck (from 'overlay 'ov-check)
   _ (? (lit? ovck) 0 (: _ (puts ";; moon-stage: no overlay module in this love") (quit 1)))
   stage-dies '(gst ir-vreg ir-chosen ir-nolab ir-noframe ir-jmpfix ir-final ir-emitted)
   (stage-join x y) (? (= x y) x 'top)
   (stage-tr op x y) 'top
   (stage-lit e) 'bot
   (stage-okr bd decl) (member? bd '(top bot) || (= bd decl))
   stage-dom (hash 'dice stage-dies 'ctors () 'tr stage-tr 'join stage-join
                   'litdie stage-lit 'okr stage-okr)
   stage-sigs '((cgfn (gst top) top)                ; sigged so its body -- both chains -- is walked
                                                    ; (params gradual: a sig VARIABLE there would ride rigid into every concrete seam)
                ; the build tail: vrfix ADVANCES (build's assembled list carries mints -- the
                ; vreg shadow, doc/archive/moon-vreg.md 5.1a -- and no vreg may reach sibcall/soften,
                ; which reason about the physical file), then sibcall + the sweeps preserve
                (vrfix (gst ir-vreg) ir-chosen)
                (sibcall (gst a) a) (soften (gst a) a) (a4ize (a) a)
                (cfoldir (a) a) (deadcell (a) a) (deaddef (a) a) (spmerge (a) a)
                (stld (a) a) (copyprop (a) a) (addrfold (a) a) (cmpfuse (a) a)
                (deadst (gst a) a) (repack (gst a) a)
                ; the post-choice chain ADVANCES: deadlab must never touch what the
                ; rankers read, unframe re-addresses before jr0 retargets, unhome
                ; renames last, cskeep sees only what ships -- one order types
                (deadlab (ir-chosen) ir-nolab)
                (coal (gst ir-nolab) ir-nolab)       ; stage-preserving: it renames, never re-addresses
                (unframe (gst ir-nolab) ir-noframe)
                (jr0 (ir-noframe) ir-jmpfix)
                (unhome (gst ir-jmpfix) ir-final)
                (cskeep (gst ir-final) ir-emitted))
   (forms text acc)
    (: r (sound text)
       (? (id? r 'torn) (: _ (puts ";; moon-stage: gen.l reads torn") (quit 1))
          !(two? r) acc
          (forms >r (<r >< acc))))
   src (open "crew/moon/gen.l" "r")
   _ (? src 0 (: _ (puts ";; moon-stage: cannot open crew/moon/gen.l") (quit 1)))
   text (slurp src)
   reps (catmap (\ f (? (two? f && id? <f ':) (ovck stage-dom stage-sigs f) ()))
                (forms text ()))
   _ (? (two? reps)
        (: _ (map (\ r (: _ (puts ";; moon-stage: ") _ (putx r) (putc 10))) reps)
           (quit 1))
        0)
   _ (puts "moon-stage: ")
   _ (putx (tally stage-sigs))
   _ (puts " sigs hold over crew/moon/gen.l (the post-choice chain types in one order)")
   _ (putc 10)
   0)
