; hook -- the native-creation hook. book['natjit] is read by ev.l's `ala` on every
; closure built, top-level or embedded: a qualifying leaf, loop, call-out, group or
; amble body is native-backed at creation and deopts into the closure's own bytecode
; `e`, so native is never wrong, only faster. the native cell keeps value[-1]=s, the
; surface src, so =/show see the source; a fresh cell per ala call keeps id? honest.
;
; deliberately outside the `glaze` module: ala reads the hook via (gv 'natjit) and
; fires/fired? are its probe, so the three must be on the book, and a post-egg stream
; installs a global only by leaking a body-less `:`. the glaze aliases stay private
; in the inner body-having `:`. the laws are test/glaze-hook.l, `make test_hook`.
(: fires {0}                                                              ; probe counter: closures the hook native-backed
   natjit
    (: arch love-arch                                                     ; the runtime target ('amd64 / 'arm64), read on the actual binary
       G (from 'glaze)                                                    ; this file is not in the module (test_glaze re-cats it alone): every name arrives by key
       emit (G 'emit)  coreir (G 'coreir)  deoptir (G 'deoptir)
       loopinfo (G 'loopinfo)  njit-loop (G 'njit-loop)
       loopinfo-n (G 'loopinfo-n)  njit-loop-n (G 'njit-loop-n)
       calloutinfo (G 'calloutinfo)  jitcalloutx (G 'jitcalloutx)
       calloutinfo-frame (G 'calloutinfo-frame)  njit-calloutframe (G 'njit-calloutframe)
       qualfr (G 'qualfr)  jitfrx (G 'jitfrx)
       anat-defs (G 'anat-defs)  jgir-ok? (G 'jgir-ok?)  jitgroupirx (G 'jitgroupirx)  grpfix (G 'grpfix)
       njit-amble (G 'njit-amble)
       asm (from 'holo 'assemble)                                         ; holo's post-hook build, pinned for the glaze: the cat's later reopen of holo never moves it
       _ (pin (G 'glazeasm) 'f asm)
       _ (pin (G 'glazeasm) 'at (from 'holo 'assemble-at))
       (hit x) (pin fires () (1 + peep fires () 0), x)
       ; grp-ok? is the group gate: autogroup's own recognizer over the defs jitgroupirx compiles. jgir-ok?
       ; only says the ops are implemented -- a body past the grammar reaches cggir, whose ? chain ends in a
       ; silent () that emits no code for an operand it cannot hold
       (grp-ok? gds) (: cands (map (gd \ [<<gd (#<gd - 1)]) gds) (#(grpfix gds cands) = #gds))   ; (name arity)
       (exprn ps e) (? (member? e ps) 1  (whole? e) 1  (atom? e) 0                    ; the leaf grammar, exact arity: (+ x 1 2) is a tower, not x + 1
                       (e @ (op a b) (member? op '(+ - * // % & | ^ < <= > >= =) && exprn ps a && exprn ps b)
                            ('? c a b) (exprn ps c && exprn ps a && exprn ps b)
                            0))
       (opnd-ok? ps e) (? (two? e) (all (opnd-ok? ps) >e)  (name? e) (member? e ps)  1)   ; every operand leaf a frame param: jgir has no slot for a free capture
       (gdefs-ok? gds) (all (gd \ opnd-ok? ><gd >gd) gds)                             ; ... and per def body, over that def's own params
       ; e = the bytecode entry (the deopt target), s = the surface (\ frame.. body), a = arity, gv = ala's
       ; folded global resolver (the book is mopped post-egg), farq = the letrec-local predicate (a farq name
       ; is wire-resolved: never gv it, the book namesake is stale), lams = the scope's lambda siblings
       ; ((name . cell)..) verbatim (a cell's (entry . imports) is what lz freezes; entry 0 declines), nm = the
       ; letrec name being bound (0 anonymous: amble's self-call seam), ti/ta = ev.l's uninterned iop/apx
       ; markers, inl = the inner-closure record ((exp src-tail cell)..) amble's site reads
       (natjit e s a gv farq lams nm ti ta inl)
        (: ps (init >s)  body (last >s)                                                ; the frame [imps args] + the surface body
           (? !(all name? ps) e                                                       ; a mint frame (bsub's binders, boxfix's cells) declines every lane
              (two? s && two? >>s && exprn ps body)                                   ; the leaf: >=1 param, a body in the grammar
              (hit (nif (emit (peep (asm arch (coreir arch ps body (deoptir arch (a = 1 ? 0 1)))) 'bytes 0)) e s a))
              (qualfr s) (hit (jitfrx s arch e))                                      ; the float-result leaf boxes a gem; deopt to e, never (ev s), which re-enters ala
              (loopinfo s @ [B I op cmp] (hit (njit-loop s B I op cmp e))             ; the counted loop, then the n-var one: the back-edge is the win
               (loopinfo-n s @ [vars ups T R inits] (hit (njit-loop-n s vars ups T R inits e))
                (: ci (calloutinfo s)                                                 ; a counted sum over (F i), F a resolvable global: the stackless bridge
                   fv (ci && !(farq <ci) && gv <ci 0)
                   (? (lit? fv) (ci @ [_ op iv arg] (hit (jitcalloutx fv s op iv arg arch)))
                      (calloutinfo-frame s @ [F N op arg] (hit (njit-calloutframe s F N a e op arg arch))   ; ... F and the bound read from the frame
                       ; the group lane: a first-order mutually recursive arith group under the frame, its tail
                       ; either (entry frame..) -- flat, no indirection -- or any arith over group calls, through
                       ; a synthetic ((__outer . frame) . tail) entry. post-feel every def is name (\ p.. B)
                       (: pd (? (two? body && id? <body ':) (anat-defs >body) ())
                          dds (? pd <pd ())  tail (? pd <>pd ())
                          okshape (pd && two? tail && all (dd \ dd @ (n . '\ . _ . _) (name? n) 0) dds)
                          gdefs (? okshape (map (dd \ (<dd . init >>dd) . last >>dd) dds) ())     ; ((name p..) . B)
                          gnames (map caap gdefs)
                          flat? (okshape && member? <tail gnames && >tail = ps)                     ; the tail is (entry frame..)
                          osym '__outer  agdefs (? okshape (gdefs + [((osym . ps) . tail)]) ())
                          (? (flat? && grp-ok? gdefs && jgir-ok? gdefs && gdefs-ok? gdefs)
                             (hit (jitgroupirx gdefs <tail arch e s))                               ; stamps the outer s: =/show transparent
                             (okshape && !(member? osym gnames) && opnd-ok? ps tail && grp-ok? agdefs && jgir-ok? agdefs && gdefs-ok? gdefs)
                             (hit (jitgroupirx agdefs osym arch e s))
                             (: aj (njit-amble s a e gv farq lams nm ti ta inl arch)                 ; amble, the last lane: 0 on decline
                                (? aj (hit aj)
                                   (getenv "LOVE_AMBLE_TRACE2" && puts ("amble-DECLINE: " + show s + "\n"), e))))))))))))
       natjit)                                                            ; the inner : answers the closure; its aliases stay private
   (fired? n) (n < peep fires () 0))                                       ; did the hook fire since count n?
