; hook -- install the native-creation hook: book['natjit], read by ev.l's `ala` on
; EVERY closure built, top-level AND embedded in a fn/let body (the reach the ev-rebind
; in love/glaze/auto.l never had -- it only saw the form handed to `ev`). A qualifying leaf (the
; emit grammar over its frame [imps args]) is native-backed AT CREATION and DEOPTS into
; the closure's own bytecode `e` on overflow -- so native is never wrong, just faster.
;
; Why this is safe where the source-rewrite jit-walk wasn't:
;  * transparent -- the native cell keeps value[-1]=s (the surface src), so =/show see
;    the source; the enclosing closure's src is never rewritten.
;  * id?-distinct -- a fresh native cell per `ala` call (no compile cache), exactly like
;    bytecode closures: !(id? (\ x E) (\ x E)) holds.
;  * macro-safe -- compiles the SURFACE body `(last >s)` (post-opfix, pre-tag), so
;    iop/apx tags never reach codegen; a macro/non-arith body simply fails the grammar
;    and stays bytecode.
;  * no double-compile -- the deopt target is `e` itself (the bytecode closure ala built),
;    passed as nat/natn's `interp`; no separate (ev lam) pass.
; Loops are NOT caught here (their body isn't leaf-grammar) -- they stay bytecode, or use
; love/glaze/auto.l's counted-loop path. Rides the BAKED glaze through (from 'glaze). x86-64 only.
;
; THIS FILE IS THE GLAZE'S GLOBALS, and it is deliberately OUTSIDE the `glaze` module
; (emit.l + auto.l): a module member is reached by key, and these four names have to be
; on the book -- natjit because ala reads the hook via (gv 'natjit) (ev.l), `fires`/`fired?`
; because they are its probe, and `bake` because it wraps ORTH's own nif. A post-egg stream
; installs a global ONLY by leaking a body-less `:` (book is mopped -> no (pin book ..)). So
; keep the ~20 glaze aliases (loopinfo/emit/calloutinfo/coreir/..) PRIVATE in an inner
; body-having `:` (one scope, returns natjit); the outer body-less `:` leaks just those four.
; The LAWS are test/glaze-hook.l, under `make test_hook`, which drives those four names.
(: fires {0}                                                                 ; probe counter: how many closures the hook native-backed (natjit writes, fired? reads)
   natjit
    (: arch love-arch                                                         ; the runtime CPU target ('amd64 / 'arm64), read at boot on the actual binary -- so the SAME baked hook.h emits amd64 on the x86 host and arm64 under qemu (mirrors auto.l's `arch`). njit-loop/njit-loop-n already close over auto.l's arch; the leaf/float/callout/group lanes below take it explicitly.
                                                                            ; `assemble` below reads BARE: main.c brackets this load with (use 'holo) .. ai_unsplice_, so it folds at compile (the capture law)
   G (from 'glaze)                                                          ; the glaze MODULE -- this file is NOT in it (test_glaze re-cats this alone
   emit (G 'emit)  coreir (G 'coreir)  deoptir (G 'deoptir)     ;   over a baked binary), so every name arrives by key. cat-loads post-boot, nothing folded
   loopinfo (G 'loopinfo)  njit-loop (G 'njit-loop)                 ;   post-boot, so nothing folded for it. loop lane
   loopinfo-n (G 'loopinfo-n)  njit-loop-n (G 'njit-loop-n)         ;   (unify with auto-ev): the counted + N-VAR back-edges
   calloutinfo (G 'calloutinfo)  jitcalloutx (G 'jitcalloutx)       ;   + the CALL-OUT loop: a counted sum whose body calls a global closure (the stackless bridge)
   calloutinfo-frame (G 'calloutinfo-frame)  njit-calloutframe (G 'njit-calloutframe)   ;   ... and its CAPTURED / multi-arg sibling (F read from the [imps args] frame)
   qualfr (G 'qualfr)  jitfrx (G 'jitfrx)                           ;   plus the float-RESULT leaf, all owned natively at
   anat-defs (G 'anat-defs)  jgir-ok? (G 'jgir-ok?)  jitgroupirx (G 'jitgroupirx)  ; creation. + the (always-on) GROUP
   grpfix (G 'grpfix)                                                    ;   ... and the recognizer that says WHETHER (below)
   njit-amble (G 'njit-amble)                                            ;   + AMBLE, the universal boring-tier translator (the LAST lane)
   asm (from 'holo 'assemble)                                            ; the assembler as it stands at install: holo's post-hook build,
   _ (pin (G 'glazeasm) 'f asm)                                          ;   pinned for the glaze (a later reopen of holo -- the cat's own
   _ (pin (G 'glazeasm) 'at (from 'holo 'assemble-at))                   ;   copy -- never moves it)
   ; lane: parse defs (anat-defs), recognize (grp-ok?), op-cover (jgir-ok?), compile PURE (jitgroupirx, e deopt).
   ; grp-ok? -- autogroup's OWN recognizer (auto.l grpfix: the descending int/value-typing fixpoint over
   ; groupok?/vok?/cok?), run here over the SAME (nameform . body) defs jitgroupirx will compile. Every def
   ; must survive, so the group is the closed glazeable subset and nothing else.
   ; ⚠ THIS IS THE GATE, not jgir-ok?. jgir-ok? answers "does jitgroupir implement these OPS" and its own
   ; header says it does NOT re-derive the grammar -- it was written as a backstop UNDER autogroup, which had
   ; already run grpfix. This hook is the other caller and had no recognizer at all, so a body outside the
   ; grammar reached cggir, whose `?` chain ends in a silent `()`: an operand it cannot emit -- a string, a
   ; nom, a list, a gem -- emits NO CODE and leaves whatever the accumulator last held. `(+ acc " ")` then
   ; compiled as `acc + k` and answered triangular numbers for three spaces. The two free-capture guards
   ; below (opnd-ok?, gdefs-ok?) are earlier patches of this same class, one shape at a time; grpfix closes
   ; it whole -- the recognizer and the codegen are one pair again, wherever the lane is entered from.
   (grp-ok? gds) (: cands (map (\ gd [<<gd (tally <gd - 1)]) gds)    ; (name arity), autogroup's armap
                    ((tally (grpfix gds cands)) = (tally gds)))
   (exprn ps e) (? (member? e ps) 1  (whole? e) 1                          ; var in frame | int literal
                   (two? e) (: op <e
                               (? (member? op '(+ - * // % & | ^ < <= > >= =)) (exprn ps <>e && exprn ps <>>e)
                                  (op = '?) ((two? >>>e) && !(two? >>>>e)
                                            && (exprn ps <>e) && (exprn ps <>>e) && (exprn ps <>>>e))
                                  0))
                   0)
   ; opnd-ok? -- every OPERAND-position leaf symbol in e is a frame param. heads are op-checked by
   ; jgir-ok?; in FIRST-ORDER group code a group name only ever appears as a HEAD, so a bare-symbol
   ; operand that isn't a frame var is a FREE capture -> reject (jgir compiles a body over the frame
   ; params only; it has no slot to bind an outer var). the synthetic-__outer tail guard.
   (opnd-ok? ps e) (? (two? e) (all (\ x (opnd-ok? ps x)) >e)  (name? e) (member? e ps)  1)
   ; gdefs-ok? -- the SAME free-capture guard, per group DEF body: jgir binds a def over ITS OWN frame
   ; params only, so an operand leaf a def reads that isn't one of that def's params is an outer capture
   ; with no slot. opnd-ok? only ever guarded the TAIL; a trailing-call named-let `(\ n (: (go i a) (n - 3)
   ; (go 0 0)))` slips a free `n` into go's body, which the group lane then compiled reading go's LAST arg
   ; slot (n=0 -> -3). Guard every def body too -> a free-reading body DEOPTS to bytecode (sound), never
   ; miscompiles. gd = ((name p..) . body): ><gd = the def's frame params, >gd = its body.
   (gdefs-ok? gds) (all (\ gd (opnd-ok? ><gd >gd)) gds)
   ; natjit e s a gv farq lamq -- e = the bytecode entry (deopt target), s = (\ frame.. body)
   ; the surface src, a = arity, gv = ala's global resolver (this hook is post-egg; `book`/`gv`
   ; are mopped here, so ala hands its folded gv down for the call-out + amble lanes to
   ; resolve a free global), farq = ala's letrec-local predicate: (farq nom) is truthy iff
   ; nom is a letrec binding name up the compile scope chain -- such a name can look FREE
   ; in s (the weave re-analysis drops the let's own names from the import row) yet is
   ; wire-resolved locally, so NO lane may gv-resolve it: the book holds at best a stale
   ; leaked namesake (bref summing fref's float loop -- the rc3/rc4 class). lams is the
   ; compile scope's lambda-sibling list ((name . cell)..), passed VERBATIM (never a per-ala
   ; closure): a cell's contents (entry . import-names) are what lz freezes into a bytecode
   ; reference site, so amble compiles the SAME application (entry curried through the
   ; import names, all in the frame by fixcaps' pour) with no staleness bet; entry 0 (a
   ; forward ref, pre-weave) declines.
   ; nm = the letrec name this closure is being bound to (0 when anonymous) -- amble's
   ; self-call seam: a tail self-call loops, any other drives the blob's own cell.
   ; ti/ta = ev.l's iop/apx tag markers (uninterned, so only ala can hand them over):
   ; a synthesized surface wears them, and adesug reads them back to plain forms.
   ; inl = the inner-closure record under this lambda ((exp src-tail (entry . imports))..):
   ; amble finds a body's inner lambda site by the node and builds the same closure.
   ; ps = (init >s) = the frame [imps args]; body = the surface body. compile
   ; body over ps; deopt -> e. non-qualifying -> e unchanged.
   (natjit e s a gv farq lams nm ti ta inl)
    (: ps (init >s)  body (last >s)
       (? !(all name? ps) e                                ; a NAMELESS (mint) frame -- bsub's fresh binders, boxfix's
                                                           ; cells -- declines every lane. mints only reach emission at
                                                           ; all since cprop emits walked heads; the lanes were written
                                                           ; for NAMED frames and name?/nil? seams bite (adesug's net-0
                                                           ; unwrap was a real miscompile). bytecode is always sound,
                                                           ; and this is exactly the coverage the lanes had before.
          (two? s && two? >>s && exprn ps body)         ; >=1 param, a body, leaf grammar
          (: _ (pin fires () (1 + (peep fires () 0)))                  ; compile via the neutral-IR lane (coreir); deopt target = e
             (nif (emit (peep (asm arch (coreir arch ps body (deoptir arch (? (a = 1) 0 1)))) 'bytes 0)) e s a))
          ; FLOAT-RESULT leaf -- (\ x <fexpr>) that BOXES a gem (auto-ev's qualfr/jitfr lane).
          ; one param only (qualfr's shape), so a captured float leaf falls through for now. deopt -> e,
          ; not (ev s): jitfr bakes (ev lam), which under the hook would re-enter ala -> so jitfrx takes e.
          (qualfr s)
            (: _ (pin fires () (1 + (peep fires () 0))) (jitfrx s arch e))
          ; NOT a leaf -- try the counted-loop lane, then the general N-VAR loop lane (auto-ev's
          ; njit-loop / njit-loop-n, but deopting to `e`, the bytecode ala already built, NOT
          ; (base-ev s): re-interpreting s here would re-enter ala -> natjit -> ... So natjit passes
          ; e; owning the loop's back-edge is the big win over the leaf. The N-VAR lane's post-overflow
          ; deopt is cf-deopt's matrix-power closure (C-finite) OR e -- both bytecode under the hook.
          (: li (loopinfo s)
             (? li (: _ (pin fires () (1 + (peep fires () 0)))
                      (njit-loop s <li <>li <>>li <>>>li e))
                (: ln (loopinfo-n s)
                   (? ln (: _ (pin fires () (1 + (peep fires () 0)))
                            (njit-loop-n s <ln <>ln <>>ln <>>>ln <>>>>ln e))
                    ; CALL-OUT loop lane: a counted sum `(\ n (sum_{i<n} (F i)))` whose per-step term calls a
                    ; RESOLVABLE GLOBAL closure F -- glazed as the STACKLESS callout bridge (emit.l jitcalloutx),
                    ; F's value baked into value[1] as (F . twin), the two soundness gates guarding the result.
                    ; F is resolved via `gv` (ala's folded resolver; the hook can't reach the book). Single-param
                    ; (no imps) => F is free => a global (the captured / multi-arg sibling is the frame lane below).
                    ; Redefinition-stale like any baked global.
                    ; COST MODEL (why unconditional, no trip-count gate): glazing happens at ala/compile time, so
                    ; the trip count n is unknown -- a predictive gate is impossible. And it's unneeded: measured
                    ; ~1.5 ns/iter callout vs ~45 ns/iter interp (20-40x, F's cost cancels), so any real use wins.
                    ; The ~167us compile is one-time PER SOURCE SITE (creation reuses the blob, ~60 ns) -> amortized
                    ; after ~4k cumulative iters. Callout also beats falling to group-swallow here (~1.5 vs ~1.8
                    ; ns/iter, measured), so the recognizer order (callout before group) is already the cheap one.
                    (: ci (calloutinfo s)                                        ; ci = (F op iv ARG); resolve F via gv --
                       fv (? (ci && !(farq <ci)) (gv <ci 0) 0)         ; NEVER for a farq name (a wire-resolved local; the book namesake is stale)
                       (? (ci && lit? fv)
                          (: _ (pin fires () (1 + (peep fires () 0))) (jitcalloutx fv s <>ci <>>ci <>>>ci arch))   ; op iv ARG
                     ; CAPTURED / multi-arg call-out: F (and the bound) are FRAME params -> read from the
                     ; [imps args] frame, no bake; deopt to value[1]=e (the bytecode twin, which captures F).
                     (: cf (calloutinfo-frame s)                                 ; cf = (Fidx Nidx op ARG)
                        (? cf
                           (: _ (pin fires () (1 + (peep fires () 0))) (njit-calloutframe s <cf <>cf a e <>>cf <>>>cf arch))   ; op ARG
                      ; GROUP lane (always-on) -- a reusable closure
                      ; (\ frame.. <first-order mutually-recursive arith group>) whose tail calls the group.
                      ; auto-ev can't reach this (never gets the closure body as a callable-later unit); natjit
                      ; does, at ala-build. PURE path: parse the defs (anat-defs), op-cover (jgir-ok?), compile
                      ; with jitgroupirx and `e` as deopt -- NO ev/base-ev/autogroup (all re-enter ala mid-build
                      ; -> crash). Scope: a FIRST-ORDER group (no church/hof lifting); the tail is a flat call
                      ; (entry frame..) OR any arith over group calls (routed through a synthetic __outer entry).
                      ; at ala the body is post-feel: define-sugar `(fib n) B` is DESUGARED to `fib (\ n B)`,
                      ; so anat-defs yields dds = ((name . (\ p.. B))..). Convert to jitgroupir's ((name p..) . B).
                      ; TWO shapes: a FLAT tail (entry frame..) -- the outer closure IS the group entry, no
                      ; indirection; or a GENERAL tail (any arith over group calls -- (1 + (fib m)), a reordered
                      ; /computed-arg call, tree recursion) -- synthesize an entry ((__outer . frame) . tail) and
                      ; make IT the group entry, so the outer closure evaluates the tail then dispatches. opnd-ok?
                      ; rejects a tail that reads a captured var (jgir binds only the frame); jgir-ok? gates ops.
                      (: pd (? (two? body && id? <body ':) (anat-defs >body) ())
                         dds (? pd <pd ())  tail (? pd <>pd ())
                         okshape (pd && two? tail && all (\ dd (two? dd && name? <dd && two? >dd && id? <>dd '\ && two? >>dd)) dds)   ; every def is name (\ p.. B)
                         gdefs (? okshape (map (\ dd (: lam >dd ([<dd] + (init >lam) >< last >lam))) dds) ())    ; ((name p..) . B)
                         gnames (map (\ d <<d) gdefs)
                         flat? (okshape && member? <tail gnames && >tail = ps)       ; tail IS (entry frame..)
                         osym '__outer  agdefs (? okshape (gdefs + [((osym >< ps) >< tail)]) ())    ; group + synthetic ((__outer . frame) . tail)
                         (? (flat? && grp-ok? gdefs && jgir-ok? gdefs && gdefs-ok? gdefs)                         ; FLAT: entry direct, zero indirection
                            (: _ (pin fires () (1 + (peep fires () 0))) (jitgroupirx gdefs <tail arch e s))   ; stamp the OUTER src s -> =/show transparent vs the bytecode twin
                            (okshape && !(member? osym gnames) && opnd-ok? ps tail && grp-ok? agdefs && jgir-ok? agdefs && gdefs-ok? gdefs)   ; GENERAL tail via the synthetic entry
                            (: _ (pin fires () (1 + (peep fires () 0))) (jitgroupirx agdefs osym arch e s))
                            ; AMBLE lane (LAST): the universal boring-tier translation -- tagged values, Sp-slot
                            ; temporaries, drive call-outs, restart deopt. njit-amble answers 0 on decline.
                            (: aj (njit-amble s a e gv farq lams nm ti ta inl arch)
                               (? aj (: _ (pin fires () (1 + (peep fires () 0))) aj)
                                  (: _ (? (getenv "LOVE_AMBLE_TRACE2") (puts ("amble-DECLINE: " + (show s) + "\n")) 0) e)))))))))))))))   ; the decline trace (forensics: what the last lane passed on)
      natjit)                                          ; inner `:` BODY = the natjit closure (its ~20 glaze aliases stay private); close the inner scope
   (fired? n) (n < (peep fires () 0)))                ; did the hook fire since count n? (outer, leaked with natjit/fires)
