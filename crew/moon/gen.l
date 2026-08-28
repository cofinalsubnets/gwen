; crew/moon/gen.l -- the code generator. AST -> (1 ir-forms) | (), in holo's neutral IR
; (chibicc was the seed). every expression computes into r0 and CARRIES ITS TYPE -- cgexpr answers
; (type forms), so pointer arithmetic scales, a dereference loads by pointee width, and an
; array decays to an address. types: 'char 'short 'int 'long | ('ptr t) | ('arr t n); the ALU
; stays 64-bit (signed overflow is UB) and widths bite only at memory and casts. lvalues have
; their own door (clval: the ADDRESS in r0 + the pointee type), so x, *p and a[i] assign through
; one path. locals ride the r4 frame; globals and string literals are LABELED RAW DATA after the
; code, reached by holo's (la) -- one RWX segment carries text and data both until the .o seam.
; args ride the SysV registers r6 r5 r2 r1 r7 r8, at most six, evaluated left to right; a
; function-pointer call HEAD goes through (callr) after parking the callee in r3. a GLOBAL
; initializer whose value is a LABEL bakes an abs64 fixup ('fix 8 abs64 label 0). a `float` is
; 4 bytes in memory but ALWAYS a double in an xmm register (a load widens, a store narrows); a
; compound literal ('clit ty init) fills a frame temp whose ADDRESS is the lvalue.
; refused, not fudged: ptr+ptr, deref of a non-pointer, assigning to an array. a by-value
; aggregate rides x64; the lanes the other targets lack are doc/misc/moon-c-gaps' asymmetries.
; the PIPELINE'S STAGES are typed: crew/moon/stage.l holds each pass's sig (input stage ->
; output stage), and the moon gate checks this file as data against it -- a new pass declares
; its sig there, and a recomposition of the strict chains is a clash naming its seam.
(module 'moon
(use 'kore)
(use 'holo) ; the assembler module: asm-text, arbytes, the linker
(:
   ; ⚠ FIRST binding on purpose: a plain VALUE folds EAGERLY when the baking compiler reaches
   ; a reader (a lambda resolves lazily), and the fold order is NOT textual. the t32 pair push;
   ; spop2 lives with the other pair helpers.
   spush2 '((sub sp sp 16) (st sp 0 r0) (st sp 4 r1))   ; a VALUE (nullary defines quote -- the (f) trap)
   wpush '((sub sp sp 16) (st sp 0 r0) (st sp 8 r2))    ; the wide kit's cell (VALUES: they lead the book, like spush2)
   wdrop '((add sp sp 16))
   ; ⚠ an UNSIGNED 64-BIT source cannot ride cvtsi2sd -- the instruction is SIGNED, so every
   ; value from 2^63 up converts NEGATIVE. so SPLIT the word: both halves are 32-bit, hence
   ; positive and exact as signed, and hi*2^32 + lo rounds ONCE (both terms exact, so the sum
   ; is the correctly-rounded whole). llong/ullong fold to long/ulong off t32, so 'ulong IS the
   ; whole 64-bit unsigned case. a VALUE, so it leads the book beside spush2. named ul2d, not
   ; u2d: cvtlld's t32 lane binds a LOCAL u2d for the 32-bit pair's half of the same trick. --
   ul2d '((mov r1 r0) (shr r1 32)                     ; hi = x >> 32
         (shl r0 32) (shr r0 32)                     ; lo = x & 0xffffffff
         (cvtsi2sd f0 r1)
         (li r1 0x41f0000000000000) (movqxr f1 r1)   ; 2^32
         (mulsd f0 f1)                               ; hi * 2^32
         (cvtsi2sd f1 r0)
         (addsd f0 f1))
   cxdty '(struct "__cplx_double")                      ; the seeded complex-double tag (a VALUE: it leads the book, like spush2)
   ; --- the ISA families: the one place an op's membership is stated. every IR pass reads these
   ; -- the derived base/disp tablets, rdsp (the dataflow view), the member? sites in the sweeps.
   ; a new op joins its family HERE; an op off every list stays opaque (the safe default).
   ; ⚠ an op that can BASE on r4 must also join a4base + uoff (unframe proves frame-cleanliness
   ; there; a missed op re-frames the fn, and a framed fn that calls the VM faults -- glaze rides
   ; Ip in rbp) and, if it touches a frame slot, deadst's scans.
   ; ⚠ VALUES, so they lead the book beside spush2: rdsp reads them and rdsp's readers fold
   ; FIRST, so a family pinned BELOW them is read empty at that fold -- one missing scare per
   ; family per reader, and file mode stops on the first. ---
   ldops9 '(ld ld1 ld2 ld4 ldu1 ldu2 ldu4)          ; the gp loads (full + narrow)
   gstops '(st st1 st2 st4)                         ; the gp stores
   fldops '(ldsd ldss) fstops '(stsd stss)          ; the f-memory pair
   stops9 (gstops + fstops)                         ; every plain store (gp head first)
   leaops '(lea lean)                               ; the address makes (lean: consumed by its own use)
   xops '(leax ldx ldxb)                            ; the indexed forms (base index scale disp)
   stxops '(stx stxb)                               ; the indexed stores
   siops '(si si1 si2 si4)                          ; the store-immediates (the addressing fold + the asn/decl delivery lanes)
   cmpmops '(cmpm cmprm)                            ; the memory cmps (made only by cmpfuse)
   aluops '(add sub and or xor imul)
   ; the arm overflow lane (add/sub_overflow): an aluops SHAPE, but the flags feed the `set vs`
   ; right behind it -- so never pure (deaddef would lift it away) and never in aluops itself,
   ; where the two-address rewrites live. ⚠ without this rdsp answers 'bar and every analysis
   ; declines the whole function.
   ; the arm32 CARRY family rides here too: adcs/sbcs/ors set the flags, and adc/sbc READ the
   ; carry a preceding adds/subs left -- neither may be lifted, so the whole family is impure.
   flagops '(adds subs adcs sbcs adc sbc ors)
   divops '(div rem udiv urem)                      ; the division protocol (r0..r3 clobber)
   ; the t32 pair lane's widening multiply: (umull dl dh a b) defines TWO, (mla d a b acc)
   ; reads three. udivll/uremll are NULLARY -- the whole 64-step expansion is the form, and it
   ; owns the r0..r3 protocol quad (r4..r7 are pushed and popped inside it, so they survive).
   wmops '(umull smull)
   llops '(udivll uremll)
   vshops '(shlv shrv sarv rorv rorv4)              ; the variable shifts/rotates (count via r1)
   unops '(neg not sx1 sx2 sx4 zx1 zx2 zx4)         ; one-register rewrites
   xmmops '(addsd subsd mulsd divsd movsd ucomisd cvtss2sd cvtsd2ss)   ; xmm-only: no gp effect
   barops '(call callr raw sys)                     ; the opaque barriers
   ; the gen state g (a tablet): 'n the label counter, 'off the frame's
   ; high-water offset, 'brk/'cnt the loop label stacks, 'glob the global
   ; table (name -> (label ty init)), 'globs the emission order, 'strs the
   ; string table ((label . text)..). env: ((name off ty)..) frame locals.
   ; the dot parts prefix from count -- glued, ".d1" and ".d" share names
   (nlab g pfx) (: k (peep g 'n 0) (pin g 'n (k + 1), name (pfx + "." + show k)))
   argregs '(r6 r5 r2 r1 r7 r8)                      ; the SysV integer/pointer arg registers
   argregs-a64 '(r0 r1 r2 r3 r4 r5 r6 r7)           ; AAPCS64: x0..x7 (early: rasg reads them
   argregs-t32 '(r0 r1 r2 r3)                        ; AAPCS32: r0..r3  through argr -- a VALUE
                                                    ; binding resolves in source order)
   (xmmreg i) (name ("f" + show i))                ; the SysV double arg registers f0..f7
   (nthr l i) (? (i < 1) <l (nthr >l (i - 1)))
   (nthty (a >< u) i) (i < 1 ? a (nthty u (i - 1)))      ; the i-th list element or ()
   (bad? x) (id? x 'bad)                             ; the gripe carrier (gripe/gripe?/okv?) lives in lex.l -- parse answers one too
   ; -- the value tuple: every expression answers (ty reg forms) -- its type, the REGISTER
   ; holding the value, and the forms that compute it. the flo lane keeps the f0 protocol (its
   ; reg field is not consulted) and dispatches on vty. the accessors keep every consumer honest
   ; about which field it reads. --
   (vty v) <v
   (vreg v) <>v
   (vfs v) <>>v
   (mkv ty reg fs) [ty reg fs]
   ; the MEM face: an lvalue as a symbolic (base, offset) memory operand -- the ('mem b o)
   ; die's read side. a consumer lays ONE fused form; matv is the
   ; degenerate face (the address on r0, exactly what clval used to lay) for unmigrated lanes.
   (mkm ty b o fs) [ty ['m b o] fs]
   (vm? v) (bad? v ? () (two? (vreg v)))
   (vmb v) <>(vreg v)
   (vmo v) <>>(vreg v)
   (mata b o) (b @ 'r4 [['lean 'r0 'r4 o]] ; consumed, so lean (the & lane lays its own lea)
                   'r0 (o @ 0 () [['add 'r0 'r0 o]])
                   (o @ 0 [['mov 'r0 b]] [['add 'r0 b o]]))

   (matv g v) (? !(vm? v) v
                 (: b (vmb v)
                    _ (rfree g b)
                    (mkv (vty v) 'r0 (vfs v + mata b (vmo v)))))
   ; -- the operand pool: an expression value may live in a home-pool register instead of riding
   ; the r0 protocol. g 'pool0 is the fn's whole set (opool minus any param homes), g 'pool the
   ; free list, reset at every statement (values never cross one). ralloc answers a register | ()
   ; (dry -> the caller keeps the shuttle); rfree is idempotent and ignores anything outside pool0.
   ; ⚠ a pool value NEVER lives across a call: pool registers are caller-saved and, unlike the
   ; homes, have no spill wrap -- the callish? gate holds that line. --
   opool '(r6 r5 r9 r10 r7 r8)
   ; the vreg SHADOW: ralloc answers a minted nom (%v0 %v1 ..) mapped in
   ; g 'vrt to the physical register today's discipline picked, and vrfix substitutes over the
   ; assembled function as build's last link -- so no vreg reaches sibcall, and holo would refuse
   ; one LOUD. the mint is the pool's alone (homes, seats, pins, borrows stay physical), every
   ; g-table holds physicals only, and rp resolves a nom wherever a predicate asks about the
   ; MACHINE. ⚠ what rp must never mediate is a same-mint id?: an honored hint answers the mint
   ; object itself, and the free list guarantees two live mints never share a physical.
   (rp g rz) (g 'vrt rz || rz)
   (vmint? g x) ?(g 'vrt x)   ; is x a minted nom? (pre-vrfix only)
   ; ⚠ one MACHINE register? every hint-honor test rides this, never bare id?: an UNHELD hint
   ; returns to the pool at a splice body's psreset, a later ralloc re-mints its physical, and
   ; the value comes back wearing the new mint -- honored by ACCIDENT, and the accident is the
   ; discipline (the old code compared physicals, so the accident counted).
   (rpeq? g a b) (id? (rp g a) (rp g b))
   ; -- the ablation knob (the pare plan's rung 0): MOON_ABLATE="tpool,cs" holds named
   ; mechanisms empty for PRICING -- ralloc tpool cs lhome homes pcs, obuild
   ; (the one build falls back to the ir1+regen dance), pcell
   ; (param cells fall back to the wrapped-home/cs-grant lanes), alloc
   ; (locals homes fall back to lpick's static pick), pdefcs
   ; (no cs seats for crossing chains), and pdef
   ; (promotion falls back to the hull window). read at
   ; RUN time, once per unit (a value binding folds at bake and would carry the baking
   ; session's environment). no gate rides it; unset is every mechanism on. --
   (ablenv _)
    (: s (getenv "MOON_ABLATE")
       (? !(string? s) ()
          (: n #s
             (cut i j acc) (i < j ? (name (snip s i j) >< acc) acc)
             (go i j acc)
              (? (n <= j) (cut i j acc)
                 (member? (s j) '(44 32)) (go (j + 1) (j + 1) (cut i j acc))
                 (go i (j + 1) acc))
             (go 0 0 ()))))
   (abl? g nm) (member? nm (g 'abl))
   ; the one-build world (moon-ssa rung 6, the x64 DEFAULT): any old-world knob
   ; keeps the dance byte-identically, and so does a non-x64 target until rung 7
   ; calibrates it. alive's widened universe and repack's paired-cs reuse ride
   ; exactly this world -- the three gates must never drift apart.
   (uniw? g) (!(a64? g || t32? g || rv? g) &&
              !(abl? g 'obuild || abl? g 'pcell || abl? g 'alloc ||
                abl? g 'pdefcs || abl? g 'pdef))
   (ralloc g) (? !(abl? g 'ralloc)
               (g 'pool @ (r9 >< u)
                ; ⚠ armor: a pinned reg on the free list is the free-list/pin split
                ; (the callish-aim window) resurfacing -- refuse LOUD
                (member? r9 (g 'rpin) ? (scare 'ralloc-pinned r9),
                 pin g 'pool u,
                 (: k9 (peep g 'vrn 0)
                    v9 (name ("%v" + show k9))
                  (pin g 'vrn (k9 + 1),
                   pin (g 'vrt) v9 r9,
                   v9)))))
   (rfree g rz) (: r (rp g rz)
                  (member? r (g 'pool0) &&
                 !(member? r (g 'pool)) &&
                 !(member? r (g 'rpin)) ?             ; a splice's register bind is not ours to free
                  (pin g 'pool (r >< g 'pool))))
   (wr? g rz) (? (id? rz 'r0) 1                        ; writable: protocol r0 or an OWNED pool reg
                 (: r (rp g rz)
                    (member? r (g 'pool0) &&   ; (a borrowed home or a bind
                     !(member? r (g 'rpin)))))  ;  reg is read-only)
   ; -- the ASSIGNMENT (5.1b): move a mint onto its copy's other side, so the copy
   ; substitutes to (mov X X) and the sweep drops it. LOCAL by construction: a move is
   ; licensed only over a straight-line window (measured: every interval-licensed drop in
   ; love.c already was one), and every rule is a mention-list scan -- no fixpoint rides the
   ; build tail. the token stays the default, so a refused move changes nothing and a leaked
   ; mint still dies LOUD at holo. two shapes: the BRIDGE (mov X %m) at %m's death -- the
   ; classic coalesce, X takes the def -- and the ADOPTION (mov %m X) at %m's birth, the
   ; call-return mov coal can never reach (its def is a call, rdsp bars it). the rules:
   ;   window   -- no control/opaque form strictly inside the span
   ;   tenancy  -- no bare-X mention inside the span but the copy itself, and no co-tenant
   ;               mint on X whose mention span overlaps
   ;   release  -- %m's token T still LOOKS occupied downstream (a mixed-name
   ;               continuation reads T bare): a bare-T mention inside the span, or a
   ;               bare-T read after it before a def, refuses -- and a branch/label before
   ;               the answer refuses too, since control could route to one (a call/ret is
   ;               the safe stop: T is caller-saved and never in lvret)
   ;   adoption is %m's ONLY def, so X is never rewritten through it
   ; most builds carry no candidate at all, and the mention walk is not free in the build
   ; tail -- so one cheap pass for an adoption-shaped mov gates the whole engine
   (rasg g fs)
    (: cfl (rafile g)
       (any9 (f >< u))
        (two? f && id? <f 'mov && vmint? g <>f && !(vmint? g <>>f) && member? <>>f cfl || any9 u)
       (any9 fs ? (rasg2 g fs cfl)))
   ; ⚠ no r4 on the arm family: a4ize retargets a base-slot r4 to fp BY POSITION, and a
   ; moved mint can sit in a base slot (a pointer riding the pool) -- the a4rider? law.
   ; ⚠ and no ARGUMENT register anywhere: the ride analysis prices ir1 as built (no
   ; foreign def of an arrival), so a mint moved onto one drops rides and the regen
   ; takes worse lanes -- lvm_band grew a spush cell the day this was learned.
   (rafile g)
    (filter (\ r9 !(member? r9 (csregs g) || id? r9 (fbase g) || id? r9 'sp ||
                    member? r9 (argr g) ||
                    (arm? g && id? r9 'r4)))
            (lvgp g))
   (rasg2 g fs cfile)
    (: t (g 'vrt)
       mt {}                                 ; nom -> ([i kind] ..) descending
       st {}                                 ; position lists (descending) + 'cd
       (padd k i) (pin st k (i >< st k))
       (madd nom i k) (pin mt nom ([i k] >< mt nom))
       ; one walk: mentions per nom (rdsp), the control classes, the mov candidates. the
       ; classes: 'ctl bars a WINDOW (any control or clobber inside a span refuses); for the
       ; release scans 'jout leaves linearly (refuse), 'stop kills a caller-saved value
       ; (clear), 'ret ends reading lvret. a label is windows-only: the leftover question is
       ; linear, and fallthrough carries it straight past one.
       (w1 (f >< u) i)
           (: _ (two? f ?
                   (: op <f
                      (? (id? op 'label) (padd 'ctl i)
                         (member? op '(jmp br jmpr sys)) (: _ (padd 'ctl i) (padd 'jout i))
                         (member? op '(call callr ud2)) (: _ (padd 'ctl i) (padd 'stop i))
                         (id? op 'ret) (: _ (padd 'ctl i) (padd 'ret i))
                         (id? op 'align) ()
                         (: rd9 (rdsp f)
                            (? (id? rd9 'bar) (: _ (padd 'ctl i) (padd 'jout i))
                               ; (no per-slot safety net: rdsp enumerates every register
                               ; slot of every op a mint can ride -- the checker instrument
                               ; verified the mention model against full liveness over
                               ; love.c on all four targets, zero aliens)
                               (: (gr (a >< u)) (madd a i 'r, gr u)
                                  (gd (a >< u)) (madd a i 'd, gd u)
                                  _ (gr <rd9)
                                  _ (gd <>rd9)
                                  _ (id? op 'mov ? (pin st 'cd ([i <>f <>>f] >< st 'cd)))
                                  ()))))))
              (w1 u (i + 1)))
       _ (w1 fs 0)
       mtr {}                                ; nom -> mentions ascending (memoized)
       (mup nom) (: o (peep mtr nom 'none)
                    (? (id? o 'none)
                       (: v (rev (peep mt nom ())) _ (pin mtr nom v) v) o))
       (fpast l hi) (: (go2 l2 b) (? !(two? l2) b    ; descending bares: the smallest one > hi
                                     (<l2 > hi) (go2 >l2 <l2)
                                     b)
                       (go2 l ()))
       (posin? l lo hi exc)                          ; an ascending mention in [lo..hi] off exc?
        (? !(two? l) ()
           (: p <(<l) (? (p > hi) ()
                         ((p >= lo) && !(member? p exc)) 1
                         (posin? >l lo hi exc))))
       (ctlin? (a >< u) lo hi)  ; descending bare list: any strictly inside?
        (? (a >= hi) (ctlin? u lo hi)
           (a > lo) 1)
       (minp l) (atom? >l ? <<l (minp >l))
       ; the release scan: does the register's LEFTOVER matter past `from`? walk linearly:
       ; the earliest event past `from` decides -- a jump-out refuses (the leftover flows
       ; where we cannot look), a call clears (caller-saved dies there), a ret clears unless
       ; the register is in lvret (an invisible read), a mention clears on a def and refuses
       ; on a read, and no event at all is clear (a route ends AT a mention, so no mention
       ; anywhere past means no reader anywhere).
       (rel? nom from)
        (: (go2 ml)
            (: p (: (nx (a >< u)) (<a > from ? [<a <>a] (nx u)) (nx ml))
               jp (fpast (st 'jout) from)
               sp (fpast (st 'stop) from)
               rp (fpast (st 'ret) from)
               big 1000000000
               pv (two? p ? <p big)
               jv (jp || big)
               sv (sp || big)
               rv (rp || big)
               (? (pv = big && jv = big && sv = big && rv = big) 1
                  (jv < pv && jv < sv && jv < rv) 0
                  (sv < pv && sv < rv) 1
                  (rv < pv) !(member? nom (lvret g))
                  (id? <>p 'd) 1))
           (go2 (mup nom)))
       ; co-tenants: every mint's mention span, listed under its CURRENT register
       tox {}
       (seed ms) (two? ms ?
                  (: m9 <ms l (mt m9)
                   (two? l ? (pin tox (t m9) ([(minp l) <<l] >< tox (t m9))),
                    seed >ms)))
       _ (seed (keys t))
       (cot? (s9 >< u) lo hi)                                ; a co-tenant span overlaps [lo..hi]?
        (<s9 <= hi && lo <= <>s9 || cot? u lo hi)
       mvd {}                                ; a mint moves at most once
       (att l)                                       ; candidates in form order, first wins
        (? !(two? l) ()
           (: c9 <l i <c9 D <>c9 S <>>c9
              dm (vmint? g D) sm (vmint? g S)
              m (? (dm && !sm) D (sm && !dm) S)
              _ (? m
                   (: X (? dm S D)
                      cur (t m)
                      dl (mt m)              ; descending; head is the last mention
                      (? (id? cur X) ()              ; already identical: the sweep takes it
                         (mvd m) ()
                         !(member? X cfile) ()
                         (atom? dl) ()
                         (: ml (mup m)
                            lo9 (? dm i (minp dl))
                            hi9 (? dm <<dl i)
                            ndf (: (nd l2 n9) (? !(two? l2) n9
                                                 (nd >l2 (? (id? <><l2 'd) (n9 + 1) n9)))
                                  (nd ml 0))
                            ; adoption: the copy is m's FIRST mention and a def. ⚠ the
                            ; BRIDGE direction stays off: moving a death-copy onto r0
                            ; robs stld's forwarding through the staging cell (the value
                            ; leaves its stable register for the one the target load
                            ; clobbers) -- lvm_band bought a whole spush pair back
                            ok (dm && (<(<ml) = i) && id? <>(<ml) 'd)
                            ; the pair's CLOSING copies are exceptions, not tenants -- the
                            ; round trip r0 -> %m -> r0 coalesces whole this way. only the
                            ; closing direction: an adoption's later (mov X %m) hands X the
                            ; value either way, where a RE-adoption (mov %m X) resets m to
                            ; X's old value and must stay a refusal.
                            exc (: (gx l2 a2)
                                    (? !(two? l2) a2
                                       (: e9 <l2 p9 <e9
                                          hit (? dm (id? <>e9 X && id? <>>e9 m && p9 > i)
                                                 (id? <>e9 m && id? <>>e9 X && p9 < i))
                                          (gx >l2 (? hit (p9 >< a2) a2))))
                                   (gx (st 'cd) [i]))
                            (? !ok ()
                               (ctlin? (st 'ctl) lo9 hi9) ()
                               (posin? (mup X) lo9 hi9 exc) ()
                               (cot? (tox X) lo9 hi9) ()
                               (posin? (mup cur) lo9 hi9 ()) ()
                               !(rel? cur hi9) ()
                               ; a multi-def adoption rewrites X in place, so X's own
                               ; leftover must be dead past the span too (the pair's
                               ; closing copy at hi9 is a def of X and clears it)
                               (dm && 1 < ndf && !(rel? X (hi9 - 1))) ()
                               (: _ (pin t m X)
                                  _ (pin mvd m 1)
                                  _ (pin tox X ([lo9 hi9] >< peep tox X ()))
                                  1))))))
              (att >l)))
       _ (att (rev (st 'cd)))
       ())
   ; the shadow's other half: substitute every mint over the assembled prologue+body+epilogue,
   ; the INNERMOST build-tail link -- sibcall matches epilogue shapes and soften does register
   ; arithmetic, so neither may meet a vreg. a build that never minted skips the walk; one
   ; that did gets the assignment first, then the identity movs it minted sweep out.
   (vrfix g fs)
    (? !(peep g 'vrn 0) fs
       (: _ (rasg g fs)
          t (g 'vrt)
          (fix1 x) (two? x ? (map fix1 x) (: p9 (t x) (p9 ? p9 x)))
          (sweep (f >< u)) (f @ ('mov x x) (sweep u) (f >< sweep u))
        (sweep (map fix1 fs))))
   (callish? (a >< u))                                ; does the AST reach a call? (an inline splice still may)
    (a = 'call || callish? a || callish? u)
   ; does the AST WRITE anything? a face's base register must survive the rhs, so a
   ; store rides the face only past a write-free right side (calls clobber the pool too)
   (wrany? (a >< u))
    (member? a '(asn post call asm vastart vaarg) || wrany? a || wrany? u)
   (tor0 g v)                                       ; land a value on the r0 protocol (frees its pool reg)
    (? (id? (vreg v) 'r0) (vfs v)
       (: rd (vreg v) rl (rev (vfs v))
          lf (two? rl ? <rl)
          ; a trailing leax retargets to r0 outright -- lea reads its sources before the dest
          ; write, so even an r0 index is licensed, and the deref's load then sits same-dest
          (? (two? lf && id? <lf 'leax && id? <>lf rd && wr? g rd)
             (: fs (rev (['leax 'r0 <>>lf <>>>lf (nthr lf 4) (nthr lf 5)] >< >rl))
              (rfree g rd,
               fs))
             (: fs (vfs v + [['mov 'r0 rd]])
              (rfree g rd,
               fs)))))
   (vdrop g v) (: fs (vfs v) (rfree g (vreg v), fs))   ; the value is DISCARDED: keep the effects, free the reg
   ; the result register for an op consuming rA: reuse a writable operand,
   ; else a fresh pool reg, else the protocol r0
   (rdst g rA) (wr? g rA ? rA (: rX (ralloc g) (rX || 'r0)))
   ; -- WHAT A REGISTER COSTS, per class, in memory operations. every gate that grants one
   ; weighs its own EXTENT -- the traffic the residency erases -- against this. the extents
   ; differ on purpose and stay where they are (ir1 touches, weighted source reads, unerasable
   ; reloads: each gate knows something the others do not); the COST is one table. `n` counts
   ; the events that class pays AT, and is the gate's own too -- a roster pays per call in
   ; scope, a seat per exit, a home per call its path passes.
   ; ⚠ a gate's n and ext must be counted on the same CLOCK, per invocation or per iteration.
   ; this door cannot check it -- both numbers arrive from the one caller -- so every call
   ; below names its clock, and mixing them is the error this arc has paid for twice.
   (rcost cls n)                                    ; -> memory ops per register, or 'never
    (cls @ 'ros  n                                  ; a rostered pin reloads at every call in scope
           'bor  (1 + n)                            ; a seat: one save, then one reload per exit
           'home (2 * n)                            ; a caller-saved home: the wrap PAIR per call
           'pool (? (0 < n) 'never 1)               ; ⚠ not a price: a pool home cannot cross a call
           'bind 7                                  ; a splice bind's park. ⚠ MEASURED (loads +0.36%
           'never)                                  ;   below it), not derived like the rows above
   (rprice cls n ext)                               ; the MARGIN: ops saved, net of ops spent
    (rcost cls n @ 'never 'never c (ext - c))
   (rpays? cls n ext)                               ; ...and it must WIN
    (rprice cls n ext @ 'never 0 m (0 < m ))
   ; -- frame-direct: a constant-indexed frame-array element is a STATIC slot (off + k*elsize) --
   ; no address compute at all. the address is consumed by its own load/store, so this holds for
   ; ESCAPING arrays too; pair/d128 elements keep the address lanes. --
   (afd g env e)                                    ; -> (off elty) | ()
    (e @ ('deref ('bin '+ ('var nm) ('num i && 0 <= i)))
           (: en (vent g env nm)
              (? !(two? en && id? <en 'loc && arr? <>>en) ()
                 (: et <>(<>>en)
                    (? ((ptr? et || member? et '(char uchar bool short ushort int uint long ulong)) &&
                        !(pair? g et))
                       [(<>en + i * tsz g et) et])))))
   ; ⚠ may an ALIAS live across a splice body? only in a register the body's STAGING can
   ; never write. x64 staging owns exactly the protocol quad r0-r3 (r3 the callr park), so
   ; every pool/home/cs register aliases -- but a seat-2/3 ride (r2/r1) must not. elsewhere
   ; the arrivals r0-r7 are staging-adjacent (a64's shuttle rides r0-r3, spush2 r2/r3), so
   ; only the pool/cs families are safe -- an arrival alias there MISCOMPILES (getenv's
   ; PATH walk was the catch: a ride aliased across a body that staged through it).
   (alsafe? g rz)
    (: r (rp g rz)
       (? (x64t? g) !(member? r '(r0 r1 r2 r3))
          (member? r (g 'pool0) || member? r (cspool g))))
   ; and may THIS param alias? only if the body never WRITES it (asn/post on its var): the
   ; register holds the CALLER's live value, so the splice's own p++ would walk it.
   ; ⚠ a C name is a STRING here, so the compare is `=` -- `id?` answers false for every
   ; distinct one, and a scan that never matches is a gate that isn't there.
   (wrs? nm e)
    (e @ (('asn || 'post) . ('var ^nm) . _) 1
     (: (go (a >< u)) (wrs? nm a || go u) (go e)))
   ; how hard does the body lean on this param? reads weighted x8 per loop level (the alive
   ; pass's convention) -- the price of a whole-body park is paid in reloads saved.
   (rdw nm e w)
    (? !(two? e) 0
       (e @ ('var ^nm) w
            (: w9 (? (member? <e '(while do for)) (w * 8) w)
               (go l a) (? !(two? l) a (go >l (a + rdw nm <l w9)))
               ((? (two? <e) (rdw nm <e w9) 0) + go >e 0))))
   ; the per-statement pool reset: pool0 minus the splice binds
   (psreset g)
    (: vp (g 'rpin)
       (? !(two? vp) (pin g 'pool (g 'pool0))
          (pin g 'pool (filter (\ r9 !(member? r9 vp)) (g 'pool0)))))
   ; -- the TARGET seam: g 'tgt ('amd64 | 'arm64 | 'thumb2) picks the ABI shapes; the IR stays
   ; neutral. arm64 (AAPCS64): args ride r0..r7, the frame base retargets to fp (x29) at the end
   ; of build (a4ize -- r4 is x4, a CALLER-saved arg register there), bl clobbers lr so the
   ; prologue saves it. thumb2 (AAPCS32, Cortex-M7): a 32-bit WORD (wsize=4), args in r0..r3, the
   ; same lr-push + fp-base + `bx lr` frame shape as arm64 (`arm?` covers both) -- r4 is
   ; callee-saved there, but riding fp keeps the arm64 build/epilogue path verbatim.
   ; register-homing is OFF there (nhome=0): every param spills to its frame slot. --
   (a64? {'tgt 'arm64}) 1
   (t32? g) (member? (g 'tgt) '(thumb2 thumb2sp thumb1))   ; the 32-bit thumb ABI family (AAPCS32): ARMv7E-M + ARMv6-M share it
   (sp64? {'tgt 'thumb2sp}) 1       ; ARMv7E-M with an SP-ONLY FPU (the playdate's STM32F746): f64 arithmetic softens
   (v6m? {'tgt 'thumb1}) 1          ; ARMv6-M refinements ONLY: narrower immediates, no imm-mul, soft-divide, pop-pc epilogue
   (rv? {'tgt 'rv64}) 1          ; RV64IMFD (LP64): 64-bit, 8 gp args, hard doubles, the lr/fp frame
   (a8? g) (a64? g || rv? g)                        ; the 8-gp-arg LP64 pair: arm64 + riscv share the AAPCS64 shapes
   (arm? g) (a64? g || t32? g || rv? g)             ; the lr-push / fp-base frame family (riscv rides it whole: fused
                                                    ; fcmp conds read gt/ge/lt/le ordered like VCMP, trap is ebreak,
                                                    ; overflow rides adds/subs + set vs, mul imm materializes)
   (wsize g) (? (t32? g) 4 8)                       ; the machine WORD in bytes (a pointer, a gp scalar, a stack cell)
   (argr g) (? (a8? g) argregs-a64 (t32? g) argregs-t32 argregs)   ; riscv a0..a7 map onto the same abstract r0..r7; rosters up top (rasg)
   (ngp g) (? (a8? g) 8 (t32? g) 4 6)
   (pkr g) (? (a64? g) 'r15 (t32? g) 'r12 (rv? g) 'r12 'r3)   ; the callr park (x64 rbx; arm64 x15; thumb2 r12/IP -- x3/r3 is the 4th ARG register; riscv t4, clear of args and its pool)
   (ip2 g) (? (a8? g) 'r1 'r2)                      ; an int-pair aggregate's SECOND register (rax:rdx / x0:x1 / a0:a1); thumb2 unused for M1 scalar
   (psc g) (? (a8? g) 'r9 'r0)                      ; the xmm-arg pop scratch (arm64/riscv r0 IS arg 0; a pool reg is dead at a call)
   (ovb g) (? (a8? g) 32 (v6m? g) 12 (t32? g) 8 16)   ; overflow params above the push cells: x64 [ret+rbp]=16; arm64/riscv 2x16; thumb2 lr+fp=8; v6m lr+fp+r4=12
   ; the arm64 HOME pool: caller-saved, never an argument register (x0..x7 all stage args, so no
   ; positional alignment with the incoming regs -- every home takes one prologue mov), and clear
   ; of the staging scratches (x9 the xmm pop scratch, x15 the callr park, x16 holo's far offset).
   hregs-a64 '(r10 r11 r12 r13 r14)
   hregs-t32 '(r5 r6 r7 r8)                          ; thumb2 callee-saved home pool (inert while nhome=0)
   (hregs g) (? (a64? g) hregs-a64 (t32? g) hregs-t32 opool)   ; the home pool (x64 rides opool, positionally arg-aligned)
   (nhome g) (? (a64? g) 5 (t32? g || rv? g) 0 6)       ; how many gp positions can home (thumb2 + riscv: none yet -- spill all params)
   ; the x64 CALLEE-SAVED pool (r11..r14 -> machine r12..r15): overflow homes for locals in
   ; non-leaf fns. a home here rides THROUGH calls unwrapped -- the callee preserves it -- at
   ; the price of one save/restore pair in the owner's frame (arm64's x19+ rungs: unclimbed)
   ; the seats residency may borrow: a subset of csregs, saved by the prologue (g 'cssv) and
   ; reloaded on every exit. ⚠ a target belongs here only once its prologue SPLICES cssv and
   ; cskeep verifies it.
   ; ⚠ NO r4 IN ANY ARM POOL -- the law is at the top of this file, and a pool is exactly where
   ; it bites: a4ize renames a base-slot r4 to fp BY POSITION, and a resident mint riding the
   ; pool CAN sit in a base slot, so the rename turns a live pointer into the frame pointer.
   ; t32 held r4 here for a day on the reading that AAPCS32 gives it the whole bank bar fp; it
   ; does, and the bank is not the question -- every other pool starts clear of r4 for this
   ; reason (a64 r19+, riscv r13+, x64 r11+), and v6m for its own (it plants r4 as a BOTTOM
   ; frame base after the sub). the M7 reset before it could print a second line.
   ;; ⚠ riscv leads with r15: seats fill from the head, and r15 is s1 -- the ONE callee-saved
   ;; register inside the RVC window (x8..x15 is s0, s1, a0..a5, and the rest is ABI-locked),
   ;; so the hottest seat is the only one whose ld/sd/mv can wear a 2-byte form.
   (cspool g) (? (abl? g 'cs) ()
                 (a64? g) '(r19 r20 r21 r22 r23 r24 r25 r26 r27 r28)
                 (rv? g) '(r15 r13 r14 r19 r20 r21 r22 r23 r24 r25 r26)
                 (v6m? g) '(r5 r6 r7 r8 r9 r10)
                 (t32? g) '(r5 r6 r7 r8 r9 r10)
                 (arm? g) ()
                 '(r11 r12 r13 r14))
   (deep? pl) (two? >>pl)                           ; a pool affords a PRICED seat: two remain after the take
   (fbase g) (arm? g ? 'fp 'r4)                   ; the frame base as post-build ir1 spells it (a4ize); arm64+thumb2 fp, x64 r4
   ; -- a DELIBERATE refusal names itself. `'bad` here is TWO answers wearing
   ; one face: "I decline, take another lane" (a policy deopt, ordinary and recoverable) and
   ; "there is no lane for this, anywhere" (a target gate, an ABI class never built). only the
   ; second is a DIAGNOSIS, so only the second speaks -- `nolane` pins the phrase and answers the
   ; same bare bad, and gfns/cgdata read the pin ONLY where the function refuses outright.
   ; LAST pin wins: the deepest site reached before the unwind is the one that knows. --
   (nolane g txt) (pin g 'nolane txt, 'bad)
   (ontgt g) (" on " + show (peep g 'tgt 'amd64))      ; " on arm64" -- for a phrase whose answer turns on the target
   (no128 g) (nolane g ("no lane for 128-bit integer arithmetic" + ontgt g))   ; every d128 site, one phrase
   ; the immediate gate: x64 ALU/cmp immediates ride le32; arm64 add/sub/cmp
   ; take imm12 (a negative flips, a 24-bit value splits in the backend), the
   ; logicals only a bottom-aligned bitmask (2^n - 1), multiply none at all.
   ; thumb2: ADDW/SUBW take a plain imm12 (0..4095, negative flips); cmp an imm8
   ; (0..255, CMN on negatives); the logicals/mul have no simple immediate form.
   (immok g cls n)
    (? (a64? g)
       (cls @ 'addsub (-16777215 <= n <= 16777215)   ; imm12, or the backend's hi12+lo12 split
              'cmp (-4095 <= n <= 4095)              ; imm12 only (no split -- flags)
              'log (0 < n && 0 = (n & (n + 1))))
       (v6m? g)
        (cls @ 'addsub (-255 <= n <= 255)             ; v6-M ADDS/SUBS imm8 (imm3 3-addr / imm8 2-addr; negative flips)
               'cmp (0 <= n <= 255))                  ; CMP imm8 only -- no CMN-imm on v6-M, so materialize negatives
       (t32? g)
        (cls @ 'addsub (-4095 <= n <= 4095)           ; ADDW/SUBW imm12
               'cmp (-255 <= n <= 255))               ; CMP/CMN imm8
       (fits32? n))
   ; the fold side of the gate: may this op take n as its immediate here? (the
   ; sweeps ask before rewriting a reg-reg form -- a decline keeps the register)
   (imma g op n) (op @ 'imul !(arm? g)
                       ('add || 'sub) (immok g 'addsub n)
                       'cmp (immok g 'cmp n)
                       (immok g 'log n))
   ; -- the type floor rides crew/moon/floor.l (shapes, sizes, the typing door): g carries only
   ; the two things the floor takes as parameters -- the machine word and the stag table. --
   (uldop sz) (sz @ 1 'ldu1 2 'ldu2 4 'ldu4 'ld)   ; unsigned load of a sz-byte word
   (fnty g nm) (: sig (g 'sigs nm)        ; a named function's type, return riding when sig'd
                (two? sig ? ['fn <sig] '(fn)))
   ; by-value aggregates (structs AND unions): a struct VALUE in cgexpr is its ADDRESS in r0, and
   ; params/args/returns/results read the eightbytes through it. aclass answers the SysV eightbyte
   ; classes -- '(int) / '(sse) at <=8 bytes, a two-symbol list at 9..16, () for a scalar, an
   ; unsized tag, or >16 bytes (MEMORY). carried: one eightbyte, plus the two uniform 16-byte
   ; pairs '(sse sse) and '(int int); MIXED pairs and MEMORY refuse. an eightbyte is SSE only when
   ; every member overlapping it is float/double; any integer/pointer member wins it for INT, and
   ; unions flatten to offset-0 fields (parse.l) so the same overlap walk classifies them.
   ; eightbyte moves ride full 8-byte loads/stores (a 1..7-byte tail reads its padding).
   (aclass g ty)
    (? !(stru? ty) ()
       (: se (peep (g 'stag) <>ty 0)
          (? !(two? se) ()
             (16 < <>se) ()
             (: cls {}
                (mark o c) (: i (o >> 3) old (cls i)
                              (pin cls i (id? old 'int || id? c 'int ? 'int 'sse)))
                (walk o mty)
                 (? (flo? mty) (mark o 'sse)
                    (arr? mty)
                     (: esz (tsz g <>mty)
                        (go i) (? (<>>mty <= i) 1
                                  (: _ (walk (o + (* i esz)) <>mty) (go (i + 1))))
                        (go 0))
                    (stru? mty)
                     (: e2 (peep (g 'stag) <>mty 0)
                        (? !(two? e2) 1 (fields o <e2)))
                    (mark o 'int))
                (fields o fs)
                 (? !(two? fs) 1
                    (: f <fs _ (walk (o + <>f) <>>f) (fields o >fs)))
                _ (fields 0 <se)
                (? (<>se <= 8) [(peep cls 0 'int)]
                   [(peep cls 0 'int) (peep cls 1 'int)])))))
   (acls2 g ty ca cb) (aclass g ty @ (^ca ^cb) 1)
   (sse2? g ty) (acls2 g ty 'sse 'sse)       ; both eightbytes SSE: the xmm0:xmm1 pair
   (ipair? g ty) (acls2 g ty 'int 'int)      ; both INT: rax:rdx (return/result lanes)
   (aone g ty) (: c (aclass g ty) (two? c && !(two? >c) ? <c))
   (aeb2 g ty) (: c (aclass g ty) (two? c && two? >c ? c))       ; a 9..16B aggregate's TWO eightbyte classes (c0 c1), else ()
   ; the MEMORY class: an aggregate PAST 16 bytes (aclass answers ()). SysV passes it wholly on
   ; the stack -- the caller lays its eightbytes in the overflow block, and the callee's copy IS
   ; that memory, addressed off the incoming frame. x64 only (AAPCS64 differs). the 16+-alignment
   ; guard is UNREACHABLE today (cc ignores __attribute__((aligned))) and refuses rather than
   ; laying a wrong ABI, for the day alignment is honored.
   (amem? g ty) (stru? ty && !(aclass g ty) && !(arm? g) && !(t32? g))
   (asalign g ty) (: e (peep (g 'stag) <>ty 0) (two? e ? <>>e 8))        ; a struct tag's alignment
   (aslots g ty) (((tsz g ty + 7) & -8) >> 3)                              ; its eightbyte count
   ; a MEMORY-class RETURN: the caller passes a hidden pointer as the implicit
   ; first gp argument, the callee fills that memory and answers the pointer.
   ; the v6-M base ABI does it past 4 bytes; SysV past 16 (aclass ()).
   (sretm? g ty) (stru? ty && (v6m? g && 4 < tsz g ty || amem? g ty))
   (fnret ty) (? !(two? ty && <ty = 'ptr) 'long        ; an indirect callee's result type: ('ptr ('fn ret)) | 'long
                 (: ft <>ty (fn? ft && two? >ft ? <>ft 'long)))
   ; unsigned scalars ride 'uchar 'ushort 'uint 'ulong -- same WIDTH as their signed
   ; twin, but a load zero-extends, `>>` is logical, compare/divide go unsigned, and
   ; a binary op is unsigned if EITHER operand is (simplified usual conversions).
   (usg? ty) (member? ty '(uchar ushort uint ulong ullong u128))
   ; llong/ullong exist only on t32 (canon folds `long long` to 'long on the 64-bit targets):
   ; the 8-byte REGISTER PAIR, lo in r0, hi in r1 -- the one type wider than the accumulator.
   (llt? ty) (ty = 'llong || ty = 'ullong)
   ; ⚠ THE SIGNEDNESS IS C'S, and C's is not "either operand is unsigned". 6.3.1.8 is a
   ; RANK rule, and `puac` (floor.l) is the tree's one spelling of it: where the signed
   ; type is strictly wider it represents every value of the unsigned one, so the common
   ; type stays SIGNED -- `long long` meets `unsigned int` at long long. Reading that pair
   ; as unsigned made a 64-bit signed divide go logical, and sqlite is where it showed:
   ; LARGEST_INT64 is `0xffffffff | ((i64)0x7fffffff<<32)`, so sqlite3MulInt64's
   ; INT64_MIN/2 came back POSITIVE, every multiply looked like an overflow, and
   ; `SELECT 4294967296*2` answered a REAL where every other sqlite answers an integer.
   ; the WIDTH is still the accumulator's, not C's -- that part was always deliberate.
   (ubin w64 ta tb)
    (: c (puac w64 ta tb)
       (? (llt? ta || llt? tb) (tusg? c ? 'ullong 'llong)
          (tusg? c) 'ulong 'long))
   ; ..and `ubin` deliberately WIDENS: it answers the accumulator's type, not C's. right for every
   ; op whose result cannot leave its operands' range, wrong for the four that CAN carry past bit
   ; 31 -- `+ - * <<` on a 32-bit unsigned result, which C wraps and a 64-bit register does not.
   ; `unsigned int` is the only sub-word type a binary result can wear (the usual conversions
   ; promote everything narrower to int), and signed intermediates ride 64-bit sign-extended, so
   ; only the unsigned face re-narrows -- what the unary lane does too (renar, its twin). on t32
   ; the accumulator IS 32 bits and the wrap is free, so this is 64-bit only.
   ; ⚠ the TYPE must be right for EVERY integer binary op, not just the carrying four: `&` cannot
   ; carry, but if `(x & -x)` reads back as 'ulong then the multiply wrapped around it sees a
   ; 64-bit operand and skips its own narrow. so rbin corrects ubin everywhere, and only + - * <<
   ; also re-narrow.
   (u32? g ty) (!(t32? g) && id? ty 'uint)
   (u32bin? g ta tb)
    (!(ptr? ta || ptr? tb) &&
     !(member? ta '(long ulong llong ullong) ||
       member? tb '(long ulong llong ullong)) &&
     (u32? g ta || u32? g tb))
   (rbin g ta tb) (u32bin? g ta tb ? 'uint (ubin !(t32? g) ta tb))
   ; ⚠ and ASK THE LITERAL what C would call it. a bare number's value tuple types 'long on the
   ; 64-bit targets (the register is 64-bit and every other consumer wants that), but C says an
   ; unsuffixed literal that fits is an `int` -- and u32bin? DISQUALIFIES on a long operand, so
   ; `uint x * <lit>` would skip its wrap and carry bits past 31 that C already threw away.
   ; a literal too WIDE for int really is long, and then the result is 64-bit and must not wrap
   ; -- hence the range test rather than a blanket 'int. --
   (numty n) (-2147483648 <= n <= 2147483647 ? 'int 'long)
   ; ..and read the literal through its written face: -1 is (un neg (num 1)), ~0 is
   ; (un bnot (num 0)). litv folds the face to `(v) | (); numty9 types a compare or
   ; divide OPERAND the way C reads the source, falling back to the value tuple's type.
   (litv e) (e @ ('num n) [n]
                 ('un op x && two? x)
                  (: r (litv x)
                     (? (atom? r) ()
                        (op = 'neg) [-<r]
                        (op = 'bnot) [(-<r - 1)])))
   (numty9 e ty) (: r (litv e) (two? r ? (numty <r) ty))
   ; ..and a literal WEARING a cast coat is still a literal: ('cast ty ('num n)) with an
   ; integer ty (1UL, and the sizeof fold) rides the immediate lanes as its masked value
   ; PLUS the coat's type. narrow coats mask to their width (signed ones sign-extend --
   ; cfold's law); long/ulong pass the 64-bit pattern whole, so t32 refuses them (long is
   ; the word there, llong the pair). a face OVER a coat (-1UL) stays unfolded: the wrap
   ; runs at the coat's width and the register lane already types it right. knum is the
   ; gate's one read: bare or coated -> (v ty) | ().
   (cnum g e)
    (e @ ('cast ty ('num n && (charm? n || sun? n)))
     (ty @ 'bool [(n != 0) ty]
           'uchar [(n & 255) ty]
           'ushort [(n & 65535) ty]
           'uint [(n & 4294967295) ty]
           ('char || 'short || 'int)
            (: w (ty @ 'char 8 'short 16 32)
               r (n & ((1 << w) - 1))
               s (r < (1 << (w - 1)) ? r (r - (1 << w)))
             [s ty])
           ('long || 'ulong) (t32? g ? () [n ty])))
   (knum g e) (e @ ('num n) [n (numty n)] (cnum g e))
   ; a compare or divide takes the COMMON converted type's signedness (C11 6.3.1.8):
   ; the signedness of the typing door's puac (floor.l) -- ONE spelling of the law,
   ; parse and gen both consuming it, width included: on t32 a long cannot hold every
   ; uint, so long-vs-uint compares UNSIGNED there and signed on the 64-bit targets.
   ; a pointer compares as the unsigned word. znrw stays beside it: int meets uint at
   ; uint and the 64-bit targets also owe the 32-bit narrow -- the signed side rides
   ; sign-extended past bit 31 and C compares at uint width.
   (cmpu g ta tb)
    (ptr? ta || ptr? tb || tusg? (puac !(t32? g) ta tb))
   (znrw g ta tb ra rb)
    (u32bin? g ta tb ? ((usg? ta ? () [['zx4 ra]]) + (usg? tb ? () [['zx4 rb]])))
   ; a case constant CONVERTS to the switch control's type (C11 6.8.4.2p5), spelled
   ; as the register's extended pattern: a 32-bit type (or the t32 word) wraps at
   ; 2^32, signed re-signs. no & here: the 64-bit mask is a BIG, and love's bitwise
   ; ops answer nothing on one -- % then a sign fix instead.
   (cfit g ty v)
    (: m (t32? g || !(member? ty '(long ulong llong ullong)) ? 4294967296 18446744073709551616)
       r (v % m)
       u (r < 0 ? (r + m) r)
       (? (usg? ty || ptr? ty) u
          (u < (m // 2)) u
          (u - m)))
   (scc op unsg) (!unsg ? op (op @ 'lt 'below 'gt 'above 'le 'be 'ge 'ae op))
   (rflip op) (op @ 'lt 'gt 'gt 'lt 'le 'ge 'ge 'le op)      ; MIRRORED (n < x reads x > n): constant-on-the-left
   (irop op) (op @ '+ 'add '- 'sub '* 'imul '& 'and '| 'or '^ 'xor 'no) ; C op -> IR mnemonic; not one of the six -> 'no
   ; the NaN-honest ==/!= settle after ucomisd: UNORDERED (PF=1 on x64) must
   ; read false for every relation and true only for != -- == ANDs the ordered
   ; bit in, != ORs the parity bit (the truth lands in r0, r1 the scratch)
   fsete '((set eq r0) (set np r1) (and r0 r0 r1))
   fsetn '((set ne r0) (set p r1) (or r0 r0 r1))
   (negrel op) (op @ 'eq 'ne 'ne 'eq 'lt 'ge 'ge 'lt 'le 'gt 'le)   ; NEGATED: branch-if-false
   (fits32? v) (-2147483648 <= v <= 2147483647)    ; holo's ALU/cmp/imul immediates ride le32
   (tsz g ty) (tysz (wsize g) (g 'stag) ty)
   ; a member's (offset ty) off a struct type, | 'bad
   (fmem g ty name)
    (? !(stru? ty) 'bad
       (: e (peep (g 'stag) <>ty 0)
          (? !(two? e) 'bad
             (: f (assoc name <e)
                (? (two? f) [<>f <>>f] 'bad)))))
   (lg2 n) (: (go k v) (v <= 1 ? k (go (k + 1) (v >> 1))) (go 0 n))   ; a true log2 -- a pointer diff over a >8-byte struct needs sar 4+, not a capped 3
   ; pointer-scale by an immediate: x64 imul takes it straight; arm mul has no immediate form, so
   ; the size rides r3 first (free mid-expression: the shuttle holds r0/r1, args load at the pops)
   (imby g rX sz) (? (arm? g) [['li 'r3 sz] ['imul rX rX 'r3]] [['imul rX rX sz]])   ; arm mul (a64/t32/v6-M) has no imm form
   (homable? pty) (ptr? pty || member? pty '(long ulong))       ; an 8-byte gp scalar: what a register home can carry
   ; a home carries int/uint too, CANONICALLY: the register always holds the extended value
   ; (every def re-narrows through cvt; a param's entry cvt canonicalizes the arrival -- a
   ; foreign caller's high half is undefined), so reads are zero forms and 32-bit wrap holds.
   ; ⚠ an int PARAM homes only by RIDING its arrival (no pool register, no wrap -- one entry
   ; cvt): a pool home for a cold int param starves the d128 pair park and the hot locals
   ; (mag_mul measured +3% corpus cycles), so a non-rideable int param keeps its slot.
   (lhomable? ty) (homable? ty || member? ty '(int uint))
   ; the homable-param fold, spelled once for its five readers (pgo/homes?/nhomable/uhomes/
   ; rideset): a float/sse param rides its own lane and spends no gp seat, the walk stops folding
   ; at the home limit, and hx folds each homable seat -- (hx acc gpi param) -> acc.
   ; the gate admits int/uint for RIDESET's sake; every other reader filters back to homable?
   (homefold g ps gpi acc hx)
    (? !(two? ps) acc
       (: pty <><ps
          (? (flo? pty || sse2? g pty || id? (aone g pty) 'sse) (homefold g >ps gpi acc hx)
             (nhome g <= gpi) acc
             (lhomable? pty) (homefold g >ps (gpi + 1) (hx acc gpi <ps) hx)
             (homefold g >ps (gpi + 1) acc hx))))
   ; -- the immediate lane: the left value already in r0, a compile-time constant on the right ->
   ; ONE imm-form op (holo's add/sub/imul/and/or/xor/cmp take a signed-32 immediate), no
   ; operand-stack shuttle. a pointer +/- scales the constant at compile time; an UNSIGNED / or %
   ; by a power of two strength-reduces to shr/and (signed stays a real div -- C rounds toward
   ; zero, sar toward -inf). answers (ty forms) | 'no (the caller falls back to the shuttle). --
   (pow2? n) (0 < n && 0 = n & (n - 1))
   (immop g op av aty n nty wnt)                    ; av the LEFT value tuple; nty the constant's C type; wnt the advisory destination
    (: rA (vreg av)
       (? (llt? aty || d128? aty) 'no               ; a pair never rides the immediate lane
          (member? op '(eq ne lt le gt ge))
           ; C converts a negative literal to the uint side (0xffffffff will not sit
           ; in a signed immediate): the shuttle materializes and narrows
           (? (u32? g aty && n < 0) 'no
              !(immok g 'cmp n) 'no
              (: rd (? wnt wnt (rdst g rA))
                 fs (vfs av + [['cmp rA n] ['set (scc op (cmpu g aty nty)) rd]])
                 _ (? (rpeq? g rd rA) () (rfree g rA))
                 (mkv 'long rd fs)))
          (member? op '(/ %))
           ; ⚠ what licenses shr/and here is the COMMON TYPE being unsigned, which is not
           ; the same as either operand being unsigned: an unsigned CONSTANT does NOT make
           ; the common type unsigned when the signed side is wider (C 6.3.1.8, and cmpu
           ; is the same read the general divide below takes). `(long)-8 / 2u` is -4, and
           ; reducing it to a LOGICAL shift answered 9223372036854775804 -- a whole
           ; quadrant off, silently, for the one divisor shape that looks safest.
           (? !((cmpu g aty nty) && pow2? n) 'no
              (op = '/) (? (n = 1) (mkv (rbin g aty nty) rA (vfs av))
                           (? (wr? g rA)
                              (mkv (rbin g aty nty) rA (vfs av + [['shr rA (lg2 n)]]))
                              (: rd (? wnt wnt (rdst g rA))   ; a borrowed home shifts a COPY (no 3-address shr)
                                 (mkv (rbin g aty nty) rd (vfs av + [['mov rd rA] ['shr rd (lg2 n)]])))))
              (n = 1) (mkv (rbin g aty nty) 'r0 (vdrop g av + '((li r0 0))))
              !(immok g 'log (n - 1)) 'no                ; t32 has no simple AND-imm: materialize (the register path)
              (: rd (? wnt wnt (rdst g rA))
                 fs (vfs av + [['and rd rA (n - 1)]])
                 _ (? (rpeq? g rd rA) () (rfree g rA))
                 (mkv (rbin g aty nty) rd fs)))
          (: sn (? (ptr? aty && member? op '(+ -)) (* n (tsz g <>aty)) n)
             iop (irop op)
             icl (? (member? op '(+ -)) 'addsub (op = '*) 'mul 'log)
             ; a bare constant reads as `int` (numty capped it: immok bounds every lane to
             ; the signed-32 range anyway); a coated one carries its own type, so
             ; `uint op <lit>` wraps and `uint op (ulong)k` widens, as C says.
             u32 (member? op '(+ - *) && u32bin? g aty nty)
             (? (id? iop 'no) 'no
                !(immok g icl sn) 'no
                (: rt (? (ptr? aty) aty (rbin g aty nty))
                   (? ((sn = 0) && member? op '(+ -)) (mkv rt rA (vfs av))
                      (: rd (? wnt wnt (rdst g rA))
                         fs (vfs av + [[iop rd rA sn]] + (? u32 [['zx4 rd]] ()))
                         _ (? (rpeq? g rd rA) () (rfree g rA))
                         (mkv rt rd fs))))))))
   (ldop ty) (ty @ 'char 'ld1 'short 'ld2 'int 'ld4
                   ('uchar || 'bool) 'ldu1 'ushort 'ldu2 'uint 'ldu4 'ld)
   (stop ty) (ty @ ('char || 'uchar || 'bool) 'st1 ('short || 'ushort) 'st2
                   ('int || 'uint) 'st4 'st)
   ; ⚠ bool's cvt is a CONVERSION, not a re-narrow: any nonzero pattern becomes 1
   ; (C11 6.3.1.2) -- every write site already runs cvt before its store, so a bool
   ; object holds 0/1 by construction and loads ride a plain ldu1.
   (cvt ty r) (ty @ 'char [['sx1 r]] 'short [['sx2 r]] 'int [['sx4 r]]
                    'uchar [['zx1 r]] 'ushort [['zx2 r]] 'uint [['zx4 r]]
                    'bool [['cmp r 0] ['set 'ne r]])
   ; -- the double lane: a 'double value lives in f0, not r0. its type is the flag; fbits is the
   ; compile-time IEEE-754 encoder; asflo/asint move a cgexpr result between the lanes; compares
   ; ride ucomisd's UNSIGNED flags, NaN-honest. a float and a double SHARE the register lane -- a
   ; value in an xmm is ALWAYS a 64-bit double, only MEMORY differs: a `float` slot is 4 bytes, so
   ; a load widens (ldss + cvtss2sd) and a store narrows (cvtsd2ss + stss). fldf/fstf pick the lane
   ; by the DECLARED type; every loaded float types 'double. --
   (flo? ty) (ty = 'double || ty = 'float)
   ; ⚠ a SUBNORMAL carries no implicit leading 1 and sits at one fixed exponent, so the normal
   ; lane's (e + bias) runs NEGATIVE for it and the pattern lands on nonsense. ⚠ and a `float`
   ; constant must ROUND to its 24 bits -- truncating leaves 0.1f a ulp light (the double lane
   ; hides it, being exact there: its fraction IS an integer).
   (mul2 x k) (k <= 1000 ? (x * k 2.0) (mul2 (x * 1000 2.0) (k - 1000)))   ; x * 2^k, split so 2^1074 never needs a double
   (fnear y)                                        ; y >= 0 -> the nearest integer, ties to EVEN
    (: f (int y) r (y - (0.0 + f))
       (? (r > 0.5) (f + 1) (r < 0.5) f (f & 1) (f + 1) f))
   (fenc ax bias p)                                 ; positive finite -> biased-exponent * 2^p + mantissa fraction
    (: mant (p 2)
       emin (1 - bias)                              ; the smallest normal's exponent
       (? (ax < emin 2.0) (fnear (mul2 ax (p - emin)))         ; subnormal: the biased exponent is 0
          (: eu ((: (go e) (? ((e + 1) 2.0 <= ax) (go (e + 1)) e)) (int (log ax / log 2.0)))
             e  ((: (go e) (? (ax < e 2.0) (go (e - 1)) e)) eu)
             m  (fnear ((ax / e 2.0 - 1.0) * (0.0 + mant)))
             ; a fraction that rounds up out of its binade carries into the exponent
             ; (0x1.ff..f -> 0x2.0), and at the top that IS the infinity pattern C wants
             (m < mant ? ((e + bias) * mant + m) ((e + 1 + bias) * mant)))))
   (fbits x)                                        ; double -> its IEEE-754 bits as the SIGNED i64 pattern
    (? (x = 0.0) 0                                  ; (the sign bit would make a BIG, and les/imnum walk i64 patterns)
       !(x = x) 9221120237041090560                 ; nan (the quiet pattern) -- fenc's climb never ends on a non-finite
       (x + x = x) (x < 0.0 ? -4503599627370496 9218868437227405312)   ; +-inf (1e999 reads as one)
       (: m (fenc (x < 0.0 ? (0.0 - x) x) 1023 52)
          (x < 0.0 ? (m - 9223372036854775808) m)))
   (fbits32 x)                                      ; double -> its IEEE-754 SINGLE (u32) bits (a `float` image)
    (? (x = 0.0) 0
       !(x = x) 2143289344
       (x + x = x) (x < 0.0 ? 4286578688 2139095040)
       (: ax (x < 0.0 ? (0.0 - x) x)
          ; under 2^128 fenc's own carry covers the top edge; at or above it the exponent
          ; would wrap, so name the infinity here
          m (ax < 128 2.0 ? (fenc ax 127 23) 2139095040)
          ((x < 0.0) * 2147483648 + m)))
   (fldf ty freg base off)                          ; load a float/double from [base+off] into freg (widen if float)
    (ty = 'float ? [['ldss freg base off] ['cvtss2sd freg freg]]
                   [['ldsd freg base off]])
   ; the narrow scratch: x64 f2 (xmm2, caller-saved, never live here); arm64 f16 -- ⚠ v2 IS the
   ; third fp ARGUMENT and v8..v15 carry a callee-saved low half, so v16+ are the only pure temps.
   ; t32 (fpv5-d16) has no register past d15, so d15/f15 is RESERVED as the scratch (the backend's
   ; converters own its S30 half) -- moon never allocates it.
   (fsc g) (? (a8? g) 'f16 (t32? g) 'f15 'f2)
   (fstf g ty base off val)                         ; store val (a double in an xmm) to [base+off] as ty (narrow if float)
    (? (ty = 'float) (: f9 (fsc g) [['cvtsd2ss f9 val] ['stss base off f9]])   ; scratch narrows (val untouched)
       [['stsd base off val]])
   ; result -> f0 as a double. t32: an unsigned word rides the U-converter (no 64-bit widen
   ; dodge on a 32-bit target); a pair source is the deferred cvtllsd (backend scares LOUD).
   (asflo g r) (? (flo? (vty r)) (vfs r)
                  (gcplx? (vty r))                  ; complex -> real reads re (C's implicit conversion)
                   (tor0 g r + (? (id? (cxb (vty r)) 'float)
                                    '((ldss f0 r0 0) (cvtss2sd f0 f0))
                                    '((ldsd f0 r0 0))))
                  (t32? g && llt? (vty r)) (tor0 g r + cvtlld g (usg? (vty r)))
                  (t32? g && usg? (vty r)) (tor0 g r + '((cvtui2sd f0 r0)))
                  (id? (vty r) 'ulong) (tor0 g r + ul2d)
                  (tor0 g r + '((cvtsi2sd f0 r0))))
   (asint g r) (? (flo? (vty r)) (vfs r + '((cvttsd2si r0 f0))) (tor0 g r))        ; result -> r0 as an int
   ; ⚠ the MIRROR of ul2d: cvttsd2si is a SIGNED truncation, so a double at or above 2^63
   ; saturates to 0x8000000000000000, and the 64-bit targets have exactly ONE truncating
   ; instruction. so BIAS around 2^63 -- under it convert straight, at or above it subtract 2^63
   ; first, convert the remainder (now signed-representable), and put the bit back (the remainder
   ; is under 2^63, so bit 63 is clear and the xor IS the add). the subtract is EXACT: a d >= 2^63
   ; has an ulp of at least 2^11 and d - 2^63 is a multiple of it, so no rounding enters.
   ; a negative, an out-of-range and a NaN fall to the plain lane and land on the signed
   ; instruction's own indefinite -- C leaves every one of them undefined. --
   (dtoul g)
    (: lb (nlab g ".du") ld (nlab g ".dv")
       [['li 'r1 0x43e0000000000000] '(movqxr f1 r1)         ; 2^63
          '(ucomisd f0 f1) ['br 'ae lb]
          '(cvttsd2si r0 f0) ['jmp ld]                        ; d < 2^63: straight
          ['label lb]
          '(subsd f0 f1)                                       ; d - 2^63, exact
          '(cvttsd2si r0 f0)
          ['li 'r1 0x8000000000000000] '(xor r0 r0 r1)         ; put 2^63 back
          ['label ld]])
   ; asint honouring an unsigned 64-bit DESTINATION: every caller that knows the type it converts
   ; INTO comes through here (a cast, a store/init, a homed assign, an outgoing arg, a return).
   ; the callers that know no destination type are integer contexts where the signed truncation is
   ; what C asks for, and the narrower unsigned types fit a signed 64-bit word anyway. --
   (asintt g r ty)
    (? (!(t32? g) && flo? (vty r) && id? ty 'ulong) (vfs r + dtoul g)
       (asint g r))
   ; -- the environments: locals shadow globals --
   (vent g env name)                                ; -> ('regv rN ty) | ('regw (lo hi) ty) | ('loc off ty) | ('glo lbl ty) | 'bad
    (: e (assoc name env)
       (? (two? e)
           (: h <>e y <>>e                             ; a homed param's "off" slot is a (reg rN) list;
              (h @ ('reg x) ['regv x y]     ; a block-scope static's is (glob lbl)
                   ('regw x) ['regw x y]    ; a d128 pair home: (lo hi)
                   ('glob x) ['glo x y]
                   ['loc h y]))
          (peep (g 'glob) name 0 @ (a . b . _) ['glo a b] 'bad)))
   ; a targetable LEAF re-emitted straight into register rX -> forms | 'no. leaves are PURE (no
   ; strs pin, no argpos), so discarding the r0 emission and re-aiming costs nothing. a homed
   ; param never lands here (its tuple already rides its home register, zero forms).
   (cgleaf g env e rX)
    (: kc (knum g e)
       (? (two? kc) [['li rX <kc]]
          !(<e = 'var) 'no
          (: en (vent g env <>e)
             (? (bad? en) 'no
                !(<en = 'loc) 'no
                (: ty <>>en
                   (stru? ty || arr? ty || flo? ty || d128? ty ? 'no
                      [[(ldop ty) rX 'r4 <>en]]))))))
   ; PARK the left operand out of r0 so the right may compute: a leaf re-aims at a fresh pool
   ; register (no protocol mov at all), anything else takes one mov. answers the (possibly
   ; re-homed) tuple | 'sh (pool dry -> the caller keeps the memory shuttle).
   (vpark g env e v)
    (? !(id? (vreg v) 'r0) v
       (: rX (ralloc g)
          (? !rX 'sh
             (: lf (cgleaf g env e rX)
                (? !(id? lf 'no) (mkv (vty v) rX lf)
                   (mkv (vty v) rX (vfs v + [['mov rX 'r0]])))))))
   ; a truth value in r0 that `cmp r0 0` can test: a double folds to d != 0.0
   (ctest g env e)
    (: r (cgexpr g env e ())
       (? (bad? r) 'bad
          (flo? (vty r))                             ; d != 0.0, NaN-honest: unordered is NONZERO (true) -- fsetn
           (mkv 'long 'r0 (vfs r + ('((li r1 0) (movqxr f1 r1) (ucomisd f0 f1)) + fsetn)))
          (d128? (vty r))                            ; wide truth: fold the pair into r0
           (mkv 'long 'r0 (vfs r + '((or r0 r0 r2))))
          r))
   ; -- the predicate splice: a call to an inline-table fn whose whole body is one `return E` with
   ; E already a BIT by shape (!/&&/||/relational, so the return-type narrowing keeps its truth)
   ; branches THROUGH the body -- args land in fresh temps (the out-of-line call's exact
   ; semantics), then E cbranches straight to lbl: no join label, no 0/1 settle, no retest.
   ; the fesc/strs decline protocol is exactly cginl's. -> forms | 'no (the fallback stands) --
   (retex f)                                     ; init-free scalar decls* + one `return E` -> (((name ty)..) E) | ()
    (: (dok ps acc9)
        (? !(two? ps) acc9
           (: p9 <ps
              (? (two? <>>p9 || arr? <>p9 || stru? <>p9) 'bad
                 (dok >ps ([<p9 <>p9] >< acc9)))))
       (go items dacc)
        (? !(two? items) ()
           (: it <items
              (? (<it = 'decl)
                  (: ds (dok <>it dacc) (? (bad? ds) () (go >items ds)))
                 !(<it = 'ret) ()
                 !(two? <>it) ()
                 (two? >items) ()
                 [dacc <>it])))
       (go <><>>>f ()))
   (abit? e9) (? !(two? e9) ()
                 (<e9 = 'un && <>e9 = 'not) 1
                 (member? <e9 '(land lor)) 1
                 (<e9 = 'bin && member? <>e9 '(eq ne lt le gt ge)))
   (cbinl g env e cc lbl)
    (: head <>e args <>>e
       il0 (peep g 'inlining ())                  ; cbinl NESTS to depth 3 (it emits no
       d0 (? !il0 0 il0)                          ; labels, so cginl's collision reason
       inlf (? (2 < d0) ()                        ; is moot -- only recursion needs a cap)
               !(<head = 'var) ()
               !(bad? (vent g env <>head)) ()     ; a local/global shadows the name
               (: c9 (g 'inl <>head)
                  (? (atom? c9) ()
                     (#args = #<>>c9) c9)))
       rx (two? inlf ? (retex inlf))
       e9 (two? rx ? <>rx)
       (? !(abit? e9) 'no
          (: fe0 (g 'fesc)
             strs0 (g 'strs)
             rp0 (g 'rpin)
             ; the register-bind license: a call-free splice (body and args) reads its scalar
             ; args from REGISTERS. an arg that is a word-typed pinned local ALIASES its pin
             ; (call-by-value holds: wrs? bars a body that assigns the param, and a later drop
             ; leaves the alias holding the passed value); anything else parks canonically in a
             ; fresh pool reg. rpin holds every bind reg against wr?/rfree/psreset for the splice's
             ; span. the slot bind stands when
             ; the type narrows to the alias, a call rides anywhere, or the pool is SHALLOW --
             ; a park may only take a register when two stay free: parks that drained the pool
             ; starved the hints and pins around them and the shuttle spushed (measured: corpus
             ; L1 loads +0.4% -- the displacement is unpriced by any other gate).
             (anyc l) (? !(two? l) () (? (callish? <l) 1 (anyc >l)))
             nocall (!(callish? e9) && !(anyc args))
             _ (pin g 'inlining (d0 + 1))
             (cbind ps as benv acc rbs)            ; cginl's bind, the same slots
              (? !(two? ps) [benv acc rbs]
                 (: pty <><ps
                    al (? (nocall && !(flo? pty) && 8 = tsz g pty &&
                           two? <as && <<as = 'var &&
                           !(wrs? <<ps <><>>>inlf))
                          (: v9 (vent g env <><as)         ; a homed/riding local (regv): a
                             (? (two? v9 && (<v9 = 'regv) && !(flo? <>>v9))   ; read-only reg holding the
                                <>v9)))                 ; canonical current value
                    al (al && alsafe? g al ? al)   ; staging must never write it
                    (? al                          ; the alias bind: zero forms
                        (: _ (pin g 'rpin (rp g al >< g 'rpin))
                           (cbind >ps >as ([<<ps ['reg al] pty] >< benv) acc rbs))
                       (: r (cgexpr g env <as ())
                          (? (bad? r) 'bad
                             (pair? g pty)         ; a pair param: an 8-byte slot, both words
                              (: off (ntmp g 'llong)
                                 (cbind >ps >as
                                        ([<<ps off pty] >< benv)
                                        (acc + asll g r + pst 'r4 off) rbs))
                             (: rb (nocall && !(flo? pty) &&
                                       two? >>(g 'pool) ?              ; park only into a DEEP pool
                                       (ralloc g))
                                (? rb              ; the park bind: the value rides a pool reg
                                    (: _ (pin g 'rpin (rp g rb >< g 'rpin))
                                       (cbind >ps >as ([<<ps ['reg rb] pty] >< benv)
                                              (acc + asintt g r pty
                                                   + ([['mov rb 'r0]] + cvt pty rb))
                                              (rb >< rbs)))
                                   (: off (ntmp g 'long)
                                      (cbind >ps >as
                                             ([<<ps off pty] >< benv)
                                             (acc + asintt g r pty
                                                  + (pty = 'bool ? (cvt pty 'r0))   ; the entry conversion the splice skips
                                                  + [['st 'r4 off 'r0]])
                                             rbs)))))))))
             (dtmp ds benv9)                       ; the body's own decls: fresh temps over the params
              (? !(two? ds) benv9
                 (: d9 <ds (dtmp >ds ([<d9 (ntmp g <>d9) <>d9] >< benv9))))
             br (cbind <>>inlf args () () ())
             ib0 (peep g 'inlbody 0)
             _ (pin g 'inlbody (ib0 + 1))          ; the body blinds the alias bind (name conflation)
             bf (? (bad? br) 'bad (cbranch g (dtmp <rx <br) e9 cc lbl))
             _ (pins g 'inlbody ib0 'inlining il0 'rpin rp0) ; the bind pins die with the splice
             (unrb (a >< u)) (rfree g a, unrb u)
             _ (two? br ? (unrb (nthr br 2)))
             (? (bad? bf)
                 (pin g 'fesc fe0, pin g 'strs strs0, 'no)
                (!fe0 && g 'fesc)          ; the body took a frame address: keep the caller lean
                 (pin g 'fesc (), pin g 'strs strs0, 'no)
                (<>br + bf)))))
   ; -- (v & k) as a bare condition: TEST is the flags-only AND, so the value's register SURVIVES
   ; the check -- no and-clobber, a mapped local stays mapped, no park/reload. eq/ne read ZF only,
   ; so truth through the mask is exact at every width. both backends carry test-imm (x64 F7 /0
   ; id, arm64 ands xzr #bitmask), and the 'log gate is exactly test's immediate reach on each. --
   (tbr g env v k cc lbl)
    (: a (cgexpr g env v ())
       (? (bad? a || flo? (vty a)) 'bad
          (: fs (vfs a + [['test (vreg a) k] ['br cc lbl]])
             _ (rfree g (vreg a))
             fs)))
   ; -- branch on a condition WITHOUT materializing its truth value: forms that jump to lbl when
   ; the condition is FALSE (cc 'eq) or TRUE (cc 'ne), the same sense as the fallback
   ; `(cmp r0 0) (br cc lbl)` over a ctest value. a comparison fuses to cmp + br<relation>, !e
   ; flips the sense, && and || short-circuit straight into the branch structure, a mask test
   ; (e & k) branches off TEST (tbr), a predicate CALL splices through cbinl, and a float compare
   ; branches off ucomisd/fcmp NaN-honest (fcb). forms | 'bad. --
   (cbranch g env e cc lbl)
    (e @
       ('un 'not x)
        (cbranch g env x (cc @ 'eq 'ne 'eq) lbl)
       ('land a b)
        (cc @ 'eq
           (: f1 (cbranch g env a 'eq lbl)
              f2 (cbranch g env b 'eq lbl)
            (bad? f1 || bad? f2 ? 'bad (f1 + f2)))
           (: lf (nlab g ".f")
              f1 (cbranch g env a 'eq lf)
              f2 (cbranch g env b 'ne lbl)
            (bad? f1 || bad? f2 ? 'bad (f1 + f2 + [['label lf]]))))
       ('lor a b)
        (cc @ 'ne
           (: f1 (cbranch g env a 'ne lbl)
              f2 (cbranch g env b 'ne lbl)
            (bad? f1 || bad? f2 ? 'bad (f1 + f2)))
           (: lt (nlab g ".t")
              f1 (cbranch g env a 'ne lt)
              f2 (cbranch g env b 'eq lbl)
            (bad? f1 || bad? f2 ? 'bad (f1 + f2 + [['label lt]]))))
       ('bin op lhs rhs && member? op '(eq ne lt le gt ge))
        ; ⚠ LAZY left-then-right: the left PARKS (allocates) before the right evaluates --
        ; allocation order must match execution order, or the right's freed inner temps alias
        ; the parked left. the float lane FUSES: discard the int-flavored emission,
        ; re-evaluate both sides through the double shuttle, then branch off the compare.
        ; UNORDERED (a NaN side) is false for every relation and true only for !=.
        ; x64: above/ae read CF=0, which unordered sets, so < and <= compare SWAPPED, and
        ; ==/!= carry the parity bit in the BRANCH STRUCTURE. arm64: fcmp's gt/ge/eq/ne are
        ; unordered-false natively, so every relation is ONE branch and negrel spells false.
        (: (fcb z9)
            (: a (cgexpr g env lhs ())
               (? (bad? a) 'bad
                  (: af (? (t32? g)                      ; the gp shuttle moves the PAIR on t32
                           (asflo g a + '((movqrx r0 f0)) + spush2)
                           (asflo g a))                  ; the word hold joins per-arm below
                     b (cgexpr g env rhs ())
                     (? (bad? b) 'bad
                        (: mid (? (t32? g)
                                  (af + asflo g b + (spop2 'r2 'r3 + '((movqxr f1 r2))))
                                  ; the left's bits HOLD in a gp reg -- movqrx aims straight at it
                                  (: bf (asflo g b)
                                     s (? (r1safe? bf) 'r1 (spare g bf))
                                     (? s (af + [['movqrx s 'f0]] +
                                           (bf + [['movqxr 'f1 s]]))
                                        (af + ('((movqrx r0 f0)) + spush 'r0) +
                                         (bf + spop 'r1 + '((movqxr f1 r1)))))))
                           swp (member? op '(lt le))
                           cf [['ucomisd (? swp 'f0 'f1) (? swp 'f1 'f0)]]
                           ecc (? (arm? g)               ; VCMP/FCMP flags: gt/ge unordered-false native
                                  (: t9 (? (member? op '(lt gt)) 'gt (member? op '(le ge)) 'ge op)
                                     (cc @ 'ne t9 (negrel t9)))
                                  (member? op '(lt gt)) (cc @ 'ne 'above 'be)
                                  (member? op '(le ge)) (cc @ 'ne 'ae 'below))
                           (? ecc (mid + cf + [['br ecc lbl]])
                              (op = (cc @ 'ne 'ne 'eq))
                               (mid + cf + [['br 'p lbl] ['br 'ne lbl]])
                              (: sk (nlab g ".u")
                                 (mid + cf + [['br 'p sk] ['br 'eq lbl] ['label sk]]))))))))
           (bcc o u) (scc (cc @ 'ne o (negrel o)) u)
           ; a pair side (t32 llong) branches off subs/sbcs via pbrf; a constant other side
           ; materializes as a sign-widened pair in r2:r3, a 32-bit other side widens the same way.
           (pnum v) [['li 'r2 (v & 4294967295)] ['li 'r3 (? (v < 0) 4294967295 0)]]
           ; a wide relation settles 0/1 through c128 (the LEFT rides the cell), then branches on it
           (wbr o2 af u2) (af + ([['c128 (scc o2 u2) 'sp]] + (wdrop + ['(cmp r0 0) ['br cc lbl]])))
           (wnum v) [['li 'r0 v] ['li 'r2 (? (v < 0) -1 0)]]
           kr (knum g rhs)
           kl (knum g lhs)
           (? (two? kr && immok g 'cmp <kr)
               (: a (cgexpr g env lhs ())
                  (? (bad? a) 'bad
                     (flo? (vty a)) (: _ (rfree g (vreg a)) (fcb ()))
                     (pair? g (vty a))
                      (vfs a + pnum <kr + pbrf g op (usg? (vty a)) 'r0 'r1 'r2 'r3 cc lbl)
                     (d128? (vty a))
                      (? !(w128? g) (no128 g)
                         (wbr op (as128 g a + (wpush + wnum <kr)) (id? (vty a) 'u128)))
                     (: n <kr
                        cc9 (bcc op (cmpu g (vty a) <>kr))
                        ; C converts a negative literal to the uint side (the masked
                        ; pattern will not sit in a signed immediate): materialize it
                        fs (? (u32? g (vty a) && (n < 0))
                              (: sc (? (id? (vreg a) 'r1) 'r0 'r1)
                                 (vfs a + [['li sc (n & 4294967295)] ['cmp (vreg a) sc] ['br cc9 lbl]]))
                              (vfs a + [['cmp (vreg a) n] ['br cc9 lbl]]))
                        _ (rfree g (vreg a))
                        fs)))
              (two? kl && immok g 'cmp <kl)
               (: b (cgexpr g env rhs ())
                  (? (bad? b) 'bad
                     (flo? (vty b)) (: _ (rfree g (vreg b)) (fcb ()))
                     (pair? g (vty b))
                      (vfs b + pnum <kl + pbrf g (rflip op) (usg? (vty b)) 'r0 'r1 'r2 'r3 cc lbl)
                     (d128? (vty b))
                      (? !(w128? g) (no128 g)
                         (wbr (rflip op) (as128 g b + (wpush + wnum <kl)) (id? (vty b) 'u128)))
                     (: n <kl
                        cc9 (bcc (rflip op) (cmpu g <>kl (vty b)))
                        fs (? (u32? g (vty b) && (n < 0))
                              (: sc (? (id? (vreg b) 'r1) 'r0 'r1)
                                 (vfs b + [['li sc (n & 4294967295)] ['cmp (vreg b) sc] ['br cc9 lbl]]))
                              (vfs b + [['cmp (vreg b) n] ['br cc9 lbl]]))
                        _ (rfree g (vreg b))
                        fs)))
              ; the destination die: when no call bars a register park, the left AIMS at its
              ; park before it evaluates -- a load/li lands there directly and vpark sees a
              ; delivered value (vreg off r0: no bridge mov). unhonored (a mapped local, a
              ; float, a wide) hands the register back and the post-hoc park stands.
              (: pk (? (callish? lhs || callish? rhs) () (ralloc g))
                 a (cgexpr g env lhs pk)
                 _ (? (pk && !(!(bad? a) && rpeq? g (vreg a) pk)) (rfree g pk) ())
                 (? (bad? a) 'bad
                    (flo? (vty a)) (: _ (rfree g (vreg a)) (fcb ()))
                    (: ah (? (callish? rhs || llt? (vty a) || d128? (vty a)) 'sh (vpark g env lhs a))           ; a call on the right / a pair left: memory only
                       (? (id? ah 'sh)
                           (: pA (pair? g (vty a))
                              wA (d128? (vty a))
                              af0 (? (wA || pA) () (tor0 g a))
                              af (? wA (as128 g a + wpush)
                                    pA (asll g a + spush2)
                                    (af0 + spush 'r0))
                              b (cgexpr g env rhs ())
                              (? (bad? b) 'bad
                                 (flo? (vty b)) (? wA 'bad (: _ (rfree g (vreg b)) (fcb ())))
                                 (wA || d128? (vty b))                 ; a wide side: c128 over the cell
                                  (? !(w128? g) (no128 g)
                                     (: u2 (id? (? (wA && d128? (vty b)) (wbin (vty a) (vty b))
                                                   wA (vty a) (vty b)) 'u128)
                                        ; a WORD left widens its cell hi by its own signedness
                                        fixa (? wA ()
                                                (usg? (vty a) || ptr? (vty a)) '((si sp 8 0))
                                                '((ld r1 sp 0) (sar r1 63) (st sp 8 r1)))
                                        (af + as128 g b + fixa +
                                         ([['c128 (scc op u2) 'sp]] + (wdrop + ['(cmp r0 0) ['br cc lbl]])))))
                                 (t32? g && (pA || llt? (vty b)))
                                  (: u2 (cmpu g (vty a) (vty b))
                                     (af + asll g b +
                                      (? pA (spop2 'r2 'r3 + pbrf g op u2 'r2 'r3 'r0 'r1 cc lbl)
                                         (spop 'r2
                                          + (? (usg? (vty a)) '((li r3 0)) '((mov r3 r2) (sar r3 31)))
                                          + pbrf g op u2 'r2 'r3 'r0 'r1 cc lbl))))
                                 (: bf (tor0 g b)
                                    ta9 (numty9 lhs (vty a)) tb9 (numty9 rhs (vty b))
                                    brf (znrw g ta9 tb9 'r1 'r0 +
                                         ['(cmp r1 r0) ['br (bcc op (cmpu g ta9 tb9)) lbl]])
                                    (? (!(two? (vfs a)) && !(id? (vreg a) 'r0) && psafe? g bf (vreg a))
                                       (bf + ([['mov 'r1 (vreg a)]] + brf))   ; the left rides: read through it
                                       (r1safe? bf) (af0 + '((mov r1 r0)) + bf + brf)
                                       (: s (spare g bf)
                                          (? s (af0 + ([['mov s 'r0]] + bf) + ([['mov 'r1 s]] + brf))
                                             (af + bf + (spop 'r1 + brf))))))))
                          (: b (cgexpr g env rhs ())
                             (? (bad? b) 'bad
                                (flo? (vty b)) (: _ (rfree g (vreg ah)) _ (rfree g (vreg b)) (fcb ()))
                                (d128? (vty b))                       ; a word park against a wide right: build its cell
                                 (? !(w128? g) (no128 g)
                                    (: fs (vfs ah + vfs b
                                           + ['(sub sp sp 16) ['st 'sp 0 (vreg ah)]]
                                           + (? (usg? (vty ah) || ptr? (vty ah)) '((si sp 8 0))
                                                [['mov 'r1 (vreg ah)] '(sar r1 63) '(st sp 8 r1)])
                                           + ([['c128 (scc op (id? (vty b) 'u128)) 'sp]] + (wdrop + ['(cmp r0 0) ['br cc lbl]])))
                                       _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                       fs))
                                (pair? g (vty b))                     ; a 32-bit park against a pair right: widen the park
                                 (: fs (vfs ah + vfs b
                                        + [['mov 'r2 (vreg ah)]]
                                        + (? (usg? (vty ah)) '((li r3 0)) '((mov r3 r2) (sar r3 31)))
                                        + pbrf g op (cmpu g (vty ah) (vty b)) 'r2 'r3 'r0 'r1 cc lbl)
                                    _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                    fs)
                                (: ta9 (numty9 lhs (vty ah)) tb9 (numty9 rhs (vty b))
                                   cc9 (bcc op (cmpu g ta9 tb9))
                                   ; the zx4 narrow cannot touch a HOME in place: copy
                                   ; through the staging pair (b first -- it may sit in r1)
                                   fs (? (two? (znrw g ta9 tb9 'r1 'r0))
                                         (vfs ah + vfs b
                                          + [['mov 'r0 (vreg b)] ['mov 'r1 (vreg ah)]]
                                          + znrw g ta9 tb9 'r1 'r0
                                          + ['(cmp r1 r0) ['br cc9 lbl]])
                                         (vfs ah + vfs b + [['cmp (vreg ah) (vreg b)] ['br cc9 lbl]]))
                                   _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                   fs)))))))))
       ; ⚠ the mask-test shapes: (v & k) / (k & v). ms is SYNTAX, so its found-test is
       ; structural -- a `?` on a term nets it, and the sign of a tree says nothing.
       (: ms (e @ ('bin >< '& >< ops) ops ())
          lhs (two? ms ? <ms)
          rhs (two? ms ? <>ms)
          kr (two? ms ? (knum g rhs))
          kl (two? ms ? (knum g lhs))
          try (? (<e = 'call) (cbinl g env e cc lbl)
                 !(two? ms) 'no
                 (two? kr && immok g 'log <kr) (tbr g env lhs <kr cc lbl)
                 (two? kl && immok g 'log <kl) (tbr g env rhs <kl cc lbl)
                 'no)
          (? !(id? try 'no) try
             (: c (cgexpr g env e ())
                (? (bad? c) 'bad
                   (flo? (vty c))
                    ; a bare double condition is d != 0.0, NaN-honest -- the parity dance
                    ; rides the branch pair; arm64's fcmp ne/eq are honest whole
                    (: fs (vfs c + (? (t32? g)               ; the zero pattern zeroes the r1:r2 pair
                                      '((li r1 0) (li r2 0) (movqxr f1 r1) (ucomisd f0 f1))
                                      '((li r1 0) (movqxr f1 r1) (ucomisd f0 f1))))
                       _ (rfree g (vreg c))
                       (? (arm? g) (fs + [['br cc lbl]])   ; VCMP/FCMP ne/eq are NaN-honest whole
                          (cc = 'ne) (fs + [['br 'p lbl] ['br 'ne lbl]])
                          (: sk (nlab g ".u")
                           (fs + [['br 'p sk] ['br 'eq lbl] ['label sk]]))))
                   (pair? g (vty c))                     ; pair truth: either word nonzero (v6-M: no r12 dest; r0 is dead past the branch)
                    (vfs c + [['ors (? (v6m? g) 'r0 'r12) 'r0 'r1] ['br cc lbl]])
                   (d128? (vty c))                       ; wide truth: either word nonzero
                    (vfs c + '((or r0 r0 r2) (cmp r0 0)) + [['br cc lbl]])
                   (: fs (vfs c + [['cmp (vreg c) 0] ['br cc lbl]])
                      _ (rfree g (vreg c))
                      fs))))))
   ; n little-endian bytes of a constant (the global initializer image)
   (les n v) (: (go i acc) (? (n <= i) (rev acc)
                              (go (i + 1) ((& (>> v (* 8 i)) 255) >< acc)))
               (go 0 ()))
   (zeros n) (n >= 1 ? (0 >< zeros (n - 1)))
   ; a straight-line block copy of sz bytes, src address in r0, dest in r1,
   ; r2 the scratch -- 8 at a time, then a byte tail (struct = struct copies)
   (bcopy g off sz acc)                              ; `ld`/`st` move one WORD -- 8 bytes, 4 on t32
    (: w (wsize g)
       (? (w <= sz) (bcopy g (off + w) (sz - w)
                     (acc + [['ld 'r2 'r0 off] ['st 'r1 off 'r2]]))
          (0 < sz) (bcopy g (off + 1) (sz - 1)
                     (acc + [['ld1 'r2 'r0 off] ['st1 'r1 off 'r2]]))
          acc))
   ; a spill that HOLDS a value across a nested sub-eval (which may itself contain a call).
   ; ⚠ a bare `push` is 8 bytes and would leave rsp at 8 mod 16 while the inner call runs -- a
   ; SysV violation glibc turns into a #GP -- so reserve a whole 16-byte cell.
   (spush r) ['(sub sp sp 16) ['st 'sp 0 r]]
   (spop r)  [['ld r 'sp 0] '(add sp sp 16)]
   ; the SHUTTLE SKIP: a right side whose forms never write r1 or touch sp (no call, no label,
   ; no control -- rdsp's 'bar) may run with the LEFT PARKED IN r1: one mov replaces the
   ; push/pop memory round-trip. the left protocol stays r1, the right r0 -- callers unchanged.
   (r1safe? fs)
    (? !(two? fs) 1
       (: f <fs
          (? !(two? f) ()
             (: rd (rdsp f)
                (? (id? rd 'bar) ()
                   (member? 'r1 <>rd) ()
                   (member? 'sp <>rd) ()
                   (r1safe? >fs))))))
   ; the POOL CELL (hom rung 3): when r1 is barred, the left may still hold in a pool register
   ; ACROSS the right's forms -- labels and internal branches are the right's own (nlab-fresh,
   ; no path enters from outside), so only a call (the pool clobber), an unknown op, or a write
   ; of the candidate bars it. an inline-spliced call passes: the bar reads the FORMS, not the AST.
   ; ⚠ the write set is read RESOLVED: a freed mint's store still lands on its physical, and a
   ; borrow licensed past it reads garbage -- the one form-scan that runs before vrfix.
   (psafe? g fs rz)
    (: r (rp g rz)
       (hit9 ds) (? !(two? ds) ()
                    (id? (rp g <ds) r) 1
                    (hit9 >ds))
       (safe9 l)
        (? !(two? l) 1
           (: f <l
              (? !(two? f) (safe9 >l)
                 (member? <f '(label br jmp)) (safe9 >l)
                 (: rd (rdsp f)
                    (? (id? rd 'bar) ()
                       (hit9 <>rd) ()
                       (safe9 >l))))))
       (safe9 fs))
   ; a free pool register bf never writes -> r | () (dry or barred: the sp cell stands).
   ; a BORROW, not an allocation: the park's span closes inside its own staging (write,
   ; bf, read -- nothing generates between), so the free list keeps its order untouched.
   (spare g bf)
    (: (go l) (? !(two? l) ()
                 (member? <l (g 'rpin))
                  (scare 'spare-pinned <l)
                 (psafe? g bf <l) <l
                 (go >l))
       (go (g 'pool)))

   ; -- the PAIR kit (t32 llong/ullong, moon rung 3): a 64-bit value rides r0 (lo) : r1 (hi),
   ; the AAPCS32 return pair. pairs NEVER enter the operand pool or the immediate lanes --
   ; every pair op runs the shuttle, with r2:r3 the popped left operand and r12 the scratch
   ; (IP, already the borrowed temp). the same 16-byte spill cell holds both words. --
   (pair? g ty) (t32? g && llt? ty)
   (pwide ty) (usg? ty ? '((li r1 0)) '((mov r1 r0) (sar r1 31)))             ; widen r0 by its SOURCE signedness
   (asll g v) (llt? (vty v) ? (tor0 g v) (tor0 g v + pwide (vty v)))      ; land a pair on the protocol
   (spop2 ra rb) [['ld ra 'sp 0] ['ld rb 'sp 4] '(add sp sp 16)]
   ; pair load/store at base+off. a load whose base is r0 loads the HIGH word first (into r1),
   ; so the low load may then overwrite the base. pst's base must never be r0/r1 (use r2/r4/r12).
   (pld base off) (id? base 'r0 ? [['ld 'r1 base (off + 4)] ['ld 'r0 base off]]
                                  [['ld 'r0 base off] ['ld 'r1 base (off + 4)]])
   (pst base off) [['st base off 'r0] ['st base (off + 4) 'r1]]
   ; the pair ++/-- tail: old rides r0:r1, the sign-widened step lands in r2:r3,
   ; the stepped pair stores back through base+off (base never r0..r3)
   (ppost base off sv)
    [['li 'r2 (sv & 4294967295)]
      ['li 'r3 (sv < 0 ? 4294967295 0)]
      '(adds r2 r0 r2) '(adc r3 r1 r3)
      ['st base off 'r2] ['st base (off + 4) 'r3]]
   ; flags := (la:lb) - (ra:rb), for a NORMALIZED relation (lt/ge; gt/le swap operands first).
   ; eq/ne OR the two word-differences instead (la is dead after its read: it takes the hi diff).
   ; v6-M: no r12-dest ALU, so the differences land IN la:lb -- ⚠ every call site's left pair
   ; must be dead past the compare.
   (pcmpf g op la lb ra rb)
    (? (v6m? g)
     ([['subs la la ra] ['sbcs lb lb rb]] + (op @ ('eq || 'ne) [['ors la la lb]]))
     (['subs 'r12 la ra] >< (op @ ('eq || 'ne) [['sbc la lb rb] ['ors 'r12 'r12 la]]
                                               [['sbcs 'r12 lb rb]])))
   ; a pair relation as (cc forms), operands lhs r2:r3 / rhs r0:r1 (the shuttle shape):
   ; gt/le compute the SWAPPED difference and flip to lt/ge; signedness picks lt/below via scc.
   (prel g op unsg)
    (? (member? op '(eq ne)) [op (pcmpf g op 'r2 'r3 'r0 'r1)]
       (member? op '(lt ge)) [(scc op unsg) (pcmpf g op 'r2 'r3 'r0 'r1)]
       [(scc (? (op = 'gt) 'lt 'ge) unsg) (pcmpf g op 'r0 'r1 'r2 'r3)])
   ; the pair BRANCH: cbranch's twin of prel -- cc 'ne branches when (la:lb OP ra:rb) holds,
   ; cc 'eq when it does not (the negated relation keeps the same operand order: !lt = ge).
   (pbrf g op unsg la lb ra rb cc lbl)
    (: o2 (cc @ 'ne op (negrel op))
       (? (member? o2 '(eq ne)) (pcmpf g o2 la lb ra rb + [['br o2 lbl]])
          (member? o2 '(lt ge)) (pcmpf g o2 la lb ra rb + [['br (scc o2 unsg) lbl]])
          (pcmpf g o2 ra rb la lb + [['br (scc (o2 @ 'gt 'lt 'ge) unsg) lbl]])))
   ; a pair shift by a CONSTANT count (kind 'shl/'shr/'sar), value on the protocol; in-place.
   (pshiftc kind c)
    (? (c < 1) ()
       (63 < c) '((li r0 0) (li r1 0))
       (kind = 'shl)
        (? (c = 32) '((mov r1 r0) (li r0 0))
           (32 < c) ['(mov r1 r0) ['shl 'r1 (c - 32)] '(li r0 0)]
           ['(mov r2 r0) ['shr 'r2 (32 - c)] ['shl 'r1 c] '(or r1 r1 r2) ['shl 'r0 c]])
       (kind = 'shr)
        (? (c = 32) '((mov r0 r1) (li r1 0))
           (32 < c) ['(mov r0 r1) ['shr 'r0 (c - 32)] '(li r1 0)]
           ['(mov r2 r1) ['shl 'r2 (32 - c)] ['shr 'r0 c] '(or r0 r0 r2) ['shr 'r1 c]])
       ; sar: the high word keeps its sign; a >=32 count floods it
       (? (c = 32) '((mov r0 r1) (sar r1 31))
          (32 < c) ['(mov r0 r1) ['sar 'r0 (c - 32)] '(sar r1 31)]
          ['(mov r2 r1) ['shl 'r2 (32 - c)] ['shr 'r0 c] '(or r0 r0 r2) ['sar 'r1 c]]))
   ; a pair shift by a VARIABLE count in r2 (0..63), value on the protocol. branchy: the big lane
   ; (count >= 32) moves one word across; the small lane composes the carry-over through r12
   ; (⚠ count 0 skips -- a 32-c shift at c=0 would be UNPREDICTABLE).
   ; v6-M: no r12 ALU dest and no free low temp -> libgcc's helper (count in r2, same staging).
   (pshiftv g kind)
    (? (v6m? g)
       [['call (kind @ 'shl '__aeabi_llsl 'shr '__aeabi_llsr '__aeabi_lasr)]]
    (: lb (nlab g ".pb") ls (nlab g ".ps") ld (nlab g ".pd")
       one (kind @ 'shl '((mov r1 r0) (shlv r1 r2) (li r0 0))
                   'shr '((mov r0 r1) (shrv r0 r2) (li r1 0))
                        '((mov r0 r1) (sarv r0 r2) (sar r1 31)))
       sm (kind @ 'shl
             '((mov r12 r0) (li r3 32) (sub r3 r3 r2) (shrv r12 r3)
                (shlv r1 r2) (or r1 r1 r12) (shlv r0 r2))
             (: hop (kind @ 'shr 'shrv 'sarv)
                ['(mov r12 r1) '(li r3 32) '(sub r3 r3 r2) '(shlv r12 r3)
                   '(shrv r0 r2) '(or r0 r0 r12) [hop 'r1 'r2]]))
       (['(cmp r2 32) ['br 'ae lb] '(cmp r2 0) ['br 'eq ld]]
        + sm + [['jmp ld] ['label lb] '(sub r2 r2 32)]
        + one + [['label ld]])))
   ; the 64-bit float bridges (VFP on AArch32 converts only through 32 bits). llong -> double:
   ; hi rides the U-converter and scales by 2^32 (exact), lo adds exactly, ONE rounding total;
   ; the signed face negates the pair in, the double out (INT64_MIN survives: its u64 face IS
   ; 2^63). value r0:r1 -> f0. v6-M: the whole bridge is one libgcc call.
   (cvtlld g sgn)
    (? (v6m? g)
       [['call (? sgn '__aeabi_ul2d '__aeabi_l2d)] '(movqxr f0 r0)]
    (: lu (nlab g ".lu") ld (nlab g ".ld")
       u2d '((cvtui2sd f0 r1)
             (li r2 0) (li r3 1106247680) (movqxr f1 r2)   ; 2^32 (0x41F00000:0)
              (mulsd f0 f1)
              (cvtui2sd f1 r0) (addsd f0 f1))
       (? sgn u2d                                    ; unsigned: straight through
          (['(cmp r1 0) ['br 'ge lu]
              '(li r2 0) '(li r3 0) '(subs r0 r2 r0) '(sbc r1 r3 r1)]
           + u2d
           + ['(li r2 0) '(li r3 0) '(movqxr f1 r2)           ; 0.0 - d
              '(subsd f1 f0) '(movsd f0 f1) ['jmp ld] ['label lu]]
           + u2d + [['label ld]]))))
   ; double -> llong, toward zero: peel sign/exponent off the PATTERN, rebuild the 53-bit integer
   ; mantissa as a pair, shift it into place, refix the sign. |d| < 1 answers 0; out-of-range is
   ; UB and shifts to garbage like the hardware would. v6-M: one libgcc call (the peel itself is
   ; sign-blind bit arithmetic -- only the libcall face needs the target's signedness).
   (cvtdll g usg)
    (? (v6m? g)
       ['(movqrx r0 f0) ['call (? usg '__aeabi_d2ulz '__aeabi_d2lz)]]
    (: l1 (nlab g ".d1") lr (nlab g ".dr") ls (nlab g ".ds") ld (nlab g ".dd")
       (['(movqrx r0 f0)
           '(mov r2 r1) '(shr r2 31) '(sub sp sp 16) '(st sp 0 r2)   ; park the sign
           '(mov r2 r1) '(shr r2 20) '(li r3 2047) '(and r2 r2 r3)   ; e
           '(li r3 1048575) '(and r1 r1 r3)
           '(li r3 1048576) '(or r1 r1 r3)                                ; mantissa | implicit bit
           '(li r3 1023) '(cmp r2 r3) ['br 'ge l1]
           '(li r0 0) '(li r1 0) '(add sp sp 16) ['jmp ld]              ; |d| < 1 -> 0
           ['label l1]
           '(sub r2 r2 1075) '(cmp r2 0) ['br 'lt lr]]
        + pshiftv g 'shl
        + [['jmp ls] ['label lr] '(li r3 0) '(sub r2 r3 r2)]
        + pshiftv g 'shr
        + [['label ls] '(ld r2 sp 0) '(add sp sp 16)
           '(cmp r2 0) ['br 'eq ld]
           '(li r2 0) '(li r3 0) '(subs r0 r2 r0) '(sbc r1 r3 r1)
           ['label ld]])))

   ; -- the WIDE kit (i128/u128, the w128? targets): a 128-bit value rides hi r2 : lo r0
   ; (rdx:rax -- mulq's own pair and the SysV i128 return; x2:x0 is aarch64's), ONLY on the r0
   ; protocol -- never the operand pool, never an immediate lane, never a home (homable?
   ; excludes it, so the universes do too). a binop's left operand parks in ONE 16-byte
   ; machine-stack cell ([sp] lo / [sp+8] hi); the carrying ops (a128/s128/m128/mw128/c128)
   ; are single holo forms reading that cell, so no pass can part a flags pair, and their 'sp
   ; operand keeps deadcell honest (rdsp answers 'bar -- every pass conservative). off w128?
   ; the guards answer 'bad, and the driver withholds __SIZEOF_INT128__ to match. --
   (d128? ty) (ty = 'i128 || ty = 'u128)
   (x64t? g) !(arm? g)
   (w128? g) (x64t? g || a64? g)             ; carries the wide pair: x64's rdx:rax, aarch64's x2:x0
   (rot4? g) (x64t? g || a64? g)             ; carries the rotate ops (ror4/rorv/rorv4); elsewhere the shifts stand
   (wbin ta tb) (? (d128? ta && d128? tb) (? (id? ta 'u128 || id? tb 'u128) 'u128 'i128)
                   (d128? ta) ta tb)                ; a word side converts to the wide side's type
   ; land a value on the wide protocol: a pair rides; a word widens by its SOURCE signedness
   (as128 g v) (? (d128? (vty v)) (tor0 g v)
                  (tor0 g v + (? (usg? (vty v) || ptr? (vty v)) '((li r2 0)) '((sx128)))))
   ; did this value's forms end in the zero-extend? (the widening-mul license: hi is 0)
   (zext128? fs) (: rl (rev fs) (two? rl && '(li r2 0) = <rl))
   ; a fresh n-byte frame cell -> its (negative) offset. bumps the frame's
   ; high-water like a local; ntmp sizes one for a type (a compound literal's home).
   ; every cell registers in g 'slots ((off n)..) -- the OBJECT map deadst and repack
   ; read (an lea escapes the whole object under it, never one granule).
   (nslot g n) (: off -(peep g 'off 0 + n)
                  _ (pin g 'off (peep g 'off 0 + n))
                  _ (pin g 'slots ([off n] >< g 'slots))
                  off)
   ; the object under a frame offset, off the slot map -- () when no cell claims it
   (obj9 sl x) (? !(charm? x) ()
                  (: (go l) (two? l ?
                             (: o <l (<o <= x && x < <o + <>o ? o (go >l))))
                     (go sl)))
   (ntmp g ty) (nslot g $ (tsz g ty + 7) & -8)
   ; the store-immediate op for (ty, k) | () -- the vi fold's license, shared by the
   ; asn/decl delivery lanes (x64 only: si is C6/C7). st sign-extends imm32, the
   ; narrow forms truncate like their register twins.
   (siop9 ty k) (stop ty @
    'st  (fits32? k ? 'si)
    'st4 (-2147483648 <= k <= 4294967295 ? 'si4)
    'st2 (-32768 <= k <= 65535 ? 'si2)
    'st1 (-128 <= k <= 255 ? 'si1))
   ; load a value from the address in r0, by type -- the dot/deref/clit value tail
   ; (arrays and functions decay to the address; a struct has no register value). the scalar
   ; tail honors the want hint: the load lands straight in the hinted register
   ; (the wide/float/aggregate lanes keep their protocols)
   (loadval g r W9)                                 ; the VALUE at an lvalue rec (mem face consumed: one fused
    (: ty (vty r) fs (vfs r)                        ;  form, and the face's base register is freed here)
       b (? (vm? r) (vmb r) 'r0) o (? (vm? r) (vmo r) 0)
       _ (rfree g b)
       (? (arr? ty) (mkv ['ptr <>ty] 'r0 (fs + mata b o))
          (gvarr? ty) (mkv ['ptr (varel ty)] 'r0 (fs + mata b o))   ; a VLA row decays to its address (p[i] a char*)
          (fn? ty) (mkv ['ptr ty] 'r0 (fs + mata b o))
          (stru? ty) (mkv ty 'r0 (fs + mata b o))    ; a struct value IS its address (the pair reps read through it)
          (flo? ty) (mkv 'double 'r0 (fs + fldf ty 'f0 b o))
          (pair? g ty) (mkv ty 'r0 (fs + pld b o))
          (d128? ty) (mkv ty 'r0 (fs + [['ld 'r2 b (o + 8)] ['ld 'r0 b o]]))   ; hi first: the low load may overwrite the base
          (: W (? W9 W9 'r0)
             (mkv ty W (fs + [[(ldop ty) W b o]])))))
   ; the VALUE at an lvalue rec: a bitfield loads its unit and extracts with two
   ; shifts (sar signed, shr unsigned), anything else loads flat.
   (lvload g r wnt)
    (: ty (vty r)
       (? (bitf? ty)
           (: uty <>ty bitoff <>>ty width <>>>ty
              usz (tsz g uty) hi (64 - (bitoff + width)) lo (64 - width)
              sgn !(usg? uty || id? uty 'bool)         ; a bool field holds 0/1: never sign-extend
              b (? (vm? r) (vmb r) 'r0) o (? (vm? r) (vmo r) 0)
              _ (rfree g b)
              (mkv 'long 'r0
                   (vfs r
                    + [[(uldop usz) 'r0 b o]]
                    + (? (hi > 0) [['shl 'r0 hi]] ())
                    + (? (lo > 0) [[(? sgn 'sar 'shr) 'r0 lo]] ()))))
          (loadval g r wnt)))
   ; the fill code for a compound literal at frame offset off: an aggregate zeroes
   ; then fills (cgfill), a scalar unwraps its single-element brace and stores.
   (clitcode g env off ty ini)
    (? (arr? ty || stru? ty)
        (: f (cgfill g env off ty ini)
           (? (bad? f) 'bad (cgzero off ((tsz g ty + 7) & -8) + f)))
       (: v (? (two? ini && <ini = 'init)
               (? (two? <>ini) <<>ini '(num 0)) ini)
          (cgstore g env off ty v)))

   ; -- lvalues: answers a MEM-faced rec (pointee-ty (m base off) forms) | a plain rec with
   ; the address on r0 (globals, clits) | 'bad. consumers lay one fused form off the face
   ; (loadval and friends); matv is the bridge for lanes still wanting the address on r0 --
   (clval g env e)
    (e @
       ('var nm)
        (: en (vent g env nm)
           (? (bad? en) 'bad
              (member? <en '(regv regw)) 'bad   ; a register home has no address (& is gated out per-fn)
              (<en = 'loc) (mkm <>>en 'r4 <>en ())
              (mkv <>>en 'r0 [['la 'r0 <>en]])))
       ('deref X)
        ; frame-direct: a constant-indexed element is a static (r4, off) face
        (: fdc (afd g env e)
           (? (two? fdc) (mkm <>fdc 'r4 <fdc ())
              ; ptr ± const: the SCALED constant folds into the face's offset, so the
              ; add never emits (p[1] is one load). static only: a var whose vent type
              ; is a plain pointer (a VLA pointee scales at runtime -- cgbin's lane)
              (: pv (X @ ('bin op a b && member? op '(+ -))
                          (: ka (knum g a) kb (knum g b)
                             (? (two? kb && !(two? ka)) [a (op @ '+ <kb (0 - <kb))]
                                (two? ka && !(two? kb) && op = '+) [b <ka])))
                 pty (<pv @ ('var nm)
                            (: en (vent g env nm)
                               (? (two? en && member? <en '(loc regv glo) && ptr? <>>en &&
                                   !(gvarr? <>(<>>en)))
                                  <>>en)))
                 off9 (? pty (: d (* <>pv (tsz g <>pty))
                               (? (fits32? d) d 'no)) 'no)
                 (? (charm? off9)
                    (: r (cgexpr g env <pv ())
                       (? (bad? r) 'bad
                          (mkm <>pty (vreg r) off9 (vfs r))))
                    (: r (cgexpr g env X ())
                       (? (bad? r) 'bad
                          !(ptr? (vty r)) 'bad
                          ; the face keeps the pointer's OWN register (a home, a pin, a park --
                          ; the consumer reads through it and frees it); no bridge to r0
                          (mkm <>(vty r) (vreg r) 0 (vfs r))))))))
       ('dot base fld)
        (: r (: lv (clval g env base)
                ; an rvalue struct base (a call's parked temp: `mk(x).a`) is
                ; its address too -- same license the asn lane rides
                (? !(bad? lv) lv
                   (: rv (cgexpr g env base ())
                      (? (bad? rv) 'bad !(stru? (vty rv)) 'bad rv))))
           (? (bad? r) 'bad
              (: fm (fmem g (vty r) fld)
                 (? (bad? fm) (: _ (? (vm? r) (rfree g (vmb r)) ()) 'bad)
                    (vm? r) (mkm <>fm (vmb r) (vmo r + <fm) (vfs r))   ; the member FOLDS into the face
                    (mkm <>fm 'r0 <fm (vfs r))))))
       ('clit ty ini)                            ; a compound literal is an lvalue: fill a temp, address it
        (: off (ntmp g ty)
           fill (clitcode g env off ty ini)
           (? (bad? fill) 'bad
              (mkv ty 'r0 (fill + [['lea 'r0 'r4 off]]))))   ; a real lea: the temp's address is the value (& pins via the addr lane, a call blocks via the window)
       ; gen's OWN lvalue, minted by the rmw lane below and reachable from no
       ; source: the object whose address is moored at frame offset off. reading
       ; it is calm, so it stands in for an lvalue that must evaluate once.
       ('moor off ty) (mkm ty 'r0 0 [['ld 'r0 'r4 off]])
       'bad)

   ; the STATIC face of a store target, one door: a deref-const element (afd), or a dot
   ; chain over a frame local whose clval face is (r4, off) with NO emission -- (off ty)
   ; | (). the frame-direct store lanes ride either identically; afd's type gate is
   ; mirrored so every rider stores flat. ⚠ only a pure
   ; var/dot chain over a loc enters clval -- any other base could emit into the discard.
   (sfd g env e)
    (? (id? <e 'deref) (afd g env e)
       !(id? <e 'dot) ()
       (: (dotv? x) (x @ ('var nm) nm  ('dot . b . _) (dotv? b)  ())
          nm (dotv? e)
          (? !nm ()
             (: en (vent g env nm)
                (? !(two? en && id? <en 'loc) ()
                   (: lv (clval g env e)
                      (? !(vm? lv) ()
                         !(id? (vmb lv) 'r4) ()
                         (two? (vfs lv)) ()
                         (: ty (vty lv)
                            (? ((ptr? ty || member? ty '(char uchar bool short ushort int uint long ulong)) &&
                                !(pair? g ty))
                               [(vmo lv) ty] ())))))))))

   ; -- the gcc builtins love.c reaches -- x64 as short raw splices, arm64 as neutral IR or one
   ; raw word (clz): trap = ud2 / brk; inf/nanf = IEEE constants into f0; expect = its value (the
   ; hint drops); clear_cache = nothing on x86; clzll = bsr+xor / clz; isinf = bits<<1 == inf<<1;
   ; the overflow trio = the op + the overflow flag (seto / set vs, mul via mulo's smulh check),
   ; result stored through arg3, the flag the value.
   (cgbuiltin g env bi args)                       ; a builtin call -> its value | 'bad; not one -> 'no
    (? (member? bi '("__builtin_trap" "__builtin_unreachable"))
        ; unreachable rides the trap: gcc emits nothing and lets the fall-through run
        ; into whatever follows, which is the one lowering that cannot be debugged
        (? (arm? g) (mkv 'long 'r0 '((trap)))
           (mkv 'long 'r0 '((ud2))))     ; the named op, not raw bytes: a raw bars unframe in every splice site
       (bi = "__builtin_inf")
        (? (t32? g) (mkv 'double 'r0 '((li r0 0) (li r1 2146435072) (movqxr f0 r0)))
           (mkv 'double 'r0 '((li r0 9218868437227405312) (movqxr f0 r0))))
       (bi = "__builtin_nanf" || bi = "__builtin_nan")   ; both take a (ignored) tag string; one quiet-NaN pattern
        (? (t32? g) (mkv 'double 'r0 '((li r0 0) (li r1 2146959360) (movqxr f0 r0)))
           (mkv 'double 'r0 '((li r0 9221120237041090560) (movqxr f0 r0))))
       (bi = "__builtin_expect") (cgexpr g env <args ())
       (bi = "__builtin___clear_cache")
        ; x86 keeps I-cache coherent with D-cache: nothing to do. arm64 does NOT (qemu-user is
        ; coherent, metal is not), so the architectural sequence: dc cvau each D-line, dsb, ic
        ; ivau each I-line, dsb+isb, strides read off CTR_EL0 (DminLine[19:16] / IminLine[3:0],
        ; log2 words). the system insns ride raw words, and the variable stride shift too.
        ; ⚠ scratch = r2/r3/r9/r15 ONLY -- NEVER r10-r14, the a64 param/local HOMES, resident
        ; across the whole fn and spilled only around real calls.
        (? !(a64? g) (mkv 'long 'r0 ())
           (: e1 (cgexpr g env <args ())
              e2 (cgexpr g env <>args ())
              lb (nlab g ".ccd") db (nlab g ".ccdd")
              li9 (nlab g ".cci") di (nlab g ".ccid")
              (? (bad? e1 || bad? e2) 'bad
                 (mkv 'long 'r0 $
                   asint g e1 + spush 'r0 + asint g e2 + '((mov r1 r0)) + spop 'r0 +
                     ['(raw (34 0 59 213))          ; mrs x2, ctr_el0
                       '(li r9 15)
                       '(and r15 r2 r9)
                       '(li r3 4)
                       '(raw (111 32 207 154))      ; lsl x15, x3, x15 -- isize
                       '(shr r2 16)
                       '(and r2 r2 r9)
                       '(raw (98 32 194 154))       ; lsl x2, x3, x2 -- dsize
                       '(sub r3 r2 1)
                       '(not r3)
                       '(and r9 r0 r3)            ; begin & ~(dsize-1)
                       ['label lb]
                       '(cmp r9 r1)
                       ['br 'ae db]
                       '(raw (41 123 11 213))       ; dc cvau, x9
                       '(add r9 r9 r2)
                       ['jmp lb]
                       ['label db]
                       '(raw (159 59 3 213))        ; dsb ish
                       '(sub r3 r15 1)
                       '(not r3)
                       '(and r9 r0 r3)            ; begin & ~(isize-1)
                       ['label li9]
                       '(cmp r9 r1)
                       ['br 'ae di]
                       '(raw (41 117 11 213))         ; ic ivau, x9
                       '(add r9 r9 r15)
                       ['jmp li9]
                       ['label di]
                       '(raw (159 59 3 213))          ; dsb ish
                       '(raw (223 63 3 213))]))))     ; isb
       (member? bi '("__builtin_clzll" "__builtin_clzl" "__builtin_clz"))
        (: e1 (cgexpr g env <args ())
           ; the `l` spelling is as wide as the target's own long: 8 bytes everywhere but t32
           ll (? (bi = "__builtin_clz") () (bi = "__builtin_clzll") 1 !(t32? g))
           pre (? ll () '((shl r0 32)))    ; clz32(x) = clz64(x << 32) -- the shift also clears upper garbage
           (? (bad? e1) 'bad
              (t32? g && !ll)              ; clz32 is one word: CLZ / libgcc's __clzsi2
               (mkv 'long 'r0 (asint g e1 +
                  (? (v6m? g) '((call __clzsi2)) '((clz r0 r0)))))
              (t32? g)                     ; the pair: clz(hi), or 32 + clz(lo) when hi is 0
               (: lz (nlab g ".cz") ld (nlab g ".czd")
                  (mkv 'long 'r0 (asll g e1 +
                    (? (v6m? g)   ; no CLZ on v6-M: libgcc's __clzsi2 (arg/result r0)
                       ['(cmp r1 0) ['br 'eq lz] '(mov r0 r1) '(call __clzsi2) ['jmp ld]
                          ['label lz] '(call __clzsi2) '(add r0 r0 32) ['label ld]]
                       ['(cmp r1 0) ['br 'eq lz] '(clz r0 r1) ['jmp ld]
                          ['label lz] '(clz r0 r0) '(add r0 r0 32) ['label ld]]))))
              (rv? g)                      ; no clz in RV64I: the 6-stage binary search (branchy; clz(0) stays UB)
               (: ls (map (\ k (nlab g ".cz")) '(0 1 2 3 4 5))
                  step (\ kk lb2 ['(mov r2 r0) ['shr 'r2 (64 - kk)] '(cmp r2 0) ['br 'ne lb2]
                                   ['add 'r1 'r1 kk] ['shl 'r0 kk] ['label lb2]])
                  (mkv 'long 'r0 (asint g e1 + pre +
                    ('((li r1 0))
                     + step 32 (nthr ls 0) + step 16 (nthr ls 1) + step 8 (nthr ls 2)
                     + step 4 (nthr ls 3) + step 2 (nthr ls 4) + step 1 (nthr ls 5)
                     + '((mov r0 r1))))))
              (mkv 'long 'r0 (asint g e1 + pre +
                              (? (a64? g) '((raw (0 16 192 218)))                    ; clz x0, x0
                                 '((raw (72 15 189 192 72 131 240 63))))))))   ; bsr rax,rax; xor rax,63
       ; count trailing zeros (dlmalloc's compute_bit2idx; ctz(0) stays UB; for the 32-bit
       ; spelling upper garbage is harmless -- the defined case has a set bit in the low 32):
       ; x64 bsf, a64 rbit+clz, rv a mask-narrowing search, t32 isolate-and-clz. x64's bsf and
       ; rv's ladder both walk UP from the low bit, so the l/ll widths cost them nothing
       (member? bi '("__builtin_ctzll" "__builtin_ctzl" "__builtin_ctz"))
        (: e1 (cgexpr g env <args ())
           ll (? (bi = "__builtin_ctz") () (bi = "__builtin_ctzll") 1 !(t32? g))
           (? (bad? e1) 'bad
              (ll && t32? g) (nolane g ("no lane for " + bi + (ontgt g)))   ; the pair lane, again
              (t32? g)
               (mkv 'long 'r0 (asint g e1 +
                  (? (v6m? g) '((call __ctzsi2))
                     '((li r1 0) (sub r1 r1 r0) (and r0 r0 r1)     ; x & -x, then 31 - clz
                        (clz r0 r0) (li r1 31) (sub r0 r1 r0)))))
              (rv? g)
               (: ls (map (\ k (nlab g ".tz")) '(0 1 2 3 4 5))
                  step (\ kk mk lb2 [['li 'r3 mk] '(mov r2 r0) '(and r2 r2 r3) '(cmp r2 0)
                                      ['br 'ne lb2] ['add 'r1 'r1 kk] ['shr 'r0 kk] ['label lb2]])
                  (mkv 'long 'r0 (asint g e1 +
                    ('((li r1 0))
                     + step 32 4294967295 (nthr ls 0) + step 16 65535 (nthr ls 1)
                     + step 8 255 (nthr ls 2) + step 4 15 (nthr ls 3)
                     + step 2 3 (nthr ls 4) + step 1 1 (nthr ls 5)
                     + '((mov r0 r1))))))
              (mkv 'long 'r0 (asint g e1 +
                              (? (a64? g && ll) '((raw (0 0 192 218 0 16 192 218)))   ; rbit x0,x0; clz x0,x0
                                 (a64? g) '((raw (0 0 192 90 0 16 192 90)))   ; rbit w0,w0; clz w0,w0
                                 '((raw (72 15 188 192))))))))                ; bsf rax,rax -- REX.W, so it serves both widths
       ; the bswap trio (glibc's <byteswap.h> inlines): fully-masked neutral shift/or
       ; expansions -- no raw (unframe stays alive), no new ops. 16/32 are width-blind
       ; and ride every target; 64 is the three-stage swap (bytes, halves, words) and
       ; falls to the undeclared refusal on t32 (a pair lane with no taker yet)
       (member? bi '("__builtin_bswap16" "__builtin_bswap32" "__builtin_bswap64"))
        (? ((bi = "__builtin_bswap64") && t32? g)
            (nolane g ("no lane for __builtin_bswap64" + ontgt g))   ; the pair lane has no taker; 'no would emit a call the link can't bind
           (: e1 (cgexpr g env <args ())
              (? (bad? e1) 'bad
                 (mkv 'long 'r0 (asint g e1 +
                   (? (bi = "__builtin_bswap16")
                      '((li r2 255) (mov r1 r0) (shr r1 8) (and r1 r1 r2)
                        (and r0 r0 r2) (shl r0 8) (or r0 r0 r1))
                      (bi = "__builtin_bswap32")
                      '((li r2 255)
                        (mov r1 r0) (shr r1 24) (and r1 r1 r2)
                        (mov r3 r0) (shr r3 16) (and r3 r3 r2) (shl r3 8) (or r1 r1 r3)
                        (mov r3 r0) (shr r3 8) (and r3 r3 r2) (shl r3 16) (or r1 r1 r3)
                        (and r0 r0 r2) (shl r0 24) (or r0 r0 r1))
                      '((li r2 71777214294589695)                  ; 0x00FF00FF00FF00FF
                        (mov r1 r0) (shr r1 8) (and r1 r1 r2)
                        (and r0 r0 r2) (shl r0 8) (or r0 r0 r1)
                        (li r2 281470681808895)                    ; 0x0000FFFF0000FFFF
                        (mov r1 r0) (shr r1 16) (and r1 r1 r2)
                        (and r0 r0 r2) (shl r0 16) (or r0 r0 r1)
                        (mov r1 r0) (shr r1 32) (shl r0 32) (or r0 r0 r1))))))))
       ; popcount: the SWAR fold -- pairs, then nibbles, then the byte counts summed
       ; by shifts alone. masks and adds only, like the bswap trio above, so it rides
       ; every target and assumes no ISA feature (x64's POPCNT is SSE4.2, a64's is
       ; SIMD; a raw lane for either is a later rung, not a correctness one). the
       ; 64-bit forms fall to the undeclared refusal on t32, whose pair lane has no
       ; taker. ⚠ the 32-bit form MASKS FIRST: the operand is an unsigned int and the
       ; word above bit 31 may carry a wider value's garbage, which would be counted.
       (member? bi '("__builtin_popcount" "__builtin_popcountl" "__builtin_popcountll"))
        (? ((bi != "__builtin_popcount") && t32? g)
            (nolane g ("no lane for " + bi + ontgt g))
           (: e1 (cgexpr g env <args ())
              w32 (bi = "__builtin_popcount" || t32? g)
              fold (\ k ['(mov r1 r0) ['shr 'r1 k] '(add r0 r0 r1)])
              ; ⚠ each arm is ONE parenthesized span: a bare `+` inside a clause arm
              ; captures the rest of the clause (the valence law), not the arm.
              w32ops ((? (t32? g) () '((li r2 4294967295) (and r0 r0 r2)))   ; the word above bit 31 is not ours to count
                      + '((li r2 1431655765) (mov r1 r0) (shr r1 1) (and r1 r1 r2) (sub r0 r0 r1)
                          (li r2 858993459) (mov r1 r0) (shr r1 2) (and r1 r1 r2) (and r0 r0 r2) (add r0 r0 r1)
                          (li r2 252645135) (mov r1 r0) (shr r1 4) (add r0 r0 r1) (and r0 r0 r2))
                      + (fold 8) + (fold 16) + '((li r2 63) (and r0 r0 r2)))
              w64ops ('((li r2 6148914691236517205) (mov r1 r0) (shr r1 1) (and r1 r1 r2) (sub r0 r0 r1)
                        (li r2 3689348814741910323) (mov r1 r0) (shr r1 2) (and r1 r1 r2) (and r0 r0 r2) (add r0 r0 r1)
                        (li r2 1085102592571150095) (mov r1 r0) (shr r1 4) (add r0 r0 r1) (and r0 r0 r2))
                      + (fold 8) + (fold 16) + (fold 32) + '((li r2 127) (and r0 r0 r2)))
              (? (bad? e1) 'bad
                 (mkv 'long 'r0 (asint g e1 + (w32 ? w32ops w64ops))))))
       ; the __sync spin-lock pair (dlmalloc's CAS_LOCK/CLEAR_LOCK): test_and_set is
       ; the atomic exchange with acquire -- x64 xchg (implicitly locked), a64 the
       ; ldaxr/stxr retry, rv amoswap.{w,d}.aq -- and release a release-ordered store
       ; of 0 (x64 TSO plain store, a64 stlr, rv fence rw,w + store). the POINTEE
       ; sizes and signs the exchange (4/8 bytes; the old value re-extends via cvt),
       ; and an unsized pointee refuses -- guessing a width miscompiles in silence
       (member? bi '("__sync_lock_test_and_set" "__sync_lock_release"))
        (: e1 (cgexpr g env <args ())
           pt (two? (vty e1) && <(vty e1) = 'ptr ? <>(vty e1))
           w (? (member? pt '(int uint)) 4
                (ptr? pt || member? pt '(long ulong)) 8 ())
           (? (t32? g) (nolane g (("no lane for " + bi) + ontgt g))
              (bad? e1) 'bad
              !w (nolane g (bi + " wants an int* or a long*/T** (the pointee sizes the exchange)"))
              (bi = "__sync_lock_release")
               (mkv 'long 'r0 (tor0 g e1 +
                 '((mov r1 r0) (li r0 0)) +
                 (? (a64? g) [['raw (w @ 4 '(32 252 159 136) '(32 252 159 200))]]   ; stlr w0/x0, [x1]
                    (rv? g) ('((raw (15 0 16 3))) +                                       ; fence rw,w
                             [[(? (w = 4) 'st4 'st) 'r1 0 'r0]])
                    [[(w @ 4 'st4 'st) 'r1 0 'r0]])))
              (: e2 (cgexpr g env <>args ())
                 (? (bad? e2) 'bad
                    (: pre (tor0 g e1 + spush 'r0 + asint g e2 + spop 'r1)   ; val r0, ptr r1
                       body (? (a64? g)
                               (: lb (nlab g ".ts")
                                  [['label lb]
                                   ['raw (w @ 4 '(34 252 95 136) '(34 252 95 200))]   ; ldaxr w2/x2, [x1]
                                   ['raw (w @ 4 '(32 124 3 136) '(32 124 3 200))]     ; stxr w3, w0/x0, [x1]
                                   '(cmp r3 0) ['br 'ne lb] '(mov r0 r2)])
                              (rv? g)
                               [['raw (w @ 4 '(47 165 165 12) '(47 181 165 12))]]      ; amoswap.w/d.aq a0,a0,(a1)
                              [['raw (w @ 4 '(135 1) '(72 135 1))]])                   ; xchg eax/rax, (rcx)
                       (mkv pt 'r0 (pre + body + cvt pt 'r0)))))))
       (bi = "__builtin_isinf")
        (: e1 (cgexpr g env <args ())
           (? (bad? e1) 'bad
              (mkv 'long 'r0 (asflo g e1 +
                        (? (v6m? g)                 ; the r12-free twin: the diffs land in r1 (dead past the set)
                           '((movqrx r0 f0) (li r2 2147483647) (and r1 r1 r2)
                              (li r2 2146435072) (subs r1 r1 r2)
                              (ors r1 r1 r0) (set eq r0))
                           (t32? g)                 ; |hi| == 0x7FF00000 and lo == 0, over the pair
                           '((movqrx r0 f0) (li r2 2147483647) (and r1 r1 r2)
                              (li r2 2146435072) (subs r12 r1 r2)
                              (ors r12 r12 r0) (set eq r0))
                           (a8? g)
                           '((movqrx r0 f0) (shl r0 1) (li r1 18437736874454810624)
                              (cmp r0 r1) (set eq r0))
                           (+ '((movqrx r0 f0) (raw (72 209 224)) (li r1 18437736874454810624))   ; shl rax,1; rcx = inf<<1
                              '((raw (72 57 200 15 148 192 72 15 182 192)))))))))     ; cmp; sete al; movzx rax,al
       (member? bi '("__builtin_add_overflow" "__builtin_sub_overflow" "__builtin_mul_overflow"))
        (: e1 (cgexpr g env <args ())
           e2 (cgexpr g env <>args ())
           ; the &t arg is consumed by the splice's own store -- argpos,
           ; like a call arg (else every overflow user loses sibcalls)
           e3 (member? <<>>args '(addr var) ? (pin g 'argpos 1),
               cgexpr g env <>>args ())
           _ (pin g 'argpos ())
           opb (? (bi = "__builtin_add_overflow") '(72 1 200)      ; add rax,rcx
                  (bi = "__builtin_sub_overflow") '(72 41 200)     ; sub rax,rcx
                  '(72 15 175 193))                                ; imul rax,rcx
           ; the op leaves r0 the result, r1 the overflow bit
           ovf (? !(arm? g)
                  [['raw (opb + '(15 144 193 72 15 182 201))]]          ; op; seto cl; movzx rcx,cl
                  !(bi = "__builtin_mul_overflow")
                   [[(bi @ "__builtin_add_overflow" 'adds 'subs) 'r0 'r0 'r1] '(set vs r1)]
                  (v6m? g)                          ; no SMULL: sign-widen both words through __aeabi_lmul,
                   (spush 'r3 +                     ; then hi vs lo>>31. r3 = the &t address -- saved around the call
                    '((mov r2 r1) (mov r1 r0) (sar r1 31)
                      (mov r3 r2) (sar r3 31) (call __aeabi_lmul)
                      (mov r2 r0) (sar r2 31) (subs r1 r1 r2) (set ne r1)) +
                    spop 'r3)
                  (t32? g)                          ; SMULL, then hi vs lo>>31 (no 64-bit regs)
                   '((smull r0 r2 r0 r1) (mov r12 r0) (sar r12 31)
                      (subs r1 r2 r12) (set ne r1))
                  (: lo (nlab g ".vo") l2 (nlab g ".vd")
                     [['mulo 'r0 'r0 'r1 'r2 lo] '(li r1 0) ['jmp l2]
                        ['label lo] '(li r1 1) ['label l2]]))
           ; ⚠ &t's frame lea must not open the sibs WINDOW: the x64 raw splice resets it as a
           ; call boundary, but arm64's neutral IR is no boundary, and an open window silently
           ; declines every following tail. the address is consumed by the splice's own store,
           ; so it reads lean here; x64 IR stays untouched (byte-identity).
           e3f (? (arm? g) (relean (tor0 g e3)) (tor0 g e3))
           (? (bad? e1 || bad? e2 || bad? e3) 'bad
              (mkv 'long 'r0 $
               asint g e1 + spush 'r0 + asint g e2 + spush 'r0 +
               e3f + '((mov r3 r0)) + spop 'r1 + spop 'r0 + ovf +
               '((st r3 0 r0) (mov r0 r1)))))
       'no)

   ; -- expressions: answers a value tuple | 'bad. wnt is the DESTINATION HINT, the die's
   ; advisory face: a consumer that knows where the value must land (an asn
   ; into a home, a ret's r0) passes it; a subexpression only gets one its consumer chose; and
   ; it is ADVISORY -- a lane that ignores it answers its usual register and the consumer
   ; bridges. --
   (cgexpr g env e wnt)
    (e @
       ('szof e2)                                   ; sizeof EXPR: the UNDECAYED type (clval keeps arrays), size a constant
        (: lv (clval g env e2)
           _ (? (vm? lv) (rfree g (vmb lv)) ())   ; the face is discarded: hand back its base
           ty (? (bad? lv) (: r (cgexpr g env e2 ()) (? (bad? r) 'bad (: _ (rfree g (vreg r)) (vty r)))) (vty lv))
           (? (bad? ty) 'bad
              (gvarr? ty && 3 < #ty)   ; sizeof a VLA local: the stashed byte size
               (: sl (g 'vslots (varix ty))
                  (? !(two? sl) 'bad (mkv 'ulong 'r0 [['ld 'r0 'r4 <>sl]])))
              (mkv 'ulong 'r0 [['li 'r0 (tsz g ty)]])))
       ('num v)
        (? (t32? g && !(-2147483648 <= v <= 4294967295))
           ; a literal past the 32-bit word: the pair (C99 decimal picks long long).
           ; split by // and % -- a big literal has no bitwise ops (the imnum lesson)
           (: u (v < 0 ? (v + 18446744073709551616) v)
              (mkv (v < 0 ? 'llong 'ullong) 'r0
                   [['li 'r0 (u % 4294967296)] ['li 'r1 (u // 4294967296)]]))
           (t32? g && 2147483647 < v)
           ; bit 31 set on the 32-bit word: UNSIGNED (0xffffffff reads uint), so a
           ; pair-widen zero-extends -- a signed 'long here sign-smeared the cast
           (: W (? wnt wnt 'r0) (mkv 'ulong W [['li W v]]))
           (w128? g && !(-9223372036854775808 <= v <= 18446744073709551615))
           ; a literal past the 64-bit word (cfold's exact u128 folds): the wide pair,
           ; split by // and % -- a big literal has no bitwise ops (the imnum lesson)
           (: u ((v < 0 ? (v + 64 2 * 64 2) v) % (64 2 * 64 2))
              (mkv 'u128 'r0
                   [['li 'r0 (u % 18446744073709551616)]
                    ['li 'r2 (u // 18446744073709551616)]]))
           (: W (wnt || 'r0) (mkv 'long W [['li W v]])))
       ('flo d)
        (? (t32? g)                                  ; the 64-bit pattern splits over the r0:r1 pair
           (: v (fbits d)
              (mkv 'double 'r0 [['li 'r0 (v & 4294967295)] ['li 'r1 ((v >> 32) & 4294967295)]
                                  '(movqxr f0 r0)]))
           (mkv 'double 'r0 [['li 'r0 (fbits d)] '(movqxr f0 r0)]))
       ('str s)
        (: lbl (nlab g ".S")
           _ (pin g 'strs ([lbl s] >< g 'strs))
           (mkv '(ptr char) 'r0 [['la 'r0 lbl]]))
       (('dot || 'moor) >< _)
        (: r (clval g env e)
           (? (bad? r) 'bad (lvload g r wnt)))
       ('var nm)
        (: en (vent g env nm)
           (? (bad? en)
               (? (g 'fns nm)     ; a function name is its address
                  (mkv ['ptr (fnty g nm)] 'r0 [['la 'r0 (name nm)]])
                  (: _ (pin g 'undecl nm) 'bad))   ; undeclared: name it for the gripe (rung 1); an undeclared CALL funnels here too (call-fixed evaluates its head)
              (<en = 'regv) (mkv <>>en <>en ())   ; a homed param IS its register: zero forms, borrowed (read-only)
              (<en = 'regw)                       ; a pair-homed d128 materializes onto the protocol (hi first)
               (mkv <>>en 'r0 [['mov 'r2 <><>en] ['mov 'r0 <<>en]])
              (: ty <>>en
                 ap? (g 'argpos)
                 _ (? (stru? ty || arr? ty) (pin g 'argpos ()) ())
                 (? (stru? ty)                    ; a struct value is its ADDRESS (the sse2 rep)
                     (? (<en = 'loc)
                        (: v (mkv ty 'r0 [['lea 'r0 'r4 <>en]]) (? ap? v (escan g v)))
                        (mkv ty 'r0 [['la 'r0 <>en]]))
                    (arr? ty)                     ; arrays decay to their address
                     (? (<en = 'loc)
                        (: v (mkv ['ptr <>ty] 'r0 [['lea 'r0 'r4 <>en]]) (? ap? v (escan g v)))
                        (mkv ['ptr <>ty] 'r0 [['la 'r0 <>en]]))
                    (gvarr? ty)                   ; a VLA local: its slot HOLDS the block pointer
                     (? (<en = 'loc)
                        (mkv ['ptr (varel ty)] 'r0 [['ld 'r0 'r4 <>en]])
                        'bad)
                    (flo? ty)                     ; a float/double loads into f0 (widened if float)
                     (? (<en = 'loc)
                        (mkv 'double 'r0 (fldf ty 'f0 'r4 <>en))
                        (mkv 'double 'r0 ([['la 'r1 <>en]] + fldf ty 'f0 'r1 0)))
                    (pair? g ty)                    ; a pair local/global reads both words (the address temp
                     (? (<en = 'loc)                ;  dodges r1 -- it is the pair's own high half)
                        (mkv ty 'r0 (pld 'r4 <>en))
                        (mkv ty 'r0 ([['la 'r2 <>en]] + pld 'r2 0)))
                    (d128? ty)                      ; a wide local/global reads both words (r1 the
                     (? !(w128? g) (no128 g)        ;  address temp -- clear of the pair's own halves)
                        (<en = 'loc)
                         (mkv ty 'r0 [['ld 'r0 'r4 <>en] ['ld 'r2 'r4 (<>en + 8)]])
                        (mkv ty 'r0 [['la 'r1 <>en] '(ld r0 r1 0) '(ld r2 r1 8)]))
                    (? (<en = 'loc)
                       (: W (? wnt wnt 'r0)
                          (mkv ty W [[(ldop ty) W 'r4 <>en]]))
                       (mkv ty 'r0 [['la 'r1 <>en] [(ldop ty) 'r0 'r1 0]]))))))
       ('addr lv)
        (: r (clval g env lv)
           ap? (g 'argpos)
           _ (pin g 'argpos ())
           (? (!(bad? r) && bitf? (vty r)) 'bad   ; &bitfield is illegal C -- refuse
              !(bad? r)                        ; under & the lvalue's address ESCAPES: a real lea, so the gate sees it
              (: fs (? (vm? r) (vfs r + (: b (vmb r) o (vmo r)
                                           (b @ 'r4 [['lea 'r0 'r4 o]]
                                                'r0 (o @ 0 () [['add 'r0 'r0 o]])
                                                (o @ 0 [['mov 'r0 b]] [['add 'r0 b o]]))))
                        (unlean (vfs r)))
                 _ (? (vm? r) (rfree g (vmb r)) ())
                 v (mkv ['ptr (vty r)] 'r0 fs)
                 (? ap? v (escan g v)))
              ; &f on a function name: same address as the bare use (no lvalue).
              ; a VARIABLE of the same name shadows it -- a homed param's regv
              ; lands here as 'bad and must DEOPT, not read &libm-exp (vent!)
              (<lv = 'var && bad? (vent g env <>lv) && g 'fns <>lv)
               (mkv ['ptr (fnty g <>lv)] 'r0 [['la 'r0 (name <>lv)]])
              'bad))
       ('deref >< _)
        (: r (clval g env e)                  ; clval already walked the pointer
           (? (bad? r) 'bad (loadval g r wnt)))
       ('asn dst rhs)
        (: rv (? (<dst = 'var) (vent g env <>dst) 'bad)
           fd (sfd g env dst)
           (? (two? rv && <rv = 'regv)   ; store into a register-homed scalar (param or local)
               (: r (cgexpr g env rhs <>rv)   ; the rhs aims straight at the home
                  (? (bad? r) 'bad
                     (flo? (vty r)) (mkv <>>rv 'r0 (asintt g r <>>rv + ([['mov <>rv 'r0]] + cvt <>>rv <>rv)))
                     (id? (vreg r) <>rv) (mkv <>>rv <>rv ((vfs r) + cvt <>>rv <>rv))   ; landed in the home already (re-narrowed)
                     (: fs (vfs r + ([['mov <>rv (vreg r)]] + cvt <>>rv <>rv))   ; straight from the value's register
                        _ (rfree g (vreg r))
                        (mkv <>>rv <>rv fs))))
              (two? rv && <rv = 'regw)     ; store into a pair-homed d128: land on the protocol, mov both halves
               (: r (cgexpr g env rhs ())
                (bad? r ? 'bad
                 (mkv <>>rv 'r0 (as128 g r + [['mov <<>rv 'r0] ['mov <><>rv 'r2]]))))
              ; STORE-DIRECT: a plain variable target's address is STATIC (a frame offset or
              ; a label) -- the value stores from wherever it sits, no shuttle around it
              (two? rv && member? <rv '(loc glo) &&
                  (: ty9 <>>rv !(stru? ty9 || arr? ty9 || flo? ty9)))
               (: ty <>>rv
                  nm <>dst
                  r (cgexpr g env rhs ())
                  (? (bad? r) 'bad
                     (pair? g ty)                     ; a pair target stores both words (r2 the address
                      (mkv ty 'r0                    ;  temp -- r1 is the pair's own high half)
                       (asll g r + (<rv @ 'loc (pst 'r4 <>rv)
                                               ([['la 'r2 <>rv]] + pst 'r2 0))))
                     (d128? ty)                       ; a wide target stores both words (r1 the address temp)
                      (? !(w128? g) (no128 g)
                       (mkv ty 'r0
                        (as128 g r + (<rv @ 'loc [['st 'r4 <>rv 'r0] ['st 'r4 (<>rv + 8) 'r2]]
                                                 [['la 'r1 <>rv] '(st r1 0 r0) '(st r1 8 r2)]))))
                     ; ⚠ A DOUBLE SOURCE MUST TRUNCATE FIRST: this lane's guard rules out a
                     ; float TARGET but says nothing about the float SOURCE, and a double's
                     ; value lives in f0 with its reg field not consulted -- so the store-the-
                     ; register lanes below would put the raw IEEE BIT PATTERN in the slot,
                     ; and the narrow lane rides tor0, which moves rather than converts.
                     (flo? (vty r))
                      (mkv ty 'r0 (asintt g r ty + cvt ty 'r0
                                   + (<rv @ 'loc [[(stop ty) 'r4 <>rv 'r0]]
                                                 [['la 'r1 <>rv] [(stop ty) 'r1 0 'r0]])))
                     (two? (cvt ty 'r0))              ; a narrow target converts on the protocol
                      (: fs (tor0 g r + cvt ty 'r0 +
                             (<rv = 'loc ?
                              [[(stop ty) 'r4 <>rv 'r0]]
                              [['la 'r1 <>rv] [(stop ty) 'r1 0 'r0]]))
                         (mkv ty 'r0 fs))
                     (<rv = 'loc)
                      (mkv ty (vreg r) (vfs r + [['st 'r4 <>rv (vreg r)]]))
                     (mkv ty (vreg r) (vfs r + [['la 'r1 <>rv] ['st 'r1 0 (vreg r)]]))))
              ; FRAME-DIRECT (store-direct's element twin): the slot is static, so no address
              ; compute, no park, no shuttle -- even past a call on the right. the write
              ; residency rides unchanged: the rhs aims at a pool register, converts in
              ; place and re-pins; unhonored hands the register back (drop-only stands).
              (two? fd)
               (: off <fd ty <>fd
                  r (cgexpr g env rhs ())
                  (? (bad? r) 'bad
                     ; a register-riding value stores ITSELF (no r0 bridge; the
                     ; consumer owns the register, 2c's ownership rule)
                     (? (!(flo? (vty r)) && atom? (cvt ty 'r0) && vreg r != 'r0)
                        (mkv ty (vreg r) (vfs r + [[(stop ty) 'r4 off (vreg r)]]))
                        (mkv ty 'r0 (asintt g r ty + cvt ty 'r0 +
                                     [[(stop ty) 'r4 off 'r0]])))))
              (: lv0 (clval g env dst)
                 ; face-direct: the base register survives a write-free rhs, so the
                 ; store reads through the face -- no matv, no address park, no cell
                 fdd (vm? lv0 && vmb lv0 != 'r0 && !(wrany? rhs))
           (? (bad? lv0) 'bad
              (arr? (vty lv0)) 'bad                ; an array is not assignable
              (stru? (vty lv0))                    ; a whole-struct copy (dest r1, src r0)
               (: lv (matv g lv0)
                  rl (matv g (clval g env rhs))   ; an rvalue struct (a call's temp) is its address too
                  rv (? (bad? rl) (cgexpr g env rhs ()) rl)
                  (? (bad? rv) 'bad
                     ; a REAL into a complex lvalue converts: re the value, im zero
                     (gcplx? (vty lv) && !(stru? (vty rv)))
                      (: b (cxb (vty lv))
                         (mkv (vty lv) 'r0
                            (vfs lv + spush 'r0 +
                             (asflo g rv + spop 'r1 + fstf g b 'r1 0 'f0
                              + '((li r2 0))
                              + (b @ 'float '((st4 r1 4 r2)) '((st r1 8 r2)))))))
                     !(stru? (vty rv)) 'bad
                     (mkv (vty lv) 'r0 $
                        vfs lv + spush 'r0 + tor0 g rv + spop 'r1 +
                          ; a complex width flip converts componentwise; else bytes
                          (? (gcplx? (vty lv) && gcplx? (vty rv) &&
                              !(<>(vty lv) = <>(vty rv)))
                             (cxcopy g (vty rv))
                             (bcopy g 0 (tsz g (vty lv)) ())))))
              (flo? (vty lv0))                     ; a float/double store (value in f0; narrow if float)
               (? fdd
                  (: b (vmb lv0) o (vmo lv0)      ; the face IS the address: fstf reads through it
                     r (cgexpr g env rhs ())
                     (? (bad? r) (: _ (rfree g b) 'bad)
                        (: fs (vfs lv0 + asflo g r + fstf g (vty lv0) b o 'f0)
                           _ (rfree g b)
                           (mkv 'double 'r0 fs))))
                  (: lv (matv g lv0)
                     r (cgexpr g env rhs ())
                     (? (bad? r) 'bad
                        (mkv 'double 'r0 (vfs lv + spush 'r0 +
                         (asflo g r + spop 'r1 + fstf g (vty lv) 'r1 0 'f0))))))
              (bitf? (vty lv0))                   ; a bitfield store: unit addr in r1, read-modify-write, result (truncated) in r0
               (: lv (matv g lv0)
                  r (cgexpr g env rhs ())
                  (? (bad? r) 'bad
                     (: bt (vty lv) uty <>bt bitoff <>>bt width <>>>bt
                        usz (tsz g uty)
                        mask ((1 << width) - 1) mv (mask << bitoff) cmask ((0 - mv) - 1)
                        sgn !(usg? uty || id? uty 'bool) lo (64 - width)
                        (mkv 'long 'r0
                           (vfs lv + spush 'r0
                            + asintt g r uty
                            + (? (id? uty 'bool) (cvt uty 'r0) ())   ; normalize BEFORE the mask ((bool)4 is 1)
                            + spop 'r1
                            + [[(uldop usz) 'r2 'r1 0]              ; r2 = the unit
                                ['li 'r3 mask] '(and r0 r0 r3)       ; r0 = v & mask (the truncated value)
                                ['li 'r3 cmask] '(and r2 r2 r3)      ; clear the field's bits
                                '(mov r3 r0)]
                            + (? (bitoff > 0) [['shl 'r3 bitoff]] ())   ; r3 = (v & mask) << bitoff
                            + ['(or r2 r2 r3) [(stop uty) 'r1 0 'r2]]
                            + (? sgn [['shl 'r0 lo] ['sar 'r0 lo]] ()))))))
              (pair? g (vty lv0))                 ; a pair store through a computed address
               (: lv (matv g lv0)
                  r (cgexpr g env rhs ())
                  (? (bad? r) 'bad
                     (mkv (vty lv) 'r0
                      (vfs lv + spush 'r0 + asll g r + spop 'r2 + pst 'r2 0))))
              (d128? (vty lv0))                   ; a wide store through a computed address (r1)
               (? !(w128? g) (no128 g)
                  (: lv (matv g lv0)
                     r (cgexpr g env rhs ())
                     (? (bad? r) 'bad
                        (mkv (vty lv) 'r0 (vfs lv + spush 'r0 +
                         as128 g r + spop 'r1 + '((st r1 0 r0) (st r1 8 r2)))))))
              ; face-direct: the store reads through the face's own base -- no matv, no
              ; address park
              fdd
               (: b (vmb lv0) o (vmo lv0)
                  ty (vty lv0)
                  r (cgexpr g env rhs ())
                  (? (bad? r) (: _ (rfree g b) 'bad)
                     (? (!(flo? (vty r)) && !(two? (cvt ty 'r0)) &&
                         !(id? (vreg r) 'r0) && !(rpeq? g (vreg r) b))
                        (: fs (vfs lv0 + vfs r + [[(stop ty) b o (vreg r)]])
                           _ (rfree g b)
                           (mkv ty (vreg r) fs))
                        (: fs (vfs lv0 + asintt g r ty + cvt ty 'r0 +
                               [[(stop ty) b o 'r0]])
                           _ (rfree g b)
                           (mkv ty 'r0 fs)))))
              ; the ADDRESS parks in a pool register past a call-free rhs (the vpark
              ; discipline, extended to the store shuttle): no memory round-trip on the
              ; address. a call on the right keeps the shuttle -- it would clobber the park.
              (: lv (matv g lv0)
                 aR (? (callish? rhs) () (ralloc g))
                 r (cgexpr g env rhs ())
                 (? (bad? r) (: _ (? aR (rfree g aR) ()) 'bad)
                    (: ty (vty lv)
                       (? aR
                          ; a register-riding value stores ITSELF (no r0 bridge)
                          (? (!(flo? (vty r)) && !(two? (cvt ty 'r0)) &&
                              !(id? (vreg r) 'r0) && !(rpeq? g (vreg r) aR))
                             (: fs (vfs lv +
                                    [['mov aR 'r0]] +
                                    vfs r +
                                    [[(stop ty) aR 0 (vreg r)]])
                                _ (rfree g aR)
                                (mkv ty (vreg r) fs))
                             (: fs (vfs lv +
                                    [['mov aR 'r0]] +
                                    asintt g r ty +
                                    cvt ty 'r0 +
                                    [[(stop ty) aR 0 'r0]])
                                _ (rfree g aR)
                                (mkv ty 'r0 fs)))
                          (mkv ty 'r0
                           (vfs lv + spush 'r0 + asintt g r ty + spop 'r1 +
                            cvt ty 'r0 + [[(stop ty) 'r1 0 'r0]]))))))))))
       ('post dst step)
        (: rv (<dst = 'var ? (vent g env <>dst) 'bad)
           fd (sfd g env dst)
           (? (two? rv && <rv = 'regv)   ; p++ on a register-homed scalar: old in r0, home += step
               (: ty <>>rv stp (ptr? ty ? (tsz g <>ty) 1)
                  (mkv ty 'r0 ([['mov 'r0 <>rv]
                                ['add <>rv <>rv (stp * step)]] + cvt ty <>rv)))
              ; STORE-DIRECT (the asn twin): a frame scalar's slot is a STATIC offset --
              ; read/step/write with a direct frame ld+st. leaves no lean, so a loop
              ; counter's slot never reads as address-taken and the local can home
              (two? rv && <rv = 'loc &&
               (: t9 <>>rv !(stru? t9 || arr? t9 || flo? t9 || bitf? t9)))
               (: ty <>>rv off <>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                  (? (pair? g ty)
                     (mkv ty 'r0 (pld 'r4 off + ppost 'r4 off (* stp step)))
                     (d128? ty)                   ; wide ++/--: step in place, store, step BACK (the
                      (? !(w128? g) (no128 g)     ;  answer is the OLD pair, and the carry ops are in-place)
                         (mkv ty 'r0 [['ld 'r0 'r4 off] ['ld 'r2 'r4 (off + 8)]
                                       [(step < 0 ? 'dec128 'inc128)]
                                       ['st 'r4 off 'r0] ['st 'r4 (off + 8) 'r2]
                                       [(step < 0 ? 'inc128 'dec128)]]))
                     (mkv ty 'r0 ([[(ldop ty) 'r0 'r4 off]
                                    ['add 'r1 'r0 (stp * step)]]
                                  + (ty @ 'bool (cvt ty 'r1))   ; b++/b-- lands on 0/1
                                  + [[(stop ty) 'r4 off 'r1]]))))
              ; frame-direct element ++/-- (the store-direct twin; pair/d128 stay on afd's far side)
              (two? fd)
               (: ty <>fd off <fd stp (? (ptr? ty) (tsz g <>ty) 1)
                  (mkv ty 'r0 ([[(ldop ty) 'r0 'r4 off]
                                 ['add 'r1 'r0 (* stp step)]]
                               + (ty @ 'bool (cvt ty 'r1))
                               + [[(stop ty) 'r4 off 'r1]])))
              (: lv0 (clval g env dst)
           (? (bad? lv0) 'bad
              (: ty (vty lv0)
                 stp (? (ptr? ty) (tsz g <>ty) 1)
                 (? (arr? ty) 'bad
                    (bitf? ty) 'bad             ; bitfield ++/-- -- refuse (rare; use `f = f + 1`)
                    (d128? ty) (nolane g "++/-- is not carried on a wide value (use `x = x + 1`)")
                    (pair? g ty)
                     (: lv (matv g lv0)
                        (mkv ty 'r0 (vfs lv +
                               '((mov r12 r0)) + (pld 'r12 0) + (ppost 'r12 0 (* stp step)))))
                    ; face-direct: no step rhs runs, so the base survives by construction
                    (vm? lv0 && !(id? (vmb lv0) 'r0))
                     (: b (vmb lv0) o (vmo lv0)
                        fs (vfs lv0 +
                            ([[(ldop ty) 'r0 b o]
                               ['add 'r1 'r0 (stp * step)]]
                             + (ty @ 'bool (cvt ty 'r1))   ; b++/b-- lands on 0/1
                             + [[(stop ty) b o 'r1]]))
                        _ (rfree g b)
                        (mkv ty 'r0 fs))
                    (: lv (matv g lv0)
                       (mkv ty 'r0 (vfs lv +
                              (['(mov r2 r0)
                                 [(ldop ty) 'r0 'r2 0]
                                 ['add 'r1 'r0 (stp * step)]]
                               + (ty @ 'bool (cvt ty 'r1))   ; b++/b-- lands on 0/1
                               + [[(stop ty) 'r2 0 'r1]]))))))))))
       ; ⚠ `E1 op= E2` EVALUATES E1 ONCE. the plain desugar duplicates it --
       ; (asn lv (bin op lv rhs)) -- which is exact only for an lvalue whose
       ; evaluation leaves no trace, so parse routes every other one here (its
       ; calm?). the address is computed once and MOORED in a frame temp; the
       ; read, the op and the store back then run over ('moor off ty), a calm
       ; lvalue standing for it -- so pointer scaling, the narrowing store,
       ; doubles, pairs, wides and bitfields each ride their own lane unchanged.
       ('rmw op dst rhs)
        (: lv (matv g (clval g env dst))
           (? (bad? lv) 'bad
              (: ty (vty lv)
                 (? (arr? ty || stru? ty) 'bad
                    (: off (ntmp g 'long)
                       mo ['moor off ty]
                       r (cgexpr g env ['asn mo ['bin op mo rhs]] ())
                       (? (bad? r) 'bad
                          (mkv (vty r) (vreg r)
                               (vfs lv + [['st 'r4 off 'r0]] + vfs r))))))))
       ('cast ty x)
        (: tgt ty
           r (cgexpr g env x (? (!(flo? tgt) && !(two? (cvt tgt 'r0))) wnt ()))
           (? (bad? r) 'bad
              (flo? tgt && d128? (vty r))
               ; (double) of a WIDE value: only a CONSTANT expression, folded exactly (love.c's
               ; (double)limb_base = 2^64; cfold is bignum-native, love's + converts a big
               ; correctly rounded; the discarded emission is pure -- the expr folded). there
               ; is no runtime wide->double lane -- an unfoldable one refuses.
               (: v (cfold x)
                  (? (cint? v) (mkv 'double 'r0 [['li 'r0 (fbits (0.0 + v))] '(movqxr f0 r0)])
                     'bad))
              (d128? tgt && flo? (vty r))                                 ; double -> wide: not carried
               (nolane g "a double is not converted to a wide integer")
              (flo? tgt && !(flo? (vty r))) (mkv 'double 'r0 (asflo g r))  ; int -> double
              (tgt = 'bool && flo? (vty r))       ; (bool)d is d != 0.0, never a truncation
               ; ((bool)0.5 is 1) -- the un-'not lane's shape with the NaN-honest !=
               (: z1 (? (t32? g) '((li r0 0) (li r1 0) (movqxr f1 r0))
                        '((li r0 0) (movqxr f1 r0)))
                  (mkv 'bool 'r0 (vfs r + z1 + '((ucomisd f0 f1)) + fsetn)))
              (!(flo? tgt) && flo? (vty r))                              ; double -> int, then narrow
               (? (pair? g tgt)                                               ; double -> llong: the pattern peel
                   (mkv tgt 'r0 (vfs r + cvtdll g (usg? tgt)))
                  (t32? g && usg? tgt)                                    ; t32 unsigned: the U-converter
                   (mkv tgt 'r0 (vfs r + '((cvttsd2ui r0 f0)) + cvt tgt 'r0))
                  (mkv tgt 'r0 (asintt g r tgt + cvt tgt 'r0)))
              ; (float) of a double ROUNDS. the register model keeps every float as a
              ; double and narrows only at a STORE (fstf), so a cast that just passed the
              ; value through lost the rounding entirely -- `(float)0.1 == 0.1` read true,
              ; and so did `0.1f == 0.1` once the f suffix started typing as float.
              (tgt = 'float && flo? (vty r))
               (mkv 'double 'r0 (vfs r + '((cvtsd2ss f0 f0) (cvtss2sd f0 f0))))
              (flo? tgt) (mkv 'double 'r0 (vfs r))                            ; double -> double
              (pair? g tgt && !(llt? (vty r)))                           ; widen to the pair (source signedness)
               (mkv tgt 'r0 (asll g r))
              (d128? tgt && !(d128? (vty r)))                            ; widen to the wide pair (source signedness)
               (w128? g ? (mkv tgt 'r0 (as128 g r)) (no128 g))
              (two? (cvt tgt 'r0)) (mkv tgt 'r0 (tor0 g r + cvt tgt 'r0))   ; int -> narrower int
              (mkv tgt (vreg r) (vfs r))))                                    ; a full-word int cast KEEPS the register (a wide one narrows to its lo)
       ('un op x)
        (: r (cgexpr g env x ())
           (? (bad? r) 'bad
              (gcplx? (vty r) && id? op 'neg)      ; -z: both components flip through the complex lane
               (: pa (cxpark g r)
                  (? (bad? pa) 'bad
                     (cxout g (cxb (vty r) @ 'float (vty r) cxdty)
                        ; each component subtracts from MINUS zero, for the reason the scalar
                        ; lane below spells out. r0 is free here -- cxout writes it last.
                        (<>pa + [['ldsd 'f0 'r4 <pa] ['ldsd 'f1 'r4 (<pa + 8)]
                                  ['li 'r0 0x8000000000000000] '(movqxr f4 r0)
                                  '(subsd f4 f0) '(movsd f0 f4)
                                  ['li 'r0 0x8000000000000000] '(movqxr f4 r0)
                                  '(subsd f4 f1) '(movsd f1 f4)]))))
              (flo? (vty r))                        ; unary on a double
               (: z1 (t32? g ? '((li r0 0) (li r1 0) (movqxr f1 r0))             ; 0.0 (the t32 pair)
                               '((li r0 0) (movqxr f1 r0)))
                  ; ⚠ NEGATION SUBTRACTS FROM MINUS ZERO, NOT FROM ZERO: `0.0 - d` is right
                  ; for every input but one -- 0.0 - 0.0 is +0.0, so -0.0 could never be
                  ; produced. -0.0 - x IS -x everywhere, so the CONSTANT carries it: same op
                  ; count, same registers, and no xorpd (d==s only on riscv).
                  n0 (? (t32? g) ['(li r0 0) ['li 'r1 0x80000000] '(movqxr f1 r0)]
                        [['li 'r0 0x8000000000000000] '(movqxr f1 r0)])
                  (op @ 'neg                 ; -d = (-0.0) - d
                         (mkv 'double 'r0 (vfs r + (n0 + '((subsd f1 f0) (movsd f0 f1)))))
                        'not                 ; !d = (d == 0.0), NaN-honest: !NaN is false -- fsete
                         (mkv 'long 'r0 (vfs r + (z1 + ('((ucomisd f0 f1)) + fsete))))
                        'bad))                           ; ~ on a double is invalid
              (pair? g (vty r))                     ; unary on the pair
               (op @ 'neg
                      (mkv (vty r) 'r0 (tor0 g r + '((li r2 0) (li r3 0)
                                                     (subs r0 r2 r0) (sbc r1 r3 r1))))
                     'bnot
                      (mkv (vty r) 'r0 (tor0 g r + '((not r0) (not r1))))
                     (mkv 'long 'r0 (tor0 g r + [['ors (v6m? g ? 'r0 'r12) 'r0 'r1] '(set eq r0)])))
              (d128? (vty r))                       ; unary on the wide pair
               (? !(w128? g) (no128 g)
                  (op = 'neg) (mkv (vty r) 'r0 (tor0 g r + '((n128))))
                  (op = 'bnot) (mkv (vty r) 'r0 (tor0 g r + '((not r0) (not r2))))
                  (mkv 'long 'r0 (tor0 g r + '((or r0 r0 r2) (cmp r0 0)) + '((set eq r0)))))
              ; -1ULL stays unsigned; a SUB-64 unsigned re-narrows after neg/~ (the model
              ; keeps unsigned intermediates zero-extended: -1u wraps at 32 bits, not 64)
              (: uty (member? op '(neg bnot) && usg? (vty r))
                 renar (uty ? (vty r @ 'uchar '((zx1 r0))
                                       'ushort '((zx2 r0))
                                       'uint '((zx4 r0))))
                 (mkv (? uty (vty r) 'long) 'r0
                    (tor0 g r +
                     (op @ 'neg '((neg r0))
                           'bnot '((not r0))
                           '((cmp r0 0) (set eq r0))) +
                     renar)))))
       ('comma ea eb)
        (: a (cgexpr g env ea ())
           b (cgexpr g env eb wnt)
           (? (bad? a || bad? b) 'bad
              (mkv (vty b) (vreg b) (vdrop g a + vfs b))))
       ('land >< _)                                ; as a VALUE: cbranch the short-circuit, then settle the bit
        (: lf (nlab g ".f") ld (nlab g ".d")
           f (cbranch g env e 'eq lf)
           (? (bad? f) 'bad
              (mkv 'long 'r0 (f + ['(li r0 1) ['jmp ld]
                                       ['label lf] '(li r0 0) ['label ld]]))))
       ('lor >< _)
        (: lt (nlab g ".t") ld (nlab g ".d")
           f (cbranch g env e 'ne lt)
           (? (bad? f) 'bad
              (mkv 'long 'r0 (f + ['(li r0 0) ['jmp ld]
                                       ['label lt] '(li r0 1) ['label ld]]))))
       ('cond cnd ea eb)
        (: le (nlab g ".e") ld (nlab g ".d")
           c (cbranch g env cnd 'eq le)
           t (cgexpr g env ea ())
           el (cgexpr g env eb ())
           (? (bad? c || bad? t || bad? el) 'bad
              ; ⚠ C's conditional CONVERTS its arms: one flo arm makes the whole result flo,
              ; so the INTEGER arm rides asflo at its edge -- typing by the then-arm alone
              ; leaves an int in r0 where the reader takes f0
              (: fl (flo? (vty t) || flo? (vty el))
                 pl (pair? g (vty t) || pair? g (vty el))   ; one pair arm pairs the other too
                 tf (? fl (asflo g t) pl (asll g t) (tor0 g t))
                 ef (? fl (asflo g el) pl (asll g el) (tor0 g el))
                 rt (? (flo? (vty t)) (vty t) fl (vty el)
                       pl (? (llt? (vty t)) (vty t) (vty el)) (vty t))
                 (mkv rt 'r0
                    (c + tf + [['jmp ld] ['label le]] + ef + [['label ld]])))))
       ; -- the varargs builtins, per target (crew/moon/include/stdarg.h). SysV: the 24-byte
       ; { int gp_offset; int fp_offset; void *overflow_arg_area; void *reg_save_area; } --
       ; va_arg fetches from the save area until the offset passes the reg limit, then from
       ; the overflow area. AAPCS64: the 32-byte { void *__stack; void *__gr_top; void
       ; *__vr_top; int __gr_offs; int __vr_offs; } -- ⚠ the offs fields are NEGATIVE, counted
       ; back from each area's top toward 0; at >= 0 the arg is on the caller stack at
       ; __stack. va_start seeds either past the named params; va_end nops. --
       ('vastart ap)
        ; cgexpr (not clval) gives the struct pointer uniformly: a va_list LOCAL (array) decays
        ; to its address, a va_list PARAM loads its value -- both land the address in r0.
        (: lv (cgexpr g env ap ())                 ; the va_list struct pointer in r0
           rsa (peep g 'rsa 0) ngp (peep g 'vagp 0) nxmm (peep g 'vaxmm 0)
           nov (peep g 'vaov 0)                  ; named params that overflowed (slots)
           (? (bad? lv) 'bad
              (: _ (pin g 'fesc 1)                                       ; the va_list HOLDS frame addresses: no sibcalls here
                 (mkv 'long 'r0 (tor0 g lv +
                   (? (t32? g)   ; AAPCS32: __ap = the first anonymous word, past the push cells + named regs + named overflow
                      [['lea 'r1 'r4 (ovb g + 4 * ngp + nov)] '(st r0 0 r1)]
                      (rv? g)   ; LP64: __ap = the first anonymous 8-byte slot, past the push cells + named regs + named overflow
                      [['lea 'r1 'r4 (ovb g + 8 * ngp + 8 * nov)] '(st r0 0 r1)]
                      (a64? g)
                      [['lea 'r1 'r4 (ovb g + 8 * nov)] '(st r0 0 r1)   ; __stack = fp+32 + named overflow
                         ['lea 'r1 'r4 (rsa + 64)]  '(st r0 8 r1)            ; __gr_top (past x0..x7)
                         ['lea 'r1 'r4 (rsa + 192)] '(st r0 16 r1)           ; __vr_top (past q0..q7)
                         ['li 'r1 -(8 * (8 - ngp))]  '(st4 r0 24 r1)    ; __gr_offs = -(unnamed gp)*8
                         ['li 'r1 -(16 * (8 - nxmm))] '(st4 r0 28 r1)]  ; __vr_offs = -(unnamed vr)*16
                      [['li 'r1 (8 * ngp)]         '(st4 r0 0 r1)   ; gp_offset = 8*named-gp
                         ['li 'r1 (48 + 16 * nxmm)] '(st4 r0 4 r1)  ; fp_offset = 48 + 16*named-xmm
                         '(lea r1 r4 16)  '(st r0 8 r1)             ; overflow_arg_area = rbp+16
                         ['lea 'r1 'r4 rsa] '(st r0 16 r1)]))))))      ; reg_save_area = rbp+rsa
       ('vaarg ap ty)
        (: lv (cgexpr g env ap ())          ; the va_list struct pointer in r0
           lov (nlab g ".vo") ld (nlab g ".vd")
           ; r1 = the relevant offset; r2 = the fetch address. SysV: an offset below the limit
           ; reads reg_save_area + offset and advances, else overflow (which advances 8).
           ; AAPCS64: a NEGATIVE offs reads top + offs (ld4 sign-extends) and advances toward
           ; 0; at >= 0 the arg rides __stack. AAPCS32: __ap walks one word per anonymous arg.
           walk (? (t32? g)
                   '((ld r2 r0 0) (mov r3 r2) (add r3 r3 4) (st r0 0 r3))
                   (rv? g)   ; LP64: __ap just walks, one 8-byte slot per anonymous arg (doubles ride gp slots)
                   '((ld r2 r0 0) (mov r3 r2) (add r3 r3 8) (st r0 0 r3))
                   (a64? g)
                   (: foff (? (flo? ty) 28 24)               ; __vr_offs / __gr_offs
                      toff (? (flo? ty) 16 8)                ; __vr_top / __gr_top
                      step (? (flo? ty) 16 8)                ; a vr slot is one q (16)
                      [['ld4 'r1 'r0 foff] '(cmp r1 0) ['br 'ge lov]
                       ['ld 'r2 'r0 toff] '(add r2 r2 r1)           ; p = top + offs
                       ['add 'r1 'r1 step] ['st4 'r0 foff 'r1]          ; advance the offs
                       ['jmp ld] ['label lov]
                       '(ld r2 r0 0) '(mov r3 r2) '(add r3 r3 8) '(st r0 0 r3)   ; p = __stack; __stack += 8
                       ['label ld]])
                   (: foff (? (flo? ty) 4 0)                 ; fp_offset (4) vs gp_offset (0)
                      lim (? (flo? ty) 176 48)               ; the register-area limit
                      step (? (flo? ty) 16 8)                ; the offset stride
                      [['ld4 'r1 'r0 foff] ['cmp 'r1 lim] ['br 'ge lov]
                       '(ld r2 r0 16) '(add r2 r2 r1)            ; p = reg_save_area + offset
                       ['add 'r1 'r1 step] ['st4 'r0 foff 'r1]         ; advance the offset
                       ['jmp ld] ['label lov]
                       '(ld r2 r0 8) '(mov r3 r2) '(add r3 r3 8) '(st r0 8 r3)   ; p = overflow; overflow += 8
                       ['label ld]]))
           (? (bad? lv) 'bad
              (t32? g && (flo? ty || llt? ty || stru? ty))        ; t32 va_arg reads WORDS only (love.c's shape)
               (nolane g ("no lane for a double/pair/struct va_arg" + ontgt g))
              (d128? ty) (nolane g "a wide va_arg is not carried")
              (flo? ty) (mkv 'double 'r0 (tor0 g lv + walk + fldf ty 'f0 'r2 0))
              (mkv ty 'r0 (tor0 g lv + walk + [[(ldop ty) 'r0 'r2 0]]))))
       ('vaend >< _) (mkv 'long 'r0 ())
       ('call fh as)
        ; the SysV ABI: integer/pointer args ride r6 r5 r2 r1 r7 r8 (<=6), double args f0..f7
        ; (<=8), SEPARATE counters, placed by the callee's PARAM type (a direct call reads the
        ; sig table and converts to it) else by the arg's own. the result rides f0 for a
        ; double-returning callee, r0 otherwise. args evaluate left to right, each pushed to
        ; the machine stack, then popped reversed into place.
        (: head fh args as
           bi (? (<head = 'var) <>head ())
           bv (cgbuiltin g env bi args)
           (? !(id? bv 'no) bv
              (: m9 (g 'must9)
                 _ (m9 ? (pin g 'must9 ()))
                 inlf (? m9 ()                                  ; a musttail call is OWED as a call: never spliced
                         (2 < peep g 'inlining 0) ()   ; splices NEST to depth 3, cbinl's cap
                         !(<head = 'var) ()
                         !(bad? (vent g env <>head)) ()   ; a local/global shadows the name
                         (g 'inl <>head))
                 ir9 (? (two? inlf && #args = #<>>inlf)
                        (cginl g env inlf args) 'no)              ; 'no: splice declined (fesc or shape) -> the real call
                 (? !(id? ir9 'no) ir9
              (: direct (<head = 'var && bad? (vent g env <>head) &&  ; not a local/global variable...
                                         g 'fns <>head)     ; ...and a KNOWN function (typos refuse)
                 sig (direct ? (g 'sigs <>head))
                 ptys (two? sig ? <>sig)
                 rt (? (two? sig) <sig 'long)
                 variadic (two? sig ? (nthr sig 2))
                 ; the ARITY CHECK: a sig'd direct call hands its callee the arguments the
                 ; prototype asked for (the named ones a FLOOR when variadic), else a short
                 ; call reads whatever the argument register held.
                 ; ⚠ AN EMPTY PARAMETER LIST IS EXEMPT AND MUST STAY SO: `int f()` and
                 ; `int f(void)` both park an empty ptys and the sigs row cannot tell them
                 ; apart -- refusing the first would refuse musl's headers.
                 nps #ptys nas #args
                 off (0 < nps && (variadic ? (nas < nps) (nas != nps)))
                 _ (? off (pin g 'arity
                               ("call to '" + <>head + "' wants "
                                + show nps + (? variadic " or more" "")
                                + (? (nps = 1) " argument" " arguments")
                                + ", given " + show nas)) 0)
                 ; every call rides the SysV registers (call-fixed); a variadic one
                 ; additionally sets al = #xmm args for its callee's register save area.
                 cr (? off 'bad (call-fixed g env head args direct ptys rt variadic))
                 (? (bad? cr) 'bad
                    !(stru? (vty cr)) cr
                    (sretm? g (vty cr)) cr   ; a MEMORY return: the sret temp's address in r0 IS the struct value
                    (sse2? g (vty cr))              ; a two-double struct result: park the xmm pair in a fresh slot, answer its address
                     (: toff (nslot g 16)
                        (mkv (vty cr) 'r0 (vfs cr +
                                           fstf g 'double 'r4 toff (xmmreg 0) +
                                           fstf g 'double 'r4 (toff + 8) (xmmreg 1) +
                                           [['lean 'r0 'r4 toff]])))
                    (t32? g &&                    ; t32: past the d-pair, only the 8B sse blob (d0) and the <=4B int one (r0) return
                     !((id? (aone g (vty cr)) 'sse && tsz g (vty cr) = 8) ||
                       (id? (aone g (vty cr)) 'int && tsz g (vty cr) <= 4))) 'bad
                    (ipair? g (vty cr))             ; a two-int struct result: park rax:rdx (arm64 x0:x1) the same way
                     (: toff (nslot g 16)
                        (mkv (vty cr) 'r0 (vfs cr +
                                     [['st 'r4 toff 'r0] ['st 'r4 (toff + 8) (ip2 g)]
                                        ['lean 'r0 'r4 toff]])))
                    (id? (aone g (vty cr)) 'sse)    ; a small SSE aggregate result: park xmm0
                     (: toff (nslot g 16)
                        (mkv (vty cr) 'r0 (vfs cr +
                                           (fstf g 'double 'r4 toff (xmmreg 0) +
                                            [['lean 'r0 'r4 toff]]))))
                    (id? (aone g (vty cr)) 'int)    ; a small INT aggregate result: park rax
                     (: toff (nslot g 16)
                        (mkv (vty cr) 'r0 (vfs cr + [['st 'r4 toff 'r0] ['lean 'r0 'r4 toff]])))
                    'bad))))))
       ('clit >< _)                              ; a compound literal as a value: fill the temp, load it
        (: lv (clval g env e)
           (? (bad? lv) 'bad (loadval g lv wnt)))
       ('bin op a b)
        (? (id? op '|)
            (: r9 (cgror g env a b wnt)
               (? (id? r9 'no) (cgbin g env e wnt) r9))
           (cgbin g env e wnt))
       'bad)
   ; a TARGETABLE gp register argument: pure and single-emission, reading only an immediate, the
   ; frame, a home register, or a label -- so it skips the push/pop cell and lands straight in its
   ; argument register, emitted between the home spills and the pops. deferring a pure read past
   ; its siblings is the same unspecified-order license the overflow-first emission rides.
   ; ⚠ a homed-param read is the one cross-entry hazard (another targeted arg may write its home
   ; first), so it carries BOTH emissions and the caller picks by conflict.
   ; -> ((ez i) forms) | ((ezm i rH) movforms ldforms) | 'no
   (ezarg g env e pty i)
    (: rT (nthr (argr g) i)
       (fin ty fs) (: aty (? pty pty ty)
                      (? (flo? aty || stru? aty || llt? aty || d128? aty) 'no [['ez i] fs]))   ; a pair arg keeps the cell path
       kc (knum g e)
       (? (two? kc) (? (t32? g && !(-2147483648 <= <kc && <kc <= 4294967295)) 'no
                       (fin 'long [['li rT <kc]]))
          (<e = 'str)
           (: lbl (nlab g ".S")
              _ (pin g 'strs ([lbl <>e] >< g 'strs))
              (fin '(ptr char) [['la rT lbl]]))
          (<e = 'addr)                          ; &var only: lea/la, argpos-equivalent (no escan)
           (? !(<<>e = 'var) 'no
              (: en (vent g env <><>e)
                 (? (bad? en) 'no
                    (<en = 'loc) (fin ['ptr <>>en] [['lea rT 'r4 <>en]])
                    (<en = 'glo) (fin ['ptr <>>en] [['la rT <>en]])
                    'no)))
          !(<e = 'var) 'no
          (: en (vent g env <>e)
             (? (bad? en) 'no
                (: ty <>>en
                   (? (<en = 'regv)             ; a param in a register: mov from it, or ld its spill slot
                       (? (flo? (? pty pty ty) || stru? (? pty pty ty)) 'no
                          ; a cs SEAT: staging never writes a callee-saved register and no
                          ; sibling's call clobbers it -- targetable unconditionally
                          (member? <>en (cspool g)) (fin ty [['mov rT <>en]])
                          (: sl (assoc <>en (peep g 'homes ()))
                             (? (two? sl)
                                 [['ezm i <>en]
                                   [['mov rT <>en]]
                                   [['ld rT 'r4 <>sl]]]
                                ; THE RIDE has no spill slot to fall back on, so it is targeted
                                ; only where nothing can move the value first: every sibling a
                                ; pure leaf (ezride -- no evaluation runs before the emission)
                                ; and the source outside the argument file (no sibling's own
                                ; targeted emission lands on it). that is the VM's whole tail-
                                ; forward fleet -- pool-homed params straight into r0..r3.
                                (peep g 'ezride () && !(member? <>en (argr g)))
                                 (fin ty [['mov rT <>en]])
                                'no)))
                      (arr? ty)                      ; array decay: its address
                       (? (<en = 'loc) (fin ['ptr <>ty] [['lea rT 'r4 <>en]])
                          (fin ['ptr <>ty] [['la rT <>en]]))
                      (stru? ty || flo? ty) 'no
                      (<en = 'loc)
                       (fin ty [[(ldop ty) rT 'r4 <>en]])
                      (fin ty [['la rT <>en] [(ldop ty) rT rT 0]])))))))   ; a global: la then a self-based load
   ; the SysV register call, stack overflow included: args 1..6 gp / 1..8 xmm ride registers, the
   ; REST the caller stack (arg7 shallowest). evaluation order is free in C, so the overflow group
   ; emits FIRST, then the register group, both right-to-left -- the register pops run forward and
   ; the overflow block is left at [sp] for the callee, which the caller cleans after the call (an
   ; odd count takes an 8-byte pad UNDER it). va? marks a VARIADIC callee: `al` then carries the
   ; number of xmm arguments, which its prologue reads to decide whether to spill them.
   ; ⚠ params named uniquely so no lambda parameter shadows a sibling :-local through boxfix.
   (call-fixed g env hd ags dir pts ret va?)
    (: (kls as i gpc xmmc ovc acc)                       ; classify + evaluate, L->R -> (((class idx) pushforms)..) | 'bad
        (? !(two? as) (rev acc)
           ; a targetable leaf skips the cell: classified 'ez/'ezm, emitted
           ; straight into its argument register after the spills.
           (: ez (? (gpc < ngp g) (ezarg g env <as (nthty pts i) gpc) 'no)
              (? !(id? ez 'no) (kls >as (i + 1) (gpc + 1) xmmc ovc (ez >< acc))
           ; a DIRECT &x / array / struct-var argument is presumed dead once the callee returns
           ; (the &Hp / &_res idiom): argpos exempts its lea from the fesc pin, one-shot. the
           ; sibs WINDOW rule still keeps the address out of a tail call's own staging.
           ; the destination die reaches the arg seats: the register group emits right-to-left,
           ; so the FIRST cell arg evaluates last -- nothing after it writes its seat (spills
           ; store, ez emissions own other seats, pops fill other registers). aim it there; an
           ; unhonored hint keeps the cell. dir only (an indirect call's head evaluates after
           ; the staging), and a HOME seat stays on the cell: it spills after this evaluation,
           ; and an ezm may read it in between.
           (: aim (: (psh9 pl) (? !(two? pl) ()
                                  (member? <<<pl '(g g2 g4 x x2 p2 ezd) || psh9 >pl))
                     rT (nthr (argr g) gpc)
                     (? (dir && gpc < ngp g && !(psh9 acc) &&
                         !(two? (assoc rT (peep g 'homes ())))) rT ()))
              a (: _ (? (member? <<as '(addr var)) (pin g 'argpos 1) ())
                   (cgexpr g env <as aim))
              _ (pin g 'argpos ())
              (? (bad? a) 'bad
                 (: pty (nthty pts i)
                    aty (? pty pty (vty a))                ; classify by the param type if known, else the arg's
                    wd (flo? aty)
                    ; t32 composites: an HFA rides the d-regs (the sse2 pair lane below = AAPCS32-VFP
                    ; d-pairs; an 8B one-sse blob = the packed S-pair, bit-identical); a <=4B int one
                    ; rides r0's word. mixed/int-pair 8..16B stays deferred -- refuse LOUD.
                    (? (t32? g && va? && stru? aty)        ; a t32 VARIADIC call rides the base PCS: no d-reg composites
                        (nolane g ("no lane for a by-value struct in a variadic call" + ontgt g))
                       (v6m? g && sse2? g aty)       ; v6-M: a POSITION-0 16B composite rides r0-r3 whole (the
                        (? (gpc = 0)                     ;  base-ABI words); any later start would SPLIT -- deferred
                           (kls >as (i + 1) 4 xmmc ovc
                                (link ['(g4 0)
                                         (tor0 g a +
                                          '((sub sp sp 16) (ld r1 r0 12) (st sp 12 r1) (ld r1 r0 8) (st sp 8 r1)
                                            (ld r1 r0 4) (st sp 4 r1) (ld r1 r0 0) (st sp 0 r1)))]
                                      acc))
                           'bad)
                       (t32? g && stru? aty &&
                           !(sse2? g aty ||
                             (id? (aone g aty) 'sse && tsz g aty = 8) ||
                             (id? (aone g aty) 'int && tsz g aty <= 4))) 'bad
                       (sse2? g aty)                       ; a two-double struct arg: push its pair (re on top), an xmm PAIR target
                        (? (6 < xmmc) 'bad
                           (kls >as (i + 1) gpc (xmmc + 2) ovc
                                (link [['x2 xmmc]
                                         (tor0 g a +
                                          (? (t32? g)     ; t32: a 16-byte cell, four word copies
                                              '((sub sp sp 16) (ld r1 r0 12) (st sp 12 r1) (ld r1 r0 8) (st sp 8 r1)
                                                (ld r1 r0 4) (st sp 4 r1) (ld r1 r0 0) (st sp 0 r1))
                                             (a8? g)      ; arm64/riscv push is a 16-byte cell of its own: lay the pair by hand
                                              '((sub sp sp 16) (ld r1 r0 8) (st sp 8 r1) (ld r1 r0 0) (st sp 0 r1))
                                             '((ld r1 r0 8) (push r1) (ld r1 r0 0) (push r1))))]
                                      acc)))
                       (stru? aty && two? (aeb2 g aty))       ; a 9..16B struct arg (not sse:sse): eb0/eb1 each into a gp or xmm reg, mirrors the param side's file rule
                        (: cls (aeb2 g aty) c0 <cls c1 <>cls
                           hfa (id? c0 'sse && id? c1 'sse)
                           f0 (? (a64? g) (? hfa 'x 'g) (? (id? c0 'sse) 'x 'g))
                           f1 (? (a64? g) (? hfa 'x 'g) (? (id? c1 'sse) 'x 'g))
                           gpn ((? (id? f0 'g) 1 0) + (? (id? f1 'g) 1 0))
                           xmn ((? (id? f0 'x) 1 0) + (? (id? f1 'x) 1 0))
                           ; register-exhausted (5+ prior int args): SysV sends the WHOLE
                           ; aggregate to memory -- both eightbytes into the overflow block,
                           ; where the param side already binds it. x64 only: AAPCS64 closes
                           ; the gp file behind a stack composite and riscv SPLITS one across
                           ; the seam, two different rules -- they keep refusing LOUD.
                           (? (ngp g < gpc + gpn || 8 < xmmc + xmn)
                               (? (arm? g) 'bad             ; arm? is every non-x64 seat here
                                  (kls >as (i + 1) gpc xmmc (ovc + 2)
                                       (link [['ov ovc 2]
                                                (tor0 g a +
                                                 '((ld r1 r0 8) (push r1) (ld r1 r0 0) (push r1)))]
                                             acc)))
                              (: i0 (? (id? f0 'g) gpc xmmc)
                                 gpc1 (? (id? f0 'g) (gpc + 1) gpc)
                                 xmc1 (? (id? f0 'x) (xmmc + 1) xmmc)
                                 i1 (? (id? f1 'g) gpc1 xmc1)
                                 (kls >as (i + 1) (gpc + gpn) (xmmc + xmn) ovc
                                      (link [['p2 f0 i0 f1 i1]
                                               (tor0 g a +
                                                (? (a8? g)
                                                 '((sub sp sp 16) (ld r1 r0 8) (st sp 8 r1) (ld r1 r0 0) (st sp 0 r1))
                                                 '((ld r1 r0 8) (push r1) (ld r1 r0 0) (push r1))))]
                                            acc)))))
                       ; a MEMORY-class by-value struct arg: its eightbytes fill N overflow
                       ; slots. ⚠ x64 pushes DEEPEST first, so the struct's HIGHEST eightbyte
                       ; goes first -- offset 0 lands at the block's low address.
                       (amem? g aty)
                        (? (< 8 (asalign g aty))              ; over-aligned MEMORY arg: deferred
                            (nolane g "a by-value struct aligned past 8 is not carried as an argument")
                           (: n (aslots g aty)
                              (mv j acc9) (? (j < 0) acc9
                                             (mv (j - 1) (acc9 + [['ld 'r1 'r0 (* 8 j)] '(push r1)])))
                              (kls >as (i + 1) gpc xmmc (ovc + n)
                                   ([['ov ovc n] (tor0 g a + mv (n - 1) ())] >< acc))))
                       (stru? aty && !(aone g aty))             ; a MIXED 16B by-value struct stays ahead
                        (nolane g ("no lane for passing this " + show (tsz g aty) + "-byte struct by value" + ontgt g))
                       (d128? aty)                               ; a wide arg: not carried (love.c passes limbs, never dlimbs)
                        (nolane g "a wide value is not carried as an argument")
                       (pair? g aty)                             ; a pair arg: an even-odd register pair (AAPCS32
                        (: gpe (& (gpc + 1) -2)                  ;  rounds up), else the stack 8-aligned -- and gp
                           (? ((gpe + 2) <= ngp g)               ;  assignment closes behind a stack arg (C.13)
                               (kls >as (i + 1) (gpe + 2) xmmc ovc
                                    ([['g2 gpe] (asll g a + spush2)] >< acc))
                               (: ov8 (& (ovc + 7) -8)
                                  (kls >as (i + 1) 4 xmmc (ov8 + 8)
                                       (link [['ov ov8 8]
                                                (asll g a + [['st 'sp ov8 'r0] ['st 'sp (ov8 + 4) 'r1]])]
                                             acc)))))
                       ; the delivered seat: aimed and honored by the arg's OWN emission --
                       ; a zero-form answer (a riding home) predates the staging
                       ; and an earlier-emitted callish sibling would clobber it: cell.
                       (aim && rpeq? g (vreg a) aim && two? (vfs a) &&
                           !(flo? (vty a) || flo? aty || stru? aty))
                        (kls >as (i + 1) (gpc + 1) xmmc ovc ([['ezd gpc] (vfs a)] >< acc))
                       ; a one-eightbyte aggregate rides the scalar path: its
                       ; 8 content bytes load into r0 (the value IS an address)
                       ; and class by its eightbyte -- INT a gp arg, SSE an xmm
                       (: one (? (stru? aty) (aone g aty) ())
                          wd2 (wd || id? one 'sse)
                          aval (? (t32? g && id? one 'sse) (tor0 g a + pld 'r0 0)           ; the t32 8B blob rides the r0:r1 pair
                                  one (tor0 g a + '((ld r0 r0 0)))
                                  wd (asflo g a + '((movqrx r0 f0)))
                                  (asintt g a aty))
                          ; a register arg rides a 16-byte cell: rsp stays 16-aligned while the
                          ; REMAINING args evaluate (a later one may hold a nested call). an
                          ; overflow arg keeps its 8-byte push -- the callee reads the block
                          ; packed, and its target is love-internal, so eval-time skew is moot.
                          rpush (aval + (? (t32? g && wd2) spush2 (spush 'r0)))     ; a t32 double (or 8B blob) is the r0:r1 pair
                          ; the overflow store: x64 pushes 8 (deepest first); arm64 stores its
                          ; 8-byte slot in the pre-allocated block, t32 the same with 4-byte slots
                          ; (sp sits at the block base whenever an arg evaluates)
                          opush (? (a8? g) (aval + [['st 'sp (* 8 ovc) 'r0]])
                                   (t32? g) (aval + [['st 'sp ovc 'r0]])
                                   (aval + '((push r0))))
                          (? (t32? g && va? && wd2)        ; a t32 VARIADIC call rides the base PCS: no d-reg doubles
                              (nolane g ("no lane for a double in a variadic call" + ontgt g))
                             (v6m? g && id? aty 'float)    ; v6-M base ABI: a bare float arg is ONE WORD (binary32,
                              (: av1 (asflo g a              ;  the gcc shape -- ai_flo_t IS float on a 32-bit love)
                                      + '((cvtsd2ss f0 f0) (movqrx r0 f0)))
                                 (? (gpc < ngp g)
                                    (kls >as (i + 1) (gpc + 1) xmmc ovc
                                         ([['g gpc] (av1 + spush 'r0)] >< acc))
                                    (kls >as (i + 1) gpc xmmc (ovc + 4)
                                         ([['ov ovc 4] (av1 + [['st 'sp ovc 'r0]])] >< acc))))
                             (v6m? g && wd2)               ; v6-M base ABI: a double/8B-blob arg is an even-odd gp
                              (: gpe ((gpc + 1) & -2)      ;  pair (aval already lands r0:r1), else the 8-aligned stack
                                 (? (gpe + 2 <= (ngp g))
                                     (kls >as (i + 1) (gpe + 2) xmmc ovc
                                          ([['g2 gpe] (aval + spush2)] >< acc))
                                     (: ov8 (& (ovc + 7) -8)
                                        (kls >as (i + 1) 4 xmmc (ov8 + 8)
                                             (link [['ov ov8 8]
                                                      (aval + [['st 'sp ov8 'r0] ['st 'sp (ov8 + 4) 'r1]])]
                                                   acc)))))
                             ; a VARIADIC riscv call: every double rides the GP lane (the callee's
                             ; packed a0..a7 block and 8-byte walk read gp slots), falling
                             ; through to the g / ov lanes below
                             (rv? g && va? && wd2 && gpc < ngp g)
                              (kls >as (i + 1) (gpc + 1) xmmc ovc ([['g gpc] rpush] >< acc))
                             (rv? g && va? && wd2)
                              (kls >as (i + 1) gpc xmmc (ovc + 1) ([['ov ovc] opush] >< acc))
                             (wd2 && xmmc < 8) (kls >as (i + 1) gpc (xmmc + 1) ovc ([['x xmmc] rpush] >< acc))
                             (t32? g && wd2)              ; a t32 fp/blob OVERFLOW arg (9+ doubles): the 4-byte slot can't carry it
                              (nolane g ("no lane for a double argument past the register file" + ontgt g))
                             (!wd2 && gpc < ngp g) (kls >as (i + 1) (gpc + 1) xmmc ovc ([['g gpc] rpush] >< acc))
                             (t32? g) (kls >as (i + 1) gpc xmmc (ovc + 4) ([['ov ovc 4] opush] >< acc))
                             (kls >as (i + 1) gpc xmmc (ovc + 1) ([['ov ovc] opush] >< acc)))))))))))
       ; v6-M MEMORY return (AAPCS32 base): a >4-byte composite result fills a caller temp
       ; whose address rides the IMPLICIT first argument (r0) -- args start at gpc 1, the
       ; address materializes fresh right before the call and again after it (the result)
       sr6 (sretm? g ret)
       sroff (? sr6 (ntmp g ret) 0)
       ; a call whose every argument is a pure leaf runs no sibling evaluation, so
       ; nothing between the reads and the call can clobber a register-resident
       ; param -- ezarg's ride lane asks for this.
       _ (pin g 'ezride (: (pure? l) (? !(two? l) 1 (member? <<l '(num var) && pure? >l))
                          (pure? ags)))
       kr (kls ags 0 (? sr6 1 0) 0 0 ())
       _ (pin g 'ezride ())
       (? (bad? kr) 'bad
          (: ov? (\ kent (<<kent = 'ov))
             ovr (: (go1 l1 acc1) (? !(two? l1) acc1
                                     (go1 >l1 (? (ov? <l1) (<l1 >< acc1) acc1)))
                    (go1 kr ()))                          ; overflow entries, REVERSED (deepest pushes first)
             rgr (: (go2 l2 acc2) (? !(two? l2) acc2
                                     (go2 >l2 (? (member? <<<l2 '(g g2 g4 x x2 p2 ezd))
                                                      (<l2 >< acc2) acc2)))
                    (go2 kr ()))                          ; PUSHING register entries, reversed (pops run forward)
             (pushcat l3 acc3) (? !(two? l3) acc3 (pushcat >l3 (acc3 + <><l3)))
             ; the xmm pops stage through a gp scratch: r0 on x64 (no arg rides it), a pool
             ; register on arm64 (r0 IS arg 0 there, and no pool value lives across a call)
             px (psc g)
             pops (: (go4 l4 acc4)
                      (? !(two? l4) acc4
                         (: kc <<l4
                            (go4 >l4 (member? <kc '(ov ez ezm ezd) ? acc4 (acc4 +
                                      (<kc @ 'g (spop (nthr (argr g) <>kc))
                                       'g2   ; the t32 pair: both words off one cell
                                        [['ld (nthr (argr g) <>kc) 'sp 0]
                                           ['ld (nthr (argr g) (<>kc + 1)) 'sp 4]
                                           '(add sp sp 16)]
                                       'g4   ; the v6-M whole-composite quad: r0-r3 off one cell
                                        '((ld r0 sp 0) (ld r1 sp 4)
                                           (ld r2 sp 8) (ld r3 sp 12)
                                           (add sp sp 16))
                                       ('x && t32? g)     ; a t32 double pops STRAIGHT into its d-reg
                                        [['ldsd (xmmreg <>kc) 'sp 0] '(add sp sp 16)]
                                       'x2   ; the struct pair: re then im (a natural 16-byte cell)
                                        (? (t32? g)   ; t32 pops STRAIGHT into the d-pair
                                           [['ldsd (xmmreg <>kc) 'sp 0] ['ldsd (xmmreg (<>kc + 1)) 'sp 8]
                                            '(add sp sp 16)]
                                           (a8? g)
                                           [['ld px 'sp 0] ['movqxr (xmmreg <>kc) px]
                                            ['ld px 'sp 8] ['movqxr (xmmreg (<>kc + 1)) px]
                                            '(add sp sp 16)]
                                           ['(pop r0) ['movqxr (xmmreg <>kc) 'r0]
                                             '(pop r0) ['movqxr (xmmreg (<>kc + 1)) 'r0]])
                                       'p2   ; a general two-eightbyte struct: eb0 (top) then eb1, each into its gp/xmm target
                                        (? (a8? g)
                                           ((<>kc @ 'g [['ld (nthr (argr g) <>>kc) 'sp 0]] [['ld px 'sp 0] ['movqxr (xmmreg <>>kc) px]])
                                            + (<>>>kc @ 'g [['ld (nthr (argr g) <>>>>kc) 'sp 8]] [['ld px 'sp 8] ['movqxr (xmmreg <>>>>kc) px]])
                                            + '((add sp sp 16)))
                                           ((<>kc @ 'g [['pop (nthr (argr g) <>>kc)]] ['(pop r0) ['movqxr (xmmreg <>>kc) 'r0]])
                                            + (<>>>kc @ 'g [['pop (nthr (argr g) <>>>>kc)]] ['(pop r0) ['movqxr (xmmreg <>>>>kc) 'r0]])))
                                       (spop px + [['movqxr (xmmreg <>kc) px]])))))))
                     (go4 kr ()))
             nxmm (: (go5 l5 n5) (? !(two? l5) n5
                                    (go5 >l5 (: ce <<l5 kk <ce
                                                     (kk @ 'x (n5 + 1) 'x2 (n5 + 2)
                                                           'p2 (n5 + (nthr ce 1 = 'x) + (nthr ce 3 = 'x))
                                                           n5))))
                     (go5 kr 0))
             ; SLOTS, not entries: a MEMORY-class struct arg occupies several (its ('ov off n)
             ; carries the count; t32's third field is bytes -- ovblk below sizes that block)
             nov (: (go8 l8 n8) (? !(two? l8) n8
                                   (: ce <<l8
                                      (go8 >l8 (n8 + (two? >>ce ? <>>ce 1)))))
                    (go8 ovr 0))
             padn ((nov & 1) = 1 ? 8 0)               ; keep sp 16-aligned across the overflow block
             ; t32 slots are BYTE offsets ((ov off sz) entries): the block spans to the highest
             ; slot's end, rounded to 8 (AAPCS32 sp alignment). x64/a64 keep 8-byte slot counts.
             ovblk (? (t32? g)
                      (: (go9 l9 m9) (? !(two? l9) ((m9 + 7) & -8)
                                        (: ce <<l9
                                           e9 (<ce @ 'ov (<>ce + (two? >>ce ? <>>ce 4)) 0)
                                           (go9 >l9 (m9 < e9 ? e9 m9))))
                         (go9 kr 0))
                      (8 * nov + padn))
             ; x64 pads then pushes deepest-first; arm64 AND t32 allocate the packed
             ; block up front and the ov forms store into their own slots
             pad (? (arm? g) (0 < ovblk ? [['sub 'sp 'sp ovblk]])
                    (0 < padn ? [['sub 'sp 'sp padn]]))
             clean (0 < ovblk ? [['add 'sp 'sp ovblk]])
             pf (pad + pushcat ovr () + pushcat rgr ())
             ; al = #xmm, set AFTER pops (r0 is no x64 arg register). AAPCS64/32 pass variadic
             ; args exactly like named ones -- no al, and ⚠ r0 IS arg 0 there: never clobber it.
             setal (va? && !(arm? g) ? [['li 'r0 nxmm]])
             ; NON-LEAF register homes (g 'homes, ((reg slot)..)): a home rides a caller-saved
             ; register, so it spills to its frame slot just before the pops (the args have all
             ; evaluated, so the register holds the latest value) and reloads after the clean; a
             ; ret-position call's reloads are DEAD and the sibcall pass skips them. filtered to
             ; this statement's wrapset (g 'stwrap): a home DEAD across this call keeps its
             ; register, no spill and no reload. 'all (no liveness ran) wraps every home.
             hs (: h9 (g 'homes) w9 (peep g 'stwrap 'all)
                   (w9 @ 'all h9 (filter (h2 \ member? <>>h2 w9) h9)))
             spills (foldl (\ a2 p2 (a2 + [['st 'r4 <>p2 <p2]])) () hs)
             relods (foldl (\ a2 p2 (a2 + [['ld <p2 'r4 <>p2]])) () hs)
             ; the targeted emissions, after the spills and before the pops: an 'ezm home mov
             ; demotes to its spill-slot load when ANOTHER targeted arg writes that home
             ezt (: (go6 l6 a6) (? !(two? l6) a6
                                   (go6 >l6 (: kc <<l6
                                             (? (member? <kc '(ez ezm))
                                              (nthr (argr g) <>kc >< a6) a6))))
                    (go6 kr ()))
             ezf (: (go7 l7 a7) (? !(two? l7) a7
                                   (go7 >l7
                                        (: kc <<l7
                                           (? (<kc = 'ez) (a7 + <><l7)
                                              !(<kc = 'ezm) a7
                                              (: rH <>>kc
                                                 (? (member? rH ezt && !(id? rH (nthr (argr g) <>kc)))
                                                    (a7 + <>><l7)
                                                    (a7 + <><l7)))))))
                    (go7 kr ()))
             (? dir
                 (: ; v6-M base ABI: a double result arrives in r0:r1, a bare float as one word
                    ; (binary32 in r0 -- widen it) -- either lands on the f0 fiction
                    fres (? !(v6m? g && flo? ret) ()
                            (id? ret 'float) '((movqxr f0 r0) (cvtss2sd f0 f0))
                            '((movqxr f0 r0)))
                    ; the hidden sret pointer rides the FIRST ARGUMENT register (rdi on x64, r0
                    ; on arm), and the result is the temp's address, materialized fresh in r0
                    srin (? sr6 [['lea (nthr (argr g) 0) 'r4 sroff]] ())
                    srout (? sr6 [['lea 'r0 'r4 sroff]] ())
                    (mkv ret 'r0 (pf + spills + ezf + pops + setal + srin + [['call (intern <>hd)]] + fres + srout + clean + relods)))
                (: hr (cgexpr g env hd ())                   ; a function-pointer value: park (x64 rbx; arm64 x15), callr
                   pk (pkr g)
                   fres (? !(v6m? g && flo? (fnret (vty hr))) ()
                           (id? (fnret (vty hr)) 'float) '((movqxr f0 r0) (cvtss2sd f0 f0))
                           '((movqxr f0 r0)))
                   (? (bad? hr) 'bad
                      (v6m? g && stru? (fnret (vty hr)) &&   ; a MEMORY-returning callee behind a POINTER: the
                          4 < tsz g (fnret (vty hr)))        ; implicit-arg shift wasn't staged
                       (nolane g ("no lane for an indirect call to a MEMORY-returning function" + ontgt g))
                      ; ⚠ t32: r12 is ALSO the far-mem/pair scratch, so a register park dies to
                      ; any far frame offset in the ez emissions -- park in a frame slot and
                      ; reload r12 as the LAST thing before the call (the sib rewrite keeps the
                      ; ld: the epilogue never touches r12).
                      (t32? g)
                       (: pko (ntmp g 'long)
                          (mkv (fnret (vty hr)) 'r0 (pf + tor0 g hr + [['st 'r4 pko 'r0]] +
                                    spills + ezf + pops + setal + [['ld pk 'r4 pko] ['callr pk]] + fres + clean + relods)))
                      (mkv (fnret (vty hr)) 'r0 (pf + tor0 g hr + [['mov pk 'r0]] +
                                spills + ezf + pops + setal + [['callr pk]] + clean + relods))))))))
   ; -- the C99 complex lane (x64): a complex value is its seeded two-member struct (gcplx?), so
   ; it flows everywhere a struct value flows. ARITHMETIC runs here: each side MATERIALIZES into a
   ; double-pair frame temp (the other side's evaluation may call), f0..f3 then carry
   ; (a.re a.im b.re b.im), the op combines into (f0 f1), and the result lands in a fresh temp
   ; whose ADDRESS is the answer. divide is the textbook formula (no Smith recovery). --
   (cxb ty) (? (<>ty = "__cplx_float") 'float 'double)
   ; -- the C99 VLA type ('varr elt dim-expr [idx]): a runtime-dim array. a LOCAL carries its
   ; parse-stamped idx into g 'vslots -> (ptrslot sizeslot); a POINTEE (char (*p)[width]) carries
   ; just the expr, re-evaluated at each scale site. --
   (gvarr? ty) (two? ty && <ty = 'varr)
   (varel ty) <>ty                             ; the element type (const-sized)
   (varn ty) <>>ty                             ; the dim EXPRESSION (an AST)
   ; ⚠ the 4th element by READER OPS, not by `at`: `at` is holo's, and gen.l is cat'd WITHOUT
   ; holo for the laws, where an unbound `at` is no error but a silent no-op -- (at ty 3) reads
   ; ((0 ty) 3) = (1 3) = 3, so every VLA site would share one slot index.
   (varix ty) <>>>ty                                ; a decl's slot index (4-long only)
   ; park a VALUE REC as a double-pair temp -> (off forms srcty) | 'bad
   (cxpark g rv)
    (: off (ntmp g cxdty)
       ty (vty rv)
       (? (gcplx? ty)
           (? (id? (cxb ty) 'float)
               [off (tor0 g rv +
                      ['(ldss f2 r0 0) '(cvtss2sd f2 f2) ['stsd 'r4 off 'f2]
                       '(ldss f2 r0 4) '(cvtss2sd f2 f2) ['stsd 'r4 (off + 8) 'f2]])
                 ty]
              [off (tor0 g rv +
                     ['(ld r2 r0 0) ['st 'r4 off 'r2]
                      '(ld r2 r0 8) ['st 'r4 (off + 8) 'r2]])
                ty])
          (stru? ty) 'bad
          [off (asflo g rv + [['stsd 'r4 off 'f0] '(li r2 0) ['st 'r4 (off + 8) 'r2]])
            ty]))
   ; evaluate an EXPR and park it -> (off forms srcty) | 'bad
   (cxmat g env x)
    (: rl (matv g (clval g env x))
       rv (? (!(bad? rl) && stru? (vty rl)) rl (cgexpr g env x ()))
       (? (bad? rv) 'bad (cxpark g rv)))
   (cxload oa ob) [['ldsd 'f0 'r4 oa] ['ldsd 'f1 'r4 (oa + 8)]
                    ['ldsd 'f2 'r4 ob] ['ldsd 'f3 'r4 (ob + 8)]]
   (cxarith op)                                     ; f0..f3 = (a.re a.im b.re b.im) -> (re im) in (f0 f1)
    (op @ '+ '((addsd f0 f2) (addsd f1 f3))
          '- '((subsd f0 f2) (subsd f1 f3))
          '* '((movsd f4 f0) (movsd f5 f1)
               (mulsd f0 f2) (mulsd f5 f3) (subsd f0 f5)                 ; re = ac - bd
               (mulsd f4 f3) (mulsd f1 f2) (addsd f1 f4))                ; im = bc + ad
          '/ '((movsd f4 f2) (mulsd f4 f2)                               ; c^2
               (movsd f5 f3) (mulsd f5 f3) (addsd f4 f5)                 ; den = c^2 + d^2
               (movsd f5 f0) (mulsd f5 f2)                               ; ac
               (movsd f6 f1) (mulsd f6 f3) (addsd f5 f6)                 ; ac + bd
               (divsd f5 f4)                                             ; re
               (mulsd f1 f2) (mulsd f0 f3) (subsd f1 f0)                 ; bc - ad
               (divsd f1 f4) (movsd f0 f5)))
   ; the (f0 f1) result into a fresh rt-typed temp; the temp's address answers
   (cxout g rt fs)
    (: ro (ntmp g rt)
       st (cxb rt @ 'float
           (fstf g 'float 'r4 ro 'f0 + fstf g 'float 'r4 (ro + 4) 'f1)
           [['stsd 'r4 ro 'f0] ['stsd 'r4 (ro + 8) 'f1]])
       (mkv rt 'r0 (fs + st + [['lean 'r0 'r4 ro]])))
   (cgcx g env op a rhs)                            ; the LEFT already evaluated (a value rec)
    (? !(id? (peep g 'tgt 'amd64) 'amd64)
        (nolane g ("no lane for C99 complex arithmetic" + ontgt g))
       (: arith (cxarith op)
          (? !(two? arith) 'bad
             (: pa (cxpark g a)
                (? (bad? pa) 'bad
                   (: mb (cxmat g env rhs)
                      (? (bad? mb) 'bad
                         (: tb <>>mb
                            rt (? (gcplx? (vty a) && id? (cxb (vty a)) 'float &&
                                   gcplx? tb && id? (cxb tb) 'float)
                                  '(struct "__cplx_float") cxdty)
                            (cxout g rt (<>pa + (<>mb + (cxload <pa <mb + arith))))))))))))
   ; -- the rotate idiom: (x >> n) | (x << (W - n)) in either order, x an unsigned W-bit pure
   ; operand -- one eval of x, one rotate (ror4/rorv4 the 32-bit pair, ror/rorv the 64-bit).
   ; a rotl count rides negated (both ISAs read the count mod W). --
   (rotp? e)                                        ; pure: safe to evaluate ONCE for the source's twice
    (e @ ((k >< _) && member? k '(num flo flof str var)) 1
         ((k . x . _) && member? k '(deref dot moor addr)) (rotp? x)
         ('cast _ x) (rotp? x)
         ('bin _ a b) (rotp? a && rotp? b)
         ('un _ x) (rotp? x)
         ())
   (rotty g env e)                                  ; the operand's declared type, by static peek
    (e @ ('var nm) (: en (vent g env nm) (? (bad? en) () <>>en))
         ('cast . ty . _) ty
         ('deref p) (: t (rotty g env p) (? (ptr? t) <>t (arr? t) <>t ()))
         ('bin op x y && member? op '(+ -))         ; index arithmetic keeps the ptr/arr side's type
          (: a (rotty g env x)
             (? (ptr? a || arr? a) a
                (: b (rotty g env y) (ptr? b || arr? b ? b)))))
   (rotw g env e)                                   ; -> 32 | 64 | (): the unsigned rotate width
    (rotty g env e @ 'uint 32 ('ulong || 'ullong) 64)
   (rotk g e)                                       ; a count as a NUMBER: knum, then the const fold
    (: k (knum g e) (two? k ? <k (cfold e)))
   (rotc? g w big n)                                ; big reads (W - n), n pure and shared
    (big @ ('bin '- wide sub)
             (: kb (rotk g wide)
                (charm? kb && kb = w && sub = n && rotp? n)))
   (cgrimm g env x w c wnt)                         ; the immediate rotate: mirror of the constant shift
    (: xv (cgexpr g env x ())
       (? (bad? xv) 'bad
          (: rt (w @ 32 'uint 'ulong)
             iop (w @ 32 'ror4 'ror)
             (? (wr? g (vreg xv))
                (mkv rt (vreg xv) (vfs xv + [[iop (vreg xv) c]]))
                (: rd (wnt || rdst g (vreg xv))
                   (mkv rt rd (vfs xv + [['mov rd (vreg xv)] [iop rd c]])))))))
   (cgrvar g env x w ce ng)                         ; the register-count rotate: mirror of the variable shift
    (: xv (cgexpr g env x ())
       (? (bad? xv) 'bad
          (: cv (cgexpr g env ce ())
             (? (bad? cv) 'bad
                (: rt (w @ 32 'uint 'ulong)
                   vop (w @ 32 'rorv4 'rorv)
                   af (tor0 g xv)
                   bf (tor0 g cv)
                   st (: s (spare g bf)             ; the value holds in a pool reg past the count's eval
                         (? s (([['mov s 'r0]] + bf) + ['(mov r1 r0) ['mov 'r0 s]])
                            (spush 'r0 + bf + '((mov r1 r0)) + spop 'r0)))
                   (mkv rt 'r0 (af + st + (ng ? '((neg r1))) + [[vop 'r0 'r1]])))))))
   (cgror g env L R wnt)                            ; -> tuple | 'bad | 'no (the general lane stands)
    (? !(rot4? g) 'no
       (: a (two? L && (<L = 'bin) && member? <>L '(shl shr) ? L)
          b (two? R && (<R = 'bin) && member? <>R '(shl shr) ? R)
          (? (!(two? a) || !(two? b) || id? <>a <>b) 'no
             (: s (<>a @ 'shr a b)            ; the shr side: a rotate is ror by ITS count
                l (<>a @ 'shr b a)
                x <>>s
                (? (!(<>>l = x) || !(rotp? x)) 'no
                   (: w (rotw g env x)
                      (? !w 'no
                         (: cs <>>>s cl <>>>l
                            ks (rotk g cs) kl (rotk g cl)
                            (? (charm? ks && charm? kl)
                                (? (ks + kl = w && 0 < ks && ks < w)
                                   (cgrimm g env x w ks wnt) 'no)
                               (charm? ks || charm? kl) 'no
                               (rotc? g w cl cs) (cgrvar g env x w cs ())   ; rotr: ror by the shr count
                               (rotc? g w cs cl) (cgrvar g env x w cl 1)    ; rotl: ror by the negated shl count
                               'no)))))))))
   (cgbin g env e wnt)
    (: op <>e
       lhs <>>e rhs <>>>e
       ; ⚠ LAZY left-then-right, like cbranch: the left PARKS (allocates) before the right
       ; evaluates. a constant side rides the immediate lane with NO right evaluation; two
       ; register operands combine 3-address; pointer scaling, divide, a variable shift, and a
       ; call on the right keep the memory shuttle (shut).
       ; the destination die: a LOAD-shaped left (dot/moor/deref -- the shapes whose r0
       ; landing costs a bridge; a var/num re-aims free via cgleaf, a nested bin already
       ; rides a pool register) AIMS at its park before it evaluates, and vpark sees a
       ; delivered value: no bridge mov. one aim per spine -- an eager aim at every level
       ; drains the pool top-down, and pass 1's wide roster then hands out param homes
       ; (the wraps law's catch). a call on either side bars it; shifts keep r0 (their
       ; lanes ride the protocol); a constant right rides immop (3-address off any
       ; register -- no bridge to win); unhonored (a float, a pair) hands it back.
       pk (? (!(two? lhs && member? <lhs '(dot moor deref)) ||
              callish? lhs || callish? rhs ||
              member? op '(shl shr) || two? (knum g rhs)) ()
             (ralloc g))
       a (cgexpr g env lhs pk)
       _ (? (pk && !(!(bad? a) && rpeq? g (vreg a) pk)) (rfree g pk) ())
       (? (bad? a) 'bad
          (gcplx? (vty a)) (cgcx g env op a rhs)    ; a complex left: the complex lane (parse lifted a real RIGHT already)
          ; a constant count rides the immediate shift; a variable count
          ; rides shlv/sarv with the count PINNED to r1 (holo's contract)
          (member? op '(shl shr) && pair? g (vty a))    ; a pair shift keeps the left type
           (: kind (? (op = 'shl) 'shl (usg? (vty a)) 'shr 'sar)
              kc (knum g rhs)
              (? (two? kc)
                  (mkv (vty a) 'r0 (tor0 g a + pshiftc kind <kc))
                 (: b (cgexpr g env rhs ())
                    (? (bad? b) 'bad
                       (mkv (vty a) 'r0 (tor0 g a + spush2 +
                                         tor0 g b + '((mov r2 r0)) + spop2 'r0 'r1 +
                                         pshiftv g kind))))))
          (member? op '(shl shr) && d128? (vty a))          ; a wide shift keeps the left type
           (? !(w128? g) (no128 g)
              (: kind (? (op = 'shl) 'shl128 (usg? (vty a)) 'shr128 'sar128)
                 vknd (? (op = 'shl) 'shl128v (usg? (vty a)) 'shr128v 'sar128v)
                 kc (knum g rhs)
                 (? (two? kc)
                     (? ((<kc < 0) || (127 < <kc)) 'bad
                        (<kc = 0) (mkv (vty a) 'r0 (tor0 g a))
                        (mkv (vty a) 'r0 (tor0 g a + [[kind <kc]])))
                    (: b (cgexpr g env rhs ())
                       (? (bad? b) 'bad
                          (mkv (vty a) 'r0 $
                           tor0 g a + wpush + tor0 g b +
                            '((mov r1 r0) (ld r0 sp 0) (ld r2 sp 8)) +   ; count -> cl, the pair back
                            wdrop + [[vknd]]))))))
          (member? op '(shl shr))
           (: uL (usg? (vty a))                             ; a `>>` on an unsigned left operand is LOGICAL (shr, not sar)
              ; `<<` is the fourth carrying op: a 32-bit unsigned shifts bits straight out of
              ; its type, so it re-narrows like + - * do. `>>` never can -- a logical right
              ; shift of an already-canonical value stays inside it.
              u32 ((op = 'shl) && u32? g (vty a))
              (nar r) (? u32 [['zx4 r]] ())
              rt (? (u32? g (vty a)) 'uint uL 'ulong 'long) ; the shift result keeps the left operand's type
              iop (? (op = 'shl) 'shl (? uL 'shr 'sar))
              vop (? (op = 'shl) 'shlv (? uL 'shrv 'sarv))
              kc (knum g rhs)
              (? (two? kc)
                  (? (wr? g (vreg a))
                     (mkv rt (vreg a) (vfs a + [[iop (vreg a) <kc]] + nar (vreg a)))
                     (: rd (? wnt wnt (rdst g (vreg a)))    ; a borrowed home shifts a COPY (no 3-address shift)
                        (mkv rt rd (vfs a + [['mov rd (vreg a)] [iop rd <kc]] + nar rd))))
                 (: b (cgexpr g env rhs ())
                    (? (bad? b) 'bad
                       (: af (tor0 g a)
                          bf (tor0 g b)
                          st (: s (spare g bf)      ; the value holds in a pool reg past the count's eval
                                (? s (([['mov s 'r0]] + bf) + ['(mov r1 r0) ['mov 'r0 s]])
                                   (spush 'r0 + bf + ('((mov r1 r0)) + spop 'r0))))
                          (mkv rt 'r0 (af + st + ([[vop 'r0 'r1]] + nar 'r0))))))))
          (: ta (vty a)
             ; a constant right rides the immediate lane; a constant LEFT swaps
             ; over (commutative ops) or mirrors (relations, rflip) onto the right
             imr (: kr (knum g rhs)
                    (? (!(flo? ta) && two? kr && !(ptr? ta && gvarr? <>ta))
                       (immop g op a ta <kr <>kr wnt) 'no))
             (? !(id? imr 'no) imr
                (: kl (? (member? op '(+ * & | ^ eq ne lt le gt ge)) (knum g lhs) ())
                   iml? (!(flo? ta) && two? kl)
                   ah (? (iml? || flo? ta || callish? rhs || llt? ta || d128? ta) 'sh
                         (vpark g env lhs a))       ; a pair lhs keeps the shuttle (no pool park)
                   ; the wide pair park: a d128 LEFT steps off the protocol into two pool
                   ; registers, ALLOCATED BEFORE the right evaluates so its scratch dodges
                   ; them; the combine reads the r-twins and the cell never opens. a callish
                   ; right keeps the cell (a call clobbers every pool register), / % too
                   ; (the divide dance owns the cell).
                   wpk (? (w128? g && d128? ta && !(callish? rhs) &&
                           member? op '(+ - * & | ^ eq ne lt le gt ge))
                          (: rl (ralloc g)
                             (? !rl ()
                                (: rh (ralloc g)
                                   (? rh [rl rh] (: _ (rfree g rl) ())))))
                          ())
                   b (cgexpr g env rhs ())
                   (? (bad? b) 'bad
                      (: tb (vty b)
                         ta9 (numty9 lhs ta)   ; C's type for a literal, read through
                         tb9 (numty9 rhs tb)   ; its written face (-1, ~0) -- see numty9
                         av (? (id? ah 'sh) a ah)
                         ; the memory shuttle: left in r1, right in r0 (today's shape) --
                         ; the cell is the LAST resort: a left already RIDING a register bf
                         ; skips reads through it, then the r1 skip, then a pool-cell park
                         (shut z9)
                          (: mid (: rA (vreg av)
                                    af9 (tor0 g av)       ; built first (the standing free order); the ride drops it
                                    bf (tor0 g b)
                                    (? (!(two? (vfs av)) && !(id? rA 'r0) && psafe? g bf rA)
                                       (bf + [['mov 'r1 rA]])
                                       (r1safe? bf) (af9 + '((mov r1 r0)) + bf)
                                       (: s (spare g bf)
                                          (? s (af9 + ([['mov s 'r0]] + bf) + [['mov 'r1 s]])
                                             (af9 + spush 'r0 + bf + spop 'r1)))))
                             back '((mov r0 r1))
                             pa (ptr? ta) pb (ptr? tb)
                             unsg (cmpu g ta9 tb9)                      ; divide and compare take the COMMON type's signedness -- see cmpu
                             u32 (u32bin? g ta9 tb9)                  ; a 32-bit unsigned result wraps -- see u32bin?
                             nar (? u32 '((zx4 r0)) ())               ; the shuttle always lands in r0 (back)
                             nty (? u32 'uint 'long)
                             (? (d128? ta || d128? tb)
                                 ; the WIDE shuttle (x64): the left rides its cell, the right the
                                 ; r2:r0 protocol; + - * & | ^ combine through the carry family,
                                 ; relations settle 0/1 via c128. a widening product (both hi
                                 ; halves provably zero) is ONE mulq; / and % stay refused
                                 ; (love.c divides through the divq asm).
                                 (? !(w128? g) (no128 g)
                                    (pa || pb) 'bad
                                    (: rt (wbin ta tb)
                                       w? (two? wpk)
                                       midw (? w? (as128 g av + ([['mov <wpk 'r0] ['mov <>wpk 'r2]] + as128 g b))
                                               (as128 g av + (wpush + as128 g b)))
                                       res (op @
                                            '+ (mkv rt 'r0 (midw + (? w? [['a128r <wpk <>wpk]] ('((a128 sp)) + wdrop))))
                                            '- (mkv rt 'r0 (midw + (? w? [['s128r <wpk <>wpk]] ('((s128 sp)) + wdrop))))
                                            '* (: zl (d128? ta ? (zext128? (vfs av)) (usg? ta9 || ptr? ta9))
                                                  zr (d128? tb ? (zext128? (vfs b)) (usg? tb9 || ptr? tb9))
                                                  (? (zl && zr)                 ; hi halves zero: ONE mulq, the park needs only lo
                                                     (mkv rt 'r0 (? w? (as128 g av + ([['mov <wpk 'r0]] + (tor0 g b + [['mw128r <wpk]])))
                                                                   (as128 g av + (wpush + (tor0 g b + ('((mw128 sp)) + wdrop))))))
                                                     (mkv rt 'r0 (midw + (? w? [['m128r <wpk <>wpk]] ('((m128 sp)) + wdrop))))))
                                            ('| || '& || '^)
                                             (: w9 (irop op)
                                                (mkv rt 'r0 (midw + (? w? [[w9 'r0 'r0 <wpk] [w9 'r2 'r2 <>wpk]]
                                                               (['(ld r1 sp 0) [w9 'r0 'r0 'r1] '(ld r1 sp 8) [w9 'r2 'r2 'r1]] + wdrop)))))
                                            ('eq || 'ne || 'lt || 'le || 'gt || 'ge)
                                             (mkv 'long 'r0 (midw + (? w? [['c128r (scc op (id? rt 'u128)) <wpk <>wpk]]
                                                               ([['c128 (scc op (id? rt 'u128)) 'sp]] + wdrop))))
                                            ('/ || '%)
                                             ; u128 by a divisor with a provably-zero high half only
                                             ; (dv128's two-step divq is unsigned): a widened unsigned
                                             ; word, a zero-extended pair, or a nonnegative literal
                                             (: zr (? (d128? tb) (zext128? (vfs b))
                                                      (two? rhs && id? <rhs 'num && 0 <= <>rhs) 1
                                                      (usg? tb9 || ptr? tb9))
                                                (? (!zr || !(id? rt 'u128)) 'bad
                                                   (mkv rt 'r0 (midw + [[(op @ '/ 'dv128 'rm128) 'sp]] + wdrop))))
                                            'bad)
                                       _ (w? ? (rfree g <wpk, rfree g <>wpk))
                                       res))
                                (t32? g && !(pa || pb) && (llt? ta || llt? tb))
                                 ; the PAIR shuttle: both sides widen to r0:r1, left pops to
                                 ; r2:r3; +,-,* and the logicals combine through the carry family,
                                 ; unsigned / and % ride the backend's restoring expansion,
                                 ; relations ride subs/sbcs. signed pair divide stays refused.
                                 (: mid2 (asll g av + spush2 + asll g b + spop2 'r2 'r3)
                                    ; v6-M divide staging: numerator (the LEFT) lands r0:r1, denominator r2:r3
                                    ; (the __aeabi_(u)ldivmod protocol); mov is the hi-form, flag-silent
                                    mid2d (asll g av + spush2 + asll g b
                                           + '((mov r2 r0) (mov r3 r1)) + spop2 'r0 'r1)
                                    rt (ubin !(t32? g) ta tb)
                                    (op @ '+ (mkv rt 'r0 (mid2 + '((adds r0 r2 r0) (adc r1 r3 r1))))
                                          '- (mkv rt 'r0 (mid2 + '((subs r0 r2 r0) (sbc r1 r3 r1))))
                                          '*
                                          (? (v6m? g)   ; no UMULL/MLA on v6-M: libgcc's exact helper (commutative, either staging)
                                             (mkv rt 'r0 (mid2 + '((call __aeabi_lmul))))
                                             (mkv rt 'r0 (mid2 + '((imul r3 r3 r0) (mla r3 r2 r1 r3)
                                                                   (umull r0 r12 r2 r0) (add r1 r12 r3)))))
                                         ('| || '& || '^)
                                          (: w9 (irop op)
                                             (mkv rt 'r0 (mid2 + [[w9 'r0 'r2 'r0] [w9 'r1 'r3 'r1]])))
                                         '/
                                          (? (v6m? g)   ; quotient rides out in r0:r1; the SIGNED face works too (__aeabi_ldivmod)
                                             (mkv rt 'r0 (mid2d + [['call (? unsg '__aeabi_uldivmod '__aeabi_ldivmod)]]))
                                             unsg (mkv rt 'r0 (mid2 + '((udivll)))) 'bad)
                                         '%
                                          (? (v6m? g)   ; the remainder rides r2:r3 -> the protocol pair
                                             (mkv rt 'r0 (mid2d + [['call (? unsg '__aeabi_uldivmod '__aeabi_ldivmod)]
                                                                    '(mov r0 r2) '(mov r1 r3)]))
                                             unsg (mkv rt 'r0 (mid2 + '((uremll)))) 'bad)
                                         ('eq || 'ne || 'lt || 'le || 'gt || 'ge)
                                          (: cf (prel g op unsg)
                                             (mkv 'long 'r0 (mid2 + <>cf + [['set <cf 'r0]])))
                                         'bad))
                                (op = '+)
                                 ; ptr + idx at a machine scale is ONE leax -- the imul/add pair
                                 ; only for odd sizes. a VLA pointee scales by a RUNTIME stride:
                                 ; park both sides, re-evaluate the dim in this env, multiply.
                                 (? (pa && pb) 'bad
                                    (pa && gvarr? <>ta)
                                     (: vt <>ta                    ; mid ran: ptr r1, idx r0 -- park before the dim eval
                                        dv (: pk (spush 'r1 + spush 'r0)
                                              de (cgexpr g env (varn vt) ())
                                              (? (bad? de) 'bad
                                                 (: es (tsz g (varel vt))
                                                    (mid + pk + asint g de +
                                                     (? (1 < es) (imby g 'r0 es) ()) +
                                                     spop 'r2 + '((imul r2 r2 r0)) +
                                                     spop 'r1 + '((add r1 r1 r2) (mov r0 r1))))))
                                        (? (bad? dv) 'bad (mkv ta 'r0 dv)))
                                    (pb && gvarr? <>tb)
                                     (: vt <>tb
                                        dv (: pk (spush 'r0 + spush 'r1)       ; ptr r0, idx r1 here
                                              de (cgexpr g env (varn vt) ())
                                              (? (bad? de) 'bad
                                                 (: es (tsz g (varel vt))
                                                    (mid + pk + asint g de +
                                                     (? (1 < es) (imby g 'r0 es) ()) +
                                                     spop 'r2 + '((imul r2 r2 r0)) +
                                                     spop 'r1 + '((add r1 r1 r2) (mov r0 r1))))))
                                        (? (bad? dv) 'bad (mkv tb 'r0 dv)))
                                    pa (: sz (tsz g <>ta)
                                          (? (member? sz '(1 2 4 8))
                                             (mkv ta 'r0 (mid + [['leax 'r0 'r1 'r0 sz 0]]))
                                             (mkv ta 'r0 (mid + (? (1 < sz) (imby g 'r0 sz) ()) +
                                                          ('((add r1 r1 r0)) + back)))))
                                    pb (: sz (tsz g <>tb)
                                          (? (member? sz '(1 2 4 8))
                                             (mkv tb 'r0 (mid + [['leax 'r0 'r0 'r1 sz 0]]))
                                             (mkv tb 'r0 (mid + (? (1 < sz) (imby g 'r1 sz) ()) +
                                                          ('((add r1 r1 r0)) + back)))))
                                    (mkv nty 'r0 (mid + '((add r1 r1 r0)) + back + nar)))
                                (op = '-)
                                 (? (pa && pb)
                                     (? (tsz g <>ta = tsz g <>tb)
                                        ; a pointer difference divides the byte gap by the pointee
                                        ; size: mid leaves left in r1, right in r0, so `sub r1 r1
                                        ; r0` is the gap. a power-of-two size shifts, any other
                                        ; takes a real signed divide; size 1 is just the move.
                                        (: esz (tsz g <>ta)
                                           (mkv 'long 'r0 (mid + '((sub r1 r1 r0)) +
                                                           (? (esz <= 1) back
                                                              (pow2? esz) (back + [['sar 'r0 (lg2 esz)]])
                                                              [['li 'r0 esz] '(div r1 r1 r0) '(mov r0 r1)]))))
                                        'bad)
                                    pa (mkv ta 'r0 (mid + (? (1 < tsz g <>ta)
                                                         (imby g 'r0 (tsz g <>ta)) ()) +
                                                    ('((sub r1 r1 r0)) + back)))
                                    pb 'bad
                                    (mkv nty 'r0 (mid + '((sub r1 r1 r0)) + back + nar)))
                                (: tail (? (member? op '(* & | ^)) [[(irop op) 'r1 'r1 'r0]]
                                           ; a mixed int/uint divide runs at uint width: znrw first
                                           (op = '/) (znrw g ta9 tb9 'r1 'r0 + [[(? unsg 'udiv 'div) 'r1 'r1 'r0]])
                                           (op = '%) (znrw g ta9 tb9 'r1 'r0 + [[(? unsg 'urem 'rem) 'r1 'r1 'r0 'r3]])
                                           'bad)
                                   m32 (u32 && (op = '*))              ; only the multiply here can carry
                                   (? !(bad? tail)
                                      (mkv (rbin g ta9 tb9) 'r0
                                           (mid + tail + back + (? m32 nar ())))
                                      (member? op '(eq ne lt le gt ge))
                                       (mkv 'long 'r0 (mid + znrw g ta9 tb9 'r1 'r0 +
                                                       ['(cmp r1 r0) ['set (scc op unsg) 'r0]]))
                                      'bad))))
                         iml (? (iml? && !(flo? tb)) (immop g (rflip op) b tb <kl <>kl wnt) 'no)
                         (? !(id? iml 'no) iml
                            ((flo? ta || flo? tb) && (d128? ta || d128? tb))                ; wide meets double
                             (nolane g "a wide integer is not mixed with a double")
                            (flo? ta || flo? tb)
                             ; the double lane: either operand double -> both to double (f1=left,
                             ; f0=right through the gp stack), then the scalar-double op or ucomisd.
                             (: mid (? (t32? g)               ; the gp stack moves PAIRS on t32
                                       (asflo g av + ('((movqrx r0 f0)) + spush2) +
                                        (asflo g b + spop2 'r2 'r3 + '((movqxr f1 r2))))
                                       ; the left's bits HOLD in a gp reg across the right --
                                       ; movqrx aims straight at it, no bridge, no cell
                                       (: af (asflo g av)
                                          bf (asflo g b)
                                          s (? (r1safe? bf) 'r1 (spare g bf))
                                          (? s (af + [['movqrx s 'f0]] +
                                                (bf + [['movqxr 'f1 s]]))
                                             (af + ('((movqrx r0 f0)) + spush 'r0) +
                                              (bf + spop 'r1 + '((movqxr f1 r1)))))))
                                (op @ '+ (mkv 'double 'r0 (mid + '((addsd f0 f1))))
                                    '* (mkv 'double 'r0 (mid + '((mulsd f0 f1))))
                                    '- (mkv 'double 'r0 (mid + '((subsd f1 f0) (movsd f0 f1))))
                                    '/ (mkv 'double 'r0 (mid + '((divsd f1 f0) (movsd f0 f1))))
                                   ; IEEE compare: UNORDERED (a NaN operand) answers false for
                                   ; every relation and true only for !=. x64: above/ae read CF=0,
                                   ; which unordered sets, so < and <= COMPARE SWAPPED, and ==/!=
                                   ; carry the parity bit explicitly. arm64: ⚠ FCMP's gt/ge are
                                   ; unordered-false natively (HI/HS would read C, which unordered
                                   ; SETS), and p/np alias vs/vc, so eq/ne ride whole.
                                   ('lt || 'le || 'gt || 'ge)   ; lt/le compare SWAPPED so the sense rides gt/ge
                                    (: swp (member? op '(lt le))
                                       (mkv 'long 'r0 (mid + [['ucomisd (swp ? 'f0 'f1) (swp ? 'f1 'f0)]
                                                              ['set (? (member? op '(lt gt)) (arm? g ? 'gt 'above)
                                                                        (arm? g) 'ge 'ae) 'r0]])))
                                   'eq (mkv 'long 'r0 (mid + ('((ucomisd f1 f0)) + fsete)))
                                   'ne (mkv 'long 'r0 (mid + ('((ucomisd f1 f0)) + fsetn)))
                                   'bad))
                            ; the REGISTER lane: both values live, 3-address combine
                            (: pa (ptr? ta) pb (ptr? tb)
                               ; a live ptr + live idx at a machine scale: ONE leax, no shuttle
                               ; (lea reads both sources before writing -- any alias). a VLA
                               ; pointee has no STATIC scale, so it never takes the leax
                               vpt ((pa && gvarr? <>ta) || (pb && gvarr? <>tb))
                               lxsz (? !(op = '+) () (pa && pb) () vpt ()
                                       pa (: s9 (tsz g <>ta) (member? s9 '(1 2 4 8) ? s9))
                                       pb (: s9 (tsz g <>tb) (member? s9 '(1 2 4 8) ? s9)))
                               (? (id? ah 'sh) (shut ())
                                  vpt (shut ())
                                  ((pa || pb) && !lxsz) (shut ())
                                  (t32? g && llt? tb) (shut ())       ; a pair right: the pair shuttle (widen the park)
                                  (d128? tb) (shut ())                ; a wide right: the wide shuttle (widen the park)
                                  lxsz
                                   (: rA (vreg ah) rB (vreg b)
                                      rd (? wnt wnt (rdst g rA))
                                      fs (vfs ah + vfs b
                                          + [['leax rd (? pa rA rB) (? pa rB rA) lxsz 0]])
                                      _ (rfree g rB) _ (? (rpeq? g rd rA) () (rfree g rA))
                                      (mkv (? pa ta tb) rd fs))
                                  (: rA (vreg ah) rB (vreg b)
                                     cunsg (cmpu g ta9 tb9)
                                     pre (vfs ah + vfs b)
                                     (? (member? op '(eq ne lt le gt ge))
                                         ; a mixed int/uint compare owes a zx4 narrow, and rA/rB
                                         ; may be HOMES -- the shuttle owns the copies
                                         (? (two? (znrw g ta9 tb9 'r1 'r0)) (shut ())
                                            (: rd (? wnt wnt (rdst g rA))   ; set runs after the cmp: rd may alias rB
                                               fs (pre + [['cmp rA rB] ['set (scc op cunsg) rd]])
                                               _ (rfree g rB) _ (? (rpeq? g rd rA) () (rfree g rA))
                                               (mkv 'long rd fs)))
                                        (member? op '(+ - * & | ^))
                                         (: iop2 (irop op)
                                            rd (? (wnt && !(rpeq? g wnt rB)) wnt (rdst g rA))
                                            u32 (member? op '(+ - *) && u32bin? g ta9 tb9)
                                            ; holo reorders a commutative rd==rb; sub CANNOT (alias-dst) --
                                            ; a borrowed left + dry pool can land rd on rB's r0
                                            (? ((op = '-) && rpeq? g rd rB) (shut ())
                                               (: fs (pre + [[iop2 rd rA rB]] + (? u32 [['zx4 rd]] ()))
                                                  _ (rfree g rB) _ (? (rpeq? g rd rA) () (rfree g rA))
                                                  (mkv (rbin g ta9 tb9) rd fs))))
                                        (shut ())))))))))))))

   ; ⚠ r3 (rbx) is CALLEE-SAVED under SysV and gen leans on it (callr park, the overflow
   ; builtins, va_arg's walk) -- an optimized gcc caller keeps LIVE LOCALS in rbx across our
   ; calls. every fn owns frame slot -8 for it: the prologue stores, this epilogue reloads.
   epi '((ld r3 r4 -8) (mov sp r4) (pop r4) (ret))
   ; the arm64 twin: bl clobbered lr, so the prologue pushed it (its own 16-byte cell) before the
   ; frame register, and x3 is CALLER-saved there so the rbx slot vanishes. ⚠ `mov` to/from sp is
   ; ORR (reads/writes XZR at 31), so the sp hop rides lea instead. cgstmt/build read the target's
   ; epilogue off g 'epi.
   epi-a64 '((lea sp fp 0) (pop fp) (pop lr) (ret))
   ; the thumb2 VARARG twin: the prologue pushed r0-r3 under fp/lr (contiguous
   ; with the caller's stack args), so the epilogue drops that 16-byte block
   ; after restoring fp/lr and before the return
   epi-t32va '((lea sp fp 0) (pop fp) (pop lr) (add sp sp 16) (ret))
   epi-rv-va '((lea sp fp 0) (pop fp) (pop lr) (add sp sp 64) (ret))                   ; drop the a0-a7 block
   ; -- the SOFT-DOUBLE pass (sp64: an SP-only FPU -- the playdate's STM32F746). fpv4/5-SP keeps
   ; the 64-bit TRANSFERS, so ldsd/stsd/movqxr/movqrx ride the hardware and the value model keeps
   ; its d-regs -- but every f64 ARITHMETIC/compare/convert is UNDEFINED there and lowers to the
   ; __aeabi_* helper gcc emits at -mfpu=*-sp-*, so a mooncc double and a gcc double agree to the
   ; BIT. every call blankets r0-r3 through a 32-byte sp cell (slots 0..12 the saves, 16 the float
   ; bounce): a gp PAIR can be live across these ops, and the cell keeps the pass sound with no
   ; liveness reasoning. ⚠ ABOVE sp always -- an IRQ frame owns the space below. d-regs need no
   ; saving: moon stages doubles only through f0/f1 + the f15 narrow scratch.
   ; a compare's consumers are the FUSED next forms (br cc / set cc / the eq-np ordered dance),
   ; rewritten as a unit with each VCMP-flavored cc mapped onto a boolean helper + sense -- plain
   ; loads restore r0-r3 without touching the flags, so the branch/set reads them across the
   ; restore. anything unrecognized after a compare scares LOUD.
   sf-bin {'addsd '__aeabi_dadd 'subsd '__aeabi_dsub
           'mulsd '__aeabi_dmul 'divsd '__aeabi_ddiv}
   ; VCMP-flavored cc -> (helper sense): sense 'ne = helper-true means the cc
   ; holds; 'eq = helper-false (the negated relations, unordered-TRUE like the
   ; flags they replace: le = !(a>b), lt = !(a>=b))
   (sf-cc cc) (cc @ 'gt '(__aeabi_dcmpgt ne)
                    'ge '(__aeabi_dcmpge ne)
                    'eq '(__aeabi_dcmpeq ne)
                    'ne '(__aeabi_dcmpeq eq)
                    'le '(__aeabi_dcmpgt eq)
                    'lt '(__aeabi_dcmpge eq))
   sf-save '((sub sp sp 32) (st sp 0 r0) (st sp 4 r1) (st sp 8 r2) (st sp 12 r3))
   sf-rest '((ld r0 sp 0) (ld r1 sp 4) (ld r2 sp 8) (ld r3 sp 12) (add sp sp 32))
   (sf-slot r) (r @ 'r0 0 'r1 4 'r2 8 'r3 12 -1)
   ; the v6-M SOFT-DOUBLE pass: no FPU registers AT ALL (the M0+/RP2040 face). the f0/f1/f15
   ; fiction rewrites onto the three frame cells cgfn pinned (fc0/fc1/fc15). every f64
   ; arithmetic/compare/convert lowers to its __aeabi_* call (sp64's helpers and fused-compare
   ; dance); the TRANSFERS -- no vldr/vstr/vmov exists -- become word loads/stores through a saved
   ; low temp (r3, or r2 when the base IS r3; mem ops never touch flags, so a transfer between a
   ; cmp and its consumer stays sound), an sp-based one offsetting across the temp's save cell.
   ; ⚠ the cells mirror register semantics only WITHIN a function: every cross-function f-reg
   ; convention was re-routed onto gp pairs upstream, so no cell ever crosses a frame.
   (fcell g f) (f @ 'f0 (peep g 'fc0 0) 'f1 (peep g 'fc1 0)
                   'f15 (peep g 'fc15 0)    (scare 'soften6-freg f))
   (nxr r) (r @ 'r0 'r1 'r1 'r2 'r2 'r3 (scare 'soften6-pair r))
   (pl6 g f lo) (: c (fcell g f) [['ld lo 'r4 c] ['ld (nxr lo) 'r4 (c + 4)]])   ; cell -> gp pair
   (ps6 g f lo) (: c (fcell g f) [['st 'r4 c lo] ['st 'r4 (c + 4) (nxr lo)]])   ; gp pair -> cell
   (m2c g f base off)                                ; [base+off] -> cell (a softened ldsd)
    (: c (fcell g f) tmp (base @ 'r3 'r2 'r3) o2 (base @ 'sp (off + 16) off)
       ['(sub sp sp 16) ['st 'sp 0 tmp]
         ['ld tmp base o2] ['st 'r4 c tmp] ['ld tmp base (o2 + 4)] ['st 'r4 (c + 4) tmp]
         ['ld tmp 'sp 0] '(add sp sp 16)])
   (c2m g base off f)                                ; cell -> [base+off] (a softened stsd)
    (: c (fcell g f) tmp (base @ 'r3 'r2 'r3) o2 (base @ 'sp (off + 16) off)
       ['(sub sp sp 16) ['st 'sp 0 tmp]
         ['ld tmp 'r4 c] ['st base o2 tmp] ['ld tmp 'r4 (c + 4)] ['st base (o2 + 4) tmp]
         ['ld tmp 'sp 0] '(add sp sp 16)])
   (w2c g f base off)                                ; [base+off] -> cell LOW word (a softened ldss)
    (: c (fcell g f) tmp (base @ 'r3 'r2 'r3) o2 (base @ 'sp (off + 16) off)
       ['(sub sp sp 16) ['st 'sp 0 tmp]
         ['ld tmp base o2] ['st 'r4 c tmp]
         ['ld tmp 'sp 0] '(add sp sp 16)])
   (c2w g base off f)                                ; cell LOW word -> [base+off] (a softened stss)
    (: c (fcell g f) tmp (base @ 'r3 'r2 'r3) o2 (base @ 'sp (off + 16) off)
       ['(sub sp sp 16) ['st 'sp 0 tmp]
         ['ld tmp 'r4 c] ['st base o2 tmp]
         ['ld tmp 'sp 0] '(add sp sp 16)])
   ; ONE walker serves both soft-double faces; the TRANSFER KIT is the whole difference. v6m: a
   ; double lives in its frame cell -- gd/pd ride pl6/ps6, the single-word bits read/write the
   ; cell low, movsd copies cell-to-cell through the saved temp, and the raw f-memory ops soften
   ; too. sp64: a double lives in its d-register -- gd/pd are movqrx/movqxr, the single-word bits
   ; bounce through the save cell's spare word, movsd bounces the pair (no VMOV.F64 on SP), and
   ; f-memory ops stay (the FPU loads/stores fine, it just can't ARITH f64).
   (soften g fs)
    (: w6 (v6m? g)
       (? !(w6 || sp64? g) fs
          (: (gd d9 lo) (? w6 (pl6 g d9 lo) [['movqrx lo d9]])   ; double -> the gp pair at lo
             (pd d9) (? w6 (ps6 g d9 'r0) [['movqxr d9 'r0]])   ; the r0:r1 pair -> double
             (gs d9) (? w6 [['ld 'r0 'r4 (fcell g d9)]]         ; SINGLE bits -> r0
                        [['stss 'sp 16 d9] '(ld r0 sp 16)])
             (ps d9) (? w6 [['st 'r4 (fcell g d9) 'r0]]         ; r0 -> the single's home
                        ['(st sp 16 r0) ['ldss d9 'sp 16]])
             (mv dD dS)                                           ; movsd: double -> double
              (? w6 (: cd (fcell g dD) cs (fcell g dS)
                       ['(sub sp sp 16) '(st sp 0 r3)
                         ['ld 'r3 'r4 cs] ['st 'r4 cd 'r3]
                         ['ld 'r3 'r4 (cs + 4)] ['st 'r4 (cd + 4) 'r3]
                         '(ld r3 sp 0) '(add sp sp 16)])
                 ['(sub sp sp 16) ['stsd 'sp 0 dS] ['ldsd dD 'sp 0] '(add sp sp 16)])
             scc (? w6 'soften6-cc 'soften-cc)
             suc (? w6 'soften6-ucomisd 'soften-ucomisd)
             (go (f >< r))
              (? !(two? f) (f >< go r)
                 (: op <f
                    h9 (sf-bin op)
                    (? h9                             ; (op fD fS): fD = fD op fS
                        (sf-save + gd <>f 'r0 +
                         gd <>>f 'r2 + [['call h9]] + pd <>f + sf-rest + go r)
                       (op = 'movsd) (mv <>f <>>f + go r)
                       (w6 && op = 'ldsd) (m2c g <>f <>>f <>>>f + go r)
                       (w6 && op = 'stsd) (c2m g <>f <>>f <>>>f + go r)
                       (w6 && op = 'ldss) (w2c g <>f <>>f <>>>f + go r)
                       (w6 && op = 'stss) (c2w g <>f <>>f <>>>f + go r)
                       (w6 && op = 'movqrx) (pl6 g <>>f <>f + go r)
                       (w6 && op = 'movqxr) (ps6 g <>f <>>f + go r)
                       (member? op '(cvtsi2sd cvtui2sd))   ; (op fD rS)
                        (: sl (sf-slot <>>f)
                           (sf-save +
                            (? (sl < 0) [['mov 'r0 <>>f]] [['ld 'r0 'sp sl]]) +
                            [['call (? (op = 'cvtsi2sd) '__aeabi_i2d '__aeabi_ui2d)]] +
                            pd <>f +
                            sf-rest + go r))
                       (member? op '(cvttsd2si cvttsd2ui))   ; (op rD fS)
                        (: sl (sf-slot <>f)
                           (sf-save + gd <>>f 'r0 +
                            [['call (? (op = 'cvttsd2si) '__aeabi_d2iz '__aeabi_d2uiz)]] +
                            (? (sl < 0) [['mov <>f 'r0]] [['st 'sp sl 'r0]]) +   ; through the save slot: the restore lands it
                            sf-rest + go r))
                       (op = 'cvtss2sd)               ; (op fD fS): single bits in, a pair out
                        (sf-save + gs <>>f + '((call __aeabi_f2d)) + pd <>f + sf-rest + go r)
                       (op = 'cvtsd2ss)               ; (op fD fS): a pair in, the narrowed word home
                        (sf-save + gd <>>f 'r0 + '((call __aeabi_d2f)) + ps <>f + sf-rest + go r)
                       (op = 'ucomisd)
                        (: args (sf-save + gd <>f 'r0 + gd <>>f 'r2)
                           n1 (? (two? r) <r ())
                           (? (two? n1 && id? <n1 'br)
                               (: hs (sf-cc <>n1)
                                  _ (? (two? hs) 0 (scare scc n1))
                                  (args + [['call <hs] '(cmp r0 0)] + sf-rest + [['br <>hs <>>n1]] + go >r))
                              (two? n1 && id? <n1 'set)
                               (: n2 (? (two? >r) <>r ())
                                  n3 (? (two? >r && (two? >>r)) <>>r ())
                                  dance (two? n2 && id? <n2 'set && member? <>n2 '(p np) &&
                                         two? n3 && member? <n3 '(and or))
                                  hs (sf-cc <>n1)
                                  _ (? (two? hs) 0 (scare scc n1))
                                  (args + [['call <hs] '(cmp r0 0)] + sf-rest + [['set <>hs <>>n1]] + go (? dance >>>r >r)))
                              (scare suc n1)))
                       (f >< go r))))
             (go fs))))
   ; the v6-M twin: ARMv6-M POP restores r0..r7 and PC but NOT LR, so the return folds into
   ; `pop {pc}` -- no separate lr-restore + bx. fp is r7 here; r4 is the BOTTOM base (t16slots),
   ; pushed just under fp and popped just before it.
   epi-t16 '((lea sp fp 0) (pop r4) (pop fp) (pop pc))
   ; the v6-M VARARG twin: POP reaches r0..r7/pc only, and pop-pc couldn't drop the r0-r3 block
   ; after it -- so the return address pops into r3 (caller-saved, never a return value) and BX r3
   ; returns once the 16-byte block drops. bit0 of the pushed lr is set, so the BX stays Thumb.
   epi-t16va '((lea sp fp 0) (pop r4) (pop fp) (pop r3) (add sp sp 16) (jmpr r3))
   ; rdsp: the table's dataflow view of one form -- what it reads, what it defines, whether it's
   ; droppable when its defs die. an op off the table answers 'bar (assume everything).
   (rdsp f)                                         ; -> [reads defs pure?] | 'bar
    (: op <f
       (rg1 x) (? (charm? x) () [x])
       (? (op = 'li) [() [<>f] 1]
          (op = 'mov) [[<>>f] [<>f] 1]
          (member? op aluops)
           (? (<>f = 'sp) [() () ()]      ; sp motion touches no gp register
              [(rg1 <>>f + rg1 <>>>f) [<>f] 1])
          (member? op flagops) [(rg1 <>>f + rg1 <>>>f) [<>f] ()]
          (op = 'push) [[<>f] () ()]
          (op = 'pop) [() [<>f] ()]
          (member? op '(shl shr sar rol ror ror4)) [[<>f] [<>f] 1]
          (member? op ldops9) [[<>>f] [<>f] 1]
          (member? op gstops) [[<>f <>>>f] () ()]
          (member? op stxops) [[<>f <>>f (nthr f 5)] () ()]
          (member? op siops) [[<>f] () ()]
          (op = 'cmpm) [([<>f] + rg1 <>>>f) () ()]
          (op = 'cmprm) [[<>f <>>f] () ()]
          (member? op leaops) [[<>>f] [<>f] 1]
          (member? op xops) [[<>>f <>>>f] [<>f] 1]
          (op = 'la) [() [<>f] 1]
          (op = 'set) [() [<>>f] 1]
          (member? op '(cmp test)) [(rg1 <>f + rg1 <>>f) () ()]
          (member? op unops) [[<>f] [<>f] 1]
          (op = 'movqxr) [[<>>f] () ()]
          (op = 'movqrx) [() [<>f] 1]
          (member? op '(cvtsi2sd cvtui2sd)) [[<>>f] () ()]
          (member? op '(cvttsd2si cvttsd2ui)) [() [<>f] ()]
          (member? op fldops) [[<>>f] () ()]
          (member? op fstops) [[<>f] () ()]
          (member? op xmmops) [() () ()]
          (member? op divops)
           [(rg1 <>>f + rg1 <>>>f) '(r0 r1 r2 r3) ()]
          (member? op llops) ['(r0 r1 r2 r3) '(r0 r1 r2 r3) ()]
          (member? op wmops) [[(nthr f 3) (nthr f 4)] [<>f <>>f] 1]
          (op = 'mla) [[<>>f <>>>f (nthr f 4)] [<>f] 1]
          (op = 'clz) [[<>>f] [<>f] 1]
          (member? op vshops) [[<>f <>>f] [<>f 'r1] ()]
          'bar))
   ; the arm64 RETARGET: gen speaks r4 for the frame base throughout, but ⚠ x4 is ALSO an AAPCS
   ; argument register -- so the rename is by POSITION, not by symbol: only the operand slot where
   ; r4 means "the frame base" becomes fp. an r4 anywhere else IS the 5th argument and stays.
   a4base (: t (foldl (\ t o (pin t o 2)) {} (ldops9 + fldops + xops + leaops + '(cmprm)))
           (foldl (\ t o (pin t o 1)) t (stops9 + stxops + siops + '(cmpm))))
   (a4ize fs)
    (: (go (f >< u))
        (: bp (two? f ? (peep a4base <f -1) -1)
         (link (0 <= bp && id? (nthr f bp) 'r4 ? (reop f bp 'fp) f) (go u)))
     (go fs))
   ; v6-M slot retarget: slots live at fp-NEGATIVE offsets, but ARMv6-M loads/stores encode only
   ; POSITIVE immediates -- and a slot must stay STABLE across the sub/add-sp shuffle moon wraps
   ; around calls, so sp can't serve either and fp stays at the frame TOP for the
   ; framesize-independent epilogue. so the v6m frame reserves a SECOND base, r4, planted at the
   ; frame BOTTOM, and this pass rewrites each fp-NEGATIVE slot access to r4 + (frame + off).
   ; frame-LINK forms carry offset 0 and keep fp. runs AFTER a4ize, v6m only.
   (t16reb f i bp fr)
    (? !(two? f) f
       ((? (i = bp) 'r4 (i = (bp + 1)) (fr + <f) <f) >< t16reb >f (i + 1) bp fr))
   (t16slots (f >< u) fr)
       (: bp (two? f ? (peep a4base <f -1) -1)
          (link (? (bp >= 0 && id? (nthr f bp) 'fp && nthr f (bp + 1) < 0) (t16reb f 0 bp fr) f)
                (t16slots u fr)))
   ; v6m has no MOVW/MOVT to build an address, so an (la Rd lbl) becomes a literal-pool load:
   ; align to a word, LDR Rd,[pc,#0], hop the pooled abs32 word, and lay it as an R_ARM_ABS32 the
   ; linker fills. runs LAST (after a4ize + t16slots), v6m only; other targets keep `la`.
   (t16la (f >< u)) ((atom? f || <f != 'la ?  [f]
                      ['(align 4) ['lapool <>f] ['raw [['fix 4 'abs32 <>>f 0]]]])
                     + t16la u)
   ; -- the SIBCALL pass: a RET-position call tail-JUMPS instead, so deep tail recursion runs flat
   ; and the lvm shape Continues by jmp at ai_tco=1 (`make vmret` reads the disassembly). the
   ; rewrite is LOCAL: (call F) followed by exactly the epilogue becomes epilogue + (jmp F) -- the
   ; epilogue parks sp on the return address, a call's entry state, so the callee answers our
   ; caller directly. (callr r3) parks the target in r10 first (the epilogue reloads r3). anything
   ; BETWEEN the call and the epilogue keeps the plain call. ⚠ GATED per fn: any (lea _ r4 _)
   ; means a frame address ESCAPES and the frame must outlive every call, so that fn keeps all its
   ; calls. the escape is tracked at the SOURCE (g 'fesc): clval's lea for an ordinary assignment
   ; is consumed by its own store and stays eligible -- only an address that becomes a VALUE pins.
   (leas? (f >< u)) (two? f && <f = 'lea && <>>f = 'r4 ? 1 (leas? u))
   (escan g r) (: _ (? (two? r && leas? (vfs r)) (pin g 'fesc 1) ()) r)
   (unlean (f >< u))
                     ((two? f && <f = 'lean ? ('lea >< >f) f) >< unlean u)
   (relean (f >< u)) ; the inverse, for a splice that consumes the address ITSELF (the overflow trio's &t)
    ((two? f && (<f = 'lea) ? ('lean >< >f) f) >< relean u)
   (epim? fs es) (? !(two? es) 1                  ; the next forms are EXACTLY es (the target's epilogue)
                    !(two? fs) ()
                    (<fs = <es && epim? >fs >es))
   ; lw = the WINDOW rule: a frame lea seen since the last call boundary means the candidate's OWN
   ; staging may hand the callee a frame address (`return f(&x)`) -- that call stays a call, and
   ; the boundary resets the window. a ternary's JOIN LABEL may sit between the call and the
   ; epilogue: the jump sequence lands BEFORE the labels, and label + epi stay for the other arm.
   (lbl-epi? (f >< u) ej) (two? f && <f = 'label ? (epim? u ej || lbl-epi? u ej))
   ; a JOIN label and then a jmp to a collected epilogue label: the call is still in ret position
   ; (labels carry no code, the jmp leads to the epilogue), so it rewrites like the bare-jmp case,
   ; keeping label + jmp for the arms that join.
   (lbl-jmp? (f >< u) el)
    (two? f ? (<f @ 'label (lbl-jmp? u el)
                    ('jmp && el <>f) 1))
   ; a nested-ternary return leaves each arm as (call F)(jmp JOIN) with JOIN leading to the
   ; epilogue: elabs collects those epilogue labels first, and a candidate followed by (jmp X),
   ; X collected, rewrites too. a label whose region is one (jmp Y) with Y collected is an
   ; epilogue label too, and that chain can point FORWARD -- so elabsx iterates elabs to a
   ; fixpoint (the set only grows, bounded by the fn's labels).
   (jmp-el? (('jmp x) >< _) t) (t x)
   (elabs fs t ej) (? !(two? fs) t
                      (: f <fs
                         _ (? (two? f && (<f = 'label) &&
                               (epim? >fs ej ||
                                lbl-epi? >fs ej || jmp-el? >fs t))
                              (pin t <>f 1) ())
                         (elabs >fs t ej)))
   (elabsx fs t ej) (: n #(keys t)
                       t2 (elabs fs t ej)
                       (#(keys t2) = n ? t2 (elabsx fs t2 ej)))
   (butret es) (two? >es ? (<es >< butret >es))          ; the epilogue's restore forms (the ret drops)
   (sibjmp g f ejx)                                 ; ejx = the epilogue this tail owes, restores first
    ; ⚠ riscv takes the DIRECT tail call through the park too, where the other targets ride `jmp`:
    ; a tail call is the one jmp that leaves its function, and riscv's jal reaches only +-1MB
    ; (rv-jal-range). so it borrows `la`'s reach (+-2GB) and the same t4 park the INDIRECT lane
    ; uses, loaded AFTER the restores so nothing has to survive them.
    (? (rv? g && (<f = 'call))
       (butret (peep g 'epi epi-a64) + [['la (pkr g) <>f] ['jmpr (pkr g)]])
       (<f = 'call) (butret ejx + [['jmp <>f]])
       ; ⚠ ejx, never the epi-a64 CONSTANT: a cs regen pins its restores into g 'epi, and the
       ; constant drops them -- the tail would jump with the callee's seats still dirty. the
       ; t32 line below owed the same read for a second reason: a VARARG t32 epilogue is
       ; epi-t32va, and the constant drops its `(add sp sp 16)` with the arriving block.
       (a64? g) (butret ejx + '((jmpr r15)))                   ; the arm64 epilogue never touches the x15 park
       (rv? g) (butret (peep g 'epi epi-a64) + [['jmpr (pkr g)]])     ; riscv: the park (t4) survives the epilogue
       (t32? g) (butret ejx + [['jmpr (pkr g)]])             ; thumb2: the callr target parks in r12 (pkr), untouched by the epilogue
       ('((mov r10 r3)) + butret ejx + '((jmpr r10))))            ; park first: the x64 epilogue reloads r3
   ; a home reload run ((ld rH r4 slot)..) after a ret-position call is DEAD, and so is a
   ; roster reload into a POOL reg (caller-saved dies at the ret) -- the tail-jump hands the
   ; frame back first. skiprel peels exactly those; the epim? verification downstream keeps
   ; the loose peel honest. ⚠ pool only: a cs-reg ld here is the EPILOGUE's own reload.
   (skiprel g fs hs)
    (? !(two? fs) fs
       (: f <fs
          (? !(two? f && member? <f ldops9 && (<>>f = 'r4)) fs
             (: h (assoc <>f hs)
                (? (two? h && (<>h = <>>>f)) (skiprel g >fs hs)
                   (member? <>f (peep g 'pool0 ())) (skiprel g >fs hs)
                   fs)))))
   (sibs g (f >< r) lw el hs ej crf)
    (: lw2 (two? f && <f = 'lea && <>>f = 'r4 || lw)
       cand (!lw2 && two? f && (<f = 'call && two? >f ||
                                <f = 'callr && id? <>f <>crf))
       rr (? cand (skiprel g r hs) r)
       (? (cand && epim? rr ej)
           (sibjmp g f ej + sibs g (drop #ej rr) () el hs ej crf)
          (cand && lbl-epi? rr ej)
           (sibjmp g f ej + sibs g rr () el hs ej crf)
          (cand && (: nx <rr (two? nx && <nx = 'jmp && peep el <>nx ())))
           (sibjmp g f ej + sibs g >rr () el hs ej crf)
          (cand && lbl-jmp? rr el)
           (sibjmp g f ej + sibs g rr () el hs ej crf)
          (: _ (? (must? f) (scare 'musttail-not-a-tail [(g 'fn) f]) 0)   ; a marked call is OWED its jump
             (f >< sibs g r (? (two? f && member? <f '(call callr raw)) () lw2) el hs ej crf))))       ; raw: a builtin splice consumes its address like a call
   ; -- the musttail contract: a PRE-slotted ret (clay's (ret e PRE)) marks its call --
   ; (call F must) / (callr PK must) -- and sibs spells the marked jump or SCARES
   ; (musttail-not-a-tail; a whole-fn decline musttail-escape) instead of falling back to a
   ; plain call. the marker dies with the rewrite, so no later pass ever meets one. --
   (must? f) (two? f && member? <f '(call callr) && member? 'must >>f)
   (anymust? (a >< u)) (must? a || anymust? u)
   (mustret g fs)                                   ; mark the LAST call/callr; none to mark refuses
    (: (go l) (? !(two? l) 'no
                 (: r (go >l)
                    (? !(id? r 'no) (<l >< r)
                       (: f <l (? (two? f && member? <f '(call callr)) (f + '(must) >< >l) 'no)))))
       r (go fs)
       (? (id? r 'no) (nolane g "a musttail return does not end in a call") r))
   (sibcall g fs) (? (g 'fesc || v6m? g)   ; v6m: the pop-pc epilogue can't restore lr for a tail-jmp, so no sibcall (deferred optimization)
                      (anymust? fs ? (scare 'musttail-escape (g 'fn)), fs)
                     (: ej (peep g 'epi epi)
                      (sibs g fs () (elabsx fs {} ej) (g 'homes) ej ['callr (pkr g)])))

   ; -- FRAME ELISION: a fn never needs the frame LINK (the entry push/mov, the sp-restore pair at
   ; every exit), only its SLOTS. the slots re-address off sp: keep (sub sp K9) with K9 = K + the
   ; link bytes (8 x64, 32 arm64), so every old base+off lands at sp+(off+K) and sp sits exactly
   ; where the framed body held it; each exit pays one (add sp K9). a slotless call-free fn drops
   ; the sub too; a fn with plain calls keeps it always -- sp holds still across a call exactly as
   ; r4 did, and K9 (8 mod 16) hands each callee the alignment push r4 used to. body sp MOTION
   ; rides: scan carries the static displacement D through the charm sp add/subs and proves it at
   ; every label and branch through labt, so a slot inside a span lands at sp+(off+K+D) and each
   ; exit unwinds K9+D. an r3 user (x64) keeps rbx's save/restore as plain sp slots.
   ; still framed: a raw splice (baked bytes can hide frame addressing), a bare push/pop, a
   ; non-charm sp write (alloca), any a64 call (bl clobbers lr), a call at a D off 16, and the
   ; frame base anywhere OUTSIDE a base slot. ⚠ rbp is retired everywhere it can be: the glaze ABI
   ; rides Ip in rbp, so a framed fn that calls into the VM faults on its own slots on return. --
   uoff (: t (foldl (\ t o (pin t o 3)) {} (ldops9 + fldops + leaops + '(cmprm)))
           t (foldl (\ t o (pin t o 5)) t xops)
           t (foldl (\ t o (pin t o 2)) t (stops9 + siops + '(cmpm)))
         (foldl (\ t o (pin t o 4)) t stxops))
   ; rebuild a form with the operand at position i9 replaced -- the a4base/uoff seam
   ; every frame-rewriting pass (a4ize, unframe, repack) lands on
   (reop f i9 v) (: (go (a >< u) i) ((i = i9 ? v a) >< go u (i + 1)) (go f 0))
   ; the riscv slot rebase: c.ld/c.sd encode an UNSIGNED offset, so a slot read off a frame base
   ; at the frame TOP can never wear a 2-byte form. riscv links fp at the frame BOTTOM instead
   ; (the prologue subs BEFORE it links), which moves fp down by exactly `frame` -- so every
   ; fp-relative displacement gains it. that is the whole pass: slots go positive, the cs
   ; restores follow, a taken frame address stays the same address, and the epilogue's own
   ; (lea sp fp 0) becomes the framesize hop that walks sp back up over the locals.
   ; runs AFTER a4ize, riscv only.
   ; ⚠ the displacement is uoff's position, NEVER bp + 1: an indexed op carries its INDEX
   ; REGISTER there and its displacement at 5, so bp + 1 would add the frame to a register name.
   (rvreb f i up fr)
    (? !(two? f) f
       ((? (i = up) (charm? <f ? (fr + <f) (scare 'rv-slot-nonum f)) <f)
          >< rvreb >f (i + 1) up fr))
   (rvslots fs fr)
    (? !(two? fs) ()
       (: f <fs
          bp (two? f ? (peep a4base <f -1) -1)
          up (two? f ? (peep uoff <f -1) -1)
          (link (bp >= 0 && up >= 0 && nthr f bp = 'fp ? (rvreb f 0 up fr) f)
                (rvslots >fs fr))))
   ; the x64 frame prologue at fs's head (unframe strips it; repack requires it)
   (pro4? fs) (nthr fs 1 = '(push r4) && nthr fs 2 = '(mov r4 sp))
   (unframe g fs)
    (: a64 (a64? g)
       fb (? a64 'fp 'r4)
       pro (? a64
              (nthr fs 1 = '(push lr) && nthr fs 2 = '(push fp) && nthr fs 3 = '(lea fp sp 0))
              (pro4? fs))
       sf (nthr fs (? a64 4 3))
       K (two? sf && <sf = 'sub && <>sf = 'sp && <>>sf = 'sp && charm? <>>>sf ? <>>>sf)
       body0 (drop (? a64 5 4) fs)
       ; -- the PARK lane (x64): a callr sib parks its target in r3 and unparks with
       ; (mov r10 r3); when everything between them neither mentions r3 nor rewrites the parked
       ; SOURCE, the pair collapses to one mov at the unpark's seat -- the fn then never WRITES
       ; r3, so the caller's rbx rides the tail-jump untouched and the rbx save + each restore's
       ; leading (ld r3 r4 -8) drop with the frame. a residual r3 keeps them as sp slots. --
       sv3 (!a64 && nthr body0 0 = '(st r4 -8 r3))
       wind ('(mov li) + leaops + xops + aluops + ldops9)
       wina (wind + gstops + '(cmp test))
       (squeeze (f >< r))
        (? !(two? f && <f = 'mov && id? <>f 'r3 && !(id? <>>f 'sp))
           (f >< squeeze r)
           (: X <>>f
              (win w n)                        ; forms between park and unpark -> count | 'no
               (? (!(two? w) || 8 < n) 'no
                  (: g2 <w
                     (? (g2 = '(mov r10 r3)) n
                        !(two? g2) 'no
                        (member? 'r3 >g2) 'no
                        !(member? <g2 wina) 'no
                        (member? <g2 wind && id? <>g2 X) 'no
                        (win >w (n + 1)))))
              k (win r 0)
              (? (id? k 'no) (f >< squeeze r)
                 (: (take w n) (? (n < 1) (['mov 'r10 X] >< squeeze (drop 1 w))
                                  (<w >< take >w (n - 1)))
                    (take r k)))))
       body (: b2 (? sv3 (drop 1 body0) body0) (? a64 b2 (squeeze b2)))
       (epi3? l9)                                    ; l9 opens with a restore run -> its length | ()
        (? a64 (? (nthr l9 0 = '(lea sp fp 0) && nthr l9 1 = '(pop fp) && nthr l9 2 = '(pop lr)) 3 ())
           (nthr l9 0 = '(mov sp r4) && nthr l9 1 = '(pop r4)) 2
           (nthr l9 0 = '(ld r3 r4 -8) && nthr l9 1 = '(mov sp r4) && nthr l9 2 = '(pop r4)) 3
           ())
       (clean? f2 bp)                                ; fb only at its base slot; a64 lr never
        (: (go e i) (? !(two? e) 1
                       (: x <e
                          (? (id? x fb && !(i = bp)) ()
                             (a64 && id? x 'lr) ()
                             (go >e (i + 1)))))
           (go f2 0))
       labt {}                               ; label -> the D every arrival agrees on
       (scan l D vs r3f live)                        ; -> 'no | (needs-sub r3-in-use)
        (? !(two? l) [vs r3f]
           (: n3 (epi3? l)
              (? n3 (scan (drop n3 l) 0 vs r3f live)
                 (: f <l
                    (? !(two? f) (scan >l D vs r3f live)
                       (<f = 'label)
                        (: d9 (peep labt <>f 'no9)
                           (? !(id? d9 'no9)
                              (? (live && !(D = d9)) 'no (scan >l d9 vs r3f 1))
                              (: D2 (? live D 0)
                                 _ (pin labt <>f D2)
                                 (scan >l D2 vs r3f 1))))
                       (<f = 'jmp)
                        (: d9 (peep labt <>f 'no9)
                           (? (id? d9 'no9) (: _ (pin labt <>f D) (scan >l D vs r3f ()))
                              !(D = d9) 'no
                              (scan >l D vs r3f ())))
                       (<f = 'br)
                        (: d9 (peep labt <>>f 'no9)
                           (? (id? d9 'no9) (: _ (pin labt <>>f D) (scan >l D vs r3f live))
                              !(D = d9) 'no
                              (scan >l D vs r3f live)))
                       (member? <f '(ret jmpr))
                        (scan >l D vs (r3f || (!a64 && member? 'r3 >f)) ())
                       (member? <f '(raw push pop)) 'no
                       (member? <f '(call callr))                 ; a plain call RIDES: sp is as
                        ; call-stable as r4 was, the callee keeps rbx, and the sub (K9 is 8 mod
                        ; 16) + a 16-multiple D hand the ABI exactly what push r4 did
                        (? a64 'no                                ; bl clobbers lr: the link stays
                           !(0 = (D & 15)) 'no
                           (scan >l D 1 (r3f || member? 'r3 >f) live))
                       (spadj? f && <f = 'sub) (scan >l (D + <>>>f) vs r3f live)
                       (spadj? f && <f = 'add)
                        (? ((D - <>>>f) < 0) 'no (scan >l (D - <>>>f) vs r3f live))
                       (member? <f '(sub add mov lea lean) && <>f = 'sp) 'no
                       (: bp (peep a4base <f -1)
                          (? !(clean? f bp) 'no
                             (scan >l D
                                   (? (0 <= bp && id? (nthr f bp) fb) 1 vs)
                                   (r3f || (!a64 && member? 'r3 >f))
                                   live))))))))
       vr (? (pro && charm? K) (scan body 0 () () 1) 'no)
       (? (id? vr 'no || (!a64 && <>vr && !sv3)) fs    ; r3 in use with no save to ride
          (: r3f (!a64 && <>vr)
             vs (<vr || r3f)                         ; the save is a slot: r3f forces the sub
             K9 (K + (? a64 32 8))
             (radj f2 bp op9 D)                      ; the base slot -> sp, its displacement -> off+K+D
              (reop (reop f2 bp 'sp) op9 ((nthr f2 op9) + K + D))
             (rw l D)
              (? !(two? l) ()
                 (: n3 (epi3? l)
                    (? n3 ((? r3f [['ld 'r3 'sp ((K + D) - 8)]] ())
                           + ((? vs [['add 'sp 'sp (K9 + D)]] (? (0 < D) [['add 'sp 'sp D]] ()))
                              + rw (drop n3 l) 0))
                       (: f <l
                          (? !(two? f) (f >< rw >l D)
                             (<f = 'label) (f >< rw >l (peep labt <>f D))
                             (spadj? f && <f = 'sub) (f >< rw >l (D + <>>>f))
                             (spadj? f && <f = 'add) (f >< rw >l (D - <>>>f))
                             (: bp (peep a4base <f -1)
                                (link (? (0 <= bp && id? (nthr f bp) fb)
                                         (radj f bp (peep uoff <f -1) D)
                                         f)
                                      (rw >l D))))))))
             body2 (? r3f ('(st r4 -8 r3) >< body) body)
             (<fs >< (? vs (['sub 'sp 'sp K9] >< rw body2 0) (rw body2 0))))))

   ; -- IR constant propagation (cfoldir): a straight-line abstract pass over a function's final
   ; forms. it tracks known small constants in registers, frame slots and machine-stack cells,
   ; folds ALU ops on them back to li's, turns reg-reg ALU with a known source into the imm form,
   ; folds loads of known slots, and evaluates known cmp+br / cmp+set pairs (a constant ternary
   ; drops its dead arm) -- so the sizeof/Width/Have arithmetic the inliner exposes collapses.
   ; knowns stay in [0, 2^30] (compare signedness and shifts agree there); labels and calls
   ; invalidate values; anything unrecognized invalidates EVERYTHING (new ops stay safe). --
   kmax 1073741824
   (kok? v) (0 <= v && v < kmax)
   (cfoldir g fs0)
    (: fb (fbase g)                         ; the frame-slot base (r4, or fp past a4ize)
       (fresh z9) {}                        ; reg/slot/cell -> (1 K) const | (2 rX) copy-of-register | ()
       blunt9 (vshops + unops)                      ; folded bluntly (see their arm below)
       (rv t k) (t k)
       (rset t k v) (pin t k v)
       (kn? v) (two? v && <v = 1)            ; a known CONSTANT (never let a copy reach arithmetic)
       ; resolve a source register to its deepest value: a const rides, a copy
       ; chases to its ROOT register (valid until that register is written)
       (rsv rg s) (: v (rv rg s) (two? v ? v [2 s]))
       ; the copy INDEX ci: source register -> the (tag key) sites holding a copy of it. ⚠ a WRITE
       ; to d must invalidate every copy-of-d anywhere -- the index visits only the recorded sites
       ; (each validated against the live tablet, so a stale entry is harmless)
       (crec ci v tg k) (two? v && <v = 2 ?  (pin ci <>v ([tg k] >< peep ci <>v ())))
       (kild ci rg sl ce d)
        (: (go1 (e >< u))
                       (: t (<e @ 'g rg 's sl ce)
                          v (t <>e)
                          _ (two? v && <v = 2 && <>v = d ? (pin t <>e ()))
                          (go1 u))
           _ (go1 (ci d))
           (pin ci d ()))
       (defr ci rg sl ce d) (rset rg d (), kild ci rg sl ce d)
       ; rebuild the index for freshly JOINED tablets (small: agreed entries only)
       (mkci rg sl ce)
        (: ci {}
           (scan1 t tg (a >< u)) (crec ci (t a) tg a, scan1 t tg u)
           _ (scan1 rg 'g (keys rg)) _ (scan1 sl 's (keys sl)) _ (scan1 ce 'c (keys ce))
           ci)
       (copyt t) (: t2 {}
                    (go1 ks) (? !(two? ks) t2 (pin t2 <ks (t <ks), go1 >ks))
                  (go1 (keys t)))
       ; the JOIN of two abstract states: a key survives only where both sides
       ; know it and agree (the meet over the flat lattice) -- a fresh tablet
       (joint ta tb) (: t2 {}
                        (go1 ks) (? !(two? ks) t2
                                    (: k1 <ks va (peep ta k1 ()) vb (peep tb k1 ())
                                       _ (? (two? va && two? vb &&
                                             <va = <vb && <>va = <>vb)
                                            (pin t2 k1 va))
                                       (go1 >ks)))
                        (go1 (keys ta)))
       (alu op a b) (op @ 'add (a + b) 'sub (a - b) 'imul (a * b)
                          'and (a & b) (a | b))  ; xor stays unfolded (imm-converted only)
       (ccev cc a b) (cc @ 'eq (a = b) 'ne !(a = b)
                           ('lt || 'below) (a < b)
                           ('le || 'be) (a <= b)
                           ('gt || 'above) (b < a)
                           (b <= a))                    ; ge / ae
       ; after an unconditional transfer, everything to the next label is DEAD
       ; (a jump can only land on a label) -- a folded ternary drops its arm
       (swallow fs) (? !(two? fs) ()
                       (: f <fs (? (two? f && <f = 'label) fs (swallow >fs))))
       (atlab? fs l) (two? fs && (: f <fs (two? f && <f = 'label && <>f = l)))
       ; back-edge labels (a jmp/br sitting BELOW its target): a loop head joins
       ; an edge this one linear pass never computes -- assume nothing there
       backs (: seen {} bt {}
                (bscan l) (? !(two? l) bt
                             (<l @ ('label a) (pin seen a 1)
                                   ('jmp j)   (seen j ? (pin bt j 1))
                                   ('br _ b)  (seen b ? (pin bt b 1)),
                              bscan >l))
                (bscan fs0))
       ; the pending in-states: every FORWARD jmp/br to L joins its state into pend[L]; L's label
       ; reads it back and joins the fall-through, so a known value CROSSES a label. ⚠ the sp
       ; cursor must agree or the cells drop.
       pend {}
       (pmerge l cur rg sl ce ci)
        (: old (peep pend l ())
           (? !(two? old) (pin pend l [cur (copyt rg) (copyt sl) (copyt ce) (copyt ci)])
              (: rg2 (joint <>old rg) sl2 (joint <>>old sl)
                 nce (? (<old = cur) (joint <>>>old ce) (fresh 0))
                 (pin pend l [<old rg2 sl2 nce (mkci rg2 sl2 nce)]))))
       ; lv: linearly LIVE -- () right after an unconditional transfer (the
       ; label then reads only its jump-in state, never the stale linear one)
       (go fs lv cur rg sl ce ci acc)
        (? !(two? fs) (rev acc)
           (: f <fs r >fs
              op (two? f ? <f)
              (? (op = 'label)
                  (: l1 <>f acc2 (f >< acc)
                     (? (peep backs l1 ()) (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) acc2)
                        (: pd (peep pend l1 ())
                           (? !(two? pd)
                               (? lv (go r 1 cur rg sl ce ci acc2)
                                  (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) acc2))
                              !lv (go r 1 <pd <>pd <>>pd <>>>pd
                                         <>>>>pd acc2)
                              (: rg2 (joint <>pd rg) sl2 (joint <>>pd sl)
                                 ce2 (? (<pd = cur) (joint <>>>pd ce) (fresh 0))
                                 (go r 1 <pd rg2 sl2 ce2 (mkci rg2 sl2 ce2) acc2))))))
                 (member? op barops)
                  (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) (f >< acc))
                 (op = 'jmp)                         ; drop the dead tail; a jmp to the very next label falls through
                  (: r2 (swallow r)
                     (? (atlab? r2 <>f) (go r2 1 cur rg sl ce ci acc)
                        (: _ (pmerge <>f cur rg sl ce ci)
                           (go r2 () cur rg sl ce ci (f >< acc)))))
                 (op = 'br)                          ; the taken edge records; the fall-through rides on
                  (: _ (pmerge <>>f cur rg sl ce ci)
                     (go r 1 cur rg sl ce ci (f >< acc)))
                 (member? op '(jmpr ret)) (go (swallow r) () cur rg sl ce ci (f >< acc))
                 (op = 'li)
                  (: nx (? (two? r) <r ())
                     (? (two? nx && member? <nx '(li mov) && <>nx = <>f)
                        (go r 1 cur rg sl ce ci acc)    ; the very next form rewrites this register: dead li
                        (: k <>>f
                           old (rv rg <>f)
                           (? (kn? old && <>old = k)
                              (go r 1 cur rg sl ce ci acc)   ; the register already holds this value
                              (: _ (defr ci rg sl ce <>f)
                                 _ (rset rg <>f (? (kok? k) [1 k] ()))
                                 (go r 1 cur rg sl ce ci (f >< acc)))))))
                 (op = 'mov)
                  (? (id? <>f <>>f) (go r 1 cur rg sl ce ci acc)   ; a self-mov says nothing
                     (: v (rsv rg <>>f)
                        old (rv rg <>f)
                        ; a source whose copy chases to the DEST restores the dest's own value.
                        ; ⚠ never on sp/fp/r4: sp moves through the cell cursor, not defr, so a
                        ; copy-of-sp record outlives the motion
                        (? ((two? old && two? v && <old = <v && <>old = <>v) ||
                            (two? v && <v = 2 && id? <>v <>f && !(member? <>f '(sp fp r4 r3))))
                             (go r 1 cur rg sl ce ci acc)     ; the dest already holds this value
                           (: _ (defr ci rg sl ce <>f)
                              _ (rset rg <>f v)
                              _ (crec ci v 'g <>f)
                              (go r 1 cur rg sl ce ci (f >< acc))))))
                 (member? op '(add sub and or xor imul))
                  (? (<>f = 'sp)                ; sp arithmetic: the cell cursor moves
                      (: n <>>>f
                         (go r 1 (? (op = 'add) (cur - n) (cur + n)) rg sl ce ci (f >< acc)))
                     (: d <>f a <>>f b <>>>f
                        ; b is a register SYMBOL or a charm immediate -- never confuse
                        ; the two (a symbol in arithmetic church-explodes)
                        bk (? (charm? b) (? (kok? b) [1 b] ()) (rv rg b))
                        ak (rv rg a)
                        (? (!(op = 'xor) && kn? ak && kn? bk)           ; both known: the op folds to a li
                            (: k (alu op <>ak <>bk)
                               _ (defr ci rg sl ce d)
                               (? (kok? k)
                                  (: _ (rset rg d [1 k])
                                     (go r 1 cur rg sl ce ci (['li d k] >< acc)))
                                  (go r 1 cur rg sl ce ci (f >< acc))))
                           (!(charm? b) && kn? bk && imma g op <>bk)   ; known reg source -> the imm form
                            (: _ (defr ci rg sl ce d)
                               (go r 1 cur rg sl ce ci ([op d a <>bk] >< acc)))
                           (: _ (defr ci rg sl ce d)
                              (go r 1 cur rg sl ce ci (f >< acc))))))
                 (member? op '(shl shr sar))         ; the 2-operand immediate shifts
                  (: d <>f k <>>f dk (rv rg d)
                     _ (defr ci rg sl ce d)
                     (? (kn? dk && charm? k)
                         (: v (? (op = 'shl) (* <>dk (k 2)) (<>dk >> k))
                            (? (kok? v)
                               (: _ (rset rg d [1 v])
                                  (go r 1 cur rg sl ce ci (['li d v] >< acc)))
                               (go r 1 cur rg sl ce ci (f >< acc))))
                        (go r 1 cur rg sl ce ci (f >< acc))))
                 (op = 'st)
                  (? (<>f = fb)
                      (: v9 (rsv rg <>>>f) _ (rset sl <>>f v9) _ (crec ci v9 's <>>f)
                         (go r 1 cur rg sl ce ci (f >< acc)))
                     (<>f = 'sp)
                      ; keyed by DEPTH (cur - off), so a cell's two words track apart (the wide
                      ; kit stores [sp] and [sp+8] into one 16-byte cell)
                      (: v9 (rsv rg <>>>f) _ (rset ce (cur - <>>f) v9) _ (crec ci v9 'c (cur - <>>f))
                         (go r 1 cur rg sl ce ci (f >< acc)))
                     (go r 1 cur rg (fresh 0) ce ci (f >< acc)))     ; a pointer store may alias the frame
                 (op = 'ld)
                  (: d <>f b <>>f off <>>>f
                     kv (? (b = fb) (rv sl off) (b = 'sp) (rv ce (cur - off)) ())
                     (? (kn? kv)
                        (: _ (defr ci rg sl ce d)
                           _ (rset rg d kv)
                           (go r 1 cur rg sl ce ci (['li d <>kv] >< acc)))
                        (two? kv && (<kv = 2))     ; the memory holds a live COPY: read the register
                         (? (id? <>kv d)
                            (go r 1 cur rg sl ce ci acc)   ; ... and it IS this register: the load says nothing
                            (: _ (defr ci rg sl ce d)
                               _ (rset rg d kv)
                               _ (crec ci kv 'g d)
                               (go r 1 cur rg sl ce ci (['mov d <>kv] >< acc))))
                        (: _ (defr ci rg sl ce d)
                           (go r 1 cur rg sl ce ci (f >< acc)))))
                 (op = 'push)                        ; cell keys are LEDGER depths: a64's 16-byte
                                                     ; push rides the same 8 (nothing addresses push padding)
                  (: cur2 (cur + 8)
                     v9 (rsv rg <>f)
                     _ (rset ce cur2 v9) _ (crec ci v9 'c cur2)
                     (go r 1 cur2 rg sl ce ci (f >< acc)))
                 (op = 'pop)
                  (: v (rv ce cur)
                     _ (defr ci rg sl ce <>f)
                     _ (rset rg <>f v)
                     _ (crec ci v 'g <>f)
                     (go r 1 (cur - 8) rg sl ce ci (f >< acc)))
                 (member? op >stops9)                ; the full-word st has its own arm above
                  (? ((<>f = fb) || (<>f = 'sp))
                      (: _ (? (<>f = fb) (rset sl <>>f ()) (rset ce cur ()))
                         (go r 1 cur rg sl ce ci (f >< acc)))
                     (go r 1 cur rg (fresh 0) ce ci (f >< acc)))
                 (member? op blunt9)                 ; DELIBERATELY blunter than rdsp's write set:
                  ; the r0..r3 clamp stays as-is -- widening it is a precision change
                  (: _ (defr ci rg sl ce 'r0) _ (defr ci rg sl ce 'r1) _ (defr ci rg sl ce 'r2) _ (defr ci rg sl ce 'r3)
                     (go r 1 cur rg sl ce ci (f >< acc)))
                 (op = 'cmp)                         ; paired with its br/set consumer
                  (: a (rv rg <>f)
                     b (? (charm? <>>f) (? (kok? <>>f) [1 <>>f] ())
                          (rv rg <>>f))
                     nx (? (two? r) <r ())
                     (? (kn? a && kn? b && two? nx && member? <nx '(br set))
                         (: tv (ccev <>nx <>a <>b)
                            (? (<nx = 'br)
                               (? tv (: r2 (swallow >r)
                                        (? (atlab? r2 <>>nx) (go r2 1 cur rg sl ce ci acc)
                                           (: _ (pmerge <>>nx cur rg sl ce ci)
                                              (go r2 () cur rg sl ce ci (['jmp <>>nx] >< acc)))))
                                     (go >r 1 cur rg sl ce ci acc))
                               (: d <>>nx k (? tv 1 0)
                                  _ (defr ci rg sl ce d)
                                  _ (rset rg d [1 k])
                                  (go >r 1 cur rg sl ce ci (['li d k] >< acc)))))
                        (go r 1 cur rg sl ce ci (f >< acc))))
                 ; every other tabled op writes exactly rdsp's defs -- fold through the write set
                 ; (empty defs leave every record standing); off the table, assume nothing
                 ; survives. ⚠ the table's defs are REGISTER effects only, so a memory-writing op
                 ; needs its own arm above, as the stores have.
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) (f >< acc))
                       (: _ ((: (dfs ds) (? !(two? ds) 1
                                            (: _ (defr ci rg sl ce <ds) (dfs >ds))) dfs) <>rd)
                          (go r 1 cur rg sl ce ci (f >< acc))))))))
       (go fs0 1 0 (fresh 0) (fresh 0) (fresh 0) (fresh 0) ()))

   ; dead CELLS: after the copy folds, a spush cell whose load folded away is write-only -- its
   ; store drops and its sub/add RESERVATION goes with it. a record STACK mirrors the sp motion
   ; exactly (16-byte cells, 8-byte pushes, pads, multi-slot cleans), and ⚠ ANY shape outside the
   ; model bails the whole fn. a record born from a sub and unwound by a matching add CANCELS when
   ; its cell is unread, its size keeps the 16-alignment every call rides, and nothing observed sp
   ; while it was open -- sp-relative addressing is translation-invariant, so nested live cells
   ; ride through unmoved. a LIVE cell under the same guards CONVERTS instead: its parked value
   ; moves to a fresh frame slot (the prologue sub grows) and the pair still cancels -- one slot
   ; per record depth serves the whole fn, since same-depth cells never overlap in time.
   ; an op outside the model that MENTIONS sp (or an opaque raw) dirties every open record rather
   ; than bailing -- the (mov sp r4) epilogue is a PATH exit on the linear walk, and its
   ; sp-mention keeps the frame's own record dirty.
   (deadcell g fs)
    (: pex (? (arm? g) '(fp lr) '(r4))       ; the epilogue's teardown pops
       cand {}                               ; st-index -> 1 while still droppable
       clds {}                               ; st-index -> its cell's ld indexes
       dirty {}                              ; sub-index -> 1: the span observed sp
       (dirt st2) (? !(two? st2) 1
                     (: _ (? (charm? <>><st2) (pin dirty <>><st2 1) ())
                        (dirt >st2)))
       (scan l i stk sw)                             ; -> the (sub st k add depth) entries | 'no
        (? !(two? l) sw
           (: f <l r >l
              (? !(two? f) (scan r (i + 1) stk sw)
                 (<f = 'sub && <>f = 'sp && charm? <>>>f)
                  (: k <>>>f
                     nx (two? r ? <r)
                     (? (k = 16 && two? nx && <nx = 'st &&
                         <>nx = 'sp && <>>nx = 0)
                        (: _ (pin cand (i + 1) 1)
                           (scan >r (i + 2) ([(i + 1) 16 i (tally stk)] >< stk) sw))
                        (scan r (i + 1) ([() k i (tally stk)] >< stk) sw)))
                 (<f = 'add && <>f = 'sp && charm? <>>>f)
                  (: (unwind st2 k w)
                      (? (k = 0) [st2 w]
                         !(two? st2) 'no
                         (: rc <st2 sz <>rc
                            (? (k < sz) 'no
                               (unwind >st2 (k - sz)
                                (? (charm? <>>rc) ([<>>rc <rc sz i (nthr rc 3)] >< w) w)))))
                     r9 (unwind stk <>>>f ())
                     (? (id? r9 'no) 'no (scan r (i + 1) <r9 (<>r9 + sw))))
                 (<f = 'push) (scan r (i + 1) ([() 8 () ()] >< stk) sw)
                 (<f = 'pop)
                  (? (member? <>f pex) (scan r (i + 1) stk sw)   ; the epilogue's frame pops: path exit
                     (two? stk && !<<stk && (<><stk = 8))
                     (scan r (i + 1) >stk sw)
                     'no)
                 (member? <f ldops9 && (<>>f = 'sp))
                  (? (two? stk && <<stk && (<>>>f = 0))
                     (: stx <<stk
                        _ (pin cand stx ())              ; loaded: the store lives
                        _ (pin clds stx (i >< peep clds stx ()))
                        (scan r (i + 1) stk sw))
                     'no)
                 (member? <f stops9 && (<>f = 'sp)) 'no
                 (member? <f fldops && (<>>f = 'sp)) 'no
                 (: _ (? ((<f = 'raw) || member? 'sp >f) (dirt stk) ())
                    (scan r (i + 1) stk sw)))))
       ok (scan fs 0 () ())
       (? (id? ok 'no) fs
          (: subdrop {} addsh {}     ; sub-index -> drop; add-index -> shrink bytes
             stconv {} ldconv {}     ; st/ld-index -> the slot's positive offset
             pf3 (nthr fs 3)                         ; the prologue's frame sub: (sub sp sp F)
             cvok (pro4? fs &&
                   two? pf3 && (<pf3 = 'sub) && (<>pf3 = 'sp) && (<>>pf3 = 'sp) && charm? <>>>pf3)
             F (? cvok <>>>pf3 0)
             (mark w cv) (? !(two? w) cv
                            (: e <w sub (nthr e 0) st (nthr e 1) k (nthr e 2) ad (nthr e 3)
                               (? (peep dirty sub ()) (mark >w cv)
                                  !(0 = k % 16) (mark >w cv)
                                  (!st || peep cand st ())
                                   (: _ (pin subdrop sub 1)
                                      _ (pin addsh ad (peep addsh ad 0 + k))
                                      (mark >w cv))
                                  (cvok && k = 16) (mark >w (e >< cv))
                                  (mark >w cv))))
             cvl (mark ok ())
             ds (: (dd l acc) (? !(two? l) acc
                                 (: d (nthr <l 4)
                                    (dd >l (? (member? d acc) acc (d >< acc)))))
                   (dd cvl ()))
             (rank d l n) (? (id? <l d) n (rank d >l (n + 1)))
             _ ((: (cvt l) (? !(two? l) 1
                              (: e <l
                                 o ((F + 8) + (8 * rank (nthr e 4) ds 0))
                                 _ (pin subdrop (nthr e 0) 1)
                                 _ (pin addsh (nthr e 3) (peep addsh (nthr e 3) 0 + 16))
                                 _ (pin stconv (nthr e 1) o)
                                 _ ((: (pl ls) (? !(two? ls) 1
                                                  (: _ (pin ldconv <ls o) (pl >ls))) pl)
                                    (peep clds (nthr e 1) ()))
                                 (cvt >l))) cvt) cvl)
             F2 ((F + 8 * tally ds + 15) & -16)
             ; ⚠ a converted cell is a frame OBJECT and must join the slot map: every pass
             ; that reads it (deadst's granule marks, repack's scan) meets an r4 touch no
             ; object claims otherwise and bars the whole fn -- 77% of love.c's frame
             ; traffic sat behind exactly that. One 8-byte slot per record depth, the same
             ; offset cvt hands the st/ld.
             _ (foldl (\ _ d (: o ((F + 8) + (8 * rank d ds 0))
                                (pin g 'slots ([(0 - o) 8] >< peep g 'slots ()))))
                      0 ds)
             (drop2 l i) (? !(two? l) ()
                            (? (peep cand i ()) (drop2 >l (i + 1))
                               (peep subdrop i ()) (drop2 >l (i + 1))
                               (: f2 <l
                                  o9 (peep stconv i ())
                                  ol (peep ldconv i ())
                                  s (peep addsh i 0)
                                  (? o9 (['st 'r4 (0 - o9) <>>>f2] >< drop2 >l (i + 1))
                                     ol ([<f2 <>f2 'r4 (0 - ol)] >< drop2 >l (i + 1))
                                     (two? cvl && i = 3) (['sub 'sp 'sp F2] >< drop2 >l (i + 1))
                                     (s = 0) (f2 >< drop2 >l (i + 1))
                                     (: k2 (<>>>f2 - s)
                                        (? (k2 = 0) (drop2 >l (i + 1))
                                           (['add 'sp 'sp k2] >< drop2 >l (i + 1))))))))
             (drop2 fs 0))))

   ; dead DEFS: within a basic block, a PURE definition whose register is re-defined before any
   ; read is dead -- the copy folds strand whole li/mov chains this way. one backward walk, where
   ; every control edge (and every op outside the tables) resets the dead set to "all live".
   ; dead_before = (dead + defs) - reads.
   (deaddef fs)
    (: (go l dead acc)                               ; l already REVERSED; acc rebuilds forward
        (? !(two? l) acc
           (: f <l r >l
              (? !(two? f) (go r dead (f >< acc))
                 (: rd (rdsp f)
                    (? (id? rd 'bar) (go r {} (f >< acc))
                       (: reads <rd defs <>rd pure <>>rd
                          (dd? ds) (? !(two? ds) 1
                                      (? (peep dead <ds ()) (dd? >ds) ()))
                          (? (pure && two? defs && dd? defs)
                             (go r dead acc)                     ; every def overwritten unread: drop
                             (: _ ((: (ad ds) (? !(two? ds) 1
                                                 (: _ (? (id? <ds 'sp) () (pin dead <ds 1))
                                                    (ad >ds))) ad) defs)
                                _ ((: (rm rs) (? !(two? rs) 1
                                                 (: _ (pin dead <rs ()) (rm >rs))) rm) reads)
                                (go r dead (f >< acc))))))))))
       (go (rev fs) {} ()))

   ; -- the arrival rename (the clang differential's home-shuffle bucket): a
   ; a call-free fn's param homes copy the arg registers up front (mov H A) and copy back at every
   ; staged tail (mov A H) -- pure shuffle whenever the arrival register was never otherwise
   ; touched. jr0 runs first: a park (mov P X) riding straight-line to its (jmpr P) retargets to
   ; (jmpr X), since the park's fresh def of a home register would block its pair. then unhome
   ; discovers each (H A) pair off the leading movs and walks once, bail-happy -- any OTHER
   ; mention of A kills the pair, as does a FRESH def of H, while an H def that READS H is the
   ; value evolving and renames sound. a survivor renames H -> A everywhere, entry mov and
   ; restores fall out as self-movs, and the param rides its ARRIVAL register end to end. --
   (jr0 fs)
    (? !(two? fs) ()
       (: f <fs
          (? !(two? f && (<f = 'mov) && !(charm? <>>f))
              (f >< jr0 >fs)
             (: P <>f X <>>f
                (scan w n)                            ; forms to the jmpr -> n | 'no
                 (? (!(two? w) || (10 < n)) 'no
                    (: f2 <w
                       (? !(two? f2) 'no
                          ((<f2 = 'jmpr) && id? <>f2 P) n
                          (: rd (rdsp f2)
                             (? (id? rd 'bar) 'no
                                (member? P >f2) 'no
                                (member? X <>rd) 'no
                                (scan >w (n + 1)))))))
                k (scan >fs 0)
                (? (id? k 'no) (f >< jr0 >fs)
                   (: (mend w i) (? (i = k) (['jmpr X] >< jr0 >w)
                                    (<w >< mend >w (i + 1)))
                      (mend >fs 0)))))))
   (unhome g fs)
    (: ar (argr g) hr (hregs g)
       (pears l n acc)                                ; the leading (mov H A) movs -> ((H A)..)
        (? (!(two? l) || (8 < n)) acc
           (: f <l
              ; ⚠ a body label ends the entry: a (mov H A) past it is loop-reachable (a splice
              ; park's per-iteration RESET wears this exact shape) and renaming erases the reset
              (? (two? f && (<f = 'label) && (0 < n)) acc
                 (two? f && (<f = 'mov) &&
                  member? <>f hr && member? <>>f ar)
                 (pears >l (n + 1) ([<>f <>>f] >< acc))
                 (pears >l (n + 1) acc))))
       (callish l) (? !(two? l) ()
                      (: f <l (? (two? f && member? <f barops) 1 (callish >l))))
       (ok? H A l seen)
        (? !(two? l) 1
           (: f <l
              (? !(two? f) (ok? H A >l seen)
                 (!seen && (<f = 'mov) && id? <>f H && id? <>>f A)
                  (ok? H A >l 1)                      ; the entry mov
                 (seen && (<f = 'mov) && id? <>f A && id? <>>f H)
                  (ok? H A >l seen)                   ; a staging restore
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (? (member? <f '(label jmp br ret)) (ok? H A >l seen)
                           ((<f = 'jmpr) && !(id? <>f A)) (ok? H A >l seen)
                           ())                        ; an op outside the tables: keep the homes
                       (: reads <rd
                          (? (member? A reads || member? A <>rd) ()
                             ; ⚠ H BEFORE the entry mov is ANOTHER value entirely (an arrival --
                             ; memchr's c spilled from r5 while s homed r5): renaming it corrupts
                             (!seen && (member? H reads || member? H <>rd)) ()
                             (member? H <>rd && !(member? H reads)) ()
                             (ok? H A >l seen))))))))
       (ren H A l)
        (? !(two? l) ()
           (: f <l
              f2 (? (two? f) ((: (sw e) (? !(two? e) e ((? (id? <e H) A <e) >< sw >e)) sw) f) f)
              (? (two? f2 && (<f2 = 'mov) && id? <>f2 <>>f2)
                 (ren H A >l)                         ; a self-mov husk drops
                 (f2 >< ren H A >l))))
       (go2 ps l)
        (? !(two? ps) l
           (: p <ps
              (go2 >ps (? (ok? <p <>p l ()) (ren <p <>p l) l))))
       (? (callish fs) fs (go2 (pears fs 0 ()) fs)))

   ; adjacent sp adjustments MERGE (nothing observes the point between two
   ; touching adds); a cancelled pair vanishes -- the husk a dead cell leaves
   (spadj? f) (two? f && member? <f '(add sub) &&
               (<>f = 'sp) && (<>>f = 'sp) && charm? <>>>f)
   (spmerge fs)
    (? !(two? fs) ()
       (: f <fs r >fs
          nx (? (two? r) <r ())
          (? (spadj? f && spadj? nx)
             (: n1 (? (<f = 'add) <>>>f (0 - <>>>f))
                n2 (? (<nx = 'add) <>>>nx (0 - <>>>nx))
                net (n1 + n2)
                (? (net = 0) (spmerge >r)
                   (net < 0) (spmerge (['sub 'sp 'sp (0 - net)] >< >r))
                   (spmerge (['add 'sp 'sp net] >< >r))))
             (f >< spmerge r))))

   ; dead frame stores: after the constant folds, a slot the function never loads back is
   ; write-only and its stores drop. ⚠ an lea marks one offset but hands out a RANGE -- the
   ; slot map (g 'slots) names the whole OBJECT under it, and every granule of an
   ; address-taken object stays live (a derived pointer reads and writes unseen); an lea no
   ; object claims bars the fn whole. a store's newly-dead feeding li sweeps in the same pass.
   (deadst g fs)
    (: used {} esc {}
       fb (fbase g)
       slots (peep g 'slots ())
       (obmark x) (: o9 (obj9 slots x)               ; the object under x rides whole | the old bar
                     (? (two? o9) (mark <o9 <>o9) (pin esc 'y 1)))
       ; ⚠ a slot is read in BYTES, never at one offset: a 16-byte struct param's second
       ; eightbyte is STORED at off+8 and read at off+12 (that eightbyte's second field),
       ; so an offset-exact liveness called the store dead and dropped it -- a wrong answer,
       ; silently. both sides speak the 8-byte SLOT RANGE an access spans instead.
       (mwid op) (? (member? op '(ld1 ldu1 st1 si1)) 1
                    (member? op '(ld2 ldu2 st2 si2)) 2
                    (member? op '(ld4 ldu4 st4 si4 ldss stss)) 4
                    8)
       (mark o w) (: (go k) (? ((>> ((o + w) - 1) 3) < k) 1 (: _ (pin used k 1) (go (k + 1))))
                     (go (o >> 3)))
       (hit? o w) (: (go k) (? ((>> ((o + w) - 1) 3) < k) () (peep used k ()) 1 (go (k + 1)))
                     (go (o >> 3)))
       (scan l) (? !(two? l) 1
                   (: f <l
                      _ (? !(two? f) ()
                           (member? <f ldops9 || member? <f fldops)
                            (<>>f = fb ? (mark <>>>f (mwid <f)))
                           (<f = 'cmpm)
                            (<>f = fb ? (mark <>>f 8))
                           (<f = 'cmprm)
                            (<>>f = fb ? (mark <>>>f 8))
                           (member? <f '(lea lean))          ; (lea sp fp 0) is teardown, not an address take
                            (<>>f = fb && <>f != 'sp ? (obmark <>>>f))
                           (member? <f xops)          ; a runtime index roams its object
                            (<>>f = fb ? (obmark (nthr f 5)))
                           (member? <f stxops)
                            (<>f = fb ? (obmark (nthr f 4))))
                      (scan >l)))
       _ (scan fs)
       (drop (f >< u)) (? (two? f && (member? <f stops9 || member? <f siops) &&
                           <>f = fb && !(hit? <>>f (mwid <f)))
                         (drop u)
                         (f >< drop u))
       (lisweep (f >< u)) (: nx (two? u ? <u)
                           (? (two? f && <f = 'li &&
                               two? nx && member? <nx '(li mov) && <>nx = <>f)
                              (lisweep u)
                              (f >< lisweep u)))
       (esc 'y ? fs (lisweep (drop fs))))

   ; -- slot repack: nslot never hands a cell twice, so a frame is the COUNT of its temporaries,
   ; not the width of a moment, and a deep frame puts disp32 under every touch. POST-CHOICE (the
   ; rankers price param slots by build offset, so ir1 must reach them unpacked; deopt restores
   ; ir1's slot map), objects whose live windows never meet share one cell and the sub shrinks. a
   ; window is the
   ; form-index span of an object's touches, widened over every backedge it meets to a fixpoint
   ; (a loop's re-read joins everything the loop spans); an address-taken object relocates whole
   ; (its lea moves with it) but lives to the last form -- a derived pointer's life is invisible.
   ; bars, unframe's own law: a raw, r4 outside a mapped base seat, a frame touch no object
   ; claims. only 8-byte cells pool (a wider cell places fresh, its 16-parity held for any
   ; aligned rider); -8 (the r3 cell) and the caller frame (0 <=) ride pinned. ships only when
   ; the sub SHRINKS. x64 only -- arm rides its own frame family. --
   ; the callee-saved file, per target. two readers: a call spares them (the liveness kit), and
   ; a write owes a save/restore pair (cskeep). ⚠ these rosters ARE the machine, and a wrong one
   ; is a miscompile rather than a missed optimization -- each is holo's own table read back
   ; (crew/holo/<tgt>.l), never the ABI document. x64 r3 r4 r11..r14 = rbx rbp r12..r15; arm64
   ; r19..r28 = x19..x28; riscv r13/r14/r15 = s2/s3/s1 (the homes) and r19..r26 = s4..s11;
   ; thumb2 r4..r10 (AAPCS32 keeps r4..r11, but r11 IS fp and holo maps both names to 11).
   (csregs g) (? (a64? g) '(r19 r20 r21 r22 r23 r24 r25 r26 r27 r28)
                 (rv? g) '(r13 r14 r15 r19 r20 r21 r22 r23 r24 r25 r26)
                 (t32? g) '(r4 r5 r6 r7 r8 r9 r10)
                 '(r3 r4 r11 r12 r13 r14))

   ; -- the liveness kit: the control flow gen.l's three liveness approximations lack, and one
   ; backward fixpoint over it. rdsp is already the per-form transfer function; the missing
   ; half was the graph, which is why deadst asks "read ANYWHERE in the fn?" and repack's
   ; window is a convex hull over touches. the graph is per FORM, not per block -- successors
   ; are the jmp/br target plus the fallthrough, an exit has none -- so there are no blocks to
   ; build and the fixpoint is repack's widen1 shape. ⚠ the answer must stay an
   ; OVER-approximation of live: a call reads AND writes the whole caller-saved file (its
   ; arguments are not in the form), a sibcall reads everything, and an op neither rdsp nor
   ; the rosters here name answers 'bar -- which refuses the fn rather than guessing. --
   (lvgp g) (? (a64? g) '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)
               (rv? g) '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12)
               (t32? g) '(r0 r1 r2 r3 r12)
               '(r0 r1 r2 r5 r6 r7 r8 r9 r10))       ; the CALLER-saved gp file -- a seat here
                                                    ; needs no save/restore pair, and no call may
                                                    ; sit inside the range that takes one
   lvquiet '(label align ud2 br jmp)                ; no register effect at all
   (lvret g) (? (t32? g) '(r0 r1) '(r0 r1 f0 f1))   ; what an exit owes back, beside the cs file
   ; ⚠ the blanket cases read the MACHINE, so a mint stays out of them: a call clobbers the
   ; caller-saved physical file, and a mint live across one must SHOW as crossing -- the
   ; assignment's whole question -- not be read+def'd into a false kill. inert post-vrfix.
   (lvtx g f u)                                     ; one form's (reads defs) | 'bar
    (? !(two? f) [() ()]
       (: op <f
          (? (member? op lvquiet) [() ()]
             (member? op '(call callr sys))
              (: c (filter (\ r9 !(member? r9 (csregs g) || vmint? g r9)) u) [c c])
             (op = 'ret) [((lvret g) + (csregs g)) ()]
             ; ⚠ v6m's exit is POP-PC, not ret (epi-t16) -- an exit the kit cannot see is an
             ; exit where nothing looks live, and coal folded straight through it
             ((op = 'pop) && (id? <>f 'pc)) [((lvret g) + (csregs g)) ()]
             (op = 'jmpr) [(filter (\ r9 !(vmint? g r9)) u) ()]
             (: d (rdsp f) (? (id? d 'bar) 'bar [<d <>d])))))
   ; ⚠ SEEDED with the whole caller-saved file, and that is load-bearing rather than tidy: the
   ; universe is what a call clobbers, and a seat is chosen from lvgp -- so a register the fn
   ; never otherwise mentions would be absent from the clobber set and read as free ACROSS a
   ; call. ai_sleep promoted into rdx over its one call and the answer survived every gate but
   ; running. A liveness universe must cover the seats, not just the traffic.
   (lvuniv g fs)                                    ; every register in play: the seats + rdsp's
    (: t {}
       (add rs) (? !(two? rs) 0 (: _ (pin t <rs 1) (add >rs)))
       _ (add (lvgp g))
       (scan l) (? !(two? l) 0
                   (: f <l d (? (two? f) (rdsp f) [() ()])
                      _ (? (id? d 'bar) 0 (: _ (add <d) (add <>d)))
                      (scan >l)))
       _ (scan fs)
       (keys t))
   (lvun a b) (? !(two? b) a (lvun (member? <b a ? a (<b >< a)) >b))     ; set union
   (lvsame a b) (#a = #b && !(two? (filter (\ r9 !(member? r9 b)) a)))
   (lvio g fs)                                      ; -> [lin lot] (index -> live in/out of it) | 'no
    (: u (lvuniv g fs)
       um (filter (\ r9 !(vmint? g r9)) u)          ; the machine share: what a blanket case reads
       (bary? (f >< l)) (lvtx g f u @ 'bar 1 (bary? l))
       (? (bary? fs) 'no
          (: labt {}
             (ix l i) (? !(two? l) 0
                         (: f <l
                            _ (two? f && id? <f 'label ? (pin labt <>f i))
                            (ix >l (i + 1))))
             _ (ix fs 0)
             (ixd l i) (? !(two? l) () ([i <l] >< ixd >l (i + 1)))
             rl (rev (ixd fs 0))
             (succ f i) (: op (? (two? f) <f 'x)
                           (? (op = 'jmp) [(peep labt <>f -1)]
                              (op = 'br) [(peep labt <>>f -1) (i + 1)]
                              (member? op '(ret jmpr)) ()
                              [(i + 1)]))
             lin {}
             lot {}
             ; ⚠ a jmp whose target is no LOCAL label is a sibcall leaving the function, and its
             ; arguments are in flight -- it reads everything. Read as a plain transfer it has no
             ; successor, so liveness would call the whole argument file dead at the one place it
             ; is most alive. The tail-threaded VM is nothing but these.
             (gone? f) (: op (two? f ? <f 'x)
                          tg (op @ 'jmp (nthr f 1) 'br (nthr f 2))
                          (member? op '(jmp br) && !(charm? (labt tg))))
             (pass l ch)                            ; one backward sweep -> did any live-in grow?
              (? !(two? l) ch
                 (: e <l i <e f <>e
                    d (? (gone? f) [um ()] (lvtx g f u))
                    lo (foldl (\ a s (lvun a (lin s))) () (succ f i))
                    li (lvun <d (filter (\ r9 !(member? r9 <>d)) lo))
                    old (lin i)
                    ch2 !(lvsame li old)
                    _ (? ch2 (pin lin i li) 0)
                    _ (pin lot i lo)
                    (pass >l (ch || ch2))))
             (fix u9) (pass rl () ? (fix ()) 0)
             _ (fix ())
             [lin lot])))
   (lvout g fs) ; index -> the registers live OUT of it | 'no
    (lvio g fs @ 'no 'no r <>r)

   ; -- COALESCING: a copy whose source dies at the copy, and whose source was DEFINED by the
   ; form just before it, is not a copy at all -- it is a def that named the wrong register. The
   ; def takes the destination and the mov drops. copyprop is the FORWARD half of this (rename
   ; the source onward while it lives); this is the backward one, and it is the first reader to
   ; ask liveness not "does this die soon" but "does it die HERE". ⚠ soundness rides on ADJACENCY: no label
   ; can sit between the def and the copy (a label is a form, and rdsp bars it), so nothing
   ; branches in between; and D cannot be live out of the def, because live-out there is live-in
   ; at the copy, which reads S and writes D. ⚠ the prologue is off limits: unframe reads it BY
   ; POSITION (its sv3 asks whether form 4 is the r3 save), so a drop up there moves the ground
   ; under it -- and the floor is asked of the SHAPE, since a fn unframe left frameless has no
   ; prologue to protect and a fixed index would skip live code instead. --
   (coal? p f i lo lot)                             ; may (mov D S) at i fold into its def p?
    (? !(two? p && two? f && id? <f 'mov && (lo < i)) ()
       (: S (nthr f 2)
          D (nthr f 1)
          d (rdsp p)
          (d @ 'bar ()
             (: dfs <>d
                ; the def names S ALONE and never reads it -- a read would keep the operand and
                ; the two-address expansion costs back exactly what the drop wins
                (two? dfs && !(two? >dfs) && id? <dfs S && id? (nthr p 1) S &&
                 ; ⚠ D must be a stranger to the def's DEFS, or the rename writes over an operand
                 ; it still needs -- and to its reads, where the emitter refuses a destination
                 ; that aliases a source (alias-dst). the one alias it welcomes is an alu's FIRST
                 ; source: the two-address lowering is (mov d a; op d b), so (add r7 r7 8) IS the
                 ; fused form -- it is (sub r7 r9 r7), the second source, that reads its own wreck.
                 !(member? S <d) && !(member? D dfs) &&
                 (!(member? D <d) || (member? <p aluops && id? (nthr p 2) D)) &&
                 !(member? S (peep lot i ())) && !(id? D S))))))
   ; every target: the liveness kit answers for the machine it is on (the rosters take g), so the
   ; x64-only bail this carried is gone with the reason for it. ⚠ what stands in for it is lvout's
   ; OWN refusal -- a form rdsp cannot read makes the whole answer 'no and the fold declines the
   ; function -- which is why enabling a target is safe before its ops are all modelled.
   (coal g fs)
    (: lot (lvout g fs)
       ; ⚠ the floor is PROLOGUE GROUND on every target, so it cannot be read off an x64 shape:
       ; pro4? is false on arm and riscv, whose own prologue is longer, and a floor derived from
       ; it opens their bring-up to the fold. 5 clears the widest one; unframe (which drops four
       ; forms and asks whether the NEXT is the r3 save) needs only that nothing at index <= 4
       ; shifts, so this is one rung more conservative than x64 requires and stays flat.
       lo 5
       (lot @ 'no fs
          ; one form of lookbehind: a kept form is emitted when the NEXT one declines it, so a
          ; folded pair leaves exactly the rewritten def and the copy never lands
          (: ; the DROP rides the same table and the same walk: a copy whose destination is read
             ; nowhere ahead is a dead def. ⚠ lot is read PRE-rename and that is exactly
             ; conservative -- a fold shortens the source's range and leaves the destination's
             ; alone, so a drop this answers safe stays safe after it
             (dead9? f i) (lo < i && two? f && hmov? f &&
                           !(member? (nthr f 1) (lot i)))
             (rn l p i)
              (? !(two? l) (two? p ? [p])
                 (: f <l
                    (? (coal? p f i lo lot) ((reop p 1 (nthr f 1)) >< rn >l () (i + 1))
                       ; a dropped copy KEEPS the lookbehind: it is gone from the output, so the
                       ; def and the copy behind it become adjacent, which is the composition
                       (dead9? f i) (rn >l p (i + 1))
                       (two? p) (p >< rn >l f (i + 1))
                       (rn >l f (i + 1)))))
             (rn fs () 0))))

   (repack g fs)
    (: slots (g 'slots)
       pro (pro4? fs)
       sf (nthr fs 3)
       K (sf @ ('sub 'sp 'sp k) (charm? k ? k))
       (? (!(two? slots) || arm? g || g 'hasasm || !pro || !(charm? K)) fs
          (: obs {}                            ; base -> (size lo hi esc wordy? first-src)
             ; the last two fields are PROMOTION's, not packing's: an object every one of whose
             ; touches is a full-word ld/st at its own base is a candidate for a register. the
             ; last carries two facts in one -- the register the FIRST store took its value
             ; from, which is also the preferred seat (taking it makes that store a self-mov
             ; and it drops), and () when the first touch was a LOAD, which reads a value this
             ; cell was never given.
             tls {}                        ; base -> ((i op reg off)..) newest-first, the chains' food
             (touch b n i e9 k9 s9 op9 x9)             ; k9: 'st | 'ld | 'no (an unpromotable shape)
              (: w (obs b)
                 _ (pin tls b ([i op9 s9 x9] >< peep tls b ()))
                 (pin obs b (? (two? w) [n <>w i (nthr w 3 || e9)
                                           ((nthr w 4) && !(id? k9 'no)) (nthr w 5)]
                               [n i i e9 !(id? k9 'no) (? (id? k9 'st) s9 ())])))
             labt {}
             (loose? e i bp) (? !(two? e) ()
                                (id? <e 'r4 && !(i = bp)) 1
                                (loose? >e (i + 1) bp))
             (scan l i sps)                            ; every frame touch noted -> the backward spans | 'no
              (? !(two? l) sps
                 (: f <l
                    (? !(two? f) (scan >l (i + 1) sps)
                       (id? <f 'label) (: _ (pin labt <>f i) (scan >l (i + 1) sps))
                       (id? <f 'raw) 'no
                       (member? <f '(push pop)) (scan >l (i + 1) sps)
                       (id? <f 'mov && ((id? <>f 'r4 && id? <>>f 'sp) ||
                                        (id? <>f 'sp && id? <>>f 'r4)))
                        (scan >l (i + 1) sps)          ; the frame link movs (unframe strips them)
                       (: tg (? (id? <f 'jmp) <>f (id? <f 'br) <>>f ())
                          li9 (? tg (peep labt tg 'no9) 'no9)
                          sp2 (? (charm? li9) ([li9 i] >< sps) sps)
                          bp (peep a4base <f -1)
                          (? (0 <= bp && id? (nthr f bp) 'r4)
                             (: X (nthr f (peep uoff <f -1))
                                o9 (obj9 slots X)
                                (? (loose? >f 1 bp) 'no
                                   (two? o9)
                                    (: _ (touch <o9 <>o9 i
                                                (member? <f (leaops + '(leax)))
                                                ; ⚠ a cs SAVE is (st r4 slot r9) and its restore
                                                ; (ld r9 r4 slot) -- the exact shape of a
                                                ; promotable temporary, and only the register it
                                                ; moves tells them apart. A caller-saved seat
                                                ; cannot stand in for a slot whose whole job is
                                                ; surviving a call, so a cs mover never promotes.
                                                (? !((<>o9 = 8) && (X = <o9)) 'no
                                                   (id? <f 'st)
                                                    (? (member? (nthr f 3) (csregs g)) 'no 'st)
                                                   (id? <f 'ld)
                                                    (? (member? (nthr f 1) (csregs g)) 'no 'ld)
                                                   'no)
                                                (? (0 < vstw <f) (nthr f 3)
                                                   (0 < vsiw <f) (nthr f 3)
                                                   (0 < vldw <f) (nthr f 1)
                                                   ())
                                                <f X)
                                       (scan >l (i + 1) sp2))
                                   (charm? X && ((X = -8) || (0 <= X))) (scan >l (i + 1) sp2)
                                   'no))
                             (loose? >f 1 -1) 'no
                             (scan >l (i + 1) sp2))))))
             spans (scan fs 0 ())
             (? (id? spans 'no) fs
                (: n2 (tally fs)
                   bases (keys obs)
                   ; an escape pins the window's tail to the last form
                   _ (foldl (\ _ b (: w (peep obs b ())
                                      (? (nthr w 3)
                                         (pin obs b [<w <>w n2 1 (nthr w 4) (nthr w 5)]) 0)))
                            0 bases)
                   (widen1 bs ch)                      ; one widening sweep -> did anything grow?
                    (? !(two? bs) ch
                       (: b <bs w (peep obs b ())
                          (grow ss lo hi)
                           (? !(two? ss) [lo hi]
                              (: s <ss
                                 (? ((lo <= <>s) && (<s <= hi))
                                    (grow >ss (? (<s < lo) <s lo) (? (hi < <>s) <>s hi))
                                    (grow >ss lo hi))))
                          r (grow spans <>w <>>w)
                          ch2 !((<r = <>w) && (<>r = <>>w))
                          _ (? ch2 (pin obs b [<w <r <>r (nthr w 3) (nthr w 4) (nthr w 5)]) 0)
                          (widen1 >bs (ch || ch2))))
                   _ (: (fix u) (? (widen1 bases ()) (fix ()) 0) (fix ()))
                   ; -- PROMOTION (moon-ssa rungs 1+2): per-DEF chains, not the hull. a
                   ; candidate cell's stores group by the loads whose reaching defs join
                   ; them (val.l's vreach/vgroup); a chain takes a caller-saved seat over
                   ; its OWN live range -- the index set where the value actually rides --
                   ; free meaning live-out nowhere in the set and defined nowhere in it,
                   ; which a call inside the RANGE still fails by construction (it defines
                   ; the whole caller-saved file) while a call merely inside the widened
                   ; window no longer does. one cell's crossing chain keeps its cell while
                   ; a call-free sibling promotes; the cell retires only when every touch
                   ; promoted. a load no store reaches stays a frame read (it reads a value
                   ; the cell was never given -- unspecified either way).
                   ; NARROW cells ride too (rung 2): a cell every one of whose touches is
                   ; the SAME width at its own base -- st/ld/ldu/si at 1/2/4/8 -- promotes
                   ; with the extension discipline: a store canonicalizes the seat to the
                   ; chain's first load's extension, a matching load is a plain mov, a
                   ; mismatched one re-extends its dest, and an si def folds the extension
                   ; at compile time into its li.
                   ; MOON_ABLATE=pdef is the HULL for pricing: one chain of every touch
                   ; over the whole widened span, full-word st/ld only, first touch a
                   ; store required -- the pre-rung compiler exactly. --
                   (shp9 b)                    ; the cell's one touch width | 0
                    (: (go l w) (? !(two? l) w
                                   (: t <l op <>t rg (nthr t 2) X (nthr t 3)
                                      w2 (? (0 < vldw op) (vldw op)
                                            (0 < vstw op) (vstw op)
                                            (0 < vsiw op) (vsiw op) 0)
                                      (? ((w2 = 0) || !(X = b) || ((0 < w) && !(w = w2))) 0
                                         ; a full-word cs mover is a save/restore in shape:
                                         ; it never promotes (the seat cannot survive a call)
                                         ((w2 = 8) && (0 < vldw op || 0 < vstw op)
                                          && member? rg (csregs g)) 0
                                         (go >l w2))))
                       (go (peep tls b ()) 0))
                   scand (foldl (\ a b (: w (peep obs b ())
                                          (? (abl? g 'pdef)
                                             (? ((<w = 8) && !(nthr w 3) && (nthr w 4) && (nthr w 5))
                                                (b >< a) a)
                                             (? (!(nthr w 3) && (0 < shp9 b)) (b >< a) a))))
                                () bases)
                   lot9 (? (two? scand) (lvout g fs) 'no)   ; index -> live-out, or the refusal
                   dft9 (? (id? lot9 'no) {}
                           (: t {}
                              u9 (lvuniv g fs)
                              (go l i) (? !(two? l) 0
                                          (: d (lvtx g <l u9)
                                             _ (? (id? d 'bar) 0 (pin t i <>d))
                                             (go >l (i + 1))))
                              _ (go fs 0)
                              t))
                   ; -- the rung-3 kit: calls by index, and the callee-saved pool a
                   ; crossing chain may take -- only regs this fn NEVER writes (the
                   ; caller's value rides our own save/restore pair), and never in a
                   ; jmpr fn (a restore before an indirect exit is the dispatch's hot
                   ; path, and the threaded VM is nothing but these).
                   callt (: t {}
                            (go l i) (? !(two? l) t
                                        (: f <l
                                           _ (? (two? f && member? <f '(call callr sys)) (pin t i 1) 0)
                                           (go >l (i + 1))))
                            (go fs 0))
                   jrf (vany (\ f (two? f && id? <f 'jmpr)) fs)
                   cswr (? (id? lot9 'no) ()
                           (: t {}
                              (go i) (? (n2 <= i) 0
                                        (: _ (foldl (\ _ r (? (member? r (csregs g)) (pin t r 1) 0))
                                                    0 (peep dft9 i ()))
                                           (go (i + 1))))
                              _ (go 0)
                              t))
                   cspool9 (? (jrf || (abl? g 'pdef) || (abl? g 'pdefcs) || id? lot9 'no) ()
                              (filter (\ r !(peep cswr r 0)) '(r11 r12 r13 r14)))
                   ; a cs reg the fn ALREADY pairs (a prologue save to a slot --
                   ; the one-build's fn-wide grants) is reusable INTERVAL capacity:
                   ; a chain may ride its dead ranges under plain liveness freeness,
                   ; and no new pair is owed -- the fn's own restore covers the exit.
                   ; old worlds keep the blanket exclusion (the parity instruments).
                   csprd (? (jrf || id? lot9 'no || !(uniw? g)) ()
                            (: t9 {}
                               (go l i) (? (!(two? l) || 12 < i) 0
                                           (: f9 <l
                                              _ (? (two? f9 && id? <f9 'st &&
                                                    (id? <>f9 'sp || id? <>f9 'r4) &&
                                                    member? (nthr f9 3) '(r11 r12 r13 r14))
                                                   (pin t9 (nthr f9 3) 1) 0)
                                              (go >l (i + 1))))
                               _ (go fs 0)
                               (filter (\ r9 (peep t9 r9 0)) '(r11 r12 r13 r14))))
                   taken {}                    ; index -> the seats promotion already holds
                   (pfree? tk r is9)
                    (? !(two? is9) 1
                       (: i <is9
                          (? (member? r (peep lot9 i ())) ()
                             (member? r (peep dft9 i ())) ()
                             (member? r (peep tk i ())) ()
                             (pfree? tk r >is9))))
                   (pseat tk rs is9) (? !(two? rs) () (pfree? tk <rs is9) <rs (pseat tk >rs is9))
                   (pclaim tk r is9) (? !(two? is9) 0
                                        (: _ (pin tk <is9 (r >< peep tk <is9 ()))
                                           (pclaim tk r >is9)))
                   (span9 lo hi) (: (go i acc) (? (i < lo) acc (go (i - 1) (i >< acc))) (go hi ()))
                   succ9 (? (id? lot9 'no) {} (vsucc fs))
                   (xt9 op) (op @ 'ld4 'sx4 'ldu4 'zx4 'ld2 'sx2 'ldu2 'zx2 'ld1 'sx1 'ldu1 'zx1 ())
                   (pxv xc w k)                ; an si value, canonically extended at compile time
                    (? (8 <= w) k
                       (: m ((8 * w) 2)
                          v (: r9 (k % m) (? (r9 < 0) (r9 + m) r9))
                          (? (member? xc '(sx1 sx2 sx4)) (? ((m // 2) <= v) (v - m) v) v)))
                   (chains9 b)                 ; one cell -> ((S pref defs ((use dst ext)..) xc)..)
                    (: w (peep obs b ())
                       tl (rev (peep tls b ()))
                       sts (map (\ t <t) (filter (\ t (0 < vstw <>t || 0 < vsiw <>t)) tl))
                       lds (map (\ t <t) (filter (\ t (0 < vldw <>t)) tl))
                       ot (foldl (\ t9 t (pin t9 <t <>t)) {} tl)
                       rg9 (foldl (\ t9 t (pin t9 <t (nthr t 2))) {} tl)
                       (rich us) (map (\ u [u (peep rg9 u ()) (xt9 (peep ot u ()))]) us)
                       (xcof us) (? !(two? us) () (xt9 (peep ot <us ())))
                       (? (abl? g 'pdef)
                          [[(span9 <>w <>>w) (nthr w 5) sts (rich lds) ()]]
                          !(two? sts) ()               ; no store: nothing to promote
                          !(two? >sts)                 ; ONE def: one chain, no reaching needed
                           (: lo0 <>w hi0 <>>w
                              clv (vclive lo0 hi0 succ9 sts lds)
                              S (: (go i acc)
                                   (? (i < lo0) acc
                                      (go (i - 1) (? ((i = <sts) || 0 < peep clv i 0) (i >< acc) acc)))
                                   (go hi0 ()))
                              [[S (peep rg9 <sts ()) sts (rich lds) (xcof lds)]])
                          (: lo0 <>w hi0 <>>w
                             rt (vreach lo0 hi0 succ9 sts)
                             gps <(vgroup rt sts lds)   ; the chains; a no-def load stays a frame read
                             clv (vclive lo0 hi0 succ9 sts lds)   ; the CELL's liveness, once
                             st9 (foldl (\ t d (pin t d 1)) {} sts)
                             (map (\ gp (: ds <gp us <>gp
                                            dt9 (foldl (\ t d (pin t d 1)) {} ds)
                                            ; live out of i AND a chain def reaching out of i:
                                            ; this chain's value rides the edge
                                            S (: (go i acc)
                                                 (? (i < lo0) acc
                                                    (go (i - 1)
                                                        (? (peep dt9 i 0) (i >< acc)
                                                           !(0 < peep clv i 0) acc
                                                           (peep st9 i 0) acc
                                                           (two? (vboth (peep rt i ()) ds)) (i >< acc)
                                                           acc)))
                                                 (go hi0 ()))
                                            [S (peep rg9 <ds ()) ds (rich us) (xcof us)]))
                                  gps))))
                   bylo (sortby (\ a b ((<>(peep obs a ())) < (<>(peep obs b ())))) scand)
                   pxm {}                      ; touch index -> (seat canonical-ext)
                   retid {}                    ; base -> every touch promoted, the cell retires
                   ; the SUBSUMPTION order, and the compile-time floor: cells the HULL
                   ; can serve take their seats first, the old way -- whole cell, one
                   ; seat, the span claimed -- with no chain analysis at all. only the
                   ; cells the hull refuses pay for per-def chains, and a per-def claim
                   ; is a subset of a hull claim, so every cell the hull takes still
                   ; fits by construction. under pdef the rest-pass falls back to the
                   ; hull shape too and this is exactly the old pass.
                   hullw (: t {}
                            (go l) (? !(two? l) t
                                      (: b <l w (peep obs b ())
                                         _ (? !((nthr w 4) && (nthr w 5)) 0
                                              (: S (span9 <>w <>>w)
                                                 pf (nthr w 5)
                                                 r (pseat taken (? (member? pf (lvgp g)) (pf >< (lvgp g)) (lvgp g)) S)
                                                 (? !r 0
                                                    (: _ (pclaim taken r S)
                                                       _ (foldl (\ _ t9 (pin pxm <t9 [r ()])) 0 (peep tls b ()))
                                                       (pin retid b 1)))))
                                         (go >l)))
                            (? (id? lot9 'no) t (go bylo)))
                   csut {}                     ; the cs seats promotion actually took
                   (ctfree? r is9) (? !(two? is9) 1
                                      (member? r (peep taken <is9 ())) ()
                                      (ctfree? r >is9))
                   (cstake rs is9) (? !(two? rs) () (ctfree? <rs is9) <rs (cstake >rs is9))
                   _ (? (id? lot9 'no) 0
                        (foldl (\ _ b
                                 (: nrw (foldl (\ k c9
                                                (: S <c9 pf <>c9 ds (nthr c9 2) rus (nthr c9 3) xc (nthr c9 4)
                                                   r0 (pseat taken (? (member? pf (lvgp g)) (pf >< (lvgp g)) (lvgp g)) S)
                                                   ; a crossing chain (every caller-saved seat fails at
                                                   ; the call) may take a CALLEE-saved one when the loop
                                                   ; weight pays for the save/restore pair -- freeness is
                                                   ; taken-only there: the caller's liveness phantom (an
                                                   ; exit owes the file back) is met by the pair itself
                                                   ; the pair costs one save + restore per INVOCATION;
                                                   ; the wrap it replaces cost st+ld per CROSSING. only
                                                   ; compounding iteration counts repay it (the grant's
                                                   ; own depth bar, at value grain): a call in the range
                                                   ; at depth >= 2, or the crossing weight's equivalent
                                                   rp9 (? r0 () (pseat taken csprd S))
                                                   r (? r0 r0
                                                        rp9 rp9
                                                        !(two? cspool9) ()
                                                        (: wc (foldl (\ a i (? (peep callt i 0)
                                                                               (a + vwt spans i) a))
                                                                     0 S)
                                                           (? (wc < 64) () (cstake cspool9 S))))
                                                   (? !r k
                                                      ; a load whose DEST is the seat but whose extension
                                                      ; disagrees with the canonical would wreck the seat
                                                      (vany (\ u3 (id? <>u3 r && (nthr u3 2) && !(id? (nthr u3 2) xc))) rus) k
                                                      (: _ (pclaim taken r S)
                                                         _ (? (member? r cspool9) (pin csut r 1) 0)
                                                         _ (foldl (\ _ i (pin pxm i [r xc])) 0 ds)
                                                         _ (foldl (\ _ u3 (pin pxm <u3 [r xc])) 0 rus)
                                                         (k + #ds + #rus)))))
                                               0 (chains9 b))
                                    (? (nrw = tally (peep tls b ())) (pin retid b 1) 0)))
                               0 (filter (\ b !(peep retid b ())) bylo)))
                   prom (keys pxm)
                   rows (sortby (\ a b (<a < <b))
                                (map (\ b (: w (peep obs b ()) [<>w b <w <>>w]))
                                     (filter (\ b !(peep retid b ())) bases)))
                   free {}                     ; the retired 8-byte cells
                   nmap {}                     ; base -> its packed seat
                   (ret1 act lo)                       ; act ((hi n nb)..) -> the still-live rest
                    (? !(two? act) ()
                       (: a <act
                          (? (<a < lo)
                             (: _ (? (<>a = 8) (pin free 8 (nthr a 2 >< peep free 8 ())) 0)
                                (ret1 >act lo))
                             (a >< ret1 >act lo))))
                   (place rs act high)                 ; sweep by lo -> the packed high-water
                    (? !(two? rs) high
                       (: r1 <rs lo <r1 b <>r1 n (nthr r1 2) hi (nthr r1 3)
                          act2 (ret1 act lo)
                          fr (? (n = 8) (peep free 8 ()) ())
                          pr (? (two? fr) (: _ (pin free 8 >fr) [<fr high])
                                (: h1 (? ((16 <= n) &&
                                          !((& (0 - (high + n)) 15) = (b & 15)))
                                         (high + 8) high)
                                   [(0 - (h1 + n)) (h1 + n)]))
                          _ (pin nmap b <pr)
                          (place >rs ([hi n <pr] >< act2) <>pr)))
                   high2 (place rows () 8)
                   ; the cs seats' save slots, fresh below the packed high water; one
                   ; pair serves every chain that shares the seat
                   csused (filter (\ r (peep csut r 0)) cspool9)
                   cslot (: t {}
                            (go l k) (? !(two? l) t
                                        (: _ (pin t <l (0 - (high2 + (8 * k))))
                                           (go >l (k + 1))))
                            (go csused 1))
                   csaves (map (\ r ['st 'r4 (peep cslot r 0) r]) csused)
                   csrests (map (\ r ['ld r 'r4 (peep cslot r 0)]) csused)
                   K2 ((high2 + 15) & -16)
                   K3 (? (two? csused) (((high2 + (8 * tally csused)) + 15) & -16) K2)
                   ; a jmp out of the label table is a sibcall; so is one to the fn's
                   ; OWN entry label (index 0) -- a self tail call re-enters through the
                   ; prologue, and its saves would clobber the caller's file
                   (exit9? f) (two? f && (member? <f '(ret jmpr) ||
                                          (id? <f 'jmp && ((peep labt <>f -1) <= 0))))
                   (tearx? l) (? !(two? l) 0
                                 (: f <l
                                    (? (exit9? f) 1
                                       !(two? f) 0
                                       ((id? <f 'mov && id? <>f 'sp && id? <>>f 'r4) ||
                                        (id? <f 'pop && id? <>f 'r4) ||
                                        (id? <f 'add && id? <>f 'sp && id? <>>f 'sp) ||
                                        (f = '(ld r3 r4 -8))) (tearx? >l)
                                       0)))
                   (radj2 f op9)                       ; a mapped touch -> its packed spelling
                    (: X (nthr f op9)
                       o9 (obj9 slots X)
                       nb (? (two? o9) (peep nmap <o9 'no9) 'no9)
                       (? !(charm? nb) f (reop f op9 (nb + (X - <o9)))))
                   (prw f i)                           ; a promoted touch -> its forms (() drops whole) | 'no
                    (: rec (peep pxm i ())
                       (? !(two? rec) 'no
                          (: r <rec xc <>rec op <f
                             (? (0 < vsiw op)
                                 [['li r (pxv xc (vsiw op) (nthr f 3))]]
                                (0 < vstw op)
                                 (: m9 (? (id? (nthr f 3) r) () [['mov r (nthr f 3)]])
                                    (? (((vstw op) < 8) && !(xc = ())) (vapp m9 [[xc r]]) m9))
                                (: d (nthr f 1)
                                   xl (xt9 op)
                                   m9 (? (id? d r) () [['mov d r]])
                                   (? (!(xl = ()) && !(id? xl xc)) (vapp m9 [[xl d]]) m9))))))
                   (rw l i inr)
                    (? !(two? l) ()
                       (: f <l
                          t9 (? (two? csused) (tearx? l) 0)
                          spl (? (t9 && !inr) csrests ())
                          p (? (two? f && (3 < i) && two? prom) (prw f i) 'no)
                          (? (id? p 'no)
                             (: f2 (? !(two? f) f
                                      (i = 3) ['sub 'sp 'sp K3]
                                      (: bp (peep a4base <f -1)
                                         (? (0 <= bp && id? (nthr f bp) 'r4)
                                            (radj2 f (peep uoff <f -1)) f)))
                                (? ((i = 4) && two? csused)
                                   (vapp spl (f2 >< vapp csaves (rw >l (i + 1) t9)))
                                   (vapp spl (f2 >< rw >l (i + 1) t9))))
                             (vapp spl (vapp p (rw >l (i + 1) t9))))))
                   (? ((K2 < K) || two? prom) (rw fs 0 0) fs))))))

   ; dead label regions: the sibcall rewrite splices its jump BEFORE a shared epilogue and leaves
   ; the label + epi standing for the other arms -- when no arm remains, the region is unreachable
   ; ink whose `ret` trips vmret's TCO gate on our own binaries. a form after an unconditional
   ; transfer drops until a label somebody still references; an unreferenced label drops with its
   ; region. refs collect EVERY symbol operand in the fn's stream -- over-approximate, so a kept
   ; label is never wrong. deadlab iterates to a fixpoint: a dropped region takes its own refs
   ; with it, exposing the next link of a dead jmp chain.
   (deadlab fs) (: r (deadlab1 fs) (? (tally r = tally fs) r (deadlab r)))
   (deadlab1 fs)
    (: refs {}
       (note ops) (? !(two? ops) 0
                     (: o <ops
                        _ (? (nom? o) (pin refs o 1)
                             (two? o) (note o))     ; nested operands (a raw splice's forms) scan whole
                      (note >ops)))
       (scan l) (? !(two? l) 0
                   (: f <l
                      _ (two? f && <f != 'label ? (note >f))
                    (scan >l)))
       _ (scan fs)
       (sweep l dead)
        (? !(two? l) ()
           (: f <l
              lbl (two? f && <f = 'label)
              (? (dead && lbl)
                  (refs <>f ? (f >< sweep >l 0) (sweep >l 1))
                 dead (sweep >l 1)
                 (f >< sweep >l (two? f && member? <f '(jmp jmpr ret))))))
       (sweep fs 0))

   ; -- the callee-saved contract, held at emission (x64): a borrowed register -- r3 r4
   ; r11-r14, machine rbx rbp r12-r15 -- written anywhere in a fn must be SAVED before its
   ; first write and RESTORED by every exit: ret, jmpr, and a sibcall jmp alike. defs read
   ; off rdsp (divops override: idiv self-saves r2 and writes r0 + its dst); the ops rdsp
   ; bars are rostered quiet here (control, calls, the d128 r0:r2 family) -- an op on
   ; neither roster REFUSES, so a new op must name its cs effect to land. a fn carrying
   ; inline asm owns its own clobbers: skipped whole. --
   csquiet '(label align ud2 trap br jmp jmpr ret call callr sys mulo   ; trap = a64's brk #0, ud2's twin
             sx128 n128 a128 a128r s128 s128r m128 m128r mw128 mw128r c128 c128r
             dv128 rm128 inc128 dec128 shl128 shr128 sar128 shl128v shr128v sar128v)
   (csdefs g f)                                       ; the cs regs one form writes | 'dark
    (? (member? <f csquiet) ()
       (member? <f divops) (filter (r \ member? r (csregs g)) [<>f])
       (: rd (rdsp f)
          (? (id? rd 'bar) 'dark
             (filter (r \ member? r (csregs g)) <>rd))))
   (cskeep g fs)                                    ; answers fs; a breach scares, naming fn + reg
    (: (rawy? (f >< l)) (two? f && id? <f 'raw ? 1 (rawy? l))
       ; ⚠ every target with a real file rides this (a64, riscv, t32/v6m): the shapes are the
       ; same, only the frame base differs (fp on arm, and a4ize has already retargeted by the
       ; time this runs). a8 alone bails -- it has no cs file to breach.
       (? (!(two? (cspool g)) || !(two? fs) || g 'hasasm || rawy? fs) fs
          (: fn (nthr <fs 1)
             labs {} saves {} defs {}
             (note l i)
              (? !(two? l) 0
                 (: f <l
                    _ (? !(two? f) 0
                         (id? <f 'label) (pin labs <>f 1)
                         (: _ (? (id? <f 'push && member? <>f (csregs g))
                                 (? (saves <>f) 0 (pin saves <>f ['push i])) 0)
                            _ (? (id? <f 'st &&
                                  (id? <>f 'sp || id? <>f 'r4 || id? <>f (fbase g)) &&
                                  member? <>>>f (csregs g))
                                 (? (saves <>>>f) 0 (pin saves <>>>f ['slot <>f <>>f i])) 0)
                            ds (csdefs g f)
                            (? (id? ds 'dark) (scare 'cs-dark-op <f)
                               (foldl (\ _ r (defs r ? 0 (pin defs r i))) 0 ds))))
                    (note >l (i + 1))))
             _ (note fs 1)
             dr (filter defs (csregs g))
             (? !(two? dr) fs                       ; no cs writes: most fns leave here
                (: _ (foldl                         ; every written reg saved, before its first write
                      (\ _ r (: sv (saves r)
                                (? !(two? sv) (scare 'cs-unsaved [fn r])
                                   (: sp9 (nthr sv (? (id? <sv 'push) 1 3))
                                      (? (sp9 < peep defs r 0) 0 (scare 'cs-save-late [fn r]))))))
                      0 dr)
                   (rest r) (: sv (saves r)  ; the restore shape a save earns
                               (? !(two? sv) ()
                                  (id? <sv 'push) ['pop r]
                                  ['ld r <>sv <>>sv]))
                   ; the DIRTY walk: a written reg is dirty until its restore reloads it; a label
                   ; joins every path in (union -- monotone, so the pass iterates to a fixpoint);
                   ; every exit must leave with the set EMPTY. stld folding a restore away where
                   ; the path never wrote is exactly what this admits and a window scan would not.
                   lst {}                   ; label -> the join of its incoming dirty sets
                   (uni a b) (foldl (\ s r (? (member? r s) s (r >< s))) b a)
                   (sub? a b) (? !(two? a) 1 (member? <a b && sub? >a b))
                   (bmark L S) (: old (peep lst L ())
                                  new (uni S old)
                                  (? (sub? new old) 0 (pin lst L new, 1)))
                   (move f S)                       ; one form's transfer on the dirty set
                    (: ds (csdefs g f)
                       S2 (uni (ds @ 'dark dr (filter (r \ member? r dr) ds)) S)
                       (filter (r \ f != rest r) S2))
                   (step l S chg fin)
                    (? !(two? l) chg
                       (: f <l
                          (? !(two? f) (step >l S chg fin)
                             (: k <f
                                dead (id? S 'dead)
                                (? (id? k 'label)
                                    (step >l (uni (? dead () S) (lst <>f)) chg fin)
                                   dead (step >l S chg fin)
                                   (id? k 'br) (step >l S (chg + bmark <>>f S) fin)
                                   (id? k 'jmp && labs <>f)
                                    (step >l 'dead (chg + bmark <>f S) fin)
                                   (member? k '(ret jmpr jmp))
                                    (: _ (? fin (foldl (\ _ r (scare 'cs-unrestored [fn r])) 0 S) 0)
                                       (step >l 'dead chg fin))
                                   (step >l (move f S) chg fin))))))
                   (fix n) (? (step fs () 0 ()) (? (n < 24) (fix (n + 1)) (scare 'cs-fix fn)) 0)
                   _ (fix 0)
                   _ (step fs () 0 1)
                   fs)))))

   ; the addressing fold: an add/lea/mov whose result is consumed as the VERY NEXT load's base
   ; folds into the load's displacement, so the member-access triple (mov base; add off; ld) lands
   ; as one insn. STORES fold the same three shapes on their base (operand 1), a leax becomes the
   ; indexed ldx/stx, and a mov feeding only a store's VALUE operand renames it -- so the parked
   ; member store (addr; park; value; st) cascades to one insn, each pass peeling a link (addrfold
   ; runs to a fixpoint on length). sound by adjacency: a dest the consumer overwrites (or one
   ; that dies on the straight line) folds outright; a LIVE dest keeps the addr insn beside the
   ; re-based form -- whichever order leaves every read pre-clobber -- for the deaddef rerun to
   ; harvest. x64 only (the a64 lane rides raw forms).
   ; a register DIES on the straight-line suffix when an insn overwrites it before any read; a
   ; label/branch/unknown op ends the certainty -- except a ret (rides only r0 out) and a call
   ; door: a call reads the argument file and nothing caller-saved survives it, so a pending def
   ; of r0/r9/r10 is dead there (variadic al included -- setal's own li r0 defs it first). the
   ; sibcall jmp is the same door: a tail target is a function symbol, labels are dot-interned.
   (dies? g rd (f >< l))
    (? !(two? f) ()
       (: rs (rdsp f)
          (? (id? rs 'bar)
              (? (id? <f 'ret) !(id? rd 'r0)
                 (member? <f '(call callr) ||
                  (id? <f 'jmp && !(46 = ((show <>f) 0))))
                  ; the door set: caller-saved, never an argument (a64 args ride r0-r7)
                  (member? rd (? (a64? g) '(r9 r10 r11 r12 r13 r14 r15) '(r0 r9 r10)))
                 ())
             (member? rd <rs) ()
             (member? rd <>rs) 1
             (dies? g rd l))))
   (addrf1 g fs)
    (? !(two? fs) ()
       (: f <fs r >fs
          nx (two? r ? <r)
          ; a leax feeding the very next full-word/byte load THROUGH its dest IS the indexed load:
          ; one SIB insn (base/index read at the leax's seat, so they may alias a dying dest)
          lx (? !(two? f && <f = 'leax && two? nx  && member? <nx '(ld ldu1)) ()
                !(id? <>>nx <>f) ()
                !(charm? <>>>nx && charm? (nthr f 5)) ()
                [(<nx @ 'ld 'ldx 'ldxb) <>nx <>>f <>>>f (nthr f 4)
                  (nthr f 5 + <>>>nx)])
          ; leax feeding a store's BASE is the indexed store (the value must not be the leax's dest)
          sx (? (two? lx) ()
                !(two? f && <f = 'leax && two? nx && member? <nx '(st st1)) ()
                !(id? <>nx <>f) ()
                !(charm? <>>nx && charm? (nthr f 5)) ()
                (id? <>>>nx <>f) ()
                [(<nx @ 'st 'stx 'stxb) <>>f <>>>f (nthr f 4)
                  (nthr f 5 + <>>nx) <>>>nx])
          hit (? (two? lx || two? sx) ()
                 !(two? f && two? nx  && member? <nx ldops9) ()
                 !(id? <>>nx <>f) ()               ; the load's base is f's dest
                 !(charm? <>>>nx) ()
                 (member? <f '(lea lean))
                  (charm? <>>>f ? [<>>f (<>>>f + <>>>nx)])
                 (<f = 'add)
                  (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>f + <>>>nx) ?
                     [<>>f (<>>>f + <>>>nx)])
                 (<f = 'sub)
                  (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>nx - <>>>f) ?
                     [<>>f (<>>>nx - <>>>f)])
                 (<f = 'mov) [<>>f <>>>nx]
                 ())
          ; the store twin: f's dest as the store's base (operand 1; the value must not be f's dest)
          sh (? (two? lx || two? sx || two? hit) ()
                !(two? f && two? nx && (member? <nx stops9 || member? <nx siops)) ()
                !(id? <>nx <>f) ()
                !(charm? <>>nx) ()
                (id? <>>>nx <>f) ()
                (member? <f '(lea lean))
                 (charm? <>>>f ? [<>>f (<>>>f + <>>nx)])
                (<f = 'add)
                 (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>f + <>>nx) ?
                    [<>>f (<>>>f + <>>nx)])
                (<f = 'sub)
                 (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>nx - <>>>f) ?
                    [<>>f (<>>nx - <>>>f)])
                (<f = 'mov) [<>>f <>>nx]
                ())
          ; the value twin: a mov whose dest rides only the store's value operand renames it
          vh (? (two? lx || two? sx || two? hit || two? sh) ()
                !(two? f && <f = 'mov && !(charm? <>>f) && two? nx && member? <nx gstops) ()
                !(id? <>>>nx <>f) ()
                (id? <>nx <>f) ()
                [<nx <>nx <>>nx <>>f])
          ; ... and a li riding only the value operand is the store-immediate, each width
          ; truncating like its register twin (x64 only: no store-immediate elsewhere)
          vi (? (two? lx || two? sx || two? hit || two? sh || two? vh) ()
                (arm? g) ()
                !(two? f && <f = 'li && charm? <>>f && two? nx && member? <nx gstops) ()
                !(id? <>>>nx <>f) ()
                (id? <>nx <>f) ()
                (: k <>>f
                   ok (<nx @ 'st (fits32? k)
                             'st4 (-2147483648 <= k <= 4294967295)
                             'st2 (-32768 <= k <= 65535)
                             (-128 <= k <= 255))
                 (ok ? [(<nx @ 'st 'si 'st1 'si1 'st2 'si2 'si4) <>nx <>>nx k])))
          ; a li NEGATED in place is the negative li -- the constant lanes then see it
          ng (? (two? lx || two? sx || two? hit || two? sh || two? vh || two? vi) ()
                !(two? f && <f = 'li && charm? <>>f && two? nx && <nx = 'neg && id? <>nx <>f) ()
                ['li <>f -<>>f])
          ; a CONSTANT index folds into the displacement, the indexed form collapsing to plain
          xj (? (two? lx || two? sx || two? hit || two? sh || two? vh || two? vi || two? ng) ()
                !(two? f && <f = 'li && charm? <>>f && two? nx) ()
                (member? <nx '(ldx ldxb leax))
                 (: d9 (nthr nx 5 + <>>f * nthr nx 4)
                    (? !(id? <>>>nx <>f) () (id? <>>nx <>f) ()
                       !(fits32? d9) ()
                       (<nx = 'leax && !(imma g 'add d9)) ()   ; the lea lands as an add imm
                       [(<nx @ 'ldx 'ld 'ldxb 'ldu1 'lea) <>nx <>>nx d9]))
                (member? <nx '(stx stxb))
                 (: d9 (nthr nx 4 + (* <>>f <>>>nx))
                    (? !(id? <>>nx <>f) () (id? <>nx <>f) () (id? (nthr nx 5) <>f) ()
                       !(fits32? d9) ()
                       [(? (<nx = 'stx) 'st 'st1) <>nx d9 (nthr nx 5)])))
          ; ... and a constant right operand rides the ALU/cmp imm forms (cfoldir's knowns are
          ; green-only, so the negatives land here)
          aj (? (two? lx || two? sx || two? hit || two? sh || two? vh || two? vi || two? ng || two? xj) ()
                !(two? f && <f = 'li && charm? <>>f && fits32? <>>f && two? nx) ()
                (member? <nx aluops)
                 (? (id? <>>>nx <>f) (? (id? <>>nx <>f) () (imma g <nx <>>f) [<nx <>nx <>>nx <>>f]))
                (<nx = 'cmp)
                 (? (id? <>>nx <>f) (? (id? <>nx <>f) () (imma g 'cmp <>>f) ['cmp <>nx <>>f])))
          (? (two? lx)
              (? (id? <>nx <>f || dies? g <>f >r) (lx >< addrf1 g >r)
                 !(id? <>f <>>f || id? <>f <>>>f)
                  (f >< lx >< addrf1 g >r)
                 !(id? <>nx <>>f || id? <>nx <>>>f)
                  (lx >< f >< addrf1 g >r)
                 (f >< addrf1 g r))
             (two? sx)
              (? (dies? g <>f >r) (sx >< addrf1 g >r)
                 !(id? <>f <>>f || id? <>f <>>>f)
                  (f >< sx >< addrf1 g >r)
                 (sx >< f >< addrf1 g >r))
             (two? hit)
              (: nl [<nx <>nx <hit <>hit]
                 (? (id? <>nx <>f || dies? g <>f >r) (nl >< addrf1 g >r)
                    !(id? <>f <hit) (f ><  nl >< addrf1 g >r)
                    (nl >< f >< addrf1 g >r)))
             (two? sh)
              (: nl [<nx <sh <>sh <>>>nx]
                 (? (dies? g <>f >r) (nl >< addrf1 g >r)
                    !(id? <>f <sh) (f >< nl >< addrf1 g >r)
                    (nl >< f >< addrf1 g >r)))
             (two? vh)
              (? (dies? g <>f >r) (vh >< addrf1 g >r)
                 (f >< vh >< addrf1 g >r))
             (two? vi)
              (? (dies? g <>f >r) (vi >< addrf1 g >r)
                 (f >< vi >< addrf1 g >r))
             (two? ng) (ng >< addrf1 g >r)
             (two? xj)
              (? (id? <>nx <>f || dies? g <>f >r) (xj >< addrf1 g >r)
                 (f >< xj >< addrf1 g >r))
             (two? aj)
              (? (id? <>nx <>f || dies? g <>f >r) (aj >< addrf1 g >r)
                 (f >< aj >< addrf1 g >r))
             (f >< addrf1 g r))))
   (addrfold g fs) (: r (addrf1 g (addrf1 g fs)) (#r = #fs ? r (addrfold g r)))

   ; the BRANCHING death proof (cmpfuse's license): X is dead at a point when NO reachable path
   ; reads it before a def. segments explore once (a label memo), a br queues its target and
   ; falls through, a jmp ends its segment into the target, ret frees every register but r0;
   ; a read, an unknown op, or a call answers live (the args are unseen). linear per query.
   (dieb? rd all fs0)
    (: seen {}
       (lab l w) (? !(two? l) ()
                    (: f <l (? (two? f && <f = 'label && id? <>f w) >l (lab >l w))))
       (go l wl)
        (? !(two? l) (next wl)
           (: f <l
              (? !(two? f) (go >l wl)
                 (<f = 'label)
                  (? (seen <>f) (next wl)
                     (: _ (pin seen <>f 1) (go >l wl)))
                 (member? <f '(jmp jmp8)) (next (<>f >< wl))
                 (member? <f '(br br8)) (go >l (<>>f >< wl))
                 (<f = 'ret) (rd @ 'r0 () (next wl))
                 (: rs (rdsp f)
                    (? (id? rs 'bar) ()
                       (member? rd <rs) ()
                       (member? rd <>rs) (next wl)
                       (go >l wl))))))
       (next wl) (? !(two? wl) 1
                  (: w <wl (seen w ? (next >wl) (pin seen w 1, go (lab all w) >wl))))
       (go fs0 ()))

   ; cmp LOAD-OP FUSION: a full-word load whose dest is consumed by the VERY NEXT cmp and dead
   ; beyond it fuses into the cmp's memory operand -- (ld X b o)(cmp X y) is (cmpm b o y),
   ; (cmp r X) is (cmprm r b o). runs after addrfold (the base already folded), the death proof
   ; follows branches. x64 only; full-word only (a narrow cmp's flags shift with the extension).
   (cmpfuse fs)
    (: (go (f >< r))
           (: nx (two? r ? <r)
              X (two? f && <f = 'ld && two? nx && <nx = 'cmp ? <>f)
              m (? !(nom? X) ()
                   (id? <>nx X)
                    (? (id? <>>nx X) ()             ; X both sides never fuses
                       (charm? <>>nx && !(fits32? <>>nx)) ()
                       ['cmpm <>>f <>>>f <>>nx])
                   (id? <>>nx X)
                    (charm? <>nx ? () ['cmprm <>nx <>>f <>>>f]))
              (two? m && dieb? X fs >r ? (m >< go >r) (f >< go r)))
       (go fs))

   ; store-forward: within one straight-line run a full-word frame slot RIDES the register that
   ; last touched it -- a store binds slot -> reg, a bound slot's reload answers the register (a
   ; mov, or nothing when it lands home), a first load binds too. a binding dies with a redef of
   ; its register, a store over its slot (byte overlap: the narrow stores too), a label (paths
   ; join), and every bar/r4 def (a call clobbers, pop r4 tears down); a branch OUT keeps the
   ; fall-through. ⚠ the narrow loads keep their round-trip (they re-extend, a different value).
   ; ⚠ dst r3/cs reloads stay -- epim?/sibjmp/unframe anchor on the epilogue's exact shapes.
   ; ⚠ an lea/lean off r4 hands out a RANGE (deadst's law): that fn keeps the adjacent pair only.
   stldkeep '(r3 r11 r12 r13 r14)
   (stldkp g) (? (a64? g) (csregs g) stldkeep)      ; the epilogue-anchor destinations, per target
   (bdropr bs r9) (filter (\ e9 (<>e9 != r9)) bs)
   (bdropo bs o9) (filter (\ e9 (: d (<e9 - o9) ((7 < d) || (d < -7)))) bs)
   (stld g fs)
    (: fb (fbase g)
       ; the epilogue's (lea sp fp 0) is teardown, not an address take
       (esc? (f >< l))
        (two? f && member? <f '(lea lean) && <>>f = fb && !(id? <>f 'sp) ? 1 (esc? l))
       fs2 (stldp fb fs ())
       (esc? fs2 ? (stlda fb fs2) (stldw fb (stldkp g) fs2 ())))
   ; the adjacent pair -- a store then a load of the SAME address, nothing between, needs
   ; no aliasing story at all (stlda's own law, which only ever asked r4). The full-width
   ; r4 pair stays with the lanes below so the slot map, stldkeep and the epilogue anchors
   ; are untouched; every anchor shape is a full ld, so the narrow pairs forward on any
   ; base, wearing the extension the load promised (a slot holds the store's low bytes,
   ; so the reload is that extension of the source register).
   ; the client is a spliced VM body: every op boundary writes Sp[0] and the next op reads it.
   (fwext s l) (s @ 'st (l @ 'ld 'mov)
                   'st4 (l @ 'ldu4 'zx4 'ld4 'sx4)
                   'st2 (l @ 'ldu2 'zx2 'ld2 'sx2)
                   'st1 (l @ 'ldu1 'zx1 'ld1 'sx1))
   (fwfs w rY rX) (? (id? w 'mov) (rY != rX ? [['mov rY rX]])
                     (rY = rX) [[w rX]]
                     [['mov rY rX] [w rY]])
   ; a store fed by an adjacent li declines: that triple is the si fold's (the forward
   ; would read the source and pin the li live, trading the store-immediate away)
   (stldp fb (f >< r) pv)
    (? !(two? r && two? f && member? <f gstops && !(<f = 'st && <>f = fb))
       (f >< stldp fb r f)
       (: nx <r
          rX <>>>f
          w (two? nx ? (fwext <f <nx))
          (? (id? w () ||
              (two? pv && <pv = 'li && <>pv = rX) ||
              !(<>>nx = <>f && <>>>nx = <>>f))
             (f >< stldp fb r f)
             (f >< fwfs w <>nx rX + stldp fb >r f))))
   (stlda fb (f >< r))                                     ; the escape lane: only the pair with nothing between is aliasing-proof
    (? !(two? r && two? f && <f = 'st && <>f = fb)
       (f >< stlda fb r)
       (: nx <r
          rX <>>>f
          (? !(two? nx && <nx = 'ld && <>>nx = fb && <>>>nx = <>>f)
             (f >< stlda fb r)
             (: rY <>nx
                (rY = rX ? (f >< stlda fb >r)
                 (f >< ['mov rY rX] >< stlda fb >r))))))
   (stldw fb keep (f >< r) bs)
    (? !(two? f) (f >< stldw fb keep r bs)
       (: op <f
          (? (op = 'label) (f >< stldw fb keep r ())
             (member? op '(br jmp jmpr ret)) (f >< stldw fb keep r bs)
             (op = 'ld && <>>f = fb)
              (: rY <>f o <>>>f
                 b9 (assoc o bs)
                 (? (member? rY keep)
                    (f >< stldw fb keep r ([o rY] >< bdropo (bdropr bs rY) o))
                    (two? b9 && two? <>b9)     ; an immediate binding: the load remats
                     (['li rY <><>b9] >< stldw fb keep r ([o rY] >< bdropr bs rY))
                    (two? b9 && <>b9 = rY) (stldw fb keep r bs)
                    (two? b9) (['mov rY <>b9] >< stldw fb keep r ([o rY] >< bdropr bs rY))
                    (f >< stldw fb keep r ([o rY] >< bdropr bs rY))))
             (member? op stops9 && <>f = fb)
              (: o <>>f
                 b2 (bdropo bs o)
                 (? (op = 'st && nom? <>>>f)
                    (f >< stldw fb keep r ([o <>>>f] >< b2))
                    (f >< stldw fb keep r b2)))
             ; a store-immediate binds its constant (full width only; narrow si's
             ; truncated bytes just invalidate) -- a later ld remats as li
             (member? op siops && <>f = fb)
              (: o <>>f
                 b2 (bdropo bs o)
                 (? (op = 'si)
                    (f >< stldw fb keep r ([o ['k <>>>f]] >< b2))
                    (f >< stldw fb keep r b2)))
             ; a reg-reg mov CARRIES its src's bindings: the slot now rides both registers,
             ; and when one dies the other still answers. sp/fp/r4 stay out (sp moves in
             ; silence -- its defs are empty by design)
             (op = 'mov && nom? <>f && nom? <>>f
                 && !(member? <>f '(sp fp r4)) && !(member? <>>f '(sp fp r4)))
              (: rY <>f rB <>>f
                 news (map (\ e9 [<e9 rY]) (filter (\ e9 (<>e9 = rB)) bs))
                 (f >< stldw fb keep r (news + bdropr bs rY)))
             (: rd (rdsp f)
                (? (id? rd 'bar) (f >< stldw fb keep r ())
                   (member? fb <>rd) (f >< stldw fb keep r ())
                   (f >< stldw fb keep r (filter (\ e9 !(member? <>e9 <>rd)) bs)))))))

   ; -- redundant re-extension (rezx): a zxN over a register already CLEAN to N drops.
   ; cleanliness is the producing op's contract -- the zx family and the unsigned loads,
   ; the 32-bit rotates (their dests wear the w-form on every target that has them), an
   ; li's own value, a mov carrying its source's. everything else clears what it
   ; defines; a bar clears the table. --
   (rezx fs)
    (: (go l t acc)
        (? !(two? l) (rev acc)
           (: f <l r >l
              (? !(two? f) (go r t (f >< acc))
                 (: op <f
                    (? (member? op '(zx1 zx2 zx4))
                        (: w (? (op = 'zx1) 8 (op = 'zx2) 16 32)
                           c (peep t <>f 0)
                           (? ((0 < c) && (c <= w)) (go r t acc)
                              (: _ (pin t <>f w) (go r t (f >< acc)))))
                       (member? op '(ldu1 ldu2 ldu4))
                        (: _ (pin t <>f (? (op = 'ldu1) 8 (op = 'ldu2) 16 32))
                           (go r t (f >< acc)))
                       (op = 'ror4)
                        (: _ (pin t <>f 32) (go r t (f >< acc)))
                       (op = 'rorv4)
                        (: _ (pin t 'r1 0) _ (pin t <>f 32) (go r t (f >< acc)))
                       (op = 'li)
                        (: k <>>f
                           _ (pin t <>f (? !(charm? k) 0 (k < 0) 0
                                           (k < 256) 8 (k < 65536) 16
                                           (k < 4294967296) 32 0))
                           (go r t (f >< acc)))
                       (op = 'mov)
                        (: _ (pin t <>f (peep t <>>f 0)) (go r t (f >< acc)))
                       (: rd (rdsp f)
                          (? (id? rd 'bar) (go r {} (f >< acc))
                             (: (dirty ds) (? !(two? ds) 0
                                              (: _ (pin t <ds 0) (dirty >ds)))
                                _ (dirty <>rd)
                                (go r t (f >< acc))))))))))
       (go fs {} ()))

   ; -- COPY PROPAGATION: after (mov a b) the two registers hold ONE value, so every later read
   ; of a may say b instead, and once nothing reads a the copy is a dead def and drops. one
   ; forward walk carrying reg -> source, killed at each def and at every control edge, then one
   ; liveness pass for the drop -- lvout answers "is this copy's destination read anywhere ahead"
   ; across the whole fn, which the block-local deaddef cannot. no window and no form count
   ; anywhere: the map lives exactly as long as the dataflow says, and coal folds what it strands.
   ; ⚠ frame anchors and r3 stay out of the map: pro4?/epim?/sibjmp/unframe all read the prologue
   ; and epilogue by SHAPE, so a rename up there moves their ground.
   (hmov? f) (two? f && <f = 'mov && nom? <>f && nom? <>>f
               && !(member? <>f '(sp fp r4 r3)) && !(member? <>>f '(sp fp r4 r3)))
   ; the forms that keep their operands, each for a reason rdsp cannot carry: addrfold CONSUMES a
   ; mov feeding an address under its own license and a read-through here would steal the pattern;
   ; cskeep reads (push <cs>) as that register's SAVE, so its operand is a contract; and a
   ; variable shift takes its count in r1 on every backend (holo scares if it does not).
   cpskip (ldops9 + gstops + stxops + siops + leaops + xops + fldops + fstops + vshops +
           '(cmpm cmprm push))
   ; the READ POSITIONS of a form, probed off rdsp itself rather than kept as a whitelist beside
   ; it: a slot where a stranger reads and does not write renames sound, so a read-modify-write
   ; slot (the shifts, the unops) and la's SYMBOL both decline by construction.
   (cprp f)
    (? (member? <f cpskip) ()
       (: (go i acc)
           (? !(i < #f) (rev acc)
              (: x (nthr f i)
                 (? !(nom? x) (go (i + 1) acc)
                    (: d (rdsp (reop f i 'zq))
                       (? (id? d 'bar) (go (i + 1) acc)
                          (member? 'zq <d && !(member? 'zq <>d)) (go (i + 1) (i >< acc))
                          (go (i + 1) acc))))))
          (go 1 ())))
   ; ⚠ the substitute must be a stranger to the form's DEFS: the two-address expansion writes the
   ; destination before reading the source, so (sub r0 r12 r0) would read its own wreck. a mov is
   ; the exception and passes ds empty -- there dest = source is the self-mov we are hunting.
   ; ⚠ an alu's FIRST source is the other exception, and the dest is exactly the substitute it
   ; welcomes: (mov d a; op d b) is the lowering, so (add r7 r7 8) is the FUSED shape. and the
   ; converse temptation -- decline a rename that BREAKS a fusion -- was measured and refused:
   ; breaking it is what lets coal fold the stranded copy and rebuild the fusion on the copy's
   ; own register, which is the better answer and 914 bytes of love.o.
   (cpsub f ps m ds)
    (? !(two? ps) f
       (: i <ps x (nthr f i)
          b (peep m x ())
          two (member? <f aluops && (i = 2))
          (cpsub (? (nom? b && (two || !(member? b ds))) (reop f i b)
                    f)
                 >ps m ds)))
   (cpkill m ds)                                    ; a def kills its own copy and every copy OF it
    (: _ (foldl (\ _ k (? (member? k ds || member? (m k) ds) (pin m k ()) 0)) 0 (keys m))
       m)
   (cpwalk (f >< u) m)
    (? !(two? f) (f >< cpwalk u m)
       (: op <f
          (? (member? op '(br align ud2)) (f >< cpwalk u m)   ; a br rides its fall-through
             (member? op '(label jmp jmpr ret)) (f >< cpwalk u {})
             (: d (rdsp f)                    ; a call and every op off the table land here
                (? (id? d 'bar) (f >< cpwalk u {})
                   (: f2 (cpsub f (cprp f) m (op @ 'mov () <>d))
                      (? (hmov? f2 && id? <>f2 <>>f2) (cpwalk u m)
                         (: m2 (cpkill m <>d)
                            _ (? (hmov? f2) (pin m2 <>f2 <>>f2) 0)
                            (f2 >< cpwalk u m2)))))))))
   ; ⚠ the forward half rides the BUILD TAIL, where its renames reach addrfold/cmpfuse/deaddef,
   ; and it asks no liveness on purpose: build runs TWICE per fn (ir1 and the regen), so a
   ; whole-fn fixpoint here is paid four times over. the backward half and the drop both ride
   ; coal, post-choice, off the one lvout that was already being paid there.
   (copyprop fs) (cpwalk fs {})

   ; -- aggregate initializers: ('init (item..)), items plain | ('dfield
   ; name val) | ('didx n val), nesting for arrays of structs. shared bits: --
   (aggish? ty) (arr? ty || stru? ty)
   ; the seeded C99 complex tags: a complex is C's SCALAR -- brace elision never descends into
   ; its components, and a single expr item initializes the WHOLE value (real -> complex converts)
   (gcplx? ty) (stru? ty && member? <>ty '("__cplx_double" "__cplx_float"))
   (elval it) (? (member? <it '(dfield didx)) <>>it it)   ; the value of an item
   (fidx fields name) (: (go l i) (? !(two? l) 0 (<<l = name) i (go >l (i + 1)))
                          (go fields 0))
   (nthfield fields i) (: (go l k) (? !(two? l) 'bad (k < 1) <l (go >l (k - 1)))
                           (go fields i))

   ; -- brace elision (C 6.7.9): a flat run of items fills a nested subaggregate whose brace was
   ; omitted -- int a[2][2] = {1,2,3,4} means {{1,2},{3,4}}. the init tree NORMALIZES up front
   ; (each elided run wrapped in an explicit ('init ..)), so the layout code below only ever sees
   ; fully-braced inits. idempotent on an already-braced tree. --
   (initval? v) (two? v && <v = 'init)
   ; a wide literal arrives as a BOUNDED clit-of-array (parse's desugar): on an
   ; array target its init IS the array's initializer (a POINTER element keeps the
   ; clit as a scalar -- it decays to its address like any compound literal)
   (clarr? ty ini) (arr? ty && !(ptr? <>ty) && two? ini && (<ini = 'clit) && arr? <>ini)
   (unelide g ty ini)                               ; normalize ini against ty
    (? (clarr? ty ini) (unelide g ty <>>ini)      ; a wide literal: take its init
       !(initval? ini) ini                        ; a scalar / non-brace: nothing to nest
       !(aggish? ty) ini                           ; a brace on a scalar type: leave (layout unwraps/scares)
       ; { "s" } on a char array: the optionally-braced string literal (C11
       ; 6.7.9p14) -- unwrap to the bare literal, the plain lane images it
       (arr? ty && !(ptr? <>ty) &&
        (: its <>ini (two? its && !(two? >its) && two? <its && id? <<its 'str)))
        <<>ini
       (arr? ty && !(ptr? <>ty) &&                   ; { L"s" }: p14's wide twin
        (: its <>ini (two? its && !(two? >its) && two? <its && id? <<its 'clit && arr? <><its)))
        (unelide g ty <>><<>ini)
       (: pr (normfill g ty <>ini) ['init <pr]))     ; <pr = normed items; <>pr (excess) dropped
   (redesig dh dk v) [dh dk v]   ; ('didx k v) | ('dfield k v)
   (normelt g ty desig dh dk val l)                 ; one element of type ty from item stream l -> (item rest)
    (? (arr? ty && !(ptr? <>ty) && two? val && id? <val 'str)
        [<l >l]                                    ; a string IS a char array's whole element -- never an elision run
       (clarr? ty val)                              ; a wide literal: the whole element, as its init
        (: w (unelide g ty <>>val)
           [(? desig (redesig dh dk w) w) >l])
       (aggish? ty && (!(gcplx? ty) && !(initval? val)))   ; ELIDE: fill ty from the run starting at val (never into a complex -- a scalar)
        (: pr (normfill g ty (val >< >l))
           w ['init <pr]
           [(? desig (redesig dh dk w) w) <>pr])
       (: nv (? (initval? val) (unelide g ty val) val)  ; a braced sub -> recurse-normalize; a scalar -> as-is
          [(? desig (redesig dh dk nv) (? (id? nv val) <l nv)) >l]))
   (normfill g ty items)                            ; consume items to fill aggregate ty -> (normed-items rest)
    (? (arr? ty)
        (: elt <>ty n <>>ty
           (go l idx acc)
            (? !(two? l) [(rev acc) ()]
               (: it <l
                  desig (<it = 'didx)
                  ; full AND plain -> the tail is the rest; a designator may jump BACK
                  ; ({ [2] = 9, [0] = 1 }), so it never ends the walk
                  (? (!desig && (< 0 n) && (<= n idx)) [(rev acc) l]
                     (: ci (? desig <>it idx)
                        pr (normelt g elt desig 'didx ci (elval it) l)
                        (go <>pr (ci + 1) (<pr >< acc))))))
           (go items 0 ()))
       (stru? ty)
        (: fields <(peep (g 'stag) <>ty 0)
           (go l cur acc)
            (? !(two? l) [(rev acc) ()]
               (: it <l
                  desig (<it = 'dfield)
                  ci (? desig (fidx fields <>it) cur)
                  fe (nthfield fields ci)
                  (? (bad? fe) [(rev acc) l]          ; unknown field / past the end -> leave the rest
                     (: pr (normelt g <>>fe desig 'dfield <>it (elval it) l)
                        (go <>pr (ci + 1) (<pr >< acc))))))
           (go items 0 ()))
       [items ()])

   ; -- LOCAL init (codegen): zero the slot, then fill (values may be runtime) --
   (cgzero off span)                                ; zero [r4+off .. +span)
    (: (go o rem acc)
        (? (8 <= rem) (go (o + 8) (rem - 8) (acc + [['st 'r4 o 'r0]]))
           (0 < rem) (go (o + 1) (rem - 1) (acc + [['st1 'r4 o 'r0]]))
           acc)
       ('((li r0 0)) + go off span ()))
   (cgstr off text)                                 ; a char array from a string: byte stores (NUL from the zero)
    (: (go i acc) (? (i < 0) acc (go (i - 1) ([['li 'r0 (text i)] ['st1 'r4 (off + i) 'r0]] + acc)))
       (go (tally text - 1) ()))
   (cgstore g env off ty val)                       ; store a scalar value expr into [r4+off]
    (: r (cgexpr g env val ())
       (? (bad? r) 'bad
          (flo? ty) (asflo g r + fstf g ty 'r4 off 'f0)
          (pair? g ty) (asll g r + pst 'r4 off)
          (asintt g r ty + cvt ty 'r0 + [[(stop ty) 'r4 off 'r0]])))
   (cgfill g env off ty ini0)                       ; fill an aggregate at frame offset off (no zero)
    (: ini (unelide g ty ini0)                        ; normalize elided braces (idempotent)
     (? (<ini = 'str) (? (arr? ty) (cgstr off <>ini) 'bad)
       !(<ini = 'init) 'bad
       (arr? ty)
        (: elt <>ty esz (tsz g elt)
           (fill l idx acc)
            (? !(two? l) acc
               (: it <l
                  ci (? (<it = 'didx) <>it idx)
                  val (elval it)
                  eoff (off + (* ci esz))
                  r (? (aggish? elt && (<val = 'init)) (cgfill g env eoff elt val)
                       (stru? elt && !(<val = 'str)) (cgsfill g env eoff elt val)
                       (cgstore g env eoff elt val))
                  (? (bad? r) 'bad (fill >l (ci + 1) (acc + r)))))
           (fill <>ini 0 ()))
       (stru? ty)
        (: fields <(peep (g 'stag) <>ty 0)
           (fill l cur acc)
            (? !(two? l) acc
               (: it <l
                  ci (? (<it = 'dfield) (fidx fields <>it) cur)
                  val (elval it)
                  fe (nthfield fields ci)
                  (? (bad? fe) 'bad
                     (: foff (off + <>fe) fty <>>fe
                        r (? (aggish? fty && (<val = 'init)) (cgfill g env foff fty val)
                             (stru? fty && !(<val = 'str)) (cgsfill g env foff fty val)
                             (bitf? fty) (cgbffill g env foff fty val)
                             (cgstore g env foff fty val))
                        (? (bad? r) 'bad (fill >l (ci + 1) (acc + r)))))))
           (fill <>ini 0 ()))
       'bad))
   ; a bitfield member of a brace initializer: read-modify-write the unit at [r4+off].
   ; cgstore would lay the whole declared unit flat -- unshifted, and the neighbours
   ; sharing it gone. the assignment lane's shape, minus the address computation.
   (cgbffill g env off bt val)
    (: r (cgexpr g env val ())
       (? (bad? r) 'bad
          (: uty <>bt bitoff <>>bt usz (tsz g uty)
             mask ((1 << <>>>bt) - 1) cmask ((0 - (mask << bitoff)) - 1)
             (asintt g r uty
              + (? (id? uty 'bool) (cvt uty 'r0) ())   ; normalize BEFORE the mask
              + [[(uldop usz) 'r2 'r4 off]
                  ['li 'r3 mask] '(and r0 r0 r3)
                  ['li 'r3 cmask] '(and r2 r2 r3)
                  '(mov r3 r0)]
              + (? (bitoff > 0) [['shl 'r3 bitoff]] ())
              + ['(or r2 r2 r3) [(stop uty) 'r4 off 'r2]]))))
   ; a complex width flip, src address in r0, dest in r1: convert componentwise
   ; (a blind block copy would read the WRONG byte shapes)
   (cxcopy g sty)
    (? (id? (cxb sty) 'float)
        '((ldss f2 r0 0) (cvtss2sd f2 f2) (stsd r1 0 f2)
          (ldss f2 r0 4) (cvtss2sd f2 f2) (stsd r1 8 f2))
        '((ldsd f2 r0 0) (cvtsd2ss f2 f2) (stss r1 0 f2)
          (ldsd f2 r0 8) (cvtsd2ss f2 f2) (stss r1 4 f2)))
   ; a struct member/element from a struct-valued EXPRESSION (the union pun's
   ; {(_Complex t)(x)} -- a whole-struct item, not a brace): source address in
   ; r0 (clval for an lvalue, cgexpr for an rvalue temp), block copy into the
   ; frame slot -- a complex width flip converts instead. a complex member also
   ; takes a REAL expr ({3.0} -- C converts): re gets the value, im a zero
   ; image. the source evaluates FIRST (it may call); the tail stores cannot.
   (cgsfill g env foff fty val)
    (: rl (matv g (clval g env val))
       rv (? (!(bad? rl) && stru? (vty rl)) rl (cgexpr g env val ()))
       (? (bad? rv) 'bad
          (stru? (vty rv))
           (: mv (tor0 g rv + [['lea 'r1 'r4 foff]])
              (? (gcplx? fty && gcplx? (vty rv) && !(<>fty = <>(vty rv)))
                 (mv + cxcopy g (vty rv))
                 (mv + bcopy g 0 (tsz g fty) ())))
          (gcplx? fty)
           (: b (cxb fty)
              (asflo g rv + (fstf g b 'r4 foff 'f0
               + ('((li r2 0))
                  + (? (b = 'float) [['st4 'r4 (foff + 4) 'r2]] [['st 'r4 (foff + 8) 'r2]])))))
          'bad))

   ; -- GLOBAL init (a constant byte image): a scalar is little-endian bytes, an aggregate a
   ; placement flatten, a POINTER an abs64 fixup ('fix 8 abs64 label 0) laid by holo/elf.
   ; ⚠ a fixup is ONE list item but eight bytes, so lengths go through imbytelen. --
   (fixit? x) (two? x && (<x = 'fix))
   (imbytelen l) (foldl (\ a x (a + (? (fixit? x) <>x 1))) 0 l)
   ; ⚠ bytes via // and % (not & and >>): a constant past the charm range is a BIG, where the
   ; bitwise ops are undefined and division is exact. a negative wraps into the unsigned range.
   ; ⚠ imnum lays a raw byte pattern -- bfover feeds it merged UNIT bits, so the bool
   ; normalization lives in imgbytes' scalar row, never here.
   (imnum g ty val) (: n (tsz g ty)
                       (pw k acc) (? (k < 1) acc (pw (k - 1) (acc * 256)))
                       u0 (? (val < 0) (val + pw n 1) val)
                       (go i v acc) (? (n <= i) (rev acc)
                                       (go (i + 1) (v // 256) (v % 256 >< acc)))
                       (go 0 u0 ()))
   (iminsert x l) (? !(two? l) [x]                    ; insert (off items) keeping offsets ascending
                     (<x < <<l) (x >< l)
                     (<l >< iminsert x >l))
   (imsort ps) (: (go l acc) (? !(two? l) acc (go >l (iminsert <l acc))) (go ps ()))
   (imflat ps total)                                ; sorted (off items).. -> total bytes, gaps zeroed
    (: (go l pos acc)
        (? !(two? l) (acc + zeros (total - pos))
           (: p <l off <p items <>p
              (go >l (off + imbytelen items) (acc + zeros (off - pos) + items))))
       (go ps 0 ()))
   ; a CONSTANT integer expression at image time -> its value | 'no. C's static initializers
   ; permit any int const-expr in a subscript or pointer offset (`&a[2*K+1]`), so the arithmetic
   ; folds here. `//` truncates toward zero (C's `/`); the machine ops wrap at the word.
   (icfold e)
    (? !(two? e) 'no
    (<e @ 'num <>e
       ; a cast TO an integer type may carry a FLOAT operand (C11 6.6p6) -- the one float an
       ; integer constant expression holds. it truncates toward zero, which is what `// 1` does.
       'cast (: v (icfold <>>e)
              (? (flo? <>e) 'no
                 !(v = 'no) (? (<>e = 'bool) (v != 0) v)
                 (: f (ffold <>>e)
                    (? (bad? f) 'no (<>e = 'bool) (? (f = 0.0) 0 1) (ceil (f // 1))))))
       'un
        (: v (icfold <>>e)
           (? (v = 'no) 'no (<>e = 'neg) -v (<>e = 'bnot) (-v - 1) 'no))
       'bin (: op <>e a (icfold <>>e) b (icfold <>>>e)
             (? (a = 'no || b = 'no) 'no
              (op @ '+ (a + b) '- (a - b) '* (a * b)
                    '/ (? (b = 0) 'no (a // b)) '% (b = 0 ? 'no (a - (a // b) * b))
                    'shl (a << b) 'shr (a >> b)
                    '& (a & b) '| (a | b) '^ (a ^ b)
                    'no)))
       'no))
   ; a CONSTANT LVALUE at image time -> (label byteoffset type) | 'no: a file-scope global, a
   ; *(const pointer) (a subscript desugars here -- a[i] is *(a+i)), or a .member off either.
   ; the type threads so pointer arithmetic and the next .member scale/offset correctly.
   (imlval g e)
    (<e @ 'var
            (? (g 'fns <>e) [(name <>e) 0 (fnty g <>e)]   ; a function IS its address
               (: en (peep (g 'glob) <>e 0)               ; glob = (label type init)
                (two? en ? [<en 0 <>en] 'no)))
           'deref
            (: p (imaddr g <>e) (? (id? p 'no) 'no [<p <>p <><>>p]))
           'dot
            (imlval g <>e @ 'no 'no b
             (: fm (fmem g <>>b <>>e) (bad? fm ? 'no [<b (<>b + <fm) <>fm])))
           'no)
   ; a CONSTANT ADDRESS expression at image time -> (label byteoffset ptrtype) | 'no: a string /
   ; function / decayed-array global (offset 0), &lvalue, or pointer +/- an int constant (scaled
   ; by the pointee). a cast retypes, so a later + scales by the new pointee.
   (imaddr g e)
    (<e @ 'cast
        (: r (imaddr g <>>e)                        ; a cast RETYPES the address (its type is the cast's) --
           (r @ 'no 'no [<r <>r <>e]))   ; so a later + scales (or doesn't) by the cast type
       'str
        (: l (nlab g ".S") _ (pin g 'strs ([l <>e] >< g 'strs)) [l 0 '(ptr char)])
       'var
        (? (peep (g 'fns) <>e ()) [(intern <>e) 0 ['ptr (fnty g <>e)]]
           (: en (peep (g 'glob) <>e 0)
              ; a name that is neither a function nor a global: NAME IT (the same 'undecl pin
              ; cgfn uses), so an undeclared identifier in a static initializer reads as itself
              ; rather than as an IR dump -- which wears a codegen gap's face.
              (? !(two? en) (: _ (pin g 'undecl <>e) 'no)
                 (: ty <>en (? (arr? ty) [<en 0 ['ptr <>ty]] [<en 0 ['ptr ty]])))))
       'addr
        (: lv (imlval g <>e) (? (id? lv 'no) 'no [<lv <>lv ['ptr <>>lv]]))
       ('bin && member? <>e '(+ -))
        ; label +/- an int constant: a POINTER type scales the constant by the pointee, an
        ; integer-typed label ((word)&x + 8) adds raw bytes -- so scale only when it's a pointer
        (: op <>e
           (imscale ty) (? (ptr? ty) (tsz g <>ty) 1)
           pa (imaddr g <>>e) kb (icfold <>>>e)
           (? (pa != 'no && kb != 'no)
               [<pa (<>pa + (op @ '- -kb kb) * imscale <>>pa) <>>pa]
              (: pb (imaddr g <>>>e) ka (icfold <>>e)
                 (? (op = '+ && pb != 'no && ka != 'no)
                     [<pb (<>pb + ka * imscale <>>pb) <>>pb]
                    'no))))
       'no)
   ; a const FLOAT expression at image time -> its gem | 'bad. the int cfold lives at parse (array
   ; dims); floats fold here where the image lays them. love's doubles are the target's IEEE
   ; doubles, so the fold IS the runtime value.
   (ffold e)
    (? !(two? e) 'bad
       ; an int const SUBTREE floats -- musl's exp_data scales its doubles by
       ; (1 << EXP_TABLE_BITS), powf_data by (double)(1 << POWF_SCALE_BITS)
       (: iv (icfold e)
          (? !(id? iv 'no) (0.0 + iv)
          (<e @ 'flo <>e
              'num (0.0 + <>e)
              'cast (flo? <>e ? (ffold <>>e) 'bad)
              'un (: v (ffold <>>e) (? (bad? v) 'bad (<>e = 'neg) (0.0 - v) 'bad))
              'bin
              (: op <>e a (ffold <>>e) b (ffold <>>>e)
                 (? (bad? a || bad? b) 'bad
                 (op @ '+ (a + b) '- (a - b) '* (* a b)
                    '/ (b = 0.0 ? 'bad (a / b))
                    'bad)))
             'bad))))
   (fim ty v)                                       ; a float value's IEEE image as ty: 4 single bytes | 8 double bytes
    (ty = 'float ? (les 4 (fbits32 v)) (les 8 (fbits v)))
   (imgbytes g ty ini)                              ; a byte/fix list of length tsz(ty) | 'bad
    (? !(two? ini) (zeros (tsz g ty))
       (<ini = 'flo) (? (flo? ty) (fim ty <>ini) 'bad)
       (<ini = 'num) (? (aggish? ty) 'bad
                             (flo? ty) (fim ty (0.0 + <>ini))
                             (id? ty 'bool) (imnum g ty (? (<>ini = 0) 0 1))   ; a bool image normalizes (C11 6.3.1.2)
                             (imnum g ty <>ini))
       (flo? ty && member? <ini '(bin un cast))       ; a const float expression: fold, then image
        (: v (ffold ini)
           (? (bad? v) 'bad (fim ty v)))
       (<ini = 'str && arr? ty)
        (: t <>ini n (tsz g ty)
           (bs i acc) (? (n <= i) (rev acc) (bs (i + 1) ((? (i < tally t) (t i) 0) >< acc)))
           (bs 0 ()))
       (clarr? ty ini) (imgbytes g ty <>>ini)          ; a wide literal at file scope: its init is the image
       (ptr? ty) (: r (imaddr g ini)                    ; a data pointer: a constant ADDRESS (word-sized abs label+offset),
                     (? !(id? r 'no) [['fix (wsize g) (? (t32? g) 'abs32 'abs64) <r <>r]]      ; or a constant INTEGER ((int*)0x1000, a null)
                        (: v (icfold ini) (? (id? v 'no) 'bad (imnum g ty v)))))
       ; a label cast to a WORD-SIZED integer ((word)nif_absent, (intptr_t)f) is
       ; still the label's address; a non-label cast folds through to its value
       (<ini = 'cast && !(aggish? ty) && !(flo? ty) && tsz g ty = wsize g)
        (: r (imaddr g ini)
           (? !(id? r 'no) [['fix (wsize g) (? (t32? g) 'abs32 'abs64) <r <>r]]
              (: v (icfold ini) (? (id? v 'no) (imgbytes g ty <>>ini) (imnum g ty v)))))
       ; an integer scalar off a constant expression parse's cfold could not take: its fold is
       ; integer, so a float through a cast ((int)(0.867 * (1 << 16))) stands until here.
       (!(aggish? ty) && !(flo? ty) && member? <ini '(bin un cast))
        (: v (icfold ini) (? (id? v 'no) 'bad (imnum g ty v)))
       (<ini = 'init)
        (: nini (unelide g ty ini)                     ; normalize elided braces (idempotent)
         (? !(id? <nini 'init) (imgbytes g ty nini)           ; unwrapped ({ "s" } -> the bare literal): re-dispatch
            (arr? ty)
            (: elt <>ty esz (tsz g elt)
               (place l idx acc)                     ; -> ((off items)..) | 'bad
                (? !(two? l) acc
                   (: it <l
                      ci (? (<it = 'didx) <>it idx)
                      eb (imgpad g elt esz (elval it))
                      (? (bad? eb) 'bad (place >l (ci + 1) ([(* ci esz) eb] >< acc)))))
               ps (place <>nini 0 ())
               (? (bad? ps) 'bad (imflat (imsort ps) (* <>>ty esz))))
           (stru? ty)
            ; a bitfield member takes no placement of its own: it merges into its
            ; UNIT's value, and the unit OVERLAYS the flattened image. two of them
            ; share one offset -- and the unit shares its bytes with whatever else
            ; lives in them -- where a placement can only sit beside its neighbours
            (: e (peep (g 'stag) <>ty 0)
               fields <e size <>e
               (place l cur acc bfs)                ; -> (((off items)..) ((uoff uty bits)..)) | 'bad
                (? !(two? l) [acc bfs]
                   (: it <l
                      ci (? (<it = 'dfield) (fidx fields <>it) cur)
                      fe (nthfield fields ci)
                      (? (bad? fe) 'bad
                         (bitf? <>>fe)
                          (: v (icfold (elval it))
                             (? (id? v 'no) 'bad
                                (place >l (ci + 1) acc (bfmerge g <>fe <>>fe v bfs))))
                         (: fb (imgpad g <>>fe (tsz g <>>fe) (elval it))
                            (? (bad? fb) 'bad (place >l (ci + 1) ([<>fe fb] >< acc) bfs))))))
               pr (place <>nini 0 () ())
               (? (bad? pr) 'bad (bfover g <>pr (imflat (imsort <pr) size))))
           'bad))
       'bad)
   ; the bitfield units of one struct image, ((uoff uty value)..) -> ((uoff bytes)..).
   ; ⚠ arithmetic, never the bitwise ops: a 64-bit unit's mask is a big, where they are
   ; undefined -- % takes the low bits (a negative wrapping in) and * is the shift.
   (pw2 n) (? (n < 1) 1 (2 * pw2 (n - 1)))
   (bfmerge g uoff bt v rows)
    (: v9 (? (id? <>bt 'bool) (? (v = 0) 0 1) v)    ; a bool field's image normalizes
       m (pw2 <>>>bt)
       lo (((v9 % m) + m) % m)                      ; the low `width` bits, a negative wrapped in
       bits (lo * pw2 <>>bt)
       ; a merged row keeps the WIDER unit -- a bool/char unit sharing its byte with
       ; a wider neighbour must not truncate the neighbour's bits at the write
       (go l) (? !(two? l) [[uoff <>bt bits]]
                 (<<l = uoff) (: u9 (? (tsz g <><l < tsz g <>bt) <>bt <><l)
                                 ([uoff u9 (<>><l + bits)] >< >l))
                 (<l >< go >l))
       (go rows))
   ; OR each unit's bytes into a struct's flattened image. a relocation in the way
   ; refuses -- a fixup is one list item wearing eight bytes, so it cannot be OR'd.
   (bfover g rows bytes)
    (: (lay img uoff ub)
        (: (go l pos acc)
            (? !(two? l) acc
               (fixit? <l)
                (? (pos + 8 > uoff && uoff + tally ub > pos) 'bad
                   (go >l (pos + 8) (acc + [<l])))
               (: b (? (uoff <= pos && pos < uoff + tally ub) (<l | nthr ub (pos - uoff)) <l)
                  (go >l (pos + 1) (acc + [b]))))
           (go img 0 ()))
       (fold l img)
        (? !(two? l) img
           (: n (lay img <<l (imnum g <><l <>><l)) (? (bad? n) 'bad (fold >l n))))
       (fold rows bytes))
   (imgpad g ty w val)                              ; imgbytes padded (with zeros) to w bytes | 'bad
    (: b (imgbytes g ty val) (? (bad? b) 'bad (b + zeros (w - imbytelen b))))
   (cgimage g ty ini)                               ; the full span-padded image | 'bad
    (: span ((tsz g ty + wsize g - 1) & -(wsize g))         ; round to the WORD (8 on x64/arm64, 4 on t32)
       raw (imgbytes g ty ini)
       (? (bad? raw) 'bad (raw + zeros (span - imbytelen raw))))

   ; a declaration list ((name ty init)..): slots by SIZE (rounded to 8), extends env ->
   ; (forms env) | 'bad. aggregate + string locals zero the slot then fill, a scalar takes its
   ; value, and only a label-valued global image refuses.
   ; -- the locals-homing hook: every homable scalar local (outside an inline splice) ticks a decl
   ; SEQUENCE number, records (seq off ty) in g 'lrec, and asks g 'lhome (pinned only for a REGEN)
   ; for a home. a homed local keeps its slot as the spill shadow and binds (reg rN) in the env,
   ; so a READ is a zero-form borrow and its init lands with a mov. ⚠ the seq must be
   ; deterministic across pass 1 and the regen: same walk, and splices don't tick. --
   (cgdecl g env pairs)
    (: ; -- the C99 VLA local: free any prior instance from this SITE first (a loop re-runs the
       ; decl; sp = ptr + rounded size also frees everything younger), evaluate the dim, stash the
       ; EXACT byte size (szof reads it back), then sub sp and stash the block pointer. the fn
       ; stays out of sibcalls and homing (fesc + hasasm).
       ; ⚠ aarch64 spells register 31 as XZR in both the mov (ORR) and the shifted-ALU forms, so
       ; `(mov sp r1)` refuses and `(sub sp sp r0)` would assemble to `neg xzr, x0` -- sp
       ; untouched, silently. so every sp move here is a `lea` (ADD-immediate, the one form where
       ; 31 IS sp) and the 16-round is a shr/shl pair, ~15 being a ROTATED bitmask immediate that
       ; holo's encoder refuses. x64 lays the same forms, two bytes wider. --
       v16 '((add r0 r0 15) (shr r0 4) (shl r0 4))   ; r0 = r0 rounded UP to 16
       vsub '((lea r1 sp 0) (sub r1 r1 r0) (lea sp r1 0))   ; sp -= r0, through a plain register
       (vone ps env2 acc p ty)
        (: sl (peep (g 'vslots) (varix ty) ())
           (? !(two? sl) 'bad
              (: po <sl so <>sl
                 es (tsz g (varel ty))
                 _ (pin g 'fesc 1) _ (pin g 'hasasm 1)
                 dr (cgexpr g env2 (varn ty) ())
                 (? (bad? dr) 'bad
                    (: lf (nlab g ".vla")
                       code ([['ld 'r0 'r4 so] '(cmp r0 0) ['br 'eq lf]]
                             + v16
                             + [['ld 'r1 'r4 po] '(add r1 r1 r0) '(lea sp r1 0)
                                 ['label lf]]
                             + asint g dr
                             + (? (1 < es) (imby g 'r0 es) ()) + [['st 'r4 so 'r0]]
                             + v16 + vsub + ['(lea r0 sp 0) ['st 'r4 po 'r0]])
                       (one >ps ([<p po ty] >< env2) (acc + code)))))))
       (one ps env2 acc)
        (? !(two? ps) [acc env2]
           (: _ (psreset g)
              p <ps
              ty <>p
              init <>>p
              (? (gvarr? ty)
                  (? (two? init)
                      (nolane g "a variable-length array cannot take an initializer")
                     !(member? (peep g 'tgt 'amd64) '(amd64 arm64))
                      (nolane g ("no lane for a variable-length array" + ontgt g))
                     (vone ps env2 acc p ty))
                 (: span (& (tsz g ty + 7) -8)
              off (nslot g span)
              hreg (? (g 'inlining)
                      ; a call-free splice's body decl takes a pool seat when cginl's read
                      ; pricing licensed the name (inlrb); rpin holds it across the body's
                      ; statements, the splice frees it (inlrbs). ⚠ REGEN attempts only
                      ; (rgon): an ir1 bind changes the slot-touch counts lpick and the
                      ; homing payoff gate rank on, and the attempt flip is unpriced --
                      ; ioputx LOST ITS SIBCALL to one (a stack-growing call in the printer)
                      (? (g 'rgon &&
                          lhomable? ty &&
                          member? <p (g 'inlrb) &&
                          deep? (g 'pool))
                          (: rb (ralloc g)
                             (? !rb ()
                                (: _ (pins g 'rpin ((rp g rb) >< g 'rpin)
                                             'inlrbs (rb >< g 'inlrbs))
                                   rb))))
                      !(lhomable? ty || (d128? ty && w128? g)) ()
                      (: sq (1 + peep g 'dseq 0)
                         _ (pins g 'dseq sq
                                   'lrec ([sq off ty <p] >< g 'lrec))
                         lht (g 'lhome)
                         (? lht (lht sq) ())))
              ; a d128's home is a PAIR (lo hi) off the cs pool -- lpick packs two seats
              env3 ([<p (? (two? hreg) ['regw hreg] hreg ['reg hreg] off) ty] >< env2)
              ; a callee-saved home stays OFF g 'homes: no call wrap (the callee preserves it),
              ; no sib peel, and ezarg must never read its never-written shadow slot
              _ (? (hreg && !(two? hreg) && !(member? hreg (cspool g)))
                   (pin g 'homes ([(rp g hreg) off <p] >< g 'homes)))
              (? !(two? init) (one >ps env3 acc)
                 ((<init = 'init) || ((<init = 'str) && arr? ty) || clarr? ty init)
                  (? ((<init = 'init) && !(aggish? ty)) 'bad      ; a brace for a scalar
                     (: f (cgfill g env3 off ty init)
                        (? (bad? f) 'bad
                           (one >ps env3 (acc + cgzero off span + f)))))
                 (arr? ty) 'bad                          ; a bare-expr init of an array
                 ; the slot is the DESTINATION (doc/misc/proto/dest.l's mem die): a licensed
                 ; constant init of a plain slotted scalar delivers as ONE store-immediate
                 ; -- no li, no cvt, no r0. bool keeps the protocol (its cvt normalizes to
                 ; 0/1 where si would store the raw constant).
                 (x64t? g && !hreg && (<init = 'num) && charm? <>init &&
                  !(flo? ty || pair? g ty || d128? ty || id? ty 'bool) &&
                  siop9 ty <>init)
                  (one >ps env3 (acc + [[(siop9 ty <>init) 'r4 off <>init]]))
                 (stru? ty)                              ; a struct local from a struct VALUE (an address): copy its bytes
                  (: r (cgexpr g env3 init ())
                     (? (bad? r) 'bad
                        (: sz (tsz g ty)
                           cpf (: (go k acc9)
                                   (? (8 <= sz - k && !(t32? g))
                                       (go (k + 8) (acc9 + [['ld 'r1 'r0 k] ['st 'r4 (off + k) 'r1]]))
                                      (4 <= sz - k) (go (k + 4) (acc9 + [['ld4 'r1 'r0 k] ['st4 'r4 (off + k) 'r1]]))
                                      (2 <= sz - k) (go (k + 2) (acc9 + [['ld2 'r1 'r0 k] ['st2 'r4 (off + k) 'r1]]))
                                      (1 <= sz - k) (go (k + 1) (acc9 + [['ld1 'r1 'r0 k] ['st1 'r4 (off + k) 'r1]]))
                                      acc9)
                                  (go 0 ()))
                           (one >ps env3 (acc + tor0 g r + cpf)))))
                 (: w9 (? (hreg && !(two? hreg)) hreg ())   ; the home is the DESTINATION (the asn regv arm's shape)
                    r (cgexpr g env3 init w9)               ; a scalar (a pointer = string rides cgexpr -> la)
                    (? (bad? r) 'bad
                       (flo? ty) (one >ps env3 (acc + asflo g r + fstf g ty 'r4 off 'f0))
                       (pair? g ty) (one >ps env3 (acc + asll g r + pst 'r4 off))
                       (d128? ty)
                        (? !(w128? g) (no128 g)
                           (two? hreg)              ; a pair home's init is two register moves
                            (one >ps env3 (acc + as128 g r + [['mov <hreg 'r0] ['mov <>hreg 'r2]]))
                           (one >ps env3 (acc + as128 g r + [['st 'r4 off 'r0] ['st 'r4 (off + 8) 'r2]])))
                       hreg                        ; a homed local's init DELIVERS to the home (a narrow home lands canonical)
                        (? (flo? (vty r))
                           (one >ps env3 (acc + asintt g r ty + ([['mov hreg 'r0]] + cvt ty hreg)))
                           (rpeq? g (vreg r) hreg) ; the destination held: no bridge
                           (one >ps env3 (acc + vfs r + cvt ty hreg))
                           (: fs (vfs r + ([['mov hreg (vreg r)]] + cvt ty hreg))
                              _ (rfree g (vreg r))
                              (one >ps env3 (acc + fs))))
                       (one >ps env3
                            (acc + asintt g r ty +
                             cvt ty 'r0 +
                             [[(stop ty) 'r4 off 'r0]])))))))))
       _ (stq g)                                     ; one statement tick for the whole decl
       (one pairs env ()))

   ; block-scope `static` ((name ty init)..): each name gets a UNIQUE global laid in .bss/.data,
   ; NOT a stack slot. the name binds in the LOCAL env to a (glob lbl) marker (so it shadows outer
   ; names and stays distinct per function) and vent turns that into a 'glo, riding the same
   ; read/write/& paths a file-scope global does. no runtime init code -- a static's initializer
   ; is a compile-time constant image. -> (forms env) | 'bad, the same shape as cgdecl.
   (cgsdecl g env pairs)
    (: (one ps env2)
        (? !(two? ps) [() env2]
           (: p <ps
              name <p ty <>p init <>>p
              lbl (nlab g (g 'fn + "."))             ; `fn.N` -- the `.` bars any C-identifier collision
              _ (pin (g 'glob) lbl [lbl ty init])   ; a LOCAL data label (internal linkage): g 'statics,
              _ (pin g 'statics (lbl >< g 'statics))      ; NOT g 'globs -- a static is never an exported symbol
              (one >ps ([name ['glob lbl] ty] >< env2))))
       _ (stq g)                                     ; the statement tick (alive numbers sdecls too)
       (one pairs env))

   ; stmt -> forms | 'bad
   ; -- inline asm (the 'asm statement): the template is holo neutral TEXT (asm-text parses it),
   ; %0..%9 the operands (outputs first, GNU numbering), %% a literal %. the body assembles to
   ; bytes RIGHT HERE and splices as ONE opaque ('raw ..), so the IR passes barrier on it,
   ; template labels stay local, and a4ize never rewrites inside. operands stage through the
   ; machine stack: every input evaluates and pushes, then pops into its register; outputs push
   ; after the body and store back through clval in REVERSE, so the store scratch can never
   ; trample an unstored output. constraints: "=r"/"+r"/"r" pick a register, "=rN"/"+rN"/"rN" pin
   ; one, "i" an immediate; clobbers are validated only -- an asm fn's homing is OFF (g 'hasasm),
   ; so nothing lives in a register across the statement. x64 refuses r4 (the frame) and the
   ; callee-saved r11-r14; arm64 adds r4 (x4, an argument register there). --
   asmregs '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10)
   asmpick '(r10 r9 r8 r7 r6 r5 r2 r1 r0)
   asmnames (foldl (\ t i (: s9 ("r" + show i) (pin t (name s9) s9))) {} (jot 11))
   (asmreg? g r9) (member? r9 asmregs || (a64? g && id? r9 'r4))
   ; a constraint's register part: "r" -> 'pick, "i" -> 'imm, "rN" -> the nom;
   ; the x86 letters "a"/"d" pin rax/rdx (the divq protocol), "rm" reads as "r"
   ;; "m" is a MEMORY operand, and the neutral surface has no such addressing mode as one token:
   ;; holo spells memory as a base and a displacement, two operands. So a "m" input lowers to the
   ;; ADDRESS of its expression in a picked register and substitutes as `rN, 0` -- two tokens where
   ;; the others substitute one, which is exactly what `vmxon %0` needs to read as `vmxon r3, 0`.
   ;; Input only: "=m" would want a store the body cannot be asked to make.
   (asmc c9) (: n #c9
                (? (n < 1) 'bad
                   (n = 1) (c9 0 @ 114 'pick 105 'imm 109 'mem 97 'r0  100 'r2 'bad)
                   (c9 = "rm") 'pick
                   (name c9)))
   ; %-substitute the template: al the operand texts, GNU order
   (asmsub tm al)
    (: n #tm
       (go i acc)
        (? (n <= i) acc
           (: c (tm i)
              (? !(c = 37) (go (i + 1) (acc + string c))
                 (i + 1 < n)
                  (: d (tm (i + 1))
                     (? (d = 37) (go (i + 2) (acc + "%"))
                        (48 <= d <= 57)
                         (: ix (d - 48)
                            (? (ix < #al) (go (i + 2) (acc + nthr al ix)) 'bad))
                        'bad))
                 'bad)))
       (go 0 ""))
   asmhb holo-bytes                                 ; bare: the cat leads with (use 'holo) (asbook.l), so this folds at compile
   (cgasm g env s)
    (: _ (pin g 'hasasm 1)
       tmpl <>s
       ; one operand list, GNU order: ('o|'x reg expr) an output (x in-out),
       ; ('i reg expr) a register input, ('b reg (addr expr)) a MEMORY input, ('m imm) an immediate
       (oconv l acc)
        (? !(two? l) (rev acc)
           (: c <<l n #c
              h (0 < n ? (c 0) 0)
              k (h = 61 || h = 43 ? (asmc (snip c 1 n)) 'bad)
              (k @ ('bad || 'imm || 'mem) 'bad
                 (oconv >l ([(? (h = 43) 'x 'o) k <><l] >< acc)))))
       (iconv l acc)
        (? !(two? l) (rev acc)
           (: k (asmc <<l) e <><l
              (k @ 'bad 'bad
                   'imm
                    (two? e && id? <e 'num && charm? <>e ?
                     (iconv >l (['m <>e] >< acc)) 'bad)
                   'mem (iconv >l (['b 'pick ['addr e]] >< acc))
                   (iconv >l (['i k e] >< acc)))))
       os (oconv <>>s ())
       is9 (iconv <>>>s ())
       (? (bad? os || bad? is9) 'bad
          (: ops9 (os + is9)
             ; pinned registers: validated, a duplicate refuses
             (pinset l acc)
              (? !(two? l) acc
                 (: e2 <l k <>e2
                    (? (id? <e2 'm || id? k 'pick) (pinset >l acc)
                       !(asmreg? g k) 'bad
                       (member? k acc) 'bad
                       (pinset >l (k >< acc)))))
             pinned (pinset ops9 ())
             (? (bad? pinned) 'bad
                (: (assign l fr acc)
                    (? !(two? l) (rev acc)
                       (: e2 <l k <>e2
                          (? (id? <e2 'm) (assign >l fr (k >< acc))
                             (id? k 'pick)
                              (? (two? fr) (assign >l >fr (<fr >< acc)) 'bad)
                             (assign >l fr (k >< acc)))))
                   asgn (assign ops9 (filter (\ r9 !(member? r9 pinned)) asmpick) ())
                   (cok? l) (? !(two? l) 1
                               (: c <l
                                  (? ((c = "memory") || (c = "cc")) (cok? >l)
                                     (asmreg? g (intern c)) (cok? >l)
                                     ())))
                   (? (bad? asgn || !(cok? <>>>>s)) 'bad
                      (: ; the operand TEXTS, walked against their kinds: an immediate prints as a
                         ; number, a register as its name, and a "m" operand as `rN, 0` -- the base
                         ; and displacement holo's memory operands are spelled with.
                         (optext l al acc)
                          (? !(two? l) (rev acc)
                             (: a9 <al
                                (optext >l >al ((? (charm? a9) (show a9)
                                                   (id? <<l 'b) ((peep asmnames a9 "r?") + ", 0")
                                                   (peep asmnames a9 "r?")) >< acc))))
                         txt (asmsub tmpl (optext ops9 asgn ()))
                         (? (bad? txt) 'bad
                            (: ; inputs (and in-out outputs) evaluate and PUSH..
                               (loads l acc)
                                (? !(two? l) acc
                                   (: e2 <l
                                      (? (member? <e2 '(x i b))
                                          (: r (cgexpr g env <>>e2 ())
                                             (? (bad? r) 'bad
                                                (flo? (vty r) || stru? (vty r)) 'bad
                                                (loads >l (acc + tor0 g r + spush 'r0))))
                                         (loads >l acc))))
                               lf (loads ops9 ())
                               (? (bad? lf) 'bad
                                  (: ; ..then POP into their registers, reverse order
                                     (pops l al acc)
                                      (? !(two? l) acc
                                         (? (member? <<l '(x i b))
                                            (pops >l >al (spop <al + acc))
                                            (pops >l >al acc)))
                                     ; the body: text -> IR forms -> BYTES, one opaque raw
                                     body [['raw (asmhb (peep g 'tgt 'amd64) (asm-text txt))]]
                                     ; outputs: push each register, store back reverse
                                     (opush l al acc)
                                      (? !(two? l) acc
                                         (? (member? <<l '(o x))
                                            (opush >l >al (acc + spush <al))
                                            (opush >l >al acc)))
                                     (ostores l al acc)
                                      (? !(two? l) acc
                                         (: e2 <l
                                            (? !(member? <e2 '(o x)) (ostores >l >al acc)
                                               (: lv (matv g (clval g env <>>e2))
                                                  (? (bad? lv) 'bad
                                                     (: ty (vty lv)
                                                        (? (flo? ty || stru? ty || arr? ty || bitf? ty) 'bad
                                                           (ostores >l >al
                                                            ((vfs lv + spop 'r1 + [[(stop ty) 'r0 0 'r1]]) + acc)))))))))
                                     osf (ostores ops9 asgn ())
                                     (? (bad? osf) 'bad
                                        (lf + pops ops9 asgn () + body + opush ops9 asgn () + osf)))))))))))))

   ; the statement CHILD MAP, one source of truth: which sub-forms of a statement are themselves
   ; statements, in forward (tick) order -- a for's init only when it declares. alive's prepass
   ; counts by it and scasel scans by it, and ⚠ cgstmt's walk must agree node for node.
   (stkids s)
    (s @ ('blk items) items
         ('if _ thn els) [thn els]
         ('if _ thn) [thn]
         ('while _ body) [body]
         ('do . body . _) [body]
         ('for ini _ _ body && two? ini && member? <ini '(decl sdecl)) [ini body]
         ('for _ _ _ body) [body]
         ('switch _ items) items)

   ; -- alive: which homable names still matter across each call (the homing regens' liveness).
   ; one backward statement-grain walk over the fn body, over a universe fixed up front: homable
   ; params + homable locals, minus any address-taken or multiply-declared name (two vars sharing
   ; a name must not conflate -- both keep the blanket wrap). a statement whose own expressions
   ; call records wrapset = livein u liveout under a statement counter that ⚠ ticks EXACTLY as the
   ; regen's cgstmt/cgdecl/cgsdecl entries do (stq below; a splice never ticks). the walk numbers
   ; DOWN from the prepass total, so the child-reversed backward order lands forward pre-order
   ; numbers; a livtab miss reads 'all, and a drifted walk is caught LOUD.
   ; loops seed with everything the loop reads (the back edge, no fixpoint -- a post-back-edge
   ; read is textually in the loop; a goto ESCAPES, so as a loop use it reads the whole universe);
   ; a label is a backward no-op. answers (livtab cross univ total ncw): livtab sq -> wrapset,
   ; cross name -> static call crossings, univ the analyzed names -- ⚠ a gate must never trust
   ; cross for an outsider -- total the statement count (the regen's numbering guard), ncw the
   ; fn's loop-weighted IN-LOOP call count (the payoff gate's per-iteration price; a depth-0
   ; call sits in the path maximum instead -- it runs at most once per invocation), spa the
   ; per-name live SPAN in tick numbers (see spn).
   (alive g f)
    (: params <>>f
       items <><>>>f
       dcl {} tak {} n9 {} liv {} crs {} spa {}
       crh {} rdc {} rdt {} sqm {}
       ; a HARD call: one no splice can erase (the callee is not an inline
       ; candidate) -- classification is economics, never soundness: a wrapped
       ; pool home survives any emitted call, this only guides who gets one
       (nhcl l) (? !(two? l) 0 (+ (nhc <l) (nhcl >l)))
       ; soft = an inline candidate (inlok? gated its size and shape at
       ; registration, so membership means the site will very likely splice);
       ; the armed shadow covers the rare decline soundly
       (soft? nm) (two? (g 'inl nm))
       ; the one build homes int locals like the dance's slot walk did, so its
       ; universe widens to lhomable -- the dance's own reading stays homable
       ; (its lalloc free?-gate keys on this set, byte-held under the knobs)
       wide9 (uniw? g)
       (nhc e) (? !(two? e) 0
                  (e @ ('call ('var nm) as) ((? (soft? nm) 0 1) + nhcl as)
                       ('call fh as) (1 + (nhc fh) + nhcl as)
                       ('szof >< _) 0
                       (nhcl e)))
       ; a return's TAIL position: the sibcall pass turns a ret-tail call into a
       ; jump, and a home is dead at its own return either way -- so the last
       ; call of a comma spine, each ternary arm, and a cast's operand harden
       ; nothing; only calls evaluated BEFORE them do
       (nhcr e) (? !(two? e) 0
                   (e @ ('call fh as) ((nhc fh) + nhcl as)
                        ('comma a b) ((nhc a) + nhcr b)
                        ('cond c a b) ((nhc c) + nhcr a + nhcr b)
                        ((k _ x) && member? k '(un cast)) (nhcr x)
                        (nhc e)))
       ; protocol dirt: a call (its splice's staging owns r0-r3), div/mod (the
       ; r0:r2 protocol), a variable shift (the count rides r1), a d128 cast
       ; (the wide kit rides r0:r2). a fn with NONE of these anywhere may ride
       ; a quad arrival -- nothing in its body can write the register
       (pdt e) (? !(two? e) 0
                  (e @ ('szof >< _) 0
                       ('call >< _) 1
                       ('asm >< _) 1
                       ('bin op a b && member? op '(/ %)) (1 + pdt a + pdt b)
                       ('bin op a b && member? op '(shl shr) &&
                        !(two? b && id? <b 'num)) (1 + pdt a + pdt b)
                       ('cast ty x) ((? (d128? ty) 1 0) + pdt x)
                       (pdtl e)))
       (pdtl l) (? !(two? l) 0
                   ((? (two? <l) (pdt <l) 0) + pdtl >l))
       ; the touch counters (rank food): every universe mention is a touch --
       ; read or write, as ir1's slot counter saw them -- raw and loop-weighted
       (bump9 nm w) (: _ (pin rdc nm (1 + peep rdc nm 0))
                       _ (pin rdt nm (w + peep rdt nm 0))
                       0)
       (rdn e w)
        (? !(two? e) 0
           (e @ ('var nm) (bump9 nm w)
                ('szof >< _) 0
                ('decl ps)                           ; an initialized decl STORES: that touch
                 (: (go2 l) (? !(two? l) 0           ; is real (ir1 counted it) but spells no var
                              (: p9 <l
                                 _ (? (two? <>>p9) (bump9 <p9 w) 0)
                                 _ (rdn <>>p9 w)
                                 (go2 >l)))
                    (go2 ps))
                (: w9 (? (member? <e '(while do for)) (w * 8) w)
                   (go l) (? !(two? l) 0
                             (: _ (? (two? <l) (rdn <l w9) 0) (go >l)))
                   (go e))))
       ; -- prepass: &-taken names, one generic sweep (sizeof's unevaluated
       ; &v over-records -- harmless, the name just keeps the blanket) --
       (pex e) (? !(two? e) ()
                  (: _ (e @ ('addr ('var nm)) (pin tak nm 1) ())
                     ; ⚠ the HEAD too: an init list's first element is an EXPRESSION in head
                     ; position (a clit's `&x` first initializer -- love.c's mm() -- hid there)
                     _ (? (two? <e) (pex <e) ())
                     (pexl >e)))
       (pexl l) (? !(two? l) () (: _ (pex <l) (pexl >l)))
       ; -- prepass: the tick total, declaration counts, homable locals (⚠ ticks mirror lst's
       ; below, statement for statement; the child walk is stkids', the shared map) --
       (pre s)
        (? !(two? s) ()
           (: k <s
              _ (pin n9 'n (1 + peep n9 'n 0))
              _ (? (k = 'asm) (pin n9 'asm 1) ())
              (? (member? k '(decl sdecl))
                  (: (go ps) (? !(two? ps) ()
                                (: p <ps
                                   _ (pin dcl <p (1 + peep dcl <p 0))
                                   _ (? ((k = 'decl) &&
                                         (? wide9 (lhomable? <>p) (homable? <>p)))
                                        (pin n9 'loc (<p >< peep n9 'loc ())) ())
                                   _ (? ((k = 'decl) && gvarr? <>p) (pin n9 'vla 1) ())
                                   ; the decl SEQUENCE, predicted: cgdecl's dseq
                                   ; ticks for exactly these (post-VLA, lhomable
                                   ; or a carried d128), in this walk order
                                   _ (? ((k = 'decl) && !(gvarr? <>p) &&
                                         (lhomable? <>p || (d128? <>p && w128? g)))
                                        (: q2 (1 + peep n9 'sq 0)
                                           _ (pin n9 'sq q2)
                                           (pin sqm q2 [<p <>p])) ())
                                   (go >ps)))
                     (go <>s))
                 (prel (stkids s)))))
       (prel l) (? !(two? l) () (: _ (pre <l) (prel >l)))
       (pdcl ps) (? !(two? ps) ()
                    (: _ (pin dcl <<ps (1 + peep dcl <<ps 0))
                       (pdcl >ps)))
       ; -- the universe: the homable gp walk for the param NAMES, plus the locals --
       (pgo ps gpi acc) (homefold g ps gpi acc
                          (\ a2 i9 p9 (? (? wide9 (lhomable? <>p9) (homable? <>p9))
                                         (<p9 >< a2) a2)))
       _ (pdcl params)
       _ (prel items)
       _ (pexl items)
       univ (filter (nm \ dcl nm = 1 && !(tak nm))
                    (pgo params 0 () + n9 'loc))
       ; ⚠ a homable param OUTSIDE the universe (shadowed, or a sizeof-& over-record) still HOMES,
       ; so it must ride EVERY wrapset -- else the filter strips its one true spill
       pales (filter (\ nm !(member? nm univ)) (pgo params 0 ()))
       ; -- the set kit (names are strings; member? compares content) --
       (sadd s2 nm) (? (member? nm s2) s2 (nm >< s2))
       (sun s2 b) (? !(two? b) s2 (sun (sadd s2 <b) >b))
       (sdel (a >< u) nm) (a = nm ? u (a >< sdel u nm))
       (tick u) (: q (peep n9 'q 0) _ (pin n9 'q (q - 1)) q)
       ; the INTERVAL leg: every statement folds its live set into
       ; a per-name [lo hi] tick span. min/max, so a hole inside costs nothing -- but the extent
       ; must be a SUPERSET of the true live range, so the control-only lanes record too: a goto
       ; reads the whole universe and a break's set is its target's, and neither is bounded by
       ; the ticks that carry a def or a use.
       (spn sq l)
        (? !(two? l) ()
           (: nm <l o (peep spa nm ())
              _ (pin spa nm (? !(two? o) [sq sq]
                                (: a <o b <>o [(? (sq < a) sq a) (? (b < sq) sq b)])))
              (spn sq >l)))
       ; nclw = the statement's IN-LOOP share of ncl (0 at depth 0): the fn total n9 'ncw feeds
       ; the payoff gate -- a depth-0 call runs at most once per invocation
       (rec sq w ncl nclw nclh)                     ; a call-bearing statement's record
        (: _ (spn sq w)
         (? (ncl < 1) ()
           (: _ (pin liv sq (sun w pales))          ; the outside-universe homes ride every wrap
              _ (pin n9 'ncw (nclw + peep n9 'ncw 0))
              _ (? (0 < nclh) (pin n9 'nhc (nclh + peep n9 'nhc 0)) ())
              (bump (a >< u)) (pin crs a (ncl + peep crs a 0),
                               (? (0 < nclh) (pin crh a (nclh + peep crh a 0)) 0),
                               bump u)
            (bump w))))
       ; -- reads of an expression, restricted to the universe -> (set ncalls). a var WRITE is not
       ; a read (the home register takes the value directly), sizeof's operand never evaluates,
       ; and an unknown head reads EVERYTHING and counts a call (future grammar fails safe) --
       (erd e s2 nc)
        (? !(two? e) [s2 nc]
           (e @ ('var nm) [(? (member? nm univ) (sadd s2 nm) s2) nc]
                ((k >< _) && member? k '(num flo str szof)) [s2 nc]
                ((k . x . _) && member? k '(dot deref addr post vastart vaend vaarg)) (erd x s2 nc)
                ((k _ x) && member? k '(un cast)) (erd x s2 nc)
                ('asn dst rhs) (: r (erd rhs s2 nc)
                                  (? (<dst = 'var) r (erd dst <r <>r)))
                ('bin _ a b) (: r (erd a s2 nc) (erd b <r <>r))
                ((k a b) && member? k '(land lor comma)) (: r (erd a s2 nc) (erd b <r <>r))
                ('cond c a b) (: r (erd c s2 nc) r2 (erd a <r <>r) (erd b <r2 <>r2))
                ('call fh as) (: r (erd fh s2 (nc + 1)) (erdl as <r <>r))
                ('clit _ ini) (erdi ini s2 nc)
                ('init items) (erdi items s2 nc)
                [(sun s2 univ) (nc + 1)]))
       (erdl l s2 nc) (? !(two? l) [s2 nc] (: r (erd <l s2 nc) (erdl >l <r <>r)))
       (erdi l s2 nc)                               ; init items: dfield/didx wrap their values
        (? !(two? l) [s2 nc]
           (: it <l
              v (? (two? it && member? <it '(dfield didx)) <>>it it)
              r (erd v s2 nc)
              (erdi >l <r <>r)))
       ; -- a loop's uses: every name its cond/step/body read, kills ignored
       ; (the back-edge seed); a goto inside escapes -> the whole universe --
       (ust s acc)
        (s @ ('blk items) (ustl items acc)
             ('expr e9) <(erd e9 acc 0)
             ('ret . rv . _) (? (two? rv) <(erd rv acc 0) acc)
             (('decl || 'sdecl) ps) (ustp ps acc)
             ('if cnd thn els) (: a1 <(erd cnd acc 0) a2 (ust thn a1) (ust els a2))
             ('if cnd thn) (: a1 <(erd cnd acc 0) (ust thn a1))
             ('while cnd body) (ust body <(erd cnd acc 0))
             ('do body cnd) (ust body <(erd cnd acc 0))
             ('for ini cnd stp body)
              (: a1 (ini @ (('decl || 'sdecl) ps) (ustp ps acc)
                           (_ . x . _) <(erd x acc 0)
                           acc)
                 a2 (? (two? cnd) <(erd cnd a1 0) a1)
                 a3 (? (two? stp) <(erd stp a2 0) a2)
                 (ust body a3))
             ('switch ce items) (ustl items <(erd ce acc 0))
             (('goto || 'asm) >< _) (sun acc univ)
             acc)
       (ustl l acc) (? !(two? l) acc (ustl >l (ust <l acc)))
       (ustp ps acc) (? !(two? ps) acc (ustp >ps <(erd <>><ps acc 0)))
       ; -- the backward walk: statements in reverse, children before their parent's tick
       ; (counting down = forward pre-order numbers). w9 is the LOOP WEIGHT (x8 per nesting
       ; level): a crossing inside a loop pays its wrap per iteration while the touch count stays
       ; static, so cross charges it heavier. ⚠ wrapsets (soundness) never depend on w9. --
       (litems l out brk cnt w9)
        (: (go l2 o) (? !(two? l2) o (go >l2 (lst <l2 o brk cnt w9)))
           (go (rev l) out))
       (dpairs ps o nc)                             ; reversed decl pairs -> (in ncalls)
        (? !(two? ps) [o nc]
           (: p <ps
              r (erd <>>p () 0)
              o2 (sun (? (member? <p univ) (sdel o <p) o) <r)
              (dpairs >ps o2 (nc + <>r))))
       (lst s out brk cnt w9)
        (? !(two? s) out
           (s @
              ('blk items) (: in (litems items out brk cnt w9) sq (tick ()) _ (spn sq in) in)
              ('expr e9)
               (: kv (e9 @ (('asn . ('var v) . _) && member? v univ) v ())
                  r (erd e9 () 0)
                  in (sun (? kv (sdel out kv) out) <r)
                  ; ⚠ the KILL rides the record too: a name born in a call-bearing statement is
                  ; invisible to in/out yet lives across the statement's OWN calls, and
                  ; statement-grain cannot order intra-statement events
                  sq (tick ()) _ (rec sq (sun (sun in out) (? kv [kv] ())) (* <>r w9)
                                      (? (1 < w9) (* <>r w9) 0) (* (nhc e9) w9))
                  in)
              ('ret . rv . _)
               (: r (? (two? rv) (erd rv () 0) '(() 0))
                  _ (pin n9 'nx (1 + peep n9 'nx 0))
                  ; a TAIL call never crosses its own arguments -- they die AT it,
                  ; and nothing lives after a return -- so only calls INSIDE the
                  ; head or the args harden this statement (the VM's tail-forward
                  ; dispatch fleet rides this: return (*fp)(a, b) crosses nothing)
                  sq (tick ()) _ (rec sq <r (* <>r w9) (? (1 < w9) (* <>r w9) 0)
                                      (* (nhcr rv) w9))
                  <r)
              ('decl ps)
               (: pr (dpairs (rev ps) out 0)
                  ks (: (go p9) (? !(two? p9) ()      ; the declared names: kills, in the record (see expr)
                                   (: nm <<p9
                                      (? (member? nm univ) (nm >< go >p9) (go >p9))))
                        (go ps))
                  sq (tick ()) _ (rec sq (sun (sun <pr out) ks) (* <>pr w9)
                                      (? (1 < w9) (* <>pr w9) 0)
                                      (* (: (hs l) (? !(two? l) 0 (+ (nhc <>><l) (hs >l))) (hs ps)) w9))
                  <pr)
              ('sdecl >< _) (: sq (tick ()) _ (spn sq out) out)   ; statics: no runtime init, no kill
              ('if >< cnd >< thn >< er)
               (: els <er
                  inE (? (two? els) (lst els out brk cnt w9) out)   ; else FIRST (reverse order)
                  inT (lst thn out brk cnt w9)
                  r (erd cnd () 0)
                  in (sun (sun inT inE) <r)
                  sq (tick ()) _ (rec sq (sun in out) (<>r * w9) (? (1 < w9) (<>r * w9) 0)
                                      ((nhc cnd) * w9))
                  in)
              ('while cnd body)
               (: w8 (8 * w9)
                  seed (sun out (ust body <(erd cnd () 0)))
                  _ (lst body seed out seed w8)
                  r (erd cnd () 0)
                  sq (tick ()) _ (rec sq seed (<>r * w8) (<>r * w8) ((nhc cnd) * w8))
                  seed)
              ('do body cnd)
               (: w8 (8 * w9)
                  seed (sun out (ust body <(erd cnd () 0)))
                  r (erd cnd () 0)
                  _ (lst body seed out seed w8)
                  sq (tick ()) _ (rec sq seed (<>r * w8) (<>r * w8) ((nhc cnd) * w8))
                  seed)
              ('for ini cnd stp body)
               (: w8 (8 * w9)
                  useb (: u1 (? (two? cnd) <(erd cnd () 0) ())
                          u2 (? (two? stp) <(erd stp u1 0) u1)
                          (ust body u2))
                  seed (sun out useb)
                  _ (lst body seed out seed w8)
                  nc9 (+ (? (two? cnd) <>(erd cnd () 0) 0)
                         (? (two? stp) <>(erd stp () 0) 0))
                  ri (? (two? ini && !(member? <ini '(decl sdecl)))
                        (erd <>ini () 0) '(() 0))
                  kv8 (? (two? ini && !(member? <ini '(decl sdecl)))
                         (<>ini @ (('asn . ('var v) . _) && member? v univ) v ())
                         ())
                  in (? !(two? ini) seed
                        (member? <ini '(decl sdecl)) (lst ini seed () () w9)   ; ticks (cgdecl does); kill applies; runs ONCE
                        (sun (? kv8 (sdel seed kv8) seed) <ri))
                  ; kv8 joins the record: the init's kill (see the expr lane)
                  sq (tick ()) _ (rec sq (sun (sun seed <ri) (? kv8 [kv8] ())) (nc9 * w8 + <>ri * w9)
                                      (nc9 * w8 + (? (1 < w9) (<>ri * w9) 0))
                                      (((? (two? cnd) (nhc cnd) 0) + (? (two? stp) (nhc stp) 0)) * w8 +
                                       (? (two? ini && !(member? <ini '(decl sdecl))) (nhc <>ini) 0) * w9))
                  in)
              ('switch ce items)
               (: r (erd ce () 0)
                  _ (pin n9 'sw (() >< peep n9 'sw ()))       ; push this switch's marker set
                  _ (litems items out out cnt w9)            ; brk = out; cont targets the outer loop
                  macc <(peep n9 'sw ())
                  _ (pin n9 'sw >(peep n9 'sw ()))            ; pop
                  in (sun (sun <r macc) out)
                  sq (tick ()) _ (rec sq in (<>r * w9) (? (1 < w9) (<>r * w9) 0)
                                      ((nhc ce) * w9))
                  in)
              (('case || 'dflt) >< _)
               (: sw (peep n9 'sw ())
                  _ (? (two? sw) (pin n9 'sw (sun <sw out >< >sw)) ())
                  sq (tick ()) _ (spn sq out) out)
              ('brk >< _) (: sq (tick ()) _ (spn sq brk) brk)
              ('cont >< _) (: sq (tick ()) _ (spn sq cnt) cnt)
              ('goto >< _) (: sq (tick ()) _ (spn sq univ) univ)   ; any target: everything
              ('lbl >< _) (: sq (tick ()) _ (spn sq out) out)
              ('asm >< _) (: sq (tick ()) _ (rec sq univ w9 (? (1 < w9) w9 0) w9) univ)   ; homing is off anyway (hasasm)
              (: sq (tick ()) _ (spn sq out) out)))   ; nop tdef
       ; -- run: liveout at the fn end is empty; the counter descends from the prepass total so
       ; lst's numbers land forward pre-order. ⚠ pre and lst disagreeing on the statement set is a
       ; compiler bug, so the counter must land exactly on zero or alive answers () --
       _ (pin n9 'q (peep n9 'n 0))
       _ (litems items () () () 1)
       _ (rdn items 1)
       ; d128 material anywhere: the wide-pair park rides two POOL registers per
       ; op (the cell never opens while they hold), so no ride may drain the
       ; file here -- mag_mul's inner loop reopened its stack cell the one time
       ; this was let through
       (wdc e) (? !(two? e) ()
                  (id? <e 'szof) ()
                  (id? <e 'cast && d128? <>e) 1
                  (: (go l) (? !(two? l) ()
                               (two? <l && wdc <l) 1
                               (go >l))
                     (go e)))
       wd9 ((: (go i) (? ((peep n9 'sq 0) < i) ()
                         (: e2 (peep sqm i ())
                            (? (two? e2 && d128? <>e2) 1 (go (i + 1)))))
            (go 1)) ||
            (: (go l) (? !(two? l) () (d128? <><l) 1 (go >l)) (go params)) ||
            (wdc items))
       pq9 ((0 = pdtl items) && !wd9)
       (? (0 = peep n9 'q 0)
          [liv crs univ (peep n9 'n 0) (peep n9 'ncw 0) spa
           crh rdc rdt sqm (peep n9 'nx 0)
           (peep n9 'asm 0) (peep n9 'vla 0) (peep n9 'sq 0) tak
           (peep n9 'nhc 0) (? pq9 1 ()) (? wd9 1 ())] ()))
   ; the alive pass's forward twin: cgstmt/cgdecl/cgsdecl entries tick this counter and pin the
   ; statement's wrapset (g 'stwrap) off g 'livtab -- a miss pins 'all, the blanket wrap. an
   ; inline splice neither ticks nor re-pins: it compiles under its statement's set.
   (stq g)
    (? (g 'inlining) (peep g 'stwrap 'all)
       (: q (1 + peep g 'lsq 0)
          _ (pin g 'lsq q)
          lt (g 'livtab)
          w (? lt (peep lt q 'all) 'all)
          _ (pin g 'stwrap w)
          w))
   ; the regen channels' NEUTRAL state, spelled once: per-fn entry and the
   ; regen teardown ride the same reset (a wrap set dies with its fn)
   (rgreset g)
    (pins g
     'livtab ()  ; liveness (set only for a wrap-bearing regen)
     'lhome ()   ; locals-homing map (cgdecl's hook reads it)
     'rgon ()    ; ...and its regen flag (the splice decl gate)
     'pcs ()     ; the param cs-seat grant ((gpi reg)..; the shadow spill reads it)
     'homes ())  ; non-leaf home set (call-fixed + sibs read it)
   ; ...and their ARMED state: a regen build homes locals (lh9, lpick's map)
   ; and wraps per-call off the liveness (av9, alive's answer)
   (rgset g lh9 av9)
    (pins g
     'homes ()
     'lhome lh9
     'rgon 1
     'livtab <av9)
   ; the per-BUILD counters, spelled once: build arms them fresh on each of
   ; its (up to two) runs per fn
   (bldreset g) (pins g
    'dseq 0                    ; the locals-homing decl sequence
    'lrec ()
    'lsq 0                     ; the alive statement counter
    'stwrap 'all
    'vrn 0                     ; the vreg mint counter (the shadow: ralloc mints, vrfix resolves)
    'vrt {})           ; ...and its per-build map, mint -> physical
   ; the g-pin RECORD: every per-fn pin and its neutral value, one roster -- cgfn's entry
   ; and each regen attempt wipe the lot, so an attempt always starts from the clean
   ; record. a NEW per-fn pin registers here; the computed ones (fn, epi, pool0/pool,
   ; vslots, the fc cells, sret's slot) re-pin over their neutrals at their owner.
   (fnreset g)
    (pins (rgreset g)          ; the regen channels' neutral state -- disjoint pin set
     'off 8                    ; the frame cursor: slot -8 = saved r3 (see epi)
     'slots ()                 ; the frame's object map ((off n)..; nslot appends)
     'fesc ()                  ; escape flag (the sibcall gate reads it)
     'inlining () 'inlbody 0   ; splice depth + body depth (the bind lanes' blind)
     'inlret () 'inlrt ()      ; splice return label + type (cgstmt's ret door)
     'inlrb () 'inlrbs ()      ; splice decl-license + its reg roster
     'rpin ()                  ; splice bind pins
     'hasasm ()                ; an asm statement pins it (homing off)
     'cssv ()                  ; the callee-saved save forms (a cs regen pins)
     'sret ())                 ; the memory-return slot (the driver re-pins under sr6f)

   (cgstmt g env s)
    (: _ (psreset g)                                 ; a statement never carries a live operand in (splice binds do)
       myw (stq g)                                   ; this statement's wrapset (alive's forward twin)
       (s @
          ('ret >< rs)                                                     ; inside an inline splice a return JUMPS to the
          (: il (g 'inlret)                                      ; splice's end label instead of the epilogue
             epj (peep g 'epi epi)                                       ; the TARGET's epilogue (cgen-core pins it)
             mt (1 < tally rs)                                           ; clay's (ret e PRE): presence is the arity
             rr9
             (? (mt && il) 'bad                     ; a musttail never inlines: the splice declines, the real call still can
                !(two? <rs)
                 (? mt (nolane g "a musttail return carries no call") (? il [['jmp il]] epj))   ; bare `return;` -- just the epilogue
                (: _ (? mt (pin g 'must9 1) 0)      ; the ret's own call STAYS a call (no splice) -- consumed at first touch
                   r (cgexpr g env <rs 'r0)         ; the return value's one true home
                   _ (pin g 'must9 ())
                   rt (? il (peep g 'inlrt 'long)                        ; the INLINED function's return type
                         <(peep (g 'sigs) (g 'fn) ['long ()]))         ; else the current function's
                   res
                   (? (bad? r) 'bad
                      (flo? rt)                                        ; a double return leaves f0 (xmm0; never inline -- gated);
                       (asflo g r                                      ; v6-M base ABI carries a double out on the r0:r1 pair,
                        + (? !(v6m? g) ()                              ; a bare float as ONE WORD (binary32 in r0)
                             (id? rt 'float) '((cvtsd2ss f0 f0) (movqrx r0 f0))
                             '((movqrx r0 f0)))
                        + epj)
                      (v6m? g && stru? rt)                 ; v6-M composite returns: >4B fills the caller's temp
                       (? (4 < tsz g rt)                   ; through the saved sret pointer (MEMORY, the base ABI);
                          (tor0 g r                        ; <=4B rides its word in r0
                           + [['ld 'r1 'r4 (peep g 'sret 0)]]
                           + bcopy g 0 (tsz g rt) () + epj)
                          (tor0 g r + '((ld r0 r0 0)) + epj))
                      (t32? g && stru? rt &&               ; t32 returns: the d-pair HFA, the 8B sse blob (d0),
                       !(sse2? g rt ||                     ; and the <=4B int one (r0); mixed/int-pair refuse LOUD
                         (id? (aone g rt) 'sse && tsz g rt = 8) ||
                         (id? (aone g rt) 'int && tsz g rt <= 4))) 'bad
                      (sse2? g rt)                                       ; a two-double struct returns as the xmm PAIR
                       (tor0 g r + fldf 'double (xmmreg 0) 'r0 0 +
                        (fldf 'double (xmmreg 1) 'r0 8 + epj))
                      (ipair? g rt)                                      ; a two-int struct returns as rax:rdx (arm64 x0:x1)
                       (tor0 g r + [['ld (ip2 g) 'r0 8] '(ld r0 r0 0)] + epj)
                      (id? (aone g rt) 'sse)                             ; a small SSE aggregate: its eightbyte in xmm0
                       (tor0 g r + fldf 'double (xmmreg 0) 'r0 0 + epj)
                      (id? (aone g rt) 'int)                             ; a small INT aggregate (a union rides here): rax
                       (tor0 g r + '((ld r0 r0 0)) + epj)
                      (amem? g rt)                                       ; a MEMORY return: fill the caller's temp through
                       (tor0 g r                                         ; the saved hidden pointer, and answer it in rax
                        + [['ld 'r1 'r4 (peep g 'sret 0)]]
                        + bcopy g 0 (tsz g rt) () + '((mov r0 r1)) + epj)
                      (stru? rt || stru? (vty r))                        ; a MIXED 16-byte pair, and every composite class
                       (nolane g ("no lane for returning this " + show (tsz g (? (stru? rt) rt (vty r)))     ; arm64 has no MEMORY return yet
                                  + "-byte struct by value" + ontgt g))
                      (flo? (vty r)) (asintt g r rt + (? (id? rt 'bool) (cvt rt 'r0) ()) + (? il [['jmp il]] epj))     ; a double value, an int return -> truncate to r0
                      (pair? g rt) (asll g r + (? il [['jmp il]] epj))        ; a pair return: r0:r1 (widen a word value)
                      (d128? rt)                                                ; a wide return rides rdx:rax (the SysV i128 pair)
                       (? (w128? g) (as128 g r + (? il [['jmp il]] epj)) (no128 g))
                      (id? rt 'bool)                             ; a bool return CONVERTS (callers read r0 whole -- cc calls untyped)
                       (tor0 g r + cvt rt 'r0 + (? il [['jmp il]] epj))
                      (tor0 g r + (? il [['jmp il]] epj)))      ; int -> int, as before (a pair value narrows to its lo)
                   (? (!mt || bad? res) res (mustret g res))))
             rr9)         ; a musttail ret MARKS its call for sibs
          ('expr pe)
           (: rv (? ((<pe = 'post) && (<<>pe = 'var))
                    (vent g env <><>pe) 'bad)
              (? (two? rv && (<rv = 'regv))     ; a statement-position x++ on a home: the old value is dead
                  (: ty <>>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     ([['add <>rv <>rv (* stp <>>pe)]] + cvt ty <>rv))
                 ; statement-position asn of a CONSTANT into a static slot: the value is
                 ; dead and the slot is the destination, so it delivers as ONE store-
                 ; immediate (x64; a value-position asn keeps the protocol -- its answer
                 ; is read).
                 (: sv (? ((<pe = 'asn) && (<<>pe = 'var)) (vent g env <><>pe) 'bad)
                    sk (? (x64t? g && two? sv && member? <sv '(loc glo) &&
                           (: t9 <>>sv
                              !(stru? t9 || arr? t9 || flo? t9 || pair? g t9 ||
                                d128? t9 || id? t9 'bool)) &&
                           (<<>>pe = 'num) && charm? <><>>pe)
                          (siop9 <>>sv <><>>pe) ())
                    (? sk (? (id? <sv 'loc)
                             [[sk 'r4 <>sv <><>>pe]]
                             [['la 'r1 <>sv] [sk 'r1 0 <><>>pe]])
                       (: r (cgexpr g env pe ())
                          (? (bad? r) 'bad (vdrop g r)))))))
          ('nop >< _) ()
          ('tdef >< _) ()                            ; a block-scope _Static_assert (already checked at parse)
          ('blk items) (cgitems g env items ())
          ('if >< cnd >< thn >< er)
           (: le (nlab g ".e")
              c (cbranch g env cnd 'eq le)
              t (cgstmt g env thn)
              (? (bad? c || bad? t) 'bad
                 !(two? <er)
                  (c + t + [['label le]])
                 (: el (cgstmt g env <er)
                    ld (nlab g ".d")
                    (? (bad? el) 'bad
                     (c + t + [['jmp ld] ['label le]] + (el + [['label ld]]))))))
          ('while cnd body)
           (: lc (nlab g ".c") ld (nlab g ".d")
              c (cbranch g env cnd 'eq ld)
              _ (pins g 'brk (ld >< g 'brk)
                        'cnt (lc >< g 'cnt))
              b (cgstmt g env body)
              _ (pins g 'brk >(g 'brk)
                        'cnt >(g 'cnt))
              (? (bad? c || bad? b) 'bad
               ([['label lc]] + c + (b + [['jmp lc] ['label ld]]))))
          ('for init cnd stp body)
           (: lc (nlab g ".c") ls (nlab g ".s") ld (nlab g ".d")
              ir (? !(two? init) [() env]
                    (<init = 'decl) (cgdecl g env <>init)
                    (<init = 'sdecl) (cgsdecl g env <>init)
                    (: r (cgexpr g env <>init ())
                       (? (bad? r) 'bad [(vdrop g r) env])))
              (? (bad? ir) 'bad
                 (: env2 <>ir
                    _ (pin g 'stwrap myw)            ; a decl init re-pinned; the cond is OURS
                    c (two? cnd ? (cbranch g env2 cnd 'eq ld))
                    _ (pins g 'brk (ld >< g 'brk)
                              'cnt (ls >< g 'cnt))
                    b (cgstmt g env2 body)
                    _ (pins g 'brk >(g 'brk)
                              'cnt >(g 'cnt)
                              'stwrap myw)           ; the body re-pinned as it went; the step is OURS
                    st2 (two? stp ? (cgexpr g env2 stp ()))
                    (? (bad? c || bad? b || bad? st2) 'bad
                       (<ir + [['label lc]] + (two? cnd ? c) +
                        b + [['label ls]] + (two? st2 ? (vdrop g st2)) +
                        [['jmp lc] ['label ld]])))))
          ('brk >< _)
           (two? (g 'brk) ? [['jmp <(g 'brk)]] 'bad)
          ('cont >< _)
           (two? (g 'cnt) ? [['jmp <(g 'cnt)]] 'bad)
          ('do body cnd)
           (: lt (nlab g ".t") lc (nlab g ".c") ld (nlab g ".d")
              _ (pins g 'brk (ld >< g 'brk)
                        'cnt (lc >< g 'cnt))
              b (cgstmt g env body)
              _ (pins g 'brk >(g 'brk)
                        'cnt >(g 'cnt)
                        'stwrap myw)                 ; the body re-pinned as it went; the cond is OURS
              c (cbranch g env cnd 'ne lt)
              (? (bad? b || bad? c) 'bad
                 ([['label lt]] +
                  b +
                  [['label lc]] +
                  (c + [['label ld]]))))
          ('goto nm) [['jmp (name (g 'fn + "." + nm))]]
          ('lbl nm) [['label (name (g 'fn + "." + nm))]]
          ('asm >< _) (cgasm g env s)
          ; a case/default label -- anywhere in the body, tied to the nearest enclosing switch by
          ; VALUE: the switch's pre-scan pinned value->label into the tablet atop g 'cases, and
          ; this emits it. no enclosing switch, or an unscanned value, refuses rather than mislabel.
          ('case cv)
           (? !(two? (g 'cases)) 'bad
              (: lab (peep <(g 'cases) cv 'no)
                 (lab @ 'no 'bad [['label lab]])))
          ('dflt >< _)
           (? !(two? (g 'cases)) 'bad
              (: lab (peep <(g 'cases) 'dflt 'no)
                 (? (lab = 'no) 'bad [['label lab]])))
          ('switch ce items)
           (: c (cgexpr g env ce ())
              (? (bad? c) 'bad
                 (: cr9 (vreg c)
                    lend (nlab g ".d")
                    tab {}                ; this switch's value->label map (and 'dflt->label)
                    cases (rev (scasel-l g items tab ()))   ; pre-scan the body (a stmt LIST) at ANY depth -> (value label) dispatch pairs
                    dflt (peep tab 'dflt lend)
                    disp (: (go l acc)
                             (? !(two? l) acc
                                (: e2 <l  v (cfit g (vty c) <e2)  lab <>e2
                                   ins (? (immok g 'cmp v)
                                          [['cmp cr9 v] ['br 'eq lab]]
                                          [['li 'r1 v] ['cmp cr9 'r1] ['br 'eq lab]])
                                   (go >l (acc + ins))))
                            (go cases ()))
                    _ (rfree g cr9)
                    _ (pins g 'brk (lend >< g 'brk)
                              'brkm (() >< peep g 'brkm ())   ; lock-step (a switch break's snap is discarded)
                              'cases (tab >< g 'cases)) ; PUSH -- cgstmt emits each case/dflt label from here
                    body (cgitems g env items ())       ; decls thread env; case/dflt labels emit via g 'cases
                    _ (pins g 'brk >(g 'brk)
                              'brkm >(peep g 'brkm ())
                              'cases >(g 'cases))      ; POP
                    (? (bad? body) 'bad
                       (vfs c + disp + [['jmp dflt]] + (body + [['label lend]]))))))
          'bad))
   (cgitems g env items acc)
    (? !(two? items) acc
       (: it <items
          (it @ ('decl ps)
                 (: r (cgdecl g env ps)
                    (? (bad? r) 'bad
                       (cgitems g <>r >items (acc + <r))))
                ('sdecl ps)
                 (: r (cgsdecl g env ps)
                    (? (bad? r) 'bad
                       (cgitems g <>r >items (acc + <r))))
                (: r (cgstmt g env it)
                   (? (bad? r) 'bad
                      (cgitems g env >items (acc + r)))))))

   ; scan a switch body for its case/default labels at ANY depth, pinning value->label into `tab`
   ; and answering the (value label) dispatch pairs. recurses through stkids but STOPS at a nested
   ; switch, whose cases belong to IT -- a case ties to its NEAREST enclosing switch, per C.
   (scasel g it tab acc)
    (? !(two? it) acc
       (it @ ('case cv)     (: lab (nlab g ".k") _ (pin tab cv lab) [cv lab >< acc])
             ('dflt >< _)   (: lab (nlab g ".k") _ (pin tab 'dflt lab) acc)
             ('switch >< _) acc                                   ; a nested switch owns its labels
             (scasel-l g (stkids it) tab acc)))
   (scasel-l g l tab acc) (? !(two? l) acc (scasel-l g >l tab (scasel g <l tab acc)))

   ; -- the inline lane: a SMALL function (or an always_inline one at any size) splices its whole
   ; body at the call site -- each arg value lands in a fresh 8-byte frame temp (exactly a callee
   ; frame's param slot, so the semantics are the out-of-line call's), the body's statements
   ; compile in place, and every `return` computes into r0 then JUMPS to the splice's end label
   ; (cgstmt's inlret door, so an if-chain inlines too). no call is emitted, so a hot caller can
   ; turn LEAF and home its params. splices NEST to depth 3 (cbinl's cap -- labels are nlab-fresh
   ; per splice, so only recursion needs one); inlret/inlrt restore to the OUTER splice's.
   ; gated out: goto/labels (two splices in one caller would collide), block statics (each splice
   ; would mint a SEPARATE instance where C owes one), va_start. --
   (noinl? x) (? (member? x '(sdecl goto lbl vastart varr)) 1   ; varr: a VLA's slots ride cgfn's per-FN prescan
                 !(two? x) ()
                 (x @ ('call _ as && 6 < tally as) 1 ()) 1     ; an overflow-arg call moves sp -- splicing it bars the caller's unframe
                 (noinl? <x || noinl? >x))
   (oneret? (i >< u))                                  ; decls* + one trailing `return expr` (the b2w shape)
    (i @ ('decl >< _) (oneret? u)
         (('ret . rv . _) && two? rv) !(two? u))
   (inlok? f sig aln)                               ; aln: __attribute__((always_inline)) -- the author's word waives the size gate
    (: rt (? (two? sig) <sig 'long)
       items <><>>>f
       (pok ps n) (? !(two? ps) (n <= 6)
                     (: pty <><ps                               ; a d128 param would bind only its lo word -- a real call takes it
                        (? (flo? pty || stru? pty || arr? pty || d128? pty) ()
                           (pok >ps (n + 1)))))
       (? !(two? sig) ()
          (flo? rt || stru? rt) ()
          (nthr sig 2) ()                            ; variadic stays a real call
          !(pok <>>f 0) ()
          ; a straight-line single-return body pays broadly (600); a branchy one
          ; only while TINY (250, the guard shape) -- big if-chains bloat icache
          (!aln && (oneret? items ? 600 250) < #(show <>>>f)) ()
          !(noinl? <>>>f)))
   (cginl g env f args)                              ; splice f's body at the site -> (ty forms) | 'no (declined)
    (: fe0 (g 'fesc)                          ; a splice that pins a CLEAR fesc would cost the CALLER
       strs0 (g 'strs)                                ; every sibcall (the lvm_peep stack blowup) -- decline it
       il0 (g 'inlining)
       ir0 (g 'inlret)
       irt0 (g 'inlrt)
       _ (pin g 'inlining ((? !il0 0 il0) + 1))
       rt <(peep (g 'sigs) <>f ['long ()])
       ilb9 (nlab g ".il")
       rp0 (g 'rpin)
       ; the ALIAS bind, cbinl's extended to a VALUE splice: a word-typed var arg over a
       ; pinned/homed local reads straight from that register -- licensed only when the whole
       ; splice is call-free AND the body never ASSIGNS that param (statements can); &param
       ; self-declines through clval (regv answers 'bad, the real call stands). rpin holds the
       ; alias against wr?/rfree/psreset across the body's own statement resets. a PARK here
       ; spans the whole body and starves its statements (an ungated one measured corpus L1
       ; loads +0.36%), so it must EARN the seat: weighted body reads >= 8 -- a read inside a
       ; loop, where the slot bind reloads every iteration -- and cbinl's deep-pool gate.
       (anyc l) (? !(two? l) () (? (callish? <l) 1 (anyc >l)))
       body9 <><>>>f
       ; the bind pricing, per loop iteration: weighted body reads earn the park
       (paid? nm) (rpays? 'bind 0 (rdw nm body9 1))
       nocall (!(callish? body9) && !(anyc args))
       (dnames x a9)
        (? !(two? x) a9
           (id? <x 'decl && two? >x)
            (: (dn ps a) (? !(two? ps) a (dn >ps (<<ps >< a)))
               (dn <>x a9))
           (dnames <x (dnames >x a9)))
       dns (dnames body9 ())
       (anyasm x) (? (id? x 'asm) 1 !(two? x) () (anyasm <x || anyasm >x))
       asm9 (anyasm body9)
       ; the CONST bind: a literal arg whose conversion to the param type folds EXACTLY
       ; (cnum's arithmetic is the slot round-trip's) substitutes into the body -- no
       ; slot, no forms, and every immediate lane reads the constant where the slot
       ; bind hid it behind a reload. declined when the body assigns or shadows the
       ; name, or carries asm (an output could write it unseen); &param declines the
       ; whole splice through clval and the real call stands.
       (bind ps as benv acc rbs sbs)                 ; arg values -> a folded constant, the alias register, a priced park, else slots
        (? !(two? ps) [benv acc rbs sbs]
           (: pty <><ps
              kb (? (asm9 || wrs? <<ps body9 || member? <<ps dns) ()
                    (: kc (knum g <as)
                       (? !(two? kc) ()
                          (: cv (cnum g ['cast pty ['num <kc]])
                             (two? cv ? ['cast pty ['num <cv]])))))
              (? (two? kb) (bind >ps >as benv acc rbs ([<<ps kb] >< sbs))
           (: al (? (nocall && !(flo? pty) && 8 = tsz g pty &&
                     two? <as && <<as = 'var && !(wrs? <<ps body9))
                  (: v9 (vent g env <><as)
                   (? (two? v9 && <v9 = 'regv && !(flo? <>>v9)) <>v9)))
              al (? al (? (alsafe? g al) al ()) ())  ; staging must never write it
              (? al                                  ; the alias bind: zero forms
                  (: _ (pin g 'rpin ((rp g al) >< g 'rpin))
                     (bind >ps >as ([<<ps ['reg al] pty] >< benv) acc rbs sbs))
                 (: r (cgexpr g env <as ())
                    (? (bad? r) 'bad
                       (pair? g pty)                 ; a pair param: an 8-byte slot, both words land
                        (: off (ntmp g 'llong)
                           (bind >ps >as
                                 ([<<ps off pty] >< benv)
                                 (acc + asll g r + pst 'r4 off) rbs sbs))
                       (: rb (? (nocall && !(flo? pty) &&
                                 paid? <<ps &&
                                 deep? (g 'pool))              ; a seat is EARNED: paid reads into a deep pool
                                 (ralloc g) ())
                          (? rb                      ; the park bind: the value rides a pool reg
                              (: _ (pin g 'rpin ((rp g rb) >< g 'rpin))
                                 (bind >ps >as ([<<ps ['reg rb] pty] >< benv)
                                       (acc + asintt g r pty
                                            + ([['mov rb 'r0]] + cvt pty rb))
                                       (rb >< rbs) sbs))
                             ; a register-riding value stores ITSELF (no r0 bridge)
                             (: off (ntmp g 'long)
                                vd (? (flo? (vty r) || id? pty 'bool ||
                                       id? (vreg r) 'r0) ()
                                      (vreg r))
                                (bind >ps >as
                                      ([<<ps off pty] >< benv)
                                      (? vd
                                         (: fs (acc + vfs r + [['st 'r4 off vd]])
                                            _ (rfree g vd)
                                            fs)
                                         (acc + asintt g r pty
                                              + (pty @ 'bool (cvt pty 'r0))   ; the entry conversion the splice skips
                                              + [['st 'r4 off 'r0]]))
                                      rbs sbs)))))))))))
       br (bind <>>f args () () () ())
       ; the body-decl license: a decl name whose weighted reads price a pool seat (the
       ; park's discipline) register-binds in cgdecl's splice lane; nesting saves/restores
       rb0 (g 'inlrb)
       rbs0 (g 'inlrbs)
       _ (pins g 'inlrb (? nocall (filter paid? dns))
          'inlrbs () 'inlret ilb9 'inlrt rt)
       ; ⚠ cgitems rides the g tablet, not a direct call: the direct edge closes the
       ; cgexpr->cginl->cgitems->cgstmt->cgexpr cycle, and the loader's analysis then re-weaves
       ; the scope so several `epi` reads compile as book reads before the pin. the indirection
       ; stays until the core analysis owns cycles in big body-less letrecs.
       ib0 (peep g 'inlbody 0)
       _ (pin g 'inlbody (ib0 + 1))                ; the body blinds the alias bind (name conflation)
       csbs (? (bad? br) () (nthr br 3))
       (vsub x) (? !(two? x) x
                   (id? <x 'var) (: p (assoc <>x csbs) (? (two? p) <>p x))
                   ((vsub <x) >< (vsub >x)))
       ibd (? (bad? br) 'bad ((g 'cgitems) g <br (? (two? csbs) (vsub body9) body9) ()))
       _ (pins g 'inlbody ib0 'inlret ir0 'inlrt irt0 'inlining il0 'rpin rp0) ; the bind pins die with the splice
       drbs (g 'inlrbs)
       _ (pins g 'inlrb rb0 'inlrbs rbs0)
       (unrb (a >< u)) (rfree g a, unrb u)
       _ (two? br ? (unrb (nthr br 2)))          ; the park regs return to the pool...
       _ (unrb drbs)                                  ; ...and the body-decl binds with them
       (? (bad? br || bad? ibd)                       ; a shape the splice can't take: the real call still can
           (pin g 'fesc fe0, pin g 'strs strs0, 'no)
          (!fe0 && g 'fesc)                   ; the splice took a frame address (an & arg too -- the
           (pin g 'fesc (), pin g 'strs strs0, 'no)   ; real call's arg lane wraps it, bind cannot): keep the caller lean
          (mkv rt 'r0 (<>br + ibd + [['label ilb9]]))))

   ; -- AST constant propagation: the CONST bind worn by a declared local. a scalar local
   ; declared ONCE from an initializer that folds, never assigned, never &-taken, and
   ; shadowing nothing (param, global, fn) reads as its cast-coated literal everywhere --
   ; no slot, no reload, no pool claim -- and its decl pair drops. an asm body blinds the
   ; pass (an output could write it unseen); the shadow gates hold because a read before
   ; the decl would be the OUTER name's. rounds to a fixpoint: a folded local may be
   ; another's initializer. --
   (kprop g f)
    (: items <><>>>f
       dcl {} tak {} wrt {} st {}
       (cen x c9)                                   ; one walk: the censuses + the decl pairs seen
        (? !(two? x) c9
           (: c8 (x @ ('addr ('var nm)) (pin tak nm 1, c9)
                     (('asn || 'post) . ('var nm) . _) (pin wrt nm 1, c9)
                     ('asm >< _) (pin st 'asm 1, c9)
                     (('decl || 'sdecl) ps)
                      (: (dn l a) (? !(two? l) a
                                     (: _ (pin dcl <<l (1 + peep dcl <<l 0))
                                        (dn >l (? (<x = 'decl && two? >><l) (<l >< a) a))))
                         (dn ps c9))
                     c9)
              c8 (cen <x c8)
              (cen >x c8)))
       cds (cen items ())
       pns (: (go l a) (? !(two? l) a (go >l (<<l >< a))) (go <>>f))
       (kok nm ty init)
        (? (st 'asm) ()
           !(member? ty '(bool char uchar short ushort int uint long ulong)) ()
           !(1 = peep dcl nm 0) ()
           (tak nm || wrt nm) ()
           (member? nm pns) ()
           (two? (peep (g 'glob) nm 0)) ()
           (g 'fns nm) ()
           (: v (: kc (knum g init)
                   (? (two? kc) <kc
                      (: lv (litv init) (? (two? lv) <lv 'no))))
              (? !(charm? v) ()
                 (: cv (cnum g ['cast ty ['num v]])
                    (two? cv ? ['cast ty ['num <cv]])))))
       sbs (: t {}
              (go l) (? !(two? l) t
                        (: p <l
                           kb (kok <p <>p <>>p)
                           _ (two? kb ? (pin t <p kb))
                           (go >l)))
              (go cds))
       (sub x)
        (? !(two? x) x
           (x @ ('var nm) (: k (peep sbs nm ()) (? (two? k) k x))
               ('decl ps)
                ['decl (: (dp l) (? !(two? l) ()
                                    (two? (peep sbs <<l ()) ? (dp >l)
                                       ((sub <l) >< dp >l)))
                          (dp ps))]
               ((sub <x) >< sub >x)))
       (? !(two? (keys sbs)) f
          (kprop g ([<f <>f <>>f] + (['blk (sub items)] >< >>>>f)))))

   (cgfn g f)                                       ; ('fn name params ('blk items)) -> forms | 'bad
    (: f (kprop g f)
       params <>>f
       _ (fnreset g)                                 ; the whole per-fn pin record wiped (one roster)
       ; v6-M has no FPU registers at all: the f0/f1/f15 fiction lives in three 8-byte frame CELLS
       ; allocated up front, and soften rewrites every f-op onto them
       _ (v6m? g ? (pins g 'fc0 (ntmp g 'double)
                           'fc1 (ntmp g 'double)
                           'fc15 (ntmp g 'double)))
       _ (pin g 'fn <>f)
       ; -- C99 VLA prep: scan the body for runtime-dim locals; each site takes a POINTER slot +
       ; a SIZE slot in the static frame, the size slots ZEROED after the param spill (the decl
       ; lane's re-execution guard reads 0 = never allocated). --
       _ (pin g 'vslots {})
       vzf (: (dscan prs a2)
               (? !(two? prs) a2
                  (: ty9 <><prs
                     (dscan >prs (? (gvarr? ty9 && 3 < tally ty9)
                                    (varix ty9 >< a2) a2))))
              (vscan x acc)                          ; walk the body for VLA decl sites
               (? !(two? x) acc
                  (id? <x 'decl && two? >x) (dscan <>x acc)
                  (vlist x acc))
              (vlist l acc) (? !(two? l) acc (vlist >l (vscan <l acc)))
              idxs (vscan (nthr f 3) ())
              (mk l a2)
               (? !(two? l) a2
                  (: po (ntmp g 'long) so (ntmp g 'long)
                     _ (pin (g 'vslots) <l [po so])
                     (mk >l (a2 + [['st 'r4 so 'r0]]))))
              (? !(two? idxs) () ('((li r0 0)) + mk idxs ())))
       sig (g 'sigs <>f)
       vararg (two? sig ? (nthr sig 2))
       ; the epilogue is per-FN: a thumb2 vararg fn drops its r0-r3 push block on
       ; the way out (epi-t32va); everyone else keeps the target default
       _ (pin g 'epi (? (vararg && t32? g) (v6m? g ? epi-t16va epi-t32va)
                        (vararg && rv? g) epi-rv-va
                        (v6m? g) epi-t16 (arm? g) epi-a64 epi))
       fb9 (fbase g)                                 ; the frame base the RANKERS see: post-build ir1 (r4, or fp once a4ized)
       ; spill each param from its SysV register: integers from r6 r5 r2 r1 r7 r8 (<=6), doubles
       ; from f0..f7 (<=8), SEPARATE counters. an OVERFLOW param already sits on the caller stack
       ; at rbp+16+8k, no store. hp is the HOME POLICY over the eligible gp params (x64/a64 regens
       ; only): () spills every one to its slot; ('ride ridelist) homes in a register or rides the
       ; arrival (the leaf lane); 'shadow homes AND takes a frame slot (the non-leaf lane).
       ; under a policy the exotic composites answer 'bad -- the regen deopts to ir1 rather than
       ; growing a memory lane the gate never counted.
       (pol? h9) (two? h9 || id? h9 'shadow)         ; a live policy (only the ride one is a list)
       (spill hp ps gpi xmmi ovi env acc)
        (? !(two? ps) [acc env]
           (: p <ps pty <>p
              (? (v6m? g && sse2? g pty)                ; v6-M: a POSITION-0 16B composite arrives whole in r0-r3
                  (? (gpi = 0)                          ;  (the caller's g4 lane); any later start splits -- deferred
                     (: off (nslot g 16)
                        (spill hp >ps 4 xmmi ovi ([<p off pty] >< env)
                               (acc + [['st 'r4 off 'r0] ['st 'r4 (off + 4) 'r1]
                                       ['st 'r4 (off + 8) 'r2] ['st 'r4 (off + 12) 'r3]])))
                     'bad)
                 (t32? g && stru? pty &&                ; t32 params: the d-pair HFA, the 8B sse blob, the <=4B int
                  !(sse2? g pty ||                      ; one (the gp lane's word store IS its <=4 bytes);
                    (id? (aone g pty) 'sse && tsz g pty = 8) ||       ; mixed/int-pair composites refuse LOUD
                    (id? (aone g pty) 'int && tsz g pty <= 4))) 'bad
                 (v6m? g && stru? pty && id? (aone g pty) 'sse)           ; v6-M 8B blob param: the even-odd gp pair (the caller's g2 lane)
                  (: gpe ((gpi + 1) & -2)
                     (? (gpe + 2 <= ngp g)
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi ([<p off pty] >< env)
                                   (acc + [['st 'r4 off (nthr (argr g) gpe)]
                                            ['st 'r4 (off + 4) (nthr (argr g) (gpe + 1))]])))
                        (: ov8 ((ovi + 7) & -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) ([<p (ovb g + ov8) pty] >< env) acc))))
                 (stru? pty && two? (aeb2 g pty))       ; a 9..16B struct param: two eightbytes into a 16-byte slot, each from a gp or an xmm reg
                  (? (pol? hp && !(sse2? g pty)) 'bad        ; a policy homes gp scalars only -- mixed/int-pair 16B deopts
                     (: cls (aeb2 g pty) c0 <cls c1 <>cls
                        ; the register FILE per eightbyte -- 'g gp / 'x xmm. SysV (x64) picks
                        ; by the eightbyte's class; AAPCS64 (arm64) sends a non-HFA composite
                        ; wholly in X regs, only an all-float HFA in V regs.
                        hfa (c0 = 'sse && c1 = 'sse)
                        f0 (a64? g ? (hfa ? 'x 'g) (c0 @ 'sse 'x 'g))
                        f1 (a64? g ? (hfa ? 'x 'g) (c1 @ 'sse 'x 'g))
                        gpn ((f0 = 'g) + (f1 = 'g))
                        xmn ((f0 = 'x) + (f1 = 'x))
                        (? (ngp g < gpi + gpn || 8 < xmmi + xmn)     ; not enough regs -> the WHOLE aggregate goes to memory (2 stack eightbytes)
                            (? (pol? hp) 'bad           ; under a policy: deopt, not a fresh memory lane
                               (t32? g)                 ; t32 stack HFA params (9+ doubles): deferred
                                (nolane g ("no lane for an HFA param past the register file" + ontgt g))
                               (spill hp >ps gpi xmmi (ovi + 2) ([<p (ovb g + (* 8 ovi)) pty] >< env) acc))
                            (: off (nslot g 16)
                               st0 (f0 @ 'g [['st 'r4 off (nthr (argr g) gpi)]] (fstf g 'double 'r4 off (xmmreg xmmi)))
                               gp1 (f0 @ 'g (gpi + 1) gpi)
                               xm1 (f0 @ 'x (xmmi + 1) xmmi)
                               st1 (f1 @ 'g [['st 'r4 (off + 8) (nthr (argr g) gp1)]] (fstf g 'double 'r4 (off + 8) (xmmreg xm1)))
                               gp2 (f1 @ 'g (gp1 + 1) gp1)
                               xm2 (f1 @ 'x (xm1 + 1) xm1)
                               (spill hp >ps gp2 xm2 ovi ([<p off pty] >< env) (acc + st0 + st1))))))
                 (stru? pty && id? (aone g pty) 'sse)       ; a small SSE aggregate: one xmm into an 8-byte slot
                  (? (xmmi < 8)
                     (: off (nslot g 8)
                        (spill hp >ps gpi (xmmi + 1) ovi ([<p off pty] >< env)
                               (acc + fstf g 'double 'r4 off (xmmreg xmmi))))
                     (? (t32? g)                        ; t32 stack blob params: deferred
                         (nolane g ("no lane for a stack-passed by-value struct param" + ontgt g))
                        (spill hp >ps gpi xmmi (ovi + 1) ([<p (ovb g + (* 8 ovi)) pty] >< env) acc)))
                 ; a MEMORY-class param: the caller laid the whole aggregate in the overflow
                 ; block, and by SysV that memory IS the callee's copy -- so the param binds
                 ; STRAIGHT to it, no slot and no store, as an overflow scalar does
                 (amem? g pty)
                  (? (pol? hp || 8 < asalign g pty) 'bad     ; under a policy, or over-aligned: deopt/refuse
                     (spill hp >ps gpi xmmi (ovi + aslots g pty)
                            ([<p (ovb g + 8 * ovi) pty] >< env) acc))
                 (stru? pty && !(id? (aone g pty) 'int))        ; MIXED pair stays ahead; a small INT one rides the gp lane
                  (nolane g ("no lane for passing this " + show (tsz g pty)
                             + "-byte struct by value" + ontgt g))
                 (v6m? g && id? pty 'float)             ; v6-M base ABI: a bare float param is ONE WORD
                  (? (gpi < ngp g)                      ;  (binary32 bits in a gp reg / a 4-byte caller slot);
                     (: off (nslot g 8) ;  clval's fldf 'float widens it on read
                        (spill hp >ps (gpi + 1) xmmi ovi ([<p off pty] >< env)
                               (acc + [['st 'r4 off (nthr (argr g) gpi)]])))
                     (spill hp >ps gpi xmmi (ovi + 4) ([<p (ovb g + ovi) pty] >< env) acc))
                 (v6m? g && flo? pty)                   ; v6-M base ABI: a double param IS a gp pair
                  (: gpe ((gpi + 1) & -2)               ;  (no FPU regs exist) -- spill both words, or
                     (? (gpe + 2 <= ngp g)            ;  read the 8-aligned caller stack
                         (: off (nslot g 8)
                          (spill hp >ps (gpe + 2) xmmi ovi ([<p off pty] >< env)
                           (acc + [['st 'r4 off (nthr (argr g) gpe)]
                            ['st 'r4 (off + 4) (nthr (argr g) (gpe + 1))]])))
                        (: ov8 ((ovi + 7) & -8)
                         (spill hp >ps 4 xmmi (ov8 + 8) ([<p (ovb g + ov8) pty] >< env) acc))))
                 (flo? pty && xmmi < 8)
                  (: off (nslot g 8)
                     (spill hp >ps gpi (xmmi + 1) ovi ([<p off pty] >< env)
                            (acc + fstf g pty 'r4 off (xmmreg xmmi))))
                 (pair? g pty)                          ; a pair param: an even-odd register pair spills
                  (: gpe ((gpi + 1) & -2)               ;  both words; else the 8-aligned stack (gp closes)
                     (? (gpe + 2 <= ngp g)
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi ([<p off pty] >< env)
                                   (acc + [['st 'r4 off (nthr (argr g) gpe)]
                                            ['st 'r4 (off + 4) (nthr (argr g) (gpe + 1))]])))
                        (: ov8 ((ovi + 7) & -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) ([<p (ovb g + ov8) pty] >< env) acc))))
                 (t32? g && flo? pty)                   ; a t32 double param past d7: the 4-byte slot can't carry it
                  (nolane g ("no lane for a double param past the register file" + ontgt g))
                 (d128? pty)                            ; a wide param: not carried (mirror of the arg refusal)
                  (nolane g "a wide value is not carried as a parameter")
                 (!(flo? pty) && gpi < ngp g)
                  (? (pol? hp && gpi < nhome g &&
                      (? (two? hp && id? <hp 'uni)
                         (member? gpi <>hp || member? gpi (nthr hp 2))
                         (homable? pty ||
                          (two? hp && (member? gpi <>hp || member? gpi (nthr hp 2))))))
                      (? (two? hp && id? <hp 'uni)
                         ; the one-build lane (moon-ssa rung 6): a rider keeps its
                         ; arrival (staging lost that register), a positional home
                         ; pays the one mov -- and EVERY home wears the armed
                         ; shadow: the slot is reserved but untouched until a call
                         ; actually emits, so a spliced call never wraps and an
                         ; untouched cell packs away downstream
                         (: inc (nthr (argr g) gpi)
                            home (? (member? gpi <>hp) inc (homereg gpi))
                            _ (pin g 'homes ([home (nslot g 8) <p] >< g 'homes))
                            mv ((home != inc ? [['mov home inc]]) + cvt pty home)
                            (spill hp >ps (gpi + 1) xmmi ovi ([<p ['reg home] pty] >< env)
                                   (acc + mv)))
                      (? (two? hp)                      ; the leaf lane: home in a register, moving from the
                         (: inc (nthr (argr g) gpi)     ; incoming arg reg if it differs -- but a rideable param
                            ; rides its ARRIVAL register: no home reserved, no entry
                            ; mov, its home register freed to the pool
                            home (member? gpi <>hp ? inc (homereg gpi))
                            mv ((home != inc ? [['mov home inc]]) + cvt pty home)
                            (spill hp >ps (gpi + 1) xmmi ovi ([<p ['reg home] pty] >< env)
                                   (acc + mv)))
                         (: pc9 (assoc gpi (g 'pcs))   ; a granted cs seat: the callee
                            ; preserves it, so no call wrap, no shadow slot -- off g 'homes
                            home (? (two? pc9) <>pc9 (homereg gpi))
                            inc (nthr (argr g) gpi)
                            _ (? (two? pc9) ()
                                 (pin g 'homes ([home (nslot g 8) <p] >< g 'homes)))
                            mv ((? (home = inc) () [['mov home inc]]) + cvt pty home)
                            (spill hp >ps (gpi + 1) xmmi ovi ([<p ['reg home] pty] >< env)
                                   (acc + mv)))))
                     (: off (nslot g 8)
                        ; a bool param CONVERTS its arrival (cc calls untyped, so the
                        ; caller passed the bare int) -- the object invariant holds at entry
                        cv9 (pty @ 'bool (cvt pty (nthr (argr g) gpi)))
                        (spill hp >ps (gpi + 1) xmmi ovi ([<p off pty] >< env)
                         (acc + cv9 + [['st 'r4 off (nthr (argr g) gpi)]]))))
                 ; an overflow param reads off the caller's block: 8-byte slots (x64/a64), 4-byte on t32
                 (t32? g) (spill hp >ps gpi xmmi (ovi + 4) ([<p (ovb g + ovi) pty] >< env) acc)
                 (spill hp >ps gpi xmmi (ovi + 1) ([<p (ovb g + 8 * ovi) pty] >< env) acc))))
       ; a VARIADIC function (SysV): a 176-byte register SAVE AREA holds the 6 gp arg registers
       ; (@ +0, step 8) and 8 xmm registers (@ +48, step 16, low 8 bytes each). named
       ; register-passed params are addressed INSIDE the area, overflow named params on the caller
       ; stack. the xmm registers save unconditionally -- reading an xmm never faults, so no `al`
       ; guard is needed on this side. a named param is classified exactly as `spill` classifies
       ; it -- a by-value AGGREGATE included: MEMORY class binds to the caller's block, one
       ; eightbyte is addressed in the save area, and a PAIR touching xmm copies into a slot of
       ; its own, because the xmm slots step 16 where a pair's eightbytes must sit 8 apart.
       (vaspill ps)
        (: rsa (nslot g 176)
           _ (pin g 'rsa rsa)
           savegp (: (go i acc) (? (5 < i) acc (go (i + 1) (acc + [['st 'r4 (rsa + 8 * i) (nthr argregs i)]]))) (go 0 ()))
           savex  (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + [['stsd 'r4 (rsa + 48 + 16 * i) (xmmreg i)]]))) (go 0 ()))
           (cls qs gpi xmmi ov env acc)
            (? !(two? qs) (: _ (pins g 'vagp gpi 'vaxmm xmmi) [((savegp + savex) + acc) env])
               (: p <qs pty <>p
                  eb2 (aeb2 g pty)
                  ovf (\ u (cls >qs gpi xmmi (ov + 8 * (? (stru? pty) (aslots g pty) 1))
                                ([<p (ovb g + ov) pty] >< env) acc))
                  (? (amem? g pty) (? (8 < asalign g pty) 'bad (ovf ()))
                     (d128? pty) (nolane g "a wide param is not carried in a variadic function")
                     eb2 (: f0 (<eb2 @ 'sse 'x 'g) f1 (<>eb2 @ 'sse 'x 'g)
                             gpn ((f0 = 'g) + (f1 = 'g))
                             xmn ((f0 = 'x) + (f1 = 'x))
                             (? (6 < gpi + gpn || 8 < xmmi + xmn) (ovf ())   ; short of registers: the WHOLE aggregate goes to memory
                                (2 = gpn) (cls >qs (gpi + 2) xmmi ov ([<p (rsa + 8 * gpi) pty] >< env) acc)
                                (: off (nslot g 16)
                                   st0 (f0 @ 'g [['st 'r4 off (nthr argregs gpi)]] (fstf g 'double 'r4 off (xmmreg xmmi)))
                                   gp1 (f0 @ 'g (gpi + 1) gpi)
                                   xm1 (f0 @ 'x (xmmi + 1) xmmi)
                                   st1 (f1 @ 'g [['st 'r4 (off + 8) (nthr argregs gp1)]] (fstf g 'double 'r4 (off + 8) (xmmreg xm1)))
                                 (cls >qs (gpi + gpn) (xmmi + xmn) ov ([<p off pty] >< env) (acc + st0 + st1)))))
                     (flo? pty || id? (aone g pty) 'sse)
                      (7 < xmmi ? (ovf ())
                       (cls >qs gpi (xmmi + 1) ov ([<p (rsa + 48 + 16 * xmmi) pty] >< env) acc))
                     (5 < gpi) (ovf ())
                     (cls >qs (gpi + 1) xmmi ov ([<p (rsa + (* 8 gpi)) pty] >< env) acc))))
           (cls ps 0 0 0 () ()))
       ; a VARIADIC function (AAPCS64): a 192-byte save area -- x0..x7 (@ +0, step 8) then q0..q7
       ; (@ +64, step 16; the low 8 bytes carry the double, the rest don't-care). named register
       ; params are addressed INSIDE the area, named overflow params on the caller stack.
       (vaspill-a64 ps)
        (: rsa (nslot g 192)
           _ (pin g 'rsa rsa)
           savegp (: (go i acc) (7 < i ? acc (go (i + 1) (acc + [['st 'r4 (rsa + 8 * i) (nthr argregs-a64 i)]]))) (go 0 ()))
           savevr (: (go i acc) (7 < i ? acc (go (i + 1) (acc + fstf g 'double 'r4 (rsa + 64 + 16 * i) (xmmreg i)))) (go 0 ()))
           (cls qs gpi vri ov env)
            (? !(two? qs) (: _ (pins g 'vagp gpi 'vaxmm vri 'vaov ov) [(savegp + savevr) env])
               (: p <qs pty <>p
                  (? (flo? pty)
                      (7 < vri ? (cls >qs gpi vri (ov + 1) ([<p (ovb g + 8 * ov) pty] >< env))
                                 (cls >qs gpi (vri + 1) ov ([<p (rsa + 64 + 16 * vri) pty] >< env)))
                     (7 < gpi ? (cls >qs gpi vri (ov + 1) ([<p (ovb g + 8 * ov) pty] >< env))
                                (cls >qs (gpi + 1) vri ov ([<p (rsa + 8 * gpi) pty] >< env))))))
           (cls ps 0 0 0 ()))
       ; a VARIADIC function (AAPCS32, thumb2 + v6m): no save area of its own -- the prologue
       ; pushed r0-r3 just under the caller's stack args, one contiguous word array past the push
       ; cells ((ovb g): thumb2 lr/fp = 8, v6m lr/fp/r4 = 12). a named register param is addressed
       ; INSIDE the block (fp+ovb+4i), a named overflow param past it. WORD named params only;
       ; doubles/pairs/structs refuse LOUD.
       (vaspill-t32 ps)
        (: (cls qs gpi ov env)
            (? !(two? qs) (: _ (pin g 'vagp gpi) _ (pin g 'vaov ov) [() env])
               (: p <qs pty <>p
                  (? (flo? pty || llt? pty || stru? pty)
                      (nolane g ("no lane for a double/pair/struct param in a variadic function" + ontgt g))
                     (3 < gpi) (cls >qs gpi (ov + 4) ([<p (ovb g + 16 + ov) pty] >< env))
                     (cls >qs (gpi + 1) ov ([<p (ovb g + 4 * gpi) pty] >< env)))))
           (cls ps 0 0 ()))
       ; a VARIADIC function (LP64, riscv): anonymous args ride the GP registers ONLY, so the
       ; prologue lays a0..a7 in a packed 64-byte block just above the push cells -- laid BEFORE
       ; push lr, contiguous with the caller's stack args -- and va_list is ONE walking pointer
       ; over 8-byte slots. a named register param is addressed INSIDE the block (fp+ovb+8i), a
       ; named overflow param past it. a named DOUBLE rides the gp lane too (the caller's va?
       ; classifier put its bits in the arg register); composites refuse LOUD.
       ; epi-rv-va drops the block on the way out.
       (vaspill-rv ps)
        (: (cls qs gpi ov env)
            (? !(two? qs) (: _ (pins g 'vagp gpi 'vaov ov) [() env])
               (: p <qs pty <>p
                  (? (stru? pty) (nolane g ("no lane for a by-value struct param in a variadic function" + ontgt g))
                     (7 < gpi) (cls >qs gpi (ov + 1) ([<p ((ovb g + 64) + 8 * ov) pty] >< env))
                     (cls >qs (gpi + 1) ov ([<p (ovb g + 8 * gpi) pty] >< env)))))
           (cls ps 0 0 ()))
       ; -- register-homed params (the leaf lane): a gp param that is an 8-byte scalar whose
       ; address never escapes can live in a CALLER-SAVED, non-scratch register for the whole
       ; function instead of a frame slot. the home pool ((hregs g), indexed by gp position) is
       ; disjoint from the scratch set (r0-r3) and from the overflow/xmm registers, so Hp and Sp
       ; MOVE out of scratch at the prologue. ⚠ applied ONLY when the fn turns out to be a LEAF:
       ; after the sibcall pass no call/callr remains, fesc is unset, there is no raw `sys`
       ; (rcx/r11 are syscall-clobbered), and it is not variadic. decided by GENERATING first,
       ; then regenerating with the register env if the gate holds.
       ; the 'shadow policy is the NON-LEAF lane: each home ALSO takes a frame slot recorded in
       ; g 'homes, which call-fixed spills before every remaining call's pops and reloads after,
       ; so a callee only ever sees its own ABI registers. a &param deopts the regen to ir1. --
       (homereg gpi) (nthr (hregs g) gpi)
       (homes? ps gpi) (0 < nhomable ps gpi 0)      ; does any gp param qualify for a register home?
       (anyop? (f >< u) op) (f @ (^op . _) 1 (anyop? u op))
       (hascall? fs) (anyop? fs 'call || anyop? fs 'callr)
       ; the non-leaf PAYOFF gate: homing saves one frame load per param READ but costs a
       ; spill+reload PER HOME around every remaining call -- worth it only where reads dominate.
       ; counted on the post-sibcall ir1: tail calls already cost nothing.
       (nhomable ps gpi n) (homefold g ps gpi n (\ n2 i9 p9 (? (homable? <>p9) (n2 + 1) n2)))
       (ncalls fs n) (? !(two? fs) n
                        (: f <fs
                           (ncalls >fs (? (two? f && member? <f '(call callr)) (n + 1) n))))
       ; the forward-path call maximum over post-sibcall ir1: labels merge by max, a jmp/ret/jmpr
       ; ends its path, a BACKWARD branch is ignored (an in-loop call is ncw's charge). this is
       ; the price a single invocation can actually pay -- the dispatch fleet's calls sit in
       ; exclusive arms, so their total says nothing about any one path.
       (pmax fs)
        (: best {}
           (mxu m c) (? (c = 'dead) m (m < c) c m)
           (bpin lab c) (c != 'dead ?  (: b (peep best lab 0) (b < c ? (pin best lab c))))
           (go l cur mx)
            (? !(two? l) (mxu mx cur)
               (: f <l
                  (? !(two? f) (go >l cur mx)
                   (<f @ 'label (go >l (: b (peep best <>f 0)
                                 (? (cur = 'dead) b (b < cur) cur b)) mx)
                       ('call || 'callr)
                        (go >l (cur @ 'dead 'dead (cur + 1)) mx)
                       'br (bpin <>>f cur, go >l cur mx)
                       'jmp (bpin <>f cur, go >l 'dead (mxu mx cur))
                       ('ret || 'jmpr) (go >l 'dead (mxu mx cur))
                       (go >l cur mx)))))
           (go fs 0 0))
       ; the forward-path call MINIMUM, pmax's twin: the price a per-invocation cost must
       ; clear -- a zero-call path (the dispatch fleet's early out) pays entry work and
       ; earns nothing, so a grant priced per invocation needs every path to call.
       (pmin fs)
        (: best {} big 1000000
           (mnu m c) (? (c = 'dead) m (c < m) c m)
           (bpin lab c) (? (c != 'dead)
                           (: b (peep best lab big) (c < b ? (pin best lab c))))
           (go l cur mn)
            (? !(two? l) (mnu mn cur)
               (: f <l
                  (? !(two? f) (go >l cur mn)
                     (: k <f
                        (k @ 'label
                            (go >l (: b (peep best <>f big)
                                    (? (cur = 'dead) b (cur < b) cur b)) mn)
                           ('call || 'callr) (go >l (cur = 'dead ? 'dead (cur + 1)) mn)
                           'br (: _ (bpin <>>f cur) (go >l cur mn))
                           'jmp (: _ (bpin <>f cur) (go >l 'dead (mnu mn cur)))
                           ('ret || 'jmpr) (go >l 'dead (mnu mn cur))
                           (go >l cur mn))))))
           (go fs 0 big))
       (pslots env acc)                             ; the homable params' PASS-1 frame slots
        (? !(two? env) acc
           (: e2 <env
              (pslots >env (? (atom? <>e2 && <>e2 < 0 && homable? <>>e2)
                                   (<>e2 >< acc) acc))))
       (nreads fs offs n)                           ; frame touches of those slots in ir1
        (? !(two? fs) n
           (: f <fs
              hit (? !(two? f) ()
                     (member? <f ldops9) (<>>f = fb9 && member? <>>>f offs)
                     (member? <f gstops) (<>f = fb9 && member? <>>f offs))
              (nreads >fs offs (? hit (n + 1) n))))
       (nrac fs offs)                               ; the unerasable reloads: a call dirties every
        (: t {}                             ; slot, a store cleans its own (stld remats the
           (go l n9)                                ; loads behind it), the FIRST dirty load counts
            (? !(two? l) n9
               (: f <l
                  (? !(two? f) (go >l n9)
                     (member? <f '(call callr))
                      (foldl (\ _ o9 (pin t o9 1)) 0 offs, go >l n9)
                     (member? <f gstops && (<>f = fb9) && member? <>>f offs)
                      (pin t <>>f (), go >l n9)
                     (member? <f ldops9 && (<>>f = fb9) && member? <>>>f offs && t <>>>f)
                      (pin t <>>>f (), go >l (n9 + 1))
                     (go >l n9))))
           (go fs 0))
       (build sp0)                                  ; assemble prologue + body + epilogue for a spill result
        (? (bad? sp0) 'bad
           (: _ (bldreset g)
              body (cgitems g <>sp0 <><>>>f ())
              (? (bad? body) 'bad
                 (: frame ((peep g 'off 0 + 15) & -16)
                    (? (arm? g)
                       ; the sibcall pass rides all targets (epim?/sibjmp read the epilogue off
                       ; g 'epi), but the IR sweeps stay OUT of the arm build: the rankers price
                       ; ir1 as built (nreads/ntouch), and a swept ir1 starves them -- marginal
                       ; homes stop paying and slot traffic ships instead. the a64 lane sweeps
                       ; the CHOSEN ir at the post-choice seam, where the frame base is already
                       ; fp (unambiguous: an r4 in the raw forms is also the 5th argument).
                       ; retargeted r4 -> fp AFTER the sibcall rewrite (its patterns speak
                       ; r4; the epilogue forms already say fp/sp/lr). lr rides its own push cell,
                       ; and there is no r3 slot: x3/r3 is caller-saved under AAPCS.
                       ((? (v6m? g) (\ ir (t16la (t16slots ir frame)))   ; v6m: fp-neg slots -> r4-positive (after a4ize), then la -> literal pool
                           (sp64? g) (\ ir (t16la ir))       ; playdate: la -> pooled ABS32 word (its loader can't relocate MOVW/MOVT)
                           (rv? g) (\ ir (rvslots ir frame))  ; riscv: fp-neg slots -> fp-positive, for the C forms
                           (\ ir ir))
                        (a4ize
                         (sibcall g
                          (soften g
                          (vrfix g
                          ((? (v6m? g)   ; v6m: extra push r4 (bottom base), planted AFTER the sub; fp stays at top
                              ([['label (intern <>f)]] +
                               (? vararg   ; vararg v6m: r0-r3 land above lr/fp/r4, contiguous with the caller stack args
                                  '((push r3) (push r2) (push r1) (push r0))
                                  ()) +
                               ['(push lr) '(push fp) '(push r4)
                                  '(lea fp sp 0) ['sub 'sp 'sp frame] '(lea r4 sp 0)])
                              ([['label (intern <>f)]] +
                               (? (vararg && t32? g)     ; vararg t32: r0-r3 land under fp/lr,
                                  '((push r3) (push r2) (push r1) (push r0))               ; contiguous with the caller stack args
                                  (vararg && rv? g)      ; vararg riscv: a0-a7 in a packed 64-byte block
                                  ['(sub sp sp 64)      ; under fp/lr, contiguous with the caller stack args
                                     '(st sp 0 r0) '(st sp 8 r1) '(st sp 16 r2) '(st sp 24 r3)
                                     '(st sp 32 r4) '(st sp 40 r5) '(st sp 48 r6) '(st sp 56 r7)]
                                  ()) +
                               (? (rv? g)     ; riscv links LAST, so fp lands at the frame bottom
                                  ['(push lr) '(push fp) ['sub 'sp 'sp frame] '(lea fp sp 0)]
                                  ['(push lr) '(push fp) '(lea fp sp 0) ['sub 'sp 'sp frame]]))) +
                           ; the cs saves ride AFTER the frame is up (their slots are fp-relative)
                           ; and BEFORE the body -- ⚠ past unframe's positional prologue read
                           peep g 'cssv () +
                           <sp0 + body + '((li r0 0)) + peep g 'epi epi-a64))))))
                       (deadst g
                        (deaddef      ; rerun: the widened addrfold strands live-dest addr insns
                        (cmpfuse
                        (addrfold g
                        (rezx         ; after stld: the forward mints zx, this drops the redundant
                        (copyprop
                        (stld g
                        (spmerge
                        (deaddef
                        (deadcell g
                        (cfoldir g
                         (sibcall g
                          (vrfix g
                          ([['label (name <>f)]
                              '(push r4) '(mov r4 sp) ['sub 'sp 'sp frame]
                              '(st r4 -8 r3)] +
                           g 'cssv + <sp0 +
                           body + '((li r0 0)) + peep g 'epi epi)))))))))))))))))))
       ; the operand pool rides BESIDE the homes: pass 1 owns all of opool, a
       ; regen loses the registers its param homes occupy
       (uhomes ps gpi acc) (homefold g ps gpi acc (\ a2 i9 p9 (? (homable? <>p9) (homereg i9 >< a2) a2)))
       ; -- the RIDE analysis (freeing a pass-through param's home): a param whose home register
       ; differs from its arrival register, and whose ARRIVAL the body never uses as scratch, can
       ; ride the arrival end to end -- the unhome outcome, but decided UP FRONT so its home
       ; register returns to the pool for a hot local. ⚠ soundness: every def of the arrival reg
       ; in ir1 must be a reload from the param's OWN spill slot; any other def, or an opaque bar
       ; op, bars the ride. leaf lane only -- ride is empty under a call. --
       (rst-stoff fs A)                             ; A's entry spill-store (base off) | 'no
        (? !(two? fs) 'no
           (: f <fs
              (? (two? f && member? <f gstops && id? <>>>f A) [<>f <>>f]
                 (rst-stoff >fs A))))
       (rst-defok fs A boff)                        ; every def of A is a reload from boff -> 1 | ()
        (? !(two? fs) 1
           (: f <fs
              (? !(two? f) (rst-defok >fs A boff)
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (member? <f '(label jmp br ret jmpr) ? (rst-defok >fs A boff))
                       !(member? A <>rd) (rst-defok >fs A boff)
                       (member? <f ldops9 &&
                        id? <>f A && id? <>>f <boff && id? <>>>f <>boff)
                        (rst-defok >fs A boff)
                       ())))))
       (rst-ok fs A) (: bo (rst-stoff fs A) (? (id? bo 'no) () (rst-defok fs A bo)))
       ; ⚠ an arrival register that COLLIDES with the build-time frame-base spelling can never be
       ; ridden: gen speaks r4 for the frame base and a4ize retargets it to fp BY POSITION, so an
       ; r4 left in a memory op's BASE slot becomes fp -- and gp 4 arrives in x4 on arm64, where a
       ; pointer param's natural use IS a base slot. the ride is the only lane that puts a value
       ; in r4 there, so barring it closes the collision; the 5th param pays one prologue mov. --
       (a4rider? A) (!(id? (fbase g) 'r4) && id? A 'r4)
       (rideset fs ps gpi acc)                      ; the rideable gp indices
        (homefold g ps gpi acc
         (\ a2 i9 p9 (: A (nthr (argr g) i9)
                        (? (!(a4rider? A) && !(id? A (homereg i9)) && rst-ok fs A)
                           (i9 >< a2) a2))))
       ; ⚠ the ride verdict above is read off ir1, where every param still sits in a FRAME SLOT --
       ; and the regen it licenses rebuilds the body with the riders in registers, so pass 2
       ; reaches for different scratch than the pass it was measured on. on x64 the only two
       ; rideable arrivals ARE the shuttle pair, so a store whose address shuttle moved off a
       ; now-occupied register can land on the very register a param was told it could ride.
       ; so the ride is a PROPOSAL: it is re-read against the code it actually produced, and a
       ; rider whose arrival picked up a FOREIGN def is DROPPED and the regen re-runs without it
       ; (the leaf loop below recomputes the pool per attempt) -- the fn keeps its register homes.
       ; a def that READS its own dest is the param's own update (Sp += n riding A: the register
       ; is the param either side of it) and passes; a foreign value must FIRST land through a
       ; def that reads nothing of A (rdst never answers an arrival, the shuttle/protocol
       ; placements are all plain movs/pops), so that def is the one that drops the rider. --
       (rideok2 fs A)                               ; no FOREIGN def of A survives in fs
        (? !(two? fs) 1
           (: f <fs
              (? !(two? f) (rideok2 >fs A)
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (? (member? <f '(label jmp br ret jmpr)) (rideok2 >fs A) ())
                       (member? A <>rd) (? (member? A <rd) (rideok2 >fs A) ())
                       (rideok2 >fs A))))))
       (ridesound? fs ks)                           ; every rider's arrival survived
        (? !(two? ks) 1
           (rideok2 fs (nthr (argr g) <ks) && ridesound? fs >ks))
       ; -- the LOCALS picker: rank the homable locals ir1 recorded by residual frame touches,
       ; drop any whose slot is lea'd, gate each on touches beating the per-call wrap, and hand
       ; the hottest the FRONT of hpool -- the operand pool keeps the rest (two reserved, so
       ; expressions never go dry). answers [lhome-tablet remaining-pool] | (). --
       (ntouch fs off n)
        (? !(two? fs) n
           (: f2 <fs
              hit (? !(two? f2) ()
                     (member? <f2 ldops9) (<>>f2 = fb9 && <>>>f2 = off)
                     (member? <f2 gstops) (<>f2 = fb9 && <>>f2 = off))
              (ntouch >fs off (? hit (n + 1) n))))
       ; only 'lea marks a slot address-taken (the & lane, a struct/va temp) -- a 'lean is an
       ; lvalue address CONSUMED by its own load/store and never escapes, exactly as the sibcall
       ; gate leas? reads it
       (leaoffs fs t) (? !(two? fs) t
                         (: f2 <fs
                            _ (? (two? f2 && id? <f2 'lea && <>>f2 = fb9)
                                 (pin t <>>>f2 1))
                            (leaoffs >fs t)))
       (lastn l n) (: d (#l - n)
                      (go2 l2 k) (k < 1 ? l2 (go2 >l2 (k - 1)))
                      (d < 1 ? l (go2 l d)))
       (dropn l n) (n < 1 || atom? l ? l (dropn >l (n - 1)))
       (lpick ir1x crs uv hp9)
        (: nc (ncalls ir1x 0)
           ; a call-free fn homes by touches alone (post-sibcall: tail calls cost nothing). with
           ; calls REMAINING, only a ZERO-CROSSING local may home -- alive's cross says it is
           ; never live across any call, so it takes no wraps and free reads are pure win.
           ; ⚠ WRAP-PAYING locals (home iff touches beat 2x loop-weighted crossings) measured 8/8
           ; rounds SLOWER (~2%): static counts don't predict the dynamic spill/reload balance.
           ; ⚠ nor may a local whose crossings are merely all OUTSIDE loops: a kill-born local's
           ; wraps land beside ALWAYS-executed calls (the irKa shape) -- per-local path
           ; sensitivity would be needed, and the measured extra win was ~0.3% loads.
           (free? nm) (member? nm uv && (peep crs nm 0 = 0))
           lt (leaoffs ir1x {})
           navl (#hp9 - 2)
           (walk rs acc)
            (? !(two? rs) acc
               (: e2 <rs sq <e2 off <>e2
                  (? (lt off) (walk >rs acc)
                     (d128? <>>e2) (walk >rs acc)     ; a pair homes off the CS pool only (never the operand pool)
                     ; per invocation: ir1 touches against a pool home, which no call survives --
                     ; a zero-crossing local pays at no call at all, so it prices as a call-free one
                     (: tc (ntouch ir1x off 0)
                        (? (rpays? 'pool (? (free? <>>>e2) 0 nc) tc)
                           (walk >rs ([tc sq] >< acc))
                           (walk >rs acc))))))
           cs (navl < 1 ? () (lastn (imsort (walk (g 'lrec) ())) navl))
           ; the CALLEE-SAVED overflow: a home there needs no wrap -- the callee preserves
           ; it -- so a CROSSING local qualifies too, and a call-free fn whose param homes
           ; drank the operand pool overflows here the same way (its cs registers otherwise
           ; sit idle while the loop locals reload per iteration). the price is the
           ; save/restore pair on EVERY invocation, and static touch counts keep lying about
           ; the payback (the pmax lesson wearing its locals face: a tc-only gate measured
           ; insns +0.2%; adding any-loop touches turned loads +0.2% -- short list/scheduler
           ; loops don't amortize the pair). only a touch under NESTED loops (depth >= 2)
           ; licenses a home: those iteration counts compound, and the pair drowns
           kp (cspool g)
           taken (foldl (\ t e9 (pin t <>e9 1)) {} cs)
           ; loop spans over ir1x, ONE per header label (the longest backward reach -- a
           ; continue's second branch must not fake depth); a touch's depth = covering spans
           spans (: labi {}
                    (gi (f >< u) i) (two? f && <f = 'label ? (pin labi <>f i),
                                     gi u (i + 1))
                    _ (gi ir1x 0)
                    reach {}
                    (go l i)
                     (? !(two? l) ()
                        (: f <l
                           tgt (? !(two? f) ()
                                  (<f = 'jmp) <>f
                                  (<f = 'br) <>>f)
                           ti (? tgt (peep labi tgt -1) -1)
                           _ (? (0 <= ti < i)
                              (: b (peep reach ti -1) (b < i ? (pin reach ti i))))
                           (go >l (i + 1))))
                    _ (go ir1x 0)
                    (mks ts acc) (? !(two? ts) acc
                                    (mks >ts ([<ts (peep reach <ts 0)] >< acc)))
                    (mks (keys reach) ()))
           (nspan i) (: (go2 ss n) (? !(two? ss) n
                                      (: s2 <ss
                                       (go2 >ss (<s2 <= i && i <= <>s2 ? (n + 1) n))))
                        (go2 spans 0))
           (lntouch fs off i n)
            (? !(two? fs) n
               (: f2 <fs
                  hit (? !(two? f2) ()
                         (member? <f2 ldops9) (<>>f2 = fb9 && <>>>f2 = off)
                         (member? <f2 gstops) (<>f2 = fb9 && <>>f2 = off))
                  (lntouch >fs off (i + 1) (hit && 1 < nspan i ? (n + 1) n))))
           ; the per-invocation price, pcs's accounting worn by the locals: a seat costs one
           ; save plus one reload per EXIT, every call, so its touches must outnumber that.
           ; ⚠ without the term the pick just drains the file -- invisible while x64 offered
           ; four seats, +166/+232 insns in vbin_fill the day arm64 offered ten.
           nx9 (: (go9 l n9) (? !(two? l) n9
                              (go9 >l (two? <l && id? <<l 'ret ? (n9 + 1) n9)))
                  (go9 ir1x 0))
           (walk2 rs acc)                             ; cs candidates carry a WIDTH: a d128 pair takes two seats
            (? !(two? rs) acc
               (: e2 <rs sq <e2 off <>e2
                  (lt off || taken sq ? (walk2 >rs acc)
                     ; per invocation: ir1 touches against a seat. ⚠ the floor of 2 is MEASURED,
                     ; not physics -- a seat never pays under three touches whatever nx9 says
                     (: tc (ntouch ir1x off 0)
                        (2 < tc && rpays? 'bor nx9 tc ?
                           (walk2 >rs ([tc sq (d128? <>>e2 ? 2 1)] >< acc))
                           (walk2 >rs acc))))))
           ; choose HOTTEST-first while seats remain (a pair spends two; one that can't fit
           ; skips so a scalar may still take the last seat) -- the choose-order link restores
           ; ascending heat, so seat ASSIGNMENT stays the old positional walk (register
           ; spellings unchanged for scalar-only fns)
           (pick l seats acc)
            (? (atom? l || seats < 1) acc
               (: e3 <l w (nthr e3 2)
                  (? (seats < w) (pick >l seats acc)
                   (pick >l (seats - w) (e3 >< acc)))))
           (mkk l regs t acc9)
            (? (!(two? l) || !(two? regs)) [t acc9]
               (: e3 <l w (nthr e3 2)
                  (? (w = 2 && two? >regs)
                      (pin t <>e3 [<regs <>regs],
                       mkk >l >>regs t (<>regs >< (<regs >< acc9)))
                     (w = 2) [t acc9]
                     (pin t <>e3 <regs,
                      mkk >l >regs t (<regs >< acc9)))))
           kpk (? !(two? kp) [{} ()]
                  (: k9 (mkk (pick (rev (imsort (walk2 (g 'lrec) ()))) #kp ()) kp {} ())
                   [<k9 (rev <>k9)]))  ; cu0 in assignment order (the save offsets ride it)
           (mk l hp t) (atom? l || atom? hp ? t
                        (pin t <><l <hp, mk >l >hp t))
           (two? cs || two? <>kpk ?
            [(mk cs hp9 <kpk)
             (dropn hp9 (tally cs))
             <>kpk]))
       ; -- the allocator (moon-ssa rung 4): locals homes placed over INTERVALS.
       ; a candidate carries alive's tick span (spa); one scan places pool + cs,
       ; and two names whose spans are DISJOINT share a seat -- capacity is
       ; intervals, not names, and one pair serves every name on a cs seat. the
       ; eligibility and the priced gates are lpick's own (a caller-saved seat
       ; needs zero crossings, a callee-saved one pays 'bor), and so is the RAW
       ; rank: a home's value is what repack cannot recover downstream, and raw
       ; touches track the long-lived names only this channel can seat -- the
       ; loop-weighted rank measured WORSE (it drains seats to deep short-lived
       ; locals repack promotes anyway), so weight only breaks ties. answers as
       ; lpick answers; MOON_ABLATE=alloc (and pdef, pdefcs) restores the pick.
       (lalloc ir1x crs uv spa9 hp9)
        (: nc (ncalls ir1x 0)
           (free? nm) (member? nm uv && (peep crs nm 0 = 0))
           lt (leaoffs ir1x {})
           navl (#hp9 - 2)
           (firstn l n) (? (atom? l || n < 1) () (<l >< firstn >l (n - 1)))
           pfile (firstn hp9 navl)
           kp (cspool g)
           ; loop spans over ir1x, one per header label (lpick's shape; vwt reads them)
           spans (: labi {}
                    (gi (f >< u) i) (two? f && <f = 'label ? (pin labi <>f i),
                                     gi u (i + 1))
                    _ (gi ir1x 0)
                    reach {}
                    (go l i)
                     (? !(two? l) ()
                        (: f <l
                           tgt (? !(two? f) ()
                                  (<f = 'jmp) <>f
                                  (<f = 'br) <>>f)
                           ti (? tgt (peep labi tgt -1) -1)
                           _ (? (0 <= ti < i)
                              (: b (peep reach ti -1) (b < i ? (pin reach ti i))))
                           (go >l (i + 1))))
                    _ (go ir1x 0)
                    (mks ts acc) (? !(two? ts) acc
                                    (mks >ts ([<ts (peep reach <ts 0)] >< acc)))
                    (mks (keys reach) ()))
           (wnt fs off i n)                    ; the slot's loop-weighted touch count
            (? !(two? fs) n
               (: f2 <fs
                  hit (? !(two? f2) ()
                         (member? <f2 ldops9) (<>>f2 = fb9 && <>>>f2 = off)
                         (member? <f2 gstops) (<>f2 = fb9 && <>>f2 = off))
                  (wnt >fs off (i + 1) (? hit (n + vwt spans i) n))))
           nx9 (: (go9 l n9) (? !(two? l) n9
                              (go9 >l (two? <l && id? <<l 'ret ? (n9 + 1) n9)))
                  (go9 ir1x 0))
           ; candidates off the locals record: (tc wtc sq span width pool? cs?) -- a ()
           ; span never shares (a shadowed name has no per-name liveness to trust).
           ; the rank is lpick's own RAW count first -- long-lived names accumulate
           ; touches, and they are the ones only this channel can seat (a short
           ; call-free range promotes in repack anyway) -- then scalars before a
           ; pair (two seats), the loop weight, and the seq makes it total
           cands (: (walk rs acc)
                     (? !(two? rs) acc
                        (: e2 <rs sq <e2 off <>e2 ty <>>e2 nm <>>>e2
                           (? (lt off) (walk >rs acc)
                              (: tc (ntouch ir1x off 0)
                                 d1 (d128? ty)
                                 pok (? d1 () (rpays? 'pool (? (free? nm) 0 nc) tc))
                                 cok (2 < tc && rpays? 'bor nx9 tc)
                                 (? !(pok || cok) (walk >rs acc)
                                    (walk >rs ([tc (wnt ir1x off 0 0) sq
                                                (? (member? nm uv) (peep spa9 nm ()) ())
                                                (? d1 2 1) pok cok] >< acc)))))))
                    (walk (g 'lrec) ()))
           byw (sortby (\ a b (? (<b < <a) 1 (<a < <b) ()
                                  ((nthr a 4) < (nthr b 4)) 1 ((nthr b 4) < (nthr a 4)) ()
                                  (<>b < <>a) 1 (<>a < <>b) ()
                                  (<>>a < <>>b)))
                       cands)
           occ {}                             ; seat -> its occupants' spans, 'x exclusive
           (fits? r sp)
            (: o (peep occ r ())
               (? (id? o 'x) ()
                  !(two? o) 1
                  !(two? sp) ()
                  (: (dj l) (? !(two? l) 1
                               (: s <l ((<>s < <sp) || (<>sp < <s) ? (dj >l) ())))
                     (dj o))))
           (claim r sp) (pin occ r (? !(two? sp) 'x
                                      (: o (peep occ r ()) (? (two? o) (sp >< o) [sp]))))
           (seat1 file sp) (? !(two? file) ()
                              (fits? <file sp) <file
                              (seat1 >file sp))
           (seat2 file sp)                    ; a d128 pair: two seats, both fitting
            (: r1 (seat1 file sp)
               (? !r1 ()
                  (: r2 (seat1 (filter (\ q !(id? q r1)) file) sp)
                     (? r2 [r1 r2] ()))))
           lh {} pw {} cw {}
           _ (foldl
              (\ _ c
                (: sq (nthr c 2) sp (nthr c 3) w2 (nthr c 4) pok (nthr c 5) cok (nthr c 6)
                   r (? pok (seat1 pfile sp) ())
                   (? r (: _ (claim r sp) _ (pin pw r 1) (pin lh sq r))
                      !cok 0
                      (w2 = 2)
                       (: p2 (seat2 kp sp)
                          (? !(two? p2) 0
                             (: _ (claim <p2 sp) _ (claim <>p2 sp)
                                _ (pin cw <p2 1) _ (pin cw <>p2 1)
                                (pin lh sq p2))))
                      (: r2 (seat1 kp sp)
                         (? !r2 0
                            (: _ (claim r2 sp) _ (pin cw r2 1) (pin lh sq r2)))))))
              0 byw)
           pused (filter (\ q (peep pw q 0)) hp9)
           cu0 (filter (\ q (peep cw q 0)) kp)
           (? (two? pused || two? cu0)
              [lh (filter (\ q !(member? q pused)) hp9) cu0]))
       ; -- the AST-side allocator (moon-ssa rung 6): lalloc's scan, fed by alive
       ; instead of ir1 -- raw mention counts rank (the rung-4 law), the loop
       ; weight breaks ties, spans share seats. candidates are the predicted decl
       ; sequence's UNIVERSE names only (a shadowed or &-taken name stays a cell;
       ; the short-lived shadowed class was repack's anyway), and a name crossing
       ; a HARD call -- one no splice can erase -- never takes a caller-saved
       ; seat; a soft crossing rides the armed shadow (the wrap fires only at a
       ; call that really emits, so a spliced call costs nothing).
       (ulloc av9 hp9 bl9)
        (: uv <>>av9 spa9 (nthr av9 5) crs9 <>av9
           crh9 (nthr av9 6) rdc9 (nthr av9 7) rdt9 (nthr av9 8) sqm9 (nthr av9 9)
           tak9 (nthr av9 14)
           lf9 (0 = nthr av9 15)                     ; leaf-shaped: every call is soft
           ; exits: the AST ret count, floored -- sibcalls eat rets in emission so
           ; this reads STRICT, and strict is the safe side (the chains catch what
           ; a refused fn-wide seat leaves, at range grain)
           nx9 (? (0 < nthr av9 10) (nthr av9 10) 1)
           nsq (nthr av9 13)
           navl (? (3 < #hp9) (#hp9 - 2) (1 < #hp9) 1 0)   ; the file already lost the param homes: keep one grantable seat while two remain
           (firstn l n) (? (atom? l || n < 1) () (<l >< firstn >l (n - 1)))
           pfile (firstn hp9 navl)
           kp (cspool g)
           ; touch counts key by NAME: a shadowed name's sites conflate, and a
           ; conflated total would buy every site its own seat -- average it out
           sitec (: t {}
                    (go2 i) (? (nsq < i) t
                               (: e2 (peep sqm9 i ())
                                  _ (? (two? e2) (pin t <e2 (1 + peep t <e2 0)) 0)
                                  (go2 (i + 1))))
                    (go2 1))
           cands (: (go i acc)
                    (? (nsq < i) acc
                       (: e2 (peep sqm9 i ())
                          (? !(two? e2) (go (i + 1) acc)
                             (: nm <e2 ty <>e2
                                u9 (member? nm uv)
                                (? (tak9 nm) (go (i + 1) acc)   ; an &-taken name stays a cell
                                   (: ns9 (peep sitec nm 1)
                                      tc (? (1 < ns9) ((peep rdc9 nm 0) // ns9)
                                            (peep rdc9 nm 0))
                                      d1 (d128? ty)
                                      ; a shadowed name conflates its sites: no span to
                                      ; trust (exclusive seat), no pool (crh conflated
                                      ; reads over-strict there anyway), cs still sound
                                      ; a leaf-shaped fn pool-homes by rank alone (the old
                                      ; leaf lane's rule; a surprise emission wraps via the
                                      ; armed shadow); a hard-calling fn keeps the STRICT
                                      ; zero-crossing rule -- fn-wide pool homes displace
                                      ; staging, and the old gate was measured
                                      pok (? (d1 || !u9) ()
                                             lf9 (2 <= tc)
                                             ((peep crs9 nm 0 = 0) && 2 <= tc))
                                      cok (2 < tc && (1 + nx9) < tc)
                                      (? !(pok || cok) (go (i + 1) acc)
                                         (go (i + 1) ([tc (? (1 < ns9) ((peep rdt9 nm 0) // ns9)
                                                             (peep rdt9 nm 0)) i
                                                       (? u9 (peep spa9 nm ()) ())
                                                       (? d1 2 1) pok cok] >< acc)))))))))
                    ; the BLOCKERS: a hard-crossing param whose weight clears the
                    ; chain bar competes for the file by rank -- a seat it wins is
                    ; left UNWRITTEN, so repack's chain lane takes it downstream at
                    ; range grain (a fn-wide grant to a colder local would starve
                    ; the chain that serves the param everywhere: inf_run's lesson)
                    (: (bgo l k acc)
                        (? !(two? l) acc
                           (bgo >l (k - 1) ([<<l <><l k () 1 () 'b] >< acc)))
                       (bgo bl9 -1 (go 1 ()))))
           byw (sortby (\ a b (? (<b < <a) 1 (<a < <b) ()
                                  ((nthr a 4) < (nthr b 4)) 1 ((nthr b 4) < (nthr a 4)) ()
                                  (<>b < <>a) 1 (<>a < <>b) ()
                                  (<>>a < <>>b)))
                       cands)
           occ {}                             ; seat -> its occupants' spans, 'x exclusive
           (fits? r sp)
            (: o (peep occ r ())
               (? (id? o 'x) ()
                  !(two? o) 1
                  !(two? sp) ()
                  (: (dj l) (? !(two? l) 1
                               (: s9 <l ((<>s9 < <sp) || (<>sp < <s9) ? (dj >l) ())))
                     (dj o))))
           (claim r sp) (pin occ r (? !(two? sp) 'x
                                      (: o (peep occ r ()) (? (two? o) (sp >< o) [sp]))))
           (seat1 file sp) (? !(two? file) ()
                              (fits? <file sp) <file
                              (seat1 >file sp))
           (seat2 file sp)
            (: r1 (seat1 file sp)
               (? !r1 ()
                  (: r2 (seat1 (filter (\ q !(id? q r1)) file) sp)
                     (? r2 [r1 r2] ()))))
           lh {} pw {} cw {}
           _ (foldl
              (\ _ c
                (: sq (nthr c 2) sp (nthr c 3) w2 (nthr c 4) pok (nthr c 5) cok (nthr c 6)
                   r (? pok (seat1 pfile sp) ())
                   (? r (: _ (claim r sp) _ (pin pw r 1) (pin lh sq r))
                      !cok 0
                      (w2 = 2)
                       (: p2 (seat2 kp sp)
                          (? !(two? p2) 0
                             (: _ (claim <p2 sp) _ (claim <>p2 sp)
                                _ (pin cw <p2 1) _ (pin cw <>p2 1)
                                (pin lh sq p2))))
                      (id? cok 'b)
                       (: r2 (seat1 kp sp)
                          (? !r2 0 (claim r2 sp)))   ; a blocker claims; nothing is written
                      (: r2 (seat1 kp sp)
                         (? !r2 0
                            (: _ (claim r2 sp) _ (pin cw r2 1) (pin lh sq r2)))))))
              0 byw)
           pused (filter (\ q (peep pw q 0)) hp9)
           cu0 (filter (\ q (peep cw q 0)) kp)
           [lh (filter (\ q !(member? q pused)) hp9) cu0])
       ; the param verdicts: a zero-hard-crossing homable param homes -- riding
       ; its arrival where that is sound by construction (the arrival leaves the
       ; pool so staging can never clobber it; a protocol-quad arrival pays the
       ; one mov to its positional seat instead) -- and every home wears the
       ; armed shadow. a hard-crossing param stays a cell (rung 5's economics).
       (upar av9 qk)
        (: uv <>>av9 crh9 (nthr av9 6) rdc9 (nthr av9 7)
           lf9 (0 = nthr av9 15)                     ; leaf-shaped: no hard call anywhere
           pq9 (qk && nthr av9 16)                    ; protocol-quiet: nothing in the body writes a quad
           wd9 (nthr av9 17)                          ; d128 material: the wide kit owns the file
           (homefold g params 0 [() () () #tpool]
            (\ a4 gpi p9
              (: nm <p9 pty <>p9
                 A (nthr (argr g) gpi)
                 nv (nthr a4 3)                       ; the file's running depth (homes and rides drain it)
                 ok (lf9 && member? nm uv && (peep crh9 nm 0 = 0))
                 (? (ok && homable? pty)
                    (? (id? A (homereg gpi)) [<a4 (gpi >< <>a4) (nthr a4 2) (nv - 1)]
                       (a4rider? A) [<a4 (gpi >< <>a4) (nthr a4 2) nv]
                       (member? A '(r0 r1 r2 r3))
                        ; a protocol-quiet body cannot clobber a quad, so it rides
                        ; free (ubuild re-checks the built forms and bares on a
                        ; surprise def); a dirty body vacates to the positional seat
                        (? pq9 [(gpi >< <a4) <>a4 (nthr a4 2) nv]
                           [<a4 (gpi >< <>a4) (nthr a4 2) (nv - 1)])
                       [(gpi >< <a4) <>a4
                        (? (member? A tpool) (A >< nthr a4 2) (nthr a4 2))
                        (? (member? A tpool) (nv - 1) nv)])
                    ; an int/uint param RIDES its arrival (one entry cvt, no pool
                    ; register minted) -- read at least once (a read-free param
                    ; earns nothing), never beside d128 material (the wide-pair
                    ; park owns the file: the mag_mul discipline), and only while
                    ; the file stays deep, two left after the take
                    (ok && lhomable? pty && (0 < peep rdc9 nm 0) && !wd9 &&
                     member? A tpool && 3 <= nv)
                     [(gpi >< <a4) <>a4 (A >< nthr a4 2) (nv - 1)]
                    (ok && lhomable? pty && (0 < peep rdc9 nm 0) &&
                     pq9 && member? A '(r0 r1 r2 r3))
                     [(gpi >< <a4) <>a4 (nthr a4 2) nv]
                    a4)))))
       ; -- the ONE build (moon-ssa rung 6): alive feeds the seats and the build
       ; runs once; the dance survives as the rare BARE fallback (a 'bad build,
       ; or a drifted tick or decl count -- either would misalign livtab or the
       ; home hook, so the rebuild takes no seats at all) and behind the knobs.
       ; the quad-ride verification: a ridden r0-r3 arrival's only legitimate
       ; def is its own entry cvt -- any other def (a staged park, a protocol
       ; op) falsifies the prediction; control forms carry no defs, and a form
       ; rdsp cannot read counts as a hit (conservative)
       (qclob? fs qrr)
        (? !(two? qrr) ()
           (: (go l) (? !(two? l) ()
                        (: f9 <l
                           (? (!(two? f9) ||
                               member? <f9 '(sx4 zx4 label align br jmp jmpr ret)) (go >l)
                              (: rd (rdsp f9)
                                 (? (id? rd 'bar) 1
                                    (: (hit l2) (? !(two? l2) ()
                                                   (member? <l2 qrr) 1
                                                   (hit >l2))
                                       (? (hit <>rd) 1 (go >l))))))))
              (go fs)))
       (ubuild av9)
        (: ok9 (two? av9 && !(nthr av9 11) && !(nthr av9 12))   ; no asm, no vla
           ; one attempt per quad policy: ride first, and if the built forms
           ; show staging took a ridden register, demote the quads to their
           ; positional seats and build once more (the dance's soundness
           ; re-read at fn grain -- attempt () has no quad rides to fail)
           (attempt qk)
            (: pv (? ok9 (upar av9 qk) '(() () ()))
               rides <pv phs <>pv excl (nthr pv 2)
               uh9 (map (\ k (homereg k)) phs)
               pool2 (filter (\ q !(member? q uh9 || member? q excl)) tpool)
               bl9 (? !ok9 ()
                      (: crh9 (nthr av9 6) rdc9 (nthr av9 7) rdt9 (nthr av9 8)
                         (homefold g params 0 ()
                          (\ a2 gpi p9
                            (: nm <p9
                               wt (peep rdt9 nm 0)
                               (? (homable? <>p9 && (0 < peep crh9 nm 0) && (64 <= wt))
                                  ([(peep rdc9 nm 0) wt] >< a2) a2))))))
               lr (? ok9 (ulloc av9 pool2 bl9) ())
               lh9 (? (two? lr) <lr {})
               lp9 (? (two? lr) <>lr pool2)
               cu0 (? (two? lr) (nthr lr 2) ())
               e0 (peep g 'epi epi)
               ; ⚠ no reset before the FIRST attempt: nothing has run since cgfn's
               ; entry -- and the VLA prep's pointer/size slots live in this frame
               ; state (the old world never regened a VLA fn, so they always
               ; survived; a reset here re-deals their offsets under them). a
               ; RETRY resets like the bare fallback (ok9 already barred VLAs
               ; from every path that can retry)
               _ (pins g 'pool0 lp9 'pool lp9)
               _ (rgset g lh9 av9)
               _ (? (two? cu0)
                    (: sv (map (\ r9 ['st 'r4 (nslot g 8) r9]) cu0)
                       _ (pin g 'cssv sv)
                       (pin g 'epi (map (\ f9 ['ld (nthr f9 3) 'r4 (nthr f9 2)]) sv + e0)))
                    ())
               ; empty grants take the bare () policy: no policy bar can fire, so a
               ; VLA or wide-param fn without grants builds ONCE like it always did
               spu (spill (? (two? rides || two? phs) ['uni rides phs] ()) params 0 0 0 () ())
               spu (? (two? vzf) [(<spu + vzf) <>spu] spu)
               irU (build spu)
               _ (pins (rgreset g) 'cssv () 'epi e0)
               qrr (: (go l) (? !(two? l) ()
                                (: A (nthr (argr g) <l)
                                   (? (member? A '(r0 r1 r2 r3)) (A >< go >l) (go >l))))
                      (go rides))
               (? (qk && two? qrr && qclob? irU qrr)
                  (: _ (pins (fnreset g) 'n n0 'strs strs0 'pool0 tpool 'pool tpool)
                     (attempt ()))
                  (bad? irU ||
                   (two? av9 && !(peep g 'lsq 0 = nthr av9 3)) ||
                   !(peep g 'dseq 0 = (? (two? av9) (nthr av9 13) (peep g 'dseq 0))))
                  (: _ (pins (fnreset g) 'n n0 'strs strs0 'pool0 tpool 'pool tpool)
                     sb (spill () params 0 0 0 () ())
                     sb (? (two? vzf) [(<sb + vzf) <>sb] sb)
                     (build sb))
                  irU))
           (attempt 1))
       n0 (peep g 'n 0) strs0 (g 'strs)             ; snapshot the accumulators for a possible regen
       ; ⚠ the operand pool is EMPTY on t32: AAPCS32 keeps r4-r11 callee-saved and the thumb2
       ; frame saves only lr+fp, while r0-r3/r12 are args and scratch -- there is no free
       ; caller-saved register to pool. riscv's own pool is t0..t3 (r8..r11), caller-saved and
       ; never args; opool's names land on riscv arg registers, so it can't ride there.
       tpool (? (abl? g 'tpool) () (t32? g) () (rv? g) '(r8 r9 r10 r11) opool)
       _ (pin g 'pool0 tpool) _ (pin g 'pool tpool)
       ; v6-M MEMORY return: this fn's >4B composite result fills the address the caller
       ; passed as the implicit first arg (r0) -- save it in its own slot, params start at
       ; gp 1. the ret lane reads g 'sret back; fnreset holds the neutral.
       srt (? (two? sig) <sig 'long)
       sr6f (sretm? g srt)
       ; -- the one build (moon-ssa rung 6, the x64 DEFAULT): seats from alive,
       ; one build per fn, the dance retired to MOON_ABLATE=obuild (and the
       ; other old-world knobs). a memory sret and varargs keep the old path
       ; either way (it already builds them once), and the other ISAs keep the
       ; dance until rung 7 calibrates them (uniw?'s law).
       uni9 ((uniw? g) && !(sr6f || vararg))
       _ (? sr6f (pin g 'sret (ntmp g 'long)) ())
       sp1 (? uni9 ()
              (vararg && a64? g) (vaspill-a64 params)
              (vararg && t32? g) (vaspill-t32 params)
              (vararg && rv? g) (vaspill-rv params)
              vararg (vaspill params)
              sr6f (spill () params 1 0 0 () [['st 'r4 (peep g 'sret 0) (nthr (argr g) 0)]])
              (spill () params 0 0 0 () ()))
       sp1 (? (uni9 || !(two? vzf)) sp1 [(<sp1 + vzf) <>sp1])   ; the VLA size-slot zeroes ride after the spills
       ir1 (? uni9 () (build sp1))
       n1 (peep g 'n 0) strs1 (g 'strs)             ; ir1's accumulators -- a DEOPT restores these
       sl1 (g 'slots)                       ; ...and ir1's slot map (repack reads the WINNER's, post-choice)
       lrec1 (g 'lrec)                      ; ir1's locals record: a leaf RETRY ranks on it (a regen leaves pass-2 offsets)
       ride (? (sr6f || vararg || peep g 'fesc () || hascall? ir1) () (rideset ir1 params 0 ()))
       (pools rd) (: rh (map (\ k (homereg k)) rd)   ; the freed homes rejoin the pool for locals
                     uh (filter (\ q !(member? q rh)) (uhomes params 0 ()))
                     (filter (\ q !(member? q uh)) tpool))
       hpool (pools ride)
       hm (!(abl? g 'homes) && !vararg && !sr6f && !(peep g 'hasasm ()) &&   ; sr6f: the hidden sret arg shifts every
           !(anyop? ir1 'sys) && homes? params 0)                   ; param register -- the pickers don't model it
       ; the alive pass runs once per homing-eligible fn: lpick reads cross/
       ; univ (a zero-crossing local homes through calls), the regens pin its
       ; livtab so call-fixed wraps only what a call actually needs
       ng (sr6f || vararg || bad? ir1 || g 'hasasm || anyop? ir1 'sys)
       av (? ng () (alive g f))
       ; -- priced int-param homes (the leaf lane): an int/uint param earns its POSITIONAL
       ; seat only when its ir1 slot touches pay the entry cvt (tc >= 2), the locals picked
       ; first (a hot local's claim wins the register), and an x64 pool seat stays deep
       ; (>= 2 free after the take). ⚠ the unpriced version was the measured anti-lever
       ; (+3.4% cycles): cold params drank the pool and reopened mag_mul's d128 stack cell.
       ; a64 homes past r10 sit outside the operand pool -- always free at their own seat. --
       (poff9 nm (e2 >< u)) (id? <e2 nm ? <>e2 (poff9 nm u))
       (ihset rd lp9)
        (homefold g params 0 [() lp9]
         (\ a2 i9 p9
            (: pl <>a2 hr (homereg i9) off (poff9 <p9 <>sp1)
               pooled (member? hr tpool)
               (? (!(homable? <>p9) &&
                   !(member? i9 rd) &&
                   !(two? off) &&
                   !(id? off ()) &&
                   (1 < ntouch ir1 off 0) &&
                   (? pooled (member? hr pl && deep? pl) 1))
                   [(i9 >< <a2) (? pooled (filter (\ q !(id? q hr)) pl) pl)]
                  a2))))
       ; -- the SEAT TABLE (lh ih lp cs): every priced register grant in one answer -- locals
       ; homes (lpick), int-param homes (ihset), the pool left after both, the cs homes
       ; taken. both lanes mint it here; regen consumes it whole. --
       (seats rd hp9 wi)
        (: oldp (abl? g 'alloc || abl? g 'pdefcs || abl? g 'pdef)
           lr (? !(av && !(abl? g 'lhome)) ()
                 oldp (lpick ir1 <>av <>>av hp9)
                 (lalloc ir1 <>av <>>av (nthr av 5) hp9))
           lp (two? lr ? <>lr hp9)
           ih (? wi (ihset rd lp) [() lp])
           [(two? lr ? <lr) <ih <>ih (two? lr (nthr lr 2))])
       st0 (seats ride hpool !(ng || abl? g 'homes || g 'fesc || hascall? ir1))
       lh <st0 ih0 <>st0                            ; the choice below reads these two; regen takes the table whole
       (deopt u) (pins g 'n n1 'strs strs1 'slots sl1, ir1)
               ; ir1 stands, accumulators + slot map restored
       ; the REGEN dance: reset the accumulators and the frame to the ir1 snapshot, pin the
       ; analyses' channels (the locals homes, the livtab), rebuild under
       ; the spill policy hp with the seat table's pool, then clear every channel. a 'bad ir2
       ; deopts: ir1 stands.
       (regen hp st)
        (: lh9 <st lp9 <>>st cs9 (nthr st 3)
           pc0 (g 'pcs)                     ; the param cs grant, captured across fnreset
           e0 (peep g 'epi epi)
           lp2 (lp9 + filter (r9 \ member? r9 tpool && !(member? r9 lp9))   ; the granted
                  (map (homereg * cap) pc0))   ; params' homeregs rejoin the pool
           _ (pins (fnreset g) ; the build starts from the clean record at ir1's accumulator snapshot
              'n n0 'strs strs0
              'pool0 lp2 'pool lp2)
           _ (rgset g lh9 av)
           _ (pin g 'pcs pc0)
           ; the callee-saved homes: one slot each past the r3 cell, stored by the
           ; prologue (g 'cssv) and reloaded on every exit -- the epilogue PIN carries
           ; the loads through each ret and sib splice, and unframe re-addresses them
           ; like any slot op
           csa (cs9 + map caup pc0)
           _ (? (two? csa)
                (: sv (map (\ r9 ['st 'r4 (nslot g 8) r9]) csa)
                   _ (pin g 'cssv sv)
                   (pin g 'epi (map (\ f9 ['ld (nthr f9 3) 'r4 (nthr f9 2)]) sv + e0)))
                ())
           ir2 (build (spill hp params 0 0 0 () ()))
           _ (pins (rgreset g) 'cssv () 'epi e0)
           ; the numbering guard: the build's forward ticks must land exactly
           ; on alive's total, or the wrap sets keyed by statement number
           ; landed on the wrong statements -- deopt to ir1, whole
           (? (bad? ir2 || (two? av && !(peep g 'lsq 0 = nthr av 3)))
              (deopt ()) ir2))
       ; the leaf lane's ride loop: a rider whose arrival picked up a def in the code it licensed
       ; DROPS, and the regen re-runs -- pool, locals picks and homes all recomputed for the
       ; smaller set, so the survivors keep their arrivals and the dropped param homes like any
       ; other. strictly shrinking (a failed check names a rider), so at most (tally ride)
       ; re-runs; the no-shrink arm is regen's own deopt wearing through -- take ir1 and stop.
       (leaf rd)
        (: _ (pin g 'lrec lrec1)                    ; the retry's lpick ranks ir1's layout, never the failed regen's
           st (seats rd (pools rd) 1)               ; the cs regs ride too -- their pair pins in regen
           i2 (regen ['ride rd <>st] st)
           (? (ridesound? i2 rd) i2
              (: rs (filter (k \ rideok2 i2 (nthr (argr g) k)) rd)
                 (#rs < #rd ? (leaf rs) (deopt ())))))
       irf (? uni9 (ubuild av)
           (? (bad? ir1) 'bad
          ((hm || lh || two? ride || two? ih0) && !(g 'fesc) && !(hascall? ir1))
           (leaf ride)
          ; calls remain: the param cells stay CELLS (moon-ssa rung 5) and repack's
          ; per-value chains place them -- caller-saved over call-free ranges, a cs
          ; seat where crossings compound, the frame exactly across calls. the
          ; fn-wide wrapped-home and cs-grant lanes below are the OLD answer, kept
          ; under MOON_ABLATE=pcell (and the pdef/pdefcs/alloc worlds, which predate
          ; the chains): ablating them on the rung-4 tree measured -1.6% cycles at
          ; +2.1% insns -- the wrap pair's latency chain at every call costs more
          ; than the forwarded slot traffic it consolidates, now that the chains
          ; recover the call-free part. ⚠ the old lanes' own laws stand where they
          ; run: fn-wide all or none (per-param verdicts measured BOTH ways and
          ; lost), two pricings for pp, the whole-set seat gate for pcs.
          (: old8 (abl? g 'pcell || abl? g 'alloc || abl? g 'pdefcs || abl? g 'pdef)
             nc (ncalls ir1 0) nh (nhomable params 0 0)
             nr (nreads ir1 (pslots <>sp1 ()) 0)
             em (homefold g params 0 0
                 (\ n2 i9 p9 (? (homable? <>p9 &&
                                 !(id? (nthr (argr g) i9) (homereg i9))) (n2 + 1) n2)))
             ; per invocation, nh homes at the caller-saved rate: the flat count, or the PATH
             ; price (only the calls one path passes, plus in-loop calls at alive's weight)
             pp (old8 && hm && (nh * rcost 'home nc < nr ||
                        (two? av && nh * rcost 'home (pmax ir1 + nthr av 4) + em < nr)))
             ; the cs-home grant: the homes take callee-saved seats and the per-call wraps
             ; retire for one save + a reload per EXIT (the epilogue rides every ret) --
             ; fn-wide, the whole set or none (the law above), seats from what lpick left
             free9 (filter (\ r9 !(member? r9 (nthr st0 3))) (cspool g))
             nx (: (go9 l n9) (? !(two? l) n9
                                 (go9 >l (? (two? <l && id? <<l 'ret) (n9 + 1) n9)))
                   (go9 ir1 0))
             nra (nrac ir1 (pslots <>sp1 ()))
             pcs (? (old8 && hm && !(abl? g 'pcs) && (0 < nh) && (nh <= tally free9) &&
                     (0 < pmin ir1) &&
                     ; per invocation, nh homes at the SEAT rate -- no wraps, one save and a
                     ; reload per exit -- and every path must call, or the entry work earns nothing
                     ((nh * rcost 'bor nx) + em < 2 * nra))
                    (: pg (homefold g params 0 [() free9]
                           (\ a2 i9 p9 (? (homable? <>p9 && two? <>a2)
                                          [([i9 <<>a2] >< <a2) ><>a2]
                                          a2)))
                       <pg))
             _ (pin g 'pcs pcs)
             (? (two? pcs || pp || lh)
                (regen (? (two? pcs || pp) 'shadow ()) st0) ir1))))
       ; ⚠ deadlab and repack join unframe in the POST-CHOICE slot and must never touch the ir1
       ; the rankers read: the dead tail's (li r0 0) is a def of r0 that BARS the ride analysis,
       ; and the rankers price param slots BY BUILD OFFSET (nreads/nrac/pslots), so a packed ir1
       ; misprices every grant. repack reads the WINNER's slot map (deopt restores ir1's) and
       ; runs after deadlab (a dead region's touches would fatten the windows). this chain's
       ; order is TYPED (stage.l): each pass advances a stage die, so only this composition checks.
       ; the a64 IR sweeps join here, on the CHOSEN ir -- after the rankers priced the raw
       ; ir1, before deadlab (cfoldir's fold-throughs strand labels for it). cmpfuse stays
       ; home: no memory-operand compare off x64.
       (swp irx) (? !(a64? g) irx
                    (deadst g
                     (deaddef
                     (addrfold g
                     (rezx
                     (copyprop
                     (stld g
                     (spmerge
                     (deaddef
                     (deadcell g
                     (cfoldir g irx)))))))))))
       (? (bad? irf) irf (cskeep g (unhome g (jr0 (unframe g (coal g (repack g (deadlab (swp irf))))))))))

   ; the data tail: globals in declaration order, then the string table -> (dataforms nifforms
   ; imgforms extras bssforms rodataforms) | 'bad, extras being ((name forms ro?) ..), one per
   ; OTHER named section -- ro? saying the section may go read-only (xro?, below).
   ; a __attribute__((section(NAME))) global lays in a stream of its own. three streams have a
   ; meaning the writer knows: `love_nifs` needs the linker's __start_/__stop_ bracket around a
   ; packed run for main()'s boot drain, `.image` must be LAST so `love bake` can grow it at the
   ; tail, and `.data` takes an unattributed global. every OTHER name carries through to the .o as
   ; a real section -- a part reads its .vectors at a fixed address, and ⚠ DECLARATION ORDER is
   ; the program's order, not ours, which is what xnames keeps.
   (cgdata g)
    (: sects (peep g 'sects {})
       consts (peep g 'consts {})
       memo (peep g 'romemo {})
       (strm nm) (: s (peep sects nm ())                  ; which stream a global belongs to
                    (? s s ".data"))                      ; the name IS the stream
       ; a global with NO initializer images as zeros by construction (imgbytes' first
       ; clause), so the file need not carry them: it rides .bss, the NOBITS lane. one
       ; that names its own section keeps it -- the attribute is the whole point.
       (bss? nm) (!(peep sects nm ()) && !(two? <>>(peep (g 'glob) nm 0)))
       ; a const global rides .rodata only if its image needs NO relocation. `union u
       ; const numap_drive[] = {{lvm_ap}..}` is const in C and still an address the
       ; linker writes, so it stays in .data -- which is the lane gcc spells
       ; .data.rel.ro. this is what keeps the flags-2 promise .rodata makes.
       (ro? nm bytes) (peep consts nm () &&
                       !(foldl (\ a x (a || fixit? x)) () bytes))
       ; ⚠ a candidate is imaged for the 'd walk and again for the 'r one, and the
       ; initializers this reaches are the 300k library texts -- so the image is laid
       ; ONCE and memoized. only candidates memoize: holding every global's bytes live
       ; would cost the whole data stream twice for nothing.
       (image1 nm ty init)
        (? !(consts nm) (cgimage g ty init)
           (: m (memo nm)
              (? (two? m) <>m
                 (: v (cgimage g ty init) _ (pin memo nm [1 v]) v))))
       ; zm splits the .data walk without imaging a global twice: 'd takes the
       ; initialized, 'z the rest, 'r the const ones .rodata claims, () everything
       ; (a named stream is never split).
       (globs gs want zm acc)
        (? !(two? gs) acc
           !(strm <gs = want) (globs >gs want zm acc)        ; another stream's
           (member? zm '(d r) && bss? <gs) (globs >gs want zm acc)
           (zm = 'z && !(bss? <gs)) (globs >gs want zm acc)
           (: ge (peep (g 'glob) <gs 0)
              lbl <ge ty <>ge init <>>ge
              _ (pin g 'undecl ()) _ (pin g 'datacause ())   ; fresh per global: the refusal below names THIS one's cause
              bytes (image1 <gs ty init)                ; a constant image | 'bad (label values -> 4d)
              ; the const split reads the image, so it lands after it: 'r keeps only what
              ; .rodata may hold, 'd only what it may not. a REFUSAL is neither, and the 'd
              ; walk owns reporting it -- 'r steps past in silence rather than saying it twice.
              roq (!(bad? bytes) && ro? <gs bytes)
              ; an aligned(N) global lands ON its boundary: this pad places it within the stream,
              ; and the .o's section header carries the max N so the linker grants the same grain.
              ; the two SPECIAL streams are packed by construction and take no pad.
              an (? (member? want '("love_nifs" ".love.image")) 0 (peep (g 'aligns) <gs 0))
              ; the cause rides a PIN, not a puts: cgdata's callers thread a bare 'bad through a
              ; dozen (bad? x) joins and a gripe is not one, so gfns's tail mints it from the pin
              (? (zm = 'r && !roq) (globs >gs want zm acc)
                 (zm = 'd && roq) (globs >gs want zm acc)
                 (bad? bytes)
                 (: u (g 'undecl)
                    _ (pin g 'datacause
                           (? u ("undeclared '" + u + "' in the initializer of '" + <gs + "'")
                              ("cannot lay the initializer of '" + <gs + "' (" + show init + ")")))
                    'bad)
                 (: als (: (go l a2) (? !(two? l) a2
                                          (go >l (g 'alias <l = <gs ?
                                                  (['label (name <l)] >< a2) a2)))
                           (go (keys (g 'alias)) ()))   ; a DATA alias (musl's __stdin_used) names the same bytes
                    (globs >gs want zm (acc + (? (1 < an) [['align an]] ())
                                           + als + [['label lbl] ['raw bytes]]))))))
       (strs ss acc)
        (? !(two? ss) acc
           (: p <ss
              (bts s i bacc) (i < 0 ? bacc (bts s (i - 1) (s i >< bacc)))
              t <>p
              (strs >ss (acc + [['label <p]
                                ['raw (bts t (#t - 1) () + '(0))]]))))
       ; ..and whether one of them may ride a lane nothing writes: the .rodata rule
       ; (ro?) asked of EVERY global in it, since flags belong to the section and one
       ; writable member decides for all -- a section anything outside the program
       ; writes into is not const, however const its other members read.
       ; the const gate comes first so a plain writable section never images anything.
       (xro? nm)
        (: (go gs) (? !(two? gs) 1
                      !(strm <gs = nm) (go >gs)
                      !(consts <gs) ()
                      (: ge (peep (g 'glob) <gs 0)
                         b (image1 <gs <>ge <>>ge)
                         (? (!(bad? b) && ro? <gs b) (go >gs) ())))
           (go (g 'globs)))
       ; the OTHER section names, first-seen in declaration order
       xnames (: (go gs acc) (? !(two? gs) (rev acc)
                                (: s (strm <gs)
                                   (go >gs (? (member? s '(".data" ".rodata" "love_nifs" ".love.image") ||
                                               member? s acc)
                                              acc (s >< acc)))))
                 (go (rev (g 'globs)) ()))
       (: gb (globs (rev (g 'globs)) ".data" 'd ())     ; exported globals, then block-scope statics
          sb (? (bad? gb) 'bad (globs (rev (g 'statics)) ".data" 'd ()))   ; (local data labels)
          zg (? (bad? gb || bad? sb) 'bad (globs (rev (g 'globs)) ".data" 'z ()))
          zs (? (bad? zg) 'bad (globs (rev (g 'statics)) ".data" 'z ()))
          ; .rodata: the string table, any global that named the section itself, and the
          ; const objects the 'r walk claims off .data. nothing here carries a relocation
          ; -- which is what lets the stream ride a lane nothing writes, at load or at run.
          rg (? (bad? zg || bad? zs) 'bad (globs (rev (g 'globs)) ".rodata" () ()))
          rs (? (bad? rg) 'bad (globs (rev (g 'statics)) ".rodata" () ()))
          cg (? (bad? rg || bad? rs) 'bad (globs (rev (g 'globs)) ".data" 'r ()))
          cs (? (bad? cg) 'bad (globs (rev (g 'statics)) ".data" 'r ()))
          nb (? (bad? cg || bad? cs) 'bad (globs (rev (g 'globs)) "love_nifs" () ()))
          ib (? (bad? nb) 'bad (globs (rev (g 'globs)) ".love.image" () ()))
          xb (? (bad? ib) 'bad
                (map (\ nm [nm (globs (rev (g 'globs)) nm () ()) (xro? nm)]) xnames))
          (? (foldl (\ a x (a || bad? x)) () [gb sb zg zs rg rs cg cs nb ib xb]) 'bad
             [(gb + sb) nb ib xb (zg + zs) (rg + rs + cg + cs + strs (g 'strs) ())])))

   ; the shared core: build the gen state, then TEXT (the functions, no _start)
   ; and DATA (globals + strings) separately -> (g textforms dataforms) | ().
   ; weaks: the __attribute__((weak)) names -- NEVER inlined (an inlined body
   ; would bake past the strong override the linker owes them).
   ; xtra: the parser's (sfns svars sects .. consts) tablets -- static fns, static
   ; globals, name -> section. () reads as three empties (the law harness).
   ; every nom anywhere in a form tree, pinned into a tablet -- the dead-static sweep's
   ; reference relation, and OVER-approximate on purpose: an opcode and a register name
   ; count too. A static wrongly kept costs bytes; one wrongly missed costs a wild jump.
   ; ⚠ a TABLET, never a list: love.c is 879 functions and a linear member? here made
   ; the sweep cost more than the compile it rides in.
   (cg-refs x t)
    (? (nom? x) (: _ (pin t x 1) t)
       !(two? x) t
       (cg-refs >x (cg-refs <x t)))
   ; the same relation over an AST, where a name is still TEXT (laid forms wear
   ; the interned nom). the data lane's roots are read here, before it is laid.
   (cg-srefs x t)
    (? (string? x) (: _ (pin t x 1) t)
       !(two? x) t
       (cg-srefs >x (cg-srefs <x t)))
   (cgen-core ast stbl sigs weaks xtra tgt)
    (: g {'n 0 'brk () 'brkm () 'cnt () 'cases () 'glob {} 'globs () 'strs () 'statics ()
          'abl (ablenv 0)                           ; the pricing knob, read per unit at run time
          'laid {}                                  ; names already on 'globs (a tentative repeat lays no twin)
          'fns {} 'inl {} 'cgitems cgitems 'stag stbl 'sigs sigs
          'tgt tgt 'epi (? (id? tgt 'thumb1) epi-t16 (member? tgt '(arm64 thumb2 thumb2sp rv64)) epi-a64 epi)
          'sects (two? xtra ? <>>xtra {})
          'alias (two? xtra && 3 < #xtra ? (nthr xtra 3) {})
          'aligns (two? xtra && 4 < #xtra ? (nthr xtra 4) {})
          'consts (two? xtra && 7 < #xtra ? (nthr xtra 7) {})
          ; the parser's restrict roster, fn name -> its `T *restrict` param names: an ALIAS
          ; PROMISE, which is why it rides beside the AST instead of in the type -- and the
          ; type tree has never carried a qualifier node. ⚠ nothing reads it yet: the consumer
          ; is the interior-store elision at a splice seam (THE LADDER step 7)
          'rstrs (two? xtra && 8 < #xtra ? (nthr xtra 8) {})
          'xfns ()                                  ; (secname forms) per fn that named a section, newest first
          'romemo {}}                               ; a const candidate's image, laid once and read by both walks
       ; pass 0: every function name -- so a bare use of one is its address (la). the sig table
       ; too: an mproto sibling (`double sin(..), cos(..);`) has a sig but no proto form of its
       ; own. a small single-return definition also lands in the INLINE table, before any body --
       ; an always_inline one (ainls, the parser's roster) at any size; a noinline one NEVER
       ; (the lvm fleet and the designed-ret seams stay out-of-line by the C's own word).
       ainls (two? xtra && 5 < #xtra ? (nthr xtra 5))
       noinls (two? xtra && 6 < #xtra ? (nthr xtra 6))
       ; -fno-inline: the driver's own noinline, said once for the whole TU. it outranks
       ; always_inline -- the flag is an INSTRUMENT (read this fn as written, not as some
       ; caller wears it), and an attribute that could override it would leave the reader
       ; with no way to say what it meant.
       nolin (two? xtra && 9 < #xtra ? (nthr xtra 9))
       (recfns fs) (? !(two? fs) 1
                      (: f <fs
                         _ (member? <f '(fn proto) ? (pin (g 'fns) <>f 1))
                         _ (? (<f = 'fn && !nolin &&
                               !(member? <>f weaks) &&
                               !(member? <>f noinls) &&
                               inlok? f (peep sigs <>f ()) (member? <>f ainls))
                              (pin (g 'inl) <>f f))
                         (recfns >fs)))
       _ (recfns <>ast)
       _ (: (go ns) (? !(two? ns) 1 (pin (g 'fns) <ns 1, go >ns))
            (go (keys sigs)))
       ; an array declared `[]` -- parse spells the open bound 0, and it is the one
       ; type a later declaration of the same name may COMPLETE.
       (openarr? ty) (two? ty && <ty = 'arr && <>>ty = 0)
       ; pass 1: record every global (name -> (label ty init)); cgimage
       ; validates the initializer at data-emission time (a label-valued image
       ; refuses the whole compile -- 4d)
       (rec fs)
        (? !(two? fs) 1
           (: f <fs
              (? !(member? <f '(gdecl xdecl)) (rec >fs)
                 (: xd (<f = 'xdecl)              ; extern: addressable, NO storage laid
                    (one ps)
                     (? !(two? ps) (rec >fs)
                        (: p <ps
                           old (g 'glob <p)
                           ; C's tentative rule (6.9.2): `int x; int x = 5;` in one TU is ONE
                           ; object -- an INITIALIZED def always wins the entry; a tentative
                           ; repeat keeps whatever stands, and lays no second symbol.
                           ; ⚠ a COMPLETE type outranks an incomplete one (6.2.7, the composite
                           ; type): `extern char x[];` then `char x[1024];` is ONE object of
                           ; 1024 bytes. Keeping the [] would lay it at SIZE ZERO, and the next
                           ; global then shares its address -- a link that succeeds and an
                           ; alias at run time (gzip's ifname/ofname, doc/misc/moon-c-gaps).
                           _ (? (!(two? old) || two? <>>p || (openarr? <>old && !(openarr? <>p)))
                                (pin (g 'glob) <p [(name <p) <>p <>>p]) ())
                           _ (? (!xd && !(peep (g 'laid) <p ()))
                                (: _ (pin (g 'laid) <p 1) (pin g 'globs (<p >< g 'globs))) ())
                           (one >ps)))
                    (one <>f)))))
       ok (rec <>ast)
       ; -- LAZY LOWERING, which is why no sweep follows it. we splice a small
       ; static into its call sites, and the out-of-line body then ships named
       ; by nothing -- 143 of love.c's 675, and 15 of the 16 syscall wrappers
       ; every nolibc member inherits from impl.h. so lay the ROOTS, read what
       ; their laid forms still call, and lay that, to closure.
       ; ⚠ THE ROOTS are every EXPORTED fn, every alias target, the
       ; section-named fns (placed for their address), and every fn an
       ; INITIALIZER names: love's kind-indexed tables carry fn addresses, and
       ; one missed there is a wild jump, not a bigger binary.
       ; ⚠ discovery order is NOT declaration order, so the pool and the label
       ; counter fall differently than laying every body would leave them. the
       ; units emit by declaration order, so .text still reads like the source
       ; -- but the bytes are not the bytes a sweep left.
       (glay _)
        (: fnix {}                              ; fn name -> its AST form
           _ (foldl (\ _ f (? (<f = 'fn) (pin fnix <>f f, 0) 0)) 0 <>ast)
           unom {}                              ; the interned nom -> that name
           _ (foldl (\ _ nm (pin unom (name nm) nm, 0)) 0 (keys fnix))
           sfns (? (two? xtra) <xtra {})
           als (peep g 'alias {})
           atg (foldl (\ a k (als k >< a)) () (keys als))
           (cut? nm) (!(id? (peep sfns nm 'no) 'no) && !(member? nm atg))
           ; ⚠ TWO tablets, and they have to be: a truth test reads a form list
           ; by its NET, which lands on zero often enough, so presence is its own
           ; mark and the forms ride beside it. laid-as-the-forms re-lays a body
           ; whose forms happen to sum to nothing, forever.
           laid {}                              ; fn name -> 1 once its body is laid
           lfm {}                               ; ..and the forms, for the ones that stayed in .text
           ; lay ONE body; answer the noms it still names. a refusal parks in
           ; 'genfail because the driver below is a loop, not an expression
           ; chain, and the cause wanted is the FIRST one.
           (lay1 nm)
            (: f (peep fnix nm ())
               _ (pin g 'undecl ()) _ (pin g 'nolane ()) _ (pin g 'arity ())   ; fresh per fn: the pins the refusal reads name THIS fn's cause
               r (cgfn g f)
               ; a refusal answers ONE gripe, and the driver puts it on stderr: half a
               ; sentence on each of two channels is what a build log then keeps
               (? (bad? r)
                  (: u (g 'undecl)      ; rung 1: an undeclared id was named -> a gripe carrying it + the enclosing fn
                     nl (g 'nolane)     ; rung 4: a deliberate refusal named the feature it has no lane for
                     ar (g 'arity)      ; a direct call disagreeing with its own prototype
                     _ (? (gripe? (g 'genfail)) 0        ; the FIRST cause stands
                          (pin g 'genfail
                               (? u (gripe () () () ("undeclared '" + u + "' (in " + nm + ")"))
                                  ar (gripe () () () (ar + (" (in " + nm + ")")))
                                  nl (gripe () () () (nl + (" (in " + nm + ")")))
                                  (gripe () () () ("cannot compile '" + nm + "' (cause unnamed)")))))
                     ())
                  ; ⚠ fn-start alignment to EVEN (2): a function's address can ride into the
                  ; heap as an object's ap, and the image codec's pointer/fixnum
                  ; discriminator is PARITY -- an odd-addressed fn pointer reads back as a
                  ; fixnum and the woken heap desyncs. ⚠ WIDER alignment was measured and
                  ; REVERTED (16 ran ~4% slower, 32 ~5%, branch-misses +9%: zeroing 4-5 low
                  ; bits aliases the BTB on this frontend-bound core). don't widen it.
                  ; an ALIAS is another NAME for the same address, so its label rides
                  ; immediately before the fn's own; the align stays outermost.
                  (: al2 (: (go l a2) (? !(two? l) a2
                                         (go >l (? (g 'alias <l = nm)
                                                   (['label (name <l)] >< a2) a2)))
                           (go (keys (g 'alias)) ()))
                     ; an aligned(N) fn lands ON its boundary, the global lane's
                     ; rule; 2 is the floor the parity law above sets.
                     an (: a (peep (g 'aligns) nm 2) (? (a < 2) 2 a))
                     fm (['align an] >< (al2 + r))
                     ; a fn that named its own section leaves .text for that
                     ; stream (love.c's data sentinels): where the code lands
                     ; is the point, the same reason a global gets the lane.
                     ; the align rides out too -- it is the section's grain, and
                     ; a run of one-fn sections at grain N tiles at exactly N.
                     sc (peep (g 'sects) nm ())
                     _ (nm = "main" ? (pin g 'saw 1))
                     _ (? sc (pin g 'xfns ([sc fm an] >< g 'xfns)) (pin lfm nm fm))
                     _ (pin laid nm 1)          ; BEFORE the walk: a recursive fn names itself
                     (keys (cg-refs fm {})))))
           (drive q)
            (? (gripe? (g 'genfail) || !(two? q)) 0
               (: nm <q
                  (? (laid nm) (drive >q)
                     (drive (foldl (\ a x (: t (unom x) (t && !(laid t) ? (t >< a) a)))
                                   >q (lay1 nm))))))
           ; ⚠ THE SECTIONED BODIES FIRST, and in DECLARATION ORDER. where the
           ; members of a named section land is the program's business, never
           ; ours: love.c's data sentinels each take their own love.data.N at a
           ; fixed stride, and ai_typ reads a kind by DIVIDING the offset. the
           ; worklist lays a root's callees before the next root, so driving
           ; these through it tiles them in discovery order and every heap
           ; object then reports the wrong rep.
           (secs fs acc)
            (? (!(two? fs) || (gripe? (g 'genfail))) acc
               (: f <fs
                  (? (<f = 'fn && g 'sects <>f && !(laid <>f))
                     (secs >fs (acc + (lay1 <>f)))
                     (secs >fs acc))))
           snoms (secs <>ast ())
           ; the roots, in declaration order, so what gets laid first still
           ; tracks the source and the pool falls the same way twice
           roots (: (go fs acc)
                     (? !(two? fs) (rev acc)
                        (: f <fs
                           (go >fs (<f = 'fn && !(cut? <>f) ? (<>f >< acc) acc))))
                    (go <>ast ()))
           ; what the sectioned bodies call joins the worklist like any other ref
           sq (foldl (\ a x (: t (unom x) (t && !(laid t) ? (t >< a) a))) () snoms)
           _ (drive (roots + sq))
           ; ⚠ THE DATA LANE NAMES FUNCTIONS TOO -- love's kind-indexed tables
           ; carry their addresses -- and it is laid LAST, so its roots are read
           ; from the INITIALIZERS instead: g 'glob holds every one, both the
           ; file-scope globals `rec` recorded and the block-scope statics a laid
           ; body adds. a body laid this round can carry its own, so this closes
           ; rather than passes. over-approximated the way cg-refs is: a literal
           ; that happens to spell a function's name keeps that function.
           (datclose _)
            (: t {}
               _ (foldl (\ _ k (: e (g 'glob k)
                                  (? (two? e) (cg-srefs <>>e t, 0) 0)))
                        0 (keys (g 'glob)))
               q (foldl (\ a s (? ((fnix s) && !(laid s)) (s >< a) a))
                        () (keys t))
               (? (two? q && !(gripe? (g 'genfail))) (drive q, datclose 0) 0))
           _ (datclose 0)
           gf (g 'genfail)
           (? (gripe? gf) gf
              (: dat (cgdata g)
                 (? (bad? dat)
                    (: c (g 'datacause)   ; the static-data refusal, minted here (globs pinned it)
                       (gripe () () () (? c c "cannot lay the static data (cause unnamed)")))
                    ; txt rides REVERSED -- a cat per fn re-copies every prior fn's
                    ; forms, quadratic over love.c's 611 -- and the rev restores order.
                    (: _ (pin g 'dropped
                          (foldl (\ a f (<f = 'fn && !(laid <>f) ? (<>f >< a) a)) () <>ast))
                       txt (foldl (\ a f (: u (<f = 'fn ? (lfm <>f))
                                          (two? u ? (revcat u a) a)))
                                  () <>ast)
                       [g (rev txt) dat (g 'saw)])))))
       (? ok (glay 0) ()))
   (cgen ast stbl sigs weaks xtra tgt)              ; the STATIC EXECUTABLE: _start (crt0) + text + data -> (1 forms) | () -- needs main
    (: crt0 (? (id? tgt 'arm64)
               '((call main) (li r8 93) (sys))            ; x0 already the exit code; x8 = __NR_exit (93), svc
               (id? tgt 'rv64)
               '((call main) (li r7 94) (sys))            ; a0 already the exit code; a7 = __NR_exit_group (94), ecall
               '((call main) (mov r6 r0) (li r0 60) (sys)))
       r (cgen-core ast stbl sigs weaks xtra tgt)
       (? (gripe? r) r                               ; a named cause rides out to the driver (a gripe is a chain, so guard before two?)
          !(two? r) ()
          !(nthr r 3) ()                          ; no main -> no program (the exe has nothing to start)
          (: dat <>>r                           ; (dataforms nifforms): the exe resolves labels itself,
             ; ..and the bss forms ride the tail as real zeros: one flat image has no
             ; NOBITS lane to put them in, so the file carries what the sections would not.
             ; rodata rides it too -- one lane, so nothing is read-only here either.
             [1 (crt0 + <>r + <>>>>>dat + <dat + <>dat + <>>>>dat)])))   ; so the nif stream just rides along
   ; the RELOCATABLE OBJECT view: text and data split, plus the DEFINED function names (global
   ; FUNC symbols, statics excluded -- they stay LOCAL, love.h's inline helpers land in every TU),
   ; the global-variable names, and the love_nifs stream -- the .o emitter turns the rest into a
   ; symbol table + relocations. the DEFINED static functions ride the tail as lnames, laid as
   ; LOCAL FUNC symbols so nm/gdb/vmret see real function boundaries.
   (cgen-obj ast stbl sigs weaks xtra tgt)          ; -> (1 textforms dataforms funcnames globnames nifforms imgforms lnames xsecs bssforms rodataforms xfnsecs) | ()
    (: r (cgen-core ast stbl sigs weaks xtra tgt)
       (? (gripe? r) r                               ; a named cause rides out (guard before two?, a gripe is a chain)
          !(two? r) ()
          (: sfns (two? xtra ? <xtra {})
             svars (two? xtra ? <>xtra {})
             sects (two? xtra ? <>>xtra {})
             (loc? nm) (!(id? (peep svars nm 'no) 'no) ||
                        peep sects nm () = "love_nifs")     ; a nif entry is static by shape; any OTHER
                                                                 ; named section stays exported (.image is read by symbol)
             als (? (two? xtra && 3 < #xtra) (nthr xtra 3) {})
             fnames (: (go fs acc) (? !(two? fs) (rev acc)
                                      (: f <fs
                                         (go >fs (? (<f = 'fn && id? (peep sfns <>f 'no) 'no)
                                                         (<>f >< acc) acc))))
                       ; an alias whose target is a function defined here is a
                       ; FUNC symbol of its own, at the same address
                       defd (: (go2 fs acc) (? !(two? fs) acc
                                               (: f <fs
                                                  (go2 >fs (? (<f = 'fn) (<>f >< acc) acc))))
                               (go2 <>ast ()))
                       (: base (go <>ast ())
                          (go3 l a2) (? !(two? l) a2
                                        (go3 >l (? (member? (als <l) defd) (<l >< a2) a2)))
                          (base + go3 (keys als) ())))
             gnames (: (go gs acc) (? !(two? gs) (rev acc)
                                      (go >gs (? (loc? <gs) acc (<gs >< acc))))
                       ; an alias onto a global (or a block-scope static) is an
                       ; OBJECT symbol of its own, at the same bytes
                       dgl (revcat (<r 'globs) $ rev (<r 'statics))
                       (: base (go (rev (<r 'globs)) ())
                          (go2 l a2) (? !(two? l) a2
                                        (go2 >l (? (member? (als <l) dgl) (<l >< a2) a2)))
                          (base + go2 (keys als) ())))
             ; ⚠ minus the swept ones: a LOCAL FUNC symbol whose body no longer rides
             ; would name an address belonging to whatever followed it.
             gone (<r 'dropped)
             lnames (: (go fs acc) (? !(two? fs) (rev acc)
                                      (: f <fs
                                         (go >fs (? (<f = 'fn && !(id? (peep sfns <>f 'no) 'no)
                                                     && !(member? <>f gone))
                                                         (<>f >< acc) acc))))
                       (go <>ast ()))
             dat <>>r
             ; the sectioned FUNCTIONS as (name forms align), folded to one stream
             ; per name in declaration order -- moon.l gives each an executable
             ; descriptor. a name's grain is the widest its members ask for.
             xfns (: (noms l acc) (? !(two? l) (rev acc)
                                     (noms >l (? (member? <<l acc) acc (<<l >< acc))))
                     (forsec nm l) (: (go l2 a2) (? !(two? l2) a2
                                                    (go >l2 (? (<<l2 = nm) (a2 + <><l2) a2)))
                                      (go l ()))
                     (grain nm l) (: (go l2 a2) (? !(two? l2) a2
                                                   (go >l2 (? (<<l2 = nm && a2 < <>><l2)
                                                              <>><l2 a2)))
                                     (go l 16))
                     (: src (rev (<r 'xfns))
                        (map (\ nm [nm (forsec nm src) (grain nm src)]) (noms src ()))))
             [1 <>r <dat fnames gnames
                <>dat <>>dat lnames <>>>dat <>>>>dat <>>>>>dat
                xfns]))))
) ; module 'moon
