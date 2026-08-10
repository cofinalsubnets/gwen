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
; aggregate rides x64; the lanes the other targets lack are doc/moon-c-gaps' asymmetries.
; the PIPELINE'S STAGES are typed: crew/moon/stage.l holds each pass's sig (input stage ->
; output stage), and the moon gate checks this file as data against it -- a new pass declares
; its sig there, and a recomposition of the strict chains is a clash naming its seam.
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
   divops '(div rem udiv urem)                      ; the division protocol (r0..r3 clobber)
   vshops '(shlv shrv sarv)                         ; the variable shifts (count via r1)
   unops '(neg not sx1 sx2 sx4 zx1 zx2 zx4)         ; one-register rewrites
   xmmops '(addsd subsd mulsd divsd movsd ucomisd cvtss2sd cvtsd2ss)   ; xmm-only: no gp effect
   barops '(call callr raw sys)                     ; the opaque barriers
   ; the gen state g (a tablet): 'n the label counter, 'off the frame's
   ; high-water offset, 'brk/'cnt the loop label stacks, 'glob the global
   ; table (name -> (label ty init)), 'globs the emission order, 'strs the
   ; string table ((label . text)..). env: ((name off ty)..) frame locals.
   (nlab g pfx) (: k (peep g 'n 0) _ (pin g 'n (k + 1)) (intern (pfx + show k)))
   argregs '(r6 r5 r2 r1 r7 r8)                      ; the SysV integer/pointer arg registers
   (xmmreg i) (intern ("f" + show i))                ; the SysV double arg registers f0..f7
   (nthr l i) (? (i < 1) <l (nthr >l (i - 1)))
   (nthty l i) (? !(two? l) () (i < 1) <l (nthty >l (i - 1)))      ; the i-th list element or ()
   (bad? x) (id? x 'bad)                             ; the gripe carrier (gripe/gripe?/okv?) lives in lex.l -- parse answers one too
   ; -- the value tuple: every expression answers (ty reg forms) -- its type, the REGISTER
   ; holding the value, and the forms that compute it. the flo lane keeps the f0 protocol (its
   ; reg field is not consulted) and dispatches on vty. the accessors keep every consumer honest
   ; about which field it reads. --
   (vty v) <v
   (vreg v) <>v
   (vfs v) <>>v
   (mkv ty reg fs) `(ty reg fs)
   ; -- the operand pool: an expression value may live in a home-pool register instead of riding
   ; the r0 protocol. g 'pool0 is the fn's whole set (opool minus any param homes), g 'pool the
   ; free list, reset at every statement (values never cross one). ralloc answers a register | ()
   ; (dry -> the caller keeps the shuttle); rfree is idempotent and ignores anything outside pool0.
   ; ⚠ a pool value NEVER lives across a call: pool registers are caller-saved and, unlike the
   ; homes, have no spill wrap -- the callish? gate holds that line. --
   opool '(r6 r5 r9 r10 r7 r8)
   (ralloc g) (: fr (peep g 'pool ())
                 (? (two? fr) (: _ (pin g 'pool >fr) <fr) ()))
   (rfree g r) (? (member? r (peep g 'pool0 ()) &&
                   !(member? r (peep g 'pool ())) &&
                   !(member? r (peep g 'vpin ())) &&               ; a vmap pin is not ours to free
                   !(member? r (peep g 'rpin ())))                 ; nor a splice's register bind
                  (pin g 'pool (r >< peep g 'pool ())) ())
   (wr? g r) (id? r 'r0 ||                             ; writable: protocol r0 or an OWNED pool reg
              (member? r (peep g 'pool0 ()) &&         ; (a borrowed home, a vmap pin or a bind
               !(member? r (peep g 'vpin ())) &&       ;  reg is read-only)
               !(member? r (peep g 'rpin ()))))
   (callish? x) (? !(two? x) ()                                ; does the AST reach a call? (an inline splice still may)
                   (<x = 'call) 1
                   (callish? <x || callish? >x))
   (tor0 g v)                                       ; land a value on the r0 protocol (frees its pool reg)
    (? (id? (vreg v) 'r0) (vfs v)
       (: rd (vreg v) rl (rev (vfs v))
          lf (? (two? rl) <rl ())
          ; a trailing leax retargets to r0 outright -- lea reads its sources before the dest
          ; write, so even an r0 index is licensed, and the deref's load then sits same-dest
          (? (two? lf && id? <lf 'leax && id? <>lf rd && wr? g rd)
             (: fs (rev (`('leax 'r0 <>>lf <>>>lf (nthr lf 4) (nthr lf 5)) >< >rl))
                _ (rfree g rd)
                fs)
             (: fs (vfs v + `(`('mov 'r0 rd)))
                _ (rfree g rd)
                fs))))
   (vdrop g v) (: fs (vfs v) _ (rfree g (vreg v)) fs)   ; the value is DISCARDED: keep the effects, free the reg
   ; the result register for an op consuming rA: reuse a writable operand,
   ; else a fresh pool reg, else the protocol r0
   (rdst g rA) (? (wr? g rA) rA (: rX (ralloc g) (? rX rX 'r0)))
   ; -- the vmap (the allocator's cross-statement leg): a universe local's value, once written
   ; through or initialized in a pool register, stays mapped name -> reg, so a re-read is ZERO
   ; forms. the pinned reg rides the borrowed-home contract, and the SLOT stays the single source
   ; of truth (every write still stores), so a flush never spills -- it only forgets. soundness is
   ; basic-block locality in GENERATION order: vmflush fires at every label and call emission
   ; point, and g 'vuniv already excludes address-taken and multiply-declared names. off while
   ; inlining (an inlinee's names would conflate with the caller's). --
   (vmon? g) ((two? (peep g 'vuniv ()) || two? (peep g 'vuarr ())) && !(peep g 'inlining ()))
   ; the READ side stays live through a splice's ARG binds (caller scope, caller names) --
   ; only the inlinee BODY blinds it ('inlbody depth: its names would conflate with the
   ; caller's). births stay gated on vmon? (no pin is born anywhere inside a splice).
   (vmget g nm)
    (? (!(two? (peep g 'vuniv ()) || two? (peep g 'vuarr ())) || (0 < peep g 'inlbody 0)) ()
       (: e9 (assoc nm (peep g 'vmap ()))
          (? (two? e9) <>e9 ())))
   ; ⚠ NOT `pins`: that is a prel MACRO (chained tablet pins), and a macro expands at head
   ; position over any local binding -- the collision answers the map itself, silently
   (vmregs l acc) (? !(two? l) acc
                     (: r9 <><l
                        (? (member? r9 acc) (vmregs >l acc)
                           (vmregs >l (r9 >< acc)))))
   (vmdrop g nm)
    (: m2 (filter (\ e9 !(id? <e9 nm)) (peep g 'vmap ()))
       (pins g 'vmap m2 'vpin (vmregs m2 ())))
   ; ⚠ no pin is BORN while a call's arguments stage (the argseal depth, set by call-fixed): the
   ; staging splices args right-to-left while generation runs left-to-right, so a pin born inside
   ; one arg would read STALE from a later-generated, earlier-spliced sibling. pins from BEFORE
   ; the call stay readable -- their loads precede the whole staging in every splice order. a
   ; depth count, not a flag: an argument may hold a nested call.
   (vmpin g nm r)
    (? (vmon? g && !(0 < peep g 'argseal 0) &&
        member? nm (peep g 'vuniv ()) &&
        member? r (peep g 'pool0 ()))
       (: _ (vmdrop g nm)
          _ (pin g 'vmap (`(nm r) >< peep g 'vmap ()))
          (? (member? r (peep g 'vpin ())) ()
             (pin g 'vpin (r >< peep g 'vpin ())))) ())
   (vmflush g) (pins g 'vmap () 'vpin ())
   ; -- the ARRAY leg of the vmap (doc/moon-regalloc.md lever 1): a once-declared 1-D
   ; gp-scalar array whose EVERY appearance is x[e] (any bare x is decay -- an escape,
   ; and under & or sizeof nothing licenses) gets element pins under MINTED keys ("x[3]").
   ; minted keys have no shared object identity, so this leg compares by CONTENT (=)
   ; throughout. reads pin (the load is canonicalizing); writes only DROP -- a constant
   ; index its own key, a variable index (or a fill, or an rmw) the whole array. the slot
   ; stays the single source of truth (write-through), so a variable-index READ needs no
   ; care at all -- and a non-escaping array admits no alias: no pointer analysis. --
   (akey nm k) (nm + "[" + show k + "]")
   (aelem g env e)                                  ; -> ('el key elty) | ('arr nm) | ()
    (? !(two? e && id? <e 'deref && two? <>e) ()
       (: X <>e
          (? !(id? <X 'bin && two? >X && id? <>X '+ &&
               two? <>>X && id? <<>>X 'var) ()
             (: nm <><>>X
                (? !(member? nm (peep g 'vuarr ())) ()
                   (: en (vent g env nm)
                      (? !(two? en && id? <en 'loc && arr? <>>en) ()
                         (: idx <>>>X
                            (? (two? idx && id? <idx 'num) `('el (akey nm <>idx) <>(<>>en))
                               `('arr nm))))))))))
   (vadrop9 g p)                                    ; drop by CONTENT prefix ("x[" the array, "x[3]" one key)
    (: n9a (tally p)
       m2 (filter (\ e9 (: k9 <e9
                            !((n9a <= tally k9) && (= p (snip k9 0 n9a)))))
                  (peep g 'vmap ()))
       (pins g 'vmap m2 'vpin (vmregs m2 ())))
   (adrop9 g env lhs)                               ; the write retires element pins
    (: ae (aelem g env lhs)
       (? !(two? ae) ()
          (id? <ae 'el) (vadrop9 g <>ae)
          (vadrop9 g (<>ae + "["))))
   (vapin g nm r)                                   ; the element pin: vmpin sans the universe check
    (? (vmon? g && !(0 < peep g 'argseal 0) &&
        member? r (peep g 'pool0 ()))
       (: _ (vadrop9 g nm)
          _ (pin g 'vmap (`(nm r) >< peep g 'vmap ()))
          (? (member? r (peep g 'vpin ())) ()
             (pin g 'vpin (r >< peep g 'vpin ())))) ())
   ; -- the forward joins: at a label whose arriving edges are all KNOWN (an if/?: join -- never
   ; a loop head, a user label, or a case), the map need not die. an entry (nm r) survives iff
   ; EVERY arriving edge carries the same pair -- on each edge r held nm's slot value when the
   ; edge left, and nothing runs ON an edge, so the intersection holds at the join. an arm whose
   ; forms end in ret/jmp/jmpr never arrives and stays out of the meet. a pinned reg is never on
   ; the free list (ralloc took it, rfree refuses vpin, a dropped pin waits for psreset), so a
   ; restored branch-point map cannot be clobbered by the other arm's scratch. --
   (vmsnap g) (peep g 'vmap ())
   (vmset g m) (pins g 'vmap m 'vpin (vmregs m ()))
   (vmeet a b) (filter (\ e9 (: o (assoc <e9 b)
                                 (? (two? o) (id? <>o <>e9) ()))) a)
   ; ⚠ may an ALIAS live across a splice body? only in a register the body's STAGING can
   ; never write. x64 staging owns exactly the protocol quad r0-r3 (r3 the callr park), so
   ; every pool/home/cs register aliases -- but a seat-2/3 ride (r2/r1) must not. elsewhere
   ; the arrivals r0-r7 are staging-adjacent (a64's shuttle rides r0-r3, spush2 r2/r3), so
   ; only the pool/cs families are safe -- an arrival alias there MISCOMPILES (getenv's
   ; PATH walk was the catch: a ride aliased across a body that staged through it).
   (alsafe? g r)
    (? (x64t? g) !(member? r '(r0 r1 r2 r3))
       (member? r (peep g 'pool0 ()) || member? r (cspool g)))
   ; and may THIS param alias? only if the body never WRITES it (asn/post on its var): the
   ; register holds the CALLER's live value, so the splice's own p++ would walk it.
   ; ⚠ a C name is a STRING here, so the compare is `=` -- `id?` answers false for every
   ; distinct one, and a scan that never matches is a gate that isn't there.
   (wrs? nm e)
    (? !(two? e) ()
       (member? <e '(asn post) &&
        two? <>e && (<<>e = 'var) && (<><>e = nm)) 1
       (: (go l) (? !(two? l) () (? (wrs? nm <l) 1 (go >l)))
          (go e)))
   ; how hard does the body lean on this param? reads weighted x8 per loop level (the alive
   ; pass's convention) -- the price of a whole-body park is paid in reloads saved.
   (rdw nm e w)
    (? !(two? e) 0
       ((<e = 'var) && two? >e && (<>e = nm)) w
       (: w9 (? (member? <e '(while do for)) (w * 8) w)
          (go l a) (? !(two? l) a (go >l (a + rdw nm <l w9)))
          ((? (two? <e) (rdw nm <e w9) 0) + go >e 0)))
   (vmout? fs)                                      ; does control fall out the bottom of the forms?
    (? !(two? fs) 1
       (: (go l) (? (two? >l) (go >l) <l)
          lf (go fs)
          !(member? <lf '(ret jmp jmpr))))
   ; the per-statement pool reset: pool0 minus the vmap pins
   (psreset g)
    (: vp (peep g 'vpin () + peep g 'rpin ())
       (? !(two? vp) (pin g 'pool (peep g 'pool0 ()))
          (pin g 'pool (filter (\ r9 !(member? r9 vp)) (peep g 'pool0 ())))))
   ; -- the TARGET seam: g 'tgt ('x64 | 'arm64 | 'thumb2) picks the ABI shapes; the IR stays
   ; neutral. arm64 (AAPCS64): args ride r0..r7, the frame base retargets to fp (x29) at the end
   ; of build (a4ize -- r4 is x4, a CALLER-saved arg register there), bl clobbers lr so the
   ; prologue saves it. thumb2 (AAPCS32, Cortex-M7): a 32-bit WORD (wsize=4), args in r0..r3, the
   ; same lr-push + fp-base + `bx lr` frame shape as arm64 (`arm?` covers both) -- r4 is
   ; callee-saved there, but riding fp keeps the arm64 build/epilogue path verbatim.
   ; register-homing is OFF there (nhome=0): every param spills to its frame slot. --
   (a64? g) (id? (peep g 'tgt ()) 'arm64)
   (t32? g) (member? (peep g 'tgt ()) '(thumb2 thumb2sp thumb1))   ; the 32-bit thumb ABI family (AAPCS32): ARMv7E-M + ARMv6-M share it
   (sp64? g) (id? (peep g 'tgt ()) 'thumb2sp)       ; ARMv7E-M with an SP-ONLY FPU (the playdate's STM32F746): f64 arithmetic softens
   (v6m? g) (id? (peep g 'tgt ()) 'thumb1)          ; ARMv6-M refinements ONLY: narrower immediates, no imm-mul, soft-divide, pop-pc epilogue
   (rv? g) (id? (peep g 'tgt ()) 'riscv64)          ; RV64IMFD (LP64): 64-bit, 8 gp args, hard doubles, the lr/fp frame
   (a8? g) (a64? g || rv? g)                        ; the 8-gp-arg LP64 pair: arm64 + riscv share the AAPCS64 shapes
   (arm? g) (a64? g || t32? g || rv? g)             ; the lr-push / fp-base frame family (riscv rides it whole: fused
                                                    ; fcmp conds read gt/ge/lt/le ordered like VCMP, trap is ebreak,
                                                    ; overflow rides adds/subs + set vs, mul imm materializes)
   (wsize g) (? (t32? g) 4 8)                       ; the machine WORD in bytes (a pointer, a gp scalar, a stack cell)
   argregs-a64 '(r0 r1 r2 r3 r4 r5 r6 r7)           ; AAPCS64: x0..x7
   argregs-t32 '(r0 r1 r2 r3)                        ; AAPCS32: r0..r3
   (argr g) (? (a8? g) argregs-a64 (t32? g) argregs-t32 argregs)   ; riscv a0..a7 map onto the same abstract r0..r7
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
   (cspool g) (? (arm? g) () '(r11 r12 r13 r14))
   (deep? pl) (two? >>pl)                           ; a pool affords a PRICED seat: two remain after the take
   (fbase g) (? (arm? g) 'fp 'r4)                   ; the frame base as post-build ir1 spells it (a4ize); arm64+thumb2 fp, x64 r4
   ; -- a DELIBERATE refusal names itself (doc/moon-diag.md). `'bad` here is TWO answers wearing
   ; one face: "I decline, take another lane" (a policy deopt, ordinary and recoverable) and
   ; "there is no lane for this, anywhere" (a target gate, an ABI class never built). only the
   ; second is a DIAGNOSIS, so only the second speaks -- `nolane` pins the phrase and answers the
   ; same bare bad, and gfns/cgdata read the pin ONLY where the function refuses outright.
   ; LAST pin wins: the deepest site reached before the unwind is the one that knows. --
   (nolane g txt) (: _ (pin g 'nolane txt) 'bad)
   (ontgt g) (" on " + show (peep g 'tgt 'x64))      ; " on arm64" -- for a phrase whose answer turns on the target
   ; the immediate gate: x64 ALU/cmp immediates ride le32; arm64 add/sub/cmp
   ; take imm12 (a negative flips, a 24-bit value splits in the backend), the
   ; logicals only a bottom-aligned bitmask (2^n - 1), multiply none at all.
   ; thumb2: ADDW/SUBW take a plain imm12 (0..4095, negative flips); cmp an imm8
   ; (0..255, CMN on negatives); the logicals/mul have no simple immediate form.
   (immok g cls n)
    (? (a64? g)
       (? (cls = 'addsub) (-16777215 <= n && n <= 16777215)   ; imm12, or the backend's hi12+lo12 split
          (cls = 'cmp) (-4095 <= n && n <= 4095)              ; imm12 only (no split -- flags)
          (cls = 'log) (0 < n && 0 = (n & n + 1))
          ())
       (v6m? g)
       (? (cls = 'addsub) (-255 <= n && n <= 255)             ; v6-M ADDS/SUBS imm8 (imm3 3-addr / imm8 2-addr; negative flips)
          (cls = 'cmp) (0 <= n && n <= 255)                   ; CMP imm8 only -- no CMN-imm on v6-M, so materialize negatives
          ())                                                 ; log/mul: materialize into a register
       (t32? g)
       (? (cls = 'addsub) (-4095 <= n && n <= 4095)           ; ADDW/SUBW imm12
          (cls = 'cmp) (-255 <= n && n <= 255)                ; CMP/CMN imm8
          ())                                                 ; log/mul: materialize into a register
       (fits32? n))
   ; -- the type floor (struct sizes come off the stag table riding in g) --
   (ptr? ty) (two? ty && <ty = 'ptr)
   (arr? ty) (two? ty && <ty = 'arr)
   (stru? ty) (two? ty && <ty = 'struct)
   (fn? ty) (two? ty && <ty = 'fn)           ; ('fn ret?) -- a function; decays to its address
   (bitf? ty) (two? ty && <ty = 'bitfield)    ; a bitfield member: (bitfield uty bitoff width); its lvalue is a UNIT address
   (uldop sz) (? (sz = 1) 'ldu1 (sz = 2) 'ldu2 (sz = 4) 'ldu4 'ld)   ; unsigned load of a sz-byte word
   (fnty g nm) (: sig (peep (g 'sigs) nm ())        ; a named function's type, return riding when sig'd
                  (? (two? sig) `('fn <sig) '(fn)))
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
             (: cls (tablet 0)
                (mark o c) (: i (>> o 3) old (peep cls i ())
                              (pin cls i (? (id? old 'int || id? c 'int) 'int 'sse)))
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
                (? (<>se <= 8) `((peep cls 0 'int))
                   `((peep cls 0 'int) (peep cls 1 'int)))))))
   (acls2 g ty ca cb) (: c (aclass g ty)
                         (two? c && two? >c &&
                          id? <c ca && id? <>c cb))
   (sse2? g ty) (acls2 g ty 'sse 'sse)       ; both eightbytes SSE: the xmm0:xmm1 pair
   (ipair? g ty) (acls2 g ty 'int 'int)      ; both INT: rax:rdx (return/result lanes)
   (aone g ty) (: c (aclass g ty) (? (two? c && !(two? >c)) <c ()))
   (aeb2 g ty) (: c (aclass g ty) (? (two? c && two? >c) c ()))       ; a 9..16B aggregate's TWO eightbyte classes (c0 c1), else ()
   ; the MEMORY class: an aggregate PAST 16 bytes (aclass answers ()). SysV passes it wholly on
   ; the stack -- the caller lays its eightbytes in the overflow block, and the callee's copy IS
   ; that memory, addressed off the incoming frame. x64 only (AAPCS64 differs). the 16+-alignment
   ; guard is UNREACHABLE today (cc ignores __attribute__((aligned))) and refuses rather than
   ; laying a wrong ABI, for the day alignment is honored.
   (amem? g ty) (stru? ty && !(aclass g ty) && !(arm? g) && !(t32? g))
   (asalign g ty) (: e (peep (g 'stag) <>ty 0) (? (two? e) <>>e 8))        ; a struct tag's alignment
   (aslots g ty) ((tsz g ty + 7 & -8) >> 3)                              ; its eightbyte count
   ; a MEMORY-class RETURN: the caller passes a hidden pointer as the implicit
   ; first gp argument, the callee fills that memory and answers the pointer.
   ; the v6-M base ABI does it past 4 bytes; SysV past 16 (aclass ()).
   (sretm? g ty) (stru? ty && ((v6m? g && (4 < tsz g ty)) || amem? g ty))
   (fnret ty) (? !(two? ty && (<ty = 'ptr)) 'long        ; an indirect callee's result type: ('ptr ('fn ret)) | 'long
                 (: ft <>ty (? (fn? ft && two? >ft) <>ft 'long)))
   ; unsigned scalars ride 'uchar 'ushort 'uint 'ulong -- same WIDTH as their signed
   ; twin, but a load zero-extends, `>>` is logical, compare/divide go unsigned, and
   ; a binary op is unsigned if EITHER operand is (simplified usual conversions).
   (usg? ty) (member? ty '(uchar ushort uint ulong ullong u128))
   ; llong/ullong exist only on t32 (canon folds `long long` to 'long on the 64-bit targets):
   ; the 8-byte REGISTER PAIR, lo in r0, hi in r1 -- the one type wider than the accumulator.
   (llt? ty) ((ty = 'llong) || (ty = 'ullong))
   (ubin ta tb) (? (llt? ta || llt? tb) (? (usg? ta || usg? tb) 'ullong 'llong)
                   (usg? ta || usg? tb) 'ulong 'long)
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
   (rbin g ta tb) (? (u32bin? g ta tb) 'uint (ubin ta tb))
   ; ⚠ and ASK THE LITERAL what C would call it. a bare number's value tuple types 'long on the
   ; 64-bit targets (the register is 64-bit and every other consumer wants that), but C says an
   ; unsuffixed literal that fits is an `int` -- and u32bin? DISQUALIFIES on a long operand, so
   ; `uint x * <lit>` would skip its wrap and carry bits past 31 that C already threw away.
   ; a literal too WIDE for int really is long, and then the result is 64-bit and must not wrap
   ; -- hence the range test rather than a blanket 'int. --
   (numty n) (? ((-2147483648 <= n) && (n <= 2147483647)) 'int 'long)
   ; ..and read the literal through its written face: -1 is (un neg (num 1)), ~0 is
   ; (un bnot (num 0)). litv folds the face to `(v) | (); numty9 types a compare or
   ; divide OPERAND the way C reads the source, falling back to the value tuple's type.
   (litv e) (? !(two? e) ()
               (<e = 'num) `(<>e)
               ((<e = 'un) && two? <>>e)
                (: r (litv <>>e)
                   (? !(two? r) ()
                      (<>e = 'neg) `((0 - <r))
                      (<>e = 'bnot) `(((0 - <r) - 1))
                      ()))
               ())
   (numty9 e ty) (: r (litv e) (? (two? r) (numty <r) ty))
   ; ..and a literal WEARING a cast coat is still a literal: ('cast ty ('num n)) with an
   ; integer ty (1UL, and the sizeof fold) rides the immediate lanes as its masked value
   ; PLUS the coat's type. narrow coats mask to their width (signed ones sign-extend --
   ; cfold's law); long/ulong pass the 64-bit pattern whole, so t32 refuses them (long is
   ; the word there, llong the pair). a face OVER a coat (-1UL) stays unfolded: the wrap
   ; runs at the coat's width and the register lane already types it right. knum is the
   ; gate's one read: bare or coated -> (v ty) | ().
   (cnum g e)
    (? !(two? e && id? <e 'cast) ()
       (: ty <>e in <>>e
          (? !(two? in && id? <in 'num && (charm? <>in || sun? <>in)) ()
             (: n <>in
                (? (ty = 'bool) `((? (n = 0) 0 1) ty)
                   (ty = 'uchar) `((& n 255) ty)
                   (ty = 'ushort) `((& n 65535) ty)
                   (ty = 'uint) `((& n 4294967295) ty)
                   (member? ty '(char short int))
                    (: w (? (ty = 'char) 8 (ty = 'short) 16 32)
                       r (& n ((<< 1 w) - 1))
                       s (? (r < (<< 1 (w - 1))) r (r - (<< 1 w)))
                       `(s ty))
                   (member? ty '(long ulong)) (? (t32? g) () `(n ty))
                   ())))))
   (knum g e) (? (two? e && id? <e 'num) `(<>e (numty <>e)) (cnum g e))
   ; a compare or divide takes the COMMON converted type's signedness (C11 6.3.1.8):
   ; the signedness of the typing door's puac (parse.l) -- ONE spelling of the law,
   ; parse and gen both consuming it, width included: on t32 a long cannot hold every
   ; uint, so long-vs-uint compares UNSIGNED there and signed on the 64-bit targets.
   ; a pointer compares as the unsigned word. znrw stays beside it: int meets uint at
   ; uint and the 64-bit targets also owe the 32-bit narrow -- the signed side rides
   ; sign-extended past bit 31 and C compares at uint width.
   (cmpu g ta tb)
    (? (ptr? ta || ptr? tb) 1
       (tusg? (puac !(t32? g) ta tb)))
   (znrw g ta tb ra rb)
    (? !(u32bin? g ta tb) ()
       ((? (usg? ta) () `(`('zx4 ra))) + (? (usg? tb) () `(`('zx4 rb)))))
   ; a case constant CONVERTS to the switch control's type (C11 6.8.4.2p5), spelled
   ; as the register's extended pattern: a 32-bit type (or the t32 word) wraps at
   ; 2^32, signed re-signs. no & here: the 64-bit mask is a BIG, and love's bitwise
   ; ops answer nothing on one -- % then a sign fix instead.
   (cfit g ty v)
    (: m (? (t32? g || !(member? ty '(long ulong llong ullong))) 4294967296 18446744073709551616)
       r (v % m)
       u (? (r < 0) (r + m) r)
       (? (usg? ty || ptr? ty) u
          (u < (m // 2)) u
          (u - m)))
   (scc op unsg) (? !unsg op (op = 'lt) 'below (op = 'gt) 'above (op = 'le) 'be (op = 'ge) 'ae op)
   (rflip op) (? (op = 'lt) 'gt (op = 'gt) 'lt (op = 'le) 'ge (op = 'ge) 'le op)      ; MIRRORED (n < x reads x > n): constant-on-the-left
   (irop op) (? (op = '+) 'add (op = '-) 'sub (op = '*) 'imul                         ; C op -> IR mnemonic; not one of the six -> 'no
                (op = '&) 'and (op = '|) 'or (op = '^) 'xor 'no)
   ; the NaN-honest ==/!= settle after ucomisd: UNORDERED (PF=1 on x64) must
   ; read false for every relation and true only for != -- == ANDs the ordered
   ; bit in, != ORs the parity bit (the truth lands in r0, r1 the scratch)
   fsete '((set eq r0) (set np r1) (and r0 r0 r1))
   fsetn '((set ne r0) (set p r1) (or r0 r0 r1))
   (negrel op) (? (op = 'eq) 'ne (op = 'ne) 'eq (op = 'lt) 'ge (op = 'ge) 'lt (op = 'le) 'gt 'le)   ; NEGATED: branch-if-false
   (fits32? v) ((-2147483648 <= v) && (v <= 2147483647))    ; holo's ALU/cmp/imul immediates ride le32
   (tsz g ty) (? (ptr? ty) (wsize g)
                 (arr? ty) (* <>>ty (tsz g <>ty))
                 (stru? ty) <>(peep (g 'stag) <>ty 0)
                 ((ty = 'char) || (ty = 'uchar) || (ty = 'bool)) 1 ((ty = 'short) || (ty = 'ushort)) 2
                 ((ty = 'int) || (ty = 'uint) || (ty = 'float)) 4
                 (llt? ty || (ty = 'double)) 8                ; 8-byte on EVERY target (t32's word is 4)
                 (d128? ty) 16
                 (wsize g))
   ; a member's (offset ty) off a struct type, | 'bad
   (fmem g ty name)
    (? !(stru? ty) 'bad
       (: e (peep (g 'stag) <>ty 0)
          (? !(two? e) 'bad
             (: f (assoc name <e)
                (? (two? f) `(<>f <>>f) 'bad)))))
   (lg2 n) (: (go k v) (? (v <= 1) k (go (k + 1) (>> v 1))) (go 0 n))   ; a true log2 -- a pointer diff over a >8-byte struct needs sar 4+, not a capped 3
   ; pointer-scale by an immediate: x64 imul takes it straight; arm mul has no immediate form, so
   ; the size rides r3 first (free mid-expression: the shuttle holds r0/r1, args load at the pops)
   (imby g rX sz) (? (arm? g) `(`('li 'r3 sz) `('imul rX rX 'r3)) `(`('imul rX rX sz)))   ; arm mul (a64/t32/v6-M) has no imm form
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
   (pow2? n) ((0 < n) && (0 = (& n (n - 1))))
   (immop g op av aty n nty wnt)                    ; av the LEFT value tuple; nty the constant's C type; wnt the advisory destination
    (: rA (vreg av)
       (? (llt? aty || d128? aty) 'no               ; a pair never rides the immediate lane
          (member? op '(eq ne lt le gt ge))
           ; C converts a negative literal to the uint side (0xffffffff will not sit
           ; in a signed immediate): the shuttle materializes and narrows
           (? (u32? g aty && (n < 0)) 'no
              !(immok g 'cmp n) 'no
              (: rd (? wnt wnt (rdst g rA))
                 fs (vfs av + `(`('cmp rA n) `('set (scc op (cmpu g aty nty)) rd)))
                 _ (? (id? rd rA) () (rfree g rA))
                 (mkv 'long rd fs)))
          (member? op '(/ %))
           ; an unsigned participant licenses the strength-reduction: an unsigned LEFT is
           ; nonnegative in its register (canonical zero-extension), an unsigned CONSTANT
           ; makes the common type unsigned -- either way shr/and read the pattern right
           (? !((usg? aty || usg? nty) && pow2? n) 'no
              (op = '/) (? (n = 1) (mkv (rbin g aty nty) rA (vfs av))
                           (? (wr? g rA)
                              (mkv (rbin g aty nty) rA (vfs av + `(`('shr rA (lg2 n)))))
                              (: rd (? wnt wnt (rdst g rA))   ; a borrowed home shifts a COPY (no 3-address shr)
                                 (mkv (rbin g aty nty) rd (vfs av + `(`('mov rd rA) `('shr rd (lg2 n))))))))
              (n = 1) (mkv (rbin g aty nty) 'r0 (vdrop g av + '((li r0 0))))
              !(immok g 'log (n - 1)) 'no                ; t32 has no simple AND-imm: materialize (the register path)
              (: rd (? wnt wnt (rdst g rA))
                 fs (vfs av + `(`('and rd rA (n - 1))))
                 _ (? (id? rd rA) () (rfree g rA))
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
                         fs (vfs av + `(`(iop rd rA sn)) + (? u32 `(`('zx4 rd)) ()))
                         _ (? (id? rd rA) () (rfree g rA))
                         (mkv rt rd fs))))))))
   (ldop ty) (? (ty = 'char) 'ld1 (ty = 'short) 'ld2 (ty = 'int) 'ld4
                ((ty = 'uchar) || (ty = 'bool)) 'ldu1 (ty = 'ushort) 'ldu2 (ty = 'uint) 'ldu4 'ld)
   (stop ty) (? ((ty = 'char) || (ty = 'uchar) || (ty = 'bool)) 'st1 ((ty = 'short) || (ty = 'ushort)) 'st2
                ((ty = 'int) || (ty = 'uint)) 'st4 'st)
   ; ⚠ bool's cvt is a CONVERSION, not a re-narrow: any nonzero pattern becomes 1
   ; (C11 6.3.1.2) -- every write site already runs cvt before its store, so a bool
   ; object holds 0/1 by construction and loads ride a plain ldu1.
   (cvt ty r) (? (ty = 'char) `(`('sx1 r)) (ty = 'short) `(`('sx2 r)) (ty = 'int) `(`('sx4 r))
                 (ty = 'uchar) `(`('zx1 r)) (ty = 'ushort) `(`('zx2 r)) (ty = 'uint) `(`('zx4 r))
                 (ty = 'bool) `(`('cmp r 0) `('set 'ne r)) ())
   ; -- the double lane: a 'double value lives in f0, not r0. its type is the flag; fbits is the
   ; compile-time IEEE-754 encoder; asflo/asint move a cgexpr result between the lanes; compares
   ; ride ucomisd's UNSIGNED flags, NaN-honest. a float and a double SHARE the register lane -- a
   ; value in an xmm is ALWAYS a 64-bit double, only MEMORY differs: a `float` slot is 4 bytes, so
   ; a load widens (ldss + cvtss2sd) and a store narrows (cvtsd2ss + stss). fldf/fstf pick the lane
   ; by the DECLARED type; every loaded float types 'double. --
   (flo? ty) ((ty = 'double) || (ty = 'float))
   ; ⚠ a SUBNORMAL carries no implicit leading 1 and sits at one fixed exponent, so the normal
   ; lane's (e + bias) runs NEGATIVE for it and the pattern lands on nonsense. ⚠ and a `float`
   ; constant must ROUND to its 24 bits -- truncating leaves 0.1f a ulp light (the double lane
   ; hides it, being exact there: its fraction IS an integer).
   (mul2 x k) (? (k <= 1000) (* x (k 2.0)) (mul2 (* x (1000 2.0)) (k - 1000)))   ; x * 2^k, split so 2^1074 never needs a double
   (fnear y)                                        ; y >= 0 -> the nearest integer, ties to EVEN
    (: f (int y) r (y - (0.0 + f))
       (? (r > 0.5) (f + 1) (r < 0.5) f (& f 1) (f + 1) f))
   (fenc ax bias p)                                 ; positive finite -> biased-exponent * 2^p + mantissa fraction
    (: mant (p 2)
       emin (1 - bias)                              ; the smallest normal's exponent
       (? (ax < emin 2.0) (fnear (mul2 ax (p - emin)))         ; subnormal: the biased exponent is 0
          (: eu ((: (go e) (? (((e + 1) 2.0) <= ax) (go (e + 1)) e)) (int (/ (log ax) (log 2.0))))
             e  ((: (go e) (? (ax < e 2.0) (go (e - 1)) e)) eu)
             m  (fnear (* ((/ ax (e 2.0)) - 1.0) (0.0 + mant)))
             ; a fraction that rounds up out of its binade carries into the exponent
             ; (0x1.ff..f -> 0x2.0), and at the top that IS the infinity pattern C wants
             (? (m < mant) (+ (* (e + bias) mant) m) (* ((e + 1) + bias) mant)))))
   (fbits x)                                        ; double -> its IEEE-754 bits as the SIGNED i64 pattern
    (? (x = 0.0) 0                                  ; (the sign bit would make a BIG, and les/imnum walk i64 patterns)
       !(x = x) 9221120237041090560                 ; nan (the quiet pattern) -- fenc's climb never ends on a non-finite
       ((x + x) = x) (? (x < 0.0) -4503599627370496 9218868437227405312)   ; +-inf (1e999 reads as one)
       (: m (fenc (? (x < 0.0) (0.0 - x) x) 1023 52)
          (? (x < 0.0) (m - 9223372036854775808) m)))
   (fbits32 x)                                      ; double -> its IEEE-754 SINGLE (u32) bits (a `float` image)
    (? (x = 0.0) 0
       !(x = x) 2143289344
       ((x + x) = x) (? (x < 0.0) 4286578688 2139095040)
       (: ax (? (x < 0.0) (0.0 - x) x)
          ; under 2^128 fenc's own carry covers the top edge; at or above it the exponent
          ; would wrap, so name the infinity here
          m (? (ax < 128 2.0) (fenc ax 127 23) 2139095040)
          (+ (* (? (x < 0.0) 1 0) 2147483648) m)))
   (fldf ty freg base off)                          ; load a float/double from [base+off] into freg (widen if float)
    (? (ty = 'float) `(`('ldss freg base off) `('cvtss2sd freg freg))
       `(`('ldsd freg base off)))
   ; the narrow scratch: x64 f2 (xmm2, caller-saved, never live here); arm64 f16 -- ⚠ v2 IS the
   ; third fp ARGUMENT and v8..v15 carry a callee-saved low half, so v16+ are the only pure temps.
   ; t32 (fpv5-d16) has no register past d15, so d15/f15 is RESERVED as the scratch (the backend's
   ; converters own its S30 half) -- moon never allocates it.
   (fsc g) (? (a8? g) 'f16 (t32? g) 'f15 'f2)
   (fstf g ty base off val)                         ; store val (a double in an xmm) to [base+off] as ty (narrow if float)
    (? (ty = 'float) (: f9 (fsc g) `(`('cvtsd2ss f9 val) `('stss base off f9)))   ; scratch narrows (val untouched)
       `(`('stsd base off val)))
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
       _ (vmflush g)
       `(`('li 'r1 0x43e0000000000000) '(movqxr f1 r1)         ; 2^63
          '(ucomisd f0 f1) `('br 'ae lb)
          '(cvttsd2si r0 f0) `('jmp ld)                        ; d < 2^63: straight
          `('label lb)
          '(subsd f0 f1)                                       ; d - 2^63, exact
          '(cvttsd2si r0 f0)
          `('li 'r1 0x8000000000000000) '(xor r0 r0 r1)         ; put 2^63 back
          `('label ld)))
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
           (: h <>e                             ; a homed param's "off" slot is a (reg rN) list;
              (? (two? h && (<h = 'reg)) `('regv <>h <>>e)     ; a block-scope static's is (glob lbl)
                 (two? h && (<h = 'regw)) `('regw <>h <>>e)    ; a d128 pair home: (lo hi)
                 (two? h && (<h = 'glob)) `('glo <>h <>>e)
                 `('loc h <>>e)))
          (: ge (peep (g 'glob) name 0)
             (? (two? ge) `('glo <ge <>ge) 'bad))))
   ; a targetable LEAF re-emitted straight into register rX -> forms | 'no. leaves are PURE (no
   ; strs pin, no argpos), so discarding the r0 emission and re-aiming costs nothing. a homed
   ; param never lands here (its tuple already rides its home register, zero forms).
   (cgleaf g env e rX)
    (: kc (knum g e)
       (? (two? kc) `(`('li rX <kc))
          !(<e = 'var) 'no
          (: en (vent g env <>e)
             (? (bad? en) 'no
                !(<en = 'loc) 'no
                (: ty <>>en
                   (? (stru? ty || arr? ty || flo? ty || d128? ty) 'no
                      `(`((ldop ty) rX 'r4 <>en))))))))
   ; PARK the left operand out of r0 so the right may compute: a leaf re-aims at a fresh pool
   ; register (no protocol mov at all), anything else takes one mov. answers the (possibly
   ; re-homed) tuple | 'sh (pool dry -> the caller keeps the memory shuttle).
   (vpark g env e v)
    (? !(id? (vreg v) 'r0) v
       (: rX (ralloc g)
          (? !rX 'sh
             (: lf (cgleaf g env e rX)
                (? !(id? lf 'no) (mkv (vty v) rX lf)
                   (mkv (vty v) rX (vfs v + `(`('mov rX 'r0)))))))))
   ; a truth value in r0 that `cmp r0 0` can test: a double folds to d != 0.0
   (ctest g env e)
    (: r (cgexpr g env e)
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
                 (dok >ps (`(<p9 <>p9) >< acc9)))))
       (go items dacc)
        (? !(two? items) ()
           (: it <items
              (? (<it = 'decl)
                  (: ds (dok <>it dacc) (? (bad? ds) () (go >items ds)))
                 !(<it = 'ret) ()
                 !(two? <>it) ()
                 (two? >items) ()
                 `(dacc <>it))))
       (go <><>>>f ()))
   (abit? e9) (? !(two? e9) ()
                 ((<e9 = 'un) && (<>e9 = 'not)) 1
                 (member? <e9 '(land lor)) 1
                 ((<e9 = 'bin) && member? <>e9 '(eq ne lt le gt ge)))
   (cbinl g env e cc lbl)
    (: head <>e args <>>e
       il0 (peep g 'inlining ())                  ; cbinl NESTS to depth 3 (it emits no
       d0 (? !il0 0 il0)                          ; labels, so cginl's collision reason
       inlf (? (2 < d0) ()                        ; is moot -- only recursion needs a cap)
               !(<head = 'var) ()
               !(bad? (vent g env <>head)) ()     ; a local/global shadows the name
               (: c9 (peep (g 'inl) <>head ())
                  (? !(two? c9) ()
                     (tally args = tally <>>c9) c9
                     ())))
       rx (? (two? inlf) (retex inlf) ())
       e9 (? (two? rx) <>rx ())
       (? !(abit? e9) 'no
          (: fe0 (peep g 'fesc ())
             strs0 (g 'strs)
             rp0 (peep g 'rpin ())
             ; the register-bind license: a call-free splice (body and args) reads its scalar
             ; args from REGISTERS. an arg that is a word-typed pinned local ALIASES its pin
             ; (call-by-value holds: wrs? bars a body that assigns the param, and a later drop
             ; leaves the alias holding the passed value); anything else parks canonically in a
             ; fresh pool reg. rpin holds every bind reg against wr?/rfree/psreset for the splice's
             ; span (a body label's flush forgets vpin, never rpin). the slot bind stands when
             ; the type narrows to the alias, a call rides anywhere, or the pool is SHALLOW --
             ; a park may only take a register when two stay free: parks that drained the pool
             ; starved the hints and pins around them and the shuttle spushed (measured: corpus
             ; L1 loads +0.4% -- the displacement is unpriced by any other gate).
             (anyc l) (? !(two? l) () (? (callish? <l) 1 (anyc >l)))
             nocall (!(callish? e9) && !(anyc args))
             _ (pin g 'inlining (d0 + 1))
             (cbind ps as benv acc rbs)            ; cginl's bind, the same slots
              (? !(two? ps) `(benv acc rbs)
                 (: pty <><ps
                    al (? (nocall && !(flo? pty) && (8 = tsz g pty) &&
                           two? <as && (<<as = 'var) &&
                           !(wrs? <<ps <><>>>inlf))
                          (: nm9 <><as
                             pM (vmget g nm9)               ; a pinned local -- or a homed/riding
                             (? pM pM                       ; one (regv): both are read-only regs
                                (: v9 (vent g env nm9)      ; holding the canonical current value
                                   (? (two? v9 && (<v9 = 'regv) && !(flo? <>>v9))
                                      <>v9 ()))))
                          ())
                    al (? al (? (alsafe? g al) al ()) ())   ; staging must never write it
                    (? al                          ; the alias bind: zero forms
                        (: _ (pin g 'rpin (al >< peep g 'rpin ()))
                           (cbind >ps >as (`(<<ps `('reg al) pty) >< benv) acc rbs))
                       (: r (cgexpr g env <as)
                          (? (bad? r) 'bad
                             (pair? g pty)         ; a pair param: an 8-byte slot, both words
                              (: off (ntmp g 'llong)
                                 (cbind >ps >as
                                        (`(<<ps off pty) >< benv)
                                        (acc + asll g r + pst 'r4 off) rbs))
                             (: rb (? (nocall && !(flo? pty) &&
                                       two? >>(peep g 'pool ()))              ; park only into a DEEP pool
                                       (ralloc g) ())
                                (? rb              ; the park bind: the value rides a pool reg
                                    (: _ (pin g 'rpin (rb >< peep g 'rpin ()))
                                       (cbind >ps >as (`(<<ps `('reg rb) pty) >< benv)
                                              (acc + asintt g r pty
                                                   + (`(`('mov rb 'r0)) + cvt pty rb))
                                              (rb >< rbs)))
                                   (: off (ntmp g 'long)
                                      (cbind >ps >as
                                             (`(<<ps off pty) >< benv)
                                             (acc + asintt g r pty
                                                  + (? (id? pty 'bool) (cvt pty 'r0) ())   ; the entry conversion the splice skips
                                                  + `(`('st 'r4 off 'r0)))
                                             rbs)))))))))
             (dtmp ds benv9)                       ; the body's own decls: fresh temps over the params
              (? !(two? ds) benv9
                 (: d9 <ds (dtmp >ds (`(<d9 (ntmp g <>d9) <>d9) >< benv9))))
             br (cbind <>>inlf args () () ())
             ib0 (peep g 'inlbody 0)
             _ (pin g 'inlbody (ib0 + 1))          ; the body blinds vmget (name conflation)
             bf (? (bad? br) 'bad (cbranch g (dtmp <rx <br) e9 cc lbl))
             _ (pins g 'inlbody ib0 'inlining il0 'rpin rp0) ; the bind pins die with the splice
             (unrb l) (? !(two? l) ()
                         (: _ (rfree g <l) (unrb >l)))
             _ (? (two? br) (unrb (nthr br 2)) ())
             (? (bad? bf)
                 (: _ (pin g 'fesc fe0) _ (pin g 'strs strs0) 'no)
                (!fe0 && peep g 'fesc ())          ; the body took a frame address: keep the caller lean
                 (: _ (pin g 'fesc ()) _ (pin g 'strs strs0) 'no)
                (<>br + bf)))))
   ; -- (v & k) as a bare condition: TEST is the flags-only AND, so the value's register SURVIVES
   ; the check -- no and-clobber, a mapped local stays mapped, no park/reload. eq/ne read ZF only,
   ; so truth through the mask is exact at every width. both backends carry test-imm (x64 F7 /0
   ; id, arm64 ands xzr #bitmask), and the 'log gate is exactly test's immediate reach on each. --
   (tbr g env v k cc lbl)
    (: a (cgexpr g env v)
       (? (bad? a || flo? (vty a)) 'bad
          (: fs (vfs a + `(`('test (vreg a) k) `('br cc lbl)))
             _ (rfree g (vreg a))
             fs)))
   ; -- branch on a condition WITHOUT materializing its truth value: forms that jump to lbl when
   ; the condition is FALSE (cc 'eq) or TRUE (cc 'ne), the same sense as the fallback
   ; `(cmp r0 0) (br cc lbl)` over a ctest value. a comparison fuses to cmp + br<relation>, !e
   ; flips the sense, && and || short-circuit straight into the branch structure, a mask test
   ; (e & k) branches off TEST (tbr), a predicate CALL splices through cbinl, and a float compare
   ; branches off ucomisd/fcmp NaN-honest (fcb). forms | 'bad. --
   (cbranch g env e cc lbl)
    (? ((<e = 'un) && (<>e = 'not))
        (cbranch g env <>>e (? (cc = 'eq) 'ne 'eq) lbl)
       (<e = 'land)
        (? (cc = 'eq)
           (: f1 (cbranch g env <>e 'eq lbl)
              f2 (cbranch g env <>>e 'eq lbl)
              (? (bad? f1 || bad? f2) 'bad (f1 + f2)))
           (: lf (nlab g ".f")
              f1 (cbranch g env <>e 'eq lf)
              f2 (cbranch g env <>>e 'ne lbl)
              _ (vmflush g)                            ; label lf joins
              (? (bad? f1 || bad? f2) 'bad (f1 + f2 + `(`('label lf))))))
       (<e = 'lor)
        (? (cc = 'ne)
           (: f1 (cbranch g env <>e 'ne lbl)
              f2 (cbranch g env <>>e 'ne lbl)
              (? (bad? f1 || bad? f2) 'bad (f1 + f2)))
           (: lt (nlab g ".t")
              f1 (cbranch g env <>e 'ne lt)
              f2 (cbranch g env <>>e 'eq lbl)
              _ (vmflush g)                            ; label lt joins
              (? (bad? f1 || bad? f2) 'bad (f1 + f2 + `(`('label lt))))))
       ((<e = 'bin) && member? <>e '(eq ne lt le gt ge))
        (: op <>e
           lhs <>>e rhs <>>>e
           ; ⚠ LAZY left-then-right: the left PARKS (allocates) before the right evaluates --
           ; allocation order must match execution order, or the right's freed inner temps alias
           ; the parked left. the float lane FUSES: discard the int-flavored emission (⚠ the map
           ; flushes first -- a pin born in the dropped forms would survive with its establishing
           ; load gone), re-evaluate both sides through the double shuttle, then branch off the
           ; compare. UNORDERED (a NaN side) is false for every relation and true only for !=.
           ; x64: above/ae read CF=0, which unordered sets, so < and <= compare SWAPPED, and
           ; ==/!= carry the parity bit in the BRANCH STRUCTURE. arm64: fcmp's gt/ge/eq/ne are
           ; unordered-false natively, so every relation is ONE branch and negrel spells false.
           (fcb z9)
            (: _ (vmflush g)
               a (cgexpr g env lhs)
               (? (bad? a) 'bad
                  (: af (? (t32? g)                      ; the gp shuttle moves the PAIR on t32
                           (asflo g a + '((movqrx r0 f0)) + spush2)
                           (asflo g a + '((movqrx r0 f0)) + spush 'r0))
                     b (cgexpr g env rhs)
                     (? (bad? b) 'bad
                        (: mid (? (t32? g)
                                  (af + asflo g b + (spop2 'r2 'r3 + '((movqxr f1 r2))))
                                  (af + asflo g b + spop 'r1 + '((movqxr f1 r1))))
                           swp (member? op '(lt le))
                           cf `(`('ucomisd (? swp 'f0 'f1) (? swp 'f1 'f0)))
                           ecc (? (arm? g)               ; VCMP/FCMP flags: gt/ge unordered-false native
                                  (: t9 (? (member? op '(lt gt)) 'gt (member? op '(le ge)) 'ge op)
                                     (? (cc = 'ne) t9 (negrel t9)))
                                  (member? op '(lt gt)) (? (cc = 'ne) 'above 'be)
                                  (member? op '(le ge)) (? (cc = 'ne) 'ae 'below)
                                  ())
                           (? ecc (mid + cf + `(`('br ecc lbl)))
                              (? (cc = 'ne) (op = 'ne) (op = 'eq))
                               (mid + cf + `(`('br 'p lbl) `('br 'ne lbl)))
                              (: sk (nlab g ".u")
                                 _ (vmflush g)
                                 (mid + cf + `(`('br 'p sk) `('br 'eq lbl) `('label sk))))))))))
           (bcc o u) (scc (? (cc = 'ne) o (negrel o)) u)
           ; a pair side (t32 llong) branches off subs/sbcs via pbrf; a constant other side
           ; materializes as a sign-widened pair in r2:r3, a 32-bit other side widens the same way.
           (pnum v) `(`('li 'r2 (& v 4294967295)) `('li 'r3 (? (v < 0) 4294967295 0)))
           ; a wide relation settles 0/1 through c128 (the LEFT rides the cell), then branches on it
           (wbr o2 af u2) (af + (`(`('c128 (scc o2 u2) 'sp)) + (wdrop + `('(cmp r0 0) `('br cc lbl)))))
           (wnum v) `(`('li 'r0 v) `('li 'r2 (? (v < 0) -1 0)))
           kr (knum g rhs)
           kl (knum g lhs)
           (? (two? kr && immok g 'cmp <kr)
               (: a (cgexpr g env lhs)
                  (? (bad? a) 'bad
                     (flo? (vty a)) (: _ (rfree g (vreg a)) (fcb ()))
                     (pair? g (vty a))
                      (vfs a + pnum <kr + pbrf g op (usg? (vty a)) 'r0 'r1 'r2 'r3 cc lbl)
                     (d128? (vty a))
                      (? !(x64t? g) 'bad
                         (wbr op (as128 g a + (wpush + wnum <kr)) (id? (vty a) 'u128)))
                     (: n <kr
                        cc9 (bcc op (cmpu g (vty a) <>kr))
                        ; C converts a negative literal to the uint side (the masked
                        ; pattern will not sit in a signed immediate): materialize it
                        fs (? (u32? g (vty a) && (n < 0))
                              (: sc (? (id? (vreg a) 'r1) 'r0 'r1)
                                 (vfs a + `(`('li sc (& n 4294967295)) `('cmp (vreg a) sc) `('br cc9 lbl))))
                              (vfs a + `(`('cmp (vreg a) n) `('br cc9 lbl))))
                        _ (rfree g (vreg a))
                        fs)))
              (two? kl && immok g 'cmp <kl)
               (: b (cgexpr g env rhs)
                  (? (bad? b) 'bad
                     (flo? (vty b)) (: _ (rfree g (vreg b)) (fcb ()))
                     (pair? g (vty b))
                      (vfs b + pnum <kl + pbrf g (rflip op) (usg? (vty b)) 'r0 'r1 'r2 'r3 cc lbl)
                     (d128? (vty b))
                      (? !(x64t? g) 'bad
                         (wbr (rflip op) (as128 g b + (wpush + wnum <kl)) (id? (vty b) 'u128)))
                     (: n <kl
                        cc9 (bcc (rflip op) (cmpu g <>kl (vty b)))
                        fs (? (u32? g (vty b) && (n < 0))
                              (: sc (? (id? (vreg b) 'r1) 'r0 'r1)
                                 (vfs b + `(`('li sc (& n 4294967295)) `('cmp (vreg b) sc) `('br cc9 lbl))))
                              (vfs b + `(`('cmp (vreg b) n) `('br cc9 lbl))))
                        _ (rfree g (vreg b))
                        fs)))
              ; the destination die: when no call bars a register park, the left AIMS at its
              ; park before it evaluates -- a load/li lands there directly and vpark sees a
              ; delivered value (vreg off r0: no bridge mov). unhonored (a mapped local, a
              ; float, a wide) hands the register back and the post-hoc park stands.
              (: pk (? (callish? lhs || callish? rhs) () (ralloc g))
                 _ (? pk (pin g 'want pk) ())
                 a (cgexpr g env lhs)
                 _ (? (pk && !(!(bad? a) && id? (vreg a) pk)) (rfree g pk) ())
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
                              b (cgexpr g env rhs)
                              (? (bad? b) 'bad
                                 (flo? (vty b)) (? wA 'bad (: _ (rfree g (vreg b)) (fcb ())))
                                 (wA || d128? (vty b))                 ; a wide side: c128 over the cell
                                  (? !(x64t? g) 'bad
                                     (: u2 (id? (? (wA && d128? (vty b)) (wbin (vty a) (vty b))
                                                   wA (vty a) (vty b)) 'u128)
                                        ; a WORD left widens its cell hi by its own signedness
                                        fixa (? wA ()
                                                (usg? (vty a) || ptr? (vty a)) '((si sp 8 0))
                                                '((ld r1 sp 0) (sar r1 63) (st sp 8 r1)))
                                        (af + as128 g b + fixa +
                                         (`(`('c128 (scc op u2) 'sp)) + (wdrop + `('(cmp r0 0) `('br cc lbl)))))))
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
                                         `('(cmp r1 r0) `('br (bcc op (cmpu g ta9 tb9)) lbl)))
                                    (? (r1safe? bf) (af0 + '((mov r1 r0)) + bf + brf)
                                       (af + bf + (spop 'r1 + brf))))))
                          (: b (cgexpr g env rhs)
                             (? (bad? b) 'bad
                                (flo? (vty b)) (: _ (rfree g (vreg ah)) _ (rfree g (vreg b)) (fcb ()))
                                (d128? (vty b))                       ; a word park against a wide right: build its cell
                                 (? !(x64t? g) 'bad
                                    (: fs (vfs ah + vfs b
                                           + `('(sub sp sp 16) `('st 'sp 0 (vreg ah)))
                                           + (? (usg? (vty ah) || ptr? (vty ah)) '((si sp 8 0))
                                                `(`('mov 'r1 (vreg ah)) '(sar r1 63) '(st sp 8 r1)))
                                           + (`(`('c128 (scc op (id? (vty b) 'u128)) 'sp)) + (wdrop + `('(cmp r0 0) `('br cc lbl)))))
                                       _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                       fs))
                                (pair? g (vty b))                     ; a 32-bit park against a pair right: widen the park
                                 (: fs (vfs ah + vfs b
                                        + `(`('mov 'r2 (vreg ah)))
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
                                          + `(`('mov 'r0 (vreg b)) `('mov 'r1 (vreg ah)))
                                          + znrw g ta9 tb9 'r1 'r0
                                          + `('(cmp r1 r0) `('br cc9 lbl)))
                                         (vfs ah + vfs b + `(`('cmp (vreg ah) (vreg b)) `('br cc9 lbl))))
                                   _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                   fs)))))))))
       (: rhs (? ((<e = 'bin) && (<>e = '&)) <>>>e ())   ; the mask-test shapes: (v & k) / (k & v)
          lhs (? (two? rhs) <>>e ())
          kr (? (two? rhs) (knum g rhs) ())
          kl (? (two? rhs) (knum g lhs) ())
          try (? (<e = 'call) (cbinl g env e cc lbl)
                 !(two? rhs) 'no
                 (two? kr && immok g 'log <kr) (tbr g env lhs <kr cc lbl)
                 (two? kl && immok g 'log <kl) (tbr g env rhs <kl cc lbl)
                 'no)
          (? !(id? try 'no) try
             (: c (cgexpr g env e)
                (? (bad? c) 'bad
                   (flo? (vty c))
                    ; a bare double condition is d != 0.0, NaN-honest -- the parity dance
                    ; rides the branch pair; arm64's fcmp ne/eq are honest whole
                    (: fs (vfs c + (? (t32? g)               ; the zero pattern zeroes the r1:r2 pair
                                      '((li r1 0) (li r2 0) (movqxr f1 r1) (ucomisd f0 f1))
                                      '((li r1 0) (movqxr f1 r1) (ucomisd f0 f1))))
                       _ (rfree g (vreg c))
                       (? (arm? g) (fs + `(`('br cc lbl)))   ; VCMP/FCMP ne/eq are NaN-honest whole
                          (cc = 'ne) (fs + `(`('br 'p lbl) `('br 'ne lbl)))
                          (: sk (nlab g ".u")
                             _ (vmflush g)
                             (fs + `(`('br 'p sk) `('br 'eq lbl) `('label sk))))))
                   (pair? g (vty c))                     ; pair truth: either word nonzero (v6-M: no r12 dest; r0 is dead past the branch)
                    (vfs c + `(`('ors (? (v6m? g) 'r0 'r12) 'r0 'r1) `('br cc lbl)))
                   (d128? (vty c))                       ; wide truth: either word nonzero
                    (vfs c + '((or r0 r0 r2) (cmp r0 0)) + `(`('br cc lbl)))
                   (: fs (vfs c + `(`('cmp (vreg c) 0) `('br cc lbl)))
                      _ (rfree g (vreg c))
                      fs))))))
   ; n little-endian bytes of a constant (the global initializer image)
   (les n v) (: (go i acc) (? (n <= i) (rev acc)
                              (go (i + 1) ((& (>> v (* 8 i)) 255) >< acc)))
               (go 0 ()))
   (zeros n) (? (n < 1) () (0 >< zeros (n - 1)))
   ; a straight-line block copy of sz bytes, src address in r0, dest in r1,
   ; r2 the scratch -- 8 at a time, then a byte tail (struct = struct copies)
   (bcopy g off sz acc)                              ; `ld`/`st` move one WORD -- 8 bytes, 4 on t32
    (: w (wsize g)
       (? (w <= sz) (bcopy g (off + w) (sz - w)
                     (acc + `(`('ld 'r2 'r0 off) `('st 'r1 off 'r2))))
          (0 < sz) (bcopy g (off + 1) (sz - 1)
                     (acc + `(`('ld1 'r2 'r0 off) `('st1 'r1 off 'r2))))
          acc))
   ; a spill that HOLDS a value across a nested sub-eval (which may itself contain a call).
   ; ⚠ a bare `push` is 8 bytes and would leave rsp at 8 mod 16 while the inner call runs -- a
   ; SysV violation glibc turns into a #GP -- so reserve a whole 16-byte cell.
   (spush r) `('(sub sp sp 16) `('st 'sp 0 r))
   (spop r)  `(`('ld r 'sp 0) '(add sp sp 16))
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

   ; -- the PAIR kit (t32 llong/ullong, moon rung 3): a 64-bit value rides r0 (lo) : r1 (hi),
   ; the AAPCS32 return pair. pairs NEVER enter the operand pool or the immediate lanes --
   ; every pair op runs the shuttle, with r2:r3 the popped left operand and r12 the scratch
   ; (IP, already the borrowed temp). the same 16-byte spill cell holds both words. --
   (pair? g ty) (t32? g && llt? ty)
   (pwide ty) (? (usg? ty) '((li r1 0)) '((mov r1 r0) (sar r1 31)))             ; widen r0 by its SOURCE signedness
   (asll g v) (? (llt? (vty v)) (tor0 g v) (tor0 g v + pwide (vty v)))      ; land a pair on the protocol
   (spop2 ra rb) `(`('ld ra 'sp 0) `('ld rb 'sp 4) '(add sp sp 16))
   ; pair load/store at base+off. a load whose base is r0 loads the HIGH word first (into r1),
   ; so the low load may then overwrite the base. pst's base must never be r0/r1 (use r2/r4/r12).
   (pld base off) (? (id? base 'r0) `(`('ld 'r1 base (off + 4)) `('ld 'r0 base off))
                     `(`('ld 'r0 base off) `('ld 'r1 base (off + 4))))
   (pst base off) `(`('st base off 'r0) `('st base (off + 4) 'r1))
   ; the pair ++/-- tail: old rides r0:r1, the sign-widened step lands in r2:r3,
   ; the stepped pair stores back through base+off (base never r0..r3)
   (ppost base off sv)
    `(`('li 'r2 (sv & 4294967295))
      `('li 'r3 (? (sv < 0) 4294967295 0))
      '(adds r2 r0 r2) '(adc r3 r1 r3)
      `('st base off 'r2) `('st base (off + 4) 'r3))
   ; flags := (la:lb) - (ra:rb), for a NORMALIZED relation (lt/ge; gt/le swap operands first).
   ; eq/ne OR the two word-differences instead (la is dead after its read: it takes the hi diff).
   ; v6-M: no r12-dest ALU, so the differences land IN la:lb -- ⚠ every call site's left pair
   ; must be dead past the compare.
   (pcmpf g op la lb ra rb)
    (? (v6m? g)
       (? (member? op '(eq ne))
          `(`('subs la la ra) `('sbcs lb lb rb) `('ors la la lb))
          `(`('subs la la ra) `('sbcs lb lb rb)))
       (member? op '(eq ne))
       `(`('subs 'r12 la ra) `('sbc la lb rb) `('ors 'r12 'r12 la))
       `(`('subs 'r12 la ra) `('sbcs 'r12 lb rb)))
   ; a pair relation as (cc forms), operands lhs r2:r3 / rhs r0:r1 (the shuttle shape):
   ; gt/le compute the SWAPPED difference and flip to lt/ge; signedness picks lt/below via scc.
   (prel g op unsg)
    (? (member? op '(eq ne)) `(op (pcmpf g op 'r2 'r3 'r0 'r1))
       (member? op '(lt ge)) `((scc op unsg) (pcmpf g op 'r2 'r3 'r0 'r1))
       `((scc (? (op = 'gt) 'lt 'ge) unsg) (pcmpf g op 'r0 'r1 'r2 'r3)))
   ; the pair BRANCH: cbranch's twin of prel -- cc 'ne branches when (la:lb OP ra:rb) holds,
   ; cc 'eq when it does not (the negated relation keeps the same operand order: !lt = ge).
   (pbrf g op unsg la lb ra rb cc lbl)
    (: o2 (? (cc = 'ne) op (negrel op))
       (? (member? o2 '(eq ne)) (pcmpf g o2 la lb ra rb + `(`('br o2 lbl)))
          (member? o2 '(lt ge)) (pcmpf g o2 la lb ra rb + `(`('br (scc o2 unsg) lbl)))
          (pcmpf g o2 ra rb la lb + `(`('br (scc (? (o2 = 'gt) 'lt 'ge) unsg) lbl)))))
   ; a pair shift by a CONSTANT count (kind 'shl/'shr/'sar), value on the protocol; in-place.
   (pshiftc kind c)
    (? (c < 1) ()
       (63 < c) '((li r0 0) (li r1 0))
       (kind = 'shl)
        (? (c = 32) '((mov r1 r0) (li r0 0))
           (32 < c) `('(mov r1 r0) `('shl 'r1 (c - 32)) '(li r0 0))
           `('(mov r2 r0) `('shr 'r2 (32 - c)) `('shl 'r1 c) '(or r1 r1 r2) `('shl 'r0 c)))
       (kind = 'shr)
        (? (c = 32) '((mov r0 r1) (li r1 0))
           (32 < c) `('(mov r0 r1) `('shr 'r0 (c - 32)) '(li r1 0))
           `('(mov r2 r1) `('shl 'r2 (32 - c)) `('shr 'r0 c) '(or r0 r0 r2) `('shr 'r1 c)))
       ; sar: the high word keeps its sign; a >=32 count floods it
       (? (c = 32) '((mov r0 r1) (sar r1 31))
          (32 < c) `('(mov r0 r1) `('sar 'r0 (c - 32)) '(sar r1 31))
          `('(mov r2 r1) `('shl 'r2 (32 - c)) `('shr 'r0 c) '(or r0 r0 r2) `('sar 'r1 c))))
   ; a pair shift by a VARIABLE count in r2 (0..63), value on the protocol. branchy: the big lane
   ; (count >= 32) moves one word across; the small lane composes the carry-over through r12
   ; (⚠ count 0 skips -- a 32-c shift at c=0 would be UNPREDICTABLE).
   ; v6-M: no r12 ALU dest and no free low temp -> libgcc's helper (count in r2, same staging).
   (pshiftv g kind)
    (? (v6m? g)
       `(`('call (? (kind = 'shl) '__aeabi_llsl (kind = 'shr) '__aeabi_llsr '__aeabi_lasr)))
    (: lb (nlab g ".pb") ls (nlab g ".ps") ld (nlab g ".pd")
       _ (vmflush g)
       one (? (kind = 'shl) '((mov r1 r0) (shlv r1 r2) (li r0 0))
              (kind = 'shr) '((mov r0 r1) (shrv r0 r2) (li r1 0))
                            '((mov r0 r1) (sarv r0 r2) (sar r1 31)))
       sm (? (kind = 'shl)
             '((mov r12 r0) (li r3 32) (sub r3 r3 r2) (shrv r12 r3)
                (shlv r1 r2) (or r1 r1 r12) (shlv r0 r2))
             (: hop (? (kind = 'shr) 'shrv 'sarv)
                `('(mov r12 r1) '(li r3 32) '(sub r3 r3 r2) '(shlv r12 r3)
                   '(shrv r0 r2) '(or r0 r0 r12) `(hop 'r1 'r2))))
       (`('(cmp r2 32) `('br 'ae lb) '(cmp r2 0) `('br 'eq ld))
        + sm + `(`('jmp ld) `('label lb) '(sub r2 r2 32))
        + one + `(`('label ld)))))
   ; the 64-bit float bridges (VFP on AArch32 converts only through 32 bits). llong -> double:
   ; hi rides the U-converter and scales by 2^32 (exact), lo adds exactly, ONE rounding total;
   ; the signed face negates the pair in, the double out (INT64_MIN survives: its u64 face IS
   ; 2^63). value r0:r1 -> f0. v6-M: the whole bridge is one libgcc call.
   (cvtlld g sgn)
    (? (v6m? g)
       `(`('call (? sgn '__aeabi_ul2d '__aeabi_l2d)) '(movqxr f0 r0))
    (: lu (nlab g ".lu") ld (nlab g ".ld")
       _ (vmflush g)
       u2d '((cvtui2sd f0 r1)
             (li r2 0) (li r3 1106247680) (movqxr f1 r2)   ; 2^32 (0x41F00000:0)
              (mulsd f0 f1)
              (cvtui2sd f1 r0) (addsd f0 f1))
       (? sgn u2d                                    ; unsigned: straight through
          (`('(cmp r1 0) `('br 'ge lu)
              '(li r2 0) '(li r3 0) '(subs r0 r2 r0) '(sbc r1 r3 r1))
           + u2d
           + `('(li r2 0) '(li r3 0) '(movqxr f1 r2)           ; 0.0 - d
              '(subsd f1 f0) '(movsd f0 f1) `('jmp ld) `('label lu))
           + u2d + `(`('label ld))))))
   ; double -> llong, toward zero: peel sign/exponent off the PATTERN, rebuild the 53-bit integer
   ; mantissa as a pair, shift it into place, refix the sign. |d| < 1 answers 0; out-of-range is
   ; UB and shifts to garbage like the hardware would. v6-M: one libgcc call (the peel itself is
   ; sign-blind bit arithmetic -- only the libcall face needs the target's signedness).
   (cvtdll g usg)
    (? (v6m? g)
       `('(movqrx r0 f0) `('call (? usg '__aeabi_d2ulz '__aeabi_d2lz)))
    (: l1 (nlab g ".d1") lr (nlab g ".dr") ls (nlab g ".ds") ld (nlab g ".dd")
       _ (vmflush g)
       (`('(movqrx r0 f0)
           '(mov r2 r1) '(shr r2 31) '(sub sp sp 16) '(st sp 0 r2)   ; park the sign
           '(mov r2 r1) '(shr r2 20) '(li r3 2047) '(and r2 r2 r3)   ; e
           '(li r3 1048575) '(and r1 r1 r3)
           '(li r3 1048576) '(or r1 r1 r3)                                ; mantissa | implicit bit
           '(li r3 1023) '(cmp r2 r3) `('br 'ge l1)
           '(li r0 0) '(li r1 0) '(add sp sp 16) `('jmp ld)              ; |d| < 1 -> 0
           `('label l1)
           '(sub r2 r2 1075) '(cmp r2 0) `('br 'lt lr))
        + pshiftv g 'shl
        + `(`('jmp ls) `('label lr) '(li r3 0) '(sub r2 r3 r2))
        + pshiftv g 'shr
        + `(`('label ls) '(ld r2 sp 0) '(add sp sp 16)
           '(cmp r2 0) `('br 'eq ld)
           '(li r2 0) '(li r3 0) '(subs r0 r2 r0) '(sbc r1 r3 r1)
           `('label ld)))))

   ; -- the WIDE kit (i128/u128, x64 only): a 128-bit value rides hi r2 : lo r0 (rdx:rax --
   ; mulq's own pair and the SysV i128 return), ONLY on the r0 protocol -- never the operand
   ; pool, never an immediate lane, never a home (homable? excludes it, so the universes do
   ; too). a binop's left operand parks in ONE 16-byte machine-stack cell ([sp] lo / [sp+8]
   ; hi); the carrying ops (a128/s128/m128/mw128/c128) are single holo forms reading that
   ; cell, so no pass can part a flags pair, and their 'sp operand keeps deadcell honest
   ; (rdsp answers 'bar -- every pass conservative). off x64 the guards answer 'bad: the
   ; driver defines __SIZEOF_INT128__ for x64 alone, so love.c meets this lane only there. --
   (d128? ty) (ty = 'i128 || ty = 'u128)
   (x64t? g) !(arm? g)
   (wbin ta tb) (? (d128? ta && d128? tb) (? (id? ta 'u128 || id? tb 'u128) 'u128 'i128)
                   (d128? ta) ta tb)                ; a word side converts to the wide side's type
   ; land a value on the wide protocol: a pair rides; a word widens by its SOURCE signedness
   (as128 g v) (? (d128? (vty v)) (tor0 g v)
                  (tor0 g v + (? (usg? (vty v) || ptr? (vty v)) '((li r2 0)) '((sx128)))))
   ; did this value's forms end in the zero-extend? (the widening-mul license: hi is 0)
   (zext128? fs) (: rl (rev fs) (two? rl && '(li r2 0) = <rl))
   ; a fresh n-byte frame cell -> its (negative) offset. bumps the frame's
   ; high-water like a local; ntmp sizes one for a type (a compound literal's home).
   (nslot g n) (: off (0 - (peep g 'off 0 + n))
                  _ (pin g 'off (peep g 'off 0 + n))
                  off)
   (ntmp g ty) (nslot g $ (tsz g ty + 7) & -8)
   ; the store-immediate op for (ty, k) | () -- the vi fold's license, shared by the
   ; asn/decl delivery lanes (x64 only: si is C6/C7). st sign-extends imm32, the
   ; narrow forms truncate like their register twins.
   (siop9 ty k)
    (: s (stop ty)
       (? (s = 'st) (? (fits32? k) 'si ())
          (s = 'st4) (? ((-2147483648 <= k) && (k <= 4294967295)) 'si4 ())
          (s = 'st2) (? ((-32768 <= k) && (k <= 65535)) 'si2 ())
          (s = 'st1) (? ((-128 <= k) && (k <= 255)) 'si1 ())
          ()))
   ; load a value from the address in r0, by type -- the dot/deref/clit value tail
   ; (arrays and functions decay to the address; a struct has no register value). the scalar
   ; tail honors the want hint: the load lands straight in the hinted register, so a decl/asn
   ; hint holds and the vmap pin fires (the wide/float/aggregate lanes keep their protocols)
   (loadval g ty addrf W9)
    (? (arr? ty) (mkv `('ptr <>ty) 'r0 addrf)
       (gvarr? ty) (mkv `('ptr (varel ty)) 'r0 addrf)   ; a VLA row decays to its address (p[i] a char*)
       (fn? ty) (mkv `('ptr ty) 'r0 addrf)
       (stru? ty) (mkv ty 'r0 addrf)                 ; a struct value IS its address (the pair reps read through it)
       (flo? ty) (mkv 'double 'r0 (addrf + fldf ty 'f0 'r0 0))
       (pair? g ty) (mkv ty 'r0 (addrf + pld 'r0 0))
       (d128? ty) (mkv ty 'r0 (addrf + '((ld r2 r0 8) (ld r0 r0 0))))   ; hi first: the low load overwrites the base
       (: W (? W9 W9 'r0)
          (mkv ty W (addrf + `(`((ldop ty) W 'r0 0))))))
   ; the VALUE at an lvalue rec: a bitfield loads its unit and extracts with two
   ; shifts (sar signed, shr unsigned), anything else loads flat.
   (lvload g r wnt)
    (: ty (vty r)
       (? (bitf? ty)
           (: uty <>ty bitoff <>>ty width <>>>ty
              usz (tsz g uty) hi (64 - (bitoff + width)) lo (64 - width)
              sgn !(usg? uty || id? uty 'bool)         ; a bool field holds 0/1: never sign-extend
              (mkv 'long 'r0
                   (vfs r
                    + `(`((uldop usz) 'r0 'r0 0))
                    + (? (hi > 0) `(`('shl 'r0 hi)) ())
                    + (? (lo > 0) `(`((? sgn 'sar 'shr) 'r0 lo)) ()))))
          (loadval g ty (vfs r) wnt)))
   ; the fill code for a compound literal at frame offset off: an aggregate zeroes
   ; then fills (cgfill), a scalar unwraps its single-element brace and stores.
   (clitcode g env off ty ini)
    (? (arr? ty || stru? ty)
        (: f (cgfill g env off ty ini)
           (? (bad? f) 'bad (cgzero off ((tsz g ty + 7) & -8) + f)))
       (: v (? (two? ini && <ini = 'init)
               (? (two? <>ini) <<>ini '(num 0)) ini)
          (cgstore g env off ty v)))

   ; -- lvalues: the ADDRESS lands in r0; answers (pointee-ty forms) | 'bad --
   (clval g env e)
    (: k <e
       (? (k = 'var)
           (: en (vent g env <>e)
              (? (bad? en) 'bad
                 (member? <en '(regv regw)) 'bad   ; a register home has no address (& is gated out per-fn)
                 ; lean, not lea: an lvalue's address is CONSUMED by its own load/store, so the
                 ; sibcall escape gate must not see it (the & lane flips it back -- it escapes there)
                 (<en = 'loc) (mkv <>>en 'r0 `(`('lean 'r0 'r4 <>en)))
                 (mkv <>>en 'r0 `(`('la 'r0 <>en)))))
          (k = 'deref)
           (: r (cgexpr g env <>e)
              (? (bad? r) 'bad
                 !(ptr? (vty r)) 'bad
                 (mkv <>(vty r) 'r0 (tor0 g r))))
          (k = 'dot)
           (: r (: lv (clval g env <>e)
                   ; an rvalue struct base (a call's parked temp: `mk(x).a`) is
                   ; its address too -- same license the asn lane rides
                   (? !(bad? lv) lv
                      (: rv (cgexpr g env <>e)
                         (? (bad? rv) 'bad !(stru? (vty rv)) 'bad rv))))
              (? (bad? r) 'bad
                 (: fm (fmem g (vty r) <>>e)
                    (? (bad? fm) 'bad
                       (mkv <>fm 'r0
                            (vfs r + (? (<fm = 0) () `(`('add 'r0 'r0 <fm)))))))))
          (k = 'clit)                               ; a compound literal is an lvalue: fill a temp, address it
           (: ty <>e ini <>>e
              off (ntmp g ty)
              fill (clitcode g env off ty ini)
              (? (bad? fill) 'bad
                 (mkv ty 'r0 (fill + `(`('lea 'r0 'r4 off))))))   ; a real lea: the temp's address is the value (& pins via the addr lane, a call blocks via the window)
          ; gen's OWN lvalue, minted by the rmw lane below and reachable from no
          ; source: the object whose address is moored at frame offset <>e. reading
          ; it is calm, so it stands in for an lvalue that must evaluate once.
          (k = 'moor) (mkv <>>e 'r0 `(`('ld 'r0 'r4 <>e)))
          'bad))

   ; -- the gcc builtins love.c reaches -- x64 as short raw splices, arm64 as neutral IR or one
   ; raw word (clz): trap = ud2 / brk; inf/nanf = IEEE constants into f0; expect = its value (the
   ; hint drops); clear_cache = nothing on x86; clzll = bsr+xor / clz; isinf = bits<<1 == inf<<1;
   ; the overflow trio = the op + the overflow flag (seto / set vs, mul via mulo's smulh check),
   ; result stored through arg3, the flag the value.
   (cgbuiltin g env bi args)                       ; a builtin call -> its value | 'bad; not one -> 'no
    (? (bi = "__builtin_trap") (? (arm? g) (mkv 'long 'r0 '((trap)))
                                  (mkv 'long 'r0 '((ud2))))     ; the named op, not raw bytes: a raw bars unframe in every splice site
       (bi = "__builtin_inf")
        (? (t32? g) (mkv 'double 'r0 '((li r0 0) (li r1 2146435072) (movqxr f0 r0)))
           (mkv 'double 'r0 '((li r0 9218868437227405312) (movqxr f0 r0))))
       (bi = "__builtin_nanf" || bi = "__builtin_nan")   ; both take a (ignored) tag string; one quiet-NaN pattern
        (? (t32? g) (mkv 'double 'r0 '((li r0 0) (li r1 2146959360) (movqxr f0 r0)))
           (mkv 'double 'r0 '((li r0 9221120237041090560) (movqxr f0 r0))))
       (bi = "__builtin_expect") (cgexpr g env <args)
       (bi = "__builtin___clear_cache")
        ; x86 keeps I-cache coherent with D-cache: nothing to do. arm64 does NOT (qemu-user is
        ; coherent, metal is not), so the architectural sequence: dc cvau each D-line, dsb, ic
        ; ivau each I-line, dsb+isb, strides read off CTR_EL0 (DminLine[19:16] / IminLine[3:0],
        ; log2 words). the system insns ride raw words, and the variable stride shift too.
        ; ⚠ scratch = r2/r3/r9/r15 ONLY -- NEVER r10-r14, the a64 param/local HOMES, resident
        ; across the whole fn and spilled only around real calls.
        (? !(a64? g) (mkv 'long 'r0 ())
           (: e1 (cgexpr g env <args)
              e2 (cgexpr g env <>args)
              _ (vmflush g)                 ; labels join + the splice clobbers pool regs
              lb (nlab g ".ccd") db (nlab g ".ccdd")
              li9 (nlab g ".cci") di (nlab g ".ccid")
              (? (bad? e1 || bad? e2) 'bad
                 (mkv 'long 'r0 $
                   asint g e1 + spush 'r0 + asint g e2 + '((mov r1 r0)) + spop 'r0 +
                     `('(raw (34 0 59 213))          ; mrs x2, ctr_el0
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
                       `('label lb)
                       '(cmp r9 r1)
                       `('br 'ae db)
                       '(raw (41 123 11 213))       ; dc cvau, x9
                       '(add r9 r9 r2)
                       `('jmp lb)
                       `('label db)
                       '(raw (159 59 3 213))        ; dsb ish
                       '(sub r3 r15 1)
                       '(not r3)
                       '(and r9 r0 r3)            ; begin & ~(isize-1)
                       `('label li9)
                       '(cmp r9 r1)
                       `('br 'ae di)
                       '(raw (41 117 11 213))         ; ic ivau, x9
                       '(add r9 r9 r15)
                       `('jmp li9)
                       `('label di)
                       '(raw (159 59 3 213))          ; dsb ish
                       '(raw (223 63 3 213)))))))     ; isb
       (member? bi '("__builtin_clzll" "__builtin_clz"))
        (: e1 (cgexpr g env <args)
           ll (bi = "__builtin_clzll")
           pre (? ll () '((shl r0 32)))    ; clz32(x) = clz64(x << 32) -- the shift also clears upper garbage
           (? (bad? e1) 'bad
              (t32? g && !ll)              ; clz32 is one word: CLZ / libgcc's __clzsi2
               (mkv 'long 'r0 (asint g e1 +
                  (? (v6m? g) '((call __clzsi2)) '((clz r0 r0)))))
              (t32? g)                     ; the pair: clz(hi), or 32 + clz(lo) when hi is 0
               (: lz (nlab g ".cz") ld (nlab g ".czd")
                  _ (vmflush g)
                  (mkv 'long 'r0 (asll g e1 +
                    (? (v6m? g)   ; no CLZ on v6-M: libgcc's __clzsi2 (arg/result r0)
                       `('(cmp r1 0) `('br 'eq lz) '(mov r0 r1) '(call __clzsi2) `('jmp ld)
                          `('label lz) '(call __clzsi2) '(add r0 r0 32) `('label ld))
                       `('(cmp r1 0) `('br 'eq lz) '(clz r0 r1) `('jmp ld)
                          `('label lz) '(clz r0 r0) '(add r0 r0 32) `('label ld))))))
              (rv? g)                      ; no clz in RV64I: the 6-stage binary search (branchy; clz(0) stays UB)
               (: ls (map (\ k (nlab g ".cz")) '(0 1 2 3 4 5))
                  _ (vmflush g)
                  step (\ kk lb2 `('(mov r2 r0) `('shr 'r2 (64 - kk)) '(cmp r2 0) `('br 'ne lb2)
                                   `('add 'r1 'r1 kk) `('shl 'r0 kk) `('label lb2)))
                  (mkv 'long 'r0 (asint g e1 + pre +
                    ('((li r1 0))
                     + step 32 (nthr ls 0) + step 16 (nthr ls 1) + step 8 (nthr ls 2)
                     + step 4 (nthr ls 3) + step 2 (nthr ls 4) + step 1 (nthr ls 5)
                     + '((mov r0 r1))))))
              (mkv 'long 'r0 (asint g e1 + pre +
                              (? (a64? g) '((raw (0 16 192 218)))                    ; clz x0, x0
                                 '((raw (72 15 189 192 72 131 240 63))))))))   ; bsr rax,rax; xor rax,63
       ; count trailing zeros, 32-bit (dlmalloc's compute_bit2idx; ctz(0) stays UB;
       ; upper garbage is harmless -- the defined case has a set bit in the low 32):
       ; x64 bsf, a64 rbit+clz, rv a mask-narrowing search, t32 isolate-and-clz
       (bi = "__builtin_ctz")
        (: e1 (cgexpr g env <args)
           (? (bad? e1) 'bad
              (t32? g)
               (mkv 'long 'r0 (asint g e1 +
                  (? (v6m? g) '((call __ctzsi2))
                     '((li r1 0) (sub r1 r1 r0) (and r0 r0 r1)     ; x & -x, then 31 - clz
                        (clz r0 r0) (li r1 31) (sub r0 r1 r0)))))
              (rv? g)
               (: ls (map (\ k (nlab g ".tz")) '(0 1 2 3 4 5))
                  _ (vmflush g)
                  step (\ kk mk lb2 `(`('li 'r3 mk) '(mov r2 r0) '(and r2 r2 r3) '(cmp r2 0)
                                      `('br 'ne lb2) `('add 'r1 'r1 kk) `('shr 'r0 kk) `('label lb2)))
                  (mkv 'long 'r0 (asint g e1 +
                    ('((li r1 0))
                     + step 32 4294967295 (nthr ls 0) + step 16 65535 (nthr ls 1)
                     + step 8 255 (nthr ls 2) + step 4 15 (nthr ls 3)
                     + step 2 3 (nthr ls 4) + step 1 1 (nthr ls 5)
                     + '((mov r0 r1))))))
              (mkv 'long 'r0 (asint g e1 +
                              (? (a64? g) '((raw (0 0 192 90 0 16 192 90)))   ; rbit w0,w0; clz w0,w0
                                 '((raw (72 15 188 192))))))))                ; bsf rax,rax
       ; the bswap trio (glibc's <byteswap.h> inlines): fully-masked neutral shift/or
       ; expansions -- no raw (unframe stays alive), no new ops. 16/32 are width-blind
       ; and ride every target; 64 is the three-stage swap (bytes, halves, words) and
       ; falls to the undeclared refusal on t32 (a pair lane with no taker yet)
       (member? bi '("__builtin_bswap16" "__builtin_bswap32" "__builtin_bswap64"))
        (? ((bi = "__builtin_bswap64") && t32? g)
            (nolane g ("no lane for __builtin_bswap64" + ontgt g))   ; the pair lane has no taker; 'no would emit a call the link can't bind
           (: e1 (cgexpr g env <args)
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
       ; the __sync spin-lock pair (dlmalloc's CAS_LOCK/CLEAR_LOCK): test_and_set is
       ; the atomic exchange with acquire -- x64 xchg (implicitly locked), a64 the
       ; ldaxr/stxr retry, rv amoswap.{w,d}.aq -- and release a release-ordered store
       ; of 0 (x64 TSO plain store, a64 stlr, rv fence rw,w + store). the POINTEE
       ; sizes and signs the exchange (4/8 bytes; the old value re-extends via cvt),
       ; and an unsized pointee refuses -- guessing a width miscompiles in silence
       (member? bi '("__sync_lock_test_and_set" "__sync_lock_release"))
        (: e1 (cgexpr g env <args)
           pt (? (two? (vty e1) && (<(vty e1) = 'ptr)) <>(vty e1) ())
           w (? (member? pt '(int uint)) 4
                (ptr? pt || member? pt '(long ulong)) 8 ())
           (? (t32? g) (nolane g (("no lane for " + bi) + ontgt g))
              (bad? e1) 'bad
              !w (nolane g (bi + " wants an int* or a long*/T** (the pointee sizes the exchange)"))
              (bi = "__sync_lock_release")
               (mkv 'long 'r0 (tor0 g e1 +
                 '((mov r1 r0) (li r0 0)) +
                 (? (a64? g) `(`('raw (? (w = 4) '(32 252 159 136) '(32 252 159 200))))   ; stlr w0/x0, [x1]
                    (rv? g) ('((raw (15 0 16 3))) +                                       ; fence rw,w
                             `(`((? (w = 4) 'st4 'st) 'r1 0 'r0)))
                    `(`((? (w = 4) 'st4 'st) 'r1 0 'r0)))))
              (: e2 (cgexpr g env <>args)
                 (? (bad? e2) 'bad
                    (: pre (tor0 g e1 + spush 'r0 + asint g e2 + spop 'r1)   ; val r0, ptr r1
                       body (? (a64? g)
                               (: lb (nlab g ".ts") _ (vmflush g)
                                  `(`('label lb)
                                     `('raw (? (w = 4) '(34 252 95 136) '(34 252 95 200)))   ; ldaxr w2/x2, [x1]
                                     `('raw (? (w = 4) '(32 124 3 136) '(32 124 3 200)))     ; stxr w3, w0/x0, [x1]
                                     '(cmp r3 0) `('br 'ne lb) '(mov r0 r2)))
                              (rv? g)
                               `(`('raw (? (w = 4) '(47 165 165 12) '(47 181 165 12))))      ; amoswap.w/d.aq a0,a0,(a1)
                              `(`('raw (? (w = 4) '(135 1) '(72 135 1)))))                   ; xchg eax/rax, (rcx)
                       (mkv pt 'r0 (pre + body + cvt pt 'r0)))))))
       (bi = "__builtin_isinf")
        (: e1 (cgexpr g env <args)
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
        (: e1 (cgexpr g env <args)
           e2 (cgexpr g env <>args)
           ; the &t arg is consumed by the splice's own store -- argpos,
           ; like a call arg (else every overflow user loses sibcalls)
           e3 (: _ (? (member? <<>>args '(addr var)) (pin g 'argpos 1) ())
                 (cgexpr g env <>>args))
           _ (pin g 'argpos ())
           opb (? (bi = "__builtin_add_overflow") '(72 1 200)      ; add rax,rcx
                  (bi = "__builtin_sub_overflow") '(72 41 200)     ; sub rax,rcx
                  '(72 15 175 193))                                ; imul rax,rcx
           ; the op leaves r0 the result, r1 the overflow bit
           ovf (? !(arm? g)
                  `(`('raw (opb + '(15 144 193 72 15 182 201))))          ; op; seto cl; movzx rcx,cl
                  !(bi = "__builtin_mul_overflow")
                   `(`((? (bi = "__builtin_add_overflow") 'adds 'subs) 'r0 'r0 'r1) '(set vs r1))
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
                     `(`('mulo 'r0 'r0 'r1 'r2 lo) '(li r1 0) `('jmp l2)
                        `('label lo) '(li r1 1) `('label l2))))
           ; ⚠ &t's frame lea must not open the sibs WINDOW: the x64 raw splice resets it as a
           ; call boundary, but arm64's neutral IR is no boundary, and an open window silently
           ; declines every following tail. the address is consumed by the splice's own store,
           ; so it reads lean here; x64 IR stays untouched (byte-identity).
           e3f (? (arm? g) (relean (tor0 g e3)) (tor0 g e3))
           _ (vmflush g)                    ; the a64 mulo lane's labels join
           (? (bad? e1 || bad? e2 || bad? e3) 'bad
              (mkv 'long 'r0 $
               asint g e1 + spush 'r0 + asint g e2 + spush 'r0 +
               e3f + '((mov r3 r0)) + spop 'r1 + spop 'r0 + ovf +
               '((st r3 0 r0) (mov r0 r1)))))
       'no)

   ; -- expressions: answers a value tuple | 'bad. g 'want is the DESTINATION HINT, pinned by a
   ; consumer that knows where the value must land (an asn into a home, a ret's r0), consumed by
   ; the TOP node only (cleared here, so subexpressions never inherit it), and always ADVISORY --
   ; a lane that ignores it answers its usual register and the consumer bridges. --
   (cgexpr g env e)
    (: wnt (peep g 'want ())
       _ (pin g 'want ())
       k <e
       (? (k = 'szof)                                   ; sizeof EXPR: the UNDECAYED type (clval keeps arrays), size a constant
           (: e2 <>e
              lv (clval g env e2)
              ty (? (bad? lv) (: r (cgexpr g env e2) (? (bad? r) 'bad (: _ (rfree g (vreg r)) (vty r)))) (vty lv))
              (? (bad? ty) 'bad
                 (gvarr? ty && 3 < tally ty)   ; sizeof a VLA local: the stashed byte size
                  (: sl (peep (g 'vslots) (varix ty) ())
                     (? !(two? sl) 'bad (mkv 'ulong 'r0 `(`('ld 'r0 'r4 <>sl)))))
                 (mkv 'ulong 'r0 `(`('li 'r0 (tsz g ty))))))
          (k = 'num)
           (? (t32? g && !(-2147483648 <= <>e && <>e <= 4294967295))
              ; a literal past the 32-bit word: the pair (C99 decimal picks long long).
              ; split by // and % -- a big literal has no bitwise ops (the imnum lesson)
              (: u (? (<>e < 0) (<>e + 18446744073709551616) <>e)
                 (mkv (? (<>e < 0) 'llong 'ullong) 'r0
                      `(`('li 'r0 (u % 4294967296)) `('li 'r1 (u // 4294967296)))))
              (t32? g && 2147483647 < <>e)
              ; bit 31 set on the 32-bit word: UNSIGNED (0xffffffff reads uint), so a
              ; pair-widen zero-extends -- a signed 'long here sign-smeared the cast
              (: W (? wnt wnt 'r0) (mkv 'ulong W `(`('li W <>e))))
              (x64t? g && !(-9223372036854775808 <= <>e && <>e <= 18446744073709551615))
              ; a literal past the 64-bit word (cfold's exact u128 folds): the wide pair,
              ; split by // and % -- a big literal has no bitwise ops (the imnum lesson)
              (: u ((? (<>e < 0) (<>e + (* (64 2) (64 2))) <>e) % (64 2 * 64 2))
                 (mkv 'u128 'r0
                      `(`('li 'r0 (u % 18446744073709551616))
                        `('li 'r2 (u // 18446744073709551616)))))
              (: W (? wnt wnt 'r0) (mkv 'long W `(`('li W <>e)))))
          (k = 'flo)
           (? (t32? g)                                  ; the 64-bit pattern splits over the r0:r1 pair
              (: v (fbits <>e)
                 (mkv 'double 'r0 `(`('li 'r0 (v & 4294967295)) `('li 'r1 ((v >> 32) & 4294967295))
                                     '(movqxr f0 r0))))
              (mkv 'double 'r0 `(`('li 'r0 (fbits <>e)) '(movqxr f0 r0))))
          (k = 'str)
           (: lbl (nlab g ".S")
              _ (pin g 'strs (`(lbl <>e) >< g 'strs))
              (mkv '(ptr char) 'r0 `(`('la 'r0 lbl))))
          (member? k '(dot moor))
           (: r (clval g env e)
              (? (bad? r) 'bad (lvload g r wnt)))
          (k = 'var)
           (: en (vent g env <>e)
              (? (bad? en)
                  (? (peep (g 'fns) <>e ())     ; a function name is its address
                     (mkv `('ptr (fnty g <>e)) 'r0 `(`('la 'r0 (intern <>e))))
                     (: _ (pin g 'undecl <>e) 'bad))   ; undeclared: name it for the gripe (rung 1); an undeclared CALL funnels here too (call-fixed evaluates its head)
                 (<en = 'regv) (mkv <>>en <>en ())   ; a homed param IS its register: zero forms, borrowed (read-only)
                 (<en = 'regw)                       ; a pair-homed d128 materializes onto the protocol (hi first)
                  (mkv <>>en 'r0 `(`('mov 'r2 <><>en) `('mov 'r0 <<>en)))
                 (: ty <>>en
                    ap? (peep g 'argpos ())
                    _ (? (stru? ty || arr? ty) (pin g 'argpos ()) ())
                    (? (stru? ty)                    ; a struct value is its ADDRESS (the sse2 rep)
                        (? (<en = 'loc)
                           (: v (mkv ty 'r0 `(`('lea 'r0 'r4 <>en))) (? ap? v (escan g v)))
                           (mkv ty 'r0 `(`('la 'r0 <>en))))
                       (arr? ty)                     ; arrays decay to their address
                        (? (<en = 'loc)
                           (: v (mkv `('ptr <>ty) 'r0 `(`('lea 'r0 'r4 <>en))) (? ap? v (escan g v)))
                           (mkv `('ptr <>ty) 'r0 `(`('la 'r0 <>en))))
                       (gvarr? ty)                   ; a VLA local: its slot HOLDS the block pointer
                        (? (<en = 'loc)
                           (mkv `('ptr (varel ty)) 'r0 `(`('ld 'r0 'r4 <>en)))
                           'bad)
                       (flo? ty)                     ; a float/double loads into f0 (widened if float)
                        (? (<en = 'loc)
                           (mkv 'double 'r0 (fldf ty 'f0 'r4 <>en))
                           (mkv 'double 'r0 (`(`('la 'r1 <>en)) + fldf ty 'f0 'r1 0)))
                       (pair? g ty)                    ; a pair local/global reads both words (the address temp
                        (? (<en = 'loc)                ;  dodges r1 -- it is the pair's own high half)
                           (mkv ty 'r0 (pld 'r4 <>en))
                           (mkv ty 'r0 (`(`('la 'r2 <>en)) + pld 'r2 0)))
                       (d128? ty)                      ; a wide local/global reads both words (r1 the
                        (? !(x64t? g) 'bad             ;  address temp -- clear of the pair's own halves)
                           (<en = 'loc)
                            (mkv ty 'r0 `(`('ld 'r0 'r4 <>en) `('ld 'r2 'r4 (<>en + 8))))
                           (mkv ty 'r0 `(`('la 'r1 <>en) '(ld r0 r1 0) '(ld r2 r1 8))))
                       (? (<en = 'loc)
                          (: rM (vmget g <>e)          ; a mapped local reads from its pinned register: ZERO forms
                             (? rM (mkv ty rM ())
                                (: W (? wnt wnt 'r0)
                                   (mkv ty W `(`((ldop ty) W 'r4 <>en))))))
                          (mkv ty 'r0 `(`('la 'r1 <>en) `((ldop ty) 'r0 'r1 0))))))))
          (k = 'addr)
           (: r (clval g env <>e)
              ap? (peep g 'argpos ())
              _ (pin g 'argpos ())
              (? (!(bad? r) && bitf? (vty r)) 'bad   ; &bitfield is illegal C -- refuse
                 !(bad? r)                        ; under & the lvalue's address ESCAPES: flip its leans to leas
                 (: v (mkv `('ptr (vty r)) 'r0 (unlean (vfs r))) (? ap? v (escan g v)))
                 ; &f on a function name: same address as the bare use (no lvalue).
                 ; a VARIABLE of the same name shadows it -- a homed param's regv
                 ; lands here as 'bad and must DEOPT, not read &libm-exp (vent!)
                 (<<>e = 'var && bad? (vent g env <><>e) && peep (g 'fns) <><>e ())
                  (mkv `('ptr (fnty g <><>e)) 'r0 `(`('la 'r0 (intern <><>e))))
                 'bad))
          (k = 'deref)
           ; the array leg: a pinned element reads as ZERO forms; a miss loads into a
           ; pinnable register (the consumer's want if it pools, else a fresh one) and
           ; pins, so the NEXT read is free. pin refused (splice, staging) -> today's path.
           (: ae (aelem g env e)
              (? (two? ae && id? <ae 'el)
                  (: rM (vmget g <>ae)
                     (? rM (mkv <>>ae rM ())
                        (: pino (vmon? g && !(0 < peep g 'argseal 0))
                           W (? wnt wnt (? pino (ralloc g) ()))
                           r (clval g env e)
                           (? (bad? r) (: _ (? (W && !wnt) (rfree g W) ()) 'bad)
                              (: v (loadval g (vty r) (vfs r) W)
                                 _ (? (W && id? (vreg v) W) (vapin g <>ae W) ())
                                 _ (? (W && !wnt && !(member? W (peep g 'vpin ())))
                                      (rfree g W) ())
                                 v)))))
                 (: r (clval g env e)                ; clval already walked the pointer
                    (? (bad? r) 'bad (loadval g (vty r) (vfs r) wnt)))))
          (k = 'asn)
           (: rv (? (<<>e = 'var) (vent g env <><>e) 'bad)
              (? (two? rv && <rv = 'regv)   ; store into a register-homed scalar (param or local)
                  (: _ (pin g 'want <>rv)         ; hint the rhs straight at the home
                     r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (flo? (vty r)) (mkv <>>rv 'r0 (asintt g r <>>rv + (`(`('mov <>rv 'r0)) + cvt <>>rv <>rv)))
                        (id? (vreg r) <>rv) (mkv <>>rv <>rv ((vfs r) + cvt <>>rv <>rv))   ; landed in the home already (re-narrowed)
                        (: fs (vfs r + (`(`('mov <>rv (vreg r))) + cvt <>>rv <>rv))   ; straight from the value's register
                           _ (rfree g (vreg r))
                           (mkv <>>rv <>rv fs))))
                 (two? rv && <rv = 'regw)     ; store into a pair-homed d128: land on the protocol, mov both halves
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (mkv <>>rv 'r0 (as128 g r
                                        + `(`('mov <<>rv 'r0) `('mov <><>rv 'r2))))))
                 ; STORE-DIRECT: a plain variable target's address is STATIC (a frame offset or
                 ; a label) -- the value stores from wherever it sits, no shuttle around it
                 (two? rv && member? <rv '(loc glo) &&
                     (: ty9 <>>rv !(stru? ty9 || arr? ty9 || flo? ty9)))
                  (: ty <>>rv
                     nm <><>e
                     ; the vmap hint: aim the rhs at a fresh pool register so the
                     ; stored value stays RESIDENT (an unhonored hint goes back)
                     hint (? (vmon? g && id? <rv 'loc &&
                               (homable? ty && member? nm (peep g 'vuniv ())))
                             (ralloc g) ())
                     _ (? hint (pin g 'want hint) ())
                     r (cgexpr g env <>>e)
                     _ (? (hint && (bad? r || !(id? (vreg r) hint))) (rfree g hint) ())
                     _ (vmdrop g nm)                  ; the write retires the old mapping
                     (? (bad? r) 'bad
                        (pair? g ty)                     ; a pair target stores both words (r2 the address
                         (mkv ty 'r0                    ;  temp -- r1 is the pair's own high half)
                              (asll g r
                               + (? (<rv = 'loc) (pst 'r4 <>rv)
                                    (`(`('la 'r2 <>rv)) + pst 'r2 0))))
                        (d128? ty)                       ; a wide target stores both words (r1 the address temp)
                         (? !(x64t? g) 'bad
                            (mkv ty 'r0
                                 (as128 g r
                                  + (? (<rv = 'loc) `(`('st 'r4 <>rv 'r0) `('st 'r4 (<>rv + 8) 'r2))
                                       `(`('la 'r1 <>rv) '(st r1 0 r0) '(st r1 8 r2))))))
                        ; ⚠ A DOUBLE SOURCE MUST TRUNCATE FIRST: this lane's guard rules out a
                        ; float TARGET but says nothing about the float SOURCE, and a double's
                        ; value lives in f0 with its reg field not consulted -- so the store-the-
                        ; register lanes below would put the raw IEEE BIT PATTERN in the slot,
                        ; and the narrow lane rides tor0, which moves rather than converts.
                        (flo? (vty r))
                         (mkv ty 'r0 (asintt g r ty + cvt ty 'r0
                                      + (? (<rv = 'loc)
                                           `(`((stop ty) 'r4 <>rv 'r0))
                                           `(`('la 'r1 <>rv) `((stop ty) 'r1 0 'r0)))))
                        (two? (cvt ty 'r0))              ; a narrow target converts on the protocol
                         (mkv ty 'r0 (tor0 g r + cvt ty 'r0 +
                                      (? (<rv = 'loc)
                                         `(`((stop ty) 'r4 <>rv 'r0))
                                         `(`('la 'r1 <>rv) `((stop ty) 'r1 0 'r0)))))
                        (<rv = 'loc)
                         (: _ (? (member? (vreg r) (peep g 'pool0 ()))
                                 (vmpin g nm (vreg r)) ())    ; the stored value stays mapped
                            (mkv ty (vreg r) (vfs r + `(`('st 'r4 <>rv (vreg r))))))
                        (mkv ty (vreg r) (vfs r + `(`('la 'r1 <>rv) `('st 'r1 0 (vreg r)))))))
                 (: lv (clval g env <>e)
              (? (bad? lv) 'bad
                 (arr? (vty lv)) 'bad                ; an array is not assignable
                 (stru? (vty lv))                    ; a whole-struct copy (dest r1, src r0)
                  (: rl (clval g env <>>e)      ; an rvalue struct (a call's temp) is its address too
                     rv (? (bad? rl) (cgexpr g env <>>e) rl)
                     (? (bad? rv) 'bad
                        ; a REAL into a complex lvalue converts: re the value, im zero
                        (gcplx? (vty lv) && !(stru? (vty rv)))
                         (: b (cxb (vty lv))
                            (mkv (vty lv) 'r0
                               (vfs lv + spush 'r0 +
                                (asflo g rv + spop 'r1 + fstf g b 'r1 0 'f0
                                 + '((li r2 0))
                                 + (? (id? b 'float) '((st4 r1 4 r2)) '((st r1 8 r2)))))))
                        !(stru? (vty rv)) 'bad
                        (mkv (vty lv) 'r0 $
                           vfs lv + spush 'r0 + tor0 g rv + spop 'r1 +
                             ; a complex width flip converts componentwise; else bytes
                             (? (gcplx? (vty lv) && gcplx? (vty rv) &&
                                 !(<>(vty lv) = <>(vty rv)))
                                (cxcopy g (vty rv))
                                (bcopy g 0 (tsz g (vty lv)) ())))))
                 (flo? (vty lv))                     ; a float/double store (addr in r1, value in f0; narrow if float)
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (mkv 'double 'r0 (vfs lv + spush 'r0 +
                         (asflo g r + spop 'r1 + fstf g (vty lv) 'r1 0 'f0)))))
                 (bitf? (vty lv))                    ; a bitfield store: unit addr in r1, read-modify-write, result (truncated) in r0
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (: bt (vty lv) uty <>bt bitoff <>>bt width <>>>bt
                           usz (tsz g uty)
                           mask ((<< 1 width) - 1) mv (<< mask bitoff) cmask ((0 - mv) - 1)
                           sgn !(usg? uty || id? uty 'bool) lo (64 - width)
                           (mkv 'long 'r0
                              (vfs lv + spush 'r0
                               + asintt g r uty
                               + (? (id? uty 'bool) (cvt uty 'r0) ())   ; normalize BEFORE the mask ((bool)4 is 1)
                               + spop 'r1
                               + `(`((uldop usz) 'r2 'r1 0)              ; r2 = the unit
                                   `('li 'r3 mask) '(and r0 r0 r3)       ; r0 = v & mask (the truncated value)
                                   `('li 'r3 cmask) '(and r2 r2 r3)      ; clear the field's bits
                                   '(mov r3 r0))
                               + (? (bitoff > 0) `(`('shl 'r3 bitoff)) ())   ; r3 = (v & mask) << bitoff
                               + `('(or r2 r2 r3) `((stop uty) 'r1 0 'r2))
                               + (? sgn `(`('shl 'r0 lo) `('sar 'r0 lo)) ()))))))
                 (pair? g (vty lv))                  ; a pair store through a computed address
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (mkv (vty lv) 'r0
                         (vfs lv + spush 'r0 + asll g r + spop 'r2 + pst 'r2 0))))
                 (d128? (vty lv))                    ; a wide store through a computed address (r1)
                  (? !(x64t? g) 'bad
                     (: r (cgexpr g env <>>e)
                        (? (bad? r) 'bad
                           (mkv (vty lv) 'r0 (vfs lv + spush 'r0 +
                            as128 g r + spop 'r1 + '((st r1 0 r0) (st r1 8 r2)))))))
                 ; the ADDRESS parks in a pool register past a call-free rhs (the vpark
                 ; discipline, extended to the store shuttle): no memory round-trip on the
                 ; address. a call on the right keeps the shuttle -- it would clobber the park.
                 ; the ARRAY leg's write residency: an element store aims its rhs at a
                 ; fresh pool register, converts IN PLACE (canonical, like an int home's
                 ; def), stores from it and RE-PINS the key -- the written value stays
                 ; resident and the read-after-write is zero forms. write-through holds
                 ; (the store still emits); unhonored (a homed var, a float) hands the
                 ; register back and the drop-only path stands.
                 (: ael (aelem g env <>e)
                    aR (? (callish? <>>e) () (ralloc g))
                    hint (? (aR && two? ael && id? <ael 'el &&
                             vmon? g && !(0 < peep g 'argseal 0))
                            (ralloc g) ())
                    _ (? hint (pin g 'want hint) ())
                    r (cgexpr g env <>>e)
                    (? (bad? r) (: _ (? aR (rfree g aR) ())
                                   _ (? hint (rfree g hint) ()) 'bad)
                       (: ty (vty lv)
                          _ (adrop9 g env <>e)     ; an element store retires its pin(s)
                          (? (hint && id? (vreg r) hint && !(flo? (vty r) || flo? ty))
                              (: fs (vfs lv +
                                     `(`('mov aR 'r0)) +
                                     vfs r +
                                     cvt ty hint +
                                     `(`((stop ty) aR 0 hint)))
                                 _ (rfree g aR)
                                 _ (vapin g <>ael hint)
                                 (mkv ty hint fs))
                             (: _ (? (hint && !(id? (vreg r) hint)) (rfree g hint) ())
                                (? aR
                                   (: fs (vfs lv +
                                          `(`('mov aR 'r0)) +
                                          asintt g r ty +
                                          cvt ty 'r0 +
                                          `(`((stop ty) aR 0 'r0)))
                                      _ (rfree g aR)
                                      (mkv ty 'r0 fs))
                                   (mkv ty 'r0
                                    (vfs lv + spush 'r0 + asintt g r ty + spop 'r1 +
                                     cvt ty 'r0 + `(`((stop ty) 'r1 0 'r0))))))))))))))
          (k = 'post)
           (: rv (? (<<>e = 'var) (vent g env <><>e) 'bad)
              _ (? (<<>e = 'var) (vmdrop g <><>e) ())   ; the RMW retires any mapping
              (? (two? rv && <rv = 'regv)   ; p++ on a register-homed scalar: old in r0, home += step
                  (: ty <>>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     (mkv ty 'r0 (`(`('mov 'r0 <>rv)
                                    `('add <>rv <>rv (* stp <>>e))) + cvt ty <>rv)))
                 ; STORE-DIRECT (the asn twin): a frame scalar's slot is a STATIC offset --
                 ; read/step/write with a direct frame ld+st. leaves no lean, so a loop
                 ; counter's slot never reads as address-taken and the local can home
                 (two? rv && (<rv = 'loc) &&
                  (: t9 <>>rv !(stru? t9 || arr? t9 || flo? t9 || bitf? t9)))
                  (: ty <>>rv off <>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     (? (pair? g ty)
                        (mkv ty 'r0 (pld 'r4 off + ppost 'r4 off (* stp <>>e)))
                        (d128? ty)                   ; wide ++/--: step in place, store, step BACK (the
                         (? !(x64t? g) 'bad          ;  answer is the OLD pair, and the carry ops are in-place)
                            (mkv ty 'r0 `(`('ld 'r0 'r4 off) `('ld 'r2 'r4 (off + 8))
                                          `((? (<>>e < 0) 'dec128 'inc128))
                                          `('st 'r4 off 'r0) `('st 'r4 (off + 8) 'r2)
                                          `((? (<>>e < 0) 'inc128 'dec128)))))
                        (mkv ty 'r0 (`(`((ldop ty) 'r0 'r4 off)
                                       `('add 'r1 'r0 (* stp <>>e)))
                                     + (? (id? ty 'bool) (cvt ty 'r1) ())   ; b++/b-- lands on 0/1
                                     + `(`((stop ty) 'r4 off 'r1))))))
                 (: lv (clval g env <>e)
              (? (bad? lv) 'bad
                 (: ty (vty lv)
                    stp (? (ptr? ty) (tsz g <>ty) 1)
                    _ (adrop9 g env <>e)           ; an element RMW retires its pin(s)
                    (? (arr? ty) 'bad
                       (bitf? ty) 'bad             ; bitfield ++/-- -- refuse (rare; use `f = f + 1`)
                       (d128? ty) 'bad             ; wide ++/-- -- refuse (use `x = x + 1`)
                       (pair? g ty)
                        (mkv ty 'r0 (vfs lv +
                               '((mov r12 r0)) + (pld 'r12 0) + (ppost 'r12 0 (* stp <>>e))))
                       (mkv ty 'r0 (vfs lv +
                              (`('(mov r2 r0)
                                 `((ldop ty) 'r0 'r2 0)
                                 `('add 'r1 'r0 (* stp <>>e)))
                               + (? (id? ty 'bool) (cvt ty 'r1) ())   ; b++/b-- lands on 0/1
                               + `(`((stop ty) 'r2 0 'r1)))))))))))
          ; ⚠ `E1 op= E2` EVALUATES E1 ONCE. the plain desugar duplicates it --
          ; (asn lv (bin op lv rhs)) -- which is exact only for an lvalue whose
          ; evaluation leaves no trace, so parse routes every other one here (its
          ; calm?). the address is computed once and MOORED in a frame temp; the
          ; read, the op and the store back then run over ('moor off ty), a calm
          ; lvalue standing for it -- so pointer scaling, the narrowing store,
          ; doubles, pairs, wides and bitfields each ride their own lane unchanged.
          (k = 'rmw)
           (: lv (clval g env <>>e)
              (? (bad? lv) 'bad
                 (: ty (vty lv)
                    (? (arr? ty || stru? ty) 'bad
                       (: off (ntmp g 'long)
                          mo `('moor off ty)
                          r (cgexpr g env `('asn mo `('bin <>e mo <>>>e)))
                          _ (adrop9 g env <>>e)    ; the moored store retires element pin(s)
                          (? (bad? r) 'bad
                             (mkv (vty r) (vreg r)
                                  (vfs lv + `(`('st 'r4 off 'r0)) + vfs r))))))))
          (k = 'cast)
           (: tgt <>e
              _ (? (!(flo? tgt) && !(two? (cvt tgt 'r0))) (pin g 'want wnt) ())
              r (cgexpr g env <>>e)
              (? (bad? r) 'bad
                 (flo? tgt && d128? (vty r))
                  ; (double) of a WIDE value: only a CONSTANT expression, folded exactly (love.c's
                  ; (double)limb_base = 2^64; cfold is bignum-native, love's + converts a big
                  ; correctly rounded; the discarded emission is pure -- the expr folded). there
                  ; is no runtime wide->double lane -- an unfoldable one refuses.
                  (: v (cfold <>>e)
                     (? (cint? v) (mkv 'double 'r0 `(`('li 'r0 (fbits (0.0 + v))) '(movqxr f0 r0)))
                        'bad))
                 (d128? tgt && flo? (vty r)) 'bad                            ; double -> wide: not carried
                 (flo? tgt && !(flo? (vty r))) (mkv 'double 'r0 (asflo g r))  ; int -> double
                 ((tgt = 'bool) && flo? (vty r))       ; (bool)d is d != 0.0, never a truncation
                  ; ((bool)0.5 is 1) -- the un-'not lane's shape with the NaN-honest !=
                  (: z1 (? (t32? g) '((li r0 0) (li r1 0) (movqxr f1 r0))
                           '((li r0 0) (movqxr f1 r0)))
                     (mkv 'bool 'r0 (vfs r + (z1 + ('((ucomisd f0 f1)) + fsetn)))))
                 (!(flo? tgt) && flo? (vty r))                              ; double -> int, then narrow
                  (? (pair? g tgt)                                               ; double -> llong: the pattern peel
                      (mkv tgt 'r0 (vfs r + cvtdll g (usg? tgt)))
                     (t32? g && usg? tgt)                                    ; t32 unsigned: the U-converter
                      (mkv tgt 'r0 (vfs r + '((cvttsd2ui r0 f0)) + cvt tgt 'r0))
                     (mkv tgt 'r0 (asintt g r tgt + cvt tgt 'r0)))
                 (flo? tgt) (mkv 'double 'r0 (vfs r))                            ; double -> double
                 (pair? g tgt && !(llt? (vty r)))                           ; widen to the pair (source signedness)
                  (mkv tgt 'r0 (asll g r))
                 (d128? tgt && !(d128? (vty r)))                            ; widen to the wide pair (source signedness)
                  (? (x64t? g) (mkv tgt 'r0 (as128 g r)) 'bad)
                 (two? (cvt tgt 'r0)) (mkv tgt 'r0 (tor0 g r + cvt tgt 'r0))   ; int -> narrower int
                 (mkv tgt (vreg r) (vfs r))))                                    ; a full-word int cast KEEPS the register (a wide one narrows to its lo)
          (k = 'un)
           (: r (cgexpr g env <>>e)
              (? (bad? r) 'bad
                 (gcplx? (vty r) && id? <>e 'neg)      ; -z: both components flip through the complex lane
                  (: pa (cxpark g r)
                     (? (bad? pa) 'bad
                        (cxout g (? (id? (cxb (vty r)) 'float) (vty r) cxdty)
                           ; each component subtracts from MINUS zero, for the reason the scalar
                           ; lane below spells out. r0 is free here -- cxout writes it last.
                           (<>pa + `(`('ldsd 'f0 'r4 <pa) `('ldsd 'f1 'r4 (<pa + 8))
                                     `('li 'r0 0x8000000000000000) '(movqxr f4 r0)
                                     '(subsd f4 f0) '(movsd f0 f4)
                                     `('li 'r0 0x8000000000000000) '(movqxr f4 r0)
                                     '(subsd f4 f1) '(movsd f1 f4))))))
                 (flo? (vty r))                        ; unary on a double
                  (: z1 (? (t32? g) '((li r0 0) (li r1 0) (movqxr f1 r0))             ; 0.0 (the t32 pair)
                           '((li r0 0) (movqxr f1 r0)))
                     ; ⚠ NEGATION SUBTRACTS FROM MINUS ZERO, NOT FROM ZERO: `0.0 - d` is right
                     ; for every input but one -- 0.0 - 0.0 is +0.0, so -0.0 could never be
                     ; produced. -0.0 - x IS -x everywhere, so the CONSTANT carries it: same op
                     ; count, same registers, and no xorpd (d==s only on riscv).
                     n0 (? (t32? g) `('(li r0 0) `('li 'r1 0x80000000) '(movqxr f1 r0))
                           `(`('li 'r0 0x8000000000000000) '(movqxr f1 r0)))
                     (? (<>e = 'neg)                 ; -d = (-0.0) - d
                         (mkv 'double 'r0 (vfs r + (n0 + '((subsd f1 f0) (movsd f0 f1)))))
                        (<>e = 'not)                 ; !d = (d == 0.0), NaN-honest: !NaN is false -- fsete
                         (mkv 'long 'r0 (vfs r + (z1 + ('((ucomisd f0 f1)) + fsete))))
                        'bad))                           ; ~ on a double is invalid
                 (pair? g (vty r))                     ; unary on the pair
                  (? (<>e = 'neg)
                      (mkv (vty r) 'r0 (tor0 g r + '((li r2 0) (li r3 0)
                                                       (subs r0 r2 r0) (sbc r1 r3 r1))))
                     (<>e = 'bnot)
                      (mkv (vty r) 'r0 (tor0 g r + '((not r0) (not r1))))
                     (mkv 'long 'r0 (tor0 g r + `(`('ors (? (v6m? g) 'r0 'r12) 'r0 'r1) '(set eq r0)))))
                 (d128? (vty r))                       ; unary on the wide pair
                  (? !(x64t? g) 'bad
                     (<>e = 'neg) (mkv (vty r) 'r0 (tor0 g r + '((n128))))
                     (<>e = 'bnot) (mkv (vty r) 'r0 (tor0 g r + '((not r0) (not r2))))
                     (mkv 'long 'r0 (tor0 g r + '((or r0 r0 r2) (cmp r0 0)) + '((set eq r0)))))
                 ; -1ULL stays unsigned; a SUB-64 unsigned re-narrows after neg/~ (the model
                 ; keeps unsigned intermediates zero-extended: -1u wraps at 32 bits, not 64)
                 (: uty (member? <>e '(neg bnot) && usg? (vty r))
                    renar (? !uty () (vty r = 'uchar) '((zx1 r0))
                             (vty r = 'ushort) '((zx2 r0))
                             (vty r = 'uint) '((zx4 r0)) ())
                    (mkv (? uty (vty r) 'long) 'r0
                       (tor0 g r +
                        (? (<>e = 'neg) '((neg r0))
                           (<>e = 'bnot) '((not r0))
                           '((cmp r0 0) (set eq r0))) +
                        renar)))))
          (k = 'comma)
           (: a (cgexpr g env <>e)
              _ (pin g 'want wnt)
              b (cgexpr g env <>>e)
              (? (bad? a || bad? b) 'bad
                 (mkv (vty b) (vreg b) (vdrop g a + vfs b))))
          (k = 'land)                                ; as a VALUE: cbranch the short-circuit, then settle the bit
           (: lf (nlab g ".f") ld (nlab g ".d")
              f (cbranch g env e 'eq lf)
              _ (vmflush g)                          ; labels lf/ld join
              (? (bad? f) 'bad
                 (mkv 'long 'r0 (f + `('(li r0 1) `('jmp ld)
                                          `('label lf) '(li r0 0) `('label ld))))))
          (k = 'lor)
           (: lt (nlab g ".t") ld (nlab g ".d")
              f (cbranch g env e 'ne lt)
              _ (vmflush g)                          ; labels lt/ld join
              (? (bad? f) 'bad
                 (mkv 'long 'r0 (f + `('(li r0 0) `('jmp ld)
                                          `('label lt) '(li r0 1) `('label ld))))))
          (k = 'cond)
           (: le (nlab g ".e") ld (nlab g ".d")
              c (cbranch g env <>e 'eq le)
              m0 (vmsnap g)                          ; the branch edge into le
              t (cgexpr g env <>>e)
              mt (vmsnap g)                          ; an expression arm always falls out
              _ (vmset g m0)                         ; the else arm enters off the branch edge
              el (cgexpr g env <>>>e)
              _ (vmset g (vmeet mt (vmsnap g)))      ; label ld joins the two edges
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
                       (c + tf + `(`('jmp ld) `('label le)) + ef + `(`('label ld)))))))
          ; -- the varargs builtins, per target (crew/moon/include/stdarg.h). SysV: the 24-byte
          ; { int gp_offset; int fp_offset; void *overflow_arg_area; void *reg_save_area; } --
          ; va_arg fetches from the save area until the offset passes the reg limit, then from
          ; the overflow area. AAPCS64: the 32-byte { void *__stack; void *__gr_top; void
          ; *__vr_top; int __gr_offs; int __vr_offs; } -- ⚠ the offs fields are NEGATIVE, counted
          ; back from each area's top toward 0; at >= 0 the arg is on the caller stack at
          ; __stack. va_start seeds either past the named params; va_end nops. --
          (k = 'vastart)
           ; cgexpr (not clval) gives the struct pointer uniformly: a va_list LOCAL (array) decays
           ; to its address, a va_list PARAM loads its value -- both land the address in r0.
           (: lv (cgexpr g env <>e)                 ; the va_list struct pointer in r0
              rsa (peep g 'rsa 0) ngp (peep g 'vagp 0) nxmm (peep g 'vaxmm 0)
              nov (peep g 'vaov 0)                  ; named params that overflowed (slots)
              (? (bad? lv) 'bad
                 (: _ (pin g 'fesc 1)                                       ; the va_list HOLDS frame addresses: no sibcalls here
                    (mkv 'long 'r0 (tor0 g lv +
                      (? (t32? g)   ; AAPCS32: __ap = the first anonymous word, past the push cells + named regs + named overflow
                         `(`('lea 'r1 'r4 (ovb g + 4 * ngp + nov)) '(st r0 0 r1))
                         (rv? g)   ; LP64: __ap = the first anonymous 8-byte slot, past the push cells + named regs + named overflow
                         `(`('lea 'r1 'r4 (ovb g + 8 * ngp + 8 * nov)) '(st r0 0 r1))
                         (a64? g)
                         `(`('lea 'r1 'r4 (ovb g + 8 * nov)) '(st r0 0 r1)   ; __stack = fp+32 + named overflow
                            `('lea 'r1 'r4 (rsa + 64))  '(st r0 8 r1)            ; __gr_top (past x0..x7)
                            `('lea 'r1 'r4 (rsa + 192)) '(st r0 16 r1)           ; __vr_top (past q0..q7)
                            `('li 'r1 -(8 * (8 - ngp)))  '(st4 r0 24 r1)    ; __gr_offs = -(unnamed gp)*8
                            `('li 'r1 -(16 * (8 - nxmm))) '(st4 r0 28 r1))  ; __vr_offs = -(unnamed vr)*16
                         `(`('li 'r1 (8 * ngp))         '(st4 r0 0 r1)   ; gp_offset = 8*named-gp
                            `('li 'r1 (48 + 16 * nxmm)) '(st4 r0 4 r1)  ; fp_offset = 48 + 16*named-xmm
                            '(lea r1 r4 16)  '(st r0 8 r1)             ; overflow_arg_area = rbp+16
                            `('lea 'r1 'r4 rsa) '(st r0 16 r1))))))))      ; reg_save_area = rbp+rsa
          (k = 'vaarg)
           (: lv (cgexpr g env <>e) ty <>>e    ; the va_list struct pointer in r0
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
                         `(`('ld4 'r1 'r0 foff) '(cmp r1 0) `('br 'ge lov)
                            `('ld 'r2 'r0 toff) '(add r2 r2 r1)           ; p = top + offs
                            `('add 'r1 'r1 step) `('st4 'r0 foff 'r1)          ; advance the offs
                            `('jmp ld) `('label lov)
                            '(ld r2 r0 0) '(mov r3 r2) '(add r3 r3 8) '(st r0 0 r3)   ; p = __stack; __stack += 8
                            `('label ld)))
                      (: foff (? (flo? ty) 4 0)                 ; fp_offset (4) vs gp_offset (0)
                         lim (? (flo? ty) 176 48)               ; the register-area limit
                         step (? (flo? ty) 16 8)                ; the offset stride
                         `(`('ld4 'r1 'r0 foff) `('cmp 'r1 lim) `('br 'ge lov)
                            '(ld r2 r0 16) '(add r2 r2 r1)            ; p = reg_save_area + offset
                            `('add 'r1 'r1 step) `('st4 'r0 foff 'r1)         ; advance the offset
                            `('jmp ld) `('label lov)
                            '(ld r2 r0 8) '(mov r3 r2) '(add r3 r3 8) '(st r0 8 r3)   ; p = overflow; overflow += 8
                            `('label ld))))
              _ (vmflush g)                           ; the walk's labels join
              (? (bad? lv) 'bad
                 (t32? g && (flo? ty || llt? ty || stru? ty)) 'bad   ; t32 va_arg reads WORDS only (love.c's shape) -- refuse LOUD
                 (d128? ty) 'bad                     ; a wide va_arg: not carried
                 (flo? ty) (mkv 'double 'r0 (tor0 g lv + walk + fldf ty 'f0 'r2 0))
                 (mkv ty 'r0 (tor0 g lv + walk + `(`((ldop ty) 'r0 'r2 0))))))
          (k = 'vaend) (mkv 'long 'r0 ())
          (k = 'call)
           ; the SysV ABI: integer/pointer args ride r6 r5 r2 r1 r7 r8 (<=6), double args f0..f7
           ; (<=8), SEPARATE counters, placed by the callee's PARAM type (a direct call reads the
           ; sig table and converts to it) else by the arg's own. the result rides f0 for a
           ; double-returning callee, r0 otherwise. args evaluate left to right, each pushed to
           ; the machine stack, then popped reversed into place.
           (: head <>e args <>>e
              bi (? (<head = 'var) <>head ())
              bv (cgbuiltin g env bi args)
              (? !(id? bv 'no) bv
                 (: m9 (peep g 'must9 ())
                    _ (? m9 (pin g 'must9 ()) 0)
                    inlf (? m9 ()                                  ; a musttail call is OWED as a call: never spliced
                            (2 < peep g 'inlining 0) ()   ; splices NEST to depth 3, cbinl's cap
                            !(<head = 'var) ()
                            !(bad? (vent g env <>head)) ()   ; a local/global shadows the name
                            (peep (g 'inl) <>head ()))
                    ir9 (? (two? inlf && tally args = tally <>>inlf)
                           (cginl g env inlf args) 'no)              ; 'no: splice declined (fesc or shape) -> the real call
                    (? !(id? ir9 'no) ir9
                 (: direct (<head = 'var && bad? (vent g env <>head) &&  ; not a local/global variable...
                                            peep (g 'fns) <>head ())     ; ...and a KNOWN function (typos refuse)
                    sig (? direct (peep (g 'sigs) <>head ()) ())
                    ptys (? (two? sig) <>sig ())
                    rt (? (two? sig) <sig 'long)
                    variadic (? (two? sig) (nthr sig 2) ())
                    ; the ARITY CHECK: a sig'd direct call hands its callee the arguments the
                    ; prototype asked for (the named ones a FLOOR when variadic), else a short
                    ; call reads whatever the argument register held.
                    ; ⚠ AN EMPTY PARAMETER LIST IS EXEMPT AND MUST STAY SO: `int f()` and
                    ; `int f(void)` both park an empty ptys and the sigs row cannot tell them
                    ; apart -- refusing the first would refuse musl's headers.
                    nps (tally ptys) nas (tally args)
                    off ((0 < nps) && (? variadic (nas < nps) !(nas = nps)))
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
                                              `(`('lean 'r0 'r4 toff)))))
                       (t32? g &&                    ; t32: past the d-pair, only the 8B sse blob (d0) and the <=4B int one (r0) return
                        !((id? (aone g (vty cr)) 'sse && tsz g (vty cr) = 8) ||
                          (id? (aone g (vty cr)) 'int && tsz g (vty cr) <= 4))) 'bad
                       (ipair? g (vty cr))             ; a two-int struct result: park rax:rdx (arm64 x0:x1) the same way
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (vfs cr +
                                        `(`('st 'r4 toff 'r0) `('st 'r4 (toff + 8) (ip2 g))
                                           `('lean 'r0 'r4 toff)))))
                       (id? (aone g (vty cr)) 'sse)    ; a small SSE aggregate result: park xmm0
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (vfs cr +
                                              (fstf g 'double 'r4 toff (xmmreg 0) +
                                               `(`('lean 'r0 'r4 toff))))))
                       (id? (aone g (vty cr)) 'int)    ; a small INT aggregate result: park rax
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (vfs cr + `(`('st 'r4 toff 'r0) `('lean 'r0 'r4 toff)))))
                       'bad))))))
          (k = 'clit)                              ; a compound literal as a value: fill the temp, load it
           (: lv (clval g env e)
              (? (bad? lv) 'bad (loadval g (vty lv) (vfs lv) wnt)))
          (k = 'bin) (cgbin g env e wnt)
          'bad))
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
                      (? (flo? aty || stru? aty || llt? aty || d128? aty) 'no `(`('ez i) fs)))   ; a pair arg keeps the cell path
       kc (knum g e)
       (? (two? kc) (? (t32? g && !(-2147483648 <= <kc && <kc <= 4294967295)) 'no
                       (fin 'long `(`('li rT <kc))))
          (<e = 'str)
           (: lbl (nlab g ".S")
              _ (pin g 'strs (`(lbl <>e) >< g 'strs))
              (fin '(ptr char) `(`('la rT lbl))))
          (<e = 'addr)                          ; &var only: lea/la, argpos-equivalent (no escan)
           (? !(<<>e = 'var) 'no
              (: en (vent g env <><>e)
                 (? (bad? en) 'no
                    (<en = 'loc) (fin `('ptr <>>en) `(`('lea rT 'r4 <>en)))
                    (<en = 'glo) (fin `('ptr <>>en) `(`('la rT <>en)))
                    'no)))
          !(<e = 'var) 'no
          (: en (vent g env <>e)
             (? (bad? en) 'no
                (: ty <>>en
                   (? (<en = 'regv)             ; a param in a register: mov from it, or ld its spill slot
                       (? (flo? (? pty pty ty) || stru? (? pty pty ty)) 'no
                          (: sl (assoc <>en (peep g 'homes ()))
                             (? (two? sl)
                                 `(`('ezm i <>en)
                                   `(`('mov rT <>en))
                                   `(`('ld rT 'r4 <>sl)))
                                ; THE RIDE has no spill slot to fall back on, so it is targeted
                                ; only where nothing can move the value first: every sibling a
                                ; pure leaf (ezride -- no evaluation runs before the emission)
                                ; and the source outside the argument file (no sibling's own
                                ; targeted emission lands on it). that is the VM's whole tail-
                                ; forward fleet -- pool-homed params straight into r0..r3.
                                (peep g 'ezride () && !(member? <>en (argr g)))
                                 (fin ty `(`('mov rT <>en)))
                                'no)))
                      (arr? ty)                      ; array decay: its address
                       (? (<en = 'loc) (fin `('ptr <>ty) `(`('lea rT 'r4 <>en)))
                          (fin `('ptr <>ty) `(`('la rT <>en))))
                      (stru? ty || flo? ty) 'no
                      (<en = 'loc) (fin ty `(`((ldop ty) rT 'r4 <>en)))
                      (fin ty `(`('la rT <>en) `((ldop ty) rT rT 0)))))))))   ; a global: la then a self-based load
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
              _ (? aim (pin g 'want aim) ())
              a (: _ (? (member? <<as '(addr var)) (pin g 'argpos 1) ())
                   (cgexpr g env <as))
              _ (pin g 'argpos ())
              (? (bad? a) 'bad
                 (: pty (nthty pts i)
                    aty (? pty pty (vty a))                ; classify by the param type if known, else the arg's
                    wd (flo? aty)
                    ; t32 composites: an HFA rides the d-regs (the sse2 pair lane below = AAPCS32-VFP
                    ; d-pairs; an 8B one-sse blob = the packed S-pair, bit-identical); a <=4B int one
                    ; rides r0's word. mixed/int-pair 8..16B stays deferred -- refuse LOUD.
                    (? (t32? g && va? && stru? aty) 'bad   ; a t32 VARIADIC call rides the base PCS: no d-reg composites -- refuse LOUD
                       (v6m? g && sse2? g aty)       ; v6-M: a POSITION-0 16B composite rides r0-r3 whole (the
                        (? (gpc = 0)                     ;  base-ABI words); any later start would SPLIT -- deferred
                           (kls >as (i + 1) 4 xmmc ovc
                                (link `('(g4 0)
                                         (tor0 g a +
                                          '((sub sp sp 16) (ld r1 r0 12) (st sp 12 r1) (ld r1 r0 8) (st sp 8 r1)
                                            (ld r1 r0 4) (st sp 4 r1) (ld r1 r0 0) (st sp 0 r1))))
                                      acc))
                           'bad)
                       (t32? g && stru? aty &&
                           !(sse2? g aty ||
                             (id? (aone g aty) 'sse && tsz g aty = 8) ||
                             (id? (aone g aty) 'int && tsz g aty <= 4))) 'bad
                       (sse2? g aty)                       ; a two-double struct arg: push its pair (re on top), an xmm PAIR target
                        (? (6 < xmmc) 'bad
                           (kls >as (i + 1) gpc (xmmc + 2) ovc
                                (link `(`('x2 xmmc)
                                         (tor0 g a +
                                          (? (t32? g)     ; t32: a 16-byte cell, four word copies
                                              '((sub sp sp 16) (ld r1 r0 12) (st sp 12 r1) (ld r1 r0 8) (st sp 8 r1)
                                                (ld r1 r0 4) (st sp 4 r1) (ld r1 r0 0) (st sp 0 r1))
                                             (a8? g)      ; arm64/riscv push is a 16-byte cell of its own: lay the pair by hand
                                              '((sub sp sp 16) (ld r1 r0 8) (st sp 8 r1) (ld r1 r0 0) (st sp 0 r1))
                                             '((ld r1 r0 8) (push r1) (ld r1 r0 0) (push r1)))))
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
                                       (link `(`('ov ovc 2)
                                                (tor0 g a +
                                                 '((ld r1 r0 8) (push r1) (ld r1 r0 0) (push r1))))
                                             acc)))
                              (: i0 (? (id? f0 'g) gpc xmmc)
                                 gpc1 (? (id? f0 'g) (gpc + 1) gpc)
                                 xmc1 (? (id? f0 'x) (xmmc + 1) xmmc)
                                 i1 (? (id? f1 'g) gpc1 xmc1)
                                 (kls >as (i + 1) (gpc + gpn) (xmmc + xmn) ovc
                                      (link `(`('p2 f0 i0 f1 i1)
                                               (tor0 g a +
                                                (? (a8? g)
                                                 '((sub sp sp 16) (ld r1 r0 8) (st sp 8 r1) (ld r1 r0 0) (st sp 0 r1))
                                                 '((ld r1 r0 8) (push r1) (ld r1 r0 0) (push r1)))))
                                            acc)))))
                       ; a MEMORY-class by-value struct arg: its eightbytes fill N overflow
                       ; slots. ⚠ x64 pushes DEEPEST first, so the struct's HIGHEST eightbyte
                       ; goes first -- offset 0 lands at the block's low address.
                       (amem? g aty)
                        (? (< 8 (asalign g aty)) 'bad         ; over-aligned MEMORY arg: deferred, refuse LOUD
                           (: n (aslots g aty)
                              (mv j acc9) (? (j < 0) acc9
                                             (mv (j - 1) (acc9 + `(`('ld 'r1 'r0 (* 8 j)) '(push r1)))))
                              (kls >as (i + 1) gpc xmmc (ovc + n)
                                   (`(`('ov ovc n) (tor0 g a + mv (n - 1) ())) >< acc))))
                       (stru? aty && !(aone g aty)) 'bad        ; a MIXED 16B by-value struct stays ahead
                       (d128? aty) 'bad                          ; a wide arg: not carried (love.c passes limbs, never dlimbs)
                       (pair? g aty)                             ; a pair arg: an even-odd register pair (AAPCS32
                        (: gpe (& (gpc + 1) -2)                  ;  rounds up), else the stack 8-aligned -- and gp
                           (? ((gpe + 2) <= ngp g)               ;  assignment closes behind a stack arg (C.13)
                               (kls >as (i + 1) (gpe + 2) xmmc ovc
                                    (`(`('g2 gpe) (asll g a + spush2)) >< acc))
                               (: ov8 (& (ovc + 7) -8)
                                  (kls >as (i + 1) 4 xmmc (ov8 + 8)
                                       (link `(`('ov ov8 8)
                                                (asll g a + `(`('st 'sp ov8 'r0) `('st 'sp (ov8 + 4) 'r1))))
                                             acc)))))
                       ; the delivered seat: aimed and honored by the arg's OWN emission --
                       ; a zero-form answer (a riding home, a vmap pin) predates the staging
                       ; and an earlier-emitted callish sibling would clobber it: cell.
                       (aim && id? (vreg a) aim && two? (vfs a) &&
                           !(flo? (vty a) || flo? aty || stru? aty))
                        (kls >as (i + 1) (gpc + 1) xmmc ovc (`(`('ezd gpc) (vfs a)) >< acc))
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
                          opush (? (a8? g) (aval + `(`('st 'sp (* 8 ovc) 'r0)))
                                   (t32? g) (aval + `(`('st 'sp ovc 'r0)))
                                   (aval + '((push r0))))
                          (? (t32? g && va? && wd2) 'bad   ; a t32 VARIADIC call rides the base PCS: no d-reg doubles -- refuse LOUD
                             (v6m? g && id? aty 'float)    ; v6-M base ABI: a bare float arg is ONE WORD (binary32,
                              (: av1 (asflo g a              ;  the gcc shape -- ai_flo_t IS float on a 32-bit love)
                                      + '((cvtsd2ss f0 f0) (movqrx r0 f0)))
                                 (? (gpc < ngp g)
                                    (kls >as (i + 1) (gpc + 1) xmmc ovc
                                         (`(`('g gpc) (av1 + spush 'r0)) >< acc))
                                    (kls >as (i + 1) gpc xmmc (ovc + 4)
                                         (`(`('ov ovc 4) (av1 + `(`('st 'sp ovc 'r0)))) >< acc))))
                             (v6m? g && wd2)               ; v6-M base ABI: a double/8B-blob arg is an even-odd gp
                              (: gpe ((gpc + 1) & -2)      ;  pair (aval already lands r0:r1), else the 8-aligned stack
                                 (? (gpe + 2 <= (ngp g))
                                     (kls >as (i + 1) (gpe + 2) xmmc ovc
                                          (`(`('g2 gpe) (aval + spush2)) >< acc))
                                     (: ov8 (& (ovc + 7) -8)
                                        (kls >as (i + 1) 4 xmmc (ov8 + 8)
                                             (link `(`('ov ov8 8)
                                                      (aval + `(`('st 'sp ov8 'r0) `('st 'sp (ov8 + 4) 'r1))))
                                                   acc)))))
                             ; a VARIADIC riscv call: every double rides the GP lane (the callee's
                             ; packed a0..a7 block and 8-byte walk read gp slots), falling
                             ; through to the g / ov lanes below
                             (rv? g && va? && wd2 && gpc < ngp g)
                              (kls >as (i + 1) (gpc + 1) xmmc ovc (`(`('g gpc) rpush) >< acc))
                             (rv? g && va? && wd2)
                              (kls >as (i + 1) gpc xmmc (ovc + 1) (`(`('ov ovc) opush) >< acc))
                             (wd2 && xmmc < 8) (kls >as (i + 1) gpc (xmmc + 1) ovc (`(`('x xmmc) rpush) >< acc))
                             (t32? g && wd2) 'bad         ; a t32 fp/blob OVERFLOW arg (9+ doubles): the 4-byte slot can't carry it -- refuse LOUD
                             (!wd2 && gpc < ngp g) (kls >as (i + 1) (gpc + 1) xmmc ovc (`(`('g gpc) rpush) >< acc))
                             (t32? g) (kls >as (i + 1) gpc xmmc (ovc + 4) (`(`('ov ovc 4) opush) >< acc))
                             (kls >as (i + 1) gpc xmmc (ovc + 1) (`(`('ov ovc) opush) >< acc)))))))))))
       ; v6-M MEMORY return (AAPCS32 base): a >4-byte composite result fills a caller temp
       ; whose address rides the IMPLICIT first argument (r0) -- args start at gpc 1, the
       ; address materializes fresh right before the call and again after it (the result)
       sr6 (sretm? g ret)
       sroff (? sr6 (ntmp g ret) 0)
       _ (pin g 'argseal (peep g 'argseal 0 + 1))       ; the vmap seals across the staging (see vmon?)
       ; a call whose every argument is a pure leaf runs no sibling evaluation, so
       ; nothing between the reads and the call can clobber a register-resident
       ; param -- ezarg's ride lane asks for this.
       _ (pin g 'ezride (: (pure? l) (? !(two? l) 1 (member? <<l '(num var) && pure? >l))
                          (pure? ags)))
       kr (kls ags 0 (? sr6 1 0) 0 0 ())
       _ (pin g 'ezride ())
       _ (pin g 'argseal (peep g 'argseal 0 - 1))
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
                            (go4 >l4 (? (member? <kc '(ov ez ezm ezd)) acc4
                                             (acc4 + (? (<kc = 'g) (spop (nthr (argr g) <>kc))
                                                          (<kc = 'g2)   ; the t32 pair: both words off one cell
                                                           `(`('ld (nthr (argr g) <>kc) 'sp 0)
                                                              `('ld (nthr (argr g) (<>kc + 1)) 'sp 4)
                                                              '(add sp sp 16))
                                                          (<kc = 'g4)   ; the v6-M whole-composite quad: r0-r3 off one cell
                                                           '((ld r0 sp 0) (ld r1 sp 4)
                                                              (ld r2 sp 8) (ld r3 sp 12)
                                                              (add sp sp 16))
                                                          (t32? g && (<kc = 'x))     ; a t32 double pops STRAIGHT into its d-reg
                                                           `(`('ldsd (xmmreg <>kc) 'sp 0) '(add sp sp 16))
                                                          (<kc = 'x2)   ; the struct pair: re then im (a natural 16-byte cell)
                                                           (? (t32? g)   ; t32 pops STRAIGHT into the d-pair
                                                              `(`('ldsd (xmmreg <>kc) 'sp 0) `('ldsd (xmmreg (<>kc + 1)) 'sp 8)
                                                                 '(add sp sp 16))
                                                              (a8? g)
                                                              `(`('ld px 'sp 0) `('movqxr (xmmreg <>kc) px)
                                                                 `('ld px 'sp 8) `('movqxr (xmmreg (<>kc + 1)) px)
                                                                 '(add sp sp 16))
                                                              `('(pop r0) `('movqxr (xmmreg <>kc) 'r0)
                                                                '(pop r0) `('movqxr (xmmreg (<>kc + 1)) 'r0)))
                                                          (<kc = 'p2)   ; a general two-eightbyte struct: eb0 (top) then eb1, each into its gp/xmm target
                                                           (? (a8? g)
                                                              ((? (id? <>kc 'g) `(`('ld (nthr (argr g) <>>kc) 'sp 0)) `(`('ld px 'sp 0) `('movqxr (xmmreg <>>kc) px)))
                                                               + (? (id? <>>>kc 'g) `(`('ld (nthr (argr g) <>>>>kc) 'sp 8)) `(`('ld px 'sp 8) `('movqxr (xmmreg <>>>>kc) px)))
                                                               + '((add sp sp 16)))
                                                              ((? (id? <>kc 'g) `(`('pop (nthr (argr g) <>>kc))) `('(pop r0) `('movqxr (xmmreg <>>kc) 'r0)))
                                                               + (? (id? <>>>kc 'g) `(`('pop (nthr (argr g) <>>>>kc))) `('(pop r0) `('movqxr (xmmreg <>>>>kc) 'r0)))))
                                                          (spop px + `(`('movqxr (xmmreg <>kc) px)))))))))
                     (go4 kr ()))
             nxmm (: (go5 l5 n5) (? !(two? l5) n5
                                    (go5 >l5 (: ce <<l5 kk <ce
                                                     (? (kk = 'x) (n5 + 1) (kk = 'x2) (n5 + 2)
                                                        (kk = 'p2) (n5 + (? (id? (nthr ce 1) 'x) 1 0) + (? (id? (nthr ce 3) 'x) 1 0))
                                                        n5))))
                     (go5 kr 0))
             ; SLOTS, not entries: a MEMORY-class struct arg occupies several (its ('ov off n)
             ; carries the count; t32's third field is bytes -- ovblk below sizes that block)
             nov (: (go8 l8 n8) (? !(two? l8) n8
                                   (: ce <<l8
                                      (go8 >l8 (n8 + (? (two? >>ce) <>>ce 1)))))
                    (go8 ovr 0))
             padn (? ((nov & 1) = 1) 8 0)               ; keep sp 16-aligned across the overflow block
             ; t32 slots are BYTE offsets ((ov off sz) entries): the block spans to the highest
             ; slot's end, rounded to 8 (AAPCS32 sp alignment). x64/a64 keep 8-byte slot counts.
             ovblk (? (t32? g)
                      (: (go9 l9 m9) (? !(two? l9) ((m9 + 7) & -8)
                                        (: ce <<l9
                                           e9 (? (id? <ce 'ov) (+ <>ce (? (two? >>ce) <>>ce 4)) 0)
                                           (go9 >l9 (? (m9 < e9) e9 m9))))
                         (go9 kr 0))
                      ((* 8 nov) + padn))
             ; x64 pads then pushes deepest-first; arm64 AND t32 allocate the packed
             ; block up front and the ov forms store into their own slots
             pad (? (arm? g) (? (0 < ovblk) `(`('sub 'sp 'sp ovblk)) ())
                    (? (0 < padn) `(`('sub 'sp 'sp padn)) ()))
             clean (? (0 < ovblk) `(`('add 'sp 'sp ovblk)) ())
             pf (pad + pushcat ovr () + pushcat rgr ())
             ; al = #xmm, set AFTER pops (r0 is no x64 arg register). AAPCS64/32 pass variadic
             ; args exactly like named ones -- no al, and ⚠ r0 IS arg 0 there: never clobber it.
             setal (? (va? && !(arm? g)) `(`('li 'r0 nxmm)) ())
             ; NON-LEAF register homes (g 'homes, ((reg slot)..)): a home rides a caller-saved
             ; register, so it spills to its frame slot just before the pops (the args have all
             ; evaluated, so the register holds the latest value) and reloads after the clean; a
             ; ret-position call's reloads are DEAD and the sibcall pass skips them. filtered to
             ; this statement's wrapset (g 'stwrap): a home DEAD across this call keeps its
             ; register, no spill and no reload. 'all (no liveness ran) wraps every home.
             hs (: h9 (peep g 'homes ()) w9 (peep g 'stwrap 'all)
                   (? (id? w9 'all) h9 (filter (\ h2 (member? <>>h2 w9)) h9)))
             spills (foldl (\ a2 p2 (a2 + `(`('st 'r4 <>p2 <p2)))) () hs)
             relods (foldl (\ a2 p2 (a2 + `(`('ld <p2 'r4 <>p2)))) () hs)
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
                 (: _ (vmflush g)                          ; the call clobbers every pool reg
                    ; v6-M base ABI: a double result arrives in r0:r1, a bare float as one word
                    ; (binary32 in r0 -- widen it) -- either lands on the f0 fiction
                    fres (? !(v6m? g && flo? ret) ()
                            (id? ret 'float) '((movqxr f0 r0) (cvtss2sd f0 f0))
                            '((movqxr f0 r0)))
                    ; the hidden sret pointer rides the FIRST ARGUMENT register (rdi on x64, r0
                    ; on arm), and the result is the temp's address, materialized fresh in r0
                    srin (? sr6 `(`('lea (nthr (argr g) 0) 'r4 sroff)) ())
                    srout (? sr6 `(`('lea 'r0 'r4 sroff)) ())
                    (mkv ret 'r0 (pf + spills + ezf + pops + setal + srin + `(`('call (intern <>hd))) + fres + srout + clean + relods)))
                (: hr (cgexpr g env hd)                   ; a function-pointer value: park (x64 rbx; arm64 x15), callr
                   pk (pkr g)
                   _ (vmflush g)                          ; the call clobbers every pool reg
                   fres (? !(v6m? g && flo? (fnret (vty hr))) ()
                           (id? (fnret (vty hr)) 'float) '((movqxr f0 r0) (cvtss2sd f0 f0))
                           '((movqxr f0 r0)))
                   (? (bad? hr) 'bad
                      (v6m? g && stru? (fnret (vty hr)) &&   ; a MEMORY-returning callee behind a POINTER: the
                          4 < tsz g (fnret (vty hr))) 'bad   ; implicit-arg shift wasn't staged -- refuse LOUD
                      ; ⚠ t32: r12 is ALSO the far-mem/pair scratch, so a register park dies to
                      ; any far frame offset in the ez emissions -- park in a frame slot and
                      ; reload r12 as the LAST thing before the call (the sib rewrite keeps the
                      ; ld: the epilogue never touches r12).
                      (t32? g)
                       (: pko (ntmp g 'long)
                          (mkv (fnret (vty hr)) 'r0 (pf + tor0 g hr + `(`('st 'r4 pko 'r0)) +
                                    spills + ezf + pops + setal + `(`('ld pk 'r4 pko) `('callr pk)) + fres + clean + relods)))
                      (mkv (fnret (vty hr)) 'r0 (pf + tor0 g hr + `(`('mov pk 'r0)) +
                                spills + ezf + pops + setal + `(`('callr pk)) + clean + relods))))))))
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
               `(off (tor0 g rv +
                      `('(ldss f2 r0 0) '(cvtss2sd f2 f2) `('stsd 'r4 off 'f2)
                        '(ldss f2 r0 4) '(cvtss2sd f2 f2) `('stsd 'r4 (off + 8) 'f2)))
                 ty)
              `(off (tor0 g rv +
                     `('(ld r2 r0 0) `('st 'r4 off 'r2)
                       '(ld r2 r0 8) `('st 'r4 (off + 8) 'r2)))
                ty))
          (stru? ty) 'bad
          `(off (asflo g rv +
                 `(`('stsd 'r4 off 'f0) '(li r2 0) `('st 'r4 (off + 8) 'r2)))
            ty)))
   ; evaluate an EXPR and park it -> (off forms srcty) | 'bad
   (cxmat g env x)
    (: rl (clval g env x)
       rv (? (!(bad? rl) && stru? (vty rl)) rl (cgexpr g env x))
       (? (bad? rv) 'bad (cxpark g rv)))
   (cxload oa ob) `(`('ldsd 'f0 'r4 oa) `('ldsd 'f1 'r4 (oa + 8))
                    `('ldsd 'f2 'r4 ob) `('ldsd 'f3 'r4 (ob + 8)))
   (cxarith op)                                     ; f0..f3 = (a.re a.im b.re b.im) -> (re im) in (f0 f1)
    (? (op = '+) '((addsd f0 f2) (addsd f1 f3))
       (op = '-) '((subsd f0 f2) (subsd f1 f3))
       (op = '*)
        '((movsd f4 f0) (movsd f5 f1)
          (mulsd f0 f2) (mulsd f5 f3) (subsd f0 f5)                 ; re = ac - bd
          (mulsd f4 f3) (mulsd f1 f2) (addsd f1 f4))                ; im = bc + ad
       (op = '/)
        '((movsd f4 f2) (mulsd f4 f2)                               ; c^2
          (movsd f5 f3) (mulsd f5 f3) (addsd f4 f5)                 ; den = c^2 + d^2
          (movsd f5 f0) (mulsd f5 f2)                               ; ac
          (movsd f6 f1) (mulsd f6 f3) (addsd f5 f6)                 ; ac + bd
          (divsd f5 f4)                                              ; re
          (mulsd f1 f2) (mulsd f0 f3) (subsd f1 f0)                 ; bc - ad
          (divsd f1 f4) (movsd f0 f5))
       ())
   ; the (f0 f1) result into a fresh rt-typed temp; the temp's address answers
   (cxout g rt fs)
    (: ro (ntmp g rt)
       st (? (id? (cxb rt) 'float)
             (fstf g 'float 'r4 ro 'f0 + fstf g 'float 'r4 (ro + 4) 'f1)
             `(`('stsd 'r4 ro 'f0) `('stsd 'r4 (ro + 8) 'f1)))
       (mkv rt 'r0 (fs + (st + `(`('lean 'r0 'r4 ro))))))
   (cgcx g env op a rhs)                            ; the LEFT already evaluated (a value rec)
    (? !(id? (peep g 'tgt 'x64) 'x64)
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
       _ (? pk (pin g 'want pk) ())
       a (cgexpr g env lhs)
       _ (? (pk && !(!(bad? a) && id? (vreg a) pk)) (rfree g pk) ())
       (? (bad? a) 'bad
          (gcplx? (vty a)) (cgcx g env op a rhs)    ; a complex left: the complex lane (parse lifted a real RIGHT already)
          ; a constant count rides the immediate shift; a variable count
          ; rides shlv/sarv with the count PINNED to r1 (holo's contract)
          (member? op '(shl shr) && pair? g (vty a))    ; a pair shift keeps the left type
           (: kind (? (op = 'shl) 'shl (usg? (vty a)) 'shr 'sar)
              kc (knum g rhs)
              (? (two? kc)
                  (mkv (vty a) 'r0 (tor0 g a + pshiftc kind <kc))
                 (: b (cgexpr g env rhs)
                    (? (bad? b) 'bad
                       (mkv (vty a) 'r0 (tor0 g a +
                                         spush2 +
                                         tor0 g b + '((mov r2 r0)) + spop2 'r0 'r1 +
                                         pshiftv g kind))))))
          (member? op '(shl shr) && d128? (vty a))          ; a wide shift keeps the left type
           (? !(x64t? g) 'bad
              (: kind (? (op = 'shl) 'shl128 (usg? (vty a)) 'shr128 'sar128)
                 vknd (? (op = 'shl) 'shl128v (usg? (vty a)) 'shr128v 'sar128v)
                 kc (knum g rhs)
                 (? (two? kc)
                     (? ((<kc < 0) || (127 < <kc)) 'bad
                        (<kc = 0) (mkv (vty a) 'r0 (tor0 g a))
                        (mkv (vty a) 'r0 (tor0 g a + `(`(kind <kc)))))
                    (: b (cgexpr g env rhs)
                       (? (bad? b) 'bad
                          (mkv (vty a) 'r0 $
                           tor0 g a + wpush + tor0 g b +
                            '((mov r1 r0) (ld r0 sp 0) (ld r2 sp 8)) +   ; count -> cl, the pair back
                            wdrop + `(`(vknd))))))))
          (member? op '(shl shr))
           (: uL (usg? (vty a))                             ; a `>>` on an unsigned left operand is LOGICAL (shr, not sar)
              ; `<<` is the fourth carrying op: a 32-bit unsigned shifts bits straight out of
              ; its type, so it re-narrows like + - * do. `>>` never can -- a logical right
              ; shift of an already-canonical value stays inside it.
              u32 ((op = 'shl) && u32? g (vty a))
              (nar r) (? u32 `(`('zx4 r)) ())
              rt (? (u32? g (vty a)) 'uint uL 'ulong 'long) ; the shift result keeps the left operand's type
              iop (? (op = 'shl) 'shl (? uL 'shr 'sar))
              vop (? (op = 'shl) 'shlv (? uL 'shrv 'sarv))
              kc (knum g rhs)
              (? (two? kc)
                  (? (wr? g (vreg a))
                     (mkv rt (vreg a) (vfs a + `(`(iop (vreg a) <kc)) + nar (vreg a)))
                     (: rd (? wnt wnt (rdst g (vreg a)))    ; a borrowed home shifts a COPY (no 3-address shift)
                        (mkv rt rd (vfs a + `(`('mov rd (vreg a)) `(iop rd <kc)) + nar rd))))
                 (: b (cgexpr g env rhs)
                    (? (bad? b) 'bad
                       (mkv rt 'r0 (tor0 g a +
                                    spush 'r0 +
                                    tor0 g b + '((mov r1 r0)) + spop 'r0 +
                                    (`(`(vop 'r0 'r1)) + nar 'r0)))))))
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
                   wpk (? (x64t? g && d128? ta && !(callish? rhs) &&
                           member? op '(+ - * & | ^ eq ne lt le gt ge))
                          (: rl (ralloc g)
                             (? !rl ()
                                (: rh (ralloc g)
                                   (? rh `(rl rh) (: _ (rfree g rl) ())))))
                          ())
                   b (cgexpr g env rhs)
                   (? (bad? b) 'bad
                      (: tb (vty b)
                         ta9 (numty9 lhs ta)   ; C's type for a literal, read through
                         tb9 (numty9 rhs tb)   ; its written face (-1, ~0) -- see numty9
                         av (? (id? ah 'sh) a ah)
                         ; the memory shuttle: left in r1, right in r0 (today's shape)
                         (shut z9)
                          (: mid (: af9 (tor0 g av)
                                    bf (tor0 g b)
                                    (? (r1safe? bf) (af9 + '((mov r1 r0)) + bf)
                                       (af9 + spush 'r0 + bf + spop 'r1)))
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
                                 (? !(x64t? g) 'bad
                                    (pa || pb) 'bad
                                    (: rt (wbin ta tb)
                                       w? (two? wpk)
                                       midw (? w? (as128 g av + (`(`('mov <wpk 'r0) `('mov <>wpk 'r2)) + as128 g b))
                                               (as128 g av + (wpush + as128 g b)))
                                       res (? (op = '+) (mkv rt 'r0 (midw + (? w? `(`('a128r <wpk <>wpk)) ('((a128 sp)) + wdrop))))
                                          (op = '-) (mkv rt 'r0 (midw + (? w? `(`('s128r <wpk <>wpk)) ('((s128 sp)) + wdrop))))
                                          (op = '*)
                                           (: zl (? (d128? ta) (zext128? (vfs av)) (usg? ta9 || ptr? ta9))
                                              zr (? (d128? tb) (zext128? (vfs b)) (usg? tb9 || ptr? tb9))
                                              (? (zl && zr)                 ; hi halves zero: ONE mulq, the park needs only lo
                                                 (mkv rt 'r0 (? w? (as128 g av + (`(`('mov <wpk 'r0)) + (tor0 g b + `(`('mw128r <wpk)))))
                                                               (as128 g av + (wpush + (tor0 g b + ('((mw128 sp)) + wdrop))))))
                                                 (mkv rt 'r0 (midw + (? w? `(`('m128r <wpk <>wpk)) ('((m128 sp)) + wdrop))))))
                                          (member? op '(& | ^))
                                           (: w9 (irop op)
                                              (mkv rt 'r0 (midw + (? w? `(`(w9 'r0 'r0 <wpk) `(w9 'r2 'r2 <>wpk))
                                                             (`('(ld r1 sp 0) `(w9 'r0 'r0 'r1) '(ld r1 sp 8) `(w9 'r2 'r2 'r1)) + wdrop)))))
                                          (member? op '(eq ne lt le gt ge))
                                           (mkv 'long 'r0 (midw + (? w? `(`('c128r (scc op (id? rt 'u128)) <wpk <>wpk))
                                                             (`(`('c128 (scc op (id? rt 'u128)) 'sp)) + wdrop))))
                                          (member? op '(/ %))
                                           ; u128 by a divisor with a provably-zero high half only
                                           ; (dv128's two-step divq is unsigned): a widened unsigned
                                           ; word, a zero-extended pair, or a nonnegative literal
                                           (: zr (? (d128? tb) (zext128? (vfs b))
                                                    (two? rhs && id? <rhs 'num && (<= 0 <>rhs)) 1
                                                    (usg? tb9 || ptr? tb9))
                                              (? (!zr || !(id? rt 'u128)) 'bad
                                                 (mkv rt 'r0 (midw + (`(`((? (op = '/) 'dv128 'rm128) 'sp)) + wdrop)))))
                                          'bad)
                                       _ (? w? (: _ (rfree g <wpk) (rfree g <>wpk)) ())
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
                                    rt (ubin ta tb)
                                    (? (op = '+) (mkv rt 'r0 (mid2 + '((adds r0 r2 r0) (adc r1 r3 r1))))
                                       (op = '-) (mkv rt 'r0 (mid2 + '((subs r0 r2 r0) (sbc r1 r3 r1))))
                                       (op = '*)
                                        (? (v6m? g)   ; no UMULL/MLA on v6-M: libgcc's exact helper (commutative, either staging)
                                           (mkv rt 'r0 (mid2 + '((call __aeabi_lmul))))
                                           (mkv rt 'r0 (mid2 + '((imul r3 r3 r0) (mla r3 r2 r1 r3)
                                                                 (umull r0 r12 r2 r0) (add r1 r12 r3)))))
                                       (member? op '(& | ^))
                                        (: w9 (irop op)
                                           (mkv rt 'r0 (mid2 + `(`(w9 'r0 'r2 'r0) `(w9 'r1 'r3 'r1)))))
                                       (op = '/)
                                        (? (v6m? g)   ; quotient rides out in r0:r1; the SIGNED face works too (__aeabi_ldivmod)
                                           (mkv rt 'r0 (mid2d + `(`('call (? unsg '__aeabi_uldivmod '__aeabi_ldivmod)))))
                                           unsg (mkv rt 'r0 (mid2 + '((udivll)))) 'bad)
                                       (op = '%)
                                        (? (v6m? g)   ; the remainder rides r2:r3 -> the protocol pair
                                           (mkv rt 'r0 (mid2d + `(`('call (? unsg '__aeabi_uldivmod '__aeabi_ldivmod))
                                                                  '(mov r0 r2) '(mov r1 r3))))
                                           unsg (mkv rt 'r0 (mid2 + '((uremll)))) 'bad)
                                       (member? op '(eq ne lt le gt ge))
                                        (: cf (prel g op unsg)
                                           (mkv 'long 'r0 (mid2 + <>cf + `(`('set <cf 'r0)))))
                                       'bad))
                                (op = '+)
                                 ; ptr + idx at a machine scale is ONE leax -- the imul/add pair
                                 ; only for odd sizes. a VLA pointee scales by a RUNTIME stride:
                                 ; park both sides, re-evaluate the dim in this env, multiply.
                                 (? (pa && pb) 'bad
                                    (pa && gvarr? <>ta)
                                     (: vt <>ta                    ; mid ran: ptr r1, idx r0 -- park before the dim eval
                                        dv (: pk (spush 'r1 + spush 'r0)
                                              de (cgexpr g env (varn vt))
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
                                              de (cgexpr g env (varn vt))
                                              (? (bad? de) 'bad
                                                 (: es (tsz g (varel vt))
                                                    (mid + pk + asint g de +
                                                     (? (1 < es) (imby g 'r0 es) ()) +
                                                     spop 'r2 + '((imul r2 r2 r0)) +
                                                     spop 'r1 + '((add r1 r1 r2) (mov r0 r1))))))
                                        (? (bad? dv) 'bad (mkv tb 'r0 dv)))
                                    pa (: sz (tsz g <>ta)
                                          (? (member? sz '(1 2 4 8))
                                             (mkv ta 'r0 (mid + `(`('leax 'r0 'r1 'r0 sz 0))))
                                             (mkv ta 'r0 (mid + (? (1 < sz) (imby g 'r0 sz) ()) +
                                                          ('((add r1 r1 r0)) + back)))))
                                    pb (: sz (tsz g <>tb)
                                          (? (member? sz '(1 2 4 8))
                                             (mkv tb 'r0 (mid + `(`('leax 'r0 'r0 'r1 sz 0))))
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
                                                              (pow2? esz) (back + `(`('sar 'r0 (lg2 esz))))
                                                              `(`('li 'r0 esz) '(div r1 r1 r0) '(mov r0 r1))))))
                                        'bad)
                                    pa (mkv ta 'r0 (mid + (? (1 < tsz g <>ta)
                                                         (imby g 'r0 (tsz g <>ta)) ()) +
                                                    ('((sub r1 r1 r0)) + back)))
                                    pb 'bad
                                    (mkv nty 'r0 (mid + '((sub r1 r1 r0)) + back + nar)))
                                (: tail (? (member? op '(* & | ^)) `(`((irop op) 'r1 'r1 'r0))
                                           ; a mixed int/uint divide runs at uint width: znrw first
                                           (op = '/) (znrw g ta9 tb9 'r1 'r0 + `(`((? unsg 'udiv 'div) 'r1 'r1 'r0)))
                                           (op = '%) (znrw g ta9 tb9 'r1 'r0 + `(`((? unsg 'urem 'rem) 'r1 'r1 'r0 'r3)))
                                           'bad)
                                   m32 (u32 && (op = '*))              ; only the multiply here can carry
                                   (? !(bad? tail)
                                      (mkv (rbin g ta9 tb9) 'r0
                                           (mid + tail + back + (? m32 nar ())))
                                      (member? op '(eq ne lt le gt ge))
                                       (mkv 'long 'r0 (mid + znrw g ta9 tb9 'r1 'r0 +
                                                       `('(cmp r1 r0) `('set (scc op unsg) 'r0))))
                                      'bad))))
                         iml (? (iml? && !(flo? tb)) (immop g (rflip op) b tb <kl <>kl wnt) 'no)
                         (? !(id? iml 'no) iml
                            ((flo? ta || flo? tb) && (d128? ta || d128? tb)) 'bad           ; wide meets double: refuse
                            (flo? ta || flo? tb)
                             ; the double lane: either operand double -> both to double (f1=left,
                             ; f0=right through the gp stack), then the scalar-double op or ucomisd.
                             (: mid (? (t32? g)               ; the gp stack moves PAIRS on t32
                                       (asflo g av + ('((movqrx r0 f0)) + spush2) +
                                        (asflo g b + spop2 'r2 'r3 + '((movqxr f1 r2))))
                                       (asflo g av + ('((movqrx r0 f0)) + spush 'r0) +
                                        (asflo g b + spop 'r1 + '((movqxr f1 r1)))))
                                (? (op = '+) (mkv 'double 'r0 (mid + '((addsd f0 f1))))
                                   (op = '*) (mkv 'double 'r0 (mid + '((mulsd f0 f1))))
                                   (op = '-) (mkv 'double 'r0 (mid + '((subsd f1 f0) (movsd f0 f1))))
                                   (op = '/) (mkv 'double 'r0 (mid + '((divsd f1 f0) (movsd f0 f1))))
                                   ; IEEE compare: UNORDERED (a NaN operand) answers false for
                                   ; every relation and true only for !=. x64: above/ae read CF=0,
                                   ; which unordered sets, so < and <= COMPARE SWAPPED, and ==/!=
                                   ; carry the parity bit explicitly. arm64: ⚠ FCMP's gt/ge are
                                   ; unordered-false natively (HI/HS would read C, which unordered
                                   ; SETS), and p/np alias vs/vc, so eq/ne ride whole.
                                   (member? op '(lt le gt ge))   ; lt/le compare SWAPPED so the sense rides gt/ge
                                    (: swp (member? op '(lt le))
                                       (mkv 'long 'r0 (mid + `(`('ucomisd (? swp 'f0 'f1) (? swp 'f1 'f0))
                                                              `('set (? (member? op '(lt gt)) (? (arm? g) 'gt 'above)
                                                                        (arm? g) 'ge 'ae) 'r0)))))
                                   (op = 'eq)
                                    (mkv 'long 'r0 (mid + ('((ucomisd f1 f0)) + fsete)))
                                   (op = 'ne)
                                    (mkv 'long 'r0 (mid + ('((ucomisd f1 f0)) + fsetn)))
                                   'bad))
                            ; the REGISTER lane: both values live, 3-address combine
                            (: pa (ptr? ta) pb (ptr? tb)
                               ; a live ptr + live idx at a machine scale: ONE leax, no shuttle
                               ; (lea reads both sources before writing -- any alias). a VLA
                               ; pointee has no STATIC scale, so it never takes the leax
                               vpt ((pa && gvarr? <>ta) || (pb && gvarr? <>tb))
                               lxsz (? !(op = '+) () (pa && pb) () vpt ()
                                       pa (: s9 (tsz g <>ta) (? (member? s9 '(1 2 4 8)) s9 ()))
                                       pb (: s9 (tsz g <>tb) (? (member? s9 '(1 2 4 8)) s9 ()))
                                       ())
                               (? (id? ah 'sh) (shut ())
                                  vpt (shut ())
                                  ((pa || pb) && !lxsz) (shut ())
                                  (t32? g && llt? tb) (shut ())       ; a pair right: the pair shuttle (widen the park)
                                  (d128? tb) (shut ())                ; a wide right: the wide shuttle (widen the park)
                                  lxsz
                                   (: rA (vreg ah) rB (vreg b)
                                      rd (? wnt wnt (rdst g rA))
                                      fs (vfs ah + vfs b
                                          + `(`('leax rd (? pa rA rB) (? pa rB rA) lxsz 0)))
                                      _ (rfree g rB) _ (? (id? rd rA) () (rfree g rA))
                                      (mkv (? pa ta tb) rd fs))
                                  (: rA (vreg ah) rB (vreg b)
                                     cunsg (cmpu g ta9 tb9)
                                     pre (vfs ah + vfs b)
                                     (? (member? op '(eq ne lt le gt ge))
                                         ; a mixed int/uint compare owes a zx4 narrow, and rA/rB
                                         ; may be HOMES -- the shuttle owns the copies
                                         (? (two? (znrw g ta9 tb9 'r1 'r0)) (shut ())
                                            (: rd (? wnt wnt (rdst g rA))   ; set runs after the cmp: rd may alias rB
                                               fs (pre + `(`('cmp rA rB) `('set (scc op cunsg) rd)))
                                               _ (rfree g rB) _ (? (id? rd rA) () (rfree g rA))
                                               (mkv 'long rd fs)))
                                        (member? op '(+ - * & | ^))
                                         (: iop2 (irop op)
                                            rd (? (wnt && !(id? wnt rB)) wnt (rdst g rA))
                                            u32 (member? op '(+ - *) && u32bin? g ta9 tb9)
                                            ; holo reorders a commutative rd==rb; sub CANNOT (alias-dst) --
                                            ; a borrowed left + dry pool can land rd on rB's r0
                                            (? ((op = '-) && id? rd rB) (shut ())
                                               (: fs (pre + `(`(iop2 rd rA rB)) + (? u32 `(`('zx4 rd)) ()))
                                                  _ (rfree g rB) _ (? (id? rd rA) () (rfree g rA))
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
   sf-bin #('addsd '__aeabi_dadd 'subsd '__aeabi_dsub
            'mulsd '__aeabi_dmul 'divsd '__aeabi_ddiv)
   ; VCMP-flavored cc -> (helper sense): sense 'ne = helper-true means the cc
   ; holds; 'eq = helper-false (the negated relations, unordered-TRUE like the
   ; flags they replace: le = !(a>b), lt = !(a>=b))
   (sf-cc cc) (? (cc = 'gt) '(__aeabi_dcmpgt ne)
                 (cc = 'ge) '(__aeabi_dcmpge ne)
                 (cc = 'eq) '(__aeabi_dcmpeq ne)
                 (cc = 'ne) '(__aeabi_dcmpeq eq)
                 (cc = 'le) '(__aeabi_dcmpgt eq)
                 (cc = 'lt) '(__aeabi_dcmpge eq)
                 ())
   sf-save '((sub sp sp 32) (st sp 0 r0) (st sp 4 r1) (st sp 8 r2) (st sp 12 r3))
   sf-rest '((ld r0 sp 0) (ld r1 sp 4) (ld r2 sp 8) (ld r3 sp 12) (add sp sp 32))
   (sf-slot r) (? (id? r 'r0) 0 (id? r 'r1) 4 (id? r 'r2) 8 (id? r 'r3) 12 -1)
   ; the v6-M SOFT-DOUBLE pass: no FPU registers AT ALL (the M0+/RP2040 face). the f0/f1/f15
   ; fiction rewrites onto the three frame cells cgfn pinned (fc0/fc1/fc15). every f64
   ; arithmetic/compare/convert lowers to its __aeabi_* call (sp64's helpers and fused-compare
   ; dance); the TRANSFERS -- no vldr/vstr/vmov exists -- become word loads/stores through a saved
   ; low temp (r3, or r2 when the base IS r3; mem ops never touch flags, so a transfer between a
   ; cmp and its consumer stays sound), an sp-based one offsetting across the temp's save cell.
   ; ⚠ the cells mirror register semantics only WITHIN a function: every cross-function f-reg
   ; convention was re-routed onto gp pairs upstream, so no cell ever crosses a frame.
   (fcell g f) (? (id? f 'f0) (peep g 'fc0 0) (id? f 'f1) (peep g 'fc1 0)
                  (id? f 'f15) (peep g 'fc15 0) (scare 'soften6-freg f))
   (nxr r) (? (id? r 'r0) 'r1 (id? r 'r1) 'r2 (id? r 'r2) 'r3 (scare 'soften6-pair r))
   (pl6 g f lo) (: c (fcell g f) `(`('ld lo 'r4 c) `('ld (nxr lo) 'r4 (c + 4))))   ; cell -> gp pair
   (ps6 g f lo) (: c (fcell g f) `(`('st 'r4 c lo) `('st 'r4 (c + 4) (nxr lo))))   ; gp pair -> cell
   (m2c g f base off)                                ; [base+off] -> cell (a softened ldsd)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `('(sub sp sp 16) `('st 'sp 0 tmp)
         `('ld tmp base o2) `('st 'r4 c tmp) `('ld tmp base (o2 + 4)) `('st 'r4 (c + 4) tmp)
         `('ld tmp 'sp 0) '(add sp sp 16)))
   (c2m g base off f)                                ; cell -> [base+off] (a softened stsd)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `('(sub sp sp 16) `('st 'sp 0 tmp)
         `('ld tmp 'r4 c) `('st base o2 tmp) `('ld tmp 'r4 (c + 4)) `('st base (o2 + 4) tmp)
         `('ld tmp 'sp 0) '(add sp sp 16)))
   (w2c g f base off)                                ; [base+off] -> cell LOW word (a softened ldss)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `('(sub sp sp 16) `('st 'sp 0 tmp)
         `('ld tmp base o2) `('st 'r4 c tmp)
         `('ld tmp 'sp 0) '(add sp sp 16)))
   (c2w g base off f)                                ; cell LOW word -> [base+off] (a softened stss)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `('(sub sp sp 16) `('st 'sp 0 tmp)
         `('ld tmp 'r4 c) `('st base o2 tmp)
         `('ld tmp 'sp 0) '(add sp sp 16)))
   ; ONE walker serves both soft-double faces; the TRANSFER KIT is the whole difference. v6m: a
   ; double lives in its frame cell -- gd/pd ride pl6/ps6, the single-word bits read/write the
   ; cell low, movsd copies cell-to-cell through the saved temp, and the raw f-memory ops soften
   ; too. sp64: a double lives in its d-register -- gd/pd are movqrx/movqxr, the single-word bits
   ; bounce through the save cell's spare word, movsd bounces the pair (no VMOV.F64 on SP), and
   ; f-memory ops stay (the FPU loads/stores fine, it just can't ARITH f64).
   (soften g fs)
    (: w6 (v6m? g)
       (? !(w6 || sp64? g) fs
          (: (gd d9 lo) (? w6 (pl6 g d9 lo) `(`('movqrx lo d9)))   ; double -> the gp pair at lo
             (pd d9) (? w6 (ps6 g d9 'r0) `(`('movqxr d9 'r0)))   ; the r0:r1 pair -> double
             (gs d9) (? w6 `(`('ld 'r0 'r4 (fcell g d9)))         ; SINGLE bits -> r0
                        `(`('stss 'sp 16 d9) '(ld r0 sp 16)))
             (ps d9) (? w6 `(`('st 'r4 (fcell g d9) 'r0))         ; r0 -> the single's home
                        `('(st sp 16 r0) `('ldss d9 'sp 16)))
             (mv dD dS)                                           ; movsd: double -> double
              (? w6 (: cd (fcell g dD) cs (fcell g dS)
                       `('(sub sp sp 16) '(st sp 0 r3)
                         `('ld 'r3 'r4 cs) `('st 'r4 cd 'r3)
                         `('ld 'r3 'r4 (cs + 4)) `('st 'r4 (cd + 4) 'r3)
                         '(ld r3 sp 0) '(add sp sp 16)))
                 `('(sub sp sp 16) `('stsd 'sp 0 dS) `('ldsd dD 'sp 0) '(add sp sp 16)))
             scc (? w6 'soften6-cc 'soften-cc)
             suc (? w6 'soften6-ucomisd 'soften-ucomisd)
             (go l)
              (? !(two? l) ()
                 (: f <l r >l
                    (? !(two? f) (f >< go r)
                       (: op <f
                          h9 (peep sf-bin op ())
                          (? h9                             ; (op fD fS): fD = fD op fS
                              (sf-save + gd <>f 'r0 +
                               gd <>>f 'r2 + `(`('call h9)) + pd <>f + sf-rest + go r)
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
                                  (? (sl < 0) `(`('mov 'r0 <>>f)) `(`('ld 'r0 'sp sl))) +
                                  `(`('call (? (op = 'cvtsi2sd) '__aeabi_i2d '__aeabi_ui2d))) +
                                  pd <>f +
                                  sf-rest + go r))
                             (member? op '(cvttsd2si cvttsd2ui))   ; (op rD fS)
                              (: sl (sf-slot <>f)
                                 (sf-save + gd <>>f 'r0 +
                                  `(`('call (? (op = 'cvttsd2si) '__aeabi_d2iz '__aeabi_d2uiz))) +
                                  (? (sl < 0) `(`('mov <>f 'r0)) `(`('st 'sp sl 'r0))) +   ; through the save slot: the restore lands it
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
                                        (args + `(`('call <hs) '(cmp r0 0)) + sf-rest + `(`('br <>hs <>>n1)) + go >r))
                                    (two? n1 && id? <n1 'set)
                                     (: n2 (? (two? >r) <>r ())
                                        n3 (? (two? >r && (two? >>r)) <>>r ())
                                        dance (two? n2 && id? <n2 'set && member? <>n2 '(p np) &&
                                               two? n3 && member? <n3 '(and or))
                                        hs (sf-cc <>n1)
                                        _ (? (two? hs) 0 (scare scc n1))
                                        (args + `(`('call <hs) '(cmp r0 0)) + sf-rest + `(`('set <>hs <>>n1)) + go (? dance >>>r >r)))
                                    (scare suc n1)))
                             (f >< go r))))))
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
   (rdsp f)                                         ; -> (L reads defs pure?) | 'bar
    (: op <f
       (rg1 x) (? (charm? x) () `(x))
       (? (op = 'li) `(() `(<>f) 1)
          (op = 'mov) `(`(<>>f) `(<>f) 1)
          (member? op aluops)
           (? (<>f = 'sp) `(() () ())      ; sp motion touches no gp register
              `((rg1 <>>f + rg1 <>>>f) `(<>f) 1))
          (op = 'push) `(`(<>f) () ())
          (op = 'pop) `(() `(<>f) ())
          (member? op '(shl shr sar rol ror)) `(`(<>f) `(<>f) 1)
          (member? op ldops9) `(`(<>>f) `(<>f) 1)
          (member? op gstops) `(`(<>f <>>>f) () ())
          (member? op stxops) `(`(<>f <>>f (nthr f 5)) () ())
          (member? op siops) `(`(<>f) () ())
          (op = 'cmpm) `((`(<>f) + rg1 <>>>f) () ())
          (op = 'cmprm) `(`(<>f <>>f) () ())
          (member? op leaops) `(`(<>>f) `(<>f) 1)
          (member? op xops) `(`(<>>f <>>>f) `(<>f) 1)
          (op = 'la) `(() `(<>f) 1)
          (op = 'set) `(() `(<>>f) 1)
          (member? op '(cmp test)) `((rg1 <>f + rg1 <>>f) () ())
          (member? op unops) `(`(<>f) `(<>f) 1)
          (op = 'movqxr) `(`(<>>f) () ())
          (op = 'movqrx) `(() `(<>f) 1)
          (op = 'cvtsi2sd) `(`(<>>f) () ())
          (op = 'cvttsd2si) `(() `(<>f) ())
          (member? op fldops) `(`(<>>f) () ())
          (member? op fstops) `(`(<>f) () ())
          (member? op xmmops) `(() () ())
          (member? op divops)
           `((rg1 <>>f + rg1 <>>>f) '(r0 r1 r2 r3) ())
          (member? op vshops) `(`(<>f <>>f) `(<>f 'r1) ())
          'bar))
   ; the arm64 RETARGET: gen speaks r4 for the frame base throughout, but ⚠ x4 is ALSO an AAPCS
   ; argument register -- so the rename is by POSITION, not by symbol: only the operand slot where
   ; r4 means "the frame base" becomes fp. an r4 anywhere else IS the 5th argument and stays.
   a4base (: t (tablet 0)
             _ (foldl (\ _ o (pin t o 2)) 0 ((ldops9 + fldops) + (xops + (leaops + '(cmprm)))))
             _ (foldl (\ _ o (pin t o 1)) 0 ((stops9 + stxops) + (siops + '(cmpm))))
             t)
   (a4ize fs)
    (: (swap f i bp)
        (? !(two? f) ()
           (link (? ((i = bp) && id? <f 'r4) 'fp <f)
                 (swap >f (i + 1) bp)))
       (go l)
        (? !(two? l) ()
           (: f <l
              bp (? (two? f) (peep a4base <f -1) -1)
              (link (? (bp < 0) f (swap f 0 bp))
                    (go >l))))
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
   (t16slots fs fr)
    (? !(two? fs) ()
       (: f <fs
          bp (? (two? f) (peep a4base <f -1) -1)
          (link (? ((bp >= 0) && id? (nthr f bp) 'fp && (nthr f (bp + 1) < 0)) (t16reb f 0 bp fr) f)
                (t16slots >fs fr))))
   ; v6m has no MOVW/MOVT to build an address, so an (la Rd lbl) becomes a literal-pool load:
   ; align to a word, LDR Rd,[pc,#0], hop the pooled abs32 word, and lay it as an R_ARM_ABS32 the
   ; linker fills. runs LAST (after a4ize + t16slots), v6m only; other targets keep `la`.
   (t16la fs)
    (? !(two? fs) ()
       (: f <fs
          ((? (two? f && id? <f 'la)
              `('(align 4) `('lapool <>f) `('raw `(`('fix 4 'abs32 <>>f 0))))
              `(f))
           + t16la >fs)))

   ; -- the SIBCALL pass: a RET-position call tail-JUMPS instead, so deep tail recursion runs flat
   ; and the lvm shape Continues by jmp at ai_tco=1 (`make vmret` reads the disassembly). the
   ; rewrite is LOCAL: (call F) followed by exactly the epilogue becomes epilogue + (jmp F) -- the
   ; epilogue parks sp on the return address, a call's entry state, so the callee answers our
   ; caller directly. (callr r3) parks the target in r10 first (the epilogue reloads r3). anything
   ; BETWEEN the call and the epilogue keeps the plain call. ⚠ GATED per fn: any (lea _ r4 _)
   ; means a frame address ESCAPES and the frame must outlive every call, so that fn keeps all its
   ; calls. the escape is tracked at the SOURCE (g 'fesc): clval's lea for an ordinary assignment
   ; is consumed by its own store and stays eligible -- only an address that becomes a VALUE pins.
   (leas? fs) (? !(two? fs) ()
                 (: f <fs
                    (? (two? f && <f = 'lea && <>>f = 'r4) 1
                       (leas? >fs))))
   (escan g r) (: _ (? (two? r && leas? (vfs r)) (pin g 'fesc 1) ()) r)
   (unlean fs) (? !(two? fs) ()                   ; & makes a consumed address ESCAPE: lean -> lea, so the gate sees it
                  (: f <fs
                     (link (? (two? f && <f = 'lean) ('lea >< >f) f)
                           (unlean >fs))))
   (relean fs) (? !(two? fs) ()                   ; the inverse, for a splice that consumes the address ITSELF (the overflow trio's &t)
                  (: f <fs
                     (link (? (two? f && (<f = 'lea)) ('lean >< >f) f)
                           (relean >fs))))
   (epim? fs es) (? !(two? es) 1                  ; the next forms are EXACTLY es (the target's epilogue)
                    !(two? fs) ()
                    ((<fs = <es) && epim? >fs >es))
   ; lw = the WINDOW rule: a frame lea seen since the last call boundary means the candidate's OWN
   ; staging may hand the callee a frame address (`return f(&x)`) -- that call stays a call, and
   ; the boundary resets the window. a ternary's JOIN LABEL may sit between the call and the
   ; epilogue: the jump sequence lands BEFORE the labels, and label + epi stay for the other arm.
   (lbl-epi? fs ej) (? !(two? fs) ()
                       (two? <fs && <<fs = 'label)
                        (? (epim? >fs ej) 1 (lbl-epi? >fs ej))
                       ())
   ; a JOIN label and then a jmp to a collected epilogue label: the call is still in ret position
   ; (labels carry no code, the jmp leads to the epilogue), so it rewrites like the bare-jmp case,
   ; keeping label + jmp for the arms that join.
   (lbl-jmp? fs el) (? !(two? fs) ()
                       (: f <fs
                          (? (two? f && <f = 'label) (lbl-jmp? >fs el)
                             (two? f && <f = 'jmp && peep el <>f ()) 1
                             ())))
   ; a nested-ternary return leaves each arm as (call F)(jmp JOIN) with JOIN leading to the
   ; epilogue: elabs collects those epilogue labels first, and a candidate followed by (jmp X),
   ; X collected, rewrites too. a label whose region is one (jmp Y) with Y collected is an
   ; epilogue label too, and that chain can point FORWARD -- so elabsx iterates elabs to a
   ; fixpoint (the set only grows, bounded by the fn's labels).
   (jmp-el? fs t) (? !(two? fs) ()
                     (: f <fs (two? f && <f = 'jmp && peep t <>f ())))
   (elabs fs t ej) (? !(two? fs) t
                      (: f <fs
                         _ (? (two? f && (<f = 'label) &&
                               (epim? >fs ej ||
                                lbl-epi? >fs ej || jmp-el? >fs t))
                              (pin t <>f 1) ())
                         (elabs >fs t ej)))
   (elabsx fs t ej) (: n (tally (keys t))
                       t2 (elabs fs t ej)
                       (? (tally (keys t2) = n) t2 (elabsx fs t2 ej)))
   (butret es) (? !(two? >es) () (<es >< butret >es))          ; the epilogue's restore forms (the ret drops)
   (sibjmp g f)
    ; ⚠ riscv takes the DIRECT tail call through the park too, where the other targets ride `jmp`:
    ; a tail call is the one jmp that leaves its function, and riscv's jal reaches only +-1MB
    ; (rv-jal-range). so it borrows `la`'s reach (+-2GB) and the same t4 park the INDIRECT lane
    ; uses, loaded AFTER the restores so nothing has to survive them.
    (? (rv? g && (<f = 'call))
       (butret (peep g 'epi epi-a64) + `(`('la (pkr g) <>f) `('jmpr (pkr g))))
       (<f = 'call) (butret (peep g 'epi epi) + `(`('jmp <>f)))
       (a64? g) (butret epi-a64 + '((jmpr r15)))               ; the arm64 epilogue never touches the x15 park
       (rv? g) (butret (peep g 'epi epi-a64) + `(`('jmpr (pkr g))))     ; riscv: the park (t4) survives the epilogue
       (t32? g) (butret epi-a64 + `(`('jmpr (pkr g))))         ; thumb2: the callr target parks in r12 (pkr), untouched by the epilogue
       ('((mov r10 r3)) + butret (peep g 'epi epi) + '((jmpr r10))))            ; park first: the x64 epilogue reloads r3
   ; a home reload run ((ld rH r4 slot)..) after a ret-position call is DEAD --
   ; the tail-jump hands the frame back first. skiprel peels exactly those.
   (skiprel fs hs)
    (? !(two? fs) fs
       (: f <fs
          (? (two? f && (<f = 'ld) && (<>>f = 'r4))
              (: h (assoc <>f hs)
                 (? (two? h && (<>h = <>>>f)) (skiprel >fs hs) fs))
             fs)))
   (sibs g fs lw el hs ej crf)
    (? !(two? fs) ()
       (: f <fs r >fs
          lw2 (? (two? f && (<f = 'lea) && (<>>f = 'r4)) 1 lw)
          cand (!lw2 && two? f && (((<f = 'call) && two? >f) ||
                                     ((<f = 'callr) && id? <>f <>crf)))
          rr (? cand (skiprel r hs) r)
          (? (cand && epim? rr ej)
              (sibjmp g f + sibs g (drop (tally ej) rr) () el hs ej crf)
             (cand && lbl-epi? rr ej)
              (sibjmp g f + sibs g rr () el hs ej crf)
             (cand && (: nx <rr (two? nx && <nx = 'jmp && peep el <>nx ())))
              (sibjmp g f + sibs g >rr () el hs ej crf)
             (cand && lbl-jmp? rr el)
              (sibjmp g f + sibs g rr () el hs ej crf)
             (: _ (? (must? f) (scare 'musttail-not-a-tail `((g 'fn) f)) 0)   ; a marked call is OWED its jump
                (f >< sibs g r (? (two? f && member? <f '(call callr raw)) () lw2) el hs ej crf)))))       ; raw: a builtin splice consumes its address like a call
   ; -- the musttail contract: a PRE-slotted ret (clay's (ret e PRE)) marks its call --
   ; (call F must) / (callr PK must) -- and sibs spells the marked jump or SCARES
   ; (musttail-not-a-tail; a whole-fn decline musttail-escape) instead of falling back to a
   ; plain call. the marker dies with the rewrite, so no later pass ever meets one. --
   (must? f) (two? f && member? <f '(call callr) && member? 'must >>f)
   (anymust? fs) (? !(two? fs) () (must? <fs || anymust? >fs))
   (mustret g fs)                                   ; mark the LAST call/callr; none to mark refuses
    (: (go l) (? !(two? l) 'no
                 (: r (go >l)
                    (? !(id? r 'no) (<l >< r)
                       (: f <l (? (two? f && member? <f '(call callr)) (f + '(must) >< >l) 'no)))))
       r (go fs)
       (? (id? r 'no) (nolane g "a musttail return does not end in a call") r))
   (sibcall g fs) (? (peep g 'fesc () || v6m? g)   ; v6m: the pop-pc epilogue can't restore lr for a tail-jmp, so no sibcall (deferred optimization)
                     (: _ (? (anymust? fs) (scare 'musttail-escape (g 'fn)) 0) fs)
                     (: ej (peep g 'epi epi)
                        (sibs g fs () (elabsx fs (tablet 0) ej) (peep g 'homes ()) ej `('callr (pkr g)))))

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
   uoff (: t (tablet 0)
           _ (foldl (\ _ o (pin t o 3)) 0 (ldops9 + fldops + leaops + '(cmprm)))
           _ (foldl (\ _ o (pin t o 5)) 0 xops)
           _ (foldl (\ _ o (pin t o 2)) 0 (stops9 + siops + '(cmpm)))
           _ (foldl (\ _ o (pin t o 4)) 0 stxops)
           t)
   (unframe g fs)
    (: a64 (a64? g)
       fb (? a64 'fp 'r4)
       pro (? a64
              (nthr fs 1 = '(push lr) && nthr fs 2 = '(push fp) && nthr fs 3 = '(lea fp sp 0))
              (nthr fs 1 = '(push r4) && nthr fs 2 = '(mov r4 sp)))
       sf (nthr fs (? a64 4 3))
       K (? (two? sf && <sf = 'sub && <>sf = 'sp && <>>sf = 'sp && charm? <>>>sf)
            <>>>sf ())
       body0 (drop (? a64 5 4) fs)
       ; -- the PARK lane (x64): a callr sib parks its target in r3 and unparks with
       ; (mov r10 r3); when everything between them neither mentions r3 nor rewrites the parked
       ; SOURCE, the pair collapses to one mov at the unpark's seat -- the fn then never WRITES
       ; r3, so the caller's rbx rides the tail-jump untouched and the rbx save + each restore's
       ; leading (ld r3 r4 -8) drop with the frame. a residual r3 keeps them as sp slots. --
       sv3 (!a64 && (nthr body0 0 = '(st r4 -8 r3)))
       wind (('(mov li) + leaops) + (xops + (aluops + ldops9)))
       wina ((wind + gstops) + '(cmp test))
       (squeeze l)
        (? !(two? l) ()
           (: f <l r >l
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
                       (: (take w n) (? (n < 1) (`('mov 'r10 X) >< squeeze (drop 1 w))
                                        (<w >< take >w (n - 1)))
                          (take r k)))))))
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
       labt (tablet 0)                               ; label -> the D every arrival agrees on
       (scan l D vs r3f live)                        ; -> 'no | (needs-sub r3-in-use)
        (? !(two? l) `(vs r3f)
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
                           !(0 = (& D 15)) 'no
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
              (: (go e i) (? !(two? e) ()
                             (link (? (i = bp) 'sp (i = op9) (<e + K + D) <e)
                                   (go >e (i + 1))))
                 (go f2 0))
             (rw l D)
              (? !(two? l) ()
                 (: n3 (epi3? l)
                    (? n3 ((? r3f `(`('ld 'r3 'sp ((K + D) - 8))) ())
                           + ((? vs `(`('add 'sp 'sp (K9 + D))) (? (0 < D) `(`('add 'sp 'sp D)) ()))
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
             (<fs >< (? vs (`('sub 'sp 'sp K9) >< rw body2 0) (rw body2 0))))))

   ; -- IR constant propagation (cfoldir): a straight-line abstract pass over a function's final
   ; forms. it tracks known small constants in registers, frame slots and machine-stack cells,
   ; folds ALU ops on them back to li's, turns reg-reg ALU with a known source into the imm form,
   ; folds loads of known slots, and evaluates known cmp+br / cmp+set pairs (a constant ternary
   ; drops its dead arm) -- so the sizeof/Width/Have arithmetic the inliner exposes collapses.
   ; knowns stay in [0, 2^30] (compare signedness and shifts agree there); labels and calls
   ; invalidate values; anything unrecognized invalidates EVERYTHING (new ops stay safe). --
   kmax 1073741824
   (kok? v) ((0 <= v) && (v < kmax))
   (cfoldir fs0)
    (: (fresh z9) (tablet 0)                        ; reg/slot/cell -> (1 K) const | (2 rX) copy-of-register | ()
       blunt9 (vshops + unops)                      ; folded bluntly (see their arm below)
       (rv t k) (peep t k ())
       (rset t k v) (pin t k v)
       (kn? v) (two? v && (<v = 1))            ; a known CONSTANT (never let a copy reach arithmetic)
       ; resolve a source register to its deepest value: a const rides, a copy
       ; chases to its ROOT register (valid until that register is written)
       (rsv rg s) (: v (rv rg s) (? (two? v) v `(2 s)))
       ; the copy INDEX ci: source register -> the (tag key) sites holding a copy of it. ⚠ a WRITE
       ; to d must invalidate every copy-of-d anywhere -- the index visits only the recorded sites
       ; (each validated against the live tablet, so a stale entry is harmless)
       (crec ci v tg k) (? (two? v && <v = 2)
                           (pin ci <>v (`(tg k) >< peep ci <>v ())) ())
       (kild ci rg sl ce d)
        (: (go1 es) (? !(two? es) ()
                       (: e <es t (? (<e = 'g) rg (<e = 's) sl ce)
                          v (peep t <>e ())
                          _ (? (two? v && (<v = 2) && (<>v = d)) (pin t <>e ()) ())
                          (go1 >es)))
           _ (go1 (peep ci d ()))
           (pin ci d ()))
       (defr ci rg sl ce d) (: _ (rset rg d ()) (kild ci rg sl ce d))
       ; rebuild the index for freshly JOINED tablets (small: agreed entries only)
       (mkci rg sl ce)
        (: ci (tablet 0)
           (scan1 t tg ks) (? !(two? ks) ()
                              (: _ (crec ci (peep t <ks ()) tg <ks) (scan1 t tg >ks)))
           _ (scan1 rg 'g (keys rg)) _ (scan1 sl 's (keys sl)) _ (scan1 ce 'c (keys ce))
           ci)
       (copyt t) (: t2 (tablet 0)
                    (go1 ks) (? !(two? ks) t2
                                (: _ (pin t2 <ks (peep t <ks ())) (go1 >ks)))
                    (go1 (keys t)))
       ; the JOIN of two abstract states: a key survives only where both sides
       ; know it and agree (the meet over the flat lattice) -- a fresh tablet
       (joint ta tb) (: t2 (tablet 0)
                        (go1 ks) (? !(two? ks) t2
                                    (: k1 <ks va (peep ta k1 ()) vb (peep tb k1 ())
                                       _ (? (two? va && two? vb &&
                                             (<va = <vb) && (<>va = <>vb))
                                            (pin t2 k1 va) ())
                                       (go1 >ks)))
                        (go1 (keys ta)))
       (alu op a b) (? (op = 'add) (a + b) (op = 'sub) (a - b) (op = 'imul) (* a b)
                       (op = 'and) (a & b) (| a b))  ; xor stays unfolded (imm-converted only)
       (ccev cc a b) (? (cc = 'eq) (a = b) (cc = 'ne) !(a = b)
                        (cc = 'lt || cc = 'below) (a < b)
                        (cc = 'le || cc = 'be) (a <= b)
                        (cc = 'gt || cc = 'above) (b < a)
                        (b <= a))                    ; ge / ae
       ; after an unconditional transfer, everything to the next label is DEAD
       ; (a jump can only land on a label) -- a folded ternary drops its arm
       (swallow fs) (? !(two? fs) ()
                       (: f <fs (? (two? f && <f = 'label) fs (swallow >fs))))
       (atlab? fs l) (two? fs && (: f <fs (two? f && <f = 'label && <>f = l)))
       ; back-edge labels (a jmp/br sitting BELOW its target): a loop head joins
       ; an edge this one linear pass never computes -- assume nothing there
       backs (: seen (tablet 0) bt (tablet 0)
                (bscan l) (? !(two? l) bt
                             (: f <l
                                _ (? !(two? f) ()
                                     (<f = 'label) (pin seen <>f 1)
                                     (<f = 'jmp) (? (peep seen <>f ()) (pin bt <>f 1) ())
                                     (<f = 'br) (? (peep seen <>>f ()) (pin bt <>>f 1) ())
                                     ())
                                (bscan >l)))
                (bscan fs0))
       ; the pending in-states: every FORWARD jmp/br to L joins its state into pend[L]; L's label
       ; reads it back and joins the fall-through, so a known value CROSSES a label. ⚠ the sp
       ; cursor must agree or the cells drop.
       pend (tablet 0)
       (pmerge l cur rg sl ce ci)
        (: old (peep pend l ())
           (? !(two? old) (pin pend l `(cur (copyt rg) (copyt sl) (copyt ce) (copyt ci)))
              (: rg2 (joint <>old rg) sl2 (joint <>>old sl)
                 nce (? (<old = cur) (joint <>>>old ce) (fresh 0))
                 (pin pend l `(<old rg2 sl2 nce (mkci rg2 sl2 nce))))))
       ; lv: linearly LIVE -- () right after an unconditional transfer (the
       ; label then reads only its jump-in state, never the stale linear one)
       (go fs lv cur rg sl ce ci acc)
        (? !(two? fs) (rev acc)
           (: f <fs r >fs
              op (? (two? f) <f ())
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
                                 _ (rset rg <>f (? (kok? k) `(1 k) ()))
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
                        bk (? (charm? b) (? (kok? b) `(1 b) ()) (rv rg b))
                        ak (rv rg a)
                        (? (!(op = 'xor) && kn? ak && kn? bk)           ; both known: the op folds to a li
                            (: k (alu op <>ak <>bk)
                               _ (defr ci rg sl ce d)
                               (? (kok? k)
                                  (: _ (rset rg d `(1 k))
                                     (go r 1 cur rg sl ce ci (`('li d k) >< acc)))
                                  (go r 1 cur rg sl ce ci (f >< acc))))
                           (!(charm? b) && kn? bk)        ; known reg source -> the imm form
                            (: _ (defr ci rg sl ce d)
                               (go r 1 cur rg sl ce ci (`(op d a <>bk) >< acc)))
                           (: _ (defr ci rg sl ce d)
                              (go r 1 cur rg sl ce ci (f >< acc))))))
                 (member? op '(shl shr sar))         ; the 2-operand immediate shifts
                  (: d <>f k <>>f dk (rv rg d)
                     _ (defr ci rg sl ce d)
                     (? (kn? dk && charm? k)
                         (: v (? (op = 'shl) (* <>dk (k 2)) (>> <>dk k))
                            (? (kok? v)
                               (: _ (rset rg d `(1 v))
                                  (go r 1 cur rg sl ce ci (`('li d v) >< acc)))
                               (go r 1 cur rg sl ce ci (f >< acc))))
                        (go r 1 cur rg sl ce ci (f >< acc))))
                 (op = 'st)
                  (? (<>f = 'r4)
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
                     kv (? (b = 'r4) (rv sl off) (b = 'sp) (rv ce (cur - off)) ())
                     (? (kn? kv)
                        (: _ (defr ci rg sl ce d)
                           _ (rset rg d kv)
                           (go r 1 cur rg sl ce ci (`('li d <>kv) >< acc)))
                        (two? kv && (<kv = 2))     ; the memory holds a live COPY: read the register
                         (? (id? <>kv d)
                            (go r 1 cur rg sl ce ci acc)   ; ... and it IS this register: the load says nothing
                            (: _ (defr ci rg sl ce d)
                               _ (rset rg d kv)
                               _ (crec ci kv 'g d)
                               (go r 1 cur rg sl ce ci (`('mov d <>kv) >< acc))))
                        (: _ (defr ci rg sl ce d)
                           (go r 1 cur rg sl ce ci (f >< acc)))))
                 (op = 'push)
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
                  (? (member? <>f '(r4 sp))
                      (: _ (? (<>f = 'r4) (rset sl <>>f ()) (rset ce cur ()))
                         (go r 1 cur rg sl ce ci (f >< acc)))
                     (go r 1 cur rg (fresh 0) ce ci (f >< acc)))
                 (member? op blunt9)                 ; DELIBERATELY blunter than rdsp's write set:
                  ; the r0..r3 clamp stays as-is -- widening it is a precision change
                  (: _ (defr ci rg sl ce 'r0) _ (defr ci rg sl ce 'r1) _ (defr ci rg sl ce 'r2) _ (defr ci rg sl ce 'r3)
                     (go r 1 cur rg sl ce ci (f >< acc)))
                 (op = 'cmp)                         ; paired with its br/set consumer
                  (: a (rv rg <>f)
                     b (? (charm? <>>f) (? (kok? <>>f) `(1 <>>f) ())
                          (rv rg <>>f))
                     nx (? (two? r) <r ())
                     (? (kn? a && kn? b && two? nx && member? <nx '(br set))
                         (: tv (ccev <>nx <>a <>b)
                            (? (<nx = 'br)
                               (? tv (: r2 (swallow >r)
                                        (? (atlab? r2 <>>nx) (go r2 1 cur rg sl ce ci acc)
                                           (: _ (pmerge <>>nx cur rg sl ce ci)
                                              (go r2 () cur rg sl ce ci (`('jmp <>>nx) >< acc)))))
                                     (go >r 1 cur rg sl ce ci acc))
                               (: d <>>nx k (? tv 1 0)
                                  _ (defr ci rg sl ce d)
                                  _ (rset rg d `(1 k))
                                  (go >r 1 cur rg sl ce ci (`('li d k) >< acc)))))
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
   (deadcell fs)
    (: cand (tablet 0)                               ; st-index -> 1 while still droppable
       clds (tablet 0)                               ; st-index -> its cell's ld indexes
       dirty (tablet 0)                              ; sub-index -> 1: the span observed sp
       (dirt st2) (? !(two? st2) 1
                     (: _ (? (charm? <>><st2) (pin dirty <>><st2 1) ())
                        (dirt >st2)))
       (scan l i stk sw)                             ; -> the (sub st k add depth) entries | 'no
        (? !(two? l) sw
           (: f <l r >l
              (? !(two? f) (scan r (i + 1) stk sw)
                 ((<f = 'sub) && (<>f = 'sp) && charm? <>>>f)
                  (: k <>>>f
                     nx (? (two? r) <r ())
                     (? ((k = 16) && two? nx && (<nx = 'st) &&
                         (<>nx = 'sp) && (<>>nx = 0))
                        (: _ (pin cand (i + 1) 1)
                           (scan >r (i + 2) (`((i + 1) 16 i (tally stk)) >< stk) sw))
                        (scan r (i + 1) (`(() k i (tally stk)) >< stk) sw)))
                 ((<f = 'add) && (<>f = 'sp) && charm? <>>>f)
                  (: (unwind st2 k w)
                      (? (k = 0) `(st2 w)
                         !(two? st2) 'no
                         (: rc <st2 sz <>rc
                            (? (k < sz) 'no
                               (unwind >st2 (k - sz)
                                (? (charm? <>>rc) (`(<>>rc <rc sz i (nthr rc 3)) >< w) w)))))
                     r9 (unwind stk <>>>f ())
                     (? (id? r9 'no) 'no (scan r (i + 1) <r9 (<>r9 + sw))))
                 (<f = 'push) (scan r (i + 1) (`(() 8 () ()) >< stk) sw)
                 (<f = 'pop)
                  (? (two? stk && !<<stk && (<><stk = 8))
                     (scan r (i + 1) >stk sw)
                     (<>f = 'r4) (scan r (i + 1) stk sw)   ; the epilogue's frame pop: path exit
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
          (: subdrop (tablet 0) addsh (tablet 0)     ; sub-index -> drop; add-index -> shrink bytes
             stconv (tablet 0) ldconv (tablet 0)     ; st/ld-index -> the slot's positive offset
             pf3 (nthr fs 3)                         ; the prologue's frame sub: (sub sp sp F)
             cvok ((nthr fs 1 = '(push r4)) && (nthr fs 2 = '(mov r4 sp)) &&
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
             (drop2 l i) (? !(two? l) ()
                            (? (peep cand i ()) (drop2 >l (i + 1))
                               (peep subdrop i ()) (drop2 >l (i + 1))
                               (: f2 <l
                                  o9 (peep stconv i ())
                                  ol (peep ldconv i ())
                                  s (peep addsh i 0)
                                  (? o9 (`('st 'r4 (0 - o9) <>>>f2) >< drop2 >l (i + 1))
                                     ol (`(<f2 <>f2 'r4 (0 - ol)) >< drop2 >l (i + 1))
                                     (two? cvl && i = 3) (`('sub 'sp 'sp F2) >< drop2 >l (i + 1))
                                     (s = 0) (f2 >< drop2 >l (i + 1))
                                     (: k2 (<>>>f2 - s)
                                        (? (k2 = 0) (drop2 >l (i + 1))
                                           (`('add 'sp 'sp k2) >< drop2 >l (i + 1))))))))
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
                    (? (id? rd 'bar) (go r (tablet 0) (f >< acc))
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
       (go (rev fs) (tablet 0) ()))

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
                   (: (mend w i) (? (i = k) (`('jmpr X) >< jr0 >w)
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
                 (pears >l (n + 1) (`(<>f <>>f) >< acc))
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
                   (net < 0) (spmerge (`('sub 'sp 'sp (0 - net)) >< >r))
                   (spmerge (`('add 'sp 'sp net) >< >r))))
             (f >< spmerge r))))

   ; dead frame stores: after the constant folds, a slot the function never loads back is
   ; write-only and its stores drop. ⚠ SAFE only when NO frame address is taken at all -- an lea
   ; marks one offset but hands out a RANGE, so per-offset liveness would lie. a store's
   ; newly-dead feeding li sweeps in the same pass.
   (deadst fs)
    (: used (tablet 0) esc (tablet 0)
       ; ⚠ a slot is read in BYTES, never at one offset: a 16-byte struct param's second
       ; eightbyte is STORED at off+8 and read at off+12 (that eightbyte's second field),
       ; so an offset-exact liveness called the store dead and dropped it -- a wrong answer,
       ; silently. both sides speak the 8-byte SLOT RANGE an access spans instead.
       (mwid op) (? (member? op '(ld1 ldu1 st1 si1)) 1
                    (member? op '(ld2 ldu2 st2 si2)) 2
                    (member? op '(ld4 ldu4 st4 si4 ldss stss)) 4
                    8)
       (mark o w) (: (go k) (? ((>> ((o + w) - 1) 3) < k) 1 (: _ (pin used k 1) (go (k + 1))))
                     (go (>> o 3)))
       (hit? o w) (: (go k) (? ((>> ((o + w) - 1) 3) < k) () (peep used k ()) 1 (go (k + 1)))
                     (go (>> o 3)))
       (scan l) (? !(two? l) 1
                   (: f <l
                      _ (? !(two? f) ()
                           (member? <f ldops9)
                            (? (<>>f = 'r4) (mark <>>>f (mwid <f)) ())
                           (member? <f fldops)
                            (? (<>>f = 'r4) (mark <>>>f (mwid <f)) ())
                           (<f = 'cmpm)
                            (? (<>f = 'r4) (mark <>>f 8) ())
                           (<f = 'cmprm)
                            (? (<>>f = 'r4) (mark <>>>f 8) ())
                           (member? <f '(lea lean))
                            (? (<>>f = 'r4) (pin esc 'y 1) ())
                           ())
                      (scan >l)))
       _ (scan fs)
       (drop l) (? !(two? l) ()
                   (: f <l
                      (? (two? f && (member? <f stops9 || member? <f siops) &&
                          (<>f = 'r4) && !(hit? <>>f (mwid <f)))
                         (drop >l)
                         (f >< drop >l))))
       (lisweep l) (? !(two? l) ()
                      (: f <l nx (? (two? >l) <>l ())
                         (? (two? f && (<f = 'li) &&
                             two? nx && member? <nx '(li mov) && (<>nx = <>f))
                            (lisweep >l)
                            (f >< lisweep >l))))
       (? (peep esc 'y ()) fs (lisweep (drop fs))))

   ; dead label regions: the sibcall rewrite splices its jump BEFORE a shared epilogue and leaves
   ; the label + epi standing for the other arms -- when no arm remains, the region is unreachable
   ; ink whose `ret` trips vmret's TCO gate on our own binaries. a form after an unconditional
   ; transfer drops until a label somebody still references; an unreferenced label drops with its
   ; region. refs collect EVERY symbol operand in the fn's stream -- over-approximate, so a kept
   ; label is never wrong. deadlab iterates to a fixpoint: a dropped region takes its own refs
   ; with it, exposing the next link of a dead jmp chain.
   (deadlab fs) (: r (deadlab1 fs) (? (tally r = tally fs) r (deadlab r)))
   (deadlab1 fs)
    (: refs (tablet 0)
       (note ops) (? !(two? ops) 0
                     (: o <ops
                        _ (? (nom? o) (pin refs o 1)
                             (two? o) (note o)      ; nested operands (a raw splice's forms) scan whole
                             0)
                        (note >ops)))
       (scan l) (? !(two? l) 0
                   (: f <l
                      _ (? (two? f && !(<f = 'label)) (note >f) 0)
                      (scan >l)))
       _ (scan fs)
       (sweep l dead)
        (? !(two? l) ()
           (: f <l
              lbl (two? f && (<f = 'label))
              (? (dead && lbl)
                  (? (peep refs <>f ()) (f >< sweep >l 0) (sweep >l 1))
                 dead (sweep >l 1)
                 (f >< sweep >l (? (two? f && member? <f '(jmp jmpr ret)) 1 0)))))
       (sweep fs 0))

   ; -- the callee-saved contract, held at emission (x64): a borrowed register -- r3 r4
   ; r11-r14, machine rbx rbp r12-r15 -- written anywhere in a fn must be SAVED before its
   ; first write and RESTORED by every exit: ret, jmpr, and a sibcall jmp alike. defs read
   ; off rdsp (divops override: idiv self-saves r2 and writes r0 + its dst); the ops rdsp
   ; bars are rostered quiet here (control, calls, the d128 r0:r2 family) -- an op on
   ; neither roster REFUSES, so a new op must name its cs effect to land. a fn carrying
   ; inline asm owns its own clobbers: skipped whole. --
   csregs '(r3 r4 r11 r12 r13 r14)
   csquiet '(label align ud2 br jmp jmpr ret call callr sys mulo
             sx128 n128 a128 a128r s128 s128r m128 m128r mw128 mw128r c128 c128r
             dv128 rm128 inc128 dec128 shl128 shr128 sar128 shl128v shr128v sar128v)
   (csdefs f)                                       ; the cs regs one form writes | 'dark
    (? (member? <f csquiet) ()
       (member? <f divops) (filter (\ r (member? r csregs)) `(<>f))
       (: rd (rdsp f)
          (? (id? rd 'bar) 'dark
             (filter (\ r (member? r csregs)) <>rd))))
   (cskeep g fs)                                    ; answers fs; a breach scares, naming fn + reg
    (: (rawy? l) (? !(two? l) ()
                    (: f <l (? (two? f && id? <f 'raw) 1 (rawy? >l))))
       (? (arm? g || !(two? fs) || peep g 'hasasm () || rawy? fs) fs
          (: fn (nthr <fs 1)
             labs (tablet 0) saves (tablet 0) defs (tablet 0)
             (note l i)
              (? !(two? l) 0
                 (: f <l
                    _ (? !(two? f) 0
                         (id? <f 'label) (pin labs <>f 1)
                         (: _ (? (id? <f 'push && member? <>f csregs)
                                 (? (peep saves <>f ()) 0 (pin saves <>f `('push i))) 0)
                            _ (? (id? <f 'st &&
                                  (id? <>f 'sp || id? <>f 'r4) && member? <>>>f csregs)
                                 (? (peep saves <>>>f ()) 0 (pin saves <>>>f `('slot <>f <>>f i))) 0)
                            ds (csdefs f)
                            (? (id? ds 'dark) (scare 'cs-dark-op <f)
                               (foldl (\ _ r (? (peep defs r ()) 0 (pin defs r i))) 0 ds))))
                    (note >l (i + 1))))
             _ (note fs 1)
             dr (filter (\ r (peep defs r ())) csregs)
             (? !(two? dr) fs                       ; no cs writes: most fns leave here
                (: _ (foldl                         ; every written reg saved, before its first write
                      (\ _ r (: sv (peep saves r ())
                                (? !(two? sv) (scare 'cs-unsaved `(fn r))
                                   (: sp9 (nthr sv (? (id? <sv 'push) 1 3))
                                      (? (sp9 < peep defs r 0) 0 (scare 'cs-save-late `(fn r)))))))
                      0 dr)
                   (rest r) (: sv (peep saves r ())  ; the restore shape a save earns
                               (? !(two? sv) ()
                                  (id? <sv 'push) `('pop r) `('ld r <>sv <>>sv)))
                   ; the DIRTY walk: a written reg is dirty until its restore reloads it; a label
                   ; joins every path in (union -- monotone, so the pass iterates to a fixpoint);
                   ; every exit must leave with the set EMPTY. stld folding a restore away where
                   ; the path never wrote is exactly what this admits and a window scan would not.
                   lst (tablet 0)                   ; label -> the join of its incoming dirty sets
                   (uni a b) (foldl (\ s r (? (member? r s) s (r >< s))) b a)
                   (sub? a b) (? !(two? a) 1 (member? <a b && sub? >a b))
                   (bmark L S) (: old (peep lst L ())
                                  new (uni S old)
                                  (? (sub? new old) 0 (: _ (pin lst L new) 1)))
                   (move f S)                       ; one form's transfer on the dirty set
                    (: ds (csdefs f)
                       S2 (uni (? (id? ds 'dark) dr (filter (\ r (member? r dr)) ds)) S)
                       (filter (\ r !(f = rest r)) S2))
                   (step l S chg fin)
                    (? !(two? l) chg
                       (: f <l
                          (? !(two? f) (step >l S chg fin)
                             (: k <f
                                dead (id? S 'dead)
                                (? (id? k 'label)
                                    (step >l (uni (? dead () S) (peep lst <>f ())) chg fin)
                                   dead (step >l S chg fin)
                                   (id? k 'br) (step >l S (chg + bmark <>>f S) fin)
                                   (id? k 'jmp && peep labs <>f ())
                                    (step >l 'dead (chg + bmark <>f S) fin)
                                   (member? k '(ret jmpr jmp))
                                    (: _ (? fin (foldl (\ _ r (scare 'cs-unrestored `(fn r))) 0 S) 0)
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
   (dies? rd l)
    (? !(two? l) ()
       (: f <l
          (? !(two? f) ()
             (: rs (rdsp f)
                (? (id? rs 'bar)
                    (? (id? <f 'ret) !(id? rd 'r0)
                       (member? <f '(call callr) ||
                        (id? <f 'jmp && !(46 = ((show <>f) 0))))
                        (member? rd '(r0 r9 r10))
                       ())
                   (member? rd <rs) ()
                   (member? rd <>rs) 1
                   (dies? rd >l))))))
   (addrf1 fs)
    (? !(two? fs) ()
       (: f <fs r >fs
          nx (? (two? r) <r ())
          ; a leax feeding the very next full-word/byte load THROUGH its dest IS the indexed load:
          ; one SIB insn (base/index read at the leax's seat, so they may alias a dying dest)
          lx (? !(two? f && <f = 'leax && two? nx  && member? <nx '(ld ldu1)) ()
                !(id? <>>nx <>f) ()
                !(charm? <>>>nx && charm? (nthr f 5)) ()
                `((? (<nx = 'ld) 'ldx 'ldxb) <>nx <>>f <>>>f (nthr f 4)
                  (nthr f 5 + <>>>nx)))
          ; leax feeding a store's BASE is the indexed store (the value must not be the leax's dest)
          sx (? (two? lx) ()
                !(two? f && <f = 'leax && two? nx && member? <nx '(st st1)) ()
                !(id? <>nx <>f) ()
                !(charm? <>>nx && charm? (nthr f 5)) ()
                (id? <>>>nx <>f) ()
                `((? (<nx = 'st) 'stx 'stxb) <>>f <>>>f (nthr f 4)
                  (nthr f 5 + <>>nx) <>>>nx))
          hit (? (two? lx || two? sx) ()
                 !(two? f && two? nx  && member? <nx ldops9) ()
                 !(id? <>>nx <>f) ()               ; the load's base is f's dest
                 !(charm? <>>>nx) ()
                 (member? <f '(lea lean))
                  (? (charm? <>>>f) `(<>>f (<>>>f + <>>>nx)) ())
                 (<f = 'add)
                  (? (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>f + <>>>nx))
                     `(<>>f (<>>>f + <>>>nx)) ())
                 (<f = 'sub)
                  (? (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>nx - <>>>f))
                     `(<>>f (<>>>nx - <>>>f)) ())
                 (<f = 'mov) `(<>>f <>>>nx)
                 ())
          ; the store twin: f's dest as the store's base (operand 1; the value must not be f's dest)
          sh (? (two? lx || two? sx || two? hit) ()
                !(two? f && two? nx && (member? <nx stops9 || member? <nx siops)) ()
                !(id? <>nx <>f) ()
                !(charm? <>>nx) ()
                (id? <>>>nx <>f) ()
                (member? <f '(lea lean))
                 (? (charm? <>>>f) `(<>>f (<>>>f + <>>nx)) ())
                (<f = 'add)
                 (? (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>f + <>>nx))
                    `(<>>f (<>>>f + <>>nx)) ())
                (<f = 'sub)
                 (? (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>nx - <>>>f))
                    `(<>>f (<>>nx - <>>>f)) ())
                (<f = 'mov) `(<>>f <>>nx)
                ())
          ; the value twin: a mov whose dest rides only the store's value operand renames it
          vh (? (two? lx || two? sx || two? hit || two? sh) ()
                !(two? f && <f = 'mov && !(charm? <>>f) && two? nx && member? <nx gstops) ()
                !(id? <>>>nx <>f) ()
                (id? <>nx <>f) ()
                `(<nx <>nx <>>nx <>>f))
          ; ... and a li riding only the value operand is the store-immediate, each width
          ; truncating like its register twin
          vi (? (two? lx || two? sx || two? hit || two? sh || two? vh) ()
                !(two? f && <f = 'li && charm? <>>f && two? nx && member? <nx gstops) ()
                !(id? <>>>nx <>f) ()
                (id? <>nx <>f) ()
                (: k <>>f
                   ok (? (<nx = 'st) (fits32? k)
                         (<nx = 'st4) ((-2147483648 <= k) && (k <= 4294967295))
                         (<nx = 'st2) ((-32768 <= k) && (k <= 65535))
                         ((-128 <= k) && (k <= 255)))
                   (? ok `((? (<nx = 'st) 'si (<nx = 'st1) 'si1 (<nx = 'st2) 'si2 'si4)
                           <>nx <>>nx k)
                      ())))
          ; a li NEGATED in place is the negative li -- the constant lanes then see it
          ng (? (two? lx || two? sx || two? hit || two? sh || two? vh || two? vi) ()
                !(two? f && <f = 'li && charm? <>>f && two? nx && <nx = 'neg && id? <>nx <>f) ()
                `('li <>f (0 - <>>f)))
          ; a CONSTANT index folds into the displacement, the indexed form collapsing to plain
          xj (? (two? lx || two? sx || two? hit || two? sh || two? vh || two? vi || two? ng) ()
                !(two? f && <f = 'li && charm? <>>f && two? nx) ()
                (member? <nx '(ldx ldxb leax))
                 (: d9 (nthr nx 5 + (* <>>f (nthr nx 4)))
                    (? !(id? <>>>nx <>f) () (id? <>>nx <>f) ()
                       !(fits32? d9) ()
                       `((? (<nx = 'ldx) 'ld (<nx = 'ldxb) 'ldu1 'lea) <>nx <>>nx d9)))
                (member? <nx '(stx stxb))
                 (: d9 (nthr nx 4 + (* <>>f <>>>nx))
                    (? !(id? <>>nx <>f) () (id? <>nx <>f) () (id? (nthr nx 5) <>f) ()
                       !(fits32? d9) ()
                       `((? (<nx = 'stx) 'st 'st1) <>nx d9 (nthr nx 5))))
                ())
          ; ... and a constant right operand rides the ALU/cmp imm forms (cfoldir's knowns are
          ; green-only, so the negatives land here)
          aj (? (two? lx || two? sx || two? hit || two? sh || two? vh || two? vi || two? ng || two? xj) ()
                !(two? f && <f = 'li && charm? <>>f && fits32? <>>f && two? nx) ()
                (member? <nx aluops)
                 (? (id? <>>>nx <>f) (? (id? <>>nx <>f) () `(<nx <>nx <>>nx <>>f)) ())
                (<nx = 'cmp)
                 (? (id? <>>nx <>f) (? (id? <>nx <>f) () `('cmp <>nx <>>f)) ())
                ())
          (? (two? lx)
              (? (id? <>nx <>f || dies? <>f >r) (lx >< addrf1 >r)
                 !(id? <>f <>>f || id? <>f <>>>f)
                  (f >< lx >< addrf1 >r)
                 !(id? <>nx <>>f || id? <>nx <>>>f)
                  (lx >< f >< addrf1 >r)
                 (f >< addrf1 r))
             (two? sx)
              (? (dies? <>f >r) (sx >< addrf1 >r)
                 !(id? <>f <>>f || id? <>f <>>>f)
                  (f >< sx >< addrf1 >r)
                 (sx >< f >< addrf1 >r))
             (two? hit)
              (: nl `(<nx <>nx <hit <>hit)
                 (? (id? <>nx <>f || dies? <>f >r) (nl >< addrf1 >r)
                    !(id? <>f <hit) (f ><  nl >< addrf1 >r)
                    (nl >< f >< addrf1 >r)))
             (two? sh)
              (: nl `(<nx <sh <>sh <>>>nx)
                 (? (dies? <>f >r) (nl >< addrf1 >r)
                    !(id? <>f <sh) (f >< nl >< addrf1 >r)
                    (nl >< f >< addrf1 >r)))
             (two? vh)
              (? (dies? <>f >r) (vh >< addrf1 >r)
                 (f >< vh >< addrf1 >r))
             (two? vi)
              (? (dies? <>f >r) (vi >< addrf1 >r)
                 (f >< vi >< addrf1 >r))
             (two? ng) (ng >< addrf1 >r)
             (two? xj)
              (? (id? <>nx <>f || dies? <>f >r) (xj >< addrf1 >r)
                 (f >< xj >< addrf1 >r))
             (two? aj)
              (? (id? <>nx <>f || dies? <>f >r) (aj >< addrf1 >r)
                 (f >< aj >< addrf1 >r))
             (f >< addrf1 r))))
   (addrfold fs) (: r (addrf1 (addrf1 fs))
                    (? (tally r = tally fs) r (addrfold r)))

   ; the BRANCHING death proof (cmpfuse's license): X is dead at a point when NO reachable path
   ; reads it before a def. segments explore once (a label memo), a br queues its target and
   ; falls through, a jmp ends its segment into the target, ret frees every register but r0;
   ; a read, an unknown op, or a call answers live (the args are unseen). linear per query.
   (dieb? rd all fs0)
    (: seen (tablet 0)
       (lab l w) (? !(two? l) ()
                    (: f <l (? (two? f && <f = 'label && id? <>f w) >l (lab >l w))))
       (go l wl)
        (? !(two? l) (next wl)
           (: f <l
              (? !(two? f) (go >l wl)
                 (<f = 'label)
                  (? (peep seen <>f ()) (next wl)
                     (: _ (pin seen <>f 1) (go >l wl)))
                 (member? <f '(jmp jmp8)) (next (<>f >< wl))
                 (member? <f '(br br8)) (go >l (<>>f >< wl))
                 (<f = 'ret) (? (id? rd 'r0) () (next wl))
                 (: rs (rdsp f)
                    (? (id? rs 'bar) ()
                       (member? rd <rs) ()
                       (member? rd <>rs) (next wl)
                       (go >l wl))))))
       (next wl) (? !(two? wl) 1
                    (: w <wl
                       (? (peep seen w ()) (next >wl)
                          (: _ (pin seen w 1) (go (lab all w) >wl)))))
       (go fs0 ()))

   ; cmp LOAD-OP FUSION: a full-word load whose dest is consumed by the VERY NEXT cmp and dead
   ; beyond it fuses into the cmp's memory operand -- (ld X b o)(cmp X y) is (cmpm b o y),
   ; (cmp r X) is (cmprm r b o). runs after addrfold (the base already folded), the death proof
   ; follows branches. x64 only; full-word only (a narrow cmp's flags shift with the extension).
   (cmpfuse fs)
    (: (go l)
        (? !(two? l) ()
           (: f <l r >l
              nx (? (two? r) <r ())
              X (? (two? f && <f = 'ld && two? nx && <nx = 'cmp) <>f ())
              m (? !(nom? X) ()
                   (id? <>nx X)
                    (? (id? <>>nx X) ()             ; X both sides never fuses
                       (charm? <>>nx && !(fits32? <>>nx)) ()
                       `('cmpm <>>f <>>>f <>>nx))
                   (id? <>>nx X)
                    (? (charm? <>nx) () `('cmprm <>nx <>>f <>>>f))
                   ())
              (? (two? m && dieb? X fs >r)
                 (m >< go >r)
                 (f >< go r))))
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
   (bdropr bs r9) (filter (\ e9 !(<>e9 = r9)) bs)
   (bdropo bs o9) (filter (\ e9 (: d (<e9 - o9) ((7 < d) || (d < -7)))) bs)
   (stld fs)
    (: (esc? l) (? !(two? l) ()
                   (: f <l
                      (? (two? f && member? <f '(lea lean) && <>>f = 'r4) 1 (esc? >l))))
       (? (esc? fs) (stlda fs) (stldw fs ())))
   (stlda fs)                                        ; the escape lane: only the pair with nothing between is aliasing-proof
    (? !(two? fs) ()
       (: f <fs r >fs
          (? !(two? r && two? f && <f = 'st && <>f = 'r4)
             (f >< stlda r)
             (: nx <r
                rX <>>>f
                (? !(two? nx && <nx = 'ld && <>>nx = 'r4 && <>>>nx = <>>f)
                   (f >< stlda r)
                   (: rY <>nx
                      (? (rY = rX) (f >< stlda >r)
                         (f >< `('mov rY rX) >< stlda >r))))))))
   (stldw fs bs)
    (? !(two? fs) ()
       (: f <fs r >fs
          (? !(two? f) (f >< stldw r bs)
             (: op <f
                (? (op = 'label) (f >< stldw r ())
                   (member? op '(br jmp jmpr ret)) (f >< stldw r bs)
                   (op = 'ld && <>>f = 'r4)
                    (: rY <>f o <>>>f
                       b9 (assoc o bs)
                       (? (member? rY stldkeep)
                          (f >< stldw r ((`(o rY)) >< bdropo (bdropr bs rY) o))
                          (two? b9 && <>b9 = rY) (stldw r bs)
                          (two? b9) (`('mov rY <>b9) >< stldw r ((`(o rY)) >< bdropr bs rY))
                          (f >< stldw r ((`(o rY)) >< bdropr bs rY))))
                   (member? op stops9 && <>f = 'r4)
                    (: o <>>f
                       b2 (bdropo bs o)
                       (? (op = 'st && nom? <>>>f)
                          (f >< stldw r ((`(o <>>>f)) >< b2))
                          (f >< stldw r b2)))
                   ; a reg-reg mov CARRIES its src's bindings: the slot now rides both registers,
                   ; and when one dies the other still answers. sp/fp/r4 stay out (sp moves in
                   ; silence -- its defs are empty by design)
                   (op = 'mov && nom? <>f && nom? <>>f
                       && !(member? <>f '(sp fp r4)) && !(member? <>>f '(sp fp r4)))
                    (: rY <>f rB <>>f
                       news (map (\ e9 `(<e9 rY)) (filter (\ e9 (<>e9 = rB)) bs))
                       (f >< stldw r (news + bdropr bs rY)))
                   (: rd (rdsp f)
                      (? (id? rd 'bar) (f >< stldw r ())
                         (member? 'r4 <>rd) (f >< stldw r ())
                         (f >< stldw r (filter (\ e9 !(member? <>e9 <>rd)) bs)))))))))

   ; adjacent reg-reg movs FOLD: after (mov a b), a back-copy or a duplicate is a husk and
   ; drops; a feed (mov d a) reads through to b, stranding the middle def for deaddef's
   ; rerun. frame anchors and r3 stay out (epim?/sibjmp/unframe anchor on their shapes).
   ; an ALU/compare/memory consumer reads through the same way, at the WHITELISTED read
   ; positions only -- div and the shifts ride protocol registers, and la's operand is a
   ; symbol, so a generic substitution has no license.
   (hmov? f) (two? f && <f = 'mov && nom? <>f && nom? <>>f
               && !(member? <>f '(sp fp r4 r3)) && !(member? <>>f '(sp fp r4 r3)))
   ; the RENAME SANDWICH: (mov X Y) M.. (mov Y X) with X dead past the back-copy and every
   ; M a straight-line form that never touches Y -- the region runs as well in Y alone, so
   ; X renames away and BOTH movs drop. implicit protocol regs ride rdsp's defs, so the Y
   ; guard covers div and the variable shifts; ⚠ a 'la symbol spelled like X would rename
   ; as if it were the register -- that collision declines.
   (swin x y q n acc)
    (? (n > 8) ()
       !(two? q) ()
       (: g <q
          (? (two? g && <g = 'mov && id? <>g y && id? <>>g x) `(acc q)
             (two? g && <g = 'la && id? <>>g x) ()
             (: rd (rdsp g)
                (? (id? rd 'bar) ()
                   (member? y <rd || member? y <>rd) ()
                   (swin x y >q (n + 1) (g >< acc)))))))
   (hren f x y) (? !(two? f) f ((? (id? <f x) y <f) >< hren >f x y))
   ; a back-copy across a QUIET window (defs-free forms, conditional brs) is still a husk:
   ; nothing between rewrites either register, and the drop lives only on the fall-through.
   ; answers the rest with the copy gone, 'no when the window closes.
   (qdrop a b q n)
    (? (n > 6) 'no
       !(two? q) 'no
       (: g <q
          (? (two? g && <g = 'mov && id? <>g b && id? <>>g a) >q
             (: ok (? (two? g && <g = 'br) 1
                      (: rd (rdsp g)
                         (? (id? rd 'bar) 0 !(two? <>rd))))
                (? !ok 'no
                   (: t (qdrop a b >q (n + 1))
                      (? (id? t 'no) 'no (g >< t))))))))
   ; no memory ops here: addrfold CONSUMES a feeding mov into a load's base and deletes
   ; it under its own license -- a read-through would steal the pattern and strand a mov
   ; deaddef can't always prove dead
   huskrd '((add 2 3) (sub 2 3) (and 2 3) (or 2 3) (xor 2 3) (imul 2 3)
            (cmp 1 2) (test 1 2))
   (hsub f a b ps i)
    (? !(two? f) ()
       ((? (member? i ps && id? <f a) b <f) >< hsub >f a b ps (i + 1)))
   (dehusk fs)
    (? !(two? fs) ()
       (: f <fs r >fs
          nx (? (two? r) <r ())
          (? !(hmov? f) (f >< dehusk r)
             (id? <>f <>>f) (dehusk r)
             (: a <>f b <>>f
                sw (swin a b r 0 ())
                ; the (1 x) wrapper: an empty rewritten tail is a legitimate result
                sk (? !(two? sw) ()
                      (: ms <sw bq <>sw
                         (? (dies? a >bq)
                            (1 >< (map (\ g (hren g a b)) (rev ms) + >bq))
                            ())))
                qt (? (two? sk) 'no (qdrop a b r 0))
                (? (two? sk) (dehusk >sk)
                   !(id? qt 'no) (f >< dehusk qt)
                   !(hmov? nx)
                    (: e9 (? (two? nx) (assoc <nx huskrd) ())
                       rd9 (? (two? e9) (rdsp nx) 'bar)
                       ; b must stay clear of the consumer's defs: the alu dance writes its
                       ; dest before reading the source, so (sub rd ra rd) reads its own wreck.
                       ; and the rewrite pays only when it STRANDS the mov (a dies past the
                       ; consumer) -- a live park's read is renamer-free, not worth the churn
                       (? (id? rd9 'bar || member? a <>rd9 || member? b <>rd9) (f >< dehusk r)
                          (: nx2 (hsub nx a b >e9 0)
                             (? ((= nx2 nx) || !(dies? a >r)) (f >< dehusk r)
                                (f >< dehusk (nx2 >< >r))))))
                   (: d <>nx s <>>nx
                      (? (id? s a && id? d b) (dehusk (f >< >r))       ; back-copy: b holds b already
                         (id? s b && id? d a) (dehusk (f >< >r))       ; a duplicate copy
                         (id? s a) (f >< dehusk (`('mov d b) >< >r))   ; the feed reads through
                         (f >< dehusk r))))))))

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
        (: its <>ini (two? its && !(two? >its) && (two? <its && (id? <<its 'str)))))
        <<>ini
       (arr? ty && !(ptr? <>ty) &&                   ; { L"s" }: p14's wide twin
        (: its <>ini (two? its && !(two? >its) && (two? <its && (id? <<its 'clit) && arr? <><its))))
        (unelide g ty <>><<>ini)
       (: pr (normfill g ty <>ini) `('init <pr)))     ; <pr = normed items; <>pr (excess) dropped
   (redesig dh dk v) `(dh dk v)   ; ('didx k v) | ('dfield k v)
   (normelt g ty desig dh dk val l)                 ; one element of type ty from item stream l -> (item rest)
    (? (arr? ty && (!(ptr? <>ty) && (two? val && id? <val 'str)))
        `(<l >l)                                    ; a string IS a char array's whole element -- never an elision run
       (clarr? ty val)                              ; a wide literal: the whole element, as its init
        (: w (unelide g ty <>>val)
           `((? desig (redesig dh dk w) w) >l))
       (aggish? ty && (!(gcplx? ty) && !(initval? val)))   ; ELIDE: fill ty from the run starting at val (never into a complex -- a scalar)
        (: pr (normfill g ty (val >< >l))
           w `('init <pr)
           `((? desig (redesig dh dk w) w) <>pr))
       (: nv (? (initval? val) (unelide g ty val) val)  ; a braced sub -> recurse-normalize; a scalar -> as-is
          `((? desig (redesig dh dk nv) (? (id? nv val) <l nv)) >l)))
   (normfill g ty items)                            ; consume items to fill aggregate ty -> (normed-items rest)
    (? (arr? ty)
        (: elt <>ty n <>>ty
           (go l idx acc)
            (? !(two? l) `((rev acc) ())
               (: it <l
                  desig (<it = 'didx)
                  ; full AND plain -> the tail is the rest; a designator may jump BACK
                  ; ({ [2] = 9, [0] = 1 }), so it never ends the walk
                  (? (!desig && (< 0 n) && (<= n idx)) `((rev acc) l)
                     (: ci (? desig <>it idx)
                        pr (normelt g elt desig 'didx ci (elval it) l)
                        (go <>pr (ci + 1) (<pr >< acc))))))
           (go items 0 ()))
       (stru? ty)
        (: fields <(peep (g 'stag) <>ty 0)
           (go l cur acc)
            (? !(two? l) `((rev acc) ())
               (: it <l
                  desig (<it = 'dfield)
                  ci (? desig (fidx fields <>it) cur)
                  fe (nthfield fields ci)
                  (? (bad? fe) `((rev acc) l)          ; unknown field / past the end -> leave the rest
                     (: pr (normelt g <>>fe desig 'dfield <>it (elval it) l)
                        (go <>pr (ci + 1) (<pr >< acc))))))
           (go items 0 ()))
       `(items ()))

   ; -- LOCAL init (codegen): zero the slot, then fill (values may be runtime) --
   (cgzero off span)                                ; zero [r4+off .. +span)
    (: (go o rem acc)
        (? (8 <= rem) (go (o + 8) (rem - 8) (acc + `(`('st 'r4 o 'r0))))
           (0 < rem) (go (o + 1) (rem - 1) (acc + `(`('st1 'r4 o 'r0))))
           acc)
       ('((li r0 0)) + go off span ()))
   (cgstr off text)                                 ; a char array from a string: byte stores (NUL from the zero)
    (: (go i acc) (? (i < 0) acc (go (i - 1) (`(`('li 'r0 (text i)) `('st1 'r4 (off + i) 'r0)) + acc)))
       (go (tally text - 1) ()))
   (cgstore g env off ty val)                       ; store a scalar value expr into [r4+off]
    (: r (cgexpr g env val)
       (? (bad? r) 'bad
          (flo? ty) (asflo g r + fstf g ty 'r4 off 'f0)
          (pair? g ty) (asll g r + pst 'r4 off)
          (asintt g r ty + cvt ty 'r0 + `(`((stop ty) 'r4 off 'r0)))))
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
    (: r (cgexpr g env val)
       (? (bad? r) 'bad
          (: uty <>bt bitoff <>>bt usz (tsz g uty)
             mask ((<< 1 <>>>bt) - 1) cmask ((0 - (<< mask bitoff)) - 1)
             (asintt g r uty
              + (? (id? uty 'bool) (cvt uty 'r0) ())   ; normalize BEFORE the mask
              + `(`((uldop usz) 'r2 'r4 off)
                  `('li 'r3 mask) '(and r0 r0 r3)
                  `('li 'r3 cmask) '(and r2 r2 r3)
                  '(mov r3 r0))
              + (? (bitoff > 0) `(`('shl 'r3 bitoff)) ())
              + `('(or r2 r2 r3) `((stop uty) 'r4 off 'r2))))))
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
    (: rl (clval g env val)
       rv (? (!(bad? rl) && stru? (vty rl)) rl (cgexpr g env val))
       (? (bad? rv) 'bad
          (stru? (vty rv))
           (: mv (tor0 g rv + `(`('lea 'r1 'r4 foff)))
              (? (gcplx? fty && gcplx? (vty rv) && !(<>fty = <>(vty rv)))
                 (mv + cxcopy g (vty rv))
                 (mv + bcopy g 0 (tsz g fty) ())))
          (gcplx? fty)
           (: b (cxb fty)
              (asflo g rv + (fstf g b 'r4 foff 'f0
               + ('((li r2 0))
                  + (? (b = 'float) `(`('st4 'r4 (foff + 4) 'r2)) `(`('st 'r4 (foff + 8) 'r2)))))))
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
   (iminsert x l) (? !(two? l) `(x)                    ; insert (off items) keeping offsets ascending
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
       (<e = 'num) <>e
       (<e = 'cast) (: v (icfold <>>e)
                       (? (id? v 'no) 'no (flo? <>e) 'no
                          (id? <>e 'bool) (? (v = 0) 0 1) v))
       (<e = 'un)
        (: v (icfold <>>e)
           (? (id? v 'no) 'no (<>e = 'neg) -v (<>e = 'bnot) (-v - 1) 'no))
       (<e = 'bin)
        (: op <>e a (icfold <>>e) b (icfold <>>>e)
           (? (id? a 'no || id? b 'no) 'no
              (op = '+) (a + b) (op = '-) (a - b) (op = '*) (a * b)
              (op = '/) (? (b = 0) 'no (a // b)) (op = '%) (? (b = 0) 'no (a - ((a // b) * b)))
              (op = 'shl) (a << b) (op = 'shr) (a >> b)
              (op = '&) (a & b) (op = '|) (a | b) (op = '^) (a ^ b)
              'no))
       'no)
   ; a CONSTANT LVALUE at image time -> (label byteoffset type) | 'no: a file-scope global, a
   ; *(const pointer) (a subscript desugars here -- a[i] is *(a+i)), or a .member off either.
   ; the type threads so pointer arithmetic and the next .member scale/offset correctly.
   (imlval g e)
    (? (<e = 'var)
        (? (peep (g 'fns) <>e ()) `((intern <>e) 0 (fnty g <>e))   ; a function IS its address
           (: en (peep (g 'glob) <>e 0)                                       ; glob = (label type init)
              (? (two? en) `(<en 0 <>en) 'no)))
       (<e = 'deref)
        (: p (imaddr g <>e) (? (id? p 'no) 'no `(<p <>p <><>>p)))
       (<e = 'dot)
        (: b (imlval g <>e)
           (? (id? b 'no) 'no
              (: fm (fmem g <>>b <>>e) (? (bad? fm) 'no `(<b (<>b + <fm) <>fm)))))
       'no)
   ; a CONSTANT ADDRESS expression at image time -> (label byteoffset ptrtype) | 'no: a string /
   ; function / decayed-array global (offset 0), &lvalue, or pointer +/- an int constant (scaled
   ; by the pointee). a cast retypes, so a later + scales by the new pointee.
   (imaddr g e)
    (? (<e = 'cast)
        (: r (imaddr g <>>e)                        ; a cast RETYPES the address (its type is the cast's) --
           (? (id? r 'no) 'no `(<r <>r <>e)))   ; so a later + scales (or doesn't) by the cast type
       (<e = 'str)
        (: l (nlab g ".S") _ (pin g 'strs (`(l <>e) >< g 'strs)) `(l 0 '(ptr char)))
       (<e = 'var)
        (? (peep (g 'fns) <>e ()) `((intern <>e) 0 `('ptr (fnty g <>e)))
           (: en (peep (g 'glob) <>e 0)
              ; a name that is neither a function nor a global: NAME IT (the same 'undecl pin
              ; cgfn uses), so an undeclared identifier in a static initializer reads as itself
              ; rather than as an IR dump -- which wears a codegen gap's face.
              (? !(two? en) (: _ (pin g 'undecl <>e) 'no)
                 (: ty <>en (? (arr? ty) `(<en 0 `('ptr <>ty)) `(<en 0 `('ptr ty)))))))
       (<e = 'addr)
        (: lv (imlval g <>e) (? (id? lv 'no) 'no `(<lv <>lv `('ptr <>>lv))))
       ((<e = 'bin) && member? <>e '(+ -))
        ; label +/- an int constant: a POINTER type scales the constant by the pointee, an
        ; integer-typed label ((word)&x + 8) adds raw bytes -- so scale only when it's a pointer
        (: op <>e
           (imscale ty) (? (ptr? ty) (tsz g <>ty) 1)
           pa (imaddr g <>>e) kb (icfold <>>>e)
           (? (!(id? pa 'no) && !(id? kb 'no))
               `(<pa (<>pa + (* (? (op = '-) (0 - kb) kb) (imscale <>>pa))) <>>pa)
              (: pb (imaddr g <>>>e) ka (icfold <>>e)
                 (? (((op = '+) && !(id? pb 'no)) && !(id? ka 'no))
                     `(<pb (<>pb + (* ka (imscale <>>pb))) <>>pb)
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
             (<e = 'flo) <>e
             (<e = 'num) (0.0 + <>e)
             (<e = 'cast) (? (flo? <>e) (ffold <>>e) 'bad)
             (<e = 'un)
              (: v (ffold <>>e)
                 (? (bad? v) 'bad (<>e = 'neg) (0.0 - v) 'bad))
             (<e = 'bin)
              (: op <>e a (ffold <>>e) b (ffold <>>>e)
                 (? (bad? a || bad? b) 'bad
                    (op = '+) (a + b) (op = '-) (a - b) (op = '*) (* a b)
                    (op = '/) (? (b = 0.0) 'bad (/ a b))
                    'bad))
             'bad)))
   (fim ty v)                                       ; a float value's IEEE image as ty: 4 single bytes | 8 double bytes
    (? (ty = 'float) (les 4 (fbits32 v)) (les 8 (fbits v)))
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
                     (? !(id? r 'no) `(`('fix (wsize g) (? (t32? g) 'abs32 'abs64) <r <>r))      ; or a constant INTEGER ((int*)0x1000, a null)
                        (: v (icfold ini) (? (id? v 'no) 'bad (imnum g ty v)))))
       ; a label cast to a WORD-SIZED integer ((word)nif_absent, (intptr_t)f) is
       ; still the label's address; a non-label cast folds through to its value
       (<ini = 'cast && !(aggish? ty) && !(flo? ty) && tsz g ty = wsize g)
        (: r (imaddr g ini)
           (? !(id? r 'no) `(`('fix (wsize g) (? (t32? g) 'abs32 'abs64) <r <>r)) (imgbytes g ty <>>ini)))
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
                      (? (bad? eb) 'bad (place >l (ci + 1) (`((* ci esz) eb) >< acc)))))
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
                (? !(two? l) `(acc bfs)
                   (: it <l
                      ci (? (<it = 'dfield) (fidx fields <>it) cur)
                      fe (nthfield fields ci)
                      (? (bad? fe) 'bad
                         (bitf? <>>fe)
                          (: v (icfold (elval it))
                             (? (id? v 'no) 'bad
                                (place >l (ci + 1) acc (bfmerge g <>fe <>>fe v bfs))))
                         (: fb (imgpad g <>>fe (tsz g <>>fe) (elval it))
                            (? (bad? fb) 'bad (place >l (ci + 1) (`(<>fe fb) >< acc) bfs))))))
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
       (go l) (? !(two? l) `(`(uoff <>bt bits))
                 (<<l = uoff) (: u9 (? (tsz g <><l < tsz g <>bt) <>bt <><l)
                                 (`(uoff u9 (<>><l + bits)) >< >l))
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
                   (go >l (pos + 8) (acc + `(<l))))
               (: b (? (uoff <= pos && pos < uoff + tally ub) (<l | nthr ub (pos - uoff)) <l)
                  (go >l (pos + 1) (acc + `(b)))))
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
                 _ (vmflush g)
                 dr (cgexpr g env2 (varn ty))
                 (? (bad? dr) 'bad
                    (: lf (nlab g ".vla")
                       code (`(`('ld 'r0 'r4 so) '(cmp r0 0) `('br 'eq lf))
                             + v16
                             + `(`('ld 'r1 'r4 po) '(add r1 r1 r0) '(lea sp r1 0)
                                 `('label lf))
                             + asint g dr
                             + (? (1 < es) (imby g 'r0 es) ()) + `(`('st 'r4 so 'r0))
                             + v16 + vsub + `('(lea r0 sp 0) `('st 'r4 po 'r0)))
                       (one >ps (`(<p po ty) >< env2) (acc + code)))))))
       (one ps env2 acc)
        (? !(two? ps) `(acc env2)
           (: _ (psreset g)
              p <ps
              ty <>p
              init <>>p
              (? (gvarr? ty)
                  (? (two? init)
                      (nolane g "a variable-length array cannot take an initializer")
                     !(member? (peep g 'tgt 'x64) '(x64 arm64))
                      (nolane g ("no lane for a variable-length array" + ontgt g))
                     (vone ps env2 acc p ty))
                 (: span (& (tsz g ty + 7) -8)
              off (nslot g span)
              hreg (? (peep g 'inlining ())
                      ; a call-free splice's body decl takes a pool seat when cginl's read
                      ; pricing licensed the name (inlrb); rpin holds it across the body's
                      ; statements, the splice frees it (inlrbs). ⚠ REGEN attempts only
                      ; (rgon): an ir1 bind changes the slot-touch counts lpick and the
                      ; homing payoff gate rank on, and the attempt flip is unpriced --
                      ; ioputx LOST ITS SIBCALL to one (a stack-growing call in the printer)
                      (? (peep g 'rgon () &&
                          lhomable? ty &&
                          member? <p (peep g 'inlrb ()) &&
                          deep? (peep g 'pool ()))
                          (: rb (ralloc g)
                             (? !rb ()
                                (: _ (pins g 'rpin (rb >< peep g 'rpin ())
                                             'inlrbs (rb >< peep g 'inlrbs ()))
                                   rb)))
                         ())
                      !(lhomable? ty || (d128? ty && x64t? g)) ()
                      (: sq (1 + peep g 'dseq 0)
                         _ (pins g 'dseq sq
                                   'lrec (`(sq off ty <p) >< peep g 'lrec ()))
                         lht (peep g 'lhome ())
                         (? lht (peep lht sq ()) ())))
              ; a d128's home is a PAIR (lo hi) off the cs pool -- lpick packs two seats
              env3 (`(<p (? (two? hreg) `('regw hreg) hreg `('reg hreg) off) ty) >< env2)
              ; a callee-saved home stays OFF g 'homes: no call wrap (the callee preserves it),
              ; no sib peel, and ezarg must never read its never-written shadow slot
              _ (? (hreg && !(two? hreg) && !(member? hreg (cspool g)))
                   (pin g 'homes (`(hreg off <p) >< peep g 'homes ())) ())
              (? !(two? init) (one >ps env3 acc)
                 ((<init = 'init) || ((<init = 'str) && arr? ty) || clarr? ty init)
                  (? ((<init = 'init) && !(aggish? ty)) 'bad      ; a brace for a scalar
                     (: f (cgfill g env3 off ty init)
                        _ (? (arr? ty) (vadrop9 g (<p + "[")) ())   ; a fill retires element pins (designated overrides re-store)
                        (? (bad? f) 'bad
                           (one >ps env3 (acc + cgzero off span + f)))))
                 (arr? ty) 'bad                          ; a bare-expr init of an array
                 ; the slot is the DESTINATION (doc/proto/dest.l's mem die): a licensed
                 ; constant init of a plain slotted scalar delivers as ONE store-immediate
                 ; -- no li, no cvt, no r0. the vmap-hint lane keeps priority (residency
                 ; pays the later reads); bool keeps the protocol (its cvt normalizes to
                 ; 0/1 where si would store the raw constant).
                 (x64t? g && !hreg && (<init = 'num) && charm? <>init &&
                  !(flo? ty || pair? g ty || d128? ty || id? ty 'bool) &&
                  !(vmon? g && homable? ty && member? <p (peep g 'vuniv ())) &&
                  siop9 ty <>init)
                  (: _ (vmdrop g <p)
                     (one >ps env3 (acc + `(`((siop9 ty <>init) 'r4 off <>init)))))
                 (stru? ty)                              ; a struct local from a struct VALUE (an address): copy its bytes
                  (: r (cgexpr g env3 init)
                     (? (bad? r) 'bad
                        (: sz (tsz g ty)
                           cpf (: (go k acc9)
                                   (? (8 <= sz - k && !(t32? g))
                                       (go (k + 8) (acc9 + `(`('ld 'r1 'r0 k) `('st 'r4 (off + k) 'r1))))
                                      (4 <= sz - k) (go (k + 4) (acc9 + `(`('ld4 'r1 'r0 k) `('st4 'r4 (off + k) 'r1))))
                                      (2 <= sz - k) (go (k + 2) (acc9 + `(`('ld2 'r1 'r0 k) `('st2 'r4 (off + k) 'r1))))
                                      (1 <= sz - k) (go (k + 1) (acc9 + `(`('ld1 'r1 'r0 k) `('st1 'r4 (off + k) 'r1))))
                                      acc9)
                                  (go 0 ()))
                           (one >ps env3 (acc + tor0 g r + cpf)))))
                 (: hint (? (vmon? g && !hreg &&
                             homable? ty && member? <p (peep g 'vuniv ()))
                            (ralloc g) ())               ; the vmap hint (asn's twin)
                    _ (? hint (pin g 'want hint)
                         (hreg && !(two? hreg)) (pin g 'want hreg)   ; the home is the DESTINATION (the asn regv arm's shape)
                         ())
                    r (cgexpr g env3 init)               ; a scalar (a pointer = string rides cgexpr -> la)
                    _ (? (hint && (bad? r || !(id? (vreg r) hint))) (rfree g hint) ())
                    _ (vmdrop g <p)
                    (? (bad? r) 'bad
                       (flo? ty) (one >ps env3 (acc + asflo g r + fstf g ty 'r4 off 'f0))
                       (pair? g ty) (one >ps env3 (acc + asll g r + pst 'r4 off))
                       (d128? ty)
                        (? !(x64t? g) 'bad
                           (two? hreg)              ; a pair home's init is two register moves
                            (one >ps env3 (acc + as128 g r + `(`('mov <hreg 'r0) `('mov <>hreg 'r2))))
                           (one >ps env3 (acc + as128 g r + `(`('st 'r4 off 'r0) `('st 'r4 (off + 8) 'r2)))))
                       hreg                        ; a homed local's init DELIVERS to the home (a narrow home lands canonical)
                        (? (flo? (vty r))
                           (one >ps env3 (acc + asintt g r ty + (`(`('mov hreg 'r0)) + cvt ty hreg)))
                           (id? (vreg r) hreg)     ; the destination held: no bridge
                           (one >ps env3 (acc + vfs r + cvt ty hreg))
                           (: fs (vfs r + (`(`('mov hreg (vreg r))) + cvt ty hreg))
                              _ (rfree g (vreg r))
                              (one >ps env3 (acc + fs))))
                       (hint && !(flo? (vty r)) &&
                        member? (vreg r) (peep g 'pool0 ()))         ; hint set = vmap on + universe local
                        (: _ (vmpin g <p (vreg r))       ; the init value stays resident
                           (one >ps env3 (acc + vfs r + `(`('st 'r4 off (vreg r))))))
                       (one >ps env3
                            (acc + asintt g r ty +
                             cvt ty 'r0 +
                             `(`((stop ty) 'r4 off 'r0)))))))))))
       _ (stq g)                                     ; one statement tick for the whole decl
       (one pairs env ()))

   ; block-scope `static` ((name ty init)..): each name gets a UNIQUE global laid in .bss/.data,
   ; NOT a stack slot. the name binds in the LOCAL env to a (glob lbl) marker (so it shadows outer
   ; names and stays distinct per function) and vent turns that into a 'glo, riding the same
   ; read/write/& paths a file-scope global does. no runtime init code -- a static's initializer
   ; is a compile-time constant image. -> (forms env) | 'bad, the same shape as cgdecl.
   (cgsdecl g env pairs)
    (: (one ps env2)
        (? !(two? ps) `(() env2)
           (: p <ps
              name <p ty <>p init <>>p
              lbl (nlab g (g 'fn + "."))             ; `fn.N` -- the `.` bars any C-identifier collision
              _ (pin (g 'glob) lbl `(lbl ty init))   ; a LOCAL data label (internal linkage): g 'statics,
              _ (pin g 'statics (lbl >< g 'statics))      ; NOT g 'globs -- a static is never an exported symbol
              (one >ps (`(name `('glob lbl) ty) >< env2))))
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
   asmnames (foldl (\ t i (: s9 ("r" + show i) (pin t (intern s9) s9))) #() (jot 11))
   (asmreg? g r9) (member? r9 asmregs || (a64? g && id? r9 'r4))
   ; a constraint's register part: "r" -> 'pick, "i" -> 'imm, "rN" -> the nom;
   ; the x86 letters "a"/"d" pin rax/rdx (the divq protocol), "rm" reads as "r"
   (asmc c9) (: n (tally c9)
                (? (n < 1) 'bad
                   (n = 1) (? (c9 0 = 114) 'pick (c9 0 = 105) 'imm
                              (c9 0 = 97) 'r0 (c9 0 = 100) 'r2 'bad)
                   (c9 = "rm") 'pick
                   (intern c9)))
   ; %-substitute the template: al the operand texts, GNU order
   (asmsub tm al)
    (: n (tally tm)
       (go i acc)
        (? (n <= i) acc
           (: c (tm i)
              (? !(c = 37) (go (i + 1) (acc + c))
                 (i + 1 < n)
                  (: d (tm (i + 1))
                     (? (d = 37) (go (i + 2) (acc + 37))
                        (48 <= d && d <= 57)
                         (: ix (d - 48)
                            (? (ix < tally al) (go (i + 2) (acc + nthr al ix)) 'bad))
                        'bad))
                 'bad)))
       (go 0 ""))
   asmhb holo-bytes                                 ; bare: the cat leads with (use 'holo) (asbook.l), so this folds at compile
   (cgasm g env s)
    (: _ (pin g 'hasasm 1)
       tmpl <>s
       ; one operand list, GNU order: ('o|'x reg expr) an output (x in-out),
       ; ('i reg expr) a register input, ('m imm) an immediate input
       (oconv l acc)
        (? !(two? l) (rev acc)
           (: c <<l n (tally c)
              h (? (0 < n) (c 0) 0)
              k (? (h = 61 || h = 43) (asmc (snip c 1 n)) 'bad)
              (? (id? k 'bad || id? k 'imm) 'bad
                 (oconv >l (`((? (h = 43) 'x 'o) k <><l) >< acc)))))
       (iconv l acc)
        (? !(two? l) (rev acc)
           (: k (asmc <<l) e <><l
              (? (id? k 'bad) 'bad
                 (id? k 'imm)
                  (? (two? e && id? <e 'num && charm? <>e)
                     (iconv >l (`('m <>e) >< acc)) 'bad)
                 (iconv >l (`('i k e) >< acc)))))
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
                      (: txt (asmsub tmpl (map (\ a9 (? (charm? a9) (show a9) (peep asmnames a9 "r?"))) asgn))
                         (? (bad? txt) 'bad
                            (: ; inputs (and in-out outputs) evaluate and PUSH..
                               (loads l acc)
                                (? !(two? l) acc
                                   (: e2 <l
                                      (? (member? <e2 '(x i))
                                          (: r (cgexpr g env <>>e2)
                                             (? (bad? r) 'bad
                                                (flo? (vty r) || stru? (vty r)) 'bad
                                                (loads >l (acc + tor0 g r + spush 'r0))))
                                         (loads >l acc))))
                               lf (loads ops9 ())
                               (? (bad? lf) 'bad
                                  (: ; ..then POP into their registers, reverse order
                                     (pops l al acc)
                                      (? !(two? l) acc
                                         (? (member? <<l '(x i))
                                            (pops >l >al (spop <al + acc))
                                            (pops >l >al acc)))
                                     ; the body: text -> IR forms -> BYTES, one opaque raw
                                     body `(`('raw (asmhb (peep g 'tgt 'x64) (asm-text txt))))
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
                                               (: lv (clval g env <>>e2)
                                                  (? (bad? lv) 'bad
                                                     (: ty (vty lv)
                                                        (? (flo? ty || stru? ty || arr? ty || bitf? ty) 'bad
                                                           (ostores >l >al
                                                            ((vfs lv + spop 'r1 + `(`((stop ty) 'r0 0 'r1))) + acc)))))))))
                                     osf (ostores ops9 asgn ())
                                     (? (bad? osf) 'bad
                                        (lf + pops ops9 asgn () + body + opush ops9 asgn () + osf)))))))))))))

   ; the statement CHILD MAP, one source of truth: which sub-forms of a statement are themselves
   ; statements, in forward (tick) order -- a for's init only when it declares. alive's prepass
   ; counts by it and scasel scans by it, and ⚠ cgstmt's walk must agree node for node.
   (stkids s)
    (: k <s
       (? (k = 'blk) <>s
          (k = 'if) (? (two? <>>>s) `(<>>s <>>>s) `(<>>s))
          (k = 'while) `(<>>s)
          (k = 'do) `(<>s)
          (k = 'for) (: ini <>s bod <>>>>s
                        (? (two? ini && member? <ini '(decl sdecl)) `(ini bod) `(bod)))
          (k = 'switch) <>>s
          ()))

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
   ; call sits in the path maximum instead -- it runs at most once per invocation).
   (alive g f)
    (: params <>>f
       items <><>>>f
       dcl #() tak #() n9 #() liv #() crs #()
       ; -- prepass: &-taken names, one generic sweep (sizeof's unevaluated
       ; &v over-records -- harmless, the name just keeps the blanket) --
       (pex e) (? !(two? e) ()
                  (: _ (? ((<e = 'addr) && two? <>e && (<<>e = 'var))
                          (pin tak <><>e 1) ())
                     (pexl >e)))
       (pexl l) (? !(two? l) () (: _ (pex <l) (pexl >l)))
       ; -- prepass: the tick total, declaration counts, homable locals (⚠ ticks mirror lst's
       ; below, statement for statement; the child walk is stkids', the shared map) --
       (pre s)
        (? !(two? s) ()
           (: k <s
              _ (pin n9 'n (1 + peep n9 'n 0))
              (? (member? k '(decl sdecl))
                  (: (go ps) (? !(two? ps) ()
                                (: p <ps
                                   _ (pin dcl <p (1 + peep dcl <p 0))
                                   _ (? ((k = 'decl) && homable? <>p)
                                        (pin n9 'loc (<p >< peep n9 'loc ())) ())
                                   (go >ps)))
                     (go <>s))
                 (prel (stkids s)))))
       (prel l) (? !(two? l) () (: _ (pre <l) (prel >l)))
       (pdcl ps) (? !(two? ps) ()
                    (: _ (pin dcl <<ps (1 + peep dcl <<ps 0))
                       (pdcl >ps)))
       ; -- the universe: the homable gp walk for the param NAMES, plus the locals --
       (pgo ps gpi acc) (homefold g ps gpi acc (\ a2 i9 p9 (? (homable? <>p9) (<p9 >< a2) a2)))
       _ (pdcl params)
       _ (prel items)
       _ (pexl items)
       univ (filter (\ nm ((peep dcl nm 0 = 1) && !(peep tak nm ())))
                    (pgo params 0 () + peep n9 'loc ()))
       ; ⚠ a homable param OUTSIDE the universe (shadowed, or a sizeof-& over-record) still HOMES,
       ; so it must ride EVERY wrapset -- else the filter strips its one true spill
       pales (filter (\ nm !(member? nm univ)) (pgo params 0 ()))
       ; -- the set kit (names are strings; member? compares content) --
       (sadd s2 nm) (? (member? nm s2) s2 (nm >< s2))
       (sun s2 b) (? !(two? b) s2 (sun (sadd s2 <b) >b))
       (sdel s2 nm) (? !(two? s2) ()
                       (<s2 = nm) >s2
                       (<s2 >< sdel >s2 nm))
       (tick u) (: q (peep n9 'q 0) _ (pin n9 'q (q - 1)) q)
       ; nclw = the statement's IN-LOOP share of ncl (0 at depth 0): the fn total n9 'ncw feeds
       ; the payoff gate -- a depth-0 call runs at most once per invocation
       (rec sq w ncl nclw)                          ; a call-bearing statement's record
        (? (ncl < 1) ()
           (: _ (pin liv sq (sun w pales))          ; the outside-universe homes ride every wrap
              _ (pin n9 'ncw (nclw + peep n9 'ncw 0))
              (bump l) (? !(two? l) ()
                          (: _ (pin crs <l (ncl + peep crs <l 0)) (bump >l)))
              (bump w)))
       ; -- reads of an expression, restricted to the universe -> (set ncalls). a var WRITE is not
       ; a read (the home register takes the value directly), sizeof's operand never evaluates,
       ; and an unknown head reads EVERYTHING and counts a call (future grammar fails safe) --
       (erd e s2 nc)
        (? !(two? e) `(s2 nc)
           (: k <e
              (? (k = 'var) `((? (member? <>e univ) (sadd s2 <>e) s2) nc)
                 (member? k '(num flo str szof)) `(s2 nc)
                 (member? k '(dot deref addr post vastart vaend vaarg)) (erd <>e s2 nc)
                 (member? k '(un cast)) (erd <>>e s2 nc)
                 (k = 'asn) (: r (erd <>>e s2 nc)
                               (? (<<>e = 'var) r (erd <>e <r <>r)))
                 (k = 'bin) (: r (erd <>>e s2 nc) (erd <>>>e <r <>r))
                 (member? k '(land lor comma)) (: r (erd <>e s2 nc) (erd <>>e <r <>r))
                 (k = 'cond) (: r (erd <>e s2 nc) r2 (erd <>>e <r <>r) (erd <>>>e <r2 <>r2))
                 (k = 'call) (: r (erd <>e s2 (nc + 1)) (erdl <>>e <r <>r))
                 (k = 'clit) (erdi <>>e s2 nc)
                 (k = 'init) (erdi <>e s2 nc)
                 `((sun s2 univ) (nc + 1)))))
       (erdl l s2 nc) (? !(two? l) `(s2 nc) (: r (erd <l s2 nc) (erdl >l <r <>r)))
       (erdi l s2 nc)                               ; init items: dfield/didx wrap their values
        (? !(two? l) `(s2 nc)
           (: it <l
              v (? (two? it && member? <it '(dfield didx)) <>>it it)
              r (erd v s2 nc)
              (erdi >l <r <>r)))
       ; -- a loop's uses: every name its cond/step/body read, kills ignored
       ; (the back-edge seed); a goto inside escapes -> the whole universe --
       (ust s acc)
        (? !(two? s) acc
           (: k <s
              (? (k = 'blk) (ustl <>s acc)
                 (k = 'expr) <(erd <>s acc 0)
                 (k = 'ret) (? (two? <>s) <(erd <>s acc 0) acc)
                 (member? k '(decl sdecl)) (ustp <>s acc)
                 (k = 'if) (: a1 <(erd <>s acc 0)
                              a2 (ust <>>s a1)
                              (? (two? <>>>s) (ust <>>>s a2) a2))
                 (k = 'while) (ust <>>s <(erd <>s acc 0))
                 (k = 'do) (ust <>s <(erd <>>s acc 0))
                 (k = 'for) (: ini <>s
                               a1 (? !(two? ini) acc
                                     (member? <ini '(decl sdecl)) (ustp <>ini acc)
                                     <(erd <>ini acc 0))
                               a2 (? (two? <>>s) <(erd <>>s a1 0) a1)
                               a3 (? (two? <>>>s) <(erd <>>>s a2 0) a2)
                               (ust <>>>>s a3))
                 (k = 'switch) (ustl <>>s <(erd <>s acc 0))
                 (member? k '(goto asm)) (sun acc univ)
                 acc)))
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
        (? !(two? ps) `(o nc)
           (: p <ps
              r (erd <>>p () 0)
              o2 (sun (? (member? <p univ) (sdel o <p) o) <r)
              (dpairs >ps o2 (nc + <>r))))
       (lst s out brk cnt w9)
        (? !(two? s) out
           (: k <s
              (? (k = 'blk) (: in (litems <>s out brk cnt w9) _ (tick ()) in)
                 (k = 'expr)
                  (: e9 <>s
                     kv (? ((<e9 = 'asn) &&
                            (<<>e9 = 'var) &&
                            member? <><>e9 univ)
                           <><>e9 ())
                     r (erd e9 () 0)
                     in (sun (? kv (sdel out kv) out) <r)
                     ; ⚠ the KILL rides the record too: a name born in a call-bearing statement is
                     ; invisible to in/out yet lives across the statement's OWN calls, and
                     ; statement-grain cannot order intra-statement events
                     sq (tick ()) _ (rec sq (sun (sun in out) (? kv `(kv) ())) (* <>r w9)
                                         (? (1 < w9) (* <>r w9) 0))
                     in)
                 (k = 'ret)
                  (: r (? (two? <>s) (erd <>s () 0) '(() 0))
                     sq (tick ()) _ (rec sq <r (* <>r w9) (? (1 < w9) (* <>r w9) 0))
                     <r)
                 (k = 'decl)
                  (: pr (dpairs (rev <>s) out 0)
                     ks (: (go ps) (? !(two? ps) ()      ; the declared names: kills, in the record (see expr)
                                      (: nm <<ps
                                         (? (member? nm univ) (nm >< go >ps) (go >ps))))
                           (go <>s))
                     sq (tick ()) _ (rec sq (sun (sun <pr out) ks) (* <>pr w9)
                                         (? (1 < w9) (* <>pr w9) 0))
                     <pr)
                 (k = 'sdecl) (: _ (tick ()) out)   ; statics: no runtime init, no kill
                 (k = 'if)
                  (: inE (? (two? <>>>s) (lst <>>>s out brk cnt w9) out)   ; else FIRST (reverse order)
                     inT (lst <>>s out brk cnt w9)
                     r (erd <>s () 0)
                     in (sun (sun inT inE) <r)
                     sq (tick ()) _ (rec sq (sun in out) (<>r * w9) (? (1 < w9) (<>r * w9) 0))
                     in)
                 (k = 'while)
                  (: w8 (8 * w9)
                     seed (sun out (ust <>>s <(erd <>s () 0)))
                     _ (lst <>>s seed out seed w8)
                     r (erd <>s () 0)
                     sq (tick ()) _ (rec sq seed (<>r * w8) (<>r * w8))
                     seed)
                 (k = 'do)
                  (: w8 (8 * w9)
                     seed (sun out (ust <>s <(erd <>>s () 0)))
                     r (erd <>>s () 0)
                     _ (lst <>s seed out seed w8)
                     sq (tick ()) _ (rec sq seed (<>r * w8) (<>r * w8))
                     seed)
                 (k = 'for)
                  (: w8 (8 * w9)
                     ini <>s cnd <>>s stp <>>>s body <>>>>s
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
                            (: e8 <>ini
                               (? (<e8 = 'asn &&
                                   <<>e8 = 'var &&
                                   member? <><>e8 univ)
                                  <><>e8 ()))
                            ())
                     in (? !(two? ini) seed
                           (member? <ini '(decl sdecl)) (lst ini seed () () w9)   ; ticks (cgdecl does); kill applies; runs ONCE
                           (sun (? kv8 (sdel seed kv8) seed) <ri))
                     ; kv8 joins the record: the init's kill (see the expr lane)
                     sq (tick ()) _ (rec sq (sun (sun seed <ri) (? kv8 `(kv8) ())) (nc9 * w8 + <>ri * w9)
                                         (nc9 * w8 + (? (1 < w9) (<>ri * w9) 0)))
                     in)
                 (k = 'switch)
                  (: r (erd <>s () 0)
                     _ (pin n9 'sw (() >< peep n9 'sw ()))       ; push this switch's marker set
                     _ (litems <>>s out out cnt w9)              ; brk = out; cont targets the outer loop
                     macc <(peep n9 'sw ())
                     _ (pin n9 'sw >(peep n9 'sw ()))            ; pop
                     in (sun (sun <r macc) out)
                     sq (tick ()) _ (rec sq in (<>r * w9) (? (1 < w9) (<>r * w9) 0))
                     in)
                 (member? k '(case dflt))
                  (: sw (peep n9 'sw ())
                     _ (? (two? sw) (pin n9 'sw (sun <sw out >< >sw)) ())
                     _ (tick ()) out)
                 (k = 'brk) (: _ (tick ()) brk)
                 (k = 'cont) (: _ (tick ()) cnt)
                 (k = 'goto) (: _ (tick ()) univ)   ; any target: everything
                 (k = 'lbl) (: _ (tick ()) out)
                 (k = 'asm) (: sq (tick ()) _ (rec sq univ w9 (? (1 < w9) w9 0)) univ)   ; homing is off anyway (hasasm)
                 (: _ (tick ()) out))))             ; nop tdef
       ; -- run: liveout at the fn end is empty; the counter descends from the prepass total so
       ; lst's numbers land forward pre-order. ⚠ pre and lst disagreeing on the statement set is a
       ; compiler bug, so the counter must land exactly on zero or alive answers () --
       _ (pin n9 'q (peep n9 'n 0))
       _ (litems items () () () 1)
       (? (0 = peep n9 'q 0) `(liv crs univ (peep n9 'n 0) (peep n9 'ncw 0)) ()))
   ; -- the array universe (the vmap's array leg): once-declared 1-D arrays of a gp
   ; scalar whose every appearance is the element shape (deref (bin + (var x) idx)).
   ; the scan LICENSES exactly that shape -- recursing into the index only -- and any
   ; other (var x) marks decay; under & and sizeof nothing licenses (the over-mark
   ; only shrinks the universe, sizeof x being the memcpy idiom's tell anyway). --
   (auniv g f)
    (: items <><>>>f
       dcl2 #() tak2 #() ar2 #()
       (ael? ty) (arr? ty && !(gvarr? ty) &&
                  (: et <>ty (ptr? et ||
                              member? et '(char uchar bool short ushort int uint long ulong))))
       (adcl s)
        (? !(two? s) ()
           (: k <s
              (? (member? k '(decl sdecl))
                  (: (go ps) (? !(two? ps) ()
                                (: p <ps
                                   _ (pin dcl2 <p (1 + peep dcl2 <p 0))
                                   _ (? ((k = 'decl) && ael? <>p)
                                        (pin ar2 'l (<p >< peep ar2 'l ())) ())
                                   (go >ps)))
                     (go <>s))
                 (adcll (stkids s)))))
       (adcll l) (? !(two? l) () (: _ (adcl <l) (adcll >l)))
       (axx e) (? !(two? e) ()                       ; & / sizeof context: every var marks
                  (? (id? <e 'var && two? >e) (pin tak2 <>e 1)
                     (: (go l) (? !(two? l) () (: _ (axx <l) (go >l))) (go e))))
       (aex e)
        (? !(two? e) ()
           (id? <e 'deref && two? >e && two? <>e &&
            (: X <>e (id? <X 'bin && two? >X && id? <>X '+ &&
                      two? <>>X && id? <<>>X 'var)))
            (aex <>>>(<>e))                          ; the licensed shape: only the index scans
           ; a bare array handed to the non-retaining mem trio stays eligible: the callee
           ; keeps no copy of the address, and the call's own vmflush retires every pin
           ; (memcpy(x, in, sizeof x) is cc_block's fill -- THE motivating idiom)
           (id? <e 'call && two? >e && two? <>e && id? <<>e 'var &&
            member? <><>e '("memcpy" "memset" "memcmp"))
            (: (goa l) (? !(two? l) ()
                          (: a2 <l
                             _ (? (two? a2 && id? <a2 'var) () (aex a2))
                             (goa >l)))
               (goa <>>e))
           (member? <e '(addr szof)) (axx >e)
           (id? <e 'var && two? >e) (pin tak2 <>e 1)
           (: (go l) (? !(two? l) () (: _ (aex <l) (go >l))) (go e)))
       _ (adcll items)
       _ (aex items)
       (filter (\ nm ((peep dcl2 nm 0 = 1) && !(peep tak2 nm ())))
               (peep ar2 'l ())))
   ; the alive pass's forward twin: cgstmt/cgdecl/cgsdecl entries tick this counter and pin the
   ; statement's wrapset (g 'stwrap) off g 'livtab -- a miss pins 'all, the blanket wrap. an
   ; inline splice neither ticks nor re-pins: it compiles under its statement's set.
   (stq g)
    (? (peep g 'inlining ()) (peep g 'stwrap 'all)
       (: q (1 + peep g 'lsq 0)
          _ (pin g 'lsq q)
          lt (peep g 'livtab ())
          w (? lt (peep lt q 'all) 'all)
          _ (pin g 'stwrap w)
          w))
   ; the regen channels' NEUTRAL state, spelled once: per-fn entry and the
   ; regen teardown ride the same reset (a wrap set dies with its fn)
   (rgreset g)
    (pins (vmflush g)
     'livtab ()  ; liveness (set only for a wrap-bearing regen)
     'vuniv ()   ; vmap off (no universe)
     'vuarr ()   ; ...and its array leg
     'lhome ()   ; locals-homing map (cgdecl's hook reads it)
     'rgon ()    ; ...and its regen flag (the splice decl gate)
     'homes ())  ; non-leaf home set (call-fixed + sibs read it)
   ; ...and their ARMED state: a regen build homes locals (lh9, lpick's map)
   ; and wraps per-call off the liveness (av9, alive's answer)
   (rgset g lh9 av9 au9)
    (pins (vmflush g)      ; vmflush pin set is disjoint from rgset
     'vuniv <>>av9         ; the vmap rides the regen (univ known)
     'vuarr au9            ; ...and the array leg its universe
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
    'stwrap 'all)
   ; the g-pin RECORD: every per-fn pin and its neutral value, one roster -- cgfn's entry
   ; and each regen attempt wipe the lot, so an attempt always starts from the clean
   ; record. a NEW per-fn pin registers here; the computed ones (fn, epi, pool0/pool,
   ; vslots, the fc cells, sret's slot) re-pin over their neutrals at their owner.
   (fnreset g)
    (pins (rgreset g)          ; the regen channels' neutral state -- disjoint pin set
     'off 8                    ; the frame cursor: slot -8 = saved r3 (see epi)
     'fesc ()                  ; escape flag (the sibcall gate reads it)
     'inlining () 'inlbody 0   ; splice depth + body depth (vmget's blind)
     'inlret () 'inlrt ()      ; splice return label + type (cgstmt's ret door)
     'inlrb () 'inlrbs ()      ; splice decl-license + its reg roster
     'rpin ()                  ; splice bind pins
     'argseal 0                ; the vmap's staging seal depth
     'hasasm ()                ; an asm statement pins it (homing off)
     'cssv ()                  ; the callee-saved save forms (a cs regen pins)
     'sret ())                 ; the memory-return slot (the driver re-pins under sr6f)

   (cgstmt g env s)
    (: _ (psreset g)                                 ; a statement never carries a live operand in (vmap pins do)
       myw (stq g)                                   ; this statement's wrapset (alive's forward twin)
       k <s
       (? (k = 'ret)                                                     ; inside an inline splice a return JUMPS to the
          (: il (peep g 'inlret ())                                      ; splice's end label instead of the epilogue
             epj (peep g 'epi epi)                                       ; the TARGET's epilogue (cgen-core pins it)
             mt (1 < tally >s)                                           ; clay's (ret e PRE): presence is the arity
             (? (mt && il) 'bad                     ; a musttail never inlines: the splice declines, the real call still can
                !(two? <>s)
                 (? mt (nolane g "a musttail return carries no call") (? il `(`('jmp il)) epj))   ; bare `return;` -- just the epilogue
                (: _ (pin g 'want 'r0)                                   ; the return value's one true home
                   _ (? mt (pin g 'must9 1) 0)      ; the ret's own call STAYS a call (no splice) -- consumed at first touch
                   r (cgexpr g env <>s)
                   _ (pin g 'must9 ())
                   rt (? il (peep g 'inlrt 'long)                        ; the INLINED function's return type
                         <(peep (g 'sigs) (g 'fn) `('long ())))         ; else the current function's
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
                           + `(`('ld 'r1 'r4 (peep g 'sret 0)))
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
                       (tor0 g r + `(`('ld (ip2 g) 'r0 8) '(ld r0 r0 0)) + epj)
                      (id? (aone g rt) 'sse)                             ; a small SSE aggregate: its eightbyte in xmm0
                       (tor0 g r + fldf 'double (xmmreg 0) 'r0 0 + epj)
                      (id? (aone g rt) 'int)                             ; a small INT aggregate (a union rides here): rax
                       (tor0 g r + '((ld r0 r0 0)) + epj)
                      (amem? g rt)                                       ; a MEMORY return: fill the caller's temp through
                       (tor0 g r                                         ; the saved hidden pointer, and answer it in rax
                        + `(`('ld 'r1 'r4 (peep g 'sret 0)))
                        + bcopy g 0 (tsz g rt) () + '((mov r0 r1)) + epj)
                      (stru? rt || stru? (vty r))                        ; a MIXED 16-byte pair, and every composite class
                       (nolane g ("no lane for returning this " + show (tsz g (? (stru? rt) rt (vty r)))     ; arm64 has no MEMORY return yet
                                  + "-byte struct by value" + ontgt g))
                      (flo? (vty r)) (asintt g r rt + (? (id? rt 'bool) (cvt rt 'r0) ()) + (? il `(`('jmp il)) epj))     ; a double value, an int return -> truncate to r0
                      (pair? g rt) (asll g r + (? il `(`('jmp il)) epj))        ; a pair return: r0:r1 (widen a word value)
                      (d128? rt)                                                ; a wide return rides rdx:rax (the SysV i128 pair)
                       (? (x64t? g) (as128 g r + (? il `(`('jmp il)) epj)) 'bad)
                      (id? rt 'bool)                             ; a bool return CONVERTS (callers read r0 whole -- cc calls untyped)
                       (tor0 g r + cvt rt 'r0 + (? il `(`('jmp il)) epj))
                      (tor0 g r + (? il `(`('jmp il)) epj)))      ; int -> int, as before (a pair value narrows to its lo)
                   (? (!mt || bad? res) res (mustret g res)))))         ; a musttail ret MARKS its call for sibs
          (k = 'expr)
           (: pe <>s
              rv (? ((<pe = 'post) && (<<>pe = 'var))
                    (vent g env <><>pe) 'bad)
              (? (two? rv && (<rv = 'regv))     ; a statement-position x++ on a home: the old value is dead
                  (: ty <>>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     (`(`('add <>rv <>rv (* stp <>>pe))) + cvt ty <>rv))
                 ; statement-position asn of a CONSTANT into a static slot: the value is
                 ; dead and the slot is the destination, so it delivers as ONE store-
                 ; immediate (x64; a value-position asn keeps the protocol -- its answer
                 ; is read). the vmap-hint lane keeps priority: a resident store pays reads.
                 (: sv (? ((<pe = 'asn) && (<<>pe = 'var)) (vent g env <><>pe) 'bad)
                    sk (? (x64t? g && two? sv && member? <sv '(loc glo) &&
                           (: t9 <>>sv
                              !(stru? t9 || arr? t9 || flo? t9 || pair? g t9 ||
                                d128? t9 || id? t9 'bool)) &&
                           !(vmon? g && id? <sv 'loc && homable? <>>sv &&
                             member? <><>pe (peep g 'vuniv ())) &&
                           (<<>>pe = 'num) && charm? <><>>pe)
                          (siop9 <>>sv <><>>pe) ())
                    (? sk (: _ (vmdrop g <><>pe)
                             (? (id? <sv 'loc)
                                `(`(sk 'r4 <>sv <><>>pe))
                                `(`('la 'r1 <>sv) `(sk 'r1 0 <><>>pe))))
                       (: r (cgexpr g env pe)
                          (? (bad? r) 'bad (vdrop g r)))))))
          (k = 'nop) ()
          (k = 'tdef) ()                            ; a block-scope _Static_assert (already checked at parse)
          (k = 'blk) (cgitems g env <>s ())
          (k = 'if)
           (: le (nlab g ".e")
              c (cbranch g env <>s 'eq le)
              m0 (vmsnap g)                            ; the map on le's arriving branch edge
              t (cgstmt g env <>>s)
              (? (bad? c || bad? t) 'bad
                 !(two? <>>>s)
                  (: _ (vmset g (? (vmout? t) (vmeet m0 (vmsnap g)) m0))   ; label le joins
                     (c + t + `(`('label le))))
                 (: mt (? (vmout? t) `((vmsnap g)) ()) ; the then edge into ld, when it arrives
                    _ (vmset g m0)                     ; the else arm enters off the branch edge
                    el (cgstmt g env <>>>s)
                    ld (nlab g ".d")
                    me (? (vmout? el) `((vmsnap g)) ())
                    _ (vmset g (? (two? mt) (? (two? me) (vmeet <mt <me) <mt)
                                  (two? me) <me ()))   ; label ld joins the arriving edges
                    (? (bad? el) 'bad
                     (c + t + `(`('jmp ld) `('label le)) + (el + `(`('label ld))))))))
          (k = 'while)
           (: lc (nlab g ".c") ld (nlab g ".d")
              _ (vmflush g)                            ; label lc heads the loop block
              c (cbranch g env <>s 'eq ld)
              _ (pins g 'brk (ld >< g 'brk)
                        'cnt (lc >< g 'cnt))
              b (cgstmt g env <>>s)
              _ (pins g 'brk >(g 'brk)
                        'cnt >(g 'cnt))
              _ (vmflush g)                            ; label ld joins
              (? (bad? c || bad? b) 'bad
               (`(`('label lc)) + c + (b + `(`('jmp lc) `('label ld))))))
          (k = 'for)
           (: init <>s cnd <>>s stp <>>>s body <>>>>s
              lc (nlab g ".c") ls (nlab g ".s") ld (nlab g ".d")
              ir (? !(two? init) `(() env)
                    (<init = 'decl) (cgdecl g env <>init)
                    (<init = 'sdecl) (cgsdecl g env <>init)
                    (: r (cgexpr g env <>init)
                       (? (bad? r) 'bad `((vdrop g r) env))))
              (? (bad? ir) 'bad
                 (: env2 <>ir
                    _ (pin g 'stwrap myw)            ; a decl init re-pinned; the cond is OURS
                    _ (vmflush g)                    ; label lc heads the loop block
                    c (? (two? cnd) (cbranch g env2 cnd 'eq ld) ())
                    _ (pins g 'brk (ld >< g 'brk)
                              'cnt (ls >< g 'cnt))
                    b (cgstmt g env2 body)
                    _ (pins g 'brk >(g 'brk)
                              'cnt >(g 'cnt)
                              'stwrap myw)           ; the body re-pinned as it went; the step is OURS
                    _ (vmflush g)                    ; label ls heads the step block
                    st2 (? (two? stp) (cgexpr g env2 stp) ())
                    _ (vmflush g)                    ; label ld joins
                    (? (bad? c || bad? b || bad? st2) 'bad
                       (<ir + `(`('label lc)) + (? (two? cnd) c ()) +
                        b + `(`('label ls)) + (? (two? st2) (vdrop g st2) ()) +
                        `(`('jmp lc) `('label ld)))))))
          (k = 'brk) (? (two? (g 'brk)) `(`('jmp <(g 'brk))) 'bad)
          (k = 'cont) (? (two? (g 'cnt)) `(`('jmp <(g 'cnt))) 'bad)
          (k = 'do)
           (: lt (nlab g ".t") lc (nlab g ".c") ld (nlab g ".d")
              _ (pins g 'brk (ld >< g 'brk)
                        'cnt (lc >< g 'cnt))
              _ (vmflush g)                           ; label lt heads the loop block
              b (cgstmt g env <>s)
              _ (pins g 'brk >(g 'brk)
                        'cnt >(g 'cnt)
                        'stwrap myw)                 ; the body re-pinned as it went; the cond is OURS
              _ (vmflush g)                          ; label lc heads the cond block
              c (cbranch g env <>>s 'ne lt)
              _ (vmflush g)                          ; label ld joins
              (? (bad? b || bad? c) 'bad
                 (`(`('label lt)) +
                  b +
                  `(`('label lc)) +
                  (c + `(`('label ld))))))
          (k = 'goto) `(`('jmp (intern (g 'fn + "." + <>s))))
          (k = 'lbl) (: _ (vmflush g) `(`('label (intern (g 'fn + "." + <>s)))))
          (k = 'asm) (cgasm g env s)
          ; a case/default label -- anywhere in the body, tied to the nearest enclosing switch by
          ; VALUE: the switch's pre-scan pinned value->label into the tablet atop g 'cases, and
          ; this emits it. no enclosing switch, or an unscanned value, refuses rather than mislabel.
          (k = 'case)
           (? !(two? (g 'cases)) 'bad
              (: lab (peep <(g 'cases) <>s 'no) _ (vmflush g)
                 (? (lab = 'no) 'bad `(`('label lab)))))
          (k = 'dflt)
           (? !(two? (g 'cases)) 'bad
              (: lab (peep <(g 'cases) 'dflt 'no) _ (vmflush g)
                 (? (lab = 'no) 'bad `(`('label lab)))))
          (k = 'switch)
           (: c (cgexpr g env <>s)
              (? (bad? c) 'bad
                 (: items <>>s
                    cr9 (vreg c)
                    lend (nlab g ".d")
                    tab #()               ; this switch's value->label map (and 'dflt->label)
                    cases (rev (scasel-l g items tab ()))   ; pre-scan the body (a stmt LIST) at ANY depth -> (value label) dispatch pairs
                    dflt (peep tab 'dflt lend)
                    disp (: (go l acc)
                             (? !(two? l) acc
                                (: e2 <l  v (cfit g (vty c) <e2)  lab <>e2
                                   ins (? (immok g 'cmp v)
                                          `(`('cmp cr9 v) `('br 'eq lab))
                                          `(`('li 'r1 v) `('cmp cr9 'r1) `('br 'eq lab)))
                                   (go >l (acc + ins))))
                            (go cases ()))
                    _ (rfree g cr9)
                    _ (pins g 'brk (lend >< g 'brk)
                              'cases (tab >< g 'cases)) ; PUSH -- cgstmt emits each case/dflt label from here
                    body (cgitems g env items ())       ; decls thread env; case/dflt labels emit via g 'cases
                    _ (pins g 'brk >(g 'brk)
                              'cases >(g 'cases))      ; POP
                    _ (vmflush g)                       ; label lend joins
                    (? (bad? body) 'bad
                       (vfs c + disp + `(`('jmp dflt)) + (body + `(`('label lend))))))))
          'bad))
   (cgitems g env items acc)
    (? !(two? items) acc
       (: it <items
          (? (<it = 'decl)
             (: r (cgdecl g env <>it)
                (? (bad? r) 'bad
                   (cgitems g <>r >items (acc + <r))))
             (<it = 'sdecl)
             (: r (cgsdecl g env <>it)
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
       (: k <it
          (? (k = 'case)  (: lab (nlab g ".k") _ (pin tab <>it lab) (L <>it lab >< acc))
             (k = 'dflt)  (: lab (nlab g ".k") _ (pin tab 'dflt lab) acc)
             (k = 'switch) acc                                    ; a nested switch owns its labels
             (scasel-l g (stkids it) tab acc))))
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
                 (id? <x 'call && (6 < tally <>>x)) 1           ; an overflow-arg call moves sp -- splicing it bars the caller's unframe
                 (noinl? <x || noinl? >x))
   (oneret? items)                                  ; decls* + one trailing `return expr` (the b2w shape)
    (? !(two? items) ()
       (: it <items
          (? (<it = 'decl) (oneret? >items)
             ((<it = 'ret) && two? <>it && !(two? >items)))))
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
          (!aln && ((? (oneret? items) 600 250) < tally (show <>>>f))) ()
          !(noinl? <>>>f)))
   (cginl g env f args)                              ; splice f's body at the site -> (ty forms) | 'no (declined)
    (: fe0 (peep g 'fesc ())                          ; a splice that pins a CLEAR fesc would cost the CALLER
       strs0 (g 'strs)                                ; every sibcall (the lvm_peep stack blowup) -- decline it
       il0 (peep g 'inlining ())
       ir0 (peep g 'inlret ())
       irt0 (peep g 'inlrt ())
       _ (pin g 'inlining ((? !il0 0 il0) + 1))
       rt <(peep (g 'sigs) <>f `('long ()))
       ilb9 (nlab g ".il")
       rp0 (peep g 'rpin ())
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
       (paid? nm) (7 < rdw nm body9 1)              ; the seat pricing: weighted body reads earn a register
       nocall (!(callish? body9) && !(anyc args))
       (bind ps as benv acc rbs)                     ; arg values -> the alias register, a priced park, else slots
        (? !(two? ps) `(benv acc rbs)
           (: pty <><ps
              al (? (nocall && !(flo? pty) && 8 = tsz g pty &&
                     two? <as && <<as = 'var &&
                     !(wrs? <<ps body9))
                    (: nm9 <><as
                       pM (vmget g nm9)
                       (? pM pM
                          (: v9 (vent g env nm9)
                             (? (two? v9 && <v9 = 'regv && !(flo? <>>v9))
                                <>v9 ()))))
                    ())
              al (? al (? (alsafe? g al) al ()) ())  ; staging must never write it
              (? al                                  ; the alias bind: zero forms
                  (: _ (pin g 'rpin (al >< peep g 'rpin ()))
                     (bind >ps >as (`(<<ps `('reg al) pty) >< benv) acc rbs))
                 (: r (cgexpr g env <as)
                    (? (bad? r) 'bad
                       (pair? g pty)                 ; a pair param: an 8-byte slot, both words land
                        (: off (ntmp g 'llong)
                           (bind >ps >as
                                 (`(<<ps off pty) >< benv)
                                 (acc + asll g r + pst 'r4 off) rbs))
                       (: rb (? (nocall && !(flo? pty) &&
                                 paid? <<ps &&
                                 deep? (peep g 'pool ()))              ; a seat is EARNED: paid reads into a deep pool
                                 (ralloc g) ())
                          (? rb                      ; the park bind: the value rides a pool reg
                              (: _ (pin g 'rpin (rb >< peep g 'rpin ()))
                                 (bind >ps >as (`(<<ps `('reg rb) pty) >< benv)
                                       (acc + asintt g r pty
                                            + (`(`('mov rb 'r0)) + cvt pty rb))
                                       (rb >< rbs)))
                             (: off (ntmp g 'long)
                                (bind >ps >as
                                      (`(<<ps off pty) >< benv)
                                      (acc + asintt g r pty
                                           + (? (id? pty 'bool) (cvt pty 'r0) ())   ; the entry conversion the splice skips
                                           + `(`('st 'r4 off 'r0)))
                                      rbs)))))))))
       br (bind <>>f args () () ())
       ; the body-decl license: a decl name whose weighted reads price a pool seat (the
       ; park's discipline) register-binds in cgdecl's splice lane; nesting saves/restores
       rb0 (peep g 'inlrb ())
       rbs0 (peep g 'inlrbs ())
       (dnames x a9)
        (? !(two? x) a9
           (id? <x 'decl && two? >x)
            (: (dn ps a) (? !(two? ps) a (dn >ps (<<ps >< a)))
               (dn <>x a9))
           (dnames <x (dnames >x a9)))
       _ (pins g 'inlrb (? nocall (filter paid? (dnames body9 ())) ())
          'inlrbs () 'inlret ilb9 'inlrt rt)
       ; ⚠ cgitems rides the g tablet, not a direct call: the direct edge closes the
       ; cgexpr->cginl->cgitems->cgstmt->cgexpr cycle, and the loader's analysis then re-weaves
       ; the scope so several `epi` reads compile as book reads before the pin. the indirection
       ; stays until the core analysis owns cycles in big body-less letrecs.
       ib0 (peep g 'inlbody 0)
       _ (pin g 'inlbody (ib0 + 1))                ; the body blinds vmget (name conflation)
       ibd (? (bad? br) 'bad ((g 'cgitems) g <br body9 ()))
       _ (pins g 'inlbody ib0 'inlret ir0 'inlrt irt0 'inlining il0 'rpin rp0) ; the bind pins die with the splice
       drbs (peep g 'inlrbs ())
       _ (pins g 'inlrb rb0 'inlrbs rbs0)
       (unrb l) (? !(two? l) ()
                   (: _ (rfree g <l) (unrb >l)))
       _ (? (two? br) (unrb (nthr br 2)) ())          ; the park regs return to the pool...
       _ (unrb drbs)                                  ; ...and the body-decl binds with them
       _ (vmflush g)                                  ; the splice's end label joins
       (? (bad? br || bad? ibd)                       ; a shape the splice can't take: the real call still can
           (: _ (pin g 'fesc fe0) _ (pin g 'strs strs0) 'no)
          (!fe0 && peep g 'fesc ())                   ; the splice took a frame address (an & arg too -- the
           (: _ (pin g 'fesc ()) _ (pin g 'strs strs0) 'no)   ; real call's arg lane wraps it, bind cannot): keep the caller lean
          (mkv rt 'r0 (<>br + ibd + `(`('label ilb9))))))

   (cgfn g f)                                       ; ('fn name params ('blk items)) -> forms | 'bad
    (: params <>>f
       _ (fnreset g)                                 ; the whole per-fn pin record wiped (one roster)
       ; v6-M has no FPU registers at all: the f0/f1/f15 fiction lives in three 8-byte frame CELLS
       ; allocated up front, and soften rewrites every f-op onto them
       _ (? (v6m? g) (: _ (pins g 'fc0 (ntmp g 'double)
                                  'fc1 (ntmp g 'double)
                                  'fc15 (ntmp g 'double)) 0) 0)
       _ (pin g 'fn <>f)
       ; -- C99 VLA prep: scan the body for runtime-dim locals; each site takes a POINTER slot +
       ; a SIZE slot in the static frame, the size slots ZEROED after the param spill (the decl
       ; lane's re-execution guard reads 0 = never allocated). --
       _ (pin g 'vslots #())
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
                     _ (pin (g 'vslots) <l `(po so))
                     (mk >l (a2 + `(`('st 'r4 so 'r0))))))
              (? !(two? idxs) () ('((li r0 0)) + mk idxs ())))
       sig (peep (g 'sigs) <>f ())
       vararg (? (two? sig) (nthr sig 2) ())
       ; the epilogue is per-FN: a thumb2 vararg fn drops its r0-r3 push block on
       ; the way out (epi-t32va); everyone else keeps the target default
       _ (pin g 'epi (? (vararg && t32? g) (? (v6m? g) epi-t16va epi-t32va)
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
        (? !(two? ps) `(acc env)
           (: p <ps pty <>p
              (? (v6m? g && sse2? g pty)                ; v6-M: a POSITION-0 16B composite arrives whole in r0-r3
                  (? (gpi = 0)                          ;  (the caller's g4 lane); any later start splits -- deferred
                     (: off (nslot g 16)
                        (spill hp >ps 4 xmmi ovi (`(<p off pty) >< env)
                               (acc + `(`('st 'r4 off 'r0) `('st 'r4 (off + 4) 'r1)
                                        `('st 'r4 (off + 8) 'r2) `('st 'r4 (off + 12) 'r3)))))
                     'bad)
                 (t32? g && stru? pty &&                ; t32 params: the d-pair HFA, the 8B sse blob, the <=4B int
                  !(sse2? g pty ||                      ; one (the gp lane's word store IS its <=4 bytes);
                    (id? (aone g pty) 'sse && (tsz g pty = 8)) ||       ; mixed/int-pair composites refuse LOUD
                    (id? (aone g pty) 'int && (tsz g pty <= 4)))) 'bad
                 (v6m? g && stru? pty && id? (aone g pty) 'sse)           ; v6-M 8B blob param: the even-odd gp pair (the caller's g2 lane)
                  (: gpe (gpi + 1 & -2)
                     (? (gpe + 2 <= ngp g)
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi (`(<p off pty) >< env)
                                   (acc + `(`('st 'r4 off (nthr (argr g) gpe))
                                            `('st 'r4 (off + 4) (nthr (argr g) (gpe + 1)))))))
                        (: ov8 (ovi + 7 & -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) (`(<p (ovb g + ov8) pty) >< env) acc))))
                 (stru? pty && two? (aeb2 g pty))       ; a 9..16B struct param: two eightbytes into a 16-byte slot, each from a gp or an xmm reg
                  (? (pol? hp && !(sse2? g pty)) 'bad        ; a policy homes gp scalars only -- mixed/int-pair 16B deopts
                     (: cls (aeb2 g pty) c0 <cls c1 <>cls
                        ; the register FILE per eightbyte -- 'g gp / 'x xmm. SysV (x64) picks
                        ; by the eightbyte's class; AAPCS64 (arm64) sends a non-HFA composite
                        ; wholly in X regs, only an all-float HFA in V regs.
                        hfa (id? c0 'sse && id? c1 'sse)
                        f0 (? (a64? g) (? hfa 'x 'g) (? (id? c0 'sse) 'x 'g))
                        f1 (? (a64? g) (? hfa 'x 'g) (? (id? c1 'sse) 'x 'g))
                        gpn (+ (? (id? f0 'g) 1 0) (? (id? f1 'g) 1 0))
                        xmn (+ (? (id? f0 'x) 1 0) (? (id? f1 'x) 1 0))
                        (? ((ngp g < (gpi + gpn)) || (8 < (xmmi + xmn)))     ; not enough regs -> the WHOLE aggregate goes to memory (2 stack eightbytes)
                            (? (pol? hp) 'bad           ; under a policy: deopt, not a fresh memory lane
                               (t32? g) 'bad            ; t32 stack HFA params (9+ doubles): deferred, refuse LOUD
                               (spill hp >ps gpi xmmi (ovi + 2) (`(<p (ovb g + (* 8 ovi)) pty) >< env) acc))
                            (: off (nslot g 16)
                               st0 (? (id? f0 'g) `(`('st 'r4 off (nthr (argr g) gpi))) (fstf g 'double 'r4 off (xmmreg xmmi)))
                               gp1 (? (id? f0 'g) (gpi + 1) gpi)
                               xm1 (? (id? f0 'x) (xmmi + 1) xmmi)
                               st1 (? (id? f1 'g) `(`('st 'r4 (off + 8) (nthr (argr g) gp1))) (fstf g 'double 'r4 (off + 8) (xmmreg xm1)))
                               gp2 (? (id? f1 'g) (gp1 + 1) gp1)
                               xm2 (? (id? f1 'x) (xm1 + 1) xm1)
                               (spill hp >ps gp2 xm2 ovi (`(<p off pty) >< env) (acc + st0 + st1))))))
                 (stru? pty && id? (aone g pty) 'sse)       ; a small SSE aggregate: one xmm into an 8-byte slot
                  (? (xmmi < 8)
                     (: off (nslot g 8)
                        (spill hp >ps gpi (xmmi + 1) ovi (`(<p off pty) >< env)
                               (acc + fstf g 'double 'r4 off (xmmreg xmmi))))
                     (? (t32? g) 'bad                   ; t32 stack blob params: deferred, refuse LOUD
                        (spill hp >ps gpi xmmi (ovi + 1) (`(<p (ovb g + (* 8 ovi)) pty) >< env) acc)))
                 ; a MEMORY-class param: the caller laid the whole aggregate in the overflow
                 ; block, and by SysV that memory IS the callee's copy -- so the param binds
                 ; STRAIGHT to it, no slot and no store, as an overflow scalar does
                 (amem? g pty)
                  (? (pol? hp || 8 < asalign g pty) 'bad     ; under a policy, or over-aligned: deopt/refuse
                     (spill hp >ps gpi xmmi (ovi + aslots g pty)
                            (`(<p (ovb g + 8 * ovi) pty) >< env) acc))
                 (stru? pty && !(id? (aone g pty) 'int))        ; MIXED pair stays ahead; a small INT one rides the gp lane
                  (nolane g ("no lane for passing this " + show (tsz g pty)
                             + "-byte struct by value" + ontgt g))
                 (v6m? g && id? pty 'float)             ; v6-M base ABI: a bare float param is ONE WORD
                  (? (gpi < ngp g)                      ;  (binary32 bits in a gp reg / a 4-byte caller slot);
                     (: off (nslot g 8) ;  clval's fldf 'float widens it on read
                        (spill hp >ps (gpi + 1) xmmi ovi (`(<p off pty) >< env)
                               (acc + `(`('st 'r4 off (nthr (argr g) gpi))))))
                     (spill hp >ps gpi xmmi (ovi + 4) (`(<p (ovb g + ovi) pty) >< env) acc))
                 (v6m? g && flo? pty)                   ; v6-M base ABI: a double param IS a gp pair
                  (: gpe (gpi + 1 & -2)               ;  (no FPU regs exist) -- spill both words, or
                     (? (gpe + 2 <= ngp g)            ;  read the 8-aligned caller stack
                         (: off (nslot g 8)
                          (spill hp >ps (gpe + 2) xmmi ovi (`(<p off pty) >< env)
                           (acc + `(`('st 'r4 off (nthr (argr g) gpe))
                            `('st 'r4 (off + 4) (nthr (argr g) (gpe + 1)))))))
                        (: ov8 (ovi + 7 & -8)
                         (spill hp >ps 4 xmmi (ov8 + 8) (`(<p (ovb g + ov8) pty) >< env) acc))))
                 (flo? pty && xmmi < 8)
                  (: off (nslot g 8)
                     (spill hp >ps gpi (xmmi + 1) ovi (`(<p off pty) >< env)
                            (acc + fstf g pty 'r4 off (xmmreg xmmi))))
                 (pair? g pty)                          ; a pair param: an even-odd register pair spills
                  (: gpe (gpi + 1 & -2)               ;  both words; else the 8-aligned stack (gp closes)
                     (? (gpe + 2 <= ngp g)
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi (`(<p off pty) >< env)
                                   (acc + `(`('st 'r4 off (nthr (argr g) gpe))
                                            `('st 'r4 (off + 4) (nthr (argr g) (gpe + 1)))))))
                        (: ov8 (ovi + 7 & -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) (`(<p (ovb g + ov8) pty) >< env) acc))))
                 (t32? g && flo? pty) 'bad              ; a t32 double param past d7: the 4-byte slot can't carry it -- refuse LOUD
                 (d128? pty) 'bad                       ; a wide param: not carried (mirror of the arg refusal)
                 (!(flo? pty) && gpi < ngp g)
                  (? (pol? hp && gpi < nhome g &&
                      (homable? pty ||
                       (two? hp && (member? gpi <>hp || member? gpi (nthr hp 2)))))
                      (? (two? hp)                      ; the leaf lane: home in a register, moving from the
                         (: inc (nthr (argr g) gpi)     ; incoming arg reg if it differs -- but a rideable param
                            ; rides its ARRIVAL register: no home reserved, no entry
                            ; mov, its home register freed to the pool
                            home (? (member? gpi <>hp) inc (homereg gpi))
                            mv ((? (home = inc) () `(`('mov home inc))) + cvt pty home)
                            (spill hp >ps (gpi + 1) xmmi ovi (`(<p `('reg home) pty) >< env)
                                   (acc + mv)))
                         (: home (homereg gpi)          ; the non-leaf lane: home + the spill shadow
                            inc (nthr (argr g) gpi)
                            off (nslot g 8)
                            _ (pin g 'homes (`(home off <p) >< peep g 'homes ()))
                            mv ((? (home = inc) () `(`('mov home inc))) + cvt pty home)
                            (spill hp >ps (gpi + 1) xmmi ovi (`(<p `('reg home) pty) >< env)
                                   (acc + mv))))
                     (: off (nslot g 8)
                        ; a bool param CONVERTS its arrival (cc calls untyped, so the
                        ; caller passed the bare int) -- the object invariant holds at entry
                        cv9 (? (id? pty 'bool) (cvt pty (nthr (argr g) gpi)) ())
                        (spill hp >ps (gpi + 1) xmmi ovi (`(<p off pty) >< env)
                               (acc + cv9 + `(`('st 'r4 off (nthr (argr g) gpi)))))))
                 ; an overflow param reads off the caller's block: 8-byte slots (x64/a64), 4-byte on t32
                 (t32? g) (spill hp >ps gpi xmmi (ovi + 4) (`(<p (ovb g + ovi) pty) >< env) acc)
                 (spill hp >ps gpi xmmi (ovi + 1) (`(<p (ovb g + 8 * ovi) pty) >< env) acc))))
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
           savegp (: (go i acc) (? (5 < i) acc (go (i + 1) (acc + `(`('st 'r4 (rsa + 8 * i) (nthr argregs i)))))) (go 0 ()))
           savex  (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + `(`('stsd 'r4 (rsa + 48 + 16 * i) (xmmreg i)))))) (go 0 ()))
           (cls qs gpi xmmi ov env acc)
            (? !(two? qs) (: _ (pins g 'vagp gpi 'vaxmm xmmi) `(((savegp + savex) + acc) env))
               (: p <qs pty <>p
                  eb2 (aeb2 g pty)
                  ovf (\ u (cls >qs gpi xmmi (ov + 8 * (? (stru? pty) (aslots g pty) 1))
                                (`(<p (ovb g + ov) pty) >< env) acc))
                  (? (amem? g pty) (? (8 < asalign g pty) 'bad (ovf ()))
                     (d128? pty) (nolane g "a wide param is not carried in a variadic function")
                     eb2 (: f0 (? (id? <eb2 'sse) 'x 'g) f1 (? (id? <>eb2 'sse) 'x 'g)
                             gpn (+ (? (id? f0 'g) 1 0) (? (id? f1 'g) 1 0))
                             xmn (+ (? (id? f0 'x) 1 0) (? (id? f1 'x) 1 0))
                             (? ((6 < (gpi + gpn)) || (8 < (xmmi + xmn))) (ovf ())   ; short of registers: the WHOLE aggregate goes to memory
                                (2 = gpn) (cls >qs (gpi + 2) xmmi ov (`(<p (rsa + 8 * gpi) pty) >< env) acc)
                                (: off (nslot g 16)
                                   st0 (? (id? f0 'g) `(`('st 'r4 off (nthr argregs gpi))) (fstf g 'double 'r4 off (xmmreg xmmi)))
                                   gp1 (? (id? f0 'g) (gpi + 1) gpi)
                                   xm1 (? (id? f0 'x) (xmmi + 1) xmmi)
                                   st1 (? (id? f1 'g) `(`('st 'r4 (off + 8) (nthr argregs gp1))) (fstf g 'double 'r4 (off + 8) (xmmreg xm1)))
                                   (cls >qs (gpi + gpn) (xmmi + xmn) ov (`(<p off pty) >< env) (acc + st0 + st1)))))
                     (flo? pty || id? (aone g pty) 'sse)
                      (? (7 < xmmi) (ovf ())
                         (cls >qs gpi (xmmi + 1) ov (`(<p (rsa + 48 + 16 * xmmi) pty) >< env) acc))
                     (5 < gpi) (ovf ())
                     (cls >qs (gpi + 1) xmmi ov (`(<p (rsa + (* 8 gpi)) pty) >< env) acc))))
           (cls ps 0 0 0 () ()))
       ; a VARIADIC function (AAPCS64): a 192-byte save area -- x0..x7 (@ +0, step 8) then q0..q7
       ; (@ +64, step 16; the low 8 bytes carry the double, the rest don't-care). named register
       ; params are addressed INSIDE the area, named overflow params on the caller stack.
       (vaspill-a64 ps)
        (: rsa (nslot g 192)
           _ (pin g 'rsa rsa)
           savegp (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + `(`('st 'r4 (rsa + 8 * i) (nthr argregs-a64 i)))))) (go 0 ()))
           savevr (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + fstf g 'double 'r4 (rsa + 64 + 16 * i) (xmmreg i)))) (go 0 ()))
           (cls qs gpi vri ov env)
            (? !(two? qs) (: _ (pins g 'vagp gpi 'vaxmm vri 'vaov ov) `((savegp + savevr) env))
               (: p <qs pty <>p
                  (? (flo? pty)
                      (? (7 < vri) (cls >qs gpi vri (ov + 1) (`(<p (ovb g + 8 * ov) pty) >< env))
                         (cls >qs gpi (vri + 1) ov (`(<p (rsa + 64 + 16 * vri) pty) >< env)))
                     (? (7 < gpi) (cls >qs gpi vri (ov + 1) (`(<p (ovb g + 8 * ov) pty) >< env))
                        (cls >qs (gpi + 1) vri ov (`(<p (rsa + 8 * gpi) pty) >< env))))))
           (cls ps 0 0 0 ()))
       ; a VARIADIC function (AAPCS32, thumb2 + v6m): no save area of its own -- the prologue
       ; pushed r0-r3 just under the caller's stack args, one contiguous word array past the push
       ; cells ((ovb g): thumb2 lr/fp = 8, v6m lr/fp/r4 = 12). a named register param is addressed
       ; INSIDE the block (fp+ovb+4i), a named overflow param past it. WORD named params only;
       ; doubles/pairs/structs refuse LOUD.
       (vaspill-t32 ps)
        (: (cls qs gpi ov env)
            (? !(two? qs) (: _ (pin g 'vagp gpi) _ (pin g 'vaov ov) `(() env))
               (: p <qs pty <>p
                  (? (flo? pty || llt? pty || stru? pty) 'bad
                     (3 < gpi) (cls >qs gpi (ov + 4) (`(<p (ovb g + 16 + ov) pty) >< env))
                     (cls >qs (gpi + 1) ov (`(<p (ovb g + 4 * gpi) pty) >< env)))))
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
            (? !(two? qs) (: _ (pins g 'vagp gpi 'vaov ov) `(() env))
               (: p <qs pty <>p
                  (? (stru? pty) 'bad
                     (7 < gpi) (cls >qs gpi (ov + 1) (`(<p ((ovb g + 64) + 8 * ov) pty) >< env))
                     (cls >qs (gpi + 1) ov (`(<p (ovb g + 8 * gpi) pty) >< env)))))
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
       (anyop? fs op) (? !(two? fs) () (: f <fs (? (two? f && <f = op) 1 (anyop? >fs op))))
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
        (: best (tablet 0)
           (mxu m c) (? (id? c 'dead) m (? (m < c) c m))
           (bpin lab c) (? (id? c 'dead) ()
                           (: b (peep best lab 0) (? (b < c) (pin best lab c) ())))
           (go l cur mx)
            (? !(two? l) (mxu mx cur)
               (: f <l
                  (? !(two? f) (go >l cur mx)
                     (: k <f
                        (? (k = 'label)
                            (go >l (: b (peep best <>f 0)
                                      (? (id? cur 'dead) b (? (b < cur) cur b))) mx)
                           (member? k '(call callr)) (go >l (? (id? cur 'dead) 'dead (cur + 1)) mx)
                           (k = 'br) (: _ (bpin <>>f cur) (go >l cur mx))
                           (k = 'jmp) (: _ (bpin <>f cur) (go >l 'dead (mxu mx cur)))
                           (member? k '(ret jmpr)) (go >l 'dead (mxu mx cur))
                           (go >l cur mx))))))
           (go fs 0 0))
       (pslots env acc)                             ; the homable params' PASS-1 frame slots
        (? !(two? env) acc
           (: e2 <env
              (pslots >env (? (!(two? <>e2) && (<>e2 < 0) && homable? <>>e2)
                                   (<>e2 >< acc) acc))))
       (nreads fs offs n)                           ; frame touches of those slots in ir1
        (? !(two? fs) n
           (: f <fs
              hit (? !(two? f) ()
                     (member? <f ldops9) ((<>>f = fb9) && member? <>>>f offs)
                     (member? <f gstops) ((<>f = fb9) && member? <>>f offs)
                     ())
              (nreads >fs offs (? hit (n + 1) n))))
       (build sp0)                                  ; assemble prologue + body + epilogue for a spill result
        (? (bad? sp0) 'bad
           (: _ (bldreset g)
              body (cgitems g <>sp0 <><>>>f ())
              (? (bad? body) 'bad
                 (: frame (peep g 'off 0 + 15 & -16)
                    (? (arm? g)
                       ; the sibcall pass rides all targets (epim?/sibjmp read the epilogue off
                       ; g 'epi), but the IR sweeps still model x64 shapes -- so arm rides the raw
                       ; forms, retargeted r4 -> fp AFTER the sibcall rewrite (its patterns speak
                       ; r4; the epilogue forms already say fp/sp/lr). lr rides its own push cell,
                       ; and there is no r3 slot: x3/r3 is caller-saved under AAPCS.
                       ((? (v6m? g) (\ ir (t16la (t16slots ir frame)))   ; v6m: fp-neg slots -> r4-positive (after a4ize), then la -> literal pool
                           (sp64? g) (\ ir (t16la ir))       ; playdate: la -> pooled ABS32 word (its loader can't relocate MOVW/MOVT)
                           (\ ir ir))
                        (a4ize
                         (sibcall g
                          (soften g
                          ((? (v6m? g)   ; v6m: extra push r4 (bottom base), planted AFTER the sub; fp stays at top
                              (`(`('label (intern <>f))) +
                               (? vararg   ; vararg v6m: r0-r3 land above lr/fp/r4, contiguous with the caller stack args
                                  '((push r3) (push r2) (push r1) (push r0))
                                  ()) +
                               `('(push lr) '(push fp) '(push r4)
                                  '(lea fp sp 0) `('sub 'sp 'sp frame) '(lea r4 sp 0)))
                              (`(`('label (intern <>f))) +
                               (? (vararg && t32? g)     ; vararg t32: r0-r3 land under fp/lr,
                                  '((push r3) (push r2) (push r1) (push r0))               ; contiguous with the caller stack args
                                  (vararg && rv? g)      ; vararg riscv: a0-a7 in a packed 64-byte block
                                  `('(sub sp sp 64)      ; under fp/lr, contiguous with the caller stack args
                                     '(st sp 0 r0) '(st sp 8 r1) '(st sp 16 r2) '(st sp 24 r3)
                                     '(st sp 32 r4) '(st sp 40 r5) '(st sp 48 r6) '(st sp 56 r7))
                                  ()) +
                               `('(push lr) '(push fp) '(lea fp sp 0) `('sub 'sp 'sp frame)))) +
                           <sp0 + body + '((li r0 0)) + peep g 'epi epi-a64)))))
                       (deadst
                        (deaddef      ; rerun: the widened addrfold strands live-dest addr insns
                        (cmpfuse
                        (addrfold
                        (dehusk
                        (stld
                        (spmerge
                        (deaddef
                        (deadcell
                        (cfoldir
                         (sibcall g
                          (`(`('label (intern <>f))
                              '(push r4) '(mov r4 sp) `('sub 'sp 'sp frame)
                              '(st r4 -8 r3)) +
                           peep g 'cssv () + <sp0 +
                           body + '((li r0 0)) + peep g 'epi epi)))))))))))))))))
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
              (? (two? f && member? <f gstops && id? <>>>f A) `(<>f <>>f)
                 (rst-stoff >fs A))))
       (rst-defok fs A boff)                        ; every def of A is a reload from boff -> 1 | ()
        (? !(two? fs) 1
           (: f <fs
              (? !(two? f) (rst-defok >fs A boff)
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (? (member? <f '(label jmp br ret jmpr)) (rst-defok >fs A boff) ())
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
       ; expressions never go dry). answers (L lhome-tablet remaining-pool) | (). --
       (ntouch fs off n)
        (? !(two? fs) n
           (: f2 <fs
              hit (? !(two? f2) ()
                     (member? <f2 ldops9) ((<>>f2 = fb9) && (<>>>f2 = off))
                     (member? <f2 gstops) ((<>f2 = fb9) && (<>>f2 = off))
                     ())
              (ntouch >fs off (? hit (n + 1) n))))
       ; only 'lea marks a slot address-taken (the & lane, a struct/va temp) -- a 'lean is an
       ; lvalue address CONSUMED by its own load/store and never escapes, exactly as the sibcall
       ; gate leas? reads it
       (leaoffs fs t) (? !(two? fs) t
                         (: f2 <fs
                            _ (? (two? f2 && id? <f2 'lea && (<>>f2 = fb9))
                                 (pin t <>>>f2 1) ())
                            (leaoffs >fs t)))
       (lastn l n) (: d (tally l - n)
                      (go2 l2 k) (? (k < 1) l2 (go2 >l2 (k - 1)))
                      (? (d < 1) l (go2 l d)))
       (dropn l n) (? ((n < 1) || !(two? l)) l (dropn >l (n - 1)))
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
           lt (leaoffs ir1x (tablet 0))
           navl (tally hp9 - 2)
           (walk rs acc)
            (? !(two? rs) acc
               (: e2 <rs sq <e2 off <>e2
                  (? (peep lt off ()) (walk >rs acc)
                     (d128? <>>e2) (walk >rs acc)     ; a pair homes off the CS pool only (never the operand pool)
                     ((0 < nc) && !(free? <>>>e2)) (walk >rs acc)
                     (: tc (ntouch ir1x off 0)
                        (? (tc <= 1) (walk >rs acc)
                           (walk >rs (`(tc sq) >< acc)))))))
           cs (? (navl < 1) () (lastn (imsort (walk (peep g 'lrec ()) ())) navl))
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
           taken (: t (tablet 0)
                    _ (foldl (\ _ e9 (pin t <>e9 1)) 0 cs)
                    t)
           ; loop spans over ir1x, ONE per header label (the longest backward reach -- a
           ; continue's second branch must not fake depth); a touch's depth = covering spans
           spans (: labi (tablet 0)
                    (gi l i) (? !(two? l) ()
                                (: f <l
                                   _ (? (two? f && id? <f 'label) (pin labi <>f i) ())
                                   (gi >l (i + 1))))
                    _ (gi ir1x 0)
                    reach (tablet 0)
                    (go l i)
                     (? !(two? l) ()
                        (: f <l
                           tgt (? !(two? f) ()
                                  (id? <f 'jmp) <>f
                                  (id? <f 'br) <>>f
                                  ())
                           ti (? tgt (peep labi tgt -1) -1)
                           _ (? ((0 <= ti) && (ti < i))
                                (: b (peep reach ti -1) (? (b < i) (pin reach ti i) ())) ())
                           (go >l (i + 1))))
                    _ (go ir1x 0)
                    (mks ts acc) (? !(two? ts) acc
                                    (mks >ts (`(<ts (peep reach <ts 0)) >< acc)))
                    (mks (keys reach) ()))
           (nspan i) (: (go2 ss n) (? !(two? ss) n
                                      (: s2 <ss
                                         (go2 >ss (? ((<s2 <= i) && (i <= <>s2)) (n + 1) n))))
                        (go2 spans 0))
           (lntouch fs off i n)
            (? !(two? fs) n
               (: f2 <fs
                  hit (? !(two? f2) ()
                         (member? <f2 ldops9) ((<>>f2 = fb9) && (<>>>f2 = off))
                         (member? <f2 gstops) ((<>f2 = fb9) && (<>>f2 = off))
                         ())
                  (lntouch >fs off (i + 1) (? (hit && (1 < nspan i)) (n + 1) n))))
           (walk2 rs acc)                             ; cs candidates carry a WIDTH: a d128 pair takes two seats
            (? !(two? rs) acc
               (: e2 <rs sq <e2 off <>e2
                  (? (peep lt off () || peep taken sq ()) (walk2 >rs acc)
                     (: tc (ntouch ir1x off 0)
                        (? (tc <= 2) (walk2 >rs acc)
                           (: ln (lntouch ir1x off 0 0)
                              (? (ln < 1) (walk2 >rs acc)
                                 (walk2 >rs (`(ln sq (? (d128? <>>e2) 2 1)) >< acc)))))))))
           ; choose HOTTEST-first while seats remain (a pair spends two; one that can't fit
           ; skips so a scalar may still take the last seat) -- the choose-order link restores
           ; ascending heat, so seat ASSIGNMENT stays the old positional walk (register
           ; spellings unchanged for scalar-only fns)
           (pick l seats acc)
            (? (!(two? l) || (seats < 1)) acc
               (: e3 <l w (nthr e3 2)
                  (? (seats < w) (pick >l seats acc)
                     (pick >l (seats - w) (e3 >< acc)))))
           (mkk l regs t acc9)
            (? (!(two? l) || !(two? regs)) `(t acc9)
               (: e3 <l w (nthr e3 2)
                  (? ((w = 2) && two? >regs)
                      (: _ (pin t <>e3 `(<regs <>regs))
                         (mkk >l >>regs t (<>regs >< (<regs >< acc9))))
                     (w = 2) `(t acc9)
                     (: _ (pin t <>e3 <regs)
                        (mkk >l >regs t (<regs >< acc9))))))
           kpk (? !(two? kp) `((tablet 0) ())
                  (: k9 (mkk (pick (rev (imsort (walk2 (peep g 'lrec ()) ()))) (tally kp) ())
                             kp (tablet 0) ())
                     `(<k9 (rev <>k9))))              ; cu0 in assignment order (the save offsets ride it)
           (mk l hp t) (? (!(two? l) || !(two? hp)) t
                          (: _ (pin t <><l <hp)
                             (mk >l >hp t)))
           (? (two? cs || two? <>kpk)
              `((mk cs hp9 <kpk)
                (dropn hp9 (tally cs))
                <>kpk)
              ()))
       n0 (peep g 'n 0) strs0 (g 'strs)             ; snapshot the accumulators for a possible regen
       ; ⚠ the operand pool is EMPTY on t32: AAPCS32 keeps r4-r11 callee-saved and the thumb2
       ; frame saves only lr+fp, while r0-r3/r12 are args and scratch -- there is no free
       ; caller-saved register to pool. riscv's own pool is t0..t3 (r8..r11), caller-saved and
       ; never args; opool's names land on riscv arg registers, so it can't ride there.
       tpool (? (t32? g) () (rv? g) '(r8 r9 r10 r11) opool)
       _ (pin g 'pool0 tpool) _ (pin g 'pool tpool)
       ; v6-M MEMORY return: this fn's >4B composite result fills the address the caller
       ; passed as the implicit first arg (r0) -- save it in its own slot, params start at
       ; gp 1. the ret lane reads g 'sret back; fnreset holds the neutral.
       srt (? (two? sig) <sig 'long)
       sr6f (sretm? g srt)
       _ (? sr6f (pin g 'sret (ntmp g 'long)) ())
       sp1 (? (vararg && a64? g) (vaspill-a64 params)
              (vararg && t32? g) (vaspill-t32 params)
              (vararg && rv? g) (vaspill-rv params)
              vararg (vaspill params)
              sr6f (spill () params 1 0 0 () `(`('st 'r4 (peep g 'sret 0) (nthr (argr g) 0))))
              (spill () params 0 0 0 () ()))
       sp1 (? (two? vzf) `((<sp1 + vzf) <>sp1) sp1)        ; the VLA size-slot zeroes ride after the spills
       ir1 (build sp1)
       n1 (peep g 'n 0) strs1 (g 'strs)             ; ir1's accumulators -- a DEOPT restores these
       lrec1 (peep g 'lrec ())                      ; ir1's locals record: a leaf RETRY ranks on it (a regen leaves pass-2 offsets)
       ride (? (sr6f || vararg || peep g 'fesc () || hascall? ir1) () (rideset ir1 params 0 ()))
       (pools rd) (: rh (map (\ k (homereg k)) rd)   ; the freed homes rejoin the pool for locals
                     uh (filter (\ q !(member? q rh)) (uhomes params 0 ()))
                     (filter (\ q !(member? q uh)) tpool))
       hpool (pools ride)
       hm (!vararg && !sr6f && !(peep g 'hasasm ()) &&              ; sr6f: the hidden sret arg shifts every
           !(anyop? ir1 'sys) && homes? params 0)                   ; param register -- the pickers don't model it
       ; the alive pass runs once per homing-eligible fn: lpick reads cross/
       ; univ (a zero-crossing local homes through calls), the regens pin its
       ; livtab so call-fixed wraps only what a call actually needs
       ng (sr6f || vararg || bad? ir1 || peep g 'hasasm () || anyop? ir1 'sys)
       av (? ng () (alive g f))
       au (? ng () (auniv g f))                     ; the array leg's universe rides the same gate
       ; -- priced int-param homes (the leaf lane): an int/uint param earns its POSITIONAL
       ; seat only when its ir1 slot touches pay the entry cvt (tc >= 2), the locals picked
       ; first (a hot local's claim wins the register), and an x64 pool seat stays deep
       ; (>= 2 free after the take). ⚠ the unpriced version was the measured anti-lever
       ; (+3.4% cycles): cold params drank the pool and reopened mag_mul's d128 stack cell.
       ; a64 homes past r10 sit outside the operand pool -- always free at their own seat. --
       (poff9 nm env) (? !(two? env) ()
                         (: e2 <env (? (id? <e2 nm) <>e2 (poff9 nm >env))))
       (ihset rd lp9)
        (homefold g params 0 `(() lp9)
         (\ a2 i9 p9
            (: pl <>a2 hr (homereg i9) off (poff9 <p9 <>sp1)
               pooled (member? hr tpool)
               (? (!(homable? <>p9) &&
                   !(member? i9 rd) &&
                   !(two? off) &&
                   !(id? off ()) &&
                   (1 < ntouch ir1 off 0) &&
                   (? pooled (member? hr pl && deep? pl) 1))
                   `((i9 >< <a2) (? pooled (filter (\ q !(id? q hr)) pl) pl))
                  a2))))
       ; -- the SEAT TABLE (lh ih lp cs): every priced register grant in one answer -- locals
       ; homes (lpick), int-param homes (ihset), the pool left after both, the cs homes
       ; taken. both lanes mint it here; regen consumes it whole. --
       (seats rd hp9 wi)
        (: lr (? av (lpick ir1 <>av <>>av hp9) ())
           lp (? (two? lr) <>lr hp9)
           ih (? wi (ihset rd lp) `(() lp))
           `((? (two? lr) <lr ()) <ih <>ih (? (two? lr) (nthr lr 2) ())))
       st0 (seats ride hpool !(ng || peep g 'fesc () || hascall? ir1))
       lh <st0 ih0 <>st0                            ; the choice below reads these two; regen takes the table whole
       (deopt u) (: _ (pin g 'n n1) _ (pin g 'strs strs1) ir1)   ; ir1 stands, accumulators restored
       ; the REGEN dance: reset the accumulators and the frame to the ir1 snapshot, pin the
       ; analyses' channels (the vmap universe, the locals homes, the livtab), rebuild under
       ; the spill policy hp with the seat table's pool, then clear every channel. a 'bad ir2
       ; deopts: ir1 stands.
       (regen hp st)
        (: lh9 <st lp9 <>>st cs9 (nthr st 3)
           _ (pins (fnreset g) ; the attempt starts from the clean record at ir1's accumulator snapshot
              'n n0 'strs strs0
              'pool0 lp9 'pool lp9)
           _ (rgset g lh9 av au)
           ; the callee-saved homes: one slot each past the r3 cell, stored by the prologue
           ; (g 'cssv) and reloaded on every exit -- the epilogue PIN carries the loads through
           ; each ret and sib splice, and unframe re-addresses them like any slot op
           e0 (peep g 'epi epi)
           _ (? (two? cs9)
                (: sv (map (\ r9 `('st 'r4 (nslot g 8) r9)) cs9)
                   _ (pin g 'cssv sv)
                   (pin g 'epi (map (\ f9 `('ld (nthr f9 3) 'r4 (nthr f9 2))) sv + e0)))
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
           i2 (regen `('ride rd <>st) st)
           (? (ridesound? i2 rd) i2
              (: rs (filter (\ k (rideok2 i2 (nthr (argr g) k))) rd)
                 (? (tally rs < tally rd) (leaf rs) (deopt ())))))
       irf (? (bad? ir1) 'bad
          ((hm || lh || two? ride || two? ih0) && !(peep g 'fesc ()) && !(hascall? ir1))
           (leaf ride)
          ; calls remain: home + spill where reads beat the wraps -- FN-WIDE, all homable params
          ; or none. ⚠ per-param verdicts were measured BOTH ways and lost: pruning a "losing"
          ; home inside a gated fn is 7/8 slower (its wraps hide behind a call that is already
          ; there), and adding zero-cross homes outside the gate is 8/8 slower (the regen trades
          ; pool registers for homes, and returning unhomed ones collides with the ride policy).
          ; TWO pricings, either licenses: the flat count (call-light overall), or the PATH price
          ; pmax + ncw -- an invocation pays wraps only at the calls its one path passes (the hot
          ; dispatch fleet: many calls, all in exclusive cold arms), plus in-loop calls at
          ; alive's per-iteration weight, plus one mov per home that differs from its arrival.
          (: nc (ncalls ir1 0) nh (nhomable params 0 0)
             nr (nreads ir1 (pslots <>sp1 ()) 0)
             em (homefold g params 0 0
                 (\ n2 i9 p9 (? (homable? <>p9 &&
                                 !(id? (nthr (argr g) i9) (homereg i9))) (n2 + 1) n2)))
             pp (hm && (2 * nc * nh < nr ||
                        (two? av && (2 * nh * (pmax ir1 + nthr av 4) + em < nr))))
             (? (pp || lh) (regen (? pp 'shadow ()) st0) ir1)))
       ; ⚠ deadlab joins unframe in the POST-CHOICE slot and must never touch the ir1 the rankers
       ; read: the dead tail's (li r0 0) is a def of r0 that BARS the ride analysis, and sweeping
       ; it early lets a pass-2 ride clobber a live arrival. this chain's order is TYPED
       ; (stage.l): each pass advances a stage die, so only this composition checks.
       (? (bad? irf) irf (cskeep g (unhome g (jr0 (unframe g (deadlab irf)))))))

   ; the data tail: globals in declaration order, then the string table -> (dataforms nifforms
   ; imgforms extras bssforms rodataforms) | 'bad, extras being ((name forms ro?) ..), one per
   ; OTHER named section -- ro? saying the section may go read-only (xro?, below).
   ; a __attribute__((section(NAME))) global lays in a stream of its own. three streams have a
   ; meaning the writer knows: `ai_nifs` needs the linker's __start_/__stop_ bracket around a
   ; packed run for main()'s boot drain, `.image` must be LAST so `love --bake` can grow it at the
   ; tail, and `.data` takes an unattributed global. every OTHER name carries through to the .o as
   ; a real section -- a bootloader SCANS for .limine_requests, and ⚠ DECLARATION ORDER is the
   ; protocol's order, not ours, which is what xnames keeps.
   (cgdata g)
    (: sects (peep g 'sects (tablet 0))
       consts (peep g 'consts (tablet 0))
       memo (peep g 'romemo (tablet 0))
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
        (? !(peep consts nm ()) (cgimage g ty init)
           (: m (peep memo nm ())
              (? (two? m) <>m
                 (: v (cgimage g ty init) _ (pin memo nm `(1 v)) v))))
       ; zm splits the .data walk without imaging a global twice: 'd takes the
       ; initialized, 'z the rest, 'r the const ones .rodata claims, () everything
       ; (a named stream is never split).
       (globs gs want zm acc)
        (? !(two? gs) acc
           !(strm <gs = want) (globs >gs want zm acc)        ; another stream's
           (member? zm '(d r) && bss? <gs) (globs >gs want zm acc)
           ((zm = 'z) && !(bss? <gs)) (globs >gs want zm acc)
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
              an (? (member? want '("ai_nifs" ".image")) 0 (peep (g 'aligns) <gs 0))
              ; the cause rides a PIN, not a puts: cgdata's callers thread a bare 'bad through a
              ; dozen (bad? x) joins and a gripe is not one, so gfns's tail mints it from the pin
              (? ((zm = 'r) && !roq) (globs >gs want zm acc)
                 ((zm = 'd) && roq) (globs >gs want zm acc)
                 (bad? bytes)
                 (: u (peep g 'undecl ())
                    _ (pin g 'datacause
                           (? u ("undeclared '" + u + "' in the initializer of '" + <gs + "'")
                              ("cannot lay the initializer of '" + <gs + "' (" + show init + ")")))
                    'bad)
                 (: als (: (go l a2) (? !(two? l) a2
                                          (go >l (? (peep (g 'alias) <l () = <gs)
                                                    (`('label (intern <l)) >< a2) a2)))
                           (go (keys (g 'alias)) ()))   ; a DATA alias (musl's __stdin_used) names the same bytes
                    (globs >gs want zm (acc + (? (1 < an) `(`('align an)) ())
                                           + als + `(`('label lbl) `('raw bytes))))))))
       (strs ss acc)
        (? !(two? ss) acc
           (: p <ss
              (bts s i bacc) (? (i < 0) bacc (bts s (i - 1) (s i >< bacc)))
              t <>p
              (strs >ss (acc + `(`('label <p)
                                     `('raw (bts t (tally t - 1) () + '(0))))))))
       ; ..and whether one of them may ride a lane nothing writes: the .rodata rule
       ; (ro?) asked of EVERY global in it, since flags belong to the section and one
       ; writable member decides for all. .limine_requests is the live example -- a
       ; bootloader writes its responses into those blocks, and nothing there is const.
       ; the const gate comes first so a plain writable section never images anything.
       (xro? nm)
        (: (go gs) (? !(two? gs) 1
                      !(strm <gs = nm) (go >gs)
                      !(peep consts <gs ()) ()
                      (: ge (peep (g 'glob) <gs 0)
                         b (image1 <gs <>ge <>>ge)
                         (? (!(bad? b) && ro? <gs b) (go >gs) ())))
           (go (g 'globs)))
       ; the OTHER section names, first-seen in declaration order
       xnames (: (go gs acc) (? !(two? gs) (rev acc)
                                (: s (strm <gs)
                                   (go >gs (? (member? s '(".data" ".rodata" "ai_nifs" ".image") ||
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
          nb (? (bad? cg || bad? cs) 'bad (globs (rev (g 'globs)) "ai_nifs" () ()))
          ib (? (bad? nb) 'bad (globs (rev (g 'globs)) ".image" () ()))
          xb (? (bad? ib) 'bad
                (map (\ nm `(nm (globs (rev (g 'globs)) nm () ()) (xro? nm))) xnames))
          (? (foldl (\ a x (a || bad? x)) () `(gb sb zg zs rg rs cg cs nb ib xb)) 'bad
             `((gb + sb) nb ib xb (zg + zs) (rg + rs + cg + cs + strs (g 'strs) ())))))

   ; the shared core: build the gen state, then TEXT (the functions, no _start)
   ; and DATA (globals + strings) separately -> (g textforms dataforms) | ().
   ; weaks: the __attribute__((weak)) names -- NEVER inlined (an inlined body
   ; would bake past the strong override the linker owes them).
   ; xtra: the parser's (sfns svars sects .. consts) tablets -- static fns, static
   ; globals, name -> section. () reads as three empties (the law harness).
   (cgen-core ast stbl sigs weaks xtra tgt)
    (: g #('n 0 'brk () 'cnt () 'cases () 'glob #() 'globs () 'strs () 'statics ()
           'laid #()                                 ; names already on 'globs (a tentative repeat lays no twin)
           'fns #() 'inl #() 'cgitems cgitems 'stag stbl 'sigs sigs
           'tgt tgt 'epi (? (id? tgt 'thumb1) epi-t16 (member? tgt '(arm64 thumb2 thumb2sp riscv64)) epi-a64 epi)
           'sects (? (two? xtra) <>>xtra #())
           'alias (? (two? xtra && 3 < tally xtra) (nthr xtra 3) #())
           'aligns (? (two? xtra && 4 < tally xtra) (nthr xtra 4) #())
           'consts (? (two? xtra && 7 < tally xtra) (nthr xtra 7) #())
           'xfns ()                                  ; (secname forms) per fn that named a section, newest first
           'romemo #())                              ; a const candidate's image, laid once and read by both walks
       ; pass 0: every function name -- so a bare use of one is its address (la). the sig table
       ; too: an mproto sibling (`double sin(..), cos(..);`) has a sig but no proto form of its
       ; own. a small single-return definition also lands in the INLINE table, before any body --
       ; an always_inline one (ainls, the parser's roster) at any size; a noinline one NEVER
       ; (the lvm fleet and the designed-ret seams stay out-of-line by the C's own word).
       ainls (? (two? xtra && 5 < tally xtra) (nthr xtra 5) ())
       noinls (? (two? xtra && 6 < tally xtra) (nthr xtra 6) ())
       (recfns fs) (? !(two? fs) 1
                      (: f <fs
                         _ (? (member? <f '(fn proto)) (pin (g 'fns) <>f 1) ())
                         _ (? (<f = 'fn &&
                               !(member? <>f weaks) &&
                               !(member? <>f noinls) &&
                               inlok? f (peep sigs <>f ()) (member? <>f ainls))
                              (pin (g 'inl) <>f f) ())
                         (recfns >fs)))
       _ (recfns <>ast)
       _ (: (go ns) (? !(two? ns) 1 (: _ (pin (g 'fns) <ns 1) (go >ns)))
            (go (keys sigs)))
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
                           old (peep (g 'glob) <p ())
                           ; C's tentative rule (6.9.2): `int x; int x = 5;` in one TU is ONE
                           ; object -- an INITIALIZED def always wins the entry; a tentative
                           ; repeat keeps whatever stands, and lays no second symbol
                           _ (? (!(two? old) || two? <>>p)
                                (pin (g 'glob) <p `((intern <p) <>p <>>p)) ())
                           _ (? (!xd && !(peep (g 'laid) <p ()))
                                (: _ (pin (g 'laid) <p 1) (pin g 'globs (<p >< g 'globs))) ())
                           (one >ps)))
                    (one <>f)))))
       ok (rec <>ast)
       ; always emit text + data (a library TU has no main); `saw` rides out so the EXECUTABLE
       ; path can insist on one. acc rides REVERSED -- a cat per fn re-copies every prior fn's
       ; forms, quadratic over love.c's 611 -- and the tail restores emission order.
       (gfns fs acc saw)
        (? !(two? fs)
           (: dat (cgdata g)
              (? !(bad? dat) `(g (rev acc) dat saw)
                 (: c (peep g 'datacause ())            ; the static-data refusal, minted here (globs pinned it)
                    (gripe () () () (? c c "cannot lay the static data (cause unnamed -- see doc/moon-diag.md)")))))
           (: f <fs
              (? (member? <f '(proto gdecl xdecl tdef)) (gfns >fs acc saw)
                 (: _ (pin g 'undecl ()) _ (pin g 'nolane ()) _ (pin g 'arity ())   ; fresh per fn: the pins the refusal reads name THIS fn's cause
                    r (cgfn g f)
                    ; a refusal answers ONE gripe, and the driver puts it on stderr: half a
                    ; sentence on each of two channels is what a build log then keeps
                    (? (bad? r)
                       (: u (peep g 'undecl ())         ; rung 1: an undeclared id was named -> a gripe carrying it + the enclosing fn
                          nl (peep g 'nolane ())        ; rung 4: a deliberate refusal named the feature it has no lane for
                          ar (peep g 'arity ())         ; a direct call disagreeing with its own prototype
                          (? u (gripe () () () ("undeclared '" + u + "' (in " + <>f + ")"))
                             ar (gripe () () () (ar + (" (in " + <>f + ")")))
                             nl (gripe () () () (nl + (" (in " + <>f + ")")))
                             (gripe () () () ("cannot compile '" + <>f + "' (cause unnamed -- see doc/moon-diag.md)"))))
                       ; ⚠ fn-start alignment to EVEN (2): a function's address can ride into the
                       ; heap as an object's ap, and the image codec's pointer/fixnum
                       ; discriminator is PARITY -- an odd-addressed fn pointer reads back as a
                       ; fixnum and the woken heap desyncs. ⚠ WIDER alignment was measured and
                       ; REVERTED (16 ran ~4% slower, 32 ~5%, branch-misses +9%: zeroing 4-5 low
                       ; bits aliases the BTB on this frontend-bound core). don't widen it.
                       ; an ALIAS is another NAME for the same address, so its label rides
                       ; immediately before the fn's own; the align stays outermost.
                       (: als (: (go l a2) (? !(two? l) a2
                                              (go >l (? (peep (g 'alias) <l () = <>f)
                                                        (`('label (intern <l)) >< a2) a2)))
                                (go (keys (g 'alias)) ()))
                          ; an aligned(N) fn lands ON its boundary, the global lane's
                          ; rule; 2 is the floor the parity law above sets.
                          an (: a (peep (g 'aligns) <>f 2) (? (a < 2) 2 a))
                          fm (`('align an) >< (als + r))
                          ; a fn that named its own section leaves .text for that
                          ; stream (love.c's data sentinels): where the code lands
                          ; is the point, the same reason a global gets the lane.
                          ; the align rides out too -- it is the section's grain, and
                          ; a run of one-fn sections at grain N tiles at exactly N.
                          sc (peep (g 'sects) <>f ())
                          sw (saw || (<>f = "main"))
                          (? sc (: _ (pin g 'xfns (`(sc fm an) >< g 'xfns))
                                   (gfns >fs acc sw))
                             (gfns >fs (revcat fm acc) sw))))))))
       (? ok (gfns <>ast () ()) ()))
   (cgen ast stbl sigs weaks xtra tgt)              ; the STATIC EXECUTABLE: _start (crt0) + text + data -> (1 forms) | () -- needs main
    (: crt0 (? (id? tgt 'arm64)
               '((call main) (li r8 93) (sys))            ; x0 already the exit code; x8 = __NR_exit (93), svc
               (id? tgt 'riscv64)
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
             `(1 (crt0 + <>r + <>>>>>dat + <dat + <>dat + <>>>>dat)))))   ; so the nif stream just rides along
   ; the RELOCATABLE OBJECT view: text and data split, plus the DEFINED function names (global
   ; FUNC symbols, statics excluded -- they stay LOCAL, love.h's inline helpers land in every TU),
   ; the global-variable names, and the ai_nifs stream -- the .o emitter turns the rest into a
   ; symbol table + relocations. the DEFINED static functions ride the tail as lnames, laid as
   ; LOCAL FUNC symbols so nm/gdb/vmret see real function boundaries.
   (cgen-obj ast stbl sigs weaks xtra tgt)          ; -> (1 textforms dataforms funcnames globnames nifforms imgforms lnames xsecs bssforms rodataforms xfnsecs) | ()
    (: r (cgen-core ast stbl sigs weaks xtra tgt)
       (? (gripe? r) r                               ; a named cause rides out (guard before two?, a gripe is a chain)
          !(two? r) ()
          (: sfns (? (two? xtra) <xtra #())
             svars (? (two? xtra) <>xtra #())
             sects (? (two? xtra) <>>xtra #())
             (loc? nm) (!(id? (peep svars nm 'no) 'no) ||
                        peep sects nm () = "ai_nifs")     ; a nif entry is static by shape; any OTHER
                                                                 ; named section stays exported (.image is read by symbol)
             als (? (two? xtra && 3 < tally xtra) (nthr xtra 3) #())
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
                                        (go3 >l (? (member? (peep als <l ()) defd) (<l >< a2) a2)))
                          (base + go3 (keys als) ())))
             gnames (: (go gs acc) (? !(two? gs) (rev acc)
                                      (go >gs (? (loc? <gs) acc (<gs >< acc))))
                       ; an alias onto a global (or a block-scope static) is an
                       ; OBJECT symbol of its own, at the same bytes
                       dgl (revcat (<r 'globs) $ rev (<r 'statics))
                       (: base (go (rev (<r 'globs)) ())
                          (go2 l a2) (? !(two? l) a2
                                        (go2 >l (? (member? (peep als <l ()) dgl) (<l >< a2) a2)))
                          (base + go2 (keys als) ())))
             lnames (: (go fs acc) (? !(two? fs) (rev acc)
                                      (: f <fs
                                         (go >fs (? (<f = 'fn && !(id? (peep sfns <>f 'no) 'no))
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
                        (map (\ nm `(nm (forsec nm src) (grain nm src))) (noms src ()))))
             `(1 <>r <dat fnames gnames <>dat <>>dat lnames <>>>dat <>>>>dat <>>>>>dat xfns)))))
