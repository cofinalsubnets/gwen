; crew/moon/gen.l -- the code generator (rung 3 stage 3). AST -> (1 ir-forms) |
; (). holo's neutral IR, chibicc-plain: every expression computes into r0 and
; now CARRIES ITS TYPE -- cgexpr answers (type forms) so pointer arithmetic
; scales, dereferences load by pointee width, and arrays decay to addresses.
; types: 'char 'short 'int 'long | ('ptr t) | ('arr t n). the ALU stays
; 64-bit (signed overflow is UB); widths bite only at memory and casts.
; lvalues have their own door (clval: the ADDRESS in r0 + the pointee type),
; so x, *p, and a[i] assign through one path. locals off the r4 frame;
; globals and string literals are LABELED RAW DATA after the code, reached
; by holo's (la) -- the one RWX segment carries text and data both until
; the .o seam (the doc). args ride the SysV registers r6 r5 r2 r1 r7 r8,
; at most six; call args evaluate left to right. stage 4b: a bare function
; name is its address (la); a call HEAD that is a function-pointer value goes
; through holo's (callr) after parking the callee in r3; a whole-struct assign
; is a word-then-byte block copy (bcopy). stage 4c: brace and designated
; initializers -- locals zero the slot then fill (cgfill, runtime values ok,
; nesting for arrays of structs), globals bake a constant byte image
; (cgimage; string-into-char-array and [] size inference included). stage 4d:
; a GLOBAL initializer whose value is a LABEL bakes an abs64 fixup ('fix 8
; abs64 label 0), the label's absolute load address -- char *s = "hi" (a
; rodata string), a static (name, fn-ptr) table (love.c's nifs), &global,
; a decayed array. fenced and refused, not fudged: ptr+ptr, deref of a
; non-pointer, assigning to an array, struct params or returns by value.
; stage 6d: the `float` scalar (4 bytes in memory, but a value in an xmm
; register is ALWAYS a double -- a load widens, a store narrows), and the
; compound literal ('clit ty init), an unnamed object filled into a frame
; temp whose ADDRESS is the lvalue (so &(struct S){..} and (int){k} work).
(:
   ; the gen state g (a tablet): 'n the label counter, 'off the frame's
   ; high-water offset, 'brk/'cnt the loop label stacks, 'glob the global
   ; table (name -> (label ty init)), 'globs the emission order, 'strs the
   ; string table ((label . text)..). env: ((name off ty)..) frame locals.
   (nlab g pfx) (: k (peep g 'n 0) _ (pin g 'n (k + 1)) (intern (pfx + (show k))))
   argregs '(r6 r5 r2 r1 r7 r8)                      ; the SysV integer/pointer arg registers
   (xmmreg i) (intern ("f" + (show i)))              ; the SysV double arg registers f0..f7
   (nthr l i) (? (i < 1) <l (nthr >l (i - 1)))
   (nthty l i) (? (! (two? l)) () (i < 1) <l (nthty >l (i - 1)))   ; the i-th list element or ()
   (bad? x) (id? x 'bad)                             ; the gripe carrier (gripe/gripe?/okv?) lives in lex.l -- parse answers one too
   ; -- the value tuple (the operand allocator's phase 0): every expression
   ; answers (ty reg forms) -- its type, the REGISTER holding the value, and
   ; the forms that compute it. reg is r0 on every lane until the allocator
   ; lands; the flo lane keeps the f0 protocol (its reg field is not consulted)
   ; and dispatches on vty as before. the accessors keep every consumer honest
   ; about which field it reads -- a (caup r) on a value tuple is a bug now. --
   (vty v) <v
   (vreg v) <>v
   (vfs v) <>>v
   (mkv ty reg fs) `(ty reg fs)
   ; -- the operand pool (phase 1): an expression value may live in a home-pool
   ; register instead of riding the r0 protocol. g 'pool0 is the fn's whole set
   ; (opool minus any param homes -- cgfn pins it per build), g 'pool the free
   ; list, reset at every statement (values never cross one). ralloc answers a
   ; register | () (dry -> the caller keeps the shuttle); rfree is idempotent
   ; and ignores anything outside pool0 (r0, a borrowed home). a pool value
   ; NEVER lives across a call: pool registers are caller-saved and, unlike
   ; the homes, have no spill wrap -- the callish? gate holds that line. --
   opool '(r6 r5 r9 r10 r7 r8)
   (ralloc g) (: fr (peep g 'pool ())
                 (? (two? fr) (: _ (pin g 'pool >fr) <fr) ()))
   (rfree g r) (? (&& (member? r (peep g 'pool0 ()))
                      (&& (! (member? r (peep g 'pool ())))
                          (! (member? r (peep g 'vpin ())))))   ; a vmap pin is not ours to free
                  (pin g 'pool (link r (peep g 'pool ()))) ())
   (wr? g r) (|| (id? r 'r0)
                 (&& (member? r (peep g 'pool0 ()))
                     (! (member? r (peep g 'vpin ())))))   ; writable: protocol r0 or an OWNED pool reg (a borrowed home or a vmap pin is read-only)
   (callish? x) (? (! (two? x)) ()                             ; does the AST reach a call? (an inline splice still may)
                   (<x = 'call) 1
                   (|| (callish? <x) (callish? >x)))
   (tor0 g v)                                       ; land a value on the r0 protocol (frees its pool reg)
    (? (id? (vreg v) 'r0) (vfs v)
       (: rd (vreg v) rl (rev (vfs v))
          lf (? (two? rl) <rl ())
          ; a trailing leax retargets to r0 outright -- lea reads its sources
          ; before the dest write, so even an r0 index is licensed -- and the
          ; deref's load then sits same-dest beside it for the addressing fold
          (? (&& (two? lf) (&& (id? <lf 'leax) (&& (id? <>lf rd) (wr? g rd))))
             (: fs (rev (link `('leax 'r0 <>>lf <>>>lf (nthr lf 4) (nthr lf 5)) >rl))
                _ (rfree g rd)
                fs)
             (: fs ((vfs v) + `(`('mov 'r0 rd)))
                _ (rfree g rd)
                fs))))
   (vdrop g v) (: fs (vfs v) _ (rfree g (vreg v)) fs)   ; the value is DISCARDED: keep the effects, free the reg
   ; the result register for an op consuming rA: reuse a writable operand,
   ; else a fresh pool reg, else the protocol r0
   (rdst g rA) (? (wr? g rA) rA (: rX (ralloc g) (? rX rX 'r0)))
   ; -- the vmap (the operand allocator's cross-statement leg): a universe
   ; local's value, once written through or initialized in a pool register,
   ; stays mapped name -> reg, so a re-read is ZERO forms. the pinned reg
   ; rides the borrowed-home contract (wr? says no, rfree skips it, the
   ; statement reset keeps it out of the free list), and the SLOT stays the
   ; single source of truth (every write still stores), so a flush never
   ; spills -- it only forgets. soundness is basic-block locality in
   ; GENERATION order: vmflush fires at every label and call emission point
   ; (form order tracks generation order in every lane), and the universe
   ; (g 'vuniv -- the alive pass's univ, pinned only for the regen builds)
   ; already excludes address-taken and multiply-declared names. off while
   ; inlining (an inlinee's names would conflate with the caller's). --
   (vmon? g) (&& (two? (peep g 'vuniv ())) (! (peep g 'inlining ())))
   (vmget g nm)
    (? (! (vmon? g)) ()
       (: e9 (assoc nm (peep g 'vmap ()))
          (? (two? e9) <>e9 ())))
   (vmdrop g nm)
    (: m2 (filter (\ e9 (! (id? <e9 nm))) (peep g 'vmap ()))
       ; NOT `pins`: that is a prel MACRO (chained tablet pins) and a macro
       ; expands at head position over any local binding -- the collision
       ; silently answered the map itself and every register guard went blind
       (vregs9 l acc) (? (! (two? l)) acc
                         (: r9 (caup <l)
                            (? (member? r9 acc) (vregs9 >l acc)
                               (vregs9 >l (link r9 acc)))))
       _ (pin g 'vmap m2)
       (pin g 'vpin (vregs9 m2 ())))
   ; no pin is BORN while a call's arguments stage (the argseal depth, set by
   ; call-fixed): the staging splices args right-to-left (evaluation order is
   ; free in C) while generation runs left-to-right, so a pin born inside one
   ; arg would read STALE from a later-generated, earlier-spliced sibling --
   ; its establishing load splices AFTER the read. pins from BEFORE the call
   ; stay readable: their loads precede the whole staging in every splice
   ; order, and no aval can clobber them (wr?/rfree hold the vpin line). a
   ; depth count, not a flag: an argument may hold a nested call.
   (vmpin g nm r)
    (? (&& (vmon? g) (&& (! (0 < (peep g 'argseal 0)))
           (&& (member? nm (peep g 'vuniv ()))
               (member? r (peep g 'pool0 ())))))
       (: _ (vmdrop g nm)
          _ (pin g 'vmap (link `(nm r) (peep g 'vmap ())))
          (? (member? r (peep g 'vpin ())) ()
             (pin g 'vpin (link r (peep g 'vpin ()))))) ())
   (vmflush g) (: _ (pin g 'vmap ()) (pin g 'vpin ()))
   ; the per-statement pool reset: pool0 minus the vmap pins
   (psreset g)
    (: vp (peep g 'vpin ())
       (? (! (two? vp)) (pin g 'pool (peep g 'pool0 ()))
          (pin g 'pool (filter (\ r9 (! (member? r9 vp))) (peep g 'pool0 ())))))
   ; -- the TARGET seam: g 'tgt ('x64 | 'arm64 | 'thumb2) picks the ABI shapes;
   ; the IR stays neutral. arm64 (AAPCS64): args ride r0..r7 (8 gp), the frame
   ; base retargets to fp (x29) at the end of build (a4ize -- r4 is x4, a CALLER-
   ; saved arg register there), bl clobbers lr so the prologue saves it, and
   ; x3 is caller-saved -- the rbx slot machinery falls away. thumb2 (AAPCS32,
   ; Cortex-M7): a 32-bit WORD (wsize=4), args in r0..r3 (4 gp), the same lr-push
   ; + fp-base + `bx lr` frame shape as arm64 (`arm?` covers both) -- r4 is
   ; callee-saved there so no retarget is forced, but riding fp keeps the arm64
   ; build/epilogue path verbatim. register-homing is OFF for now (nhome=0): every
   ; param spills to its frame slot, the simplest correct lane. --
   (a64? g) (id? (peep g 'tgt ()) 'arm64)
   (t32? g) (member? (peep g 'tgt ()) '(thumb2 thumb2sp thumb1))   ; the 32-bit thumb ABI family (AAPCS32): ARMv7E-M + ARMv6-M share it
   (sp64? g) (id? (peep g 'tgt ()) 'thumb2sp)       ; ARMv7E-M with an SP-ONLY FPU (the playdate's STM32F746): f64 arithmetic softens
   (v6m? g) (id? (peep g 'tgt ()) 'thumb1)          ; ARMv6-M refinements ONLY: narrower immediates, no imm-mul, soft-divide, pop-pc epilogue
   (arm? g) (|| (a64? g) (t32? g))                  ; the lr-push / fp-base / bx-lr frame family
   (wsize g) (? (t32? g) 4 8)                       ; the machine WORD in bytes (a pointer, a gp scalar, a stack cell)
   argregs-a64 '(r0 r1 r2 r3 r4 r5 r6 r7)           ; AAPCS64: x0..x7
   argregs-t32 '(r0 r1 r2 r3)                        ; AAPCS32: r0..r3
   (argr g) (? (a64? g) argregs-a64 (t32? g) argregs-t32 argregs)
   (ngp g) (? (a64? g) 8 (t32? g) 4 6)
   (pkr g) (? (a64? g) 'r15 (t32? g) 'r12 'r3)      ; the callr park (x64 rbx; arm64 x15; thumb2 r12/IP -- x3/r3 is the 4th ARG register)
   (ip2 g) (? (a64? g) 'r1 'r2)                     ; an int-pair aggregate's SECOND register (rax:rdx / x0:x1); thumb2 unused for M1 scalar
   (psc g) (? (a64? g) 'r9 'r0)                     ; the xmm-arg pop scratch (arm64 r0 IS arg 0; a pool reg is dead at a call)
   (ovb g) (? (a64? g) 32 (v6m? g) 12 (t32? g) 8 16)  ; overflow params above the push cells: x64 [ret+rbp]=16; arm64 2x16; thumb2 lr+fp=8; v6m lr+fp+r4=12
   ; the arm64 HOME pool: caller-saved, never an argument register (x0..x7 all
   ; stage args at calls, so no positional alignment with the incoming regs --
   ; every home takes one prologue mov), and clear of the staging scratches
   ; (x9 the xmm pop scratch, x15 the callr park, x16 holo's far-offset lane).
   hregs-a64 '(r10 r11 r12 r13 r14)
   hregs-t32 '(r5 r6 r7 r8)                          ; thumb2 callee-saved home pool (inert while nhome=0)
   (hregs g) (? (a64? g) hregs-a64 (t32? g) hregs-t32 opool)   ; the home pool (x64 rides opool, positionally arg-aligned)
   (nhome g) (? (a64? g) 5 (t32? g) 0 6)            ; how many gp positions can home (thumb2: none yet -- spill all params)
   (fbase g) (? (arm? g) 'fp 'r4)                   ; the frame base as post-build ir1 spells it (a4ize); arm64+thumb2 fp, x64 r4
   ; the immediate gate: x64 ALU/cmp immediates ride le32; arm64 add/sub/cmp
   ; take imm12 (a negative flips, a 24-bit value splits in the backend), the
   ; logicals only a bottom-aligned bitmask (2^n - 1), multiply none at all.
   ; thumb2: ADDW/SUBW take a plain imm12 (0..4095, negative flips); cmp an imm8
   ; (0..255, CMN on negatives); the logicals/mul have no simple immediate form.
   (immok g cls n)
    (? (a64? g)
       (? (cls = 'addsub) (&& (-16777215 <= n) (n <= 16777215))   ; imm12, or the backend's hi12+lo12 split
          (cls = 'cmp) (&& (-4095 <= n) (n <= 4095))              ; imm12 only (no split -- flags)
          (cls = 'log) (&& (0 < n) (0 = (& n (n + 1))))
          ())
       (v6m? g)
       (? (cls = 'addsub) (&& (-255 <= n) (n <= 255))             ; v6-M ADDS/SUBS imm8 (imm3 3-addr / imm8 2-addr; negative flips)
          (cls = 'cmp) (&& (0 <= n) (n <= 255))                   ; CMP imm8 only -- no CMN-imm on v6-M, so materialize negatives
          ())                                                     ; log/mul: materialize into a register
       (t32? g)
       (? (cls = 'addsub) (&& (-4095 <= n) (n <= 4095))           ; ADDW/SUBW imm12
          (cls = 'cmp) (&& (-255 <= n) (n <= 255))                ; CMP/CMN imm8
          ())                                                     ; log/mul: materialize into a register
       (fits32? n))
   ; -- the type floor (struct sizes come off the stag table riding in g) --
   (ptr? ty) (&& (two? ty) (<ty = 'ptr))
   (arr? ty) (&& (two? ty) (<ty = 'arr))
   (stru? ty) (&& (two? ty) (<ty = 'struct))
   (fn? ty) (&& (two? ty) (<ty = 'fn))         ; ('fn ret?) -- a function; decays to its address
   (bitf? ty) (&& (two? ty) (<ty = 'bitfield))  ; a bitfield member: (bitfield uty bitoff width); its lvalue is a UNIT address
   (uldop sz) (? (sz = 1) 'ldu1 (sz = 2) 'ldu2 (sz = 4) 'ldu4 'ld)   ; unsigned load of a sz-byte word
   (fnty g nm) (: sig (peep (g 'sigs) nm ())        ; a named function's type, return riding when sig'd
                  (? (two? sig) `('fn <sig) `('fn)))
   ; by-value aggregates (structs AND unions): a struct VALUE in cgexpr is its
   ; ADDRESS in r0; params/args/returns/results read the eightbytes through it.
   ; carried classes: one eightbyte ('int in a gp reg / 'sse in an xmm) and the
   ; two uniform 16-byte pairs -- '(sse sse) the xmm pair (love.c's struct ai_zn,
   ; full lanes) and '(int int) rax:rdx (return/result lanes; int-pair args and
   ; params stay refused). MIXED 16-byte pairs and >16 bytes (MEMORY) refuse.
   ; the SysV eightbyte CLASSES of a small aggregate -- '(int) / '(sse) at <=8
   ; bytes, a two-symbol list at 9..16 -- or () (a scalar, an unsized tag, or
   ; >16 bytes: MEMORY class, still refused). an eightbyte is SSE only when
   ; every member overlapping it is float/double; any integer/pointer member
   ; wins it for INT. unions flatten to offset-0 fields (parse.l), so the same
   ; overlap walk classifies them -- a union of long and double is INT, like
   ; the ABI says. eightbyte moves ride full 8-byte loads/stores (a 1..7-byte
   ; tail reads its padding; frame temps and section interiors both hold it).
   (aclass g ty)
    (? (! (stru? ty)) ()
       (: se (peep (g 'stag) <>ty 0)
          (? (! (two? se)) ()
             (16 < <>se) ()
             (: cls (tablet 0)
                (mark o c) (: i (>> o 3) old (peep cls i ())
                              (pin cls i (? (|| (id? old 'int) (id? c 'int)) 'int 'sse)))
                (walk o mty)
                 (? (flo? mty) (mark o 'sse)
                    (arr? mty)
                     (: esz (tsz g <>mty)
                        (go i) (? (<>>mty <= i) 1
                                  (: _ (walk (o + (* i esz)) <>mty) (go (i + 1))))
                        (go 0))
                    (stru? mty)
                     (: e2 (peep (g 'stag) <>mty 0)
                        (? (! (two? e2)) 1 (fields o <e2)))
                    (mark o 'int))
                (fields o fs)
                 (? (! (two? fs)) 1
                    (: f <fs _ (walk (o + <>f) <>>f) (fields o >fs)))
                _ (fields 0 <se)
                (? (<>se <= 8) `((peep cls 0 'int))
                   `((peep cls 0 'int) (peep cls 1 'int)))))))
   (acls2 g ty ca cb) (: c (aclass g ty)
                         (&& (two? c) (&& (two? >c)
                             (&& (id? <c ca) (id? <>c cb)))))
   (sse2? g ty) (acls2 g ty 'sse 'sse)       ; both eightbytes SSE: the xmm0:xmm1 pair
   (ipair? g ty) (acls2 g ty 'int 'int)      ; both INT: rax:rdx (return/result lanes)
   (aone g ty) (: c (aclass g ty) (? (&& (two? c) (! (two? >c))) <c ()))
   (aeb2 g ty) (: c (aclass g ty) (? (&& (two? c) (two? >c)) c ()))   ; a 9..16B aggregate's TWO eightbyte classes (c0 c1), else ()
   (fnret ty) (? (! (&& (two? ty) (<ty = 'ptr))) 'long   ; an indirect callee's result type: ('ptr ('fn ret)) | 'long
                 (: ft <>ty (? (&& (fn? ft) (two? >ft)) <>ft 'long)))
   ; unsigned scalars ride 'uchar 'ushort 'uint 'ulong -- same WIDTH as their signed
   ; twin, but a load zero-extends, `>>` is logical, compare/divide go unsigned, and
   ; a binary op is unsigned if EITHER operand is (simplified usual conversions).
   (usg? ty) (member? ty '(uchar ushort uint ulong ullong))
   ; llong/ullong exist only on t32 (canon folds `long long` to 'long on the 64-bit targets):
   ; the 8-byte REGISTER PAIR, lo in r0, hi in r1 -- the one type wider than the accumulator.
   (llt? ty) (|| (ty = 'llong) (ty = 'ullong))
   (ubin ta tb) (? (|| (llt? ta) (llt? tb)) (? (|| (usg? ta) (usg? tb)) 'ullong 'llong)
                   (|| (usg? ta) (usg? tb)) 'ulong 'long)
   (scc op unsg) (? (! unsg) op (op = 'lt) 'below (op = 'gt) 'above (op = 'le) 'be (op = 'ge) 'ae op)
   (rflip op) (? (op = 'lt) 'gt (op = 'gt) 'lt (op = 'le) 'ge (op = 'ge) 'le op)      ; MIRRORED (n < x reads x > n): constant-on-the-left
   (negrel op) (? (op = 'eq) 'ne (op = 'ne) 'eq (op = 'lt) 'ge (op = 'ge) 'lt (op = 'le) 'gt 'le)   ; NEGATED: branch-if-false
   (fits32? v) (&& (-2147483648 <= v) (v <= 2147483647))    ; holo's ALU/cmp/imul immediates ride le32
   (tsz g ty) (? (ptr? ty) (wsize g)
                 (arr? ty) (* <>>ty (tsz g <>ty))
                 (stru? ty) (caup (peep (g 'stag) <>ty 0))
                 (|| (ty = 'char) (ty = 'uchar)) 1 (|| (ty = 'short) (ty = 'ushort)) 2
                 (|| (ty = 'int) (|| (ty = 'uint) (ty = 'float))) 4
                 (|| (llt? ty) (ty = 'double)) 8              ; 8-byte on EVERY target (t32's word is 4)
                 (wsize g))
   ; a member's (offset ty) off a struct type, | 'bad
   (fmem g ty name)
    (? (! (stru? ty)) 'bad
       (: e (peep (g 'stag) <>ty 0)
          (? (! (two? e)) 'bad
             (: f (assoc name <e)
                (? (two? f) `(<>f <>>f) 'bad)))))
   (lg2 n) (: (go k v) (? (v <= 1) k (go (k + 1) (>> v 1))) (go 0 n))   ; a true log2 -- a pointer diff over a >8-byte struct needs sar 4+, not a capped 3
   ; pointer-scale by an immediate: x64 imul takes it straight; arm64 mul has
   ; no immediate form, so the size rides r3 first (free mid-expression: the
   ; shuttle holds r0/r1, and args only load at the pops)
   (imby g rX sz) (? (arm? g) `(`('li 'r3 sz) `('imul rX rX 'r3)) `(`('imul rX rX sz)))   ; arm mul (a64/t32/v6-M) has no imm form
   (homable? pty) (|| (ptr? pty) (member? pty '(long ulong)))   ; an 8-byte gp scalar: what a register home can carry
   ; -- the immediate lane: the left value already in r0, a compile-time constant
   ; on the right -> ONE imm-form op (holo's add/sub/imul/and/or/xor/cmp take a
   ; signed-32 immediate), no operand-stack shuttle -- 6 insns and a memory
   ; round-trip fold to 1. a pointer +/- scales the constant at compile time; an
   ; UNSIGNED / or % by a power of two strength-reduces to shr/and (the b2w
   ; shape; signed stays a real div -- C rounds toward zero, sar toward -inf).
   ; answers (ty forms) | 'no (the caller falls back to the shuttle). --
   (pow2? n) (&& (0 < n) (0 = (& n (n - 1))))
   (immop g op av aty n wnt)                        ; av the LEFT value tuple; wnt the advisory destination
    (: rA (vreg av)
       (? (llt? aty) 'no                            ; a pair never rides the immediate lane
          (member? op '(eq ne lt le gt ge))
           (? (immok g 'cmp n)
              (: rd (? wnt wnt (rdst g rA))
                 fs ((vfs av) + `(`('cmp rA n) `('set (scc op (|| (usg? aty) (ptr? aty))) rd)))
                 _ (? (id? rd rA) () (rfree g rA))
                 (mkv 'long rd fs))
              'no)
          (member? op '(/ %))
           (? (! (&& (usg? aty) (pow2? n))) 'no
              (op = '/) (? (n = 1) (mkv 'ulong rA (vfs av))
                           (? (wr? g rA)
                              (mkv 'ulong rA ((vfs av) + `(`('shr rA (lg2 n)))))
                              (: rd (? wnt wnt (rdst g rA))   ; a borrowed home shifts a COPY (no 3-address shr)
                                 (mkv 'ulong rd ((vfs av) + `(`('mov rd rA) `('shr rd (lg2 n))))))))
              (n = 1) (mkv 'ulong 'r0 ((vdrop g av) + `(`('li 'r0 0))))
              (! (immok g 'log (n - 1))) 'no             ; t32 has no simple AND-imm: materialize (the register path)
              (: rd (? wnt wnt (rdst g rA))
                 fs ((vfs av) + `(`('and rd rA (n - 1))))
                 _ (? (id? rd rA) () (rfree g rA))
                 (mkv 'ulong rd fs)))
          (: sn (? (&& (ptr? aty) (member? op '(+ -))) (* n (tsz g <>aty)) n)
             iop (? (op = '+) 'add (op = '-) 'sub (op = '*) 'imul
                    (op = '&) 'and (op = '|) 'or (op = '^) 'xor 'no)
             icl (? (member? op '(+ -)) 'addsub (op = '*) 'mul 'log)
             (? (id? iop 'no) 'no
                (! (immok g icl sn)) 'no
                (: rt (? (ptr? aty) aty (ubin aty 'long))
                   (? (&& (sn = 0) (member? op '(+ -))) (mkv rt rA (vfs av))
                      (: rd (? wnt wnt (rdst g rA))
                         fs ((vfs av) + `(`(iop rd rA sn)))
                         _ (? (id? rd rA) () (rfree g rA))
                         (mkv rt rd fs))))))))
   (ldop ty) (? (ty = 'char) 'ld1 (ty = 'short) 'ld2 (ty = 'int) 'ld4
                (ty = 'uchar) 'ldu1 (ty = 'ushort) 'ldu2 (ty = 'uint) 'ldu4 'ld)
   (stop ty) (? (|| (ty = 'char) (ty = 'uchar)) 'st1 (|| (ty = 'short) (ty = 'ushort)) 'st2
                (|| (ty = 'int) (ty = 'uint)) 'st4 'st)
   (cvt ty r) (? (ty = 'char) `(`('sx1 r)) (ty = 'short) `(`('sx2 r)) (ty = 'int) `(`('sx4 r))
                 (ty = 'uchar) `(`('zx1 r)) (ty = 'ushort) `(`('zx2 r)) (ty = 'uint) `(`('zx4 r)) ())
   ; -- the double lane (stage 6a, the gem): a 'double value lives in f0, not r0.
   ; its type is the flag; fbits is the compile-time IEEE-754 encoder; asflo/asint
   ; move a cgexpr result between the lanes (int<->double conversion); compares
   ; ride ucomisd's UNSIGNED flags, NaN-honest (the binop lane). --
   ; a float and a double share the register lane: a value in an xmm register is
   ; ALWAYS a 64-bit double; only MEMORY differs -- a `float` slot is 4 bytes, so
   ; a load widens (ldss + cvtss2sd) and a store narrows (cvtsd2ss + stss). fldf/
   ; fstf pick the lane by the DECLARED type; every loaded float types 'double.
   (flo? ty) (|| (ty = 'double) (ty = 'float))
   (fenc ax bias mant)                              ; positive finite -> biased-exponent * mant + mantissa fraction
    (: eu ((: (go e) (? (((e + 1) 2.0) <= ax) (go (e + 1)) e)) (int (/ (log ax) (log 2.0))))
       e  ((: (go e) (? (ax < (e 2.0)) (go (e - 1)) e)) eu)
       (+ (* (e + bias) mant) (int (* ((/ ax (e 2.0)) - 1.0) (0.0 + mant)))))
   (fbits x)                                        ; finite double -> its IEEE-754 bits as the SIGNED i64 pattern
    (? (x = 0.0) 0                                  ; (the sign bit would make a BIG, and les/imnum walk i64 patterns)
       (: m (fenc (? (x < 0.0) (0.0 - x) x) 1023 4503599627370496)
          (? (x < 0.0) (m - 9223372036854775808) m)))
   (fbits32 x)                                      ; finite double -> its IEEE-754 SINGLE (u32) bits (a `float` image)
    (? (x = 0.0) 0
       (+ (* (? (x < 0.0) 1 0) 2147483648)
          (fenc (? (x < 0.0) (0.0 - x) x) 127 8388608)))
   (fldf ty freg base off)                          ; load a float/double from [base+off] into freg (widen if float)
    (? (ty = 'float) `(`('ldss freg base off) `('cvtss2sd freg freg))
       `(`('ldsd freg base off)))
   ; the narrow scratch: x64 f2 (xmm2, caller-saved, never live here); arm64
   ; f16 -- v2 IS the third fp ARGUMENT (a param spill narrowing through it
   ; clobbered the still-unspilled param c), and v8..v15 carry a callee-saved
   ; low half; v16+ are the pure temps. t32 (fpv5-d16) has no register past
   ; d15, so d15/f15 is RESERVED as the scratch (the backend's converters
   ; already own its S30 half) -- moon never allocates it.
   (fsc g) (? (a64? g) 'f16 (t32? g) 'f15 'f2)
   (fstf g ty base off val)                         ; store val (a double in an xmm) to [base+off] as ty (narrow if float)
    (? (ty = 'float) (: f9 (fsc g) `(`('cvtsd2ss f9 val) `('stss base off f9)))   ; scratch narrows (val untouched)
       `(`('stsd base off val)))
   ; result -> f0 as a double. t32: an unsigned word rides the U-converter (no 64-bit widen
   ; dodge on a 32-bit target); a pair source is the deferred cvtllsd (backend scares LOUD).
   (asflo g r) (? (flo? (vty r)) (vfs r)
                  (&& (t32? g) (llt? (vty r))) ((tor0 g r) + (cvtlld g (usg? (vty r))))
                  (&& (t32? g) (usg? (vty r))) ((tor0 g r) + `(`('cvtui2sd 'f0 'r0)))
                  ((tor0 g r) + `(`('cvtsi2sd 'f0 'r0))))
   (asint g r) (? (flo? (vty r)) ((vfs r) + `(`('cvttsd2si 'r0 'f0))) (tor0 g r))  ; result -> r0 as an int
   ; -- the environments: locals shadow globals --
   (vent g env name)                                ; -> ('regv rN ty) | ('loc off ty) | ('glo lbl ty) | 'bad
    (: e (assoc name env)
       (? (two? e)
           (: h <>e                             ; a homed param's "off" slot is a (reg rN) list;
              (? (&& (two? h) (<h = 'reg)) `('regv <>h <>>e)   ; a block-scope static's is (glob lbl)
                 (&& (two? h) (<h = 'glob)) `('glo <>h <>>e)
                 `('loc h <>>e)))
          (: ge (peep (g 'glob) name 0)
             (? (two? ge) `('glo <ge <>ge) 'bad))))
   ; a targetable LEAF re-emitted straight into register rX -> forms | 'no.
   ; leaves are PURE (no strs pin, no argpos), so discarding the r0 emission
   ; and re-aiming costs nothing; the narrow loads extend exactly like the
   ; r0 lanes. a homed param never lands here (its tuple already rides its
   ; home register, zero forms).
   (cgleaf g env e rX)
    (? (<e = 'num) `(`('li rX <>e))
       (! (<e = 'var)) 'no
       (: en (vent g env <>e)
          (? (bad? en) 'no
             (! (<en = 'loc)) 'no
             (: ty <>>en
                (? (|| (stru? ty) (|| (arr? ty) (flo? ty))) 'no
                   `(`((ldop ty) rX 'r4 <>en)))))))
   ; PARK the left operand out of r0 so the right may compute: a leaf re-aims
   ; at a fresh pool register (no protocol mov at all), anything else takes
   ; one mov. answers the (possibly re-homed) tuple | 'sh (pool dry -> the
   ; caller keeps the memory shuttle).
   (vpark g env e v)
    (? (! (id? (vreg v) 'r0)) v
       (: rX (ralloc g)
          (? (! rX) 'sh
             (: lf (cgleaf g env e rX)
                (? (! (id? lf 'no)) (mkv (vty v) rX lf)
                   (mkv (vty v) rX ((vfs v) + `(`('mov rX 'r0)))))))))
   ; a truth value in r0 that `cmp r0 0` can test: a double folds to d != 0.0
   (ctest g env e)
    (: r (cgexpr g env e)
       (? (bad? r) 'bad
          (flo? (vty r))                             ; d != 0.0, NaN-honest: unordered is NONZERO (true), so `or` in the parity bit
           (mkv 'long 'r0 (cat (vfs r) `(`('li 'r1 0) `('movqxr 'f1 'r1) `('ucomisd 'f0 'f1)
                                          `('set 'ne 'r0) `('set 'p 'r1) `('or 'r0 'r0 'r1))))
          r))
   ; -- the predicate splice: a call to an inline-table fn whose whole body is
   ; one `return E` with E already a BIT by shape (!/&&/||/relational -- a 0/1,
   ; so the return-type narrowing keeps its truth) branches THROUGH the body:
   ; args land in fresh temps (the out-of-line call's exact semantics), then E
   ; cbranches straight to lbl -- no join label, no 0/1 settle, no retest. the
   ; love.c hot conditions are nearly all inline predicates, and the settle+retest
   ; was the clang differential's largest bucket. depth 1 and the fesc/strs
   ; decline protocol exactly as cginl. -> forms | 'no (the fallback stands) --
   (retex f)                                     ; init-free scalar decls* + one `return E` -> (((name ty)..) E) | ()
    (: (dok ps acc9)
        (? (! (two? ps)) acc9
           (: p9 <ps
              (? (|| (two? <>>p9) (|| (arr? <>p9) (stru? <>p9))) 'bad
                 (dok >ps (link `(<p9 <>p9) acc9)))))
       (go items dacc)
        (? (! (two? items)) ()
           (: it <items
              (? (<it = 'decl)
                  (: ds (dok <>it dacc) (? (bad? ds) () (go >items ds)))
                 (! (<it = 'ret)) ()
                 (! (two? <>it)) ()
                 (two? >items) ()
                 `(dacc <>it))))
       (go (caup (caup (cuup f))) ()))
   (abit? e9) (? (! (two? e9)) ()
                 (&& (<e9 = 'un) (<>e9 = 'not)) 1
                 (member? <e9 '(land lor)) 1
                 (&& (<e9 = 'bin) (member? <>e9 '(eq ne lt le gt ge))))
   (cbinl g env e cc lbl)
    (: head <>e args <>>e
       il0 (peep g 'inlining ())                  ; cbinl NESTS to depth 3 (it emits no
       d0 (? (! il0) 0 il0)                       ; labels, so cginl's collision reason
       inlf (? (2 < d0) ()                        ; is moot -- only recursion needs a cap)
               (! (<head = 'var)) ()
               (! (bad? (vent g env <>head))) ()  ; a local/global shadows the name
               (: c9 (peep (g 'inl) <>head ())
                  (? (! (two? c9)) ()
                     ((tally args) = (tally <>>c9)) c9
                     ())))
       rx (? (two? inlf) (retex inlf) ())
       e9 (? (two? rx) <>rx ())
       (? (! (abit? e9)) 'no
          (: fe0 (peep g 'fesc ())
             strs0 (g 'strs)
             _ (pin g 'inlining (d0 + 1))
             (cbind ps as benv acc)                ; cginl's bind, the same slots
              (? (! (two? ps)) `(benv acc)
                 (: r (cgexpr g env <as)
                    (? (bad? r) 'bad
                       (pair? g (caup (cap ps)))   ; a pair param: an 8-byte slot, both words
                        (: off (ntmp g 'llong)
                           (cbind >ps >as
                                  (link `((cap (cap ps)) off (caup (cap ps))) benv)
                                  (acc + (asll g r) + (pst 'r4 off))))
                       (: off (ntmp g 'long)
                          (cbind >ps >as
                                 (link `((cap (cap ps)) off (caup (cap ps))) benv)
                                 (acc + (asint g r) + `(`('st 'r4 off 'r0))))))))
             (dtmp ds benv9)                       ; the body's own decls: fresh temps over the params
              (? (! (two? ds)) benv9
                 (: d9 <ds (dtmp >ds (link `(<d9 (ntmp g <>d9) <>d9) benv9))))
             br (cbind <>>inlf args () ())
             bf (? (bad? br) 'bad (cbranch g (dtmp <rx <br) e9 cc lbl))
             _ (pin g 'inlining il0)
             (? (bad? bf)
                 (: _ (pin g 'fesc fe0) _ (pin g 'strs strs0) 'no)
                (&& (! fe0) (peep g 'fesc ()))     ; the body took a frame address: keep the caller lean
                 (: _ (pin g 'fesc ()) _ (pin g 'strs strs0) 'no)
                (<>br + bf)))))
   ; -- (v & k) as a bare condition: TEST is the flags-only AND, so the value's
   ; register SURVIVES the check -- no and-clobber, a mapped local stays mapped,
   ; and the park/reload the destructive form once forced never exists. eq/ne
   ; read ZF only, so truth through the mask is exact at every width. both
   ; backends carry test-imm (x64 F7 /0 id, arm64 ands xzr #bitmask); the 'log
   ; gate is exactly test's immediate reach on each. --
   (tbr g env v k cc lbl)
    (: a (cgexpr g env v)
       (? (|| (bad? a) (flo? (vty a))) 'bad
          (: fs ((vfs a) + `(`('test (vreg a) k) `('br cc lbl)))
             _ (rfree g (vreg a))
             fs)))
   ; -- branch on a condition WITHOUT materializing its truth value: forms that
   ; jump to lbl when the condition is FALSE (cc 'eq) or TRUE (cc 'ne), the same
   ; sense as the fallback `(cmp r0 0) (br cc lbl)` over a ctest value. a
   ; comparison fuses to cmp + br<relation> (the cmp/set/movzx/cmp/br chain
   ; folds to cmp/br on EVERY if/while/for), !e flips the sense, && and ||
   ; short-circuit straight into the branch structure, a mask test (e & k)
   ; branches off TEST (the tbr lane), and a predicate CALL splices through
   ; cbinl. a float compare branches straight off ucomisd/fcmp, NaN-honest
   ; (the fcb lane). forms | 'bad. --
   (cbranch g env e cc lbl)
    (? (&& (<e = 'un) (<>e = 'not))
        (cbranch g env <>>e (? (cc = 'eq) 'ne 'eq) lbl)
       (<e = 'land)
        (? (cc = 'eq)
           (: f1 (cbranch g env <>e 'eq lbl)
              f2 (cbranch g env <>>e 'eq lbl)
              (? (|| (bad? f1) (bad? f2)) 'bad (f1 + f2)))
           (: lf (nlab g ".f")
              f1 (cbranch g env <>e 'eq lf)
              f2 (cbranch g env <>>e 'ne lbl)
              _ (vmflush g)                            ; label lf joins
              (? (|| (bad? f1) (bad? f2)) 'bad (f1 + f2 + `(`('label lf))))))
       (<e = 'lor)
        (? (cc = 'ne)
           (: f1 (cbranch g env <>e 'ne lbl)
              f2 (cbranch g env <>>e 'ne lbl)
              (? (|| (bad? f1) (bad? f2)) 'bad (f1 + f2)))
           (: lt (nlab g ".t")
              f1 (cbranch g env <>e 'ne lt)
              f2 (cbranch g env <>>e 'eq lbl)
              _ (vmflush g)                            ; label lt joins
              (? (|| (bad? f1) (bad? f2)) 'bad (f1 + f2 + `(`('label lt))))))
       (&& (<e = 'bin) (member? <>e '(eq ne lt le gt ge)))
        (: op <>e
           lhs <>>e rhs <>>>e
           ; LAZY left-then-right: the left PARKS (allocates) before the right
           ; evaluates -- allocation order must match execution order, or the
           ; right's freed inner temps alias the parked left. a flo side falls
           ; back to ctest (both sides re-evaluate whole, the old shape).
           ; the float lane FUSES: discard the int-flavored emission (the map
           ; flushes first -- a pin born in the dropped forms would survive
           ; with its establishing load gone, the lvm_aall stale-index fault),
           ; re-evaluate both sides whole through the double shuttle, then
           ; branch straight off the compare -- no set dance, no retest.
           ; UNORDERED (a NaN side) answers false for every relation and true
           ; only for !=. x64: above/ae read CF=0, which unordered sets, so
           ; < and <= compare SWAPPED; == and != carry the parity bit in the
           ; BRANCH STRUCTURE (p||ne is two branches in, np&&eq hops the eq
           ; branch over). arm64: fcmp's gt/ge/eq/ne are unordered-false
           ; natively, so every relation is ONE branch and negrel spells the
           ; false sense.
           (fcb z9)
            (: _ (vmflush g)
               a (cgexpr g env lhs)
               (? (bad? a) 'bad
                  (: af (? (t32? g)                      ; the gp shuttle moves the PAIR on t32
                           (cat (asflo g a) (cat `(`('movqrx 'r0 'f0)) spush2))
                           (cat (asflo g a) (cat `(`('movqrx 'r0 'f0)) (spush 'r0))))
                     b (cgexpr g env rhs)
                     (? (bad? b) 'bad
                        (: mid (? (t32? g)
                                  (cat af (cat (asflo g b) ((spop2 'r2 'r3) + `(`('movqxr 'f1 'r2)))))
                                  (cat af (cat (asflo g b) (cat (spop 'r1) `(`('movqxr 'f1 'r1))))))
                           swp (member? op '(lt le))
                           cf `(`('ucomisd (? swp 'f0 'f1) (? swp 'f1 'f0)))
                           ecc (? (arm? g)               ; VCMP/FCMP flags: gt/ge unordered-false native
                                  (: t9 (? (member? op '(lt gt)) 'gt (member? op '(le ge)) 'ge op)
                                     (? (cc = 'ne) t9 (negrel t9)))
                                  (member? op '(lt gt)) (? (cc = 'ne) 'above 'be)
                                  (member? op '(le ge)) (? (cc = 'ne) 'ae 'below)
                                  ())
                           (? ecc (cat mid (cat cf `(`('br ecc lbl))))
                              (? (cc = 'ne) (op = 'ne) (op = 'eq))
                               (cat mid (cat cf `(`('br 'p lbl) `('br 'ne lbl))))
                              (: sk (nlab g ".u")
                                 _ (vmflush g)
                                 (cat mid (cat cf `(`('br 'p sk) `('br 'eq lbl) `('label sk)))))))))))
           (bcc o u) (scc (? (cc = 'ne) o (negrel o)) u)
           (cu2 ta tb) (|| (|| (usg? ta) (usg? tb)) (|| (ptr? ta) (ptr? tb)))
           ; a pair side (t32 llong) branches off subs/sbcs via pbrf; a constant other side
           ; materializes as a sign-widened pair in r2:r3, a 32-bit other side widens the same way.
           (pnum v) `(`('li 'r2 (& v 4294967295)) `('li 'r3 (? (v < 0) 4294967295 0)))
           (? (&& (<rhs = 'num) (immok g 'cmp <>rhs))
               (: a (cgexpr g env lhs)
                  (? (bad? a) 'bad
                     (flo? (vty a)) (: _ (rfree g (vreg a)) (fcb ()))
                     (pair? g (vty a))
                      ((vfs a) + (pnum <>rhs) + (pbrf g op (usg? (vty a)) 'r0 'r1 'r2 'r3 cc lbl))
                     (: fs ((vfs a) + `(`('cmp (vreg a) <>rhs) `('br (bcc op (cu2 (vty a) 'long)) lbl)))
                        _ (rfree g (vreg a))
                        fs)))
              (&& (<lhs = 'num) (immok g 'cmp <>lhs))
               (: b (cgexpr g env rhs)
                  (? (bad? b) 'bad
                     (flo? (vty b)) (: _ (rfree g (vreg b)) (fcb ()))
                     (pair? g (vty b))
                      ((vfs b) + (pnum <>lhs) + (pbrf g (rflip op) (usg? (vty b)) 'r0 'r1 'r2 'r3 cc lbl))
                     (: fs ((vfs b) + `(`('cmp (vreg b) <>lhs) `('br (bcc (rflip op) (cu2 'long (vty b))) lbl)))
                        _ (rfree g (vreg b))
                        fs)))
              (: a (cgexpr g env lhs)
                 (? (bad? a) 'bad
                    (flo? (vty a)) (: _ (rfree g (vreg a)) (fcb ()))
                    (: ah (? (|| (callish? rhs) (llt? (vty a))) 'sh (vpark g env lhs a))   ; a call on the right / a pair left: memory only
                       (? (id? ah 'sh)
                           (: pA (pair? g (vty a))
                              af (? pA ((asll g a) + spush2) ((tor0 g a) + (spush 'r0)))
                              b (cgexpr g env rhs)
                              (? (bad? b) 'bad
                                 (flo? (vty b)) (: _ (rfree g (vreg b)) (fcb ()))
                                 (&& (t32? g) (|| pA (llt? (vty b))))
                                  (: u2 (|| (usg? (vty a)) (usg? (vty b)))
                                     (cat af (cat (asll g b)
                                      (? pA ((spop2 'r2 'r3) + (pbrf g op u2 'r2 'r3 'r0 'r1 cc lbl))
                                         ((spop 'r2)
                                          + (? (usg? (vty a)) `(`('li 'r3 0)) `(`('mov 'r3 'r2) `('sar 'r3 31)))
                                          + (pbrf g op u2 'r2 'r3 'r0 'r1 cc lbl))))))
                                 (cat af (cat (tor0 g b)
                                  ((spop 'r1) + `(`('cmp 'r1 'r0) `('br (bcc op (cu2 (vty a) (vty b))) lbl)))))))
                          (: b (cgexpr g env rhs)
                             (? (bad? b) 'bad
                                (flo? (vty b)) (: _ (rfree g (vreg ah)) _ (rfree g (vreg b)) (fcb ()))
                                (pair? g (vty b))                     ; a 32-bit park against a pair right: widen the park
                                 (: fs ((vfs ah) + (vfs b)
                                        + `(`('mov 'r2 (vreg ah)))
                                        + (? (usg? (vty ah)) `(`('li 'r3 0)) `(`('mov 'r3 'r2) `('sar 'r3 31)))
                                        + (pbrf g op (|| (usg? (vty ah)) (usg? (vty b))) 'r2 'r3 'r0 'r1 cc lbl))
                                    _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                    fs)
                                (: fs ((vfs ah) + (vfs b) + `(`('cmp (vreg ah) (vreg b)) `('br (bcc op (cu2 (vty ah) (vty b))) lbl)))
                                   _ (rfree g (vreg ah)) _ (rfree g (vreg b))
                                   fs)))))))))
       (: rhs (? (&& (<e = 'bin) (<>e = '&)) <>>>e ())   ; the mask-test shapes: (v & k) / (k & v)
          lhs (? (two? rhs) <>>e ())
          try (? (<e = 'call) (cbinl g env e cc lbl)
                 (! (two? rhs)) 'no
                 (&& (<rhs = 'num) (immok g 'log <>rhs)) (tbr g env lhs <>rhs cc lbl)
                 (&& (<lhs = 'num) (immok g 'log <>lhs)) (tbr g env rhs <>lhs cc lbl)
                 'no)
          (? (! (id? try 'no)) try
             (: c (cgexpr g env e)
                (? (bad? c) 'bad
                   (flo? (vty c))
                    ; a bare double condition is d != 0.0, NaN-honest -- the
                    ; parity dance rides the branch pair (ctest's set/or
                    ; settle dies); arm64's fcmp ne/eq are honest whole
                    (: fs (cat (vfs c) (? (t32? g)           ; the zero pattern zeroes the r1:r2 pair
                                          `(`('li 'r1 0) `('li 'r2 0) `('movqxr 'f1 'r1) `('ucomisd 'f0 'f1))
                                          `(`('li 'r1 0) `('movqxr 'f1 'r1) `('ucomisd 'f0 'f1))))
                       _ (rfree g (vreg c))
                       (? (arm? g) (fs + `(`('br cc lbl)))   ; VCMP/FCMP ne/eq are NaN-honest whole
                          (cc = 'ne) (fs + `(`('br 'p lbl) `('br 'ne lbl)))
                          (: sk (nlab g ".u")
                             _ (vmflush g)
                             (fs + `(`('br 'p sk) `('br 'eq lbl) `('label sk))))))
                   (pair? g (vty c))                     ; pair truth: either word nonzero (v6-M: no r12 dest; r0 is dead past the branch)
                    ((vfs c) + `(`('ors (? (v6m? g) 'r0 'r12) 'r0 'r1) `('br cc lbl)))
                   (: fs ((vfs c) + `(`('cmp (vreg c) 0) `('br cc lbl)))
                      _ (rfree g (vreg c))
                      fs))))))
   ; n little-endian bytes of a constant (the global initializer image)
   (les n v) (: (go i acc) (? (n <= i) (rev acc)
                              (go (i + 1) (link (& (>> v (* 8 i)) 255) acc)))
               (go 0 ()))
   (zeros n) (? (n < 1) () (link 0 (zeros (n - 1))))
   ; a straight-line block copy of sz bytes, src address in r0, dest in r1,
   ; r2 the scratch -- 8 at a time, then a byte tail (struct = struct copies)
   (bcopy g off sz acc)                              ; `ld`/`st` move one WORD -- 8 bytes, 4 on t32
    (: w (wsize g)
       (? (w <= sz) (bcopy g (off + w) (sz - w)
                     (acc + `(`('ld 'r2 'r0 off) `('st 'r1 off 'r2))))
          (0 < sz) (bcopy g (off + 1) (sz - 1)
                     (acc + `(`('ld1 'r2 'r0 off) `('st1 'r1 off 'r2))))
          acc))
   ; a spill that HOLDS a value across a nested sub-eval (which may itself contain
   ; a call). a bare `push` is 8 bytes and would leave rsp at 8 mod 16 while the
   ; inner call runs -- a SysV alignment violation that glibc's fork child path
   ; (movaps on rbp-relative slots) turns into a #GP. reserve a whole 16-byte cell
   ; so rsp stays 16-aligned through the sub-eval; the callee cleans it as a pair.
   (spush r) `(`('sub 'sp 'sp 16) `('st 'sp 0 r))
   (spop r)  `(`('ld r 'sp 0) `('add 'sp 'sp 16))

   ; -- the PAIR kit (t32 llong/ullong, moon rung 3): a 64-bit value rides r0 (lo) : r1 (hi),
   ; the AAPCS32 return pair. pairs NEVER enter the operand pool or the immediate lanes --
   ; every pair op runs the shuttle, with r2:r3 the popped left operand and r12 the scratch
   ; (IP, already the borrowed temp). the same 16-byte spill cell holds both words. --
   (pair? g ty) (&& (t32? g) (llt? ty))
   (pwide ty) (? (usg? ty) `(`('li 'r1 0)) `(`('mov 'r1 'r0) `('sar 'r1 31)))   ; widen r0 by its SOURCE signedness
   (asll g v) (? (llt? (vty v)) (tor0 g v) ((tor0 g v) + (pwide (vty v))))      ; land a pair on the protocol
   spush2 `(`('sub 'sp 'sp 16) `('st 'sp 0 'r0) `('st 'sp 4 'r1))   ; a VALUE (nullary defines quote -- the (f) trap)
   (spop2 ra rb) `(`('ld ra 'sp 0) `('ld rb 'sp 4) `('add 'sp 'sp 16))
   ; pair load/store at base+off. a load whose base is r0 loads the HIGH word first (into r1),
   ; so the low load may then overwrite the base. pst's base must never be r0/r1 (use r2/r4/r12).
   (pld base off) (? (id? base 'r0) `(`('ld 'r1 base (off + 4)) `('ld 'r0 base off))
                     `(`('ld 'r0 base off) `('ld 'r1 base (off + 4))))
   (pst base off) `(`('st base off 'r0) `('st base (off + 4) 'r1))
   ; the pair ++/-- tail: old rides r0:r1, the sign-widened step lands in r2:r3,
   ; the stepped pair stores back through base+off (base never r0..r3)
   (ppost base off sv)
    `(`('li 'r2 (& sv 4294967295))
      `('li 'r3 (? (sv < 0) 4294967295 0))
      `('adds 'r2 'r0 'r2) `('adc 'r3 'r1 'r3)
      `('st base off 'r2) `('st base (off + 4) 'r3))
   ; flags := (la:lb) - (ra:rb), for a NORMALIZED relation (lt/ge; gt/le swap operands first).
   ; eq/ne OR the two word-differences instead (la is dead after its read: it takes the hi diff).
   ; v6-M: no r12-dest ALU exists, so the differences land IN la:lb -- every call site's left pair
   ; is dead past the compare (a branch consumes it; prel's set overwrites r0 after the flags).
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
       (? (member? o2 '(eq ne)) ((pcmpf g o2 la lb ra rb) + `(`('br o2 lbl)))
          (member? o2 '(lt ge)) ((pcmpf g o2 la lb ra rb) + `(`('br (scc o2 unsg) lbl)))
          ((pcmpf g o2 ra rb la lb) + `(`('br (scc (? (o2 = 'gt) 'lt 'ge) unsg) lbl)))))
   ; a pair shift by a CONSTANT count (kind 'shl/'shr/'sar), value on the protocol; in-place.
   (pshiftc kind c)
    (? (c < 1) ()
       (63 < c) `(`('li 'r0 0) `('li 'r1 0))
       (kind = 'shl)
        (? (c = 32) `(`('mov 'r1 'r0) `('li 'r0 0))
           (32 < c) `(`('mov 'r1 'r0) `('shl 'r1 (c - 32)) `('li 'r0 0))
           `(`('mov 'r2 'r0) `('shr 'r2 (32 - c)) `('shl 'r1 c) `('or 'r1 'r1 'r2) `('shl 'r0 c)))
       (kind = 'shr)
        (? (c = 32) `(`('mov 'r0 'r1) `('li 'r1 0))
           (32 < c) `(`('mov 'r0 'r1) `('shr 'r0 (c - 32)) `('li 'r1 0))
           `(`('mov 'r2 'r1) `('shl 'r2 (32 - c)) `('shr 'r0 c) `('or 'r0 'r0 'r2) `('shr 'r1 c)))
       ; sar: the high word keeps its sign; a >=32 count floods it
       (? (c = 32) `(`('mov 'r0 'r1) `('sar 'r1 31))
          (32 < c) `(`('mov 'r0 'r1) `('sar 'r0 (c - 32)) `('sar 'r1 31))
          `(`('mov 'r2 'r1) `('shl 'r2 (32 - c)) `('shr 'r0 c) `('or 'r0 'r0 'r2) `('sar 'r1 c))))
   ; a pair shift by a VARIABLE count in r2 (0..63), value on the protocol. branchy: the big
   ; lane (count >= 32) moves one word across; the small lane composes the carry-over through
   ; r12 (count 0 skips -- a 32-c shift at c=0 would be UNPREDICTABLE). labels via nlab.
   ; v6-M: no r12 ALU dest and no free low temp -> libgcc's helper (count in r2, the same staging).
   (pshiftv g kind)
    (? (v6m? g)
       `(`('call (? (kind = 'shl) '__aeabi_llsl (kind = 'shr) '__aeabi_llsr '__aeabi_lasr)))
    (: lb (nlab g ".pb") ls (nlab g ".ps") ld (nlab g ".pd")
       _ (vmflush g)
       one (? (kind = 'shl) `(`('mov 'r1 'r0) `('shlv 'r1 'r2) `('li 'r0 0))
              (kind = 'shr) `(`('mov 'r0 'r1) `('shrv 'r0 'r2) `('li 'r1 0))
              `(`('mov 'r0 'r1) `('sarv 'r0 'r2) `('sar 'r1 31)))
       sm (? (kind = 'shl)
             `(`('mov 'r12 'r0) `('li 'r3 32) `('sub 'r3 'r3 'r2) `('shrv 'r12 'r3)
                `('shlv 'r1 'r2) `('or 'r1 'r1 'r12) `('shlv 'r0 'r2))
             (: hop (? (kind = 'shr) 'shrv 'sarv)
                `(`('mov 'r12 'r1) `('li 'r3 32) `('sub 'r3 'r3 'r2) `('shlv 'r12 'r3)
                   `('shrv 'r0 'r2) `('or 'r0 'r0 'r12) `(hop 'r1 'r2))))
       (`(`('cmp 'r2 32) `('br 'ae lb) `('cmp 'r2 0) `('br 'eq ld))
        + sm + `(`('jmp ld) `('label lb) `('sub 'r2 'r2 32))
        + one + `(`('label ld)))))
   ; the 64-bit float bridges (VFP on AArch32 converts only through 32 bits). llong -> double:
   ; hi rides the U-converter and scales by 2^32 (exact -- hi*2^32 is representable), lo adds
   ; exactly, ONE rounding total; the signed face negates the pair in, the double out (INT64_MIN
   ; survives: its u64 face IS 2^63). value r0:r1 -> f0.
   ; v6-M: the whole bridge is one libgcc call (pair already rides r0:r1, the arg protocol).
   (cvtlld g sgn)
    (? (v6m? g)
       `(`('call (? sgn '__aeabi_ul2d '__aeabi_l2d)) `('movqxr 'f0 'r0))
    (: lu (nlab g ".lu") ld (nlab g ".ld")
       _ (vmflush g)
       u2d `(`('cvtui2sd 'f0 'r1)
              `('li 'r2 0) `('li 'r3 1106247680) `('movqxr 'f1 'r2)   ; 2^32 (0x41F00000:0)
              `('mulsd 'f0 'f1)
              `('cvtui2sd 'f1 'r0) `('addsd 'f0 'f1))
       (? sgn u2d                                    ; unsigned: straight through
          (`(`('cmp 'r1 0) `('br 'ge lu)
              `('li 'r2 0) `('li 'r3 0) `('subs 'r0 'r2 'r0) `('sbc 'r1 'r3 'r1))
           + u2d
           + `(`('li 'r2 0) `('li 'r3 0) `('movqxr 'f1 'r2)           ; 0.0 - d
              `('subsd 'f1 'f0) `('movsd 'f0 'f1) `('jmp ld) `('label lu))
           + u2d + `(`('label ld))))))
   ; double -> llong, toward zero: peel sign/exponent off the PATTERN, rebuild the 53-bit
   ; integer mantissa as a pair, shift it into place (the variable pair shifters), refix the
   ; sign. |d| < 1 answers 0; out-of-range is UB and shifts to garbage like the hardware would.
   ; v6-M: one libgcc call (f0 -> the r0:r1 arg pair; toward-zero like the peel, which is
   ; sign-blind bit arithmetic -- only the libcall face needs the target's signedness).
   (cvtdll g usg)
    (? (v6m? g)
       `(`('movqrx 'r0 'f0) `('call (? usg '__aeabi_d2ulz '__aeabi_d2lz)))
    (: l1 (nlab g ".d1") lr (nlab g ".dr") ls (nlab g ".ds") ld (nlab g ".dd")
       _ (vmflush g)
       (`(`('movqrx 'r0 'f0)
           `('mov 'r2 'r1) `('shr 'r2 31) `('sub 'sp 'sp 16) `('st 'sp 0 'r2)   ; park the sign
           `('mov 'r2 'r1) `('shr 'r2 20) `('li 'r3 2047) `('and 'r2 'r2 'r3)   ; e
           `('li 'r3 1048575) `('and 'r1 'r1 'r3)
           `('li 'r3 1048576) `('or 'r1 'r1 'r3)                                ; mantissa | implicit bit
           `('li 'r3 1023) `('cmp 'r2 'r3) `('br 'ge l1)
           `('li 'r0 0) `('li 'r1 0) `('add 'sp 'sp 16) `('jmp ld)              ; |d| < 1 -> 0
           `('label l1)
           `('sub 'r2 'r2 1075) `('cmp 'r2 0) `('br 'lt lr))
        + (pshiftv g 'shl)
        + `(`('jmp ls) `('label lr) `('li 'r3 0) `('sub 'r2 'r3 'r2))
        + (pshiftv g 'shr)
        + `(`('label ls) `('ld 'r2 'sp 0) `('add 'sp 'sp 16)
           `('cmp 'r2 0) `('br 'eq ld)
           `('li 'r2 0) `('li 'r3 0) `('subs 'r0 'r2 'r0) `('sbc 'r1 'r3 'r1)
           `('label ld)))))

   ; a fresh n-byte frame cell -> its (negative) offset. bumps the frame's
   ; high-water like a local; ntmp sizes one for a type (a compound literal's home).
   (nslot g n) (: off (0 - ((peep g 'off 0) + n))
                  _ (pin g 'off ((peep g 'off 0) + n))
                  off)
   (ntmp g ty) (nslot g (& ((tsz g ty) + 7) -8))
   ; load a value from the address in r0, by type -- the dot/deref/clit value tail
   ; (arrays and functions decay to the address; a struct has no register value)
   (loadval g ty addrf)
    (? (arr? ty) (mkv `('ptr <>ty) 'r0 addrf)
       (fn? ty) (mkv `('ptr ty) 'r0 addrf)
       (stru? ty) (mkv ty 'r0 addrf)                 ; a struct value IS its address (the pair reps read through it)
       (flo? ty) (mkv 'double 'r0 (addrf + (fldf ty 'f0 'r0 0)))
       (pair? g ty) (mkv ty 'r0 (addrf + (pld 'r0 0)))
       (mkv ty 'r0 (addrf + `(`((ldop ty) 'r0 'r0 0)))))
   ; the fill code for a compound literal at frame offset off: an aggregate zeroes
   ; then fills (cgfill), a scalar unwraps its single-element brace and stores.
   (clitcode g env off ty ini)
    (? (|| (arr? ty) (stru? ty))
        (: f (cgfill g env off ty ini)
           (? (bad? f) 'bad ((cgzero off (& ((tsz g ty) + 7) -8)) + f)))
       (: v (? (&& (two? ini) (<ini = 'init))
               (? (two? <>ini) (cap (caup ini)) `('num 0)) ini)
          (cgstore g env off ty v)))

   ; -- lvalues: the ADDRESS lands in r0; answers (pointee-ty forms) | 'bad --
   (clval g env e)
    (: k <e
       (? (k = 'var)
           (: en (vent g env <>e)
              (? (bad? en) 'bad
                 (<en = 'regv) 'bad             ; a register home has no address (& is gated out per-fn)
                 ; lean, not lea: an lvalue's address is CONSUMED by its own
                 ; load/store -- the sibcall escape gate must not see it. the
                 ; & lane flips it back to lea (there it really escapes).
                 (<en = 'loc) (mkv <>>en 'r0 `(`('lean 'r0 'r4 <>en)))
                 (mkv <>>en 'r0 `(`('la 'r0 <>en)))))
          (k = 'deref)
           (: r (cgexpr g env <>e)
              (? (bad? r) 'bad
                 (! (ptr? (vty r))) 'bad
                 (mkv (caup (vty r)) 'r0 (tor0 g r))))
          (k = 'dot)
           (: r (: lv (clval g env <>e)
                   ; an rvalue struct base (a call's parked temp: `mk(x).a`) is
                   ; its address too -- same license the asn lane rides
                   (? (! (bad? lv)) lv
                      (: rv (cgexpr g env <>e)
                         (? (bad? rv) 'bad (! (stru? (vty rv))) 'bad rv))))
              (? (bad? r) 'bad
                 (: fm (fmem g (vty r) <>>e)
                    (? (bad? fm) 'bad
                       (mkv <>fm 'r0
                            (cat (vfs r)
                                 (? (<fm = 0) () `(`('add 'r0 'r0 <fm)))))))))
          (k = 'clit)                               ; a compound literal is an lvalue: fill a temp, address it
           (: ty <>e ini <>>e
              off (ntmp g ty)
              fill (clitcode g env off ty ini)
              (? (bad? fill) 'bad
                 (mkv ty 'r0 (fill + `(`('lea 'r0 'r4 off))))))   ; a real lea: the temp's address is the value (& pins via the addr lane, a call blocks via the window)
          'bad))

   ; -- expressions: answers a value tuple | 'bad. g 'want is the DESTINATION
   ; HINT: pinned by a consumer that knows where the value must land (an asn
   ; into a home, a ret's r0), consumed by the TOP node only (cleared here, so
   ; subexpressions never inherit it), and always ADVISORY -- a lane that
   ; ignores it answers its usual register and the consumer bridges. --
   (cgexpr g env e)
    (: wnt (peep g 'want ())
       _ (pin g 'want ())
       k <e
       (? (k = 'szof)                                   ; sizeof EXPR: the UNDECAYED type (clval keeps arrays), size a constant
           (: e2 <>e
              lv (clval g env e2)
              ty (? (bad? lv) (: r (cgexpr g env e2) (? (bad? r) 'bad (: _ (rfree g (vreg r)) (vty r)))) (vty lv))
              (? (bad? ty) 'bad (mkv 'ulong 'r0 `(`('li 'r0 (tsz g ty))))))
          (k = 'num)
           (? (&& (t32? g) (! (&& (-2147483648 <= <>e) (<>e <= 4294967295))))
              ; a literal past the 32-bit word: the pair (C99 decimal picks long long).
              ; split by // and % -- a big literal has no bitwise ops (the imnum lesson)
              (: u (? (<>e < 0) (<>e + 18446744073709551616) <>e)
                 (mkv (? (<>e < 0) 'llong 'ullong) 'r0
                      `(`('li 'r0 (% u 4294967296)) `('li 'r1 (// u 4294967296)))))
              (&& (t32? g) (2147483647 < <>e))
              ; bit 31 set on the 32-bit word: UNSIGNED (0xffffffff reads uint), so a
              ; pair-widen zero-extends -- a signed 'long here sign-smeared the cast
              (: W (? wnt wnt 'r0) (mkv 'ulong W `(`('li W <>e))))
              (: W (? wnt wnt 'r0) (mkv 'long W `(`('li W <>e)))))
          (k = 'flo)
           (? (t32? g)                                  ; the 64-bit pattern splits over the r0:r1 pair
              (: v (fbits <>e)
                 (mkv 'double 'r0 `(`('li 'r0 (& v 4294967295)) `('li 'r1 (& (>> v 32) 4294967295))
                                     `('movqxr 'f0 'r0))))
              (mkv 'double 'r0 `(`('li 'r0 (fbits <>e)) `('movqxr 'f0 'r0))))
          (k = 'str)
           (: lbl (nlab g ".S")
              _ (pin g 'strs (link `(lbl <>e) (g 'strs)))
              (mkv `('ptr 'char) 'r0 `(`('la 'r0 lbl))))
          (k = 'dot)
           (: r (clval g env e)
              (? (bad? r) 'bad
                 (: ty (vty r)
                    (? (bitf? ty)                          ; a bitfield: load the unit, then a 2-shift extract (sar signed, shr unsigned)
                        (: uty <>ty bitoff <>>ty width <>>>ty
                           usz (tsz g uty) hi (64 - (bitoff + width)) lo (64 - width)
                           sgn (! (usg? uty))
                           (mkv 'long 'r0
                                ((vfs r)
                                 + `(`((uldop usz) 'r0 'r0 0))
                                 + (? (hi > 0) `(`('shl 'r0 hi)) ())
                                 + (? (lo > 0) `(`((? sgn 'sar 'shr) 'r0 lo)) ()))))
                       (loadval g ty (vfs r))))))
          (k = 'var)
           (: en (vent g env <>e)
              (? (bad? en)
                  (? (peep (g 'fns) <>e ())     ; a function name is its address
                     (mkv `('ptr (fnty g <>e)) 'r0 `(`('la 'r0 (intern <>e))))
                     (: _ (pin g 'undecl <>e) 'bad))   ; undeclared: name it for the gripe (rung 1); an undeclared CALL funnels here too (call-fixed evaluates its head)
                 (<en = 'regv) (mkv <>>en <>en ())   ; a homed param IS its register: zero forms, borrowed (read-only)
                 (: ty <>>en
                    ap? (peep g 'argpos ())
                    _ (? (|| (stru? ty) (arr? ty)) (pin g 'argpos ()) ())
                    (? (stru? ty)                    ; a struct value is its ADDRESS (the sse2 rep)
                        (? (<en = 'loc)
                           (: v (mkv ty 'r0 `(`('lea 'r0 'r4 <>en))) (? ap? v (escan g v)))
                           (mkv ty 'r0 `(`('la 'r0 <>en))))
                       (arr? ty)                     ; arrays decay to their address
                        (? (<en = 'loc)
                           (: v (mkv `('ptr <>ty) 'r0 `(`('lea 'r0 'r4 <>en))) (? ap? v (escan g v)))
                           (mkv `('ptr <>ty) 'r0 `(`('la 'r0 <>en))))
                       (flo? ty)                     ; a float/double loads into f0 (widened if float)
                        (? (<en = 'loc)
                           (mkv 'double 'r0 (fldf ty 'f0 'r4 <>en))
                           (mkv 'double 'r0 (`(`('la 'r1 <>en)) + (fldf ty 'f0 'r1 0))))
                       (pair? g ty)                    ; a pair local/global reads both words (the address temp
                        (? (<en = 'loc)                ;  dodges r1 -- it is the pair's own high half)
                           (mkv ty 'r0 (pld 'r4 <>en))
                           (mkv ty 'r0 (`(`('la 'r2 <>en)) + (pld 'r2 0))))
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
              (? (&& (! (bad? r)) (bitf? (vty r))) 'bad   ; &bitfield is illegal C -- refuse
                 (! (bad? r))                        ; under & the lvalue's address ESCAPES: flip its leans to leas
                 (: v (mkv `('ptr (vty r)) 'r0 (unlean (vfs r))) (? ap? v (escan g v)))
                 ; &f on a function name: same address as the bare use (no lvalue).
                 ; a VARIABLE of the same name shadows it -- a homed param's regv
                 ; lands here as 'bad and must DEOPT, not read &libm-exp (vent!)
                 (&& ((cap (caup e)) = 'var)
                     (&& (bad? (vent g env (caup (caup e))))
                         (peep (g 'fns) (caup (caup e)) ())))
                  (mkv `('ptr (fnty g (caup (caup e)))) 'r0 `(`('la 'r0 (intern (caup (caup e))))))
                 'bad))
          (k = 'deref)
           (: r (clval g env e)                     ; clval already walked the pointer
              (? (bad? r) 'bad (loadval g (vty r) (vfs r))))
          (k = 'asn)
           (: rv (? ((cap (caup e)) = 'var) (vent g env (caup (caup e))) 'bad)
              (? (&& (two? rv) (<rv = 'regv))   ; store into a register-homed scalar (param or local)
                  (: _ (pin g 'want <>rv)         ; hint the rhs straight at the home
                     r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (flo? (vty r)) (mkv <>>rv 'r0 ((asint g r) + `(`('mov <>rv 'r0))))
                        (id? (vreg r) <>rv) (mkv <>>rv <>rv (vfs r))   ; landed in the home already
                        (: fs ((vfs r) + `(`('mov <>rv (vreg r))))   ; straight from the value's register
                           _ (rfree g (vreg r))
                           (mkv <>>rv <>rv fs))))
                 ; STORE-DIRECT: a plain variable target's address is STATIC (a
                 ; frame offset or a label) -- the value stores from wherever it
                 ; sits, no lean/spush/spop shuttle around it
                 (&& (two? rv) (&& (member? <rv '(loc glo))
                     (: ty9 <>>rv (! (|| (stru? ty9) (|| (arr? ty9) (flo? ty9)))))))
                  (: ty <>>rv
                     nm (caup (caup e))
                     ; the vmap hint: aim the rhs at a fresh pool register so the
                     ; stored value stays RESIDENT (an unhonored hint goes back)
                     hint (? (&& (vmon? g) (&& (id? <rv 'loc)
                               (&& (homable? ty) (member? nm (peep g 'vuniv ())))))
                             (ralloc g) ())
                     _ (? hint (pin g 'want hint) ())
                     r (cgexpr g env <>>e)
                     _ (? (&& hint (|| (bad? r) (! (id? (vreg r) hint)))) (rfree g hint) ())
                     _ (vmdrop g nm)                  ; the write retires the old mapping
                     (? (bad? r) 'bad
                        (pair? g ty)                     ; a pair target stores both words (r2 the address
                         (mkv ty 'r0                    ;  temp -- r1 is the pair's own high half)
                              ((asll g r)
                               + (? (<rv = 'loc) (pst 'r4 <>rv)
                                    (`(`('la 'r2 <>rv)) + (pst 'r2 0)))))
                        (two? (cvt ty 'r0))              ; a narrow target converts on the protocol
                         (mkv ty 'r0 (cat (tor0 g r)
                                      (cat (cvt ty 'r0)
                                           (? (<rv = 'loc)
                                              `(`((stop ty) 'r4 <>rv 'r0))
                                              `(`('la 'r1 <>rv) `((stop ty) 'r1 0 'r0))))))
                        (<rv = 'loc)
                         (: _ (? (member? (vreg r) (peep g 'pool0 ()))
                                 (vmpin g nm (vreg r)) ())    ; the stored value stays mapped
                            (mkv ty (vreg r) ((vfs r) + `(`('st 'r4 <>rv (vreg r))))))
                        (mkv ty (vreg r) ((vfs r) + `(`('la 'r1 <>rv) `('st 'r1 0 (vreg r)))))))
                 (: lv (clval g env <>e)
              (? (bad? lv) 'bad
                 (arr? (vty lv)) 'bad                ; an array is not assignable
                 (stru? (vty lv))                    ; a whole-struct copy (dest r1, src r0)
                  (: rl (clval g env <>>e)      ; an rvalue struct (a call's temp) is its address too
                     rv (? (bad? rl) (cgexpr g env <>>e) rl)
                     (? (bad? rv) 'bad
                        (! (stru? (vty rv))) 'bad
                        (mkv (vty lv) 'r0
                           (cat (vfs lv) (cat (spush 'r0)
                            (cat (tor0 g rv) (cat (spop 'r1)
                             (bcopy g 0 (tsz g (vty lv)) ()))))))))
                 (flo? (vty lv))                     ; a float/double store (addr in r1, value in f0; narrow if float)
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (mkv 'double 'r0 (cat (vfs lv) (cat (spush 'r0)
                         ((asflo g r) + (spop 'r1) + (fstf g (vty lv) 'r1 0 'f0)))))))
                 (bitf? (vty lv))                    ; a bitfield store: unit addr in r1, read-modify-write, result (truncated) in r0
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (: bt (vty lv) uty <>bt bitoff <>>bt width <>>>bt
                           usz (tsz g uty)
                           mask ((<< 1 width) - 1) mv (<< mask bitoff) cmask ((0 - mv) - 1)
                           sgn (! (usg? uty)) lo (64 - width)
                           (mkv 'long 'r0
                              ((vfs lv) + (spush 'r0)
                               + (asint g r) + (spop 'r1)
                               + `(`((uldop usz) 'r2 'r1 0)              ; r2 = the unit
                                   `('li 'r3 mask) `('and 'r0 'r0 'r3)   ; r0 = v & mask (the truncated value)
                                   `('li 'r3 cmask) `('and 'r2 'r2 'r3)  ; clear the field's bits
                                   `('mov 'r3 'r0))
                               + (? (bitoff > 0) `(`('shl 'r3 bitoff)) ())   ; r3 = (v & mask) << bitoff
                               + `(`('or 'r2 'r2 'r3) `((stop uty) 'r1 0 'r2))
                               + (? sgn `(`('shl 'r0 lo) `('sar 'r0 lo)) ()))))))
                 (pair? g (vty lv))                  ; a pair store through a computed address
                  (: r (cgexpr g env <>>e)
                     (? (bad? r) 'bad
                        (mkv (vty lv) 'r0 (cat (vfs lv) (cat (spush 'r0)
                         ((asll g r) + (spop 'r2) + (pst 'r2 0)))))))
                 ; the ADDRESS parks in a pool register past a call-free rhs
                 ; (the vpark discipline, extended to the store shuttle): no
                 ; spush/spop, no memory round-trip on the address. a call on
                 ; the right keeps the shuttle -- it would clobber the park.
                 (: aR (? (callish? <>>e) () (ralloc g))
                    r (cgexpr g env <>>e)
                    (? (bad? r) (: _ (? aR (rfree g aR) ()) 'bad)
                       (: ty (vty lv)
                          (? aR
                             (: fs (cat (vfs lv)
                                    (cat `(`('mov aR 'r0))
                                     (cat (asint g r)
                                      (cat (cvt ty 'r0)
                                           `(`((stop ty) aR 0 'r0))))))
                                _ (rfree g aR)
                                (mkv ty 'r0 fs))
                             (mkv ty 'r0 (cat (vfs lv)
                                    (cat (spush 'r0)
                                     (cat (asint g r)
                                      (cat (spop 'r1)
                                       (cat (cvt ty 'r0)
                                            `(`((stop ty) 'r1 0 'r0))))))))))))))))
          (k = 'post)
           (: rv (? ((cap (caup e)) = 'var) (vent g env (caup (caup e))) 'bad)
              _ (? ((cap (caup e)) = 'var) (vmdrop g (caup (caup e))) ())   ; the RMW retires any mapping
              (? (&& (two? rv) (<rv = 'regv))   ; p++ on a register-homed scalar: old in r0, home += step
                  (: ty <>>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     (mkv ty 'r0 `(`('mov 'r0 <>rv)
                                    `('add <>rv 'r0 (* stp <>>e)))))
                 ; STORE-DIRECT (the asn STORE-DIRECT twin): a frame scalar's
                 ; slot is a STATIC offset -- read/step/write with a direct
                 ; frame ld+st, no lean-materialize + indirect RMW. leaves no
                 ; lean, so a loop counter's slot no longer reads as address-
                 ; taken and the local can home (lpick's leaoffs)
                 (&& (two? rv) (&& (<rv = 'loc)
                     (: t9 <>>rv (! (|| (stru? t9) (|| (arr? t9) (|| (flo? t9) (bitf? t9))))))))
                  (: ty <>>rv off <>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     (? (pair? g ty)
                        (mkv ty 'r0 ((pld 'r4 off) + (ppost 'r4 off (* stp <>>e))))
                        (mkv ty 'r0 `(`((ldop ty) 'r0 'r4 off)
                                       `('add 'r1 'r0 (* stp <>>e))
                                       `((stop ty) 'r4 off 'r1)))))
                 (: lv (clval g env <>e)
              (? (bad? lv) 'bad
                 (: ty (vty lv)
                    stp (? (ptr? ty) (tsz g <>ty) 1)
                    (? (arr? ty) 'bad
                       (bitf? ty) 'bad             ; bitfield ++/-- -- refuse (rare; use `f = f + 1`)
                       (pair? g ty)
                        (mkv ty 'r0 (cat (vfs lv)
                               (`(`('mov 'r12 'r0)) + (pld 'r12 0) + (ppost 'r12 0 (* stp <>>e)))))
                       (mkv ty 'r0 (cat (vfs lv)
                              `(`('mov 'r2 'r0)
                                 `((ldop ty) 'r0 'r2 0)
                                 `('add 'r1 'r0 (* stp <>>e))
                                 `((stop ty) 'r2 0 'r1))))))))))
          (k = 'cast)
           (: tgt <>e
              _ (? (&& (! (flo? tgt)) (! (two? (cvt tgt 'r0)))) (pin g 'want wnt) ())
              r (cgexpr g env <>>e)
              (? (bad? r) 'bad
                 (&& (flo? tgt) (! (flo? (vty r)))) (mkv 'double 'r0 (asflo g r))  ; int -> double
                 (&& (! (flo? tgt)) (flo? (vty r)))                              ; double -> int, then narrow
                  (? (pair? g tgt)                                               ; double -> llong: the pattern peel
                      (mkv tgt 'r0 ((vfs r) + (cvtdll g (usg? tgt))))
                     (&& (t32? g) (usg? tgt))                                    ; t32 unsigned: the U-converter
                      (mkv tgt 'r0 ((vfs r) + `(`('cvttsd2ui 'r0 'f0)) + (cvt tgt 'r0)))
                     (mkv tgt 'r0 ((asint g r) + (cvt tgt 'r0))))
                 (flo? tgt) (mkv 'double 'r0 (vfs r))                            ; double -> double
                 (&& (pair? g tgt) (! (llt? (vty r))))                           ; widen to the pair (source signedness)
                  (mkv tgt 'r0 (asll g r))
                 (two? (cvt tgt 'r0)) (mkv tgt 'r0 ((tor0 g r) + (cvt tgt 'r0)))   ; int -> narrower int
                 (mkv tgt (vreg r) (vfs r))))                                    ; a full-word int cast KEEPS the register
          (k = 'un)
           (: r (cgexpr g env <>>e)
              (? (bad? r) 'bad
                 (flo? (vty r))                        ; unary on a double
                  (: z1 (? (t32? g) `(`('li 'r0 0) `('li 'r1 0) `('movqxr 'f1 'r0))   ; 0.0 (the t32 pair)
                           `(`('li 'r0 0) `('movqxr 'f1 'r0)))
                     (? (<>e = 'neg)                 ; -d = 0.0 - d
                         (mkv 'double 'r0 (cat (vfs r) (z1 + `(`('subsd 'f1 'f0) `('movsd 'f0 'f1)))))
                        (<>e = 'not)                 ; !d = (d == 0.0), NaN-honest: !NaN is false (eq AND ordered)
                         (mkv 'long 'r0 (cat (vfs r) (z1 + `(`('ucomisd 'f0 'f1) `('set 'eq 'r0)
                                                             `('set 'np 'r1) `('and 'r0 'r0 'r1)))))
                        'bad))                           ; ~ on a double is invalid
                 (pair? g (vty r))                     ; unary on the pair
                  (? (<>e = 'neg)
                      (mkv (vty r) 'r0 ((tor0 g r) + `(`('li 'r2 0) `('li 'r3 0)
                                                       `('subs 'r0 'r2 'r0) `('sbc 'r1 'r3 'r1))))
                     (<>e = 'bnot)
                      (mkv (vty r) 'r0 ((tor0 g r) + `(`('not 'r0) `('not 'r1))))
                     (mkv 'long 'r0 ((tor0 g r) + `(`('ors (? (v6m? g) 'r0 'r12) 'r0 'r1) `('set 'eq 'r0)))))
                 ; -1ULL stays unsigned; a SUB-64 unsigned re-narrows after
                 ; neg/~ (the model keeps unsigned intermediates canonically
                 ; zero-extended: -1u must wrap at 32 bits, not ride 64)
                 (: uty (&& (member? <>e '(neg bnot)) (usg? (vty r)))
                    renar (? (! uty) () ((vty r) = 'uchar) `(`('zx1 'r0))
                             ((vty r) = 'ushort) `(`('zx2 'r0))
                             ((vty r) = 'uint) `(`('zx4 'r0)) ())
                    (mkv (? uty (vty r) 'long) 'r0
                       (cat (tor0 g r)
                            (cat (? (<>e = 'neg) `(`('neg 'r0))
                                    (<>e = 'bnot) `(`('not 'r0))
                                    `(`('cmp 'r0 0) `('set 'eq 'r0)))
                                 renar))))))
          (k = 'comma)
           (: a (cgexpr g env <>e)
              _ (pin g 'want wnt)
              b (cgexpr g env <>>e)
              (? (|| (bad? a) (bad? b)) 'bad
                 (mkv (vty b) (vreg b) ((vdrop g a) + (vfs b)))))
          (k = 'land)                                ; as a VALUE: cbranch the short-circuit, then settle the bit
           (: lf (nlab g ".f") ld (nlab g ".d")
              f (cbranch g env e 'eq lf)
              _ (vmflush g)                          ; labels lf/ld join
              (? (bad? f) 'bad
                 (mkv 'long 'r0 (cat f `(`('li 'r0 1) `('jmp ld)
                                          `('label lf) `('li 'r0 0) `('label ld))))))
          (k = 'lor)
           (: lt (nlab g ".t") ld (nlab g ".d")
              f (cbranch g env e 'ne lt)
              _ (vmflush g)                          ; labels lt/ld join
              (? (bad? f) 'bad
                 (mkv 'long 'r0 (cat f `(`('li 'r0 0) `('jmp ld)
                                          `('label lt) `('li 'r0 1) `('label ld))))))
          (k = 'cond)
           (: le (nlab g ".e") ld (nlab g ".d")
              c (cbranch g env <>e 'eq le)
              t (cgexpr g env <>>e)
              _ (vmflush g)                          ; label le joins
              el (cgexpr g env <>>>e)
              _ (vmflush g)                          ; label ld joins
              (? (|| (bad? c) (|| (bad? t) (bad? el))) 'bad
                 ; C's conditional CONVERTS its arms: one flo arm makes the whole
                 ; result flo, so the INTEGER arm rides asflo at its edge -- typing
                 ; by the then-arm alone left `(Cp(a) ? im(a) : 0)`'s int 0 in r0
                 ; while the reader took f0 (the eq lane's charm×twin face).
                 (: fl (|| (flo? (vty t)) (flo? (vty el)))
                    pl (|| (pair? g (vty t)) (pair? g (vty el)))   ; one pair arm pairs the other too
                    tf (? fl (asflo g t) pl (asll g t) (tor0 g t))
                    ef (? fl (asflo g el) pl (asll g el) (tor0 g el))
                    rt (? (flo? (vty t)) (vty t) fl (vty el)
                          pl (? (llt? (vty t)) (vty t) (vty el)) (vty t))
                    (mkv rt 'r0
                       (cat c
                        (cat tf (cat `(`('jmp ld) `('label le))
                         (ef + `(`('label ld))))))))))
          ; -- the varargs builtins, per target (crew/moon/include/stdarg.h). SysV: the
          ; 24-byte { int gp_offset; int fp_offset; void *overflow_arg_area; void
          ; *reg_save_area; } -- va_arg fetches from the save area until the offset
          ; passes the reg limit, then from the overflow area. AAPCS64: the 32-byte
          ; { void *__stack; void *__gr_top; void *__vr_top; int __gr_offs; int
          ; __vr_offs; } -- the offs fields are NEGATIVE, counted back from each
          ; area's top, rising by the stride toward 0; at >= 0 the arg is on the
          ; caller stack at __stack. va_start seeds either past the named params;
          ; va_end nops. --
          (k = 'vastart)
           ; cgexpr (not clval) gives the struct pointer uniformly: a va_list LOCAL
           ; (array) decays to its address, a va_list PARAM (a decayed pointer)
           ; loads its value -- both land the struct address in r0.
           (: lv (cgexpr g env <>e)                 ; the va_list struct pointer in r0
              rsa (peep g 'rsa 0) ngp (peep g 'vagp 0) nxmm (peep g 'vaxmm 0)
              nov (peep g 'vaov 0)                  ; named params that overflowed (slots)
              (? (bad? lv) 'bad
                 (: _ (pin g 'fesc 1)                                       ; the va_list HOLDS frame addresses: no sibcalls here
                    (mkv 'long 'r0 (cat (tor0 g lv)
                      (? (t32? g)   ; AAPCS32: __ap = the first anonymous word, past the push cells + named regs + named overflow
                         `(`('lea 'r1 'r4 ((ovb g) + ((* 4 ngp) + nov))) `('st 'r0 0 'r1))
                         (a64? g)
                         `(`('lea 'r1 'r4 ((ovb g) + (* 8 nov))) `('st 'r0 0 'r1)   ; __stack = fp+32 + named overflow
                            `('lea 'r1 'r4 (rsa + 64))  `('st 'r0 8 'r1)            ; __gr_top (past x0..x7)
                            `('lea 'r1 'r4 (rsa + 192)) `('st 'r0 16 'r1)           ; __vr_top (past q0..q7)
                            `('li 'r1 (0 - (* 8 (8 - ngp))))  `('st4 'r0 24 'r1)    ; __gr_offs = -(unnamed gp)*8
                            `('li 'r1 (0 - (* 16 (8 - nxmm)))) `('st4 'r0 28 'r1))  ; __vr_offs = -(unnamed vr)*16
                         `(`('li 'r1 (* 8 ngp))         `('st4 'r0 0 'r1)   ; gp_offset = 8*named-gp
                            `('li 'r1 (48 + (* 16 nxmm))) `('st4 'r0 4 'r1)  ; fp_offset = 48 + 16*named-xmm
                            `('lea 'r1 'r4 16)  `('st 'r0 8 'r1)             ; overflow_arg_area = rbp+16
                            `('lea 'r1 'r4 rsa) `('st 'r0 16 'r1))))))))      ; reg_save_area = rbp+rsa
          (k = 'vaarg)
           (: lv (cgexpr g env <>e) ty <>>e    ; the va_list struct pointer in r0
              lov (nlab g ".vo") ld (nlab g ".vd")
              ; r1 = the relevant offset; r2 = the fetch address. SysV: offset below
              ; the limit reads reg_save_area + offset and advances; else overflow,
              ; which advances 8. AAPCS64: a NEGATIVE offs reads top + offs (ld4
              ; sign-extends) and advances toward 0; at >= 0 the arg rides __stack,
              ; which advances 8 (every AAPCS stack slot is one 8-byte cell).
              ; AAPCS32 (t32): __ap just walks, one word per anonymous arg.
              walk (? (t32? g)
                      `(`('ld 'r2 'r0 0) `('mov 'r3 'r2) `('add 'r3 'r3 4) `('st 'r0 0 'r3))
                      (a64? g)
                      (: foff (? (flo? ty) 28 24)               ; __vr_offs / __gr_offs
                         toff (? (flo? ty) 16 8)                ; __vr_top / __gr_top
                         step (? (flo? ty) 16 8)                ; a vr slot is one q (16)
                         `(`('ld4 'r1 'r0 foff) `('cmp 'r1 0) `('br 'ge lov)
                            `('ld 'r2 'r0 toff) `('add 'r2 'r2 'r1)           ; p = top + offs
                            `('add 'r1 'r1 step) `('st4 'r0 foff 'r1)          ; advance the offs
                            `('jmp ld) `('label lov)
                            `('ld 'r2 'r0 0) `('mov 'r3 'r2) `('add 'r3 'r3 8) `('st 'r0 0 'r3)   ; p = __stack; __stack += 8
                            `('label ld)))
                      (: foff (? (flo? ty) 4 0)                 ; fp_offset (4) vs gp_offset (0)
                         lim (? (flo? ty) 176 48)               ; the register-area limit
                         step (? (flo? ty) 16 8)                ; the offset stride
                         `(`('ld4 'r1 'r0 foff) `('cmp 'r1 lim) `('br 'ge lov)
                            `('ld 'r2 'r0 16) `('add 'r2 'r2 'r1)            ; p = reg_save_area + offset
                            `('add 'r1 'r1 step) `('st4 'r0 foff 'r1)         ; advance the offset
                            `('jmp ld) `('label lov)
                            `('ld 'r2 'r0 8) `('mov 'r3 'r2) `('add 'r3 'r3 8) `('st 'r0 8 'r3)   ; p = overflow; overflow += 8
                            `('label ld))))
              _ (vmflush g)                           ; the walk's labels join
              (? (bad? lv) 'bad
                 (&& (t32? g) (|| (flo? ty) (|| (llt? ty) (stru? ty)))) 'bad   ; t32 va_arg reads WORDS only (love.c's shape) -- refuse LOUD
                 (flo? ty) (mkv 'double 'r0 ((tor0 g lv) + walk + (fldf ty 'f0 'r2 0)))
                 (mkv ty 'r0 ((tor0 g lv) + walk + `(`((ldop ty) 'r0 'r2 0))))))
          (k = 'vaend) (mkv 'long 'r0 ())
          (k = 'call)
           ; the SysV ABI: integer/pointer args ride r6 r5 r2 r1 r7 r8 (<=6),
           ; double args ride f0..f7 (<=8), SEPARATE counters, placed by the
           ; callee's PARAM type (a direct call reads the sig table -- converting
           ; the arg to it), else by the arg's own type. the result rides f0 for
           ; a double-returning callee, r0 otherwise. args evaluate left to right,
           ; each pushed to the machine stack, then popped reversed into place.
           (: head <>e args <>>e
              bi (? (<head = 'var) <>head ())
              (? ; the gcc builtins love.c reaches -- x64 as short raw splices, arm64
                 ; as neutral IR (its ops cover them) or one raw word (clz):
                 ; trap = ud2 / brk; inf/nanf = IEEE constants into f0; expect =
                 ; its value (the hint drops); clear_cache = nothing (coherent on
                 ; x86; qemu-user keeps arm64 honest -- real-metal arm64 is the
                 ; nolibc rung's business); clzll = bsr+xor / clz; isinf =
                 ; bits<<1 == inf<<1; the overflow trio = the op + the overflow
                 ; flag (seto / set vs, mul via mulo's smulh check), result
                 ; stored through arg3, the flag the value.
                 (bi = "__builtin_trap") (? (arm? g) (mkv 'long 'r0 `(`('trap)))
                                            (mkv 'long 'r0 `(`('raw `(15 11)))))
                 (bi = "__builtin_inf")
                  (? (t32? g) (mkv 'double 'r0 `(`('li 'r0 0) `('li 'r1 2146435072) `('movqxr 'f0 'r0)))
                     (mkv 'double 'r0 `(`('li 'r0 9218868437227405312) `('movqxr 'f0 'r0))))
                 (|| (bi = "__builtin_nanf") (bi = "__builtin_nan"))   ; both take a (ignored) tag string; one quiet-NaN pattern
                  (? (t32? g) (mkv 'double 'r0 `(`('li 'r0 0) `('li 'r1 2146959360) `('movqxr 'f0 'r0)))
                     (mkv 'double 'r0 `(`('li 'r0 9221120237041090560) `('movqxr 'f0 'r0))))
                 (bi = "__builtin_expect") (cgexpr g env <args)
                 (bi = "__builtin___clear_cache")
                  ; x86 keeps I-cache coherent with D-cache: nothing to do. arm64
                  ; does NOT (qemu-user is coherent, metal is not): the architectural
                  ; sequence -- dc cvau each D-line, dsb, ic ivau each I-line, dsb+isb,
                  ; strides read off CTR_EL0 (DminLine[19:16] / IminLine[3:0], log2
                  ; words). the system insns ride raw words (llvm-mc-checked), and the
                  ; variable stride shift too (IR shlv wants its count in r1 -- r1
                  ; holds `end` here).
                  ; scratch = r2/r3/r9/r15 ONLY: r9 rides the pool (dead here by
                  ; callish? parking + vmflush), r15 the callr park (live only at a
                  ; tail), r2/r3 arg regs past their args. NEVER r10-r14 -- those are
                  ; the a64 param/local HOMES, resident across the whole fn and only
                  ; spilled around real calls (this lane clobbering the r10 home was
                  ; a boot segfault the day lvm_nif first passed the homing gate).
                  (? (! (a64? g)) (mkv 'long 'r0 ())
                     (: e1 (cgexpr g env <args)
                        e2 (cgexpr g env <>args)
                        _ (vmflush g)                 ; labels join + the splice clobbers pool regs
                        lb (nlab g ".ccd") db (nlab g ".ccdd")
                        li9 (nlab g ".cci") di (nlab g ".ccid")
                        (? (|| (bad? e1) (bad? e2)) 'bad
                           (mkv 'long 'r0
                             (cat (asint g e1) (cat (spush 'r0)
                              (cat (asint g e2) (cat (`(`('mov 'r1 'r0)) + (spop 'r0))
                               `(`('raw `(34 0 59 213))          ; mrs x2, ctr_el0
                                  `('li 'r9 15)
                                  `('and 'r15 'r2 'r9)
                                  `('li 'r3 4)
                                  `('raw `(111 32 207 154))      ; lsl x15, x3, x15 -- isize
                                  `('shr 'r2 16)
                                  `('and 'r2 'r2 'r9)
                                  `('raw `(98 32 194 154))       ; lsl x2, x3, x2 -- dsize
                                  `('sub 'r3 'r2 1)
                                  `('not 'r3)
                                  `('and 'r9 'r0 'r3)            ; begin & ~(dsize-1)
                                  `('label lb)
                                  `('cmp 'r9 'r1)
                                  `('br 'ae db)
                                  `('raw `(41 123 11 213))       ; dc cvau, x9
                                  `('add 'r9 'r9 'r2)
                                  `('jmp lb)
                                  `('label db)
                                  `('raw `(159 59 3 213))        ; dsb ish
                                  `('sub 'r3 'r15 1)
                                  `('not 'r3)
                                  `('and 'r9 'r0 'r3)            ; begin & ~(isize-1)
                                  `('label li9)
                                  `('cmp 'r9 'r1)
                                  `('br 'ae di)
                                  `('raw `(41 117 11 213))       ; ic ivau, x9
                                  `('add 'r9 'r9 'r15)
                                  `('jmp li9)
                                  `('label di)
                                  `('raw `(159 59 3 213))        ; dsb ish
                                  `('raw `(223 63 3 213)))))))))))   ; isb
                 (bi = "__builtin_clzll")
                  (: e1 (cgexpr g env <args)
                     (? (bad? e1) 'bad
                        (t32? g)                     ; the pair: clz(hi), or 32 + clz(lo) when hi is 0
                         (: lz (nlab g ".cz") ld (nlab g ".czd")
                            _ (vmflush g)
                            (mkv 'long 'r0 ((asll g e1) +
                              (? (v6m? g)   ; no CLZ on v6-M: libgcc's __clzsi2 (arg/result r0)
                                 `(`('cmp 'r1 0) `('br 'eq lz) `('mov 'r0 'r1) `('call '__clzsi2) `('jmp ld)
                                    `('label lz) `('call '__clzsi2) `('add 'r0 'r0 32) `('label ld))
                                 `(`('cmp 'r1 0) `('br 'eq lz) `('clz 'r0 'r1) `('jmp ld)
                                    `('label lz) `('clz 'r0 'r0) `('add 'r0 'r0 32) `('label ld))))))
                        (mkv 'long 'r0 ((asint g e1) +
                                        (? (a64? g) `(`('raw `(0 16 192 218)))                    ; clz x0, x0
                                           `(`('raw `(72 15 189 192 72 131 240 63))))))))   ; bsr rax,rax; xor rax,63
                 (bi = "__builtin_isinf")
                  (: e1 (cgexpr g env <args)
                     (? (bad? e1) 'bad
                        (mkv 'long 'r0 (cat (asflo g e1)
                                  (? (v6m? g)                 ; the r12-free twin: the diffs land in r1 (dead past the set)
                                     `(`('movqrx 'r0 'f0) `('li 'r2 2147483647) `('and 'r1 'r1 'r2)
                                        `('li 'r2 2146435072) `('subs 'r1 'r1 'r2)
                                        `('ors 'r1 'r1 'r0) `('set 'eq 'r0))
                                     (t32? g)                 ; |hi| == 0x7FF00000 and lo == 0, over the pair
                                     `(`('movqrx 'r0 'f0) `('li 'r2 2147483647) `('and 'r1 'r1 'r2)
                                        `('li 'r2 2146435072) `('subs 'r12 'r1 'r2)
                                        `('ors 'r12 'r12 'r0) `('set 'eq 'r0))
                                     (a64? g)
                                     `(`('movqrx 'r0 'f0) `('shl 'r0 1) `('li 'r1 18437736874454810624)
                                        `('cmp 'r0 'r1) `('set 'eq 'r0))
                                     (cat `(`('movqrx 'r0 'f0) `('raw `(72 209 224)) `('li 'r1 18437736874454810624))   ; shl rax,1; rcx = inf<<1
                                          `(`('raw `(72 57 200 15 148 192 72 15 182 192)))))))))     ; cmp; sete al; movzx rax,al
                 (member? bi '("__builtin_add_overflow" "__builtin_sub_overflow" "__builtin_mul_overflow"))
                  (: e1 (cgexpr g env <args)
                     e2 (cgexpr g env <>args)
                     ; the &t arg is consumed by the splice's own store -- argpos,
                     ; like a call arg (else every overflow user loses sibcalls)
                     e3 (: _ (? (member? (cap (cauup args)) '(addr var)) (pin g 'argpos 1) ())
                           (cgexpr g env <>>args))
                     _ (pin g 'argpos ())
                     opb (? (bi = "__builtin_add_overflow") `(72 1 200)      ; add rax,rcx
                            (bi = "__builtin_sub_overflow") `(72 41 200)     ; sub rax,rcx
                            `(72 15 175 193))                                ; imul rax,rcx
                     ; the op leaves r0 the result, r1 the overflow bit
                     ovf (? (! (arm? g))
                            `(`('raw (opb + `(15 144 193 72 15 182 201))))          ; op; seto cl; movzx rcx,cl
                            (! (bi = "__builtin_mul_overflow"))
                             `(`((? (bi = "__builtin_add_overflow") 'adds 'subs) 'r0 'r0 'r1) `('set 'vs 'r1))
                            (v6m? g)                          ; no SMULL: sign-widen both words through __aeabi_lmul,
                             (cat (spush 'r3)                 ; then hi vs lo>>31. r3 = the &t address -- saved around the call
                              (cat `(`('mov 'r2 'r1) `('mov 'r1 'r0) `('sar 'r1 31)
                                     `('mov 'r3 'r2) `('sar 'r3 31) `('call '__aeabi_lmul)
                                     `('mov 'r2 'r0) `('sar 'r2 31) `('subs 'r1 'r1 'r2) `('set 'ne 'r1))
                               (spop 'r3)))
                            (t32? g)                          ; SMULL, then hi vs lo>>31 (no 64-bit regs)
                             `(`('smull 'r0 'r2 'r0 'r1) `('mov 'r12 'r0) `('sar 'r12 31)
                                `('subs 'r1 'r2 'r12) `('set 'ne 'r1))
                            (: lo (nlab g ".vo") l2 (nlab g ".vd")
                               `(`('mulo 'r0 'r0 'r1 'r2 lo) `('li 'r1 0) `('jmp l2)
                                  `('label lo) `('li 'r1 1) `('label l2))))
                     ; &t's frame lea must not open the sibs WINDOW: the x64 raw
                     ; splice resets it as a call boundary, but arm64's neutral IR
                     ; is no boundary -- the open window reached every lvm ap's
                     ; Continue and silently declined its tail (74k C frames at
                     ; boot, the raw-arm64 stack gone). the address is consumed
                     ; by the splice's own store, so it reads lean here; x64 IR
                     ; stays untouched (byte-identity).
                     e3f (? (arm? g) (relean (tor0 g e3)) (tor0 g e3))
                     _ (vmflush g)                    ; the a64 mulo lane's labels join
                     (? (|| (bad? e1) (|| (bad? e2) (bad? e3))) 'bad
                        (mkv 'long 'r0 (cat (asint g e1) (cat (spush 'r0)
                                  (cat (asint g e2) (cat (spush 'r0)
                                   (cat e3f (cat (`(`('mov 'r3 'r0)) + (spop 'r1) + (spop 'r0))
                                    (cat ovf
                                         `(`('st 'r3 0 'r0) `('mov 'r0 'r1))))))))))))
                 (: inlf (? (peep g 'inlining ()) ()
                            (! (<head = 'var)) ()
                            (! (bad? (vent g env <>head))) ()   ; a local/global shadows the name
                            (peep (g 'inl) <>head ()))
                    ir9 (? (&& (two? inlf) ((tally args) = (tally <>>inlf)))
                           (cginl g env inlf args) 'no)              ; 'no: splice declined (fesc or shape) -> the real call
                    (? (! (id? ir9 'no)) ir9
                 (: direct (&& (<head = 'var)
                               (&& (bad? (vent g env <>head))                ; not a local/global variable...
                                   (peep (g 'fns) <>head ())))               ; ...and a KNOWN function (typos refuse)
                    sig (? direct (peep (g 'sigs) <>head ()) ())
                    ptys (? (two? sig) <>sig ())
                    rt (? (two? sig) <sig 'long)
                    variadic (? (two? sig) (nthr sig 2) ())
                    ; every call rides the SysV registers (call-fixed); a variadic one
                    ; additionally sets al = #xmm args for its callee's register save area.
                    cr (call-fixed g env head args direct ptys rt variadic)
                    (? (bad? cr) 'bad
                       (! (stru? (vty cr))) cr
                       (&& (v6m? g) (4 < (tsz g (vty cr)))) cr   ; v6-M MEMORY return: the sret temp's address in r0 IS the struct value
                       (sse2? g (vty cr))              ; a two-double struct result: park the xmm pair in a fresh slot, answer its address
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (cat (vfs cr)
                                        (cat (fstf g 'double 'r4 toff (xmmreg 0))
                                         (cat (fstf g 'double 'r4 (toff + 8) (xmmreg 1))
                                              `(`('lea 'r0 'r4 toff)))))))
                       (&& (t32? g)                    ; t32: past the d-pair, only the 8B sse blob (d0) and the <=4B int one (r0) return
                           (! (|| (&& (id? (aone g (vty cr)) 'sse) ((tsz g (vty cr)) = 8))
                                  (&& (id? (aone g (vty cr)) 'int) ((tsz g (vty cr)) <= 4))))) 'bad
                       (ipair? g (vty cr))             ; a two-int struct result: park rax:rdx (arm64 x0:x1) the same way
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (cat (vfs cr)
                                        `(`('st 'r4 toff 'r0) `('st 'r4 (toff + 8) (ip2 g))
                                           `('lea 'r0 'r4 toff)))))
                       (id? (aone g (vty cr)) 'sse)    ; a small SSE aggregate result: park xmm0
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (cat (vfs cr)
                                        (cat (fstf g 'double 'r4 toff (xmmreg 0))
                                             `(`('lea 'r0 'r4 toff))))))
                       (id? (aone g (vty cr)) 'int)    ; a small INT aggregate result: park rax
                        (: toff (nslot g 16)
                           (mkv (vty cr) 'r0 (cat (vfs cr)
                                        `(`('st 'r4 toff 'r0) `('lea 'r0 'r4 toff)))))
                       'bad))))))
          (k = 'clit)                              ; a compound literal as a value: fill the temp, load it
           (: lv (clval g env e)
              (? (bad? lv) 'bad (loadval g (vty lv) (vfs lv))))
          (k = 'bin) (cgbin g env e wnt)
          'bad))
   ; a TARGETABLE gp register argument: pure and single-emission, reading only
   ; an immediate, the frame, a home register, or a label -- so it skips the
   ; push/pop cell entirely and lands straight in its own argument register,
   ; emitted between the home spills and the pops (targets are disjoint; pops
   ; read only sp cells). deferring a pure read past its siblings is the same
   ; unspecified-order license the overflow-first emission already rides. a
   ; homed-param read is the one cross-entry hazard (another targeted arg may
   ; write its home first), so it carries BOTH emissions -- the home mov and
   ; the spill-slot load -- and the caller picks by conflict.
   ; -> ((ez i) forms) | ((ezm i rH) movforms ldforms) | 'no
   (ezarg g env e pty i)
    (: rT (nthr (argr g) i)
       (fin ty fs) (: aty (? pty pty ty)
                      (? (|| (flo? aty) (|| (stru? aty) (llt? aty))) 'no `(`('ez i) fs)))   ; a pair arg keeps the cell path
       (? (<e = 'num) (? (&& (t32? g) (! (&& (-2147483648 <= <>e) (<>e <= 4294967295)))) 'no
                         (fin 'long `(`('li rT <>e))))
          (<e = 'str)
           (: lbl (nlab g ".S")
              _ (pin g 'strs (link `(lbl <>e) (g 'strs)))
              (fin `('ptr 'char) `(`('la rT lbl))))
          (<e = 'addr)                          ; &var only: lea/la, argpos-equivalent (no escan)
           (? (! ((cap (caup e)) = 'var)) 'no
              (: en (vent g env (caup (caup e)))
                 (? (bad? en) 'no
                    (<en = 'loc) (fin `('ptr <>>en) `(`('lea rT 'r4 <>en)))
                    (<en = 'glo) (fin `('ptr <>>en) `(`('la rT <>en)))
                    'no)))
          (! (<e = 'var)) 'no
          (: en (vent g env <>e)
             (? (bad? en) 'no
                (: ty <>>en
                   (? (<en = 'regv)             ; a homed param: mov from the home, or ld its spill slot
                       (? (|| (flo? (? pty pty ty)) (stru? (? pty pty ty))) 'no
                          (: sl (assoc <>en (peep g 'homes ()))
                             (? (! (two? sl)) 'no
                                `(`('ezm i <>en)
                                   `(`('mov rT <>en))
                                   `(`('ld rT 'r4 <>sl))))))
                      (arr? ty)                      ; array decay: its address
                       (? (<en = 'loc) (fin `('ptr <>ty) `(`('lea rT 'r4 <>en)))
                          (fin `('ptr <>ty) `(`('la rT <>en))))
                      (|| (stru? ty) (flo? ty)) 'no
                      (<en = 'loc) (fin ty `(`((ldop ty) rT 'r4 <>en)))
                      (fin ty `(`('la rT <>en) `((ldop ty) rT rT 0)))))))))   ; a global: la then a self-based load
   ; the SysV register call, stack overflow included: args 1..6 gp / 1..8 xmm ride
   ; registers, the REST ride the caller stack (arg7 shallowest). evaluation order is
   ; free in C, so the overflow group emits FIRST, then the register group, both
   ; right-to-left -- the register pops then run forward and the overflow block is
   ; left sitting at [sp] for the callee; the caller cleans it after the call (an odd
   ; count takes an 8-byte pad UNDER it). va? marks a call to a VARIADIC function --
   ; additionally `al` (rax's low byte) carries the number of vector (xmm) arguments,
   ; which its variadic callee's prologue reads to decide whether to spill the xmm
   ; registers. params named uniquely so no lambda parameter shadows a sibling
   ; :-local through boxfix's shared cell (stage 5).
   (call-fixed g env hd ags dir pts ret va?)
    (: (kls as i gpc xmmc ovc acc)                       ; classify + evaluate, L->R -> (((class idx) pushforms)..) | 'bad
        (? (! (two? as)) (rev acc)
           ; a targetable leaf skips the cell: classified 'ez/'ezm, emitted
           ; straight into its argument register after the spills.
           (: ez (? (gpc < (ngp g)) (ezarg g env <as (nthty pts i) gpc) 'no)
              (? (! (id? ez 'no)) (kls >as (i + 1) (gpc + 1) xmmc ovc (link ez acc))
           ; a DIRECT &x / array / struct-var argument is presumed dead once the
           ; callee returns (the &Hp / &_res idiom): argpos exempts its lea from
           ; the fesc pin, one-shot, cleared right after. the sibs WINDOW rule
           ; still keeps the address out of a tail call's own staging.
           (: a (: _ (? (member? (cap (cap as)) '(addr var)) (pin g 'argpos 1) ())
                   (cgexpr g env <as))
              _ (pin g 'argpos ())
              (? (bad? a) 'bad
                 (: pty (nthty pts i)
                    aty (? pty pty (vty a))                ; classify by the param type if known, else the arg's
                    wd (flo? aty)
                    ; t32 composites: an HFA rides the d-regs (the sse2 pair lane below = AAPCS32-VFP
                    ; d-pairs; an 8B one-sse blob = the packed S-pair, bit-identical); a <=4B int one
                    ; rides r0's word. mixed/int-pair 8..16B stays deferred -- refuse LOUD.
                    (? (&& (t32? g) (&& va? (stru? aty))) 'bad   ; a t32 VARIADIC call rides the base PCS: no d-reg composites -- refuse LOUD
                       (&& (v6m? g) (sse2? g aty))       ; v6-M: a POSITION-0 16B composite rides r0-r3 whole (the
                        (? (gpc = 0)                     ;  base-ABI words); any later start would SPLIT -- deferred
                           (kls >as (i + 1) 4 xmmc ovc
                                (link `(`('g4 0)
                                         ((tor0 g a) +
                                          `(`('sub 'sp 'sp 16) `('ld 'r1 'r0 12) `('st 'sp 12 'r1) `('ld 'r1 'r0 8) `('st 'sp 8 'r1)
                                             `('ld 'r1 'r0 4) `('st 'sp 4 'r1) `('ld 'r1 'r0 0) `('st 'sp 0 'r1))))
                                      acc))
                           'bad)
                       (&& (t32? g) (&& (stru? aty)
                           (! (|| (sse2? g aty)
                                  (|| (&& (id? (aone g aty) 'sse) ((tsz g aty) = 8))
                                      (&& (id? (aone g aty) 'int) ((tsz g aty) <= 4))))))) 'bad
                       (sse2? g aty)                       ; a two-double struct arg: push its pair (re on top), an xmm PAIR target
                        (? (6 < xmmc) 'bad
                           (kls >as (i + 1) gpc (xmmc + 2) ovc
                                (link `(`('x2 xmmc)
                                         ((tor0 g a) +
                                          (? (t32? g)     ; t32: a 16-byte cell, four word copies
                                             `(`('sub 'sp 'sp 16) `('ld 'r1 'r0 12) `('st 'sp 12 'r1) `('ld 'r1 'r0 8) `('st 'sp 8 'r1)
                                                `('ld 'r1 'r0 4) `('st 'sp 4 'r1) `('ld 'r1 'r0 0) `('st 'sp 0 'r1))
                                             (a64? g)     ; arm64 push is a 16-byte cell of its own: lay the pair by hand
                                             `(`('sub 'sp 'sp 16) `('ld 'r1 'r0 8) `('st 'sp 8 'r1) `('ld 'r1 'r0 0) `('st 'sp 0 'r1))
                                             `(`('ld 'r1 'r0 8) `('push 'r1) `('ld 'r1 'r0 0) `('push 'r1)))))
                                      acc)))
                       (&& (stru? aty) (two? (aeb2 g aty)))   ; a 9..16B struct arg (not sse:sse): eb0/eb1 each into a gp or xmm reg, mirrors the param side's file rule
                        (: cls (aeb2 g aty) c0 <cls c1 <>cls
                           hfa (&& (id? c0 'sse) (id? c1 'sse))
                           f0 (? (a64? g) (? hfa 'x 'g) (? (id? c0 'sse) 'x 'g))
                           f1 (? (a64? g) (? hfa 'x 'g) (? (id? c1 'sse) 'x 'g))
                           gpn (+ (? (id? f0 'g) 1 0) (? (id? f1 'g) 1 0))
                           xmn (+ (? (id? f0 'x) 1 0) (? (id? f1 'x) 1 0))
                           (? (|| ((ngp g) < (gpc + gpn)) (8 < (xmmc + xmn))) 'bad   ; register-exhausted 16B struct arg: refuse (the memory-arg lane is unimplemented; rare -- needs 5+ prior int args)
                              (: i0 (? (id? f0 'g) gpc xmmc)
                                 gpc1 (? (id? f0 'g) (gpc + 1) gpc)
                                 xmc1 (? (id? f0 'x) (xmmc + 1) xmmc)
                                 i1 (? (id? f1 'g) gpc1 xmc1)
                                 (kls >as (i + 1) (gpc + gpn) (xmmc + xmn) ovc
                                      (link `(`('p2 f0 i0 f1 i1)
                                               ((tor0 g a) +
                                                (? (a64? g)
                                                   `(`('sub 'sp 'sp 16) `('ld 'r1 'r0 8) `('st 'sp 8 'r1) `('ld 'r1 'r0 0) `('st 'sp 0 'r1))
                                                   `(`('ld 'r1 'r0 8) `('push 'r1) `('ld 'r1 'r0 0) `('push 'r1)))))
                                            acc)))))
                       (&& (stru? aty) (! (aone g aty))) 'bad   ; a MIXED/>16B by-value struct stays ahead
                       (pair? g aty)                             ; a pair arg: an even-odd register pair (AAPCS32
                        (: gpe (& (gpc + 1) -2)                  ;  rounds up), else the stack 8-aligned -- and gp
                           (? ((gpe + 2) <= (ngp g))             ;  assignment closes behind a stack arg (C.13)
                               (kls >as (i + 1) (gpe + 2) xmmc ovc
                                    (link `(`('g2 gpe) ((asll g a) + spush2)) acc))
                               (: ov8 (& (ovc + 7) -8)
                                  (kls >as (i + 1) 4 xmmc (ov8 + 8)
                                       (link `(`('ov ov8 8)
                                                ((asll g a) + `(`('st 'sp ov8 'r0) `('st 'sp (ov8 + 4) 'r1))))
                                             acc)))))
                       ; a one-eightbyte aggregate rides the scalar path: its
                       ; 8 content bytes load into r0 (the value IS an address)
                       ; and class by its eightbyte -- INT a gp arg, SSE an xmm
                       (: one (? (stru? aty) (aone g aty) ())
                          wd2 (|| wd (id? one 'sse))
                          aval (? (&& (t32? g) (id? one 'sse)) ((tor0 g a) + (pld 'r0 0))   ; the t32 8B blob rides the r0:r1 pair
                                  one ((tor0 g a) + `(`('ld 'r0 'r0 0)))
                                  wd ((asflo g a) + `(`('movqrx 'r0 'f0)))
                                  (asint g a))
                          ; a register arg rides a 16-byte cell: rsp stays 16-aligned
                          ; while the REMAINING args evaluate (a later arg may hold a
                          ; nested call, and the SysV ABI wants rsp%16==0 at it). an
                          ; overflow arg keeps its 8-byte push -- the callee reads the
                          ; overflow block packed, and its target is love-internal (never
                          ; a libc movaps-on-rbp path), so a stray eval-time skew is moot.
                          rpush (aval + (? (&& (t32? g) wd2) spush2 (spush 'r0)))   ; a t32 double (or 8B blob) is the r0:r1 pair
                          ; an overflow arg: x64 pushes 8 bytes (deepest first); arm64
                          ; stores into the pre-allocated packed block at its own slot
                          ; (sp sits at the block base whenever an arg evaluates)
                          ; the overflow store: x64 pushes 8 (deepest first); arm64 stores its 8-byte
                          ; slot in the pre-allocated block; t32 the same block with 4-byte slots
                          opush (? (a64? g) (aval + `(`('st 'sp (* 8 ovc) 'r0)))
                                   (t32? g) (aval + `(`('st 'sp ovc 'r0)))
                                   (aval + `(`('push 'r0))))
                          (? (&& (t32? g) (&& va? wd2)) 'bad   ; a t32 VARIADIC call rides the base PCS: no d-reg doubles -- refuse LOUD
                             (&& (v6m? g) (id? aty 'float))    ; v6-M base ABI: a bare float arg is ONE WORD (binary32,
                              (: av1 ((asflo g a)              ;  the gcc shape -- ai_flo_t IS float on a 32-bit love)
                                      + `(`('cvtsd2ss 'f0 'f0) `('movqrx 'r0 'f0)))
                                 (? (gpc < (ngp g))
                                    (kls >as (i + 1) (gpc + 1) xmmc ovc
                                         (link `(`('g gpc) (av1 + (spush 'r0))) acc))
                                    (kls >as (i + 1) gpc xmmc (ovc + 4)
                                         (link `(`('ov ovc 4) (av1 + `(`('st 'sp ovc 'r0)))) acc))))
                             (&& (v6m? g) wd2)             ; v6-M base ABI: a double/8B-blob arg is an even-odd gp
                              (: gpe (& (gpc + 1) -2)      ;  pair (aval already lands r0:r1), else the 8-aligned stack
                                 (? ((gpe + 2) <= (ngp g))
                                     (kls >as (i + 1) (gpe + 2) xmmc ovc
                                          (link `(`('g2 gpe) (aval + spush2)) acc))
                                     (: ov8 (& (ovc + 7) -8)
                                        (kls >as (i + 1) 4 xmmc (ov8 + 8)
                                             (link `(`('ov ov8 8)
                                                      (aval + `(`('st 'sp ov8 'r0) `('st 'sp (ov8 + 4) 'r1))))
                                                   acc)))))
                             (&& wd2 (xmmc < 8)) (kls >as (i + 1) gpc (xmmc + 1) ovc (link `(`('x xmmc) rpush) acc))
                             (&& (t32? g) wd2) 'bad         ; a t32 fp/blob OVERFLOW arg (9+ doubles): the 4-byte slot can't carry it -- refuse LOUD
                             (&& (! wd2) (gpc < (ngp g))) (kls >as (i + 1) (gpc + 1) xmmc ovc (link `(`('g gpc) rpush) acc))
                             (t32? g) (kls >as (i + 1) gpc xmmc (ovc + 4) (link `(`('ov ovc 4) opush) acc))
                             (kls >as (i + 1) gpc xmmc (ovc + 1) (link `(`('ov ovc) opush) acc)))))))))))
       ; v6-M MEMORY return (AAPCS32 base): a >4-byte composite result fills a caller temp
       ; whose address rides the IMPLICIT first argument (r0) -- args start at gpc 1, the
       ; address materializes fresh right before the call and again after it (the result)
       sr6 (&& (v6m? g) (&& (stru? ret) (4 < (tsz g ret))))
       sroff (? sr6 (ntmp g ret) 0)
       _ (pin g 'argseal ((peep g 'argseal 0) + 1))     ; the vmap seals across the staging (see vmon?)
       kr (kls ags 0 (? sr6 1 0) 0 0 ())
       _ (pin g 'argseal ((peep g 'argseal 0) - 1))
       (? (bad? kr) 'bad
          (: ov? (\ kent ((cap (cap kent)) = 'ov))
             ovr (: (go1 l1 acc1) (? (! (two? l1)) acc1
                                     (go1 >l1 (? (ov? <l1) (link <l1 acc1) acc1)))
                    (go1 kr ()))                          ; overflow entries, REVERSED (deepest pushes first)
             rgr (: (go2 l2 acc2) (? (! (two? l2)) acc2
                                     (go2 >l2 (? (member? (cap (cap (cap l2))) '(g g2 g4 x x2 p2))
                                                      (link <l2 acc2) acc2)))
                    (go2 kr ()))                          ; PUSHING register entries, reversed (pops run forward)
             (pushcat l3 acc3) (? (! (two? l3)) acc3 (pushcat >l3 (acc3 + (caup (cap l3)))))
             ; the xmm pops stage through a gp scratch: r0 on x64 (no arg rides
             ; it), a pool register on arm64 (r0 IS arg 0, and no pool value
             ; lives across a call)
             px (psc g)
             pops (: (go4 l4 acc4)
                      (? (! (two? l4)) acc4
                         (: kc (cap (cap l4))
                            (go4 >l4 (? (member? <kc '(ov ez ezm)) acc4
                                             (cat acc4 (? (<kc = 'g) (spop (nthr (argr g) <>kc))
                                                          (<kc = 'g2)   ; the t32 pair: both words off one cell
                                                           `(`('ld (nthr (argr g) <>kc) 'sp 0)
                                                              `('ld (nthr (argr g) (<>kc + 1)) 'sp 4)
                                                              `('add 'sp 'sp 16))
                                                          (<kc = 'g4)   ; the v6-M whole-composite quad: r0-r3 off one cell
                                                           `(`('ld 'r0 'sp 0) `('ld 'r1 'sp 4)
                                                              `('ld 'r2 'sp 8) `('ld 'r3 'sp 12)
                                                              `('add 'sp 'sp 16))
                                                          (&& (t32? g) (<kc = 'x))   ; a t32 double pops STRAIGHT into its d-reg
                                                           `(`('ldsd (xmmreg <>kc) 'sp 0) `('add 'sp 'sp 16))
                                                          (<kc = 'x2)   ; the struct pair: re then im (a natural 16-byte cell)
                                                           (? (t32? g)   ; t32 pops STRAIGHT into the d-pair
                                                              `(`('ldsd (xmmreg <>kc) 'sp 0) `('ldsd (xmmreg (<>kc + 1)) 'sp 8)
                                                                 `('add 'sp 'sp 16))
                                                              (a64? g)
                                                              `(`('ld px 'sp 0) `('movqxr (xmmreg <>kc) px)
                                                                 `('ld px 'sp 8) `('movqxr (xmmreg (<>kc + 1)) px)
                                                                 `('add 'sp 'sp 16))
                                                              `(`('pop 'r0) `('movqxr (xmmreg <>kc) 'r0)
                                                                 `('pop 'r0) `('movqxr (xmmreg (<>kc + 1)) 'r0)))
                                                          (<kc = 'p2)   ; a general two-eightbyte struct: eb0 (top) then eb1, each into its gp/xmm target
                                                           (? (a64? g)
                                                              ((? (id? <>kc 'g) `(`('ld (nthr (argr g) <>>kc) 'sp 0)) `(`('ld px 'sp 0) `('movqxr (xmmreg <>>kc) px)))
                                                               + (? (id? <>>>kc 'g) `(`('ld (nthr (argr g) <>>>>kc) 'sp 8)) `(`('ld px 'sp 8) `('movqxr (xmmreg <>>>>kc) px)))
                                                               + `(`('add 'sp 'sp 16)))
                                                              ((? (id? <>kc 'g) `(`('pop (nthr (argr g) <>>kc))) `(`('pop 'r0) `('movqxr (xmmreg <>>kc) 'r0)))
                                                               + (? (id? <>>>kc 'g) `(`('pop (nthr (argr g) <>>>>kc))) `(`('pop 'r0) `('movqxr (xmmreg <>>>>kc) 'r0)))))
                                                          ((spop px) + `(`('movqxr (xmmreg <>kc) px)))))))))
                     (go4 kr ()))
             nxmm (: (go5 l5 n5) (? (! (two? l5)) n5
                                    (go5 >l5 (: ce (cap (cap l5)) kk <ce
                                                     (? (kk = 'x) (n5 + 1) (kk = 'x2) (n5 + 2)
                                                        (kk = 'p2) (+ n5 (+ (? (id? (nthr ce 1) 'x) 1 0) (? (id? (nthr ce 3) 'x) 1 0)))
                                                        n5))))
                     (go5 kr 0))
             nov (tally ovr)
             padn (? ((& nov 1) = 1) 8 0)                 ; keep sp 16-aligned across the overflow block
             ; t32 slots are BYTE offsets ((ov off sz) entries): the block spans to the highest
             ; slot's end, rounded to 8 (AAPCS32 sp alignment). x64/a64 keep 8-byte slot counts.
             ovblk (? (t32? g)
                      (: (go9 l9 m9) (? (! (two? l9)) (& (m9 + 7) -8)
                                        (: ce (cap (cap l9))
                                           e9 (? (id? <ce 'ov) (+ <>ce (? (two? (cup (cup ce))) <>>ce 4)) 0)
                                           (go9 >l9 (? (m9 < e9) e9 m9))))
                         (go9 kr 0))
                      ((* 8 nov) + padn))
             ; x64 pads then pushes deepest-first; arm64 AND t32 allocate the packed
             ; block up front and the ov forms store into their own slots
             pad (? (arm? g) (? (0 < ovblk) `(`('sub 'sp 'sp ovblk)) ())
                    (? (0 < padn) `(`('sub 'sp 'sp padn)) ()))
             clean (? (0 < ovblk) `(`('add 'sp 'sp ovblk)) ())
             pf (pad + (pushcat ovr ()) + (pushcat rgr ()))
             ; al = #xmm, set AFTER pops (r0 is no x64 arg register); AAPCS64
             ; passes variadic args exactly like named ones -- no al, nothing.
             ; AAPCS32 likewise has no al -- and r0 IS arg 0, never clobber it.
             setal (? (&& va? (! (arm? g))) `(`('li 'r0 nxmm)) ())
             ; NON-LEAF register homes (g 'homes, ((reg slot)..)): a home rides a
             ; caller-saved register, so it spills to its frame slot just before
             ; the pops (the args' own evaluation -- and any assignment inside it
             ; -- has already run, so the register holds the latest value) and
             ; reloads right after the clean. a ret-position call's reloads are
             ; DEAD -- the sibcall pass skips them and tail-jumps (skiprel).
             ; ...filtered to this statement's wrapset (g 'stwrap, the alive
             ; pass): a home DEAD across this call keeps its register -- no
             ; spill, no reload. 'all (no liveness ran) wraps every home.
             hs (: h9 (peep g 'homes ()) w9 (peep g 'stwrap 'all)
                   (? (id? w9 'all) h9 (filter (\ h2 (member? <>>h2 w9)) h9)))
             spills (foldl (\ a2 p2 (a2 + `(`('st 'r4 <>p2 <p2)))) () hs)
             relods (foldl (\ a2 p2 (a2 + `(`('ld <p2 'r4 <>p2)))) () hs)
             ; the targeted emissions, after the spills and before the pops: an
             ; 'ezm home mov demotes to its spill-slot load when ANOTHER targeted
             ; arg writes that home (a self-mov stays -- the fold drops it).
             ezt (: (go6 l6 a6) (? (! (two? l6)) a6
                                   (go6 >l6 (: kc (cap (cap l6))
                                                    (? (member? <kc '(ez ezm))
                                                       (link (nthr (argr g) <>kc) a6) a6))))
                    (go6 kr ()))
             ezf (: (go7 l7 a7) (? (! (two? l7)) a7
                                   (go7 >l7
                                        (: kc (cap (cap l7))
                                           (? (<kc = 'ez) (a7 + (caup (cap l7)))
                                              (! (<kc = 'ezm)) a7
                                              (: rH <>>kc
                                                 (? (&& (member? rH ezt) (! (id? rH (nthr (argr g) <>kc))))
                                                    (a7 + (cauup (cap l7)))
                                                    (a7 + (caup (cap l7)))))))))
                    (go7 kr ()))
             (? dir
                 (: _ (vmflush g)                          ; the call clobbers every pool reg
                    ; v6-M base ABI: a double result arrives in r0:r1, a bare float as one word
                    ; (binary32 in r0 -- widen it) -- either lands on the f0 fiction
                    fres (? (! (&& (v6m? g) (flo? ret))) ()
                            (id? ret 'float) `(`('movqxr 'f0 'r0) `('cvtss2sd 'f0 'f0))
                            `(`('movqxr 'f0 'r0)))
                    srlea (? sr6 `(`('lea 'r0 'r4 sroff)) ())   ; the sret address in, then back out as the result
                    (mkv ret 'r0 (pf + spills + ezf + pops + setal + srlea + `(`('call (intern <>hd))) + fres + srlea + clean + relods)))
                (: hr (cgexpr g env hd)                   ; a function-pointer value: park (x64 rbx; arm64 x15), callr
                   pk (pkr g)
                   _ (vmflush g)                          ; the call clobbers every pool reg
                   fres (? (! (&& (v6m? g) (flo? (fnret (vty hr))))) ()
                           (id? (fnret (vty hr)) 'float) `(`('movqxr 'f0 'r0) `('cvtss2sd 'f0 'f0))
                           `(`('movqxr 'f0 'r0)))
                   (? (bad? hr) 'bad
                      (&& (v6m? g) (&& (stru? (fnret (vty hr)))   ; a MEMORY-returning callee behind a POINTER: the
                          (4 < (tsz g (fnret (vty hr)))))) 'bad   ; implicit-arg shift wasn't staged -- refuse LOUD
                      ; t32: r12 is ALSO the far-mem/pair scratch, so a register park dies to any
                      ; far frame offset in the ez emissions -- park in a frame slot instead and
                      ; reload r12 as the LAST thing before the call (the far lane loading it is
                      ; itself r12-into-r12, safe). the sib rewrite keeps the ld: the epilogue
                      ; never touches r12.
                      (t32? g)
                       (: pko (ntmp g 'long)
                          (mkv (fnret (vty hr)) 'r0 (cat pf (cat (tor0 g hr) (cat `(`('st 'r4 pko 'r0))
                                    (spills + ezf + pops + setal + `(`('ld pk 'r4 pko) `('callr pk)) + fres + clean + relods))))))
                      (mkv (fnret (vty hr)) 'r0 (cat pf (cat (tor0 g hr) (cat `(`('mov pk 'r0))
                                (spills + ezf + pops + setal + `(`('callr pk)) + clean + relods)))))))))))
   (cgbin g env e wnt)
    (: op <>e
       lhs <>>e rhs <>>>e
       ; LAZY left-then-right, like cbranch: the left PARKS (allocates) before
       ; the right evaluates. a constant side rides the immediate lane with NO
       ; right evaluation; two register operands combine 3-address; pointer
       ; scaling, divide, a variable shift, and a call on the right keep the
       ; memory shuttle (shut).
       a (cgexpr g env lhs)
       (? (bad? a) 'bad
          ; a constant count rides the immediate shift; a variable count
          ; rides shlv/sarv with the count PINNED to r1 (holo's contract)
          (&& (member? op '(shl shr)) (pair? g (vty a)))    ; a pair shift keeps the left type
           (: kind (? (op = 'shl) 'shl (usg? (vty a)) 'shr 'sar)
              (? (<rhs = 'num)
                  (mkv (vty a) 'r0 ((tor0 g a) + (pshiftc kind <>rhs)))
                 (: b (cgexpr g env rhs)
                    (? (bad? b) 'bad
                       (mkv (vty a) 'r0 (cat (tor0 g a)
                              (cat spush2
                               (cat (tor0 g b) (cat `(`('mov 'r2 'r0)) (cat (spop2 'r0 'r1)
                                                (pshiftv g kind)))))))))))
          (member? op '(shl shr))
           (: uL (usg? (vty a))                             ; a `>>` on an unsigned left operand is LOGICAL (shr, not sar)
              rt (? uL 'ulong 'long)                        ; the shift result keeps the left operand's signedness
              iop (? (op = 'shl) 'shl (? uL 'shr 'sar))
              vop (? (op = 'shl) 'shlv (? uL 'shrv 'sarv))
              (? (<rhs = 'num)
                  (? (wr? g (vreg a))
                     (mkv rt (vreg a) ((vfs a) + `(`(iop (vreg a) <>rhs))))
                     (: rd (? wnt wnt (rdst g (vreg a)))    ; a borrowed home shifts a COPY (no 3-address shift)
                        (mkv rt rd ((vfs a) + `(`('mov rd (vreg a)) `(iop rd <>rhs))))))
                 (: b (cgexpr g env rhs)
                    (? (bad? b) 'bad
                       (mkv rt 'r0 (cat (tor0 g a)
                              (cat (spush 'r0)
                               (cat (tor0 g b) (cat `(`('mov 'r1 'r0)) (cat (spop 'r0)
                                                `(`(vop 'r0 'r1))))))))))))
          (: ta (vty a)
             ; a constant right rides the immediate lane; a constant LEFT swaps
             ; over (commutative ops) or mirrors (relations, rflip) onto the right
             imr (? (&& (! (flo? ta)) (<rhs = 'num)) (immop g op a ta <>rhs wnt) 'no)
             (? (! (id? imr 'no)) imr
                (: iml? (&& (! (flo? ta)) (&& (<lhs = 'num)
                            (member? op '(+ * & | ^ eq ne lt le gt ge))))
                   ah (? (|| iml? (|| (flo? ta) (|| (callish? rhs) (llt? ta)))) 'sh
                         (vpark g env lhs a))       ; a pair lhs keeps the shuttle (no pool park)
                   b (cgexpr g env rhs)
                   (? (bad? b) 'bad
                      (: tb (vty b)
                         av (? (id? ah 'sh) a ah)
                         ; the memory shuttle: left in r1, right in r0 (today's shape)
                         (shut z9)
                          (: mid ((tor0 g av) + (spush 'r0) + (tor0 g b) + (spop 'r1))
                             back `(`('mov 'r0 'r1))
                             pa (ptr? ta) pb (ptr? tb)
                             unsg (|| (usg? ta) (usg? tb))            ; either operand unsigned -> unsigned divide
                             cunsg (|| unsg (|| pa pb))               ; a POINTER relational compares UNSIGNED too (C: addresses are unsigned; matters where a pointer sets bit 63)
                             (? (&& (t32? g) (&& (! (|| pa pb)) (|| (llt? ta) (llt? tb))))
                                 ; the PAIR shuttle: both sides widen to r0:r1, left pops to r2:r3;
                                 ; +,-,* and the logicals combine 3-address through the carry family,
                                 ; unsigned / and % ride the backend's restoring expansion, relations
                                 ; ride subs/sbcs (prel). signed pair divide stays refused.
                                 (: mid2 ((asll g av) + spush2 + (asll g b) + (spop2 'r2 'r3))
                                    ; v6-M divide staging: numerator (the LEFT) lands r0:r1, denominator r2:r3
                                    ; (the __aeabi_(u)ldivmod protocol); mov is the hi-form, flag-silent
                                    mid2d ((asll g av) + spush2 + (asll g b)
                                           + `(`('mov 'r2 'r0) `('mov 'r3 'r1)) + (spop2 'r0 'r1))
                                    rt (ubin ta tb)
                                    (? (op = '+) (mkv rt 'r0 (mid2 + `(`('adds 'r0 'r2 'r0) `('adc 'r1 'r3 'r1))))
                                       (op = '-) (mkv rt 'r0 (mid2 + `(`('subs 'r0 'r2 'r0) `('sbc 'r1 'r3 'r1))))
                                       (op = '*)
                                        (? (v6m? g)   ; no UMULL/MLA on v6-M: libgcc's exact helper (commutative, either staging)
                                           (mkv rt 'r0 (mid2 + `(`('call '__aeabi_lmul))))
                                           (mkv rt 'r0 (mid2 + `(`('imul 'r3 'r3 'r0) `('mla 'r3 'r2 'r1 'r3)
                                                                 `('umull 'r0 'r12 'r2 'r0) `('add 'r1 'r12 'r3)))))
                                       (op = '&) (mkv rt 'r0 (mid2 + `(`('and 'r0 'r2 'r0) `('and 'r1 'r3 'r1))))
                                       (op = '|) (mkv rt 'r0 (mid2 + `(`('or 'r0 'r2 'r0) `('or 'r1 'r3 'r1))))
                                       (op = '^) (mkv rt 'r0 (mid2 + `(`('xor 'r0 'r2 'r0) `('xor 'r1 'r3 'r1))))
                                       (op = '/)
                                        (? (v6m? g)   ; quotient rides out in r0:r1; the SIGNED face works too (__aeabi_ldivmod)
                                           (mkv rt 'r0 (mid2d + `(`('call (? unsg '__aeabi_uldivmod '__aeabi_ldivmod)))))
                                           unsg (mkv rt 'r0 (mid2 + `(`('udivll)))) 'bad)
                                       (op = '%)
                                        (? (v6m? g)   ; the remainder rides r2:r3 -> the protocol pair
                                           (mkv rt 'r0 (mid2d + `(`('call (? unsg '__aeabi_uldivmod '__aeabi_ldivmod))
                                                                  `('mov 'r0 'r2) `('mov 'r1 'r3))))
                                           unsg (mkv rt 'r0 (mid2 + `(`('uremll)))) 'bad)
                                       (member? op '(eq ne lt le gt ge))
                                        (: cf (prel g op unsg)
                                           (mkv 'long 'r0 (mid2 + <>cf + `(`('set <cf 'r0)))))
                                       'bad))
                                (op = '+)
                                 ; ptr + idx at a machine scale is ONE leax (x64 a SIB lea,
                                 ; a64 an add-shifted) -- the imul/add pair only for odd sizes
                                 (? (&& pa pb) 'bad
                                    pa (: sz (tsz g <>ta)
                                          (? (member? sz '(1 2 4 8))
                                             (mkv ta 'r0 (cat mid `(`('leax 'r0 'r1 'r0 sz 0))))
                                             (mkv ta 'r0 (cat mid (cat (? (1 < sz) (imby g 'r0 sz) ())
                                                      (`(`('add 'r1 'r1 'r0)) + back))))))
                                    pb (: sz (tsz g <>tb)
                                          (? (member? sz '(1 2 4 8))
                                             (mkv tb 'r0 (cat mid `(`('leax 'r0 'r0 'r1 sz 0))))
                                             (mkv tb 'r0 (cat mid (cat (? (1 < sz) (imby g 'r1 sz) ())
                                                      (`(`('add 'r1 'r1 'r0)) + back))))))
                                    (mkv 'long 'r0 (mid + `(`('add 'r1 'r1 'r0)) + back)))
                                (op = '-)
                                 (? (&& pa pb)
                                     (? ((tsz g <>ta) = (tsz g <>tb))
                                        ; a pointer difference divides the byte gap by the pointee size.
                                        ; mid leaves left in r1, right in r0; `sub r1 r1 r0` makes r1 the
                                        ; gap. a power-of-two size shifts (move to r0, sar); any other
                                        ; size takes a real signed divide (the / lane's shape: dividend
                                        ; r1, divisor r0, result r1 -> r0). size 1 is just the move.
                                        (: esz (tsz g <>ta)
                                           (mkv 'long 'r0 (cat mid (cat `(`('sub 'r1 'r1 'r0))
                                                            (? (esz <= 1) back
                                                               (pow2? esz) (back + `(`('sar 'r0 (lg2 esz))))
                                                               `(`('li 'r0 esz) `('div 'r1 'r1 'r0) `('mov 'r0 'r1)))))))
                                        'bad)
                                    pa (mkv ta 'r0 (cat mid (cat (? (1 < (tsz g <>ta))
                                                             (imby g 'r0 (tsz g <>ta)) ())
                                                      (`(`('sub 'r1 'r1 'r0)) + back))))
                                    pb 'bad
                                    (mkv 'long 'r0 (mid + `(`('sub 'r1 'r1 'r0)) + back)))
                                (: tail (? (op = '*) `(`('imul 'r1 'r1 'r0))
                                           (op = '/) `(`((? unsg 'udiv 'div) 'r1 'r1 'r0))
                                           (op = '%) `(`((? unsg 'urem 'rem) 'r1 'r1 'r0 'r3))
                                           (op = '&) `(`('and 'r1 'r1 'r0))
                                           (op = '|) `(`('or 'r1 'r1 'r0))
                                           (op = '^) `(`('xor 'r1 'r1 'r0))
                                           'bad)
                                   (? (! (bad? tail))
                                      (mkv (ubin ta tb) 'r0 (mid + tail + back))
                                      (member? op '(eq ne lt le gt ge))
                                       (mkv 'long 'r0 (mid + `(`('cmp 'r1 'r0) `('set (scc op cunsg) 'r0))))
                                      'bad))))
                         iml (? (&& iml? (! (flo? tb))) (immop g (rflip op) b tb <>lhs wnt) 'no)
                         (? (! (id? iml 'no)) iml
                            (|| (flo? ta) (flo? tb))
                             ; the double lane: either operand double -> both to double (f1=left,
                             ; f0=right through the gp stack), then the scalar-double op or ucomisd.
                             (: mid (? (t32? g)               ; the gp stack moves PAIRS on t32
                                       (cat (asflo g av) (cat (`(`('movqrx 'r0 'f0)) + spush2)
                                            ((asflo g b) + (spop2 'r2 'r3) + `(`('movqxr 'f1 'r2)))))
                                       (cat (asflo g av) (cat (`(`('movqrx 'r0 'f0)) + (spush 'r0))
                                            ((asflo g b) + (spop 'r1) + `(`('movqxr 'f1 'r1))))))
                                (? (op = '+) (mkv 'double 'r0 (mid + `(`('addsd 'f0 'f1))))
                                   (op = '*) (mkv 'double 'r0 (mid + `(`('mulsd 'f0 'f1))))
                                   (op = '-) (mkv 'double 'r0 (mid + `(`('subsd 'f1 'f0) `('movsd 'f0 'f1))))
                                   (op = '/) (mkv 'double 'r0 (mid + `(`('divsd 'f1 'f0) `('movsd 'f0 'f1))))
                                   ; IEEE compare: UNORDERED (a NaN operand) must answer false for
                                   ; every relation and true only for !=. x64: above/ae read CF=0,
                                   ; which unordered sets, so < and <= COMPARE SWAPPED and ride
                                   ; above/ae; == and != carry the parity bit (unordered -> PF=1)
                                   ; explicitly. arm64: FCMP's gt/ge are unordered-false natively
                                   ; (HI/HS would read C, which unordered SETS -- true, wrong), and
                                   ; p/np alias vs/vc in the backend, so eq/ne ride whole.
                                   (member? op '(lt le))
                                    (mkv 'long 'r0 (cat mid `(`('ucomisd 'f0 'f1)
                                                               `('set (? (arm? g) (? (op = 'lt) 'gt 'ge)
                                                                         (? (op = 'lt) 'above 'ae)) 'r0))))
                                   (member? op '(gt ge))
                                    (mkv 'long 'r0 (cat mid `(`('ucomisd 'f1 'f0)
                                                               `('set (? (arm? g) (? (op = 'gt) 'gt 'ge)
                                                                         (? (op = 'gt) 'above 'ae)) 'r0))))
                                   (op = 'eq)
                                    (mkv 'long 'r0 (cat mid `(`('ucomisd 'f1 'f0) `('set 'eq 'r0)
                                                               `('set 'np 'r1) `('and 'r0 'r0 'r1))))
                                   (op = 'ne)
                                    (mkv 'long 'r0 (cat mid `(`('ucomisd 'f1 'f0) `('set 'ne 'r0)
                                                               `('set 'p 'r1) `('or 'r0 'r0 'r1))))
                                   'bad))
                            ; the REGISTER lane: both values live, 3-address combine
                            (: pa (ptr? ta) pb (ptr? tb)
                               ; a live ptr + live idx at a machine scale: ONE leax, no
                               ; shuttle (lea reads both sources before writing -- any alias)
                               lxsz (? (! (op = '+)) () (&& pa pb) ()
                                       pa (: s9 (tsz g <>ta) (? (member? s9 '(1 2 4 8)) s9 ()))
                                       pb (: s9 (tsz g <>tb) (? (member? s9 '(1 2 4 8)) s9 ()))
                                       ())
                               (? (id? ah 'sh) (shut ())
                                  (&& (|| pa pb) (! lxsz)) (shut ())
                                  (&& (t32? g) (llt? tb)) (shut ())   ; a pair right: the pair shuttle (widen the park)
                                  lxsz
                                   (: rA (vreg ah) rB (vreg b)
                                      rd (? wnt wnt (rdst g rA))
                                      fs ((vfs ah) + (vfs b)
                                          + `(`('leax rd (? pa rA rB) (? pa rB rA) lxsz 0)))
                                      _ (rfree g rB) _ (? (id? rd rA) () (rfree g rA))
                                      (mkv (? pa ta tb) rd fs))
                                  (: rA (vreg ah) rB (vreg b)
                                     cunsg (|| (|| (usg? ta) (usg? tb)) (|| pa pb))
                                     pre ((vfs ah) + (vfs b))
                                     (? (member? op '(eq ne lt le gt ge))
                                         (: rd (? wnt wnt (rdst g rA))   ; set runs after the cmp: rd may alias rB
                                            fs (pre + `(`('cmp rA rB) `('set (scc op cunsg) rd)))
                                            _ (rfree g rB) _ (? (id? rd rA) () (rfree g rA))
                                            (mkv 'long rd fs))
                                        (member? op '(+ - * & | ^))
                                         (: iop2 (? (op = '+) 'add (op = '-) 'sub (op = '*) 'imul
                                                    (op = '&) 'and (op = '|) 'or 'xor)
                                            rd (? (&& wnt (! (id? wnt rB))) wnt (rdst g rA))
                                            ; holo reorders a commutative rd==rb; sub CANNOT (alias-dst) --
                                            ; a borrowed left + dry pool can land rd on rB's r0
                                            (? (&& (op = '-) (id? rd rB)) (shut ())
                                               (: fs (pre + `(`(iop2 rd rA rB)))
                                                  _ (rfree g rB) _ (? (id? rd rA) () (rfree g rA))
                                                  (mkv (ubin ta tb) rd fs))))
                                        (shut ())))))))))))))

   ; r3 (rbx) is CALLEE-SAVED under SysV and gen leans on it (callr park, the
   ; overflow builtins, va_arg's walk) -- an optimized gcc caller keeps LIVE
   ; LOCALS in rbx across our calls (the love boot died in main's argv fold: its
   ; loop bound rode ebx straight through gxr). every fn owns frame slot -8 for
   ; it: the prologue stores, this shared epilogue reloads.
   epi `(`('ld 'r3 'r4 -8) `('mov 'sp 'r4) `('pop 'r4) `('ret))
   ; the arm64 twin: bl clobbered lr, so the prologue pushed it (its own
   ; 16-byte cell) before the frame register; x3 is CALLER-saved there, so the
   ; rbx slot vanishes. `mov` to/from sp is ORR (reads/writes XZR at 31) --
   ; the sp hop rides lea (add-immediate) instead. cgstmt/build read the
   ; target's epilogue off g 'epi (pinned by cgen-core).
   epi-a64 `(`('lea 'sp 'fp 0) `('pop 'fp) `('pop 'lr) `('ret))
   ; the thumb2 VARARG twin: the prologue pushed r0-r3 under fp/lr (contiguous
   ; with the caller's stack args), so the epilogue drops that 16-byte block
   ; after restoring fp/lr and before the return
   epi-t32va `(`('lea 'sp 'fp 0) `('pop 'fp) `('pop 'lr) `('add 'sp 'sp 16) `('ret))
   ; -- the SOFT-DOUBLE pass (sp64: an SP-only FPU -- the playdate's STM32F746).
   ; fpv4/5-SP keeps the 64-bit TRANSFERS (vldr/vstr.64, the core<->D vmov pair
   ; moves), so ldsd/stsd/movqxr/movqrx ride the hardware and the double value
   ; model keeps its d-regs -- but every f64 ARITHMETIC/compare/convert is
   ; UNDEFINED there and lowers to its __aeabi_* libgcc call, the exact helpers
   ; gcc emits at -mfpu=*-sp-*, so a mooncc double and a gcc double agree to
   ; the BIT. every call blankets r0-r3 through a 32-byte sp cell (slots 0..12
   ; the saves, 16 the float bounce) -- a gp PAIR can be live across these ops
   ; (cvtlld holds its lo word over the first convert), and the cell keeps the
   ; pass sound with no liveness reasoning; ABOVE sp always (an IRQ frame owns
   ; the space below). d-regs need no saving: moon stages doubles only through
   ; f0/f1 (both consumed operands) + the f15 narrow scratch, never d2..d7.
   ; a compare's consumers are the FUSED next forms (br cc / set cc / the
   ; eq-np ordered dance) -- rewritten as a unit, each VCMP-flavored cc mapped
   ; onto a boolean helper + sense: plain loads restore r0-r3 after the cmp
   ; without touching the flags, so the branch/set reads them across the
   ; restore. anything unrecognized after a compare scares LOUD.
   sf-bin (: t (tablet 0)
             _ (pin t 'addsd '__aeabi_dadd) _ (pin t 'subsd '__aeabi_dsub)
             _ (pin t 'mulsd '__aeabi_dmul) _ (pin t 'divsd '__aeabi_ddiv)
             t)
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
   sf-save `(`('sub 'sp 'sp 32) `('st 'sp 0 'r0) `('st 'sp 4 'r1) `('st 'sp 8 'r2) `('st 'sp 12 'r3))
   sf-rest `(`('ld 'r0 'sp 0) `('ld 'r1 'sp 4) `('ld 'r2 'sp 8) `('ld 'r3 'sp 12) `('add 'sp 'sp 32))
   (sf-slot r) (? (id? r 'r0) 0 (id? r 'r1) 4 (id? r 'r2) 8 (id? r 'r3) 12 -1)
   ; the v6-M SOFT-DOUBLE pass: no FPU registers AT ALL (the M0+/RP2040 face). the f0/f1/f15
   ; fiction rewrites onto the three frame cells cgfn pinned (fc0/fc1/fc15 -- r4-negative ir1
   ; slots; t16slots turns them positive later). every f64 arithmetic/compare/convert lowers
   ; to its __aeabi_* call (sp64's exact helpers and fused-compare dance); the TRANSFERS --
   ; no vldr/vstr/vmov exists -- become word loads/stores through a saved low temp (r3, or r2
   ; when the base IS r3; push/pop and plain mem ops never touch flags, so a transfer between
   ; a cmp and its consumer stays sound). an sp-based transfer offsets across the temp's
   ; 16-byte save cell; the cells mirror register semantics 1:1 WITHIN a function -- every
   ; cross-function f-reg convention (args/params/returns/results) was re-routed onto gp
   ; pairs upstream (the base ABI), so no cell ever crosses a frame. residue scares LOUD.
   (fcell g f) (? (id? f 'f0) (peep g 'fc0 0) (id? f 'f1) (peep g 'fc1 0)
                  (id? f 'f15) (peep g 'fc15 0) (scare 'soften6-freg f))
   (nxr r) (? (id? r 'r0) 'r1 (id? r 'r1) 'r2 (id? r 'r2) 'r3 (scare 'soften6-pair r))
   (pl6 g f lo) (: c (fcell g f) `(`('ld lo 'r4 c) `('ld (nxr lo) 'r4 (c + 4))))   ; cell -> gp pair
   (ps6 g f lo) (: c (fcell g f) `(`('st 'r4 c lo) `('st 'r4 (c + 4) (nxr lo))))   ; gp pair -> cell
   (m2c g f base off)                                ; [base+off] -> cell (a softened ldsd)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `(`('sub 'sp 'sp 16) `('st 'sp 0 tmp)
          `('ld tmp base o2) `('st 'r4 c tmp) `('ld tmp base (o2 + 4)) `('st 'r4 (c + 4) tmp)
          `('ld tmp 'sp 0) `('add 'sp 'sp 16)))
   (c2m g base off f)                                ; cell -> [base+off] (a softened stsd)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `(`('sub 'sp 'sp 16) `('st 'sp 0 tmp)
          `('ld tmp 'r4 c) `('st base o2 tmp) `('ld tmp 'r4 (c + 4)) `('st base (o2 + 4) tmp)
          `('ld tmp 'sp 0) `('add 'sp 'sp 16)))
   (w2c g f base off)                                ; [base+off] -> cell LOW word (a softened ldss)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `(`('sub 'sp 'sp 16) `('st 'sp 0 tmp)
          `('ld tmp base o2) `('st 'r4 c tmp)
          `('ld tmp 'sp 0) `('add 'sp 'sp 16)))
   (c2w g base off f)                                ; cell LOW word -> [base+off] (a softened stss)
    (: c (fcell g f) tmp (? (id? base 'r3) 'r2 'r3) o2 (? (id? base 'sp) (off + 16) off)
       `(`('sub 'sp 'sp 16) `('st 'sp 0 tmp)
          `('ld tmp 'r4 c) `('st base o2 tmp)
          `('ld tmp 'sp 0) `('add 'sp 'sp 16)))
   ; ONE walker serves both soft-double faces; the TRANSFER KIT is the whole
   ; difference. v6m: a double lives in its frame cell -- gd/pd ride pl6/ps6,
   ; the single-word bits read/write the cell low directly, movsd copies
   ; cell-to-cell through the saved temp, and the raw f-memory ops (ldsd &c,
   ; no vldr exists) soften too. sp64: a double lives in its d-register --
   ; gd/pd are movqrx/movqxr, the single-word bits bounce through the save
   ; cell's spare word, movsd bounces the pair (no VMOV.F64 on SP), and
   ; f-memory ops stay (the FPU loads/stores fine, it just can't ARITH f64).
   (soften g fs)
    (: w6 (v6m? g)
       (? (! (|| w6 (sp64? g))) fs
          (: (gd d9 lo) (? w6 (pl6 g d9 lo) `(`('movqrx lo d9)))   ; double -> the gp pair at lo
             (pd d9) (? w6 (ps6 g d9 'r0) `(`('movqxr d9 'r0)))   ; the r0:r1 pair -> double
             (gs d9) (? w6 `(`('ld 'r0 'r4 (fcell g d9)))         ; SINGLE bits -> r0
                        `(`('stss 'sp 16 d9) `('ld 'r0 'sp 16)))
             (ps d9) (? w6 `(`('st 'r4 (fcell g d9) 'r0))         ; r0 -> the single's home
                        `(`('st 'sp 16 'r0) `('ldss d9 'sp 16)))
             (mv dD dS)                                           ; movsd: double -> double
              (? w6 (: cd (fcell g dD) cs (fcell g dS)
                       `(`('sub 'sp 'sp 16) `('st 'sp 0 'r3)
                         `('ld 'r3 'r4 cs) `('st 'r4 cd 'r3)
                         `('ld 'r3 'r4 (cs + 4)) `('st 'r4 (cd + 4) 'r3)
                         `('ld 'r3 'sp 0) `('add 'sp 'sp 16)))
                 `(`('sub 'sp 'sp 16) `('stsd 'sp 0 dS) `('ldsd dD 'sp 0) `('add 'sp 'sp 16)))
             scc (? w6 'soften6-cc 'soften-cc)
             suc (? w6 'soften6-ucomisd 'soften-ucomisd)
             (go l)
              (? (! (two? l)) ()
                 (: f <l r >l
                    (? (! (two? f)) (link f (go r))
                       (: op <f
                          h9 (peep sf-bin op ())
                          (? h9                             ; (op fD fS): fD = fD op fS
                              (cat sf-save
                               (cat (gd <>f 'r0)
                                (cat (gd <>>f 'r2)
                                 (cat `(`('call h9))
                                  (cat (pd <>f)
                                   (cat sf-rest (go r)))))))
                             (op = 'movsd) (cat (mv <>f <>>f) (go r))
                             (&& w6 (op = 'ldsd)) (cat (m2c g <>f <>>f <>>>f) (go r))
                             (&& w6 (op = 'stsd)) (cat (c2m g <>f <>>f <>>>f) (go r))
                             (&& w6 (op = 'ldss)) (cat (w2c g <>f <>>f <>>>f) (go r))
                             (&& w6 (op = 'stss)) (cat (c2w g <>f <>>f <>>>f) (go r))
                             (&& w6 (op = 'movqrx)) (cat (pl6 g <>>f <>f) (go r))
                             (&& w6 (op = 'movqxr)) (cat (ps6 g <>f <>>f) (go r))
                             (member? op '(cvtsi2sd cvtui2sd))   ; (op fD rS)
                              (: sl (sf-slot <>>f)
                                 (cat sf-save
                                  (cat (? (sl < 0) `(`('mov 'r0 <>>f)) `(`('ld 'r0 'sp sl)))
                                   (cat `(`('call (? (op = 'cvtsi2sd) '__aeabi_i2d '__aeabi_ui2d)))
                                    (cat (pd <>f)
                                     (cat sf-rest (go r)))))))
                             (member? op '(cvttsd2si cvttsd2ui))   ; (op rD fS)
                              (: sl (sf-slot <>f)
                                 (cat sf-save
                                  (cat (gd <>>f 'r0)
                                   (cat `(`('call (? (op = 'cvttsd2si) '__aeabi_d2iz '__aeabi_d2uiz)))
                                    (cat (? (sl < 0) `(`('mov <>f 'r0)) `(`('st 'sp sl 'r0)))   ; through the save slot: the restore lands it
                                     (cat sf-rest (go r)))))))
                             (op = 'cvtss2sd)               ; (op fD fS): single bits in, a pair out
                              (cat sf-save
                               (cat (gs <>>f)
                                (cat `(`('call '__aeabi_f2d))
                                 (cat (pd <>f)
                                  (cat sf-rest (go r))))))
                             (op = 'cvtsd2ss)               ; (op fD fS): a pair in, the narrowed word home
                              (cat sf-save
                               (cat (gd <>>f 'r0)
                                (cat `(`('call '__aeabi_d2f))
                                 (cat (ps <>f)
                                  (cat sf-rest (go r))))))
                             (op = 'ucomisd)
                              (: args (cat sf-save (cat (gd <>f 'r0) (gd <>>f 'r2)))
                                 n1 (? (two? r) <r ())
                                 (? (&& (two? n1) (id? <n1 'br))
                                     (: hs (sf-cc <>n1)
                                        _ (? (two? hs) 0 (scare scc n1))
                                        (cat (args + `(`('call <hs) `('cmp 'r0 0)) + sf-rest + `(`('br <>hs <>>n1)))
                                             (go >r)))
                                    (&& (two? n1) (id? <n1 'set))
                                     (: n2 (? (two? >r) <>r ())
                                        n3 (? (&& (two? >r) (two? (cup (cup r)))) <>>r ())
                                        dance (&& (two? n2) (&& (id? <n2 'set) (&& (member? <>n2 '(p np))
                                                                (&& (two? n3) (member? <n3 '(and or))))))
                                        hs (sf-cc <>n1)
                                        _ (? (two? hs) 0 (scare scc n1))
                                        (cat (args + `(`('call <hs) `('cmp 'r0 0)) + sf-rest + `(`('set <>hs <>>n1)))
                                             (go (? dance (cup (cup (cup r))) >r))))
                                    (scare suc n1)))
                             (link f (go r)))))))
             (go fs))))
   ; the v6-M twin: ARMv6-M POP restores r0..r7 and PC but NOT LR, so the return folds into
   ; `pop {pc}` (the pushed lr lands straight in pc) -- no separate lr-restore + bx. fp is r7
   ; here (the backend numbers it). r4 is the BOTTOM base (see t16slots): pushed just under fp,
   ; popped just before it. mov sp,fp is framesize-independent, so this pins as g 'epi.
   epi-t16 `(`('lea 'sp 'fp 0) `('pop 'r4) `('pop 'fp) `('pop 'pc))
   ; the v6-M VARARG twin: POP reaches r0..r7/pc only (no pop-into-lr, and pop-pc
   ; couldn't drop the r0-r3 block after it) -- so the return address pops into r3
   ; (caller-saved, never a return value: r0/r1 carry those) and BX r3 returns
   ; after the 16-byte block drops. bit0 of the pushed lr is already set (BL set
   ; it), so the BX stays in Thumb state.
   epi-t16va `(`('lea 'sp 'fp 0) `('pop 'r4) `('pop 'fp) `('pop 'r3) `('add 'sp 'sp 16) `('jmpr 'r3))
   ; --- the ISA families: the one place an op's membership is stated. every
   ; IR pass reads these -- the derived base/disp tablets (a4ize/t16slots/
   ; unframe re-address through them), rdsp (the dataflow view), and the
   ; member? sites in the sweeps. a new op joins its family HERE and every
   ; pass follows; an op off every list stays opaque (the safe default). ---
   ldops9 '(ld ld1 ld2 ld4 ldu1 ldu2 ldu4)          ; the gp loads (full + narrow)
   gstops '(st st1 st2 st4)                         ; the gp stores
   fldops '(ldsd ldss) fstops '(stsd stss)          ; the f-memory pair
   stops9 (gstops + fstops)                         ; every plain store (gp head first)
   leaops '(lea lean)                               ; the address makes (lean: consumed by its own use)
   xops '(leax ldx ldxb)                            ; the indexed forms (base index scale disp)
   stxops '(stx stxb)                               ; the indexed stores
   aluops '(add sub and or xor imul)
   divops '(div rem udiv urem)                      ; the division protocol (r0..r3 clobber)
   vshops '(shlv shrv sarv)                         ; the variable shifts (count via r1)
   unops '(neg not sx1 sx2 sx4 zx1 zx2 zx4)         ; one-register rewrites
   xmmops '(addsd subsd mulsd divsd movsd ucomisd cvtss2sd cvtsd2ss)   ; xmm-only: no gp effect
   barops '(call callr raw sys)                     ; the opaque barriers
   ; the arm64 RETARGET: gen speaks r4 for the frame base throughout, but x4
   ; is ALSO an AAPCS argument register -- so the rename is by POSITION, not
   ; by symbol: only the operand slot where r4 means "the frame base" (a
   ; memory op's base, a lea's base) becomes fp. an r4 anywhere else IS the
   ; 5th argument (a spill's source, a pop's target) and stays. the arm64
   ; prologue/epilogue say fp outright.
   a4base (: t (tablet 0)
             _ (foldl (\ _ o (pin t o 2)) 0 (ldops9 + fldops + xops + leaops))
             _ (foldl (\ _ o (pin t o 1)) 0 (stops9 + stxops))
             t)
   (a4ize fs)
    (: (swap f i bp)
        (? (! (two? f)) ()
           (link (? (&& (i = bp) (id? <f 'r4)) 'fp <f)
                 (swap >f (i + 1) bp)))
       (go l)
        (? (! (two? l)) ()
           (: f <l
              bp (? (two? f) (peep a4base <f -1) -1)
              (link (? (bp < 0) f (swap f 0 bp))
                    (go >l))))
       (go fs))
   ; v6-M slot retarget: slots live at fp-NEGATIVE offsets (the shared x64/arm model), but ARMv6-M
   ; loads/stores encode only POSITIVE immediates. slots must also stay STABLE across the sub/add-sp
   ; shuffle moon wraps around calls (an sp-relative slot would read the wrong cell mid-staging), so
   ; fp CAN'T serve: fp stays at the frame TOP for the framesize-independent epilogue (mov sp,fp). the
   ; v6m frame therefore reserves a SECOND base, r4, planted at the frame BOTTOM (stable, never moves
   ; during staging). this pass rewrites each fp-NEGATIVE slot access to r4 + (frame + off) -- the same
   ; cell, now a positive offset from a stable low base. frame-LINK forms carry offset 0, never < 0,
   ; so they keep fp. runs AFTER a4ize (which named the base fp), v6m only. r4 is free post-a4ize
   ; (the build-time r4 frame-base all became fp; v6-M has 4 arg regs r0..r3, so r4 is never the 5th).
   (t16reb f i bp fr)
    (? (! (two? f)) f
       (link (? (i = bp) 'r4 (i = (bp + 1)) (fr + <f) <f) (t16reb >f (i + 1) bp fr)))
   (t16slots fs fr)
    (? (! (two? fs)) ()
       (: f <fs
          bp (? (two? f) (peep a4base <f -1) -1)
          (link (? (&& (bp >= 0) (&& (id? (nthr f bp) 'fp) ((nthr f (bp + 1)) < 0))) (t16reb f 0 bp fr) f)
                (t16slots >fs fr))))
   ; v6m has no MOVW/MOVT to build an address, so an (la Rd lbl) becomes a literal-pool load: align to
   ; a word, LDR Rd,[pc,#0], hop the pooled abs32 word (the backend's lapool), and lay the word as an
   ; R_ARM_ABS32 the linker fills. runs LAST (after a4ize + t16slots), v6m only; other targets keep `la`
   ; (rip-rel lea / adrp). the align keeps the word 4-aligned so LDR's #0 offset always reaches it.
   (t16la fs)
    (? (! (two? fs)) ()
       (: f <fs
          ((? (&& (two? f) (id? <f 'la))
              `(`('align 4) `('lapool <>f) `('raw `(`('fix 4 'abs32 <>>f 0))))
              `(f))
           + (t16la >fs))))

   ; -- the SIBCALL pass (stage 8): a RET-position call tail-JUMPS instead, so
   ; deep tail recursion runs flat and the lvm shape Continues by jmp at
   ; ai_tco=1 (`make vmret` reads the disassembly). the rewrite is LOCAL:
   ; (call F) followed by exactly the epilogue becomes epilogue + (jmp F) -- at
   ; a ret-position call the operand stack is exactly the frame, and the
   ; epilogue parks sp on the return address, a call's entry state, so the
   ; callee answers our caller directly. (callr r3) parks the target in r10
   ; first (the epilogue reloads r3). anything BETWEEN the call and the
   ; epilogue -- an overflow-arg clean, a result conversion, a struct
   ; materialization -- breaks the pattern and keeps the plain call. GATED per
   ; fn: any (lea _ r4 _) means a frame address ESCAPES (a local's address, a
   ; struct temp, a va register save area) and the frame must outlive every
   ; call -- that fn keeps all its calls. the ESCAPE is tracked at the SOURCE
   ; (the g 'fesc pin, reset per fn): clval's lea for ordinary assignment is
   ; consumed by its own store and stays eligible -- only an address that
   ; becomes a VALUE (the & lane, a local array decaying, a struct-value rep,
   ; a compound literal, va_start's save area, the struct-return temp) pins.
   (leas? fs) (? (! (two? fs)) ()
                 (: f <fs
                    (? (&& (two? f) (&& (<f = 'lea) (<>>f = 'r4))) 1
                       (leas? >fs))))
   (escan g r) (: _ (? (&& (two? r) (leas? (vfs r))) (pin g 'fesc 1) ()) r)
   (unlean fs) (? (! (two? fs)) ()                   ; & makes a consumed address ESCAPE: lean -> lea, so the gate sees it
                  (: f <fs
                     (link (? (&& (two? f) (<f = 'lean)) (link 'lea >f) f)
                           (unlean >fs))))
   (relean fs) (? (! (two? fs)) ()                   ; the inverse, for a splice that consumes the address ITSELF (the overflow trio's &t)
                  (: f <fs
                     (link (? (&& (two? f) (<f = 'lea)) (link 'lean >f) f)
                           (relean >fs))))
   (epim? fs es) (? (! (two? es)) 1                  ; the next forms are EXACTLY es (the target's epilogue)
                    (! (two? fs)) ()
                    (&& (<fs = <es) (epim? >fs >es)))
   ; lw = the WINDOW rule: a frame lea seen since the last call boundary means
   ; the candidate's OWN staging may hand the callee a frame address (`return
   ; f(&x)`, `return f(buf)`) -- that call stays a call; the boundary resets
   ; the window (a consumed &Hp two statements up doesn't block the Continue).
   ; a ternary's JOIN LABEL may sit between the call and the epilogue (lvm_eval's
   ; Continue): the jump sequence lands BEFORE the labels and the labels + epi
   ; stay -- the other arm still flows through them.
   (lbl-epi? fs ej) (? (! (two? fs)) ()
                       (&& (two? <fs) ((cap (cap fs)) = 'label))
                        (? (epim? >fs ej) 1 (lbl-epi? >fs ej))
                       ())
   ; a nested-ternary return leaves each arm as (call F)(jmp JOIN) with JOIN
   ; leading to the epilogue: elabs collects those epilogue labels first, and
   ; a candidate followed by (jmp X), X collected, rewrites too (the dead jmp
   ; drops -- nothing lands mid-instruction).
   (elabs fs t ej) (? (! (two? fs)) t
                      (: f <fs
                         _ (? (&& (two? f) (&& (<f = 'label)
                                               (|| (epim? >fs ej) (lbl-epi? >fs ej))))
                              (pin t <>f 1) ())
                         (elabs >fs t ej)))
   (butret es) (? (! (two? >es)) () (link <es (butret >es)))   ; the epilogue's restore forms (the ret drops)
   (sibjmp g f)
    (? (<f = 'call) ((butret (peep g 'epi epi)) + `(`('jmp <>f)))
       (a64? g) ((butret epi-a64) + `(`('jmpr 'r15)))          ; the arm64 epilogue never touches the x15 park
       (t32? g) ((butret epi-a64) + `(`('jmpr (pkr g))))       ; thumb2: the callr target parks in r12 (pkr), untouched by the epilogue
       (`(`('mov 'r10 'r3)) + (butret epi) + `(`('jmpr 'r10))))   ; park first: the x64 epilogue reloads r3
   ; a home reload run ((ld rH r4 slot)..) after a ret-position call is DEAD --
   ; the tail-jump hands the frame back first. skiprel peels exactly those.
   (skiprel fs hs)
    (? (! (two? fs)) fs
       (: f <fs
          (? (&& (two? f) (&& (<f = 'ld) (<>>f = 'r4)))
              (: h (assoc <>f hs)
                 (? (&& (two? h) (<>h = <>>>f)) (skiprel >fs hs) fs))
             fs)))
   (sibs g fs lw el hs ej crf)
    (? (! (two? fs)) ()
       (: f <fs r >fs
          lw2 (? (&& (two? f) (&& (<f = 'lea) (<>>f = 'r4))) 1 lw)
          cand (&& (! lw2) (&& (two? f) (|| (&& (<f = 'call) (two? >f))
                                            (f = crf))))
          rr (? cand (skiprel r hs) r)
          (? (&& cand (epim? rr ej))
              ((sibjmp g f) + (sibs g (drop (tally ej) rr) () el hs ej crf))
             (&& cand (lbl-epi? rr ej))
              ((sibjmp g f) + (sibs g rr () el hs ej crf))
             (&& cand (: nx <rr (&& (two? nx) (&& (<nx = 'jmp) (peep el <>nx ())))))
              ((sibjmp g f) + (sibs g >rr () el hs ej crf))
             (link f (sibs g r (? (&& (two? f) (member? <f '(call callr raw))) () lw2) el hs ej crf)))))   ; raw: a builtin splice consumes its address like a call
   (sibcall g fs) (? (|| (peep g 'fesc ()) (v6m? g)) fs   ; v6m: the pop-pc epilogue can't restore lr for a tail-jmp, so no sibcall (deferred optimization)
                     (: ej (peep g 'epi epi)
                        (sibs g fs () (elabs fs (tablet 0) ej) (peep g 'homes ()) ej `('callr (pkr g)))))

   ; -- FRAME ELISION: a fn never needs the frame LINK (the entry push/mov,
   ; the sp-restore pair at every exit), only its SLOTS. the slots re-address
   ; off sp: keep (sub sp K9) with K9 = K + the link bytes (8 x64, 32 arm64),
   ; so every old base+off lands at sp+(off+K) and sp sits exactly where the
   ; framed body held it; each exit pays one (add sp K9) for the restore
   ; pair. a slotless call-free fn drops the sub too and its exits are bare;
   ; a fn with plain calls keeps the sub always -- sp holds still across a
   ; call exactly as r4 did, and K9 (8 mod 16) hands each callee the
   ; alignment push r4 used to. body sp MOTION rides: scan carries the
   ; static displacement D through the charm sp add/subs (spush spans,
   ; overflow-arg staging) and proves it at every label and branch through
   ; labt -- a dead label assumes 0 and every arrival verifies -- so a slot
   ; inside a span lands at sp+(off+K+D) and each exit unwinds K9+D. an r3
   ; user (x64) keeps rbx's save/restore as plain sp slots (the framed slot
   ; -8 was its alone; the callr PARK still squeezes out first -- the sv3
   ; lane below -- so a park-only r3 costs nothing). still framed: a raw
   ; splice (baked bytes can hide frame addressing), a bare push/pop, a
   ; non-charm sp write (alloca), any a64 call (bl clobbers lr), a call at a
   ; D off 16, and the frame base anywhere OUTSIDE a base slot (a4base; uoff
   ; is each op's displacement slot). rbp itself is retired everywhere it
   ; can be: the glaze ABI rides Ip in rbp, so a framed fn that calls into
   ; the VM faults on its own slots when the run returns -- and r4 never
   ; joins any pool (a native clobbers it on every VM-reaching call). --
   uoff (: t (tablet 0)
           _ (foldl (\ _ o (pin t o 3)) 0 (ldops9 + fldops + leaops))
           _ (foldl (\ _ o (pin t o 5)) 0 xops)
           _ (foldl (\ _ o (pin t o 2)) 0 stops9)
           _ (foldl (\ _ o (pin t o 4)) 0 stxops)
           t)
   (unframe g fs)
    (: a64 (a64? g)
       fb (? a64 'fp 'r4)
       pro (? a64
              (&& ((nthr fs 1) = '(push lr)) (&& ((nthr fs 2) = '(push fp)) ((nthr fs 3) = '(lea fp sp 0))))
              (&& ((nthr fs 1) = '(push r4)) ((nthr fs 2) = '(mov r4 sp))))
       sf (nthr fs (? a64 4 3))
       K (? (&& (two? sf) (&& (<sf = 'sub) (&& (<>sf = 'sp) (&& (<>>sf = 'sp) (charm? <>>>sf)))))
            <>>>sf ())
       body0 (drop (? a64 5 4) fs)
       ; -- the PARK lane (x64): a callr sib parks its target in r3 and unparks
       ; with (mov r10 r3); when everything between them (folded-away staging,
       ; or a few surviving arg movs) neither mentions r3 nor rewrites the
       ; parked SOURCE, the pair collapses to one mov at the unpark's seat --
       ; then the fn never WRITES r3 at all, so the caller's rbx rides the
       ; tail-jump untouched (the callee-saved contract made unnecessary, not
       ; elided) and the rbx save (body head, slot -8 is its alone) and each
       ; restore run's leading (ld r3 r4 -8) drop with the frame. a residual
       ; r3 no longer bails: scan's r3f keeps the save and every restore as
       ; sp slots instead. --
       sv3 (&& (! a64) ((nthr body0 0) = '(st r4 -8 r3)))
       wind (('(mov li) + leaops) + (xops + (aluops + ldops9)))
       wina ((wind + gstops) + '(cmp test))
       (squeeze l)
        (? (! (two? l)) ()
           (: f <l r >l
              (? (! (&& (two? f) (&& (<f = 'mov) (&& (id? <>f 'r3) (! (id? <>>f 'sp))))))
                 (link f (squeeze r))
                 (: X <>>f
                    (win w n)                        ; forms between park and unpark -> count | 'no
                     (? (|| (! (two? w)) (8 < n)) 'no
                        (: g2 <w
                           (? (g2 = '(mov r10 r3)) n
                              (! (two? g2)) 'no
                              (member? 'r3 >g2) 'no
                              (! (member? <g2 wina)) 'no
                              (&& (member? <g2 wind) (id? <>g2 X)) 'no
                              (win >w (n + 1)))))
                    k (win r 0)
                    (? (id? k 'no) (link f (squeeze r))
                       (: (take w n) (? (n < 1) (link `('mov 'r10 X) (squeeze (drop 1 w)))
                                        (link <w (take >w (n - 1))))
                          (take r k)))))))
       body (: b2 (? sv3 (drop 1 body0) body0) (? a64 b2 (squeeze b2)))
       (epi3? l9)                                    ; l9 opens with a restore run -> its length | ()
        (? a64 (? (&& ((nthr l9 0) = '(lea sp fp 0)) (&& ((nthr l9 1) = '(pop fp)) ((nthr l9 2) = '(pop lr)))) 3 ())
           (&& ((nthr l9 0) = '(mov sp r4)) ((nthr l9 1) = '(pop r4))) 2
           (&& ((nthr l9 0) = '(ld r3 r4 -8)) (&& ((nthr l9 1) = '(mov sp r4)) ((nthr l9 2) = '(pop r4)))) 3
           ())
       (clean? f2 bp)                                ; fb only at its base slot; a64 lr never
        (: (go e i) (? (! (two? e)) 1
                       (: x <e
                          (? (&& (id? x fb) (! (i = bp))) ()
                             (&& a64 (id? x 'lr)) ()
                             (go >e (i + 1)))))
           (go f2 0))
       labt (tablet 0)                               ; label -> the D every arrival agrees on
       (scan l D vs r3f live)                        ; -> 'no | (needs-sub r3-in-use)
        (? (! (two? l)) `(vs r3f)
           (: n3 (epi3? l)
              (? n3 (scan (drop n3 l) 0 vs r3f live)
                 (: f <l
                    (? (! (two? f)) (scan >l D vs r3f live)
                       (<f = 'label)
                        (: d9 (peep labt <>f 'no9)
                           (? (! (id? d9 'no9))
                              (? (&& live (! (D = d9))) 'no (scan >l d9 vs r3f 1))
                              (: D2 (? live D 0)
                                 _ (pin labt <>f D2)
                                 (scan >l D2 vs r3f 1))))
                       (<f = 'jmp)
                        (: d9 (peep labt <>f 'no9)
                           (? (id? d9 'no9) (: _ (pin labt <>f D) (scan >l D vs r3f ()))
                              (! (D = d9)) 'no
                              (scan >l D vs r3f ())))
                       (<f = 'br)
                        (: d9 (peep labt <>>f 'no9)
                           (? (id? d9 'no9) (: _ (pin labt <>>f D) (scan >l D vs r3f live))
                              (! (D = d9)) 'no
                              (scan >l D vs r3f live)))
                       (member? <f '(ret jmpr))
                        (scan >l D vs (|| r3f (&& (! a64) (member? 'r3 >f))) ())
                       (member? <f '(raw push pop)) 'no
                       (member? <f '(call callr))                 ; a plain call RIDES: sp is as
                        ; call-stable as r4 was, the callee keeps rbx, and the sub (K9 is 8 mod
                        ; 16) + a 16-multiple D hand the ABI exactly what push r4 did
                        (? a64 'no                                ; bl clobbers lr: the link stays
                           (! (0 = (& D 15))) 'no
                           (scan >l D 1 (|| r3f (member? 'r3 >f)) live))
                       (&& (spadj? f) (<f = 'sub)) (scan >l (D + <>>>f) vs r3f live)
                       (&& (spadj? f) (<f = 'add))
                        (? ((D - <>>>f) < 0) 'no (scan >l (D - <>>>f) vs r3f live))
                       (&& (member? <f '(sub add mov lea lean)) (<>f = 'sp)) 'no
                       (: bp (peep a4base <f -1)
                          (? (! (clean? f bp)) 'no
                             (scan >l D
                                   (? (&& (0 <= bp) (id? (nthr f bp) fb)) 1 vs)
                                   (|| r3f (&& (! a64) (member? 'r3 >f)))
                                   live))))))))
       vr (? (&& pro (charm? K)) (scan body 0 () () 1) 'no)
       (? (|| (id? vr 'no) (&& (! a64) (&& <>vr (! sv3)))) fs    ; r3 in use with no save to ride
          (: r3f (&& (! a64) <>vr)
             vs (|| <vr r3f)                         ; the save is a slot: r3f forces the sub
             K9 (K + (? a64 32 8))
             (radj f2 bp op9 D)                      ; the base slot -> sp, its displacement -> off+K+D
              (: (go e i) (? (! (two? e)) ()
                             (link (? (i = bp) 'sp (i = op9) ((<e + K) + D) <e)
                                   (go >e (i + 1))))
                 (go f2 0))
             (rw l D)
              (? (! (two? l)) ()
                 (: n3 (epi3? l)
                    (? n3 ((? r3f `(`('ld 'r3 'sp ((K + D) - 8))) ())
                           + ((? vs `(`('add 'sp 'sp (K9 + D))) (? (0 < D) `(`('add 'sp 'sp D)) ()))
                              + (rw (drop n3 l) 0)))
                       (: f <l
                          (? (! (two? f)) (link f (rw >l D))
                             (<f = 'label) (link f (rw >l (peep labt <>f D)))
                             (&& (spadj? f) (<f = 'sub)) (link f (rw >l (D + <>>>f)))
                             (&& (spadj? f) (<f = 'add)) (link f (rw >l (D - <>>>f)))
                             (: bp (peep a4base <f -1)
                                (link (? (&& (0 <= bp) (id? (nthr f bp) fb))
                                         (radj f bp (peep uoff <f -1) D)
                                         f)
                                      (rw >l D))))))))
             body2 (? r3f (link `('st 'r4 -8 'r3) body) body)
             (link <fs (? vs (link `('sub 'sp 'sp K9) (rw body2 0)) (rw body2 0))))))

   ; -- IR constant propagation (cfoldir): a straight-line abstract pass over a
   ; function's final forms. it tracks known small constants in registers, frame
   ; slots, and machine-stack cells, folds ALU ops on them back to li's, turns
   ; reg-reg ALU with a known source into the imm form, folds loads of known
   ; slots, and evaluates known cmp+br / cmp+set pairs (a constant ternary
   ; drops its dead arm). the payoff: sizeof/Width/Have arithmetic -- constant
   ; subgraphs the inliner exposes -- collapses the way a real compiler folds
   ; them. knowns stay in [0, 2^30] (compare signedness and shifts agree there);
   ; labels and calls invalidate values (joins/effects); anything unrecognized
   ; invalidates EVERYTHING (new ops stay safe by default). --
   kmax 1073741824
   (kok? v) (&& (0 <= v) (v < kmax))
   (cfoldir fs0)
    (: (fresh z9) (tablet 0)                        ; reg/slot/cell -> (1 K) const | (2 rX) copy-of-register | ()
       (rv t k) (peep t k ())
       (rset t k v) (pin t k v)
       (kn? v) (&& (two? v) (<v = 1))          ; a known CONSTANT (never let a copy reach arithmetic)
       ; resolve a source register to its deepest value: a const rides, a copy
       ; chases to its ROOT register (valid until that register is written)
       (rsv rg s) (: v (rv rg s) (? (two? v) v `(2 s)))
       ; the copy INDEX ci: source register -> the (tag key) sites holding a copy
       ; of it ('g rg / 's sl / 'c ce). a WRITE to d must invalidate every
       ; copy-of-d anywhere -- the index visits only the recorded sites (each
       ; validated against the live tablet, so a stale entry is harmless)
       ; instead of walking every key of all three tablets per register write
       (crec ci v tg k) (? (&& (two? v) (<v = 2))
                           (pin ci <>v (link `(tg k) (peep ci <>v ()))) ())
       (kild ci rg sl ce d)
        (: (go1 es) (? (! (two? es)) ()
                       (: e <es t (? (<e = 'g) rg (<e = 's) sl ce)
                          v (peep t <>e ())
                          _ (? (&& (two? v) (&& (<v = 2) (<>v = d))) (pin t <>e ()) ())
                          (go1 >es)))
           _ (go1 (peep ci d ()))
           (pin ci d ()))
       (defr ci rg sl ce d) (: _ (rset rg d ()) (kild ci rg sl ce d))
       ; rebuild the index for freshly JOINED tablets (small: agreed entries only)
       (mkci rg sl ce)
        (: ci (tablet 0)
           (scan1 t tg ks) (? (! (two? ks)) ()
                              (: _ (crec ci (peep t <ks ()) tg <ks) (scan1 t tg >ks)))
           _ (scan1 rg 'g (keys rg)) _ (scan1 sl 's (keys sl)) _ (scan1 ce 'c (keys ce))
           ci)
       (copyt t) (: t2 (tablet 0)
                    (go1 ks) (? (! (two? ks)) t2
                                (: _ (pin t2 <ks (peep t <ks ())) (go1 >ks)))
                    (go1 (keys t)))
       ; the JOIN of two abstract states: a key survives only where both sides
       ; know it and agree (the meet over the flat lattice) -- a fresh tablet
       (joint ta tb) (: t2 (tablet 0)
                        (go1 ks) (? (! (two? ks)) t2
                                    (: k1 <ks va (peep ta k1 ()) vb (peep tb k1 ())
                                       _ (? (&& (two? va) (&& (two? vb)
                                                (&& (<va = <vb) (<>va = <>vb))))
                                            (pin t2 k1 va) ())
                                       (go1 >ks)))
                        (go1 (keys ta)))
       (alu op a b) (? (op = 'add) (a + b) (op = 'sub) (a - b) (op = 'imul) (* a b)
                       (op = 'and) (& a b) (| a b))  ; xor stays unfolded (imm-converted only)
       (ccev cc a b) (? (cc = 'eq) (a = b) (cc = 'ne) (! (a = b))
                        (|| (cc = 'lt) (cc = 'below)) (a < b)
                        (|| (cc = 'le) (cc = 'be)) (a <= b)
                        (|| (cc = 'gt) (cc = 'above)) (b < a)
                        (b <= a))                    ; ge / ae
       ; after an unconditional transfer, everything to the next label is DEAD
       ; (a jump can only land on a label) -- a folded ternary drops its arm
       (swallow fs) (? (! (two? fs)) ()
                       (: f <fs (? (&& (two? f) (<f = 'label)) fs (swallow >fs))))
       (atlab? fs l) (&& (two? fs) (: f <fs (&& (two? f) (&& (<f = 'label) (<>f = l)))))
       ; back-edge labels (a jmp/br sitting BELOW its target): a loop head joins
       ; an edge this one linear pass never computes -- assume nothing there
       backs (: seen (tablet 0) bt (tablet 0)
                (bscan l) (? (! (two? l)) bt
                             (: f <l
                                _ (? (! (two? f)) ()
                                     (<f = 'label) (pin seen <>f 1)
                                     (<f = 'jmp) (? (peep seen <>f ()) (pin bt <>f 1) ())
                                     (<f = 'br) (? (peep seen <>>f ()) (pin bt <>>f 1) ())
                                     ())
                                (bscan >l)))
                (bscan fs0))
       ; the pending in-states: every FORWARD jmp/br to L joins its state into
       ; pend[L]; L's label reads it back and joins the fall-through -- so a
       ; known value CROSSES a label (the spush around a folded ternary reaches
       ; its spop). the sp cursor must agree or the cells drop.
       pend (tablet 0)
       (pmerge l cur rg sl ce ci)
        (: old (peep pend l ())
           (? (! (two? old)) (pin pend l `(cur (copyt rg) (copyt sl) (copyt ce) (copyt ci)))
              (: rg2 (joint <>old rg) sl2 (joint <>>old sl)
                 nce (? (<old = cur) (joint <>>>old ce) (fresh 0))
                 (pin pend l `(<old rg2 sl2 nce (mkci rg2 sl2 nce))))))
       ; lv: linearly LIVE -- () right after an unconditional transfer (the
       ; label then reads only its jump-in state, never the stale linear one)
       (go fs lv cur rg sl ce ci acc)
        (? (! (two? fs)) (rev acc)
           (: f <fs r >fs
              op (? (two? f) <f ())
              (? (op = 'label)
                  (: l1 <>f acc2 (link f acc)
                     (? (peep backs l1 ()) (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) acc2)
                        (: pd (peep pend l1 ())
                           (? (! (two? pd))
                               (? lv (go r 1 cur rg sl ce ci acc2)
                                  (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) acc2))
                              (! lv) (go r 1 <pd <>pd <>>pd <>>>pd
                                         (cap (cup (cup (cup (cup pd))))) acc2)
                              (: rg2 (joint <>pd rg) sl2 (joint <>>pd sl)
                                 ce2 (? (<pd = cur) (joint <>>>pd ce) (fresh 0))
                                 (go r 1 <pd rg2 sl2 ce2 (mkci rg2 sl2 ce2) acc2))))))
                 (member? op barops)
                  (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) (link f acc))
                 (op = 'jmp)                         ; drop the dead tail; a jmp to the very next label falls through
                  (: r2 (swallow r)
                     (? (atlab? r2 <>f) (go r2 1 cur rg sl ce ci acc)
                        (: _ (pmerge <>f cur rg sl ce ci)
                           (go r2 () cur rg sl ce ci (link f acc)))))
                 (op = 'br)                          ; the taken edge records; the fall-through rides on
                  (: _ (pmerge <>>f cur rg sl ce ci)
                     (go r 1 cur rg sl ce ci (link f acc)))
                 (member? op '(jmpr ret)) (go (swallow r) () cur rg sl ce ci (link f acc))
                 (op = 'li)
                  (: nx (? (two? r) <r ())
                     (? (&& (two? nx) (&& (member? <nx '(li mov)) (<>nx = <>f)))
                        (go r 1 cur rg sl ce ci acc)    ; the very next form rewrites this register: dead li
                        (: k <>>f
                           old (rv rg <>f)
                           (? (&& (kn? old) (<>old = k))
                              (go r 1 cur rg sl ce ci acc)   ; the register already holds this value
                              (: _ (defr ci rg sl ce <>f)
                                 _ (rset rg <>f (? (kok? k) `(1 k) ()))
                                 (go r 1 cur rg sl ce ci (link f acc)))))))
                 (op = 'mov)
                  (? (id? <>f <>>f) (go r 1 cur rg sl ce ci acc)   ; a self-mov says nothing
                     (: v (rsv rg <>>f)
                        old (rv rg <>f)
                        (? (&& (two? old) (&& (two? v)
                              (&& (<old = <v) (<>old = <>v))))
                           (go r 1 cur rg sl ce ci acc)     ; the dest already holds this value
                           (: _ (defr ci rg sl ce <>f)
                              _ (rset rg <>f v)
                              _ (crec ci v 'g <>f)
                              (go r 1 cur rg sl ce ci (link f acc))))))
                 (member? op '(add sub and or xor imul))
                  (? (<>f = 'sp)                ; sp arithmetic: the cell cursor moves
                      (: n <>>>f
                         (go r 1 (? (op = 'add) (cur - n) (cur + n)) rg sl ce ci (link f acc)))
                     (: d <>f a <>>f b <>>>f
                        ; b is a register SYMBOL or a charm immediate -- never confuse
                        ; the two (a symbol in arithmetic church-explodes)
                        bk (? (charm? b) (? (kok? b) `(1 b) ()) (rv rg b))
                        ak (rv rg a)
                        (? (&& (! (op = 'xor)) (&& (kn? ak) (kn? bk)))  ; both known: the op folds to a li
                            (: k (alu op <>ak <>bk)
                               _ (defr ci rg sl ce d)
                               (? (kok? k)
                                  (: _ (rset rg d `(1 k))
                                     (go r 1 cur rg sl ce ci (link `('li d k) acc)))
                                  (go r 1 cur rg sl ce ci (link f acc))))
                           (&& (! (charm? b)) (kn? bk))   ; known reg source -> the imm form
                            (: _ (defr ci rg sl ce d)
                               (go r 1 cur rg sl ce ci (link `(op d a <>bk) acc)))
                           (: _ (defr ci rg sl ce d)
                              (go r 1 cur rg sl ce ci (link f acc))))))
                 (member? op '(shl shr sar))         ; the 2-operand immediate shifts
                  (: d <>f k <>>f dk (rv rg d)
                     _ (defr ci rg sl ce d)
                     (? (&& (kn? dk) (charm? k))
                         (: v (? (op = 'shl) (* <>dk (k 2)) (>> <>dk k))
                            (? (kok? v)
                               (: _ (rset rg d `(1 v))
                                  (go r 1 cur rg sl ce ci (link `('li d v) acc)))
                               (go r 1 cur rg sl ce ci (link f acc))))
                        (go r 1 cur rg sl ce ci (link f acc))))
                 (op = 'st)
                  (? (<>f = 'r4)
                      (: v9 (rsv rg <>>>f) _ (rset sl <>>f v9) _ (crec ci v9 's <>>f)
                         (go r 1 cur rg sl ce ci (link f acc)))
                     (<>f = 'sp)
                      (: v9 (rsv rg <>>>f) _ (rset ce cur v9) _ (crec ci v9 'c cur)
                         (go r 1 cur rg sl ce ci (link f acc)))
                     (go r 1 cur rg (fresh 0) ce ci (link f acc)))   ; a pointer store may alias the frame
                 (op = 'ld)
                  (: d <>f b <>>f off <>>>f
                     kv (? (b = 'r4) (rv sl off) (&& (b = 'sp) (off = 0)) (rv ce cur) ())
                     (? (kn? kv)
                        (: _ (defr ci rg sl ce d)
                           _ (rset rg d kv)
                           (go r 1 cur rg sl ce ci (link `('li d <>kv) acc)))
                        (&& (two? kv) (<kv = 2))   ; the memory holds a live COPY: read the register
                         (? (id? <>kv d)
                            (go r 1 cur rg sl ce ci acc)   ; ... and it IS this register: the load says nothing
                            (: _ (defr ci rg sl ce d)
                               _ (rset rg d kv)
                               _ (crec ci kv 'g d)
                               (go r 1 cur rg sl ce ci (link `('mov d <>kv) acc))))
                        (: _ (defr ci rg sl ce d)
                           (go r 1 cur rg sl ce ci (link f acc)))))
                 (op = 'push)
                  (: cur2 (cur + 8)
                     v9 (rsv rg <>f)
                     _ (rset ce cur2 v9) _ (crec ci v9 'c cur2)
                     (go r 1 cur2 rg sl ce ci (link f acc)))
                 (op = 'pop)
                  (: v (rv ce cur)
                     _ (defr ci rg sl ce <>f)
                     _ (rset rg <>f v)
                     _ (crec ci v 'g <>f)
                     (go r 1 (cur - 8) rg sl ce ci (link f acc)))
                 (member? op >stops9)                ; the full-word st has its own arm above
                  (? (member? <>f '(r4 sp))
                      (: _ (? (<>f = 'r4) (rset sl <>>f ()) (rset ce cur ()))
                         (go r 1 cur rg sl ce ci (link f acc)))
                     (go r 1 cur rg (fresh 0) ce ci (link f acc)))
                 (member? op >ldops9)                ; the full-word ld has its own arm above
                  (: _ (defr ci rg sl ce <>f)
                     (go r 1 cur rg sl ce ci (link f acc)))
                 (member? op '(lea lean la movqrx cvttsd2si set))
                  (: _ (defr ci rg sl ce (? (op = 'set) <>>f <>f))
                     (go r 1 cur rg sl ce ci (link f acc)))
                 (member? op xops)                   ; indexed forms: the dest is new, sp/frame state stands
                  (: _ (defr ci rg sl ce <>f)
                     (go r 1 cur rg sl ce ci (link f acc)))
                 (member? op '(div rem udiv urem shlv shrv sarv neg not sx1 sx2 sx4 zx1 zx2 zx4))
                  (: _ (defr ci rg sl ce 'r0) _ (defr ci rg sl ce 'r1) _ (defr ci rg sl ce 'r2) _ (defr ci rg sl ce 'r3)
                     (go r 1 cur rg sl ce ci (link f acc)))
                 (op = 'cmp)                         ; paired with its br/set consumer
                  (: a (rv rg <>f)
                     b (? (charm? <>>f) (? (kok? <>>f) `(1 <>>f) ())
                          (rv rg <>>f))
                     nx (? (two? r) <r ())
                     (? (&& (kn? a) (&& (kn? b) (&& (two? nx) (member? <nx '(br set)))))
                         (: tv (ccev <>nx <>a <>b)
                            (? (<nx = 'br)
                               (? tv (: r2 (swallow >r)
                                        (? (atlab? r2 <>>nx) (go r2 1 cur rg sl ce ci acc)
                                           (: _ (pmerge <>>nx cur rg sl ce ci)
                                              (go r2 () cur rg sl ce ci (link `('jmp <>>nx) acc)))))
                                     (go >r 1 cur rg sl ce ci acc))
                               (: d <>>nx k (? tv 1 0)
                                  _ (defr ci rg sl ce d)
                                  _ (rset rg d `(1 k))
                                  (go >r 1 cur rg sl ce ci (link `('li d k) acc)))))
                        (go r 1 cur rg sl ce ci (link f acc))))
                 (op = 'test)                        ; flags only, writes nothing: every record stands
                  (go r 1 cur rg sl ce ci (link f acc))
                 (member? op '(movqxr ucomisd ldsd ldss cvtsi2sd cvtss2sd cvtsd2ss addsd subsd mulsd divsd movsd))
                  (go r 1 cur rg sl ce ci (link f acc)) ; the xmm lane leaves gp state alone
                 ; anything else: assume nothing survives
                 (go r 1 cur (fresh 0) (fresh 0) (fresh 0) (fresh 0) (link f acc)))))
       (go fs0 1 0 (fresh 0) (fresh 0) (fresh 0) (fresh 0) ()))

   ; dead CELLS: after the copy folds a spush cell whose load folded away is
   ; write-only -- its store drops, and its sub/add RESERVATION goes with it
   ; (the orphan sweep). a record STACK mirrors the sp motion exactly --
   ; 16-byte cells (sub+st adjacent, the spush shape), 8-byte pushes, pads,
   ; multi-slot cleans -- and ANY shape outside the model bails the whole fn.
   ; a record born from a sub and unwound by a matching add CANCELS (drop the
   ; sub, shrink the add) when its cell is unread, its size keeps the
   ; 16-alignment every call rides, and nothing observed sp while it was open:
   ; sp-relative addressing is translation-invariant, so nested live cells
   ; ride through unmoved. a LIVE cell under the same guards CONVERTS instead:
   ; its parked value moves to a fresh frame slot below the frame (the
   ; prologue sub grows) and the sub/add pair still cancels -- overlapping
   ; cells sit at distinct record depths and same-depth cells never overlap
   ; in time, so one slot per depth serves the whole fn; a frame slot rides
   ; every call, as the cell did. an op outside the model that MENTIONS sp
   ; (or a raw, opaque) dirties every open record instead of bailing -- the
   ; (mov sp r4) epilogue is a PATH exit on the linear walk, and its
   ; sp-mention keeps the frame's own record dirty (its sub is also never
   ; add-unwound: two belts).
   (deadcell fs)
    (: cand (tablet 0)                               ; st-index -> 1 while still droppable
       clds (tablet 0)                               ; st-index -> its cell's ld indexes
       dirty (tablet 0)                              ; sub-index -> 1: the span observed sp
       (dirt st2) (? (! (two? st2)) 1
                     (: _ (? (charm? (cauup (cap st2))) (pin dirty (cauup (cap st2)) 1) ())
                        (dirt >st2)))
       (scan l i stk sw)                             ; -> the (sub st k add depth) entries | 'no
        (? (! (two? l)) sw
           (: f <l r >l
              (? (! (two? f)) (scan r (i + 1) stk sw)
                 (&& (<f = 'sub) (&& (<>f = 'sp) (charm? <>>>f)))
                  (: k <>>>f
                     nx (? (two? r) <r ())
                     (? (&& (k = 16) (&& (two? nx) (&& (<nx = 'st)
                            (&& (<>nx = 'sp) (<>>nx = 0)))))
                        (: _ (pin cand (i + 1) 1)
                           (scan >r (i + 2) (link `((i + 1) 16 i (tally stk)) stk) sw))
                        (scan r (i + 1) (link `(() k i (tally stk)) stk) sw)))
                 (&& (<f = 'add) (&& (<>f = 'sp) (charm? <>>>f)))
                  (: (unwind st2 k w)
                      (? (k = 0) `(st2 w)
                         (! (two? st2)) 'no
                         (: rc (cap st2) sz (caup rc)
                            (? (k < sz) 'no
                               (unwind >st2 (k - sz)
                                (? (charm? (cauup rc)) (link `((cauup rc) (cap rc) sz i (nthr rc 3)) w) w)))))
                     r9 (unwind stk <>>>f ())
                     (? (id? r9 'no) 'no (scan r (i + 1) (cap r9) ((caup r9) + sw))))
                 (<f = 'push) (scan r (i + 1) (link `(() 8 () ()) stk) sw)
                 (<f = 'pop)
                  (? (&& (two? stk) (&& (! (cap (cap stk))) ((caup (cap stk)) = 8)))
                     (scan r (i + 1) >stk sw)
                     (<>f = 'r4) (scan r (i + 1) stk sw)   ; the epilogue's frame pop: path exit
                     'no)
                 (&& (member? <f ldops9) (<>>f = 'sp))
                  (? (&& (two? stk) (&& (cap (cap stk)) (<>>>f = 0)))
                     (: stx (cap (cap stk))
                        _ (pin cand stx ())              ; loaded: the store lives
                        _ (pin clds stx (link i (peep clds stx ())))
                        (scan r (i + 1) stk sw))
                     'no)
                 (&& (member? <f stops9) (<>f = 'sp)) 'no
                 (&& (member? <f fldops) (<>>f = 'sp)) 'no
                 (: _ (? (|| (<f = 'raw) (member? 'sp >f)) (dirt stk) ())
                    (scan r (i + 1) stk sw)))))
       ok (scan fs 0 () ())
       (? (id? ok 'no) fs
          (: subdrop (tablet 0) addsh (tablet 0)     ; sub-index -> drop; add-index -> shrink bytes
             stconv (tablet 0) ldconv (tablet 0)     ; st/ld-index -> the slot's positive offset
             pf3 (nthr fs 3)                         ; the prologue's frame sub: (sub sp sp F)
             cvok (&& ((nthr fs 1) = '(push r4)) (&& ((nthr fs 2) = '(mov r4 sp))
                   (&& (two? pf3) (&& (<pf3 = 'sub) (&& (<>pf3 = 'sp) (&& (<>>pf3 = 'sp) (charm? <>>>pf3)))))))
             F (? cvok <>>>pf3 0)
             (mark w cv) (? (! (two? w)) cv
                            (: e <w sub (nthr e 0) st (nthr e 1) k (nthr e 2) ad (nthr e 3)
                               (? (peep dirty sub ()) (mark >w cv)
                                  (! (0 = (% k 16))) (mark >w cv)
                                  (|| (! st) (peep cand st ()))
                                   (: _ (pin subdrop sub 1)
                                      _ (pin addsh ad ((peep addsh ad 0) + k))
                                      (mark >w cv))
                                  (&& cvok (k = 16)) (mark >w (link e cv))
                                  (mark >w cv))))
             cvl (mark ok ())
             ds (: (dd l acc) (? (! (two? l)) acc
                                 (: d (nthr (cap l) 4)
                                    (dd >l (? (member? d acc) acc (link d acc)))))
                   (dd cvl ()))
             (rank d l n) (? (id? <l d) n (rank d >l (n + 1)))
             _ ((: (cvt l) (? (! (two? l)) 1
                              (: e (cap l)
                                 o ((F + 8) + (8 * (rank (nthr e 4) ds 0)))
                                 _ (pin subdrop (nthr e 0) 1)
                                 _ (pin addsh (nthr e 3) ((peep addsh (nthr e 3) 0) + 16))
                                 _ (pin stconv (nthr e 1) o)
                                 _ ((: (pl ls) (? (! (two? ls)) 1
                                                  (: _ (pin ldconv <ls o) (pl >ls))) pl)
                                    (peep clds (nthr e 1) ()))
                                 (cvt >l))) cvt) cvl)
             F2 (& ((F + (8 * (tally ds))) + 15) -16)
             (drop2 l i) (? (! (two? l)) ()
                            (? (peep cand i ()) (drop2 >l (i + 1))
                               (peep subdrop i ()) (drop2 >l (i + 1))
                               (: f2 <l
                                  o9 (peep stconv i ())
                                  ol (peep ldconv i ())
                                  s (peep addsh i 0)
                                  (? o9 (link `('st 'r4 (0 - o9) <>>>f2) (drop2 >l (i + 1)))
                                     ol (link `(<f2 <>f2 'r4 (0 - ol)) (drop2 >l (i + 1)))
                                     (&& (two? cvl) (i = 3)) (link `('sub 'sp 'sp F2) (drop2 >l (i + 1)))
                                     (s = 0) (link f2 (drop2 >l (i + 1)))
                                     (: k2 (<>>>f2 - s)
                                        (? (k2 = 0) (drop2 >l (i + 1))
                                           (link `('add 'sp 'sp k2) (drop2 >l (i + 1)))))))))
             (drop2 fs 0))))

   ; dead DEFS: within a basic block, a PURE definition whose register is
   ; re-defined before any read is dead -- the copy folds strand whole li/mov
   ; chains this way. one backward walk; every control edge (label, transfer,
   ; call, raw, sys) resets the dead set (everything assumed live across it);
   ; an op outside the tables does the same. dead_before = (dead + defs) - reads.
   (rdsp f)                                         ; -> (L reads defs pure?) | 'bar
    (: op <f
       (rg1 x) (? (charm? x) () `(x))
       (? (op = 'li) `(() `(<>f) 1)
          (op = 'mov) `(`(<>>f) `(<>f) 1)
          (member? op '(add sub and or xor imul))
           (? (<>f = 'sp) `(() () ())      ; sp motion touches no gp register
              `(((rg1 <>>f) + (rg1 <>>>f)) `(<>f) 1))
          (op = 'push) `(`(<>f) () ())
          (op = 'pop) `(() `(<>f) ())
          (member? op '(shl shr sar rol ror)) `(`(<>f) `(<>f) 1)
          (member? op '(ld ld1 ld2 ld4 ldu1 ldu2 ldu4)) `(`(<>>f) `(<>f) 1)
          (member? op '(st st1 st2 st4)) `(`(<>f <>>>f) () ())
          (member? op '(lea lean)) `(`(<>>f) `(<>f) 1)
          (member? op '(leax ldx ldxb)) `(`(<>>f <>>>f) `(<>f) 1)
          (op = 'la) `(() `(<>f) 1)
          (op = 'set) `(() `(<>>f) 1)
          (member? op '(cmp test)) `(((rg1 <>f) + (rg1 <>>f)) () ())
          (member? op '(neg not)) `(`(<>f) `(<>f) 1)
          (member? op '(sx1 sx2 sx4 zx1 zx2 zx4)) `(`(<>f) `(<>f) 1)
          (op = 'movqxr) `(`(<>>f) () ())
          (op = 'movqrx) `(() `(<>f) 1)
          (op = 'cvtsi2sd) `(`(<>>f) () ())
          (op = 'cvttsd2si) `(() `(<>f) ())
          (member? op '(ldsd ldss)) `(`(<>>f) () ())
          (member? op '(stsd stss)) `(`(<>f) () ())
          (member? op '(addsd subsd mulsd divsd movsd ucomisd cvtss2sd cvtsd2ss)) `(() () ())
          (member? op '(div rem udiv urem))
           `(((rg1 <>>f) + (rg1 <>>>f)) `('r0 'r1 'r2 'r3) ())
          (member? op '(shlv shrv sarv)) `(`(<>f <>>f) `(<>f 'r1) ())
          'bar))
   (deaddef fs)
    (: (go l dead acc)                               ; l already REVERSED; acc rebuilds forward
        (? (! (two? l)) acc
           (: f <l r >l
              (? (! (two? f)) (go r dead (link f acc))
                 (: rd (rdsp f)
                    (? (id? rd 'bar) (go r (tablet 0) (link f acc))
                       (: reads <rd defs <>rd pure <>>rd
                          (dd? ds) (? (! (two? ds)) 1
                                      (? (peep dead <ds ()) (dd? >ds) ()))
                          (? (&& pure (&& (two? defs) (dd? defs)))
                             (go r dead acc)                     ; every def overwritten unread: drop
                             (: _ ((: (ad ds) (? (! (two? ds)) 1
                                                 (: _ (? (id? <ds 'sp) () (pin dead <ds 1))
                                                    (ad >ds))) ad) defs)
                                _ ((: (rm rs) (? (! (two? rs)) 1
                                                 (: _ (pin dead <rs ()) (rm >rs))) rm) reads)
                                (go r dead (link f acc))))))))))
       (go (rev fs) (tablet 0) ()))

   ; -- the arrival rename (the clang differential's home-shuffle bucket): a
   ; call-free fn's param homes copy the arg registers up front (mov H A) and
   ; copy back at every staged tail (mov A H) -- pure shuffle whenever the
   ; arrival register was never otherwise touched. jr0 first: a park (mov P X)
   ; riding straight-line to its (jmpr P) retargets to (jmpr X) -- the park's
   ; fresh def of a home register would otherwise block its pair. then unhome
   ; discovers each (H A) pair off the leading movs and walks once, bail-happy:
   ; any OTHER mention of A kills the pair (r1/r2 are the shuttle/div protocol
   ; -- a fn that needed them keeps its homes), as does a FRESH def of H (a
   ; second live range would merge onto A while a restored copy still rides to
   ; the transfer); an H def that READS H (the hp bump, the sp walk) is the
   ; value evolving and renames sound. a survivor renames H -> A everywhere,
   ; the entry mov and every restore fall out as self-movs, and the param
   ; rides its ARRIVAL register end to end. self-tail-calls stay sound: the
   ; loop-back label sits above the (dropped) entry movs and the value is
   ; already seated. --
   (jr0 fs)
    (? (! (two? fs)) ()
       (: f <fs
          (? (! (&& (two? f) (&& (<f = 'mov) (! (charm? <>>f)))))
              (link f (jr0 >fs))
             (: P <>f X <>>f
                (scan w n)                            ; forms to the jmpr -> n | 'no
                 (? (|| (! (two? w)) (10 < n)) 'no
                    (: f2 <w
                       (? (! (two? f2)) 'no
                          (&& (<f2 = 'jmpr) (id? <>f2 P)) n
                          (: rd (rdsp f2)
                             (? (id? rd 'bar) 'no
                                (member? P >f2) 'no
                                (member? X <>rd) 'no
                                (scan >w (n + 1)))))))
                k (scan >fs 0)
                (? (id? k 'no) (link f (jr0 >fs))
                   (: (mend w i) (? (i = k) (link `('jmpr X) (jr0 >w))
                                    (link <w (mend >w (i + 1))))
                      (mend >fs 0)))))))
   (unhome g fs)
    (: ar (argr g) hr (hregs g)
       (pears l n acc)                                ; the leading (mov H A) movs -> ((H A)..)
        (? (|| (! (two? l)) (8 < n)) acc
           (: f <l
              (? (&& (two? f) (&& (<f = 'mov)
                    (&& (member? <>f hr) (member? <>>f ar))))
                 (pears >l (n + 1) (link `(<>f <>>f) acc))
                 (pears >l (n + 1) acc))))
       (callish l) (? (! (two? l)) ()
                      (: f <l (? (&& (two? f) (member? <f barops)) 1 (callish >l))))
       (ok? H A l seen)
        (? (! (two? l)) 1
           (: f <l
              (? (! (two? f)) (ok? H A >l seen)
                 (&& (! seen) (&& (<f = 'mov) (&& (id? <>f H) (id? <>>f A))))
                  (ok? H A >l 1)                      ; the entry mov
                 (&& (<f = 'mov) (&& (id? <>f A) (id? <>>f H)))
                  (ok? H A >l seen)                   ; a staging restore
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (? (member? <f '(label jmp br ret)) (ok? H A >l seen)
                           (&& (<f = 'jmpr) (! (id? <>f A))) (ok? H A >l seen)
                           ())                        ; an op outside the tables: keep the homes
                       (: reads <rd
                          (? (|| (member? A reads) (member? A <>rd)) ()
                             (&& (member? H <>rd) (! (member? H reads))) ()
                             (ok? H A >l seen))))))))
       (ren H A l)
        (? (! (two? l)) ()
           (: f <l
              f2 (? (two? f) ((: (sw e) (? (! (two? e)) e (link (? (id? <e H) A <e) (sw >e))) sw) f) f)
              (? (&& (two? f2) (&& (<f2 = 'mov) (id? <>f2 <>>f2)))
                 (ren H A >l)                         ; a self-mov husk drops
                 (link f2 (ren H A >l)))))
       (go2 ps l)
        (? (! (two? ps)) l
           (: p <ps
              (go2 >ps (? (ok? <p <>p l ()) (ren <p <>p l) l))))
       (? (callish fs) fs (go2 (pears fs 0 ()) fs)))

   ; adjacent sp adjustments MERGE (nothing observes the point between two
   ; touching adds); a cancelled pair vanishes -- the husk a dead cell leaves
   (spadj? f) (&& (two? f) (&& (member? <f '(add sub))
                (&& (<>f = 'sp) (&& (<>>f = 'sp) (charm? <>>>f)))))
   (spmerge fs)
    (? (! (two? fs)) ()
       (: f <fs r >fs
          nx (? (two? r) <r ())
          (? (&& (spadj? f) (spadj? nx))
             (: n1 (? (<f = 'add) <>>>f (0 - <>>>f))
                n2 (? (<nx = 'add) <>>>nx (0 - <>>>nx))
                net (n1 + n2)
                (? (net = 0) (spmerge >r)
                   (net < 0) (spmerge (link `('sub 'sp 'sp (0 - net)) >r))
                   (spmerge (link `('add 'sp 'sp net) >r))))
             (link f (spmerge r)))))

   ; dead frame stores: after the constant folds, a slot the function never
   ; loads back is write-only -- its stores drop (the b2w-shape arg temps whose
   ; every read folded to a li). SAFE only when NO frame address is taken at
   ; all (any lea/lean of r4 skips the pass: an lea marks one offset but hands
   ; out a RANGE -- a va save area, an aggregate fill -- so per-offset liveness
   ; would lie). a store's newly-dead feeding li sweeps in the same pass.
   (deadst fs)
    (: used (tablet 0) esc (tablet 0)
       (scan l) (? (! (two? l)) 1
                   (: f <l
                      _ (? (! (two? f)) ()
                           (member? <f ldops9)
                            (? (<>>f = 'r4) (pin used <>>>f 1) ())
                           (member? <f fldops)
                            (? (<>>f = 'r4) (pin used <>>>f 1) ())
                           (member? <f '(lea lean))
                            (? (<>>f = 'r4) (pin esc 'y 1) ())
                           ())
                      (scan >l)))
       _ (scan fs)
       (drop l) (? (! (two? l)) ()
                   (: f <l
                      (? (&& (two? f) (&& (member? <f stops9)
                             (&& (<>f = 'r4) (! (peep used <>>f ())))))
                         (drop >l)
                         (link f (drop >l)))))
       (lisweep l) (? (! (two? l)) ()
                      (: f <l nx (? (two? >l) <>l ())
                         (? (&& (two? f) (&& (<f = 'li)
                               (&& (two? nx) (&& (member? <nx '(li mov)) (<>nx = <>f)))))
                            (lisweep >l)
                            (link f (lisweep >l)))))
       (? (peep esc 'y ()) fs (lisweep (drop fs))))

   ; the addressing fold: an add/lea/mov whose result is consumed as the VERY
   ; NEXT load's base folds into the load's displacement, so the member-access
   ; triple (mov base; add off; ld) lands as the one-insn form every real
   ; compiler emits. sound by adjacency: a dest the load overwrites (or one
   ; that dies on the straight line) folds outright; a LIVE dest keeps the
   ; addr insn beside the re-based load -- whichever order leaves every read
   ; pre-clobber -- for the deaddef rerun to harvest. two passes chain the
   ; mov through a folded add. x64 only (the a64 lane rides raw forms).
   ; a register DIES on the straight-line suffix when an insn overwrites it
   ; before any read -- a label/branch/call/unknown op ends the certainty
   (dies? rd l)
    (? !(two? l) ()
       (: f <l
          (? !(two? f) ()
             (: rs (rdsp f)
                (? (id? rs 'bar) ()
                   (member? rd <rs) ()
                   (member? rd <>rs) 1
                   (dies? rd >l))))))
   (addrf1 fs)
    (? !(two? fs) ()
       (: f <fs r >fs
          nx (? (two? r) <r ())
          ; a leax feeding the very next full-word/byte load THROUGH its dest
          ; IS the indexed load: one SIB insn (base/index read at the leax's
          ; seat, so they may alias an overwritten or dying dest)
          lx (? !(two? f && <f = 'leax && two? nx  && member? <nx '(ld ldu1)) ()
                !(id? <>>nx <>f) ()
                !(&& (charm? <>>>nx) (charm? (nthr f 5))) ()
                `((? (<nx = 'ld) 'ldx 'ldxb) <>nx <>>f <>>>f (nthr f 4)
                  ((nthr f 5) + <>>>nx)))
          hit (? (two? lx) ()
                 !(two? f && two? nx  && member? <nx ldops9) ()
                 !(id? <>>nx <>f) ()               ; the load's base is f's dest
                 !(charm? <>>>nx) ()
                 (member? <f '(lea lean))
                  (? (charm? <>>>f) `(<>>f (<>>>f + <>>>nx)) ())
                 (<f = 'add)
                  (? (charm? <>>>f && !(id? <>>f 'sp) && fits32? (<>>>f + <>>>nx))
                     `(<>>f (<>>>f + <>>>nx)) ())
                 (<f = 'mov) `(<>>f <>>>nx)
                 ())
          (? (two? lx)
              (? (id? <>nx <>f || dies? <>f >r) (lx >< addrf1 >r)
                 !(id? <>f <>>f || id? <>f <>>>f)
                  (f >< lx >< addrf1 >r)
                 !(id? <>nx <>>f || id? <>nx <>>>f)
                  (lx >< f >< addrf1 >r)
                 (f >< addrf1 r))
             (two? hit)
              (: nl `(<nx <>nx <hit <>hit)
                 (? (id? <>nx <>f || dies? <>f >r) (nl >< addrf1 >r)
                    !(id? <>f <hit) (f ><  nl >< addrf1 >r)
                    (nl >< f >< addrf1 >r)))
             (f >< addrf1 r))))
   (addrfold fs) (addrf1 (addrf1 fs))

   ; store-forward: a full-word frame store followed by ITS OWN reload reads the
   ; register instead -- (st r4 o rX) (ld rY r4 o) keeps the store and turns the
   ; reload into a mov (the same-register form drops it). the narrow loads
   ; (ld4 ..) keep their memory round-trip: they re-extend, a different value.
   (stld fs)
    (? !(two? fs) ()
       (: f <fs r >fs
          (? !(two? r && two? f && <f = 'st && <>f = 'r4)
             (f >< stld r)
             (: nx <r
                rX <>>>f
                (? !(two? nx && <nx = 'ld && <>>nx = 'r4 && <>>>nx = <>>f)
                   (f >< stld r)
                   (: rY <>nx
                      (? (rY = rX) (f >< stld >r)
                         (f >< `('mov rY rX) >< stld >r))))))))

   ; -- aggregate initializers: ('init (item..)), items plain | ('dfield
   ; name val) | ('didx n val), nesting for arrays of structs. shared bits: --
   (aggish? ty) (arr? ty || stru? ty)
   (elval it) (? (member? <it '(dfield didx)) <>>it it)   ; the value of an item
   (fidx fields name) (: (go l i) (? !(two? l) 0 (<<l = name) i (go >l (i + 1)))
                          (go fields 0))
   (nthfield fields i) (: (go l k) (? !(two? l) 'bad (k < 1) <l (go >l (k - 1)))
                           (go fields i))

   ; -- brace elision (C 6.7.9): a flat run of items fills a nested subaggregate whose
   ; brace was omitted -- int a[2][2] = {1,2,3,4} means {{1,2},{3,4}}. we NORMALIZE the
   ; init tree up front (wrapping each elided run in an explicit ('init ..)), so the layout
   ; code below only ever sees fully-braced inits. idempotent on an already-braced tree. --
   (initval? v) (two? v && <v = 'init)
   (unelide g ty ini)                               ; normalize ini against ty
    (? !(initval? ini) ini                        ; a scalar / non-brace: nothing to nest
       !(aggish? ty) ini                           ; a brace on a scalar type: leave (layout unwraps/scares)
       (: pr (normfill g ty <>ini) `('init <pr)))     ; <pr = normed items; <>pr (excess) dropped
   (redesig dh dk v) `(dh dk v)   ; ('didx k v) | ('dfield k v)
   (normelt g ty desig dh dk val l)                 ; one element of type ty from item stream l -> (item rest)
    (? (aggish? ty && !(initval? val))          ; ELIDE: fill ty from the run starting at val
        (: pr (normfill g ty (link val >l))
           w `('init <pr)
           `((? desig (redesig dh dk w) w) <>pr))
       (: nv (? (initval? val) (unelide g ty val) val)  ; a braced sub -> recurse-normalize; a scalar -> as-is
          `((? desig (redesig dh dk nv) (? (id? nv val) <l nv)) >l)))
   (normfill g ty items)                            ; consume items to fill aggregate ty -> (normed-items rest)
    (? (arr? ty)
        (: elt <>ty n <>>ty
           (go l idx acc)
            (? (! (two? l)) `((rev acc) ())
               (&& (< 0 n) (<= n idx)) `((rev acc) l)  ; array full -> the tail is the rest
               (: it <l
                  desig (<it = 'didx)
                  ci (? desig <>it idx)
                  pr (normelt g elt desig 'didx ci (elval it) l)
                  (go <>pr (ci + 1) (link <pr acc))))
           (go items 0 ()))
       (stru? ty)
        (: fields (cap (peep (g 'stag) <>ty 0))
           (go l cur acc)
            (? (! (two? l)) `((rev acc) ())
               (: it <l
                  desig (<it = 'dfield)
                  ci (? desig (fidx fields <>it) cur)
                  fe (nthfield fields ci)
                  (? (bad? fe) `((rev acc) l)          ; unknown field / past the end -> leave the rest
                     (: pr (normelt g <>>fe desig 'dfield <>it (elval it) l)
                        (go <>pr (ci + 1) (link <pr acc))))))
           (go items 0 ()))
       `(items ()))

   ; -- LOCAL init (codegen): zero the slot, then fill (values may be runtime) --
   (cgzero off span)                                ; zero [r4+off .. +span)
    (: (go o rem acc)
        (? (8 <= rem) (go (o + 8) (rem - 8) (acc + `(`('st 'r4 o 'r0))))
           (0 < rem) (go (o + 1) (rem - 1) (acc + `(`('st1 'r4 o 'r0))))
           acc)
       (`(`('li 'r0 0)) + (go off span ())))
   (cgstr off text)                                 ; a char array from a string: byte stores (NUL from the zero)
    (: (go i acc) (? (i < 0) acc (go (i - 1) (`(`('li 'r0 (text i)) `('st1 'r4 (off + i) 'r0)) + acc)))
       (go ((tally text) - 1) ()))
   (cgstore g env off ty val)                       ; store a scalar value expr into [r4+off]
    (: r (cgexpr g env val)
       (? (bad? r) 'bad
          (flo? ty) ((asflo g r) + (fstf g ty 'r4 off 'f0))
          (pair? g ty) ((asll g r) + (pst 'r4 off))
          ((asint g r) + (cvt ty 'r0) + `(`((stop ty) 'r4 off 'r0)))))
   (cgfill g env off ty ini0)                       ; fill an aggregate at frame offset off (no zero)
    (: ini (unelide g ty ini0)                        ; normalize elided braces (idempotent)
     (? (<ini = 'str) (? (arr? ty) (cgstr off <>ini) 'bad)
       (! (<ini = 'init)) 'bad
       (arr? ty)
        (: elt <>ty esz (tsz g elt)
           (fill l idx acc)
            (? (! (two? l)) acc
               (: it <l
                  ci (? (<it = 'didx) <>it idx)
                  val (elval it)
                  eoff (off + (* ci esz))
                  r (? (&& (aggish? elt) (<val = 'init)) (cgfill g env eoff elt val)
                       (cgstore g env eoff elt val))
                  (? (bad? r) 'bad (fill >l (ci + 1) (acc + r)))))
           (fill <>ini 0 ()))
       (stru? ty)
        (: fields (cap (peep (g 'stag) <>ty 0))
           (fill l cur acc)
            (? (! (two? l)) acc
               (: it <l
                  ci (? (<it = 'dfield) (fidx fields <>it) cur)
                  val (elval it)
                  fe (nthfield fields ci)
                  (? (bad? fe) 'bad
                     (: foff (off + <>fe) fty <>>fe
                        r (? (&& (aggish? fty) (<val = 'init)) (cgfill g env foff fty val)
                             (cgstore g env foff fty val))
                        (? (bad? r) 'bad (fill >l (ci + 1) (acc + r)))))))
           (fill <>ini 0 ()))
       'bad))

   ; -- GLOBAL init (a constant byte image, where a scalar is little-endian
   ; bytes, an aggregate a placement flatten, and a POINTER an abs64 fixup --
   ; ('fix 8 abs64 label 0), the label's absolute address, laid by holo/elf.
   ; a fixup is ONE list item but eight bytes, so lengths go through imbytelen. --
   (fixit? x) (&& (two? x) (<x = 'fix))
   (imbytelen l) (foldl (\ a x (a + (? (fixit? x) <>x 1))) 0 l)
   ; bytes via // and % (not & and >>): a constant past the charm range is a BIG, and the
   ; bitwise ops are undefined there (a 2/pi table imaged as ff.. before this); division is
   ; exact at every rung. a negative wraps into the type's unsigned range first.
   (imnum g ty val) (: n (tsz g ty)
                       (pw k acc) (? (k < 1) acc (pw (k - 1) (acc * 256)))
                       u0 (? (val < 0) (val + (pw n 1)) val)
                       (go i v acc) (? (n <= i) (rev acc)
                                       (go (i + 1) (v // 256) (link (% v 256) acc)))
                       (go 0 u0 ()))
   (iminsert x l) (? (! (two? l)) `(x)                 ; insert (off items) keeping offsets ascending
                     (<x < (cap (cap l))) (link x l)
                     (link <l (iminsert x >l)))
   (imsort ps) (: (go l acc) (? (! (two? l)) acc (go >l (iminsert <l acc))) (go ps ()))
   (imflat ps total)                                ; sorted (off items).. -> total bytes, gaps zeroed
    (: (go l pos acc)
        (? (! (two? l)) (acc + (zeros (total - pos)))
           (: p <l off <p items <>p
              (go >l (off + (imbytelen items)) (acc + (zeros (off - pos)) + items))))
       (go ps 0 ()))
   ; a CONSTANT integer expression at image time -> its value | 'no. C's static
   ; initializers permit any int const-expr in an array subscript / pointer
   ; offset (`&a[2*K+1]`), so fold the arithmetic here. `//` truncates toward
   ; zero (C's `/`); the machine ops (<<, &..) wrap at the word, as cc's do.
   (icfold e)
    (? (! (two? e)) 'no
       (<e = 'num) <>e
       (<e = 'cast) (: v (icfold <>>e) (? (id? v 'no) 'no (flo? <>e) 'no v))
       (<e = 'un)
        (: v (icfold <>>e)
           (? (id? v 'no) 'no (<>e = 'neg) (0 - v) (<>e = 'bnot) ((0 - v) - 1) 'no))
       (<e = 'bin)
        (: op <>e a (icfold <>>e) b (icfold <>>>e)
           (? (|| (id? a 'no) (id? b 'no)) 'no
              (op = '+) (a + b) (op = '-) (a - b) (op = '*) (* a b)
              (op = '/) (? (b = 0) 'no (// a b)) (op = '%) (? (b = 0) 'no (a - (* (// a b) b)))
              (op = 'shl) (<< a b) (op = 'shr) (>> a b)
              (op = '&) (& a b) (op = '|) (| a b) (op = '^) (^ a b)
              'no))
       'no)
   ; a CONSTANT LVALUE at image time -> (label byteoffset type) | 'no: a
   ; file-scope global, a *(const pointer) (an array subscript desugars here --
   ; a[i] is *(a+i)), or a .member off either. the type threads so pointer
   ; arithmetic and the next .member scale/offset correctly.
   (imlval g e)
    (? (<e = 'var)
        (? (peep (g 'fns) <>e ()) `((intern <>e) 0 (fnty g <>e))   ; a function IS its address
           (: en (peep (g 'glob) <>e 0)                                       ; glob = (label type init)
              (? (two? en) `(<en 0 <>en) 'no)))
       (<e = 'deref)
        (: p (imaddr g <>e) (? (id? p 'no) 'no `(<p <>p (caup (cauup p)))))
       (<e = 'dot)
        (: b (imlval g <>e)
           (? (id? b 'no) 'no
              (: fm (fmem g <>>b <>>e) (? (bad? fm) 'no `(<b (<>b + <fm) <>fm)))))
       'no)
   ; a CONSTANT ADDRESS expression at image time -> (label byteoffset ptrtype) |
   ; 'no: a string / function / decayed-array global (offset 0), &lvalue (the
   ; lvalue's address), or pointer +/- an int constant (scaled by the pointee).
   ; a cast retypes (so a later + scales by the new pointee). subsumes what a
   ; pointer initializer used to take and adds every label+constant-offset form.
   (imaddr g e)
    (? (<e = 'cast)
        (: r (imaddr g <>>e)                        ; a cast RETYPES the address (its type is the cast's) --
           (? (id? r 'no) 'no `(<r <>r <>e)))   ; so a later + scales (or doesn't) by the cast type
       (<e = 'str)
        (: l (nlab g ".S") _ (pin g 'strs (link `(l <>e) (g 'strs))) `(l 0 `('ptr 'char)))
       (<e = 'var)
        (? (peep (g 'fns) <>e ()) `((intern <>e) 0 `('ptr (fnty g <>e)))
           (: en (peep (g 'glob) <>e 0)
              (? (! (two? en)) 'no
                 (: ty <>en (? (arr? ty) `(<en 0 `('ptr <>ty)) `(<en 0 `('ptr ty)))))))
       (<e = 'addr)
        (: lv (imlval g <>e) (? (id? lv 'no) 'no `(<lv <>lv `('ptr <>>lv))))
       (&& (<e = 'bin) (member? <>e '(+ -)))
        ; label +/- an int constant. a POINTER type scales the constant by the
        ; pointee (C pointer arithmetic); an integer-typed label ((word)&x + 8)
        ; adds the raw bytes -- so scale by the pointee ONLY when it's a pointer.
        (: op <>e
           (imscale ty) (? (ptr? ty) (tsz g <>ty) 1)
           pa (imaddr g <>>e) kb (icfold <>>>e)
           (? (&& (! (id? pa 'no)) (! (id? kb 'no)))
               `(<pa (<>pa + (* (? (op = '-) (0 - kb) kb) (imscale <>>pa))) <>>pa)
              (: pb (imaddr g <>>>e) ka (icfold <>>e)
                 (? (&& (&& (op = '+) (! (id? pb 'no))) (! (id? ka 'no)))
                     `(<pb (<>pb + (* ka (imscale <>>pb))) <>>pb)
                    'no))))
       'no)
   ; a const FLOAT expression at image time -> its gem | 'bad. the int cfold
   ; lives at parse (array dims); floats fold here where the image lays them
   ; (`static const double toint = 1/DBL_EPSILON;` -- fdlibm's opener).
   ; same IEEE doubles as the target, so the fold is the runtime value.
   (ffold e)
    (? (! (two? e)) 'bad
       (<e = 'flo) <>e
       (<e = 'num) (0.0 + <>e)
       (<e = 'cast) (? (flo? <>e) (ffold <>>e) 'bad)
       (<e = 'un)
        (: v (ffold <>>e)
           (? (bad? v) 'bad (<>e = 'neg) (0.0 - v) 'bad))
       (<e = 'bin)
        (: op <>e a (ffold <>>e) b (ffold <>>>e)
           (? (|| (bad? a) (bad? b)) 'bad
              (op = '+) (a + b) (op = '-) (a - b) (op = '*) (* a b)
              (op = '/) (? (b = 0.0) 'bad (/ a b))
              'bad))
       'bad)
   (fim ty v)                                       ; a float value's IEEE image as ty: 4 single bytes | 8 double bytes
    (? (ty = 'float) (les 4 (fbits32 v)) (les 8 (fbits v)))
   (imgbytes g ty ini)                              ; a byte/fix list of length tsz(ty) | 'bad
    (? (! (two? ini)) (zeros (tsz g ty))
       (<ini = 'flo) (? (flo? ty) (fim ty <>ini) 'bad)
       (<ini = 'num) (? (aggish? ty) 'bad
                             (flo? ty) (fim ty (0.0 + <>ini)) (imnum g ty <>ini))
       (&& (flo? ty) (member? <ini '(bin un cast)))   ; a const float expression: fold, then image
        (: v (ffold ini)
           (? (bad? v) 'bad (fim ty v)))
       (&& (<ini = 'str) (arr? ty))
        (: t <>ini n (tsz g ty)
           (bs i acc) (? (n <= i) (rev acc) (bs (i + 1) (link (? (i < (tally t)) (t i) 0) acc)))
           (bs 0 ()))
       (ptr? ty) (: r (imaddr g ini)                    ; a data pointer: a constant ADDRESS (word-sized abs label+offset),
                     (? (! (id? r 'no)) `(`('fix (wsize g) (? (t32? g) 'abs32 'abs64) <r <>r))   ; or a constant INTEGER ((int*)0x1000, a null)
                        (: v (icfold ini) (? (id? v 'no) 'bad (imnum g ty v)))))
       ; a label cast to a WORD-SIZED integer ((word)nif_absent, (intptr_t)f) is
       ; still the label's address; a non-label cast folds through to its value
       (&& (<ini = 'cast) (&& (! (aggish? ty)) (&& (! (flo? ty)) ((tsz g ty) = (wsize g)))))
        (: r (imaddr g ini)
           (? (! (id? r 'no)) `(`('fix (wsize g) (? (t32? g) 'abs32 'abs64) <r <>r)) (imgbytes g ty <>>ini)))
       (<ini = 'init)
        (: nini (unelide g ty ini)                     ; normalize elided braces (idempotent)
         (? (arr? ty)
            (: elt <>ty esz (tsz g elt)
               (place l idx acc)                     ; -> ((off items)..) | 'bad
                (? (! (two? l)) acc
                   (: it <l
                      ci (? (<it = 'didx) <>it idx)
                      eb (imgpad g elt esz (elval it))
                      (? (bad? eb) 'bad (place >l (ci + 1) (link `((* ci esz) eb) acc)))))
               ps (place <>nini 0 ())
               (? (bad? ps) 'bad (imflat (imsort ps) (* <>>ty esz))))
           (stru? ty)
            (: e (peep (g 'stag) <>ty 0)
               fields <e size <>e
               (place l cur acc)
                (? (! (two? l)) acc
                   (: it <l
                      ci (? (<it = 'dfield) (fidx fields <>it) cur)
                      fe (nthfield fields ci)
                      (? (bad? fe) 'bad
                         (: fb (imgpad g <>>fe (tsz g <>>fe) (elval it))
                            (? (bad? fb) 'bad (place >l (ci + 1) (link `(<>fe fb) acc)))))))
               ps (place <>nini 0 ())
               (? (bad? ps) 'bad (imflat (imsort ps) size)))
           'bad))
       'bad)
   (imgpad g ty w val)                              ; imgbytes padded (with zeros) to w bytes | 'bad
    (: b (imgbytes g ty val) (? (bad? b) 'bad (b + (zeros (w - (imbytelen b))))))
   (cgimage g ty ini)                               ; the full span-padded image | 'bad
    (: span (& ((tsz g ty) + ((wsize g) - 1)) (0 - (wsize g)))   ; round to the WORD (8 on x64/arm64, 4 on t32)
       raw (imgbytes g ty ini)
       (? (bad? raw) 'bad (raw + (zeros (span - (imbytelen raw))))))

   ; a declaration list ((name ty init)..): slots by SIZE (rounded to 8),
   ; extends env -> (forms env) | 'bad. aggregate + string locals zero the
   ; slot then fill; a scalar takes its value; only a label-valued global image
   ; refuses (4d).
   ; -- the locals-homing hook: every homable scalar local (outside an inline
   ; splice) ticks a decl SEQUENCE number, records (seq off ty) in g 'lrec
   ; (pass 1 reads it back to rank candidates), and asks g 'lhome (seq ->
   ; register, pinned only for a REGEN) for a home. a homed local keeps its
   ; slot as the spill shadow (g 'homes wraps it around calls, exactly a
   ; homed param), binds (reg rN) in the env -- so a READ is a zero-form
   ; borrow on the phase-1 contract, which is why this pays now where the
   ; r0-shuttle version lost -- and its init lands with a mov. the seq is
   ; deterministic across pass 1 and the regen (same walk; splices don't
   ; tick, and a mis-target only ever deopts through &-local's 'bad). --
   (cgdecl g env pairs)
    (: (one ps env2 acc)
        (? (! (two? ps)) `(acc env2)
           (: _ (psreset g)
              p <ps
              ty <>p
              init <>>p
              span (& ((tsz g ty) + 7) -8)
              off (nslot g span)
              hreg (? (|| (peep g 'inlining ()) (! (homable? ty))) ()
                      (: sq (1 + (peep g 'dseq 0))
                         _ (pin g 'dseq sq)
                         _ (pin g 'lrec (link `(sq off ty <p) (peep g 'lrec ())))
                         lht (peep g 'lhome ())
                         (? lht (peep lht sq ()) ())))
              env3 (link `(<p (? hreg `('reg hreg) off) ty) env2)
              _ (? hreg (pin g 'homes (link `(hreg off <p) (peep g 'homes ()))) ())
              (? (! (two? init)) (one >ps env3 acc)
                 (|| (<init = 'init) (&& (<init = 'str) (arr? ty)))
                  (? (&& (<init = 'init) (! (aggish? ty))) 'bad   ; a brace for a scalar
                     (: f (cgfill g env3 off ty init)
                        (? (bad? f) 'bad
                           (one >ps env3 (acc + (cgzero off span) + f)))))
                 (arr? ty) 'bad                          ; a bare-expr init of an array
                 (stru? ty)                              ; a struct local from a struct VALUE (an address): copy its bytes
                  (: r (cgexpr g env3 init)
                     (? (bad? r) 'bad
                        (: sz (tsz g ty)
                           cpf (: (go k acc9)
                                   (? (&& (8 <= (sz - k)) (! (t32? g)))
                                       (go (k + 8) (acc9 + `(`('ld 'r1 'r0 k) `('st 'r4 (off + k) 'r1))))
                                      (4 <= (sz - k)) (go (k + 4) (acc9 + `(`('ld4 'r1 'r0 k) `('st4 'r4 (off + k) 'r1))))
                                      (2 <= (sz - k)) (go (k + 2) (acc9 + `(`('ld2 'r1 'r0 k) `('st2 'r4 (off + k) 'r1))))
                                      (1 <= (sz - k)) (go (k + 1) (acc9 + `(`('ld1 'r1 'r0 k) `('st1 'r4 (off + k) 'r1))))
                                      acc9)
                                  (go 0 ()))
                           (one >ps env3 (acc + (tor0 g r) + cpf)))))
                 (: hint (? (&& (vmon? g) (&& (! hreg)
                              (&& (homable? ty) (member? <p (peep g 'vuniv ())))))
                            (ralloc g) ())               ; the vmap hint (asn's twin)
                    _ (? hint (pin g 'want hint) ())
                    r (cgexpr g env3 init)               ; a scalar (a pointer = string rides cgexpr -> la)
                    _ (? (&& hint (|| (bad? r) (! (id? (vreg r) hint)))) (rfree g hint) ())
                    _ (vmdrop g <p)
                    (? (bad? r) 'bad
                       (flo? ty) (one >ps env3 (acc + (asflo g r) + (fstf g ty 'r4 off 'f0)))
                       (pair? g ty) (one >ps env3 (acc + (asll g r) + (pst 'r4 off)))
                       hreg (one >ps env3           ; a homed local's init is a register move
                                 (acc + (asint g r) + `(`('mov hreg 'r0))))
                       (&& hint (&& (! (flo? (vty r)))
                           (member? (vreg r) (peep g 'pool0 ()))))   ; hint set = vmap on + universe local
                        (: _ (vmpin g <p (vreg r))       ; the init value stays resident
                           (one >ps env3 (acc + (vfs r) + `(`('st 'r4 off (vreg r))))))
                       (one >ps env3
                            (cat acc (cat (asint g r)
                                     (cat (cvt ty 'r0)
                                          `(`((stop ty) 'r4 off 'r0)))))))))))
       _ (stq g)                                     ; one statement tick for the whole decl
       (one pairs env ()))

   ; block-scope `static` ((name ty init)..): each name gets a UNIQUE global
   ; laid in .bss/.data (persists across calls, one instance), NOT a stack slot.
   ; the name binds in the LOCAL env to a (glob lbl) marker (so it shadows outer
   ; names + is distinct per function), and vent turns that into a 'glo the same
   ; read/write/& paths a file-scope global already use. no runtime init code --
   ; a static's initializer is a compile-time constant image (cgdata emits it via
   ; cgimage, exactly like a file-scope global; a non-constant init -> 'bad there).
   ; -> (forms env) | 'bad, the same shape as cgdecl.
   (cgsdecl g env pairs)
    (: (one ps env2)
        (? (! (two? ps)) `(() env2)
           (: p <ps
              name <p ty <>p init <>>p
              lbl (nlab g ((g 'fn) + "."))           ; `fn.N` -- the `.` bars any C-identifier collision
              _ (pin (g 'glob) lbl `(lbl ty init))   ; a LOCAL data label (internal linkage): g 'statics,
              _ (pin g 'statics (link lbl (g 'statics)))  ; NOT g 'globs -- a static is never an exported symbol
              (one >ps (link `(name `('glob lbl) ty) env2))))
       _ (stq g)                                     ; the statement tick (alive numbers sdecls too)
       (one pairs env))

   ; stmt -> forms | 'bad
   ; -- inline asm (the 'asm statement): the template is holo neutral TEXT
   ; (crew/holo/text.l's asm-text parses it; the cats carry text.l), %0..%9 the
   ; operands (outputs first, GNU numbering), %% a literal %. the body assembles
   ; to bytes RIGHT HERE and splices as ONE opaque ('raw ..): the IR passes
   ; barrier on raw, template labels stay local to the splice, and arm64's
   ; a4ize never rewrites inside it. operands stage through the machine stack
   ; (spush/spop 16-byte cells -- a call inside an operand expr stays aligned):
   ; every input evaluates and pushes, then pops into its register; outputs
   ; push after the body and store back through clval in reverse, so the store
   ; scratch (r0 the address, r1 the value) can never trample an unstored
   ; output. constraints: "=r"/"+r"/"r" pick a register, "=rN"/"+rN"/"rN" pin
   ; one, "i" an immediate (a parse-time constant); clobbers "memory"/"cc"/reg
   ; names -- validated only: an asm fn's homing is OFF (g 'hasasm, the 'sys
   ; precedent), so nothing lives in a register across the statement. x64
   ; refuses r4 (the frame) and the callee-saved r11-r14; r3 rides every
   ; prologue's -8 slot; arm64 adds r4 (x4, an argument register there). --
   asmregs '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10)
   asmpick '(r10 r9 r8 r7 r6 r5 r2 r1 r0)
   asmnames (: t (tablet 0)
               _ (foldl (\ _ i (: s9 ("r" + (show i)) _ (pin t (intern s9) s9) 0)) 0 (jot 11))
               t)
   (asmreg? g r9) (|| (member? r9 asmregs) (&& (a64? g) (id? r9 'r4)))
   ; a constraint's register part: "r" -> 'pick, "i" -> 'imm, "rN" -> the nom
   (asmc c9) (: n (tally c9)
                (? (n < 1) 'bad
                   (n = 1) (? ((c9 0) = 114) 'pick ((c9 0) = 105) 'imm 'bad)
                   (intern c9)))
   ; %-substitute the template: al the operand texts, GNU order
   (asmsub tm al)
    (: n (tally tm)
       (go i acc)
        (? (n <= i) acc
           (: c (tm i)
              (? (! (c = 37)) (go (i + 1) (acc + c))
                 ((i + 1) < n)
                  (: d (tm (i + 1))
                     (? (d = 37) (go (i + 2) (acc + 37))
                        (&& (48 <= d) (d <= 57))
                         (: ix (d - 48)
                            (? (ix < (tally al)) (go (i + 2) (acc + (nthr al ix))) 'bad))
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
        (? (! (two? l)) (rev acc)
           (: c (cap <l) n (tally c)
              h (? (0 < n) (c 0) 0)
              k (? (|| (h = 61) (h = 43)) (asmc (snip c 1 n)) 'bad)
              (? (|| (id? k 'bad) (id? k 'imm)) 'bad
                 (oconv >l (link `((? (h = 43) 'x 'o) k (caup <l)) acc)))))
       (iconv l acc)
        (? (! (two? l)) (rev acc)
           (: k (asmc (cap <l)) e (caup <l)
              (? (id? k 'bad) 'bad
                 (id? k 'imm)
                  (? (&& (two? e) (&& (id? <e 'num) (charm? <>e)))
                     (iconv >l (link `('m <>e) acc)) 'bad)
                 (iconv >l (link `('i k e) acc)))))
       os (oconv <>>s ())
       is9 (iconv <>>>s ())
       (? (|| (bad? os) (bad? is9)) 'bad
          (: ops9 (os + is9)
             ; pinned registers: validated, a duplicate refuses
             (pinset l acc)
              (? (! (two? l)) acc
                 (: e2 <l k (caup e2)
                    (? (|| (id? <e2 'm) (id? k 'pick)) (pinset >l acc)
                       (! (asmreg? g k)) 'bad
                       (member? k acc) 'bad
                       (pinset >l (link k acc)))))
             pinned (pinset ops9 ())
             (? (bad? pinned) 'bad
                (: (assign l fr acc)
                    (? (! (two? l)) (rev acc)
                       (: e2 <l k (caup e2)
                          (? (id? <e2 'm) (assign >l fr (link k acc))
                             (id? k 'pick)
                              (? (two? fr) (assign >l >fr (link <fr acc)) 'bad)
                             (assign >l fr (link k acc)))))
                   asgn (assign ops9 (filter (\ r9 (! (member? r9 pinned))) asmpick) ())
                   (cok? l) (? (! (two? l)) 1
                               (: c <l
                                  (? (|| (c = "memory") (c = "cc")) (cok? >l)
                                     (asmreg? g (intern c)) (cok? >l)
                                     ())))
                   (? (|| (bad? asgn) (! (cok? (cauup (cuup s))))) 'bad
                      (: txt (asmsub tmpl (map (\ a9 (? (charm? a9) (show a9) (peep asmnames a9 "r?"))) asgn))
                         (? (bad? txt) 'bad
                            (: ; inputs (and in-out outputs) evaluate and PUSH..
                               (loads l acc)
                                (? (! (two? l)) acc
                                   (: e2 <l
                                      (? (member? <e2 '(x i))
                                          (: r (cgexpr g env (cauup e2))
                                             (? (bad? r) 'bad
                                                (|| (flo? (vty r)) (stru? (vty r))) 'bad
                                                (loads >l (acc + (tor0 g r) + (spush 'r0)))))
                                         (loads >l acc))))
                               lf (loads ops9 ())
                               (? (bad? lf) 'bad
                                  (: ; ..then POP into their registers, reverse order
                                     (pops l al acc)
                                      (? (! (two? l)) acc
                                         (? (member? (cap <l) '(x i))
                                            (pops >l >al ((spop <al) + acc))
                                            (pops >l >al acc)))
                                     ; the body: text -> IR forms -> BYTES, one opaque raw
                                     body `(`('raw (asmhb (peep g 'tgt 'x64) (asm-text txt))))
                                     ; outputs: push each register, store back reverse
                                     (opush l al acc)
                                      (? (! (two? l)) acc
                                         (? (member? (cap <l) '(o x))
                                            (opush >l >al (acc + (spush <al)))
                                            (opush >l >al acc)))
                                     (ostores l al acc)
                                      (? (! (two? l)) acc
                                         (: e2 <l
                                            (? (! (member? <e2 '(o x))) (ostores >l >al acc)
                                               (: lv (clval g env (cauup e2))
                                                  (? (bad? lv) 'bad
                                                     (: ty (vty lv)
                                                        (? (|| (flo? ty) (|| (stru? ty) (|| (arr? ty) (bitf? ty)))) 'bad
                                                           (ostores >l >al
                                                            (((vfs lv) + (spop 'r1) + `(`((stop ty) 'r0 0 'r1))) + acc)))))))))
                                     osf (ostores ops9 asgn ())
                                     (? (bad? osf) 'bad
                                        (lf + (pops ops9 asgn ()) + body + (opush ops9 asgn ()) + osf)))))))))))))

   ; -- alive: which homable names still matter across each call (the homing
   ; regens' liveness). one backward statement-grain walk over the fn body,
   ; sets tiny name lists over a universe fixed up front: homable params +
   ; homable locals, minus any address-taken or multiply-declared name (two
   ; vars sharing a name must not conflate -- both keep the blanket wrap).
   ; a statement whose own expressions call records wrapset = livein u liveout
   ; under a statement counter that ticks EXACTLY as the regen's cgstmt/cgdecl/
   ; cgsdecl entries do (stq below, the dseq protocol: a splice never ticks,
   ; reset per build); the walk numbers DOWN from the prepass total, so the
   ; child-reversed backward order lands forward pre-order numbers, and a
   ; livtab miss reads 'all (the blanket wrap): any desync degrades, never
   ; miscompiles. loops seed with everything the loop reads (the back edge,
   ; no fixpoint -- a post-back-edge read is textually in the loop; a goto
   ; ESCAPES the loop, so as a loop use it reads the whole universe); a label
   ; is a backward no-op (what's live there is what's read after it).
   ; answers (livtab cross univ): livtab sq -> wrapset, cross name -> static
   ; call crossings (a universe member at cross 0 rides every call for free),
   ; univ the analyzed names -- a gate must never trust cross for an outsider.
   (alive g f)
    (: params <>>f
       items (caup (caup (cuup f)))
       dcl (tablet 0) tak (tablet 0) n9 (tablet 0)
       liv (tablet 0) crs (tablet 0)
       ; -- prepass: &-taken names, one generic sweep (sizeof's unevaluated
       ; &v over-records -- harmless, the name just keeps the blanket) --
       (pex e) (? (! (two? e)) ()
                  (: _ (? (&& (<e = 'addr) (&& (two? <>e) ((cap (caup e)) = 'var)))
                          (pin tak (caup (caup e)) 1) ())
                     (pexl >e)))
       (pexl l) (? (! (two? l)) () (: _ (pex <l) (pexl >l)))
       ; -- prepass: the tick total, declaration counts, homable locals
       ; (ticks mirror lst's below, statement for statement) --
       (pre s)
        (? (! (two? s)) ()
           (: k <s
              _ (pin n9 'n (1 + (peep n9 'n 0)))
              (? (member? k '(decl sdecl))
                  (: (go ps) (? (! (two? ps)) ()
                                (: p <ps
                                   _ (pin dcl <p (1 + (peep dcl <p 0)))
                                   _ (? (&& (k = 'decl) (homable? <>p))
                                        (pin n9 'loc (link <p (peep n9 'loc ()))) ())
                                   (go >ps)))
                     (go <>s))
                 (k = 'blk) (prel <>s)
                 (k = 'if) (: _ (pre <>>s) (? (two? <>>>s) (pre <>>>s) ()))
                 (k = 'while) (pre <>>s)
                 (k = 'do) (pre <>s)
                 (k = 'for) (: ini <>s
                               _ (? (&& (two? ini) (member? <ini '(decl sdecl))) (pre ini) ())
                               (pre (cauup (cuup s))))
                 (k = 'switch) (prel <>>s)
                 ())))
       (prel l) (? (! (two? l)) () (: _ (pre <l) (prel >l)))
       (pdcl ps) (? (! (two? ps)) ()
                    (: _ (pin dcl (cap (cap ps)) (1 + (peep dcl (cap (cap ps)) 0)))
                       (pdcl >ps)))
       ; -- the universe: nhomable's gp walk for the param NAMES, plus the locals --
       (pgo ps gpi acc)
        (? (! (two? ps)) acc
           (: pty (caup (cap ps))
              (? (|| (flo? pty) (|| (sse2? g pty) (id? (aone g pty) 'sse))) (pgo >ps gpi acc)
                 ((nhome g) <= gpi) acc
                 (homable? pty) (pgo >ps (gpi + 1) (link (cap (cap ps)) acc))
                 (pgo >ps (gpi + 1) acc))))
       _ (pdcl params)
       _ (prel items)
       _ (pexl items)
       univ (filter (\ nm (&& ((peep dcl nm 0) = 1) (! (peep tak nm ()))))
                    ((pgo params 0 ()) + (peep n9 'loc ())))
       ; a homable param OUTSIDE the universe (shadowed, or a sizeof-& over-
       ; record) still HOMES -- it must ride EVERY wrapset (its blanket), or
       ; the filter would strip its one true spill
       pales (filter (\ nm (! (member? nm univ))) (pgo params 0 ()))
       ; -- the set kit (names are strings; member? compares content) --
       (sadd s2 nm) (? (member? nm s2) s2 (link nm s2))
       (sun s2 b) (? (! (two? b)) s2 (sun (sadd s2 <b) >b))
       (sdel s2 nm) (? (! (two? s2)) ()
                       (<s2 = nm) >s2
                       (link <s2 (sdel >s2 nm)))
       (tick u) (: q (peep n9 'q 0) _ (pin n9 'q (q - 1)) q)
       (rec sq w ncl)                               ; a call-bearing statement's record
        (? (ncl < 1) ()
           (: _ (pin liv sq (sun w pales))          ; the outside-universe homes ride every wrap
              (bump l) (? (! (two? l)) ()
                          (: _ (pin crs <l (ncl + (peep crs <l 0))) (bump >l)))
              (bump w)))
       ; -- reads of an expression, restricted to the universe -> (set ncalls).
       ; a var WRITE is not a read (asn to a var recurses the rhs alone; the
       ; home register takes the value directly, the slot is only ever read by
       ; wrap-emitted reloads); sizeof's operand never evaluates; an unknown
       ; head reads EVERYTHING and counts a call (future grammar fails safe) --
       (erd e s2 nc)
        (? (! (two? e)) `(s2 nc)
           (: k <e
              (? (k = 'var) `((? (member? <>e univ) (sadd s2 <>e) s2) nc)
                 (member? k '(num flo str szof)) `(s2 nc)
                 (member? k '(dot deref addr post vastart vaend vaarg)) (erd <>e s2 nc)
                 (member? k '(un cast)) (erd <>>e s2 nc)
                 (k = 'asn) (: r (erd <>>e s2 nc)
                               (? ((cap (caup e)) = 'var) r (erd <>e <r <>r)))
                 (k = 'bin) (: r (erd <>>e s2 nc) (erd <>>>e <r <>r))
                 (member? k '(land lor comma)) (: r (erd <>e s2 nc) (erd <>>e <r <>r))
                 (k = 'cond) (: r (erd <>e s2 nc) r2 (erd <>>e <r <>r) (erd <>>>e <r2 <>r2))
                 (k = 'call) (: r (erd <>e s2 (nc + 1)) (erdl <>>e <r <>r))
                 (k = 'clit) (erdi <>>e s2 nc)
                 (k = 'init) (erdi <>e s2 nc)
                 `((sun s2 univ) (nc + 1)))))
       (erdl l s2 nc) (? (! (two? l)) `(s2 nc) (: r (erd <l s2 nc) (erdl >l <r <>r)))
       (erdi l s2 nc)                               ; init items: dfield/didx wrap their values
        (? (! (two? l)) `(s2 nc)
           (: it <l
              v (? (&& (two? it) (member? <it '(dfield didx))) <>>it it)
              r (erd v s2 nc)
              (erdi >l <r <>r)))
       ; -- a loop's uses: every name its cond/step/body read, kills ignored
       ; (the back-edge seed); a goto inside escapes -> the whole universe --
       (ust s acc)
        (? (! (two? s)) acc
           (: k <s
              (? (k = 'blk) (ustl <>s acc)
                 (k = 'expr) (cap (erd <>s acc 0))
                 (k = 'ret) (? (two? <>s) (cap (erd <>s acc 0)) acc)
                 (member? k '(decl sdecl)) (ustp <>s acc)
                 (k = 'if) (: a1 (cap (erd <>s acc 0))
                              a2 (ust <>>s a1)
                              (? (two? <>>>s) (ust <>>>s a2) a2))
                 (k = 'while) (ust <>>s (cap (erd <>s acc 0)))
                 (k = 'do) (ust <>s (cap (erd <>>s acc 0)))
                 (k = 'for) (: ini <>s
                               a1 (? (! (two? ini)) acc
                                     (member? <ini '(decl sdecl)) (ustp <>ini acc)
                                     (cap (erd (caup ini) acc 0)))
                               a2 (? (two? <>>s) (cap (erd <>>s a1 0)) a1)
                               a3 (? (two? <>>>s) (cap (erd <>>>s a2 0)) a2)
                               (ust (cauup (cuup s)) a3))
                 (k = 'switch) (ustl <>>s (cap (erd <>s acc 0)))
                 (member? k '(goto asm)) (sun acc univ)
                 acc)))
       (ustl l acc) (? (! (two? l)) acc (ustl >l (ust <l acc)))
       (ustp ps acc) (? (! (two? ps)) acc (ustp >ps (cap (erd (cauup (cap ps)) acc 0))))
       ; -- the backward walk: statements in reverse, children before their
       ; parent's tick (counting down = forward pre-order numbers). w9 is the
       ; LOOP WEIGHT (x8 per nesting level): a crossing inside a loop pays its
       ; wrap per iteration while the touch count stays static, so cross
       ; charges it heavier -- the gate stays honest where the static-wrap
       ; variant lost. wrapsets (soundness) never depend on w9. --
       (litems l out brk cnt w9)
        (: (go l2 o) (? (! (two? l2)) o (go >l2 (lst <l2 o brk cnt w9)))
           (go (rev l) out))
       (dpairs ps o nc)                             ; reversed decl pairs -> (in ncalls)
        (? (! (two? ps)) `(o nc)
           (: p <ps
              r (erd (cauup p) () 0)
              o2 (sun (? (member? <p univ) (sdel o <p) o) <r)
              (dpairs >ps o2 (nc + <>r))))
       (lst s out brk cnt w9)
        (? (! (two? s)) out
           (: k <s
              (? (k = 'blk) (: in (litems <>s out brk cnt w9) _ (tick ()) in)
                 (k = 'expr)
                  (: e9 <>s
                     kv (? (&& ((cap e9) = 'asn)
                             (&& ((cap (caup e9)) = 'var)
                                 (member? (caup (caup e9)) univ)))
                           (caup (caup e9)) ())
                     r (erd e9 () 0)
                     in (sun (? kv (sdel out kv) out) <r)
                     ; the KILL rides the record too: a name born in a call-bearing
                     ; statement is invisible to in/out yet lives across the
                     ; statement's OWN calls (m = llen(a), n = llen(b) -- m crosses
                     ; the second call inside one tick). statement-grain cannot
                     ; order intra-statement events, so the kill joins the set.
                     sq (tick ()) _ (rec sq (sun (sun in out) (? kv `(kv) ())) (* <>r w9))
                     in)
                 (k = 'ret)
                  (: r (? (two? <>s) (erd <>s () 0) '(() 0))
                     sq (tick ()) _ (rec sq <r (* <>r w9))
                     <r)
                 (k = 'decl)
                  (: pr (dpairs (rev <>s) out 0)
                     ks (: (go ps) (? (! (two? ps)) ()   ; the declared names: kills, in the record (see expr)
                                      (: nm (cap (cap ps))
                                         (? (member? nm univ) (link nm (go >ps)) (go >ps))))
                           (go <>s))
                     sq (tick ()) _ (rec sq (sun (sun <pr out) ks) (* <>pr w9))
                     <pr)
                 (k = 'sdecl) (: _ (tick ()) out)   ; statics: no runtime init, no kill
                 (k = 'if)
                  (: inE (? (two? <>>>s) (lst <>>>s out brk cnt w9) out)   ; else FIRST (reverse order)
                     inT (lst <>>s out brk cnt w9)
                     r (erd <>s () 0)
                     in (sun (sun inT inE) <r)
                     sq (tick ()) _ (rec sq (sun in out) (* <>r w9))
                     in)
                 (k = 'while)
                  (: w8 (* 8 w9)
                     seed (sun out (ust <>>s (cap (erd <>s () 0))))
                     _ (lst <>>s seed out seed w8)
                     r (erd <>s () 0)
                     sq (tick ()) _ (rec sq seed (* <>r w8))
                     seed)
                 (k = 'do)
                  (: w8 (* 8 w9)
                     seed (sun out (ust <>s (cap (erd <>>s () 0))))
                     r (erd <>>s () 0)
                     _ (lst <>s seed out seed w8)
                     sq (tick ()) _ (rec sq seed (* <>r w8))
                     seed)
                 (k = 'for)
                  (: w8 (* 8 w9)
                     ini <>s cnd <>>s stp <>>>s body (cauup (cuup s))
                     useb (: u1 (? (two? cnd) (cap (erd cnd () 0)) ())
                             u2 (? (two? stp) (cap (erd stp u1 0)) u1)
                             (ust body u2))
                     seed (sun out useb)
                     _ (lst body seed out seed w8)
                     nc9 (+ (? (two? cnd) (caup (erd cnd () 0)) 0)
                            (? (two? stp) (caup (erd stp () 0)) 0))
                     ri (? (&& (two? ini) (! (member? <ini '(decl sdecl))))
                           (erd (caup ini) () 0) '(() 0))
                     kv8 (? (&& (two? ini) (! (member? <ini '(decl sdecl))))
                            (: e8 (caup ini)
                               (? (&& ((cap e8) = 'asn)
                                     (&& ((cap (caup e8)) = 'var)
                                         (member? (caup (caup e8)) univ)))
                                  (caup (caup e8)) ()))
                            ())
                     in (? (! (two? ini)) seed
                           (member? <ini '(decl sdecl)) (lst ini seed () () w9)   ; ticks (cgdecl does); kill applies; runs ONCE
                           (sun (? kv8 (sdel seed kv8) seed) <ri))
                     ; kv8 joins the record: the init's kill (see the expr lane)
                     sq (tick ()) _ (rec sq (sun (sun seed <ri) (? kv8 `(kv8) ())) (+ (* nc9 w8) (* <>ri w9)))
                     in)
                 (k = 'switch)
                  (: r (erd <>s () 0)
                     _ (pin n9 'sw (link () (peep n9 'sw ())))   ; push this switch's marker set
                     _ (litems <>>s out out cnt w9)              ; brk = out; cont targets the outer loop
                     macc (cap (peep n9 'sw ()))
                     _ (pin n9 'sw (cup (peep n9 'sw ())))       ; pop
                     in (sun (sun <r macc) out)
                     sq (tick ()) _ (rec sq in (* <>r w9))
                     in)
                 (member? k '(case dflt))
                  (: sw (peep n9 'sw ())
                     _ (? (two? sw) (pin n9 'sw (link (sun <sw out) >sw)) ())
                     _ (tick ()) out)
                 (k = 'brk) (: _ (tick ()) brk)
                 (k = 'cont) (: _ (tick ()) cnt)
                 (k = 'goto) (: _ (tick ()) univ)   ; any target: everything
                 (k = 'lbl) (: _ (tick ()) out)
                 (k = 'asm) (: sq (tick ()) _ (rec sq univ w9) univ)   ; homing is off anyway (hasasm)
                 (: _ (tick ()) out))))             ; nop tdef
       ; -- run: liveout at the fn end is empty; the counter descends from the
       ; prepass total so lst's numbers land forward pre-order --
       _ (pin n9 'q (peep n9 'n 0))
       _ (litems items () () () 1)
       `(liv crs univ))
   ; the alive pass's forward twin: cgstmt/cgdecl/cgsdecl entries tick this
   ; counter and pin the statement's wrapset (g 'stwrap) off g 'livtab -- a
   ; miss (or no livtab at all) pins 'all, the blanket wrap. an inline splice
   ; neither ticks nor re-pins: it compiles under its statement's set.
   (stq g)
    (? (peep g 'inlining ()) (peep g 'stwrap 'all)
       (: q (1 + (peep g 'lsq 0))
          _ (pin g 'lsq q)
          lt (peep g 'livtab ())
          w (? lt (peep lt q 'all) 'all)
          _ (pin g 'stwrap w)
          w))

   (cgstmt g env s)
    (: _ (psreset g)                                 ; a statement never carries a live operand in (vmap pins do)
       myw (stq g)                                   ; this statement's wrapset (alive's forward twin)
       k <s
       (? (k = 'ret)                                                     ; inside an inline splice a return JUMPS to the
          (: il (peep g 'inlret ())                                      ; splice's end label instead of the epilogue
             epj (peep g 'epi epi)                                       ; the TARGET's epilogue (cgen-core pins it)
             (? (! (two? <>s)) (? il `(`('jmp il)) epj)           ; bare `return;` -- just the epilogue
                (: _ (pin g 'want 'r0)                                   ; the return value's one true home
                   r (cgexpr g env <>s)
                   rt (? il (peep g 'inlrt 'long)                        ; the INLINED function's return type
                         (cap (peep (g 'sigs) (g 'fn) `('long ()))))    ; else the current function's
                   (? (bad? r) 'bad
                      (flo? rt)                                        ; a double return leaves f0 (xmm0; never inline -- gated);
                       ((asflo g r)                                    ; v6-M base ABI carries a double out on the r0:r1 pair,
                        + (? (! (v6m? g)) ()                           ; a bare float as ONE WORD (binary32 in r0)
                             (id? rt 'float) `(`('cvtsd2ss 'f0 'f0) `('movqrx 'r0 'f0))
                             `(`('movqrx 'r0 'f0)))
                        + epj)
                      (&& (v6m? g) (stru? rt))             ; v6-M composite returns: >4B fills the caller's temp
                       (? (4 < (tsz g rt))                 ; through the saved sret pointer (MEMORY, the base ABI);
                          ((tor0 g r)                      ; <=4B rides its word in r0
                           + `(`('ld 'r1 'r4 (peep g 'sret 0)))
                           + (bcopy g 0 (tsz g rt) ()) + epj)
                          ((tor0 g r) + `(`('ld 'r0 'r0 0)) + epj))
                      (&& (t32? g) (&& (stru? rt)          ; t32 returns: the d-pair HFA, the 8B sse blob (d0),
                          (! (|| (sse2? g rt)              ; and the <=4B int one (r0); mixed/int-pair refuse LOUD
                                 (|| (&& (id? (aone g rt) 'sse) ((tsz g rt) = 8))
                                     (&& (id? (aone g rt) 'int) ((tsz g rt) <= 4))))))) 'bad
                      (sse2? g rt)                                       ; a two-double struct returns as the xmm PAIR
                       (cat (tor0 g r) (cat (fldf 'double (xmmreg 0) 'r0 0)
                                       ((fldf 'double (xmmreg 1) 'r0 8) + epj)))
                      (ipair? g rt)                                      ; a two-int struct returns as rax:rdx (arm64 x0:x1)
                       ((tor0 g r) + `(`('ld (ip2 g) 'r0 8) `('ld 'r0 'r0 0)) + epj)
                      (id? (aone g rt) 'sse)                             ; a small SSE aggregate: its eightbyte in xmm0
                       ((tor0 g r) + (fldf 'double (xmmreg 0) 'r0 0) + epj)
                      (id? (aone g rt) 'int)                             ; a small INT aggregate (a union rides here): rax
                       ((tor0 g r) + `(`('ld 'r0 'r0 0)) + epj)
                      (|| (stru? rt) (stru? (vty r))) 'bad               ; a MIXED 16-byte pair or >16 bytes (MEMORY): still refused
                      (flo? (vty r)) ((asint g r) + (? il `(`('jmp il)) epj))   ; a double value, an int return -> truncate to r0
                      (pair? g rt) ((asll g r) + (? il `(`('jmp il)) epj))      ; a pair return: r0:r1 (widen a word value)
                      ((tor0 g r) + (? il `(`('jmp il)) epj))))))    ; int -> int, as before (a pair value narrows to its lo)
          (k = 'expr)
           (: pe <>s
              rv (? (&& (<pe = 'post) ((cap (caup pe)) = 'var))
                    (vent g env (caup (caup pe))) 'bad)
              (? (&& (two? rv) (<rv = 'regv))   ; a statement-position x++ on a home: the old value is dead
                  (: ty <>>rv stp (? (ptr? ty) (tsz g <>ty) 1)
                     `(`('add <>rv <>rv (* stp <>>pe))))
                 (: r (cgexpr g env pe)
                    (? (bad? r) 'bad (vdrop g r)))))
          (k = 'nop) ()
          (k = 'tdef) ()                            ; a block-scope _Static_assert (already checked at parse)
          (k = 'blk) (cgitems g env <>s ())
          (k = 'if)
           (: le (nlab g ".e")
              c (cbranch g env <>s 'eq le)
              t (cgstmt g env <>>s)
              _ (vmflush g)                            ; label le joins
              (? (|| (bad? c) (bad? t)) 'bad
                 (! (two? <>>>s))
                  (c + t + `(`('label le)))
                 (: el (cgstmt g env <>>>s)
                    ld (nlab g ".d")
                    _ (vmflush g)                      ; label ld joins
                    (? (bad? el) 'bad
                       (cat c
                        (cat t (cat `(`('jmp ld) `('label le))
                         (el + `(`('label ld))))))))))
          (k = 'while)
           (: lc (nlab g ".c") ld (nlab g ".d")
              _ (vmflush g)                            ; label lc heads the loop block
              c (cbranch g env <>s 'eq ld)
              _ (pin g 'brk (link ld (g 'brk)))
              _ (pin g 'cnt (link lc (g 'cnt)))
              b (cgstmt g env <>>s)
              _ (pin g 'brk (cup (g 'brk)))
              _ (pin g 'cnt (cup (g 'cnt)))
              _ (vmflush g)                            ; label ld joins
              (? (|| (bad? c) (bad? b)) 'bad
                 (cat `(`('label lc))
                  (cat c
                   (b + `(`('jmp lc) `('label ld)))))))
          (k = 'for)
           (: init <>s cnd <>>s stp <>>>s body (cauup (cuup s))
              lc (nlab g ".c") ls (nlab g ".s") ld (nlab g ".d")
              ir (? (! (two? init)) `(() env)
                    (<init = 'decl) (cgdecl g env <>init)
                    (<init = 'sdecl) (cgsdecl g env <>init)
                    (: r (cgexpr g env <>init)
                       (? (bad? r) 'bad `((vdrop g r) env))))
              (? (bad? ir) 'bad
                 (: env2 <>ir
                    _ (pin g 'stwrap myw)            ; a decl init re-pinned; the cond is OURS
                    _ (vmflush g)                    ; label lc heads the loop block
                    c (? (two? cnd) (cbranch g env2 cnd 'eq ld) ())
                    _ (pin g 'brk (link ld (g 'brk)))
                    _ (pin g 'cnt (link ls (g 'cnt)))
                    b (cgstmt g env2 body)
                    _ (pin g 'brk (cup (g 'brk)))
                    _ (pin g 'cnt (cup (g 'cnt)))
                    _ (pin g 'stwrap myw)            ; the body re-pinned as it went; the step is OURS
                    _ (vmflush g)                    ; label ls heads the step block
                    st2 (? (two? stp) (cgexpr g env2 stp) ())
                    _ (vmflush g)                    ; label ld joins
                    (? (|| (bad? c) (|| (bad? b) (bad? st2))) 'bad
                       (cat <ir
                        (cat `(`('label lc))
                         (cat (? (two? cnd) c ())
                          (cat b
                           (cat `(`('label ls))
                            (cat (? (two? st2) (vdrop g st2) ())
                                 `(`('jmp lc) `('label ld))))))))))))
          (k = 'brk) (? (two? (g 'brk)) `(`('jmp (cap (g 'brk)))) 'bad)
          (k = 'cont) (? (two? (g 'cnt)) `(`('jmp (cap (g 'cnt)))) 'bad)
          (k = 'do)
           (: lt (nlab g ".t") lc (nlab g ".c") ld (nlab g ".d")
              _ (pin g 'brk (link ld (g 'brk)))
              _ (pin g 'cnt (link lc (g 'cnt)))
              _ (vmflush g)                           ; label lt heads the loop block
              b (cgstmt g env <>s)
              _ (pin g 'brk (cup (g 'brk)))
              _ (pin g 'cnt (cup (g 'cnt)))
              _ (pin g 'stwrap myw)                  ; the body re-pinned as it went; the cond is OURS
              _ (vmflush g)                          ; label lc heads the cond block
              c (cbranch g env <>>s 'ne lt)
              _ (vmflush g)                          ; label ld joins
              (? (|| (bad? b) (bad? c)) 'bad
                 (cat `(`('label lt))
                  (cat b
                   (cat `(`('label lc))
                    (c + `(`('label ld))))))))
          (k = 'goto) `(`('jmp (intern ((g 'fn) + "." + <>s))))
          (k = 'lbl) (: _ (vmflush g) `(`('label (intern ((g 'fn) + "." + <>s)))))
          (k = 'asm) (cgasm g env s)
          ; a case/default label -- anywhere in the body, tied to the nearest enclosing
          ; switch by VALUE. the switch's pre-scan (below) pinned value->label into the
          ; tablet at the top of the g 'cases stack; here we just emit that label. a
          ; case/default with no enclosing switch (empty stack) or an unscanned value
          ; refuses rather than mislabel.
          (k = 'case)
           (? (! (two? (g 'cases))) 'bad
              (: lab (peep (cap (g 'cases)) <>s 'no) _ (vmflush g)
                 (? (lab = 'no) 'bad `(`('label lab)))))
          (k = 'dflt)
           (? (! (two? (g 'cases))) 'bad
              (: lab (peep (cap (g 'cases)) 'dflt 'no) _ (vmflush g)
                 (? (lab = 'no) 'bad `(`('label lab)))))
          (k = 'switch)
           (: c (cgexpr g env <>s)
              (? (bad? c) 'bad
                 (: items <>>s
                    cr9 (vreg c)
                    lend (nlab g ".d")
                    tab (tablet 0)               ; this switch's value->label map (and 'dflt->label)
                    cases (rev (scasel-l g items tab ()))   ; pre-scan the body (a stmt LIST) at ANY depth -> (value label) dispatch pairs
                    dflt (peep tab 'dflt lend)
                    disp (: (go l acc)
                             (? (! (two? l)) acc
                                (: e2 <l  v (cap e2)  lab (caup e2)
                                   ins (? (immok g 'cmp v)
                                          `(`('cmp cr9 v) `('br 'eq lab))
                                          `(`('li 'r1 v) `('cmp cr9 'r1) `('br 'eq lab)))
                                   (go >l (cat acc ins))))
                            (go cases ()))
                    _ (rfree g cr9)
                    _ (pin g 'brk (link lend (g 'brk)))
                    _ (pin g 'cases (link tab (g 'cases)))   ; PUSH -- cgstmt emits each case/dflt label from here
                    body (cgitems g env items ())            ; decls thread env; case/dflt labels emit via g 'cases
                    _ (pin g 'brk (cup (g 'brk)))
                    _ (pin g 'cases (cup (g 'cases)))         ; POP
                    _ (vmflush g)                             ; label lend joins
                    (? (bad? body) 'bad
                       (cat (vfs c)
                        (cat disp
                         (cat `(`('jmp dflt))
                          (body + `(`('label lend))))))))))
          'bad))
   (cgitems g env items acc)
    (? (! (two? items)) acc
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

   ; scan a switch body for its case/default labels at ANY depth, pinning value->label
   ; (and 'dflt->label) into `tab` and returning the (value label) pairs for the
   ; dispatch. recurses through blocks / if / loops but STOPS at a nested switch (whose
   ; cases belong to IT) -- so a case ties to its NEAREST enclosing switch, per C. the
   ; container shapes mirror cgstmt's, so scasel finds exactly the labels cgstmt emits.
   (scasel g it tab acc)
    (? (! (two? it)) acc
       (: k <it
          (? (k = 'case)  (: lab (nlab g ".k") _ (pin tab <>it lab) (link (L <>it lab) acc))
             (k = 'dflt)  (: lab (nlab g ".k") _ (pin tab 'dflt lab) acc)
             (k = 'blk)   (scasel-l g <>it tab acc)               ; ('blk items)
             (k = 'if)    (scasel g <>>>it tab (scasel g <>>it tab acc))  ; then + else
             (k = 'while) (scasel g <>>it tab acc)                ; ('while cond body)
             (k = 'do)    (scasel g <>it tab acc)                 ; ('do body cond)
             (k = 'for)   (scasel g (cauup (cuup it)) tab acc)    ; ('for init cnd stp body)
             acc)))                                                ; a nested switch / any leaf: not ours
   (scasel-l g l tab acc) (? (! (two? l)) acc (scasel-l g >l tab (scasel g <l tab acc)))

   ; -- the inline lane (the b2w problem): a SMALL function splices its whole
   ; body at the call site -- each arg value lands in a fresh 8-byte frame temp
   ; (exactly a callee frame's param slot, so the semantics are the out-of-line
   ; call's: full-word stores, (ldop pty) reads), the body's statements compile
   ; in place (decls become caller temps), and every `return` computes its value
   ; into r0 then JUMPS to the splice's end label (cgstmt's inlret door -- so
   ; the ai_nilp if-chain shape inlines, not just a single-return body). no call
   ; is emitted, so a hot caller can turn LEAF and its params take register
   ; homes -- the gcc win on the dispatch ops twice over: no call overhead AND
   ; no frame reloads. depth 1: a body being inlined compiles its own calls
   ; out-of-line (the g 'inlining flag). gated out: goto/labels (two splices of
   ; one body in one caller would collide), block statics (each splice would
   ; mint a SEPARATE instance -- C owes one), va_start. --
   (noinl? x) (? (member? x '(sdecl goto lbl vastart)) 1
                 (! (two? x)) ()
                 (|| (noinl? <x) (noinl? >x)))
   (oneret? items)                                  ; decls* + one trailing `return expr` (the b2w shape)
    (? (! (two? items)) ()
       (: it <items
          (? (<it = 'decl) (oneret? >items)
             (&& (<it = 'ret) (&& (two? <>it) (! (two? >items)))))))
   (inlok? f sig)
    (: rt (? (two? sig) <sig 'long)
       items (caup (caup (cuup f)))
       (pok ps n) (? (! (two? ps)) (n <= 6)
                     (: pty (caup (cap ps))
                        (? (|| (flo? pty) (|| (stru? pty) (arr? pty))) ()
                           (pok >ps (n + 1)))))
       (? (! (two? sig)) ()
          (|| (flo? rt) (stru? rt)) ()
          (nthr sig 2) ()                            ; variadic stays a real call
          (! (pok <>>f 0)) ()
          ; a straight-line single-return body pays broadly (600); a branchy one
          ; only while TINY (250, the guard shape) -- big if-chains bloat icache
          ((? (oneret? items) 600 250) < (tally (show <>>>f))) ()
          (! (noinl? <>>>f))))
   (cginl g env f args)                              ; splice f's body at the site -> (ty forms) | 'no (declined)
    (: fe0 (peep g 'fesc ())                          ; a splice that pins a CLEAR fesc would cost the CALLER
       strs0 (g 'strs)                                ; every sibcall (the lvm_peep stack blowup) -- decline it
       _ (pin g 'inlining 1)
       rt (cap (peep (g 'sigs) <>f `('long ())))
       ilb9 (nlab g ".il")
       (bind ps as benv acc)                         ; arg values -> fresh full-word slots, left to right
        (? (! (two? ps)) `(benv acc)
           (: r (cgexpr g env <as)
              (? (bad? r) 'bad
                 (pair? g (caup (cap ps)))           ; a pair param: an 8-byte slot, both words land
                  (: off (ntmp g 'llong)
                     (bind >ps >as
                           (link `((cap (cap ps)) off (caup (cap ps))) benv)
                           (acc + (asll g r) + (pst 'r4 off))))
                 (: off (ntmp g 'long)
                    (bind >ps >as
                          (link `((cap (cap ps)) off (caup (cap ps))) benv)
                          (acc + (asint g r) + `(`('st 'r4 off 'r0))))))))
       br (bind <>>f args () ())
       _ (pin g 'inlret ilb9)
       _ (pin g 'inlrt rt)
       ; cgitems rides the g tablet, not a direct call: the direct edge closes the
       ; cgexpr->cginl->cgitems->cgstmt->cgexpr cycle and the loader's analysis
       ; re-weaves the scope -- 7 `epi` reads compile as book reads before the pin
       ; (`;; missing epi` per load; output stays right, the reads soft-fail to
       ; runtime lookups). deterministic, not a layout artifact -- retested under
       ; content-hashed noms, same 7 -- so the indirection stays until the core
       ; analysis owns cycles in big body-less letrecs.
       ibd (? (bad? br) 'bad ((g 'cgitems) g <br (caup (caup (cuup f))) ()))
       _ (pin g 'inlret ())
       _ (pin g 'inlining ())
       _ (vmflush g)                                  ; the splice's end label joins
       (? (|| (bad? br) (bad? ibd))                   ; a shape the splice can't take: the real call still can
           (: _ (pin g 'fesc fe0) _ (pin g 'strs strs0) 'no)
          (&& (! fe0) (peep g 'fesc ()))              ; the body took a frame address: keep the caller lean
           (: _ (pin g 'fesc ()) _ (pin g 'strs strs0) 'no)
          (mkv rt 'r0 (<>br + ibd + `(`('label ilb9))))))

   (cgfn g f)                                       ; ('fn name params ('blk items)) -> forms | 'bad
    (: params <>>f
       _ (pin g 'off 8)                              ; slot -8 = saved r3 (see epi)
       ; v6-M has no FPU registers at all: the f0/f1/f15 fiction lives in three 8-byte frame
       ; CELLS, allocated up front; soften (the v6m face) rewrites every f-op onto them (cells mirror
       ; the register semantics 1:1, so any staging valid in d-regs is valid in cells)
       _ (? (v6m? g) (: _ (pin g 'fc0 (ntmp g 'double)) _ (pin g 'fc1 (ntmp g 'double))
                        _ (pin g 'fc15 (ntmp g 'double)) 0) 0)
       _ (pin g 'fesc ())                            ; fresh escape flag (the sibcall gate reads it)
       _ (pin g 'inlining ())                        ; fresh inline-depth flag
       _ (pin g 'inlret ())                          ; fresh inline-return label (cgstmt's ret door)
       _ (pin g 'homes ())                           ; fresh non-leaf home set (call-fixed + sibs read it)
       _ (pin g 'lhome ())                           ; fresh locals-homing map (cgdecl's hook reads it)
       _ (pin g 'livtab ())                          ; fresh liveness (set only for a wrap-bearing regen)
       _ (pin g 'fn <>f)
       sig (peep (g 'sigs) <>f ())
       vararg (? (two? sig) (nthr sig 2) ())
       ; the epilogue is per-FN: a thumb2 vararg fn drops its r0-r3 push block on
       ; the way out (epi-t32va); everyone else keeps the target default
       _ (pin g 'epi (? (&& vararg (t32? g)) (? (v6m? g) epi-t16va epi-t32va)
                        (v6m? g) epi-t16 (arm? g) epi-a64 epi))
       fb9 (fbase g)                                 ; the frame base the RANKERS see: post-build ir1 (r4, or fp once a4ized)
       ; spill each param from its SysV register: integers from r6 r5 r2 r1 r7 r8
       ; (<=6), doubles from f0..f7 (<=8), SEPARATE counters. an OVERFLOW param
       ; already sits on the caller stack -- addressed at rbp+16+8k, no store.
       ; hp is the HOME POLICY over the eligible gp params (x64/a64 regens only):
       ; () spills every one to its slot; 'ride homes in a register (or rides
       ; the arrival, the leaf lane); 'shadow homes AND takes a frame slot (the
       ; spill shadow in g 'homes, the non-leaf lane). under a policy the exotic
       ; composites (mixed/int-pair/xmm-exhausted 16B) answer 'bad -- the regen
       ; deopts to ir1 rather than growing a memory lane the gate never counted.
       (spill hp ps gpi xmmi ovi env acc)
        (? (! (two? ps)) `(acc env)
           (: p <ps pty <>p
              (? (&& (v6m? g) (sse2? g pty))            ; v6-M: a POSITION-0 16B composite arrives whole in r0-r3
                  (? (gpi = 0)                          ;  (the caller's g4 lane); any later start splits -- deferred
                     (: off (nslot g 16)
                        (spill hp >ps 4 xmmi ovi (link `(<p off pty) env)
                               (acc + `(`('st 'r4 off 'r0) `('st 'r4 (off + 4) 'r1)
                                        `('st 'r4 (off + 8) 'r2) `('st 'r4 (off + 12) 'r3)))))
                     'bad)
                 (&& (t32? g) (&& (stru? pty)           ; t32 params: the d-pair HFA, the 8B sse blob, the <=4B int
                     (! (|| (sse2? g pty)               ; one (the gp lane's word store IS its <=4 bytes);
                            (|| (&& (id? (aone g pty) 'sse) ((tsz g pty) = 8))   ; mixed/int-pair composites refuse LOUD
                                (&& (id? (aone g pty) 'int) ((tsz g pty) <= 4))))))) 'bad
                 (&& (v6m? g) (&& (stru? pty) (id? (aone g pty) 'sse)))   ; v6-M 8B blob param: the even-odd gp pair (the caller's g2 lane)
                  (: gpe (& (gpi + 1) -2)
                     (? ((gpe + 2) <= (ngp g))
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi (link `(<p off pty) env)
                                   (acc + `(`('st 'r4 off (nthr (argr g) gpe))
                                            `('st 'r4 (off + 4) (nthr (argr g) (gpe + 1)))))))
                        (: ov8 (& (ovi + 7) -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) (link `(<p ((ovb g) + ov8) pty) env) acc))))
                 (&& (stru? pty) (two? (aeb2 g pty)))   ; a 9..16B struct param: two eightbytes into a 16-byte slot, each from a gp or an xmm reg
                  (? (&& hp (! (sse2? g pty))) 'bad     ; a policy homes gp scalars only -- mixed/int-pair 16B deopts
                     (: cls (aeb2 g pty) c0 <cls c1 <>cls
                        ; the register FILE per eightbyte -- 'g gp / 'x xmm. SysV (x64) picks
                        ; by the eightbyte's class; AAPCS64 (arm64) sends a non-HFA composite
                        ; wholly in X regs, only an all-float HFA in V regs.
                        hfa (&& (id? c0 'sse) (id? c1 'sse))
                        f0 (? (a64? g) (? hfa 'x 'g) (? (id? c0 'sse) 'x 'g))
                        f1 (? (a64? g) (? hfa 'x 'g) (? (id? c1 'sse) 'x 'g))
                        gpn (+ (? (id? f0 'g) 1 0) (? (id? f1 'g) 1 0))
                        xmn (+ (? (id? f0 'x) 1 0) (? (id? f1 'x) 1 0))
                        (? (|| ((ngp g) < (gpi + gpn)) (8 < (xmmi + xmn)))   ; not enough regs -> the WHOLE aggregate goes to memory (2 stack eightbytes)
                            (? hp 'bad                  ; under a policy: deopt, not a fresh memory lane
                               (t32? g) 'bad            ; t32 stack HFA params (9+ doubles): deferred, refuse LOUD
                               (spill hp >ps gpi xmmi (ovi + 2) (link `(<p ((ovb g) + (* 8 ovi)) pty) env) acc))
                            (: off (nslot g 16)
                               st0 (? (id? f0 'g) `(`('st 'r4 off (nthr (argr g) gpi))) (fstf g 'double 'r4 off (xmmreg xmmi)))
                               gp1 (? (id? f0 'g) (gpi + 1) gpi)
                               xm1 (? (id? f0 'x) (xmmi + 1) xmmi)
                               st1 (? (id? f1 'g) `(`('st 'r4 (off + 8) (nthr (argr g) gp1))) (fstf g 'double 'r4 (off + 8) (xmmreg xm1)))
                               gp2 (? (id? f1 'g) (gp1 + 1) gp1)
                               xm2 (? (id? f1 'x) (xm1 + 1) xm1)
                               (spill hp >ps gp2 xm2 ovi (link `(<p off pty) env) (cat acc (cat st0 st1)))))))
                 (&& (stru? pty) (id? (aone g pty) 'sse))   ; a small SSE aggregate: one xmm into an 8-byte slot
                  (? (xmmi < 8)
                     (: off (nslot g 8)
                        (spill hp >ps gpi (xmmi + 1) ovi (link `(<p off pty) env)
                               (acc + (fstf g 'double 'r4 off (xmmreg xmmi)))))
                     (? (t32? g) 'bad                   ; t32 stack blob params: deferred, refuse LOUD
                        (spill hp >ps gpi xmmi (ovi + 1) (link `(<p ((ovb g) + (* 8 ovi)) pty) env) acc)))
                 (&& (stru? pty) (! (id? (aone g pty) 'int))) 'bad   ; MIXED pair / >16B stays ahead; a small INT one rides the gp lane
                 (&& (v6m? g) (id? pty 'float))         ; v6-M base ABI: a bare float param is ONE WORD
                  (? (gpi < (ngp g))                    ;  (binary32 bits in a gp reg / a 4-byte caller slot);
                     (: off (nslot g 8) ;  clval's fldf 'float widens it on read
                        (spill hp >ps (gpi + 1) xmmi ovi (link `(<p off pty) env)
                               (acc + `(`('st 'r4 off (nthr (argr g) gpi))))))
                     (spill hp >ps gpi xmmi (ovi + 4) (link `(<p ((ovb g) + ovi) pty) env) acc))
                 (&& (v6m? g) (flo? pty))               ; v6-M base ABI: a double param IS a gp pair
                  (: gpe (& (gpi + 1) -2)               ;  (no FPU regs exist) -- spill both words, or
                     (? ((gpe + 2) <= (ngp g))          ;  read the 8-aligned caller stack
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi (link `(<p off pty) env)
                                   (acc + `(`('st 'r4 off (nthr (argr g) gpe))
                                            `('st 'r4 (off + 4) (nthr (argr g) (gpe + 1)))))))
                        (: ov8 (& (ovi + 7) -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) (link `(<p ((ovb g) + ov8) pty) env) acc))))
                 (&& (flo? pty) (xmmi < 8))
                  (: off (nslot g 8)
                     (spill hp >ps gpi (xmmi + 1) ovi (link `(<p off pty) env)
                            (acc + (fstf g pty 'r4 off (xmmreg xmmi)))))
                 (pair? g pty)                          ; a pair param: an even-odd register pair spills
                  (: gpe (& (gpi + 1) -2)               ;  both words; else the 8-aligned stack (gp closes)
                     (? ((gpe + 2) <= (ngp g))
                         (: off (nslot g 8)
                            (spill hp >ps (gpe + 2) xmmi ovi (link `(<p off pty) env)
                                   (acc + `(`('st 'r4 off (nthr (argr g) gpe))
                                            `('st 'r4 (off + 4) (nthr (argr g) (gpe + 1)))))))
                        (: ov8 (& (ovi + 7) -8)
                           (spill hp >ps 4 xmmi (ov8 + 8) (link `(<p ((ovb g) + ov8) pty) env) acc))))
                 (&& (t32? g) (flo? pty)) 'bad          ; a t32 double param past d7: the 4-byte slot can't carry it -- refuse LOUD
                 (&& (! (flo? pty)) (gpi < (ngp g)))
                  (? (&& hp (&& (homable? pty) (gpi < (nhome g))))
                      (? (id? hp 'ride)                 ; the leaf lane: home in a register, moving from the
                         (: inc (nthr (argr g) gpi)     ; incoming arg reg if it differs -- but a rideable param
                            ; rides its ARRIVAL register: no home reserved, no entry
                            ; mov, its home register freed to the pool
                            home (? (member? gpi (peep g 'ride ())) inc (homereg gpi))
                            mv (? (home = inc) () `(`('mov home inc)))
                            (spill hp >ps (gpi + 1) xmmi ovi (link `(<p `('reg home) pty) env)
                                   (acc + mv)))
                         (: home (homereg gpi)          ; the non-leaf lane: home + the spill shadow
                            inc (nthr (argr g) gpi)
                            off (nslot g 8)
                            _ (pin g 'homes (link `(home off <p) (peep g 'homes ())))
                            mv (? (home = inc) () `(`('mov home inc)))
                            (spill hp >ps (gpi + 1) xmmi ovi (link `(<p `('reg home) pty) env)
                                   (acc + mv))))
                     (: off (nslot g 8)
                        (spill hp >ps (gpi + 1) xmmi ovi (link `(<p off pty) env)
                               (acc + `(`('st 'r4 off (nthr (argr g) gpi)))))))
                 ; an overflow param reads off the caller's block: 8-byte slots (x64/a64), 4-byte on t32
                 (t32? g) (spill hp >ps gpi xmmi (ovi + 4) (link `(<p ((ovb g) + ovi) pty) env) acc)
                 (spill hp >ps gpi xmmi (ovi + 1) (link `(<p ((ovb g) + (* 8 ovi)) pty) env) acc))))
       ; a VARIADIC function (SysV): a 176-byte register SAVE AREA holds the 6 gp
       ; arg registers (@ +0, step 8) and 8 xmm registers (@ +48, step 16, low 8
       ; bytes each). named register-passed params are addressed INSIDE the area;
       ; overflow named params ride the caller stack (rbp+16+). va_start reads back
       ; g's rsa / vagp / vaxmm. the xmm registers are saved unconditionally (safe
       ; -- reading an xmm never faults -- so no `al` guard is needed on this side).
       (vaspill ps)
        (: rsa (nslot g 176)
           _ (pin g 'rsa rsa)
           savegp (: (go i acc) (? (5 < i) acc (go (i + 1) (acc + `(`('st 'r4 (rsa + (* 8 i)) (nthr argregs i)))))) (go 0 ()))
           savex  (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + `(`('stsd 'r4 (rsa + 48 + (* 16 i)) (xmmreg i)))))) (go 0 ()))
           (cls qs gpi xmmi ov env)
            (? (! (two? qs)) (: _ (pin g 'vagp gpi) _ (pin g 'vaxmm xmmi) `((savegp + savex) env))
               (: p <qs pty <>p
                  (? (flo? pty)
                      (? (7 < xmmi) (cls >qs gpi xmmi (ov + 8) (link `(<p (16 + ov) pty) env))
                         (cls >qs gpi (xmmi + 1) ov (link `(<p (rsa + 48 + (* 16 xmmi)) pty) env)))
                     (? (5 < gpi) (cls >qs gpi xmmi (ov + 8) (link `(<p (16 + ov) pty) env))
                        (cls >qs (gpi + 1) xmmi ov (link `(<p (rsa + (* 8 gpi)) pty) env))))))
           (cls ps 0 0 0 ()))
       ; a VARIADIC function (AAPCS64): a 192-byte save area -- x0..x7 (@ +0,
       ; step 8) then q0..q7 (@ +64, step 16; the low 8 bytes carry the double,
       ; the rest is don't-care -- cc has no vector types). named register
       ; params are addressed INSIDE the area; named overflow params ride the
       ; caller stack ((ovb g)+). va_start reads back rsa / vagp / vaxmm / vaov.
       (vaspill-a64 ps)
        (: rsa (nslot g 192)
           _ (pin g 'rsa rsa)
           savegp (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + `(`('st 'r4 (rsa + (* 8 i)) (nthr argregs-a64 i)))))) (go 0 ()))
           savevr (: (go i acc) (? (7 < i) acc (go (i + 1) (acc + (fstf g 'double 'r4 (rsa + 64 + (* 16 i)) (xmmreg i))))) (go 0 ()))
           (cls qs gpi vri ov env)
            (? (! (two? qs)) (: _ (pin g 'vagp gpi) _ (pin g 'vaxmm vri) _ (pin g 'vaov ov) `((savegp + savevr) env))
               (: p <qs pty <>p
                  (? (flo? pty)
                      (? (7 < vri) (cls >qs gpi vri (ov + 1) (link `(<p ((ovb g) + (* 8 ov)) pty) env))
                         (cls >qs gpi (vri + 1) ov (link `(<p (rsa + 64 + (* 16 vri)) pty) env)))
                     (? (7 < gpi) (cls >qs gpi vri (ov + 1) (link `(<p ((ovb g) + (* 8 ov)) pty) env))
                        (cls >qs (gpi + 1) vri ov (link `(<p (rsa + (* 8 gpi)) pty) env))))))
           (cls ps 0 0 0 ()))
       ; a VARIADIC function (AAPCS32, thumb2 + v6m): no save area of its own --
       ; the prologue pushed r0-r3 just under the caller's stack args, one
       ; contiguous word array past the push cells ((ovb g): thumb2 lr/fp = 8,
       ; v6m lr/fp/r4 = 12). a named register param is addressed INSIDE the push
       ; block (fp+ovb+4i), a named overflow param past it (fp+ovb+16+). va_start
       ; reads back vagp/vaov. WORD named params only (love.c's shape: pointers
       ; and uintptr_t); doubles/pairs/structs refuse LOUD.
       (vaspill-t32 ps)
        (: (cls qs gpi ov env)
            (? (! (two? qs)) (: _ (pin g 'vagp gpi) _ (pin g 'vaov ov) `(() env))
               (: p <qs pty <>p
                  (? (|| (flo? pty) (|| (llt? pty) (stru? pty))) 'bad
                     (3 < gpi) (cls >qs gpi (ov + 4) (link `(<p (((ovb g) + 16) + ov) pty) env))
                     (cls >qs (gpi + 1) ov (link `(<p ((ovb g) + (* 4 gpi)) pty) env)))))
           (cls ps 0 0 ()))
       ; -- register-homed params (the leaf lane). a gp param that is an 8-byte
       ; scalar (long/ulong/pointer) whose address never escapes can live in a
       ; CALLER-SAVED, non-scratch register for the whole function instead of a
       ; frame slot -- gcc's win on the hot dispatch ops (lvm_arg0 &c keep Sp/Hp/
       ; Ip in registers). the home pool ((hregs g), indexed by gp position) is
       ; disjoint from the codegen's scratch set (r0-r3) and from the
       ; overflow/xmm registers -- so Hp (incoming r2) and Sp (incoming r1) MOVE
       ; out of scratch at the prologue. it is applied ONLY when the function turns
       ; out to be a LEAF: after the sibcall pass no `call`/`callr` remains (every
       ; call tail-jumped, so no callee -- and no GLAZED callee -- ever clobbers a
       ; home), fesc is unset (no frame address escaped, so no param's address was
       ; taken), there is no raw `sys` (rcx/r11 are syscall-clobbered), and it is
       ; not variadic. decided by GENERATING first, then regenerating with the
       ; register env if the gate holds. --
       ; the 'shadow policy is the NON-LEAF lane: each home ALSO takes a frame
       ; slot (its spill shadow) recorded in g 'homes -- call-fixed spills the
       ; homes there just before every remaining call's pops and reloads after,
       ; so a callee (glazed or not) only ever sees its own ABI registers. a
       ; &param deopts the whole regen (clval on a regv answers 'bad) back to ir1.
       (homereg gpi) (nthr (hregs g) gpi)
       (homes? ps gpi)                              ; does any gp param qualify for a register home?
        (? (! (two? ps)) ()
           (: pty (caup (cap ps))
              (? (|| (flo? pty) (|| (sse2? g pty) (id? (aone g pty) 'sse))) (homes? >ps gpi)
                 ((nhome g) <= gpi) ()
                 (homable? pty) 1
                 (homes? >ps (gpi + 1)))))
       (anyop? fs op) (? (! (two? fs)) () (: f <fs (? (&& (two? f) (<f = op)) 1 (anyop? >fs op))))
       (hascall? fs) (|| (anyop? fs 'call) (anyop? fs 'callr))
       ; the non-leaf PAYOFF gate: homing saves one frame load per param READ but
       ; costs a spill+reload PER HOME around every remaining call -- worth it
       ; only where reads dominate (the hot dispatch shape: many reads, a call
       ; or two). counted on the post-sibcall ir1: tail calls already cost nothing.
       (nhomable ps gpi n)
        (? (! (two? ps)) n
           (: pty (caup (cap ps))
              (? (|| (flo? pty) (|| (sse2? g pty) (id? (aone g pty) 'sse))) (nhomable >ps gpi n)
                 ((nhome g) <= gpi) n
                 (homable? pty) (nhomable >ps (gpi + 1) (n + 1))
                 (nhomable >ps (gpi + 1) n))))
       (ncalls fs n) (? (! (two? fs)) n
                        (: f <fs
                           (ncalls >fs (? (&& (two? f) (member? <f '(call callr))) (n + 1) n))))
       (pslots env acc)                             ; the homable params' PASS-1 frame slots
        (? (! (two? env)) acc
           (: e2 <env
              (pslots >env (? (&& (! (two? <>e2)) (&& (<>e2 < 0) (homable? <>>e2)))
                                   (link <>e2 acc) acc))))
       (nreads fs offs n)                           ; frame touches of those slots in ir1
        (? (! (two? fs)) n
           (: f <fs
              hit (? (! (two? f)) ()
                     (member? <f ldops9) (&& (<>>f = fb9) (member? <>>>f offs))
                     (member? <f gstops) (&& (<>f = fb9) (member? <>>f offs))
                     ())
              (nreads >fs offs (? hit (n + 1) n))))
       (build sp0)                                  ; assemble prologue + body + epilogue for a spill result
        (? (bad? sp0) 'bad
           (: _ (pin g 'dseq 0)                     ; the locals-homing decl sequence, fresh per build
              _ (pin g 'lrec ())
              _ (pin g 'lsq 0)                      ; the alive statement counter, fresh per build
              _ (pin g 'stwrap 'all)
              body (cgitems g <>sp0 (caup (caup (cuup f))) ())
              (? (bad? body) 'bad
                 (: frame (& ((peep g 'off 0) + 15) -16)
                    (? (arm? g)
                       ; the sibcall pass rides all targets (epim?/sibjmp read
                       ; the epilogue off g 'epi); the IR sweeps still model x64
                       ; shapes (8-byte pushes, the r4 frame roles) -- arm64 AND
                       ; thumb2 ride the raw forms, retargeted r4 -> fp AFTER the
                       ; sibcall rewrite (their patterns speak r4; the epilogue
                       ; forms already say fp/sp/lr and pass through a4ize
                       ; untouched). lr rides its own push cell (bl clobbers
                       ; it); no r3 slot -- x3/r3 is caller-saved under AAPCS.
                       ((? (v6m? g) (\ ir (t16la (t16slots ir frame)))   ; v6m: fp-neg slots -> r4-positive (after a4ize), then la -> literal pool
                           (sp64? g) (\ ir (t16la ir))       ; playdate: la -> pooled ABS32 word (its loader can't relocate MOVW/MOVT)
                           (\ ir ir))
                        (a4ize
                         (sibcall g
                          (soften g
                          (cat (? (v6m? g)   ; v6m: extra push r4 (bottom base), planted AFTER the sub; fp stays at top
                                  (cat `(`('label (intern <>f)))
                                   (cat (? vararg   ; vararg v6m: r0-r3 land above lr/fp/r4, contiguous with the caller stack args
                                           `(`('push 'r3) `('push 'r2) `('push 'r1) `('push 'r0))
                                           ())
                                        `(`('push 'lr) `('push 'fp) `('push 'r4)
                                           `('lea 'fp 'sp 0) `('sub 'sp 'sp frame) `('lea 'r4 'sp 0))))
                                  (cat `(`('label (intern <>f)))
                                   (cat (? (&& vararg (t32? g))   ; vararg t32: r0-r3 land under fp/lr,
                                           `(`('push 'r3) `('push 'r2) `('push 'r1) `('push 'r0))   ; contiguous with the caller stack args
                                           ())
                                        `(`('push 'lr) `('push 'fp) `('lea 'fp 'sp 0) `('sub 'sp 'sp frame)))))
                           (cat <sp0
                            (body + `(`('li 'r0 0)) + (peep g 'epi epi-a64))))))))
                       (deadst
                        (deaddef      ; rerun: the widened addrfold strands live-dest addr insns
                        (addrfold
                        (stld
                        (spmerge
                        (deaddef
                        (deadcell
                        (cfoldir
                         (sibcall g
                          (cat `(`('label (intern <>f))
                                  `('push 'r4) `('mov 'r4 'sp) `('sub 'sp 'sp frame)
                                  `('st 'r4 -8 'r3))
                           (cat <sp0
                            (body + `(`('li 'r0 0)) + epi)))))))))))))))))
       ; the operand pool rides BESIDE the homes: pass 1 owns all of opool, a
       ; regen loses the registers its param homes occupy
       (uhomes ps gpi acc)
        (? (|| (! (two? ps)) ((nhome g) <= gpi)) acc
           (: pty (caup (cap ps))
              (? (|| (flo? pty) (|| (sse2? g pty) (id? (aone g pty) 'sse))) (uhomes >ps gpi acc)
                 (homable? pty) (uhomes >ps (gpi + 1) (link (homereg gpi) acc))
                 (uhomes >ps (gpi + 1) acc))))
       ; -- the RIDE analysis (freeing a pass-through param's home): a param
       ; whose home register (r9/r10 on x64; every gp home on a64) differs from
       ; its arrival register and whose ARRIVAL register the body never uses as
       ; scratch can ride the arrival end to end -- exactly the unhome outcome,
       ; but decided UP FRONT so its home register returns to the pool for a hot
       ; local (tapn's Hp sits idle in r9 while the copy loop's counter slots).
       ; soundness: every def of the arrival reg in ir1 must be a reload from
       ; the param's OWN spill slot (its staging restores); any other def (the
       ; shuttle's spop/la/ld scratch, a div/shift clobber) bars the ride. an
       ; opaque bar op (raw/sys/call) bars it too. leaf lane only -- ride is
       ; empty under a call, so no spill policy ever meets a freed home. --
       (rst-stoff fs A)                             ; A's entry spill-store (base off) | 'no
        (? (! (two? fs)) 'no
           (: f <fs
              (? (&& (two? f) (&& (member? <f gstops) (id? <>>>f A))) `(<>f <>>f)
                 (rst-stoff >fs A))))
       (rst-defok fs A boff)                        ; every def of A is a reload from boff -> 1 | ()
        (? (! (two? fs)) 1
           (: f <fs
              (? (! (two? f)) (rst-defok >fs A boff)
                 (: rd (rdsp f)
                    (? (id? rd 'bar)
                        (? (member? <f '(label jmp br ret jmpr)) (rst-defok >fs A boff) ())
                       (! (member? A <>rd)) (rst-defok >fs A boff)
                       (&& (member? <f ldops9)
                           (&& (id? <>f A) (&& (id? <>>f <boff) (id? <>>>f <>boff))))
                        (rst-defok >fs A boff)
                       ())))))
       (rst-ok fs A) (: bo (rst-stoff fs A) (? (id? bo 'no) () (rst-defok fs A bo)))
       (rideset fs ps gpi acc)                      ; the rideable gp indices
        (? (|| (! (two? ps)) ((nhome g) <= gpi)) acc
           (: pty (caup (cap ps))
              (? (|| (flo? pty) (|| (sse2? g pty) (id? (aone g pty) 'sse))) (rideset fs >ps gpi acc)
                 (homable? pty)
                  (rideset fs >ps (gpi + 1)
                     (? (&& (! (id? (nthr (argr g) gpi) (homereg gpi))) (rst-ok fs (nthr (argr g) gpi)))
                        (link gpi acc) acc))
                 (rideset fs >ps (gpi + 1) acc))))
       ; -- the LOCALS picker: rank the homable locals ir1 recorded (cgdecl's
       ; hook) by residual frame touches, drop any whose slot is lea/lean'd
       ; (address taken), gate each on touches beating the per-call wrap, and
       ; hand the hottest the FRONT of hpool -- the operand pool keeps the
       ; rest (two reserved, so expressions never go dry outright). answers
       ; (L lhome-tablet remaining-pool) | (). --
       (ntouch fs off n)
        (? (! (two? fs)) n
           (: f2 <fs
              hit (? (! (two? f2)) ()
                     (member? <f2 ldops9) (&& (<>>f2 = fb9) (<>>>f2 = off))
                     (member? <f2 gstops) (&& (<>f2 = fb9) (<>>f2 = off))
                     ())
              (ntouch >fs off (? hit (n + 1) n))))
       ; only 'lea marks a slot address-taken (the & lane, a struct/va temp) --
       ; a 'lean is an lvalue address CONSUMED by its own load/store (an
       ; ordinary assign, a ++/--) and never escapes, exactly as the sibcall
       ; gate leas? already reads it. counting lean here needlessly barred a
       ; loop counter from homing (its RMW leans its own slot).
       (leaoffs fs t) (? (! (two? fs)) t
                         (: f2 <fs
                            _ (? (&& (two? f2) (&& (id? <f2 'lea) (<>>f2 = fb9)))
                                 (pin t <>>>f2 1) ())
                            (leaoffs >fs t)))
       (lastn l n) (: d ((tally l) - n)
                      (go2 l2 k) (? (k < 1) l2 (go2 >l2 (k - 1)))
                      (? (d < 1) l (go2 l d)))
       (dropn l n) (? (|| (n < 1) (! (two? l))) l (dropn >l (n - 1)))
       (lpick ir1x crs uv)
        (: nc (ncalls ir1x 0)
           ; a call-free fn homes by touches alone (post-sibcall: tail calls
           ; cost nothing). with calls REMAINING, only a ZERO-CROSSING local
           ; may home -- the alive pass's cross says it is never live across
           ; any call, so it takes no wraps at all and free reads are pure
           ; win. WRAP-PAYING locals (home iff touches beat 2x loop-weighted
           ; crossings) were BUILT AND MEASURED 2026-07-16: 8/8 interleaved
           ; rounds SLOWER than zero-cross-only (~2%) -- static counts do not
           ; predict the dynamic spill/reload balance. don't retry as written.
           (free? nm) (&& (member? nm uv) ((peep crs nm 0) = 0))
           lt (leaoffs ir1x (tablet 0))
           navl ((tally hpool) - 2)
           (walk rs acc)
            (? (! (two? rs)) acc
               (: e2 <rs sq <e2 off <>e2
                  (? (peep lt off ()) (walk >rs acc)
                     (&& (0 < nc) (! (free? <>>>e2))) (walk >rs acc)
                     (: tc (ntouch ir1x off 0)
                        (? (tc <= 1) (walk >rs acc)
                           (walk >rs (link `(tc sq) acc)))))))
           cs (? (navl < 1) () (lastn (imsort (walk (peep g 'lrec ()) ())) navl))
           (mk l hp t) (? (|| (! (two? l)) (! (two? hp))) t
                          (: _ (pin t (caup (cap l)) <hp)
                             (mk >l >hp t)))
           (? (two? cs) `((mk cs hpool (tablet 0)) (dropn hpool (tally cs))) ()))
       n0 (peep g 'n 0) strs0 (g 'strs)             ; snapshot the accumulators for a possible regen
       ; the operand pool is EMPTY on t32: AAPCS32 keeps r4-r11 callee-saved and the thumb2
       ; frame saves only lr+fp, while r0-r3/r12 are args and the scratch -- there is no free
       ; caller-saved register to pool (a pooled r6 clobbered gcc callers SILENTLY).
       tpool (? (t32? g) () opool)
       _ (pin g 'pool0 tpool) _ (pin g 'pool tpool)
       _ (pin g 'vuniv ()) _ (vmflush g)            ; vmap off for the ir1 build (no universe yet)
       _ (pin g 'hasasm ())                         ; fresh per fn: an asm statement pins it (homing off)
       _ (pin g 'argseal 0)                         ; fresh per fn: the vmap's staging seal depth
       ; v6-M MEMORY return: this fn's >4B composite result fills the address the caller
       ; passed as the implicit first arg (r0) -- save it in its own slot, params start at
       ; gp 1. the ret lane reads g 'sret back; reset per fn (stale pins would misroute).
       srt (? (two? sig) <sig 'long)
       sr6f (&& (v6m? g) (&& (stru? srt) (4 < (tsz g srt))))
       _ (? sr6f (pin g 'sret (ntmp g 'long)) (pin g 'sret ()))
       sp1 (? (&& vararg (a64? g)) (vaspill-a64 params)
              (&& vararg (t32? g)) (vaspill-t32 params)
              vararg (vaspill params)
              sr6f (spill () params 1 0 0 () `(`('st 'r4 (peep g 'sret 0) 'r0)))
              (spill () params 0 0 0 () ()))
       ir1 (build sp1)
       n1 (peep g 'n 0) strs1 (g 'strs)             ; ir1's accumulators -- a DEOPT restores these
       ride (? (|| vararg (|| (peep g 'fesc ()) (hascall? ir1))) () (rideset ir1 params 0 ()))
       _ (pin g 'ride ride)                          ; the 'ride policy reads it: a rideable param binds to its arrival
       hpool (: rh (map (\ k (homereg k)) ride)      ; the freed homes rejoin the pool for locals
                uh (filter (\ q (! (member? q rh))) (uhomes params 0 ()))
                (filter (\ q (! (member? q uh))) tpool))
       hm (&& (! vararg) (&& (! (peep g 'hasasm ())) (&& (! (anyop? ir1 'sys)) (homes? params 0))))
       ; the alive pass runs once per homing-eligible fn: lpick reads cross/
       ; univ (a zero-crossing local homes through calls), the regens pin its
       ; livtab so call-fixed wraps only what a call actually needs
       av (? (|| vararg (|| (bad? ir1) (|| (peep g 'hasasm ()) (anyop? ir1 'sys)))) () (alive g f))
       lr (? av (lpick ir1 (caup av) (cauup av)) ())
       lh (? (two? lr) <lr ())
       lp0 (? (two? lr) <>lr hpool)
       ; the REGEN dance, once: reset the accumulators and the frame to the
       ; ir1 snapshot, pin the analyses' channels (the vmap universe, the
       ; locals homes, the livtab -- per-call wrap sets; tail calls still
       ; stage through call-fixed), rebuild under the spill policy hp, then
       ; clear every channel (the wrap set dies with this fn). a 'bad ir2
       ; deopts: ir1 stands, its accumulators restored.
       (regen hp)
        (: _ (pin g 'n n0) _ (pin g 'strs strs0) _ (pin g 'off 8) _ (pin g 'fesc ())
           _ (pin g 'pool0 lp0) _ (pin g 'pool lp0)
           _ (pin g 'vuniv (cauup av)) _ (vmflush g)   ; the vmap rides the regen (univ known)
           _ (pin g 'homes ()) _ (pin g 'lhome lh)
           _ (pin g 'livtab (cap av))
           ir2 (build (spill hp params 0 0 0 () ()))
           _ (pin g 'livtab ())
           _ (pin g 'vuniv ()) _ (vmflush g)
           _ (pin g 'lhome ()) _ (pin g 'homes ())
           (? (bad? ir2) (: _ (pin g 'n n1) _ (pin g 'strs strs1) ir1) ir2))
       irf (? (bad? ir1) 'bad
          (&& (|| hm lh) (&& (! (peep g 'fesc ())) (! (hascall? ir1))))
           (regen 'ride)
          ; calls remain: home + spill, where reads beat the wraps -- FN-WIDE,
          ; all homable params or none. per-param verdicts were built and
          ; measured BOTH ways 2026-07-16 and lost interleaved: pruning a
          ; "losing" home inside a gated fn is 7/8 slower (its wraps hide
          ; behind the call that is already there; its register reads were
          ; pure win), and adding zero-cross homes outside the gate is 8/8
          ; slower (the regen trades operand-pool registers for the homes;
          ; returning unhomed regs to the pool then collides with the leaf
          ; lane's home-them-all 'ride policy). the fn-wide gate stands.
          (: pp (&& hm (: nc (ncalls ir1 0) nh (nhomable params 0 0)
                          ((* 2 (* nc nh)) < (nreads ir1 (pslots <>sp1 ()) 0))))
             (? (|| pp lh) (regen (? pp 'shadow ())) ir1)))
       (? (bad? irf) irf (unhome g (jr0 (unframe g irf)))))

   ; the data tail: globals in declaration order, then the string table
   ; -> (dataforms nifforms) | 'bad. a global pinned to the `ai_nifs` section
   ; (AI_NIF's registry entries) lays in ITS OWN stream -- the .o emitter puts
   ; it in a real `ai_nifs` ELF section so the linker's __start_/__stop_
   ; bracket symbols exist and main()'s boot drain finds the entries. any
   ; OTHER named section stays in .data (reached by symbol, not by bracket).
   (cgdata g)
    (: sects (peep g 'sects (tablet 0))
       (nif? nm) (? ((peep sects nm ()) = "ai_nifs") 1 ())   ; canonical 1/() -- the walk compares with id?
       (globs gs nif acc)
        (? (! (two? gs)) acc
           (! (id? (nif? <gs) nif)) (globs >gs nif acc)   ; the other stream's
           (: ge (peep (g 'glob) <gs 0)
              lbl <ge ty <>ge init <>>ge
              bytes (cgimage g ty init)                 ; a constant image | 'bad (label values -> 4d)
              (? (bad? bytes) 'bad
                 (globs >gs nif (acc + `(`('label lbl) `('raw bytes)))))))
       (strs ss acc)
        (? (! (two? ss)) acc
           (: p <ss
              (bts s i bacc) (? (i < 0) bacc (bts s (i - 1) (link (s i) bacc)))
              t <>p
              (strs >ss (cat acc `(`('label <p)
                                         `('raw ((bts t ((tally t) - 1) ()) + `(0))))))))
       (: gb (globs (rev (g 'globs)) () ())           ; exported globals, then block-scope statics
          sb (? (bad? gb) 'bad (globs (rev (g 'statics)) () ()))   ; (local data labels), then strings
          nb (? (|| (bad? gb) (bad? sb)) 'bad (globs (rev (g 'globs)) 1 ()))
          (? (|| (bad? gb) (|| (bad? sb) (bad? nb))) 'bad
             `((gb + sb + (strs (g 'strs) ())) nb))))

   ; the shared core: build the gen state, then TEXT (the functions, no _start)
   ; and DATA (globals + strings) separately -> (g textforms dataforms) | ().
   ; weaks: the __attribute__((weak)) names -- NEVER inlined (an inlined body
   ; would bake past the strong override the linker owes them).
   ; xtra: the parser's (sfns svars sects) tablets -- static fns, static
   ; globals, name -> section. () reads as three empties (the law harness).
   (cgen-core ast stbl sigs weaks xtra tgt)
    (: g (: t (tablet 0) _ (pin t 'n 0) _ (pin t 'brk ()) _ (pin t 'cnt ()) _ (pin t 'cases ())
            _ (pin t 'glob (tablet 0)) _ (pin t 'globs ()) _ (pin t 'strs ()) _ (pin t 'statics ())
            _ (pin t 'fns (tablet 0)) _ (pin t 'inl (tablet 0)) _ (pin t 'cgitems cgitems)
            _ (pin t 'stag stbl) _ (pin t 'sigs sigs)
            _ (pin t 'tgt tgt) _ (pin t 'epi (? (id? tgt 'thumb1) epi-t16 (member? tgt '(arm64 thumb2 thumb2sp)) epi-a64 epi))
            _ (pin t 'sects (? (two? xtra) <>>xtra (tablet 0))) t)
       ; pass 0: every function name -- so a bare use of one is its address (la).
       ; the sig table too: an mproto sibling (`double sin(..), cos(..);`) has a
       ; sig but no proto form of its own. a small single-return definition also
       ; lands in the INLINE table (order-independent: recorded before any body).
       (recfns fs) (? (! (two? fs)) 1
                      (: f <fs
                         _ (? (member? <f '(fn proto)) (pin (g 'fns) <>f 1) ())
                         _ (? (&& (<f = 'fn)
                                  (&& (! (member? <>f weaks))
                                      (inlok? f (peep sigs <>f ()))))
                              (pin (g 'inl) <>f f) ())
                         (recfns >fs)))
       _ (recfns <>ast)
       _ (: (go ns) (? (! (two? ns)) 1 (: _ (pin (g 'fns) <ns 1) (go >ns)))
            (go (keys sigs)))
       ; pass 1: record every global (name -> (label ty init)); cgimage
       ; validates the initializer at data-emission time (a label-valued image
       ; refuses the whole compile -- 4d)
       (rec fs)
        (? (! (two? fs)) 1
           (: f <fs
              (? (! (member? <f '(gdecl xdecl))) (rec >fs)
                 (: xd (<f = 'xdecl)              ; extern: addressable, NO storage laid
                    (one ps)
                     (? (! (two? ps)) (rec >fs)
                        (: p <ps
                           _ (pin (g 'glob) <p `((intern <p) <>p <>>p))
                           _ (? xd () (pin g 'globs (link <p (g 'globs))))
                           (one >ps)))
                    (one <>f)))))
       ok (rec <>ast)
       ; always emit text + data (a library TU has no main); `saw` rides out so
       ; the EXECUTABLE path can insist on one.
       ; acc rides REVERSED (a cat per fn re-copies every prior fn's forms --
       ; quadratic over love.c's 611 fns); the tail restores emission order.
       (gfns fs acc saw)
        (? (! (two? fs))
           (: dat (cgdata g) (? (bad? dat) () `(g (rev acc) dat saw)))
           (: f <fs
              (? (member? <f '(proto gdecl xdecl tdef)) (gfns >fs acc saw)
                 (: _ (pin g 'undecl ())               ; fresh per fn: the pin the refusal reads names THIS fn's cause
                    r (cgfn g f)
                    (? (bad? r)
                       (: u (peep g 'undecl ())         ; rung 1: an undeclared id was named -> a gripe carrying it + the enclosing fn
                          (? u (gripe () () () ("undeclared '" + u + "' (in " + <>f + ")"))
                             (: _ (puts (+ ";; cgfn refuses " (<>f + "\n"))) ())))   ; cause unknown: today's note + a bare bad
                       ; fn-start alignment to EVEN (2): a function's address can
                       ; ride into the heap as an object's ap (the lvm_* dispatch
                       ; kind), and the love image codec's pointer/fixnum discriminator
                       ; is PARITY -- every pointer even, every fixnum odd (love.c
                       ; img_encode). an odd-addressed fn pointer reads back as a
                       ; fixnum and the woken heap desyncs. WIDER alignment was built
                       ; and measured 2026-07-16 and REVERTED: 16 ran 8/8 corpus
                       ; rounds ~4% SLOWER, 32 ~5%, branch-misses +9% -- zeroing 4-5
                       ; low bits aliases the BTB on this frontend-bound core. 2
                       ; zeroes only bit 0, so the BTB is untouched (avg half a nop
                       ; per fn) and the codec invariant holds. don't widen it.
                       (gfns >fs (revcat (link `('align 2) r) acc)
                             (|| saw (<>f = "main"))))))))
       (? ok (gfns <>ast () ()) ()))
   (cgen ast stbl sigs weaks xtra tgt)              ; the STATIC EXECUTABLE: _start (crt0) + text + data -> (1 forms) | () -- needs main
    (: crt0 (? (id? tgt 'arm64)
               `(`('call 'main) `('li 'r8 93) `('sys))            ; x0 already the exit code; x8 = __NR_exit (93), svc
               `(`('call 'main) `('mov 'r6 'r0) `('li 'r0 60) `('sys)))
       r (cgen-core ast stbl sigs weaks xtra tgt)
       (? (gripe? r) r                               ; a named cause rides out to the driver (a gripe is a chain, so guard before two?)
          (! (two? r)) ()
          (! (nthr r 3)) ()                          ; no main -> no program (the exe has nothing to start)
          (: dat <>>r                           ; (dataforms nifforms): the exe resolves labels itself,
             `(1 (crt0 + <>r + <dat + <>dat)))))   ; so the nif stream just rides along
   ; the RELOCATABLE OBJECT view (stage 7a): text and data split, plus the
   ; DEFINED function names (global FUNC symbols, statics excluded -- they stay
   ; LOCAL, love.h's inline helpers land in every TU) and global-variable names
   ; (global OBJECT symbols, statics excluded), plus the ai_nifs stream -- the
   ; .o emitter (crew/holo/obj.l) turns the rest (undefined labels,
   ; cross-section refs) into a symbol table + relocations.
   (cgen-obj ast stbl sigs weaks xtra tgt)          ; -> (1 textforms dataforms funcnames globnames nifforms) | ()
    (: r (cgen-core ast stbl sigs weaks xtra tgt)
       (? (gripe? r) r                               ; a named cause rides out (guard before two?, a gripe is a chain)
          (! (two? r)) ()
          (: sfns (? (two? xtra) <xtra (tablet 0))
             svars (? (two? xtra) <>xtra (tablet 0))
             sects (? (two? xtra) <>>xtra (tablet 0))
             (loc? nm) (|| (! (id? (peep svars nm 'no) 'no))
                           ((peep sects nm ()) = "ai_nifs"))     ; a nif entry is static by shape; any OTHER
                                                                 ; named section stays exported (.image is read by symbol)
             fnames (: (go fs acc) (? (! (two? fs)) (rev acc)
                                      (: f <fs
                                         (go >fs (? (&& (<f = 'fn)
                                                            (id? (peep sfns <>f 'no) 'no))
                                                         (link <>f acc) acc))))
                       (go <>ast ()))
             gnames (: (go gs acc) (? (! (two? gs)) (rev acc)
                                      (go >gs (? (loc? <gs) acc (link <gs acc))))
                       (go (rev (<r 'globs)) ()))
             dat <>>r
             `(1 <>r <dat fnames gnames <>dat)))))
