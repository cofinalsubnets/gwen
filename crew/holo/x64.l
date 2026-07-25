; x64 -- the x86-64 BACKEND for the neutral assembler (crew/holo/holo.l). it lowers each IR form to a list
; of machine-code bytes (and label fixups), in 64-bit operand mode throughout. every encoding here
; was checked against `objdump -d -M intel`: emit the bytes, disassemble, confirm the mnemonic.
;
; the abstract register file maps onto the 16 GPRs, reserving rsp as `sp` so r0..r14 are a clean
; general pool. the mapping is deliberate: r0=rax (so li/sys/ret land the result where the Linux
; syscall and SysV-return ABIs expect it):
;   sp->rsp  r0->rax r1->rcx r2->rdx r3->rbx  r4->rbp r5->rsi r6->rdi
;   r7->r8 r8->r9 r9->r10 r10->r11 r11->r12 r12->r13 r13->r14 r14->r15
; the two awkward x86 base registers therefore surface as sp (rsp, needs a SIB byte) and r4 (rbp,
; whose disp-less form is illegal) -- `mem` handles both, so [sp+off] and [r4+off] encode right.
;
; three-address ALU forms lower to x86's destructive two-address shape: (add d a b) becomes
; `mov d,a; add d,b`, elided when d==a, and swapped when d aliases b under a commutative op. a
; genuine three-way alias on a non-commutative op (d==b, d!=a) has no two-instruction encoding, so
; it raises rather than emit something subtly wrong.
;
; the data tables (registers, the ALU group, the condition codes) come FIRST: the emit functions
; read them by mutation-visible reference, the way tele's tape is defined before its users.

(: ;; -- the register file: abstract name -> x86 encoding number (rsp kept aside as sp). --
   x64-regs (tabof
        `(('sp >< 4)
           ('r0 >< 0) ('r1 >< 1) ('r2 >< 2) ('r3 >< 3)  ('r4 >< 5)
           ('r5 >< 6) ('r6 >< 7) ('r7 >< 8) ('r8 >< 9)  ('r9 >< 10)
           ('r10 >< 11) ('r11 >< 12) ('r12 >< 13) ('r13 >< 14) ('r14 >< 15)))

   ;; -- the ALU table: op -> (rr-opcode  imm-/digit  commutative?). --
   x64-alu (tabof
        `(('add >< `(1 0 1)) ('or >< `(9 1 1)) ('and >< `(33 4 1))
           ('sub >< `(41 5 0)) ('xor >< `(49 6 1))
           ('adds >< `(1 0 1)) ('subs >< `(41 5 0))))    ; flag-setting -- x86 add/sub already set OF; pair with (br vs/vc ..)

   ;; -- condition codes: IR name -> the 0F 8x opcode low byte. --
   x64-cc (tabof
        `(('eq >< 132) ('ne >< 133) ('lt >< 140) ('ge >< 141)
           ('le >< 142) ('gt >< 143) ('below >< 130) ('ae >< 131)
           ('be >< 134) ('above >< 135) ('s >< 136) ('ns >< 137)
           ('vs >< 128) ('vc >< 129)                        ; jo / jno (overflow set / clear)
           ('p >< 138) ('np >< 139)))                       ; jp / jnp (parity: ucomisd's UNORDERED bit -- NaN-honest ==/!=)

   ;; -- the encoding bytes: REX.WRXB, ModRM(mod,reg,rm), SIB(scale,index,base). --
   (rex w r x b) (64 | ((w << 3) | ((r << 2) | ((x << 1) | b))))    ; 0x40 | wrxb
   (modrm m reg rm) ((m << 6) | (((reg & 7) << 3) | (rm & 7)))
   (sib sc ix bs)   ((sc << 6) | (((ix & 7) << 3) | (bs & 7)))
   (hi n) (n >> 3)                                                  ; the REX extension bit of a reg
   (lo n) (n & 7)                                                   ; its low 3 bits (the ModRM field)
   (rn r) (look x64-regs r 'badreg)

   ;; -- reg<-reg and reg<-imm32 forms, shared by mov/the ALU/cmp. --
   ;; alu-rr: OPCODE /r with ModRM.reg = source, ModRM.rm = dest (the AT&T `op %src,%dst`).
   (alu-rr opcode d s)
      (: dn (rn d) sn (rn s)
         `((rex 1 (hi sn) 0 (hi dn)) opcode (modrm 3 sn dn)))
   ;; alu-ri: the immediate group, `ext` selecting add/or/and/sub/xor/cmp --
   ;; 0x83 /ext ib when the immediate fits a signed byte (both forms
   ;; sign-extend, same flags), else 0x81 /ext id.
   (alu-ri ext d imm)
      (: dn (rn d)
         (? (&& (-128 <= imm) (imm <= 127))
            `((rex 1 0 0 (hi dn)) 131 (modrm 3 ext dn) (byte imm))
            (`((rex 1 0 0 (hi dn)) 129 (modrm 3 ext dn)) + (le32 imm))))
   (alu-step rr ext d y) (? (reg? y) (alu-rr rr d y) (alu-ri ext d y))
   (reg-eq a b) ((rn a) = (rn b))

   (emit-mov d s) (alu-rr 137 d s)                                  ; 0x89 mov r/m64, r64
   ;; cmp against 0 IS test (r - 0 and r AND r agree on every flag: SF/ZF/PF
   ;; from r, CF/OF clear both ways) -- 3 bytes for 7
   (emit-cmp a b) (? (&& !(reg? b) (b = 0)) (emit-test a a) (alu-step 57 7 a b))

   ;; (li d imm): B8+rd id for an unsigned-32 immediate -- the 5-byte mov-r32
   ;; form, which ZERO-extends across the full register (REX.B alone lifts the
   ;; high regs to 6); C7 /0 id sign-extends a negative signed-32 (7 bytes);
   ;; else the 10-byte movabs (B8+rd io). li density is high (every constant
   ;; and every call-staged argument), so the short form is a real text lever.
   (emit-li d imm)
      (: dn (rn d)
         (? (&& (0 <= imm) (imm <= 4294967295))
            ((? ((hi dn) = 0) () `((rex 0 0 0 1))) + ((184 | (lo dn)) >< (le32 imm)))
            (fits32 imm)
            (`((rex 1 0 0 (hi dn)) 199 (modrm 3 0 dn)) + (le32 imm))
            (`((rex 1 0 0 (hi dn)) (184 | (lo dn))) + (le64 imm))))

   ;; the three-address ALU, lowered to two-address (see the header). the lea
   ;; fusion of a fresh-dest plain add/sub (mov+alu -> one lea) was BUILT AND
   ;; MEASURED 2026-07-16, both reg-reg and imm-only variants: 8/8 interleaved
   ;; corpus rounds SLOWER (~3-4%) despite -3.3% DYNAMIC instructions -- the
   ;; dance's mov is rename-eliminated (zero cycles, no port), so the saved
   ;; insn was free, and at IPC ~3.7 the corpus is frontend/layout-bound, not
   ;; instruction-starved (exp-1's lesson again). don't retry as written.
   (emit-alu op d a b)
      (: meta (peep x64-alu op 0) rr (at meta 0) ext (at meta 1) comm (at meta 2)
         (? (reg-eq d a)                  (alu-step rr ext d b)
            (&& (reg? b) (reg-eq d b))    (? comm (alu-step rr ext d a) (scare 'alias-dst `(op d a b)))
            ((emit-mov d a) + (alu-step rr ext d b))))

   ;; imul: the 3-operand reg<-reg<-imm (0x69 /r id) is a true three-address insn, no dance; the
   ;; reg<-reg<-reg case folds onto the two-address 0F AF /r the same way the ALU does.
   (imul-rr d s) (: dn (rn d) sn (rn s) `((rex 1 (hi dn) 0 (hi sn)) 15 175 (modrm 3 dn sn)))
   (imul-ri d a imm) (: dn (rn d) an (rn a)
                        (`((rex 1 (hi dn) 0 (hi an)) 105 (modrm 3 dn an)) + (le32 imm)))
   (emit-imul d a b)
      (? (reg? b)
         (? (reg-eq d a) (imul-rr d b)
            (reg-eq d b)  (imul-rr d a)
            ((emit-mov d a) + (imul-rr d b)))
         (imul-ri d a b))

   ;; shift/rotate by an immediate count: C1 /ext ib in general, the 1-byte-shorter D1 /ext for a count
   ;; of 1 (what tag math -- sar/shr by 1 -- mostly wants). ext selects the op (shl/shr/sar/rol/ror).
   (shift-ext op) (? (op = 'shl) 4 (op = 'shr) 5 (op = 'sar) 7 (op = 'rol) 0 (op = 'ror) 1 (scare 'badshift op))
   (emit-shift op d c)
      (: dn (rn d) ext (shift-ext op)
         (? (c = 1) `((rex 1 0 0 (hi dn)) 209 (modrm 3 ext dn))                       ; D1 /ext        (count 1)
            (`((rex 1 0 0 (hi dn)) 193 (modrm 3 ext dn)) + `((byte c)))))           ; C1 /ext ib

   ;; test: set flags from a AND b, discarding the result. 0x85 /r for reg,reg; 0xF7 /0 id for reg,imm32.
   (emit-test a b)
      (? (reg? b) (: an (rn a) bn (rn b) `((rex 1 (hi bn) 0 (hi an)) 133 (modrm 3 bn an)))   ; 85 test r/m64,r64
         (: an (rn a) (`((rex 1 0 0 (hi an)) 247 (modrm 3 0 an)) + (le32 b))))             ; F7 /0 id

   ;; an indexed memory operand [base + index*scale + disp], scale in {1,2,4,8}: ALWAYS a SIB byte. the
   ;; rbp/r13-base 0-disp quirk applies (force a disp byte); rsp can be a base but NOT an index (enc 4 =
   ;; "no index"), which the abstract file already keeps as `sp`, never a general r0..r14.
   (scale-bits s) (? (s = 1) 0 (s = 2) 1 (s = 4) 2 (s = 8) 3 (scare 'badscale s))
   (memx regf base index scale disp)
      (: bn (rn base) ixn (rn index) sc (scale-bits scale)
         md (? (&& (disp = 0) !((lo bn) = 5)) 0 (? (fits8 disp) 1 2))
         dispb (? (md = 0) () (? (md = 1) `((byte disp)) (le32 disp)))
         (`((modrm md (lo regf) 4) (sib sc ixn bn)) + dispb))
   ;; the indexed family's shared core, REX.W + opcode per face (as x64-mem below).
   (x64-memx w opc regf base index scale disp)
      (: fn (rn regf) bn (rn base) ixn (rn index)
         (((rex w (hi fn) (hi ixn) (hi bn)) >< opc) + (memx fn base index scale disp)))
   (emit-ldx d base index scale disp)  (x64-memx 1 `(139) d base index scale disp)     ; 8B    mov r64, [base+index*scale+disp]
   (emit-stx base index scale disp s)  (x64-memx 1 `(137) s base index scale disp)     ; 89    mov [base+index*scale+disp], r64
   (emit-leax d base index scale disp) (x64-memx 1 `(141) d base index scale disp)     ; 8D    lea r64, [base+index*scale+disp]
   (emit-ldxb d base index scale disp) (x64-memx 1 `(15 182) d base index scale disp)  ; 0F B6 movzx r64, BYTE (zero-extend)
   (emit-stxb base index scale disp s) (x64-memx 0 `(136) s base index scale disp)     ; 88    mov BYTE, sl (REX.W=0; REX present forces spl/sil.. not ah/ch.. for s 4-7)

   ;; a memory operand [base+disp]: ModRM (+ SIB for an rsp/r12 base) (+ displacement). picks the
   ;; shortest disp form, and forces a 0-disp byte for an rbp/r13 base (its disp-less slot is taken).
   (mem regf base disp)
      (: bn (rn base) blo (lo bn)
         md (? (&& (disp = 0) !(blo = 5)) 0 (? (fits8 disp) 1 2))
         head (? (blo = 4) `((modrm md (lo regf) 4) (sib 0 4 blo)) `((modrm md (lo regf) blo)))
         dispb (? (md = 0) () (? (md = 1) `((byte disp)) (le32 disp)))
         (head + dispb))
   ;; one core for the whole [base+off] family: prefix bytes, the REX.W bit, the opcode
   ;; bytes -- each face below is a partial spelling of it (mnemonics in the comments).
   (x64-mem pfx w opc regf base off)
      (: fn (rn regf) bn (rn base)
         (pfx + (((rex w (hi fn) 0 (hi bn)) >< opc) + (mem fn base off))))
   (emit-ld d base off)   (x64-mem () 1 `(139) d base off)      ; 8B     mov r64, [base+off]
   (emit-st base off s)   (x64-mem () 1 `(137) s base off)      ; 89     mov [base+off], r64
   (emit-lea d base off)  (x64-mem () 1 `(141) d base off)      ; 8D     lea r64, [base+off]
   ;; the sized loads (SIGN-extending -- C's char/short/int) and stores (truncating).
   (emit-ld1 d base off)  (x64-mem () 1 `(15 190) d base off)   ; 0F BE  movsx r64, BYTE
   (emit-ld2 d base off)  (x64-mem () 1 `(15 191) d base off)   ; 0F BF  movsx r64, WORD
   (emit-ld4 d base off)  (x64-mem () 1 `(99) d base off)       ; 63     movsxd r64, DWORD
   ;; the ZERO-extending sized loads (C's unsigned char/short/int). ldu4 is a plain 32-bit mov (REX.W=0), which zeroes the top.
   (emit-ldu1 d base off) (x64-mem () 1 `(15 182) d base off)   ; 0F B6  movzx r64, BYTE
   (emit-ldu2 d base off) (x64-mem () 1 `(15 183) d base off)   ; 0F B7  movzx r64, WORD
   (emit-ldu4 d base off) (x64-mem () 0 `(139) d base off)      ; 8B     mov r32, DWORD (zero-extends)
   (emit-st1 base off s)  (x64-mem () 0 `(136) s base off)      ; 88     mov BYTE, s8 (REX present: spl/sil, never ah/ch)
   (emit-st2 base off s)  (x64-mem `(102) 0 `(137) s base off)  ; 66 89  mov WORD, s16
   (emit-st4 base off s)  (x64-mem () 0 `(137) s base off)      ; 89     mov DWORD, s32
   ;; in-register sign extension (the C casts): movsx/movsxd reg64 <- reg's low w bytes
   (emit-sx w d)
      (: dn (rn d)
         opc (? (w = 1) `(15 190) (w = 2) `(15 191) `(99))
         (`((rex 1 (hi dn) 0 (hi dn))) + (opc + `((modrm 3 (lo dn) (lo dn))))))
   ;; in-register ZERO extension: movzx r64,r8/r16 (0F B6 / 0F B7); w=4 is mov r32,r32 (REX.W=0 zeroes the top).
   (emit-zx w d)
      (: dn (rn d)
         (? (w = 1) (`((rex 1 (hi dn) 0 (hi dn)) 15 182) + `((modrm 3 (lo dn) (lo dn))))
            (w = 2) (`((rex 1 (hi dn) 0 (hi dn)) 15 183) + `((modrm 3 (lo dn) (lo dn))))
            (`((rex 0 (hi dn) 0 (hi dn)) 139) + `((modrm 3 (lo dn) (lo dn))))))
   ;; shifts by a REGISTER count: D3 /ext, the count in CL -- so the IR contract
   ;; pins the count register to r1 (rcx) on every backend; d must differ.
   (emit-shiftv op d c)
      (: _ (? (reg-eq c 'r1) 0 (scare 'shiftv-count-not-r1 `(op d c)))
         _ (? (reg-eq d 'r1) (scare 'shiftv-dst-is-r1 `(op d c)) 0)
         dn (rn d)
         ext (? (op = 'shlv) 4 (op = 'shrv) 5 7)
         (`((rex 1 0 0 (hi dn)) 211) + `((modrm 3 ext (lo dn)))))

   ;; push/pop: 0x50+rd / 0x58+rd, a bare REX.B (0x41) for the extended registers, no REX.W.
   (emit-push s) (: sn (rn s) (? (sn >= 8) `(65 (80 | (lo sn))) `((80 | sn))))
   (emit-pop  d) (: dn (rn d) (? (dn >= 8) `(65 (88 | (lo dn))) `((88 | dn))))

   ;; in-place unary: F7 /3 neg, F7 /2 not, FF /0 inc, FF /1 dec.
   (emit-un opcode ext d) (: dn (rn d) `((rex 1 0 0 (hi dn)) opcode (modrm 3 ext dn)))

   ;; control transfer. call/jmp are E8/E9 cd; a conditional is 0F 8x cd. each leaves a rel32 fixup
   ;; the driver fills once the target label's address is known.
   (jcc-op c) (look x64-cc c 'badcc)
   (emit-call label)     `(232 (fix 4 'rel32 label 0))
   ;; the address of a label: RIP-relative lea -- the rel32 measures from the
   ;; end of its own field, exactly x64-patch's convention, so data placed by
   ;; (label)+(raw ..) is addressable with no new fixup kind.
   (emit-la d label)
      (: dn (rn d) `((rex 1 (hi dn) 0 0) 141 (modrm 0 (lo dn) 5) (fix 4 'rel32 label 0)))
   (emit-jmp  label)     `(233 (fix 4 'rel32 label 0))
   (emit-br cond label)  `(15 (jcc-op cond) (fix 4 'rel32 label 0))
   ;; the 2-byte short forms the relaxer (holo.l relax) rewrites in-range branches to:
   ;; EB rel8 / 7x rel8 -- the short jcc opcode is the 0F 8x second byte less 0x10.
   (emit-jmp8 label)     `(235 (fix 1 'rel8 label 0))
   (emit-br8 cond label) `(((jcc-op cond) - 16) (fix 1 'rel8 label 0))
   ;; jmpr: FF /4 -- an indirect jump through a register (the glaze's Continue / deopt tail). REX.B
   ;; for an extended register; no REX.W (jmp defaults to 64-bit operand size).
   (emit-jmpr d) (: dn (rn d) ((? (dn >= 8) `(65) ()) + `(255 (modrm 3 4 dn))))
   ;; callr: FF /2 -- an indirect call through a register (a call through a function pointer). same
   ;; shape as jmpr with the /digit at 2; REX.B for r8+, no REX.W.
   (emit-callr d) (: dn (rn d) ((? (dn >= 8) `(65) ()) + `(255 (modrm 3 2 dn))))
   ;; mulo: d <- a*b (low 64), branch to lbl on signed overflow. imul (0F AF) sets OF when the
   ;; product leaves signed-64, so a plain `jo` is exact -- the scratch `t` is UNUSED on x86 (only
   ;; aarch64 needs it, for SMULH).
   (emit-mulo d a b t lbl) ((emit-imul d a b) + `(15 128 (fix 4 'rel32 lbl 0)))   ; <imul> ; 0F 80 jo rel32
   ;; set: d <- (cond) ? 1 : 0 from the last cmp -- SETcc dl (0F 9x /0) then MOVZX d,dl (0F B6 /r). a
   ;; REX (B for r8b+, a bare 0x40 for spl/bpl/sil/dil) forces the byte operand to be the low byte,
   ;; not ah/ch/dh/bh. SETcc's opcode is the Jcc low byte + 0x10 (0F 84 je -> 0F 94 sete).
   (emit-set cond d)
      (: o (jcc-op cond) dn (rn d)
         rb (? (dn >= 8) `(65) (? (dn >= 4) `(64) ()))
         (rb + (`(15 (o + 16) (modrm 3 0 dn)) + `((rex 1 (hi dn) 0 (hi dn)) 15 182 (modrm 3 dn dn)))))
   ;; div/rem: x86 idiv is fixed-register (RDX:RAX / r/m -> RAX quotient, RDX remainder), and rdx is
   ;; commonly live (the glaze keeps Hp there), so the sequence SAVES rdx itself: push rdx; push the
   ;; divisor to memory; mov rax,a; cqo (sign-extend); idiv [rsp]; result -> rax (rem: from rdx);
   ;; add rsp,8; pop rdx; mov d,rax. clobbers rax; `d` must not be r2 (rdx). caller guards b != 0.
   (emit-div remf sgn d a b)
      (catall `(`(82)                                     ; push rdx
                 (emit-push b)                              ; push the divisor
                 (? (reg-eq a 'r0) () (emit-mov 'r0 a))     ; rax <- a (dividend)
                 (? sgn `(72 153) `(49 210))              ; signed: cqo ; unsigned: xor edx,edx
                 `(72 247 (? sgn 60 52) 36)                ; idiv (/7) | div (/6) qword [rsp]
                 (? remf `(72 137 208) ())                 ; rem: mov rax, rdx
                 `(72 131 196 8)                           ; add rsp, 8
                 `(90)                                     ; pop rdx
                 (? (reg-eq d 'r0) () (emit-mov d 'r0))))   ; d <- rax

   ;; -- SSE2 / packed-double (the float-grid glaze lane). xmm regs are a SEPARATE abstract file
   ;;    f0..f15 -> 0..15 (so the GP file is untouched); a `raw` escape splices pre-encoded bytes
   ;;    (the cold derive/preload sequences the float emitter already produces). every encoding here
   ;;    was objdump-checked. REX is emitted only when an operand is r>=8/xmm>=8 or REX.W is needed. --
   x64-xregs (fregs 16)                                                             ; f0..f15 -> 0..15
   (xrn r) (look x64-xregs r 'badxreg)
   ;; the xmm binop table: op -> (prefix opcode). prefix 102=66 / 242=F2 / 0=none. modrm.reg=d, rm=s.
   x64-sse (tabof
        `(`('movupd 102 16) `('movsd 242 16) `('movhlps 0 18) `('unpcklpd 102 20)
           `('mulpd 102 89) `('addpd 102 88) `('subpd 102 92) `('divpd 102 94)
           `('mulsd 242 89) `('addsd 242 88) `('subsd 242 92) `('divsd 242 94)
           `('xorpd 102 87) `('pcmpeqd 102 118) `('andpd 102 84) `('psubq 102 251)
           `('cvtss2sd 243 90) `('cvtsd2ss 242 90)                                    ; F3/F2 0F 5A /r  widen/narrow single<->double (the `float` lane)
           `('ucomisd 102 46)))                                                       ; 66 0F 2E /r  ucomisd xmm,xmm (sets flags)
   (xrex w r b) (? (|| (w = 1) (|| (r >= 8) (b >= 8))) `((rex w (hi r) 0 (hi b))) ())   ; REX only when needed
   (emit-sse op d s)                                                                     ; xmm <- xmm two-address
      (: meta (peep x64-sse op 0) pfx (at meta 0) opc (at meta 1) dn (xrn d) sn (xrn s)
         (catall `((? (pfx = 0) () `(pfx)) (xrex 0 dn sn) `(15 opc (modrm 3 dn sn)))))
   (emit-cmppd d s imm) (: dn (xrn d) sn (xrn s)                                         ; 66 0F C2 /r ib  (predicate imm: 1=LT 2=LE)
      (catall `(`(102) (xrex 0 dn sn) `(15 194 (modrm 3 dn sn) (byte imm)))))
   (emit-movmskpd g x) (: gn (rn g) xn (xrn x)                                           ; 66 0F 50 /r  gp(reg) <- xmm(rm)
      (catall `(`(102) (xrex 0 gn xn) `(15 80 (modrm 3 gn xn)))))
   (emit-cvtsi2sd x g) (: xn (xrn x) gn (rn g)                                           ; F2 REX.W 0F 2A /r  xmm(reg) <- gp(rm)
      (catall `(`(242) `((rex 1 (hi xn) 0 (hi gn))) `(15 42 (modrm 3 xn gn)))))
   (emit-cvttsd2si g x) (: gn (rn g) xn (xrn x)                                          ; F2 REX.W 0F 2C /r  gp(reg) <- xmm(rm) (truncate toward zero)
      (catall `(`(242) `((rex 1 (hi gn) 0 (hi xn))) `(15 44 (modrm 3 gn xn)))))
   (emit-movqxr x g) (: xn (xrn x) gn (rn g)                                             ; 66 REX.W 0F 6E /r  xmm(reg) <- gp(rm)
      (catall `(`(102) `((rex 1 (hi xn) 0 (hi gn))) `(15 110 (modrm 3 xn gn)))))
   (emit-movqrx g x) (: xn (xrn x) gn (rn g)                                             ; 66 REX.W 0F 7E /r  gp(rm) <- xmm(reg)
      (catall `(`(102) `((rex 1 (hi xn) 0 (hi gn))) `(15 126 (modrm 3 xn gn)))))
   ;; movsd to/from MEMORY [base+disp] -- the spill/box ops the reg-reg SSE lane can't reach. F2 0F 11 /r
   ;; stores xmm->[mem], F2 0F 10 /r loads [mem]->xmm; the xmm number rides ModRM.reg via `mem`.
   (emit-stsd base disp s)                                                               ; (stsd base disp f)  [base+disp] <- xmm(s)
      (: sn (xrn s) (catall `(`(242) (xrex 0 sn (rn base)) `(15 17) (mem sn base disp))))
   (emit-ldsd d base disp)                                                               ; (ldsd f base disp)  xmm(d) <- [base+disp]
      (: dn (xrn d) (catall `(`(242) (xrex 0 dn (rn base)) `(15 16) (mem dn base disp))))
   ;; movss to/from MEMORY -- the 4-byte single lane (the C `float` slot). F3 0F 11 /r stores, F3 0F 10 /r loads.
   (emit-stss base disp s)                                                               ; (stss base disp f)  [base+disp] <- xmm(s) (low 32)
      (: sn (xrn s) (catall `(`(243) (xrex 0 sn (rn base)) `(15 17) (mem sn base disp))))
   (emit-ldss d base disp)                                                               ; (ldss f base disp)  xmm(d) <- [base+disp] (low 32)
      (: dn (xrn d) (catall `(`(243) (xrex 0 dn (rn base)) `(15 16) (mem dn base disp))))

   ;; the dispatch: one IR form -> its byte/fixup chunk.
   (x64-lower ins)
      (: op <ins
         (? (op = 'li)   (emit-li   (at ins 1) (at ins 2))
            (op = 'mov)  (emit-mov  (at ins 1) (at ins 2))
            (op = 'cmp)  (emit-cmp  (at ins 1) (at ins 2))
            (op = 'imul) (emit-imul (at ins 1) (at ins 2) (at ins 3))
            (op = 'ld)   (emit-ld   (at ins 1) (at ins 2) (at ins 3))
            (op = 'st)   (emit-st   (at ins 1) (at ins 2) (at ins 3))
            (op = 'lea)  (emit-lea  (at ins 1) (at ins 2) (at ins 3))
            (op = 'lean) (emit-lea  (at ins 1) (at ins 2) (at ins 3))    ; cc's CONSUMED frame address (an lvalue's own store eats it) -- same bytes, invisible to the sibcall escape gate
            (op = 'ldx)  (emit-ldx  (at ins 1) (at ins 2) (at ins 3) (at ins 4) (at ins 5))   ; (ldx d base index scale disp)
            (op = 'stx)  (emit-stx  (at ins 1) (at ins 2) (at ins 3) (at ins 4) (at ins 5))   ; (stx base index scale disp s)
            (op = 'leax) (emit-leax (at ins 1) (at ins 2) (at ins 3) (at ins 4) (at ins 5))   ; (leax d base index scale disp)
            (op = 'ld1)  (emit-ld1  (at ins 1) (at ins 2) (at ins 3))
            (op = 'ld2)  (emit-ld2  (at ins 1) (at ins 2) (at ins 3))
            (op = 'ld4)  (emit-ld4  (at ins 1) (at ins 2) (at ins 3))
            (op = 'st1)  (emit-st1  (at ins 1) (at ins 2) (at ins 3))
            (op = 'st2)  (emit-st2  (at ins 1) (at ins 2) (at ins 3))
            (op = 'st4)  (emit-st4  (at ins 1) (at ins 2) (at ins 3))
            (op = 'ldu1) (emit-ldu1 (at ins 1) (at ins 2) (at ins 3))
            (op = 'ldu2) (emit-ldu2 (at ins 1) (at ins 2) (at ins 3))
            (op = 'ldu4) (emit-ldu4 (at ins 1) (at ins 2) (at ins 3))
            (op = 'sx1)  (emit-sx 1 (at ins 1))
            (op = 'sx2)  (emit-sx 2 (at ins 1))
            (op = 'sx4)  (emit-sx 4 (at ins 1))
            (op = 'zx1)  (emit-zx 1 (at ins 1))
            (op = 'zx2)  (emit-zx 2 (at ins 1))
            (op = 'zx4)  (emit-zx 4 (at ins 1))
            (member? op '(shlv shrv sarv)) (emit-shiftv op (at ins 1) (at ins 2))
            (op = 'ldxb) (emit-ldxb (at ins 1) (at ins 2) (at ins 3) (at ins 4) (at ins 5))   ; (ldxb d base index scale disp) -- zero-extend byte load
            (op = 'stxb) (emit-stxb (at ins 1) (at ins 2) (at ins 3) (at ins 4) (at ins 5))   ; (stxb base index scale disp s) -- low-byte store
            (op = 'test) (emit-test (at ins 1) (at ins 2))
            (member? op '(shl shr sar rol ror)) (emit-shift op (at ins 1) (at ins 2))
            (op = 'push) (emit-push (at ins 1))
            (op = 'pop)  (emit-pop  (at ins 1))
            (op = 'neg)  (emit-un 247 3 (at ins 1))
            (op = 'not)  (emit-un 247 2 (at ins 1))
            (op = 'inc)  (emit-un 255 0 (at ins 1))
            (op = 'dec)  (emit-un 255 1 (at ins 1))
            (op = 'jmp)  (emit-jmp  (at ins 1))
            (op = 'br)   (emit-br   (at ins 1) (at ins 2))
            (op = 'jmp8) (emit-jmp8 (at ins 1))
            (op = 'br8)  (emit-br8  (at ins 1) (at ins 2))
            (op = 'jmpr) (emit-jmpr (at ins 1))
            (op = 'callr) (emit-callr (at ins 1))
            (op = 'mulo) (emit-mulo (at ins 1) (at ins 2) (at ins 3) (at ins 4) (at ins 5))   ; (mulo d a b t lbl)
            (op = 'set)  (emit-set  (at ins 1) (at ins 2))
            (op = 'div)  (emit-div 0 1 (at ins 1) (at ins 2) (at ins 3))
            (op = 'rem)  (emit-div 1 1 (at ins 1) (at ins 2) (at ins 3))                      ; (rem d a b t) -- t ignored on x86 (rdx)
            (op = 'udiv) (emit-div 0 0 (at ins 1) (at ins 2) (at ins 3))                      ; unsigned: xor rdx + div (/6)
            (op = 'urem) (emit-div 1 0 (at ins 1) (at ins 2) (at ins 3))
            (op = 'call) (emit-call (at ins 1))
            (op = 'la)   (emit-la   (at ins 1) (at ins 2))
            (op = 'lar)  (emit-la   (at ins 1) (at ins 2))   ; near pc-rel address == la on x86 (rip-rel lea, already translation-invariant); arm64 differs (ADR vs ADRP)
            (op = 'ret)  `(195)
            (op = 'sys)  `(15 5)
            (op = 'nop)  `(144)
            (op = 'trap) `(204)
            (op = 'raw)  (at ins 1)                                                       ; splice pre-encoded bytes verbatim (no fixups)
            (two? (peep x64-sse op ())) (emit-sse op (at ins 1) (at ins 2))               ; xmm<-xmm (movupd/movsd/mulpd/addpd/subpd/.../psubq/unpcklpd/movhlps)
            (op = 'cmppd)    (emit-cmppd    (at ins 1) (at ins 2) (at ins 3))
            (op = 'movmskpd) (emit-movmskpd (at ins 1) (at ins 2))                        ; (movmskpd gp xmm)
            (op = 'cvtsi2sd) (emit-cvtsi2sd (at ins 1) (at ins 2))                        ; (cvtsi2sd xmm gp)
            (op = 'cvttsd2si) (emit-cvttsd2si (at ins 1) (at ins 2))                      ; (cvttsd2si gp xmm)  gp <- (int)xmm
            (op = 'movqxr)   (emit-movqxr   (at ins 1) (at ins 2))                        ; (movqxr xmm gp)  xmm <- gp
            (op = 'movqrx)   (emit-movqrx   (at ins 1) (at ins 2))                        ; (movqrx gp xmm)  gp <- xmm
            (op = 'stsd)     (emit-stsd     (at ins 1) (at ins 2) (at ins 3))             ; (stsd base disp xmm)  [base+disp] <- xmm
            (op = 'ldsd)     (emit-ldsd     (at ins 1) (at ins 2) (at ins 3))             ; (ldsd xmm base disp)  xmm <- [base+disp]
            (op = 'stss)     (emit-stss     (at ins 1) (at ins 2) (at ins 3))             ; (stss base disp xmm)  [base+disp] <- xmm (4-byte single)
            (op = 'ldss)     (emit-ldss     (at ins 1) (at ins 2) (at ins 3))             ; (ldss xmm base disp)  xmm <- [base+disp] (4-byte single)
            (two? (peep x64-alu op ())) (emit-alu op (at ins 1) (at ins 2) (at ins 3))
            (scare 'bad-op op)))

   ;; two relocation kinds: rel32, a 32-bit pc-relative field measured from the END of the field
   ;; (branches, la); and abs64, an 8-byte ABSOLUTE address of the label (a pointer baked into data --
   ;; needs the assembly ORG set to the load vaddr, which elf64 does).
   (x64-patch kind target site width aux)
      (? (kind = 'rel32) (le32 (target - (site + width)))
         (kind = 'rel8)  (: d (target - (site + width))          ; the relaxer proved the fit at the
                            _ (? (fits8 d) 0 (scare 'rel8-range d))   ; pessimistic layout; this scare
                            `((byte d)))                              ; is the loud net under that proof
         (kind = 'abs64) (le64 (target + aux))     ; aux = a byte ADDEND (&arr[i]/&s.field: label + constant offset)
         (scare 'badfix kind))

   ;; -- register the backend under its target names. --
   x64 (tablet 0)
   _ (pin x64 'lower x64-lower)
   _ (pin x64 'patch x64-patch)
   _ (pin x64 'nop `(144))                     ; the ('align n) filler unit: one-byte nop
   _ (pin x64 'short 1)                        ; opt into branch relaxation (jmp8/br8 exist here)
   _ (defbackend 'x64 x64)
   _ (defbackend 'amd64 x64))
