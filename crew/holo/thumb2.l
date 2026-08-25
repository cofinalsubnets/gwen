; thumb2 -- the ARMv7E-M (Cortex-M7) BACKEND for the neutral assembler (crew/holo/holo.l), the third
; target. like arm64.l it pins a {lower,patch} pair (here under 'thumb2 and 'cortex-m7) and the driver,
; IR forms, and byte plumbing stay untouched -- but the ISA is a different animal from aarch64 despite
; the shared "ARM" name. this is 32-bit Thumb-2: MIXED 16/32-bit little-endian instructions, r0..r15
; (r13=sp, r14=lr, r15=pc), a two-address-ish data-processing surface. every word here was checked
; against `llvm-mc --triple=thumbv7em-none-eabi` (--disassemble / --show-encoding). load after holo.l.
;
; the byte order trap: a 32-bit Thumb-2 instruction is TWO 16-bit halfwords, the HIGH halfword FIRST
; in memory, each halfword little-endian. so word W = (hw1<<16)|hw2 lays as le16(hw1) ++ le16(hw2) --
; NOT le32(W). `t32` does exactly that; `t16` is one narrow halfword.
;
; the layout stays byte-granular (holo.l's revonto counts bytes, a fixup carries its byte width), so
; the mixed 16/32 widths just fall out -- each lowered form reports its own length, no special-casing.
;
; register file: abstract r0..r12 -> hardware r0..r12 (identity); sp->13, lr->14, pc->15, fp->11. we
; use the WIDE (32-bit) encodings uniformly for the data-processing ops (they reach r0..r12 evenly,
; where the narrow 16-bit forms only see r0..r7) -- correctness over compactness for this rung.
;
; immediates: add/sub take a plain 12-bit ADDW/SUBW (0..4095, a negative flips the op); cmp takes an
; 8-bit modified-immediate (0..255 rides through untouched). the logicals (and/or/xor) have only the
; Thumb modified-immediate form, so an immediate there RAISES -- moon's `immok` seam blocks it upstream
; and materializes the value into a register instead (mirrors arm64.l's bitmask stance).

(module 'holo
(: ;; -- registers. abstract number = hardware number for r0..r12; sp/lr/pc/fp are named. r12 (IP) is
   ;;    the AAPCS intra-call scratch; the indexed stores borrow it as an unnamed address temp. --
   thumb-regs (tabof
        [('sp >< 13) ('lr >< 14) ('pc >< 15) ('fp >< 11)
          ('r0 >< 0) ('r1 >< 1) ('r2 >< 2) ('r3 >< 3)  ('r4 >< 4)
          ('r5 >< 5) ('r6 >< 6) ('r7 >< 7) ('r8 >< 8)  ('r9 >< 9)
          ('r10 >< 10) ('r11 >< 11) ('r12 >< 12)])
   (rn r) (look thumb-regs r 'badreg)

   ;; -- condition codes: IR name -> the AArch32 cond field (same 4-bit encoding aarch64 uses). --
   thumb-cc (tabof
        [('eq >< 0) ('ne >< 1) ('ae >< 2) ('below >< 3) ('s >< 4) ('ns >< 5)
          ('vs >< 6) ('vc >< 7) ('p >< 6) ('np >< 7)
          ('above >< 8) ('be >< 9) ('ge >< 10) ('lt >< 11) ('gt >< 12) ('le >< 13)])

   ;; -- emit: a narrow halfword, or a 32-bit word as hi-halfword-first little-endian pair. --
   (t16 w) (le16 w)
   (t32 w) (le16 (w >> 16) + le16 (w & 65535))

   ;; (li d imm): MOVW the low 16, then MOVT the high 16 if nonzero. imm normalizes into [0,2^32).
   ;; the shared scatter (i/imm4/imm3/imm8) takes the hw register NUMBER -- `patch` reuses it below.
   (thumb-mw16 base dn imm16)                                 ; base 0xF240 = MOVW (T3), 0xF2C0 = MOVT (T1)
      (: hw1 (base | ((((imm16 >> 11) & 1) << 10) | ((imm16 >> 12) & 15)))      ; i (bit11), imm4 (bits15:12)
         hw2 ((((imm16 >> 8) & 7) << 12) | ((dn << 8) | (imm16 & 255)))         ; imm3 (bits10:8), Rd, imm8
         (t32 ((hw1 << 16) | hw2)))
   (thumb-movw d imm16) (thumb-mw16 0xF240 (rn d) imm16)      ; MOVW Rd, #imm16
   (thumb-movt d imm16) (thumb-mw16 0xF2C0 (rn d) imm16)      ; MOVT Rd, #imm16 -- sets the top halfword
   (thumb-li d imm)
      (: u (? (imm < 0) (imm + 4294967296) imm) lo (u & 65535) hi ((u >> 16) & 65535)
         (thumb-movw d lo + (? (hi = 0) () (thumb-movt d hi))))

   ;; (la d lbl): materialize lbl's absolute address -- MOVW the low half, MOVT the high, each its
   ;; own 4-byte fixup (movw16/movt16, aux = Rd's hw number; patch scatters the resolved half in).
   ;; always BOTH (a fixup's size is fixed before the address is known). a .o leaves the pair as
   ;; R_ARM_THM_MOVW_ABS_NC / R_ARM_THM_MOVT_ABS (obj.l); v6-M has no MOVW/MOVT and pools instead.
   (thumb-la d label) (: dn (rn d) [(fix 4 'movw16 label dn) (fix 4 'movt16 label dn)])
   ;; (lapool Rd): LDR.W Rd,[pc,#4] + B.N over the pad+word + NOP pad -- the literal-pool
   ;; la (gen's t16la lays (align 4) before and the abs32 word after). the playdate face:
   ;; its loader relocates ABS32 words at load time, never a MOVW/MOVT pair.
   (thumb-lapool d) (: dn (rn d) (catall [(t32 (0xF8DF0004 | (dn << 12))) (t16 0xE002) (t16 0xBF00)]))

   ;; (mov Rd, Rm). MOV.W is ORR with Rn=15, and that form can name neither sp nor pc in
   ;; either slot -- UNPREDICTABLE, which a core is free to execute as the move you meant.
   ;; the 16-bit hi-register T1 (thumb-movr) reaches all sixteen, so those two route there.
   (thumb-sppc? n) ((n = 13) || (n = 15))
   (thumb-mov d s)
      (: dn (rn d) sn (rn s)
         (? (thumb-sppc? dn || thumb-sppc? sn) (thumb-movr d s)
            (t32 (0xEA4F0000 | ((dn << 8) | sn)))))                ; MOV.W Rd, Rm (ORR Rd,Rn=15,Rm)

   ;; -- ALU register form (wide, three-address): op -> the hw1 base (S=0). d = a OP b.
   ;; the row list stays bound: the emitter tablet below pins the group off it. --
   thumb-alu-rows
        [('add >< 0xEB00) ('sub >< 0xEBA0) ('and >< 0xEA00) ('or >< 0xEA40) ('xor >< 0xEA80)
          ('adds >< 0xEB10) ('subs >< 0xEBB0)                  ; the S-variant: set the flags
          ('adc >< 0xEB40) ('adcs >< 0xEB50)                   ; the carry family -- the 64-bit
          ('sbc >< 0xEB60) ('sbcs >< 0xEB70)                   ;   pair lanes (moon rung 3)
          ('ors >< 0xEA50)]                                    ; ORRS: pair equality / pair truth
   thumb-alu (tabof thumb-alu-rows)
   (thumb-alu-rrr base d a b)                                 ; hw1 = base|Rn(a); hw2 = Rd<<8 | Rm(b)
      (: dn (rn d) an (rn a) bn (rn b) (t32 (((base | an) << 16) | ((dn << 8) | bn))))
   ;; add/sub #imm12 via ADDW/SUBW (T4): a plain 12-bit unsigned; a negative flips the op.
   (thumb-addsub-imm op d a imm)
      (: pos (? (imm < 0) (0 - imm) imm)
         fop (? (imm < 0) (? (op = 'add) 'sub (op = 'sub) 'add op) op)
         _   (? (pos <= 4095) 0 (scare 'imm12-range imm))
         base (? (fop = 'add) 0xF200 0xF2A0)                   ; ADDW / SUBW hw1 base
         dn (rn d) an (rn a)
         hw1 (base | ((((pos >> 11) & 1) << 10) | an))         ; i = imm12 bit 11
         hw2 ((((pos >> 8) & 7) << 12) | ((dn << 8) | (pos & 255)))   ; imm3, Rd, imm8
         (t32 ((hw1 << 16) | hw2)))
   (thumb-aluop op d a b)
      (: base (peep thumb-alu op 0)
         (? (reg? b) (thumb-alu-rrr base d a b)
            (member? op '(add sub)) (thumb-addsub-imm op d a b)
            (scare 'thumb-logimm [op d a b])))                 ; and/or/xor #imm: materialize instead (immok blocks it)

   (thumb-cmp a b)                                            ; CMP.W Rn, Rm  /  CMP Rn, #imm8 (T2)
      (: an (rn a)
         (? (reg? b) (: bn (rn b) (t32 (((0xEBB0 | an) << 16) | (0x0F00 | bn))))
            (: pos (? (b < 0) (0 - b) b)
               _ (? (pos <= 255) 0 (scare 'cmp-imm-range b))
               ; a negative compares via CMN (SUB->ADD): hw1 base 0xF110 vs CMP's 0xF1B0
               (t32 ((((? (b < 0) 0xF110 0xF1B0) | an) << 16) | (0x0F00 | pos))))))

   (thumb-imul d a b)                                         ; MUL Rd, Rn, Rm (T2); no immediate form
      (? (reg? b) (: dn (rn d) an (rn a) bn (rn b) (t32 (((0xFB00 | an) << 16) | (0xF000 | ((dn << 8) | bn)))))
         (scare 'thumb-mul-imm ['imul d a b]))
   ;; the widening multiply family (the 64-bit pair lanes): UMULL RdLo,RdHi,Rn,Rm (T1) and
   ;; MLA Rd,Rn,Rm,Ra (T1, mls without bit4). CLZ Rd,Rm (T1) for clzll.
   (thumb-umull dl dh a b)
      (: ln (rn dl) hn (rn dh) an (rn a) bn (rn b)
         (t32 (((0xFBA0 | an) << 16) | ((ln << 12) | ((hn << 8) | bn)))))
   (thumb-mla d a b acc)                                      ; d = a*b + acc
      (: dn (rn d) an (rn a) bn (rn b) cn (rn acc)
         (t32 (((0xFB00 | an) << 16) | ((cn << 12) | ((dn << 8) | bn)))))
   (thumb-clz d s) (: dn (rn d) sn (rn s) (t32 (((0xFAB0 | sn) << 16) | (0xF080 | ((dn << 8) | sn)))))
   ;; the 64-bit unsigned divide/remainder EXPANSION (moon's pair `/` and `%`): dividend r2:r3,
   ;; divisor r0:r1, result r0:r1. a 64-step restoring shift-subtract -- (rem:dividend) doubles,
   ;; a quotient bit rides in at bit 0; a carry OUT of rem (bit 64) forces the subtract (rem is
   ;; then >= 2^64 > divisor; the 64-bit subtract still lands right, the result is < divisor).
   ;; owns r2..r7 + r12-free; r4..r7 saved around it, so pool values above survive. branches are
   ;; FIXED narrow offsets (no labels, like thumb1's divide) -- the layout below is byte-counted:
   ;;   0 push*4, 16 movw*3, 28 LOOP adds/adcs*3, 44 bcs+12, 46 subs, 50 sbcs, 54 bcc+14,
   ;;   56 SUB subs, 60 sbcs, 64 addw#1, 68 NEXT subw, 72 cmp, 76 bne LOOP(-52), 78 tail.
   (t2-udivmod which)
      (catall [(thumb-push 'r4) (thumb-push 'r5) (thumb-push 'r6) (thumb-push 'r7)
                (thumb-movw 'r4 0) (thumb-movw 'r5 0) (thumb-movw 'r6 64)
                (thumb-aluop 'adds 'r2 'r2 'r2)                ; loop: (rem:dividend) <<= 1
                (thumb-aluop 'adcs 'r3 'r3 'r3)
                (thumb-aluop 'adcs 'r4 'r4 'r4)
                (thumb-aluop 'adcs 'r5 'r5 'r5)
                (t16 0xD204)                                   ; bcs +8 -> the subtract (bit 64 set)
                (thumb-aluop 'subs 'r7 'r4 'r0)                ; trial rem - divisor (flags only)
                (thumb-aluop 'sbcs 'r7 'r5 'r1)
                (t16 0xD305)                                   ; bcc +10 -> next (rem < divisor)
                (thumb-aluop 'subs 'r4 'r4 'r0)                ; commit: rem -= divisor
                (thumb-aluop 'sbcs 'r5 'r5 'r1)
                (thumb-addsub-imm 'add 'r2 'r2 1)              ; quotient bit (bit 0 is clear)
                (thumb-addsub-imm 'sub 'r6 'r6 1)              ; next: counter
                (thumb-cmp 'r6 0)
                (t16 0xD1E6)                                   ; bne loop (-52)
                (? (id? which 'div)
                   (thumb-movr 'r0 'r2 + thumb-movr 'r1 'r3)
                   (thumb-movr 'r0 'r4 + thumb-movr 'r1 'r5))
                (thumb-pop 'r7) (thumb-pop 'r6) (thumb-pop 'r5) (thumb-pop 'r4)])

   ;; loads/stores. LDR/STR Rt,[Rn,#imm12] (T3, positive 0..4095) else the T4 8-bit signed offset
   ;; (a frame slot's small negative), else the FAR lane: r12 (IP, the unnamed temp) carries
   ;; base+off and the access rides [r12, #0]. disp is a raw byte offset.
   (thumb-mem-far bn off)                                    ; r12 = off ; r12 += Rbn
      (thumb-li 'r12 off + t32 ((0xEB0C << 16) | (0x0C00 | bn)))
   (thumb-mem-r lbase ubase dt bn off)                       ; dt/bn already encodings
      (? ((off >= 0) && (off <= 4095))
         (t32 (((ubase | bn) << 16) | ((dt << 12) | off)))
         ((off >= -255) && (off < 0))
         (t32 (((lbase | bn) << 16) | ((dt << 12) | (0xC00 | (0 - off)))))   ; T4: P=1 U=0 W=0 = 0b1100, imm8=|off|
         (thumb-mem-far bn off + t32 (((ubase | 12) << 16) | (dt << 12))))       ; far: [r12, #0]
   (thumb-mem lbase ubase d base off) (thumb-mem-r lbase ubase (rn d) (rn base) off)
   ;; the word ops (`ld`/`st`) are 32-bit here (a 32-bit target's word); the sized family follows.
   (thumb-ld d base off)  (thumb-mem 0xF850 0xF8D0 d base off)   ; LDR.W  (T4 neg / T3 pos)
   (thumb-st base off s)  (thumb-mem 0xF840 0xF8C0 s base off)   ; STR.W
   (thumb-ld4 d base off) (thumb-mem 0xF850 0xF8D0 d base off)   ; int == word on a 32-bit target
   (thumb-st4 base off s) (thumb-mem 0xF840 0xF8C0 s base off)
   (thumb-ldu1 d base off) (thumb-mem 0xF810 0xF890 d base off)  ; LDRB (zero-extend)
   (thumb-ldu2 d base off) (thumb-mem 0xF830 0xF8B0 d base off)  ; LDRH
   (thumb-ld1 d base off)  (thumb-mem 0xF910 0xF990 d base off)  ; LDRSB (sign-extend)
   (thumb-ld2 d base off)  (thumb-mem 0xF930 0xF9B0 d base off)  ; LDRSH
   (thumb-st1 base off s)  (thumb-mem 0xF800 0xF880 s base off)  ; STRB
   (thumb-st2 base off s)  (thumb-mem 0xF820 0xF8A0 s base off)  ; STRH

   ;; indexed addressing: unlike v6-M, T32 has the scaled register offset in hardware. leax rides
   ;; ADD.W Rd, Rn, Rm, LSL #amt (reads both sources before the write, so d may alias either);
   ;; ldx/ldxb take the one-insn LDR/LDRB register form at disp 0, else synthesize off the dest.
   (thumb-shamt scale) (? (scale = 1) 0 (scale = 2) 1 (scale = 4) 2 (scale = 8) 3 (scare 'badscale scale))
   (thumb-addsh d b ix amt)                                   ; ADD.W Rd, Rb, Rix, LSL #amt
      (: dn (rn d) bn (rn b) xn (rn ix)
         (t32 (((0xEB00 | bn) << 16) | ((dn << 8) | (((amt & 3) << 6) | xn)))))
   (thumb-leax d base ix scale disp)
      (catall [(thumb-addsh d base ix (thumb-shamt scale))
                (? (disp = 0) () (thumb-addsub-imm 'add d d disp))])
   (thumb-memsh lbase d b ix amt)                             ; LDR/LDRB Rt, [Rn, Rm, LSL #amt]
      (: dn (rn d) bn (rn b) xn (rn ix)
         (t32 (((lbase | bn) << 16) | ((dn << 12) | ((amt << 4) | xn)))))
   (thumb-ldx d base ix scale disp)
      (? (disp = 0) (thumb-memsh 0xF850 d base ix (thumb-shamt scale))
         (catall [(thumb-addsh d base ix (thumb-shamt scale)) (thumb-ld d d disp)]))
   (thumb-ldxb d base ix scale disp)                          ; zero-extending byte (x64 movzx)
      (? (disp = 0) (thumb-memsh 0xF810 d base ix (thumb-shamt scale))
         (catall [(thumb-addsh d base ix (thumb-shamt scale)) (thumb-ldu1 d d disp)]))

   ;; MOV (register) T1, the 16-bit hi-register form -- the ONLY move that legally targets sp
   ;; (MOV.W / ADDW with Rd=sp,Rn!=sp are UNPREDICTABLE on ARMv7-M). reaches all of r0..r15.
   (thumb-movr d s) (: dn (rn d) sn (rn s) (t16 (0x4600 | ((((dn >> 3) & 1) << 7) | ((sn << 3) | (dn & 7))))))
   ;; lea with a zero offset is a plain register copy -- ride movr so `lea sp, fp, 0` (the frame
   ;; teardown) and `lea fp, sp, 0` (setup) encode as the sp-legal 16-bit move.
   (thumb-lea d base off)  (? (off = 0) (thumb-movr d base) (thumb-addsub-imm 'add d base off))

   (thumb-push s) (: st (rn s) (t32 (0xF84D0000 | ((st << 12) | (0xD00 | 4)))))   ; STR Rt,[sp,#-4]!  P=1 U=0 W=1, imm8=4
   (thumb-pop  d) (: dt (rn d) (t32 (0xF85D0000 | ((dt << 12) | (0xB00 | 4)))))   ; LDR Rt,[sp],#4    P=0 U=1 W=1 (bit11=1), imm8=4

   ;; sign/zero-extend the low w bytes in place. w=4 is a NO-OP -- an int is already the full 32-bit
   ;; word here (unlike the 64-bit targets, where sx4/zx4 widen 32->64). w=1/2 -> SXTB/SXTH / UXTB/UXTH.
   (thumb-sx w d) (? (w = 4) ()
                     (: dn (rn d) (t32 ((? (w = 1) 0xFA4FF080 0xFA0FF080) | ((dn << 8) | dn)))))   ; SXTB / SXTH Rd, Rd
   (thumb-zx w d) (? (w = 4) ()
                     (: dn (rn d) (t32 ((? (w = 1) 0xFA5FF080 0xFA1FF080) | ((dn << 8) | dn)))))   ; UXTB / UXTH Rd, Rd
   (thumb-neg d) (: dn (rn d) (t32 (((0xF1C0 | dn) << 16) | (dn << 8))))          ; RSB Rd, Rd, #0
   (thumb-not d) (: dn (rn d) (t32 (0xEA6F0000 | ((dn << 8) | dn))))              ; MVN.W Rd, Rm
   (thumb-inc d) (thumb-addsub-imm 'add d d 1)
   (thumb-dec d) (thumb-addsub-imm 'sub d d 1)

   ;; shifts by an immediate: MOV.W Rd, Rm, <type> #count  (type: lsl 0 lsr 1 asr 2 ror 3). in place.
   (thumb-shift op d c)
      (: dn (rn d)
         ty (? (op = 'shl) 0 (op = 'shr) 1 (op = 'sar) 2 (op = 'ror) 3 (scare 'badshift op))
         hw2 ((((c >> 2) & 7) << 12) | ((dn << 8) | (((c & 3) << 6) | ((ty << 4) | dn))))
         (t32 (0xEA4F0000 | hw2)))
   ;; shift by a register: LSL/LSR/ASR.W Rd,Rd,Rc, d in place. the IR contract parks the count in
   ;; r1, but the T32 encoding takes ANY count register -- the pair-shift lanes (whose r1 is the
   ;; pair's high word) ride that freedom, so no r1 guard here (thumb1 keeps its restriction).
   (thumb-shiftv op d c)
      (: dn (rn d) cn (rn c)
         base (? (op = 'shlv) 0xFA00 (op = 'shrv) 0xFA20 (op = 'sarv) 0xFA40 (scare 'badshiftv op))
         (t32 (((base | dn) << 16) | (0xF000 | ((dn << 8) | cn)))))

   ;; signed/unsigned divide (T1); remainder = a - (a/b)*b via MLS and a scratch t.
   (thumb-div d a b)  (: dn (rn d) an (rn a) bn (rn b) (t32 (((0xFB90 | an) << 16) | (0xF0F0 | ((dn << 8) | bn)))))   ; SDIV
   (thumb-udiv d a b) (: dn (rn d) an (rn a) bn (rn b) (t32 (((0xFBB0 | an) << 16) | (0xF0F0 | ((dn << 8) | bn)))))   ; UDIV
   (thumb-mls d n m a) (: dn (rn d) nn (rn n) mn (rn m) an (rn a) (t32 (((0xFB00 | nn) << 16) | ((an << 12) | ((dn << 8) | (0x10 | mn))))))
   (thumb-rem d a b t)  (thumb-div t a b + thumb-mls d t b a)          ; t=a/b ; d = a - t*b
   (thumb-urem d a b t) (thumb-udiv t a b + thumb-mls d t b a)

   ;; test a, b -> TST.W (ANDS Rn, Rm, Rd discarded to pc/15). register only for M1.
   (thumb-test a b)
      (? (reg? b) (: an (rn a) bn (rn b) (t32 (((0xEA10 | an) << 16) | (0x0F00 | bn))))
         (scare 'thumb-test-imm ['test a b]))

   ;; set: d <- (cond) ? 1 : 0, via an IT block. MOV d,#0 ; IT cond ; MOV d,#1 (the conditional one).
   ;; MOVW reaches all of r0..r12 (the narrow MOV only r0..r7), so use it uniformly.
   (thumb-set cond d)
      (: cc (look thumb-cc cond 'badcc)
         (catall [(thumb-movw d 0) (t16 (0xBF08 | (cc << 4))) (thumb-movw d 1)]))

   ;; -- scalar-double VFP (moon rung 4): f0..f15 -> d0..d15 (fpv5-d16, the M7's FPU). the IR
   ;; keeps the SSE mnemonics two-address (d = op(d, s)); VFP is three-address, so d repeats as
   ;; the first source. the single-precision views ride S[2n] -- always EVEN, the low half of
   ;; Dn, so the odd-register encoding bits never arise. d15 (S30) is the converters' scratch:
   ;; f15 is RESERVED on this target. movqxr/movqrx move a GP PAIR (a 64-bit pattern on a
   ;; 32-bit target): the named gp register is the LOW word, register+1 the high -- one VMOV.
   ;; every encoding below was checked against llvm-mc --triple=thumbv7em-none-eabihf. --
   thumb-fregs (fregs 16)
   (frn r) (look thumb-fregs r 'badfreg)
   (t2-fp3 hi lo d m) (: dn (frn d) mn (frn m) (t32 (((hi | dn) << 16) | ((dn << 12) | (lo | mn)))))
   (thumb-addsd d m) (t2-fp3 0xEE30 0xB00 d m)                ; VADD.F64 Dd, Dd, Dm
   (thumb-subsd d m) (t2-fp3 0xEE30 0xB40 d m)                ; VSUB.F64
   (thumb-mulsd d m) (t2-fp3 0xEE20 0xB00 d m)                ; VMUL.F64
   (thumb-divsd d m) (t2-fp3 0xEE80 0xB00 d m)                ; VDIV.F64
   (thumb-movsd d m) (: dn (frn d) mn (frn m) (t32 ((0xEEB0 << 16) | ((dn << 12) | (0xB40 | mn)))))
   (thumb-ucomisd a b)                                        ; VCMP.F64 Da, Db ; VMRS APSR_nzcv
      (: an (frn a) bn (frn b)                                ;   (flags land like a64 fcmp: unordered -> V)
         (t32 ((0xEEB4 << 16) | ((an << 12) | (0xB40 | bn))) + t32 0xEEF1FA10))
   ;; int <-> double through the target's own S view (VMOV S[2f],Rg is one word since 2f>>1 = f).
   (thumb-cvtxsd base f g)                                    ; VMOV S[2f], Rg ; VCVT.F64.{S,U}32 Df, S[2f]
      (: fn (frn f) gn (rn g)
         (t32 (((0xEE00 | fn) << 16) | ((gn << 12) | 0xA10))
          + t32 ((0xEEB8 << 16) | ((fn << 12) | (base | fn)))))
   (thumb-cvtsi2sd f g) (thumb-cvtxsd 0xBC0 f g)              ; signed
   (thumb-cvtui2sd f g) (thumb-cvtxsd 0xB40 f g)              ; unsigned (t32 has no 64-bit widen dodge)
   (thumb-cvtsdxi hi g f)                                     ; VCVT.{S,U}32.F64 S30, Df ; VMOV Rg, S30
      (: gn (rn g) fn (frn f)
         (t32 ((hi << 16) | (0xFBC0 | fn))
          + t32 ((0xEE1F << 16) | ((gn << 12) | 0xA10))))
   (thumb-cvttsd2si g f) (thumb-cvtsdxi 0xEEBD g f)           ; toward zero, signed
   (thumb-cvttsd2ui g f) (thumb-cvtsdxi 0xEEBC g f)           ; toward zero, unsigned
   (thumb-movqxr f glo)                                       ; VMOV Df, Rlo, Rlo+1 (the pair in)
      (: fn (frn f) gn (rn glo)
         (t32 (((0xEC40 | (gn + 1)) << 16) | ((gn << 12) | (0xB10 | fn)))))
   (thumb-movqrx glo f)                                       ; VMOV Rlo, Rlo+1, Df (the pair out)
      (: gn (rn glo) fn (frn f)
         (t32 (((0xEC50 | (gn + 1)) << 16) | ((gn << 12) | (0xB10 | fn)))))
   ;; VLDR/VSTR: imm8 words, 4-aligned, +-1020; past that the r12 far build (as thumb-mem).
   (thumb-fmem pbase nbase lo tn bn off)
      (: _ (? (0 = (off % 4)) 0 (scare 'fmem-align off))
         u (? (off < 0) (0 - off) off)
         (? (u <= 1020)
            (t32 ((((? (off < 0) nbase pbase) | bn) << 16) | ((tn << 12) | (lo | (u >> 2)))))
            (thumb-mem-far bn off + t32 (((pbase | 12) << 16) | ((tn << 12) | lo)))))
   (thumb-stsd base off s) (thumb-fmem 0xED80 0xED00 0xB00 (frn s) (rn base) off)   ; VSTR Ds
   (thumb-ldsd d base off) (thumb-fmem 0xED90 0xED10 0xB00 (frn d) (rn base) off)   ; VLDR Dd
   (thumb-stss base off s) (thumb-fmem 0xED80 0xED00 0xA00 (frn s) (rn base) off)   ; VSTR S[2s]
   (thumb-ldss d base off) (thumb-fmem 0xED90 0xED10 0xA00 (frn d) (rn base) off)   ; VLDR S[2d]
   (thumb-cvtss2sd d s) (: dn (frn d) sn (frn s) (t32 ((0xEEB7 << 16) | ((dn << 12) | (0xAC0 | sn)))))   ; VCVT.F64.F32 Dd, S[2s]
   (thumb-cvtsd2ss d s) (: dn (frn d) sn (frn s) (t32 ((0xEEB7 << 16) | ((dn << 12) | (0xBC0 | sn)))))   ; VCVT.F32.F64 S[2d], Ds
   (thumb-smull dl dh a b)                                    ; SMULL RdLo, RdHi, Rn, Rm (T1)
      (: ln (rn dl) hn (rn dh) an (rn a) bn (rn b)
         (t32 (((0xFB80 | an) << 16) | ((ln << 12) | ((hn << 8) | bn)))))

   ;; control. B/BL carry a scrambled pc-relative offset in a bitfield; the fixup stashes the cond (or 0)
   ;; in aux and `patch` builds the whole 4-byte word. an unconditional B (jmp) rides T4, a conditional
   ;; one (br) rides T3, and BL (call) rides T1 -- all 32-bit, all 4-byte fixups.
   (thumb-jmp  label) [(fix 4 'b   label 0)]
   (thumb-call label) [(fix 4 'bl  label 0)]
   (thumb-br cond label) (: cc (look thumb-cc cond 'badcc)
                            [(fix 4 'bcc label cc)])
   (thumb-jmpr d)  (: dn (rn d) (t16 (0x4700 | (dn << 3))))    ; BX Rm
   (thumb-callr d) (: dn (rn d) (t16 (0x4780 | (dn << 3))))    ; BLX Rm

   ;; -- the SYSTEM lane. M-profile draws ONE shareability domain, so a barrier takes only
   ;; `sy`: naming any other asks for a distinction this core does not have, and widening it
   ;; to SY in silence would let a shared lay read as portable when it is not. (arm64.l's
   ;; barrier table is the wider vocabulary, for the arch that earns it.)
   (thumb-dsb opt) (? (id? opt 'sy) (t32 0xF3BF8F4F) (scare 'thumb-barrier-opt opt))
   ;; BKPT #imm8 -- the debugger halt. `trap` is this same instruction at 0; the immediate
   ;; needs a door of its own because 0xAB is the SEMIHOSTING call, which is how a board with
   ;; no uart says exit(n) to qemu.
   (thumb-brk imm) (? ((imm >= 0) && (imm <= 255)) (t16 (0xBE00 | imm))
                      (scare 'thumb-brk-imm imm))
   ;; MRS Rd, <sysm> -- the special registers, which are not in the general file and so have
   ;; no other door. only the two stack pointers are named: a fault handler asks which one was
   ;; active, and that is the whole of what this tree needs to read.
   thumb-sysm (tabof [('msp >< 8) ('psp >< 9)])
   (thumb-mrs d sys)
      (: sm (look thumb-sysm sys 'badsysm)
         (t32 (0xF3EF0000 | (0x8000 | (rn d << 8) | sm))))

   ;; the dispatch: an EMITTER TABLET (shapes in holo.l's ir-arity), rows partial-applied,
   ;; nullaries their halfwords outright, `raw` the identity. the alu group pins off its rows.
   thumb-emitters (emsof
        [('li >< thumb-li) ('mov >< thumb-mov) ('cmp >< thumb-cmp) ('imul >< thumb-imul)
          ('umull >< thumb-umull) ('smull >< thumb-smull) ('mla >< thumb-mla)
          ('addsd >< thumb-addsd) ('subsd >< thumb-subsd) ('mulsd >< thumb-mulsd)
          ('divsd >< thumb-divsd) ('movsd >< thumb-movsd) ('ucomisd >< thumb-ucomisd)
          ('cvtsi2sd >< thumb-cvtsi2sd) ('cvtui2sd >< thumb-cvtui2sd)
          ('cvttsd2si >< thumb-cvttsd2si) ('cvttsd2ui >< thumb-cvttsd2ui)
          ('movqxr >< thumb-movqxr) ('movqrx >< thumb-movqrx)
          ('stsd >< thumb-stsd) ('ldsd >< thumb-ldsd) ('stss >< thumb-stss) ('ldss >< thumb-ldss)
          ('cvtss2sd >< thumb-cvtss2sd) ('cvtsd2ss >< thumb-cvtsd2ss)
          ('clz >< thumb-clz)
          ('udivll >< t2-udivmod 'div) ('uremll >< t2-udivmod 'rem)        ; nullary: the whole expansion
          ('ld >< thumb-ld) ('st >< thumb-st)
          ('ld1 >< thumb-ld1) ('ld2 >< thumb-ld2) ('ld4 >< thumb-ld4)
          ('st1 >< thumb-st1) ('st2 >< thumb-st2) ('st4 >< thumb-st4)
          ('ldu1 >< thumb-ldu1) ('ldu2 >< thumb-ldu2)
          ('ldu4 >< thumb-ld4)                               ; 32-bit zero-extend == word load here
          ('sx1 >< thumb-sx 1) ('sx2 >< thumb-sx 2) ('sx4 >< thumb-sx 4)
          ('zx1 >< thumb-zx 1) ('zx2 >< thumb-zx 2) ('zx4 >< thumb-zx 4)
          ('shlv >< thumb-shiftv 'shlv) ('shrv >< thumb-shiftv 'shrv) ('sarv >< thumb-shiftv 'sarv)
          ('shl >< thumb-shift 'shl) ('shr >< thumb-shift 'shr) ('sar >< thumb-shift 'sar)
          ('rol >< thumb-shift 'rol) ('ror >< thumb-shift 'ror)
          ('lea >< thumb-lea) ('lean >< thumb-lea)
          ('la >< thumb-la) ('lapool >< thumb-lapool)
          ('leax >< thumb-leax) ('ldx >< thumb-ldx) ('ldxb >< thumb-ldxb)
          ('push >< thumb-push) ('pop >< thumb-pop)
          ('neg >< thumb-neg) ('not >< thumb-not) ('inc >< thumb-inc) ('dec >< thumb-dec)
          ('jmp >< thumb-jmp) ('br >< thumb-br) ('jmpr >< thumb-jmpr) ('callr >< thumb-callr)
          ('set >< thumb-set) ('test >< thumb-test)
          ('div >< thumb-div) ('rem >< thumb-rem) ('udiv >< thumb-udiv) ('urem >< thumb-urem)
          ('call >< thumb-call)
          ('raw >< 1)
          ('dsb >< thumb-dsb) ('brk >< thumb-brk)            ; -- the system lane --
          ('mrs >< thumb-mrs)
          ('isb >< t32 0xF3BF8F6F)                           ; isb sy
          ('wfi >< t16 0xBF30)                               ; wait for interrupt
          ('ret >< t16 0x4770)                               ; BX lr
          ('sys >< t16 0xDF00)                               ; SVC #0
          ('nop >< t16 0xBF00)
          ('trap >< t16 0xBE00)])                            ; BKPT #0
   _ (foldl (\ _ p (emrow thumb-emitters <p (thumb-aluop <p))) 0 thumb-alu-rows)
   (thumb-lower ins) (: f lower-via (? (lit? f) (f thumb-emitters ins) (scare 'holo-bake-stale 'lower-via)))

   ;; a relocation. Thumb pc reads as (site + 4), and the branch fields hold a /2 (halfword) offset
   ;; scrambled across S/J1/J2. patch computes the byte offset O = target-(site+4) then packs it.
   ;;   b   (T4, unconditional, jmp):  imm25 = S:I1:I2:imm10:imm11:0, I1=~(J1^S), I2=~(J2^S)
   ;;   bl  (T1, call):                same offset packing, hw2 base 0xD000 (vs B's 0x9000)
   ;;   bcc (T3, conditional br):      imm21 = S:J2:J1:imm6:imm11:0 (no ~), cond in hw1, aux carries it
   (thumb-patch kind target site width aux)
      (: o (target - (site + 4))
         (? (member? kind '(b bl))
            (: imm11 ((o >> 1) & 2047)  imm10 ((o >> 12) & 1023)
               i2 ((o >> 22) & 1)  i1 ((o >> 23) & 1)  s ((o >> 24) & 1)
               j1 (((i1 ^ 1) ^ s) & 1)  j2 (((i2 ^ 1) ^ s) & 1)
               hw1 (0xF000 | ((s << 10) | imm10))
               hw2 ((? (kind = 'bl) 0xD000 0x9000) | ((j1 << 13) | ((j2 << 11) | imm11)))
               (t32 ((hw1 << 16) | hw2)))
            (kind = 'bcc)
            (: imm11 ((o >> 1) & 2047)  imm6 ((o >> 12) & 63)
               j1 ((o >> 18) & 1)  j2 ((o >> 19) & 1)  s ((o >> 20) & 1)
               hw1 (0xF000 | ((s << 10) | ((aux << 6) | imm6)))
               hw2 (0x8000 | ((j1 << 13) | ((j2 << 11) | imm11)))
               (t32 ((hw1 << 16) | hw2)))
            ; la's absolute halves (aux = Rd): internal resolve scatters the address in; a .o keeps
            ; the pair relocated (obj.l fills the base encoding with the in-field addend instead).
            (kind = 'movw16) (thumb-mw16 0xF240 aux (target & 65535))
            (kind = 'movt16) (thumb-mw16 0xF2C0 aux ((target >> 16) & 65535))
            (scare 'badfix kind)))

   ;; the LINKER's door onto that same scatter. ARM is REL, so the field keeps both the
   ;; addend and the aux -- movw16/movt16 read their Rd back out of hw2, and the ADDEND is
   ;; CHECKED rather than decoded: the base encoding thumb-patch itself bakes is rebuilt and
   ;; compared, so a field carrying any other one refuses instead of linking somewhere
   ;; plausible and wrong. no inverse scatter exists anywhere, which is the point.
   ;; ⚠ the thumb bit rides in `target` (the linker folds it out of the symbol) and passes
   ;; through untouched -- every field here shifts the offset right by at least one.
   (thumb-refix kind bs target site width)
      (: aux (? (member? kind '(movw16 movt16)) (at bs 3 & 15) 0)     ; MOVW/MOVT T3: Rd, hw2 bits 11:8
         _ (? (thumb-patch kind 0 0 width aux = bs) 0 (scare 'thumb-refix-addend kind))
         (thumb-patch kind target site width aux))

   thumb2 (tablet 0)
   _ (pin thumb2 'lower thumb-lower)
   _ (pin thumb2 'patch thumb-patch)
   _ (pin thumb2 'refix thumb-refix)
   _ (pin thumb2 'nop '(0 191))                ; the ('align n) filler unit: NOP (16-bit) bf00 LE
   _ (defbackend 'thumb2 thumb2)
   _ (defbackend 'cortex-m7 thumb2)
   _ (defbackend 'thumb2sp thumb2))   ; the SP-only-FPU face (playdate): same encoder, gen softens f64
) ; module 'holo
