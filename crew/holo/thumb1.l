; thumb1 -- the ARMv6-M (Cortex-M0/M0+, e.g. RP2040) BACKEND for the neutral assembler (crew/holo/holo.l),
; a SKELETON. sibling to thumb2.l but a LOWER rung: this is 16-bit Thumb-1, not the wide Thumb-2, so
; despite the shared "thumb" name almost every encoder here is its own animal. registers r0..r15
; (r13=sp, r14=lr, r15=pc); the DATA-PROCESSING surface is TWO-ADDRESS and reaches only r0..r7 (the
; "low" regs) -- the hi regs (r8..r15) are touchable only by MOV/ADD/CMP-hi, BX/BLX, and push/pop of
; lr/pc. load after holo.l. NOTHING here is llvm-mc-checked yet: every word wants a pass against
; `llvm-mc --triple=thumbv6m-none-eabi --show-encoding` before it earns a place in test/holo/golden.l.
;
; what v6-M TAKES AWAY vs thumb2 (why this isn't a crib of thumb2.l):
;   * no MOVW/MOVT  -> `li` synthesizes the constant (MOVS + LSLS/ADDS byte-build; a literal pool is
;                      the later optimization -- see t1-li). works pool-free today.
;   * no SDIV/UDIV/MLS -> div/rem expand INLINE to a restoring shift-subtract loop (t1-udivmod +
;                      signed abs/fixup) -- self-contained, no callout, owns the r0..r3 clobber set.
;   * no IT block   -> `set` is a branch-over instead of a predicated MOV.
;   * no wide ALU   -> two-address low-reg ops; add/sub keep a 3-address register form, the logicals
;                      and mul do not (mov-first when d != a, with the d==b hazard flagged).
;   * branch ranges -> B is +-2KB, Bcc is +-256B, only BL is wide (+-16MB). far B/Bcc want a VENEER;
;                      the range check scares for now (see t1-patch) -- a real driver pass is TODO.
;   * loads/stores  -> 5-bit POSITIVE imm offsets only (no negative frame slots): moon's frame layout
;                      must keep offsets >= 0. signed loads have no immediate form -> LDRB+SXTB / LDRH+SXTH.
;
; the byte plumbing: every v6-M instruction is ONE 16-bit halfword EXCEPT BL (two halfwords, hi first,
; each le16 -- same trap thumb2.l documents). `t16` emits one narrow halfword; `t32` the BL pair.

(module 'holo
(: ;; -- registers: abstract number = hardware number for r0..r12; sp/lr/pc/fp named. (shared shape
   ;;    with thumb2.l -- could be factored to an arm32-common, but it's ~15 lines, little leverage.) --
   t1-regs (tabof
        `(('sp >< 13) ('lr >< 14) ('pc >< 15) ('fp >< 7)   ; fp=r7: the Thumb frame pointer (a LOW reg, so slot
           ;; loads reach it with the imm5 form -- r11 could not; moon speaks 'fp, this backend numbers it)
           ('r0 >< 0) ('r1 >< 1) ('r2 >< 2) ('r3 >< 3)  ('r4 >< 4)
           ('r5 >< 5) ('r6 >< 6) ('r7 >< 7) ('r8 >< 8)  ('r9 >< 9)
           ('r10 >< 10) ('r11 >< 11) ('r12 >< 12)))
   (rn r) (look t1-regs r 'badreg)
   (reg-eq a b) (rn a = rn b)
   (lo? r) (rn r <= 7)                   ; a "low" register -- the only ones most 16-bit ops reach
   (need-lo r why) (? (lo? r) 0 (scare why r))   ; guard: v6-M op wants r0..r7

   ;; -- condition codes: IR name -> the 4-bit AArch32 cond field (same encoding aarch64/thumb2 use). --
   t1-cc (tabof
        `(('eq >< 0) ('ne >< 1) ('ae >< 2) ('below >< 3) ('s >< 4) ('ns >< 5)
           ('vs >< 6) ('vc >< 7) ('p >< 6) ('np >< 7)
           ('above >< 8) ('be >< 9) ('ge >< 10) ('lt >< 11) ('gt >< 12) ('le >< 13)))
   (ccn c) (look t1-cc c 'badcc)

   ;; -- emit. one narrow halfword; t32 is the BL pair (hi halfword first, each little-endian). --
   (t16 w) (le16 (w & 65535))
   (t32 w) (le16 (w >> 16) + le16 (w & 65535))

   ;; ---- constant materialization (no MOVW/MOVT on v6-M) ----
   ;; MOVS Rd,#imm8 (T1); MVNS/LSLS/ADDS the byte-build for the rest. all LOW-REG.
   (t1-movs d imm8) (: _ (need-lo d 'movs-hi) dn (rn d) (t16 (0x2000 | ((dn << 8) | (imm8 & 255)))))
   (t1-mvns d s)    (: dn (rn d) sn (rn s) (t16 (0x43C0 | ((sn << 3) | dn))))         ; MVNS Rd, Rm  (Rd = ~Rm)
   (t1-lsls-imm d c) (: dn (rn d) (t16 (0x0000 | (((c & 31) << 6) | ((dn << 3) | dn)))))  ; LSLS Rd,Rd,#c
   (t1-lsls-rri d m c) (: dn (rn d) mn (rn m) (t16 (0x0000 | (((c & 31) << 6) | ((mn << 3) | dn)))))  ; LSLS Rd,Rm,#c (3-addr)
   (t1-adds-i8 d imm8) (: _ (need-lo d 'adds-hi) dn (rn d) (t16 (0x3000 | ((dn << 8) | (imm8 & 255)))))  ; ADDS Rdn,#imm8
   ;; byte-build any 32-bit u: MOVS top byte, then (LSLS 8; ADDS byte) down the word. 7 halfwords worst
   ;; case -- a literal pool (LDR Rd,[pc,#off] + a pooled word + a pc-rel fixup) is the size win, LATER.
   (t1-li-synth d u)
      (catall `((t1-movs d ((u >> 24) & 255))
                (t1-lsls-imm d 8) (t1-adds-i8 d ((u >> 16) & 255))
                (t1-lsls-imm d 8) (t1-adds-i8 d ((u >> 8) & 255))
                (t1-lsls-imm d 8) (t1-adds-i8 d (u & 255))))
   (t1-li d imm)
      (: u (? (imm < 0) (imm + 4294967296) imm)
         (? (u <= 255) (t1-movs d u)
            ((imm < 0) && (imm >= -256))                          ; small negative: MOVS ~imm ; MVNS
               (t1-movs d (((0 - imm) - 1) & 255) + t1-mvns d d)
            (t1-li-synth d u)))

   ;; MOV (register) T1 hi-form -- the ONLY move that legally targets sp; reaches all r0..r15, no flags.
   (t1-movr d s) (: dn (rn d) sn (rn s) (t16 (0x4600 | ((((dn >> 3) & 1) << 7) | ((sn << 3) | (dn & 7))))))
   ;; ADD (register) T2 hi-form: ADD Rdn, Rdn, Rm -- two-address, reaches ALL of r0..r14, no flags.
   ;; the far-mem lane leans on it (a hi base folds into a low scratch without touching flags).
   (t1-add-any d m) (: dn (rn d) mn (rn m) (t16 (0x4400 | ((((dn >> 3) & 1) << 7) | ((mn << 3) | (dn & 7))))))

   ;; ---- ALU ----
   ;; add/sub: genuine 3-address register forms (T1, low regs). d = a OP b.
   (t1-add-rrr d a b) (: dn (rn d) an (rn a) bn (rn b) (t16 (0x1800 | ((bn << 6) | ((an << 3) | dn)))))
   (t1-sub-rrr d a b) (: dn (rn d) an (rn a) bn (rn b) (t16 (0x1A00 | ((bn << 6) | ((an << 3) | dn)))))
   (t1-addsub op d a b) (? (reg? b) (? (op = 'add) (t1-add-rrr d a b) (t1-sub-rrr d a b))
                            (t1-addsub-imm op d a b))
   ;; add/sub #imm: #imm3 3-address (0..7), else #imm8 two-address (needs d==a). negative flips the op.
   (t1-addsub-imm op d a imm)
      (: pos (? (imm < 0) (0 - imm) imm)
         fop (? (imm < 0) (? (op = 'add) 'sub (op = 'sub) 'add op) op)
         dn (rn d) an (rn a)
         (? ((dn = 13) && (an = 13))                       ; SUB/ADD SP, SP, #imm7*4 -- the ONLY sp-arith form on v6-M;
               (: _ (? (0 = (pos & 3)) 0 (scare 'sp-adj-align imm))   ; a big frame CHUNKS through 508-byte steps
                  (chunks p acc)
                   (? (p <= 0) acc
                      (: c (? (508 < p) 508 p)
                         (chunks (p - c) (acc + t16 ((? (fop = 'add) 0xB000 0xB080) | (c >> 2))))))
                  (chunks pos ()))
            (pos <= 7) (t16 ((? (fop = 'add) 0x1C00 0x1E00) | ((pos << 6) | ((an << 3) | dn))))   ; imm3 (3-addr, low)
            (pos <= 255) ((? (dn = an) () (t1-movr d a))  ; imm8 (2-addr): mov d,a first when d != a
                          + t16 ((? (fop = 'add) 0x3000 0x3800) | ((dn << 8) | pos)))
            ; far: build the immediate in a saved low scratch, then the 3-address register form
            ; (the ADDS/SUBS family sets flags regardless, so t1-li's flag writes cost nothing new)
            (: _ (need-lo d 'addsub-far-hi) _ (need-lo a 'addsub-far-hi)
               s (t1-lowscratch d a '(r0 r1 r2 r3 r4 r5 r6 r7))
               (catall `((t1-push s) (t1-li s pos)
                         ((? (fop = 'add) t1-add-rrr t1-sub-rrr) d a s) (t1-pop s))))))
   ;; the two-address logicals + mul (format-4, low regs): Rd = Rd OP Rm. every f4 op here is
   ;; COMMUTATIVE (and/or/xor/mul), so d==b just swaps the operand read; else mov-first when d != a.
   t1-f4 (tabof `(('and >< 0x4000) ('or >< 0x4300) ('xor >< 0x4040) ('imul >< 0x4340)))
   (t1-alu2 op d a b)
      (: base (peep t1-f4 op 0)
         dn (rn d)
         (? (reg-eq d a) (t16 (base | ((rn b << 3) | dn)))
            (reg-eq d b) (t16 (base | ((rn a << 3) | dn)))
            (t1-movr d a + t16 (base | ((rn b << 3) | dn)))))
   ;; the carry family (the pair kit rides it): ADCS/SBCS Rdn,Rm (format-4, two-address, flag-consuming).
   ;; MOV (hi-form) never touches flags, so the mov-first staging keeps the carry intact. ADCS is
   ;; commutative (d==b swaps); SBCS is not -- d==b borrows a saved low scratch (push/pop keep flags).
   (t1-adc d a b)
      (: dn (rn d)
         (? (reg-eq d a) (t16 (0x4140 | ((rn b << 3) | dn)))
            (reg-eq d b) (t16 (0x4140 | ((rn a << 3) | dn)))
            (t1-movr d a + t16 (0x4140 | ((rn b << 3) | dn)))))
   (t1-sbc d a b)
      (: dn (rn d)
         (? (reg-eq d a) (t16 (0x4180 | ((rn b << 3) | dn)))
            (reg-eq d b)
             (: s (t1-lowscratch d a '(r0 r1 r2 r3 r4 r5 r6 r7))
                (catall `((t1-push s) (t1-movr s b) (t1-movr d a)
                          (t16 (0x4180 | ((rn s << 3) | rn d))) (t1-pop s))))
            (t1-movr d a + t16 (0x4180 | ((rn b << 3) | dn)))))

   (t1-cmp a b)                                            ; CMP Rn,Rm (low T1 / hi T2) or CMP Rn,#imm8
      (? (reg? b)
         (: an (rn a) bn (rn b)
            (? ((an <= 7) && (bn <= 7)) (t16 (0x4280 | ((bn << 3) | an)))              ; T1, both low
               (t16 (0x4500 | ((((an >> 3) & 1) << 7) | ((bn << 3) | (an & 7)))))))    ; T2, hi allowed
         (: _ (? (b < 0) (scare 'cmp-neg-imm b) 0)         ; no CMP #neg (CMN is reg-only) -- immok blocks upstream
            _ (? (b <= 255) 0 (scare 'cmp-imm-range b)) _ (need-lo a 'cmp-hi-imm)
            (t16 (0x2800 | ((rn a << 8) | b)))))

   (t1-test a b)                                           ; TST Rn,Rm (format-4, register only, low)
      (? (reg? b) (: an (rn a) bn (rn b) (t16 (0x4200 | ((bn << 3) | an))))
         (scare 'test-imm `('test a b)))

   ;; ---- loads / stores. 5-bit POSITIVE imm (word imm5*4, half imm5*2, byte imm5). sp-rel word is imm8*4.
   ;;      an out-of-reach offset (or a hi register) rides the FAR lane below. ----
   ;; the far/hi lane: synthesize base+off in a register, then the [Rn,#0] form. a LOAD whose low dest
   ;; is distinct from base builds the address IN the dest (li d,off ; add d,base ; op d,[d]); every
   ;; other shape borrows a saved low scratch (push s ; li s,off ; add s,base ; op via s ; pop s) --
   ;; t1-add-any reaches a hi base, and a hi LOAD dest lands via MOV d,s (hi-legal, no flags). the
   ;; classic customers: the callr park reload (ld r12,r4,slot) and any frame slot past imm5 reach.
   ;; NOTE t1-li sets flags -- sound because a mem op never sits between a cmp and its fused consumer.
   (t1-mem-far base16 sh d base off ldq)
      (: (op1 rd rb) (t16 (base16 | ((rn rb << 3) | rn rd)))           ; the [Rb,#0] form (imm5=0)
         (? (ldq && lo? d && !(reg-eq d base))
            (catall `((t1-li d off) (t1-add-any d base) (op1 d d)))
            (: s (t1-lowscratch d base '(r0 r1 r2 r3 r4 r5 r6 r7))
               (? ldq
                  (catall `((t1-push s) (t1-li s off) (t1-add-any s base) (op1 s s) (t1-movr d s) (t1-pop s)))
                  (: _ (need-lo d 'st-src-hi)                          ; a STORE's source has no bounce: must be low
                     (catall `((t1-push s) (t1-li s off) (t1-add-any s base) (op1 d s) (t1-pop s))))))))
   (t1-mem base16 sp16 sh d base off ldq)                 ; base16: [Rn,#imm5] op ; sp16: [sp,#imm8] op
      (: dn (rn d)
         (? (reg-eq base 'sp)
            (: _ (? (&& (off >= 0) (0 = (off & 3)) ((off >> 2) <= 255)) 0 (scare 'mem-sp-range off)) _ (need-lo d 'mem-sp-hi)
               (t16 (sp16 | ((dn << 8) | (off >> 2)))))
            (: u (off >> sh)
               ok (lo? d && lo? base &&
                   (off >= 0) && (0 = (off & ((1 << sh) - 1))) && (u <= 31))
               (? ok (t16 (base16 | ((u << 6) | ((rn base << 3) | dn))))
                  (t1-mem-far base16 sh d base off ldq)))))
   (t1-ld  d base off) (t1-mem 0x6800 0x9800 2 d base off 1)   ; LDR
   (t1-st  base off s) (t1-mem 0x6000 0x9000 2 s base off ())  ; STR
   (t1-ldb d base off) (t1-mem 0x7800 0x7800 0 d base off 1)   ; LDRB (no sp16 form; sp path unused for byte)
   (t1-stb base off s) (t1-mem 0x7000 0x7000 0 s base off ())  ; STRB
   (t1-ldh d base off) (t1-mem 0x8800 0x8800 1 d base off 1)   ; LDRH
   (t1-sth base off s) (t1-mem 0x8000 0x8000 1 s base off ())  ; STRH
   ;; signed loads have no immediate form on v6-M -> unsigned load then sign-extend in place.
   (t1-sxb d) (: dn (rn d) (t16 (0xB240 | ((dn << 3) | dn))))   ; SXTB Rd,Rd
   (t1-sxh d) (: dn (rn d) (t16 (0xB200 | ((dn << 3) | dn))))   ; SXTH Rd,Rd
   (t1-uxb d) (: dn (rn d) (t16 (0xB2C0 | ((dn << 3) | dn))))   ; UXTB Rd,Rd
   (t1-uxh d) (: dn (rn d) (t16 (0xB280 | ((dn << 3) | dn))))   ; UXTH Rd,Rd
   (t1-lds1 d base off) (t1-ldb d base off + t1-sxb d)          ; LDRSB == LDRB + SXTB
   (t1-lds2 d base off) (t1-ldh d base off + t1-sxh d)          ; LDRSH == LDRH + SXTH

   ;; ---- indexed addressing: d <- base + index*scale (+disp). v6-M has no scaled-index memory op, so
   ;;      synthesize LSLS+ADDS. moon's leax contract READS both sources before writing (x64 SIB / arm64
   ;;      add-shifted), so d may ALIAS base or index -- and the two live sources are always DISTINCT regs.
   ;;      scale 1 -> ADDS (3-addr, alias-safe outright). scale>1 shifts index by log2: shift into d works
   ;;      unless d==base (that shift would clobber base first), so that lone case borrows a saved low temp.
   (t1-lg2 scale) (? (scale = 2) 1 (scale = 4) 2 (scale = 8) 3 (scale = 1) 0 (scare 'badscale scale))
   (t1-lowscratch a b lst)                                ; first low reg not aliasing a or b
      (? !lst (scare 'no-scratch `(a b))
         (: r <lst (? (reg-eq r a || reg-eq r b) (t1-lowscratch a b >lst) r)))
   (t1-eaddr d base index scale)                          ; d <- base + index*scale, honoring d==base|index
      (: k (t1-lg2 scale)
         (? (scale = 1) (t1-add-rrr d base index)
            !(reg-eq d base) (t1-lsls-rri d index k + t1-add-rrr d base d)
            (: s (t1-lowscratch d index '(r0 r1 r2 r3 r4 r5 r6 r7))
               (catall `((t1-push s) (t1-lsls-rri s index k) (t1-add-rrr d base s) (t1-pop s))))))
   (t1-leax d base index scale disp)
      (t1-eaddr d base index scale + (? (disp = 0) () (t1-addsub-imm 'add d d disp)))
   (t1-ldx  d base index scale disp) (t1-eaddr d base index scale + t1-ld  d d disp)       ; fused leax+LDR
   (t1-ldxb d base index scale disp) (t1-eaddr d base index scale + t1-ldb d d disp)       ; fused leax+LDRB

   ;; ---- extends as IR ops (sx1/2 sign, zx1/2 zero; w=4 is a no-op -- int is already the full word) ----
   (t1-sx w d) (? (w = 4) () (w = 1) (t1-sxb d) (t1-sxh d))
   (t1-zx w d) (? (w = 4) () (w = 1) (t1-uxb d) (t1-uxh d))

   (t1-neg d) (: dn (rn d) (t16 (0x4240 | ((dn << 3) | dn))))   ; RSBS Rd,Rd,#0
   (t1-not d) (: dn (rn d) (t16 (0x43C0 | ((dn << 3) | dn))))   ; MVNS Rd,Rd
   (t1-inc d) (t1-addsub-imm 'add d d 1)
   (t1-dec d) (t1-addsub-imm 'sub d d 1)

   ;; shifts. by immediate (in place): LSLS/LSRS/ASRS Rd,Rd,#c. ROR has no immediate form on v6-M.
   (t1-shift op d c)
      (: dn (rn d)
         base (? (op = 'shl) 0x0000 (op = 'shr) 0x0800 (op = 'sar) 0x1000 (scare 'badshift op))
         (t16 (base | (((c & 31) << 6) | ((dn << 3) | dn)))))
   ;; by register (any LOW count reg; d in place, d != count): LSLS/LSRS/ASRS/RORS Rd,Rs (format-4).
   (t1-shiftv op d c)
      (: _ (need-lo c 'shiftv-count-hi) _ (need-lo d 'shiftv-hi)
         _ (? (reg-eq d c) (scare 'shiftv-dest-is-count `(op d c)) 0)
         dn (rn d) cn (rn c)
         base (? (op = 'shlv) 0x4080 (op = 'shrv) 0x40C0 (op = 'sarv) 0x4100 (scare 'badshiftv op))
         (t16 (base | ((cn << 3) | dn))))

   ;; div/rem: v6-M has NO hardware divide (and no MLS), so each site expands INLINE to a restoring
   ;; shift-subtract routine -- self-contained, no runtime callout, no external symbol. the IR always
   ;; arrives canonical from gen.l ((div r1 r1 r0), (rem r1 r1 r0 r3), &c) and the op is declared to
   ;; clobber r0..r3 (gen.l's rdsp), so the routine OWNS all four scratch regs. branch offsets are
   ;; FIXED within the blob (like t1-set's branch-over) -- no labels, so many div sites never collide.
   (t1-div-shape d a b)   ; the canonical arrival is all this backend lowers; anything else is loud
      (? (reg-eq d 'r1 && reg-eq a 'r1 && reg-eq b 'r0) 0 (scare 'div-operands `(d a b)))
   ;; UDIVMOD core: N=r1 (dividend), D=r0 (divisor) -> quotient r1, remainder r2 (D kept, r3 spent).
   ;; 32 restoring steps: LSLS shifts N left leaving old bit31 in carry, ADCS folds it into R (=R<<1|c);
   ;; BLO skips the subtract when R<D. verified byte-exact vs llvm-mc (thumbv6m); the two branches are
   ;; blo #2 (over the subtract) and bne #-18 (back to the shift).
   (t1-udivmod u)
      (catall `((t16 0x2200) (t16 0x2320)      ; movs r2,#0 ; movs r3,#32
                (t16 0x0049) (t16 0x4152)      ; L: lsls r1,r1,#1 ; adcs r2,r2,r2
                (t16 0x4282) (t16 0xD301)      ;    cmp r2,r0 ; blo S
                (t16 0x1A12) (t16 0x1C49)      ;    subs r2,r2,r0 ; adds r1,r1,#1
                (t16 0x1E5B) (t16 0xD1F7)))    ; S: subs r3,r3,#1 ; bne L
   ;; SABS: |a|,|b| by the (x^m)-m trick (m = x>>31 arithmetic mask); leaves sa in r2, sb in r3.
   (t1-sabs u)
      (catall `((t16 0x17CA) (t16 0x17C3)      ; asrs r2,r1,#31 (sa) ; asrs r3,r0,#31 (sb)
                (t16 0x4051) (t16 0x1A89)      ; eors r1,r1,r2 ; subs r1,r1,r2  -> |a|
                (t16 0x4058) (t16 0x1AC0)))    ; eors r0,r0,r3 ; subs r0,r0,r3  -> |b|
   ;; unsigned: udiv leaves the quotient in r1 already; urem copies the remainder r2 -> r1.
   (t1-udiv d a b)   (: _ (t1-div-shape d a b) (t1-udivmod ()))
   (t1-urem d a b t) (: _ (t1-div-shape d a b) (catall `((t1-udivmod ()) (t16 0x0011))))  ; movs r1,r2
   ;; signed: abs, run unsigned, re-apply the sign. quotient's sign = sa^sb; remainder's = sa
   ;; (C truncates toward zero, so the remainder follows the dividend). the sign mask rides the
   ;; stack across the loop (which spends r2/r3); (x^m)-m negates when m=-1, is identity when m=0.
   (t1-div d a b)
      (: _ (t1-div-shape d a b)
         (catall `((t1-sabs ()) (t16 0x405A)   ; eors r2,r2,r3 -> qsign = sa^sb
                   (t1-push 'r2) (t1-udivmod ()) (t1-pop 'r3)
                   (t16 0x4059) (t16 0x1AC9))))  ; eors r1,r1,r3 ; subs r1,r1,r3
   (t1-rem d a b t)
      (: _ (t1-div-shape d a b)
         (catall `((t1-sabs ())                ; sa already in r2
                   (t1-push 'r2) (t1-udivmod ()) (t1-pop 'r3)
                   (t16 0x0011)                ; movs r1,r2  -> |rem|
                   (t16 0x4059) (t16 0x1AC9))))  ; eors r1,r1,r3 ; subs r1,r1,r3

   ;; la (part 2): the literal-pool LOAD. v6-M has no MOVW/MOVT to build an address, so gen.l expands
   ;; (la Rd lbl) into (align 4) (lapool Rd) (raw ((fix 4 abs32 lbl 0))) -- the pooled address word sits
   ;; immediately after, 4-aligned by the align, so LDR Rd,[pc,#0] (Align(pc+4,4)=here+4) reaches it;
   ;; then B #2 hops the 4-byte word. Rd must be low (LDR-literal T1 is 3-bit).
   (t1-lapool d) (: _ (need-lo d 'lapool-hi) (catall `((t16 (0x4800 | (rn d << 8))) (t16 0xE001))))

   ;; set: d <- (cond) ? 1 : 0 without IT. the branch must come FIRST: MOVS sets N/Z on v6-M (no
   ;; flag-preserving MOV #imm exists below r8), so any movs before the Bcc destroys the compare.
   ;; Bcond .one ; MOVS d,#0 ; B .end ; .one: MOVS d,#1 ; .end: -- fixed offsets (imm8=1, imm11=0).
   (t1-set cond d)
      (: cc (ccn cond) _ (need-lo d 'set-hi)
         (catall `((t16 (0xD000 | ((cc << 8) | 1))) (t1-movs d 0) (t16 0xE000) (t1-movs d 1))))

   ;; lea. off 0 -> movr (sp-legal). sp base: ADD/SUB sp (imm7*4) when d==sp, else ADD Rd,sp,#imm8*4.
   ;; small non-sp: ADDS #imm3. bigger -> TODO scratch materialize.
   (t1-lea d base off)
      (? (off = 0) (t1-movr d base)
         (reg-eq d 'sp && reg-eq base 'sp)
            (t1-addsub-imm 'add d base off)                ; the sp-arith form (chunked; negative flips)
         (reg-eq base 'sp && off > 0)
            (: _ (? ((0 = (off & 3)) && ((off >> 2) <= 255)) 0 (scare 'lea-sp-off-range off)) _ (need-lo d 'lea-hi)
               (t16 (0xA800 | ((rn d << 8) | (off >> 2)))))
         (off > 0 && (off <= 255)) (t1-addsub-imm 'add d base off)     ; imm3 3-addr, or mov+imm8 2-addr
         (!(reg-eq d base) && lo? d)                       ; far (or negative): build the offset in d, fold the base in
            (catall `((t1-li d off) (t1-add-any d base)))
         ;; a HI dest cannot hold the offset (t1-li is low-only), so borrow a saved low scratch --
         ;; t1-mem-far's own lane, worn by lea. sp base is barred: the push would shift it under us.
         ;; ⚠ t1-li sets flags, as it already did in the low-dest lane above -- so a lea between a
         ;; cmp and its branch would breach, on both lanes. inherited, not introduced here.
         !(reg-eq base 'sp)
            (: s (t1-lowscratch d base '(r0 r1 r2 r3 r4 r5 r6 r7))
               (catall `((t1-push s) (t1-li s off) (t1-add-any s base) (t1-movr d s) (t1-pop s))))
         (scare 'lea-range `(d base off)))

   ;; push/pop are register-LIST 16-bit forms; single-reg lists cover r0..r7 (+ lr on push / pc on pop).
   ;; a hi reg (r8..r12) can't be pushed directly on v6-M -- route through a low scratch (TODO if it arises).
   (t1-push s) (: sn (rn s) (? (sn = 14) (t16 (0xB400 | 256)) (sn <= 7) (t16 (0xB400 | (1 << sn))) (scare 'push-hi s)))
   (t1-pop  d) (: dn (rn d) (? (dn = 15) (t16 (0xBC00 | 256)) (dn <= 7) (t16 (0xBC00 | (1 << dn))) (scare 'pop-hi d)))

   ;; control. the SHORT label branches (B +-2KB, Bcc +-256B) can't cross a big love.c function,
   ;; so every LABEL branch emits WIDE: jmp = BL (+-16MB -- lr is DEAD mid-frame, the prologue
   ;; pushed it and the epilogue pops it straight to pc); br = the inverted Bcc hop (+4, over the
   ;; BL) then BL. AArch32 condition codes invert by cc^1 (the table is adjacent true/false pairs).
   ;; the fixed-offset blob branches (div/set/lapool) stay short. a relax pass that narrows the
   ;; near ones back to 2 bytes is a size optimization for later; the 'b/'bcc patch lanes remain.
   (t1-jmp  label) `((fix 4 'bl  label 0))
   (t1-br cond label) (: cc (ccn cond)
      (cat (t16 (0xD000 | (((cc ^ 1) << 8) | 1)))   ; B<inv> .+4: hop the BL
           `((fix 4 'bl label 0))))
   (t1-call label) `((fix 4 'bl  label 0))
   (t1-jmpr d)  (: dn (rn d) (t16 (0x4700 | (dn << 3))))       ; BX Rm
   (t1-callr d) (: dn (rn d) (t16 (0x4780 | (dn << 3))))       ; BLX Rm

   ;; the dispatch: an EMITTER TABLET (shapes in holo.l's ir-arity), rows partial-applied,
   ;; nullaries their halfwords outright, `raw` the identity. every ALU op sets flags on
   ;; v6-M, so adds/subs ARE add/sub and the S-names alias their plain rows.
   t1-emitters (emsof
        `(('li >< t1-li) ('mov >< t1-movr) ('cmp >< t1-cmp) ('test >< t1-test)
           ('add >< t1-addsub 'add) ('adds >< t1-addsub 'add)
           ('sub >< t1-addsub 'sub) ('subs >< t1-addsub 'sub)
           ('adc >< t1-adc) ('adcs >< t1-adc) ('sbc >< t1-sbc) ('sbcs >< t1-sbc)
           ('and >< t1-alu2 'and) ('or >< t1-alu2 'or) ('ors >< t1-alu2 'or)
           ('xor >< t1-alu2 'xor) ('imul >< t1-alu2 'imul)
           ('ld >< t1-ld) ('ld4 >< t1-ld) ('ldu4 >< t1-ld)
           ('st >< t1-st) ('st4 >< t1-st)
           ('ldu1 >< t1-ldb) ('ldu2 >< t1-ldh) ('ld1 >< t1-lds1) ('ld2 >< t1-lds2)
           ('st1 >< t1-stb) ('st2 >< t1-sth)
           ('sx1 >< t1-sx 1) ('sx2 >< t1-sx 2) ('sx4 >< t1-sx 4)
           ('zx1 >< t1-zx 1) ('zx2 >< t1-zx 2) ('zx4 >< t1-zx 4)
           ('shl >< t1-shift 'shl) ('shr >< t1-shift 'shr) ('sar >< t1-shift 'sar)
           ('ror >< (\ d c (scare 'v6m-no-ror-imm 'ror)))     ; RORS is register-only on v6-M
           ('shlv >< t1-shiftv 'shlv) ('shrv >< t1-shiftv 'shrv) ('sarv >< t1-shiftv 'sarv)
           ('lea >< t1-lea) ('lean >< t1-lea)
           ('leax >< t1-leax) ('ldx >< t1-ldx) ('ldxb >< t1-ldxb)
           ('push >< t1-push) ('pop >< t1-pop)
           ('neg >< t1-neg) ('not >< t1-not) ('inc >< t1-inc) ('dec >< t1-dec)
           ('set >< t1-set)
           ('jmp >< t1-jmp) ('br >< t1-br) ('jmpr >< t1-jmpr) ('callr >< t1-callr) ('call >< t1-call)
           ('div >< t1-div) ('rem >< t1-rem) ('udiv >< t1-udiv) ('urem >< t1-urem)
           ('lapool >< t1-lapool)
           ('raw >< 1)
           ('ret >< t16 0x4770)                               ; BX lr
           ('sys >< t16 0xDF00)                               ; SVC #0
           ('nop >< t16 0xBF00)
           ('trap >< t16 0xBE00)))                            ; BKPT #0
   (t1-lower ins) (: f lower-via (? (lit? f) (f t1-emitters ins) (scare 'holo-bake-stale 'lower-via)))

   ;; a relocation. Thumb pc reads as (site + 4); branch fields hold a /2 (halfword) offset.
   ;;   b   (T2, unconditional, 2 bytes): 0xE000 | imm11               range +-2046
   ;;   bcc (T1, conditional,   2 bytes): 0xD000 | cond<<8 | imm8      range +-254   (cond in aux)
   ;;   bl  (T1, wide,          4 bytes): S:J1:J2:imm10:imm11 (as thumb2.l) -- I1=~(J1^S), I2=~(J2^S)
   ;; the range scares are placeholders: a real driver wants a VENEER pass (load target, BX) for far ones.
   (t1-patch kind target site width aux)
      (: o (target - (site + 4))
         (? (kind = 'b)
            (: _ (? ((o >= -2046) && (o <= 2046)) 0 (scare 'b-range-needs-veneer o))
               (t16 (0xE000 | ((o >> 1) & 2047))))
            (kind = 'bcc)
            (: _ (? ((o >= -254) && (o <= 254)) 0 (scare 'bcc-range-needs-veneer o))
               (t16 (0xD000 | ((aux << 8) | ((o >> 1) & 255)))))
            (kind = 'bl)
            (: imm11 ((o >> 1) & 2047)  imm10 ((o >> 12) & 1023)
               i2 ((o >> 22) & 1)  i1 ((o >> 23) & 1)  s ((o >> 24) & 1)
               j1 (((i1 ^ 1) ^ s) & 1)  j2 (((i2 ^ 1) ^ s) & 1)
               hw1 (0xF000 | ((s << 10) | imm10))
               hw2 (0xD000 | ((j1 << 13) | ((j2 << 11) | imm11)))
               (t32 ((hw1 << 16) | hw2)))
            (kind = 'abs32)                                 ; a pooled 32-bit ABSOLUTE address (la's word)
            (le32 (target + aux))                           ; internal resolve only; a .o leaves it an R_ARM_ABS32
            (scare 'badfix kind)))

   ;; ⚠ no `refix` here, and that is deliberate. `patch` is about what THIS CORE can
   ;; execute; the linker's `refix` is about a REL field's FORMAT, and those are the
   ;; ABI's -- R_ARM_THM_CALL is the wide BL T1 encoding on both cores (t1-patch lays
   ;; it bit-for-bit), and a v6-M image can carry a v7-M object whose MOVW/MOVT pair
   ;; still has to resolve. so there is one arm32 re-encoder, it lives in thumb2.l,
   ;; and crew/holo/link.l asks for it BY FAMILY -- which is what lets a cat that only
   ;; LAYS v6-M code (port/rp2040's boot2) carry this file alone.

   t1 (tablet 0)
   _ (pin t1 'lower t1-lower)
   _ (pin t1 'patch t1-patch)
   _ (pin t1 'nop '(0 191))                    ; the ('align n) filler unit: NOP (16-bit) bf00 LE
   _ (defbackend 'thumb1 t1)
   _ (defbackend 'cortex-m0 t1))
) ; module 'holo
