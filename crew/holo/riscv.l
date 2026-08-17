; riscv -- the RV64 BACKEND for the neutral assembler (crew/holo/holo.l), the fifth target (the
; pine64 ox64's T-Head C906 and qemu -M virt). like the others it pins a {lower,patch} pair -- here
; under 'riscv64 and 'rv64 -- and the driver, IR forms and byte plumbing stay untouched. fixed 32-bit
; little-endian instructions (no compressed forms emitted), native three-address ALU, RV64IMFD only
; (no Zbb/Zba: what qemu-virt, the C906 and every rv64gc core share). every word here was checked
; against `llvm-mc --triple=riscv64` (--show-encoding / --disassemble). load after holo.l.
;
; the novel bit: RISC-V HAS NO FLAGS REGISTER, and the IR is flag-shaped (cmp .. br cond). so the
; assembler itself is the flags register: cmp/test/ucomisd emit nothing (or only an immediate
; materialization) and REMEMBER their operands in `rv-flags`; br/set read the memory and fuse it
; into the native compare-and-branch (beq/blt/bltu ..) or compare-into-register (slt/sltu/feq/flt/
; fle) forms. the contract is exactly x86's: a br/set must be reached by a cmp in the same program
; with its operand registers still live -- a br on a stale memory reads garbage precisely where x86
; would branch on garbage flags. adds/subs compute their overflow bit into x30 the same way
; (no V flag to set), so `br vs` reads a register's sign.
;
; branches: the fused compare-branch reaches only +-4KB, too short for love.c-scale texts, so every
; label branch is WIDE (thumb1's move): the INVERTED fused branch hops over a jal x0 (+-1MB, one
; j21 fixup). call/la/lar are the auipc pair (+-2GB, one 8-byte 'pcrel fixup carrying both base
; words). immediates never scare: past 12 bits they materialize through a scratch, so moon's immok
; seam can treat this target like x64.
;
; scratch discipline (two, both outside the abstract file): x31 (t6) is the PER-INSTRUCTION
; scratch -- address synthesis, immediate materialization, rotate temps -- dead after every lowered
; chunk. x30 (t5) is the FLAG scratch: it holds a materialized cmp immediate, a test result, or an
; adds/subs overflow bit FROM the compare TO its br/set, so it must survive between instructions --
; the one riscv-specific caveat: a far-displacement stx/stxb between a cmp and its br borrows x30
; and breaks the fusion (real codegen keeps cmp and br adjacent; nothing emits that shape).

(module 'holo
(: ;; -- registers: r0..r7 -> a0..a7 (x10..x17, the LP64D argument file), r8..r12 -> t0..t4
   ;;    (x5,x6,x7,x28,x29 caller-saved), r13/r14/r15 -> s2/s3/s1 (x18,x19,x9 callee-saved, the
   ;;    homes), sp -> x2, fp -> s0 (x8), lr -> ra (x1). r19..r26 -> s4..s11 (x20..x27) is the
   ;;    callee-saved bank (arm64's r19..r28 shape) for a future glaze. x3 (gp) and x4 (tp) are
   ;;    platform-reserved and never touched; x30/x31 (t5/t6) are the two scratches above.
   rv-regs (tabof
        `(('sp >< 2) ('fp >< 8) ('lr >< 1)
           ('r0 >< 10) ('r1 >< 11) ('r2 >< 12) ('r3 >< 13) ('r4 >< 14)
           ('r5 >< 15) ('r6 >< 16) ('r7 >< 17)
           ('r8 >< 5) ('r9 >< 6) ('r10 >< 7) ('r11 >< 28) ('r12 >< 29)
           ('r13 >< 18) ('r14 >< 19) ('r15 >< 9)
           ('r19 >< 20) ('r20 >< 21) ('r21 >< 22) ('r22 >< 23)
           ('r23 >< 24) ('r24 >< 25) ('r25 >< 26) ('r26 >< 27)))
   (rvn r) (look rv-regs r 'badreg)
   (rv-reg-eq a b) (rvn a = rvn b)

   ;; -- f0..f7 -> fa0..fa7 (f10..f17, the D argument file), f8..f15 -> ft0..ft7 (f0..f7 pure
   ;;    temps), f16..f19 -> ft8..ft11 (f28..f31, the narrow scratch bank). --
   rv-fregs (tabof
        `(('f0 >< 10) ('f1 >< 11) ('f2 >< 12) ('f3 >< 13) ('f4 >< 14)
           ('f5 >< 15) ('f6 >< 16) ('f7 >< 17)
           ('f8 >< 0) ('f9 >< 1) ('f10 >< 2) ('f11 >< 3) ('f12 >< 4)
           ('f13 >< 5) ('f14 >< 6) ('f15 >< 7)
           ('f16 >< 28) ('f17 >< 29) ('f18 >< 30) ('f19 >< 31)))
   (rvf r) (look rv-fregs r 'badfreg)

   ;; -- the six instruction formats, each a word builder. immediates arrive SIGNED; & masks
   ;;    two's-complement bits into the field (exact on negative charms). --
   (rv-r f7 rs2 rs1 f3 rd op) (le32 ((f7 << 25) | ((rs2 << 20) | ((rs1 << 15) | ((f3 << 12) | ((rd << 7) | op))))))
   (rv-i imm rs1 f3 rd op)    (le32 (((imm & 4095) << 20) | ((rs1 << 15) | ((f3 << 12) | ((rd << 7) | op)))))
   (rv-s imm rs1 rs2 f3)      (: u (imm & 4095)
                                 (le32 (((u >> 5) << 25) | ((rs2 << 20) | ((rs1 << 15) | ((f3 << 12) | (((u & 31) << 7) | 0x23)))))))
   (rv-b f3 rs1 rs2 rel)      (: u (rel & 8191)          ; 13-bit two's complement, bit 0 always 0
                                 (le32 ((((u >> 12) & 1) << 31) | ((((u >> 5) & 63) << 25) | ((rs2 << 20) | ((rs1 << 15) | ((f3 << 12) | ((((u >> 1) & 15) << 8) | ((((u >> 11) & 1) << 7) | 0x63)))))))))
   (rv-u imm20 rd op)         (le32 (((imm20 & 1048575) << 12) | ((rd << 7) | op)))
   (rv-jword rel)             (: u (rel & 2097151)       ; 21-bit two's complement jal offset field
                                 ((((u >> 20) & 1) << 31) | ((((u >> 1) & 1023) << 21) | ((((u >> 11) & 1) << 20) | (((u >> 12) & 255) << 12)))))

   (rv-fits12 n) ((n >= -2048) && (n <= 2047))
   ;; split n into (lo12 . rest*4096): lo is the sign-adjusted low 12 bits, rest exact.
   (rv-lo12 n) (: low (n & 4095) (? (low > 2047) (low - 4096) low))

   ;; (li d imm): addi / lui+addiw (the 32-bit wraparound pair) / recursive shift-and-add chunks.
   ;; input normalizes into signed [-2^63, 2^63) -- a bignum hash constant >= 2^63 arrives unsigned.
   (rv-li-rec dn s)
      (? (rv-fits12 s) (rv-i s 0 0 dn 0x13)                                 ; addi d, x0, s
         (fits32 s)
         (: lo (rv-lo12 s) hi ((s - lo) // 4096)                            ; addiw wraps mod 2^32, so
            (catall `(                                                      ; hi=2^19 lands s=2^31-ish exactly
               (rv-u hi dn 0x37)                                            ; lui d, hi
               (? (lo = 0) () (rv-i lo dn 0 dn 0x1B)))))                    ; addiw d, d, lo
         (: lo (rv-lo12 s)
            (catall `(
               (rv-li-rec dn ((s - lo) // 4096))                            ; the high chunks
               (rv-i 12 dn 1 dn 0x13)                                       ; slli d, d, 12
               (? (lo = 0) () (rv-i lo dn 0 dn 0x13))))))                   ; addi d, d, lo
   (rv-li d imm) (rv-li-rec (rvn d) (? (imm > 9223372036854775807) (imm - 18446744073709551616) imm))

   (rv-mov d s) (rv-i 0 (rvn s) 0 (rvn d) 0x13)                             ; addi d, s, 0

   ;; -- ALU: op -> (funct3 . funct7). three-address native; an immediate rides addi/andi/ori/xori
   ;;    when it fits 12 bits, else materializes in x31 -- no immediate ever scares here. --
   rv-alu-rows
        `(('add >< '(0 0)) ('sub >< '(0 32)) ('and >< '(7 0)) ('or >< '(6 0)) ('xor >< '(4 0))
           ('imul >< '(0 1)))
   rv-alu (tabof rv-alu-rows)
   (rv-alu-rrr f3 f7 dn an bn) (rv-r f7 bn an f3 dn 0x33)
   (rv-aluop op d a b)
      (: meta (peep rv-alu op 0) f3 (at meta 0) f7 (at meta 1)
         dn (rvn d) an (rvn a)
         (? (reg? b) (rv-alu-rrr f3 f7 dn an (rvn b))
            ;; immediate: sub flips to addi -imm; add/and/or/xor ride the I-form; else via x31.
            ((op = 'sub) && rv-fits12 (0 - b)) (rv-i (0 - b) an 0 dn 0x13)
            (rv-fits12 b && member? op '(add and or xor))
             (rv-i b an (? (op = 'add) 0 f3) dn 0x13)
            (catall `((rv-li-rec 31 b) (rv-alu-rrr f3 f7 dn an 31)))))

   ;; -- the flag memory: what the last cmp/test/ucomisd/adds left behind. 'a/'b are hardware
   ;;    numbers (GPR, or FPR when 'f), 'v the overflow-bit register (-1 = no adds/subs yet). --
   rv-flags (tablet 0)
   (rv-flag-set f a b v)
      (: _ (pin rv-flags 'f f) _ (pin rv-flags 'a a) _ (pin rv-flags 'b b) _ (pin rv-flags 'v v) ())

   (rv-cmp a b)
      (: an (rvn a)
         (? (reg? b) (rv-flag-set 0 an (rvn b) -1)
            (b = 0)  (rv-flag-set 0 an 0 -1)                                 ; compare against x0, free
            (: bs (rv-li-rec 30 b) _ (rv-flag-set 0 an 30 -1) bs)))          ; imm -> x30, the flag scratch
   (rv-test a b)                                                             ; x30 = a AND b; compare vs x0
      (: an (rvn a)
         bs (? (reg? b) (rv-alu-rrr 7 0 30 an (rvn b))
               (rv-fits12 b) (rv-i b an 7 30 0x13)                           ; andi x30, a, b
               (catall `((rv-li-rec 31 b) (rv-alu-rrr 7 0 30 an 31))))
         _ (rv-flag-set 0 30 0 -1)
         bs)

   ;; adds/subs: do the op AND leave the overflow sign bit in x30 (v = (d^a)&(d^b) for add,
   ;; (a^b)&(a^d) for sub -- both need the operands' OLD values, saved first so d may alias).
   ;; the flag memory then reads d-vs-x0 for eq/ne/lt.., x30's sign for vs/vc.
   (rv-addsub-s op d a b)
      (: dn (rvn d) an (rvn a)
         pre (? (reg? b) (rv-i 0 (rvn b) 0 30 0x13) (rv-li-rec 30 b))        ; x30 = b's old value
         f7  (? (op = 'adds) 0 32)
         _   (rv-flag-set 0 dn 0 30)
         (catall `(
            pre
            (rv-i 0 an 0 31 0x13)                                            ; x31 = a's old value
            (rv-alu-rrr 0 f7 dn 31 30)                                       ; d = a op b (off the saves)
            (? (op = 'adds)
               (catall `((rv-alu-rrr 4 0 31 dn 31)                           ; x31 = d ^ a
                          (rv-alu-rrr 4 0 30 dn 30)))                        ; x30 = d ^ b
               (catall `((rv-alu-rrr 4 0 30 31 30)                           ; x30 = a ^ b
                          (rv-alu-rrr 4 0 31 31 dn))))                       ; x31 = a ^ d
            (rv-alu-rrr 7 0 30 30 31))))                                     ; x30 = the overflow sign

   ;; the fused compare-branch: cond -> (funct3 rs1 rs2), gt/le/above/be by swapping. inversion is
   ;; f3 ^ 1 (beq/bne, blt/bge, bltu/bgeu pair on the low bit), operands unmoved.
   (rv-int-b cond an bn)
      (? (cond = 'eq) `(0 an bn) (cond = 'ne) `(1 an bn)
         (cond = 'lt) `(4 an bn) (cond = 'ge) `(5 an bn)
         (cond = 'gt) `(4 bn an) (cond = 'le) `(5 bn an)
         (cond = 'below) `(6 an bn) (cond = 'ae) `(7 an bn)
         (cond = 'above) `(6 bn an) (cond = 'be) `(7 bn an)
         (scare 'rv-badcc cond))
   ;; fp conds compute a 0/1 predicate into x31 (flt/fle/feq write GPRs, quiet on NaN), plus the
   ;; branch SENSE: 1 = taken when x31 is set, 0 = taken when clear (below = NOT ae, etc; p/np
   ;; check both operands ordered). x86's ucomisd conds map: above/ae/below/be/eq/ne/p/np -- and
   ;; the SIGNED four read as A64's FCMP condition codes: gt/ge are unordered-FALSE, lt/le their
   ;; exact complements (unordered-TRUE, so a branch on the NEGATED relation is NaN-honest --
   ;; the arm-family convention moon's fcmp lanes lean on). so lt == below and le == be here.
   (rv-fp-pred cond fa fb)
      (? (cond = 'above) `((rv-r 0x51 fa fb 1 31 0x53) 1)                    ; flt.d x31, fb, fa
         (cond = 'gt)    `((rv-r 0x51 fa fb 1 31 0x53) 1)
         (cond = 'be)    `((rv-r 0x51 fa fb 1 31 0x53) 0)
         (cond = 'le)    `((rv-r 0x51 fa fb 1 31 0x53) 0)                    ; le = !gt (fires on NaN)
         (cond = 'ae)    `((rv-r 0x51 fa fb 0 31 0x53) 1)                    ; fle.d x31, fb, fa
         (cond = 'ge)    `((rv-r 0x51 fa fb 0 31 0x53) 1)
         (cond = 'below) `((rv-r 0x51 fa fb 0 31 0x53) 0)
         (cond = 'lt)    `((rv-r 0x51 fa fb 0 31 0x53) 0)                    ; lt = !ge (fires on NaN)
         (cond = 'eq)    `((rv-r 0x51 fb fa 2 31 0x53) 1)                    ; feq.d x31, fa, fb (ordered)
         (cond = 'ne)    `((rv-r 0x51 fb fa 2 31 0x53) 0)
         (cond = 'p)     `((catall `((rv-r 0x51 fa fa 2 31 0x53)             ; x31 = (fa ordered)
                                      (rv-r 0x51 fb fb 2 30 0x53)            ; x30 = (fb ordered)
                                      (rv-alu-rrr 7 0 31 31 30))) 0)         ; and -> 0 iff unordered
         (cond = 'np)    `((catall `((rv-r 0x51 fa fa 2 31 0x53)
                                      (rv-r 0x51 fb fb 2 30 0x53)
                                      (rv-alu-rrr 7 0 31 31 30))) 1)
         (scare 'rv-fp-badcc cond))
   (rv-flag-read tag)
      (: a (peep rv-flags 'a -1)
         _ (? (a < 0) (scare 'rv-no-cmp tag) 0)
         `(a (peep rv-flags 'b -1) (peep rv-flags 'f 0) (peep rv-flags 'v -1)))

   ;; (br cond label): the INVERTED fused branch hops the wide jal (thumb1's shape). s/ns re-derive
   ;; the sign of a-b through x31; vs/vc read the adds/subs overflow register.
   (rv-br cond label)
      (: st (rv-flag-read 'br) an (at st 0) bn (at st 1)
         jal `((fix 4 'j21 label 0x6F))                                      ; jal x0, label
         (? (at st 2)                                                        ; fp memory
            (: p (rv-fp-pred cond an bn)
               (catall `((at p 0)
                          (rv-b (? (at p 1) 0 1) 31 0 8)                     ; hop: beqz/bnez x31, +8
                          jal)))
            (member? cond '(s ns))
            (catall `((rv-alu-rrr 0 32 31 an bn)                             ; x31 = a - b (the SF bit)
                       (rv-b (? (cond = 's) 5 4) 31 0 8)                     ; hop: bgez/bltz x31, +8
                       jal))
            (member? cond '(vs vc))
            (: v (at st 3) _ (? (v < 0) (scare 'rv-no-overflow cond) 0)
               (catall `((rv-b (? (cond = 'vs) 5 4) v 0 8) jal)))
            (: t (rv-int-b cond an bn)
               (catall `((rv-b (at t 0 ^ 1) (at t 1) (at t 2) 8) jal)))))

   ;; (set cond d): the compare-into-register forms. eq/ne against x0 collapse to seqz/snez.
   (rv-set cond d)
      (: st (rv-flag-read 'set) an (at st 0) bn (at st 1) dn (rvn d)
         (? (at st 2)
            (: p (rv-fp-pred cond an bn)
               (catall `((at p 0)
                          (? (at p 1) (rv-i 0 31 0 dn 0x13)                  ; d = x31
                             (rv-i 1 31 3 dn 0x13)))))                       ; d = (x31 < 1)  (seqz)
            (member? cond '(eq ne))
            (: xs (? (bn = 0) () (rv-alu-rrr 4 0 31 an bn))                  ; x31 = a ^ b
               t  (? (bn = 0) an 31)
               (catall `(xs (? (cond = 'eq) (rv-i 1 t 3 dn 0x13)             ; sltiu d, t, 1
                               (rv-alu-rrr 3 0 dn 0 t)))))                   ; sltu d, x0, t
            (member? cond '(s ns))
            (catall `((rv-alu-rrr 0 32 31 an bn)
                       (rv-alu-rrr 2 0 dn 31 0)                              ; slt d, x31, x0
                       (? (cond = 's) () (rv-i 1 dn 4 dn 0x13))))            ; xori d, d, 1
            (member? cond '(vs vc))
            (: v (at st 3) _ (? (v < 0) (scare 'rv-no-overflow cond) 0)
               (catall `((rv-alu-rrr 2 0 dn v 0)
                          (? (cond = 'vs) () (rv-i 1 dn 4 dn 0x13)))))
            (: neg (member? cond '(ge le ae be))                             ; the complement conds
               t (rv-int-b (? (cond = 'ge) 'lt (cond = 'le) 'gt
                              (cond = 'ae) 'below (cond = 'be) 'above cond) an bn)
               (catall `((rv-alu-rrr (? (at t 0 = 4) 2 3) 0 dn (at t 1) (at t 2))     ; slt/sltu
                          (? neg (rv-i 1 dn 4 dn 0x13) ()))))))              ; xori d, d, 1

   ;; -- memory: one core, sd/ld and every sized/fp face a spelling of it. a 12-bit offset rides
   ;;    the instruction; a far one builds base+hi in x31 and rides the low 12. --
   (rv-mem-far bn off go)                                                    ; go: (basereg lo12) -> bytes
      (: lo (rv-lo12 off) hi ((off - lo) // 4096)
         _ (? ((hi >= -524288) && (hi <= 524287)) 0 (scare 'rv-mem-range off))
         (catall `((rv-u hi 31 0x37)                                         ; lui x31, hi
                    (rv-alu-rrr 0 0 31 31 bn)                                ; add x31, x31, base
                    (go 31 lo))))
   (rv-ld-f3 f3 opc dt bn off)
      (? (rv-fits12 off) (rv-i off bn f3 dt opc)
         (rv-mem-far bn off (\ b lo (rv-i lo b f3 dt opc))))
   (rv-st-f3 f3 bn off st)
      (? (rv-fits12 off) (rv-s off bn st f3)
         (rv-mem-far bn off (\ b lo (rv-s lo b st f3))))
   (rv-ld d base off)   (rv-ld-f3 3 0x03 (rvn d) (rvn base) off)             ; ld
   (rv-st base off s)   (rv-st-f3 3 (rvn base) off (rvn s))                  ; sd
   (rv-ld1 d base off)  (rv-ld-f3 0 0x03 (rvn d) (rvn base) off)             ; lb (sign-extends)
   (rv-ld2 d base off)  (rv-ld-f3 1 0x03 (rvn d) (rvn base) off)             ; lh
   (rv-ld4 d base off)  (rv-ld-f3 2 0x03 (rvn d) (rvn base) off)             ; lw
   (rv-ldu1 d base off) (rv-ld-f3 4 0x03 (rvn d) (rvn base) off)             ; lbu (zero-extends)
   (rv-ldu2 d base off) (rv-ld-f3 5 0x03 (rvn d) (rvn base) off)             ; lhu
   (rv-ldu4 d base off) (rv-ld-f3 6 0x03 (rvn d) (rvn base) off)             ; lwu
   (rv-st1 base off s)  (rv-st-f3 0 (rvn base) off (rvn s))                  ; sb
   (rv-st2 base off s)  (rv-st-f3 1 (rvn base) off (rvn s))                  ; sh
   (rv-st4 base off s)  (rv-st-f3 2 (rvn base) off (rvn s))                  ; sw
   (rv-fld d base off)  (rv-ld-f3 3 0x07 (rvf d) (rvn base) off)             ; fld
   (rv-fst base off s)  (: bn (rvn base) sn (rvf s)
                           (? (rv-fits12 off) (: u (off & 4095) (le32 (((u >> 5) << 25) | ((sn << 20) | ((bn << 15) | ((3 << 12) | (((u & 31) << 7) | 0x27)))))))
                              (rv-mem-far bn off (\ b lo (: u (lo & 4095) (le32 (((u >> 5) << 25) | ((sn << 20) | ((b << 15) | ((3 << 12) | (((u & 31) << 7) | 0x27)))))))))))
   (rv-flds d base off) (rv-ld-f3 2 0x07 (rvf d) (rvn base) off)             ; flw
   (rv-fsts base off s) (: bn (rvn base) sn (rvf s)
                           (? (rv-fits12 off) (: u (off & 4095) (le32 (((u >> 5) << 25) | ((sn << 20) | ((bn << 15) | ((2 << 12) | (((u & 31) << 7) | 0x27)))))))
                              (rv-mem-far bn off (\ b lo (: u (lo & 4095) (le32 (((u >> 5) << 25) | ((sn << 20) | ((b << 15) | ((2 << 12) | (((u & 31) << 7) | 0x27)))))))))))

   ;; in-register extends (the C casts). sx4 is one addiw; the byte/half pairs shift up and back.
   (rv-sx w d) (: dn (rvn d)
                  (? (w = 4) (rv-i 0 dn 0 dn 0x1B)                           ; addiw d, d, 0 (sext.w)
                     (: c (? (w = 1) 56 48)
                        (catall `((rv-i c dn 1 dn 0x13)                      ; slli d, d, c
                                   (rv-i (1024 + c) dn 5 dn 0x13))))))       ; srai d, d, c
   (rv-zx w d) (: dn (rvn d)
                  (? (w = 1) (rv-i 255 dn 7 dn 0x13)                         ; andi d, d, 255
                     (: c (? (w = 2) 48 32)
                        (catall `((rv-i c dn 1 dn 0x13)                      ; slli d, d, c
                                   (rv-i c dn 5 dn 0x13))))))                ; srli d, d, c

   ;; shifts by immediate; rol/ror synthesize through x31 (no rotate in the base ISA).
   (rv-shift op d c)
      (: dn (rvn d)
         (? (op = 'shl) (rv-i c dn 1 dn 0x13)                                ; slli
            (op = 'shr) (rv-i c dn 5 dn 0x13)                                ; srli
            (op = 'sar) (rv-i (1024 + c) dn 5 dn 0x13)                       ; srai
            (member? op '(rol ror))
            (: k (? (op = 'ror) c ((64 - c) & 63))                           ; rol c = ror 64-c
               (catall `((rv-i k dn 5 31 0x13)                               ; srli x31, d, k
                          (rv-i ((64 - k) & 63) dn 1 dn 0x13)                ; slli d, d, 64-k
                          (rv-alu-rrr 6 0 dn dn 31))))                       ; or d, d, x31
            (scare 'rv-badshift op)))
   (rv-shiftv op d c)                                                        ; count pinned to r1 (the IR contract)
      (: _ (? (rv-reg-eq c 'r1) 0 (scare 'shiftv-count-not-r1 `(op d c)))
         _ (? (rv-reg-eq d 'r1) (scare 'shiftv-dst-is-r1 `(op d c)) 0)
         dn (rvn d) cn (rvn c)
         (? (op = 'shlv) (rv-alu-rrr 1 0 dn dn cn)                           ; sll
            (op = 'shrv) (rv-alu-rrr 5 0 dn dn cn)                           ; srl
            (rv-alu-rrr 5 32 dn dn cn)))                                     ; sra

   (rv-lea d base off)
      (: dn (rvn d) bn (rvn base)
         (? (rv-fits12 off) (rv-i off bn 0 dn 0x13)                          ; addi d, base, off
            (catall `((rv-li-rec 31 off) (rv-alu-rrr 0 0 dn bn 31)))))

   ;; indexed addressing: no [base + ix*scale] form, so synthesize -- x31 carries the address,
   ;; ldx borrows its own dest for a far displacement's high half, stx borrows x30 (the caveat
   ;; in the header). scale 1 skips the shift.
   (rv-shamt scale) (? (scale = 1) 0 (scale = 2) 1 (scale = 4) 2 (scale = 8) 3 (scare 'badscale scale))
   (rv-ixaddr bn ixn sc)                                                     ; x31 = base + ix*scale
      (: amt (rv-shamt sc)
         (? (amt = 0) (rv-alu-rrr 0 0 31 ixn bn)
            (catall `((rv-i amt ixn 1 31 0x13)                               ; slli x31, ix, amt
                       (rv-alu-rrr 0 0 31 31 bn)))))                         ; add x31, x31, base
   (rv-ix-far dp tn go)                                                      ; tn carries hi; go lo -> bytes
      (? (rv-fits12 dp) (go dp)
         (: lo (rv-lo12 dp) hi ((dp - lo) // 4096)
            (catall `((rv-u hi tn 0x37)                                      ; lui t, hi
                       (rv-alu-rrr 0 0 31 31 tn)                             ; add x31, x31, t
                       (go lo)))))
   (rv-ldx-f3 f3 d base index scale disp)
      (: dn (rvn d)
         (catall `((rv-ixaddr (rvn base) (rvn index) scale)
                    (rv-ix-far disp dn (\ lo (rv-i lo 31 f3 dn 0x03))))))
   (rv-ldx d base index scale disp)  (rv-ldx-f3 3 d base index scale disp)   ; ld
   (rv-ldxb d base index scale disp) (rv-ldx-f3 4 d base index scale disp)   ; lbu
   (rv-stx-f3 f3 base index scale disp s)
      (catall `((rv-ixaddr (rvn base) (rvn index) scale)
                 (rv-ix-far disp 30 (\ lo (rv-s lo 31 (rvn s) f3)))))
   (rv-stx base index scale disp s)  (rv-stx-f3 3 base index scale disp s)   ; sd
   (rv-stxb base index scale disp s) (rv-stx-f3 0 base index scale disp s)   ; sb
   (rv-leax d base index scale disp)
      (: dn (rvn d) amt (rv-shamt scale)
         (catall `((? (amt = 0) (rv-alu-rrr 0 0 dn (rvn index) (rvn base))
                      (catall `((rv-i amt (rvn index) 1 31 0x13)
                                 (rv-alu-rrr 0 0 dn 31 (rvn base)))))
                    (? (disp = 0) ()
                       (rv-fits12 disp) (rv-i disp dn 0 dn 0x13)
                       (catall `((rv-li-rec 31 disp) (rv-alu-rrr 0 0 dn dn 31)))))))

   (rv-push s) (catall `((rv-i -16 2 0 2 0x13) (rv-s 0 2 (rvn s) 3)))        ; addi sp,sp,-16 ; sd s,0(sp)
   (rv-pop d)  (catall `((rv-i 0 2 3 (rvn d) 0x03) (rv-i 16 2 0 2 0x13)))    ; ld d,0(sp) ; addi sp,sp,16

   (rv-neg d) (rv-alu-rrr 0 32 (rvn d) 0 (rvn d))                            ; sub d, x0, d
   (rv-not d) (rv-i -1 (rvn d) 4 (rvn d) 0x13)                               ; xori d, d, -1
   (rv-inc d) (rv-i 1 (rvn d) 0 (rvn d) 0x13)
   (rv-dec d) (rv-i -1 (rvn d) 0 (rvn d) 0x13)

   ;; M: div/rem round toward zero like x86, but never trap -- /0 answers -1 (quot) or the
   ;; dividend (rem), and the caller already guards zero. t is arm64's scratch shape, unused here.
   (rv-imul d a b) (rv-aluop 'imul d a b)
   (rv-div d a b)     (rv-alu-rrr 4 1 (rvn d) (rvn a) (rvn b))               ; div
   (rv-rem d a b t)   (rv-alu-rrr 6 1 (rvn d) (rvn a) (rvn b))               ; rem
   (rv-udiv d a b)    (rv-alu-rrr 5 1 (rvn d) (rvn a) (rvn b))               ; divu
   (rv-urem d a b t)  (rv-alu-rrr 7 1 (rvn d) (rvn a) (rvn b))               ; remu

   ;; mulo: mulh holds the high 64; the product fits signed 64 iff that equals the low half's
   ;; sign fill. t is clobbered and MUST differ from a, b, d (mulh runs first). flag-free, so
   ;; the hop-over-jal shape directly.
   (rv-mulo d a b t lbl)
      (: dn (rvn d) an (rvn a) bn (rvn b) tn (rvn t)
         (catall `((rv-alu-rrr 1 1 tn an bn)                                 ; mulh t, a, b
                    (rv-alu-rrr 0 1 dn an bn)                                ; mul  d, a, b
                    (rv-i 1087 dn 5 31 0x13)                                 ; srai x31, d, 63
                    (rv-b 0 tn 31 8)                                         ; beq t, x31, +8 (hop)
                    `((fix 4 'j21 lbl 0x6F)))))                              ; jal x0, lbl

   ;; control. jmp is the wide jal (+-1MB, scares beyond); call/la/lar are auipc pairs (+-2GB,
   ;; one 8-byte fixup whose aux packs both base words -- 'pcrel patches hi20 into the first,
   ;; lo12 into the second). la and lar coincide: auipc is byte pc-relative by nature, so the
   ;; pair is translation-invariant (x86's rip-rel lea already is; arm64 needed ADR for that).
   (rv-jmp label)  `((fix 4 'j21 label 0x6F))
   (rv-jmpr d)     (rv-i 0 (rvn d) 0 0 0x67)                                 ; jalr x0, 0(d)
   (rv-call label) `((fix 8 'pcrel label ((((1 << 15) | ((1 << 7) | 0x67)) * 4294967296) + (0x17 | (1 << 7)))))
   (rv-callr d)    (rv-i 0 (rvn d) 0 1 0x67)                                 ; jalr ra, 0(d)
   (rv-la d label) (: dn (rvn d)
                      `((fix 8 'pcrel label ((((dn << 15) | ((dn << 7) | 0x13)) * 4294967296) + (0x17 | (dn << 7))))))

   ;; -- scalar-double FP (D). SSE-named IR two-address; d repeats as the first source. rm rides
   ;;    the funct3 slot: 7 = dynamic for arithmetic, 1 = toward-zero for the C-cast cvttsd2si. --
   rv-fp3 (tabof `(('addsd >< 1) ('subsd >< 5) ('mulsd >< 9) ('divsd >< 13)))
   (rv-fp3op op d s) (: dn (rvf d) (rv-r (peep rv-fp3 op 0) (rvf s) dn 7 dn 0x53))
   (rv-fmov d s)   (: sn (rvf s) (rv-r 0x11 sn sn 0 (rvf d) 0x53))           ; fsgnj.d d, s, s
   (rv-fcmp a b)   (rv-flag-set 1 (rvf a) (rvf b) -1)                        ; remember, emit nothing
   (rv-scvtf d g)  (rv-r 0x69 2 (rvn g) 7 (rvf d) 0x53)                      ; fcvt.d.l d, g
   (rv-fcvtzs g d) (rv-r 0x61 2 (rvf d) 1 (rvn g) 0x53)                      ; fcvt.l.d g, d, rtz
   (rv-fmovxr d g) (rv-r 0x79 0 (rvn g) 0 (rvf d) 0x53)                      ; fmv.d.x
   (rv-fmovrx g d) (rv-r 0x71 0 (rvf d) 0 (rvn g) 0x53)                      ; fmv.x.d
   (rv-fwiden d s)  (rv-r 0x21 0 (rvf s) 0 (rvf d) 0x53)                     ; fcvt.d.s
   (rv-fnarrow d s) (rv-r 0x20 1 (rvf s) 7 (rvf d) 0x53)                     ; fcvt.s.d (rounds)
   (rv-xorpd d s)                                                            ; zero a double (d==s only)
      (? (rvf d = rvf s) (rv-r 0x79 0 0 0 (rvf d) 0x53)                      ; fmv.d.x d, x0
         (scare 'rv-xorpd-general `(d s)))

   (rv-tp d) (rv-i 0 4 0 (rvn d) 0x13)                                       ; addi d, tp, 0

   ;; the dispatch: an EMITTER TABLET (shapes in holo.l's ir-arity), rows partial-applied.
   rv-emitters (emsof
        `(('li >< rv-li) ('mov >< rv-mov) ('cmp >< rv-cmp) ('test >< rv-test)
           ('imul >< rv-imul)
           ('ld >< rv-ld) ('st >< rv-st)
           ('ld1 >< rv-ld1) ('ld2 >< rv-ld2) ('ld4 >< rv-ld4)
           ('st1 >< rv-st1) ('st2 >< rv-st2) ('st4 >< rv-st4)
           ('ldu1 >< rv-ldu1) ('ldu2 >< rv-ldu2) ('ldu4 >< rv-ldu4)
           ('sx1 >< rv-sx 1) ('sx2 >< rv-sx 2) ('sx4 >< rv-sx 4)
           ('zx1 >< rv-zx 1) ('zx2 >< rv-zx 2) ('zx4 >< rv-zx 4)
           ('shlv >< rv-shiftv 'shlv) ('shrv >< rv-shiftv 'shrv) ('sarv >< rv-shiftv 'sarv)
           ('shl >< rv-shift 'shl) ('shr >< rv-shift 'shr) ('sar >< rv-shift 'sar)
           ('rol >< rv-shift 'rol) ('ror >< rv-shift 'ror)
           ('lea >< rv-lea) ('lean >< rv-lea)
           ('push >< rv-push) ('pop >< rv-pop)
           ('neg >< rv-neg) ('not >< rv-not) ('inc >< rv-inc) ('dec >< rv-dec)
           ('jmp >< rv-jmp) ('br >< rv-br) ('jmpr >< rv-jmpr) ('callr >< rv-callr)
           ('mulo >< rv-mulo) ('set >< rv-set)
           ('adds >< rv-addsub-s 'adds) ('subs >< rv-addsub-s 'subs)
           ('div >< rv-div) ('rem >< rv-rem) ('udiv >< rv-udiv) ('urem >< rv-urem)
           ('ldx >< rv-ldx) ('stx >< rv-stx) ('leax >< rv-leax)
           ('ldxb >< rv-ldxb) ('stxb >< rv-stxb)
           ('call >< rv-call) ('la >< rv-la) ('lar >< rv-la)
           ('tp >< rv-tp)
           ('movsd >< rv-fmov)
           ('addsd >< rv-fp3op 'addsd) ('subsd >< rv-fp3op 'subsd)
           ('mulsd >< rv-fp3op 'mulsd) ('divsd >< rv-fp3op 'divsd)
           ('ucomisd >< rv-fcmp) ('cvtsi2sd >< rv-scvtf) ('cvttsd2si >< rv-fcvtzs)
           ('movqxr >< rv-fmovxr) ('movqrx >< rv-fmovrx)
           ('stsd >< rv-fst) ('ldsd >< rv-fld) ('stss >< rv-fsts) ('ldss >< rv-flds)
           ('cvtss2sd >< rv-fwiden) ('cvtsd2ss >< rv-fnarrow)
           ('xorpd >< rv-xorpd)
           ('raw >< 1)
           ('ret >< rv-i 0 1 0 0 0x67)                                       ; jalr x0, 0(ra)
           ('sys >< le32 0x73)                                               ; ecall
           ('nop >< le32 0x13)                                               ; addi x0, x0, 0
           ('trap >< le32 0x100073)))                                        ; ebreak
   _ (foldl (\ _ p (emrow rv-emitters <p (rv-aluop <p))) 0 rv-alu-rows)
   (rv-lower ins) (: f lower-via (? (lit? f) (f rv-emitters ins) (scare 'holo-bake-stale 'lower-via)))

   ;; relocations: j21 is the jal offset bitfield (+-1MB, scares past); pcrel splits target-site
   ;; across the auipc pair in aux (lo word first in memory), hi rounded so lo stays signed 12-bit;
   ;; abs64 is the 8-byte data pointer.
   (rv-patch kind target site width aux)
      (: rb (target - site)
         (? (kind = 'j21)
            (: _ (? ((rb >= -1048576) && (rb <= 1048575)) 0 (scare 'rv-jal-range rb))
               (le32 (aux | rv-jword rb)))
            (kind = 'pcrel)
            (: lo (rv-lo12 rb) hi ((rb - lo) // 4096)
               _ (? ((hi >= -524288) && (hi <= 524287)) 0 (scare 'rv-pcrel-range rb))
               (le32 ((aux % 4294967296) | ((hi & 1048575) << 12))
                + le32 ((aux // 4294967296) | ((lo & 4095) << 20))))
            (kind = 'abs64) (le64 (target + aux))
            (scare 'badfix kind)))

   rv (tablet 0)
   _ (pin rv 'lower rv-lower)
   _ (pin rv 'patch rv-patch)
   _ (pin rv 'nop '(19 0 0 0))                 ; the ('align n) filler unit: NOP 00000013 LE
   _ (defbackend 'riscv64 rv)
   _ (defbackend 'rv64 rv))
) ; module 'holo
