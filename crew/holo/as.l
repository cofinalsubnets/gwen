; as -- a REAL assembler front over holo: it reads standard GNU AT&T `.s` and emits the SAME
; byte/fixup item stream the neutral backend (crew/holo/x64.l) produces, so crew/holo/obj.l lays the
; relocatable `.o` and crew/holo/link.l links it -- the front tooth of the gcc-free chain, paired
; with link.l's `ld`. where crew/holo/text.l parses holo's OWN mnemonics over abstract registers,
; THIS parses real x86-64: physical registers (%rax/%eax/..), the AT&T operand grammar, the directive
; set. the encoders mirror x64.l form-for-form but read the register number off the name and carry the
; operand WIDTH (x64.l is 64-bit-mode only). M1 scope: 64/32-bit GPR core -- reg/imm ALU, mov, unary,
; push/pop, lea rip, call/jmp/jcc (NEAR; branch relaxation is M2), ret/nop/syscall/leave/cq[dt]o.
;
; load AFTER holo.l + x64.l (bare, dev/test context -- the encoder primitives le32/byte/fix/modrm..
; are still on the global book; the eventual host path reaches them through the `holo` book):
;   out/host/love -l crew/holo/holo.l -l crew/holo/x64.l -l crew/holo/as.l ...

(: ;; --- the physical register file: name -> (enc width-in-bytes). the low 8 have irregular
   ;; byte/word names, so spell the rows; r8..r15 are uniform (rN / rNd / rNw / rNb). ---
   as-regs (tablet 0)
   (as-reg! nm e w) (pin as-regs (intern nm) `(e w))
   _ (foldl (\ _ row
               (: e (at row 0)
                  _ (as-reg! (at row 1) e 8) _ (as-reg! (at row 2) e 4)
                  _ (as-reg! (at row 3) e 2) _ (as-reg! (at row 4) e 1) 0))
        0 '((0 "rax" "eax" "ax" "al")  (1 "rcx" "ecx" "cx" "cl")
             (2 "rdx" "edx" "dx" "dl")  (3 "rbx" "ebx" "bx" "bl")
             (4 "rsp" "esp" "sp" "spl") (5 "rbp" "ebp" "bp" "bpl")
             (6 "rsi" "esi" "si" "sil") (7 "rdi" "edi" "di" "dil")))
   _ (foldl (\ _ i
               (: n ("r" + show i)
                  _ (as-reg! n i 8) _ (as-reg! (n + "d") i 4)
                  _ (as-reg! (n + "w") i 2) _ (as-reg! (n + "b") i 1) 0))
        0 '(8 9 10 11 12 13 14 15))

   ;; --- byte/number reading: AT&T immediates are decimal, 0x-hex, optionally signed. ---
   (as-hexd c) (? ((c >= 48) && (c <= 57)) (c - 48)
                  ((c >= 97) && (c <= 102)) (c - 87)
                  ((c >= 65) && (c <= 70)) (c - 55) (scare 'as-hexdigit c))
   (as-decv s) (foldl (\ a i ((a * 10) + (s i - 48))) 0 (jot (tally s)))
   (as-hexv s) (foldl (\ a i ((a * 16) + as-hexd (s i))) 0 (jot (tally s)))
   (as-neg? s) ((tally s > 0) && (s 0 = 45))
   (as-hex? s) ((tally s >= 2) && (s 0 = 48) && ((s 1 = 120) || (s 1 = 88)))
   (as-num s)  (? (as-neg? s) (0 - as-num (snip s 1 (tally s)))
                  (as-hex? s) (as-hexv (snip s 2 (tally s)))
                  (as-decv s))
   (as-numish? s) ((tally s > 0) && ((s 0 = 45) || ((s 0 >= 48) && (s 0 <= 57))))

   ;; --- little string helpers over the byte string (love has no split/find). ---
   (as-has? s c) ((: (g i) (? (i >= tally s) 0 (s i = c) 1 (g (i + 1)))) 0)
   (as-idx s c)  ((: (g i) (? (i >= tally s) -1 (s i = c) i (g (i + 1)))) 0)
   (as-ws? c)    ((c = 32) || (c = 9))
   (as-trim s)   (: n (tally s)
      a ((: (gl i) (? ((i < n) && as-ws? (s i)) (gl (i + 1)) i)) 0)
      b ((: (gr j) (? (j > a && as-ws? (s (j - 1))) (gr (j - 1)) j)) n)
      (snip s a b))
   (as-decomment s) (: h (as-idx s 35)                       ; '#' starts a comment to end of line
      (? (h < 0) s (snip s 0 h)))
   (as-lines s) (: n (tally s)                               ; split on newline
      ((: (g i st acc) (? (i >= n) (rev (snip s st n >< acc))
                          (s i = 10) (g (i + 1) (i + 1) (snip s st i >< acc))
                          (g (i + 1) st acc))) 0 0 ()))
   ;; split a string on a byte, trimming each field (used for the operand list on ',').
   (as-split s c) (: n (tally s)
      ((: (g i st acc) (? (i >= n) (rev (as-trim (snip s st n) >< acc))
                          (s i = c) (g (i + 1) (i + 1) (as-trim (snip s st i) >< acc))
                          (g (i + 1) st acc))) 0 0 ()))
   ;; the head mnemonic and the operand-string of an instruction line.
   (as-mnem s)  (: i (as-idx s 32) j (as-idx s 9)
                   k (? (i < 0) j (j < 0) i (? (i < j) i j))
                   (? (k < 0) s (snip s 0 k)))
   (as-rest s)  (: i (as-idx s 32) j (as-idx s 9)
                   k (? (i < 0) j (j < 0) i (? (i < j) i j))
                   (? (k < 0) "" (as-trim (snip s k (tally s)))))

   ;; --- operand parsing: -> a tagged form. (rg enc w) | (im n) | (lbl sym) | (rip sym) | (mem base disp) ---
   (as-reglook nm) (: r (peep as-regs (intern nm) ()) (? (two? r) r (scare 'as-badreg nm)))
   (as-operand t)
      (? (as-has? t 40)                                       ; contains '(' -> a memory operand
         (: op (as-idx t 40) inner (snip t (op + 1) (tally t - 1))     ; strip the ')'
            disp (snip t 0 op)
            (? (inner = "%rip") `('rip (intern disp))        ; sym(%rip)
               (: r (as-reglook (snip inner 1 (tally inner)))  ; disp(%reg) / (%reg)
                  `('mem <r (? (0 = tally disp) 0 (as-num disp))))))
         (t 0 = 37) ('rg >< as-reglook (snip t 1 (tally t)))        ; %reg
         (t 0 = 36) `('im (as-num (snip t 1 (tally t))))             ; $imm
         (as-numish? t) `('im (as-num t))                           ; a bare number
         `('lbl (intern t)))                                        ; a symbol (branch/call target)
   (as-ops rest) (? (0 = tally rest) () (map as-operand (as-split rest 44)))

   ;; --- encoder primitives (physical, width-aware; x64.l is the form reference). ---
   (as-hi n) (n >> 3)  (as-lo n) (n & 7)
   (as-rex w r x b) (? (w > 0 || r > 0 || x > 0 || b > 0)               ; emit REX only when a bit is set
                       `((64 | ((w << 3) | ((r << 2) | ((x << 1) | b))))) ())
   (as-mrm m r rm) ((m << 6) | (((r & 7) << 3) | (rm & 7)))
   (as-w8? o) (at o 2 = 8)                                    ; a register operand is 64-bit?
   (as-regn o) (at o 1)                                       ; its encoding number

   ;; the ALU group: mnemonic -> (rr-opcode  imm-ext). reg field carries the source (AT&T src,dst).
   as-alu (tabof
        `(('add >< '(1 0)) ('or >< '(9 1)) ('and >< '(33 4)) ('sub >< '(41 5))
           ('xor >< '(49 6)) ('cmp >< '(57 7))))
   ;; the condition codes: jcc mnemonic -> 0F 8x low byte.
   as-cc (tabof
        `(('je >< 132) ('jz >< 132) ('jne >< 133) ('jnz >< 133) ('jl >< 140) ('jge >< 141)
           ('jle >< 142) ('jg >< 143) ('jb >< 130) ('jc >< 130) ('jae >< 131) ('jnc >< 131)
           ('jbe >< 134) ('ja >< 135) ('js >< 136) ('jns >< 137) ('jo >< 128) ('jno >< 129)
           ('jp >< 138) ('jnp >< 139)))

   ;; alu $imm,%d and alu %s,%d.  d is the r/m, s the reg field.
   (as-alu-ri ext d w imm)
      (? (fits8 imm) (as-rex (? (w = 8) 1 0) 0 0 (as-hi d) + `(131 (as-mrm 3 ext d) (byte imm)))
         (catall `((as-rex (? (w = 8) 1 0) 0 0 (as-hi d)) `(129 (as-mrm 3 ext d)) (le32 imm))))
   (as-alu-rr rr d w s)
      (as-rex (? (w = 8) 1 0) (as-hi s) 0 (as-hi d) + `(rr (as-mrm 3 s d)))

   ;; mov: reg<-reg (89), reg<-imm (b8+rd / c7 /0 / movabs by width and range).
   (as-mov-rr d w s) (as-rex (? (w = 8) 1 0) (as-hi s) 0 (as-hi d) + `(137 (as-mrm 3 s d)))
   (as-mov-ri d w imm)
      (? (w = 8)
         (? (fits32 imm) (catall `((as-rex 1 0 0 (as-hi d)) `(199 (as-mrm 3 0 d)) (le32 imm)))    ; c7 /0 id
            (catall `((as-rex 1 0 0 (as-hi d)) `((184 | as-lo d)) (le64 imm))))                  ; movabs
         (catall `((as-rex 0 0 0 (as-hi d)) `((184 | as-lo d)) (le32 imm))))                     ; b8+rd id (32-bit)

   ;; --- the per-instruction encoders (each -> a byte/fix item list). ---
   (as-2 mn a b)                                             ; two-operand
      (: ka <a kb <b
         (? (mn = 'mov)
            (? ((ka = 'rg) && (kb = 'rg)) (as-mov-rr (as-regn b) (at >b 1) (as-regn a))
               ((ka = 'im) && (kb = 'rg)) (as-mov-ri (as-regn b) (at >b 1) (at a 1))
               ((ka = 'rip) && (kb = 'rg)) (as-lea (at a 1) (as-regn b))                     ; lea alias? no -- mov from rip is a load, M2
               (scare 'as-mov-form `(a b)))
            (mn = 'lea)
            (? ((ka = 'rip) && (kb = 'rg)) (as-lea (at a 1) (as-regn b))
               (scare 'as-lea-form `(a b)))
            (mn = 'test)
            (? ((ka = 'rg) && (kb = 'rg))
               (as-rex (? (as-w8? a) 1 0) (as-hi (as-regn a)) 0 (as-hi (as-regn b)) + `(133 (as-mrm 3 (as-regn a) (as-regn b))))
               (scare 'as-test-form `(a b)))
            (: alu (peep as-alu mn ())                       ; the ALU group
               (? !(two? alu) (scare 'as-op2 mn)
                  (kb = 'rg)
                  (: d (as-regn b) w (at >b 1)
                     (? (ka = 'rg) (as-alu-rr (at alu 0) d w (as-regn a))
                        (ka = 'im) (as-alu-ri (at alu 1) d w (at a 1))
                        (scare 'as-alu-src `(a b))))
                  (scare 'as-alu-dst `(a b))))))
   (as-lea sym d) (catall `((as-rex 1 (as-hi d) 0 0) `(141 (as-mrm 0 (as-lo d) 5)) `((fix 4 'rel32 sym 0))))
   (as-1 mn a)                                               ; one-operand
      (: k <a
         (? (mn = 'push) (: n (as-regn a) (? (n >= 8) `(65 (80 | as-lo n)) `((80 | n))))
            (mn = 'pop)  (: n (as-regn a) (? (n >= 8) `(65 (88 | as-lo n)) `((88 | n))))
            (member? mn '(neg not inc dec))
            (: n (as-regn a) w (at a 2)
               opc (? (member? mn '(inc dec)) 255 247)
               ext (? (mn = 'neg) 3 (mn = 'not) 2 (mn = 'inc) 0 1)
               (as-rex (? (w = 8) 1 0) 0 0 (as-hi n) + `(opc (as-mrm 3 ext n))))
            (member? mn '(call jmp))
            (: sym (at a 1) (? (mn = 'call) `(232 (fix 4 'rel32 sym 0)) `(233 (fix 4 'rel32 sym 0))))
            (peep as-cc mn ()) (: sym (at a 1) `(15 (peep as-cc mn 0) (fix 4 'rel32 sym 0)))
            (scare 'as-op1 mn)))
   (as-0 mn)                                                 ; nullary
      (? (mn = 'ret) '(195) (mn = 'nop) '(144) (mn = 'syscall) '(15 5)
         (mn = 'leave) '(201) (member? mn '(cqo cqto)) '(72 153)
         (mn = 'cdq) '(153) (mn = 'cdqe) '(72 152)             ; cdq is 99, cdqe is REX.W+98 (eax->rax) -- NOT the same byte
         (mn = 'hlt) '(244) (scare 'as-op0 mn))

   (as-instr mn ops)
      (: n (tally ops)
         (? (n = 0) (as-0 mn)
            (n = 1) (as-1 mn <ops)
            (n = 2) (as-2 mn <ops <>ops)
            (scare 'as-arity `(mn n))))

   ;; --- pass one: walk the lines, encode each, assign addresses, record labels + .globl. returns
   ;;     a tablet { 'items 'labels 'globls }. label defs emit no bytes; directives steer. ---
   (as-lay src)
      (: labels (tablet 0)  globls (tablet 0)  st (tablet 0)
         (go lines addr items)
            (? !(two? lines) (: _ (pin st 'items (rev items)) _ (pin st 'labels labels)
                                   _ (pin st 'globls globls) st)
               (: raw (as-trim (as-decomment <lines))
                  (? (0 = tally raw) (go >lines addr items)
                     (raw (tally raw - 1) = 58)                      ; "name:" -> a label
                     (: _ (pin labels (intern (snip raw 0 (tally raw - 1))) addr) (go >lines addr items))
                     (raw 0 = 46)                                    ; ".directive ..."
                     (: mn (as-mnem raw) rest (as-rest raw)
                        (? (mn = ".globl") (: _ (pin globls (intern rest) 1) (go >lines addr items))
                           (member? mn '(".text" ".data" ".section" ".p2align" ".align" ".type" ".size" ".file" ".ident"))
                           (go >lines addr items)                ; M1: single .text section, ignore layout dirs
                           (scare 'as-directive mn)))
                     (: mn (intern (as-mnem raw))
                        r (revonto (as-instr mn (as-ops (as-rest raw))) items 0)
                        (go >lines (addr + <r) >r)))))
         (go (as-lines src) 0 ()))

   ;; --- resolve intra-section fixups (near rel32) to final .text bytes, and the hex for goldens. ---
   (as-text src) (: st (as-lay src)
                    (resolve (peep st 'items ()) (peep st 'labels 0) (peep backends 'x64 ()) 0))
   (as-hex src)  (hexs (as-text src)))
