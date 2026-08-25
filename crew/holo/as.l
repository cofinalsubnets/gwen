; as -- a REAL assembler front over holo: it reads standard GNU AT&T `.s` and emits the SAME
; byte/fixup item stream the neutral backend (crew/holo/x64.l) produces, so crew/holo/obj.l lays the
; relocatable `.o` and crew/holo/link.l links it -- the front tooth of the gcc-free chain, paired
; with link.l's `ld`. where crew/holo/text.l parses holo's OWN mnemonics over abstract registers,
; THIS parses real x86-64: physical registers (%rax/%eax/..), the AT&T operand grammar, the directive
; set. the encoders mirror x64.l form-for-form but read the register number off the name and carry the
; operand WIDTH (x64.l is 64-bit-mode only). scope: the GPR core at all four widths -- ALU, mov,
; unary, push/pop, lea, test over register, immediate AND memory operands; call/jmp/jcc (NEAR;
; branch relaxation is M2), and the nullaries -- the sign-extend family in both spellings (cltq
; as well as cdqe), the flag ops, and the trap/system handful. a memory operand is AT&T's full
; disp(base,index,scale), rip-relative and indirect `*` included; the size suffix (movb/addl/..)
; gives the width wherever no register operand carries one.
;
; load AFTER holo.l + x64.l (bare, dev/test context -- the encoder primitives le32/byte/fix/modrm..
; are still on the global book; the eventual host path reaches them through the `holo` book):
;   out/host/love -l crew/holo/holo.l -l crew/holo/x64.l -l crew/holo/as.l ...

(module 'holo
(: ;; --- the physical register file: name -> (enc width-in-bytes). the low 8 have irregular
   ;; byte/word names, so spell the rows; r8..r15 are uniform (rN / rNd / rNw / rNb). ---
   as-regs (tablet 0)
   (as-reg! nm e w) (pin as-regs (intern nm) [e w])
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
   ;; ⚠ EVERY BYTE, or it is not a numeral. as-num reads digits off the FRONT and stops, so an
   ;; unchecked `$N' comes back 30 -- the character's own value -- and encodes as a number nobody
   ;; wrote. a symbolic immediate is a thing this front cannot do; it must SAY so.
   (as-dig? c)  ((c >= 48) && (c <= 57))
   (as-hexc? c) (as-dig? c || ((c >= 97) && (c <= 102)) || ((c >= 65) && (c <= 70)))
   (as-allof? s p k) (: n (tally s)
      ((: (g i) (? (i >= n) (n > k) (p (s i)) (g (i + 1)) 0)) k))
   (as-num? s) (? (as-neg? s) (as-num? (snip s 1 (tally s)))
                  (as-hex? s) (as-allof? s as-hexc? 2)
                  (as-allof? s as-dig? 0))
   (as-imm s) (? (as-num? s) (as-num s) (scare 'as-badnum s))   ; a number where one is required

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
   ;; split a string on a byte, trimming each field (the (base,index,scale) fields inside a paren).
   (as-split s c) (: n (tally s)
      ((: (g i st acc) (? (i >= n) (rev (as-trim (snip s st n) >< acc))
                          (s i = c) (g (i + 1) (i + 1) (as-trim (snip s st i) >< acc))
                          (g (i + 1) st acc))) 0 0 ()))
   ;; the operand list splits on TOP-LEVEL commas only -- a memory operand carries its own inside
   ;; the parens, and cutting there would tear it in three.
   (as-oplist s) (: n (tally s)
      ((: (g i st d acc)
          (? (i >= n) (rev (as-trim (snip s st n) >< acc))
             (s i = 40) (g (i + 1) st (d + 1) acc)
             (s i = 41) (g (i + 1) st (d - 1) acc)
             ((s i = 44) && (d = 0)) (g (i + 1) (i + 1) 0 (as-trim (snip s st i) >< acc))
             (g (i + 1) st d acc))) 0 0 0 ()))
   ;; the head mnemonic and the operand-string of an instruction line.
   (as-mnem s)  (: i (as-idx s 32) j (as-idx s 9)
                   k (? (i < 0) j (j < 0) i (? (i < j) i j))
                   (? (k < 0) s (snip s 0 k)))
   (as-rest s)  (: i (as-idx s 32) j (as-idx s 9)
                   k (? (i < 0) j (j < 0) i (? (i < j) i j))
                   (? (k < 0) "" (as-trim (snip s k (tally s)))))

   ;; --- operand parsing: -> a tagged form.
   ;;   (rg enc w) | (im n) | (lbl sym) | (rip sym) | (ind o) | (mem base index scale disp)
   ;; a memory operand is AT&T's disp(base,index,scale) with either register absent (-1 then); a
   ;; leading `*` marks the indirect branch target it wraps. ---
   (as-reglook nm) (: r (peep as-regs (intern nm) ()) (? (two? r) r (scare 'as-badreg nm)))
   (as-mreg f) (? (0 = tally f) -1 <(as-reglook (snip f 1 (tally f))))   ; a %reg field, or absent
   (as-operand t)
      (? (t 0 = 42) ['ind (as-operand (snip t 1 (tally t)))]     ; *%reg / *mem
         (as-has? t 40)                                       ; contains '(' -> a memory operand
         (: op (as-idx t 40) inner (snip t (op + 1) (tally t - 1))     ; strip the ')'
            disp (snip t 0 op)
            (? (inner = "%rip") ['rip (intern disp)]         ; sym(%rip) -- a SYMBOL displacement
               (: fs (as-split inner 44) k (tally fs)        ; ... everywhere else, a number
                  d (? (0 = tally disp) 0 (as-imm disp))
                  (? (k > 3) (scare 'as-memform t)
                     ['mem (as-mreg <fs) (? (k < 2) -1 (as-mreg (at fs 1)))
                           (? (k < 3) 1 (as-imm (at fs 2))) d]))))
         (t 0 = 37) ('rg >< as-reglook (snip t 1 (tally t)))        ; %reg
         (t 0 = 36) ['im (as-imm (snip t 1 (tally t)))]              ; $imm
         (as-num? t) ['im (as-num t)]                               ; a bare number
         ['lbl (intern t)])                                         ; a symbol (branch/call target)
   (as-ops rest) (? (0 = tally rest) () (map as-operand (as-oplist rest)))

   ;; --- encoder primitives (physical, width-aware; x64.l is the form reference). ---
   (as-hi n) (n >> 3)  (as-lo n) (n & 7)
   (as-rex f w r x b) (? (f > 0 || w > 0 || r > 0 || x > 0 || b > 0)   ; only when a bit is set,
                         [(64 | ((w << 3) | ((r << 2) | ((x << 1) | b))))] ())    ; or f forces one
   (as-mrm m r rm)  ((m << 6) | (((r & 7) << 3) | (rm & 7)))
   (as-sib sc ix b) ((sc << 6) | (((ix & 7) << 3) | (b & 7)))
   (as-scb s) (? (s = 1) 0 (s = 2) 1 (s = 4) 2 (s = 8) 3 (scare 'as-badscale s))
   (as-regn o) (at o 1)                                       ; a register operand's encoding number
   (as-regw o) (? (<o = 'rg) (at o 2) 0)                      ; ... and its width, 0 if it isn't one
   ;; spl/bpl/sil/dil name the low four's bytes only UNDER a REX -- bare, those slots spell
   ;; ah/ch/dh/bh -- so a byte operand in 4..7 forces an otherwise-empty one.
   (as-frc o) (? !(as-regw o = 1) 0 ((as-regn o >= 4) && (as-regn o <= 7)))
   (as-frcs ops) (foldl (\ a o (? (a > 0) a (as-frc o))) 0 ops)

   ;; --- the r/m half: a register (mod 3) or a memory operand, which lays a ModRM (+ SIB)
   ;; (+ displacement) and hands up the REX X and B bits. an rbp/r13 base has no disp-less slot
   ;; (mod 0 there means disp32) so force a disp byte; an rsp/r12 base needs a SIB to say so; a
   ;; base-less form is mod 0 + SIB base 5 + a full disp32; rip-relative is mod 0, rm 5, and the
   ;; four bytes are a rel32 fixup. ---
   (as-mem-rm rf o)
      (: b (at o 1) ix (at o 2) sc (at o 3) d (at o 4)
         (? (b < 0) ([(as-mrm 0 rf 4) (as-sib (as-scb sc) (? (ix < 0) 4 ix) 5)] + le32 d)
            (: md (? ((d = 0) && !(as-lo b = 5)) 0 (fits8 d) 1 2)
               dsp (? (md = 0) () (md = 1) [(byte d)] (le32 d))
               hd (? (ix >= 0)      [(as-mrm md rf 4) (as-sib (as-scb sc) ix b)]
                     (as-lo b = 4)  [(as-mrm md rf 4) (as-sib 0 4 b)]
                     [(as-mrm md rf (as-lo b))])
               (hd + dsp))))
   (as-rm rf o) (? (<o = 'mem) (as-mem-rm rf o)
                   (<o = 'rip) [(as-mrm 0 rf 5) (fix 4 'rel32 (at o 1) 0)]
                   (<o = 'rg)  [(as-mrm 3 rf (as-regn o))]
                   (scare 'as-notrm o))
   (as-rm-x o)  (? !(<o = 'mem) 0 (: i (at o 2) (? (i < 0) 0 (as-hi i))))
   (as-rm-b o)  (? (<o = 'mem) (: b (at o 1) (? (b < 0) 0 (as-hi b)))
                   (<o = 'rip) 0 (as-hi (as-regn o)))
   (as-mem? o)  ((<o = 'mem) || (<o = 'rip))

   ;; --- operand size. the AT&T suffix sets it; absent one it is read off a register operand, and
   ;; 0 means neither said -- fine for the 64-bit-default forms (push/pop/call/jmp), a raise for
   ;; anything else. width 2 takes a 66 prefix, width 1 the opcode one BELOW the wide one (89/88,
   ;; 01/00, c7/c6, f7/f6, ff/fe all sit in that relation). ---
   (as-sfxw c) (? (c = 98) 1 (c = 119) 2 (c = 108) 4 (c = 113) 8 0)
   as-sfxable '(mov add or and sub xor cmp test neg not inc dec push pop lea)
   (as-mnw s) (: n (tally s) w (? (n < 2) 0 (as-sfxw (s (n - 1))))    ; -> (stem width)
                 st (? (w = 0) 'x (intern (snip s 0 (n - 1))))
                 (? ((w > 0) && (member? st as-sfxable)) [st w] [(intern s) 0]))
   (as-p16 w)  (? (w = 2) '(102) ())
   (as-rexw w) (? (w = 8) 1 0)
   (as-opw opc w) (? (w = 1) (opc - 1) opc)
   (as-imw w n)   (? (w = 1) [(byte n)] (w = 2) (le16 n) (le32 n))
   (as-width sw ops)
      (: rw (foldl (\ a o (? (a > 0) a (as-regw o))) 0 ops)
         (? (sw = 0) rw
            ((rw > 0) && !(rw = sw)) (scare 'as-widthclash [sw rw])
            sw))
   ;; the whole reg-with-r/m family in one spelling: size prefix, REX, opcode, ModRM tail. `rn` is
   ;; the reg field -- a register encoding, or an opcode extension digit.
   (as-emit w frc opc rn rm)
      (catall [(as-p16 w) (as-rex frc (as-rexw w) (as-hi rn) (as-rm-x rm) (as-rm-b rm))
                opc (as-rm rn rm)])

   ;; the ALU group: mnemonic -> (rr-opcode  imm-ext). reg field carries the source (AT&T src,dst);
   ;; the reg->r/m direction is the table's opcode and r/m->reg is +2 (mov's 89/8b sit the same way).
   as-alu (tabof
        [('add >< '(1 0)) ('or >< '(9 1)) ('and >< '(33 4)) ('sub >< '(41 5))
          ('xor >< '(49 6)) ('cmp >< '(57 7))])
   ;; the condition codes: jcc mnemonic -> 0F 8x low byte.
   as-cc (tabof
        [('je >< 132) ('jz >< 132) ('jne >< 133) ('jnz >< 133) ('jl >< 140) ('jge >< 141)
          ('jle >< 142) ('jg >< 143) ('jb >< 130) ('jc >< 130) ('jae >< 131) ('jnc >< 131)
          ('jbe >< 134) ('ja >< 135) ('js >< 136) ('jns >< 137) ('jo >< 128) ('jno >< 129)
          ('jp >< 138) ('jnp >< 139)])

   ;; alu $imm,r/m: 80 /ext ib for a byte, else the 83 /ext ib short form where the immediate fits,
   ;; else 81 /ext iz.
   (as-alu-i ext w frc rm imm)
      (? (w = 1)     (as-emit 1 frc '(128) ext rm + [(byte imm)])
         (fits8 imm) (as-emit w frc '(131) ext rm + [(byte imm)])
         (as-emit w frc '(129) ext rm + as-imw w imm))

   ;; mov $imm,%reg: the b0/b8+r short forms by width, movabs where a 64-bit immediate needs it,
   ;; c7 /0 id where it fits signed-32.
   (as-mov-ri d w frc imm)
      (: rx (as-rex frc (as-rexw w) 0 0 (as-hi d))
         (? (w = 1) (catall [rx [(176 | as-lo d) (byte imm)]])
            (w = 2) (catall ['(102) rx [(184 | as-lo d)] (le16 imm)])
            (w = 4) (catall [rx [(184 | as-lo d)] (le32 imm)])
            (fits32 imm) (catall [rx [199 (as-mrm 3 0 d)] (le32 imm)])
            (catall [rx [(184 | as-lo d)] (le64 imm)])))

   ;; --- the per-instruction encoders (each -> a byte/fix item list). ---
   ;; ⚠ a rip-relative operand with an immediate TAIL is refused, not encoded: x64-patch measures
   ;; rel32 from the end of its own field, and holo's fixup carries no addend to walk past the tail.
   (as-2 mn w a b)                                           ; two-operand: AT&T src,dst
      (: ka <a kb <b frc (as-frcs [a b])
         alu (peep as-alu mn ())
         rr  (? (mn = 'mov) 137 (mn = 'test) 133 (two? alu) (at alu 0) 0)
         opi (? (mn = 'mov) 199 (mn = 'test) 247 0)          ; the imm-to-r/m opcode, 0 = the ALU's
         (? (mn = 'lea)
            (? ((kb = 'rg) && as-mem? a) (as-emit w frc '(141) (as-regn b) a)
               (scare 'as-lea-form [a b]))
            (rr = 0) (scare 'as-op2 mn)
            (w = 0)  (scare 'as-nowidth [mn a b])
            ((as-mem? a) && (as-mem? b)) (scare 'as-twomem [a b])
            (ka = 'rg) (as-emit w frc [(as-opw rr w)] (as-regn a) b)                ; reg -> r/m
            ((kb = 'rg) && as-mem? a)                                              ; r/m -> reg
            (as-emit w frc [(as-opw (? (mn = 'test) rr (rr + 2)) w)] (as-regn b) a)
            (ka = 'im)                                                             ; $imm -> r/m
            (: _ (? (<b = 'rip) (scare 'as-ripimm [a b]) 0)
               im (at a 1)
               (? ((mn = 'mov) && (kb = 'rg)) (as-mov-ri (as-regn b) w frc im)
                  (opi > 0) (as-emit w frc [(as-opw opi w)] 0 b + as-imw w im)
                  (as-alu-i (at alu 1) w frc b im)))
            (scare 'as-op2-form [a b])))
   (as-1 mn w a)                                             ; one-operand
      (: k <a  frc (as-frc a)
         pw (? (w = 2) 2 0)                                  ; push/pop take 66 but never REX.W
         (? (mn = 'push)                                     ; ff /6
            (? (k = 'rg) (: n (as-regn a)
                            ((as-p16 pw) + (? (n >= 8) [65 (80 | as-lo n)] [(80 | n)])))
               (as-emit pw 0 '(255) 6 a))
            (mn = 'pop)                                      ; 8f /0
            (? (k = 'rg) (: n (as-regn a)
                            ((as-p16 pw) + (? (n >= 8) [65 (88 | as-lo n)] [(88 | n)])))
               (as-emit pw 0 '(143) 0 a))
            (member? mn '(neg not inc dec))
            (: opc (? (member? mn '(inc dec)) 255 247)
               ext (? (mn = 'neg) 3 (mn = 'not) 2 (mn = 'inc) 0 1)
               (? (w = 0) (scare 'as-nowidth [mn a]) (as-emit w frc [(as-opw opc w)] ext a)))
            (k = 'ind)                                       ; call/jmp through a register or memory
            (: o (at a 1)
               (? (mn = 'call) (as-emit 0 0 '(255) 2 o)
                  (mn = 'jmp)  (as-emit 0 0 '(255) 4 o)
                  (scare 'as-op1 mn)))
            (member? mn '(call jmp))
            (: sym (at a 1) (? (mn = 'call) [232 (fix 4 'rel32 sym 0)] [233 (fix 4 'rel32 sym 0)]))
            (peep as-cc mn ()) (: sym (at a 1) [15 (peep as-cc mn 0) (fix 4 'rel32 sym 0)])
            (scare 'as-op1 mn)))
   ;; --- the nullaries, in both spellings. AT&T names the sign-extend family c<from>t<to> and
   ;; Intel names it c<from><to>, which puts the two halves of it ONE LETTER APART under names
   ;; that pair up wrong: cltq widens the accumulator (eax -> rax) and cltd splits it across
   ;; edx:eax, and likewise cwtl against cwtd. gcc emits the AT&T spelling of all six. ---
   as-nils (tabof
        [('ret >< '(195)) ('retq >< '(195)) ('nop >< '(144)) ('hlt >< '(244))
          ('syscall >< '(15 5)) ('leave >< '(201)) ('leaveq >< '(201))
          ('cbw >< '(102 152))  ('cbtw >< '(102 152))        ; al -> ax
          ('cwde >< '(152))     ('cwtl >< '(152))            ; ax -> eax
          ('cdqe >< '(72 152))  ('cltq >< '(72 152))         ; eax -> rax
          ('cwd >< '(102 153))  ('cwtd >< '(102 153))        ; ax -> dx:ax
          ('cdq >< '(153))      ('cltd >< '(153))            ; eax -> edx:eax
          ('cqo >< '(72 153))   ('cqto >< '(72 153))         ; rax -> rdx:rax
           ;; the flag, trap and system nullaries. `int3' is what holo's own text calls trap.
          ('int3 >< '(204)) ('ud2 >< '(15 11)) ('pause >< '(243 144))
          ('cpuid >< '(15 162)) ('rdtsc >< '(15 49))
          ('cli >< '(250)) ('sti >< '(251))                  ; the interrupt flag
          ('clc >< '(248)) ('stc >< '(249)) ('cmc >< '(245))   ; ... the carry flag
          ('cld >< '(252)) ('std >< '(253))                  ; ... and the direction flag
          ('lahf >< '(159)) ('sahf >< '(158))
          ('xlat >< '(215)) ('xlatb >< '(215))
          ('sysret >< '(15 7)) ('sysretl >< '(15 7)) ('sysretq >< '(72 15 7))])
   (as-0 mn) (: b (peep as-nils mn ()) (? (two? b) b (scare 'as-op0 mn)))

   (as-instr mn sw ops)
      (: n (tally ops) w (as-width sw ops)
         (? (n = 0) (as-0 mn)
            (n = 1) (as-1 mn w <ops)
            (n = 2) (as-2 mn w <ops <>ops)
            (scare 'as-arity [mn n])))

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
                     (: mw (as-mnw (as-mnem raw))
                        r (revonto (as-instr <mw (at mw 1) (as-ops (as-rest raw))) items 0)
                        (go >lines (addr + <r) >r)))))
         (go (as-lines src) 0 ()))

   ;; --- resolve intra-section fixups (near rel32) to final .text bytes, and the hex for goldens. ---
   (as-text src) (: st (as-lay src)
                    (resolve (peep st 'items ()) (peep st 'labels 0) (peep backends 'x64 ()) 0))
   (as-hex src)  (hexs (as-text src)))
) ; module 'holo
