; holotest -- the gate for the neutral assembler + its x86-64 backend. each golden below is the exact
; hex the assembler emits, and EVERY one was checked against the real toolchain: emit the bytes,
; `objdump -d -M intel`, confirm the mnemonic. so this file is the frozen record of that round-trip
; -- if a golden drifts, either the encoder broke or the disassembler disagrees, and either is a bug.
; run: cat crew/holo/{holo,x64,arm64,thumb2,thumb1,text,elf,holotest}.l | out/host/love
; (exits nonzero on any miss). it gates both backends, the text front-end, and the ELF linker.

(: pass (tablet 0) _ (pin pass 0 0)
   fail (tablet 0) _ (pin fail 0 0)
   (bump t)  (pin t 0 (1 + (peep t 0 0)))
   (sayln s) (: _ (puts s) (flush out))
   (chk-eq name want got)
      (? (want = got) (bump pass)
         (: _ (bump fail)
            (sayln ("FAIL " + (name + (": want " + ((show want) + (" got " + ((show got) + "\n")))))))))
   (chk2 name want target prog) (chk-eq name want (holo-hex target prog))
   (chk name want prog) (chk2 name want 'x64 prog)

   ;; -- the golden table: (name  expected-hex  IR-program). --
   tests `(
      ;; immediates
      `("li.small"    "b83c000000"            '((li r0 60)))            ; mov eax,60 (zero-extends)
      `("li.hireg"    "41b83c000000"          '((li r7 60)))            ; mov r8d,60 (REX.B alone)
      `("li.u32"      "b800000090"            '((li r0 2415919104)))    ; mov eax,0x90000000 -- zero-extend, no movabs
      `("li.neg1"     "48c7c0ffffffff"        '((li r0 -1)))
      `("li.movabs"   "48b84c9c8cdac1fc0300"  '((li r0 1122334455667788)))
      ;; register moves (low + REX-extended)
      `("mov.rr"      "4889d8"                '((mov r0 r3)))
      `("mov.ext"     "4d89f8"                '((mov r7 r14)))
      ;; ALU, register source
      `("add.rr"      "4801d8"                '((add r0 r0 r3)))
      `("sub.rr"      "4829c8"                '((sub r0 r0 r1)))
      `("xor.zero"    "4831c0"                '((xor r0 r0 r0)))
      `("or.rr"       "4809c8"                '((or  r0 r0 r1)))
      `("and.rr"      "4821c8"                '((and r0 r0 r1)))
      ;; ALU, immediate source (0x81 group)
      `("add.imm"     "4883c005"              '((add r0 r0 5)))     ; 0x83 ib: the byte-immediate short form
      `("cmp.imm"     "4883f807"              '((cmp r0 7)))
      `("cmp.rr"      "4839d8"                '((cmp r0 r3)))
      ;; three-address -> two-address lowering
      `("alu.movop"   "4889c24829da"          '((sub r2 r0 r3)))   ; mov rdx,rax ; sub rdx,rbx
      `("alu.swap"    "4801d8"                '((add r0 r3 r0)))   ; commutes -> add rax,rbx, no mov
      `("add.movop"   "4889c24801da"          '((add r2 r0 r3)))   ; mov rdx,rax ; add rdx,rbx (no lea fusion -- the emit-alu note)
      `("add.movop.i" "4889c14883c105"        '((add r1 r0 5)))    ; imm form keeps the dance too
      ;; ('align n) pads to the boundary with the backend's nop unit (lay-level, no lowering)
      `("align.pad"   "c3909090c3"            '((ret) (align 4) (label x) (ret)))
      `("align.zero"  "c3"                    '((align 16) (ret)))  ; already on the boundary: no pad
      ;; imul (true 3-operand imm form; rr folds two-address)
      `("imul.rr"     "480fafc3"              '((imul r0 r0 r3)))
      `("imul.imm"    "4869d007000000"        '((imul r2 r0 7)))
      ;; memory: the rsp(SIB) and rbp(forced-disp) base quirks, disp8 + disp32
      `("ld.base"     "488b07"                '((ld r0 r6 0)))
      `("ld.disp8"    "488b4708"              '((ld r0 r6 8)))
      `("ld.rbp0"     "488b4500"              '((ld r0 r4 0)))     ; rbp base forces a 0 disp byte
      `("ld.rbp.d8"   "488b4564"              '((ld r0 r4 100)))
      ;; the sized family (movsx/movsxd loads, truncating stores, in-register
      ;; extends, CL shifts) -- each checked vs objdump -M intel (mnemonics in
      ;; the trailing comments)
      `("ld1.rbp"     "480fbe45f8"            '((ld1 r0 r4 -8)))     ; movsx  rax, BYTE [rbp-8]
      `("ld2.sp"      "480fbf5c2410"          '((ld2 r3 sp 16)))     ; movsx  rbx, WORD [rsp+16]
      `("ld4.rbp"     "4c63459c"              '((ld4 r7 r4 -100)))   ; movsxd r8, DWORD [rbp-100]
      `("st1.rbp"     "408845f8"              '((st1 r4 -8 r0)))     ; mov BYTE [rbp-8], al
      `("st2.sp"      "6640895c2410"          '((st2 sp 16 r3)))     ; mov WORD [rsp+16], bx
      `("st4.rbp"     "4489459c"              '((st4 r4 -100 r7)))   ; mov DWORD [rbp-100], r8d
      `("sx1"         "480fbec0"              '((sx1 r0)))           ; movsx rax, al
      `("sx2.hi"      "4d0fbfc0"              '((sx2 r7)))           ; movsx r8, r8w
      `("sx4"         "4863db"                '((sx4 r3)))           ; movsxd rbx, ebx
      `("ldu1.rbp"    "480fb645f8"            '((ldu1 r0 r4 -8)))    ; movzx rax, BYTE [rbp-8]  (unsigned char)
      `("ldu2.sp"     "480fb75c2410"          '((ldu2 r3 sp 16)))    ; movzx rbx, WORD [rsp+16] (unsigned short)
      `("ldu4.rbp"    "448b459c"              '((ldu4 r7 r4 -100)))  ; mov r8d, DWORD [rbp-100] (unsigned int, zero-extends)
      `("zx1"         "480fb6c0"              '((zx1 r0)))           ; movzx rax, al
      `("zx2.hi"      "4d0fb7c0"              '((zx2 r7)))           ; movzx r8, r8w
      `("zx4"         "408bdb"                '((zx4 r3)))           ; mov ebx, ebx (zero-extends)
      ;; cas: lock cmpxchg, the accumulator operand wired to r0 (rax)
      `("cas.rsi"     "f0480fb10e"            '((cas r0 r1 r5)))     ; lock cmpxchg [rsi], rcx
      `("cas.hi"      "f04d0fb111"            '((cas r0 r9 r8)))     ; lock cmpxchg [r9], r10
      `("cas4.rdi"    "f0400fb10f"            '((cas4 r0 r1 r6)))    ; lock cmpxchg DWORD [rdi], ecx
      `("cas4.rbp"    "f0440fb15500"          '((cas4 r0 r9 r4)))    ; lock cmpxchg DWORD [rbp], r10d (forced 0 disp)
      `("udiv"        "52564889d831d248f734244883c4085a4889c3"      '((udiv r3 r3 r5)))   ; xor rdx + div (unsigned)
      `("urem"        "52564889d831d248f734244889d04883c4085a4889c3" '((urem r3 r3 r5 r2)))  ; unsigned rem (rdx; the scratch t is rdx's job here, ignored)
      `("shlv"        "48d3e0"                '((shlv r0 r1)))       ; shl rax, cl
      `("shrv.hi"     "49d3e8"                '((shrv r7 r1)))       ; shr r8, cl
      `("sarv"        "48d3fb"                '((sarv r3 r1)))       ; sar rbx, cl
      ;; la: rip-relative lea onto a data label laid with (label)+(raw ..)
      `("la.data"     "488d05070000004c8d050000000041424300"
                       '((la r0 x) (la r7 x) (label x) (raw (65 66 67 0))))  ; lea rax/r8, [rip -> x]
      ;; abs64: an 8-byte ABSOLUTE address of a label baked into data (a static pointer). at org 0,
      ;; tgt sits at offset 8 (just past the fixup), so the word reads 8, then ret.
      `("abs64.data"  "0800000000000000c3"
                       '((raw ((fix 8 abs64 tgt 0))) (label tgt) (ret)))
      `("st.sib"      "4889442410"            '((st sp 16 r0)))    ; rsp base needs a SIB byte
      `("st.disp32"   "48899c2400010000"      '((st sp 256 r3)))
      `("lea.disp8"   "488d4710"              '((lea r0 r6 16)))
      `("lea.neg"     "488d6424e0"            '((lea sp sp -32)))
      `("lean"        "488d4710"              '((lean r0 r6 16)))       ; cc's consumed frame address: lea's bytes, off the sibcall gate's books
      ;; stack
      `("push.pop"    "5059"                  '((push r0) (pop r1)))
      `("push.ext"    "41504158"              '((push r7) (pop r7)))
      ;; shifts & rotates: D1 /ext for a count of 1, C1 /ext ib otherwise
      `("sar.1"       "48d1f8"                '((sar r0 1)))      ; the fixnum untag (rax)
      `("shr.1.ext"   "49d1e9"                '((shr r8 1)))      ; r8 -> x86 r9 (REX.B): shr r9,1
      `("shl.imm"     "48c1e004"              '((shl r0 4)))
      `("rol.32"      "48c1c020"              '((rol r0 32)))     ; the hash rotate (rol rax,32)
      `("sar.imm.ext" "49c1fd05"              '((sar r12 5)))     ; r12 -> x86 r13: sar r13,5
      ;; test: 0x85 /r reg,reg ; 0xF7 /0 id reg,imm32
      `("test.rr"     "4885c0"                '((test r0 r0)))    ; test rax,rax (divide-by-zero check)
      `("test.imm"    "48f7c001000000"        '((test r0 1)))     ; test rax,1   (the fixnum low-bit guard)
      ;; SIB memory: [base + index*scale + disp]. r7->x86 r8, r10->x86 r11 (the map-probe's slot regs)
      `("leax.1.1"    "488d4c0001"            '((leax r1 r0 r0 1 1)))   ; lea rcx,[rax+rax*1+1]
      `("leax.dbl"    "4c8d1c12"              '((leax r10 r2 r2 1 0)))  ; lea r11,[rdx+rdx*1]
      `("ldx.scale8"  "4b8b04d8"              '((ldx r0 r7 r10 8 0)))   ; mov rax,[r8+r11*8]
      `("ldx.s8.d8"   "4b8b44d808"            '((ldx r0 r7 r10 8 8)))   ; mov rax,[r8+r11*8+8]
      `("stx.scale4"  "48890c98"              '((stx r0 r3 4 0 r1)))    ; mov [rax+rbx*4],rcx
      `("ldxb.s1.d16" "480fb6440310"          '((ldxb r0 r3 r0 1 16)))  ; movzx rax,byte [rbx+rax*1+16]  (the string-lane peep read)
      `("stxb.s1.d16" "4088440310"            '((stxb r3 r0 1 16 r0)))  ; mov byte [rbx+rax*1+16],al     (the cask-lane pin store)
      ;; unary in-place
      `("neg"         "48f7d8"                '((neg r0)))
      `("not"         "48f7d3"                '((not r3)))
      `("inc"         "48ffc0"                '((inc r0)))
      `("dec"         "48ffcf"                '((dec r6)))
      ;; zero-operand
      `("ret"         "c3"                    '((ret)))
      `("sys"         "0f05"                  '((sys)))
      `("nop"         "90"                    '((nop)))
      `("trap"        "cc"                    '((trap)))
      ;; control flow with label fixups (rel32, measured from end of field). plain assemble
      ;; lays every branch long-form; the relaxing layout is its own entry (laylax, below).
      `("call.fwd"    "e800000000c3"          '((call foo) (label foo) (ret)))
      `("loop"        "4839c80f840800000048ffc8e9efffffffc3"
                       '((label top) (cmp r0 r1) (br eq done) (dec r0) (jmp top) (label done) (ret)))
      `("jcc.all"
         "0f8c240000000f851e0000000f8f180000000f8d120000000f8e0c0000000f87060000000f8200000000c3"
         '((br lt a) (br ne a) (br gt a) (br ge a) (br le a) (br above a) (br below a) (label a) (ret)))
      ;; the P0 glaze ops -- indirect jump, flag-setting add/sub (+ the vs/vc conds), overflow-checked
      ;; multiply, condition->0/1. each objdump-verified (-M intel).
      `("jmpr.rax"    "ffe0"                  '((jmpr r0)))              ; jmp rax  (Continue/deopt tail)
      `("jmpr.r8"     "41ffe0"                '((jmpr r7)))              ; jmp r8
      `("callr.rax"   "ffd0"                  '((callr r0)))            ; call rax (through a fn pointer)
      `("callr.r8"    "41ffd0"                '((callr r7)))            ; call r8
      `("set.eq"      "0f94c0480fb6c0"        '((set eq r0)))           ; sete al ; movzx rax,al
      `("set.lt"      "0f9cc0480fb6c0"        '((set lt r0)))           ; setl al ; movzx rax,al
      `("set.p"       "0f9ac0480fb6c0"        '((set p r0)))            ; setp al ; movzx rax,al (ucomisd unordered)
      `("set.np"      "0f9bc1480fb6c9"        '((set np r1)))           ; setnp cl ; movzx rcx,cl
      `("adds"        "4c01c0"                '((adds r0 r0 r7)))       ; add rax,r8  (sets OF -> br vs)
      `("subs"        "4c29c0"                '((subs r0 r0 r7)))       ; sub rax,r8  (sets OF)
      `("mulo.prog"   "490fafc00f8005000000e902000000ffe6c3"           ; imul rax,r8 ; jo ovf ; jmp done ; jmp rsi ; ret
                       '((mulo r0 r0 r7 r4 ovf) (jmp done) (label ovf) (jmpr r5) (label done) (ret)))
      `("div"         "524150489948f73c244883c4085a"                   ; push rdx;push r8;cqo;idiv [rsp];add rsp,8;pop rdx
                       '((div r0 r0 r7)))
      `("rem"         "524150489948f73c244889d04883c4085a"             ; ... mov rax,rdx (remainder) ...
                       '((rem r0 r0 r7 r4)))
      ;; a whole program: write(1, buf, 13); exit(0)  -- r0=rax carries the syscall number
      `("syscall.prog"
         "b801000000bf01000000be00100000ba0d0000000f05b83c000000bf000000000f05"
         '((li r0 1) (li r6 1) (li r5 4096) (li r2 13) (sys) (li r0 60) (li r6 0) (sys)))
      ;; SSE2 / double lane (the float glaze). reg-reg binops, gp<->xmm moves, and the xmm<->[mem]
      ;; spill/box ops + ucomisd that the glaze's float-result lane uses. objdump-verified (-M intel).
      `("addsd"       "f20f58c1"               '((addsd f0 f1)))         ; addsd xmm0,xmm1
      `("mulsd"       "f20f59c1"               '((mulsd f0 f1)))         ; mulsd xmm0,xmm1
      `("cvtsi2sd"    "f2480f2ac0"             '((cvtsi2sd f0 r0)))      ; cvtsi2sd xmm0,rax
      `("cvttsd2si"   "f2480f2cc0"             '((cvttsd2si r0 f0)))     ; cvttsd2si rax,xmm0 (truncate)
      `("movqxr"      "66480f6ec0"             '((movqxr f0 r0)))        ; movq xmm0,rax
      `("ucomisd"     "660f2ec8"               '((ucomisd f1 f0)))       ; ucomisd xmm1,xmm0 (sets flags)
      `("stsd.disp"   "f20f114208"             '((stsd r2 8 f0)))        ; movsd [rdx+8],xmm0   (box store)
      `("stsd.rsp"    "f20f110424"             '((stsd sp 0 f0)))        ; movsd [rsp],xmm0     (spill)
      `("ldsd.rsp"    "f20f100c24"             '((ldsd f1 sp 0)))        ; movsd xmm1,[rsp]     (reload)
      ;; the 4-byte single lane (the C `float` slot): widen/narrow + movss load/store
      `("cvtss2sd"    "f30f5ac1"               '((cvtss2sd f0 f1)))      ; cvtss2sd xmm0,xmm1 (single->double)
      `("cvtsd2ss"    "f20f5ac8"               '((cvtsd2ss f1 f0)))      ; cvtsd2ss xmm1,xmm0 (double->single)
      `("stss"        "f30f114208"             '((stss r2 8 f0)))        ; movss [rdx+8],xmm0
      `("ldss"        "f30f100b"               '((ldss f1 r3 0))))       ; movss xmm1,[rbx]
   _ (foldl (\ _ t (chk (at t 0) (at t 1) (at t 2))) 0 tests)

   ;; -- label addresses are exposed on the assemble result, for a loader/linker to read. --
   loopres (assemble 'x64 '((label top) (cmp r0 r1) (br eq done) (dec r0) (jmp top) (label done) (ret)))
   _ (chk-eq "addr.top"  0  (label-at loopres 'top))
   _ (chk-eq "addr.done" 17 (label-at loopres 'done))

   ;; -- the relaxing layout (laylax): the object writer's entry. in-range jmp/br land as the
   ;; 2-byte short forms (EB rel8 / 7x rel8); an external or weak-marked target stays rel32.
   ;; same (items . labels) shape as lay, resolved the same way.
   rx-be (peep backends 'x64 ())
   (rx prog longs)
      (: p (laylax prog rx-be 0 (tablet 0) longs)
         (hexs (resolve <p >p rx-be 0)))
   _ (chk-eq "rlx.loop" "4839c8740548ffc8ebf6c3"
        (rx '((label top) (cmp r0 r1) (br eq done) (dec r0) (jmp top) (label done) (ret)) (tablet 0)))
   _ (chk-eq "rlx.jcc.all" "7c0c750a7f087d067e0477027200c3"
        (rx '((br lt a) (br ne a) (br gt a) (br ge a) (br le a) (br above a) (br below a) (label a) (ret)) (tablet 0)))
   _ (chk-eq "rlx.mulo" "490fafc00f8002000000eb02ffe6c3"                ; the mulo-inner jo stays rel32; the jmp lands short
        (rx '((mulo r0 r0 r7 r4 ovf) (jmp done) (label ovf) (jmpr r5) (label done) (ret)) (tablet 0)))
   _ (chk-eq "rlx.weak-stays-long" "e90b0000004839c80f8402000000ffe6c3" ; both wf-targets held long by the longs set
        (: lg (tablet 0) _ (pin lg 'wf 1)
           (rx '((jmp wf) (cmp r0 r1) (br eq wf) (jmpr r5) (label wf) (ret)) lg)))
   _ (chk-eq "rlx.labels" 10
        (: p (laylax '((label top) (cmp r0 r1) (br eq done) (dec r0) (jmp top) (label done) (ret))
                     rx-be 0 (tablet 0) (tablet 0))
           (peep >p 'done ())))

   ;; -- the encoder REFUSES a three-way alias it cannot honestly encode (d <- a OP d, d != a,
   ;; non-commutative): it raises 'alias-dst rather than emit subtly wrong bytes. we trap the scare
   ;; with a help that delegates everything else (incl the reader's own eof/more) back to welp. --
   caught (tablet 0) _ (pin caught 0 0)
   _ (: help (\ s a b (? (a = 'alias-dst) (: _ (pin caught 0 1) ()) (welp s a b))))
   _ (holo-bytes 'x64 '((sub r0 r3 r0)))
   _ (chk-eq "alias.refused" 1 (peep caught 0 0))

   ;; -- the text front-end (crew/holo/text.l) parses to the same IR, so it assembles byte-identically. --
   _ (chk-eq "text.basic" (holo-hex 'x64 '((li r0 60) (ret)))
                          (holo-hex 'x64 (asm-text "li r0, 60\nret")))
   _ (chk-eq "text.full"
        (holo-hex 'x64 '((add r0 r0 r1) (label loop) (dec r0) (br ne loop) (ret)))
        (holo-hex 'x64 (asm-text "add r0, r0, r1   ; with a comment\nlabel loop\n  dec r0\n  br ne, loop\n\n  ret\n")))

   ;; -- the ELF linker (crew/holo/elf.l) wraps assembled bytes in a well-formed static executable. --
   elfprog '((li r0 60) (li r6 0) (sys))
   elfres  (elf64 'x64 (\ a elfprog) `(65 66 67))
   elfimg  (peep elfres 'bytes 0)
   _ (chk-eq "elf.magic"   127     (at elfimg 0))        ; \x7f E L F
   _ (chk-eq "elf.machine" 62      (at elfimg 18))       ; e_machine = x86-64
   _ (chk-eq "elf.entry"   4194424 (peep elfres 'entry 0))
   _ (chk-eq "elf.size"    (120 + ((tally (holo-bytes 'x64 elfprog)) + 3)) (tally elfimg))
   _ (chk-eq "elf.arm64-machine" 183 (at (peep (elf64 'arm64 (\ a elfprog) ()) 'bytes 0) 18))

   ;; -- the aarch64 backend (crew/holo/arm64.l): the SAME neutral IR, a second target. fixed 32-bit
   ;; little-endian words; every golden checked vs llvm-mc / llvm-objdump. --
   a64tests `(
      `("a64.li"     "800780d2"          '((li r0 60)))
      `("a64.movk"   "00cf8ad28046a2f2"  '((li r0 305419896)))    ; 0x12345678: movz + movk
      `("a64.mov"    "e00301aa"          '((mov r0 r1)))
      `("a64.add"    "2000028b"          '((add r0 r1 r2)))        ; native 3-address, no dance
      `("a64.sub"    "200002cb"          '((sub r0 r1 r2)))
      `("a64.and"    "2000028a"          '((and r0 r1 r2)))
      `("a64.or"     "200002aa"          '((or  r0 r1 r2)))
      `("a64.xor"    "200002ca"          '((xor r0 r1 r2)))        ; -> eor
      `("a64.addi"   "20140091"          '((add r0 r1 5)))
      `("a64.subi"   "201400d1"          '((sub r0 r1 5)))
      `("a64.addneg" "201400d1"          '((add r0 r1 -5)))        ; negative imm flips add->sub
      `("a64.cmp"    "1f0001eb"          '((cmp r0 r1)))
      `("a64.cmpi"   "1f1c00f1"          '((cmp r0 7)))
      `("a64.mul"    "207c029b"          '((imul r0 r1 r2)))
      `("a64.neg"    "e00300cb"          '((neg r0)))
      `("a64.not"    "e00320aa"          '((not r0)))              ; -> mvn
      `("a64.ldr"    "200040f9"          '((ld r0 r1 0)))
      `("a64.ldr8"   "200440f9"          '((ld r0 r1 8)))
      `("a64.str"    "e30b00f9"          '((st sp 16 r3)))
      `("a64.ldur"   "20805ff8"          '((ld r0 r1 -8)))         ; unaligned/neg -> LDUR
      `("a64.push"   "e00f1ff8"          '((push r0)))             ; str x0,[sp,#-16]!
      `("a64.pop"    "e00741f8"          '((pop r0)))              ; ldr x0,[sp],#16
      `("a64.lea"    "ff8300d1"          '((lea sp sp -32)))
      `("a64.ret"    "c0035fd6"          '((ret)))
      `("a64.svc"    "010000d4"          '((sys)))
      `("a64.nop"    "1f2003d5"          '((nop)))
      `("a64.align"  "c0035fd61f2003d51f2003d51f2003d5c0035fd6" '((ret) (align 16) (label x) (ret)))  ; nop-word fill
      `("a64.brk"    "000020d4"          '((trap)))
      `("a64.loop"   "1f0001eb60000054000400d1fdffff17c0035fd6"
                      '((label top) (cmp r0 r1) (br eq done) (dec r0) (jmp top) (label done) (ret)))
      ;; the P0 glaze ops -- each llvm-mc-verified (--triple=aarch64). adds/subs are the S-variant
      ;; (set NZCV); set is CSET; mulo is the SMULH+CMP-vs-ASR#63 overflow idiom (no flag-setting mul).
      `("a64.jmpr"   "00001fd6"          '((jmpr r0)))                 ; br x0
      `("a64.callr"  "00003fd6"          '((callr r0)))                ; blr x0  (through a fn pointer)
      `("a64.abs64"  "0800000000000000c0035fd6"                        ; an 8-byte absolute label address, then ret
                      '((raw ((fix 8 abs64 tgt 0))) (label tgt) (ret)))
      `("a64.set.eq" "e0179f9a"          '((set eq r0)))               ; cset x0, eq
      `("a64.set.vs" "e0779f9a"          '((set vs r0)))               ; cset x0, vs  (overflow -> 0/1)
      `("a64.adds"   "000007ab"          '((adds r0 r0 r7)))           ; adds x0,x0,x7
      `("a64.subs"   "600007eb"          '((subs r0 r3 r7)))           ; subs x0,x3,x7
      `("a64.mulo.prog" "047c479b007c079b9ffc80eb4100005402000014a0001fd6c0035fd6"   ; smulh x4,x0,x7; mul x0,x0,x7; cmp x4,x0,asr#63; b.ne ovf; b done; br x5; ret
                      '((mulo r0 r0 r7 r4 ovf) (jmp done) (label ovf) (jmpr r5) (label done) (ret)))
      ;; P1: shifts/test/div/rem/indexed -- the integer + map lanes on aarch64. llvm-mc-verified.
      `("a64.sar"    "00fc4193"          '((sar r0 1)))                ; asr x0,x0,#1   (untag)
      `("a64.shr"    "e7fc41d3"          '((shr r7 1)))                ; lsr x7,x7,#1
      `("a64.shl"    "00f07dd3"          '((shl r0 3)))                ; lsl x0,x0,#3
      `("a64.rol"    "0080c093"          '((rol r0 32)))               ; ror x0,x0,#32  (the hash rotate)
      `("a64.ror"    "0020c093"          '((ror r0 8)))                ; ror x0,x0,#8
      `("a64.test.r" "1f0007ea"          '((test r0 r7)))              ; tst x0,x7
      `("a64.test.i" "1f0040f2"          '((test r0 1)))               ; tst x0,#1  (the gap/fixnum low-bit check)
      `("a64.and.i"  "00084092"          '((and r0 r0 7)))             ; and x0,x0,#7  (bitmask immediate)
      `("a64.div"    "000cc79a"          '((div r0 r0 r7)))            ; sdiv x0,x0,x7
      `("a64.rem"    "040cc79a8080079b"  '((rem r0 r0 r7 r4)))         ; sdiv x4,x0,x7 ; msub x0,x4,x7,x0
      `("a64.ldx"    "e00c008b000440f9"  '((ldx r0 r7 r0 8 8)))        ; add x0,x7,x0,lsl#3 ; ldr x0,[x0,#8]
      `("a64.leax"   "2101098b21040091"  '((leax r1 r9 r9 1 1)))       ; add x1,x9,x9 ; add x1,x1,#1
      `("a64.stx"    "f00c008b080600f9"  '((stx r7 r0 8 8 r8)))        ; add x16,x7,x0,lsl#3 ; str x8,[x16,#8]
      `("a64.ldxb"   "6000008b00404039"  '((ldxb r0 r3 r0 1 16)))      ; add x0,x3,x0 ; ldrb w0,[x0,#16]   (string-lane peep read)
      `("a64.stxb"   "7000008b00420039"  '((stxb r3 r0 1 16 r0)))      ; add x16,x3,x0 ; strb w0,[x16,#16] (cask-lane pin store)
      `("a64.li.big" "a0828fd240e9aff22037cff2e0c6f3f2"               ; movz+3 movk = 0x9e3779b97f4a7c15 (bignum-safe lane via //%)
                      '((li r0 11400714819323198485)))
      ;; the link register (x30) by name -- a callee spills it across nested calls (BL clobbers it).
      `("a64.push.lr" "fe0f1ff8"         '((push lr)))                ; str x30, [sp,#-16]!
      `("a64.pop.lr"  "fe0741f8"         '((pop lr)))                 ; ldr x30, [sp],#16
      `("a64.grpfn"  "fe0f1ff8040000940000038bfe0741f8c0035fd6e00303aac0035fd6"   ; the nested-call shape: save lr; bl sib; work; restore lr; ret; sib: ...
                      '((push lr) (call sib) (add r0 r0 r3) (pop lr) (ret) (label sib) (mov r0 r3) (ret)))
      ;; the OVF-anchor idiom -- mov to/from sp MUST be `lea` (add #0), not the ORR-based `mov` (which
      ;; reads/writes XZR for reg 31, never sp). set anchor = sp ; deopt abandons frames with sp = anchor.
      `("a64.anchor"  "f7030091"          '((lea r23 sp 0)))           ; mov x23, sp
      `("a64.abandon" "ff020091"          '((lea sp r23 0)))           ; mov sp, x23
      ;; a WHOLE glaze leaf for (\ x (x + 1)) -- the integer leaf ABI end to end: g/Ip/Hp/Sp kept in
      ;; x0..x3, acc=x4, temp=x5. fetch+fixnum-guard Sp[0]; untag; +1 with overflow->deopt; putfix;
      ;; store Sp[0]; Continue (Ip+=16; br [Ip]); deopt tail-applies interp (value[1] at [Ip+8]).
      `("a64.glaze.leaf"
         "640040f99f0040f26001005484fc4193840400b106010054840004abc6000054840040b2640000f921400091250040f9a0001fd6250440f9e10305aa250040f9a0001fd6"
         '((ld r4 r3 0) (test r4 1) (br eq deopt) (sar r4 1) (adds r4 r4 1) (br vs deopt)
           (adds r4 r4 r4) (br vs deopt) (or r4 r4 1) (st r3 0 r4) (add r1 r1 16) (ld r5 r1 0) (jmpr r5)
           (label deopt) (ld r5 r1 8) (mov r1 r5) (ld r5 r1 0) (jmpr r5)))
      ;; scalar-double FP (the float glaze lane): f0..f15 -> d0..d15. SSE-named IR, two-address, mapped to
      ;; the arm64 scalar-double form. each golden checked vs llvm-objdump (mnemonics in the trailing comment).
      `("a64.cvttsd2si" "0400789e"        '((cvttsd2si r4 f0)))          ; fcvtzs x4, d0 (toward zero)
      `("a64.cvtsi2sd" "8000629e"          '((cvtsi2sd f0 r4)))          ; scvtf d0, x4
      `("a64.movqxr"   "8100679e"          '((movqxr f1 r4)))            ; fmov  d1, x4  (gpr bits -> fpr)
      `("a64.movqrx"   "0400669e"          '((movqrx r4 f0)))            ; fmov  x4, d0  (fpr -> gpr bits)
      `("a64.movsd"    "2040601e"          '((movsd f0 f1)))             ; fmov  d0, d1
      `("a64.addsd"    "0028611e"          '((addsd f0 f1)))             ; fadd  d0, d0, d1
      `("a64.subsd"    "2138601e"          '((subsd f1 f0)))             ; fsub  d1, d1, d0
      `("a64.mulsd"    "0008611e"          '((mulsd f0 f1)))             ; fmul  d0, d0, d1
      `("a64.divsd"    "2118601e"          '((divsd f1 f0)))             ; fdiv  d1, d1, d0
      `("a64.ucomisd"  "0020611e"          '((ucomisd f0 f1)))           ; fcmp  d0, d1
      `("a64.stsd"     "400400fd"          '((stsd r2 8 f0)))            ; str   d0, [x2, #8]
      `("a64.ldsd"     "610040fd"          '((ldsd f1 r3 0)))            ; ldr   d1, [x3]
      `("a64.cvtss2sd" "20c0221e"          '((cvtss2sd f0 f1)))          ; fcvt  d0, s1  (single -> double)
      `("a64.cvtsd2ss" "0140621e"          '((cvtsd2ss f1 f0)))          ; fcvt  s1, d0  (double -> single)
      `("a64.stss"     "400800bd"          '((stss r2 8 f0)))            ; str   s0, [x2, #8]
      `("a64.ldss"     "610040bd"          '((ldss f1 r3 0)))            ; ldr   s1, [x3]
      `("a64.xorpd"    "841c242e"          '((xorpd f4 f4)))             ; eor   v4.8b, v4.8b, v4.8b  (zero a double)
      ;; the sized family, checked vs llvm-objdump
      `("a64.ld1"      "80809f38"          '((ld1 r0 r4 -8)))            ; ldursb x0, [x4, #-8]
      `("a64.ld2"      "e3238079"          '((ld2 r3 sp 16)))            ; ldrsh  x3, [sp, #16]
      `("a64.ld4"      "87c099b8"          '((ld4 r7 r4 -100)))          ; ldursw x7, [x4, #-100]
      `("a64.st1"      "80801f38"          '((st1 r4 -8 r0)))            ; sturb  w0, [x4, #-8]
      `("a64.st2"      "e3230079"          '((st2 sp 16 r3)))            ; strh   w3, [sp, #16]
      `("a64.st4"      "87c019b8"          '((st4 r4 -100 r7)))          ; stur   w7, [x4, #-100]
      `("a64.sx1"      "001c4093"          '((sx1 r0)))                  ; sxtb x0, w0
      `("a64.sx2"      "e73c4093"          '((sx2 r7)))                  ; sxth x7, w7
      `("a64.sx4"      "637c4093"          '((sx4 r3)))                  ; sxtw x3, w3
      `("a64.ldu1"     "80204039"          '((ldu1 r0 r4 8)))            ; ldrb w0, [x4, #8] (zero-extends)
      `("a64.ldu2"     "83204079"          '((ldu2 r3 r4 16)))           ; ldrh w3, [x4, #16]
      `("a64.ldu4"     "870840b9"          '((ldu4 r7 r4 8)))            ; ldr w7, [x4, #8] (zero-extends to x7)
      `("a64.zx1"      "001c40d3"          '((zx1 r0)))                  ; ubfx x0, x0, #0, #8  (uxtb)
      `("a64.zx2"      "633c40d3"          '((zx2 r3)))                  ; ubfx x3, x3, #0, #16 (uxth)
      `("a64.zx4"      "e77c40d3"          '((zx4 r7)))                  ; ubfx x7, x7, #0, #32 (uxtw)
      `("a64.udiv"     "6308c59a"          '((udiv r3 r3 r5)))           ; udiv x3, x3, x5
      `("a64.urem"     "6208c59a438c059b"  '((urem r3 r3 r5 r2)))        ; udiv x2,x3,x5 ; msub x3,x2,x5,x3
      `("a64.shlv"     "0020c19a"          '((shlv r0 r1)))              ; lsl x0, x0, x1
      `("a64.shrv"     "e724c19a"          '((shrv r7 r1)))              ; lsr x7, x7, x1
      `("a64.sarv"     "6328c19a"          '((sarv r3 r1)))              ; asr x3, x3, x1
      ;; the cc-rung lanes (mooncc -t arm64): lean; the 24-bit add/sub split; the FAR memory
      ;; lane (movz x16 + add/sub off the base); fp LDUR/STUR + far (frame slots run negative);
      ;; f16+ registers; cmp's cmn flip; p/np as vs/vc (FCMP parks unordered in V). llvm-mc-verified.
      `("a64.lean"     "808000d1"          '((lean r0 r4 -32)))          ; sub x0, x4, #32 (lea's alias)
      `("a64.addi.24"  "2040409100100091"  '((add r0 r1 65540)))         ; add x0,x1,#16,lsl#12 ; add x0,x0,#4
      `("a64.far.ld"   "902580d2300010cb000240f9" '((ld r0 r1 -300)))    ; movz x16,#300 ; sub x16,x1,x16 ; ldr x0,[x16]
      `("a64.stur.d"   "40801ffc"          '((stsd r2 -8 f0)))           ; stur d0, [x2, #-8]
      `("a64.ldur.d"   "83005ffc"          '((ldsd f3 r4 -16)))          ; ldur d3, [x4, #-16]
      `("a64.far.fld"  "902580d2300010cb000240fd" '((ldsd f0 r1 -300)))  ; movz x16,#300 ; sub x16,x1,x16 ; ldr d0,[x16]
      `("a64.freg.hi"  "500400fd"          '((stsd r2 8 f16)))           ; str d16, [x2, #8] (the narrow scratch)
      `("a64.cmn"      "1f1c00b1"          '((cmp r0 -7)))               ; cmn x0, #7 (negative imm flips)
      `("a64.set.np"   "e0679f9a"          '((set np r0)))               ; cset x0, vc (np aliases vc)
      `("a64.la"       "000000900040009107000090e740009141424300"
                        '((la r0 x) (la r7 x) (label x) (raw (65 66 67 0)))))  ; adrp+add pair: x at +16, page delta 0, lo12 16
   _ (foldl (\ _ t (chk2 (at t 0) (at t 1) 'arm64 (at t 2))) 0 a64tests)

   ;; -- the thumb2 backend (crew/holo/thumb2.l): the SAME neutral IR, a THIRD target -- ARMv7E-M
   ;; (Cortex-M7, the Teensy 4.1 / Playdate device CPU). MIXED 16/32-bit little-endian; a 32-bit
   ;; word lays hi-halfword-first (le16(hw1)++le16(hw2)). every golden was disassembled by
   ;; `llvm-mc --disassemble --triple=thumbv7em-none-eabi` (mnemonics in the trailing comment); the
   ;; branch-offset scrambling (S/J1/J2/I1/I2) was cross-checked over forward/backward spans across
   ;; the imm11 boundary. --
   t2tests `(
      `("t2.li"      "40f20500"          '((li r0 5)))                 ; movw r0, #5
      `("t2.movt"    "45f27861c1f23421"  '((li r1 305419896)))         ; 0x12345678: movw + movt
      `("t2.li.neg"  "4ff6ff72cff6ff72"  '((li r2 -1)))                ; movw #0xffff ; movt #0xffff
      `("t2.mov"     "4fea0300"          '((mov r0 r3)))               ; mov.w r0, r3
      `("t2.add"     "01eb0200"          '((add r0 r1 r2)))            ; add.w r0, r1, r2
      `("t2.sub"     "a5eb0604"          '((sub r4 r5 r6)))            ; sub.w r4, r5, r6
      `("t2.and"     "01ea0200"          '((and r0 r1 r2)))            ; and.w r0, r1, r2
      `("t2.or"      "41ea0200"          '((or  r0 r1 r2)))            ; orr.w r0, r1, r2
      `("t2.xor"     "88ea0907"          '((xor r7 r8 r9)))            ; eor.w r7, r8, r9
      `("t2.addi"    "01f26400"          '((add r0 r1 100)))           ; addw r0, r1, #100
      `("t2.subi"    "a1f26400"          '((sub r0 r1 100)))           ; subw r0, r1, #100
      `("t2.addneg"  "a1f20800"          '((add r0 r1 -8)))            ; negative imm flips add->subw
      `("t2.cmp"     "b0eb010f"          '((cmp r0 r1)))               ; cmp.w r0, r1
      `("t2.cmpi"    "b0f12a0f"          '((cmp r0 42)))               ; cmp.w r0, #42
      `("t2.mul"     "01fb02f0"          '((imul r0 r1 r2)))           ; mul r0, r1, r2
      `("t2.ldr"     "d1f80800"          '((ld r0 r1 8)))              ; ldr.w r0, [r1, #8]
      `("t2.str"     "c1f80800"          '((st r1 8 r0)))              ; str.w r0, [r1, #8]
      `("t2.ldr.neg" "51f8040c"          '((ld r0 r1 -4)))            ; ldr r0, [r1, #-4] (T4)
      `("t2.ld1"     "91f90300"          '((ld1 r0 r1 3)))             ; ldrsb.w r0, [r1, #3]
      `("t2.st1"     "81f80300"          '((st1 r1 3 r0)))             ; strb.w r0, [r1, #3]
      `("t2.lea"     "01f20800"          '((lea r0 r1 8)))             ; addw r0, r1, #8
      `("t2.lea.mov" "dd46"              '((lea sp fp 0)))             ; mov sp, r11 -- lea #0 to sp rides the sp-legal 16-bit MOV
      `("t2.push"    "4df8044d"          '((push r4)))                 ; str r4, [sp, #-4]!
      `("t2.pop"     "5df8044b"          '((pop r4)))                  ; ldr r4, [sp], #4
      `("t2.sx1"     "4ffa80f0"          '((sx1 r0)))                  ; sxtb.w r0, r0
      `("t2.sx2"     "0ffa83f3"          '((sx2 r3)))                  ; sxth.w r3, r3
      `("t2.zx1"     "5ffa80f0"          '((zx1 r0)))                  ; uxtb.w r0, r0
      `("t2.zx2"     "1ffa87f7"          '((zx2 r7)))                  ; uxth.w r7, r7
      `("t2.sx4"     ""                  '((sx4 r0)))                  ; NO-OP: an int is already the full 32-bit word
      `("t2.neg"     "c0f10000"          '((neg r0)))                  ; rsb.w r0, r0, #0
      `("t2.not"     "6fea0000"          '((not r0)))                  ; mvn.w r0, r0
      `("t2.inc"     "00f20100"          '((inc r0)))                  ; addw r0, r0, #1
      `("t2.dec"     "a0f20100"          '((dec r0)))                  ; subw r0, r0, #1
      `("t2.shl"     "4feac000"          '((shl r0 3)))                ; lsl.w r0, r0, #3
      `("t2.shr"     "4fead000"          '((shr r0 3)))                ; lsr.w r0, r0, #3
      `("t2.sar"     "4feae000"          '((sar r0 3)))                ; asr.w r0, r0, #3
      `("t2.shlv"    "00fa01f0"          '((shlv r0 r1)))              ; lsl.w r0, r0, r1 (count in r1)
      `("t2.sdiv"    "91fbf2f0"          '((div r0 r1 r2)))            ; sdiv r0, r1, r2
      `("t2.udiv"    "b1fbf2f0"          '((udiv r0 r1 r2)))           ; udiv r0, r1, r2
      `("t2.test"    "10ea010f"          '((test r0 r1)))              ; tst.w r0, r1
      `("t2.jmpr"    "1847"              '((jmpr r3)))                 ; bx r3
      `("t2.callr"   "9847"              '((callr r3)))                ; blx r3
      `("t2.ret"     "7047"              '((ret)))                     ; bx lr
      `("t2.sys"     "00df"              '((sys)))                     ; svc #0
      `("t2.nop"     "00bf"              '((nop)))                     ; nop
      `("t2.set"     "40f2000008bf40f20100" '((set eq r0)))            ; movw r0,#0 ; it eq ; movweq r0,#1
      ;; branches: T4 (jmp), T3 conditional (br), T1 (call). the pc-relative offset is a bitfield
      ;; scrambled across S/J1/J2 -- the fixup lays the whole 32-bit word in `patch`.
      `("t2.jmp.fwd" "00f001b800bf" '((jmp l) (nop) (label l)))        ; b.w  -> +2 (past the nop)
      `("t2.jmp.bwd" "00bffff7fdbf" '((label l) (nop) (jmp l)))        ; b.w  -> -6 (back to l)
      `("t2.br.fwd"  "00f0018000bf" '((br eq l) (nop) (label l)))      ; beq.w -> +2
      `("t2.call.fwd" "00f001f800bf" '((call l) (nop) (label l)))      ; bl    -> +2
      ;; a whole trivial leaf end to end: r0 = r0 + r1 ; return.
      `("t2.leaf"    "00eb01007047" '((add r0 r0 r1) (ret)))           ; add.w r0,r0,r1 ; bx lr
      ;; a counted loop (the small-loop battery): the forward beq reaches `done`, the backward bne
      ;; reaches `top`, both fixed 32-bit T3 landing byte-exact on their labels.
      `("t2.loop"    "01eb0000b0eb020f00f0068001f20101b1eb020f7ff4f6af7047"
                      '((add r0 r1 r0) (label top) (cmp r0 r2) (br eq done)
                        (add r1 r1 1) (cmp r1 r2) (br ne top) (label done) (ret)))
      ;; la: the MOVW/MOVT absolute pair (movw16/movt16 fixups, always both) -- x at +16, so
      ;; movw #16 ; movt #0, twice (r0 then r7). the thumb2 answer to a64's adrp+add.
      `("t2.la"      "40f21000c0f2000040f21007c0f2000741424300"
                      '((la r0 x) (la r7 x) (label x) (raw (65 66 67 0))))
      ;; the 64-bit pair lanes (moon rung 3): the carry ALU family, the widening multiply
      ;; family, clz, and a variable shift whose count is NOT r1 (T32 takes any count reg).
      `("t2.adc"     "43eb0101"          '((adc r1 r3 r1)))            ; adc.w r1, r3, r1
      `("t2.adcs"    "53eb0303"          '((adcs r3 r3 r3)))           ; adcs.w r3, r3, r3
      `("t2.sbc"     "63eb0101"          '((sbc r1 r3 r1)))            ; sbc.w r1, r3, r1
      `("t2.sbcs"    "75eb0107"          '((sbcs r7 r5 r1)))           ; sbcs.w r7, r5, r1
      `("t2.ors"     "5cea020c"          '((ors r12 r12 r2)))          ; orrs.w r12, r12, r2
      `("t2.umull"   "a2fb000c"          '((umull r0 r12 r2 r0)))      ; umull r0, r12, r2, r0
      `("t2.mla"     "02fb0133"          '((mla r3 r2 r1 r3)))         ; mla r3, r2, r1, r3
      `("t2.clz"     "b1fa81f0"          '((clz r0 r1)))               ; clz r0, r1
      `("t2.shlv.r2" "01fa02f1"          '((shlv r1 r2)))              ; lsl.w r1, r1, r2
      ;; indexed addressing: ADD.W shifted (leax), LDR/LDRB register-offset (ldx/ldxb at disp 0),
      ;; and the disp!=0 synthesis off the dest.
      `("t2.leax"    "01eb8200"          '((leax r0 r1 r2 4 0)))       ; add.w r0, r1, r2, lsl #2
      `("t2.leax.d"  "04ebcc0909f20409"  '((leax r9 r4 r12 8 4)))      ; add.w r9,r4,r12,lsl#3 ; addw r9,r9,#4
      `("t2.ldx"     "51f82200"          '((ldx r0 r1 r2 4 0)))        ; ldr.w r0, [r1, r2, lsl #2]
      `("t2.ldxb"    "11f80250"          '((ldxb r5 r1 r2 1 0)))       ; ldrb.w r5, [r1, r2]
      `("t2.ldx.d"   "00eb0300d0f80400"  '((ldx r0 r0 r3 1 4)))        ; disp!=0: add.w r0,r0,r3 ; ldr.w r0,[r0,#4]
      ;; the scalar-double VFP lane (moon rung 4): f0..f15 -> d0..d15, S[2n] the single views,
      ;; movqxr/movqrx move a gp PAIR (named reg = lo, +1 = hi). llvm-mc (eabihf, +vfp4d16).
      `("t2.addsd"   "30ee010b"          '((addsd f0 f1)))             ; vadd.f64 d0, d0, d1
      `("t2.subsd"   "32ee432b"          '((subsd f2 f3)))             ; vsub.f64 d2, d2, d3
      `("t2.mulsd"   "20ee010b"          '((mulsd f0 f1)))             ; vmul.f64 d0, d0, d1
      `("t2.divsd"   "80ee010b"          '((divsd f0 f1)))             ; vdiv.f64 d0, d0, d1
      `("t2.movsd"   "b0ee443b"          '((movsd f3 f4)))             ; vmov.f64 d3, d4
      `("t2.ucomisd" "b4ee410bf1ee10fa"  '((ucomisd f0 f1)))           ; vcmp.f64 ; vmrs APSR_nzcv
      `("t2.cvtsi2sd" "02ee100ab8eec22b" '((cvtsi2sd f2 r0)))          ; vmov s4, r0 ; vcvt.f64.s32 d2, s4
      `("t2.cvtui2sd" "02ee100ab8ee422b" '((cvtui2sd f2 r0)))          ; vmov s4, r0 ; vcvt.f64.u32 d2, s4
      `("t2.cvttsd2si" "bdeec1fb1fee100a" '((cvttsd2si r0 f1)))        ; vcvt.s32.f64 s30, d1 ; vmov r0, s30
      `("t2.cvttsd2ui" "bceec1fb1fee100a" '((cvttsd2ui r0 f1)))        ; vcvt.u32.f64 s30, d1 ; vmov r0, s30
      `("t2.movqxr"  "41ec150b"          '((movqxr f5 r0)))            ; vmov d5, r0, r1 (the pair in)
      `("t2.movqrx"  "53ec152b"          '((movqrx r2 f5)))            ; vmov r2, r3, d5 (the pair out)
      `("t2.ldsd"    "94ed021b"          '((ldsd f1 r4 8)))            ; vldr d1, [r4, #8]
      `("t2.ldsd.n"  "14ed041b"          '((ldsd f1 r4 -16)))          ; vldr d1, [r4, #-16]
      `("t2.stsd"    "80ed002b"          '((stsd r0 0 f2)))            ; vstr d2, [r0]
      `("t2.ldss"    "94ed011a"          '((ldss f1 r4 4)))            ; vldr s2, [r4, #4]
      `("t2.stss"    "04ed021a"          '((stss r4 -8 f1)))           ; vstr s2, [r4, #-8]
      `("t2.cvtss2sd" "b7eec11a"         '((cvtss2sd f1 f1)))          ; vcvt.f64.f32 d1, s2
      `("t2.cvtsd2ss" "b7eec11b"         '((cvtsd2ss f1 f1)))          ; vcvt.f32.f64 s2, d1
      `("t2.smull"   "80fb0102"          '((smull r0 r2 r0 r1)))       ; smull r0, r2, r0, r1
      ;; the 64-bit unsigned divide/remainder expansions: dividend r2:r3 / divisor r0:r1 ->
      ;; r0:r1. 64-step restoring loop, fixed narrow branches (bhs +8 / blo +10 / bne -52),
      ;; r4-r7 saved. disassembly-verified whole (llvm-mc); the qemu gate proves the values.
      `("t2.udivll"  "4df8044d4df8045d4df8046d4df8047d40f2000440f2000540f2400612eb020253eb030354eb040455eb050504d2b4eb000775eb010705d3b4eb000475eb010502f20102a6f20106b6f1000fe6d1104619465df8047b5df8046b5df8045b5df8044b"
                      '((udivll)))
      `("t2.uremll"  "4df8044d4df8045d4df8046d4df8047d40f2000440f2000540f2400612eb020253eb030354eb040455eb050504d2b4eb000775eb010705d3b4eb000475eb010502f20102a6f20106b6f1000fe6d1204629465df8047b5df8046b5df8045b5df8044b"
                      '((uremll))))
   _ (foldl (\ _ t (chk2 (at t 0) (at t 1) 'thumb2 (at t 2))) 0 t2tests)

   ;; -- the thumb1 backend (crew/holo/thumb1.l): the SAME neutral IR, a FOURTH target -- ARMv6-M
   ;; (Cortex-M0/M0+, the RP2040 CPU). 16-bit Thumb-1: two-address, low-registers, no MOVW/MOVT
   ;; (so `li` byte-builds the constant), no wide ALU, no IT (so `set` branches over), no hardware
   ;; divide. every golden disassembled/encoded by `llvm-mc --triple=thumbv6m-none-eabi` (mnemonics
   ;; in the trailing comment). branches: B T2 (jmp, +-2KB), Bcc T1 (br, +-256B), BL T1 (call, wide);
   ;; the pc-relative offset (halfword-scaled) was cross-checked forward AND backward. --
   t1tests `(
      `("t1.li"      "0520"              '((li r0 5)))                 ; movs r0, #5
      `("t1.li.neg1" "0020c043"          '((li r0 -1)))                ; movs r0,#0 ; mvns r0,r0
      `("t1.li.negN" "ff22d243"          '((li r2 -256)))              ; movs r2,#255 ; mvns r2,r2 (~255)
      `("t1.li.big"  "1221090234310902563109027831" '((li r1 305419896)))  ; 0x12345678 byte-build (no litpool)
      `("t1.mov"     "1846"              '((mov r0 r3)))               ; mov r0, r3 (hi-form)
      `("t1.add"     "8818"              '((add r0 r1 r2)))            ; adds r0, r1, r2 (3-addr)
      `("t1.sub"     "ac1b"              '((sub r4 r5 r6)))            ; subs r4, r5, r6
      `("t1.addi3"   "481d"              '((add r0 r1 5)))             ; adds r0, r1, #5 (imm3, 3-addr)
      `("t1.addi8"   "6430"              '((add r0 r0 100)))           ; adds r0, #100 (imm8, 2-addr)
      `("t1.subneg"  "0838"              '((add r0 r0 -8)))            ; negative imm flips add->subs #8
      `("t1.and"     "1040"              '((and r0 r0 r2)))            ; ands r0, r2 (2-addr)
      `("t1.or"      "1143"              '((or  r1 r1 r2)))            ; orrs r1, r2
      `("t1.xor"     "6340"              '((xor r3 r3 r4)))            ; eors r3, r4
      `("t1.mul"     "5043"              '((imul r0 r0 r2)))           ; muls r0, r2
      `("t1.cmp"     "8842"              '((cmp r0 r1)))               ; cmp r0, r1
      `("t1.cmpi"    "2a28"              '((cmp r0 42)))               ; cmp r0, #42
      `("t1.test"    "0842"              '((test r0 r1)))              ; tst r0, r1
      `("t1.ld"      "8868"              '((ld r0 r1 8)))              ; ldr r0, [r1, #8]
      `("t1.st"      "8860"              '((st r1 8 r0)))              ; str r0, [r1, #8]
      `("t1.ldsp"    "0498"              '((ld r0 sp 16)))             ; ldr r0, [sp, #16] (T2)
      `("t1.ldb"     "c878"              '((ldu1 r0 r1 3)))            ; ldrb r0, [r1, #3]
      `("t1.ldh"     "8888"              '((ldu2 r0 r1 4)))            ; ldrh r0, [r1, #4]
      `("t1.ld1"     "c87840b2"          '((ld1 r0 r1 3)))             ; ldrb + sxtb (no signed-imm load on v6-M)
      `("t1.ld2"     "888800b2"          '((ld2 r0 r1 4)))             ; ldrh + sxth
      `("t1.stb"     "c870"              '((st1 r1 3 r0)))             ; strb r0, [r1, #3]
      `("t1.sx1"     "40b2"              '((sx1 r0)))                  ; sxtb r0, r0
      `("t1.sx2"     "1bb2"              '((sx2 r3)))                  ; sxth r3, r3
      `("t1.zx1"     "c0b2"              '((zx1 r0)))                  ; uxtb r0, r0
      `("t1.zx2"     "bfb2"              '((zx2 r7)))                  ; uxth r7, r7
      `("t1.sx4"     ""                  '((sx4 r0)))                  ; NO-OP: an int is already the full word
      `("t1.neg"     "4042"              '((neg r0)))                  ; rsbs r0, r0, #0
      `("t1.not"     "c043"              '((not r0)))                  ; mvns r0, r0
      `("t1.inc"     "401c"              '((inc r0)))                  ; adds r0, r0, #1
      `("t1.dec"     "401e"              '((dec r0)))                  ; subs r0, r0, #1
      `("t1.shl"     "c000"              '((shl r0 3)))                ; lsls r0, r0, #3
      `("t1.shr"     "c008"              '((shr r0 3)))                ; lsrs r0, r0, #3
      `("t1.sar"     "c010"              '((sar r0 3)))                ; asrs r0, r0, #3
      `("t1.shlv"    "8840"              '((shlv r0 r1)))              ; lsls r0, r0, r1 (count in r1)
      `("t1.shrv"    "c840"              '((shrv r0 r1)))              ; lsrs r0, r0, r1
      `("t1.sarv"    "0841"              '((sarv r0 r1)))              ; asrs r0, r0, r1
      `("t1.shrv.r2" "d040"              '((shrv r0 r2)))              ; lsrs r0, r0, r2 (any low count reg)
      ;; the carry family (the pair kit): ADCS commutes on d==b; SBCS d==b borrows a saved scratch
      ;; (push/pop/hi-MOV never touch the carry). the f4 logicals commute on d==b the same way.
      `("t1.adc"     "5941"              '((adc r1 r3 r1)))            ; adcs r1, r3 (d==b commutes)
      `("t1.sbc"     "1a468a41"          '((sbc r2 r3 r1)))            ; mov r2,r3 ; sbcs r2,r1
      `("t1.sbc.db"  "01b408461946814101bc" '((sbc r1 r3 r1)))         ; push{r0};mov r0,r1;mov r1,r3;sbcs r1,r0;pop{r0}
      `("t1.ors"     "0843"              '((ors r0 r0 r1)))            ; orrs r0, r1
      `("t1.and.db"  "1040"              '((and r0 r2 r0)))            ; ands r0, r2 (d==b commutes)
      `("t1.lea0"    "bd46"              '((lea sp fp 0)))             ; mov sp, r7 (fp=r7 on v6-M; lea #0 rides the sp-legal MOV)
      `("t1.leasp"   "04a8"              '((lea r0 sp 16)))            ; add r0, sp, #16
      `("t1.leaa3"   "081d"              '((lea r0 r1 4)))             ; adds r0, r1, #4
      `("t1.allocsp" "84b0"              '((lea sp sp -16)))           ; sub sp, sp, #16 (frame alloc)
      ;; indexed address (leax) + the fused indexed loads (ldx/ldxb). scale>1 shifts index; scale 1 is a
      ;; bare ADDS. the dest may alias base or index -- d==base with scale>1 borrows a saved low temp (r2).
      `("t1.leax"    "90000818"          '((leax r0 r1 r2 4 0)))       ; lsls r0,r2,#2 ; adds r0,r1,r0
      `("t1.leax.s1" "6319"              '((leax r3 r4 r5 1 0)))       ; scale 1: adds r3,r4,r5 outright
      `("t1.leax.base" "04b48a00801804bc" '((leax r0 r0 r1 4 0)))     ; d==base: push r2;lsls r2,r1,#2;adds r0,r0,r2;pop r2
      `("t1.leax.disp" "900008180830"    '((leax r0 r1 r2 4 8)))       ; ... ; adds r0,#8 (trailing disp)
      `("t1.ldx"     "900008180068"      '((ldx r0 r1 r2 4 0)))        ; addr synth ; ldr r0,[r0]
      `("t1.ldxb"    "88180078"          '((ldxb r0 r1 r2 1 0)))       ; adds r0,r1,r2 ; ldrb r0,[r0]
      `("t1.push"    "10b4"              '((push r4)))                 ; push {r4}
      `("t1.pushlr"  "00b5"              '((push lr)))                 ; push {lr}
      `("t1.pop"     "10bc"              '((pop r4)))                  ; pop {r4}
      `("t1.poppc"   "00bd"              '((pop pc)))                  ; pop {pc}
      `("t1.set"     "01d0002000e00120"  '((set eq r0)))              ; beq .one ; movs r0,#0 ; b .end ; .one: movs r0,#1 (branch FIRST: movs sets N/Z on v6-M)
      ;; the far/hi memory lane: out-of-reach imm5 offsets and hi registers synthesize the address.
      `("t1.ld.far"  "802008440068"      '((ld r0 r1 128)))           ; movs r0,#128 ; add r0,r1 ; ldr r0,[r0]
      `("t1.ld.hi"   "01b4082020440068844601bc" '((ld r12 r4 8)))     ; push{r0};movs r0,#8;add r0,r4;ldr r0,[r0];mov r12,r0;pop{r0}
      `("t1.st.far"  "04b4c8220a44106004bc" '((st r1 200 r0)))         ; push{r2};movs r2,#200;add r2,r1;str r0,[r2];pop{r2}
      `("t1.jmpr"    "1847"              '((jmpr r3)))                 ; bx r3
      `("t1.callr"   "9847"              '((callr r3)))                ; blx r3
      `("t1.ret"     "7047"              '((ret)))                     ; bx lr
      `("t1.sys"     "00df"              '((sys)))                     ; svc #0
      `("t1.nop"     "00bf"              '((nop)))                     ; nop
      `("t1.trap"    "00be"              '((trap)))                    ; bkpt #0
      ;; branches: every LABEL branch is WIDE (BL, +-16MB; lr is dead mid-frame) -- the short
      ;; B/Bcc can't cross a big function. br = the inverted-Bcc hop over the BL.
      `("t1.jmp.fwd" "00f001f800bf"      '((jmp l) (nop) (label l)))   ; bl .+2 -> past the nop
      `("t1.jmp.bwd" "00bffff7fdff"      '((label l) (nop) (jmp l)))   ; bl .-6 -> back to l
      `("t1.br.fwd"  "01d100f001f800bf"  '((br eq l) (nop) (label l))) ; bne .+4 (hop) ; bl l
      `("t1.br.bwd"  "00bf01d0fff7fcff"  '((label l) (nop) (br ne l))) ; beq .+4 (hop) ; bl l
      `("t1.call.fwd" "00f001f800bf"     '((call l) (nop) (label l)))  ; bl #2
      ;; a whole trivial leaf end to end: r0 = r0 + r1 ; return.
      `("t1.leaf"    "40187047"          '((add r0 r0 r1) (ret)))      ; adds r0,r0,r1 ; bx lr
      ;; div/rem: v6-M has no hardware divide -> the inline restoring shift-subtract blob (t1-udivmod)
      ;; plus, for the signed pair, the abs-and-refix sleeve. fixed self-contained branches (blo/bne).
      `("t1.udiv" "0022202349005241824201d3121a491c5b1ef7d1"
                  '((udiv r1 r1 r0)))                                  ; the 32-step unsigned core
      `("t1.urem" "0022202349005241824201d3121a491c5b1ef7d11100"
                  '((urem r1 r1 r0 r3)))                               ; core ; movs r1,r2 (remainder)
      `("t1.div"  "ca17c3175140891a5840c01a5a4004b40022202349005241824201d3121a491c5b1ef7d108bc5940c91a"
                  '((div r1 r1 r0)))                                   ; |a|,|b| ; qsign=sa^sb ; core ; refix
      `("t1.rem"  "ca17c3175140891a5840c01a04b40022202349005241824201d3121a491c5b1ef7d108bc11005940c91a"
                  '((rem r1 r1 r0 r3))))                               ; |a|,|b| ; core ; sign=sa ; refix
   _ (foldl (\ _ t (chk2 (at t 0) (at t 1) 'thumb1 (at t 2))) 0 t1tests)

   ;; -- summary; nonzero exit on any miss, so `make`-style gates see the failure. --
   np (peep pass 0 0) nf (peep fail 0 0)
   _ (sayln ("crew/holo/holotest: " + ((show np) + (" passed, " + ((show nf) + " failed\n")))))
   _ (? (0 = nf) 0 (quit 1)))
