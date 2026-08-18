; the gate for crew/holo/as.l, the real AT&T x86-64 front. each golden below is the exact
; .text hex `/usr/bin/as` emits for the SAME source (assemble, `objcopy -O binary --only-section=.text`,
; hexdump) -- so this file is the frozen record of that byte-for-byte round-trip against the system
; assembler. if a golden drifts, either as.l broke or GAS changed, and either is worth a look.
; regenerate a golden: printf '%s\n' 'SRC' | as --64 -o t.o - && objcopy -O binary -j .text t.o t.bin && xxd -p t.bin
; run: cat crew/holo/{holo,x64,as}.l test/holo/as.l | out/host/love  (nonzero exit on any miss)
;
; SCOPE: the straight-line battery -- register, immediate and memory operands at all four widths,
; the size suffix where a register carries none. branch relaxation (jmp/jcc to a near label) and the
; accumulator short forms (add $imm8,%al -> 04) are GAS optimizations as.l does not do, so they are
; byte-DIFFERENT-but-correct and stay out of this battery (they are the M2 items); external-symbol
; call/lea need the reloc-preserving .o path, so the symbol tests below define their target
; in-snippet where as-hex resolves the rel32 in place.
(use 'holo) ; the cat's wrapped sources just registered the module

(: pass #(0 0) fail #(0 0)
   (bump t)  (pin t 0 $ 1 + peep t 0 0)
   (sayln s) (puts s, flush out)
   (chk-eq name want got)
      (? (want = got) (bump pass)
       (bump fail,
        sayln $ "FAIL " + name + ": want " + show want + " got " + show got + "\n"))
   (chk name want src) (chk-eq name want (as-hex src))

   ;; -- the golden table: (name  expected-hex  AT&T-source). byte-identical to /usr/bin/as. --
   tests '(
      ;; mov: immediate (width from the destination), movabs by range, register-to-register
      ("mov.imm.q"   "48c7c001000000"        "mov $1, %rax")
      ("mov.imm.l"   "bb02000000"            "mov $2, %ebx")
      ("mov.movabs"  "48b8cb04fb711f010000"  "mov $1234567890123, %rax")
      ("mov.rr"      "4889c3"                "mov %rax, %rbx")
      ("mov.rr.ext"  "4d89c1"                "mov %r8, %r9")
      ;; ALU, register source (low + REX-extended, 64- and 32-bit)
      ("add.rr"      "4801ca"                "add %rcx, %rdx")
      ("sub.rr.l"    "29f7"                  "sub %esi, %edi")
      ("and.rr"      "4821c3"                "and %rax, %rbx")
      ("or.rr.l"     "09ca"                  "or %ecx, %edx")
      ("xor.rr.ext"  "4d31d3"                "xor %r10, %r11")
      ("cmp.rr"      "4839c3"                "cmp %rax, %rbx")
      ("test.rr"     "4885c0"                "test %rax, %rax")
      ;; ALU, immediate source (imm8 short form 83/ext, imm32 81/ext to a non-accumulator)
      ("add.imm8"    "4883c005"              "add $5, %rax")
      ("add.imm32"   "4881c380000000"        "add $128, %rbx")
      ("sub.imm32"   "4881ebe8030000"        "sub $1000, %rbx")
      ("and.imm32"   "4881e1ff000000"        "and $255, %rcx")
      ;; unary
      ("neg"         "49f7da"                "neg %r10")
      ("not"         "48f7d0"                "not %rax")
      ("inc.l"       "41ffc3"                "inc %r11d")
      ("dec"         "48ffcb"                "dec %rbx")
      ;; stack
      ("push"        "55"                    "push %rbp")
      ("pop"         "5d"                    "pop %rbp")
      ("push.ext"    "4154"                  "push %r12")
      ;; rip-relative + internal call (target defined in-snippet, rel32 resolved in place)
      ("call.int"    "e800000000c3"          "call target\ntarget:\n ret")
      ("lea.rip"     "488d3500000000c3"      "lea target(%rip), %rsi\ntarget:\n ret")
      ;; nullary
      ("ret"         "c3"                    "ret")
      ("nop"         "90"                    "nop")
      ("syscall"     "0f05"                  "syscall")
      ("leave"       "c9"                    "leave")
      ("cqto"        "4899"                  "cqto")
      ("cdq"         "99"                    "cdq")
      ("cdqe"        "4898"                  "cdqe")
      ("hlt"         "f4"                    "hlt")
      ;; memory, base+displacement -- and the two encodings the base register forces: an rsp/r12
      ;; base needs a SIB to say so, an rbp/r13 base has no disp-less slot and takes a 0 byte
      ("mem.st"       "488903"                "mov %rax, (%rbx)")
      ("mem.ld"       "488b03"                "mov (%rbx), %rax")
      ("mem.disp8"    "48894308"              "mov %rax, 8(%rbx)")
      ("mem.disp8.n"  "488945f8"              "mov %rax, -8(%rbp)")
      ("mem.disp32"   "48898300100000"        "mov %rax, 4096(%rbx)")
      ("mem.rsp"      "48890424"              "mov %rax, (%rsp)")
      ("mem.rsp.d"    "4889442410"            "mov %rax, 16(%rsp)")
      ("mem.r12"      "49890424"              "mov %rax, (%r12)")
      ("mem.r13"      "49894500"              "mov %rax, (%r13)")
      ("mem.ext"      "4d894108"              "mov %r8, 8(%r9)")
      ;; ... at each width: the 66 prefix, the byte opcode, and sil's forced REX
      ("mem.st.l"     "8903"                  "movl %eax, (%rbx)")
      ("mem.st.w"     "668903"                "movw %ax, (%rbx)")
      ("mem.st.b"     "8803"                  "movb %al, (%rbx)")
      ("mem.st.b.sil" "408833"                "movb %sil, (%rbx)")
      ;; indexed: the scale bits, an index in the extended bank, and the base-less form
      ("sib.plain"    "4889040b"              "mov %rax, (%rbx,%rcx)")
      ("sib.scale4"   "4889048b"              "mov %rax, (%rbx,%rcx,4)")
      ("sib.disp8"    "488944cb08"            "mov %rax, 8(%rbx,%rcx,8)")
      ("sib.ext.ix"   "4a894455f8"            "mov %rax, -8(%rbp,%r10,2)")
      ("sib.rsp"      "4889048c"              "mov %rax, (%rsp,%rcx,4)")
      ("sib.ext"      "4b8904ec"              "mov %rax, (%r12,%r13,8)")
      ("sib.nobase"   "4889048d10000000"      "mov %rax, 16(,%rcx,4)")
      ;; immediate to memory -- neither operand carries a width, so the suffix is the only source
      ("mem.imm.q"    "48c70301000000"        "movq $1, (%rbx)")
      ("mem.imm.l"    "c745fc01000000"        "movl $1, -4(%rbp)")
      ("mem.imm.w"    "66c7030100"            "movw $1, (%rbx)")
      ("mem.imm.b"    "c60301"                "movb $1, (%rbx)")
      ;; the ALU group over memory, both directions, and its immediate forms by width
      ("alu.mem.st"   "480103"                "addq %rax, (%rbx)")
      ("alu.mem.ld"   "480303"                "addq (%rbx), %rax")
      ("alu.mem.ld.x" "4c3b23"                "cmpq (%rbx), %r12")
      ("alu.sib.l"    "290453"                "subl %eax, (%rbx,%rdx,2)")
      ("alu.mem.i8"   "48830305"              "addq $5, (%rbx)")
      ("alu.mem.i32"  "488103e8030000"        "addq $1000, (%rbx)")
      ("alu.mem.i.b"  "800305"                "addb $5, (%rbx)")
      ("alu.mem.i.w"  "66830305"              "addw $5, (%rbx)")
      ("alu.sib.imm"  "4881648b08ff000000"    "andq $255, 8(%rbx,%rcx,4)")
      ("test.mem"     "488503"                "testq %rax, (%rbx)")
      ("test.mem.imm" "f70305000000"          "testl $5, (%rbx)")
      ;; unary, stack and lea over memory
      ("neg.mem"      "48f71b"                "negq (%rbx)")
      ("not.mem.l"    "f713"                  "notl (%rbx)")
      ("inc.mem.d"    "48ff4308"              "incq 8(%rbx)")
      ("dec.mem.b"    "fe0b"                  "decb (%rbx)")
      ("push.mem"     "ff33"                  "pushq (%rbx)")
      ("pop.mem"      "8f03"                  "popq (%rbx)")
      ("push.mem.rsp" "ff742408"              "pushq 8(%rsp)")
      ("lea.disp"     "488d4308"              "lea 8(%rbx), %rax")
      ("lea.rsp"      "488d2c24"              "leaq (%rsp), %rbp")
      ("lea.sib"      "498d6cc418"            "leaq 24(%r12,%rax,8), %rbp")
      ;; the narrow register forms the suffix reaches
      ("add.rr.b"     "00c3"                  "add %al, %bl")
      ("add.rr.w"     "6601c3"                "add %ax, %bx")
      ("mov.rr.b"     "88c3"                  "mov %al, %bl")
      ("mov.rr.w"     "6689c3"                "mov %ax, %bx")
      ("mov.rr.b.sil" "4088f0"                "mov %sil, %al")
      ("mov.imm.b"    "b301"                  "movb $1, %bl")
      ("mov.imm.w"    "66bb0100"              "movw $1, %bx")
      ("neg.b"        "f6d8"                  "negb %al")
      ("inc.w"        "66ffc3"                "incw %bx")
      ("push.w"       "6650"                  "pushw %ax")
      ("pop.w.ext"    "66415c"                "popw %r12w")
      ("push.mem.w"   "66ff33"                "pushw (%rbx)")
      ;; indirect call/jmp, through a register or through memory
      ("call.ind.r"   "ffd0"                  "call *%rax")
      ("jmp.ind.r"    "ffe0"                  "jmp *%rax")
      ("call.ind.m"   "ff5308"                "call *8(%rbx)")
      ("jmp.ind.sib"  "ff24cb"                "jmp *(%rbx,%rcx,8)")
      ;; rip-relative past lea, target in-snippet so the rel32 resolves in place
      ("rip.ld"       "488b0500000000c3"      "movq t(%rip), %rax\nt:\n ret")
      ("rip.st"       "48890500000000c3"      "movq %rax, t(%rip)\nt:\n ret")
      ("rip.alu"      "48031d00000000c3"      "addq t(%rip), %rbx\nt:\n ret")
      ("rip.neg"      "48f71d00000000c3"      "negq t(%rip)\nt:\n ret")
      ("rip.push"     "ff3500000000c3"        "pushq t(%rip)\nt:\n ret")
      ("rip.call.ind" "ff1500000000c3"        "call *t(%rip)\nt:\n ret"))

   _ (foldl (\ _ t (chk (at t 0) (at t 1) (at t 2))) 0 tests)
   np (peep pass 0 0) nf (peep fail 0 0)
   _ (sayln $ "test/holo/as: " + show np + " passed, " + show nf + " failed\n")
   _ (? (0 = nf) 0 (quit 1)))
