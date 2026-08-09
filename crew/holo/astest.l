; astest -- the gate for crew/holo/as.l, the real AT&T x86-64 front. each golden below is the exact
; .text hex `/usr/bin/as` emits for the SAME source (assemble, `objcopy -O binary --only-section=.text`,
; hexdump) -- so this file is the frozen record of that byte-for-byte round-trip against the system
; assembler. if a golden drifts, either as.l broke or GAS changed, and either is worth a look.
; regenerate a golden: printf '%s\n' 'SRC' | as --64 -o t.o - && objcopy -O binary -j .text t.o t.bin && xxd -p t.bin
; run: out/host/love -l crew/holo/holo.l -l crew/holo/x64.l -l crew/holo/as.l crew/holo/astest.l  (exits nonzero on any miss).
;
; SCOPE: the M1 straight-line battery -- suffix-less base mnemonics with the width read off the
; register (as.l takes no AT&T size suffix yet; that is the gcc -S north star). branch relaxation
; (jmp/jcc to a near label) and the accumulator short forms (add $imm32,%rax -> 05) are GAS
; optimizations as.l does not do, so they are byte-DIFFERENT-but-correct and stay out of this battery
; (they are the M2 items); external-symbol call/lea need the reloc-preserving .o path, so the two
; symbol tests below define their target in-snippet where as-hex resolves the rel32 in place.
(: pass (tablet 0) _ (pin pass 0 0)
   fail (tablet 0) _ (pin fail 0 0)
   (bump t)  (pin t 0 (1 + peep t 0 0))
   (sayln s) (: _ (puts s) (flush out))
   (chk-eq name want got)
      (? (want = got) (bump pass)
         (: _ (bump fail)
            (sayln ("FAIL " + (name + (": want " + (show want + (" got " + (show got + "\n")))))))))
   (chk name want src) (chk-eq name want (as-hex src))

   ;; -- the golden table: (name  expected-hex  AT&T-source). byte-identical to /usr/bin/as. --
   tests `(
      ;; mov: immediate (width from the destination), movabs by range, register-to-register
      '("mov.imm.q"   "48c7c001000000"        "mov $1, %rax")
      '("mov.imm.l"   "bb02000000"            "mov $2, %ebx")
      '("mov.movabs"  "48b8cb04fb711f010000"  "mov $1234567890123, %rax")
      '("mov.rr"      "4889c3"                "mov %rax, %rbx")
      '("mov.rr.ext"  "4d89c1"                "mov %r8, %r9")
      ;; ALU, register source (low + REX-extended, 64- and 32-bit)
      '("add.rr"      "4801ca"                "add %rcx, %rdx")
      '("sub.rr.l"    "29f7"                  "sub %esi, %edi")
      '("and.rr"      "4821c3"                "and %rax, %rbx")
      '("or.rr.l"     "09ca"                  "or %ecx, %edx")
      '("xor.rr.ext"  "4d31d3"                "xor %r10, %r11")
      '("cmp.rr"      "4839c3"                "cmp %rax, %rbx")
      '("test.rr"     "4885c0"                "test %rax, %rax")
      ;; ALU, immediate source (imm8 short form 83/ext, imm32 81/ext to a non-accumulator)
      '("add.imm8"    "4883c005"              "add $5, %rax")
      '("add.imm32"   "4881c380000000"        "add $128, %rbx")
      '("sub.imm32"   "4881ebe8030000"        "sub $1000, %rbx")
      '("and.imm32"   "4881e1ff000000"        "and $255, %rcx")
      ;; unary
      '("neg"         "49f7da"                "neg %r10")
      '("not"         "48f7d0"                "not %rax")
      '("inc.l"       "41ffc3"                "inc %r11d")
      '("dec"         "48ffcb"                "dec %rbx")
      ;; stack
      '("push"        "55"                    "push %rbp")
      '("pop"         "5d"                    "pop %rbp")
      '("push.ext"    "4154"                  "push %r12")
      ;; rip-relative + internal call (target defined in-snippet, rel32 resolved in place)
      '("call.int"    "e800000000c3"          "call target\ntarget:\n ret")
      '("lea.rip"     "488d3500000000c3"      "lea target(%rip), %rsi\ntarget:\n ret")
      ;; nullary
      '("ret"         "c3"                    "ret")
      '("nop"         "90"                    "nop")
      '("syscall"     "0f05"                  "syscall")
      '("leave"       "c9"                    "leave")
      '("cqto"        "4899"                  "cqto")
      '("cdq"         "99"                    "cdq")
      '("cdqe"        "4898"                  "cdqe")
      '("hlt"         "f4"                    "hlt"))

   _ (foldl (\ _ t (chk (at t 0) (at t 1) (at t 2))) 0 tests)
   np (peep pass 0 0) nf (peep fail 0 0)
   _ (sayln ("crew/holo/astest: " + (show np + (" passed, " + (show nf + " failed\n")))))
   _ (? (0 = nf) 0 (quit 1)))
