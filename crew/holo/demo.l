; demo -- build two runnable binaries from the assembler, the proof that the bytes are real machine
; code and not just plausible hex. writes to /tmp and prints how to run each:
;
;   /tmp/love-x64.elf    a Linux x86-64 static executable -- write(1,msg,n); exit(7). run it directly:
;                        chmod +x /tmp/love-x64.elf && /tmp/love-x64.elf ; echo $?
;
;   /tmp/love-arm64.elf  a bare-metal aarch64 image for the qemu `virt` board -- it writes a line to
;                      the pl011 uart at 0x09000000, then spins. run it on an emulated ARM cpu:
;                        qemu-system-aarch64 -M virt -cpu cortex-a57 -nographic -kernel /tmp/love-arm64.elf
;                      (ctrl-a x to quit qemu.)
;
; run: out/host/love -l crew/holo/holo.l -l crew/holo/amd64.l -l crew/holo/arm64.l -l crew/holo/elf.l crew/holo/demo.l

(module 'holo
(: say (\ s (: _ (puts s) (flush out)))

   ;; -- the x86-64 userland program: write the message via the Linux syscall ABI, then exit(7).
   ;; r0=rax(syscall#), r6=rdi(fd/arg0), r5=rsi(arg1), r2=rdx(arg2). --
   xmsg "hi from love-asm (x86-64 linux)\n"
   (x64-prog a) ['(li r0 1) '(li r6 1) ['li 'r5 a] ['li 'r2 (tally xmsg)] '(sys)
                  '(li r0 60) '(li r6 7) '(sys)]
   xe (elf64 'amd64 x64-prog (map xmsg (jot (tally xmsg))))
   _ (write-bytes "/tmp/love-x64.elf" (peep xe 'bytes 0))

   ;; -- the aarch64 bare-metal program: no syscalls (there's no OS), just store each byte to the
   ;; uart data register, then loop. li the uart base once into r1; per char li r2,c ; st r1,0,r2. --
   amsg "hi from love-asm running on emulated aarch64!\n"
   (a-char c) [['li 'r2 c] '(st r1 0 r2)]
   a-prog ([['li 'r1 0x09000000]]
           + foldr (\ c acc (a-char c + acc)) () (map amsg (jot (tally amsg)))
           + '((label hang) (jmp hang)))
   ae (elf64-at 0x40000000 'arm64 (\ a a-prog) ())          ; 0x40000000 = qemu-virt RAM base
   _ (write-bytes "/tmp/love-arm64.elf" (peep ae 'bytes 0))

   _ (say "wrote /tmp/love-x64.elf and /tmp/love-arm64.elf\n")
   _ (say "  x64:   chmod +x /tmp/love-x64.elf && /tmp/love-x64.elf ; echo $?\n")
   _ (say "  arm64: qemu-system-aarch64 -M virt -cpu cortex-a57 -nographic -kernel /tmp/love-arm64.elf\n"))
) ; module 'holo
