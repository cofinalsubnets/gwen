; port/virt/mkstart.l -- lay start.o for the qemu-virt (riscv64) port: the one
; piece of the image that is not C, and even it is holo IR, not foreign asm.
; the board's mask ROM jumps to the DRAM base (0x80000000) with everything at
; reset defaults -- sp is garbage, the FPU is OFF (mstatus.FS = 0), mtvec
; points nowhere. the driver's crt0 sits at that address (the -Ttext trick in
; the Makefile) and its first act is `call __ai_start` -- so this object
; overrides __ai_start STRONG (the same weak seam nolibc uses) and does the
; three things C cannot: pin sp, open the FPU, aim mtvec at a fault tail that
; hands mcause/mepc to main.c's fault_report (exit 98 -- loud, never a silent
; trap loop). the CSR instructions ride the 'raw escape (holo carries no Zicsr
; rows -- these four words are the only ones the tree needs).
;
; runs after crew/holo/{elf,obj}.l in a cat with the riscv backend spliced
; into the sealed holo module (the mksys-riscv shape; port/virt/Makefile).
(: ;; csr rd rs1: the Zicsr I-form (opcode 0x73) -- f3 2 = csrrs, 1 = csrrw
   (csrw f3 rd rs1 csr) ((csr << 20) | ((rs1 << 15) | ((f3 << 12) | ((rd << 7) | 0x73))))
   (mkstart dst)
    (: fsen (le32 (csrw 2 0 5 768))     ; csrrs x0, mstatus, t0 -- FS on (0x300)
       mtv  (le32 (csrw 1 0 5 773))     ; csrrw x0, mtvec, t0   -- direct mode (0x305)
       mcz  (le32 (csrw 2 10 0 834))    ; csrrs a0, mcause, x0  (0x342)
       mez  (le32 (csrw 2 11 0 833))    ; csrrs a1, mepc, x0    (0x341)
       ir `(`('label '__ai_start)
            `('li 'sp 2181038080)       ; 0x82000000 -- tops out under the pool
            `('li 'r8 24576)            ; t0 = 0x6000: mstatus.FS = Dirty
            `('raw fsen)                ; the FPU opens before any fld/fsd
            `('la 'r8 '__ai_trap)
            `('raw mtv)
            `('call 'main)
            `('label 'spin) `('jmp 'spin)
            ;; the machine-trap tail: a fresh stack (the faulting one may BE
            ;; the fault), cause and pc into the argument file, then C.
            `('label '__ai_trap)
            `('li 'sp 2181038080)
            `('raw mcz)
            `('raw mez)
            `('call 'fault_report)
            `('label 'spin2) `('jmp 'spin2))
       (write-bytes dst (objelf 'riscv64 ir () '("__ai_start") () () () () () () ()))))
