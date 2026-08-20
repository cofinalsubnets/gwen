; free/uefi/mkefi.l -- lay the firmware seam and bind the UEFI loader:
; port/virt/mkstart.l's sibling, the one piece of the boot that is not C, and
; even it is holo IR. runs after crew/holo/{elf,obj,link,pe}.l in a
; (use 'holo) cat; (mkboot out arch objs..) links mooncc's loader objects into
; the PE32+ that goes on the ESP as BOOTX64.EFI / BOOTAA64.EFI.
;
; three thunks in one object, laid FIRST so the PE entry is the first text byte.
; what they have to say differs by arch because the CALLING CONVENTION does:
; UEFI on x86-64 is ms_abi and the loader C is sysv, so both directions need a
; shuffle; UEFI on aarch64 is AAPCS64, which is what the C already speaks, so
; the two call thunks are pure tail jumps and only efi_go carries weight.
;   efiboot -- firmware enters here.
;   efi_call -- every UEFI service goes back OUT through this.
;   efi_go(tables, entry) -- the instructions C cannot spell: interrupts off,
;     our tables in, jump to the kernel. never returns.
(use 'kore)   ; the u-floor (uread), off the module the cat's own u.l registered

;; -- x86-64: ms_abi in, sysv out -------------------------------------------
;; rsi/rdi are ms callee-saved and sysv scratch, so they are the only registers
;; to save around the sysv call (the loader C keeps off the floats, so xmm6..15
;; never move; sub 8 squares the stack for sysv). register names are holo's
;; file: r1=rcx r2=rdx r5=rsi r6=rdi r7=r8 r8=r9.
(: efi-ir-x64
    '((label efiboot)
      (push r5)                   ; rsi
      (push r6)                   ; rdi
      (sub sp sp 8)
      (mov r6 r1)                 ; rdi = ImageHandle
      (mov r5 r2)                 ; rsi = SystemTable*
      (call efi_main)
      (add sp sp 8)
      (pop r6)
      (pop r5)
      (ret)                       ; rax = the EFI status, back to firmware
      (label efi_call)
      (mov r0 r6)                 ; rax = fn
      (sub sp sp 56)              ; shadow 32 + arg5 slot + 16-alignment
      (st sp 32 r8)               ; e (sysv r9) -> the stack arg
      (mov r8 r7)                 ; ms r9 = d (sysv r8)
      (mov r7 r1)                 ; ms r8 = c (sysv rcx)
      (mov r1 r5)                 ; ms rcx = a (sysv rsi); b rides rdx as-is
      (callr r0)
      (add sp sp 56)
      (ret)
      ;; efi_go(pml4, entry): the loader's own code stays mapped through the
      ;; switch (the new tables identity-map the low 4G), and the current stack
      ;; rides along -- firmware-typed memory kboot never hands the heap.
      ;; ⚠ EFER.NXE first: firmware is not obliged to have set it, and our
      ;; tables NX the hhdm entry -- bit 63 is RESERVED while NXE is clear, so
      ;; the first walk after the cr3 load would fault instead of honouring it.
      ;; rdmsr/wrmsr touch only eax/edx/ecx, so rdi and rsi ride through.
      (label efi_go)
      (raw (250))                 ; cli
      (raw (185 128 0 0 192))     ; mov $0xC0000080, %ecx -- EFER
      (raw (15 50))               ; rdmsr
      (raw (13 0 8 0 0))          ; or $0x800, %eax -- NXE
      (raw (15 48))               ; wrmsr
      (raw (15 34 223))           ; mov cr3, rdi
      (jmpr r5))                  ; jmp rsi -- kmain, never back

;; -- aarch64: AAPCS64 both ways ---------------------------------------------
;; x0..x5 carry (fn a b c d e) in and (a b c d e) out, so efi_call is a shuffle
;; and a TAIL jump: fn returns straight to efi_call's caller with LR untouched.
;; efiboot is the same trick with no shuffle at all -- UEFI hands x0/x1 exactly
;; where efi_main(handle, st) wants them.
   efi-ir-arm64
    '((label efiboot)
      (jmp efi_main)              ; x0/x1 already right; LR rides through
      (label efi_call)
      (mov r6 r0)                 ; x6 = fn, then slide a..e down one register
      (mov r0 r1) (mov r1 r2) (mov r2 r3) (mov r3 r4) (mov r4 r5)
      (jmpr r6)
      ;; efi_go(tables, entry): x0 is the TTBR0 root and TTBR1's is the page
      ;; after it -- loader.c lays l0_lo and l0_hi adjacent for exactly this.
      ;; ⚠ the MMU is already ON here (firmware's identity map), so this is a
      ;; SWITCH, not a bring-up: both maps identity the code we are running, so
      ;; the fetch after the write lands in the same place either way, and the
      ;; TLBI + ISB is what makes the new one authoritative.
      (label efi_go)
      (msri daifset 15)           ; firmware stops servicing at ExitBootServices
      (dsb sy)
      (li r2 0xff00)              ; MAIR: attr0 Device-nGnRnE, attr1 Normal WB
      (msr mair_el1 r2)
      (li r2 0x2B5103510)         ; TCR: 48-bit VA both halves, 4K, 40-bit PA
      (msr tcr_el1 r2)
      (msr ttbr0_el1 r0)
      (add r2 r0 4096)
      (msr ttbr1_el1 r2)
      (dsb ish)
      (tlbi vmalle1 zr)
      (dsb ish)
      (isb)
      (jmpr r1))                  ; the kernel's entry, never back

   efi-exports '("efi_call" "efi_go")
   (efi-tgt arch) (? (arch = "aarch64") 'arm64 'x64)
   (efi-ir arch)  (? (arch = "aarch64") efi-ir-arm64 efi-ir-x64)
   (mkboot out arch objs)
    (: srcs (map (\ f (: r (uread f)
                         (? (two? r) [f <>r]
                            (: _ (say err ("mkefi: cannot open " + f)) (quit 1)))))
                 objs)
       (pelink (efi-tgt arch)
               (link ["efi0" (string (objelf (efi-tgt arch) (efi-ir arch)
                                             () efi-exports () () () () () () ()))]
                     srcs)
               out)))
