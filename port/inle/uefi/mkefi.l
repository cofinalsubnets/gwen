; port/inle/uefi/mkefi.l -- lay the ms_abi seam and bind the UEFI loader:
; port/virt/mkstart.l's sibling, the one piece of the boot that is not C, and
; even it is holo IR. runs after crew/holo/{elf,obj,link,pe}.l in a
; (use 'holo) cat; (mkboot out objs..) links mooncc's loader objects into the
; PE32+ that goes on the ESP as BOOTX64.EFI.
;
; two thunks in one object, laid FIRST so the PE entry is the first text byte:
;   efiboot -- firmware calls here ms_abi (rcx = ImageHandle, rdx = the
;     SystemTable). rsi/rdi are ms callee-saved and sysv scratch, so they are
;     the only registers to save around the sysv call (the loader C keeps off
;     the floats, so xmm6..15 never move; sub 8 squares the stack for sysv).
;   efi_call -- every UEFI service goes back out through this: sysv
;     (fn a b c d e) -> ms (rcx rdx r8 r9 [rsp+32]) with the 32-byte shadow.
;     register names are holo's file: r1=rcx r2=rdx r5=rsi r6=rdi r7=r8 r8=r9.
(: efi-ir `(`('label 'efiboot)
            `('push 'r5)                ; rsi
            `('push 'r6)                ; rdi
            `('sub 'sp 'sp 8)
            `('mov 'r6 'r1)             ; rdi = ImageHandle
            `('mov 'r5 'r2)             ; rsi = SystemTable*
            `('call 'efi_main)
            `('add 'sp 'sp 8)
            `('pop 'r6)
            `('pop 'r5)
            `('ret)                     ; rax = the EFI status, back to firmware
            `('label 'efi_call)
            `('mov 'r0 'r6)             ; rax = fn
            `('sub 'sp 'sp 56)          ; shadow 32 + arg5 slot + 16-alignment
            `('st 'sp 32 'r8)           ; e (sysv r9) -> the stack arg
            `('mov 'r8 'r7)             ; ms r9 = d (sysv r8)
            `('mov 'r7 'r1)             ; ms r8 = c (sysv rcx)
            `('mov 'r1 'r5)             ; ms rcx = a (sysv rsi); b rides rdx as-is
            `('callr 'r0)
            `('add 'sp 'sp 56)
            `('ret)
            ;; efi_go(pml4, entry) -- the three instructions C cannot spell:
            ;; interrupts off, our page tables in, jump to the kernel's high
            ;; entry. the loader's own code stays mapped through the switch
            ;; (the new tables identity-map the low 4G), and the current stack
            ;; rides along -- firmware-typed memory kboot never hands the heap.
            `('label 'efi_go)
            `('raw `(250))              ; cli
            `('raw `(15 34 223))        ; mov cr3, rdi
            `('jmpr 'r5))               ; jmp rsi -- kmain, never back
   efi-exports '("efi_call" "efi_go")
   (mkboot out objs)
    (: srcs (map (\ f (: r (uread f)
                         (? (two? r) `(f <>r)
                            (: _ (say err ("mkefi: cannot open " + f)) (quit 1)))))
                 objs)
       (pelink (link `("efi0" (string (objelf 'x64 efi-ir () efi-exports () () () () () ())))
                     srcs)
               out)))
