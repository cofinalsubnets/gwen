; free/mkboot.l -- lay boot.o, the kernel's BRING-UP: the code that runs before there is a C environment to run in. mkvec.l's
; sibling and mksys.l's class -- register-exact leaves C cannot say.
;
;   (lay-boot dst "x86_64")   the PVH note qemu reads to find the 32-bit entry,
;                             the page tables + GDT, the long-mode ladder, khop
;   (lay-boot dst "aarch64")  the translation tables, MAIR/TCR/TTBR, the MMU
;                             switch, the hop into the mapped world, khop
;
; the link is FLAT on both arches (klink.l: vaddr = paddr, bias 0), so a label's
; running address, its linked address, and the address a page table wants to hold
; are ONE number. `la` (x86 rip-relative lea; aarch64 adrp+add) answers it, and a
; field can name it whole -- the PVH note's entry word, the GDT register's base,
; the ljmp's target. `lia` (x86 movabs; aarch64 an inline literal) stays only
; where an absolute is wanted for its own sake: the hop that takes the first
; fetch after the MMU comes on, where a pc-relative branch would ride a broken
; map instead of failing on it, and the stack it lands on.
;
; runs after crew/holo/{elf,obj}.l in a (use 'holo) cat, with the target's
; backend text joined (a frontend bakes holo with the NATIVE backend only);
; free/kernel.mk drives it. register vocabulary is holo's -- x86: r0=rax
; r1=rcx r2=rdx r3=rbx r5=rsi r6=rdi; aarch64: rN = xN.
(:
   (b-rep n x) (? (n < 1) () (x >< (b-rep (n - 1) x)))
   (b-zeros n) (b-rep n 0)

   (b-abs32 lbl ad) `(`('fix 4 'abs32 lbl ad))   ; a 4-byte physical address
   (b-abs64 lbl ad) `(`('fix 8 'abs64 lbl ad))   ; ..and its 8-byte twin
   (b-quad v) `('raw (le64 v))

   ;; -- the x86-32 helper table -------------------------------------------
   ;; holo lays x64, and the PVH stub's first thirty instructions run in 32-bit
   ;; protected mode, where the same opcodes mean different things: 0x41 is
   ;; `inc %ecx` here and a REX prefix there, and ModRM mod=00 rm=101 is an
   ;; absolute disp32 here and RIP-relative there. teaching holo a mode nothing
   ;; else in the tree needs would be the wrong shape, and frozen raw words
   ;; would be unreadable -- so the encodings live here as named byte-list
   ;; functions, each one checked against `llvm-mc -triple=i386 --show-encoding`
   ;; and each carrying its AT&T source. the PVH protocol is frozen, so this is
   ;; write-once code.
   ;; a memory operand off %edi always takes the disp32 form (mod=10), even
   ;; where a disp8 would fit -- one shape, three bytes, no branch on the offset.
   i-cli            '(250)                                   ; fa           cli
   i-xor-ecx        '(49 201)                                ; 31 c9        xor %ecx, %ecx
   i-eax-ecx        '(137 200)                               ; 89 c8        mov %ecx, %eax
   i-st-edi0        '(137 7)                                 ; 89 07        mov %eax, (%edi)
   i-st-edi-idx8    '(137 4 207)                             ; 89 04 cf     mov %eax, (%edi,%ecx,8)
   i-inc-ecx        '(65)                                    ; 41           inc %ecx
   i-rdmsr          '(15 50)                                 ; 0f 32        rdmsr
   i-wrmsr          '(15 48)                                 ; 0f 30        wrmsr
   (i-edi-sym l a)  (191 >< (b-abs32 l a))                   ; bf id        mov $sym, %edi
   (i-eax-sym l a)  (184 >< (b-abs32 l a))                   ; b8 id        mov $sym, %eax
   (i-ecx-imm v)    (185 >< (le32 v))                        ; b9 id        mov $imm, %ecx
   (i-shl-eax n)    `(193 224 n)                             ; c1 e0 ib     shl $n, %eax
   (i-or-eax v)     (13 >< (le32 v))                         ; 0d id        or $imm32, %eax
   (i-or-eax8 v)    `(131 200 v)                             ; 83 c8 ib     or $imm8, %eax
   (i-add-eax v)    (5 >< (le32 v))                          ; 05 id        add $imm32, %eax
   (i-cmp-ecx v)    (`(129 249) + (le32 v))                  ; 81 f9 id     cmp $imm32, %ecx
   (i-jne lbl)      `(15 133 `('fix 4 'rel32 lbl 0))         ; 0f 85 cd     jne rel32
   (i-st-edi d)     (`(137 135) + (le32 d))                  ; 89 87 disp32 mov %eax, d(%edi)
   (i-sti-edi d l a) ((`(199 135) + (le32 d)) + (b-abs32 l a))  ; c7 87 disp32 id  movl $sym, d(%edi)
   (i-stimm-edi d v) ((`(199 135) + (le32 d)) + (le32 v))       ; c7 87 disp32 id  movl $imm, d(%edi)
   (i-lgdt l a)     (`(15 1 21) + (b-abs32 l a))             ; 0f 01 15 id  lgdt sym
   (i-ldcr n)       `(15 32 (192 | (n << 3)))                ; 0f 20 /r     mov %crN, %eax
   (i-stcr n)       `(15 34 (192 | (n << 3)))                ; 0f 22 /r     mov %eax, %crN
   (i-ljmp l a)     ((234 >< (b-abs32 l a)) + '(8 0))        ; ea off32 sel16  ljmp $0x08, $sym
   (i32 bs) `('raw bs)                                       ; one 32-bit instruction, one form

   ;; -- x86_64 ------------------------------------------------------------
   ;; the note `qemu -kernel` reads to find the 32-bit entry. it is an ELF NOTE
   ;; the LOADER consults, not advisory metadata, so link.l places it like
   ;; progbits instead of dropping it (dropping it would strand the kernel).
   x-note
    (`(`('raw ((le32 4) + (le32 4) + (le32 18))))    ; namesz 4, descsz 4, XEN_ELFNOTE_PHYS32_ENTRY
     + `(`('raw '(88 101 110 0)))                    ; "Xen\0"
     + `(`('raw (b-abs32 'pvh32 0))))                ; the PHYSICAL address of the 32-bit entry

   ;; the page tables, zero-filled and part of the file (they are .boot's bytes,
   ;; not .bss -- the loader hands them over whole). one PML4, one PDPT the
   ;; identity and hhdm windows share, four PDs of 2 MiB entries covering
   ;; physical 0..4 GiB. RAM above 4 GiB stays unmapped -- pvh.c drops those
   ;; e820 entries.
   x-boot
    ('((label pml4)) + `(`('raw (b-zeros 4096)))
     + '((label pdpt_id)) + `(`('raw (b-zeros 4096)))
     + '((label pd0)) + `(`('raw (b-zeros 16384)))
     ;; the GDT: a null, a 64-bit code segment at 0x08, a data segment at 0x10.
     + `(`('align 8) `('label 'gdt))
     + `((b-quad 0) (b-quad 0x00209A0000000000) (b-quad 0x0000920000000000))
     + `(`('label 'gdt_end) `('align 8) `('label 'gdtreg))
     + `(`('raw (le16 23)))                          ; gdt_end - gdt - 1
     ;; lgdt runs before paging, so the base it loads must be PHYSICAL
     + `(`('raw (b-abs64 'gdt 0))))

   ;; the ladder. 32-bit protected mode, paging off, ebx -> hvm_start_info.
   x-boot-text
    (`(`('label 'pvh32) (i32 i-cli))
     ;; the four PDs: entry i = (i << 21) | P|RW|PS -- 2048 qwords, phys 0..4 GiB.
     ;; the high dword of each entry stays the zero the loader left there.
     + `((i32 (i-edi-sym 'pd0 0)) (i32 i-xor-ecx) `('label 'pd_loop)
         (i32 i-eax-ecx) (i32 (i-shl-eax 21)) (i32 (i-or-eax 0x83))
         (i32 i-st-edi-idx8) (i32 i-inc-ecx) (i32 (i-cmp-ecx 0x800))
         (i32 (i-jne 'pd_loop)))
     ;; identity + hhdm PDPT: entries 0..3 -> the four PDs
     + `((i32 (i-edi-sym 'pdpt_id 0)) (i32 (i-eax-sym 'pd0 3))
         (i32 i-st-edi0)
         (i32 (i-add-eax 0x1000)) (i32 (i-st-edi 8))
         (i32 (i-add-eax 0x1000)) (i32 (i-st-edi 16))
         (i32 (i-add-eax 0x1000)) (i32 (i-st-edi 24)))
     ;; PML4: [0] identity, [256] the hhdm at 0xffff8000_00000000 -- one PDPT
     ;; described twice, and NX on the second description is the whole reason the
     ;; hhdm exists. one bit at the top of the walk covers every page under it, and
     ;; the hhdm is how the kernel reaches ram (kboot.hhdm), so heap and every page
     ;; it hands out stop being executable without a second table. ⚠ the identity
     ;; window keeps X: the kernel image and the framebuffer are read through it.
     + `((i32 (i-edi-sym 'pml4 0)) (i32 (i-eax-sym 'pdpt_id 3))
         (i32 i-st-edi0) (i32 (i-st-edi (256 * 8)))
         (i32 (i-stimm-edi ((256 * 8) + 4) 0x80000000)))
     ;; the long-mode ladder: GDT, CR4.PAE, CR3, EFER.LME, CR0.PG|PE, far jump
     + `((i32 (i-lgdt 'gdtreg 0))
         (i32 (i-ldcr 4)) (i32 (i-or-eax8 0x20)) (i32 (i-stcr 4))     ; CR4.PAE
         (i32 (i-eax-sym 'pml4 0)) (i32 (i-stcr 3))
         (i32 (i-ecx-imm 0xC0000080)) (i32 i-rdmsr)                 ; EFER
         (i32 (i-or-eax 0x900)) (i32 i-wrmsr)                         ; LME | NXE
         ; ⚠ NXE before PG: with it clear, bit 63 of a table entry is RESERVED, and
         ; the first walk after paging comes on faults instead of honouring the NX.
         (i32 (i-ldcr 0)) (i32 (i-or-eax 0x80000001)) (i32 (i-stcr 0)))   ; PG | PE
     + `((i32 (i-ljmp 'pvh64 0)))
     ;; ..and we are 64-bit. the data selectors, then the hop into the mapped
     ;; world -- absolute on purpose: it is the first fetch after paging, and a
     ;; relative branch would ride a broken map instead of failing on it.
     + `(`('label 'pvh64))
     + `(`('raw '(102 184 16 0))                     ; 66 b8 10 00  mov $0x10, %ax
         `('raw '(142 216))                          ; 8e d8        mov %ax, %ds
         `('raw '(142 192))                          ; 8e c0        mov %ax, %es
         `('raw '(142 208)))                         ; 8e d0        mov %ax, %ss
     + '((lia r0 khop) (jmpr r0)))

   ;; past the hop: a stack, SSE on before ANY C runs (pvh_to_kboot is compiled
   ;; and free to vectorize -- the same dance as the top of kmain, which stays
   ;; for a door that hands over with C already reachable), then C.
   x-text
    '((label khop)
       (lia sp kstack)
       (add sp sp 0x10000)                            ; + kstack_size
       (ldcr r0 0)
       (and r0 r0 -5)                                ; ~(1 << 2): CR0.EM = 0
       (or r0 r0 2)                                  ; CR0.MP = 1
       (stcr 0 r0)
       (ldcr r0 4)
       (or r0 r0 0x600)                               ; CR4.OSFXSR | OSXMMEXCPT
       (stcr 4 r0)
       ;; hvm_start_info (a low physical address) -> arg0. the zx4 is `mov
       ;; %ebx, %edi`: the 32-bit half is all the loader set.
       (mov r6 r3) (zx4 r6)
       (call pvh_to_kboot)
       (call kmain)
       (label x_park)
       (cli) (hlt) (jmp x_park))

   ;; -- aarch64 -----------------------------------------------------------
   ;; the tables mirror x86's shape index for index: one L1 named twice, as the
   ;; identity map under TTBR0 (where the kernel runs) and as the hhdm at
   ;; L0[256] under TTBR1. the first gig is virt's MMIO ground and is laid as a
   ;; real L2 of 2 MiB Device blocks -- arch.c's mmio_map walks TTBR1 expecting
   ;; a TABLE there (a 1 GiB block would read as a bogus L2 pointer) and re-lays
   ;; the two slots it cares about, same shape. RAM (1..4 GiB) rides 1 GiB
   ;; Normal blocks, and the kernel's own gig is one of them.
   a-boot
    ('((label l0_lo)) + `(`('raw (b-zeros 4096)))    ; TTBR0: [0] -> l1_id
     + '((label l0_hi)) + `(`('raw (b-zeros 4096)))  ; TTBR1: [256] hhdm -> l1_id
     + '((label l1_id)) + `(`('raw (b-zeros 4096)))  ; [0] -> l2_dev, [1..3] 1G Normal blocks
     + '((label l2_dev)) + `(`('raw (b-zeros 4096))))   ; 512 x 2M Device blocks over phys 0..1G

   a-gig 0x40000705                                 ; 0x40000000 | 0x705  valid | AttrIndx 1 | SH inner | AF
   ;; PXNTable | UXNTable, bits 59 and 60 of a TABLE descriptor: nothing under
   ;; this entry may be fetched from, at either EL. x86's NX bit, one level up.
   a-xntab 0x1800000000000000
   a-boot-text
    ('((label a64boot)
       ;; the DOOR, one bit, read off the MACHINE and not off who booted us: a
       ;; firmware hand-off enters with the MMU ON, `qemu -kernel` with it off.
       ;; GAS said this with `tbz x9, #0, 1f`; the neutral surface has
       ;; no test-bit-and-branch and no immediate AND, so the bit is masked
       ;; through a register. x9/x10 are dead at the entry.
       (mrs r9 sctlr_el1)
       (li r10 1) (and r10 r9 r10) (cmp r10 0)
       (br eq a64_cold)
       (jmp kmain)                                   ; already mapped: straight into C, nothing to bring up
       (label a64_cold)
       (mov r21 r0)                                  ; x0 is 0 today (probed); a loader that
                                                     ;   passes a dtb pointer is honored at khop
       (li r6 3)                                     ; a table descriptor's low bits: valid | table
                                                     ;   (a LITERAL: this block is quoted, so a name
                                                     ;    here would ride through as a symbol)
       ;; l2_dev: 512 x 2 MiB Device blocks (valid | AF, AttrIndx 0). `la` is
       ;; pc-relative and we are running LOW, so every address written into a
       ;; table below is the physical one, which is what a table must hold.
       (la r0 l2_dev)
       (li r1 0)
       (li r2 0x401)
       (label a64_dev)
       (mov r3 r1) (shl r3 21) (or r3 r3 r2)
       (stx r0 r1 8 0 r3)                            ; str x3, [x0, x1, lsl #3]
       (add r1 r1 1) (cmp r1 512) (br ne a64_dev)
       ;; l1_id: [0] table -> l2_dev, [1..3] 1 GiB Normal blocks
       (la r1 l1_id)
       (or r3 r0 r6) (st r1 0 r3))
     + `(`('li 'r2 a-gig) `('st 'r1 8 'r2)
         `('li 'r2 (a-gig + 0x40000000)) `('st 'r1 16 'r2)
         `('li 'r2 (a-gig + 0x80000000)) `('st 'r1 24 'r2))
     ;; the L0s: the same L1 under both -- identity for the kernel to run in,
     ;; the hhdm for reaching ram by physical address. ⚠ the XN pair rides the
     ;; hhdm descriptor ALONE, which is the whole reason the second window is
     ;; worth its entry: one bit at the top of the walk covers every page under
     ;; it, so the heap and every page it hands out stop being executable
     ;; through the map the kernel reads them by, while the identity walk keeps
     ;; X for the image it runs from. the x86 stub says this at PML4[256].
     + `('(or r3 r1 r6)
         '(la r0 l0_lo) '(st r0 0 r3)
         '(la r2 l0_hi)
         `('li 'r5 a-xntab) '(or r5 r5 r3) '(st r2 2048 r5))   ; [256], and nothing under it runs
     + '(;; MAIR: attr0 Device-nGnRnE (what mmio_attr_index prefers), attr1 Normal WB
         (li r3 0xff00)
         (msr mair_el1 r3)
         ;; TCR: 48-bit VA both halves, 4K granules, WB WA inner-shareable walks,
         ;; 40-bit PA -- T0SZ=16 IRGN0/ORGN0=1 SH0=3 | T1SZ=16 IRGN1/ORGN1=1 SH1=3
         ;; TG1=4K | IPS=40-bit
         (li r3 0x2B5103510)
         (msr tcr_el1 r3)
         (msr ttbr0_el1 r0)
         (msr ttbr1_el1 r2)
         ;; FP/SIMD on before ANY C runs (dtb.c is compiled and free to
         ;; vectorize) -- archinit's own set stays for the MMU-on door.
         (mrs r3 cpacr_el1)
         (li r4 0x300000) (or r3 r3 r4)        ; 3 << 20
         (msr cpacr_el1 r3)
         (dsb ish)
         (tlbi vmalle1 zr)
         (dsb ish)
         (isb)
         (mrs r3 sctlr_el1)
         (li r4 0x1005) (or r3 r3 r4)          ; M | C | I = 1<<0 | 1<<2 | 1<<12
         (msr sctlr_el1 r3)
         (isb)
         ;; into the mapped world. the link is flat, so this lands where the
         ;; pc already is -- but it rides the literal anyway, because it is the
         ;; first fetch after the MMU came on and a pc-relative branch would
         ;; hide a broken identity map instead of failing on it. the align is
         ;; lia's, not decoration: the literal sits at the instruction's own
         ;; address + 8, so the site has to be even-worded for the load.
         (align 8)
         (lia r9 khop)
         (jmpr r9)))

   a-text
    '((label khop)
       (la r9 kstack)                                ; flat: pc-relative IS the linked address
       (li r10 0x10000) (add r9 r9 r10)              ; + kstack_size
       (lea sp r9 0)                                 ; mov sp, x9
       ;; qemu lays the dtb at the RAM base; a loader that passed one in x0 wins.
       ;; GAS said this with csel; the neutral surface says it with a branch.
       (li r1 0x40000000)                             ; RAMB, virt's RAM base
       (cmp r21 0)
       (br ne a64_havedtb)
       (mov r21 r1)
       (label a64_havedtb)
       (mov r0 r21)
       (call dtb_to_kboot)
       (call kmain)
       (label a64_park)
       (wfi)
       (jmp a64_park))

   ;; the boot stack, both arches: kmain never returns, and 64 KiB is what a
   ;; firmware hand-off hands over. .bss so it costs the file nothing.
   b-stack `(`('label 'kstack) `('raw (b-zeros 65536)))

   ;; -- the two objects ---------------------------------------------------
   ;; SHF flags: 2 ALLOC, 3 ALLOC|WRITE, 6 ALLOC|EXEC. types: 1 PROGBITS,
   ;; 7 NOTE, 8 NOBITS. .boot stays PROGBITS on both arches -- its zeros are
   ;; file bytes today and the loader hands the whole span over.
   (lay-boot dst arch) (write-bytes dst (lay-boot-bytes arch))
   (lay-boot-bytes arch)
    (? (arch = "x86_64")
       (objsecs 'x64
                `(`(".note.pvh"  7 2 4    x-note)
                  `(".boot"      1 3 4096 x-boot)
                  `(".boot.text" 1 6 16   x-boot-text)
                  `(".text"      1 6 16   x-text)
                  `(".bss"       8 3 16   b-stack))
                ()                       ; the ELF entry is kmain's; the PVH entry rides the note
                () ()
                '("pvh32" "pvh64" "khop"))
       (arch = "aarch64")
       (objsecs 'arm64
                `(`(".boot"      1 3 4096 a-boot)
                  `(".boot.text" 1 6 16   a-boot-text)
                  `(".text"      1 6 16   a-text)
                  `(".bss"       8 3 16   b-stack))
                '("a64boot")             ; klink.l names it as the ELF entry
                () ()
                '("khop"))
       (scare 'mkboot-arch arch)))
