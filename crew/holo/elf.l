; elf -- a minimal static-executable LINKER: it wraps assembled bytes in a Linux ELF so the kernel
; will load and run them. one PT_LOAD segment maps the whole file (headers included) at a fixed base;
; the entry point is the first byte of code. it is the smallest thing that turns the assembler's
; output into a file you can chmod +x and run -- the literal "probe the binary" proof. it is target-
; aware: pass 'x64 or 'arm64 and it assembles with that backend and stamps the matching e_machine
; (the rest of the ELF is identical across both -- 64-bit LSB ET_EXEC).
;
; `elf64` takes a target and a program-BUILDER (\ dataddr -> IR): the builder is handed the absolute
; address its trailing data will live at, so a program can `li` a pointer to its own string. the
; round trip: assemble once with a placeholder to measure the code (every address here is < 2^31, so
; the immediate-load is the stable short form and the length doesn't move), place the data just past
; it, then assemble for real. load after holo.l + the backend(s).

(module 'holo
(: elf-base 4194304                                            ; 0x400000, the classic ET_EXEC base
   elf-hdr  120                                                ; 64-byte ELF header + 56-byte phdr (catall is in holo.l)
   elf-machines (tabof
        [('x64 >< 62) ('amd64 >< 62) ('arm64 >< 183) ('aarch64 >< 183)
          ('thumb1 >< 40) ('thumb2 >< 40) ('thumb2sp >< 40) ('cortex-m0 >< 40) ('cortex-m7 >< 40) ('arm >< 40)    ; EM_ARM
          ('riscv64 >< 243) ('rv64 >< 243)])                   ; EM_RISCV

   ;; one 56-byte program header. p_paddr is p_vaddr's twin on every hosted target -- the
   ;; kernel lane is where they part (a high virtual address loaded at a low physical one).
   (elf64-phdr ty fl off va pa fsz msz al)
    (catall [(le32 ty) (le32 fl) (le64 off) (le64 va) (le64 pa)
             (le64 fsz) (le64 msz) (le64 al)])

   ;; the 64-byte ELF header + N program headers (each already 56 bytes of elf64-phdr).
   ;; etype = 2 (ET_EXEC, fixed base) or 3 (ET_DYN, a static-PIE the kernel loads at a random
   ;; base -- the mooncc `-pie` path, whose data pointers self-relocate at startup).
   ;; sh is () for a headerless image, or (shoff shnum shstrndx) to point at a section-header
   ;; table -- the symbol table rides there, past p_filesz, mapped by nothing and read by nm/gdb.
   (elf64-hdrn etype entry machine sh phs)
    (: shoff (? (two? sh) <sh 0) shnum (? (two? sh) <>sh 0) shstrx (? (two? sh) <>>sh 0)
       rv? (machine = 243)
      (catall [
        '(127 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0)               ; e_ident: \x7fELF, 64-bit LSB SysV
        (le16 etype) (le16 machine) (le32 1)                   ; e_type (2 EXEC / 3 DYN), e_machine, version 1
        (le64 entry) (le64 64) (le64 shoff)                    ; e_entry, e_phoff=64, e_shoff
        (le32 (? rv? 5 0)) (le16 64) (le16 56)                 ; e_flags (riscv: RVC | FLOAT_ABI_DOUBLE), e_ehsize=64, e_phentsize=56
        (le16 (tally phs)) (le16 (? (two? sh) 64 0)) (le16 shnum) (le16 shstrx)
        (catall phs)]))

   ;; the 120-byte header for a single R+X+W load segment at `base` covering the whole file.
   ;; memsz >= filesz: the excess is .bss, address space the kernel hands over already
   ;; zeroed. an image with no .bss passes filesz for both, and the segment is exactly its file.
   (elf64-hdr etype base entry filesz memsz machine sh)
    (elf64-hdrn etype entry machine sh
                ; globals ride the one segment as labeled raw data (cc stage 3)
                [(elf64-phdr 1 7 0 base base filesz memsz 4096)])

   ;; build a complete executable image for TARGET, loaded at `base`. `mkprog` is (\ dataddr -> IR);
   ;; `data` is the byte list appended after the code. returns a tablet { 'bytes 'entry 'dataddr }.
   ;; (the classic ET_EXEC base 0x400000 suits a Linux userland binary; a qemu-virt bare-metal image
   ;; wants 0x40000000, the start of that board's RAM.)
   (elf64-at base target mkprog data)
      (: machine (peep elf-machines target 0)
         _ (? (machine = 0) (scare 'no-elf-machine target) 0)
         org (base + elf-hdr)                                          ; the vaddr of the first code byte
         probe-len (tally (holo-bytes-at target (mkprog (base + 65536)) org))  ; measure the code (length stable)
         dataddr (base + elf-hdr + probe-len)
         ;; assemble at ORG so labels carry absolute load addresses -- pc-relative fixups are
         ;; unchanged; an abs64 data pointer (cc's static char*/fn-ptr tables) resolves outright.
         code (holo-bytes-at target (mkprog dataddr) org)
         filesz (elf-hdr + tally code + tally data)
         entry (base + elf-hdr)
         img (catall [(elf64-hdr 2 base entry filesz filesz machine ()) code data])
         res (tablet 0)
         _ (pin res 'bytes img) _ (pin res 'entry entry) _ (pin res 'dataddr dataddr)
         res)
   (elf64 target mkprog data) (elf64-at elf-base target mkprog data)

   elf-hdr32 84                                                ; 52-byte ELF32 header + 32-byte phdr

   ;; -- the ELF32 twins. ⚠ NOT one shape at two widths, which is why they are their own
   ;; doors: an Elf32_Phdr keeps p_flags SEVENTH where an Elf64_Phdr keeps it SECOND (64
   ;; bits moved it up to close an alignment hole), so no width knob reaches it. the file
   ;; header and the section header do hold their order, but nothing yet asks either for a
   ;; class-generic door -- when the 32-bit link does, link.l's field table is where it
   ;; comes from. obj.l's objsecs/objsecs32 is the same split for the same reason.

   ;; one 32-byte program header.
   (elf32-phdr ty fl off va pa fsz msz al)
    (catall [(le32 ty) (le32 off) (le32 va) (le32 pa)
             (le32 fsz) (le32 msz) (le32 fl) (le32 al)])

   ;; the 52-byte ELF32 header + N program headers (each already 32 bytes of elf32-phdr).
   ;; ⚠ e_entry carries the THUMB BIT on EM_ARM -- bit 0 is how a loader knows to enter in
   ;; Thumb state, and an M-profile part has no other state to enter. every EM_ARM backend
   ;; here is a Thumb one, so the machine number is the whole test. e_flags says EABI
   ;; version 5, the tag an arm reader checks first (obj.l stamps the same on its .o).
   (elf32-hdrn etype entry machine sh phs)
    (: shoff (? (two? sh) <sh 0) shnum (? (two? sh) <>sh 0) shstrx (? (two? sh) <>>sh 0)
       arm? (machine = 40)
      (catall [
        '(127 69 76 70 1 1 1 0 0 0 0 0 0 0 0 0)               ; e_ident: \x7fELF, 32-bit LSB SysV
        (le16 etype) (le16 machine) (le32 1)                   ; e_type (2 EXEC / 3 DYN), e_machine, version 1
        (le32 (? arm? (entry | 1) entry)) (le32 52) (le32 shoff)    ; e_entry, e_phoff=52, e_shoff
        (le32 (? arm? 83886080 0)) (le16 52) (le16 32)         ; e_flags=EABI_VER5, e_ehsize=52, e_phentsize=32
        (le16 (tally phs)) (le16 (? (two? sh) 40 0)) (le16 shnum) (le16 shstrx)
        (catall phs)]))

   ;; the 84-byte header for a single R+X+W load segment at `base` covering the whole file.
   (elf32-hdr etype base entry filesz memsz machine sh)
    (elf32-hdrn etype entry machine sh
                [(elf32-phdr 1 7 0 base base filesz memsz 4096)])

   ;; elf64-at's twin at 32 bits. `entry` answers the code ADDRESS, even -- the thumb bit
   ;; is the header's business, not an address you can compare or disassemble at.
   (elf32-at base target mkprog data)
      (: machine (peep elf-machines target 0)
         _ (? (machine = 0) (scare 'no-elf-machine target) 0)
         org (base + elf-hdr32)
         probe-len (tally (holo-bytes-at target (mkprog (base + 65536)) org))
         dataddr (base + elf-hdr32 + probe-len)
         code (holo-bytes-at target (mkprog dataddr) org)
         filesz (elf-hdr32 + tally code + tally data)
         img (catall [(elf32-hdr 2 base org filesz filesz machine ()) code data])
         res (tablet 0)
         _ (pin res 'bytes img) _ (pin res 'entry org) _ (pin res 'dataddr dataddr)
         res)
   (elf32 target mkprog data) (elf32-at elf-base target mkprog data)

   ;; write a byte list to a file. the blob rides a CASK, not a string: a cask is the
   ;; byte array and says raw, where a string is text that merely happens to be bytes
   ;; today.
   (write-bytes path bs) (: q (open path "w") _ (say q (cask bs)) (close q)))
) ; module 'holo
