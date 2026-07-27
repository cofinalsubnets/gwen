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

(: elf-base 4194304                                            ; 0x400000, the classic ET_EXEC base
   elf-hdr  120                                                ; 64-byte ELF header + 56-byte phdr (catall is in holo.l)
   elf-machines (tabof
        `(('x64 >< 62) ('amd64 >< 62) ('arm64 >< 183) ('aarch64 >< 183)
           ('thumb1 >< 40) ('thumb2 >< 40) ('thumb2sp >< 40) ('cortex-m0 >< 40) ('cortex-m7 >< 40) ('arm >< 40)   ; EM_ARM
           ('riscv64 >< 243) ('rv64 >< 243)))                  ; EM_RISCV

   ;; the 120-byte header for a single R+X load segment at `base` covering the whole file.
   ;; etype = 2 (ET_EXEC, fixed base) or 3 (ET_DYN, a static-PIE the kernel loads at a random
   ;; base -- the mooncc `-pie` path, whose data pointers self-relocate at startup).
   ;; memsz >= filesz: the excess is .bss, address space the kernel hands over already
   ;; zeroed. an image with no .bss passes filesz for both, and the segment is exactly its file.
   ;; sh is () for a headerless image, or (shoff shnum shstrndx) to point at a section-header
   ;; table -- the symbol table rides there, past p_filesz, mapped by nothing and read by nm/gdb.
   (elf64-hdr etype base entry filesz memsz machine sh)
    (: shoff (? (two? sh) <sh 0) shnum (? (two? sh) <>sh 0) shstrx (? (two? sh) <>>sh 0)
      (catall `(
         `(127 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0)              ; e_ident: \x7fELF, 64-bit LSB SysV
         (le16 etype) (le16 machine) (le32 1)                  ; e_type (2 EXEC / 3 DYN), e_machine, version 1
         (le64 entry) (le64 64) (le64 shoff)                   ; e_entry, e_phoff=64, e_shoff
         (le32 0) (le16 64) (le16 56)                          ; e_flags, e_ehsize=64, e_phentsize=56
         (le16 1) (le16 (? (two? sh) 64 0)) (le16 shnum) (le16 shstrx)   ; e_phnum=1, then the section-header trio
         (le32 1) (le32 7) (le64 0)                            ; PT_LOAD, flags RWX -- globals ride the one segment as labeled raw data (cc stage 3); split segments arrive with the .o seam
         (le64 base) (le64 base)                               ; p_vaddr, p_paddr
         (le64 filesz) (le64 memsz) (le64 4096))))             ; p_filesz, p_memsz, p_align

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
         filesz (elf-hdr + (tally code) + (tally data))
         entry (base + elf-hdr)
         img (catall `((elf64-hdr 2 base entry filesz filesz machine ()) code data))
         res (tablet 0)
         _ (pin res 'bytes img) _ (pin res 'entry entry) _ (pin res 'dataddr dataddr)
         res)
   (elf64 target mkprog data) (elf64-at elf-base target mkprog data)

   ;; write a byte list to a file. the blob rides a CASK, not a string: a cask is the
   ;; byte array and says raw, where a string is text that merely happens to be bytes
   ;; today (doc/proto/widen.md -- every ELF used to go out through one).
   (write-bytes path bs) (: q (open path "w") _ (say q (cask bs)) (close q)))
