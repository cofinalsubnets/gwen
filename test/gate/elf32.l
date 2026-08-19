; test/gate/elf32.l -- build a Linux ARM executable with holo's ELF32 writer and let a real
; LOADER judge it. no assembler, no linker, no libc: elf32-at lays the 84-byte header and the
; thumb backend lays the code, so the only foreign thing in the lane is the cpu (qemu-arm).
; the two fields worth a gate are the ones a loader refuses or faults on rather than warns
; about -- an Elf32_Phdr keeps p_flags SEVENTH where an Elf64_Phdr keeps it second, and
; e_entry's bit 0 is what enters Thumb state. write(1,msg,n) then exit(42), EABI: the call
; number rides r7, the arguments r0..r2, and `svc #0` is holo's `sys`.
; run: cat holo.l <backend>.l elf.l elf32.l, then (elf32-write '<tgt> "<path>").
; ⚠ holo rides module 'holo, so the cat's own names are not ambient here: this file has
; to ASK for them. Without the (use), the writer answers `;; missing elf32` -- which is
; what a gate cat catting a module's files looks like from the outside.
(use 'holo)
(: (elf32-write tgt path)
   (: msg "love-asm speaks arm32 linux\n"
      (prog a) `(`('li 'r0 1) `('li 'r1 a) `('li 'r2 (tally msg)) `('li 'r7 4) `('sys)
                 `('li 'r0 42) `('li 'r7 1) `('sys))
      e (elf32 tgt prog (map msg (jot (tally msg))))
      _ (write-bytes path (peep e 'bytes 0))
      (flush out)))
