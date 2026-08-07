; port/rp2040/rlink.l -- the Raspberry Pi Pico link, ours. holo's ldbare32
; (crew/holo/link.l) in place of arm-none-eabi-ld + rp2040.lds. cat it after
; crew/holo/{elf,obj,link}.l and run the result:
;
;   love <cat> OUT OBJ.o..
;
; the RP2040 boots XIP from flash at 0x10000000, and the mask ROM reads the first
; 256 bytes as a CHECKSUMMED second stage -- mkboot2.l lays it, CRC and all -- then
; hands control to the vector table right behind it at 0x10000100. both addresses
; are the mask ROM's, so the map names both lanes: their flags cannot say where the
; silicon looks, and .boot2 in particular must be FIRST or the checksum is read off
; the wrong bytes.
;
; the stack is the top of the 264 KB main SRAM; .data is stored in flash and runs in
; SRAM (the `copy` run), which reset_handler moves across off ldbare32's brackets.
(: sram-top 0x20042000                      ; 0x20000000 + 264 KB
   ; presence out of band: an unreadable file and an EMPTY one are the same value,
   ; and an empty .o here would link a firmware with a silent hole in it.
   (rread p) (: r (uread p) (? (two? r) `(p <>r) (scare 'rlink-open p)))
   (rar? p) (: n (tally p) (&& (2 < n) ((snip p (n - 2) n) = ".a")))
   (: as >argv
      out <as ins >as
      objs (filter (\ p (! (rar? p))) ins)
      ars (filter rar? ins)
      _ (? (two? objs) 0 (scare 'rlink-no-objects out))
      ; v6-M has no divide and no FPU, so the __aeabi_* set comes from gcc's cortex-m0
      ; libgcc -- named on the line, pulled by need through the ranlib index.
      set (map rread objs)
      lib (set + (ldarch set (map rread ars) 'thumb1))
      ; flash says R|X outright: .vectors is const to the silicon and still carries the
      ; handler ADDRESSES that make it writable to any compiler (gcc's answer too), and
      ; nothing on the run is written once the linker has resolved them. rp2040.lds
      ; asserted the same thing in a PHDRS block, for the same reason.
      runs `(`('at 0x10000000 '(flags 5) (intern ".boot2"))
             `('at 0x10000100 (intern ".vectors") 'text 'rodata)
             `('copy 0x20000000 'data 'bss))
      _ (ld-write out (ldbare32 'thumb1 runs lib `(`("__stack_top__" sram-top))))
      0))
