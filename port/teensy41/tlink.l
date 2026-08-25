; port/teensy41/tlink.l -- the Teensy 4.1 link, ours. holo's ldbare32
; (crew/holo/link.l) in place of arm-none-eabi-ld + teensy41.lds. cat it after
; crew/holo/{elf,obj,link}.l and run the result:
;
;   love <cat> OUT OBJ.o..
;
; the i.MX RT1062 boots XIP from the FlexSPI NOR at 0x60000000, and the ROM reads
; three structures at FIXED flash offsets before any of our code runs: the config
; block at 0x000 and the image vector table at 0x1000, boot data behind it. those
; addresses are the contract, so the map states them -- a named lane is the board's
; word, and their flags (ALLOC-only) would otherwise file them with rodata, behind
; the code, where the ROM does not look.
;
; ⚠ the 0x1000 gap between the two `at` runs is FILE bytes: the runs fuse into one
; segment and the linker pads across, which is what makes flash offsets and flash
; addresses the same number -- the whole reason the .hex verify can read the FCFB
; tag at 0 and the IVT at 0x1000 by seeking.
;
; RAM is the dedicated 512 KB OCRAM2 at 0x20200000, mapped at reset independent of
; the FlexRAM (ITCM/DTCM) split -- so nothing has to reconfigure FlexRAM to have
; working memory. .data is STORED in flash and RUNS there: the `copy` run, the
; script's `> OCRAM2 AT> FLASH`, and cstartup moves it across off the brackets
; ldbare32 pins.
(: ocram2-top 0x20280000                    ; 0x20200000 + 512 KB
   ; presence out of band: an unreadable file and an EMPTY one are the same value,
   ; and an empty .o here would link a firmware with a silent hole in it.
   (tread p) (: r (uread p) (? (two? r) [p <>r] (scare 'tlink-open p)))
   (tar? p) (: n (tally p) (&& (2 < n) ((snip p (n - 2) n) = ".a")))
   (: as >argv
      out <as ins >as
      objs (filter (\ p (! (tar? p))) ins)
      ars (filter tar? ins)
      _ (? (two? objs) 0 (scare 'tlink-no-objects out))
      set (map tread objs)
      lib (set + (ldarch set (map tread ars) 'thumb2))
      runs [['at 0x60000000 (intern ".flashconfig")]
            ['at 0x60001000 (intern ".ivt") (intern ".bootdata") (intern ".vectors")
                            'text 'rodata 'image]
            ['copy 0x20200000 'data 'nifs 'bss]]
      _ (ld-write out (ldbare32 'thumb2 runs lib [["__stack_top__" ocram2-top]]))
      0))
