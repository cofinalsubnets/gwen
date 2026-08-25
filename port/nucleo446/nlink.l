; port/nucleo446/nlink.l -- the Nucleo-F446RE link, ours. holo's ldbare32
; (crew/holo/link.l) in place of arm-none-eabi-ld + nucleo446.lds. cat it after
; crew/holo/{elf,obj,link}.l and run the result:
;
;   love <cat> OUT OBJ.o..
;
; what the linker script carried is the map below and nothing else. the F446 has
; two regions and the boot needs three facts about them: code and literals live in
; the 512 KB flash at 0x08000000, RAM is the contiguous 128 KB SRAM at 0x20000000,
; and .data is STORED in flash but RUNS in SRAM -- which is the `copy` run, the
; script's `> SRAM AT> FLASH`. cstartup moves it across off the brackets ldbare32
; pins (__data_load__ / __data_start__ / __data_end__, __bss_start__/__bss_end__).
;
; ⚠ .vectors is named FIRST and by hand. the core fetches its initial SP and reset
; vector from flash base before it runs anything, so the table's address is a fact
; about the silicon -- but the section's own flags say ALLOC-only, which would file
; it with rodata, after the code. flags are what an object can say; where the board
; looks is not something an object knows. so the map says it.
;
; ⚠ and .startup is NOT named: it rides text by its flags, and that is right --
; cstartup is reached through the vector table's reset word, an address rather than
; a position, so nothing depends on it leading. e_entry is a courtesy for objdump.
;
; __stack_top__ is the board's too: the top of SRAM, which no object can know and
; the link has no memory map to ask.
(: sram-top 0x20020000                      ; 0x20000000 + 128 KB
   ; presence out of band: an unreadable file and an EMPTY one are the same value,
   ; and an empty .o here would link a firmware with a silent hole in it.
   (nread p) (: r (uread p) (? (two? r) [p <>r] (scare 'nlink-open p)))
   (nar? p) (: n (tally p) (&& (2 < n) ((snip p (n - 2) n) = ".a")))
   (: as >argv
      out <as ins >as
      objs (filter (\ p (! (nar? p))) ins)
      ars (filter nar? ins)
      _ (? (two? objs) 0 (scare 'nlink-no-objects out))
      ; the soft-double set: mooncc's thumb2sp lane calls __aeabi_* for f64, and
      ; gcc's cortex-m4 hard-float multilib libgcc is where those live. named on the
      ; line and pulled by need through the ranlib index, so the .a is a LIBRARY this
      ; link reads and not a tool it runs.
      set (map nread objs)
      lib (set + (ldarch set (map nread ars) 'thumb2sp))
      runs [['at 0x08000000 (intern ".vectors") 'text 'rodata]
            ['copy 0x20000000 'data 'bss]]
      _ (ld-write out (ldbare32 'thumb2sp runs lib [["__stack_top__" sram-top]]))
      0))
