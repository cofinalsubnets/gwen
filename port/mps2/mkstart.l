; port/mps2/mkstart.l -- lay start.o for the mps2-an500 board: the vector table, the
; FPU gate and the semihosting trampoline. The one piece of this image that is not C,
; and even it is holo IR, not foreign asm -- port/virt/mkstart.l's shape at thumb2.
;
; the M7 boots from the vectors at 0: word 0 is the initial SP, word 1 the reset
; entry, then NMI..SysTick. ⚠ so the table LEADS this object's .text and this object
; leads the link -- address 0 is where the core looks, and the core is what places it,
; not a section name a linker script hunts for. it is read-only data sitting in the
; executable lane, which is what address 0 is.
;
; the image is TWO regions: code and literals in SSRAM1 at 0, .data and .bss in
; SSRAM2/3 at 0x20000000 -- and .data is STORED after the code, because a loader
; puts bytes where the segment says and only one segment can own an address. so
; _start copies it across and makes .bss's zeros, off the three brackets only the
; linker can know (__data_load__ / __data_start__ / __data_end__, __bss_start__ /
; __bss_end__ -- crew/holo/link.l's ldbare32 pins them). BYTE loops: a section's
; size is its own business and need not be a multiple of four, and a few thousand
; iterations at boot cost nothing next to getting the tail wrong.
;
; ⚠ every vector is an ADDRESS CARRYING THE THUMB BIT, and it is the object's FUNC
; symbols that carry it (obj.l sets bit 0 on every one) -- so the fixups here take
; addend 0 where the old .S wrote `_start+1`. Both land on the same byte, because
; R_ARM_ABS32 is (S + A) | T with bit 0 masked out of S first -- but only ours is
; right if a linker skips that mask.
;
; ⚠ CPACR (0xE000ED88) must open CP10/CP11 before any VFP instruction or the core
; locks up ("can't escalate 3 to HardFault") -- love.c is full of doubles, so this is
; the first thing _start does and the barriers after it are not decoration.
;
; runs after crew/holo/{elf,obj}.l in a cat with the thumb2 backend spliced into the
; sealed holo module (the mkboot2 shape; port/mps2/Makefile drives it).
(: (mkstart dst)
   (: cpacr 0xE000ED88  cp1011 0xF00000
      ;; the top of SSRAM2/3 (0x20000000 + 4M) -- the C stack grows down from there.
      ;; a board fact, so the board's own lay says it; the link has no memory map.
      stack-top 0x20400000
      ;; the table: SP, reset, then 14 fault vectors that all land in _fault. any
      ;; fault exits 98 -- loud, never a lockup with no word about why. a relocated
      ;; word is a `fix` ITEM riding the `raw` escape, which is the one door that takes
      ;; bytes and fixups mixed -- lay knows `label` and `align` and lowers all else.
      (w32 l) [['fix 4 'abs32 l 0]]
      txt [['raw ((le32 stack-top) + (w32 '_start)
                  + (foldl (\ a _ (a + (w32 '_fault))) () (jot 14)))]
           ['label '_start]
           ['li 'r0 cpacr] ['ld4 'r1 'r0 0]
           ['li 'r2 cp1011] ['or 'r1 'r1 'r2] ['st4 'r0 0 'r1]
           ['dsb 'sy] ['isb]
           ['la 'r0 '__data_load__] ['la 'r1 '__data_start__] ['la 'r2 '__data_end__]
           ['label 'dcopy] ['cmp 'r1 'r2] ['br 'ae 'dzero]
           ['ldu1 'r3 'r0 0] ['st1 'r1 0 'r3]
           ['add 'r0 'r0 1] ['add 'r1 'r1 1] ['jmp 'dcopy]
           ['label 'dzero] ['la 'r0 '__bss_start__] ['la 'r1 '__bss_end__] ['li 'r2 0]
           ['label 'bloop] ['cmp 'r0 'r1] ['br 'ae 'bdone]
           ['st1 'r0 0 'r2] ['add 'r0 'r0 1] ['jmp 'bloop]
           ['label 'bdone]
           ['call 'main]
           ['label 'spin] ['jmp 'spin]             ; main never returns (m7_exit leaves through qemu)
            ;; uintptr_t sh_call(uintptr_t op, uintptr_t arg) -- AAPCS already placed
            ;; op/arg in r0/r1, and the trap answers in r0.
           ['label 'sh_call] ['brk 171] ['ret]
            ;; the fault tail: hand the stacked frame to C, which prints pc/lr and exits 98
           ['label '_fault] ['mov 'r0 'sp] ['call 'fault_report]
           ['label 'spin2] ['jmp 'spin2]]
      (write-bytes dst (objsecs32 'thumb2 [[".text" 1 6 4 txt]]
                                  '("_start" "sh_call") () ()))))
