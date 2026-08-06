; port/mps2/mkstart.l -- lay start.o for the mps2-an500 board: the vector table, the
; FPU gate and the semihosting trampoline. The one piece of this image that is not C,
; and even it is holo IR, not foreign asm -- port/virt/mkstart.l's shape at thumb2,
; port/rp2040/mkboot2.l's for the named section.
;
; qemu -kernel loads the ELF segments straight into RAM (so .data needs no copy and
; fresh RAM is zero, .bss included) and the M7 boots from the vectors at 0: word 0 is
; the initial SP, word 1 the reset entry, then NMI..SysTick. ⚠ every one of those
; ADDRESSES CARRIES THE THUMB BIT, and it is the object's FUNC symbols that carry it
; (obj.l sets bit 0 on every one) -- so the fixups here take addend 0 where the old .S
; wrote `_start+1`. Both land on the same byte, because R_ARM_ABS32 is (S + A) | T with
; bit 0 masked out of S first -- but only ours is right if a linker skips that mask.
;
; ⚠ CPACR (0xE000ED88) must open CP10/CP11 before any VFP instruction or the core
; locks up ("can't escalate 3 to HardFault") -- love.c is full of doubles, so this is
; the first thing _start does and the barriers after it are not decoration.
;
; runs after crew/holo/{elf,obj}.l in a cat with the thumb2 backend spliced into the
; sealed holo module (the mkboot2 shape; port/mps2/Makefile drives it).
(: (mkstart dst)
   (: cpacr 0xE000ED88  cp1011 0xF00000
      ;; the table: SP, reset, then 14 fault vectors that all land in _fault. any
      ;; fault exits 98 -- loud, never a lockup with no word about why. a relocated
      ;; word is a `fix` ITEM riding the `raw` escape, which is the one door that takes
      ;; bytes and fixups mixed -- lay knows `label` and `align` and lowers all else.
      (w32 l) `(`('fix 4 'abs32 l 0))
      vec `(`('raw ((w32 '__stack_top__) + (w32 '_start)
                    + (foldl (\ a _ (a + (w32 '_fault))) () (jot 14)))))
      txt `(`('label '_start)
            `('li 'r0 cpacr) `('ld4 'r1 'r0 0)
            `('li 'r2 cp1011) `('or 'r1 'r1 'r2) `('st4 'r0 0 'r1)
            `('dsb 'sy) `('isb)
            `('call 'main)
            `('label 'spin) `('jmp 'spin)          ; main never returns (m7_exit leaves through qemu)
            ;; uintptr_t sh_call(uintptr_t op, uintptr_t arg) -- AAPCS already placed
            ;; op/arg in r0/r1, and the trap answers in r0.
            `('label 'sh_call) `('brk 171) `('ret)
            ;; the fault tail: hand the stacked frame to C, which prints pc/lr and exits 98
            `('label '_fault) `('mov 'r0 'sp) `('call 'fault_report)
            `('label 'spin2) `('jmp 'spin2))
      (write-bytes dst (objsecs32 'thumb2 `(`(".vectors" 1 2 4 vec) `(".text" 1 6 4 txt))
                                  '("_start" "sh_call") () ()))))
