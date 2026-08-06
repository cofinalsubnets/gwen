; port/teensy41/mkboot.l -- lay boot.o for the Teensy 4.1 (i.MX RT1062): the four
; ROM-facing blocks and the handful of instructions C cannot say. port/mps2/mkstart.l's
; shape, with a boot ROM to satisfy instead of a bare vector table.
;
; the ROM reads three structures at FIXED flash offsets before any of our code runs --
; the FlexSPI config block at 0x000, the image vector table at 0x1000, the boot data
; right after it -- so their addresses are the contract and teensy41.lds pins them by
; section name. every word below is byte-for-byte what first silicon booted with
; (2026-07; the README carries the wrong-offset lore).
;
; ⚠ a vector is an ADDRESS CARRYING THE THUMB BIT, and obj.l sets bit 0 on every FUNC
; symbol -- so `cstartup` here takes addend 0 where the old .S wrote a bare symbol and
; leaned on .thumb_func. R_ARM_ABS32 is (S + A) | T either way.
;
; ⚠ sp is not a register MOVW/MOVT can name (UNPREDICTABLE, same law as MOV.W), so the
; stack pointer arrives through r0 -- `la` into a general register, then the 16-bit
; hi-register mov. `ldr sp,=__stack_top__` was the .S spelling of exactly that.
;
; runs after crew/holo/{elf,obj}.l in a cat with the thumb2 backend spliced into the
; sealed holo module (the mkboot2 shape; port/teensy41/Makefile drives it).
(: (mkboot dst)
   (: (zeros n) (? (n < 1) () (0 >< (zeros (n - 1))))
      (wsym l) `(`('fix 4 'abs32 l 0))         ; a word the linker fills: (S + A) | T

      ;; -- 1. the FlexSPI configuration block at flash 0x000 (RT1060 RM 9.6.3.1) ------
      ;; how the ROM is to talk to the QSPI NOR. a 512-byte block of mostly zero with
      ;; seven live fields, so the RM's OFFSETS are the table and the gaps are computed:
      ;; deviceType reads at 0x44 because the row says 0x44, not because 52 zeros were
      ;; counted correctly above it. that is the whole difference between this and .org.
      fcfb-size 512
      fcfb-words '((0x00 0x42464346)    ; "FCFB" tag
                   (0x04 0x56010000)    ; version V1.0
                   (0x0C 0x00020101)    ; readSampleClkSrc=1, csHold=1, csSetup=2
                   (0x44 0x00030401)    ; deviceType=1 (NOR), pads=4 (quad), 60 MHz
                   (0x50 0x00800000)    ; sflashA1Size = 8 MB (Teensy 4.1)
                   (0x80 0x0A1804EB)    ; LUT[0]: CMD 0xEB (1 pad), RADDR 24 (4 pads)
                   (0x84 0x26043206))   ; LUT[1]: DUMMY 6 cycles, READ (4 pads)
      (fcfb-lay ws pos)
         (? (two? ws)
            (: w <ws  o (at w 0)
               _ (? (o < pos) (scare 'fcfb-overlap o) 0)
               ((zeros (o - pos)) + (le32 (at w 1)) + (fcfb-lay >ws (o + 4))))
            (zeros (fcfb-size - pos)))

      ;; -- 2. the image vector table at flash 0x1000, and the boot data after it ------
      ;; the ROM validates the header and jumps to entry. boot_data.length is how much
      ;; of flash the ROM treats as the image -- teensy41.lds sums the sections into
      ;; __image_size__, the one number here that only the link can know.
      ivt ((le32 0x432000D1)                   ; tag 0xD1, len 0x0020, ver 0x43
            + (wsym 'cstartup)                 ; entry
            + (le32 0) + (le32 0)              ; rsv1, dcd
            + (wsym 'boot_data) + (wsym 'image_vector_table)
            + (le32 0) + (le32 0))             ; csf, rsv2
      bdata ((le32 0x60000000) + (wsym '__image_size__) + (le32 0))

      ;; -- 3. the ARM vector table (VTOR points here once we run; the ROM boots via
      ;; the IVT, so word 0's SP is advisory). every fault lands somewhere that speaks.
      ;; ⚠ DebugMon is isr_hardfault too: with DEMCR.MON_EN a bkpt (a __builtin_trap,
      ;; a contract guard) arrives HERE, and it must REPORT rather than lock up mute.
      vecs ((wsym '__stack_top__) + (wsym 'cstartup)
             + (wsym 'default_handler) + (wsym 'isr_hardfault)     ; NMI, HardFault
             + (wsym 'default_handler) + (wsym 'default_handler)   ; MemManage, BusFault
             + (wsym 'default_handler)                             ; UsageFault
             + (zeros 16)                                          ; 7..10 reserved
             + (wsym 'default_handler)                             ; SVCall
             + (wsym 'isr_hardfault)                               ; DebugMon
             + (le32 0)                                            ; 13 reserved
             + (wsym 'default_handler) + (wsym 'default_handler))  ; PendSV, SysTick

      ;; -- 4. crt0: the ROM jumps here with a ROM-provided SP ------------------------
      startup `(`('label 'cstartup)
                `('la 'r0 '__stack_top__) `('mov 'sp 'r0)
                `('jmp 'cmain))

      ;; -- 5. the instructions C cannot say ------------------------------------------
      ;; HardFault: hand the active stack's exception frame to hardfault_report (C).
      ;; EXC_RETURN bit 2 says which SP was active -- clear means MSP. the old .S asked
      ;; with `ite eq` + mrseq/mrsne; a branch pair says the same thing without an IT
      ;; block, and TST takes no immediate here (thumb-test is register-only), so the 4
      ;; rides r1 -- free, since the hardware stacked r0-r3 on the way in.
      ;; ⚠ the reporter runs on a FRESH stack: if the fault WAS a stack overflow (or sp
      ;; is garbage), pushing its frame on the old sp double-faults into lockup and the
      ;; report never reaches the wire.
      text `(`('label 'isr_hardfault)
             `('li 'r1 4) `('test 'lr 'r1) `('br 'eq 'hf-msp)
             `('mrs 'r0 'psp) `('jmp 'hf-frame)
             `('label 'hf-msp) `('mrs 'r0 'msp)
             `('label 'hf-frame)
             `('la 'r1 '__stack_top__) `('mov 'sp 'r1)
             `('jmp 'hardfault_report)
             `('label 'default_handler) `('wfi) `('jmp 'default_handler)
             ;; the helpers teensy41.c calls where it would otherwise want inline asm
             `('label 'arm_dsb_isb) `('dsb 'sy) `('isb) `('ret)
             `('label 'arm_dsb) `('dsb 'sy) `('ret)
             `('label 'arm_wfi) `('wfi) `('ret)
             `('label 'arm_bkpt) `('brk 0) `('ret))

      ;; ALLOC-only for the four data blocks (2), ALLOC|EXEC for the two code ones (6);
      ;; PROGBITS throughout -- the ROM is handed these bytes, zeros included.
      (write-bytes dst
       (objsecs32 'thumb2
        `(`(".flashconfig" 1 2 4 `(`('raw (fcfb-lay fcfb-words 0))))
          `(".ivt"         1 2 4 `(`('label 'image_vector_table) `('raw ivt)))
          `(".bootdata"    1 2 4 `(`('label 'boot_data) `('raw bdata)))
          `(".vectors"     1 2 4 `(`('label 'vectors) `('raw vecs)))
          `(".startup"     1 6 4 startup)
          `(".text"        1 6 4 text))
        '("cstartup" "isr_hardfault" "default_handler"
          "arm_dsb_isb" "arm_dsb" "arm_wfi" "arm_bkpt")
        '("image_vector_table" "boot_data" "vectors") ()))))
