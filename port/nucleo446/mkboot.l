; port/nucleo446/mkboot.l -- lay boot.o for the Nucleo-F446RE (STM32F446RE): the
; vector table, crt0, the fault shim and the helpers C cannot say. port/teensy41's
; mkboot.l stripped to the generic Cortex-M shape -- the F446 boots straight from the
; vector table at flash base, so there are no vendor ROM blocks to satisfy.
;
; ⚠ a vector is an ADDRESS CARRYING THE THUMB BIT, and obj.l sets bit 0 on every FUNC
; symbol -- so `cstartup` here takes addend 0 where the old .S wrote a bare symbol and
; leaned on .thumb_func. R_ARM_ABS32 is (S + A) | T either way.
;
; ⚠ sp is not a register MOVW/MOVT can name (UNPREDICTABLE, same law as MOV.W), so the
; stack pointer arrives through a general register and the 16-bit hi-register mov.
;
; the lay says 'thumb2sp, which is thumb2's encoder under another nom (gen softens f64
; there) -- boot code touches no float, so the two are the same bytes here; naming the
; port's own target is what keeps the object's machine agreeing with its neighbours'.
;
; runs after crew/holo/{elf,obj}.l in a cat with the thumb2 backend spliced into the
; sealed holo module (the mkboot2 shape; port/nucleo446/Makefile drives it).
(: (mkboot dst)
   (: (zeros n) (? (n < 1) () (0 >< (zeros (n - 1))))
      (wsym l) `(`('fix 4 'abs32 l 0))         ; a word the linker fills: (S + A) | T

      ;; -- the ARM vector table at flash base ----------------------------------------
      ;; every fault lands somewhere that speaks: the four faults go to the reporter,
      ;; the rest idle. word 0 is the SP the core loads before the first instruction.
      vecs ((wsym '__stack_top__) + (wsym 'cstartup)
             + (wsym 'default_handler)                             ; NMI
             + (wsym 'isr_hardfault) + (wsym 'isr_hardfault)       ; HardFault, MemManage
             + (wsym 'isr_hardfault) + (wsym 'isr_hardfault)       ; BusFault, UsageFault
             + (zeros 16)                                          ; 7..10 reserved
             + (wsym 'default_handler) + (wsym 'default_handler)   ; SVCall, DebugMon
             + (le32 0)                                            ; 13 reserved
             + (wsym 'default_handler) + (wsym 'default_handler))  ; PendSV, SysTick

      ;; -- crt0 ----------------------------------------------------------------------
      startup `(`('label 'cstartup)
                `('la 'r0 '__stack_top__) `('mov 'sp 'r0)
                `('jmp 'cmain))

      ;; -- the instructions C cannot say ----------------------------------------------
      ;; HardFault: hand the active stack's exception frame to hardfault_report (C).
      ;; EXC_RETURN bit 2 says which SP was active -- clear means MSP. the old .S asked
      ;; with `ite eq` + mrseq/mrsne; a branch pair says the same without an IT block,
      ;; and TST takes no immediate here (thumb-test is register-only), so the 4 rides
      ;; r1 -- free, since the hardware stacked r0-r3 on the way in.
      ;; ⚠ the reporter runs on a FRESH stack: if the fault WAS a stack overflow (or sp
      ;; is garbage), pushing its frame on the old sp double-faults into lockup and the
      ;; report never reaches the wire (the teensy41 lesson).
      text `(`('label 'isr_hardfault)
             `('li 'r1 4) `('test 'lr 'r1) `('br 'eq 'hf-msp)
             `('mrs 'r0 'psp) `('jmp 'hf-frame)
             `('label 'hf-msp) `('mrs 'r0 'msp)
             `('label 'hf-frame)
             `('la 'r1 '__stack_top__) `('mov 'sp 'r1)
             `('jmp 'hardfault_report)
             `('label 'default_handler) `('wfi) `('jmp 'default_handler)
             ;; semihosting SYS_EXIT_EXTENDED, the qemu gate's exit door: r1 points at
             ;; a two-word block of (reason, code). the pushes lay it in that order --
             ;; r0's code goes down first so the reason lands under it, at sp.
             ;; ⚠ on silicon a bkpt with no debugger escalates to lockup, so QSMOKE only.
             `('label 'sh_exit)
             `('li 'r1 0x20026)                 ; ADP_Stopped_ApplicationExit
             `('push 'r0) `('push 'r1) `('mov 'r1 'sp)
             `('li 'r0 0x20) `('brk 171)
             `('label 'shx-spin) `('jmp 'shx-spin)
             ;; the helpers nucleo446.c calls where it would otherwise want inline asm
             `('label 'arm_dsb_isb) `('dsb 'sy) `('isb) `('ret)
             `('label 'arm_wfi) `('wfi) `('ret))

      ;; ALLOC-only for the table (2), ALLOC|EXEC for the two code lanes (6).
      (write-bytes dst
       (objsecs32 'thumb2sp
        `(`(".vectors" 1 2 4 `(`('label 'vectors) `('raw vecs)))
          `(".startup" 1 6 4 startup)
          `(".text"    1 6 4 text))
        '("cstartup" "isr_hardfault" "default_handler" "sh_exit" "arm_dsb_isb" "arm_wfi")
        '("vectors") ()))))
