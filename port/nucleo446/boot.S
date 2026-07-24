@ Nucleo-F446RE boot + the asm-only functions -- the teensy41 boot.S stripped
@ to its generic Cortex-M shape: no vendor ROM blocks (the F446 boots straight
@ from the vector table at flash base), just vectors + crt0 + the HardFault
@ shim + the helpers mooncc's C calls in place of inline asm. sh_exit is the
@ qemu gate's exit door (semihosting SYS_EXIT_EXTENDED); the device build
@ never calls it.
.syntax unified
.cpu cortex-m4
.fpu fpv4-sp-d16
.thumb

@ --- ARM vector table at flash base (the F446 boots from it directly) ------
.section .vectors,"a"
.global vectors
vectors:
  .word __stack_top__           @ 0  initial SP
  .word cstartup                @ 1  reset
  .word default_handler         @ 2  NMI
  .word isr_hardfault           @ 3  HardFault
  .word isr_hardfault           @ 4  MemManage
  .word isr_hardfault           @ 5  BusFault
  .word isr_hardfault           @ 6  UsageFault
  .word 0, 0, 0, 0              @ 7..10 reserved
  .word default_handler         @ 11 SVCall
  .word default_handler         @ 12 DebugMon
  .word 0                       @ 13 reserved
  .word default_handler         @ 14 PendSV
  .word default_handler         @ 15 SysTick

@ --- crt0 ------------------------------------------------------------------
.section .startup,"ax"
.thumb_func
.global cstartup
cstartup:
  ldr sp, =__stack_top__
  b cmain

.text
@ HardFault: hand the active stack's exception frame to hardfault_report (C).
@ The reporter runs on a FRESH stack: if the fault WAS a stack overflow,
@ pushing the reporter's frame on the old MSP double-faults into lockup and
@ the report never reaches the wire (the teensy41 lesson).
.thumb_func
.global isr_hardfault
isr_hardfault:
  tst lr, #4                    @ EXC_RETURN bit 2: which SP was active
  ite eq
  mrseq r0, msp
  mrsne r0, psp
  ldr r1, =__stack_top__
  mov sp, r1
  b hardfault_report

.thumb_func
.global default_handler
default_handler:
  wfi
  b default_handler

@ semihosting SYS_EXIT_EXTENDED: qemu -semihosting leaves with r0's code.
@ On silicon a bkpt with no debugger escalates to lockup -- QSMOKE only.
.thumb_func
.global sh_exit
sh_exit:
  ldr r1, =0x20026              @ ADP_Stopped_ApplicationExit
  push {r0}
  push {r1}
  mov r1, sp
  movs r0, #0x20
  bkpt 0xAB
  b .

@ the helpers mooncc's C calls in place of inline asm
.thumb_func
.global arm_dsb_isb
arm_dsb_isb:
  dsb
  isb
  bx lr

.thumb_func
.global arm_wfi
arm_wfi:
  wfi
  bx lr
