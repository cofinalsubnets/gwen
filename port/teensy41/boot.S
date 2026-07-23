@ Teensy 4.1 boot image + the asm-only functions -- everything mooncc cannot
@ carry: the ROM-facing blocks need exact SECTIONS at exact flash offsets
@ (mooncc folds named data sections into .data), and the startup/fault/barrier
@ code is bare instructions. This is the verify-first surface: the FCFB and
@ IVT words are byte-for-byte the values first silicon booted with (2026-07,
@ see the old teensy41.c comments and README for the wrong-offset lore).
.syntax unified
.cpu cortex-m7
.fpu fpv5-d16
.thumb

@ --- 1. FlexSPI Configuration Block at flash 0x000 (RT1060 RM 9.6.3.1) -----
@ Tells the ROM how to talk to the QSPI NOR; the LUT holds the 0xEB quad read.
@ 512-byte block, sparse fields at the RM's offsets.
.section .flashconfig,"a"
  .word 0x42464346              @ 0x00 "FCFB" tag
  .word 0x56010000              @ 0x04 version V1.0
  .org 0x0C
  .word 0x00020101              @ readSampleClkSrc=1, csHold=1, csSetup=2
  .org 0x44
  .word 0x00030401              @ deviceType=1 (NOR), pads=4 (quad), 60 MHz
  .org 0x50
  .word 0x00800000              @ sflashA1Size = 8 MB (Teensy 4.1)
  .org 0x80
  .word 0x0A1804EB              @ LUT[0]: CMD 0xEB (1 pad), RADDR 24 (4 pads)
  .word 0x26043206              @ LUT[1]: DUMMY 6 cycles, READ (4 pads)
  .org 0x200                    @ pad the full 512-byte block

@ --- 2. Image Vector Table at flash 0x1000 + Boot Data ---------------------
@ The ROM validates the header and jumps to entry (the .word of a .thumb_func
@ symbol links with the thumb bit -- (S+A)|T).
.section .ivt,"a"
.global image_vector_table
image_vector_table:
  .word 0x432000D1              @ tag 0xD1, len 0x0020, ver 0x43
  .word cstartup                @ entry
  .word 0                       @ rsv1
  .word 0                       @ dcd
  .word boot_data
  .word image_vector_table
  .word 0                       @ csf
  .word 0                       @ rsv2

.section .bootdata,"a"
.global boot_data
boot_data:
  .word 0x60000000              @ image start (FLASH_BASE)
  .word __image_size__          @ bytes the ROM copies/maps
  .word 0                       @ plugin

@ --- 3. ARM vector table (VTOR points here once we run) --------------------
.section .vectors,"a"
.global vectors
vectors:
  .word __stack_top__           @ 0  initial SP (advisory: the ROM boots via IVT)
  .word cstartup                @ 1  reset
  .word default_handler         @ 2  NMI
  .word isr_hardfault           @ 3  HardFault
  .word default_handler         @ 4  MemManage
  .word default_handler         @ 5  BusFault
  .word default_handler         @ 6  UsageFault
  .word 0, 0, 0, 0              @ 7..10 reserved
  .word default_handler         @ 11 SVCall
  .word isr_hardfault           @ 12 DebugMon: with DEMCR.MON_EN a bkpt (a
                                @ __builtin_trap, a contract guard) lands HERE
                                @ and REPORTS instead of locking up mute
  .word 0                       @ 13 reserved
  .word default_handler         @ 14 PendSV
  .word default_handler         @ 15 SysTick

@ --- crt0: the ROM jumps here with a ROM-provided SP -----------------------
.section .startup,"ax"
.thumb_func
.global cstartup
cstartup:
  ldr sp, =__stack_top__
  b cmain

.text
@ HardFault: hand the active stack's exception frame to hardfault_report (C).
@ The reporter runs on a FRESH stack (__stack_top__): if the fault WAS a stack
@ overflow (or SP is garbage), pushing the reporter's frame on the old MSP
@ double-faults into lockup and the report never reaches the wire.
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

@ the helpers mooncc's C calls in place of inline asm
.thumb_func
.global arm_dsb_isb
arm_dsb_isb:
  dsb
  isb
  bx lr

.thumb_func
.global arm_dsb
arm_dsb:
  dsb
  bx lr

.thumb_func
.global arm_wfi
arm_wfi:
  wfi
  bx lr

.thumb_func
.global arm_bkpt
arm_bkpt:
  bkpt 0
  bx lr
