/* Nucleo-F446RE (STM32F446RE) bare-metal link map (Cortex-M4, no HAL).
 *
 * Boots from the 512 KB flash at 0x08000000 -- the F446 needs no vendor boot
 * blocks, just the ARM vector table at flash base (aliased at 0 for reset).
 * .data is loaded from flash and copied to SRAM by cstartup; .bss is zeroed
 * there. All RAM (data/bss/stack) rides the contiguous 128 KB SRAM at
 * 0x20000000. The teensy41.lds shape minus the i.MX ROM sections. */

OUTPUT_FORMAT("elf32-littlearm")
ENTRY(cstartup)

MEMORY {
  FLASH (rx)  : ORIGIN = 0x08000000, LENGTH = 512k
  SRAM  (rwx) : ORIGIN = 0x20000000, LENGTH = 128k
}

SECTIONS {
  .vectors : {
    KEEP(*(.vectors))
  } > FLASH

  .text : {
    KEEP(*(.startup))
    *(.text .text.*)
  } > FLASH

  .rodata : {
    *(.rodata .rodata.*)
    *(.srodata .srodata.*)
    . = ALIGN(4);
  } > FLASH

  /* .data: VMA in SRAM, LMA in flash. cstartup copies it. */
  . = ALIGN(4);
  .data : {
    __data_start__ = .;
    *(.data .data.*)
    *(.sdata .sdata.*)
    . = ALIGN(4);
    __data_end__ = .;
  } > SRAM AT> FLASH
  __data_load__ = LOADADDR(.data);

  .bss (NOLOAD) : {
    . = ALIGN(4);
    __bss_start__ = .;
    *(.bss .bss.*)
    *(.sbss .sbss.*)
    *(COMMON)
    . = ALIGN(4);
    __bss_end__ = .;
  } > SRAM

  __stack_top__ = ORIGIN(SRAM) + LENGTH(SRAM);

  /DISCARD/ : {
    *(.eh_frame*)
    *(.note .note.*)
    *(.ARM.exidx*)
    *(.ARM.attributes)
  }
}
