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
    /* love_data: the sentinel tiling, laid by mx.l -- edit the table there. */
    . = ALIGN(128);
    __love_data = .;
    KEEP(*(love_data.0))  . = __love_data + 128;
    KEEP(*(love_data.1))  . = __love_data + 256;
    KEEP(*(love_data.2))  . = __love_data + 384;
    KEEP(*(love_data.3))  . = __love_data + 512;
    KEEP(*(love_data.4))  . = __love_data + 640;
    KEEP(*(love_data.5))  . = __love_data + 768;
    KEEP(*(love_data.6))  . = __love_data + 896;
    KEEP(*(love_data.7))  . = __love_data + 1024;
    KEEP(*(love_data.8))  . = __love_data + 1152;
    /* end love_data */
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
