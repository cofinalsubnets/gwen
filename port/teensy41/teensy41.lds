/* Teensy 4.1 (i.MX RT1062) bare-metal link map (Cortex-M7, no Teensyduino).
 *
 * Boots XIP from the 8 MB FlexSPI NOR at 0x60000000. The boot ROM expects, at
 * fixed flash offsets: the FlexSPI Configuration Block at 0x000 (.flashconfig),
 * the Image Vector Table at 0x1000 (.ivt) with the Boot Data right after, then
 * the ARM vector table and code/rodata. .data is loaded from flash and copied
 * to OCRAM2 by cstartup (crt0 in teensy41.c); .bss is zeroed there. We run all
 * RAM (data/bss/heap/stack) out of the dedicated 512 KB OCRAM2 at 0x20200000,
 * which is mapped at reset independent of the FlexRAM (ITCM/DTCM) split -- so
 * the scaffold needs no FlexRAM reconfiguration to have working memory (moving
 * hot code to ITCM / stack to DTCM is a documented optimisation, see README).
 * Mirrors the freestanding arch/<arch>/*.lds. */

OUTPUT_FORMAT("elf32-littlearm")
ENTRY(cstartup)

MEMORY {
  FLASH  (rx)  : ORIGIN = 0x60000000, LENGTH = 8128k   /* Teensy 4.1: 8 MB NOR */
  OCRAM2 (rwx) : ORIGIN = 0x20200000, LENGTH = 512k
  ITCM   (rwx) : ORIGIN = 0x00000000, LENGTH = 0       /* unused in scaffold */
  DTCM   (rwx) : ORIGIN = 0x20000000, LENGTH = 0       /* unused in scaffold */
}

SECTIONS {
  /* FlexSPI configuration block at flash base (ROM reads it first). */
  .flashconfig 0x60000000 : { KEEP(*(.flashconfig)) } > FLASH

  /* Image Vector Table at the fixed 0x1000 serial-NOR offset, then Boot Data. */
  .ivt 0x60001000 : {
    KEEP(*(.ivt))
    KEEP(*(.bootdata))
  } > FLASH

  /* ARM vector table (VTOR points here once we run; the ROM boots via the IVT). */
  .vectors : {
    . = ALIGN(4);
    KEEP(*(.vectors))
  } > FLASH

  .text : {
    KEEP(*(.startup))
    *(.text .text.*)
    /* the data sentinels, tiled at love.h's ai_data_stride in enum d order:
       love.h reads the slot index AS the kind, so where these land is the
       whole point. one `. =` per slot pins the stride whatever the bodies
       measure -- an overgrown one moves the counter backwards and ld says so.
       mirrors love_data.ld, the host link's fragment. */
    . = ALIGN(16);
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
  } > FLASH

  .rodata : {
    *(.rodata .rodata.*)
    *(.srodata .srodata.*)
    . = ALIGN(4);
  } > FLASH

  /* The baked heap image (qemu-baked by port/mps2's baker, embedded whole).
   * Stays in flash -- the wake copies the blob into the PSRAM major pool.
   * Placed BEFORE .data so this rule (not .data's) claims love-img.o's
   * payload; 8-aligned for the image header's uint64 fields. */
  .image : {
    . = ALIGN(8);
    *love-img.o(.data .data.*)
  } > FLASH

  /* .data: VMA in OCRAM2, LMA in flash. cstartup copies it. */
  . = ALIGN(4);
  .data : {
    __data_start__ = .;
    *(.data .data.*)
    *(.sdata .sdata.*)
    . = ALIGN(4);
    __data_end__ = .;
  } > OCRAM2 AT> FLASH
  __data_load__ = LOADADDR(.data);

  .bss (NOLOAD) : {
    . = ALIGN(4);
    __bss_start__ = .;
    *(.bss .bss.*)
    *(.sbss .sbss.*)
    *(COMMON)
    . = ALIGN(4);
    __bss_end__ = .;
  } > OCRAM2

  __stack_top__ = ORIGIN(OCRAM2) + LENGTH(OCRAM2);

  /* boot_data.length: how much of flash the ROM treats as the image. */
  __image_size__ = SIZEOF(.flashconfig) + SIZEOF(.ivt) + SIZEOF(.vectors)
                 + SIZEOF(.text) + SIZEOF(.rodata) + SIZEOF(.image) + SIZEOF(.data);

  /DISCARD/ : {
    *(.eh_frame*)
    *(.note .note.*)
    *(.ARM.exidx*)
    *(.ARM.attributes)
  }
}
