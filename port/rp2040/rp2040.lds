/* RP2040 bare-metal link map (Cortex-M0+, no SDK).
 *
 * Flash (XIP) at 0x10000000: the 256-byte checksummed second stage (.boot2,
 * laid by mkboot2.l) sits first, then the vector table at 0x10000100 -- the
 * address the bootrom's stage-2 hands control to -- then code/rodata. .data is
 * loaded from flash and copied to SRAM by reset_handler (crt0 in rp2040.c);
 * .bss is zeroed there. The stack lives at the top of the 264 KB main SRAM.
 *
 * ⚠ the PHDRS block is not decoration: a compiler-named section comes out
 * ALLOC|WRITE (objsec-named has no way to know the table is const -- the
 * kernel's scanned-for .limine_requests, the other user of that door, must be
 * writable), so .vectors would drag the whole flash segment to RWX. Naming the
 * segments says what the regions actually are: flash R|X, SRAM R|W. */

OUTPUT_FORMAT("elf32-littlearm")
ENTRY(reset_handler)

MEMORY {
  FLASH (rx) : ORIGIN = 0x10000000, LENGTH = 2048k
  RAM  (rw)  : ORIGIN = 0x20000000, LENGTH = 264k
}

PHDRS {
  flash PT_LOAD FLAGS(5);   /* R|X */
  ram   PT_LOAD FLAGS(6);   /* R|W */
}

SECTIONS {
  /* 256-byte checksummed boot2 at flash base. */
  .boot2 : { KEEP(*(.boot2)) } > FLASH :flash
  ASSERT(SIZEOF(.boot2) == 256, "boot2 must be exactly 256 bytes")

  /* Vector table at 0x10000100 (immediately after boot2). */
  .vectors : { KEEP(*(.vectors)) } > FLASH :flash

  .text : {
    *(.text .text.*)
    /* the data sentinels, tiled at love.h's ai_data_stride in enum d order:
       love.h reads the slot index AS the kind, so where these land is the
       whole point. one `. =` per slot pins the stride whatever the bodies
       measure -- an overgrown one moves the counter backwards and ld says so.
       the base takes the STRIDE's own alignment, never a lesser one, or ld
       bumps the counter past the very slot it was placing.
       mirrors love_data.ld, the host link's fragment. */
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
  } > FLASH :flash

  .rodata : {
    *(.rodata .rodata.*)
    *(.srodata .srodata.*)
  } > FLASH :flash

  /* .data: VMA in SRAM, LMA in flash. reset_handler copies it. */
  . = ALIGN(4);
  .data : {
    __data_start__ = .;
    *(.data .data.*)
    *(.sdata .sdata.*)
    . = ALIGN(4);
    __data_end__ = .;
  } > RAM AT> FLASH :ram
  __data_load__ = LOADADDR(.data);

  .bss (NOLOAD) : {
    . = ALIGN(4);
    __bss_start__ = .;
    *(.bss .bss.*)
    *(.sbss .sbss.*)
    *(COMMON)
    . = ALIGN(4);
    __bss_end__ = .;
  } > RAM :ram

  __stack_top__ = ORIGIN(RAM) + LENGTH(RAM);

  /DISCARD/ : {
    *(.eh_frame*)
    *(.note .note.*)
    *(.ARM.exidx*)
    *(.ARM.attributes)
  }
}
