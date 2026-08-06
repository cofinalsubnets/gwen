/* mps2-an500, for the THUMB1 lane only -- the one link here still owed to
 * arm-none-eabi-ld, because it pulls gcc's libgcc.a and crew/holo's linker has
 * no archives. The thumb2 image is bound by `mooncc -Ttext 0` and needs no
 * script at all; see the Makefile.
 * 4 MB ZBT SSRAM1 at 0 (code, the M7 boots from the vector table start.o's
 * .text opens with), 4 MB SSRAM2/3 at 0x20000000 (data + bss + the C stack at
 * its top). The 16 MB PSRAM at 0x60000000 is the love arena -- reached by
 * address from main.c, no section here. qemu -kernel loads every segment
 * directly. ⚠ start.o leads the link: address 0 must be the vector table. */
MEMORY
{
  CODE (rx)  : ORIGIN = 0x00000000, LENGTH = 4M
  DATA (rwx) : ORIGIN = 0x20000000, LENGTH = 4M
}
SECTIONS
{
  .text : { *(.text*) *(.rodata*) } > CODE
  .data : { *(.data*) } > DATA
  .bss  : { *(.bss*) *(COMMON) } > DATA
  __stack_top__ = ORIGIN(DATA) + LENGTH(DATA);
}
