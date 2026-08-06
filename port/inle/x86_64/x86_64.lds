/* Tell the linker that we want an x86_64 ELF64 output file */
OUTPUT_FORMAT(elf64-x86-64)

/* The ELF entry is the limine door (limine enters e_entry in long mode, high
   half mapped). The PVH door (qemu -kernel, no bootloader/firmware) enters
   through the .note.pvh ELF note instead -- mkboot.l's 32-bit stub, which does
   the bring-up limine would have done and then calls kmain itself. */
ENTRY(kmain)

/* Define the program headers we want so the bootloader gives us the right */
/* MMU permissions; this also allows us to exert more control over the linking */
/* process. */
PHDRS
{
    note PT_NOTE;
    boot PT_LOAD;
    limine_requests PT_LOAD;
    text PT_LOAD;
    rodata PT_LOAD;
    data PT_LOAD;
}

SECTIONS
{
    /* We want to be placed in the topmost 2GiB of the address space, for optimisations */
    /* and because that is what the Limine spec mandates (every PT_LOAD vaddr high). */
    /* The PVH loader loads by PHYSICAL address instead, so every section carries an */
    /* AT() LMA of vaddr - KVMA: the image lands at 2 MiB and mkboot.l's 32-bit code, */
    /* executing at its physical address before paging, subtracts KVMA by hand. */
    . = 0xffffffff80000000 + 0x200000;

    .note.pvh : AT(ADDR(.note.pvh) - 0xffffffff80000000) {
        KEEP(*(.note.pvh))
    } :note

    /* mkboot.l: page tables + GDT (.boot) and the 32/64-bit ladder (.boot.text). */
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    .boot : AT(ADDR(.boot) - 0xffffffff80000000) {
        KEEP(*(.boot))
        KEEP(*(.boot.text))
    } :boot

    /* Define a section to contain the Limine requests and assign it to its own PHDR */
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    .limine_requests : AT(ADDR(.limine_requests) - 0xffffffff80000000) {
        KEEP(*(.limine_requests_start))
        KEEP(*(.limine_requests))
        KEEP(*(.limine_requests_end))
    } :limine_requests

    /* Move to the next memory page for .text */
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .text : AT(ADDR(.text) - 0xffffffff80000000) {
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
    } :text

    /* Move to the next memory page for .rodata */
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .rodata : AT(ADDR(.rodata) - 0xffffffff80000000) {
        *(.rodata .rodata.*)
    } :rodata

    /* Add a .note.gnu.build-id output section in case a build ID flag is added to the */
    /* linker command. */
    .note.gnu.build-id : {
        *(.note.gnu.build-id)
    } :rodata

    /* Move to the next memory page for .data */
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .data : AT(ADDR(.data) - 0xffffffff80000000) {
        *(.data .data.*)
    } :data

    /* NOTE: .bss needs to be the last thing mapped to :data, otherwise lots of */
    /* unnecessary zeros will be written to the binary. */
    /* If you need, for example, .init_array and .fini_array, those should be placed */
    /* above this. */
    .bss : AT(ADDR(.bss) - 0xffffffff80000000) {
        *(.bss .bss.*)
        *(COMMON)
    } :data

    /* the physical footprint's far edge: pvh.c subtracts [2 MiB, here - KVMA) */
    /* from the e820 usable ranges, since qemu's -kernel load reserves nothing. */
    kimage_end = .;

    /* Discard .note.* and .eh_frame* since they may cause issues on some hosts. */
    /DISCARD/ : {
        *(.eh_frame*)
        *(.note .note.*)
    }
}
