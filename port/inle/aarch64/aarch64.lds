/* Tell the linker that we want an aarch64 ELF64 output file */
OUTPUT_FORMAT(elf64-littleaarch64)

/* The ELF entry is mkboot.l's stub, which serves BOTH doors: limine enters it
   with the MMU on and it branches straight to kmain; qemu -kernel enters it
   at its physical address with the MMU off (qemu translates a high e_entry
   through the load segments -- probed) and it does the bring-up limine would
   have done. */
ENTRY(a64boot)

/* Define the program headers we want so the bootloader gives us the right */
/* MMU permissions; this also allows us to exert more control over the linking */
/* process. */
PHDRS
{
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
    /* The qemu -kernel loader loads by PHYSICAL address instead, so every section */
    /* carries an AT() LMA of vaddr - KVMA + 0x40000000 (the virt machine's RAM base): */
    /* the image lands 2 MiB into RAM, leaving the base for the device tree qemu */
    /* writes there. */
    . = 0xffffffff80000000 + 0x200000;

    /* mkboot.l: translation tables (.boot) and the EL1 MMU ladder (.boot.text). */
    .boot : AT(ADDR(.boot) - 0xffffffff80000000 + 0x40000000) {
        KEEP(*(.boot))
        KEEP(*(.boot.text))
    } :boot

    /* Define a section to contain the Limine requests and assign it to its own PHDR */
    . = ALIGN(CONSTANT(MAXPAGESIZE));
    .limine_requests : AT(ADDR(.limine_requests) - 0xffffffff80000000 + 0x40000000) {
        KEEP(*(.limine_requests_start))
        KEEP(*(.limine_requests))
        KEEP(*(.limine_requests_end))
    } :limine_requests

    /* Move to the next memory page for .text */
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .text : AT(ADDR(.text) - 0xffffffff80000000 + 0x40000000) {
        *(.text .text.*)
        /* love_data: the sentinel tiling, laid by mx.l -- edit the table there. */
        . = ALIGN(16);
        __love_data = .;
        KEEP(*(love_data.0))  . = __love_data + 16;
        KEEP(*(love_data.1))  . = __love_data + 32;
        KEEP(*(love_data.2))  . = __love_data + 48;
        KEEP(*(love_data.3))  . = __love_data + 64;
        KEEP(*(love_data.4))  . = __love_data + 80;
        KEEP(*(love_data.5))  . = __love_data + 96;
        KEEP(*(love_data.6))  . = __love_data + 112;
        KEEP(*(love_data.7))  . = __love_data + 128;
        KEEP(*(love_data.8))  . = __love_data + 144;
        /* end love_data */
    } :text

    /* Move to the next memory page for .rodata */
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .rodata : AT(ADDR(.rodata) - 0xffffffff80000000 + 0x40000000) {
        *(.rodata .rodata.*)
    } :rodata

    /* Add a .note.gnu.build-id output section in case a build ID flag is added to the */
    /* linker command. */
    .note.gnu.build-id : {
        *(.note.gnu.build-id)
    } :rodata

    /* Move to the next memory page for .data */
    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .data : AT(ADDR(.data) - 0xffffffff80000000 + 0x40000000) {
        *(.data .data.*)
    } :data

    /* NOTE: .bss needs to be the last thing mapped to :data, otherwise lots of */
    /* unnecessary zeros will be written to the binary. */
    /* If you need, for example, .init_array and .fini_array, those should be placed */
    /* above this. */
    .bss : AT(ADDR(.bss) - 0xffffffff80000000 + 0x40000000) {
        *(.bss .bss.*)
        *(COMMON)
    } :data

    /* the physical footprint's far edge: dtb.c hands the heap everything from */
    /* the RAM base up to here-translated-physical, since the qemu loader */
    /* reserves nothing. */
    kimage_end = .;

    /* Discard .note.* and .eh_frame* since they may cause issues on some hosts. */
    /DISCARD/ : {
        *(.eh_frame*)
        *(.note .note.*)
    }
}
