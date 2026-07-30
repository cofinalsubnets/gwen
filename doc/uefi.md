# the uefi door

`port/inle/uefi/` is our own `BOOTX64.EFI`: the loader that puts the inle kernel
on real hardware without limine, gnu-efi, or any foreign toolchain. mooncc
compiles it, holo lays the PE32+ the firmware runs, and the whole thing is about
250 lines of C plus three thunks of holo IR.

    make uefi          # -> out/free/esp/{EFI/BOOT/BOOTX64.EFI, love.elf}
    make test_uefi     # the same door under qemu, corpus over serial

`test_uefi` rides `make test_slow` at ~64s, the same as the `-kernel` door. It is
the only gate that hands the kernel a framebuffer, so it is the only one that runs
`fbdraw` (`port/inle/kmain.c`) at all -- which is worth knowing when this lane and
`test_kernel` disagree about how long the same corpus takes.

Copy `out/free/esp/` onto a FAT32 EFI system partition and the machine boots
love. `EFI/BOOT/BOOTX64.EFI` is the removable-media path every UEFI firmware
looks for on its own, so nothing has to be registered with the boot manager.

## three doors, one kernel

The kernel ELF is the same binary through all of them; only who brings it up
differs.

| door | brought up by | what it needs | where it shines |
| --- | --- | --- | --- |
| PVH | `port/inle/mkboot.l` | nothing (`qemu -kernel`) | the gate: no downloads, ~4.5s |
| UEFI | this loader | the machine's own firmware | real hardware, framebuffer console |
| limine | limine | a downloaded bootloader + firmware image | the historical lane, aarch64 |

`kmain` cannot tell them apart: each door fills the same `struct k_boot`
(`port/inle/k.h`) and jumps. The limine request section answers NULL when
nothing filled it, so the limine path costs the other two doors nothing.

## what the loader does

1. **finds itself.** `LoadedImage` -> `DeviceHandle` -> `SimpleFileSystem` ->
   the root of the volume it was booted from, then opens `love.elf` beside it.
   No path configuration: the kernel is the loader's neighbour by construction.
2. **loads the kernel.** Reads the ELF whole, copies every `PT_LOAD` into one
   fresh 2 MiB-aligned span. The link addresses only fix *relative* placement --
   the kernel runs wherever the firmware had pages free.
3. **fills `kboot`.** Finds the `kboot` symbol in the kernel's own symbol table
   (our binaries carry one on purpose -- the same symtab `nm` and `gdb` read)
   and writes the memory map, the hhdm offset, and the GOP framebuffer into it.
   This is why the loader needs no shared header with the kernel beyond the
   struct shape: the *binary* says where its `kboot` lives.
4. **exits boot services**, retrying once with a re-fetched map key -- the one
   place UEFI insists on a dance rather than a call.
5. **switches to our page tables and jumps.** Identity + hhdm over the low 4 GiB
   in 2 MiB pages, and `0xffffffff80000000` over wherever the kernel landed.

Only conventional memory reaches `kboot.ram`: the loader's own pages, the page
tables, and the kernel span stay out, so the kernel heap never eats the ground
it is standing on.

## the two things C cannot say

`mkefi.l` lays them in holo IR, the way `port/virt/mkstart.l` lays riscv's
bring-up. UEFI is ms_abi and the compiler emits SysV, so the seam is explicit:

- `efiboot` -- the PE entry. Firmware calls it ms_abi (`rcx` = ImageHandle,
  `rdx` = SystemTable); it saves the two registers that are ms-callee-saved but
  SysV-scratch (`rsi`/`rdi`) and calls `efi_main` the SysV way.
- `efi_call` -- every UEFI service call, SysV in, ms out, with the 32-byte
  shadow space the Microsoft ABI requires above the arguments.
- `efi_go` -- `cli`, load `cr3`, jump. Three instructions with no C spelling.

The loader stays integer-only on purpose: the entry thunk saves no xmm
registers, which is sound exactly as long as no float sneaks into the C.

## the PE32+ container

`crew/holo/pe.l` is `link.l`'s sibling -- same lay-and-patch heart over the same
mooncc objects, different clothing. One RWX `.text` section holds text, rodata,
data and nifs contiguously (bss is the zero tail the loader grants past
`SizeOfRawData`), and `.reloc` carries the abs64 sites as DIR64 base
relocations, so the firmware may load the image anywhere and fix the pointers
itself. mooncc's codegen is rip-relative for local symbols, so that table is
usually tiny or empty -- but the section must exist regardless: an image without
`.reloc` is one the firmware refuses to slide.

## traps met on the way

- **2 MiB PDEs need 2 MiB-aligned physical addresses.** The kernel's lowest load
  address is page-aligned (`0x201000`), not 2 MiB-aligned; using it directly as
  the map anchor sets reserved bits in every PDE and the first instruction fetch
  after `mov cr3` takes a `#PF`. The loader floors the anchor to 2 MiB.
- **OVMF's exception dump is the debugger.** `RIP`/`CR2`/`CR3` in that dump name
  the fault exactly -- reading it settled the above in one boot, where guessing
  at the page tables would have taken many.
