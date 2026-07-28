// PVH boot: qemu -kernel enters pvh32 below in 32-bit protected mode, paging
// off, ebx -> hvm_start_info (the e820 map) -- qemu's own pvh option ROM, no
// bootloader and no firmware image anywhere. This stub owns the whole
// bring-up limine used to do: page tables, GDT, the long-mode ladder, a
// stack, then C (pvh.c fills kboot from the start_info, kmain takes over).
// The limine iso lane still boots this same ELF -- limine uses the ELF entry
// (kmain) and never runs a byte of this section; conversely qemu's PVH
// loader takes its entry from the ELF note, not e_entry. One kernel, both
// doors.
//
// LINKED HIGH, RUNS LOW: limine requires every PT_LOAD vaddr in the top
// 2 GiB, so .boot cannot be linked at its physical address -- it is linked
// at KVMA+2M with an AT() LMA of 2M (qemu loads by p_paddr; probed). Until
// the ladder lands in the high hop, code EXECUTES at the low physical
// address: every absolute symbol reference in the 32-bit part must subtract
// KVMA by hand, near jumps are relative and need nothing.

.set KVMA, 0xffffffff80000000

  .section .note.pvh, "a", @note
  .balign 4
  .long 4                             // namesz "Xen\0"
  .long 4                             // descsz
  .long 18                            // XEN_ELFNOTE_PHYS32_ENTRY
  .asciz "Xen"
  .long pvh32 - KVMA

  .section .boot, "aw"
// Page tables, zero-filled by the loader (they are file bytes). One PML4,
// two PDPTs (identity+hhdm share one, the kernel high map gets the other),
// four PDs of 2 MiB entries covering physical 0..4 GiB. The identity map
// serves the ladder itself, the hhdm window at 0xffff8000_00000000 serves
// kboot/meminit, and PML4[511]/PDPT[510] alias the same first gigabyte at
// KVMA so vaddr = phys + KVMA over the whole loaded image. RAM above 4 GiB
// stays unmapped -- pvh.c drops those e820 entries (qemu -m beyond ~3 GiB
// splits high; the kernel lanes run 256/512 MiB).
  .balign 4096
pml4:     .skip 4096
pdpt_id:  .skip 4096
pdpt_hi:  .skip 4096
pd0:      .skip 4096 * 4

  .balign 8
gdt:
  .quad 0
  .quad 0x00209A0000000000            // 0x08: 64-bit code
  .quad 0x0000920000000000            // 0x10: data
gdt_end:
  .balign 8
gdtreg:
  .word gdt_end - gdt - 1
  .quad gdt - KVMA                    // lgdt runs before paging: physical base

  .section .boot.text, "ax"
  .code32
  .global pvh32
pvh32:
  cli
  // the four PDs: entry i = (i << 21) | P|RW|PS -- 2048 qwords, phys 0..4G
  mov $(pd0 - KVMA), %edi
  xor %ecx, %ecx
1:
  mov %ecx, %eax
  shl $21, %eax
  or  $0x83, %eax
  mov %eax, (%edi,%ecx,8)             // high dword stays the loader's zero
  inc %ecx
  cmp $2048, %ecx
  jne 1b
  // identity + hhdm PDPT: entries 0..3 -> the four PDs
  mov $(pdpt_id - KVMA), %edi
  mov $(pd0 - KVMA + 0x03), %eax
  mov %eax, 0(%edi)
  add $4096, %eax
  mov %eax, 8(%edi)
  add $4096, %eax
  mov %eax, 16(%edi)
  add $4096, %eax
  mov %eax, 24(%edi)
  // kernel-high PDPT: entry 510 -> PD0 (KVMA maps phys 0..1G)
  mov $(pdpt_hi - KVMA), %edi
  movl $(pd0 - KVMA + 0x03), 510*8(%edi)
  // PML4: [0] identity, [256] hhdm (0xffff8000_00000000), [511] kernel-high
  mov $(pml4 - KVMA), %edi
  mov $(pdpt_id - KVMA + 0x03), %eax
  mov %eax, 0(%edi)
  mov %eax, 256*8(%edi)
  movl $(pdpt_hi - KVMA + 0x03), 511*8(%edi)

  lgdt (gdtreg - KVMA)
  mov %cr4, %eax
  or  $(1 << 5), %eax                 // CR4.PAE
  mov %eax, %cr4
  mov $(pml4 - KVMA), %eax
  mov %eax, %cr3
  mov $0xC0000080, %ecx               // EFER
  rdmsr
  or  $(1 << 8), %eax                 // LME
  wrmsr
  mov %cr0, %eax
  or  $0x80000001, %eax               // PG | PE
  mov %eax, %cr0
  ljmp $0x08, $(pvh64 - KVMA)

  .code64
pvh64:
  mov $0x10, %ax
  mov %ax, %ds
  mov %ax, %es
  mov %ax, %ss
  movabs $khop, %rax                  // hop to the high mapping
  jmp *%rax

  .text
khop:
  movabs $(kstack + kstack_size), %rsp
  // SSE on before ANY C runs (pvh_to_kboot is clang-compiled and free to
  // vectorize) -- the same dance as the top of kmain, which stays for the
  // limine door where IT is the first code to run.
  mov %cr0, %rax
  and $~(1 << 2), %rax                // CR0.EM = 0
  or  $(1 << 1), %rax                 // CR0.MP = 1
  mov %rax, %cr0
  mov %cr4, %rax
  or  $((1 << 9) | (1 << 10)), %rax   // CR4.OSFXSR | OSXMMEXCPT
  mov %rax, %cr4
  mov %ebx, %edi                      // hvm_start_info (phys, low) -> arg0
  call pvh_to_kboot
  call kmain
2:
  cli
  hlt
  jmp 2b

// the boot stack: kmain never returns and limine's own (64 KiB) is the
// precedent for depth; .bss so it costs the file nothing.
  .set kstack_size, 65536
  .bss
  .balign 16
kstack:
  .skip kstack_size
