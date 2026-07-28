// aarch64 -kernel boot: qemu -M virt enters the ELF entry at EL1, MMU off,
// PC translated through the load segments (probed: a high e_entry inside a
// PT_LOAD enters at its LMA) -- no bootloader, no firmware image anywhere.
// This stub owns the bring-up limine used to do: translation tables, MAIR/
// TCR/TTBR, the MMU switch, a stack, then C (dtb.c fills kboot from the
// device tree qemu lays at the RAM base, kmain takes over). x86_64/boot.S
// is the model; the door DISCRIMINATOR here is one bit: limine enters with
// the MMU ON (SCTLR_EL1.M set), qemu -kernel with it OFF, so e_entry serves
// both -- the limine path runs two instructions and lands in kmain.
//
// LINKED HIGH, RUNS LOW: every PT_LOAD vaddr sits in the top 2 GiB (limine
// requires it) with an AT() LMA of vaddr - KVMA + 0x40000000 (virt's RAM
// base; qemu loads by p_paddr). Until the hop, code executes at the low
// physical address -- adrp is pc-relative, so every address it forms is the
// RUNNING (low) one, and `ldr =sym` (an absolute literal) is the LINKED
// (high) one. The two spellings ARE the two worlds; x86 subtracted KVMA by
// hand where we just pick the right instruction.
//
// The tables mirror x86's shape index for index: hhdm at L0[256], the
// kernel gig at L1[510]. TTBR0 gets the identity map (the switch itself),
// TTBR1 the hhdm + KVMA. The first gig is virt's MMIO ground and is laid
// as a real L2 of 2 MiB Device blocks -- arch.c's mmio_map walks TTBR1
// expecting a TABLE there (a 1 GiB block would read as a bogus L2 pointer),
// and it finds our l2_dev and re-lays the two slots it cares about, same
// shape. RAM (1..4 GiB) rides 1 GiB Normal blocks; above 4 GiB stays
// unmapped, dtb.c clips (the kernel lanes run 256/512 MiB).
//
// Table walks are configured cacheable (TCR IRGN/ORGN WB) while the tables
// were written with the cache off -- coherent under TCG, where the gate
// lives; a real-metal ride wants a dc civac sweep over the tables first.

.set KVMA, 0xffffffff80000000
.set RAMB, 0x40000000

  .section .boot, "aw"
// Translation tables, zero file bytes the loader hands over whole. One L0
// per TTBR, a shared L1 (identity and hhdm see the same gig entries), the
// kernel-high L1, and the Device L2 over the first gig.
  .balign 4096
l0_lo:  .skip 4096                    // TTBR0: [0] -> l1_id
l0_hi:  .skip 4096                    // TTBR1: [256] hhdm -> l1_id, [511] -> l1_hi
l1_id:  .skip 4096                    // [0] -> l2_dev, [1..3] 1G Normal blocks
l1_hi:  .skip 4096                    // [510] the kernel gig: KVMA -> RAMB
l2_dev: .skip 4096                    // 512 x 2M Device blocks over phys 0..1G

  .section .boot.text, "ax"
  .global a64boot
a64boot:
  mrs  x9, sctlr_el1                  // the door: MMU on = limine brought us up
  tbz  x9, #0, 1f
  b    kmain
1:
  mov  x21, x0                        // x0 is 0 today (probed); a loader that
                                      // passes a dtb pointer is honored below
  // l2_dev: 2M Device blocks (valid | AF, AttrIndx 0)
  adrp x0, l2_dev
  add  x0, x0, :lo12:l2_dev
  mov  x1, #0
  mov  x2, #0x401
2:
  lsl  x3, x1, #21
  orr  x3, x3, x2
  str  x3, [x0, x1, lsl #3]
  add  x1, x1, #1
  cmp  x1, #512
  b.ne 2b
  // l1_id: [0] table -> l2_dev, [1..3] 1G Normal blocks
  // (valid | AttrIndx 1 | SH inner | AF = 0x705)
  adrp x1, l1_id
  add  x1, x1, :lo12:l1_id
  orr  x3, x0, #3
  str  x3, [x1]
  ldr  x2, =(0x40000000 | 0x705)
  str  x2, [x1, #8]
  ldr  x2, =(0x80000000 | 0x705)
  str  x2, [x1, #16]
  ldr  x2, =(0xC0000000 | 0x705)
  str  x2, [x1, #24]
  // l1_hi: [510] = the kernel gig, KVMA + V -> RAMB + V
  adrp x0, l1_hi
  add  x0, x0, :lo12:l1_hi
  ldr  x2, =(0x40000000 | 0x705)
  str  x2, [x0, #510*8]
  // the L0s: identity for the switch, hhdm + kernel-high for the life after
  orr  x3, x1, #3
  adrp x0, l0_lo
  add  x0, x0, :lo12:l0_lo
  str  x3, [x0]
  adrp x2, l0_hi
  add  x2, x2, :lo12:l0_hi
  str  x3, [x2, #256*8]
  adrp x4, l1_hi
  add  x4, x4, :lo12:l1_hi
  orr  x4, x4, #3
  str  x4, [x2, #511*8]
  // MAIR: attr0 Device-nGnRnE (what mmio_attr_index prefers), attr1 Normal WB
  ldr  x3, =0xff00
  msr  mair_el1, x3
  // TCR: 48-bit VA both halves, 4K granules, WB WA inner-shareable walks,
  // 40-bit PA -- T0SZ=16 IRGN0/ORGN0=1 SH0=3 | T1SZ=16 IRGN1/ORGN1=1 SH1=3
  // TG1=4K | IPS=40-bit
  ldr  x3, =0x2B5103510
  msr  tcr_el1, x3
  msr  ttbr0_el1, x0
  msr  ttbr1_el1, x2
  // FP/SIMD on before ANY C runs (dtb.c is clang-compiled and free to
  // vectorize) -- archinit's own set stays for the limine door.
  mrs  x3, cpacr_el1
  orr  x3, x3, #(3 << 20)
  msr  cpacr_el1, x3
  dsb  ish
  tlbi vmalle1
  dsb  ish
  isb
  mrs  x3, sctlr_el1
  orr  x3, x3, #(1 << 0)              // M
  orr  x3, x3, #(1 << 2)              // C
  orr  x3, x3, #(1 << 12)             // I
  msr  sctlr_el1, x3
  isb
  ldr  x9, =khop                      // the hop: an absolute literal is high
  br   x9

  .text
khop:
  adrp x9, kstack
  add  x9, x9, :lo12:kstack
  add  x9, x9, #16, lsl #12           // + kstack_size (64 KiB)
  mov  sp, x9
  mov  x1, #RAMB                      // qemu lays the dtb at the RAM base
  cmp  x21, #0
  csel x0, x1, x21, eq
  bl   dtb_to_kboot
  bl   kmain
3:
  wfi
  b    3b

// the boot stack: kmain never returns and limine's own (64 KiB) is the
// precedent for depth; .bss so it costs the file nothing.
  .set kstack_size, 65536
  .bss
  .balign 16
kstack:
  .skip kstack_size
