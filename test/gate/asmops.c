// the probe test_asmops compiles: one call to every inline in the kernel's
// per-arch asmops.h, so mooncc has to parse the header, take the __mooncc__
// half of every #ifdef, and hand each NEUTRAL template to holo. the gate then
// disassembles the object and demands the privileged instructions it expects,
// which is what proves the templates encode to what they say rather than to
// something that merely assembled.
//
// this file is deliberately NOT under free/<a>/ -- kernel.mk globs that
// directory, and a probe living there would join the kernel build.
#include <stdint.h>
#include "asmops.h"

#if defined(__x86_64__)
uint64_t k_asmops_probe(uint16_t port, uint8_t v) {
  uint32_t b, c, d;
  k_cli();
  k_sti();
  k_wait();
  k_sse_enable();
  k_outb(port, v);
  k_outl(port, v);
  k_int3();
  k_ud2();
  k_divzero();
  k_wrmsr(0xc0000080u, 0);
  k_cpuid(0, &b, &c, &d);
  k_vmsave(port);
  k_vmrun(port);
  k_vmload(port);
  k_stgi();
  k_clgi();
  uint64_t pa = port, grax = 0;
  char dt[10];
  k_vmxon(&pa);
  k_vmclear(&pa);
  k_vmptrld(&pa);
  k_vmwrite(0x6c14, pa);
  k_vmxoff();
  k_sgdt(dt);
  k_sidt(dt);
  k_lgdt(dt);
  k_wr_cr0(k_rd_cr0());
  k_wr_cr4(k_rd_cr4());
  return k_rd_cr2() + k_inb(port) + k_inl(port) + k_rdmsr(0xc0000080u) + b + c + d
       + k_rd_cr0() + k_rd_cr3() + k_vmread(0x4402) + (uint64_t) k_vmlaunch(&grax) + grax; }

// ⚠ AND EACH OP MUST EXIST AS ITS OWN FUNCTION for the differential to have
// anything to compare. It is not enough to CALL them: a `static inline` whose
// every call is inlined is dead, and a compiler is right to drop the body --
// mooncc's dead-static sweep does exactly that, and then its side of the
// comparison is empty while clang's (at -O0) is full. Taking each op's ADDRESS
// is what keeps it, by the language's own rule rather than by an optimizer's
// mood. The array is never read; it only has to exist.
void const *const k_asmops_keep[] = {
  (void const*) k_cli,
  (void const*) k_sti,
  (void const*) k_wait,
  (void const*) k_sse_enable,
  (void const*) k_outb,
  (void const*) k_outl,
  (void const*) k_int3,
  (void const*) k_ud2,
  (void const*) k_divzero,
  (void const*) k_rd_cr2,
  (void const*) k_rd_cr0,
  (void const*) k_rd_cr3,
  (void const*) k_rd_cr4,
  (void const*) k_wr_cr0,
  (void const*) k_wr_cr4,
  (void const*) k_rdmsr,
  (void const*) k_wrmsr,
  (void const*) k_cpuid,
  (void const*) k_vmrun,
  (void const*) k_vmsave,
  (void const*) k_vmload,
  (void const*) k_stgi,
  (void const*) k_clgi,
  (void const*) k_vmxon,
  (void const*) k_vmclear,
  (void const*) k_vmptrld,
  (void const*) k_vmxoff,
  (void const*) k_vmread,
  (void const*) k_vmwrite,
  (void const*) k_sgdt,
  (void const*) k_sidt,
  (void const*) k_lgdt,
  (void const*) k_vmlaunch,
  (void const*) k_inb,
  (void const*) k_inl
};

#elif defined(__aarch64__)
uint64_t k_asmops_probe(void *va, uintptr_t p, volatile uint64_t *block) {
  k_isb();
  k_dsb_ish();
  k_wait();
  k_wr_cntp_tval_el0(1);
  k_wr_cntp_ctl_el0(1);
  k_wr_vbar_el1(p);
  k_daif_mask_all();
  k_daif_unmask_irq();
  k_tlbi_all();
  k_dc_cvau(p);
  k_ic_ivau(p);
  k_sp_to_el1h();
  k_fpen_enable();
  k_brk0();
  k_udf0();
  k_semihost_exit(block);
  k_psci_system_reset();
  return k_rd_ctr_el0() + k_rd_mair_el1() + k_rd_ttbr1_el1()
       + k_rd_cntfrq_el0() + k_at_s1e1w_par(va); }
void const *const k_asmops_keep[] = {
  (void const*) k_isb,
  (void const*) k_dsb_ish,
  (void const*) k_wait,
  (void const*) k_wr_cntp_tval_el0,
  (void const*) k_wr_cntp_ctl_el0,
  (void const*) k_wr_vbar_el1,
  (void const*) k_daif_mask_all,
  (void const*) k_daif_unmask_irq,
  (void const*) k_tlbi_all,
  (void const*) k_dc_cvau,
  (void const*) k_ic_ivau,
  (void const*) k_sp_to_el1h,
  (void const*) k_fpen_enable,
  (void const*) k_brk0,
  (void const*) k_udf0,
  (void const*) k_semihost_exit,
  (void const*) k_psci_system_reset,
  (void const*) k_rd_ctr_el0,
  (void const*) k_rd_mair_el1,
  (void const*) k_rd_ttbr1_el1,
  (void const*) k_rd_cntfrq_el0,
  (void const*) k_at_s1e1w_par
};

#else
#error "asmops probe: no arch"
#endif
