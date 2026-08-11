// vmx -- the Intel twin of svm.c (doc/vmx.md), answering the same question on
// the other vendor: can a guest run under inle, and does the exit land back in
// ordinary C? Enable VMX, build one VMCS, run a seven-byte 32-bit guest that
// loads a sentinel and executes CPUID, and take the exit.
//
// It is the same spike and roughly three times the file, for three reasons the
// SVM half did not have to pay:
//   * the VMCS is OPAQUE. Every field goes through vmwrite with its own
//     encoding -- forty-odd of them, where the VMCB was plain stores.
//   * a VM exit resumes at HOST_RIP, not after the launch, so the entry is an
//     assembly block with a label (asmops.h's k_vmlaunch) rather than a call.
//   * real mode needs "unrestricted guest", which needs EPT. So this guest runs
//     in 32-bit PAGED protected mode instead, and brings its own page directory
//     -- and a TSS, and a GDT, because VMX checks host and guest TR and inle
//     has never had one.
//
// The guest runs under EPT, so it has an address space of its own: it believes
// its code is at guest-physical 0 and its page directory at 0x1000, and neither
// is true of the machine. There are now TWO translations under every guest
// fetch -- the guest's own directory turns a linear address into a
// guest-physical one, and the EPT turns that into a host-physical one -- and
// the gate's laws cannot pass unless both happened.
//
// ⚠ every physical address is `va - khhdm`, which holds for kernel-heap memory
// and NOT for image statics -- blk.c's law. The caller hands in one
// k_vmx_need() block and everything is carved out of it.
#include "k.h"
#include "asmops.h"
#include <stdint.h>

#define msr_feature_control 0x3au
#define msr_vmx_basic       0x480u
#define msr_vmx_pinbased    0x481u
#define msr_vmx_procbased   0x482u
#define msr_vmx_exit        0x483u
#define msr_vmx_entry       0x484u
#define msr_vmx_cr0_fixed0  0x486u
#define msr_vmx_cr0_fixed1  0x487u
#define msr_vmx_cr4_fixed0  0x488u
#define msr_vmx_cr4_fixed1  0x489u
// the TRUE_* capability MSRs, which say what the CPU will really accept where
// the four above still carry the old default1 bits. ⚠ they exist only when
// IA32_VMX_BASIC bit 55 says so; reading one on a part that lacks it faults.
#define msr_vmx_true_pin    0x48du
#define msr_vmx_true_proc   0x48eu
#define msr_vmx_true_exit   0x48fu
#define msr_vmx_true_entry  0x490u
#define msr_vmx_proc2       0x48bu   // the secondary controls' allowed bits
#define msr_vmx_ept_cap     0x48cu   // IA32_VMX_EPT_VPID_CAP

// the VMCS fields this spike touches (arch/x86/include/asm/vmx.h's numbers).
#define f_vpid              0x0000
#define f_guest_es_sel      0x0800
#define f_guest_tr_sel      0x080e
#define f_host_es_sel       0x0c00
#define f_host_cs_sel       0x0c02
#define f_host_tr_sel       0x0c0c
#define f_eptp              0x201a
#define f_vmcs_link         0x2800
#define f_pin_ctl           0x4000
#define f_cpu_ctl           0x4002
#define f_excep_bitmap      0x4004
#define f_cr3_target_count  0x400a
#define f_exit_ctl          0x400c
#define f_exit_msr_store    0x400e
#define f_exit_msr_load     0x4010
#define f_entry_ctl         0x4012
#define f_entry_msr_load    0x4014
#define f_entry_intr        0x4016
#define f_secondary_ctl     0x401e
#define f_vm_insn_error     0x4400
#define f_exit_reason       0x4402
#define f_guest_es_limit    0x4800
#define f_guest_gdtr_limit  0x4810
#define f_guest_idtr_limit  0x4812
#define f_guest_es_ar       0x4814
#define f_guest_ldtr_ar     0x4820
#define f_guest_tr_ar       0x4822
#define f_guest_intr_state  0x4824
#define f_guest_activity    0x4826
#define f_guest_sysenter_cs 0x482a
#define f_host_sysenter_cs  0x4c00
#define f_cr0_mask          0x6000
#define f_cr4_mask          0x6002
#define f_cr0_shadow        0x6004
#define f_cr4_shadow        0x6006
#define f_guest_cr0         0x6800
#define f_guest_cr3         0x6802
#define f_guest_cr4         0x6804
#define f_guest_es_base     0x6806
#define f_guest_tr_base     0x6814
#define f_guest_gdtr_base   0x6816
#define f_guest_idtr_base   0x6818
#define f_guest_dr7         0x681a
#define f_guest_rsp         0x681c
#define f_guest_rip         0x681e
#define f_guest_rflags      0x6820
#define f_guest_pending_dbg 0x6822
#define f_guest_sysenter_sp 0x6824
#define f_guest_sysenter_ip 0x6826
#define f_host_cr0          0x6c00
#define f_host_cr3          0x6c02
#define f_host_cr4          0x6c04
#define f_host_fs_base      0x6c06
#define f_host_gs_base      0x6c08
#define f_host_tr_base      0x6c0a
#define f_host_gdtr_base    0x6c0c
#define f_host_idtr_base    0x6c0e
#define f_host_sysenter_sp  0x6c10
#define f_host_sysenter_ip  0x6c12

// the eight segment fields march in step, 2 apart, in the order es cs ss ds fs
// gs ldtr tr -- so one loop can lay all eight of a kind.
#define seg_es 0
#define seg_cs 1
#define seg_ss 2
#define seg_ds 3
#define seg_fs 4
#define seg_gs 5
#define seg_ldtr 6
#define seg_tr 7

#define vmx_pg     4096u
// the vmxon region, the VMCS, the guest's code, its page directory, a GDT with
// a TSS descriptor in it, the TSS that descriptor points at -- and the four
// levels of EPT under all of it.
#define vmx_pages  10u
// where the guest BELIEVES its two pages are. Neither is where they live: the
// EPT is what makes guest-physical 0 the code page and 0x1000 the directory,
// and the two laws in the gate cannot pass unless that translation happened.
#define gpa_code   0x0000u
#define gpa_pd     0x1000u
#define vmx_sentinel 0x1234u
// our own GDT: null, the boot code and data selectors copied, then a 16-byte
// 64-bit TSS descriptor at index 3. TR selects it.
#define vmx_tr_sel 0x18u

uintptr_t k_vmx_need(void) { return vmx_pages * vmx_pg + vmx_pg - 1; }

static void vmx_zero(unsigned char *p, uintptr_t n) { while (n--) *p++ = 0; }
static uint64_t pa_of(void const *va) { return (uint64_t) ((uintptr_t) va - khhdm); }
static void w64(unsigned char *p, uintptr_t off, uint64_t v) {
  *(uint64_t*) (p + off) = v; }

// a control word, reconciled with what this CPU will actually accept: the low
// half of the capability MSR is the bits that MUST be 1, the high half the bits
// that MAY be. ⚠ asking for a bit the CPU forbids is a refused entry with no
// other symptom, and so is failing to set one it demands.
static uint32_t ctl_fit(uint32_t msr, uint32_t want) {
  uint64_t m = k_rdmsr(msr);
  return (want | (uint32_t) m) & (uint32_t) (m >> 32); }

// Does this machine offer VMX, and has firmware left it reachable? ⚠ the lock
// bit is the trap: locked with the outside-SMX bit clear means the BIOS turned
// VMX off, and then the CR4.VMXE write below #GPs into a triple fault.
bool k_vmx_ok(void) {
  uint32_t b, c, d;
  k_cpuid(1, &b, &c, &d);
  if (!(c & (1u << 5))) return false;                    // ECX.VMX
  uint64_t fc = k_rdmsr(msr_feature_control);
  if ((fc & 1) && !(fc & (1u << 2))) return false;       // locked, and locked OFF
  // EPT is not optional here (the spike gives its guest an address space of its
  // own), so the capability is part of the question rather than a fallback:
  // secondary controls must be reachable, EPT among them, with a 4-level walk
  // and write-back memory. Every VMX part since Nehalem answers yes.
  if (!(k_rdmsr(msr_vmx_procbased) & (1ull << 63))) return false;   // secondary allowed
  if (!(k_rdmsr(msr_vmx_proc2) & (1ull << 33))) return false;       // ..and EPT among them
  uint64_t ec = k_rdmsr(msr_vmx_ept_cap);
  return (ec & (1ull << 6)) && (ec & (1ull << 14)); }  // 4-level walk, write-back

int k_vmx_spike(void *mem, uint64_t *reason, uint64_t *rax, uint64_t *rip,
                uint64_t *err) {
  if (!k_vmx_ok()) return -1;
  unsigned char *base = (unsigned char*)
    (((uintptr_t) mem + vmx_pg - 1) & ~(uintptr_t) (vmx_pg - 1));
  unsigned char *vmxon = base,
                *vmcs  = base + 1 * vmx_pg,
                *guest = base + 2 * vmx_pg,
                *gpd   = base + 3 * vmx_pg,   // the guest's 4 MiB-page directory
                *gdt   = base + 4 * vmx_pg,
                *tss   = base + 5 * vmx_pg,
                *ept4  = base + 6 * vmx_pg,   // the four EPT levels, pml4 down to pt
                *ept3  = base + 7 * vmx_pg,
                *ept2  = base + 8 * vmx_pg,
                *ept1  = base + 9 * vmx_pg;
  vmx_zero(base, vmx_pages * vmx_pg);

  // the guest, in 32-bit protected mode: load the sentinel, then CPUID, which
  // exits UNCONDITIONALLY under VMX -- there is no intercept bit to set, which
  // is one of the few places Intel asks for less than AMD. The hlt is a
  // backstop; reaching it would mean the exit did not happen.
  guest[0] = 0xb8;                                       // mov eax, imm32
  guest[1] = (unsigned char) (vmx_sentinel & 0xff);
  guest[2] = (unsigned char) (vmx_sentinel >> 8);
  guest[3] = 0; guest[4] = 0;
  guest[5] = 0x0f; guest[6] = 0xa2;                      // cpuid
  guest[7] = 0xf4;                                       // hlt

  // the guest's own paging: one directory of 4 MiB PSE pages mapping its linear
  // space onto its GUEST-PHYSICAL space, one to one. That is as far as the guest
  // can see; the EPT below decides what those addresses actually are.
  for (uint32_t i = 0; i < 1024; i++)
    *(uint32_t*) (gpd + 4 * i) = (i << 22) | 0x83u;      // present, write, PS

  // the EPT, four levels down to 4 KiB leaves, mapping exactly the two pages the
  // guest can reach: its code at guest-physical 0 and its page directory at
  // 0x1000. ⚠ a leaf carries a memory type in bits 5:3 where the upper levels
  // carry only the three permission bits -- write-back is 6, and an EPT with no
  // memory type is a refused entry.
  w64(ept4, 0, pa_of(ept3) | 0x7);                       // read | write | execute
  w64(ept3, 0, pa_of(ept2) | 0x7);
  w64(ept2, 0, pa_of(ept1) | 0x7);
  w64(ept1, 8 * (gpa_code >> 12), pa_of(guest) | 0x7 | (6u << 3));
  w64(ept1, 8 * (gpa_pd >> 12), pa_of(gpd) | 0x7 | (6u << 3));

  // our own GDT, because the host TR selector may not be 0 and inle has never
  // loaded a TR. The two boot descriptors are WRITTEN rather than copied out of
  // the live table: mkboot.l lays exactly these two words, and reading them
  // back would mean dereferencing a GDTR base that was loaded before paging.
  // A 64-bit TSS descriptor goes at index 3, which is what TR selects.
  // ⚠ every exit reloads GDTR from this table, so it is put back by hand at the
  // foot -- these pages are a love string's bytes, and the collector is free to
  // move them the moment we return.
  unsigned char gdtr[10], idtr[10];
  k_sgdt(gdtr);
  k_sidt(idtr);
  w64(gdt, 0x00, 0);
  w64(gdt, 0x08, 0x00209a0000000000ull);                 // mkboot.l's code selector
  w64(gdt, 0x10, 0x0000920000000000ull);                 // ..and its data twin
  // a 64-bit TSS descriptor: limit 0x67, type 9 (available), present.
  uint64_t tb = (uint64_t) (uintptr_t) tss;
  w64(gdt, 0x18, 0x67u | ((tb & 0xffffffu) << 16) | (0x89ull << 40)
                 | (((tb >> 24) & 0xffu) << 56));
  w64(gdt, 0x20, tb >> 32);

  // --- enable. CR4.VMXE first, then the fixed-bit reconciliation both control
  // registers owe, then the revision id every region has to be stamped with.
  uint64_t fc = k_rdmsr(msr_feature_control);
  if (!(fc & 1)) k_wrmsr(msr_feature_control, fc | 5);   // lock it on, ours to set
  uint64_t cr4 = k_rd_cr4() | (1ull << 13);              // CR4.VMXE
  cr4 = (cr4 | k_rdmsr(msr_vmx_cr4_fixed0)) & k_rdmsr(msr_vmx_cr4_fixed1);
  k_wr_cr4(cr4);
  // ⚠ AND THE SAME FOR CR0, WHICH HAS TO BE WRITTEN BACK AND NOT MERELY
  // COMPUTED: vmxon #GPs -- it does not fail, it FAULTS -- unless the live CR0
  // already satisfies IA32_VMX_CR0_FIXED0. That MSR demands NE, a bit this
  // kernel has never had a reason to set, so on Intel the very first vmxon
  // takes the machine down with a #GP at the instruction itself.
  uint64_t cr0 = (k_rd_cr0() | k_rdmsr(msr_vmx_cr0_fixed0))
                 & k_rdmsr(msr_vmx_cr0_fixed1);
  k_wr_cr0(cr0);

  uint64_t basic = k_rdmsr(msr_vmx_basic);
  *(uint32_t*) vmxon = (uint32_t) basic & 0x7fffffffu;
  *(uint32_t*) vmcs  = (uint32_t) basic & 0x7fffffffu;
  // ⚠ bit 55 says the TRUE_* capability MSRs exist; without it the four
  // ordinary ones are the only truth, and reading 0x48d on such a part faults.
  uint32_t true_msrs = (basic >> 55) & 1;

  uint64_t pa = pa_of(vmxon);
  k_vmxon(&pa);
  pa = pa_of(vmcs);
  k_vmclear(&pa);
  k_vmptrld(&pa);

  // --- the controls. CPUID needs no bit: it exits on its own.
  k_vmwrite(f_pin_ctl, ctl_fit(true_msrs ? msr_vmx_true_pin : msr_vmx_pinbased, 0));
  k_vmwrite(f_cpu_ctl, ctl_fit(true_msrs ? msr_vmx_true_proc : msr_vmx_procbased,
                               1u << 31));              // activate secondary controls
  // ..which is the only door to EPT. ⚠ the secondary word has no TRUE_ twin:
  // 0x48b is the whole truth about what this part will take.
  k_vmwrite(f_secondary_ctl, ctl_fit(msr_vmx_proc2, 1u << 1));
  // EPTP: the table, write-back (6), and a walk length of 4 given as 3.
  k_vmwrite(f_eptp, pa_of(ept4) | 6u | (3u << 3));
  k_vmwrite(f_exit_ctl, ctl_fit(true_msrs ? msr_vmx_true_exit : msr_vmx_exit,
                                1u << 9));               // host address-space size
  k_vmwrite(f_entry_ctl, ctl_fit(true_msrs ? msr_vmx_true_entry : msr_vmx_entry, 0));
  k_vmwrite(f_excep_bitmap, 0);
  k_vmwrite(f_cr3_target_count, 0);
  k_vmwrite(f_exit_msr_store, 0);
  k_vmwrite(f_exit_msr_load, 0);
  k_vmwrite(f_entry_msr_load, 0);
  k_vmwrite(f_entry_intr, 0);
  k_vmwrite(f_vpid, 0);
  k_vmwrite(f_cr0_mask, 0);
  k_vmwrite(f_cr4_mask, 0);
  // ⚠ the link pointer is ~0 and not 0. Zero is a valid-looking shadow VMCS
  // pointer, and the entry is refused for it.
  k_vmwrite(f_vmcs_link, ~0ull);

  // --- host state. Everything the exit will restore, which is everything
  // except the general registers: those come back as the guest left them.
  k_vmwrite(f_host_cr0, cr0);
  k_vmwrite(f_host_cr3, k_rd_cr3());
  k_vmwrite(f_host_cr4, cr4);
  k_vmwrite(f_host_cs_sel, 0x08);
  // es ss ds fs gs -- the host selector fields march 2 apart in the order
  // es cs ss ds fs gs tr, so cs's slot is the one skipped here.
  for (int s = 0; s < 7; s++)
    if (s != 1 && s != 6) k_vmwrite(f_host_es_sel + 2 * s, 0x10);
  k_vmwrite(f_host_tr_sel, vmx_tr_sel);
  k_vmwrite(f_host_tr_base, tb);
  k_vmwrite(f_host_gdtr_base, (uintptr_t) gdt);
  k_vmwrite(f_host_idtr_base, *(uint64_t const*) (idtr + 2));
  k_vmwrite(f_host_fs_base, 0);
  k_vmwrite(f_host_gs_base, 0);
  k_vmwrite(f_host_sysenter_cs, 0);
  k_vmwrite(f_host_sysenter_sp, 0);
  k_vmwrite(f_host_sysenter_ip, 0);

  // --- guest state. Every segment is based at 0 now: the code sits at
  // guest-physical 0 because the EPT put it there, so the guest's rip is a plain
  // offset and reads 5 at the exit, exactly as the SVM twin's does.
  uint64_t gcr0 = (0x80000021ull | k_rdmsr(msr_vmx_cr0_fixed0))
                  & k_rdmsr(msr_vmx_cr0_fixed1);         // PG | NE | PE
  uint64_t gcr4 = ((1ull << 4) | k_rdmsr(msr_vmx_cr4_fixed0))
                  & k_rdmsr(msr_vmx_cr4_fixed1);         // CR4.PSE
  k_vmwrite(f_guest_cr0, gcr0);
  k_vmwrite(f_guest_cr3, gpa_pd);                        // a GUEST-physical address now
  k_vmwrite(f_guest_cr4, gcr4);
  k_vmwrite(f_cr0_shadow, gcr0);
  k_vmwrite(f_cr4_shadow, gcr4);
  for (int s = 0; s < 8; s++) {
    k_vmwrite(f_guest_es_sel + 2 * s, s == seg_cs ? 0x08 :
                                      s == seg_tr ? vmx_tr_sel :
                                      s == seg_ldtr ? 0 : 0x10);
    k_vmwrite(f_guest_es_base + 2 * s, s == seg_tr ? tb : 0);
    k_vmwrite(f_guest_es_limit + 2 * s, s == seg_tr ? 0x67 : 0xffffffffu);
    // access rights: 32-bit code, 32-bit data, an UNUSABLE ldtr (bit 16), and a
    // busy 32-bit TSS -- ⚠ the guest's tr may not be unusable, so it gets a
    // real descriptor even though nothing will ever task-switch to it.
    k_vmwrite(f_guest_es_ar + 2 * s, s == seg_cs ? 0xc09b :
                                     s == seg_ldtr ? 0x10000 :
                                     s == seg_tr ? 0x8b : 0xc093);
  }
  k_vmwrite(f_guest_gdtr_base, (uintptr_t) gdt);
  k_vmwrite(f_guest_gdtr_limit, 0x2f);
  k_vmwrite(f_guest_idtr_base, 0);
  k_vmwrite(f_guest_idtr_limit, 0xffff);
  k_vmwrite(f_guest_dr7, 0x400);
  k_vmwrite(f_guest_rsp, 0);
  k_vmwrite(f_guest_rip, 0);
  k_vmwrite(f_guest_rflags, 0x2);                        // bit 1 reads 1, always
  k_vmwrite(f_guest_pending_dbg, 0);
  k_vmwrite(f_guest_intr_state, 0);
  k_vmwrite(f_guest_activity, 0);
  k_vmwrite(f_guest_sysenter_cs, 0);
  k_vmwrite(f_guest_sysenter_sp, 0);
  k_vmwrite(f_guest_sysenter_ip, 0);

  int bad = k_vmlaunch(rax);
  *reason = bad ? 0 : (k_vmread(f_exit_reason) & 0xffff);
  *rip = bad ? 0 : k_vmread(f_guest_rip);
  *err = bad ? k_vmread(f_vm_insn_error) : 0;
  // put the descriptor tables back before these pages stop being ours, and
  // leave VMX operation so a second ask starts from the same floor as the first.
  k_lgdt(gdtr);
  k_vmxoff();
  return bad ? 1 : 0; }
