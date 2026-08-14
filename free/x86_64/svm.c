// svm -- the AMD-V spike (doc/svm.md): can a guest run under inle, and does the
// exit land back in ordinary C? Enable SVM, build one VMCB, run a six-byte
// real-mode guest that loads a sentinel and executes CPUID, and take the
// intercept. The answer comes back as three numbers the gate can read.
//
// The question this file exists to answer is about the EXIT PATH, not about
// virtualization: `vmrun` returns to the instruction after itself, so the
// handler is straight-line C right here and the love VM above never learns a
// guest ran. That shape is AMD's -- the Intel twin has to stand up a host-RIP
// entry point of its own to say the same thing.
//
// ⚠ every physical address is `va - khhdm`, which holds for kernel-heap memory
// and NOT for image statics -- blk.c's law, same reason. The caller hands in
// one k_svm_need() block and everything below is carved out of it.
// ⚠ this file keeps NO state: the block lives in kmain's hand, the CPU's own
// enable bit is idempotent, and the VMCB is rebuilt on every run.
#include "k.h"
#include "asmops.h"
#include <stdint.h>

// --- the VMCB, by byte offset (AMD APM vol 2 appendix B; Linux's
// arch/x86/include/asm/svm.h is the same table). Control area at 0, guest save
// state at 0x400. Written by offset rather than through a packed struct, which
// is blk.c's idiom for a layout the hardware owns.
#define vmcb_icept3   0x00c    // INTR..SHUTDOWN: CPUID is bit 18, HLT bit 24
#define vmcb_icept4   0x010    // VMRUN is bit 0
#define vmcb_iopm     0x040
#define vmcb_msrpm    0x048
#define vmcb_asid     0x058
#define vmcb_exitcode 0x070
#define vmcb_es       0x400    // a segment is u16 sel, u16 attrib, u32 limit, u64 base
#define vmcb_cs       0x410
#define vmcb_ss       0x420
#define vmcb_ds       0x430
#define vmcb_cpl      0x4cb
#define vmcb_efer     0x4d0
#define vmcb_cr4      0x548
#define vmcb_cr3      0x550
#define vmcb_cr0      0x558
#define vmcb_rflags   0x570
#define vmcb_rip      0x578
#define vmcb_rax      0x5f8

#define msr_efer      0xc0000080u
#define msr_vm_cr     0xc0010114u
#define msr_vm_hsave  0xc0010117u
#define efer_svme     (1ull << 12)

#define svm_pg        4096u
// hsave, the guest VMCB, the host VMCB and the guest's own page are one page
// each; the IOPM is three by the architecture and the MSRPM two.
#define svm_pages     9u
// the sentinel the guest loads into AX before its CPUID. Reading it back out of
// the VMCB is what proves a guest instruction RETIRED, rather than the entry
// having merely been accepted.
#define svm_sentinel  0x1234u

uintptr_t k_svm_need(void) { return svm_pages * svm_pg + svm_pg - 1; }

static void svm_zero(unsigned char *p, uintptr_t n) { while (n--) *p++ = 0; }
static void w16(unsigned char *p, uintptr_t off, uint16_t v) {
  *(uint16_t*) (p + off) = v; }
static void w32(unsigned char *p, uintptr_t off, uint32_t v) {
  *(uint32_t*) (p + off) = v; }
static void w64(unsigned char *p, uintptr_t off, uint64_t v) {
  *(uint64_t*) (p + off) = v; }
static uint64_t r64(unsigned char const *p, uintptr_t off) {
  return *(uint64_t const*) (p + off); }
static uint64_t pa_of(void const *va) { return (uint64_t) ((uintptr_t) va - khhdm); }

// one VMCB segment: selector, the packed attribute pair, limit, base.
static void wseg(unsigned char *v, uintptr_t off, uint16_t attr, uint64_t base) {
  w16(v, off, 0); w16(v, off + 2, attr);
  w32(v, off + 4, 0xffff); w64(v, off + 8, base); }

// Does this machine offer SVM at all? CPUID first, then VM_CR -- in that order,
// because the MSR does not exist on a CPU whose CPUID does not claim SVM.
// ⚠ firmware can leave the feature bit up and SVMDIS set, and then the EFER
// write below #GPs: a triple fault, in a kernel with no handler for one. Asking
// is the whole difference between an absence and a dead machine.
//
// The usual max-extended-leaf probe is absent because k_cpuid cannot answer eax
// (asmops.h says why), and it would buy nothing: Fn8000_000A is defined exactly
// when Fn8000_0001_ECX.SVM is set, so the feature bit already carries it.
bool k_svm_ok(void) {
  uint32_t b, c, d;
  k_cpuid(0x80000001u, &b, &c, &d);
  if (!(c & (1u << 2))) return false;                 // ECX.SVM
  if (k_rdmsr(msr_vm_cr) & (1u << 4)) return false;   // VM_CR.SVMDIS
  k_cpuid(0x8000000au, &b, &c, &d);
  return b > 1; }                                     // ASIDs: we use 1

// Run one guest. Answers 0 and fills the three outs, or -1 if the machine has
// no SVM to run it with.
int k_svm_spike(void *mem, uint64_t *exitcode, uint64_t *rax, uint64_t *rip) {
  if (!k_svm_ok()) return -1;
  unsigned char *base = (unsigned char*)
    (((uintptr_t) mem + svm_pg - 1) & ~(uintptr_t) (svm_pg - 1));
  unsigned char *hsave = base,
                *vmcb  = base + 1 * svm_pg,
                *hvmcb = base + 2 * svm_pg,
                *guest = base + 3 * svm_pg,
                *iopm  = base + 4 * svm_pg,
                *msrpm = base + 7 * svm_pg;
  svm_zero(base, svm_pages * svm_pg);

  // the guest, in real mode: load the sentinel, then CPUID, which we intercept.
  // So the saved RAX is the sentinel and the saved RIP is the CPUID's OWN
  // address (an instruction intercept reports the instruction, not the one
  // after it) -- three facts the gate can tell apart. The hlt is a backstop and
  // is intercepted too; reaching it would mean the intercept did not fire.
  guest[0] = 0xb8;                                    // mov ax, imm16
  guest[1] = (unsigned char) (svm_sentinel & 0xff);
  guest[2] = (unsigned char) (svm_sentinel >> 8);
  guest[3] = 0x0f; guest[4] = 0xa2;                   // cpuid
  guest[5] = 0xf4;                                    // hlt

  // the control area. ⚠ THREE of these are consistency checks wearing the face
  // of ordinary settings, and each one alone answers exit code -1 (INVALID)
  // with nothing else to say: the VMRUN intercept must be SET, the ASID must be
  // NON-ZERO, and (below) the guest's EFER.SVME must be set.
  w32(vmcb, vmcb_icept3, (1u << 18) | (1u << 24));    // CPUID, HLT
  w32(vmcb, vmcb_icept4, 1u);                         // VMRUN
  w64(vmcb, vmcb_iopm, pa_of(iopm));
  w64(vmcb, vmcb_msrpm, pa_of(msrpm));
  w32(vmcb, vmcb_asid, 1);

  // the guest save state. No nested paging and no guest paging, so a guest
  // physical address IS a host physical one -- which is why CS's base is the
  // code page's own physical address and RIP is 0.
  wseg(vmcb, vmcb_cs, 0x009b, pa_of(guest));          // present, code, readable
  wseg(vmcb, vmcb_es, 0x0093, 0);                     // present, data, writable
  wseg(vmcb, vmcb_ss, 0x0093, 0);
  wseg(vmcb, vmcb_ds, 0x0093, 0);
  vmcb[vmcb_cpl] = 0;
  w64(vmcb, vmcb_efer, efer_svme);
  w64(vmcb, vmcb_cr4, 0);
  w64(vmcb, vmcb_cr3, 0);
  w64(vmcb, vmcb_cr0, 0x10);                          // ET; PE and PG clear -> real mode
  w64(vmcb, vmcb_rflags, 0x2);                        // bit 1 reads 1, always
  w64(vmcb, vmcb_rip, 0);
  w64(vmcb, vmcb_rax, 0);

  // enabling is two writes: the feature bit, then where to park host state.
  k_wrmsr(msr_efer, k_rdmsr(msr_efer) | efer_svme);
  k_wrmsr(msr_vm_hsave, pa_of(hsave));

  // ⚠ the five-step entry, and every step is load-bearing. clgi first, so no
  // interrupt lands between the host save and the entry; vmsave for the state
  // vmrun does not carry; vmrun; vmload to take it back; and stgi LAST, because
  // #VMEXIT left GIF clear and until this runs the machine is deaf -- a missing
  // stgi is a dead timer that reads exactly like a hang inside the guest.
  k_clgi();
  k_vmsave(pa_of(hvmcb));
  k_vmrun(pa_of(vmcb));
  k_vmload(pa_of(hvmcb));
  k_stgi();

  *exitcode = r64(vmcb, vmcb_exitcode);
  *rax = r64(vmcb, vmcb_rax);
  *rip = r64(vmcb, vmcb_rip);
  return 0; }
