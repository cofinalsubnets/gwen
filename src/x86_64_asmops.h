// asmops -- the x86_64 privileged instructions, one static inline each, in BOTH
// inline-asm spellings.
//
// the kernel is the last place in the tree that talks to the machine in
// assembler, and it has to say the same thing to two compilers: clang wants
// GNU's AT&T template, mooncc wants holo's NEUTRAL text (crew/holo/text.l --
// mnemonic, then operands, one instruction per LINE; the registers are the
// neutral file r0=rax r1=rcx r2=rdx r3=rbx r5=rsi r6=rdi r7..r14=r8..r15).
// so the spelling lives HERE, once per operation, and every call site says the
// operation's NAME. that is what keeps the clang build alive as the differential
// twin after the flip: both halves compile the same kernel, and K_TEST must
// agree across them.
//
// two things are worth knowing before editing:
//
// * MOST OF IT IS SHARED. a bare mnemonic (`cli`) and a `mnemonic op, op` line
//   read the same in both dialects once each compiler has substituted its own
//   register names into %0 -- so those lines carry no #ifdef at all. the
//   divergences are exactly three: AT&T's operand order and its constraint
//   letters ("a"/"Nd" pin by letter where the neutral surface pins by register
//   name, "r0"/"r2"), the ops holo NAMES differently (trap for int3, ldcr/stcr
//   for the control-register moves), and immediates (AT&T's $ / ARM's #).
// * A MULTI-INSTRUCTION TEMPLATE SEPARATES ON \n, NEVER `;`. the neutral reader
//   takes `;` as a COMMENT to end of line, so a `;`-joined template would
//   assemble its first instruction and silently drop the rest. GNU is happy
//   with \n either way, so \n is the form that serves both.
#pragma once
#include <stdint.h>

// --- the interrupt flag, and the wait ---------------------------------
// `cli` and `hlt` spell the same in both dialects.
static inline void k_cli(void) { asm volatile ("cli"); }
static inline void k_sti(void) { asm volatile ("sti"); }
// the idle wait, kmain's kwait: halt until the next interrupt. the aarch64
// twin of this name is `wfi`, which is the whole reason kmain calls k_wait()
// and not either mnemonic.
static inline void k_wait(void) { asm volatile ("hlt"); }

// --- control registers ------------------------------------------------
// #PF reports the faulting address in CR2. holo names the control-register
// moves ldcr/stcr (crN is an operand, not part of the mnemonic).
static inline uint64_t k_rd_cr2(void) {
  uint64_t v;
#ifdef __mooncc__
  asm volatile ("ldcr %0, 2" : "=r"(v));
#else
  asm volatile ("mov %%cr2, %0" : "=r"(v));
#endif
  return v; }

// VMX needs the host's own CR0/CR3/CR4 written into the VMCS, and CR4.VMXE set
// before vmxon -- so the control registers grew readers beside CR2's.
static inline uint64_t k_rd_cr0(void) {
  uint64_t v;
#ifdef __mooncc__
  asm volatile ("ldcr %0, 0" : "=r"(v));
#else
  asm volatile ("mov %%cr0, %0" : "=r"(v));
#endif
  return v; }

static inline uint64_t k_rd_cr3(void) {
  uint64_t v;
#ifdef __mooncc__
  asm volatile ("ldcr %0, 3" : "=r"(v));
#else
  asm volatile ("mov %%cr3, %0" : "=r"(v));
#endif
  return v; }

static inline uint64_t k_rd_cr4(void) {
  uint64_t v;
#ifdef __mooncc__
  asm volatile ("ldcr %0, 4" : "=r"(v));
#else
  asm volatile ("mov %%cr4, %0" : "=r"(v));
#endif
  return v; }

static inline void k_wr_cr0(uint64_t v) {
#ifdef __mooncc__
  asm volatile ("stcr 0, %0" :: "r"(v) : "memory");
#else
  asm volatile ("mov %0, %%cr0" :: "r"(v) : "memory");
#endif
}

static inline void k_wr_cr4(uint64_t v) {
#ifdef __mooncc__
  asm volatile ("stcr 4, %0" :: "r"(v) : "memory");
#else
  asm volatile ("mov %0, %%cr4" :: "r"(v) : "memory");
#endif
}

// CR0.EM=0 / CR0.MP=1 and CR4.OSFXSR|OSXMMEXCPT: enable x87/SSE. this runs
// before ANY other C in kmain -- neither compiler guarantees it will not emit
// an SSE instruction (a struct copy is enough), and one of those #UDs into a
// triple fault with no output while SSE is still masked. ONE asm block, with
// the "memory" clobber holding any vectorized access below it.
static inline void k_sse_enable(void) {
#ifdef __mooncc__
  asm volatile (
    "ldcr r0, 0\n"
    "and r0, r0, -5\n"                 // CR0.EM = 0
    "or r0, r0, 2\n"                   // CR0.MP = 1
    "stcr 0, r0\n"
    "ldcr r0, 4\n"
    "or r0, r0, 1536\n"                // CR4.OSFXSR | CR4.OSXMMEXCPT
    "stcr 4, r0"
    ::: "r0", "memory");
#else
  asm volatile (
    "mov %%cr0, %%rax\n\t"
    "and $~(1 << 2), %%rax\n\t"        // CR0.EM = 0
    "or  $(1 << 1), %%rax\n\t"         // CR0.MP = 1
    "mov %%rax, %%cr0\n\t"
    "mov %%cr4, %%rax\n\t"
    "or  $((1 << 9) | (1 << 10)), %%rax\n\t"   // CR4.OSFXSR | CR4.OSXMMEXCPT
    "mov %%rax, %%cr4"
    ::: "rax", "memory");
#endif
}

// --- model-specific registers -----------------------------------------
// rdmsr/wrmsr are register-CONTRACTED like the port ops: the MSR number rides
// ecx and the value edx:eax, so the neutral half pins by register name (r0=rax,
// r1=rcx, r2=rdx) where AT&T pins by letter.
static inline uint64_t k_rdmsr(uint32_t msr) {
  uint32_t lo, hi;
#ifdef __mooncc__
  asm volatile ("rdmsr" : "=r0"(lo), "=r2"(hi) : "r1"(msr));
#else
  asm volatile ("rdmsr" : "=a"(lo), "=d"(hi) : "c"(msr));
#endif
  return ((uint64_t) hi << 32) | lo; }

static inline void k_wrmsr(uint32_t msr, uint64_t v) {
#ifdef __mooncc__
  asm volatile ("wrmsr" :: "r0"((uint32_t) v), "r2"((uint32_t) (v >> 32)), "r1"(msr));
#else
  asm volatile ("wrmsr" :: "a"((uint32_t) v), "d"((uint32_t) (v >> 32)), "c"(msr));
#endif
}

// CPUID: the leaf rides eax, the answers come back in eax/ebx/ecx/edx.
//
// ⚠ EAX IS AN INPUT HERE AND NOT AN OUTPUT, which is a real limit and not a
// preference: mooncc refuses an output pinned to a register that is also an
// input (nothing spells GNU's tied "0" constraint on the neutral surface), so
// a caller wanting the eax answer -- the max-leaf probe is the only one that
// ever does -- has to ask a different question. `cpuid` still WRITES eax; that
// is safe unnamed because an asm function's homing is off, so mooncc keeps
// nothing in a register across the statement.
static inline void k_cpuid(uint32_t leaf, uint32_t *b, uint32_t *c, uint32_t *d) {
  uint32_t rb, rc, rd;
#ifdef __mooncc__
  asm volatile ("cpuid" : "=r3"(rb), "=r1"(rc), "=r2"(rd) : "r0"(leaf));
#else
  // GNU cannot clobber a register it is also given as an input, so eax is
  // named as the tied output it architecturally is, and then dropped.
  uint32_t ra;
  asm volatile ("cpuid" : "=a"(ra), "=b"(rb), "=c"(rc), "=d"(rd) : "0"(leaf));
  (void) ra;
#endif
  *b = rb; *c = rc; *d = rd; }

// --- SVM, the AMD-V lane (src/x86_64_svm.c) ---------------------
// vmrun/vmload/vmsave take the VMCB's PHYSICAL address in rax and name no
// operand on holo's surface; AT&T names %rax and LLVM prints it back bare, so
// the two halves disassemble alike.
//
// ⚠ THE CLOBBER LIST IS THE CONTRACT. #VMEXIT restores RAX, RSP, RIP, RFLAGS,
// the segments and the control registers from the host save area -- and NO
// other GPR. rbx/rcx/rdx/rsi/rdi/r8..r15 come back holding whatever the guest
// left in them, so clang is told so by name. rbp is not nameable (it is the
// frame pointer), which is the standing reason a guest that runs real code
// wants a save/restore stub around vmrun rather than this inline.
//
// ⚠ the two halves DIVERGE, and legitimately: mooncc's asm surface reaches
// only r0-r3 and r5-r10 (the frame and the callee-saved four are refused), so
// it could not name half the list -- and does not need to, because an asm
// function's homing is off and nothing of its own lives in a register across
// the statement. "memory" is the whole of what it has to be told.
#ifdef __mooncc__
#define k_svm_clobbers "memory"
#else
#define k_svm_clobbers "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", \
                       "r10", "r11", "r12", "r13", "r14", "r15", "memory"
#endif

static inline void k_vmrun(uint64_t vmcb_pa) {
#ifdef __mooncc__
  asm volatile ("vmrun" :: "r0"(vmcb_pa) : k_svm_clobbers);
#else
  asm volatile ("vmrun %%rax" :: "a"(vmcb_pa) : k_svm_clobbers);
#endif
}

// the host state vmrun does NOT save: FS, GS, TR, LDTR and the syscall MSRs.
// vmsave before the entry and vmload after it are what keep the kernel's own
// segment state off the guest's floor.
static inline void k_vmsave(uint64_t vmcb_pa) {
#ifdef __mooncc__
  asm volatile ("vmsave" :: "r0"(vmcb_pa) : "memory");
#else
  asm volatile ("vmsave %%rax" :: "a"(vmcb_pa) : "memory");
#endif
}

static inline void k_vmload(uint64_t vmcb_pa) {
#ifdef __mooncc__
  asm volatile ("vmload" :: "r0"(vmcb_pa) : "memory");
#else
  asm volatile ("vmload %%rax" :: "a"(vmcb_pa) : "memory");
#endif
}

// the global interrupt flag. ⚠ #VMEXIT leaves GIF CLEAR: between the exit and
// the stgi the machine takes no interrupt at all, so a missing stgi is a deaf
// machine wearing a hang's face. both spell the same in either dialect.
static inline void k_stgi(void) { asm volatile ("stgi" ::: "memory"); }
static inline void k_clgi(void) { asm volatile ("clgi" ::: "memory"); }

// --- VMX, the Intel lane (src/x86_64_vmx.c) ---------------------
// Nothing here is register-contracted the way SVM's ops are: vmxon, vmclear and
// vmptrld take a MEMORY operand holding a physical address. Both dialects spell
// those three the SAME way, with no #ifdef between them -- mooncc grew the "m"
// constraint for exactly this, and lowers it to the address in a register plus
// a zero displacement, which is how holo spells a memory operand (crew/moon/
// gen.l's asmc). sgdt/sidt/lgdt below ride the same door.
static inline void k_vmxon(uint64_t *pa) {
  asm volatile ("vmxon %0" :: "m"(*pa) : "cc", "memory"); }

static inline void k_vmclear(uint64_t *pa) {
  asm volatile ("vmclear %0" :: "m"(*pa) : "cc", "memory"); }

static inline void k_vmptrld(uint64_t *pa) {
  asm volatile ("vmptrld %0" :: "m"(*pa) : "cc", "memory"); }

static inline void k_vmxoff(void) { asm volatile ("vmxoff" ::: "cc", "memory"); }

// the field pair. ModRM.reg names the FIELD in both directions; the two
// dialects differ only in which operand they write first.
static inline uint64_t k_vmread(uint64_t field) {
  uint64_t v;
#ifdef __mooncc__
  asm volatile ("vmread %0, %1" : "=r"(v) : "r"(field) : "cc");
#else
  asm volatile ("vmread %1, %0" : "=r"(v) : "r"(field) : "cc");
#endif
  return v; }

static inline void k_vmwrite(uint64_t field, uint64_t v) {
#ifdef __mooncc__
  asm volatile ("vmwrite %0, %1" :: "r"(field), "r"(v) : "cc");
#else
  asm volatile ("vmwrite %1, %0" :: "r"(field), "r"(v) : "cc");
#endif
}

// the descriptor-table READS. lgdt/lidt were here from the bring-up; VMX is
// what needs to write the bases down before it can promise to restore them.
static inline void k_sgdt(void *p) {
  asm volatile ("sgdt %0" :: "m"(*(char (*)[10]) p) : "memory"); }

static inline void k_sidt(void *p) {
  asm volatile ("sidt %0" :: "m"(*(char (*)[10]) p) : "memory"); }

// ..and the write back, which VMX needs because every exit reloads GDTR from
// the VMCS and the table it names is not the one the boot laid.
static inline void k_lgdt(void const *p) {
  asm volatile ("lgdt %0" :: "m"(*(char const (*)[10]) p) : "memory"); }

// k_vmlaunch -- the entry, which on this vendor cannot be one instruction.
//
// ⚠ A VM EXIT DOES NOT RESUME AFTER `vmlaunch`. It resumes at the HOST_RIP in
// the VMCS with the HOST_RSP in the VMCS, so this block writes both to its own
// label and its own stack pointer first. That is the whole structural
// difference from SVM's `vmrun`, which simply came back.
//
// ⚠ And the two outcomes arrive at the same place by different roads: a
// REFUSED launch falls THROUGH to the next instruction, while a guest that ran
// and exited lands on the label. Only the marker register tells them apart --
// it is set to 1 before the launch and to 0 on the label.
//
// ⚠ VMX saves no guest GPR: rax is read at the label because by the next
// instruction it is gone. By the same token the host's own GPRs are NOT
// restored on exit (rsp and rip are, and nothing else), so a guest that writes
// more than rax wants a save/restore stub around this instead of an inline.
static inline int k_vmlaunch(uint64_t *guest_rax) {
  uint64_t bad, grax;
#ifdef __mooncc__
  asm volatile (
    "lea r0, sp, 0\n"          // the rsp the exit has to come back to
    "li r1, 27668\n"           // HOST_RSP  (0x6c14)
    "vmwrite r1, r0\n"
    "la r0, vmx-back\n"
    "li r1, 27670\n"           // HOST_RIP  (0x6c16)
    "vmwrite r1, r0\n"
    "li %0, 1\n"
    "vmlaunch\n"
    "jmp vmx-done\n"
    "label vmx-back\n"
    "mov %1, r0\n"             // the guest's rax, before anything else takes it
    "li %0, 0\n"
    "label vmx-done"
    : "=r"(bad), "=r2"(grax) :: "r0", "r1", "cc", "memory");
#else
  asm volatile (
    "movq %%rsp, %%rax\n"
    "movq $0x6c14, %%rcx\n"
    "vmwrite %%rax, %%rcx\n"
    "leaq 1f(%%rip), %%rax\n"
    "movq $0x6c16, %%rcx\n"
    "vmwrite %%rax, %%rcx\n"
    "movq $1, %0\n"
    "vmlaunch\n"
    "jmp 2f\n"
    "1:\n"
    "movq %%rax, %1\n"
    "movq $0, %0\n"
    "2:"
    : "=r"(bad), "=r"(grax) :: "rax", "rcx", "cc", "memory");
#endif
  *guest_rax = grax;
  return (int) bad; }

// --- port I/O ---------------------------------------------------------
// holo's in/out are register-CONTRACTED and take no operands: the port is in
// dx, the datum in al/ax/eax. so the neutral half pins by register name where
// AT&T pins by constraint letter ("a" is al/eax, "Nd" is dx or a byte
// immediate) -- same two registers, said two ways.
static inline void k_outb(uint16_t port, uint8_t v) {
#ifdef __mooncc__
  asm volatile ("outb" :: "r0"(v), "r2"(port));
#else
  asm volatile ("outb %0, %1" :: "a"(v), "Nd"(port));
#endif
}

static inline uint8_t k_inb(uint16_t port) {
  uint8_t v;
#ifdef __mooncc__
  asm volatile ("inb" : "=r0"(v) : "r2"(port));
#else
  asm volatile ("inb %1, %0" : "=a"(v) : "Nd"(port));
#endif
  return v; }

static inline void k_outl(uint16_t port, uint32_t v) {
#ifdef __mooncc__
  asm volatile ("outl" :: "r0"(v), "r2"(port));
#else
  asm volatile ("outl %0, %w1" :: "a"(v), "Nd"(port));
#endif
}

static inline uint32_t k_inl(uint16_t port) {
  uint32_t v;
#ifdef __mooncc__
  asm volatile ("inl" : "=r0"(v) : "r2"(port));
#else
  asm volatile ("inl %1, %0" : "=a"(v) : "Nd"(port));
#endif
  return v; }

// --- deliberate faults (the `fault` builtin's backend) ----------------
// int3 is CC, the one-byte breakpoint, which holo calls `trap`; the two-byte
// CD 03 is holo's `int 3` and a different instruction under vm86. we want the
// short one, the same one every debugger plants.
static inline void k_int3(void) {
#ifdef __mooncc__
  asm volatile ("trap");
#else
  asm volatile ("int3");
#endif
}

static inline void k_ud2(void) { asm volatile ("ud2"); }

// #DE: 1 / 0. the divisor register is zeroed right here rather than passed in,
// so nothing about the caller can make this NOT fault.
static inline void k_divzero(void) {
#ifdef __mooncc__
  asm volatile (
    "li r0, 1\n"
    "li r1, 0\n"
    "udiv r0, r0, r1"
    ::: "r0", "r1", "r2", "cc");
#else
  asm volatile ("xorl %%edx,%%edx; movl $1,%%eax; xorl %%ecx,%%ecx;"
                "divl %%ecx" ::: "eax","ecx","edx");
#endif
}
