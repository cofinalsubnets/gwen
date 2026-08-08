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
