// asmops -- the aarch64 privileged instructions, one static inline each, in
// BOTH inline-asm spellings. the x86_64 twin (port/inle/x86_64/asmops.h) opens
// with the why; the short version is that the kernel has to say the same thing
// to two compilers, clang in GNU's ARM template and mooncc in holo's NEUTRAL
// text (crew/holo/text.l -- mnemonic, then operands, one instruction per LINE,
// rN = xN), so the spelling lives here and the call sites say the NAME.
//
// aarch64 shares more than x86 does: `mrs`/`msr`/`dc`/`ic`/`at`/`dsb`/`isb`/
// `wfi` all read identically in the two dialects once each compiler has put its
// own register name into %0. what diverges is small and enumerable:
//
// * the `#` on an immediate -- ARM writes `brk #0`, the neutral surface `brk 0`.
// * the ops holo NAMES differently: `msri` for the PSTATE-field immediate (a
//   different instruction from a register `msr`, and holo does not overload the
//   mnemonic), and `dbrk` for `hlt #imm` (bare `hlt` is x86's, and one op name
//   carries one arity tree-wide).
// * `tlbi`/`ic`/`dc`/`at` are one SYS encoding in holo, so the whole-system
//   forms still take their register slot -- `tlbi vmalle1is, zr` where GNU
//   writes `tlbi vmalle1is`.
// * `mov` CANNOT SAY SP here. it is ORR against XZR, where encoding 31 reads as
//   the zero register: `mov x9, sp` would assemble as x9 <- 0. the SP move is
//   `add Xd, Xn, #0`, which is holo's `lea d, s, 0` (holo scares on the wrong
//   one rather than emitting it).
// * a multi-instruction template separates on \n, NEVER `;` -- the neutral
//   reader takes `;` as a comment to end of line and would silently drop
//   everything after the first instruction. GNU is happy with \n either way.
#pragma once
#include <stdint.h>

// --- barriers and the wait --------------------------------------------
// these spell the same in both dialects.
static inline void k_isb(void)     { asm volatile ("isb" ::: "memory"); }
static inline void k_dsb_ish(void) { asm volatile ("dsb ish" ::: "memory"); }
// the idle wait, kmain's kwait; the x86_64 twin of this name is `hlt`.
static inline void k_wait(void)    { asm volatile ("wfi"); }

// --- system register reads --------------------------------------------
// `mrs %0, <reg>` is shared: holo carries the sysreg names in its own table
// (crew/holo/arm64.l), so the line the assembler sees is the line written here.
static inline uint64_t k_rd_ctr_el0(void) {
  uint64_t v; asm volatile ("mrs %0, ctr_el0" : "=r"(v)); return v; }
static inline uint64_t k_rd_mair_el1(void) {
  uint64_t v; asm volatile ("mrs %0, mair_el1" : "=r"(v)); return v; }
static inline uint64_t k_rd_ttbr1_el1(void) {
  uint64_t v; asm volatile ("mrs %0, ttbr1_el1" : "=r"(v)); return v; }
static inline uint64_t k_rd_cntfrq_el0(void) {
  uint64_t v; asm volatile ("mrs %0, cntfrq_el0" : "=r"(v)); return v; }

// --- system register writes -------------------------------------------
static inline void k_wr_cntp_tval_el0(uint64_t v) {
  asm volatile ("msr cntp_tval_el0, %0" :: "r"(v)); }
static inline void k_wr_cntp_ctl_el0(uint64_t v) {
  asm volatile ("msr cntp_ctl_el0, %0" :: "r"(v)); }
// the vector base, with the isb that makes it live before the next fetch.
static inline void k_wr_vbar_el1(uintptr_t v) {
  asm volatile ("msr vbar_el1, %0\n isb" :: "r"(v) : "memory"); }

// --- PSTATE fields (the msr-IMMEDIATE form) ---------------------------
// a different instruction from the register msr above -- holo calls it `msri`.
static inline void k_daif_mask_all(void) {          // D, A, I, F all masked
#ifdef __mooncc__
  asm volatile ("msri daifset, 15");
#else
  asm volatile ("msr daifset, #0xf");
#endif
}

static inline void k_daif_unmask_irq(void) {        // DAIF.I = 0
#ifdef __mooncc__
  asm volatile ("msri daifclr, 2");
#else
  asm volatile ("msr daifclr, #2");
#endif
}

// --- address translation and the TLB ----------------------------------
// AT writes its answer to PAR_EL1, and the isb is what makes it readable.
static inline uint64_t k_at_s1e1w_par(void *va) {
  uint64_t par;
  asm volatile ("at s1e1w, %1\n isb\n mrs %0, par_el1"
                : "=r"(par) : "r"(va) : "memory");
  return par; }

// invalidate the whole EL1 TLB, inner-shareable, with the barriers that make
// a just-written descriptor visible before and the new mapping usable after.
static inline void k_tlbi_all(void) {
#ifdef __mooncc__
  asm volatile ("dsb ish\n tlbi vmalle1is, zr\n dsb ish\n isb" ::: "memory");
#else
  asm volatile ("dsb ish\n tlbi vmalle1is\n dsb ish\n isb" ::: "memory");
#endif
}

// --- cache maintenance (__clear_cache's two halves) -------------------
// shared: an operation name then a register is the same line in both dialects.
static inline void k_dc_cvau(uintptr_t p) {         // clean D to unification
  asm volatile ("dc cvau, %0" :: "r"(p) : "memory"); }
static inline void k_ic_ivau(uintptr_t p) {         // invalidate I
  asm volatile ("ic ivau, %0" :: "r"(p) : "memory"); }

// --- bring-up ---------------------------------------------------------
// EL1t -> EL1h: exception entry always switches to SP_EL1, so SP_EL1 must
// point at the current stack before anything can fault. SP_EL1 cannot be
// written by `msr` at EL1 (that is EL2+), so the idiom is capture SP, select
// SP_EL1, write SP. ONE asm block, so SP is never live-but-garbage across a
// memory access; the value is unchanged across the switch, so C carries on.
static inline void k_sp_to_el1h(void) {
#ifdef __mooncc__
  asm volatile ("lea r9, sp, 0\n msri spsel, 1\n lea sp, r9, 0"
                ::: "r9", "memory");
#else
  asm volatile ("mov x9, sp\n msr spsel, #1\n mov sp, x9"
                ::: "x9", "memory");
#endif
}

// CPACR_EL1.FPEN = 0b11, so C may use doubles. a bootloader can leave FPEN
// clear at EL1, which traps on the first FP register access.
// the neutral half materializes the mask into a register: holo's bitmask
// immediate encoder takes bottom-aligned runs only, and 3 << 20 is rotated.
static inline void k_fpen_enable(void) {
#ifdef __mooncc__
  asm volatile ("mrs r9, cpacr_el1\n"
                "li r10, 3145728\n"                 // 3 << 20
                "or r9, r9, r10\n"
                "msr cpacr_el1, r9\n"
                "isb"
                ::: "r9", "r10", "memory");
#else
  asm volatile ("mrs x9, cpacr_el1\n"
                "orr x9, x9, #(3 << 20)\n"
                "msr cpacr_el1, x9\n"
                "isb"
                ::: "x9", "memory");
#endif
}

// --- deliberate faults (the `fault` builtin's backend) ----------------
static inline void k_brk0(void) {
#ifdef __mooncc__
  asm volatile ("brk 0");
#else
  asm volatile ("brk #0");
#endif
}

static inline void k_udf0(void) {
#ifdef __mooncc__
  asm volatile ("udf 0");
#else
  asm volatile ("udf #0");
#endif
}

// --- the two hypercall-shaped exits -----------------------------------
// ARM semihosting SYS_EXIT: x0 the operation, x1 the {reason, code} block.
// `hlt #0xf000` is the semihosting trap -- holo calls it `dbrk`, since bare
// `hlt` is x86's and an op name carries one arity tree-wide. GNU pins the two
// registers by declaring them; the neutral surface pins by constraint name.
static inline void k_semihost_exit(volatile uint64_t *block) {
#ifdef __mooncc__
  uint64_t op = 0x18, arg = (uint64_t) (uintptr_t) block;
  asm volatile ("dbrk 61440" :: "r0"(op), "r1"(arg) : "memory");   // 61440 = 0xf000
#else
  register uint64_t op asm("x0") = 0x18;
  register uint64_t arg asm("x1") = (uint64_t) (uintptr_t) block;
  asm volatile ("hlt #0xf000" :: "r"(op), "r"(arg) : "memory");
#endif
}

// PSCI SYSTEM_RESET over HVC (what QEMU's 'virt' exposes). the function id
// rides x0 in and out.
static inline void k_psci_system_reset(void) {
  uint64_t fn = 0x84000009;
#ifdef __mooncc__
  asm volatile ("hvc 0" : "+r0"(fn) :: "memory");
#else
  register uint64_t r0 asm("x0") = fn;
  asm volatile ("hvc #0" : "+r"(r0) :: "memory");
  fn = r0;
#endif
  (void) fn; }
