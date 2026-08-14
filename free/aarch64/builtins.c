// the two compiler-rt builtins the freestanding aarch64 build needs and cannot
// get. the kernel links -nostdlib against our own libc/, which ships no
// builtins, so a call clang emits to the runtime library is simply undefined at
// link time. on x86_64 neither of these ever comes up: love.c's div2by1 takes an
// inline `divq` there, and the I-cache is coherent with the D-cache. aarch64 has
// no 128-bit divide instruction and a non-coherent I-cache, so both become real
// calls -- and the aarch64 kernel did not link until this file existed.
#include <stdint.h>
#include <stddef.h>
#include "asmops.h"                    // the privileged instructions, both spellings

// --- __clear_cache: make freshly written bytes safe to execute ---------------
// the glaze JITs into the (RWX) heap on the freestanding path; on aarch64 the
// I-cache is NOT coherent with the D-cache, so without this the core happily
// runs whatever stale bytes the I-cache already held. clean the data lines to
// the point of unification, then invalidate the instruction lines over the same
// range. line sizes are per-CPU: CTR_EL0 carries them as log2-words, DminLine in
// bits [19:16] and IminLine in [3:0], so 4 << field is the size in bytes.
//
// NOT stub-able. a no-op links fine and then miscompiles at runtime, on real
// silicon only, the first time the glaze emits code -- which is why this is a
// real implementation and not an empty body.
void __clear_cache(void *start, void *end);
void __clear_cache(void *start, void *end) {
  uint64_t ctr = k_rd_ctr_el0();
  uintptr_t dline = (uintptr_t) 4 << ((ctr >> 16) & 0xf);
  uintptr_t iline = (uintptr_t) 4 << (ctr & 0xf);
  uintptr_t lo = (uintptr_t) start, hi = (uintptr_t) end, p;

  for (p = lo & ~(dline - 1); p < hi; p += dline)
    k_dc_cvau(p);
  k_dsb_ish();                                  // the cleans must land before the invalidates
  for (p = lo & ~(iline - 1); p < hi; p += iline)
    k_ic_ivau(p);
  k_dsb_ish();                                  // ...and those before the fetch
  k_isb(); }

// --- __udivti3 / __umodti3: unsigned 128-bit division ------------------------
// love.c's div2by1 (the 128/64 step inside Knuth-D long division and the decimal
// printer) is one `divq` on x86-64; off it, the #else branch is a plain __int128
// divide and clang emits this call.
//
// restoring binary long division, with the divisor pre-aligned to the dividend's
// most significant bit -- so the loop runs clz(b) - clz(a) + 1 times, not a flat
// 128. love.c's caller guarantees hi < d, which puts the count near 64. the
// both-halves-fit-in-64 case skips the loop entirely for a single hardware udiv.
//
// no __int128 division appears below, only shifts, compares and subtracts, so
// this cannot recurse back into itself.
static int clz128(unsigned __int128 x) {              // x != 0
  uint64_t hi = (uint64_t) (x >> 64);
  return hi ? __builtin_clzll(hi) : 64 + __builtin_clzll((uint64_t) x); }

static unsigned __int128 udiv128(unsigned __int128 a, unsigned __int128 b,
                                 unsigned __int128 *rem) {
  if (!b) return *rem = 0, (unsigned __int128) 0;     // undefined in C; answer 0 rather than fault
  if (b > a) return *rem = a, (unsigned __int128) 0;
  if (!(a >> 64))                                     // both fit a word: one udiv, no loop
    return *rem = (uint64_t) a % (uint64_t) b, (unsigned __int128) ((uint64_t) a / (uint64_t) b);

  unsigned __int128 q = 0;
  int sh = clz128(b) - clz128(a);                     // >= 0: b <= a, and aligning cannot overflow
  for (b <<= sh; sh >= 0; sh--, b >>= 1) {
    q <<= 1;
    if (a >= b) a -= b, q |= 1; }
  return *rem = a, q; }

unsigned __int128 __udivti3(unsigned __int128 a, unsigned __int128 b);
unsigned __int128 __udivti3(unsigned __int128 a, unsigned __int128 b) {
  unsigned __int128 rem;
  return udiv128(a, b, &rem); }

// clang usually folds a % b into a - (a / b) * b and never calls this, but the
// pair is cheap and a missing half is a link error at the worst moment.
unsigned __int128 __umodti3(unsigned __int128 a, unsigned __int128 b);
unsigned __int128 __umodti3(unsigned __int128 a, unsigned __int128 b) {
  unsigned __int128 rem;
  return udiv128(a, b, &rem), rem; }
