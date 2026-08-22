/* test/nif/stub.h -- the runtime the host nifs name, and nothing else.
 *
 * a nif file is an ALGORITHM plus a love-facing wrapper. the algorithm is what
 * this corpus differentials, and it is reachable by including the .c: every
 * entry point is a static, so a harness that includes the file sees all of them
 * and needs no seam cut into host/.
 *
 * what the wrapper needs is a handful of runtime symbols -- the string/cask
 * predicates, the allocator door, and four lvm ops the nif's dispatch row names.
 * none of them is called here: main() enters at the algorithm, never at the lvm.
 * they exist so the object LINKS, which is why every body is a stub and why a
 * stub answering nonsense is harmless.
 *
 * ⚠ include this AFTER the nif source. the types are love.h's and the
 * declarations are love.h's; this only lays the bodies. */
#ifndef NIF_STUB_H
#define NIF_STUB_H
#include "love.h"

const struct ai_mint ai_mint_zero = {0, 0};

bool ai_strp(ai_word w) { (void) w; return false; }
struct ai *ai_strof(struct ai *g, const char *s) { (void) s; return g; }
struct ai *str0(struct ai *g, uintptr_t n) { (void) n; return g; }

lvm(lvm_ret0) { return g; }
lvm(lvm_cur) { return g; }
lvm(lvm_cask) { return g; }
lvm(_lvm_ghelp) { return g; }

#endif
