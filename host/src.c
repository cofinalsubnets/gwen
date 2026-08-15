// host/src.c -- the artifact's own source, handed back out. Host-only, auto-globbed
// + AI_NIF-registered (no love.c/love.h/main.c edit), the fs.c discipline:
//
//   (source-gz ()) -> the embedded love-<ver>.tar.gz bytes | () when none is baked in
//
// mk/tools/mksrc.l lays the archive into an object as two .rodata symbols and the dist
// link pulls it in; love/verbs.l's `source` verb inflates what this answers. doc/dist.md.
//
// ⚠ PRESENCE RIDES THE KIND, NOT THE NET. an artifact with no source baked in and one
// carrying an empty archive must not read alike, and every nothing here is nil by
// design -- so absence is the ZERO POINT and any archive at all is a STRING, which
// `string?` separates even at zero bytes. `(! s)` would call both of them absent.
//
// ⚠ THE EMPTY BLOB IS DEFINED HERE, WEAKLY, and the dist link overrides it with a
// strong one -- ldsyms' "a strong def beats any weak", the same override test_moon
// already pins. So the symbols are ALWAYS defined and the length alone says whether
// there is source: no null test, and nothing to link specially for a plain build.
// ⚠ A WEAK *UNDEFINED* DATUM WOULD NOT HAVE WORKED, which is worth writing down: we
// link -pie, gcc reaches such a symbol through the GOT so it reads a true NULL, and
// ours emits a plain rip-relative lea -- which answers LOAD_BASE + 0, never null. The
// test silently never fires and the length is read out of the ELF header.
#include "love.h"
#include <string.h>

__attribute__((weak)) const unsigned char ai_srcgz[1] = {0};
__attribute__((weak)) const uintptr_t ai_srcgz_len = 0;

ai_noinline static struct ai *host_srcgz(struct ai *g) {
 const unsigned char *p = ai_srcgz;
 uintptr_t n = ai_srcgz_len;
 if (!n) return g->sp[0] = ZeroPoint, g;
 if (!ai_ok(g = str0(g, n))) return g;             // pushes: the archive over the arg
 // ⚠ no re-read after str0's collect: the source is .rodata, not the heap, so the
 // pointer cannot have moved -- unlike the port in love.c's readn.
 if (n) memcpy(txt(g->sp[0]), p, (size_t) n);
 g->sp[1] = g->sp[0];
 g->sp += 1;
 return g; }
static lvm(lvm_srcgz) {
 Pack(g); g = host_srcgz(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

static union u const nif_srcgz[] = {{lvm_srcgz}, {lvm_ret0}};
AI_NIF("source-gz", nif_srcgz);
