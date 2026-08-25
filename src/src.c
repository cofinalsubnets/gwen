// src/src.c -- the artifact's own source, handed back out. host-only, auto-globbed
// + AiNif-registered (no love.c/love.h/main.c edit), the fs.c discipline:
//
//   (source-gz ()) -> the embedded love-<ver>.tar.gz bytes | () when none is baked in
//
// tools/mksrc.l lays the archive into an object as two .rodata symbols and the dist
// link pulls it in; love/verbs.l's `source` verb inflates what this answers. doc/misc/dist.md.
//
// presence rides the kind, not the net. an artifact with no source baked in and one
// carrying an empty archive must not read alike, and every nothing here is nil by
// design -- so absence is the zero point and any archive at all is a string, which
// `string?` separates even at zero bytes. `(! s)` would call both of them absent.
//
// the empty blob is defined here, weakly, and the dist link overrides it with a
// strong one -- ldsyms' "a strong def beats any weak", the same override test_moon
// already pins. so the symbols are always defined and the length alone says whether
// there is source: no null test, and nothing to link specially for a plain build.
// a weak *undefined* datum would not have worked, which is worth writing down: we
// link -pie, gcc reaches such a symbol through the got so it reads a true NULL, and
// ours emits a plain rip-relative lea -- which answers LOAD_BASE + 0, never null. the
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
 // no re-read after str0's collect: the source is .rodata, not the heap, so the
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
AiNif("source-gz", nif_srcgz);

// (runtime-gz "x64"|"arm64"|"riscv64") -> that ISA's nolibc archive, deflated;
// (runtime-gz "id") -> the pure tree-slice hash the archives were cut from
// (moon.l's rtcid). () when none is carried. tools/mkrt.l lays them, the
// same weak/strong law as the source blob above; moon.l's rtcarried consumes.
__attribute__((weak)) const unsigned char ai_rtgz_x64[1] = {0};
__attribute__((weak)) const uintptr_t ai_rtgz_x64_len = 0;
__attribute__((weak)) const unsigned char ai_rtgz_arm64[1] = {0};
__attribute__((weak)) const uintptr_t ai_rtgz_arm64_len = 0;
__attribute__((weak)) const unsigned char ai_rtgz_riscv64[1] = {0};
__attribute__((weak)) const uintptr_t ai_rtgz_riscv64_len = 0;
__attribute__((weak)) const unsigned char ai_rtgz_id[1] = {0};
__attribute__((weak)) const uintptr_t ai_rtgz_id_len = 0;

ai_noinline static struct ai *host_rtgz(struct ai *g) {
 const unsigned char *p = 0;
 uintptr_t n = 0;
 ai_word a = g->sp[0];
 if (ai_strp(a)) {
  const char *s = (const char*) txt(a);
  uintptr_t sl = len(a);
  if (sl == 3 && !memcmp(s, "x64", 3))          p = ai_rtgz_x64,     n = ai_rtgz_x64_len;
  else if (sl == 5 && !memcmp(s, "arm64", 5))   p = ai_rtgz_arm64,   n = ai_rtgz_arm64_len;
  else if (sl == 7 && !memcmp(s, "riscv64", 7)) p = ai_rtgz_riscv64, n = ai_rtgz_riscv64_len;
  else if (sl == 2 && !memcmp(s, "id", 2))      p = ai_rtgz_id,      n = ai_rtgz_id_len; }
 if (!n) return g->sp[0] = ZeroPoint, g;
 if (!ai_ok(g = str0(g, n))) return g;
 memcpy(txt(g->sp[0]), p, (size_t) n);          // .rodata: no re-read after the collect
 g->sp[1] = g->sp[0];
 g->sp += 1;
 return g; }
static lvm(lvm_rtgz) {
 Pack(g); g = host_rtgz(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

static union u const nif_rtgz[] = {{lvm_rtgz}, {lvm_ret0}};
AiNif("runtime-gz", nif_rtgz);
