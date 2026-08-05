// emscripten host shim for love.
//
// l's frontend contract (see g/g.h): the host must define ai_clock, the
// ai_stdin/ai_stdout ports, and the ai_fd_port_vt vtable that backs any port
// with fd >= 0. Here stdout's putc appends to a JS-visible byte buffer that
// the page drains via ai_out_ptr/len/reset; stdin always reads EOF (the
// REPL feeds source through ai_eval, not the stdin port). boot.l is
// embedded and evaluated once by ai_init.
#include "love.h"
#include <emscripten.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdnoreturn.h>

// the egg's three texts, one per ai_egg_ argument (love.h): the boot stitches the
// corpus rather than reading it whole -- p0 takes egg + p1, p1 takes the corpus.
static const char src_egg[] =
#include "egg.h"
;
static const char src_p1[] =
#include "p1.h"
;
static const char src_corpus[] =
#include "prel.h"
 " "
#include "ev.h"
;
static const char boot_ai[] =
  "(use 'uu) (: uu (from 'uu))"   // the library layers, ALL modules (registered in ai_init, loaded by
  "(use 'coin)"                   //   name; the corpus asserts on each): the uu kernel keeps its
  "(use 'rng)"                    //   one-name surface, then coin, rng, q, kanren in the old eval order
  "(use 'q)"
  "(use 'kanren)"
;
// the module sources, name-keyed (see host/main.c): registered before boot_ai evals
static const char src_uu[] =
#include "uu.h"
;
static const char src_coin[] =
#include "coin.h"
;
static const char src_rng[] =
#include "rng.h"
;
static const char src_q[] =
#include "q.h"
;
static const char src_kanren[] =
#include "kanren.h"
;

// 256K: a single ai_eval can emit a lot before the page drains it -- the
// whole test corpus (test_wasm) runs in one eval and prints ~25K of dots +
// the summary. _writen lands what fits and answers the count, so an overflowing
// eval truncates rather than overruns.
//
// ⚠ AND A FULL BUFFER SAYS SO. ai_stdout is a STATIC port: it carries no write
// run (rung 4), and zputc offers a refused byte twice before giving up, so what
// does not fit here really is on the floor. A bigger number would only move the
// cliff; the honest edge is to SAY the answer is short. out_tail is held back
// from the buffer for that one line, so a truncated eval reads as truncated
// instead of stopping mid-word.
//
// ⚠ AND IT SAYS ONLY WHAT IS TRUE -- WHICH IS NOT A BYTE COUNT. lvm_fputs answers
// a refusal by re-offering the whole remainder, and then the byte alone through
// zputc, twice: a device sees each lost byte many times over and cannot tell
// attempts from bytes. What it can tell is THAT it ran out, so that is all it says.
#define out_tail 64
static char     out_buf[1 << 18];
static uint32_t out_len;
static int      out_full;

static void out_note(void) {
  for (char const *s = "\n; output truncated -- the page's buffer is full\n"; *s; s++)
    out_buf[out_len++] = *s; }

uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? 0
       : ts.tv_sec * 1000u + ts.tv_nsec / 1000000u; }

// --- ports ----------------------------------------------------------------
// Output goes to out_buf; the page reads it back through the exports below.
static intptr_t _writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  (void) fp;
  uintptr_t cap = sizeof out_buf - out_tail,
            room = out_len < cap ? cap - out_len : 0,
            k = room < n ? room : n;
  memcpy(out_buf + out_len, src, k);
  out_len += (uint32_t) k;
  out_full |= k < n;
  return (intptr_t) k; }
static struct ai *_flush(struct ai *g) { return g; }

// No real stdin: every read is at the end (-1), never merely quiet -- the page
// feeds source through ai_eval, not the stdin port, so nothing is coming.
static intptr_t _readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  return (void) g, (void) dst, (void) n, -1; }

// fd values are nominal: all I/O routes through the vtable regardless. We
// just need fd >= 0 so the dispatcher picks ai_fd_port_vt over a synth slot.
struct ai_io ai_stdin  = { .ap = lvm_port_io, .fd = putcharm(0),
                         .ungetc_buf = putcharm(EOF) };
struct ai_io ai_stdout = { .ap = lvm_port_io, .fd = putcharm(1),
                         .ungetc_buf = putcharm(EOF) };
// No separate error stream in the browser host; route err to out's fd.
struct ai_io ai_stderr = { .ap = lvm_port_io, .fd = putcharm(1),
                         .ungetc_buf = putcharm(EOF) };
struct ai_port_vt const ai_fd_port_vt = { _flush, _writen, _readn, NULL };

// (exit n) -- a frontend nif, like main.c's and kmain.c's. The wasm host needs
// it for the same reason they do: the test harness aborts a failed assert with
// (exit 1), and -- subtler -- a closure captures its free globals at creation,
// so a body that merely MENTIONS `exit` (e.g. an assert's unrun fail branch)
// raises (scare 'missing 'exit) at the define if the name is absent. Without
// this, every assert fired a spurious missing-scare on wasm, inflating help-log.
// emscripten maps exit() to an ExitStatus the JS caller catches (see test.mjs).
static noreturn lvm(lvm_exit) { exit(getcharm(Sp[0])); }
static union u const nif_exit[] = {{lvm_exit}, {lvm_ret0}};

// the source library (love.h): .rodata, name -> baked .l text, read by `use`.
static struct ai_lib const libs[] = {
  {"uu", src_uu}, {"coin", src_coin}, {"rng", src_rng}, {"q", src_q},
  {"kanren", src_kanren}, {NULL, NULL} };
struct ai_lib const *ai_libs(void) { return libs; }

// --- exported entry points ------------------------------------------------
static struct ai *F;

EMSCRIPTEN_KEEPALIVE
int ai_init(void) {
  F = ai_ini();
  if (!ai_ok(F)) return ai_code_of(F);
  struct ai_def d[] = {{"exit", (ai_word) nif_exit}};
  F = ai_defn(F, d, countof(d));
  if (!ai_ok(F)) return ai_code_of(F);
  F = ai_egg_(F, src_egg, src_p1, src_corpus);
  F = ai_evals_(F, boot_ai);
  // THE SESSION: a fresh writable layer, C-side -- everything the page ever
  // feeds through ai_eval defglobs here, never in the base.
  F = ai_layer_(F);
  return ai_code_of(F); }

EMSCRIPTEN_KEEPALIVE
int ai_eval(const char *src) {
  out_len = 0, out_full = 0;
  F = ai_evals_(F, src);
  if (out_full) out_note();
  return ai_code_of(F); }

EMSCRIPTEN_KEEPALIVE char*    ai_out_ptr(void) { return out_buf; }
EMSCRIPTEN_KEEPALIVE uint32_t ai_out_len(void) { return out_len; }
EMSCRIPTEN_KEEPALIVE void     ai_out_reset(void) { out_len = 0; }
