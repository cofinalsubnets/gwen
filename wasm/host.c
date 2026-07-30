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
#include <stdnoreturn.h>

// the egg's four texts, one per ai_egg_ argument (love.h): the boot stitches the
// corpus rather than reading it whole -- p0 takes egg/p1/prel, p1 takes ev.
static const char src_egg[] =
#include "egg.h"
;
static const char src_p1[] =
#include "p1.h"
;
static const char src_prel[] =
#include "prel.h"
;
static const char src_ev[] =
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
// the summary. Output past the cap is dropped (never overruns, see _putc).
static char     out_buf[1 << 18];
static uint32_t out_len;

uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? 0
       : ts.tv_sec * 1000u + ts.tv_nsec / 1000000u; }

// --- ports ----------------------------------------------------------------
// Output goes to out_buf; the page reads it back through the exports below.
static struct ai *_putc(struct ai *g, int c) {
  if (out_len < sizeof out_buf) out_buf[out_len++] = (char) c;
  return g; }
static struct ai *_flush(struct ai *g) { return g; }

// No real stdin: getc reports EOF (honouring any pushed-back byte first).
static struct ai *_getc(struct ai *g) {
  struct ai_io *i = ai_core_of(g)->io;
  if (getcharm(i->ungetc_buf) != EOF) {
    int c = getcharm(i->ungetc_buf);
    i->ungetc_buf = putcharm(EOF);
    return ai_core_of(g)->b = c, g; }
  i->eof_seen = putcharm(true);
  return ai_core_of(g)->b = EOF, g; }
static struct ai *_ungetc(struct ai *g, int c) {
  struct ai_io *i = ai_core_of(g)->io;
  i->ungetc_buf = putcharm(c);
  i->eof_seen = putcharm(false);
  return ai_core_of(g)->b = c, g; }
static struct ai *_eof(struct ai *g) {
  struct ai_io *i = ai_core_of(g)->io;
  return ai_core_of(g)->b = (getcharm(i->ungetc_buf) == EOF) && getcharm(i->eof_seen), g; }

// fd values are nominal: all I/O routes through the vtable regardless. We
// just need fd >= 0 so the dispatcher picks ai_fd_port_vt over a synth slot.
struct ai_io ai_stdin  = { .ap = lvm_port_io, .fd = putcharm(0),
                         .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_io ai_stdout = { .ap = lvm_port_io, .fd = putcharm(1),
                         .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
// No separate error stream in the browser host; route err to out's fd.
struct ai_io ai_stderr = { .ap = lvm_port_io, .fd = putcharm(1),
                         .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_port_vt const ai_fd_port_vt = { _getc, _ungetc, _eof, _putc, _flush, NULL, NULL };  // no bulk lanes: per-byte fallback

// (exit n) -- a frontend nif, like main.c's and kmain.c's. The wasm host needs
// it for the same reason they do: the test harness aborts a failed assert with
// (exit 1), and -- subtler -- a closure captures its free globals at creation,
// so a body that merely MENTIONS `exit` (e.g. an assert's unrun fail branch)
// raises (scare 'missing 'exit) at the define if the name is absent. Without
// this, every assert fired a spurious missing-scare on wasm, inflating help-log.
// emscripten maps exit() to an ExitStatus the JS caller catches (see test.mjs).
static noreturn lvm(lvm_exit) { exit(getcharm(Sp[0])); }
static union u const nif_exit[] = {{lvm_exit}, {lvm_ret0}};

// --- exported entry points ------------------------------------------------
static struct ai *F;

EMSCRIPTEN_KEEPALIVE
int ai_init(void) {
  F = ai_ini();
  if (!ai_ok(F)) return ai_code_of(F);
  struct ai_def d[] = {{"exit", (ai_word) nif_exit}};
  F = ai_defn(F, d, countof(d));
  if (!ai_ok(F)) return ai_code_of(F);
  F = ai_lib_(F, "uu", src_uu);
  F = ai_lib_(F, "coin", src_coin);
  F = ai_lib_(F, "rng", src_rng);
  F = ai_lib_(F, "q", src_q);
  F = ai_lib_(F, "kanren", src_kanren);
  F = ai_egg_(F, src_egg, src_p1, src_prel, src_ev);
  F = ai_evals_(F, boot_ai);
  // THE SESSION: a fresh writable layer, C-side -- everything the page ever
  // feeds through ai_eval defglobs here, never in the base.
  F = ai_layer_(F);
  return ai_code_of(F); }

EMSCRIPTEN_KEEPALIVE
int ai_eval(const char *src) {
  out_len = 0;
  F = ai_evals_(F, src);
  return ai_code_of(F); }

EMSCRIPTEN_KEEPALIVE char*    ai_out_ptr(void) { return out_buf; }
EMSCRIPTEN_KEEPALIVE uint32_t ai_out_len(void) { return out_len; }
EMSCRIPTEN_KEEPALIVE void     ai_out_reset(void) { out_len = 0; }
