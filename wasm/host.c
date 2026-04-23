// emscripten host shim for gwen lisp.
// gputc/gflush feed a JS-visible output buffer; ggetc pulls from a string
// set per-call. boot.g is embedded and evaluated once by gwen_init.
#include "g.h"
#include <emscripten.h>
#include <string.h>
#include <time.h>

static const char boot_g[] =
#include "boot.h"
;

static char     out_buf[1 << 14];
static uint32_t out_len;

static const char *in_buf;
static uint32_t    in_pos, in_len;

int gputc(struct g *f, int c) {
  if (out_len < sizeof out_buf) out_buf[out_len++] = (char) c;
  return c; }
int gflush(struct g *f) { return 0; }
int ggetc(struct g *f) {
  return in_pos < in_len ? (unsigned char) in_buf[in_pos++] : -1; }
int gungetc(struct g *f, int c) {
  return in_pos > 0 ? (in_pos--, c) : -1; }
int geof(struct g *f) { return in_pos >= in_len; }

uintptr_t g_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? 0
       : ts.tv_sec * 1000u + ts.tv_nsec / 1000000u; }

static struct g *F;

EMSCRIPTEN_KEEPALIVE
int gwen_init(void) {
  F = g_ini();
  if (!g_ok(F)) return g_code_of(F);
  in_buf = boot_g, in_pos = 0, in_len = sizeof boot_g - 1;
  F = g_evals_(F, boot_g);
  return g_code_of(F); }

EMSCRIPTEN_KEEPALIVE
int gwen_eval(const char *src) {
  out_len = 0;
  in_buf = "", in_pos = 0, in_len = 0;
  F = g_evals_(F, src);
  return g_code_of(F); }

EMSCRIPTEN_KEEPALIVE char*    gwen_out_ptr(void) { return out_buf; }
EMSCRIPTEN_KEEPALIVE uint32_t gwen_out_len(void) { return out_len; }
EMSCRIPTEN_KEEPALIVE void     gwen_out_reset(void) { out_len = 0; }
