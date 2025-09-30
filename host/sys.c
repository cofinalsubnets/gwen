#include "i.h"
#include <time.h>
#include <unistd.h>
#include <string.h>

Vm(p_isatty) {
  Sp[0] = isatty(getnum(Sp[0])) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }

void *g_malloc(g_core*f, size_t n) {
  return malloc(n); }
void g_free(g_core*f, void*x) {
  return free(x); }

NoInline uintptr_t g_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }


typedef struct file_input {
  g_input in;
  g_file file;
} file_input;
static int p_file_getc(input *i) { return getc(((file_input*) i)->file); }
static int p_file_ungetc(input *i, int c) { return ungetc(c, ((file_input*) i)->file); }
static int p_file_eof(input *i) { return feof(((file_input*) i)->file); }

static NoInline g_core *g_read1f(g_core *f, g_file i) {
  file_input fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  return g_read1i(f, (input*) &fi); }
static NoInline g_core *g_readsf(g_core *f) {
  string *s = (string*) pop1(f);
  char n[256]; // :)
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  g_file i = fopen(n, "r");
  if (!i) return g_push(f, 1, nil);
  file_input fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  f = g_readsi(f, (input*) &fi);
  fclose(i);
  return f; }

Vm(readf) {
  string *s = str(Sp[0]);
  if (!strp(Sp[0]) || s->len > 255) return
    Sp[0] = nil,
    Ip += 1,
    Continue();
  Pack(f);
  f = g_readsf(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Ip += 1;
  return Continue(); }

Vm(read0) {
  Pack(f);
  f = g_read1f(f, g_stdin);
  if (g_code_of(f) == g_status_eof) return // no error but end of file
    f = g_core_of(f),
    Unpack(f),
    Ip += 1,
    Continue();
  f = g_cons_l(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Ip += 1;
  return Continue(); }
g_core *g_read1(g_core *f) { return g_read1f(f, g_stdin); }

Inline g_core *g_run(g_core *f) {
  return !g_ok(f) ? f : f->ip->ap(f, f->ip, f->hp, f->sp); }
