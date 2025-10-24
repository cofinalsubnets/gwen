#include "i.h"
#include "sys.h"

static const char main_[] =
#include "main.h"
;

static const char boot_sequence[] =
#include "boot.h"
;



#include <unistd.h>
static Vm(p_isatty) {
  Sp[0] = isatty(g_getnum(Sp[0])) ? g_putnum(-1) : nil;
  Ip += 1;
  return Continue(); }
static NoInline struct g *g_readsf(struct g *f) {
  struct g_string *s = (struct g_string*) pop1(f);
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
  struct g_string *s = str(Sp[0]);
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
static struct g *main_args(struct g *f, const char **argv) {
  const char *a = *argv;
  return a == NULL ? g_push(f, 1, g_nil) :
    g_cons_r(main_args(g_strof(f, a), argv + 1)); }

static union x
  bif_isatty[] = {{p_isatty}, {ret0}},
  bif_read[] = {{read0}, {ret0}},
  bif_putc[] = {{prc}, {ret0}},
  bif_readf[] = {{readf}, {ret0}};

static struct {
  const char *n;
  g_cell *v;
} defs[] = {
  {"isatty", bif_isatty},
  {"readf", bif_readf},
  {"read", bif_read},
  {"putc", bif_putc}, };

static void report(g_core *f) {
  enum g_status s = g_code_of(f);
  f = g_core_of(f);
  switch (s) {
    case g_status_oom:
      fprintf(stderr, "# oom@%ldB\n", !f ? 0 : f->len * sizeof(g_word) * 2);
    default: }; }

int main(int _argc, const char **argv) {
  g_core *f = g_ini();
  f = g_evals_(f, boot_sequence);
  for (uintptr_t i = 0; i < LEN(defs); i++)
    f = g_define(g_push(f, 1, defs[i].v), defs[i].n);
  f = g_evals(f, main_);
  f = main_args(f, argv);
  f = g_apply(f);
  report(f);
  return g_fin(f); }
