#include "i.h"
#include "sys.h"
#include <unistd.h>
static Vm(p_isatty) {
  Sp[0] = isatty(g_getnum(Sp[0])) ? g_putnum(-1) : g_nil;
  Ip += 1;
  return Continue(); }
static NoInline struct g *g_readsf(struct g *f) {
  struct g_string *s = (struct g_string*) pop1(f);
  char n[256]; // :)
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  g_file i = fopen(n, "r");
  if (!i) return g_push(f, 1, g_nil);
  file_input fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  f = g_readsi(f, (struct g_in*) &fi);
  fclose(i);
  return f; }

Vm(readf) {
  struct g_string *s = (struct g_string*) Sp[0];
  if (!strp(Sp[0]) || s->len > 255) return
    Sp[0] = g_nil,
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

static Vm(prc) {
  g_fputc(g_getnum(*Sp), g_stdout);
  Ip += 1;
  return Continue(); }

static union x
  bif_isatty[] = {{p_isatty}, {ret0}},
  bif_read[] = {{read0}, {ret0}},
  bif_putc[] = {{prc}, {ret0}},
  bif_readf[] = {{readf}, {ret0}};

static void report(struct g *f) {
  enum g_status s = g_code_of(f);
  f = g_core_of(f);
  switch (s) {
    case g_status_oom:
      fprintf(stderr, "# oom@%ldB\n", !f ? 0 : f->len * sizeof(intptr_t) * 2);
    default: }; }

int main(int argc, const char **argv) {
  static struct { const char *n; union x *v; } defs[] = {
    {"isatty", bif_isatty},
    {"readf", bif_readf},
    {"read", bif_read},
    {"putc", bif_putc}, };
  struct g *f = g_ini();
  for (uintptr_t i = 0; i < LEN(defs); i++)
    f = g_define(g_push(f, 1, defs[i].v), defs[i].n);
  f = g_evals(f,
#include "boot.h"
#include "main.h"
  );
  while (*argv) f = g_strof(f, *argv++);
  f = g_push(f, 1, g_nil);
  while (argc--) f = g_cons_r(f);
  f = g_apply(f);
  report(f);
  return g_fin(f); }
