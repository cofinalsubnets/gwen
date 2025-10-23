#include "i.h"
#include "sys.h"

static const char main_[] =
#include "main.h"
;

static const char boot_sequence[] =
#include "boot.h"
;

static g_core *main_args(g_core *f, const char **argv) {
  const char *a = *argv;
  return a == NULL ? g_push(f, 1, g_nil) :
    g_cons_r(main_args(g_strof(f, a), argv + 1)); }

static union g_cell
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

#define LEN(x) (sizeof(x)/sizeof(*x))
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
