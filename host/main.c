#include "i.h"
#include "sys.h"

static const char boot[] =
#include "boot.h"
;

static const char main_[] =
#include "main.h"
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
  {"putc", bif_putc},
};

#define LEN(x) (sizeof(x)/sizeof(*x))
int main(int _argc, const char **argv) {
  g_core *f = g_evals_(g_ini(), boot);
  for (int i = 0; i < LEN(defs); i++)
    f = g_define(g_push(f, 1, defs[i].v), defs[i].n);
  return g_fin(g_apply(main_args(g_evals(f, main_), argv))); }
