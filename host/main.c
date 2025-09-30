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

static g_core *de(const char *k, g_cell* v, g_core *f) {
  return g_define(g_push(f, 1, v), k); }

int main(int _argc, const char **argv) {
  g_core *f = de("isatty", bif_isatty,
      de("readf", bif_readf,
      de("read", bif_read,
        de("putc", bif_putc,
        g_ini()))));
  return g_fin(g_apply(main_args(g_evals(g_pop(g_evals(f, boot), 1), main_), argv))); }
