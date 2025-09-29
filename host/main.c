#include "i.h"
static const char p[] =
#include "main.h"
;

static g_core *main_args(g_core *f, const char **argv) {
  const char *a = *argv;
  return a == NULL ? g_push(f, 1, g_nil) :
    g_cons_r(main_args(g_strof(f, a), argv + 1)); }

int main(int _argc, const char **argv) {
  return g_fin(g_apply(main_args(g_evals(g_ini(), p), argv))); }
