#include "gw.h"
static const char p[] =
#include "boot.h"
;

int main(int _ac, const char **av) {
  g_core *f = g_ini();
  int n = 0;
  f = g_readcs(f, p);
  f = g_push(f, 3, g_nil, g_var(f, g_var_qt), g_nil);
  while (av[n]) f = g_strof(f, av[n++]);
  f = g_push(f, 1, g_nil);
  while (n--) f = g_cons_r(f);
  f = g_cons_l(f);
  f = g_cons_r(f);
  f = g_cons_l(f);
  f = g_cons_r(f);
  f = g_eval(f);
  g_fin(f);
  return g_code_of(f); }
