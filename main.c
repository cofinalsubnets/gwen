#include "gw.h"
static const char p[] =
#include "main.h"
;

int main(int ac, const char **av) {
  g_core *f = g_ini();
  f = g_read1s(f, p);
  f = g_push(f, 3, g_nil, g_var(f, g_var_qt), g_nil);
  for (int i = 0; i < ac; i++)
    f = g_strof(f, av[i]);
  f = g_push(f, 1, g_nil);
  for (int i = 0; i < ac; i++)
    f = g_cons_r(f);
  f = g_cons_l(f);
  f = g_cons_r(f);
  f = g_cons_l(f);
  f = g_cons_r(f);
  f = g_eval(f);
  return g_fin(f); }
