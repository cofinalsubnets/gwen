#include "gw.h"

static const char g_main[] =
#ifdef MAIN_H
#include MAIN_H
#else
#include "main.h"
#endif
;

int main(int ac, const char **av) {
  g_core *f = g_ini();
  if (g_ok(f)) f = g_run(f, g_main, av);
  g_fin(core_of(f));
  return code_of(f); }
