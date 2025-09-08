#include "gw.h"

static const char g_main[] =
#ifdef g_main_h
#include g_main_h
#else
#include "boot.h"
#endif
;

int main(int ac, const char **av) {
  g_core *f = g_ini();
  if (g_ok(f)) f = g_run(f, g_main, av);
  g_fin(g_core_of(f));
  return g_code_of(f); }
