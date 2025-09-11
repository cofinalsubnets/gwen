#include "gw.h"

static const char m[] =
#ifdef g_main_h
#include g_main_h
#else
#include "boot.h"
#endif
;

int main(int ac, const char **av) {
  g_core *f = g_ini();
  f = g_main(f, m, av);
  int s = g_code_of(f);
  g_fin(f);
  return s; }
