#include "gw.h"
#include <stdlib.h>

static const char g_main[] =
#ifdef g_main_h
#include g_main_h
#else
#include "boot.h"
#endif
;

int main(int ac, const char **av) {
  return g_code_of(g_run(g_ini(), g_main, av)); }
