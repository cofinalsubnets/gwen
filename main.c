#include "gw.h"

static const char m[] =
#ifdef g_main_h
#include g_main_h
#else
#include "boot.h"
#endif
;

int main(int ac, const char **av) {
  return g_main(m, av); }
