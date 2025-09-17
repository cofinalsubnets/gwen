#include "gw.h"

int main(void) {
  g_core *f = g_ini();
  f = g_read1f(f, stdin);
  if (g_ok(f)) g_writef(f, stdout),
               g_pop(f, 1);
  while (g_ok(f = g_read1f(f, stdin)))
    fputc(' ', stdout),
    g_writef(f, stdout),
    g_pop(f, 1);
  g_fin(f);
  return g_code_of(f); }
