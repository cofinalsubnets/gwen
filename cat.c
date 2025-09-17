#include "gw.h"

int main(void) {
  g_core *f = g_ini();
  f = g_read1(f);
  if (g_ok(f)) g_write1(f),
               g_pop(f, 1);
  while (g_ok(f = g_read1(f)))
    putchar(' '),
    g_write1(f),
    g_pop(f, 1);
  g_fin(f);
  return g_code_of(f); }
