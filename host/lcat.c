#include "gw.h"
#include <stdio.h>

int main(void) {
  g_core *f = g_ini();
  if (g_ok(f = g_read1(f)))
    g_write1(f),
    f = g_pop(f, 1);
  while (g_ok(f = g_read1(f)))
    putchar(' '),
    g_write1(f),
    f = g_pop(f, 1);
  return g_fin(f); }
