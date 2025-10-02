#include "gw.h"
#include <stdio.h>

#define N (1<<20)
static g_word g_static_pool[2][N];

int main(void) {
  g_core *f = g_ini_static(N, g_static_pool);
  if (g_ok(f = g_read1(f)))
    g_write1(f),
    f = g_pop(f, 1);
  while (g_ok(f = g_read1(f)))
    putchar(' '),
    g_write1(f),
    f = g_pop(f, 1);
  return g_fin(f); }
