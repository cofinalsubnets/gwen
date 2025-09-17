#include "i.h"

int main(void) {
  g_core *f = g_ini();
  f = p_read1f(f, stdin);
  if (g_ok(f)) transmit(f, stdout, pop1(f));
  while (g_ok(f = p_read1f(f, stdin)))
    fputc(' ', stdout),
    transmit(f, stdout, pop1(f));
  g_fin(f);
  return 0; }
