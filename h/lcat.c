#include "g.h"
#include "sys.h"
#include <stdio.h>

int main(void) {
  struct g *f = gini();
  if (gokp(f = gread1(f)))
    f = gwrite1(f),
    f = gpop(f, 1);
  while (gokp(f = gread1(f)))
    putchar(' '),
    gwrite1(f),
    f = gpop(f, 1);
  enum g_status s = g_code_of(f);
  gfin(g_core_of(f));
  return s; }
