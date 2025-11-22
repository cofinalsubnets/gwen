#include "g.h"
#include "sys.h"
#include <stdio.h>

int main(void) {
  struct g *f = gini();
  if (gokp(f = gread1(f)))
    gwrite1(f),
    f = gpop(f, 1);
  while (gokp(f = gread1(f)))
    putchar(' '),
    gwrite1(f),
    f = gpop(f, 1);
  return gfin(f); }
