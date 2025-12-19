#include "g.h"
#include "sys.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
static const char
  boot[] =
#include "boot.h"
  ,
  repl[] = "(:(repl p)(: r(,(puts p)(read 0))(? r(,(.(ev(A r)))(putc 10)(repl p))))(repl\" ;; \"))" ,
  rel[] =     "((:(R _)(: r(read _)(? r(,(ev'ev(A r))(R _)))))0)";
int main(int argc, const char **argv) {
  struct g *f = gini();
  while (*argv) f = gstrof(f, *argv++);
  f = gpush(f, 1, gnil);
  while (argc--) f = gconsr(f);
  f = gdef1(f, "argv");
  f = gevals_(f, boot);
  f = gevals_(f, isatty(STDIN_FILENO) ? repl : rel);
  enum g_status s = g_code_of(f);
  if (s != g_status_ok) {
    f = g_core_of(f);
    if (!f) fprintf(stderr, "# f@0 %d\n", s);
    else fprintf(stderr, "# f@%lx %lx.%ld.%ld.%ld\n",
      (long unsigned) f,
      (long unsigned) f->pool,
      f->len,
      f->hp - (intptr_t*) f,
      (intptr_t*) f + f->len - f->sp); }
  gfin(f);
  return s; }
