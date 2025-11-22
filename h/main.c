#include "g.h"
#include "sys.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, const char **argv) {
  struct g *f = gini();
  while (*argv) f = gstrof(f, *argv++);
  f = gpush(f, 1, gnil);
  while (argc--) f = gconsr(f);
  f = gdef1(f, "argv");
  f = gevals_(f,
#include "boot.h"
  );
  f = gevals_(f,
    isatty(STDIN_FILENO) ? 
    "(:(repl p)(: r(,(puts p)(read 0))(? r(,(.(ev(A r)))(putc 10)(repl p))))(repl\" ;; \"))" :
    "((:(R _)(: r(read _)(? r(,(ev'ev(A r))(R _)))))0)");

  if (!gokp(f)) {
    enum g_status s = g_code_of(f);
    f = g_core_of(f);
    fprintf(stderr, "# f@%lx ", (uintptr_t) f);
    if (s == g_status_oom)
      fprintf(stderr, "oom@%ldB", !f ? 0 : f->len * sizeof(intptr_t) * 2);
    else fprintf(stderr, "error %d", s);
    fprintf(stderr, "\n"); }
  return gfin(f); }

