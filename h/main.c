#include "g.h"
#include "sys.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, const char **argv) {
  struct g *f = g_ini();
  while (*argv) f = g_strof(f, *argv++);
  f = g_push(f, 1, g_nil);
  while (argc--) f = g_cons_r(f);
  struct g_def arg_def[1] = {{"argv", (void*) g_pop1(f) }};
  f = g_defns(f, 1, arg_def);
  f = g_evals(f,
#include "boot.h"
    "(:(R _)(: r(read _)(? r(,(ev'ev(A r))(R _))))(R 0))");
  if (!g_ok(f)) {
    enum g_status s = g_code_of(f);
    f = g_core_of(f);
    fprintf(stderr, "# f@%lx ", (uintptr_t) f);
    if (s == g_status_oom)
      fprintf(stderr, "oom@%ldB", !f ? 0 : f->len * sizeof(intptr_t) * 2);
    else fprintf(stderr, "error %d", s);
    fprintf(stderr, "\n"); }
  return g_fin(f); }
