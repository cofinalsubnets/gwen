#include "g.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

static g_vm(p_isatty) {
  Sp[0] = isatty(g_getnum(Sp[0])) ? g_putnum(-1) : g_nil;
  Ip += 1;
  return Continue(); }

struct fi { struct g_in in; FILE *file; };
static int p_file_getc(struct g_in *i) {
  struct fi *fi = (struct fi*) i;
  return getc(fi->file); }
static int p_file_ungetc(struct g_in *i, int c) {
  struct fi *fi = (struct fi*) i;
  return ungetc(c, fi->file); }
static int p_file_eof(struct g_in *i) {
  struct fi *fi = (struct fi*) i;
  return feof((fi)->file); }

static g_noinline struct g *readf_noinline(struct g *f) {
  intptr_t x = g_pop1(f);
  uint8_t *text = g_str_txt(x);
  uintptr_t len = g_str_len(x);
  char n[256]; // :)
  memcpy(n, text, len);
  n[len] = 0;
  FILE *i = fopen(n, "r");
  if (!i) return g_push(f, 1, g_nil);
  struct fi fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  f = g_readsi(f, (struct g_in*) &fi);
  fclose(i);
  return f; }

g_vm(readf) {
  if (!g_strp(Sp[0]) || g_str_len(Sp[0]) > 255) return
    Sp[0] = g_nil,
    Ip += 1,
    Continue();
  Pack(f);
  f = readf_noinline(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Ip += 1;
  return Continue(); }


static union x
  bif_isatty[] = {{p_isatty}, {ret0}},
  bif_readf[] = {{readf}, {ret0}};

static const char *main_prog =
#include "boot.h"
#include "main.h"
;

static struct g_def defs[] = {
  {"isatty", bif_isatty},
  {"readf", bif_readf}, };

int main(int argc, const char **argv) {
  struct g *f = g_ini();
  f = g_defns(f, LEN(defs), defs);
  f = g_evals(f, main_prog);
  while (*argv) f = g_strof(f, *argv++);
  f = g_push(f, 1, g_nil);
  while (argc--) f = g_cons_r(f);
  f = g_apply(f);
  if (!g_ok(f)) {
    enum g_status s = g_code_of(f);
    f = g_core_of(f);
    fprintf(stderr, "# f@%lx ", (uintptr_t) f);
    if (s == g_status_oom)
      fprintf(stderr, "oom@%ldB", !f ? 0 : f->len * sizeof(intptr_t) * 2);
    else fprintf(stderr, "error %d", s);
    fprintf(stderr, "\n"); }
  return g_fin(f); }
