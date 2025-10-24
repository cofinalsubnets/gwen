#ifndef _g_host_sys_h
#define _g_host_sys_h
#include <stdio.h>
#include <stdlib.h>
typedef FILE *g_file;
#define g_stdout stdout
#define g_stdin stdin
#define g_stderr stderr
#define g_fprintf fprintf
#define g_fputc putc
static Inline struct g *g_run(struct g *f) {
  return !g_ok(f) ? f : f->ip->ap(f, f->ip, f->hp, f->sp); }
NoInline struct g *g_read1f(struct g *f, g_file i);
int p_file_getc(struct g_input *i);
int p_file_ungetc(struct g_input *i, int c);
int p_file_eof(struct g_input *i);
typedef struct file_input {
  struct g_input in;
  g_file file;
} file_input;
#endif
