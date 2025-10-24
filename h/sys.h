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
NoInline struct g *g_read1f(struct g *f, g_file i);
int p_file_getc(struct g_in *i);
int p_file_ungetc(struct g_in *i, int c);
int p_file_eof(struct g_in *i);
typedef struct file_input {
  struct g_in in;
  g_file file;
} file_input;
#endif
