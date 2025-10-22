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
g_vm p_isatty, readf, read0;
static Inline g_core *g_run(g_core *f) {
  return !g_ok(f) ? f : f->ip->ap(f, f->ip, f->hp, f->sp); }
#endif
