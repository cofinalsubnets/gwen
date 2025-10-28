#include "g.h"
#include "sys.h"
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
// noinline because it leaks a stack address
g_noinline uintptr_t g_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }
struct g*g_stdout_putc(struct g*f,struct g_out*, int c) { return putchar(c), f; }
int g_getc(struct g_in*) { return getc(stdin); }
int g_ungetc(struct g_in*, int c) { return ungetc(c, stdin); }
int g_eof(struct g_in*) { return feof(stdin); }
int p_file_getc(struct g_in *i) {
  struct fi *fi = (struct fi*) i;
  return getc(fi->file); }
int p_file_ungetc(struct g_in *i, int c) {
  struct fi *fi = (struct fi*) i;
  return ungetc(c, fi->file); }
int p_file_eof(struct g_in *i) {
  struct fi *fi = (struct fi*) i;
  return feof((fi)->file); }

