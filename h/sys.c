#include "g.h"
#include "sys.h"
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

// noinline this because it leaks a stack address
g_noinline uintptr_t g_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }

void g_stdout_putc(int c) { putchar(c); }

int g_stdin_getc(void) {
  return getc(stdin); }
int g_stdin_ungetc(int c) {
  return ungetc(c, stdin); }
int g_stdin_eof(void) {
  return feof(stdin); }
int p_file_getc(struct g_in *i) {
  struct fi *fi = (struct fi*) i;
  return getc(fi->file); }
int p_file_ungetc(struct g_in *i, int c) {
  struct fi *fi = (struct fi*) i;
  return ungetc(c, fi->file); }
int p_file_eof(struct g_in *i) {
  struct fi *fi = (struct fi*) i;
  return feof((fi)->file); }

