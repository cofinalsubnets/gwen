#include "../g/g.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

// noinline this because it leaks a stack address
g_noinline uintptr_t g_clock(void) {
 struct timespec ts;
 int s = clock_gettime(CLOCK_REALTIME, &ts);
 return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }

int gputc(struct g*f, int c) {
 return putc(c, stdout); }

int ggetc(struct g*f) {
 return getc(stdin); }

int gungetc(struct g*_, int c) {
 return ungetc(c, stdin); }

int geof(struct g*_) {
 return feof(stdin); }

int gflush(struct g*) {
 return fflush(stdout); }

int main(int argc, char const **argv) {
 struct g *f;
 for (f = gini(); *argv; f = gstrof(f, *argv++));
 for (f = gpush(f, 1, gnil); argc--; f = gconsr(f));
 f = gdef1(f, "argv");
 f = gevals_(f, "((:(go _)(: r(read _)(? r(,(ev'ev(A r))(go _)))))0)");
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
