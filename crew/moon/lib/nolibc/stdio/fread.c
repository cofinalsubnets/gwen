#include "../impl.h"

/* ⚠ THE PUSHBACK BELONGS TO THE STREAM, not to getc. C says the next input of
 * ANY kind sees an ungetc'd byte, so draining f->un here is not a courtesy to
 * getc -- skipping it both DROPS the byte and leaves it in place, and a program
 * that probes for EOF the portable way then never reaches it. bzip2's myfeof is
 * exactly that probe (fgetc, then ungetc if it was not EOF): with fread reading
 * past the pushback, every myfeof re-served the same stale byte, so compressing
 * any non-empty file spun forever at EOF -- and the first byte of the file was
 * quietly missing from the output besides. */
size_t fread(void *p, size_t sz, size_t n, FILE *f) {
  size_t total = sz * n, got = 0;
  unsigned char *d = p;
  if (total && f->un) { d[got++] = (unsigned char) (f->un - 1); f->un = 0; }
  while (got < total) {
    long k = read(f->fd, d + got, (long) (total - got));
    if (k < 0) { if (__errno_v == EINTR) continue; f->err = 1; break; }
    if (k == 0) { f->eof = 1; break; }
    got += (size_t) k; }
  return sz ? got / sz : 0; }
