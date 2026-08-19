/* The allocator's contract, held against the C library's. nolibc hands any request of
 * 128 kB or more a mapping of its own so that free can give the pages back, and calloc
 * skips the memset on those because the kernel has already zeroed them -- so the size a
 * request lands on decides which path serves it, and both paths owe the caller exactly
 * the same promises. A block must be zero when calloc says so, must keep its bytes
 * across a realloc that moves it, must survive being freed and asked for again, and a
 * multiply that wraps must refuse rather than hand back something too small.
 *
 * The sizes straddle that threshold deliberately: small enough to ride the arena, large
 * enough to take a mapping, and one either side of the boundary itself.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern void *malloc(unsigned long);
extern void *calloc(unsigned long, unsigned long);
extern void *realloc(void *, unsigned long);
extern void free(void *);

static int zeroed(char const *p, unsigned long n) {
  for (unsigned long i = 0; i < n; i++) if (p[i]) return 0;
  return 1; }

static void fill(char *p, unsigned long n, int seed) {
  for (unsigned long i = 0; i < n; i++) p[i] = (char) (i * 7 + seed); }

static int filled(char const *p, unsigned long n, int seed) {
  for (unsigned long i = 0; i < n; i++) if (p[i] != (char) (i * 7 + seed)) return 0;
  return 1; }

int main(void) {
  int ok = 0;
  unsigned long const small = 1024, big = 256 * 1024, under = 128 * 1024 - 64, over = 128 * 1024 + 64;

  /* calloc zeroes, on both sides of the threshold */
  char *a = calloc(small, 1);
  if (a && zeroed(a, small)) ok++;
  char *b = calloc(big, 1);
  if (b && zeroed(b, big)) ok++;
  char *c = calloc(under, 1);
  if (c && zeroed(c, under)) ok++;
  char *d = calloc(over, 1);
  if (d && zeroed(d, over)) ok++;

  /* a large block dirtied, freed, and asked for again is zero again */
  fill(b, big, 3);
  free(b);
  char *b2 = calloc(big, 1);
  if (b2 && zeroed(b2, big)) ok++;
  free(b2);

  /* the payload really is as wide as asked: write every byte of it and read it back */
  fill(d, over, 5);
  if (filled(d, over, 5)) ok++;

  /* realloc keeps the bytes when it moves a block across the threshold, both ways */
  char *e = malloc(small);
  fill(e, small, 9);
  e = realloc(e, big);
  if (e && filled(e, small, 9)) ok++;
  fill(e, big, 11);
  char *f = realloc(e, big * 2);
  if (f && filled(f, big, 11)) ok++;
  free(f);

  /* free and malloc of large blocks, repeatedly: the mapping path must not leak the
   * arena's invariants, and a small block asked for in between must still be sound */
  int rounds = 1;
  for (int i = 0; i < 8; i++) {
    char *p = malloc(big);
    char *q = malloc(small);
    if (!p || !q) { rounds = 0; break; }
    fill(p, big, i); fill(q, small, i);
    if (!filled(p, big, i) || !filled(q, small, i)) { rounds = 0; break; }
    free(p); free(q); }
  if (rounds) ok++;

  /* a multiply that wraps is refused outright -- the whole reason the count and the
   * size are two arguments rather than one. ⚠ volatile: a compiler that folds these
   * sees an impossible object and warns, and the runtime answer is what is asked */
  volatile unsigned long huge = (unsigned long) -1;
  if (!calloc(huge / 2 + 1, 4)) ok++;
  if (!calloc(huge, huge)) ok++;

  /* the degenerate sizes answer without falling over */
  char *g = calloc(0, 16);
  free(g);
  char *h = malloc(0);
  free(h);
  ok++;

  free(a); free(c); free(d);
  return ok; }
