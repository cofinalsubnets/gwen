/* &local as a compound literal's FIRST initializer is address-taken (the head-
 * position slot of the init list): the callee writes through the registered
 * pointer, so a register copy kept across the call would read stale -- love.c's
 * mm() root shape, the one the &-scan once missed. */
struct r { long *x; struct r *n; };
static struct r *R0;
static long bump(long k) {
  for (struct r *p = R0; p; p = p->n) *p->x += k;
  return k & 1;
}
int main() {
  long lam = 5;
  long j, s = 0;
  (R0 = &((struct r){ &lam, R0 }));
  do { j = 0; long d = 6; for (; d > 0; d--) { if (bump(d)) { j++; s += lam; } } } while (j > 3);
  return (s + lam) == 91 ? 42 : (int) (s + lam);
}
