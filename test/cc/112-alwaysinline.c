/* always_inline honored (the ai_inline lever): a marked static splices at any
 * size -- multi-return if-chains, loops, switches -- and noinline bars the
 * table by name. splices NEST to depth 3, so a marked chain flattens; an &
 * -taking arg or body declines back to the real call; a struct-returning call
 * inside a spliced body parks its result through a lean (the sibcall window
 * must not read it as arg staging -- the zn_false shape). inlining is
 * semantics-neutral, so every check must answer exactly as gcc -O0.
 *
 * Each check contributes 1, so the exit code IS the number that passed. */

#define AI static inline __attribute__((always_inline))
#define NOI static __attribute__((noinline))

struct zn { double re, im; };
static struct zn zmk(double a, double b) { struct zn z; z.re = a; z.im = b; return z; }

/* multi-return if-chain (the ai_nilp shape) */
AI int nilp(long x) {
 if (x == 0) return 1;
 if (x & 1) return x < 0;
 if (x > 100) return zmk(-1.0, (double) x).re <= 0;   /* struct call + member-of-return */
 return 0; }

/* a loop body (the map_probe shape) */
AI long probe(long const *a, long n, long k) {
 for (long i = 0; i < n; i++) if (a[i] == k) return i;
 return -1; }

/* a switch body */
AI int rank(int t) {
 switch (t) {
  case 0: return 7;
  case 1: case 2: return 9;
  default: return t * 2; } }

/* the marked chain: three deep, flattens to the cap */
AI long l3(long x) { return x + 1; }
AI long l2(long x) { return l3(x) * 2; }
AI long l1(long x) { return l2(x) + l3(x); }

/* an &-taking arg: the splice declines, the real call answers the same */
AI long viaptr(long *p) { return *p + 5; }

/* noinline honored: stays a real call, same answer */
NOI long heavy(long x) { return x * 3 + 1; }

/* wide ++/-- and compound assigns THROUGH a splice */
AI long bump2(long *p) { *p += 2; return *p; }

int main(void) {
 int ok = 0;
 long a[5] = { 3, 1, 4, 1, 5 };
 long v = 40;

 if (nilp(0) == 1) ok++;
 if (nilp(-3) == 1) ok++;
 if (nilp(7) == 0) ok++;
 if (nilp(200) == 1) ok++;               /* the struct lane: -1.0 <= 0 */
 if (nilp(4) == 0) ok++;

 if (probe(a, 5, 4) == 2) ok++;
 if (probe(a, 5, 9) == -1) ok++;
 if (probe(a, 5, 1) == 1) ok++;          /* first hit wins */

 if (rank(0) == 7) ok++;
 if (rank(2) == 9) ok++;
 if (rank(6) == 12) ok++;

 if (l1(10) == 33) ok++;                 /* (10+1)*2 + (10+1) */
 if (l1(l1(0)) == 12) ok++;              /* nested at the call site too: l1(0)=3, l1(3)=12 */

 if (viaptr(&v) == 45) ok++;
 if (heavy(v) == 121) ok++;

 if (bump2(&v) == 42 && v == 42) ok++;

 { struct zn z = zmk(2.5, -1.0);         /* member reads off a parked return */
   if (z.re > 0 && zmk(0.0, 3.0).re <= 0) ok++; }

 return ok; }
