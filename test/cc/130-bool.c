/* _Bool is ONE BYTE and conversion to it compares with 0 (C11 6.3.1.2) -- never
 * a truncation. Every write site normalizes: init, assignment, cast, ++/--, the
 * bitfield RMW, a param's arrival, a return; a static's image folds the same way.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

typedef _Bool bl;

struct m { char a; bl b; char c; };
struct bf { bl x:1; unsigned y:3; bl z:1; };

static bl keep;
static bl initd = 5;
static struct bf w0 = { 4, 5, 1 };

static bl ret5(void) { return 5; }
static bl thru(bl b) { return b; }
static int take(bl b) { return b == 1; }

int main(void) {
  int n = 0;
  bl t = 42, f = 0, half = (bl)0.5;
  struct m s;
  struct bf w;
  double d = 2.5;

  n += sizeof(bl) == 1;
  n += sizeof(struct m) == 3;
  n += t == 1;                    /* init normalizes */
  n += f == 0;
  n += half == 1;                 /* (bl)0.5 compares, never truncates */
  n += initd == 1;                /* the static image normalizes */
  keep = -3;    n += keep == 1;   /* a global store normalizes */
  t = 1; t++;   n += t == 1;      /* ++ lands on 0/1 */
  f = 0; f--;   n += f == 1;      /* -- on 0 is (bl)-1 = 1 */
  n += ret5() == 1;               /* a return converts */
  n += take(7);                   /* an arrival converts (cc calls untyped) */
  n += thru(200) == 1;
  w.x = 4;      n += w.x == 1;    /* the field RMW normalizes BEFORE the mask */
  w.y = 5;      n += w.y == 5;
  w.z = 1;      n += w.z == 1;    /* a 1-bit bool field reads unsigned */
  n += w0.x == 1 && w0.y == 5 && w0.z == 1;   /* ..and its image agrees */
  s.a = 1; s.b = 200; s.c = 3; n += s.b == 1;
  n += (bl)3 == 1;
  n += (bl)0 == 0;
  n += (bl)d == 1;
  n += t + t == 2;                /* bool promotes to int */
  n += (t - 2 < 0);               /* ..a SIGNED int */
  return n;                       /* 22 */
}
