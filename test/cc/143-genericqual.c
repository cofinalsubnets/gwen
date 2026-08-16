/* _Generic over QUALIFIED types, held to gcc (C11 6.5.1.1 + DR481). Two rules
 * do all the work here and they pull opposite ways:
 *
 *   - the controlling expression is LVALUE-CONVERTED, so its TOP-level const or
 *     volatile is gone before any row is tried: `const int x` picks `int:`.
 *   - everything INSIDE a pointer survives, and compatibility is exact, so
 *     `const char *` and `char *` are two different rows.
 *
 * cc's type language carries no qualifier at all (a node for one would reach
 * every ptr dispatch site in gen), so parse keeps _Generic its own marked copy
 * -- ps 'qtys, beside 'locals and shadowed with it. This file is the battery
 * that says the copy tracks: locals, params, block scope, globals, typedefs,
 * struct members, array decay, & and *, and const told apart from volatile.
 *
 * ⚠ the battery compares EXIT CODES, so every check folds into the status:
 * 0 is agreement, and the first failing check's number comes back instead.
 */

typedef const char *ccp;                     /* the qualifier rides the TYPEDEF */
typedef char *cp;                            /* ..and does not, here */
struct S { const char *a; char *b; volatile int c; };

const char *g_ccp;
char *g_cp;
const int g_ci = 3;

static int p_c(const char *s)   { return _Generic(s, char *: 1, const char *: 2, default: 9); }
static int p_p(char *s)         { return _Generic(s, char *: 1, const char *: 2, default: 9); }
static int p_ci(const int *u)   { return _Generic(u, int *: 1, const int *: 2, default: 9); }
static int p_vi(volatile int *v) { return _Generic(v, int *: 1, const int *: 2, volatile int *: 3, default: 9); }

int main(void) {
  const char *a = "x";
  char *b = 0;
  const char **c = &a;
  char * const d = 0;                        /* a const POINTER: top-level, so it goes */
  const char e[4] = { 0 };
  char f[4] = { 0 };
  ccp h = "x";
  cp i = 0;
  struct S s = { 0, 0, 0 };
  const volatile int *j = 0;
  int k = 0;
  const int m = 0;
  volatile int n = 0;

  /* the pointee's qualifier decides */
  if (_Generic(a, char *: 1, const char *: 2, default: 9) != 2) return 1;
  if (_Generic(b, char *: 1, const char *: 2, default: 9) != 1) return 2;
  if (_Generic(c, char **: 1, const char **: 2, default: 9) != 2) return 3;

  /* ..and the pointer's own does not: lvalue conversion drops it */
  if (_Generic(d, char *: 1, const char *: 2, default: 9) != 1) return 4;
  if (_Generic(m, int: 1, const int: 2, default: 9) != 1) return 5;
  if (_Generic(n, int: 1, volatile int: 2, default: 9) != 1) return 6;

  /* an array decays and keeps its ELEMENT qualifier */
  if (_Generic(e, char *: 1, const char *: 2, default: 9) != 2) return 7;
  if (_Generic(f, char *: 1, const char *: 2, default: 9) != 1) return 8;
  if (_Generic("x", char *: 1, const char *: 2, default: 9) != 1) return 9;

  /* a typedef carries what it was spelled with */
  if (_Generic(h, char *: 1, const char *: 2, default: 9) != 2) return 10;
  if (_Generic(i, char *: 1, const char *: 2, default: 9) != 1) return 11;

  /* members, and globals */
  if (_Generic(s.a, char *: 1, const char *: 2, default: 9) != 2) return 12;
  if (_Generic(s.b, char *: 1, const char *: 2, default: 9) != 1) return 13;
  if (_Generic(s.c, int: 1, volatile int: 2, default: 9) != 1) return 14;
  if (_Generic(g_ccp, char *: 1, const char *: 2, default: 9) != 2) return 15;
  if (_Generic(g_cp, char *: 1, const char *: 2, default: 9) != 1) return 16;
  if (_Generic(g_ci, int: 1, const int: 2, default: 9) != 1) return 17;

  /* const is not volatile, and both at once is a third type */
  if (_Generic(j, int *: 1, const int *: 2, volatile int *: 3,
                  const volatile int *: 4, default: 9) != 4) return 18;

  /* & rebuilds a pointer to the qualified thing; * takes one apart */
  if (_Generic(&m, int *: 1, const int *: 2, default: 9) != 2) return 19;
  if (_Generic(&k, int *: 1, const int *: 2, default: 9) != 1) return 20;
  if (_Generic(*a, char: 1, const char: 2, default: 9) != 1) return 21;
  if (_Generic(a[0], char: 1, const char: 2, default: 9) != 1) return 22;

  /* a row whose type-name is top-level qualified can never be selected --
   * no lvalue-converted controlling type is compatible with it */
  if (_Generic((int *)0, int * const: 1, default: 2) != 2) return 23;
  if (_Generic((const int *)0, int *: 1, int * const: 2, const int *: 3, default: 9) != 3) return 24;

  /* a block-scope declaration shadows the qualifier with the name */
  { const char *b2 = 0; if (_Generic(b2, char *: 1, const char *: 2, default: 9) != 2) return 25; }
  { char *a2 = 0; if (_Generic(a2, char *: 1, const char *: 2, default: 9) != 1) return 26; }
  if (_Generic(a, char *: 1, const char *: 2, default: 9) != 2) return 27;

  /* parameters bind like locals */
  if (p_c(a) != 2) return 28;
  if (p_p(b) != 1) return 29;
  if (p_ci(0) != 2) return 30;
  if (p_vi(0) != 3) return 31;
  return 0;
}
