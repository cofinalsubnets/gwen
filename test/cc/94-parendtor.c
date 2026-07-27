/* parenthesized declarators, the recursive lane (pdirect). three shapes that
   real corpora hit: the REDUNDANT (name) -- lua wraps every API declarator so
   a same-named macro can coexist; the qualified pointer (*const p) -- sqlite's
   IOMETHODS finder table; and the NESTED (*(*x)(a))(b) -- sqlite's xDlSym, a
   fn-ptr-returning-fn-ptr struct member. the type grows inside-out: a paren
   declarator's pointee is what trails its `)`. exit agrees with gcc only if
   layout, calls-through, and the def's bound params all line up. */

/* the redundant (name): proto, def, and a paren-of-paren */
int (add3)(int a, int b, int c);
int (add3)(int a, int b, int c) { return a + b + c; }
int ((mul2))(int x) { return x * 2; }

/* (*const p): a const-qualified pointer inside the parens, initialized */
static int fortyone(void) { return 41; }
static int (*const FINDER)(void) = fortyone;

/* the nested member: x is a ptr to fn(int) returning a ptr to fn(void)->int */
static int nine(void) { return 9; }
static int (*sel(int k))(void) { return k ? nine : fortyone; }
struct vfs { int (*(*xsym)(int))(void); };

/* an array of fn pointers via the paren declarator, and a ptr-to-array */
static int (*tab[2])(void);

int main(void)
{
  struct vfs v;
  int a[3] = { 1, 2, 3 };
  int (*pa)[3] = &a;
  int r = 0;
  v.xsym = sel;
  tab[0] = fortyone; tab[1] = nine;
  r += (add3)(1, 2, 3);        /* 6 -- the call side parens too */
  r += mul2(5);                /* 10 */
  r += FINDER();               /* 41 */
  r += v.xsym(1)();            /* 9 */
  r += v.xsym(0)();            /* 41 */
  r += tab[1]();               /* 9 */
  r += (*pa)[2];               /* 3 */
  return r;                    /* 119 */
}
