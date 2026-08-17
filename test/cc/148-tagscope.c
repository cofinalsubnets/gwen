/* C11 6.7.2.3 -- a struct or union tag defined inside a block belongs to that block, so an
 * inner `struct T` neither reaches the file scope nor collides with an outer one of the same
 * spelling. Ours kept one flat tag table keyed by the spelling, so the LAST definition in the
 * TU laid out every earlier one's members -- gen resolves an offset long after the brace
 * closed, which made the collision a refusal and the escape a wrong answer.
 *
 * 147-enumscope.c holds the enum-constant half of block scope; this is the tag half, and the
 * two do NOT share a mechanism (a tag key rides out to gen, so it renames rather than retires).
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

struct T { int x; int y; };            /* file scope: 8 bytes */
union  U { int i; double d; };         /* file scope: 8 bytes */
struct Node { struct Node *next; int v; };

/* an inner tag does not escape: outer() still sees the file-scope T */
static int inner(void) { struct T { char c; }; return (int)sizeof(struct T); }
static int outer(void) { return (int)sizeof(struct T); }

/* two blocks, same spelling, different layouts -- and a MEMBER of each, which is the
 * part gen resolves after the whole TU is parsed */
static int first(void) {
  struct T { int a; int b; };
  struct T s;
  s.a = 11; s.b = 22;
  return s.b + (int)sizeof(struct T);   /* 22 + 8 */
}
static int second(void) {
  struct T { double pad; int z; };
  struct T s;
  s.pad = 0; s.z = 33;
  return s.z + (int)sizeof(struct T);   /* 33 + 16 */
}

/* the file-scope T is still itself here, members and all */
static int filescope(void) {
  struct T s;
  s.x = 5; s.y = 7;
  return s.x * 10 + s.y;                /* 57 */
}

/* a self-referential inner tag: the body's own name is the tag being defined */
static int selfref(void) {
  struct Node { struct Node *link; char small; };
  struct Node a, b;
  a.small = 3; b.small = 4; a.link = &b;
  return a.link->small + (int)sizeof(struct Node);  /* 4 + 16 */
}

/* ..and the FILE-scope Node is untouched by it */
static int filenode(void) {
  struct Node a, b;
  a.v = 9; b.v = 8; a.next = &b;
  return a.next->v + (int)sizeof(struct Node);      /* 8 + 16 */
}

/* nesting: an inner block's tag retires into the enclosing BLOCK's, not the file's */
static int nested(void) {
  struct T { char q[3]; };
  int r = (int)sizeof(struct T);                    /* 3 */
  { struct T { char q[5]; }; r += (int)sizeof(struct T); }   /* 5 */
  r += (int)sizeof(struct T);                       /* 3 again */
  return r;                                         /* 11 */
}

/* a union tag scopes the same way */
static int uni(void) {
  union U { char c[3]; };
  return (int)sizeof(union U);                      /* 3, not 8 */
}
static int fileuni(void) { return (int)sizeof(union U); }   /* 8 */

/* a block typedef over a block tag, and a tag declared with its declarator */
static int tdef(void) {
  typedef struct T { short s; short t; } T2;
  T2 v; v.s = 6; v.t = 7;
  return v.s * 10 + v.t + (int)sizeof(T2);          /* 67 + 4 */
}

/* a forward reference from inside a block still means the file-scope tag */
static int forward(struct Node *p) {
  struct Node *q = p;
  return q->v;
}

/* two sequential blocks in one function, each with its own T */
static int sequential(void) {
  int r = 0;
  { struct T { char q[2]; }; r += (int)sizeof(struct T); }   /* 2 */
  r += (int)sizeof(struct T);                                /* 8 */
  { struct T { char q[4]; }; r += (int)sizeof(struct T); }   /* 4 */
  r += (int)sizeof(struct T);                                /* 8 */
  return r;                                                  /* 22 */
}

int main(void) {
  struct Node n; n.v = 42; n.next = 0;
  int ok = 0;
  if (inner() == 1) ok++;
  if (outer() == 8) ok++;
  if (first() == 30) ok++;
  if (second() == 49) ok++;
  if (filescope() == 57) ok++;
  if (selfref() == 20) ok++;
  if (filenode() == 24) ok++;
  if (nested() == 11) ok++;
  if (uni() == 3) ok++;
  if (fileuni() == 8) ok++;
  if (tdef() == 71) ok++;
  if (forward(&n) == 42) ok++;
  if (sequential() == 22) ok++;
  if (sizeof(struct T) == 8 && sizeof(union U) == 8) ok++;
  printf("in=%d out=%d f=%d s=%d fs=%d sr=%d fn=%d ne=%d u=%d fu=%d td=%d fw=%d sq=%d ok=%d\n",
         inner(), outer(), first(), second(), filescope(), selfref(), filenode(), nested(),
         uni(), fileuni(), tdef(), forward(&n), sequential(), ok);
  return ok;
}
