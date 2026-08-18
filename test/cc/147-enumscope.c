/* C11 6.2.1p7 -- an enum constant declared in a BLOCK has that block's scope, so it
 * comes back off at the closing brace. Ours pinned into one flat table and never took
 * it off, so a constant declared inside one function answered in every later one.
 * 146-declscope.c holds the other half of the same rule (a declarator is in scope for
 * the initializers after it); 78-enumshadow.c holds the local-hides-a-constant case.
 *
 * The shape that shows it: a file-scope constant and a block-scope one of the SAME
 * name. Both spellings compile either way, so only the VALUE says which scope won.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

enum { E = 3, F = 1, G = 6, P = 2, Q = 5 };

/* the escape itself: 99 in here, and E is still 3 everywhere after */
static int leaker(void) { enum { E = 99 }; return E; }
static int reader(void) { return E; }

/* a nested block's constant retires into the enclosing block's, not the file's */
static int nested(void) {
  enum { E = 20 };
  int r = E;
  { enum { E = 300 }; r += E; }
  r += E;
  return r; /* 20 + 300 + 20 */
}

/* a block constant over a file-scope one, restored at the brace */
static int overfile(void) {
  int r = F;
  { enum { F = 10 }; r += F; }
  r += F;
  return r; /* 1 + 10 + 1 */
}

/* a LOCAL over a block constant -- the two shadow paths stacking */
static int localover(void) {
  enum { G = 40 };
  int r = G;
  { int G = 200; r += G; }
  r += G;
  return r; /* 40 + 200 + 40 */
}

/* the typedef arm parses its own enum body, so it drains too */
static int tdleaker(void) { typedef enum { P = 88 } pt; pt v = (pt)1; return P + (int)v; }
static int tdreader(void) { return P; }

/* a tagged block enum WITH a declarator: the constants go under the declarator's
 * own shadowing, and the tag's signedness does not follow the constant out */
static int tagged(void) {
  enum q { Q = 50 } v = Q;
  return (int)v + Q;
}
static int tagreader(void) { return Q; }

/* a block constant is gone for a LATER block in the same function, too */
static int sequential(void) {
  int r = 0;
  { enum { E = 7 }; r += E; }
  r += E;
  { enum { E = 8 }; r += E; }
  r += E;
  return r; /* 7 + 3 + 8 + 3 */
}

int main(void) {
  int ok = 0;
  if (leaker() == 99) ok++;
  if (reader() == 3) ok++;
  if (nested() == 340) ok++;
  if (overfile() == 12) ok++;
  if (localover() == 280) ok++;
  if (tdleaker() == 89) ok++;
  if (tdreader() == 2) ok++;
  if (tagged() == 100) ok++;
  if (tagreader() == 5) ok++;
  if (sequential() == 21) ok++;
  if (E == 3 && F == 1 && G == 6 && P == 2 && Q == 5) ok++;
  printf("leak=%d read=%d nest=%d over=%d loc=%d td=%d tdr=%d tag=%d tagr=%d seq=%d ok=%d\n",
         leaker(), reader(), nested(), overfile(), localover(), tdleaker(), tdreader(),
         tagged(), tagreader(), sequential(), ok);
  return ok;
}
