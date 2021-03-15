#include "lips.h"
////
/// lisp parser
//
// this should be portable to lisp as soon as
// the string processing primitives are good
// enough, at which point it can be called the
// bootstrap parser
#define err_eof "unexpected eof"
#define err_rpar "unmatched right delimiter"

NoInline const char *tnom(enum type t) { switch (t) {
  case Hom: return "hom";
  case Num: return "num";
  case Tbl: return "table";
  case Two: return "pair";
  case Tup: return "tuple";
  case Oct: return "string";
  case Sym: return "symbol";
  default:  return "nil"; } }

typedef obj P(vm, FILE*);
static P atom, reads, quote, str;

#define readx(v,m)(errp(v,"parse",0,m),0)

static int read0(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#':
    case ';': do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

obj parse(vm v, FILE *i) {
  for (int c;;) switch ((c = read0(i))) {
    case EOF:  return 0;
    case ')':  return readx(v, err_rpar);
    case '(':  return reads(v, i);
    case '"':  return str(v, i);
    case '\'': return quote(v, i);
    default:   return ungetc(c, i), atom(v, i); } }

static obj quote(vm v, FILE *i) {
  obj r = parse(v, i);
  return !r ? r :
   (r = pair(v, r, nil), pair(v, Qt, r)); }

static obj reads(vm v, FILE *i) {
  obj x, y, c;
  switch ((c = read0(i))) {
    case EOF: return readx(v, err_eof);
    case ')': return nil;
    default:  ungetc(c, i);
              if (!(x = parse(v, i))) return x;
              with(x, y = reads(v, i));
              return y ? pair(v, x, y) : y; } }

static obj rloop(vm v, FILE *i, oct o, num n, num lim,
  obj (*re)(vm, FILE*, oct, num, num)) {
  obj x;
  o->len = n, x = putoct(o);
  return o->text[n-1] == 0 ? x :
    (with(x, o = cells(v, 1 + b2w(2*n))),
     memcpy(o->text, getoct(x)->text, o->len = n),
     re(v, i, o, n, 2 * n)); }

static obj atom_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0;
      goto out;
    default: o->text[n++] = x; } out:
  return rloop(v, p, o, n, lim, atom_); }

static obj str_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case '\\': if ((x = fgetc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0; goto out; }
    default: o->text[n++] = x; } out:
  return rloop(v, p, o, n, lim, str_); }

static obj atom(vm v, FILE *i) {
  obj o = atom_(v, i, cells(v, 2), 0, 8);
  char *st = NULL;
  num j = strtol(chars(o), &st, 0);
  return !st || *st ? intern(v, o) : putnum(j); }

static obj str(vm v, FILE *i) {
  return str_(v, i, cells(v, 2), 0, 8); }

void emsep(vm v, obj x, FILE *o, char s) {
  emit(v, x, o), fputc(s, o); }

static void emit_2(vm v, obj x, FILE *o) {
  twop(Y(x)) ? (emsep(v, X(x), o, ' '), emit_2(v, Y(x), o)) :
                emsep(v, X(x), o, ')'); }

static void emhomn(vm v, obj x, FILE *o) {
  fputc('\\', o);
  switch (kind(x)) {
    case Two:
      if (symp(X(x))) emit(v, X(x), o);
      if (nilp(Y(x))) return;
      return emhomn(v, Y(x), o);
    case Sym: return emit(v, x, o); } }

static void emoct(vm v, obj x, FILE *o) {
  fputc('"', o);
  oct s = getoct(x);
  for (num i = 0, l = s->len - 1; i < l; i++)
    if (s->text[i] == '"') fputs("\\\"", o);
    else fputc(s->text[i], o);
  fputc('"', o); }
static void emtbl(vm v, obj x, FILE *o) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

void emit(vm v, obj x, FILE *o) {
  switch (kind(x)) {
    case Num: fprintf(o, "%ld", getnum(x)); break;
    case Sym: fputs(symnom(x), o); break;
    case Hom: return emhomn(v, homnom(v, x), o);
    case Two: fputc('(', o); emit_2(v, x, o); break;
    case Oct: return emoct(v, x, o);
    case Tbl: return emtbl(v, x, o);
    default: fputs("()", o); } }

void vferrp(vm v, FILE *o, const char *seg, obj x, const char *msg, va_list xs) {
  if (seg) fputc('[', o), fputs(seg, o), fputs("] ", o);
  if (x) emsep(v, x, o, ' '), fputs(";; ", o);
  vfprintf(o, msg, xs), fputc('\n', o); }

void errp(vm v, const char *seg, obj x, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg), vferrp(v, stderr, seg, x, msg, xs), va_end(xs); }

obj err(vm v, const char *seg, obj x, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg); vferrp(v, stderr, seg, x, msg, xs); va_end(xs);
  return restart(v); }
