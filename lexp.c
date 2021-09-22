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

// obviously this only works if the type names
// are all 4 bytes long (counting the NUL)
const uint32_t *tnoms = (uint32_t*)
 "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";

typedef obj read_loop(lips, FILE*, str, i64, i64);
static obj r1s(lips, FILE*), readz(lips, const char*);
static read_loop str_read_loop, atom_read_loop;

static Inline obj readx(lips v, char *msg) { return errp(v, msg), restart(v); }
static Inline obj read_loop_init(lips v, FILE *i, read_loop *loop) {
 return loop(v, i, cells(v, 2), 0, 8); }

static int r0(FILE *i) {
 for (int c;;) switch ((c = getc(i))) {
  case '#': case ';':
   do c = getc(i); while (c != '\n' && c != EOF);
  case ' ': case '\t': case '\n': continue;
  default: return c; } }

obj parse(lips v, FILE* i) {
 int c = r0(i);
 obj x, y;
 switch (c) {
  case EOF: return 0;
  case ')': return readx(v, err_rpar);
  case '(': return r1s(v, i);
  case '"': return read_loop_init(v, i, str_read_loop);
  case '\'': return x = pair(v, parse(v, i), nil), pair(v, Qt, x);
  default: return
   ungetc(c, i),
   x = read_loop_init(v, i, atom_read_loop),
   y = readz(v, chars(x)),
   nump(y) ? y : intern(v, x); } }

static obj r1s(lips v, FILE *i) {
 obj x, y, c = r0(i);
 switch (c) {
  case EOF: return readx(v, err_eof);
  case ')': return nil;
  default: return
   ungetc(c, i),
   x = parse(v, i),
   with(x, y = r1s(v, i)),
   pair(v, x, y); } }

static NoInline obj
read_loop_cont(lips v, FILE *i, str o, i64 n, i64 lim, read_loop *loop) {
 obj x; return
  o->len = n, x = putstr(o),
  o->text[n-1] == 0 ? x :
   (with(x, o = cells(v, 1 + b2w(2*n))),
    cpy8(o->text, getstr(x)->text, o->len = n),
    loop(v, i, o, n, 2 * n)); }

static obj atom_read_loop(lips v, FILE *p, str o, i64 n, i64 lim) {
 for (obj x; n < lim;) switch (x = getc(p)) {
  case ' ': case '\n': case '\t': case ';': case '#':
  case '(': case ')': case '\'': case '"':
   ungetc(x, p); case EOF:
   o->text[n++] = 0;
   goto out;
  default: o->text[n++] = x; } out:
 return read_loop_cont(v, p, o, n, lim, atom_read_loop); }

static obj str_read_loop(lips v, FILE *p, str o, i64 n, i64 lim) {
 for (obj x; n < lim;) switch (x = getc(p)) {
  case '\\': if ((x = getc(p)) == EOF) {
  case EOF: case '"': o->text[n++] = 0; goto out; }
  default: o->text[n++] = x; } out:
 return read_loop_cont(v, p, o, n, lim, str_read_loop); }

static NoInline obj readz_2(const char *s, i64 rad) {
 static const char *dig = "0123456789abcdef";
 if (!*s) return nil;
 i64 a = 0;
 for (int i, c; (c = *s++); a += i) {
  a *= rad, i = sidx(dig, cmin(c));
  if (i < 0 || i >= rad) return nil; }
 return N_(a); }

static NoInline obj readz_1(const char *s) {
 if (*s == '0') switch (cmin(s[1])) {
  case 'b': return readz_2(s+2, 2);
  case 'o': return readz_2(s+2, 8);
  case 'd': return readz_2(s+2, 10);
  case 'z': return readz_2(s+2, 12);
  case 'x': return readz_2(s+2, 16); }
 return readz_2(s, 10); }

static Inline obj readz(lips _, const char *s) {
 obj q;
 switch (*s) {
  case '-': return nump(q = readz_1(s+1)) ? N_(-N(q)) : q;
  case '+': s++;
  default: return readz_1(s); } }

u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, x, o), fputc(s, o); }

static u0 emstr(lips v, str s, FILE *o) {
 fputc('"', o);
 for (char *t = s->text; *t; fputc(*t++, o)) if (*t == '"') fputc('\\', o);
 fputc('"', o); }

static u0 emtbl(lips v, tbl t, FILE *o) {
 fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

static u0 emsym(lips v, sym y, FILE *o) {
 nilp(y->nom) ? fprintf(o, "#sym@%lx", (long) y) :
                fputs(chars(y->nom), o); }

static u0 emtwo_(lips v, two w, FILE *o) {
 twop(w->y) ? (emsep(v, w->x, o, ' '), emtwo_(v, gettwo(w->y), o)) :
              emsep(v, w->x, o, ')'); }

static u0 emtwo(lips v, two w, FILE *o) {
 w->x == Qt && twop(w->y) && nilp(Y(w->y)) ?
  (fputc('\'', o), emit(v, X(w->y), o)) :
  (fputc('(', o), emtwo_(v, w, o)); }

static u0 emvec(lips v, vec e, FILE *o) {
 fputc('[', o);
 if (e->len) for (mem i = e->xs, l = i + e->len;;) {
  emit(v, *i++, o);
  if (i < l) fputc(' ', o);
  else break; }
 fputc(']', o); }

static u0 emhomn(lips v, obj x, FILE *o) {
 fputc('\\', o);
 switch (kind(x)) {
  case Sym: return emit(v, x, o);
  case Two: if (symp(X(x))) emit(v, X(x), o);
            if (twop(Y(x))) emhomn(v, Y(x), o); } }

u0 emit(lips v, obj x, FILE *o) {
 switch (kind(x)) {
  case Hom: return emhomn(v, homnom(v, x), o);
  case Num: return (u0) fprintf(o, "%ld", (long) N(x));
  case Sym: return emsym(v, getsym(x), o);
  case Two: return emtwo(v, gettwo(x), o);
  case Str: return emstr(v, getstr(x), o);
  case Tbl: return emtbl(v, gettbl(x), o);
  case Vec: return emvec(v, getvec(x), o);
  default:  fputs("()", o); } }

u0 errp(lips v, char *msg, ...) {
 va_list xs;
 fputs("# ", stderr);
 va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
 fputc('\n', stderr); }
