#include "i.h"
struct g_in g_stdin = { .getc = (void*) ggetc, .ungetc = (void*) gungetc, .eof = (void*) geof };
struct g_out g_stdout = { .putc = (void*) gputc, .flush = (void*) gflush };
static struct g *grbufn(struct g *f) {
 f = g_have(f, str_type_width + 2);
 if (g_ok(f)) {
  union u *k = bump(f, str_type_width + 1);
  *--f->sp = word(k);
  struct g_vec *o = (struct g_vec*) k;
  ini_str(o, sizeof(intptr_t)); }
 return f; }

static struct g *grbufg(struct g *f) {
 size_t len = len(f->sp[0]),
        req = str_type_width + 2 * b2w(len);
 f = g_have(f, req);
 if (g_ok(f)) {
  struct g_vec *o = bump(f, req);
  ini_str(o, 2 * len);
  memcpy(txt(o), txt(f->sp[0]), len);
  f->sp[0] = (g_num) o; }
 return f; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int g_r_getc(struct g*f, struct g_in *i) {
 for (int c;;) switch (c = i->getc(f, i)) {
  default: return c;
  case '#': case ';':
   while (!i->eof(f, i) && (c = i->getc(f, i)) != '\n' && c != '\r');
  case 0: case ' ': case '\t': case '\n': case '\r': case '\f':
   continue; } }

long strtol(char const*restrict, char**restrict, int);

struct g *g_read1(struct g*f, struct g_in* i) {
 if (!g_ok(f)) return f;
 int c = g_r_getc(f, i);
 switch (c) {
  case '(':  return g_reads(f, i);
  case ')': case EOF:  return encode(f, g_status_eof);
  case '\'': return
   f = gxr(g_push(g_read1(f, i), 1, g_nil)),
   g_ok(f) ? gxl(g_push(f, 1, f->quote)) : f;
  case '"': {
   size_t n = 0;
   f = grbufn(f);
   for (size_t lim = sizeof(g_word); g_ok(f); f = grbufg(f), lim *= 2)
    for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
     if ((c = i->getc(f, i)) == EOF || c == '"' ||
         (c == '\\' && (c =i->getc(f, i)) == EOF))
      return len(b) = n, f;
   return f; } }

 uintptr_t n = 1, lim = sizeof(intptr_t);
 f = grbufn(f);
 if (g_ok(f))
  for (txt(f->sp[0])[0] = c; g_ok(f); f = grbufg(f), lim *= 2)
   for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
    switch (c = i->getc(f, i)) {
     default: continue;
     case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
     case '(': case ')': case '"': case '\'': case 0 : case EOF:
      i->ungetc(f, c, i);
      len(b) = n;
      txt(b)[n] = 0; // zero terminate for strtol ; n < lim so this is safe
      char *e;
      long j = strtol(txt(b), &e, 0);
      if (*e == 0) f->sp[0] = gputnum(j);
      else f = g_intern(f);
      return f; }
 return f; }

struct g *g_reads(struct g *f, struct g_in* i) {
 intptr_t n = 0;
 for (int c; g_ok(f); n++) {
  c = g_r_getc(f, i);
  if (c == EOF || c == ')') break;
  i->ungetc(f, c, i);
  f = g_read1(f, i); }
 for (f = g_push(f, 1, g_nil); n--; f = gxr(f));
 return f; }


int g_putn(struct g *f, struct g_out *o, intptr_t n, uintptr_t base) {
 if (n < 0) o->putc(f, '-', o), n = -n;
 uintptr_t q = n / base, r = n % base;
 if (q) g_putn(f, o, q, base);
 return o->putc(f, g_digits[r], o); }

static int gvfprintf(struct g*f, struct g_out*o, char const *fmt, va_list xs) {
 int c;
 while ((c = *fmt++)) {
  if (c != '%') o->putc(f, c, o);
  else pass: switch ((c = *fmt++)) {
   case 0: return c;
   case 'l': goto pass;
   case 'b': c = g_putn(f, o, va_arg(xs, uintptr_t), 2); continue;
   case 'o': c = g_putn(f, o, va_arg(xs, uintptr_t), 8); continue;
   case 'd': c = g_putn(f, o, va_arg(xs, uintptr_t), 10); continue;
   case 'x': c = g_putn(f, o, va_arg(xs, uintptr_t), 16); continue;
   default: c = o->putc(f, c, o); } }
 return c; }

static int gfprintf(struct g *f, struct g_out *o, char const *fmt, ...) {
 va_list xs;
 va_start(xs, fmt);
 int r = gvfprintf(f, o, fmt, xs);
 va_end(xs);
 return r; }

typedef int g_wr_t(struct g*, struct g_out*, intptr_t);
static g_wr_t g_em_two, g_em_vec, g_em_sym, g_em_tab;
static g_wr_t *emitters[] = {
 [two_class] = g_em_two,
 [vec_class] = g_em_vec,
 [tbl_class] = g_em_tab,
 [sym_class] = g_em_sym, };

static int g_em_tab(struct g*f, struct g_out*o, intptr_t x) {
 struct g_tab *t = tbl(x);
 return gfprintf(f, o, "#tab:%d/%d@%x", t->len, t->cap, x); }

static int g_em_sym(struct g*f, struct g_out*o, intptr_t x) {
 int r = 0;
 struct g_vec * s = sym(x)->nom;
 if (s && vec_strp(s)) for (intptr_t i = 0; i < len(s); r = o->putc(f, txt(s)[i++], o));
 else r = gfprintf(f, o, "#sym@%x", (intptr_t) x);
 return r; }
int gfputx(struct g *f, struct g_out *o, intptr_t x) {
 return nump(x) ? gfprintf(f, o, "%d", (g_num) ggetnum(x)) :
        datp(x) ? emitters[typ(x)](f, o, x) :
                  gfprintf(f, o, "#%lx", (long) x); }
static int g_em_two(struct g*f, struct g_out*o, intptr_t x) {
 if (A(x) == word(f->quote) && twop(B(x))) return
  o->putc(f, '\'', o),
  gfputx(f, o, AB(x));
 for (o->putc(f, '(', o);; o->putc(f, ' ', o)) {
  gfputx(f, o, A(x));
  if (!twop(x = B(x))) return o->putc(f, ')', o); } }

static int g_em_vec(struct g*f, struct g_out*o, intptr_t x) {
  int r = 0;
  struct g_vec *v = vec(x);
  if (!vec_strp(v)) {
   intptr_t rank = v->rank, *shape = v->shape;
   r = gfprintf(f, o, "#vec@%x:%d.%d", (intptr_t) x, (intptr_t) v->type, (intptr_t) v->rank);
   for (intptr_t i = rank, *j = shape; i--; r = gfprintf(f, o, ".%d", (intptr_t) *j++)); }
  else {
   uintptr_t len = vlen(v);
   char *text = vtxt(v);
   o->putc(f, '"', o);
   for (char c; len--; o->putc(f, c, o))
    if ((c = *text++) == '\\' || c == '"') o->putc(f, '\\', o);
   r = o->putc(f, '"', o); }
  return r; }

g_vm(g_vm_dot) {
 gfputx(f, &g_stdout, Sp[0]);
 Ip += 1;
 return Continue(); }

g_vm(g_vm_read) {
 Pack(f);
 f = g_read1(f, &g_stdin);
 if (g_code_of(f) == g_status_eof) return // no error but end of file
  f = g_core_of(f),
  Unpack(f),
  Ip += 1,
  Continue();
 f = gxl(f);
 if (!g_ok(f)) return f;
 return Unpack(f),
        Ip += 1,
        Continue(); }

g_vm(g_vm_putn) {
 uintptr_t n = ggetnum(Sp[0]), b = ggetnum(Sp[1]);
 g_putn(f, &g_stdout, n, b);
 Sp[1] = Sp[0];
 Sp += 1;
 Ip += 1;
 return Continue(); }

g_vm(g_vm_puts) {
 if (strp(Sp[0])) {
  struct g_vec *s = vec(Sp[0]);
  for (intptr_t i = 0; i < len(s);) gputc(f, txt(s)[i++]);
  gflush(f); }
 Ip += 1;
 return Continue(); }

g_vm(g_vm_getc) {
 Pack(f);
 int i = ggetc(f);
 Unpack(f);
 Sp[0] = gputnum(i);
 Ip += 1;
 return Continue(); }

g_vm(g_vm_putc) {
 gputc(f, ggetnum(*Sp));
 Ip += 1;
 return Continue(); }
