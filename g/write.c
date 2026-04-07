#include "i.h"

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
g_vm(g_vm_putc) {
 gputc(f, ggetnum(*Sp));
 Ip += 1;
 return Continue(); }
g_vm(g_vm_puts) {
 if (strp(Sp[0])) {
  struct g_vec *s = vec(Sp[0]);
  for (intptr_t i = 0; i < len(s);) gputc(f, txt(s)[i++]);
  gflush(f); }
 Ip += 1;
 return Continue(); }

g_vm(g_vm_putn) {
 uintptr_t n = ggetnum(Sp[0]), b = ggetnum(Sp[1]);
 g_putn(f, &g_stdout, n, b);
 Sp[1] = Sp[0];
 Sp += 1;
 Ip += 1;
 return Continue(); }

g_vm(g_vm_dot) {
 gfputx(f, &g_stdout, Sp[0]);
 Ip += 1;
 return Continue(); }

