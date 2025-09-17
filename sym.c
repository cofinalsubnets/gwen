#include "i.h"

g_core *g_symof(g_core *f, const char *nom) {
  f = g_strof(f, nom);
  return g_intern(f); }

static Inline void ini_sym(symbol *y, string *nom, uintptr_t code) {
  y->ap = data;
  y->typ = &sym_type;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0; }

static Inline void ini_anon(symbol *y, word code) {
  y->ap = data;
  y->typ = &sym_type;
  y->nom = 0;
  y->code = code; }

static symbol *g_intern_r(core *v, string *b, symbol **y) {
  symbol *z = *y;
  if (!z) { // found an empty spot, insert new symbol
    z = bump(v, Width(symbol)),
    ini_sym(z, b, hash(v, putnum(hash(v, (word) b)))),
    *y = z;
    return z; }
  string *a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z :
    g_intern_r(v, b, i < 0 ? &z->l : &z->r); }

g_core *g_intern(g_core *f) {
  f = g_have(f, Width(symbol));
  if (g_ok(f)) f->sp[0] = (g_word) g_intern_r(f, str(f->sp[0]), &f->symbols);
  return f; }

Vm(nomsym) {
  Have(Width(symbol));
  symbol *y;
  Pack(f);
  y = g_intern_r(f, str(f->sp[0]), &f->symbols),
  Unpack(f);
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

Vm(gensym) {
  if (strp(Sp[0])) return nomsym(f, Ip, Hp, Sp);
  const int req = Width(symbol) - 2;
  Have(req);
  symbol *y = (symbol*) Hp;
  Hp += req;
  ini_anon(y, rand());
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

Vm(symnom) {
  word y = Sp[0];
  y = symp(y) && ((symbol*)y)->nom ? word(((symbol*)y)->nom) : nil;
  Sp[0] = y;
  Ip += 1;
  return Continue(); }

static word cp_sym(core *f, word x, word *p0, word *t0);
static uintptr_t xx_sym(core *v, word _);
static void
  wk_sym(core *f, word x, word *p0, word *t0),
  em_sym(core *f, FILE *o, word x);
type
  sym_type = { .xx = xx_sym, .cp = cp_sym, .wk = wk_sym, .eq = neql, .em = em_sym, };

static uintptr_t xx_sym(core *v, word _) { return sym(_)->code; }

static word cp_sym(core *f, word x, word *p0, word *t0) {
  symbol *src = sym(x), *dst;
  if (src->nom) dst = g_intern_r(f, str(cp(f, W(src->nom), p0, t0)), &f->symbols);
  else dst = bump(f, Width(symbol) - 2),
       ini_anon(dst, src->code);
  return (word) (src->ap = (vm*) dst); }

static void wk_sym(core *f, word x, word *p0, word *t0) {
  f->cp += Width(symbol) - (sym(x)->nom ? 0 : 2); }

static void em_sym(core *f, FILE *o, word x) {
  string* s = sym(x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }
