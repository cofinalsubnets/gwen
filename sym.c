#include "i.h"

static symbol *intern_seek(core *v, string *b, symbol **y) {
  symbol *z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(symbol)), b, hash(v, putnum(hash(v, (word) b))));
  string *a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_seek(v, b, i < 0 ? &z->l : &z->r); }

symbol *intern(core *f, string* b) {
  if (avail(f) < Width(symbol)) {
    bool ok;
    avec(f, b, ok = g_please(f, Width(symbol)));
    if (!ok) return 0; }
  return intern_seek(f, b, &f->symbols); }

g_core *g_intern_c(g_core *f) {
  if (!g_ok(f)) return f;
  f = avail(f) < Width(symbol) ? please(f, Width(symbol)) : f;
  if (g_ok(f)) f->sp[0] = (g_word) intern_seek(f, str(f->sp[0]), &f->symbols);
  return f; }

symbol *ini_sym(symbol *y, string *nom, uintptr_t code) {
  return y->ap = data, y->typ = &sym_type, y->nom = nom, y->code = code, y->l = y->r = 0, y; }
static symbol *ini_anon(symbol *y, word code) {
  return y->ap = data, y->typ = &sym_type, y->nom = 0, y->code = code, y; }
Vm(nomsym) {
  Have(Width(symbol));
  symbol *y; return
    Pack(f),
    y = intern_seek(f, str(f->sp[0]), &f->symbols),
    Unpack(f),
    Sp[0] = W(y),
    Ip++,
    Continue(); }

Vm(gensym) {
  if (strp(Sp[0])) return Ap(nomsym, f);
  const int req = Width(symbol) - 2;
  Have(req);
  symbol *y = (symbol*) Hp;
  return Hp += req, Sp[0] = W(ini_anon(y, rand())), Ip++, Continue(); }

Vm(symnom) {
  word y = Sp[0];
  y = symp(y) && ((symbol*)y)->nom ? W(((symbol*)y)->nom) : nil;
  return Sp[0] = y, Ip++, Continue(); }

static word cp_sym(core *f, word x, word *p0, word *t0);
static uintptr_t xx_sym(core *v, word _);
static void
  wk_sym(core *f, word x, word *p0, word *t0),
  em_sym(core *f, FILE *o, word x);
type
  sym_type = { .xx = xx_sym, .cp = cp_sym, .wk = wk_sym, .eq = neql, .em = em_sym, };

static uintptr_t xx_sym(core *v, word _) { return sym(_)->code; }
static word cp_sym(core *f, word x, word *p0, word *t0) {
  symbol *src = sym(x),
         *dst = src->nom ?
           intern_seek(f, str(cp(f, W(src->nom), p0, t0)), &f->symbols) :
           ini_anon(bump(f, Width(symbol) - 2), src->code);
  return (word) (src->ap = (vm*) dst); }

static void wk_sym(core *f, word x, word *p0, word *t0) {
  f->cp += Width(symbol) - (sym(x)->nom ? 0 : 2); }

static void em_sym(core *f, FILE *o, word x) {
  string* s = sym(x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }
