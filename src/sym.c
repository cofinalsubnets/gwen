#include "i.h"

static Inline void ini_sym(symbol *y, string *nom, uintptr_t code) {
  y->ap = data;
  y->typ = &sym_type;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0; }

static Inline void ini_anon(symbol *y, g_word code) {
  y->ap = data;
  y->typ = &sym_type;
  y->nom = 0;
  y->code = code; }

static symbol *g_intern_r(g_core *v, string *b, symbol **y) {
  symbol *z = *y;
  if (!z) return // found an empty spot, insert new symbol
    z = bump(v, Width(g_symbol)),
    ini_sym(z, b, hash(v, putnum(hash(v, (g_word) b)))),
    *y = z;
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
  if (strp(Sp[0])) return Ap(nomsym, f);
  const int req = Width(symbol) - 2;
  Have(req);
  symbol *y = (symbol*) Hp;
  Hp += req;
  ini_anon(y, g_clock());
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

Vm(symnom) {
  g_word y = Sp[0];
  y = symp(y) && ((symbol*)y)->nom ? word(((symbol*)y)->nom) : nil;
  Sp[0] = y;
  Ip += 1;
  return Continue(); }

static g_word cp_sym(g_core *f, g_word x, g_word *p0, g_word *t0);
static uintptr_t xx_sym(g_core *v, g_word _);
static void wk_sym(g_core *f, g_word x, g_word *p0, g_word *t0);
static g_core * em_sym(g_core *f, g_file o, g_word x),
              *show_sym(g_core*, g_word);
g_type sym_type = {
  .xx = xx_sym,
  .cp = cp_sym,
  .wk = wk_sym,
  .eq = neql,
  .em = em_sym,
  .show = show_sym,
  .ap = self, };

static uintptr_t xx_sym(g_core *v, g_word _) { return sym(_)->code; }

static g_word cp_sym(g_core *f, g_word x, g_word *p0, g_word *t0) {
  g_symbol *src = sym(x), *dst;
  if (src->nom) dst = g_intern_r(f, str(cp(f, word(src->nom), p0, t0)), &f->symbols);
  else dst = bump(f, Width(symbol) - 2),
       ini_anon(dst, src->code);
  return (g_word) (src->ap = (g_vm*) dst); }

static void wk_sym(g_core *f, g_word x, g_word *p0, g_word *t0) {
  f->cp += Width(symbol) - (sym(x)->nom ? 0 : 2); }

static g_core *em_sym(g_core *f, g_file o, g_word x) {
  string* s = sym(x)->nom;
  if (s) for (int i = 0; i < s->len; g_fputc(s->text[i++], o));
  else g_fprintf(o, "#sym@%lx", (long) x);
  return f; }

static g_core*show_sym(g_core*f,g_word x) {
  g_word n = (g_word) ((g_symbol*) x)->nom;
  return g_push(f, 1, n ? n : nil); }

Vm(symbolp) {
  Sp[0] = symp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }
