#include "i.h"

#define S1(i) ((Cell[]){{i}})
#define S2(i) ((Cell[]){{curry},{.x=putnum(2)},{i}})
#define S3(i) ((Cell[]){{curry},{.x=putnum(3)},{i}})

#define dict_entry(n, d) {n, d},
#define bifs(_) \
  _("+", S2(add)) _("-", S2(sub)) _("*", S2(mul)) _("/", S2(quot)) _("%", S2(rem)) \
  _("<", S2(lt))  _("<=", S2(le)) _("=", S2(eq))  _(">=", S2(ge))  _(">", S2(gt)) \
  _("rand", S1(rng)) \
  _("X", S2(cons)) _("A", S1(car)) _("B", S1(cdr)) \
  _("sget", S2(sget)) _("ssub", S3(ssub)) _("slen", S1(slen)) _("scat", S2(scat)) \
  _(".", S1(display)) _("putc", S1(prc)) \
  _("~", S1(bnot)) \
  _("thd", S1(thda)) _("peek", S1(peek)) _("poke", S2(poke))\
  _("trim", S1(trim)) _("seek", S2(seek)) \
  _("tnew", S1(tnew)) _("tkeys", S1(tkeys)) _("tlen", S1(tlen))\
  _("tset", S3(tset)) _("tget", S3(tget)) _("tdel", S3(tdel))\
  _("twop", S1(pairp)) _("strp", S1(stringp))\
  _("symp", S1(symbolp)) _("nump", S1(fixnump))\
  _("sym", S1(gensym)) _("nom", S1(symnom))\
  _("ev", S1(ev0)) _("::", S2(defmacro)) \
  _("read", S1(read0)) _("readf", S1(readf))

static struct { const char *n; Cell *v; } ini_dict[] = { bifs(dict_entry) };

static Symbol *literal_symbol(Core *f, const char *nom) {
  String *o = literal_string(f, nom);
  return o ? intern(f, o) : 0; }

static NoInline bool p_define(Core *f, const char *k, Word v) {
  Symbol *y;
  avec(f, v, y = literal_symbol(f, k));
  return y && table_set(f, f->vars.dict, (Word) y, v); }

Vm(mbind, int s) {
#if TCO
  return s == Ok ? Ap(Ip->ap, f) : s;
#else
  return s;
#endif
}

static void p_fin(Core *f) { free(f->pool < f->loop ? f->pool : f->loop); }
static Status p_ini(Core *f) {
  memset(f, 0, sizeof(Core));
  const uintptr_t len0 = 1;
  Word *pool = malloc(2 * len0 * sizeof(Word));
  if (!pool) return Oom;
  f->t0 = clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
  f->bind = mbind;
  if (!(f->vars.dict = new_table(f)) ||
      !(f->vars.macro = new_table(f)) ||
      !(f->vars.quote = literal_symbol(f, "`")) ||
      !(f->vars.begin = literal_symbol(f, ",")) ||
      !(f->vars.let = literal_symbol(f, ":")) ||
      !(f->vars.cond = literal_symbol(f, "?")) ||
      !(f->vars.lambda = literal_symbol(f, "\\")) ||
      !p_define(f, "global-namespace", (Word) f->vars.dict))
    return p_fin(f), Oom;
  for (long i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!p_define(f, ini_dict[i].n, (Word) ini_dict[i].v))
      return p_fin(f), Oom;
  return Ok; }

void p_close(Core *f) { p_fin(f), free(f); }
Core *p_open(void) {
  Core *f = malloc(sizeof(Core));
  return !f || p_ini(f) == Ok ? f : (free(f), NULL); }
