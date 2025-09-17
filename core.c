#include "i.h"
#include <assert.h>

#ifndef g_version
#define g_version ""
#endif

static Inline g_core *g_ini_def(g_core *f, const char *k, word v) {
  f = g_push(f, 1, v);
  f = g_symof(f, k);
  return g_hash_put_2(f); }

g_core *g_run(g_core *f) {
  return !g_ok(f) ? f :
    f->ip->ap(f, f->ip, f->hp, f->sp); }

g_core *g_eval(g_core *f) {
  return g_eva(f, g_yield); }

g_word g_var(g_core *f, enum g_var n) {
  return f->vars[n]; }

enum g_status g_fin(g_core *f) {
  enum g_status g = g_code_of(f);
  if ((f = g_core_of(f))) {
    for (struct dtor *d = f->dtors; d; d = d->next) d->d(f, d->x);
    f->free(f, f); }
  return g; }

g_core *g_ini_m(void *(*g_malloc)(g_core*, size_t), void (*g_free)(g_core*, void*)) {
  const size_t len0 = 1024;
  g_core *f = g_malloc(NULL, sizeof(g_core) + 2 * len0 * sizeof(word));
  if (!f) return encode(f, g_status_oom);

  memset(f, 0, sizeof(core));
  word *pool = f->end;

  f->t0 = g_clock();
  f->len = len0;
  f->hp = f->pool = pool;
  f->sp = f->loop = pool + len0;
  f->malloc = g_malloc;
  f->free = g_free;
  static cell bif_yield[] = { {g_yield}, {.m = bif_yield} };
  f->ip = bif_yield;

  f = g_tbl(f);
  f = g_tbl(f);
  f = g_symof(f, ":");
  f = g_symof(f, "?");
  f = g_symof(f, "`");
  f = g_symof(f, ",");
  f = g_symof(f, "\\");

  if (g_ok(f))
    f->lambda = sym(pop1(f)),
    f->begin = sym(pop1(f)),
    f->quote = sym(pop1(f)),
    f->cond = sym(pop1(f)),
    f->let = sym(pop1(f)),
    f->macro = tbl(pop1(f)),
    f->dict = tbl(f->sp[0]);

  f = g_strof(f, g_version);
  f = g_symof(f, "version");
  f = g_hash_put_2(f);
  f = g_ini_def(f, "globals", (word) f->dict);
  f = g_ini_def(f, "macros", (word) f->macro);

  //f = g_push(f, 2, putnum(9), f->macro);
  //f = g_hash_put_2(f);

#define bifs(_) \
  _(bif_clock, "clock", S1(sysclock)) _(bif_isatty, "isatty", S1(p_isatty))\
  _(bif_add, "+", S2(add)) _(bif_sub, "-", S2(sub)) _(bif_mul, "*", S2(mul)) _(bif_quot, "/", S2(quot)) _(bif_rem, "%", S2(rem)) \
  _(bif_lt, "<", S2(lt))  _(bif_le, "<=", S2(le)) _(bif_eq, "=", S2(eq)) _(bif_ge, ">=", S2(ge))  _(bif_gt, ">", S2(gt)) \
  _(bif_bnot, "~", S1(bnot)) _(bif_rand, "rand", S1(rng)) \
  _(bif_cons, "X", S2(cons)) _(bif_car, "A", S1(car)) _(bif_cdr, "B", S1(cdr)) \
  _(bif_sget, "sget", S2(sget)) _(bif_ssub, "ssub", S3(ssub)) _(bif_slen, "slen", S1(slen)) _(bif_scat, "scat", S2(scat)) \
  _(bif_read, "read", S1(read0)) _(bif_readf, "readf", S1(readf)) _(bif_dot, ".", S1(dot)) _(bif_putc, "putc", S1(prc)) \
  _(bif_sym, "sym", S1(gensym)) _(bif_nom, "nom", S1(symnom))\
  _(bif_thd, "thd", S1(thda)) _(bif_peek, "peek", S1(peek)) _(bif_poke, "poke", S2(poke)) _(bif_trim, "trim", S1(trim)) _(bif_seek, "seek", S2(seek)) \
  _(bif_tnew, "tnew", S1(tnew)) _(bif_tkeys, "tkeys", S1(tkeys)) _(bif_tlen, "tlen", S1(tlen)) _(bif_tset, "tset", S3(tset)) _(bif_tget, "tget", S3(tget)) _(bif_tdel, "tdel", S3(tdel))\
  _(bif_twop, "twop", S1(pairp)) _(bif_strp, "strp", S1(stringp)) _(bif_symp, "symp", S1(symbolp)) _(bif_nump, "nump", S1(fixnump)) _(bif_nilp, "nilp", S1(nullp))\
  _(bif_ev, "ev", S1(ev0))
#define S1(i) {{i}, {ret0}}
#define S2(i) {{curry},{.x=putnum(2)},{i}, {ret0}}
#define S3(i) {{curry},{.x=putnum(3)},{i}, {ret0}}
#define built_in_function(n, _, d) static const cell n[] = d;
  bifs(built_in_function);
#define bif_dict_entry(bn, n, _) f = g_ini_def(f, n, W(bn));
  bifs(bif_dict_entry);
#define insts(_) _(free_variable) _(ret) _(ap) _(tap) _(apn) _(tapn) _(jump) _(cond) _(ref) _(imm) _(drop1) _(curry) _(defglob) _(late_bind) _(ret0)
#define i_dict_entry(i) f = g_ini_def(f, "i_"#i, W(i));
  insts(i_dict_entry);
#define height(f) (f->pool+f->len-f->sp)

  /*
  f = g_push(f, 3, f->dict, nil, f->macro),
  f = g_hash_put(f),
  f = g_pop(f, 1);
  */

  f = g_pop(f, 1);
  assert(height(f) == 0);
  return f; }
