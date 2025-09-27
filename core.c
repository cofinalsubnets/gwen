#include "i.h"

enum g_status g_fin(g_core *f) {
  enum g_status s = g_code_of(f);
  if ((f = g_core_of(f))) g_free(f->pool);
  return s; }

static NoInline g_core *g_ini_def(g_core *f, const char *k, g_word v) {
  return g_hash_put(g_symof(g_push(f, 1, v), k)); }


#define S1(i) {{i}, {ret0}}
#define S2(i) {{curry},{.x=putnum(2)},{i}, {ret0}}
#define S3(i) {{curry},{.x=putnum(3)},{i}, {ret0}}
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

#define built_in_function(n, _, d) static const union g_cell n[] = d;
bifs(built_in_function);
static union g_cell bif_yield[] = { {g_yield}, {.m = bif_yield} };

#define sy(b, c) g_symof(c, b)
#define st(b, c) g_strof(c, b)
#define de(a, b, c) g_ini_def(c, a, b)
#ifndef g_version
#define g_version ""
#endif
#define bif_dict_entry(bn, n, _) f = g_ini_def(f, n, word(bn));
#define insts(_) _(free_variable) _(ret) _(ap) _(tap) _(apn) _(tapn) _(jump) _(cond) _(ref) _(imm) _(drop1) _(curry) _(defglob) _(late_bind) _(ret0)
#define i_dict_entry(i) f = g_ini_def(f, "i_"#i, (g_word)(i));

struct g_core *g_ini(void) {
  const size_t len0 = 1 << 10;
  struct g_core *f = g_malloc(2 * len0 * sizeof(g_word));
  if (!f) return encode(f, g_status_oom);
  memset(f, 0, sizeof(struct g_core));
  f->t0 = g_clock();
  f->pool = (g_word*) f;
  f->len = len0;
  f->hp = f->end;
  f->sp = (g_word*) f + len0;
  f->ip = bif_yield;
  f = sy("\\", sy(",", sy("`", sy("?", sy(":", g_tbl(g_tbl(f)))))));
  if (g_ok(f)) f->lambda = sym(pop1(f)),
               f->begin = sym(pop1(f)),
               f->quote = sym(pop1(f)),
               f->cond = sym(pop1(f)),
               f->let = sym(pop1(f)),
               f->macro = tbl(pop1(f)),
               f->dict = tbl(f->sp[0]);
  f = de("macros", (g_word) f->macro,
       de("globals", (g_word) f->dict,
        g_hash_put(sy("version", st(g_version, f)))));
  bifs(bif_dict_entry);
  insts(i_dict_entry);
  return g_pop(f, 1); }
