#include "i.h"
void *memset(void*, int, size_t);

#ifndef g_version
#define g_version ""
#endif
#define LEN(a) (sizeof(a)/sizeof(*a))
#define S1(i) {{i}, {ret0}}
#define S2(i) {{curry},{.x=putnum(2)},{i}, {ret0}}
#define S3(i) {{curry},{.x=putnum(3)},{i}, {ret0}}
#define bifs(_) \
  _(bif_clock, "clock", S1(sysclock))\
  _(bif_add, "+", S2(add)) _(bif_sub, "-", S2(sub)) _(bif_mul, "*", S2(mul)) _(bif_quot, "/", S2(quot)) _(bif_rem, "%", S2(rem)) \
  _(bif_lt, "<", S2(lt))  _(bif_le, "<=", S2(le)) _(bif_eq, "=", S2(eq)) _(bif_ge, ">=", S2(ge))  _(bif_gt, ">", S2(gt)) \
  _(bif_bnot, "~", S1(bnot)) _(bif_bsl, "<<", S2(bsl)) _(bif_bsr, ">>", S2(bsr))\
  _(bif_band, "&", S2(band)) _(bif_bor, "|", S2(bor)) _(bif_bxor, "^", S2(bxor))\
  _(bif_cons, "X", S2(cons)) _(bif_car, "A", S1(car)) _(bif_cdr, "B", S1(cdr)) \
  _(bif_sget, "sget", S2(sget)) _(bif_ssub, "ssub", S3(ssub)) _(bif_slen, "slen", S1(slen)) _(bif_scat, "scat", S2(scat)) \
  _(bif_dot, ".", S1(dot)) _(bif_putc, "putc", S1(prc)) \
  _(bif_sym, "sym", S1(gensym)) _(bif_nom, "nom", S1(symnom))\
  _(bif_thd, "thd", S1(thda)) _(bif_peek, "peek", S1(peek)) _(bif_poke, "poke", S2(poke)) _(bif_trim, "trim", S1(trim)) _(bif_seek, "seek", S2(seek)) \
  _(bif_tnew, "tnew", S1(tnew)) _(bif_tkeys, "tkeys", S1(tkeys)) _(bif_tlen, "tlen", S1(tlen)) _(bif_tset, "tset", S3(tset)) _(bif_tget, "tget", S3(tget)) _(bif_tdel, "tdel", S3(tdel))\
  _(bif_twop, "twop", S1(pairp)) _(bif_strp, "strp", S1(stringp)) _(bif_symp, "symp", S1(symbolp)) _(bif_nump, "nump", S1(fixnump)) _(bif_nilp, "nilp", S1(nullp))\
  _(bif_ev, "ev", S1(ev0))
#define built_in_function(n, _, d) static const union g_cell n[] = d;
#define biff(b, n, _) {n, b},
bifs(built_in_function);
static Vm(g_stop) { return Pack(f), f; }
static union g_cell bif_stop[] = { {g_stop} };
static const struct { const char *n; const union g_cell *x; } bifff[] = { bifs(biff) };

#define insts(_)\
  _(free_variable) _(ret) _(ap) _(tap) _(apn) _(tapn) _(jump) _(cond) _(ref) _(imm) _(drop1) _(curry) _(defglob) _(late_bind) _(ret0)
#define i_entry(i) {"i_"#i,i},
static const struct {
  const char *n;
  g_vm *i;
} i_dict[] = {
  insts(i_entry)
};

static g_core *g_symof(g_core *f, const char *nom) {
  return g_intern(g_strof(f, nom)); }

static g_core *g_ini_def(g_core *f, const char *k, g_word v) {
  return g_hash_put(g_symof(g_push(f, 1, v), k)); }

g_core *g_define(g_core *f, const char *s) {
  if (g_ok(f)) f = g_intern(g_strof(g_push(f, 1, f->dict), s));
  if (!g_ok(f)) return f;
  g_word w = f->sp[1];
  f->sp[1] = f->sp[2];
  f->sp[2] = w;
  return g_pop(g_hash_put(f), 1); }

g_core *g_ini_m(g_malloc_t *malloc, g_free_t *free) {
  return g_ini_m_n(malloc, free, 1 << 10); }

struct g_core *g_ini_m_n(g_malloc_t *g_malloc, g_free_t *g_free, const size_t len0) {
  struct g_core *f = g_malloc(NULL, 2 * len0 * sizeof(g_word));
  if (!f) return encode(f, g_status_oom);
  memset(f, 0, sizeof(struct g_core));
  f->t0 = g_clock();
  f->pool = (g_word*) f;
  f->len = len0;
  f->malloc = g_malloc;
  f->free = g_free;
  f->hp = f->end;
  f->sp = (g_word*) f + len0;
  f->ip = bif_stop;
  f = g_tbl(f); // dict
  f = g_tbl(f); // macro
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
    f->dict = tbl(f->sp[0]); // keep this one on stack for following definitions
  f = g_ini_def(f, "macros", (g_word) f->macro);
  f = g_ini_def(f, "globals", (g_word) f->dict);
  f = g_hash_put(g_symof(g_strof(f, g_version), "version"));
  for (size_t i = 0; i < LEN(bifff); i++)
    f = g_ini_def(f, bifff[i].n, (g_word) bifff[i].x);
  for (size_t i = 0; i < LEN(i_dict); i++)
    f = g_ini_def(f, i_dict[i].n, (g_word) i_dict[i].i);
  return g_pop(f, 1); }

enum g_status g_fin(g_core *f) {
  enum g_status s = g_code_of(f);
  if ((f = g_core_of(f))) f->free(f, f->pool);
  return s; }


Vm(sysclock) {
  Sp[0] = putnum(g_clock());
  Ip += 1;
  return Continue(); }
