#include "i.h"

static g_core *g_strof_c(g_core *f, const char *cs) {
  size_t nbytes = strlen(cs),
         nwords = b2w(nbytes),
         req = Width(string) + nwords + 1;
  f = g_have(f, req);
  if (g_ok(f)) {
    g_string *o = (g_string*) f->hp;
    f->hp += Width(string) + nwords;
    ini_str(o, nbytes);
    memcpy(o->text, cs, nbytes);
    push1(f, o); }
  return f; }

static Inline g_core *g_symof_c(g_core *f, const char *nom) {
  f = g_strof_c(f, nom);
  return g_intern_c(f); }

static g_core *g_ini_def_c(g_core *f, const char *k, word v) {
  f = pushc(f, 1, v);
  f = g_symof_c(f, k);
  f = pushc(f, 1, f->dict);
  f = g_hash_put_c(f);
  if (g_ok(f)) f->sp++;
  return f; }


static g_core *g_tbl_new(g_core *f) {
  f = g_cells(f, Width(table) + 1);
  if (g_ok(f)) {
    table *t = tbl(f->sp[0]);
    struct entry **tab = (struct entry**) (t + 1);
    tab[0] = 0;
    ini_table(t, 0, 1, tab); }
  return f; }

static Vm(g_vm_fin) {
  int s = code_of(f);
  g_fin(f);
  return s; }

g_core *g_run(g_core *f, const char *p, const char **av) {
  int n = 0;
  for (f = p_readcs(f, p);   av[n]; f = g_strof_c(f, av[n++]));
  for (f = pushc(f, 1, nil); n--;   f = g_cons_stack(f, 1, 0));
  f = pushc(f, 1, nil);
  f = g_cons_stack(f, 1, 0);
  f = pushc(f, 1, f->quote);
  f = g_cons_stack(f, 0, 1);
  f = pushc(f, 1, nil);
  f = g_cons_stack(f, 1, 0);
  f = g_cons_stack(f, 1, 0);
  f = g_eval_c(f, g_vm_fin);
  return f; }

void g_fin(g_core *f) {
  if ((f = core_of(f))) {
    for (struct dtor *d = f->dtors; d; d = d->next) d->d(f, d->x);
    g_free(min(f->pool, f->loop));
    g_free(f); } }


static cell bif_fin[] = { {g_vm_fin} };

g_core *g_ini() {
  g_core *f = g_malloc(sizeof(g_core));
  if (!f) return encode(NULL, g_status_oom);

  memset(f, 0, sizeof(core));
  size_t len = 1;
  word *pool = g_malloc(2 * len * sizeof(word));
  if (!pool) return encode(f, g_status_oom);

  f->t0 = g_clock();
  f->len = len;
  f->hp = f->pool = pool;
  f->sp = f->loop = pool + len;

  f = g_tbl_new(f);
  f = g_tbl_new(f);
  f = g_symof_c(f, "ev");
  f = g_symof_c(f, ":");
  f = g_symof_c(f, "?");
  f = g_symof_c(f, "`");
  f = g_symof_c(f, ",");
  f = g_symof_c(f, "\\");

  if (g_ok(f))
    f->lambda = sym(pop1(f)),
    f->begin = sym(pop1(f)),
    f->quote = sym(pop1(f)),
    f->cond = sym(pop1(f)),
    f->let = sym(pop1(f)),
    f->eval = sym(pop1(f)),
    f->macro = tbl(pop1(f)),
    f->dict = tbl(pop1(f));

  f = g_ini_def_c(f, "globals", word(f->dict));
  f = g_ini_def_c(f, "macros", word(f->macro));
  f = g_strof_c(f, g_version);
  f = g_ini_def_c(f, "version", pop1(f));

  static cell bif_yield[] = { {g_yield}, {.m = bif_yield} };
  f->ip = bif_yield;

#define insts(_) _(free_variable) _(ret) _(ap) _(tap) _(apn) _(tapn) _(jump) _(cond) _(ref) _(imm) _(drop1) _(curry) _(defglob) _(late_bind) _(ret0)
#define i_entry(i)        f = g_ini_def_c(f, "i_"#i, W(i));
  insts(i_entry);
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
#define built_in_function(n, _, d) static const cell n[] = d;
#define bif_dict_entry(bn, n, _) f = g_ini_def_c(f, n, W(bn));
  bifs(built_in_function);
  bifs(bif_dict_entry);

  return f; }
