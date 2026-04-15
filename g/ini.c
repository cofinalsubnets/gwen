#include "i.h"
#include <stdarg.h>

enum g_status g_fin(struct g *f) {
  enum g_status s = g_code_of(f);
  f = g_core_of(f);
  if (f) f->free(f, f->pool);
  return s; }

#define S1(i) {{i}, {g_vm_ret0}}
#define S2(i) {{g_vm_curry},{.x=gputnum(2)},{i}, {g_vm_ret0}}
#define S3(i) {{g_vm_curry},{.x=gputnum(3)},{i}, {g_vm_ret0}}
#define bifs(_) \
 _(bif_clock, "clock", S1(g_vm_clock)) _(bif_addr, "vminfo", S1(g_vm_info))\
 _(bif_add, "+", S2(g_vm_add)) _(bif_sub, "-", S2(g_vm_sub)) _(bif_mul, "*", S2(g_vm_mul)) _(bif_quot, "/", S2(g_vm_quot)) _(bif_rem, "%", S2(g_vm_rem)) \
 _(bif_lt, "<", S2(g_vm_lt))  _(bif_le, "<=", S2(g_vm_le)) _(bif_eq, "=", S2(g_vm_eq)) _(bif_ge, ">=", S2(g_vm_ge))  _(bif_gt, ">", S2(g_vm_gt)) \
 _(bif_bnot, "~", S1(g_vm_bnot)) _(bif_bsl, "<<", S2(g_vm_bsl)) _(bif_bsr, ">>", S2(g_vm_bsr))\
 _(bif_band, "&", S2(g_vm_band)) _(bif_bor, "|", S2(g_vm_bor)) _(bif_bxor, "^", S2(g_vm_bxor))\
 _(bif_cons, "X", S2(g_vm_cons)) _(bif_g_vm_car, "A", S1(g_vm_car)) _(bif_g_vm_cdr, "B", S1(g_vm_cdr)) \
 _(bif_sget, "sget", S2(g_vm_sget)) _(bif_ssub, "ssub", S3(g_vm_ssub)) _(bif_scat, "scat", S2(g_vm_scat)) \
 _(bif_g_vm_dot, ".", S1(g_vm_dot)) _(bif_read, "read", S1(g_vm_read)) _(bif_getc, "getc", S1(g_vm_getc))\
 _(bif_putc, "putc", S1(g_vm_putc)) _(bif_prn, "putn", S2(g_vm_putn)) _(bif_puts, "puts", S1(g_vm_puts))\
 _(bif_sym, "sym", S1(g_vm_gensym)) _(bif_nom, "nom", S1(g_vm_symnom)) _(bif_thd, "thd", S1(g_vm_thda)) _(bif_g_vm_peek, "peek", S1(g_vm_peek)) _(bif_g_vm_poke, "poke", S2(g_vm_poke)) _(bif_trim, "trim", S1(g_vm_trim)) _(bif_g_vm_seek, "seek", S2(g_vm_seek)) \
 _(bif_len, "len", S1(g_vm_len))\
 _(bif_tset2, "set", S3(g_vm_tset2)) _(bif_tabnew, "tnew", S1(g_vm_tnew)) _(bif_tabkeys, "tkeys", S1(g_vm_tkeys)) _(bif_tabget, "tget", S3(g_vm_tget)) _(bif_tabdel, "tdel", S3(g_vm_tdel))\
 _(bif_twop, "twop", S1(g_vm_twop)) _(bif_strp, "strp", S1(g_vm_strp)) _(bif_symp, "symp", S1(g_vm_symp)) _(bif_tabp, "tabp", S1(g_vm_tabp)) _(bif_nump, "nump", S1(g_vm_nump)) _(bif_nilp, "nilp", S1(g_vm_nilp))\
 _(bif_ev, "ev", S1(g_vm_eval))
#define built_in_function(n, _, d) static union u const n[] = d;
bifs(built_in_function);
#define insts(_) _(g_vm_freev) _(g_vm_ret) _(g_vm_ap) _(g_vm_tap) _(g_vm_apn) _(g_vm_tapn) _(g_vm_jump) _(g_vm_cond) _(g_vm_arg) _(g_vm_quote) _(g_vm_drop1) _(g_vm_curry) _(g_vm_defglob) _(g_vm_lazyb) _(g_vm_ret0)
#define biff(b, n, _) {n, (intptr_t) b},
#define i_entry(i) {#i, (intptr_t) i},

static g_vm(g_vm_yield) { return Pack(f), f; }
static union u yield[] = { {g_vm_yield} };
static struct g_def const def1[] = { bifs(biff) insts(i_entry) {0}};
g_noinline struct g *g_ini_m(g_malloc_t *ma, g_free_t *fr) {
 uintptr_t const len0 = 1 << 10;
 struct g *f = ma(NULL, 2 * len0 * sizeof(g_word));
 if (f == NULL) return encode(f, g_status_oom);
 memset(f, 0, sizeof(struct g));
 f->len = len0;
 f->pool = (void*) f;
 f->malloc = ma;
 f->free = fr;
 f->hp = f->end;
 f->sp = (word*) f + len0;
 f->ip = yield;
 f->t0 = g_clock(); // this goes right before first allocation so gc always sees initialized t0
 f = mktbl(mktbl(f)); // dict and macro tables
 f = g_intern(g_strof(g_intern(g_strof(f, "\\")), "`"));
 if (!g_ok(f)) return f;
 f->quote = nom(pop1(f));
 f->lambda = nom(pop1(f));
 word m = pop1(f), d = pop1(f);
 f->macro = tbl(m), f->dict = tbl(d);
 struct g_def def0[] = { {"globals", d, }, {"macros", m, }, {0}, };
 return g_defs(g_defs(f, def0), def1); }

static void *g_libc_malloc(struct g*f, size_t n) { return malloc(n); }
static void g_libc_free(struct g*f, void *x) { free(x); }
struct g *g_ini(void) { return g_ini_m(g_libc_malloc, g_libc_free); }

struct g *g_defs(struct g*f, struct g_def const*defs) {
 if (!g_ok(f)) return f;
 f = g_push(f, 1, f->dict);
 for (int n = 0; defs[n].n; n++)
  f = g_tput(g_intern(g_strof(g_push(f, 1, defs[n].x), defs[n].n)));
 if (g_ok(f)) f->sp++;
 return f; }
