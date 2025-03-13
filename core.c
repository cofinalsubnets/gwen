#include "i.h"

#define S1(i) ((Cell[]){{i}})
#define S2(i) ((Cell[]){{curry},{.x=putnum(2)},{i}})
#define S3(i) ((Cell[]){{curry},{.x=putnum(3)},{i}})

static struct {
  const char *n;
  Cell *v;
} ini_dict[] = {
  {"+", S2(add)}, {"-", S2(sub)}, {"*", S2(mul)}, {"/", S2(quot)}, {"%", S2(rem)}, 
  {"<", S2(lt)}, {"<=", S2(le)}, {"=", S2(eq)}, {">=", S2(ge)}, {">", S2(gt)},
  {"rand", S1(rng)},
  {"X", S2(cons)}, {"A", S1(car)}, {"B", S1(cdr)},
  {"sget", S2(sget)}, {"ssub", S3(ssub)}, {"slen", S1(slen)}, {"scat", S2(scat)},
  {"error", S1(error)},
  {".", S1(display)}, {"putc", S1(prc)},
  {"~", S1(bnot)},
  // thread functions
  {"thd", S1(thda)}, {"peek", S1(peek)}, {"poke", S2(poke)},
  {"trim", S1(trim)}, {"seek", S2(seek)},
  // table functions
  {"tnew", S1(tnew)}, {"tkeys", S1(tkeys)}, {"tlen", S1(tlen)},
  {"tset", S3(tset)}, {"tget", S3(tget)}, {"tdel", S3(tdel)},
  // type predicates
  {"twop", S1(pairp)}, {"strp", S1(stringp)}, {"symp", S1(symbolp)}, {"nump", S1(fixnump)},

  // symbols
  {"sym", S1(gensym)}, {"nom", S1(symnom)},

  {"ev", S1(ev0)},
  {"::", S2(defmacro)},
};

static NoInline bool p_define(Core *f, const char *k, Word v) {
  if (!pushs(f, 1, v)) return false;
  Symbol* y = literal_symbol(f, k);
  v = pop1(f);
  return y && table_set(f, f->dict, (Word) y, v); }

void p_fin(Core *f) { free(f->pool < f->loop ? f->pool : f->loop); }
void p_close(Core *f) { p_fin(f), free(f); }
PStatus p_ini(Core *f) {
  memset(f, 0, sizeof(Core));
  const uintptr_t len0 = 1;
  Word *pool = malloc(2 * len0 * sizeof(Word));
  if (!pool) return Oom;
  f->t0 = clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
#define Definition(a, b) p_define(f, a, (Word) b) &&
  if (!(f->dict = new_table(f)) ||
      !(f->macro = new_table(f)) ||
      !p_define(f, "global-namespace", (Word) f->dict))
    return p_fin(f), Oom;
  for (long i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!p_define(f, ini_dict[i].n, (Word) ini_dict[i].v))
      return p_fin(f), Oom;
  return Ok; }

Core *p_open(void) {
  Core *f = malloc(sizeof(Core));
  return !f || p_ini(f) == Ok ? f : (free(f), NULL); }


static NoInline Word pushsr(Core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return p_please(f, m) ? m : n;
  Word x = va_arg(xs, Word), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

Word pushs(Core *f, uintptr_t m, ...) {
  va_list xs; va_start(xs, m);
  Word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, Word));
  va_end(xs);
  return r; }

size_t p_drop(Core *f, size_t n) {
  size_t h = p_height(f);
  n = min(n, h);
  f->sp += n;
  return n; }

size_t p_height(Core *f) { return f->pool + f->len - f->sp; }
