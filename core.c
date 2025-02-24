#include "i.h"

#define S1(i) ((PCell[]){{i}})
#define S2(i) ((PCell[]){{curry},{.x=putnum(2)},{i}})
#define S3(i) ((PCell[]){{curry},{.x=putnum(3)},{i}})

static struct {
  const char *n;
  PCell *v;
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
  {"gensym", S1(gensym)},

  {"ev", S1(ev0)},
  {"::", S2(defmacro)},
};

static NoInline bool p_define(PCore *f, const char *k, PWord v) {
  if (!pushs(f, 1, v)) return false;
  PSymbol* y = literal_symbol(f, k);
  v = pop1(f);
  return y && table_set(f, f->dict, (PWord) y, v); }
PCore *p_open(void) {
  PCore *f = malloc(sizeof(PCore));
  if (!f) return f;
  memset(f, 0, sizeof(PCore));
  const uintptr_t len0 = 1;
  PWord *pool = malloc(2 * len0 * sizeof(PWord));
  if (!pool) return p_close(f), NULL;
  f->t0 = clock();
  f->sp = f->loop = (f->hp = f->pool = pool) + (f->len = len0);
#define Definition(a, b) p_define(f, a, (PWord) b) &&
  if (!(f->dict = new_table(f)) ||
      !(f->macro = new_table(f)) ||
      !p_define(f, "global-namespace", (PWord) f->dict))
    return p_close(f), NULL;
  for (long i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++)
    if (!p_define(f, ini_dict[i].n, (PWord) ini_dict[i].v))
      return p_close(f), NULL;
  return f; }

void p_close(PCore *f) {
  if (f) free(f->pool < f->loop ? f->pool : f->loop), free(f); }

static NoInline PWord pushsr(PCore *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return p_please(f, m) ? m : n;
  PWord x = va_arg(xs, PWord), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

PWord pushs(PCore *f, uintptr_t m, ...) {
  va_list xs; va_start(xs, m);
  PWord n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, PWord));
  va_end(xs);
  return r; }

size_t p_drop(PCore *f, size_t n) {
  size_t h = stack_height(f);
  n = min(n, h);
  f->sp += n;
  return n; }

