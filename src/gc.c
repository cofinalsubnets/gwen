#include "i.h"

ob *new_pool(size_t n) {
  return malloc(n * sizeof(ob)); }

Vm(gc) { size_t req = v->xp; return
  CallOut(req = please(v, req)),
  req ? ApY(ip, xp) : Yield(OomError, xp); }

static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  for (GF(d) = (vm*) dst; d-- > dst;
    G(d) = (vm*) cp(v, (ob) G(d), pool0, top0));
  return (ob) (src - ini + dst); }

NoInline ob cp(la v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    ((typ) GF(src))->evac(v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }
      
////
/// garbage collector
//
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool.
//
// copy : la_clock_t la size_t
// relocate all reachable data into a newly allocated
// memory pool of the given length. return 0 if a new
// pool can't be allocated or else a positive integer
// value u that's higher the less time we spend in GC:
//
//   u = t1 == t2 ? 1 : (t2 - t0) / (t2 - t1)
//
// where
//
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)

#define MinVim 32
#define MaxVim (MinVim<<2)

static void copy_from(li, ob*, ob*);
NoInline bool please(li v, size_t req) {
  size_t t1 = clock(), t0 = v->t0, t2,
         have = v->len;
  ob *pool0 = v->pool, *pool1 = new_pool(have);
  if (!pool1) return false;

  v->pool = pool1;
  copy_from(v, pool0, pool0 + have);
  free(pool0);
  t2 = v->t0 = clock();

  size_t vim = t2 == t1 ? MaxVim : (t2 - t0) / (t2 - t1),
         want = have,
         need = have - (Avail - req);

  // grow if we're too slow or small
  if (want < need || vim < MinVim)
    do want <<= 1, vim <<= 1;
    while (want < need || vim < MinVim);

  // shrink if we're too big and fast
  else if (want >> 1 > need && vim > MaxVim)
    do want >>= 1, vim >>= 1;
    while (want >> 1 > need && vim > MaxVim);

  if (want == have) return true;

  pool1 = new_pool(want);
  if (!pool1) return need <= have;

  pool0 = v->pool;
  v->pool = pool1;
  v->len = want;
  copy_from(v, pool0, pool0 + have);
  free(pool0);
  v->t0 = clock();
  return true; }

static NoInline void copy_from(li v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->len = len1;
  v->hp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  v->lex = (struct glob*) cp(v, (ob) v->lex, pool0, top0);
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  frame fp = v->fp;
  for (;;) {
    while (sp < (ob*) fp) *sp++ = cp(v, *sp0++, pool0, top0);
    if (sp0 == top0) break;
    sf fp0 = (sf) sp0;
    fp->argc = fp0->argc;
    fp->subd = (sf) ((ob*) fp0->subd + shift);
    fp->clos = (ob*) cp(v, (ob) fp0->clos, pool0, top0);
    fp->retp = (mo) cp(v, (ob) fp0->retp, pool0, top0);
    sp = fp->argv;
    sp0 = fp0->argv;
    fp = fp->subd; } }
