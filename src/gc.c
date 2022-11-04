#include "la.h"
#include <time.h>

static clock_t copy(la, size_t);
static void copy_(la, size_t, ob*);

// FIXME the garbage collector works pretty well but it could be better:
//
// - it uses stack recursion so a process that constructs infinite
//   data will stack overflow, rather than fail gracefully with oom.
//   we could fix this with cheney's algorithm.
//
// - we allocate a new pool every cycle rather than keeping two pools
//   at all times. theoretically this means we have less memory allocated
//   most of the time, and if malloc is efficient then the overhead from
//   calling it every cycle should be negligible, but it would still be
//   better only to call out when we need to grow or shrink the pool.

////
/// garbage collector
//
// please : u1 la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool.
bool please(la v, size_t req) {
  // copy into a new pool of the same size.
  size_t len = v->len, vit = copy(v, len);
  // if this fails then the request fails.
  if (!vit) return 0;
  size_t tar = len, all = len - (Avail - req);
  // adjust size up if we're too small or slow.
  while (all > tar || vit < 32) tar <<= 1, vit <<= 1;
  // adjust size down if we're big and fast enough.
  while (all < tar>>1 && vit >= 128) tar >>= 1, vit >>= 1;
  // if we don't need to resize, return success.
  return tar == len
    // otherwise adjust the size and copy again.
    || copy(v, tar)
    // if that fails, succeed if we have enough free space.
    || all <= len; }


// copy : clock_t la size_t
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
static clock_t copy(la v, size_t len1) {
  clock_t t1 = clock(), t0 = v->t0, t2;

  ob *pool0 = v->pool,
     *pool1 = calloc(len1, sizeof(ob));
  if (!pool1) return 0;

  copy_(v, len1, pool1);
  free(pool0);

  t2 = v->t0 = clock();
  t1 = t2 - t1;
  return t1 ? (t2 - t0) / t1 : 1; }

static NoInline ob cp_mo(la v, mo src, ob *pool0, ob *top0) {
  tag fin = motag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;

  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  for (GF(d) = (vm*) dst; d-- > dst;
    G(d) = (vm*) cp(v, (ob) G(d), pool0, top0));
  return (ob) (src - ini + dst); }

#define stale(o) ((ob*)(o) >= pool0 && (ob*) o < top0)
Gc(cp) {
  if (nump(x) || !stale(x)) return x;
  ob y = (ob) G(x);
  if (!nump(y) && livep(v, y)) return y;
  if ((vm*) y == disp) return
    ((mtbl) GF(x))->evac(v, x, pool0, top0);
  return cp_mo(v, (mo) x, pool0, top0); }


static void copy_(la v, size_t len1, ob *pool1) {
  ob len0 = v->len,
     *sp0 = v->sp,
     *pool0 = v->pool,
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->len = len1;
  v->hp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (fr) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  v->topl = (tbl) cp(v, (ob) v->topl, pool0, top0);
  for (size_t i = LexN; i--;)
    v->lex[i] = cp(v, v->lex[i], pool0, top0);
  for (keep r = v->safe; r; r = r->et)
    *r->it = (void**) cp(v, (ob) *r->it, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  fr fp = v->fp;
  for (;;) {
    while (sp < (ob*) fp) *sp++ = cp(v, *sp0++, pool0, top0);
    if (sp0 == top0) break;
    fr fp0 = (fr) sp0;
    fp->argc = fp0->argc;
    fp->subd = (fr) ((ob*) fp0->subd + shift);
    fp->clos = (ob*) cp(v, (ob) fp0->clos, pool0, top0);
    fp->retp = (mo) cp(v, (ob) fp0->retp, pool0, top0);
    sp = fp->argv;
    sp0 = fp0->argv;
    fp = fp->subd; } }

// Run a GC cycle from inside the VM
// XXX calling convention: size of request (bare size_t) in v->xp
NoInline Vm(gc) {
  size_t req = v->xp;
  CallOut(req = please(v, req));
  return req ? ApN(0, xp) : ApC(xoom, xp); }