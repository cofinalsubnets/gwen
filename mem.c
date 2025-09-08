#include "i.h"

static NoInline word pushsr(core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return g_please(f, m) ? m : n;
  word x = va_arg(xs, word), y;
  return avec(f, x, y = pushsr(f, m, n - 1, xs)), y ? *--f->sp = x : y; }

word pushs(core *f, uintptr_t m, ...) {
  va_list xs;
  va_start(xs, m);
  word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, word));
  return va_end(xs), r; }

static core *pushcr(core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (n == m) return please(f, m);
  word x = va_arg(xs, word);
  avec(f, x, f = pushcr(f, m, n + 1, xs));
  if (g_ok(f)) push1(f, x);
  return f; }

static core *vpushc(core *f, uintptr_t m, va_list xs) {
  if (avail(f) < m) f = pushcr(f, m, 0, xs);
  else {
    f->sp -= m;
    for (word n = 0; n < m; n++)
      f->sp[n] = va_arg(xs, word); }
  return f;
}

core *pushc(core *f, uintptr_t m, ...) {
  va_list xs;
  va_start(xs, m);
  f = vpushc(f, m, xs);
  va_end(xs);
  return f; }

g_core *g_list_n(g_core *f, uintptr_t m) {
  f = pushc(f, 1, nil);
  while (g_ok(f) && m--)
    f = g_cons_stack(f, 1, 0);
  return f; }

g_core *g_list(g_core *f, uintptr_t m, ...) {
  va_list xs;
  va_start(xs, m);
  f = vpushc(f, m, xs);
  va_end(xs);
  f = g_ok(f) ? g_list_n(f, m) : f;
  return f; }

void *bump(core *f, size_t n) {
  void *x = f->hp;
  return f->hp += n,
         x; }

void *cells(core *f, size_t n) {
  return n <= avail(f) || g_please(f, n) ? bump(f, n) : 0; }

NoInline bool g_please(core *f, size_t n) {
  return g_ok(please(f, n)); }

static NoInline void copy_from(core*, word*, uintptr_t);
// garbage collector
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   v = (t2 - t0) / (t2 - t1)
// between
#define v_lo 8
#define too_little (len1 < req || v < v_lo)
// and
#define v_hi (v_lo << 6)
#define too_big (len1 >> 1 > req && v > v_hi)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
NoInline core *please(core *f, uintptr_t req0) {
  word *src = f->pool, *dest = f->loop;
  f->pool = dest, f->loop = src;    // swap
  size_t t0 = f->t0, t1 = g_clock() , // get last gc end time
         len0 = f->len;             // get original length
                                    //
  copy_from(f, src, len0);          // copy to new pool
  size_t t2 = f->t0 = g_clock(),      // get and set last gc end time
         total = len0,
         avail = avail(f),
         used = total - avail,
         req = req0 + used,
         v = t2 == t1 ?             // t1 and t2 can be the same
           v_hi :                   // in that case choose high
           (t2 - t0) / (t2 - t1),   // otherwise take ratio of total run time to gc time
         len1 = len0;               // initial destination size same as the target size
  // if v is out of bounds then calculate len1
  // by inverse proportionally adjusting len and v until v is in bounds
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return f; // no change reqired, hopefully the most common case
  // at this point we got a new target length and are gonna try and resize
  word *dest2 = f->malloc(len1 * 2 * sizeof(word)); // allocate pool with the new target size
  if (!dest2) return req <= total ? f : encode(f, Oom); // if this fails still return true if the original pool is not too small
  // we got the new pool so copy again and return true
  f->len = len1;            // set core variables referring to new pool
  f->pool = dest2;          //
  f->loop = dest2 + len1;   //
  copy_from(f, dest, len0); // do second copy
  f->free(min(src, dest));  // free original pool
  f->t0 = g_clock();       // set last gc timestamp
  return f; }            // size successfully adjusted


// this function expects pool loop and len to have been set already on the state
static NoInline void copy_from(core *f, word *p0, uintptr_t len0) {
  word len1 = f->len, // target pool length
       *p1 = f->pool, // target pool
       *t0 = p0 + len0, // source pool top
       *t1 = p1 + len1, // target pool top
       *sp0 = f->sp, // source pool stack
       sn = t0 - sp0, // stack height
       *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  f->sp = sp1;
  f->hp = f->cp = p1;
  f->symbols = 0;
  // copy variables
  f->ip     = (cell*)   CP(f->ip);
  f->dict   = (table*)  CP(f->dict);
  f->macro  = (table*)  CP(f->macro);
  f->quote  = (symbol*) CP(f->quote);
  f->begin  = (symbol*) CP(f->begin);
  f->let    = (symbol*) CP(f->let);
  f->cond   = (symbol*) CP(f->cond);
  f->lambda = (symbol*) CP(f->lambda);
  // copy stack
  while (sn--) *sp1++ = CP(*sp0++);
  // copy protected values
  for (struct root *r = f->safe; r; *r->ptr = CP(*r->ptr), r = r->next);
  // copy all reachable values using cheney's method
  for (cell *k; (k = R(f->cp)) < R(f->hp);)
    if (datp(k)) typof(k)->wk(f, W(k), p0, t0); // is data
    else { while (k->x) k->x = CP(k->x), k++;     // is thread
           f->cp = (word*) k + 2; }
  // run destructors ...
  // this has never been tested or used
  struct dtor *nd = NULL;
  for (struct dtor *n, *d = f->dtors; d; d = d->next)
    if (!owns(f, R(d->x)->x)) d->d(f, d->x);
    else n = bump(f, Width(struct dtor)),
         n->d = d->d,
         n->x = R(d->x)->x,
         n->next = nd,
         nd = n;
  f->dtors = nd; }

NoInline word cp(core *v, word x, word *p0, word *t0) {
  // if it's a number or outside managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  cell *src = (cell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && owns(v, x)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return typof(src)->cp(v, (word) src, p0, t0);
  // it's a thread, find the end to find the head
  struct tag *t = ttag(src);
  cell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  // copy source contents to dest and write dest addresses to source
  for (cell *s = ini; (d->x = s->x); s++->x = W(d++));
  ((struct tag*) d)->head = dst;
  return W(dst + (src - ini)); }

NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  int s = g_please(f, n) ? Ok : Oom;
  Unpack(f);
  return s != Ok ? s : Ap(f->ip->ap, f); }

