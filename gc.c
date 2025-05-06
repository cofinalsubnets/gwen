#include "i.h"
NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  bool ok = p_please(f, n);
  Unpack(f);
  return ok ? Continue() : Oom; }

void *bump(Core *f, size_t n) { void *x = f->hp; return f->hp += n, x; }
void *cells(Core *f, size_t n) { return
  n <= avail(f) || p_please(f, n) ? bump(f, n) : 0; }

static NoInline void copy_from(Core*, Word*, uintptr_t);
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
NoInline bool p_please(Core *f, uintptr_t req) {
  Word *b0p0 = f->pool, *b0p1 = f->loop;
  f->pool = b0p1, f->loop = b0p0;
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;
  // do initial copy to alternate pool
  copy_from(f, b0p0, len0);
  size_t t2 = f->t0 = clock(), // set last gc timestamp
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1), // speed factor
         len1 = len0; // target size
  // calculate the minimum memory required to be able to return true:
  //    req + used = req + (total - free)
  req += len0 - avail(f);
  // if v is out of bounds calculate new len to compensate assuming v = k*len
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true; // no change reqired, common case
  // we are going to try and resize
  // allocate a pool with the new target size
  Word *b1p0 = malloc(len1 * 2 * sizeof(Word));
  // if it failed we can still still return true if request was satisfied by original pool
  if (!b1p0) return req <= len0;
  // we got a new pool so copy again
  // reset core variables on new pool
  f->loop = (f->pool = b1p0) + (f->len = len1);
  copy_from(f, b0p1, len0); // do second copy
  free(b0p0 < b0p1 ? b0p0 : b0p1); // free old pool
  f->t0 = clock(); // set last gc timestamp
  return true; } // size successfully adjusted


// this function expects pool loop and len to have been set already on the state
static NoInline void copy_from(Core *f, Word *p0, uintptr_t len0) {
  Word len1 = f->len, // target pool length
       *p1 = f->pool, // target pool
       *t0 = p0 + len0, // source pool top
       *t1 = p1 + len1, // target pool top
       *sp0 = f->sp, // source pool stack
       sn = t0 - sp0, // stack height
       *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  f->sp = sp1, f->hp = p1, f->symbols = 0;
  // copy stack and variables
  while (sn--) *sp1++ = cp(f, *sp0++, p0, t0);
  f->ip = (Cell*) cp(f, (Word) f->ip, p0, t0);
  for (int i = 0; i < NPVars; i++)
    f->var_array[i] = cp(f, f->var_array[i], p0, t0);
  // copy protected values
  for (Mm *r = f->safe; r; r = r->next) *r->addr = cp(f, *r->addr, p0, t0);
  // copy all reachable values using cheney's method
  f->cp = p1;
  for (Cell *k; (k = R(f->cp)) < R(f->hp);)
    if (k->ap == data) k[1].typ->evac(f, Z(k), p0, t0); // is data
    else { // is thread
      while (k->x) k->x = cp(f, k->x, p0, t0), k++;
      f->cp = (Word*) k + 2; } }

NoInline Word cp(Core *v, Word x, Word *p0, Word *t0) {
  // if it's a number or out of managed memory then return it
  if (nump(x) || !bounded(p0, x, t0)) return x;
  Cell *src = (Cell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && bounded(v->pool, x, v->pool + v->len)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return dtyp(src)->copy(v, (Word) src, p0, t0);
  // it's a thread, find the end
  struct tag *t = ttag(src);
  Cell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  for (Cell *s = ini; (d->x = s->x); s++->x = (Word) d++);
  d[1].ap = (Vm*) dst;
  return (Word) (src - ini + dst); }
