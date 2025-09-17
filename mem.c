#include "i.h"
#define avail(f) (f->sp-f->hp)
#define CP(x) cp(f, W(x), p0, t0)
static g_core *please(g_core*, uintptr_t);

void *g_malloc(g_core *f, size_t n) { return malloc(n); }
void g_free(g_core *f, void *x) { return free(x); }

static core *g_pushr(core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (n == m) return please(f, m);
  word x = va_arg(xs, word);
  MM(f, &x);
  f = g_pushr(f, m, n + 1, xs);
  UM(f);
  if (g_ok(f)) *--f->sp = x;
  return f; }

g_core *g_push(core *f, uintptr_t m, ...) {
  if (g_ok(f)) {
    va_list xs;
    va_start(xs, m);
    g_word n = 0;
    if (avail(f) < m) f = g_pushr(f, m, n, xs);
    else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, word));
    va_end(xs); }
  return f; }

Inline g_core *g_have(g_core *f, uintptr_t n) {
  return !g_ok(f) ? f : avail(f) < n ? please(f, n) : f; }

g_core *g_cells(g_core *f, size_t n) {
  f = g_have(f, n);
  if (g_ok(f)) {
    cell *k = (cell*) f->hp;
    f->hp += n;
    *--f->sp = word(k); }
  return f; }


NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  return g_run(please(f, n)); }

// keep
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
static NoInline void copy_from(core*, word*, word*, uintptr_t);
static NoInline void swap(core *f) {
  word *pool0 = f->pool, *loop0 = f->loop;
  f->pool = loop0, f->loop = pool0;
  copy_from(f, pool0, f->sp, f->len); }

NoInline core *please(core *f, uintptr_t req0) {
  word *loop0 = f->loop;
  size_t t0 = f->t0, t1 = g_clock(),
         len0 = f->len;
  swap(f);          // copy to new pool
  size_t t2 = f->t0 = g_clock(),      // get and set last gc end time
         req = req0 + len0 - avail(f),
         v = t2 == t1 ?  v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;
  // if v is out of bounds then calculate len1
  // by inverse proportionally adjusting len and v until v is in bounds
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return f; // no change reqired, hopefully the most common case
  // at this point we got a new target length and are gonna try and resize
  g_core *g = f->malloc(f, sizeof(core) + len1 * 2 * sizeof(word)); // allocate pool with the new target size
  if (!g) return req <= len0 ? f : encode(f, Oom); // if this fails still return true if the original pool is not too small
  memset(g, 0, sizeof(core));
  // we got the new pool so copy again and return true
  g->len = len1;            // set core variables referring to new pool
  g->pool = g->end;          //
  g->loop = g->end + len1;   //
  memcpy(g->vars, f->vars, sizeof(word) * g_var_N);
  g->malloc = f->malloc;
  g->free = f->free;
  g->safe = f->safe;
  copy_from(g, loop0, f->sp, len0); // do second copy
  f->free(f, f);  // free original pool
  g->t0 = g_clock();       // set last gc timestamp
  return g; }            // size successfully adjusted

// this function expects pool loop and len to have been set already on the state
static NoInline void copy_from(core *f, word *p0, word *sp0, uintptr_t len0) {
  g_word
    len1 = f->len, // target pool length
    *p1 = f->pool, // target pool
    *t0 = p0 + len0, // source pool top
    *t1 = p1 + len1, // target pool top
    sn = t0 - sp0, // stack height
    *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  f->sp = sp1;
  f->hp = f->cp = p1;
  f->symbols = 0;
  // copy all reachable values
  for (int n = 0; n < g_var_N; n++) // variables
    f->vars[n] = CP(f->vars[n]);
  while (sn--) *sp1++ = CP(*sp0++); // stack
  for (struct root *r = f->safe; r; *r->ptr = CP(*r->ptr), r = r->next); // C values
  // use cheney's algorithm
  while (f->cp < f->hp)
    if (datp(f->cp)) typ(f->cp)->wk(f, word(f->cp), p0, t0);
    else { while (*f->cp) *f->cp = CP(*f->cp),
                          f->cp++;
           f->cp += 2; }
  // run destructors ...
  // this has never been tested or used
  struct dtor *nd = NULL;
  for (struct dtor *n, *d = f->dtors; d; d = d->next)
    if (!owns(f, cell(d->x)->x)) d->d(f, d->x);
    else n = bump(f, Width(struct dtor)),
         n->d = d->d,
         n->x = cell(d->x)->x,
         n->next = nd,
         nd = n;
  f->dtors = nd; }

NoInline word cp(core *v, word x, word *p0, word *t0) {
  // if it's a number or outside managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  cell *src = (cell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (!nump(x) && owns(v, x)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return typ(src)->cp(v, (word) src, p0, t0);
  // it's a thread, find the end to find the head
  struct tag *t = ttag(src);
  cell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  // copy source contents to dest and write dest addresses to source
  for (cell *s = ini; (d->x = s->x); s++->x = W(d++));
  ((struct tag*) d)->head = dst;
  return word(dst + (src - ini)); }
