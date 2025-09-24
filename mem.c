#include "i.h"
#define avail(f) (f->sp-f->hp)
static g_core *please(g_core*, uintptr_t);

static g_core *g_pushr(g_core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (n == m) return please(f, m);
  word x = va_arg(xs, word);
  MM(f, &x);
  f = g_pushr(f, m, n + 1, xs);
  UM(f);
  if (g_ok(f)) *--f->sp = x;
  return f; }

g_core *g_push(g_core *f, uintptr_t m, ...) {
  if (g_ok(f)) {
    va_list xs;
    va_start(xs, m);
    g_word n = 0;
    if (avail(f) < m) f = g_pushr(f, m, n, xs);
    else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, word));
    va_end(xs); }
  return f; }

g_core *g_pop(g_core *f, uintptr_t m) {
  if (g_ok(f)) f->sp += m;
  return f; }

Inline g_core *g_have(g_core *f, uintptr_t n) {
  return !g_ok(f) || avail(f) >= n ? f : please(f, n); }

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
static g_core *copy_core(g_core*, g_word*, uintptr_t, g_core*);
NoInline g_core *please(g_core *f, uintptr_t req0) {
  size_t t0 = f->t0, t1 = g_clock(),
         len0 = f->len;
  word *pool0 = f->pool,
       *pool1 = pool0 == f->end ? pool0 + len0 : f->end;
  f = copy_core(f, pool1, len0, f);
  size_t t2 = f->t0,      // get and set last gc end time
         req = req0 + len0 - avail(f),
         v = t2 == t1 ?  v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;
  // if v is out of bounds then calculate len1
  // by inverse proportionally adjusting len and v until v is in bounds
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return f; // no change reqired, hopefully the most common case
  // at this point we got a new target length and are gonna try and resize
  g_core *g = g_malloc(sizeof(g_core) + len1 * 2 * sizeof(word)); // allocate pool with the new target size
  if (!g) return req <= len0 ? f : encode(f, g_status_oom); // if this fails still return true if the original pool is not too small
  g = copy_core(g, g->end, len1, f);
  // we got the new pool so copy again and return true
  g_free(f);     // free original pool
  return g; }        // size successfully adjusted

static void copy_from(g_core*, word*, word*, uintptr_t);
static g_core *copy_core(g_core *g, g_word *p1, uintptr_t len1, g_core *f) {
  g_word *p0 = f->pool, *sp0 = f->sp;
  uintptr_t len0 = f->len;
  g->pool = p1;
  g->len = len1;
  g->safe = f->safe;

  for (int i = 0; i < g_var_N; i++) g->vars[i] = f->vars[i];
  g_word
    *t0 = p0 + len0, // source pool top
    *t1 = p1 + len1, // target pool top
    sn = t0 - sp0, // stack height
    *sp1 = t1 - sn; // target pool stack
  // reset stack, heap, symbols
  g->sp = sp1;
  g->hp = g->cp = p1;
  g->symbols = 0;
  // copy all reachable values
  for (int n = 0; n < g_var_N; n++) // variables
    g->vars[n] = cp(g, f->vars[n], p0, t0);
  while (sn--) *sp1++ = cp(g, *sp0++, p0, t0); // stack
  for (struct root *r = g->safe; r; *r->ptr = cp(g, *r->ptr, p0, t0), r = r->next); // C values
  // use cheney's algorithm
  while (g->cp < g->hp)
    if (datp(g->cp)) typ(g->cp)->wk(g, word(g->cp), p0, t0);
    else { while (*g->cp) *g->cp = cp(g, *g->cp, p0, t0),
                          g->cp++;
           g->cp += 2; }
  g->t0 = g_clock();
  return g; }

NoInline word cp(g_core *v, word x, word *p0, word *t0) {
  // if it's a number or outside managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  cell *src = (cell*) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (!nump(x) && owns(v, x)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return typ(src)->cp(v, (word) src, p0, t0);
  // it's a thread, find the end to find the head
  struct g_tag *t = ttag(src);
  cell *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  // copy source contents to dest and write dest addresses to source
  for (cell *s = ini; (d->x = s->x); s++->x = word(d++));
  ((struct g_tag*) d)->head = dst;
  return word(dst + (src - ini)); }
