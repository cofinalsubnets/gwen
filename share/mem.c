#include "i.h"
#include <stdarg.h>

g_core *g_pop(g_core *f, uintptr_t m) {
  if (g_ok(f)) f->sp += m;
  return f; }

static g_core *g_pushr(g_core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (n == m) return please(f, m);
  g_word x = va_arg(xs, g_word);
  MM(f, &x);
  f = g_pushr(f, m, n + 1, xs);
  UM(f);
  if (g_ok(f)) *--f->sp = x;
  return f; }

#define avail(f) ((uintptr_t)(f->sp-f->hp))
g_core *g_push(g_core *f, uintptr_t m, ...) {
  if (!g_ok(f)) return f;
  va_list xs;
  va_start(xs, m);
  uintptr_t n = 0;
  if (avail(f) < m) f = g_pushr(f, m, n, xs);
  else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, g_word));
  va_end(xs);
  return f; }

g_core *g_have(g_core *f, uintptr_t n) {
  return !g_ok(f) || avail(f) >= n ? f : please(f, n); }

g_core *g_cells(g_core *f, uintptr_t n) {
  f = g_have(f, n + 1);
  if (g_ok(f)) {
    g_cell *k = (g_cell*) f->hp;
    f->hp += n;
    *--f->sp = word(k); }
  return f; }

NoInline Vm(gc, uintptr_t n) {
  Pack(f);
  f = please(f, n);
  if (g_ok(f)) return Unpack(f), Continue();
  return f; }

static g_core *copy_core(g_core*, g_word*, uintptr_t, g_core*);

// keep v between
#define v_lo 8
// and
#define v_hi (v_lo << 6)
// where
//   v = (t2 - t0) / (t2 - t1)
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
NoInline g_core *please(g_core *f, uintptr_t req0) {
//  g_fprintf(g_stderr, "alloc");
//  g_dbg(f);
  size_t t0 = f->t0, t1 = g_sys_clock(),
         len0 = f->len;
  g_word *w = (g_word*) f, *w0 = f->pool;
  g_core *g = (g_core*) (w == w0 ? w0 + len0 : w0);
//  g_fprintf(g_stderr, "pre copy");
  f = copy_core(g, f->pool, f->len, f);
//  g_fprintf(g_stderr, "post copy");
  size_t t2 = f->t0,      // get and set last gc end time
         req = req0 + len0 - avail(f),
         v = t2 == t1 ?  v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;

#define too_little (len1 < req || v < v_lo)
#define grow() (len1<<=1,v<<=1)
#define too_big (len1 >> 1 > req && v > v_hi)
#define shrink() (len1>>=1,v>>=1)
  if   (too_little) do grow(); while (too_little); // too small -> calculate bigger size
  else if (too_big) do shrink(); while (too_big);  // too big -> calculate smaller size
  else {
//    g_fprintf(g_stderr, "gc done");
    return f; }                                  // just right -> all done

 // g_fprintf(g_stderr, "alloc try len0=%d len1=%d", len0, len1);
  // allocate a new pool with target size
  g = f->malloc(f, len1 * 2 * sizeof(g_word));
  if (!g) {
    if (req <= len0) return f;
  //  g_fprintf(g_stderr, "alloc failed");
    return encode(f, g_status_oom); }
 // g_fprintf(g_stderr, "alloc ok");
  g = copy_core(g, (g_word*) g, len1, f);
  f->free(f, f->pool);
  return g; }

static g_core *copy_core(g_core *g, g_word *p1, uintptr_t len1, g_core *f) {
  g->free = f->free;
  g->malloc = f->malloc;
  g->pool = p1;
  g->len = len1;

  uintptr_t len0 = f->len;
  g_word *p0 = (g_word*) f,
         *t0 = (g_word*) f + len0, // source pool top
         *t1 = (g_word*) g + len1, // target pool top
         ht = t0 - f->sp; // stack height

  // reset stack, heap, symbols
  g->sp = t1 - ht;
  g->hp = g->cp = g->end;
  g->symbols = 0;

  // copy variables
  for (int n = 0; n < g_var_N; n++)
    g->vars[n] = cp(g, f->vars[n], p0, t0);

  // copy stack
  for (int n = 0; n < ht; n++)
    g->sp[n] = cp(g, f->sp[n], p0, t0);

  // copy saved values
  for (struct root *r = g->safe = f->safe; r; r = r->next)
    *r->ptr = cp(g, *r->ptr, p0, t0);

  // use cheney's algorithm to avoid unbounded recursion
  while (g->cp < g->hp)
    if (datp(g->cp)) typ(g->cp)->wk(g, (g_word) g->cp, p0, t0);
    else for (g->cp += 2; g->cp[-2]; g->cp++)
      g->cp[-2] = cp(g, g->cp[-2], p0, t0);

  g->t0 = g_sys_clock();
  return g; }

NoInline g_word cp(g_core *f, g_word x, g_word *p0, g_word *t0) {
  // if it's a number or it's outside managed memory then return it
  if (nump(x) || !within(p0, x, t0)) return x;
  g_cell *src = (g_cell*) x;
  x = src->x; // get its contents
  // if it contains a pointer to the new space then return the pointer
  if (!nump(x) && within((g_word*) f, x, (g_word*) f + f->len)) return x;
  // if it's data then call the copy function
  if (x == (g_word) data) return typ(src)->cp(f, (g_word) src, p0, t0);
  // it's a thread, find the end to find the head
  struct g_tag *t = ttag(src);
  g_cell *ini = t->head,
         *d = bump(f, t->end - ini),
         *dst = d;
  // copy source contents to dest and write dest addresses to source
  for (g_cell *s = ini; (d->x = s->x); s++->x = (g_word) d++);
  ((struct g_tag*) d)->head = dst;
  return (g_word) (dst + (src - ini)); }
