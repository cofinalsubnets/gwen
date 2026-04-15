#include "i.h"

static word gcp(struct g*, word, word const *, word const *);

static void g_wk_vec(struct g*g, struct g_vec *v, word const*p0, word const *t0) {
 g->cp += b2w(g_vec_bytes(v)); }

static void g_wk_two(struct g*g, struct g_pair *w, word const *p0, word const*t0) {
 g->cp += Width(struct g_pair);
 w->a = gcp(g, w->a, p0, t0);
 w->b = gcp(g, w->b, p0, t0); }

static void g_wk_tab(struct g*g, struct g_tab *t, word const*p0, word const*t0) {
 g->cp += Width(struct g_tab) + t->cap + t->len * Width(struct g_kvs);
 for (intptr_t i = 0, lim = t->cap; i < lim; i++)
  for (struct g_kvs*e = t->tab[i]; e;
   e->key = gcp(g, e->key, p0, t0),
   e->val = gcp(g, e->val, p0, t0),
   e = e->next); }

static void g_wk_sym(struct g*g, struct g_atom *y, intptr_t const*p0, intptr_t const*t0) {
 g->cp += Width(struct g_atom) - (y->nom ? 0 : 2); }

static void (*wks[])(struct g*, word*, intptr_t const*, intptr_t const*) = {
 [two_q] = (void*) g_wk_two,
 [vec_q] = (void*) g_wk_vec,
 [sym_q] = (void*) g_wk_sym,
 [tbl_q] = (void*) g_wk_tab, };


static intptr_t g_cp_two(struct g*f, struct g_pair *src, intptr_t const *p0, intptr_t const*t0) {
 struct g_pair *dst = bump(f, Width(struct g_pair));
 ini_two(dst, src->a, src->b);
 src->ap = (g_vm_t*) dst;
 return word(dst); }

static intptr_t g_cp_vec(struct g*f, struct g_vec *src, intptr_t const*p0, intptr_t const*t0) {
 uintptr_t bytes = g_vec_bytes(src);
 struct g_vec *dst = bump(f, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

static intptr_t g_cp_sym(struct g*f, struct g_atom *src, intptr_t const*p0, intptr_t const*t0) {
 struct g_atom *dst;
 if (src->nom) dst = g_intern_r(f, (struct g_vec*) gcp(f, word(src->nom), p0, t0), &f->symbols);
 else dst = bump(f, Width(struct g_atom) - 2),
      ini_anon(dst, src->code);
 return (intptr_t) (src->ap = (g_vm_t*) dst); }

static intptr_t g_cp_tab(struct g*f, struct g_tab *src, intptr_t const*p0, intptr_t const*t0) {
 uintptr_t len = src->len, cap = src->cap;
 struct g_tab *dst = bump(f, Width(struct g_tab) + cap + Width(struct g_kvs) * len);
 struct g_kvs **tab = (struct g_kvs**) (dst + 1),
              *dd = (struct g_kvs*) (tab + cap);
 ini_tab(dst, len, cap, tab);
 src->ap = (g_vm_t*) dst;
 for (struct g_kvs *d, *s, *last; cap--; tab[cap] = last)
   for (s = src->tab[cap], last = NULL; s;
     d = dd++, d->key = s->key, d->val = s->val, d->next = last,
     last = d, s = s->next);
 return word(dst); }

static intptr_t (*copiers[])(struct g*, word, word const*, word const*) = {
 [two_q] = (void*) g_cp_two,
 [vec_q] = (void*) g_cp_vec,
 [sym_q] = (void*) g_cp_sym,
 [tbl_q] = (void*) g_cp_tab, };

#define cp(...) gcp(__VA_ARGS__)
struct g *g_have(struct g *f, intptr_t n) {
 return !g_ok(f) || avail(f) >= n ? f : g_please(f, n); }

static struct g *g_pushr(struct g *f, uintptr_t m, uintptr_t n, va_list xs) {
 if (n == m) return g_please(f, m);
 intptr_t x = va_arg(xs, intptr_t);
 MM(f, &x);
 f = g_pushr(f, m, n + 1, xs);
 UM(f);
 if (g_ok(f)) *--f->sp = x;
 return f; }

struct g *g_push(struct g *f, intptr_t m, ...) {
 if (!g_ok(f)) return f;
 va_list xs;
 va_start(xs, m);
 intptr_t n = 0;
 if (avail(f) < m) f = g_pushr(f, m, n, xs);
 else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, intptr_t));
 va_end(xs);
 return f; }

static g_noinline struct g *gcg(struct g*g, intptr_t *p1, uintptr_t len1, struct g *f) {
 memcpy(g, f, sizeof(struct g));
 g->pool = (void*) p1;
 g->len = len1;
 uintptr_t len0 = f->len;
 word const *p0 = ptr(f),
            *t0 = ptr(f) + len0, // source top
            *sp0 = f->sp;
 word h = t0 - sp0; // stack height
 g->sp = ptr(g) + len1 - h;
 g->hp = g->cp = g->end;
 g->ip = cell(gcp(g, word(g->ip), p0, t0));
 g->symbols = 0;
 for (uintptr_t i = 0; i < g_nvars; i++)
  g->v[i] = gcp(g, g->v[i], p0, t0);
 for (intptr_t n = 0; n < h; n++)
  g->sp[n] = gcp(g, sp0[n], p0, t0);
 for (struct g_root *s = g->root; s; s = s->next)
  *s->ptr = gcp(g, *s->ptr, p0, t0);
 while (g->cp < g->hp)
  if (datp(g->cp)) wks[typ(g->cp)](g, g->cp, p0, t0);
  else for (g->cp += 2; g->cp[-2]; g->cp++)
   g->cp[-2] = gcp(g, g->cp[-2], p0, t0);
 return g; }

g_noinline struct g *g_please(struct g *f, uintptr_t req0) {
 uintptr_t const
  t0 = f->t0, // end of last gc period
  t1 = g_clock(), // end of current non-gc period
  len0 = f->len;
 // find alternate pool
 struct g *g = f == f->pool ? (struct g*) (cell(f) + f->len) : f->pool;
 f = gcg(g, (void*) f->pool, f->len, f);
 uintptr_t const
  v_f = 2, // 2 or 3 seem like good values
  v_lo = 1 << v_f,
  v_hi = v_lo << v_f,
  req = req0 + len0 - avail(f),
  t2 = g_clock();
 uintptr_t
  len1 = len0,
  v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1);
 if (len1 < req || v < v_lo) // if too small
  do len1 <<= 1, v <<= 1; // then grow
  while (len1 < req || v < v_lo);
 else if (len1 > 2 * req && v > v_hi) // else if too big
  do len1 >>= 1, v >>= 1; // then shrink
  while (len1 > 2 * req && v > v_hi);
 else return f->t0 = t2, f; // else right size -> all done
 return // allocate a new pool with target size
  !(g = f->malloc(f, len1 * 2 * sizeof(word))) ?
   encode(f, req <= len0 ? g_status_ok : g_status_oom) :
   (g = gcg(g, (word*) g, len1, f),
    f->free(f, f->pool),
    g->t0 = g_clock(),
    g); }

static g_noinline intptr_t gcp(struct g *f, intptr_t x, intptr_t const *p0, intptr_t const *t0) {
  // if it's a number or it's outside managed memory then return it
  if (odd(x) || ptr(x) < p0 || ptr(x) >= t0) return x;
  union u *src = cell(x);
  x = src->x; // get its contents
  // if it contains a pointer to the new space then return the pointer
  if (even(x) && ptr(f) <= ptr(x)
              && ptr(x) < ptr(f) + f->len) return x;
  // if it's data then call the copy function
  if (x != (intptr_t) g_vm_data) {
   // it's a thread, find the end to find the head
   struct g_tag *t = ttag(src);
   union u *ini = t->head,
           *d = bump(f, t->end - ini),
           *dst = d;
   // copy source contents to dest and write dest addresses to source
   for (union u*s = ini; (d->x = s->x); s++->x = (intptr_t) d++);
   ((struct g_tag*) d)->head = dst;
   return (intptr_t) (dst + (src - ini)); }
  return copiers[typ(src)](f, (intptr_t) src, p0, t0); }
