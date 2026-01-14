#include "i.h"
static g_noinline struct g_atom *g_intern_r(struct g *v, struct g_vec *b, struct g_atom **y);
int
 memcmp(void const*, void const*, size_t);
void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);
static struct g
 *please(struct g*, uintptr_t);
g_noinline g_vm(g_vm_gc, uintptr_t n) {
  Pack(f);
  f = please(f, n);
  if (g_ok(f)) return Unpack(f), Continue();
  return f; }

static intptr_t
  g_gc_cp(struct g*,intptr_t,intptr_t*,intptr_t*);

#define cp(...) g_gc_cp(__VA_ARGS__)
struct g *g_have(struct g *f, intptr_t n) {
 return !g_ok(f) || avail(f) >= n ? f : please(f, n); }
static struct g *g_pushr(struct g *f, uintptr_t m, uintptr_t n, va_list xs) {
 if (n == m) return please(f, m);
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

static g_noinline struct g *g_gc_cpg(struct g*g, intptr_t *p1, uintptr_t len1, struct g *f) {
 memcpy(g, f, sizeof(struct g));
 g->pool = (void*) p1;
 g->len = len1;
 uintptr_t len0 = f->len;
 g_word
  *p0 = ptr(f),
  *t0 = ptr(f) + len0, // source top
  *sp0 = f->sp,
  h = t0 - sp0; // stack height
 g->sp = ptr(g) + len1 - h;
 g->hp = g->cp = g->end;
 g->ip = cell(g_gc_cp(g, word(g->ip), p0, t0));
 g->symbols = 0;
 for (uintptr_t i = 0; i < g_nvars; i++)
  g->v[i] = g_gc_cp(g, g->v[i], p0, t0);
 for (intptr_t n = 0; n < h; n++)
  g->sp[n] = g_gc_cp(g, sp0[n], p0, t0);
 for (struct g_root *s = g->root; s; s = s->next)
  *s->ptr = g_gc_cp(g, *s->ptr, p0, t0);
 while (g->cp < g->hp)
  if (!datp(g->cp)) for (g->cp += 2; g->cp[-2]; g->cp++)
   g->cp[-2] = g_gc_cp(g, g->cp[-2], p0, t0);
  else if (typ(g->cp) == sym_class)
   g->cp += Width(struct g_atom) - (((struct g_atom*) g->cp)->nom ? 0 : 2);
  else if (typ(g->cp) == two_class) {
   struct g_pair *w = (void*) g->cp;
   g->cp += Width(struct g_pair);
   w->a = g_gc_cp(g, w->a, p0, t0);
   w->b = g_gc_cp(g, w->b, p0, t0); }
  else if (typ(g->cp) == tbl_class) {
   struct g_tab *t = (void*) g->cp;
   g->cp += Width(struct g_tab) + t->cap + t->len * Width(struct g_kvs);
   for (intptr_t i = 0, lim = t->cap; i < lim; i++)
    for (struct g_kvs*e = t->tab[i]; e;
     e->key = g_gc_cp(g, e->key, p0, t0),
     e->val = g_gc_cp(g, e->val, p0, t0),
     e = e->next); }
  else g->cp += b2w(g_vec_bytes((struct g_vec*) g->cp));
 return g; }
g_noinline struct g *please(struct g *f, uintptr_t req0) {
 uintptr_t const
  t0 = f->t0, // end of last gc period
  t1 = g_clock(), // end of current non-gc period
  len0 = f->len;
 // find alternate pool
 struct g *g = f == f->pool ? (struct g*) (cell(f) + f->len) : f->pool;
 f = g_gc_cpg(g, (void*) f->pool, f->len, f);
 uintptr_t const
  v_lo = 8,
  v_hi = 64,
  req = req0 + len0 - avail(f);
 uintptr_t
  len1 = len0,
  t2 = g_clock(),
  v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1);
 if (len1 < req || v < v_lo) // if too small
  do len1 <<= 1, v <<= 1; // then grow
  while (len1 < req || v < v_lo);
 else if (len1 > 2 * req && v > v_hi) // else if too big
  do len1 >>= 1, v >>= 1; // then shrink
  while (len1 > 2 * req && v > v_hi);
 else return f->t0 = t2, f; // else right size -> all done
 // allocate a new pool with target size
 g = f->malloc(f, len1 * 2 * sizeof(g_num));
 if (!g) return encode(f, req <= len0 ? g_status_ok : g_status_oom);
 g = g_gc_cpg(g, (g_num*) g, len1, f);
 f->free(f, f->pool);
 return g->t0 = g_clock(), g; }


static g_noinline intptr_t g_gc_cp(struct g *f, intptr_t x, intptr_t *p0, intptr_t *t0) {
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
  x = (intptr_t) src;
  if (typ(src) == two_class){
   struct g_pair *src = two(x),
                 *dst = bump(f, Width(struct g_pair));
   ini_two(dst, src->a, src->b);
   src->ap = (g_vm_t*) dst;
   return word(dst); }
  if (typ(src) == sym_class) {
   struct g_atom *src = sym(x), *dst;
   if (src->nom) dst = g_intern_r(f, (struct g_vec*) g_gc_cp(f, word(src->nom), p0, t0), &f->symbols);
   else dst = bump(f, Width(struct g_atom) - 2),
        ini_anon(dst, src->code);
   return (intptr_t) (src->ap = (g_vm_t*) dst); }
  if (typ(src) == tbl_class) {
   struct g_tab *src = tbl(x);
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
  else {
   struct g_vec *src = vec(x);
   uintptr_t bytes = g_vec_bytes(src);
   struct g_vec *dst = bump(f, b2w(bytes));
   src->ap = memcpy(dst, src, bytes);
   return word(dst); } }


struct g *g_intern(struct g*f) {
  f = g_have(f, Width(struct g_atom));
  if (g_ok(f)) f->sp[0] = (intptr_t) g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols);
  return f; }

static g_noinline struct g_atom *g_intern_r(struct g *v, struct g_vec *b, struct g_atom **y) {
 struct g_atom *z = *y;
 if (!z) return // found an empty spot, insert new symbol
  z = bump(v, Width(struct g_atom)),
  z->ap = g_vm_data,
  z->typ = sym_class,
  z->nom = b,
  z->code = g_hash(v, gputnum(g_hash(v, (intptr_t) b))),
  z->l = z->r = 0,
  *y = z;
 struct g_vec *a = z->nom;
 int i = len(a) < len(b) ? -1 :
         len(a) > len(b) ? 1 :
         memcmp(txt(a), txt(b), len(a));
 return i == 0 ? z :
  g_intern_r(v, b, i < 0 ? &z->l : &z->r); }

g_vm(g_vm_nomsym) {
 Have(Width(struct g_atom));
 struct g_atom *y;
 Pack(f);
 y = g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols),
 Unpack(f);
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }
