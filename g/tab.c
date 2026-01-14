#include "i.h"
// some libc functions we use
int
 memcmp(void const*, void const*, size_t);
void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);
g_vm(g_vm_tnew) {
 Have(Width(struct g_tab) + 1);
 struct g_tab *t = (struct g_tab*) Hp;
 struct g_kvs **tab = (struct g_kvs**) (t + 1);
 Hp += Width(struct g_tab) + 1;
 tab[0] = 0;
 ini_tab(t, 0, 1, tab);
 Sp[0] = (intptr_t) t;
 Ip++;
 return Continue(); }

// relies on table capacity being a power of 2
static g_inline uintptr_t index_of_key(struct g *f, struct g_tab *t, intptr_t k) {
 return (t->cap - 1) & g_hash(f, k); }

intptr_t g_tget(struct g *f, intptr_t zero, struct g_tab *t, intptr_t k) {
 uintptr_t i = index_of_key(f, t, k);
 struct g_kvs *e = t->tab[i];
 while (e && !eql(f, k, e->key)) e = e->next;
 return e ? e->val : zero; }

g_noinline struct g *g_tput(struct g *f) {
 struct g_tab *t = (struct g_tab*) f->sp[2];
 g_num v = f->sp[1],
       k = f->sp[0];
 uintptr_t i = index_of_key(f, t, k);
 struct g_kvs *e = t->tab[i];
 while (e && !eql(f, k, e->key)) e = e->next;

 if (e) return e->val = v, f->sp += 2, f;

 f = g_have(f, Width(struct g_kvs) + 1);
 if (!g_ok(f)) return f;
 e = bump(f, Width(struct g_kvs));
 t = (struct g_tab*) f->sp[2];
 k = f->sp[0];
 v = f->sp[1];

 e->key = k;
 e->val = v;
 e->next = t->tab[i];
 t->tab[i] = e;

 intptr_t cap0 = t->cap,
          load = ++t->len / cap0;

 if (load < 2) return f->sp += 2, f;

 // grow the table
 intptr_t cap1 = 2 * cap0;
 struct g_kvs **tab0, **tab1;

 f = g_have(f, cap1 + 1);
 if (!g_ok(f)) return f;
 tab1 = bump(f, cap1);
 t = (struct g_tab*) f->sp[2];
 tab0 = t->tab;
 memset(tab1, 0, cap1 * sizeof(intptr_t));
 for (t->cap = cap1, t->tab = tab1; cap0--;)
  for (struct g_kvs *e, *es = tab0[cap0]; es;
   e = es,
   es = es->next,
   i = (cap1-1) & g_hash(f, e->key),
   e->next = tab1[i],
   tab1[i] = e);

 return f->sp += 2, f; }

  
static struct g_kvs *gtabdelr(struct g *f, struct g_tab *t, intptr_t k, intptr_t *v, struct g_kvs *e) {
 if (e) {
  if (eql(f, e->key, k)) return
   t->len--,
   *v = e->val,
   e->next;
  e->next = gtabdelr(f, t, k, v, e->next); }
 return e; }

static g_noinline intptr_t gtabdel(struct g *f, struct g_tab *t, intptr_t k, intptr_t v) {
 uintptr_t idx = index_of_key(f, t, k);
 t->tab[idx] = gtabdelr(f, t, k, &v, t->tab[idx]);
 if (t->cap > 1 && t->len / t->cap < 1) {
  intptr_t cap = t->cap;
  struct g_kvs *coll = 0, *x, *y; // collect all entries in one list
  for (intptr_t i = 0; i < cap; i++)
   for (x = t->tab[i], t->tab[i] = 0; x;)
    y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 1;
  for (intptr_t i; coll;)
   i = (cap - 1) & g_hash(f, coll->key),
   x = coll->next,
   coll->next = t->tab[i],
   t->tab[i] = coll,
   coll = x; }
 return v; }

g_vm(g_vm_tget) { return
 Sp[2] = g_tget(f, Sp[0], tbl(Sp[1]), Sp[2]),
 Sp += 2,
 Ip += 1,
 Continue(); }

g_vm(g_vm_tset) {
 if (tabp(Sp[0])) {
  g_num t = Sp[0],
        k = Sp[1],
        v = Sp[2];
  Sp[0] = k;
  Sp[1] = v;
  Sp[2] = t;
  Pack(f);
  f = g_tput(f);
  if (!g_ok(f)) return f;
  Unpack(f); }
 return Ip += 1,
        Continue(); }

g_vm(g_vm_tdel) {
 if (tabp(Sp[1])) Sp[2] = gtabdel(f, (struct g_tab*) Sp[1], Sp[2], Sp[0]);
 Sp += 2;
 Ip += 1;
 return Continue(); }

g_vm(g_vm_tlen) { return
 Sp[0] = tabp(Sp[0]) ? gputnum(((struct g_tab*)Sp[0])->len) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_tkeys) {
 intptr_t list = g_nil;
 if (tabp(Sp[0])) {
  struct g_tab *t = (struct g_tab*) Sp[0];
  intptr_t len = t->len;
  Have(len * Width(struct g_pair));
  struct g_pair *pairs = (struct g_pair*) Hp;
  Hp += len * Width(struct g_pair);
  for (uintptr_t i = t->cap; i;)
   for (struct g_kvs *e = t->tab[--i]; e; e = e->next)
    ini_two(pairs, e->key, list),
    list = (intptr_t) pairs, pairs++; }
 Sp[0] = list;
 Ip += 1;
 return Continue(); }

struct g *g_tnew(struct g*f) {
 f = g_have(f, Width(struct g_tab) + 2);
 if (g_ok(f)) {
  struct g_tab *t = bump(f, Width(struct g_tab) + 1);
  *--f->sp = word(t);
  struct g_kvs **tab = (struct g_kvs**) (t + 1);
  tab[0] = 0, ini_tab(t, 0, 1, tab); }
 return f; }

// general g_hashing method...
uintptr_t g_hash(struct g *f, intptr_t x) {
 if (nump(x)) {
  int const shift = sizeof(g_num) * 4;
  return x *= mix, (x << shift) | (x >> shift); }
 else if (!datp(x)) {
  if (!owns(f, x)) return mix ^ (mix * x);
  // it's a function, g_hash by length
  struct g_tag *t = ttag((void*)x);
  intptr_t len = (union u*) t - t->head;
  return mix ^ (mix * len); }
 else if (typ(x) == two_class)
  return mix ^ (g_hash(f, A(x)) * g_hash(f, B(x)));
 else if (typ(x) == sym_class)
  return sym(x)->code;
 else if (typ(x) == tbl_class)
  return mix;
 else {
  void *_ = (void*) x;
  uintptr_t len = g_vec_bytes(_), h = 2166136261;
  for (uint8_t *bs = _; len--; h ^= *bs++, h *= 16777619);
  return h; } }



