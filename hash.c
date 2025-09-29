#include "i.h"
#include <string.h>

g_core *g_tbl(g_core *f) {
  f = g_cells(f, Width(g_table) + 1);
  if (g_ok(f)) {
    g_table *t = tbl(f->sp[0]);
    struct entry **tab = (struct entry**) (t + 1);
    tab[0] = 0;
    ini_table(t, 0, 1, tab); }
  return f; }

// general hashing method...
uintptr_t hash(g_core *f, g_word x) {
  if (nump(x)) {
    const int shift = sizeof(g_word) * 4;
    return x *= mix, (x << shift) | (x >> shift); }
  if (datp(x)) return typ(x)->xx(f, x);
  if (!owns(f, x)) return mix ^ (mix * x);

  // it's a function, hash by length
  struct g_tag *t = ttag((g_cell*) x);
  g_word len = (g_cell*) t - t->head;
  return mix ^ (mix * len); }

static g_core* em_tbl(g_core*, g_file*, g_word);
static void wk_tbl(g_core*, g_word, g_word*, g_word*);
static g_word cp_tbl(g_core*, g_word, g_word*, g_word*);
static uintptr_t xx_tbl(g_core*, g_word);

g_type tbl_type = {
  .xx = xx_tbl,
  .cp = cp_tbl,
  .wk = wk_tbl,
  .eq = neql,
  .ap = self,
  .em = em_tbl, };

static g_core *em_tbl(g_core *f, g_file *o, g_word x) {
  g_table *t = (g_table*) x;
  fprintf(o, "#table:%ld/%ld@%lx", (long) t->len, (long) t->cap, (long) x);
  return f; }

static void wk_tbl(g_core *f, g_word x, g_word *p0, g_word *t0) {
  g_table *t = (g_table*) x;
  f->cp += Width(g_table) + t->cap + t->len * Width(struct entry);
  for (g_word i = 0, lim = t->cap; i < lim; i++)
    for (struct entry*e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

static g_word cp_tbl(g_core *f, g_word x, g_word *p0, g_word *t0) {
  g_table *src = (g_table*) x;
  size_t len = src->len, cap = src->cap;
  g_table *dst = bump(f, Width(g_table) + cap + Width(struct entry) * len);
  struct entry **tab = (struct entry**) (dst + 1),
               *dd = (struct entry*) (tab + cap);
  ini_table(dst, len, cap, tab);
  src->ap = (g_vm*) dst;
  for (struct entry *d, *s, *last; cap--; tab[cap] = last)
    for (s = src->tab[cap], last = NULL; s;
      d = dd++,
      d->key = s->key,
      d->val = s->val,
      d->next = last,
      last = d,
      s = s->next);
  return word(dst); }

// FIXME very poor hashing method :(
static uintptr_t xx_tbl(g_core *f, g_word h) { return mix; }

// relies on table capacity being a power of 2
static Inline g_word index_of_key(g_core *f, g_table *t, g_word k) {
  return (t->cap - 1) & hash(f, k); }

NoInline g_core *g_hash_put(g_core *f) {
  g_table *t = (g_table*) f->sp[2];
  g_word v = f->sp[1],
         k = f->sp[0];

  g_word i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;

  if (e) {
    e->val = v;
    goto done; }

  f = g_cells(f, Width(struct entry));
  if (!g_ok(f)) return f;

  e = (struct entry*) pop1(f);
  t = (g_table*) f->sp[2];
  k = f->sp[0];
  v = f->sp[1];

  e->key = k;
  e->val = v;
  e->next = t->tab[i];
  t->tab[i] = e;

  g_word cap0 = t->cap,
         load = ++t->len / cap0;

  if (load < 2) goto done;

  // grow the table
  g_word cap1 = 2 * cap0;
  struct entry **tab0, **tab1;
  f = g_cells(f, cap1);
  if (!g_ok(f)) return f;
  tab1 = (struct entry**) pop1(f);
  t = (g_table*) f->sp[2];
  tab0 = t->tab;
  memset(tab1, 0, cap1 * sizeof(g_word));
  for (t->cap = cap1, t->tab = tab1; cap0--;)
    for (struct entry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

done:
  f->sp += 2;
  return f; }

  
static struct entry *table_delete_r(g_core *f, g_table *t, g_word k, g_word *v, struct entry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return
    t->len--,
    *v = e->val,
    e->next;
  e->next = table_delete_r(f, t, k, v, e->next);
  return e; }

static NoInline g_word table_delete(g_core *f, g_table *t, g_word k, g_word v) {
  g_word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->cap > 1 && t->len / t->cap < 1) {
    g_word cap = t->cap;
    struct entry *coll = 0, *x, *y; // collect all entries in one list
    for (g_word i = 0; i < cap; i++)
      for (x = t->tab[i], t->tab[i] = 0; x;)
        y = x, x = x->next, y->next = coll, coll = y;
    t->cap = cap >>= 1;
    for (g_word i; coll;)
      i = (cap - 1) & hash(f, coll->key),
      x = coll->next,
      coll->next = t->tab[i],
      t->tab[i] = coll,
      coll = x; }
  return v; }

Vm(tnew) {
  Have(Width(g_table) + 1);
  g_table *t = (g_table*) Hp;
  struct entry **tab = (struct entry**) (t + 1);
  Hp += Width(g_table) + 1;
  tab[0] = 0;
  ini_table(t, 0, 1, tab);
  Sp[0] = (g_word) t;
  Ip++;
  return Continue(); }

g_word g_hash_get(g_core *f, g_word zero, g_table *t, g_word k) {
  size_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

Vm(tget) {
  Sp[2] = g_hash_get(f, Sp[0], tbl(Sp[1]), Sp[2]);
  Sp += 2;
  Ip += 1;
  return Continue(); }

Vm(tset) {
  if (tblp(Sp[0])) {
    g_word t = Sp[0],
           k = Sp[1],
           v = Sp[2];
    Sp[0] = k;
    Sp[1] = v;
    Sp[2] = t;
    Pack(f);
    f = g_hash_put(f);
    if (!g_ok(f)) return f;
    Unpack(f); }
  Ip += 1;
  return Continue(); }

Vm(tdel) {
  Sp[2] = !tblp(Sp[1]) ? nil : table_delete(f, (g_table*) Sp[1], Sp[2], Sp[0]);
  Sp += 2;
  Ip += 1;
  return Continue(); }

Vm(tlen) {
  Sp[0] = tblp(Sp[0]) ? putnum(((g_table*)Sp[0])->len) : nil;
  Ip += 1;
  return Continue(); }

Vm(tkeys) {
  g_word list = nil;
  if (tblp(Sp[0])) {
    g_table *t = (g_table*) Sp[0];
    g_word len = t->len;
    Have(len * Width(g_pair));
    g_pair *pairs = (g_pair*) Hp;
    Hp += len * Width(g_pair);
    for (int i = t->cap; i;)
      for (struct entry *e = t->tab[--i]; e; e = e->next)
        ini_pair(pairs, e->key, list),
        list = (g_word) pairs, pairs++; }
  Sp[0] = list;
  Ip += 1;
  return Continue(); }
