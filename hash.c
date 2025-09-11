#include "i.h"

// general hashing method...
uintptr_t hash(g_core *f, g_word x) {
  if (nump(x)) {
    const int shift = sizeof(word) * 4;
    return x *= mix, (x << shift) | (x >> shift); }
  if (datp(x)) return typ(x)->xx(f, x);
  if (!owns(f, x)) return mix ^ (mix * x);

  // it's a function, hash by length
  struct tag *t = ttag((cell*) x);
  word len = (cell*) t - t->head;
  return mix ^ (mix * len); }


static void em_tbl(core *f, FILE *o, word x),
            wk_tbl(core *f, word x, word *p0, word *t0);
static word cp_tbl(core *f, word x, word *p0, word *t0);
static uintptr_t xx_tbl(core *f, word h);

methods
  tbl_type = { .xx = xx_tbl, .cp = cp_tbl, .wk = wk_tbl, .eq = neql, .em = em_tbl, };

static void em_tbl(g_core *f, FILE *o, word x) {
  g_table *t = (g_table*) x;
  fprintf(o, "#table:%ld/%ld@%lx", (long) t->len, (long) t->cap, (long) x); }
static void wk_tbl(core *f, word x, word *p0, word *t0) {
  table *t = (table*) x;
  f->cp += Width(table) + t->cap + t->len * Width(struct entry);
  for (word i = 0, lim = t->cap; i < lim; i++)
    for (struct entry*e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

static word cp_tbl(core *f, word x, word *p0, word *t0) {
  table *src = (table*) x;
  size_t len = src->len, cap = src->cap;
  table *dst = bump(f, Width(table) + cap + Width(struct entry) * len);
  struct entry **tab = (struct entry**) (dst + 1),
               *dd = (struct entry*) (tab + cap);
  ini_table(dst, len, cap, tab);
  src->ap = (vm*) dst;
  for (struct entry *d, *s, *last; cap--; tab[cap] = last)
    for (s = src->tab[cap], last = NULL; s;
      d = dd++,
      d->key = s->key,
      d->val = s->val,
      d->next = last,
      last = d,
      s = s->next);
  return (word) dst; }

// FIXME very poor hashing method :(
static uintptr_t xx_tbl(core *f, word h) { return mix; }

// relies on table capacity being a power of 2
static Inline word index_of_key(core *f, table *t, word k) {
  return (t->cap - 1) & hash(f, k); }

NoInline g_core *g_hash_put_c(g_core *f) {
  if (!g_ok(f)) return f;
  g_table *t = (g_table*) f->sp[0];
  g_word k = f->sp[1], v = f->sp[2], i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  if (e) e->val = v;
  else {
    f = g_cells(f, Width(struct entry));
    if (!g_ok(f)) return f;
    e = (struct entry*) pop1(f);
    t = (g_table*) f->sp[0];
    k = f->sp[1], v = f->sp[2];
    e->key = k, e->val = v, e->next = t->tab[i], t->tab[i] = e;
    word cap0 = t->cap, load = ++t->len / cap0;
    if (load > 1) {
      // grow the table
      word cap1 = 2 * cap0;
      struct entry **tab0, **tab1;
      f = g_cells(f, cap1);
      if (!g_ok(f)) return f;
      tab1 = (struct entry**) pop1(f);
      t = (g_table*) f->sp[0];
      tab0 = t->tab;
      memset(tab1, 0, cap1 * sizeof(word));
      for (t->cap = cap1, t->tab = tab1; cap0--;)
        for (struct entry *e, *es = tab0[cap0]; es;
          e = es,
          es = es->next,
          i = (cap1-1) & hash(f, e->key),
          e->next = tab1[i],
          tab1[i] = e); } }
  f->sp += 2;
  f->sp[0] = (g_word) t;
  return f; }

static struct entry *table_delete_r(core *f, table *t, word k, word *v, struct entry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

static NoInline word table_delete(core *f, table *t, word k, word v) {
  word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->cap > 1 && t->len / t->cap < 1) {
    word cap = t->cap;
    struct entry *coll = 0, *x, *y; // collect all entries in one list
    for (word i = 0; i < cap; i++)
      for (x = t->tab[i], t->tab[i] = 0; x;)
        y = x, x = x->next, y->next = coll, coll = y;
    t->cap = cap >>= 1;
    for (word i; coll;)
      i = (cap - 1) & hash(f, coll->key),
      x = coll->next,
      coll->next = t->tab[i],
      t->tab[i] = coll,
      coll = x; }
  return v; }

Vm(tnew) {
  Have(Width(table) + 1);
  table *t = (table*) Hp;
  struct entry **tab = (struct entry**) (t + 1);
  Hp += Width(table) + 1;
  tab[0] = 0;
  ini_table(t, 0, 1, tab);
  Sp[0] = (word) t;
  Ip++;
  return Continue(); }

g_core *g_tget(core *f) {
  table *t = (table*) f->sp[1];
  word k = f->sp[2],
       zero = f->sp[0];
  size_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  f->sp += 2;
  f->sp[0] = e ? e->val : zero;
  return f; }

Vm(tget) {
  Pack(f);
  f = g_tget(f);
  Unpack(f);
  Ip += 1;
  return Continue(); }

Vm(tset) {
  if (tblp(Sp[0])) {
    Pack(f);
    f = g_hash_put_c(f);
    if (!g_ok(f)) return code_of(f);
    Unpack(f); }
  Ip += 1;
  return Continue(); }

Vm(tdel) {
  Sp[2] = !tblp(Sp[1]) ? nil : table_delete(f, (table*) Sp[1], Sp[2], Sp[0]);
  Sp += 2;
  Ip += 1;
  return Continue(); }

Vm(tlen) {
  Sp[0] = tblp(Sp[0]) ? putnum(((table*)Sp[0])->len) : nil;
  Ip += 1;
  return Continue(); }

Vm(tkeys) {
  word list = nil;
  if (tblp(Sp[0])) {
    table *t = (table*) Sp[0];
    word len = t->len;
    Have(len * Width(pair));
    pair *pairs = (pair*) Hp;
    Hp += len * Width(pair);
    for (int i = t->cap; i;)
      for (struct entry *e = t->tab[--i]; e; e = e->next)
        ini_pair(pairs, e->key, list),
        list = (word) pairs, pairs++; }
  Sp[0] = list;
  Ip += 1;
  return Continue(); }
