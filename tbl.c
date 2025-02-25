#include "i.h"

// FIXME very poor hashing method :(
static PWord hash_table(PCore *f, PWord h) { return mix; }

static PWord copy_table(PCore *f, PWord x, PWord *p0, PWord *t0) {
  PTable *src = (PTable*) x;
  PWord i = src->cap;
  PTable *dst = bump(f, Width(PTable) + i);
  src->ap = (PVm*) ini_table(dst, src->len, src->cap, (void*) (dst + 1));

  //FIXME do these allocations in a block with the rest
  for (PTableEntry *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(f, Width(PTableEntry)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (PWord) dst; }

static void walk_table(PCore *f, PWord x, PWord *p0, PWord *t0) {
  PTable *t = (PTable*) x;
  f->cp += Width(PTable) + t->cap + t->len * Width(PTableEntry);
  for (PWord i = 0, lim = t->cap; i < lim; i++)
    for (PTableEntry *e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

Table *ini_table(Table *t, uintptr_t len, uintptr_t cap, PTableEntry **tab) {
  return t->ap = data,
         t->typ = &table_type,
         t->len = len,
         t->cap = cap,
         t->tab = tab,
         t; }

static void print_table(PCore *f, PFile *o, PWord x) {
  PTable *t = (PTable*) x;
  fprintf(o, "#table:%ld/%ld@%lx", (long) t->len, (long) t->cap, (long) x); }

// this is a totally ad hoc, unproven hashing method.
//
// its performance on hash tables and anonymous functions
// is very bad (they all go to the same bucket!)
//
// strings, symbols, and numbers do better. for pairs it
// depends on what they contain.
//
// copying GC complicates the use of memory addresses for
// hashing mutable data, which is the obvious way to fix
// the bad cases. we would either need to assign each datum
// a unique identifier when it's created & hash using that,
// or use the address but rehash as part of garbage collection.

PWord hash(PCore *f, PWord x) {
  if (nump(x)) {
    const int half_word_bits = sizeof(PWord) * 4;
    x *= mix;
    x = (x << half_word_bits) | (x >> half_word_bits);
    return x; }

  if (datp(x)) return dtyp(x)->hash(f, x);
  if (!bounded(f->pool, x, f->pool+f->len)) return mix ^ (mix * x);
  // it's a function, hash by length
  struct tag *t = ttag((PCell*) x);
  PWord len = (PCell*) t - t->head;
  return mix ^ (mix * len); }

PTable *new_table(PCore *f) {
  PTable *t = cells(f, Width(PTable) + 1);
  if (!t) return t;
  PTableEntry **tab = (void*) (t + 1);
  tab[0] = 0;
  return ini_table(t, 0, 1, tab); }


static NoInline PTable *table_insert(PCore *f, PTable *t, PWord k, PWord v, PWord i) {
  PTableEntry *e;
  avec(f, t, avec(f, k, avec(f, v, e = cells(f, Width(PTableEntry)))));
  if (!e) return 0;
  e->key = k, e->val = v, e->next = t->tab[i];
  t->tab[i] = e;
  PWord cap0 = t->cap, load = ++t->len / cap0;
  if (load <= 1) return t;
  // grow the table
  PTableEntry **tab0, **tab1;
  PWord cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  tab0 = t->tab;
  if (!tab1) return 0;
  memset(tab1, 0, cap1 * sizeof(PWord));
  for (PWord i; cap0--;)
    for (PTableEntry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1, t->tab = tab1;
  return t; }

static Inline PWord index_of_key(PCore *f, PTable *t, PWord k) {
  // relies on table capacity being a power of 2
  return (t->cap - 1) & hash(f, k); }

NoInline PTable *table_set(PCore *f, PTable *t, PWord k, PWord v) {
  PWord index = index_of_key(f, t, k);
  PTableEntry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  if (entry) return entry->val = v, t;
  return table_insert(f, t, k, v, index); }

static PTableEntry *table_delete_r(PCore *f, PTable *t, PWord k, PWord *v, PTableEntry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

static void table_shrink(PCore *f, PTable *t) {
  PWord cap = t->cap;
  PTableEntry *coll = 0, *x, *y; // collect all entries in one list
  for (PWord i = 0; i < cap; i++)
    for (x = t->tab[i], t->tab[i] = 0; x;)
      y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 2;
  for (PWord i; coll;)
    i = (cap - 1) & hash(f, coll->key),
    x = coll->next,
    coll->next = t->tab[i],
    t->tab[i] = coll,
    coll = x; }

static NoInline PWord table_delete(PCore *f, PTable *t, PWord k, PWord v) {
  PWord idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->len / t->cap > 1) table_shrink(f, t);
  return v; }

Vm(tnew) {
  Have(Width(PTable) + 1);
  PTable *t = (PTable*) Hp;
  PTableEntry **tab = (PTableEntry**) (t + 1);
  Hp += Width(PTable) + 1;
  tab[0] = 0;
  return op(1, (PWord) ini_table(t, 0, 1, tab)); }

PWord table_get(PCore *f, PTable *t, PWord k, PWord zero) {
  PTableEntry *entry = t->tab[index_of_key(f, t, k)];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  return entry ? entry->val : zero; }

Vm(tget) {
  return op(3, !tblp(Sp[1]) ? Sp[2] :
    table_get(f, (PTable*) Sp[1], Sp[2], Sp[0])); }

Vm(tset) {
  PWord x = Sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  PTable *t = table_set(f, (PTable*) x, Sp[1], Sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, Sp[2]); }

Vm(tdel) {
  PWord x = Sp[1];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (PTable*) x, Sp[2], Sp[0])); }

Vm(tlen) {
  PWord x = Sp[0];
  if (!tblp(x)) return op(1, nil);
  PTable *t = (PTable*) x;
  return op(1, putnum(t->len)); }

Vm(tkeys) {
  if (!tblp(Sp[0])) return op(1, nil);
  PTable *t = (PTable*) Sp[0];
  PWord len = t->len, list = nil;
  Have(len * Width(PPair));
  PPair *pairs = (PPair*) Hp;
  Hp += len * Width(PPair);
  for (int i = t->cap; i;)
    for (PTableEntry *e = t->tab[--i]; e; e = e->next)
      pairs->ap = data, pairs->typ = &pair_type,
      pairs->a = e->key, pairs->b = list,
      list = (PWord) pairs, pairs++;
  return op(1, list); }

PType table_type = {
  .hash = hash_table,
  .copy = copy_table,
  .evac = walk_table,
  .equal = not_equal,
  .emit = print_table,
};
