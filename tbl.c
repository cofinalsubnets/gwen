#include "i.h"

// FIXME very poor hashing method :(
static Word hash_table(Core *f, Word h) { return mix; }

static Word copy_table(Core *f, Word x, Word *p0, Word *t0) {
  Table *src = (Table*) x;
  Word i = src->cap;
  Table *dst = bump(f, Width(Table) + i);
  src->ap = (Vm*) ini_table(dst, src->len, src->cap, (void*) (dst + 1));

  //FIXME do these allocations in a block with the rest
  for (TableEntry *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(f, Width(TableEntry)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (Word) dst; }

static void walk_table(Core *f, Word x, Word *p0, Word *t0) {
  Table *t = (Table*) x;
  f->cp += Width(Table) + t->cap + t->len * Width(TableEntry);
  for (Word i = 0, lim = t->cap; i < lim; i++)
    for (TableEntry *e = t->tab[i]; e;
      e->key = cp(f, e->key, p0, t0),
      e->val = cp(f, e->val, p0, t0),
      e = e->next); }

Table *ini_table(Table *t, uintptr_t len, uintptr_t cap, TableEntry **tab) {
  return t->ap = data,
         t->typ = &table_type,
         t->len = len,
         t->cap = cap,
         t->tab = tab,
         t; }

static void print_table(Core *f, PFile *o, Word x) {
  Table *t = (Table*) x;
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

Word hash(Core *f, Word x) {
  if (nump(x)) {
    const int half_word_bits = sizeof(Word) * 4;
    x *= mix;
    x = (x << half_word_bits) | (x >> half_word_bits);
    return x; }

  if (datp(x)) return dtyp(x)->hash(f, x);
  if (!bounded(f->pool, x, f->pool+f->len)) return mix ^ (mix * x);
  // it's a function, hash by length
  struct tag *t = ttag((Cell*) x);
  Word len = (Cell*) t - t->head;
  return mix ^ (mix * len); }

Table *new_table(Core *f) {
  Table *t = cells(f, Width(Table) + 1);
  if (!t) return t;
  TableEntry **tab = (void*) (t + 1);
  tab[0] = 0;
  return ini_table(t, 0, 1, tab); }


static NoInline Table *table_insert(Core *f, Table *t, Word k, Word v, Word i) {
  TableEntry *e;
  avec(f, t, avec(f, k, avec(f, v, e = cells(f, Width(TableEntry)))));
  if (!e) return 0;
  e->key = k, e->val = v, e->next = t->tab[i];
  t->tab[i] = e;
  Word cap0 = t->cap, load = ++t->len / cap0;
  if (load <= 1) return t;
  // grow the table
  TableEntry **tab0, **tab1;
  Word cap1 = 2 * cap0;
  avec(f, t, tab1 = cells(f, cap1));
  tab0 = t->tab;
  if (!tab1) return 0;
  memset(tab1, 0, cap1 * sizeof(Word));
  for (Word i; cap0--;)
    for (TableEntry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  t->cap = cap1, t->tab = tab1;
  return t; }

static Inline Word index_of_key(Core *f, Table *t, Word k) {
  // relies on table capacity being a power of 2
  return (t->cap - 1) & hash(f, k); }

NoInline Table *table_set(Core *f, Table *t, Word k, Word v) {
  Word index = index_of_key(f, t, k);
  TableEntry *entry = t->tab[index];
  while (entry && !eql(f, k, entry->key)) entry = entry->next;
  if (entry) return entry->val = v, t;
  return table_insert(f, t, k, v, index); }

static TableEntry *table_delete_r(Core *f, Table *t, Word k, Word *v, TableEntry *e) {
  if (!e) return e;
  if (eql(f, e->key, k)) return t->len--, *v = e->val, e->next;
  return e->next = table_delete_r(f, t, k, v, e->next), e; }

static void table_shrink(Core *f, Table *t) {
  Word cap = t->cap;
  TableEntry *coll = 0, *x, *y; // collect all entries in one list
  for (Word i = 0; i < cap; i++)
    for (x = t->tab[i], t->tab[i] = 0; x;)
      y = x, x = x->next, y->next = coll, coll = y;
  t->cap = cap >>= 2;
  for (Word i; coll;)
    i = (cap - 1) & hash(f, coll->key),
    x = coll->next,
    coll->next = t->tab[i],
    t->tab[i] = coll,
    coll = x; }

static NoInline Word table_delete(Core *f, Table *t, Word k, Word v) {
  Word idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->len / t->cap > 1) table_shrink(f, t);
  return v; }

Vm(tnew) {
  Have(Width(Table) + 1);
  Table *t = (Table*) Hp;
  TableEntry **tab = (TableEntry**) (t + 1);
  Hp += Width(Table) + 1;
  tab[0] = 0;
  return op(1, (Word) ini_table(t, 0, 1, tab)); }

Word table_get(Core *f, Table *t, Word k, Word zero) {
  size_t i = index_of_key(f, t, k);
  TableEntry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

Vm(tget) {
  return op(3, !tblp(Sp[1]) ? Sp[2] :
    table_get(f, (Table*) Sp[1], Sp[2], Sp[0])); }

Vm(tset) {
  Word x = Sp[0];
  if (!tblp(x)) return op(3, nil);
  Pack(f);
  Table *t = table_set(f, (Table*) x, Sp[1], Sp[2]);
  Unpack(f);
  return !t ? Oom : op(3, Sp[2]); }

Vm(tdel) {
  Word x = Sp[1];
  return op(3, !tblp(x) ? nil :
    table_delete(f, (Table*) x, Sp[2], Sp[0])); }

Vm(tlen) {
  Word x = Sp[0];
  if (!tblp(x)) return op(1, nil);
  Table *t = (Table*) x;
  return op(1, putnum(t->len)); }

Vm(tkeys) {
  if (!tblp(Sp[0])) return op(1, nil);
  Table *t = (Table*) Sp[0];
  Word len = t->len, list = nil;
  Have(len * Width(Pair));
  Pair *pairs = (Pair*) Hp;
  Hp += len * Width(Pair);
  for (int i = t->cap; i;)
    for (TableEntry *e = t->tab[--i]; e; e = e->next)
      pairs->ap = data, pairs->typ = &pair_type,
      pairs->a = e->key, pairs->b = list,
      list = (Word) pairs, pairs++;
  return op(1, list); }

Type table_type = {
  .hash = hash_table,
  .copy = copy_table,
  .evac = walk_table,
  .equal = not_equal,
  .emit = print_table,
};
