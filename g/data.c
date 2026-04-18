#include "i.h"
// some libc functions we use
g_vm(g_vm_tnew) {
 Have(Width(struct g_tab) + 1);
 struct g_tab *t = (struct g_tab*) Hp;
 struct g_kvs **tab = (struct g_kvs**) (t + 1);
 return
  Hp += Width(struct g_tab) + 1,
  tab[0] = 0,
  ini_tab(t, 0, 1, tab),
  Sp[0] = (intptr_t) t,
  Ip++,
  Continue(); }

op11(g_vm_tblp, tblp(Sp[0]) ? gputnum(-1) : g_nil)

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
 word v = f->sp[1],
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

g_vm(g_vm_tget2) {
  word z = Sp[0], k = Sp[1], x = Sp[2], n;
  if (even(x) && datp(x)) switch (typ(x)) {
    case tbl_q: z = g_tget(f, z, tbl(x), k); break;
    case vec_q:
      if (nump(k) && (n = getnum(k)) >= 0 && n < (word) len(x))
        z = putnum(txt(x)[n]);
      break;
    case two_q:
      if (nump(k) && (n = getnum(k)) >= 0) {
       while (n-- && twop(x = B(x)));
       if (twop(x)) z = A(x); } }
  return Sp[2] = z, Sp += 2, Ip += 1, Continue(); }

g_vm(g_vm_tset2) {
 if (tblp(Sp[2])) {
  Pack(f);
  if (!g_ok(f = g_tput(f))) return f;
  Unpack(f); }
 else Sp += 2;
 return Ip += 1, Continue(); }

g_vm(g_vm_tdel) {
 if (tblp(Sp[1])) Sp[2] = gtabdel(f, (struct g_tab*) Sp[1], Sp[2], Sp[0]);
 Sp += 2;
 Ip += 1;
 return Continue(); }

g_vm(g_vm_tlen) { return
 Sp[0] = tblp(Sp[0]) ? gputnum(((struct g_tab*)Sp[0])->len) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_tkeys) {
 intptr_t list = g_nil;
 if (tblp(Sp[0])) {
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

struct g *mktbl(struct g*f) {
 f = g_have(f, Width(struct g_tab) + 2);
 if (g_ok(f)) {
  struct g_tab *t = bump(f, Width(struct g_tab) + 1);
  *--f->sp = word(t);
  struct g_kvs **tab = (struct g_kvs**) (t + 1);
  tab[0] = 0, ini_tab(t, 0, 1, tab); }
 return f; }

static uintptr_t g_xx_two(struct g*f, struct g_pair *w) {
 return mix ^ (g_hash(f, w->a) * g_hash(f, w->b)); }
static uintptr_t g_xx_sym(struct g*f, struct g_atom *y) {
 return y->code; }
static uintptr_t g_xx_tab(struct g*f, struct g_tab *_) {
 return mix; }
static uintptr_t g_xx_vec(struct g*f, void *_) {
 uintptr_t len = g_vec_bytes(_), h = 2166136261;
 for (uint8_t *bs = _; len--; h ^= *bs++, h *= 16777619);
 return h; }
static uintptr_t (*hashers[])(struct g*, word) = {
 [two_q] = (void*) g_xx_two,
 [vec_q] = (void*) g_xx_vec,
 [tbl_q] = (void*) g_xx_tab,
 [sym_q] = (void*) g_xx_sym, };

#define SHIFT (sizeof(intptr_t)<<2)
// general g_hashing method...
uintptr_t g_hash(struct g *f, intptr_t x) {
 if (nump(x)) return (x*mix << SHIFT) | (x*mix >> SHIFT);
 if (datp(x)) return hashers[typ(x)](f, x);
 // it's a function, hash by length
 uintptr_t r = mix, *y = (uintptr_t *)x;
 while (*y++) r ^= r * mix;
 return r; }


op11(g_vm_car, twop(Sp[0]) ? A(Sp[0]) : Sp[0])
op11(g_vm_cdr, twop(Sp[0]) ? B(Sp[0]) : g_nil)
op11(g_vm_twop, twop(Sp[0]) ? gputnum(-1) : g_nil)
g_vm(g_vm_cons) {
 Have(Width(struct g_pair));
 struct g_pair *w = (struct g_pair*) Hp;
 Hp += Width(struct g_pair);
 ini_two(w, Sp[0], Sp[1]);
 *++Sp = word(w);
 Ip++;
 return Continue(); }

g_noinline struct g *gxl(struct g *f) {
 f = g_have(f, Width(struct g_pair));
 if (g_ok(f)) {
  struct g_pair *p = bump(f, Width(struct g_pair));
  ini_two(p, f->sp[0], f->sp[1]);
  *++f->sp = (intptr_t) p; }
 return f; }

struct g *gxr(struct g *f) {
 word x = f->sp[0];
 f->sp[0] = f->sp[1];
 f->sp[1] = x;
 return gxl(f); }

op11(g_vm_strp, strp(Sp[0]) ? gputnum(-1) : g_nil)

g_vm(g_vm_ssub) {
 if (!strp(Sp[0])) Sp[2] = g_nil;
 else {
  struct g_vec*s = (struct g_vec*) Sp[0], *t;
  intptr_t i = odd(Sp[1]) ? ggetnum(Sp[1]) : 0,
           j = odd(Sp[2]) ? ggetnum(Sp[2]) : 0;
  i = MAX(i, 0);
  i = MIN(i, (word) len(s));
  j = MAX(j, i);
  j = MIN(j, (word) len(s));
  if (i == j) Sp[2] = g_nil;
  else {
   size_t req = str_type_width + b2w(j - i);
   Have(req);
   t = (struct g_vec*) Hp;
   Hp += req;
   ini_str(t, j - i);
   memcpy(txt(t), txt(s) + i, j - i);
   Sp[2] = (word) t; } }
 return Ip += 1, Sp += 2, Continue(); }

g_vm(g_vm_scat) {
 intptr_t a = Sp[0], b = Sp[1];
 if (!strp(a)) Sp += 1;
 else if (!strp(b)) Sp[1] = a, Sp += 1;
 else {
  struct g_vec *x = vec(a), *y = vec(b), *z;
  uintptr_t
   len = len(x) + len(y),
   req = str_type_width + b2w(len);
  Have(req);
  z = (struct g_vec*) Hp;
  Hp += req;
  ini_str(z, len);
  memcpy(txt(z), txt(x), len(x));
  memcpy(txt(z) + len(x), txt(y), len(y));
  *++Sp = word(z); }
 return Ip++, Continue(); }

static size_t const vt_size[] = { [g_vect_u8]  = 1, };

uintptr_t g_vec_bytes(struct g_vec *v) {
 uintptr_t len = vt_size[v->type],
           rank = v->rank,
           *shape = v->shape;
 while (rank--) len *= *shape++;
 return sizeof(struct g_vec) + v->rank * sizeof(word) + len; }

static void ini_vecv(struct g_vec *v, uintptr_t type, uintptr_t rank, va_list xs) {
 uintptr_t *shape = v->shape;
 v->ap = g_vm_data;
 v->typ = vec_q;
 v->type = type;
 v->rank = rank;
 while (rank--) *shape++ = va_arg(xs, uintptr_t); }

void ini_vec(struct g_vec *v, uintptr_t type, uintptr_t rank, ...) {
 va_list xs;
 va_start(xs, rank);
 ini_vecv(v, type, rank, xs);
 va_end(xs); }

static struct g *vec0(struct g*f, uintptr_t type, uintptr_t rank, ...) {
 uintptr_t len = vt_size[type];
 va_list xs;
 va_start(xs, rank);
 for (uintptr_t i = rank; i--; len *= va_arg(xs, uintptr_t));
 va_end(xs);
 uintptr_t nbytes = sizeof(struct g_vec) + rank * sizeof(word) + len,
           ncells = b2w(nbytes);
 f = g_have(f, ncells + 1);
 if (g_ok(f)) {
  struct g_vec *v = bump(f, ncells);
  *--f->sp = word(v);
  va_start(xs, rank);
  ini_vecv(v, type, rank, xs);
  memset(v->shape + rank, 0, len);
  va_end(xs); }
 return f; }

struct g *g_strof(struct g *f, char const *cs) {
 uintptr_t len = 0;
 for (char const *ks = cs; *ks++; len++);
 f = vec0(f, g_vect_char, 1, len);
 if (g_ok(f)) memcpy(txt(f->sp[0]), cs, len);
 return f; }

g_vm(g_vm_gensym) {
 if (strp(Sp[0])) return Ap(g_vm_nomsym, f);
 uintptr_t const req = Width(struct g_atom) - 2;
 Have(req);
 struct g_atom *y = (struct g_atom*) Hp;
 return
  Hp += req,
  ini_anon(y, g_clock()),
  Sp[0] = word(y),
  Ip += 1,
  Continue(); }

g_vm(g_vm_symnom) {
 intptr_t y = Sp[0];
 return
  y = symp(y) && sym(y)->nom ? word(sym(y)->nom) : g_nil,
  Sp[0] = y,
  Ip += 1,
  Continue(); }

op11(g_vm_symp, symp(Sp[0]) ? gputnum(-1) : g_nil)
struct g *g_intern(struct g*f) {
 f = g_have(f, Width(struct g_atom));
 if (g_ok(f)) f->sp[0] = (word) g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols);
 return f; }

g_noinline struct g_atom *g_intern_r(struct g *v, struct g_vec *b, struct g_atom **y) {
 struct g_atom *z = *y;
 if (!z) return // found an empty spot, insert new symbol
  z = bump(v, Width(struct g_atom)),
  z->ap = g_vm_data,
  z->typ = sym_q,
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
 return
  Pack(f),
  y = g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols),
  Unpack(f),
  Sp[0] = word(y),
  Ip += 1,
  Continue(); }
