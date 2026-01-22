#include "i.h"
#include <stdarg.h>

// some libc functions we use
int
 memcmp(void const*, void const*, size_t);
void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);


struct g_in g_stdin = { .getc = (void*) ggetc, .ungetc = (void*) gungetc, .eof = (void*) geof };
struct g_out g_stdout = { .putc = (void*) gputc, .flush = (void*) gflush };

static void ini_vecv(struct g_vec *v, uintptr_t type, uintptr_t rank, va_list xs) {
 intptr_t *shape = v->shape;
 v->ap = g_vm_data;
 v->typ = vec_class;
 v->type = type;
 v->rank = rank;
 while (rank--) *shape++ = va_arg(xs, uintptr_t); }


static void ini_vec(struct g_vec *v, uintptr_t type, uintptr_t rank, ...) {
  va_list xs;
  va_start(xs, rank);
  ini_vecv(v, type, rank, xs);
  va_end(xs); }

void ini_str(struct g_vec *s, uintptr_t len) {
  ini_vec((struct g_vec*) s, g_vect_char, 1, len); }

void ini_two(struct g_pair *w, intptr_t a, intptr_t b) {
  w->ap = g_vm_data; w->typ = two_class; w->a = a; w->b = b; }
void ini_tab(struct g_tab *t, uintptr_t len, uintptr_t cap, struct g_kvs**tab) {
  t->ap = g_vm_data; t->typ = tbl_class; t->len = len; t->cap = cap; t->tab = tab; }
void ini_anon(struct g_atom *y, uintptr_t code) {
  y->ap = g_vm_data; y->typ = sym_class; y->nom = 0; y->code = code; }

static bool g_strp(intptr_t _) {
 return even(_) && typ(_) == vec_class && vec_strp((struct g_vec*)_); }


enum g_status g_fin(struct g *f) {
  enum g_status s = g_code_of(f);
  f = g_core_of(f);
  if (f) f->free(f, f->pool);
  return s; }

struct g_tag *ttag(union u *k) {
  while (k->x) k++;
  return (struct g_tag*) k; }

static size_t const vt_size[] = {
  [g_vect_u8]  = 1, [g_vect_i8]  = 1, [g_vect_f8]  = 1,
  [g_vect_u16] = 2, [g_vect_i16] = 2, [g_vect_f16] = 2,
  [g_vect_u32] = 4, [g_vect_i32] = 4, [g_vect_f32] = 4,
  [g_vect_u64] = 8, [g_vect_i64] = 8, [g_vect_f64] = 8, };


uintptr_t g_vec_bytes(struct g_vec *v) {
 intptr_t len = vt_size[v->type],
          rank = v->rank,
          *shape = v->shape;
 while (rank--) len *= *shape++;
 return sizeof(struct g_vec) + v->rank * sizeof(g_num) + len; }

static g_noinline struct g *gx_(struct g *f, int i, int j) {
 f = g_have(f, Width(struct g_pair));
 if (g_ok(f)) {
  struct g_pair *p = bump(f, Width(struct g_pair));
  ini_two(p, f->sp[i], f->sp[j]);
  *++f->sp = (intptr_t) p; }
 return f; }

struct g *gxl(struct g *f) { return gx_(f, 0, 1); }
struct g *gxr(struct g *f) { return gx_(f, 1, 0); }

static g_vm(g_vm_uncurry) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

g_vm(g_vm_curry) {
 union u *k = (union u*) Hp, *j = k;
 uintptr_t n = ggetnum(Ip[1].x);
 size_t S = 3 + Width(struct g_tag);

 if (n == 2) {
  Have(S);
  j[0].ap = g_vm_uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k; }

 else {
  S += 2;
  Have(S);
  j += 2;
  k[0].ap = g_vm_curry;
  k[1].x = gputnum(n - 1);
  j[0].ap = g_vm_uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k; }

 Hp += S;
 Ip = cell(*Sp);
 Sp[0] = word(k);
 return Continue(); }

g_vm(g_vm_jump) {
 Ip = Ip[1].m;
 return Continue(); }

g_vm(g_vm_cond) {
 Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2;
 return Continue(); }

// load instructions
//
// push an g_vm_pushkediate value
g_vm(g_vm_pushk) {
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 Ip += 2;
 return Continue(); }

g_vm(g_vm_eval) { return
 Ip++,
 Pack(f),
 f = g_c0(f, g_vm_jump),
 g_ok(f) ? (Unpack(f), Continue()) : f; }

g_vm(g_vm_data) {
 intptr_t x = word(Ip);
 return Sp += 1,
        Ip = cell(Sp[0]),
        Sp[0] = x,
        Continue(); }


// push a value from the stack
g_vm(g_vm_pushr) {
 Have1();
 return Sp[-1] = Sp[ggetnum(Ip[1].x)],
        Sp -= 1,
        Ip += 2,
        Continue(); }

// call and return
// apply function to one argument
g_vm(g_vm_ap) {
 union u *k;
 if (odd(Sp[1])) Ip++, Sp++;
 else k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 return Continue(); }

// tail call
g_vm(g_vm_tap) {
 intptr_t x = Sp[0], j = Sp[1];
 Sp += ggetnum(Ip[1].x) + 1;
 if (even(j)) Ip = cell(j), Sp[0] = x;
 else Sp += 1, Ip = cell(Sp[0]), Sp[0] = j;
 return Continue(); }

// apply to multiple arguments
g_vm(g_vm_apn) {
 size_t n = ggetnum(Ip[1].x);
 union u*ra = Ip + 2; // return address
 // this instruction is only emitted when the callee is known to be a function
 // so putting a value off the stack into Ip is safe. the +2 is cause we leave
 // the currying instruction in there... should be skipped in compiler instead FIXME
 return
  Ip = cell(Sp[n]) + 2,
  Sp[n] = word(ra), // store return address
  Continue(); }

// tail call
g_vm(g_vm_tapn) {
 size_t n = ggetnum(Ip[1].x),
        r = ggetnum(Ip[2].x);
 Ip = cell(Sp[n]) + 2;
 g_num *o = Sp;
 for (Sp += r + 1; n--; Sp[n] = o[n]);
 return Continue(); }

// return
g_vm(g_vm_ret) {
 g_num n = ggetnum(Ip[1].x) + 1;
 Ip = cell(Sp[n]);
 Sp[n] = Sp[0];
 Sp += n;
 return Continue(); }

g_vm(g_vm_ret0) { return
 Ip = cell(Sp[1]),
 Sp[1] = Sp[0],
 Sp += 1,
 Continue(); }

#define opf(nom, op) g_vm(nom) {\
 intptr_t a = ggetnum(Sp[0]), b = ggetnum(Sp[1]);\
 *++Sp = gputnum(a op b);\
 return Ip++, Continue(); }
opf(g_vm_bsr, >>)
opf(g_vm_bsl, <<)
opf(g_vm_mul, *)
#define op0f(nom, op) g_vm(nom) {\
 intptr_t a = ggetnum(Sp[0]), b = ggetnum(Sp[1]);\
 *++Sp = b == 0 ? g_nil : gputnum(a op b);\
 return Ip++, Continue(); }

op0f(g_vm_quot, /)
op0f(g_vm_rem, %)
#define op(nom, n, x) g_vm(nom) { intptr_t _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
op(g_vm_add, 2, (Sp[0]+Sp[1]-1)|1)
op(g_vm_sub, 2, (Sp[0]-Sp[1])|1)
op(g_vm_eq, 2, eql(f, Sp[0], Sp[1]) ? gputnum(-1) : g_nil)
op(g_vm_lt, 2, Sp[0] < Sp[1] ? gputnum(-1) : g_nil)
op(g_vm_le, 2, Sp[0] <= Sp[1] ? gputnum(-1) : g_nil)
op(g_vm_gt, 2, Sp[0] > Sp[1] ? gputnum(-1) : g_nil)
op(g_vm_ge, 2, Sp[0] >= Sp[1] ? gputnum(-1) : g_nil)
op(g_vm_bnot, 1, ~Sp[0] | 1)
op(g_vm_band, 2, (Sp[0] & Sp[1]) | 1)
op(g_vm_bor, 2, (Sp[0] | Sp[1]) | 1)
op(g_vm_bxor, 2, (Sp[0] ^ Sp[1]) | 1)
op(g_vm_nump, 1, odd(Sp[0]) ? gputnum(-1) : g_nil)
g_vm(g_vm_gensym) {
 if (g_strp(Sp[0])) return Ap(g_vm_nomsym, f);
 uintptr_t const req = Width(struct g_atom) - 2;
 Have(req);
 struct g_atom *y = (struct g_atom*) Hp;
 Hp += req;
 ini_anon(y, g_clock());
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }

g_vm(g_vm_symnom) {
 intptr_t y = Sp[0];
 y = symp(y) && sym(y)->nom ? word(sym(y)->nom) : g_nil;
 Sp[0] = y;
 Ip += 1;
 return Continue(); }

g_vm(g_vm_symp) { return
 Sp[0] = symp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_tabp) { return
 Sp[0] = tabp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_slen) { return
 Sp[0] = g_strp(Sp[0]) ? gputnum(len(Sp[0])) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_ssub) {
 if (!g_strp(Sp[0])) Sp[2] = g_nil;
 else {
  struct g_vec*s = ((struct g_vec*)Sp[0]), *t;
  intptr_t i = odd(Sp[1]) ? ggetnum(Sp[1]) : 0,
           j = odd(Sp[2]) ? ggetnum(Sp[2]) : 0;
  i = MAX(i, 0);
  i = MIN(i, (intptr_t) len(s));
  j = MAX(j, i);
  j = MIN(j, (intptr_t) len(s));
  if (i == j) Sp[2] = g_nil;
  else {
   size_t req = str_type_width + b2w(j - i);
   Have(req);
   t = (struct g_vec*) Hp;
   Hp += req;
   ini_str(t, j - i);
   memcpy(txt(t), txt(s) + i, j - i);
   Sp[2] = (intptr_t) t; } }
 return Ip += 1,
        Sp += 2,
        Continue(); }

g_vm(g_vm_sget) {
 if (!g_strp(Sp[0])) Sp[1] = g_nil;
 else {
  struct g_vec *s = (struct g_vec*) Sp[0];
  intptr_t i = ggetnum(Sp[1]);
  i = MIN(i, len(s) - 1);
  i = MAX(i, 0);
  Sp[1] = gputnum(txt(s)[i]); }
 return Ip += 1,
        Sp += 1,
        Continue(); }

g_vm(g_vm_scat) {
 intptr_t a = Sp[0], b = Sp[1];
 if (!g_strp(a)) Sp += 1;
 else if (!g_strp(b)) Sp[1] = a, Sp += 1;
 else {
  struct g_vec
   *x = vec(a),
   *y = vec(b),
   *z;
  uintptr_t
   len = len(x) + len(y),
   req = str_type_width + b2w(len);
  Have(req);
  z = (struct g_vec*) Hp;
  Hp += req;
  ini_str(z, len);
  memcpy(txt(z), txt(x), len(x));
  memcpy(txt(z) + len(x), txt(y), len(y));
  Sp[1] = word(z); }
 return Ip++, Continue(); }

g_vm(g_vm_strp) { return
 Sp[0] = g_strp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_car) { return
 Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0],
 Ip++,
 Continue(); }

g_vm(g_vm_cdr) { return
 Sp[0] = twop(Sp[0]) ? B(Sp[0]) : g_nil,
 Ip++,
 Continue(); }

g_vm(g_vm_cons) {
 Have(Width(struct g_pair));
 struct g_pair *w = (struct g_pair*) Hp;
 Hp += Width(struct g_pair);
 ini_two(w, Sp[0], Sp[1]);
 *++Sp = word(w);
 Ip++;
 return Continue(); }

g_vm(g_vm_twop) { return
 Sp[0] = twop(Sp[0]) ? gputnum(-1) : g_nil,
 Ip++,
 Continue(); }

static g_noinline bool eql_cont(struct g *f, intptr_t a, intptr_t b) {
 if (!(even(a | b) && cell(a)->ap == g_vm_data && cell(b)->ap == g_vm_data && typ(a) == typ(b))) return false;
 intptr_t t = typ(a);
 // FIXME could overflow the stack -- use off pool for this
 if (t == two_class) return eql(f, A(a), A(b)) && eql(f, B(a), B(b));
 if (t == vec_class) return 0 == memcmp(vec(a), vec(b), g_vec_bytes(vec(a)));
 return false; }

g_inline bool eql(struct g *f, intptr_t a, intptr_t b) {
 return a == b || eql_cont(f, a, b); }

static g_vm(g_vm_clock) { return
 Sp[0] = gputnum(g_clock()),
 Ip += 1,
 Continue(); }

static g_vm(g_vm_nilp) { return
 Sp[0] = nilp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

g_vm(g_vm_putc) { return
 gputc(f, ggetnum(*Sp)),
 Ip += 1,
 Continue(); }

g_vm(g_vm_info) {
  size_t const req = 4 * Width(struct g_pair);
  Have(req);
  struct g_pair *si = (struct g_pair*) Hp;
  Hp += req;
  Sp[0] = word(si);
  ini_two(si, gputnum(f), word(si + 1));
  ini_two(si + 1, gputnum(f->len), word(si + 2));
  ini_two(si + 2, gputnum(Hp - ptr(f)), word(si + 3));
  ini_two(si + 3, gputnum(ptr(f) + f->len - Sp), g_nil);
  Ip += 1;
  return Continue(); }

static g_vm(gputs) {
  if (g_strp(Sp[0])) {
    struct g_vec *s = vec(Sp[0]);
    for (intptr_t i = 0; i < len(s);) gputc(f, txt(s)[i++]);
    gflush(f); }
  return Ip += 1,
         Continue(); }

static g_vm(g_vm_getc) {
  Pack(f);
  int i = ggetc(f);
  Unpack(f);
  Sp[0] = gputnum(i);
  Ip += 1;
  return Continue(); }

#define S1(i) {{i}, {g_vm_ret0}}
#define S2(i) {{g_vm_curry},{.x=gputnum(2)},{i}, {g_vm_ret0}}
#define S3(i) {{g_vm_curry},{.x=gputnum(3)},{i}, {g_vm_ret0}}
#define bifs(_) \
 _(bif_clock, "clock", S1(g_vm_clock)) _(bif_addr, "vminfo", S1(g_vm_info))\
 _(bif_add, "+", S2(g_vm_add)) _(bif_sub, "-", S2(g_vm_sub)) _(bif_mul, "*", S2(g_vm_mul)) _(bif_quot, "/", S2(g_vm_quot)) _(bif_rem, "%", S2(g_vm_rem)) \
 _(bif_lt, "<", S2(g_vm_lt))  _(bif_le, "<=", S2(g_vm_le)) _(bif_eq, "=", S2(g_vm_eq)) _(bif_ge, ">=", S2(g_vm_ge))  _(bif_gt, ">", S2(g_vm_gt)) \
 _(bif_bnot, "~", S1(g_vm_bnot)) _(bif_bsl, "<<", S2(g_vm_bsl)) _(bif_bsr, ">>", S2(g_vm_bsr))\
 _(bif_band, "&", S2(g_vm_band)) _(bif_bor, "|", S2(g_vm_bor)) _(bif_bxor, "^", S2(g_vm_bxor))\
 _(bif_cons, "X", S2(g_vm_cons)) _(bif_g_vm_car, "A", S1(g_vm_car)) _(bif_g_vm_cdr, "B", S1(g_vm_cdr)) \
 _(bif_sget, "sget", S2(g_vm_sget)) _(bif_ssub, "ssub", S3(g_vm_ssub)) _(bif_slen, "slen", S1(g_vm_slen)) _(bif_scat, "scat", S2(g_vm_scat)) \
 _(bif_g_vm_dot, ".", S1(g_vm_dot)) _(bif_read, "read", S1(g_vm_read)) _(bif_getc, "getc", S1(g_vm_getc))\
 _(bif_putc, "putc", S1(g_vm_putc)) _(bif_prn, "putn", S2(g_vm_putn)) _(bif_puts, "puts", S1(gputs))\
 _(bif_sym, "sym", S1(g_vm_gensym)) _(bif_nom, "nom", S1(g_vm_symnom)) _(bif_thd, "thd", S1(thda)) _(bif_g_vm_peek, "peek", S1(g_vm_peek)) _(bif_g_vm_poke, "poke", S2(g_vm_poke)) _(bif_trim, "trim", S1(trim)) _(bif_g_vm_seek, "seek", S2(g_vm_seek)) \
 _(bif_tabnew, "tnew", S1(g_vm_tnew)) _(bif_tabkeys, "tkeys", S1(g_vm_tkeys)) _(bif_tablen, "tlen", S1(g_vm_tlen)) _(bif_tset, "tset", S3(g_vm_tset)) _(bif_tabget, "tget", S3(g_vm_tget)) _(bif_tabdel, "tdel", S3(g_vm_tdel))\
 _(bif_twop, "twop", S1(g_vm_twop)) _(bif_strp, "strp", S1(g_vm_strp)) _(bif_symp, "symp", S1(g_vm_symp)) _(bif_tabp, "tabp", S1(g_vm_tabp)) _(bif_nump, "nump", S1(g_vm_nump)) _(bif_nilp, "nilp", S1(g_vm_nilp))\
 _(bif_ev, "ev", S1(g_vm_eval))
#define built_in_function(n, _, d) static union u const n[] = d;
bifs(built_in_function);
#define insts(_) _(g_vm_freev) _(g_vm_ret) _(g_vm_ap) _(g_vm_tap) _(g_vm_apn) _(g_vm_tapn) _(g_vm_jump) _(g_vm_cond) _(g_vm_pushr) _(g_vm_pushk) _(g_vm_drop1) _(g_vm_curry) _(g_vm_defglob) _(g_vm_lazyb) _(g_vm_ret0)
#define biff(b, n, _) {n, (intptr_t) b},
#define i_entry(i) {#i, (intptr_t) i},

static g_vm(g_vm_yield) { return Pack(f), f; }
static union u yield[] = { {g_vm_yield} };
static struct g_def const g_defs0[] = { bifs(biff) insts(i_entry) {0}};
g_noinline struct g *g_ini_m(
 void *(*ma)(struct g*, size_t),
 void (*fr)(struct g*, void*))
{
 uintptr_t const len0 = 1 << 10;
 struct g *f = ma(NULL, 2 * len0 * sizeof(g_word));
 if (f == NULL) return encode(f, g_status_oom);
 memset(f, 0, sizeof(struct g));
 f->len = len0;
 f->pool = (void*) f;
 f->malloc = ma;
 f->free = fr;
 f->hp = f->end;
 f->sp = (intptr_t*) f + len0;
 f->ip = yield;
 f->t0 = g_clock(); // this goes right before first allocation so gc always sees initialized t0
 f = g_tnew(g_tnew(f)); // dict and macro tables
 f = g_intern(g_strof(g_intern(g_strof(f, "\\")), "`"));
 if (!g_ok(f)) return f;
 f->quote = nom(g_pop1(f));
 f->lambda = nom(g_pop1(f));
 f->macro = tbl(g_pop1(f));
 f->dict = tbl(g_pop1(f));
 struct g_def defs[] = {
  {"globals", (intptr_t) f->dict, },
  {"macros", (intptr_t) f->macro, },
  {0}, };
 f = g_defs(f, defs);
 f = g_defs(f, g_defs0);
 return f; }

g_vm(g_vm_seek) { return
 Sp[1] = word(cell(Sp[1]) + ggetnum(Sp[0])),
 Sp += 1,
 Ip += 1,
 Continue(); }

g_vm(g_vm_peek) { return
 Sp[0] = cell(Sp[0])->x,
 Ip += 1,
 Continue(); }

g_vm(g_vm_poke) { return
 cell(Sp[1])->x = Sp[0],
 Sp += 1,
 Ip += 1,
 Continue(); }

g_vm(thda) {
 size_t n = ggetnum(Sp[0]);
 Have(n + Width(struct g_tag));
 union u *k = (union u*) Hp;
 struct g_tag *t = (struct g_tag*) (k + n);
 Hp += n + Width(struct g_tag);
 t->null = NULL;
 t->head = k;
 memset(k, -1, n * sizeof(g_word));
 Sp[0] = word(k);
 Ip += 1;
 return Continue(); }

g_vm(trim) {
 union u *k = cell(Sp[0]);
 return ttag(k)->head = k,
        Ip += 1,
        Continue(); }

static struct g *g_vec0(struct g*f, uintptr_t type, uintptr_t rank, ...) {
 uintptr_t len = vt_size[type];
 va_list xs;
 va_start(xs, rank);
 for (uintptr_t i = rank; i--; len *= va_arg(xs, uintptr_t));
 va_end(xs);
 uintptr_t nbytes = sizeof(struct g_vec) + rank * sizeof(g_word) + len,
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
 f = g_vec0(f, g_vect_char, 1, len);
 if (g_ok(f)) memcpy(txt(f->sp[0]), cs, len);
 return f; }

static void *g_malloc(struct g*f, size_t n) { return malloc(n); }
static void g_free(struct g*f, void *x) { free(x); }
struct g *g_ini(void) { return g_ini_m(g_malloc, g_free); }

struct g *g_def1(struct g*f, char const *s) {
 if (!g_ok(f)) return f;
 struct g_def d[] = {{s, g_pop1(f)}, {0}};
 return g_defs(f, d); }

struct g *g_defs(struct g*f, struct g_def const*defs) {
 if (!g_ok(f)) return f;
 f = g_push(f, 1, f->dict);
 for (int n = 0; defs[n].n; n++)
  f = g_tput(g_intern(g_strof(g_push(f, 1, defs[n].x), defs[n].n)));
 if (g_ok(f)) f->sp++;
 return f; }
