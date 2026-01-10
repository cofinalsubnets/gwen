#include "g.h"
#include <stdarg.h>
#ifndef EOF
#define EOF -1
#endif

// some libc functions we use
int
 memcmp(void const*, void const*, size_t);
void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);

#define g_vect_char g_vect_u8
#define avail(f) ((f->sp-f->hp))
static struct g
  *g_have(struct g*, intptr_t),
  *please(struct g*, uintptr_t);

static struct g_in g_stdin = { .getc = (void*) ggetc, .ungetc = (void*) gungetc, .eof = (void*) geof };
static struct g_out g_stdout = { .putc = (void*) gputc, .flush = (void*) gflush };

enum g_vec_type {
 g_vect_u8,  g_vect_i8,
 g_vect_u16, g_vect_i16,
 g_vect_u32, g_vect_i32,
 g_vect_u64, g_vect_i64,
 g_vect_f8,  g_vect_f16,
 g_vect_f32, g_vect_f64, };

static struct g *g_c0(struct g *f, g_vm_t *y);

static g_inline struct g *encode(struct g*f, enum g_status s) {
  return (struct g*) ((uintptr_t) f | s); }

static g_vm_t g_vm_data;
enum class { two_class, vec_class, sym_class, tbl_class, };
static void inivecv(struct g_vec *v, uintptr_t type, uintptr_t rank, va_list xs) {
 intptr_t *shape = v->shape;
 v->ap = g_vm_data;
 v->typ = vec_class;
 v->type = type;
 v->rank = rank;
 while (rank--) *shape++ = va_arg(xs, uintptr_t); }

static struct g_atom *g_intern_r(struct g*, struct g_vec*, struct g_atom**);

#define vlen(_)((struct g_vec*)(_))->shape[0]
#define vtxt(_) ((char*)(((struct g_vec*)(_))->shape+1))
#define len(_) vlen(_)
#define txt(_) vtxt(_)
#define avail(f) ((f->sp-f->hp))
static struct g
  *g_tput(struct g*),
  *g_intern(struct g*),
  *g_tnew(struct g*);
static struct g *g_have(struct g *f, intptr_t n) {
 return !g_ok(f) || avail(f) >= n ? f : please(f, n); }

static void inivec(struct g_vec *v, uintptr_t type, uintptr_t rank, ...) {
  va_list xs;
  va_start(xs, rank);
  inivecv(v, type, rank, xs);
  va_end(xs); }

static void inistr(struct g_vec *s, uintptr_t len) {
  inivec((struct g_vec*) s, g_vect_char, 1, len); }

struct g_pair {
  g_vm_t *ap;
  uintptr_t typ;
  intptr_t a, b; };

static void ini_two(struct g_pair *w, intptr_t a, intptr_t b) {
  w->ap = g_vm_data; w->typ = two_class; w->a = a; w->b = b; }
static void ini_tab(struct g_tab *t, uintptr_t len, uintptr_t cap, struct g_kvs**tab) {
  t->ap = g_vm_data; t->typ = tbl_class; t->len = len; t->cap = cap; t->tab = tab; }
static void ini_anon(struct g_atom *y, uintptr_t code) {
  y->ap = g_vm_data; y->typ = sym_class; y->nom = 0; y->code = code; }

#define odd(_) ((uintptr_t)(_)&1)
#define even(_) !odd(_)
#define cell(_) ((union u*)(_))
#define typ(_) cell(_)[1].typ
#if g_tco
#define g_status_yield g_status_ok
#else
#define g_status_yield g_status_eof
#endif
#define dict_of(_) (_)->dict
#define nilp(_) (word(_)==g_nil)
#define A(o) two(o)->a
#define B(o) two(o)->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define push1(f, _) (*--(f)->sp=(_))
static g_inline g_num *n2p(g_num n) {
  return (g_num*) n; }
static g_inline g_num p2n(g_num *p) {
  return (g_num) p; }
static g_inline struct g_vec *vec(g_num n) {
  return (struct g_vec*) n2p(n); }
static g_inline struct g_tab *tbl(g_num n) {
  return (struct g_tab*) n2p(n); }
static g_inline struct g_pair *two(g_num n) {
  return (struct g_pair*) n2p(n); }
static g_inline struct g_atom *sym(g_num n) {
  return (struct g_atom*) n2p(n); }
#define nom(_) sym(_)
#define ptr(_) ((g_num*)(_))
#define num(_) ((g_num)(_))
#define word(_) num(_)
#define datp(_) (cell(_)->ap==g_vm_data)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->root=&((struct g_root){(g_num*)(r),f->root})))
#define UM(f) (f->root=f->root->next)
#define mix ((uintptr_t)2708237354241864315)
#define within(a, b, c) (num(a)<=num(b)&&num(b)<num(c))
#define owns(f, x) within((intptr_t*)f, x, (intptr_t*)f + f->len)
_Static_assert(sizeof(union u) == sizeof(intptr_t));
_Static_assert(-1 >> 1 == -1, "sign extended shift");


#define str_type_width (Width(struct g_vec) + 1)

static g_inline bool twop(g_num _) { return even(_) && typ(_) == two_class; }
static g_inline bool tabp(g_num _) { return even(_) && typ(_) == tbl_class; }
static g_inline bool symp(g_num _) { return even(_) && typ(_) == sym_class; }
static g_inline bool nump(g_num _) { return odd(_); }
static g_inline bool vec_strp(struct g_vec *s) { return s->type == g_vect_char && s->rank == 1; }
static bool g_strp(intptr_t _) { return even(_) && typ(_) == vec_class && vec_strp((struct g_vec*)_); }


static struct g *g_symof(struct g *f, char const *nom) {
  return g_intern(g_strof(f, nom)); }

static struct g *g_intern(struct g*f) {
  f = g_have(f, Width(struct g_atom));
  if (g_ok(f)) f->sp[0] = (intptr_t) g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols);
  return f; }
static int gfputx(struct g*, struct g_out*, intptr_t);

static g_vm_t
  g_vm_info, g_vm_yieldk, g_vm_dot,
  g_vm_symnom, g_vm_read, g_vm_putc,
  g_vm_gensym, g_vm_twop, g_vm_nump, g_vm_symp, g_vm_strp, g_vm_tabp,
  g_vm_band, g_vm_bor, g_vm_bxor, g_vm_bsr, g_vm_bsl, g_vm_bnot,
  g_vm_ssub, g_vm_sget, g_vm_slen, g_vm_scat,
  g_vm_cons, g_vm_car, g_vm_cdr,
  g_vm_lt, g_vm_le, g_vm_eq, g_vm_gt, g_vm_ge,
  g_vm_tset, g_vm_tget, g_vm_tdel, g_vm_tnew, g_vm_tkeys, g_vm_tlen,
  g_vm_seek, g_vm_peek, g_vm_poke, trim, thda, g_vm_add, g_vm_sub, g_vm_mul, g_vm_quot, g_vm_rem,
  g_vm_defglob, g_vm_drop1, g_vm_pushk, g_vm_pushr,
  g_vm_freev, g_vm_eval,
  g_vm_cond, g_vm_jump, g_vm_ap, g_vm_tap, g_vm_apn, g_vm_tapn, g_vm_ret, g_vm_lazyb;

enum g_status g_fin(struct g *f) {
  enum g_status s = g_code_of(f);
  f = g_core_of(f);
  if (f) f->free(f, f->pool);
  return s; }

static struct g *g_eval(struct g *f) {
 f = g_c0(f, g_vm_yieldk);
#if g_tco
 return g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f; }
#else
 while (g_ok(f)) f = f->ip->ap(f);
 return g_code_of(f) == g_status_eof ? g_core_of(f) : f; }
#endif

static struct g_tag { union u *null, *head, end[]; } *ttag(union u *k) {
  while (k->x) k++;
  return (struct g_tag*) k; }

static size_t const vt_size[] = {
  [g_vect_u8]  = 1, [g_vect_i8]  = 1, [g_vect_f8]  = 1,
  [g_vect_u16] = 2, [g_vect_i16] = 2, [g_vect_f16] = 2,
  [g_vect_u32] = 4, [g_vect_i32] = 4, [g_vect_f32] = 4,
  [g_vect_u64] = 8, [g_vect_i64] = 8, [g_vect_f64] = 8, };

// function state using this type
typedef struct env {
  struct env *par;
  // these parameters represent stack state at a point in compile process
  intptr_t
    args,  // list  // function positional arguments
    imps,  // list  // closure variables
    stack, // list  // current values on stack
    lams,  // alist // known function definitions
    alts, ends; } env;

static bool eql(struct g*, intptr_t, intptr_t);
static intptr_t
  g_gc_cp(struct g*,intptr_t,intptr_t*,intptr_t*),
  g_tget(struct g*,intptr_t, struct g_tab*,intptr_t);
#define cp(...) g_gc_cp(__VA_ARGS__)
static uintptr_t g_hash(struct g*, g_num);

static g_inline void *bump(struct g *f, uintptr_t n) {
 void *x = f->hp;
 f->hp += n;
 return x; }

static struct g *grbufn(struct g *f) {
 f = g_have(f, str_type_width + 2);
 if (g_ok(f)) {
  union u *k = bump(f, str_type_width + 1);
  *--f->sp = word(k);
  struct g_vec *o = (struct g_vec*) k;
  inistr(o, sizeof(intptr_t)); }
 return f; }

static struct g *grbufg(struct g *f) {
 size_t len = len(f->sp[0]),
        req = str_type_width + 2 * b2w(len);
 f = g_have(f, req);
 if (g_ok(f)) {
  struct g_vec *o = bump(f, req);
  inistr(o, 2 * len);
  memcpy(txt(o), txt(f->sp[0]), len);
  f->sp[0] = (g_num) o; }
 return f; }


////
/// " the parser "
//
//
// get the next significant character from the stream
static int g_r_getc(struct g*f, struct g_in *i) {
 for (int c;;) switch (c = i->getc(f, i)) {
  default: return c;
  case '#': case ';':
   while (!i->eof(f, i) && (c = i->getc(f, i)) != '\n' && c != '\r');
  case 0: case ' ': case '\t': case '\n': case '\r': case '\f':
   continue; } }

static struct g *g_reads(struct g*, struct g_in*);
long strtol(char const*restrict, char**restrict, int);
static struct g *g_read1(struct g*f, struct g_in* i) {
 if (!g_ok(f)) return f;
 int c = g_r_getc(f, i);
 switch (c) {
  case '(':  return g_reads(f, i);
  case ')': case EOF:  return encode(f, g_status_eof);
  case '\'': return
   f = gxr(g_push(g_read1(f, i), 1, g_nil)),
   g_ok(f) ? gxl(g_push(f, 1, f->quote)) : f;
  case '"': {
   size_t n = 0;
   f = grbufn(f);
   for (size_t lim = sizeof(g_word); g_ok(f); f = grbufg(f), lim *= 2)
    for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
     if ((c = i->getc(f, i)) == EOF || c == '"' ||
         (c == '\\' && (c =i->getc(f, i)) == EOF))
      return len(b) = n, f;
   return f; } }

 uintptr_t n = 1, lim = sizeof(intptr_t);
 f = grbufn(f);
 if (g_ok(f))
  for (txt(f->sp[0])[0] = c; g_ok(f); f = grbufg(f), lim *= 2)
   for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
    switch (c = i->getc(f, i)) {
     default: continue;
     case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
     case '(': case ')': case '"': case '\'': case 0 : case EOF:
      i->ungetc(f, c, i);
      len(b) = n;
      txt(b)[n] = 0; // zero terminate for strtol ; n < lim so this is safe
      char *e;
      long j = strtol(txt(b), &e, 0);
      if (*e == 0) f->sp[0] = gputnum(j);
      else f = g_intern(f);
      return f; }
 return f; }

static struct g *g_reads(struct g *f, struct g_in* i) {
 intptr_t n = 0;
 for (int c; g_ok(f); n++) {
  c = g_r_getc(f, i);
  if (c == EOF || c == ')') break;
  i->ungetc(f, c, i);
  f = g_read1(f, i); }
 for (f = g_push(f, 1, g_nil); n--; f = gxr(f));
 return f; }

static g_vm(g_vm_dot) { return
 gfputx(f, &g_stdout, Sp[0]),
 Ip += 1,
 Continue(); }

static struct g *g_pushr(struct g *f, uintptr_t m, uintptr_t n, va_list xs) {
 if (n == m) return please(f, m);
 intptr_t x = va_arg(xs, intptr_t);
 MM(f, &x);
 f = g_pushr(f, m, n + 1, xs);
 UM(f);
 if (g_ok(f)) *--f->sp = x;
 return f; }

static uintptr_t vec_bytes(struct g_vec *v) {
 intptr_t len = vt_size[v->type],
          rank = v->rank,
          *shape = v->shape;
 while (rank--) len *= *shape++;
 return sizeof(struct g_vec) + v->rank * sizeof(g_num) + len; }

static intptr_t assq(struct g *f, g_num l, g_num k) {
 for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
 return 0; }

static intptr_t memq(struct g *f, g_num l, g_num k) {
 for (; twop(l); l = B(l)) if (eql(f, k, A(l))) return l;
 return 0; }

static size_t llen(intptr_t l) {
 size_t n = 0;
 while (twop(l)) n++, l = B(l);
 return n; }

static struct g *append(struct g *f) {
 uintptr_t i = 0;
 for (intptr_t l; g_ok(f) && twop(f->sp[0]); i++)
  l = B(f->sp[0]),
  f->sp[0] = A(f->sp[0]),
  f = g_push(f, 1, l);
 if (!g_ok(f)) return f;
 if (i == 0) return f->sp++, f;
 if (g_ok(f)) f->sp[0] = f->sp[i + 1];
 while (i--) f = gxr(f);
 if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
 return f; }

static g_num reverse(struct g *f, g_num l) {
 g_num n = g_nil;
 for (g_num m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
 return n; }

static g_noinline struct g *enscope(struct g *f, struct env *par, intptr_t args, intptr_t imps) {
 f = g_push(f, 3, args, imps, par);
 uintptr_t n = Width(env);
 f = g_have(f, n + Width(struct g_tag) + 1);
 if (g_ok(f)) {
  union u *k = bump(f, n + Width(struct g_tag));
  *--f->sp = (intptr_t) k;
  struct g_tag *t = (struct g_tag*) (k + n);
  t->null = NULL;
  t->head = k;
  struct env *c = (struct env*) k;
  c->stack = c->alts = c->ends = c->lams = g_nil;
  c->args = f->sp[1],
  c->imps = f->sp[2],
  c->par = (struct env*) f->sp[3];
  f->sp[3] = (intptr_t) c,
  f->sp += 3; }
 return f; }

static struct g *gx2(struct g *f, intptr_t a, intptr_t b) {
 return gxl(g_push(f, 2, a, b)); }

static g_noinline struct g *gx_(struct g *f, int i, int j) {
 f = g_have(f, Width(struct g_pair));
 if (g_ok(f)) {
  struct g_pair *p = bump(f, Width(struct g_pair));
  ini_two(p, f->sp[i], f->sp[j]);
  *++f->sp = (intptr_t) p; }
 return f; }

struct g *gxl(struct g *f) { return gx_(f, 0, 1); }
struct g *gxr(struct g *f) { return gx_(f, 1, 0); }

#define Ana(n, ...) struct g *n(struct g *f, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct g *n(struct g *f, struct env **c, size_t m, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, g_c0_if, g_c0_let, g_c0_apargs;
static cata pull, g_c1_i, g_c1_ix, g_c1var, g_c1ap, g_c1_yield, g_c1_ret;

// generic instruction ana handlers
static struct g *g_c0_ix(struct g *f, g_vm_t *i, intptr_t x) {
 return g_push(f, 3, g_c1_ix, i, x); }

#define g_pop1(f) (*(f)->sp++)
// don't inline this so callers can tail call optimize
static g_noinline struct g *g_c0(struct g *f, g_vm_t *y) {
 f = enscope(f, (struct env*) g_nil, g_nil, g_nil);
 if (!g_ok(f)) return f;
 struct env *c = (struct env*) ptr(g_pop1(f));
 g_num x = f->sp[0];
 f->sp[0] = (g_num) g_c1_yield;
 avec(f, c,
  avec(f, x, f = g_c0_ix(f, y, word(f->ip))),
  f = analyze(f, &c, x),
  f = pull(f, &c, 0));
 return f; }

static g_noinline g_vm(g_vm_gc, uintptr_t n) {
  Pack(f);
  f = please(f, n);
  if (g_ok(f)) return Unpack(f), Continue();
  return f; }

#define Have1() if (Sp == Hp) return Ap(g_vm_gc, f, 1)
#define Have(n) if (Sp < Hp + n) return Ap(g_vm_gc, f, n)
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

static g_vm(g_vm_jump) {
 Ip = Ip[1].m;
 return Continue(); }

static g_vm(g_vm_cond) {
 Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2;
 return Continue(); }

// load instructions
//
// push an g_vm_pushkediate value
static g_vm(g_vm_pushk) {
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 Ip += 2;
 return Continue(); }

static g_vm(g_vm_yieldk) { return
 Ip = Ip[1].m,
 Pack(f),
 encode(f, g_status_yield); }

static g_vm(g_vm_eval) { return
 Ip++,
 Pack(f),
 f = g_c0(f, g_vm_jump),
 g_ok(f) ? (Unpack(f), Continue()) : f; }

#define Kp (f->ip)
static Cata(g_c1_yield) {
 f = g_have(f, m + Width(struct g_tag));
 if (g_ok(f)) {
  union u *k = bump(f, m + Width(struct g_tag));
  struct g_tag *t = (struct g_tag*) (k + m);
  t->null = NULL;
  t->head = k; 
  memset(k, -1, m * sizeof(intptr_t));
  Kp = k + m; }
 return f; }

static Cata(g_c1_if_pop_exit) {
  f = pull(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(g_c1_if_pop_branch) {
 f = pull(f, c, m + 2);
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = g_vm_cond,
  Kp[1].x = A((*c)->alts);
 (*c)->alts = B((*c)->alts);
 return f; }

static Cata(g_c1_if_push_branch) {
 f = pull(f, c, m);
 f = gx2(f, (intptr_t) Kp, (*c)->alts);
 if (g_ok(f)) (*c)->alts = g_pop1(f);
 return f; }

static Cata(g_c1_if_push_exit) {
 f = pull(f, c, m);
 f = gx2(f, (intptr_t) Kp, (*c)->ends);
 if (g_ok(f)) (*c)->ends = g_pop1(f);
 return f; }

static Cata(g_c1_if_jump_out) {
 f = pull(f, c, m + 3);
 if (g_ok(f)) {
  union u *a = cell(A((*c)->ends));
  if (a->ap == g_vm_ret || a->ap == g_vm_tap)
   Kp = memcpy(Kp - 2, a, 2 * sizeof(g_num));
  else if (a->ap == g_vm_tapn)
   Kp = memcpy(Kp - 3, a, 3 * sizeof(g_num));
  else
   Kp -= 2,
   Kp[0].ap = g_vm_jump,
   Kp[1].x = (g_num) a; }
 return f; }

static Cata(g_c1ap) {
 f = pull(f, c, m + 1);
 if (g_ok(f)) {
  if (Kp[0].ap == g_vm_ret) Kp[0].ap = g_vm_tap;
  else Kp -= 1, Kp[0].ap = g_vm_ap; }
 return f; }

static Cata(g_c1apn) {
 g_num arity = g_pop1(f);
 f = pull(f, c, m + 2);
 if (g_ok(f)) {
  if (Kp[0].ap == g_vm_ret) Kp -= 1, Kp[0].ap = g_vm_tapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = g_vm_apn, Kp[1].x = arity; }
 return f; }

static Cata(g_c1_ix_, g_vm_t *i, g_num x) {
 avec(f, x, f = pull(f, c, m + 2));
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = i,
  Kp[1].x = x;
 return f; }

static Cata(g_c1var_2) {
 g_num var = g_pop1(f),
       stack = g_pop1(f),
       i = 0;
 while (twop(stack))
  if (eql(f, A(stack), var)) break;
  else stack = B(stack), i++;
 return g_c1_ix_(f, c, m, g_vm_pushr, gputnum(i)); }

// emit stack g_vm_pushrerence instruction
static Cata(g_c1var) {
 g_num l, v = g_pop1(f), // variable name
          i = llen(g_pop1(f)); // stack inset
 for (l = (*c)->imps; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
 for (l = (*c)->args; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
out:
 return g_c1_ix_(f, c, m, g_vm_pushr, gputnum(i)); }

static g_inline Cata(pull) {
 return ((cata*) g_pop1(f))(f, c, m); }

static Cata(g_c1_i) {
 g_vm_t *i = (g_vm_t*) g_pop1(f);
 f = pull(f, c, m + 1);
 if (g_ok(f)) Kp -= 1, Kp[0].ap = i;
 return f; }

static Cata(g_c1_ix) {
 g_vm_t *i = (g_vm_t*) g_pop1(f);
 g_num x = g_pop1(f);
 return g_c1_ix_(f, c, m, i, x); }

static Cata(g_c1_curry) {
 struct env *e = (struct env*) g_pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return ar == 1 ?  pull(f, c, m) :
  g_c1_ix_(f, c, m, g_vm_curry, gputnum(ar)) ; }

static Cata(g_c1_ret) {
 struct env *e = (struct env*) g_pop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return g_c1_ix_(f, c, m, g_vm_ret, gputnum(ar)); }

static struct g
 *g_c0_lambda(struct g*, struct env**, intptr_t, intptr_t),
 *g_c0_seq(struct g*, struct env**, intptr_t, intptr_t);

static g_noinline Ana(analyze) {
 if (!g_ok(f)) return f;
 // is it a variable?
 if (symp(x)) for (struct env *d = *c;; d = d->par) {
  if (nilp(d)) { // free variable?
   intptr_t y = g_tget(f, 0, dict_of(f), x);
   if (y) return g_c0_ix(f, g_vm_pushk, y);
   f = gx2(f, x, (*c)->imps);
   if (g_ok(f)) (*c)->imps = g_pop1(f),
                f = g_c0_ix(f, g_vm_freev, (*c)->imps);
   return f; }
  // defined as a function by a local let form?
  intptr_t y;
  if ((y = assq(f, d->lams, x))) return
   avec(f, y, f = g_c0_apargs(f, c, BB(y))),
   g_c0_ix(f, g_vm_lazyb, y);
  // non function definition from local let form?
  if (memq(f, d->stack, x)) return
   g_push(f, 3, g_c1var_2, x, d->stack);
  // closure or positional argument?
  if (memq(f, d->imps, x) || memq(f, d->args, x)) {
   if (*c != d) { // if we have found the variable in an enclosing scope then import it
    f = gx2(f, x, (*c)->imps);
    if (g_ok(f)) x = A((*c)->imps = g_pop1(f)); }
   return g_push(f, 3, g_c1var, x, (*c)->stack); } }
 // if it's not a variable and it's not a list (pair) then it evals to itself
 if (!twop(x)) return g_c0_ix(f, g_vm_pushk, x);
 intptr_t a = A(x), b = B(x);
 // singleton list?
 if (!twop(b)) return analyze(f, c, a); // value of first element
 // special form?
 if (symp(a) && nom(a)->nom && len(nom(a)->nom) == 1) switch (*txt(nom(a)->nom)) {
  case '`': return g_c0_ix(f, g_vm_pushk, !twop(b) ? b : A(b));
  case ':': return !twop(B(b)) ? analyze(f, c, A(b)) :
                                 g_c0_let(f, c, b);
  case ',': return g_c0_seq(f, c, A(b), B(b));
  case '\\': return f = g_c0_lambda(f, c, g_nil, b),
                    analyze(f, c, g_ok(f) ? g_pop1(f) : 0);
  case '?': return !twop(B(b)) ? analyze(f, c, A(b)) :
                                 g_c0_if(f, c, b); }
 // macro?
 intptr_t mac = g_tget(f, 0, f->macro, a);
 if (mac) return
  f = g_push(f, 5, b, g_nil ,f->quote, g_nil, mac),
  f = g_eval(gxr(gxl(gxr(gxl(f))))),
  analyze(f, c, g_ok(f) ? g_pop1(f) : 0);
 // application.
 avec(f, a, f = g_c0_apargs(f, c, b));
 return analyze(f, c, a); }

static struct g *g_c0_lambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp) {
 f = enscope(f, *c, exp, imps);
 if (!g_ok(f)) return f;
 struct env *d = (struct env*) g_pop1(f);
 MM(f, &d);
 intptr_t x = d->args;

 // push exp args onto stack
 if (!twop(x)) f = g_push(f, 2, g_nil, g_nil);
 else { // there's at least one argument
  MM(f, &x);
  int n = 0;
  while (twop(B(x))) f = g_push(f, 1, A(x)), n++, x = B(x);
  f = g_push(f, 1, g_nil);
  while (n--) f = gxr(f);
  UM(f);
  f = g_push(f, 1, A(x)); }

 union u*k, *ip;
 if (g_ok(f))
  exp = g_pop1(f),
  d->args = f->sp[0],
  f->sp[0] = word(g_c1_yield),
  avec(f, exp, f = g_push(f, 2, g_c1_ret, d)),
  f = analyze(f, &d, exp),
  f = g_push(f, 2, g_c1_curry, d),
  ip = f->ip,
  avec(f, ip, f = pull(f, &d, 0));

 if (g_ok(f))
  k = f->ip,
  ttag(k)->head = k,
  f->ip = ip,
  f = gx2(f, word(k), d->imps);

 return UM(f), f; }

static Ana(g_c0_if_r) { return
 avec(f, x, f =
  !twop(x) || !twop(B(x)) ?
    g_push(f, 1, g_c1_if_jump_out) :
    (f = g_c0_if_r(f, c, BB(x)),
     f = g_push(f, 2, g_c1_if_jump_out, g_c1_if_push_branch),
     f = analyze(f, c, AB(x)),
     g_push(f, 1, g_c1_if_pop_branch))),
 analyze(f, c, twop(x) ? A(x) : g_nil); }

static Ana(g_c0_if) { return
 avec(f, x, f = g_push(f, 1, g_c1_if_push_exit)),
 g_push(g_c0_if_r(f, c, x), 1, g_c1_if_pop_exit); }

static struct g *g_c0_seq(struct g*f, struct env**c, intptr_t a, intptr_t b) {
 if (g_ok(f) && twop(b)) avec(f, a, f = g_c0_seq(f, c, A(b), B(b)),
                                    f = g_push(f, 2, g_c1_i, g_vm_drop1));
 return analyze(f, c, a); }

static struct g *g_c0_apargsr(struct g *f, struct env**c, intptr_t x) {
 if (!twop(x)) return f;
 avec(f, x, f = g_c0_apargsr(f, c, B(x)),
            f = g_push(f, 1, g_c1ap));
 return analyze(f, c, A(x)); }

// evaluate function call arguments and apply
static struct g *g_c0_apargs(struct g *f, struct env **c, intptr_t x) {
 avec(f, x, f = gx2(f, g_nil, (*c)->stack));
 if (g_ok(f))
  (*c)->stack = g_pop1(f),
  f = g_c0_apargsr(f, c, x),
  (*c)->stack = B((*c)->stack);
 return f; }

static g_num ldels(struct g *f, g_num lam, g_num l) {
 if (!twop(l)) return g_nil;
 intptr_t m = ldels(f, lam, B(l));
 if (!assq(f, lam, A(l))) B(l) = m, m = l;
 return m; }

static bool lambp(struct g *f, g_num x) {
 if (!twop(x) || !symp(A(x))) return false;
 struct g_vec *n = sym(A(x))->nom;
 return n && len(n) == 1 && *txt(n) == '\\'; }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static struct g *g_c0_let(struct g *f, struct env **b, g_num exp) {
 struct g_root *mm = f->root;
#define forget() ((f)->root=(mm),f)
 MM(f, &exp);
 f = enscope(f, *b, (*b)->args, (*b)->imps);
 if (!g_ok(f)) return forget();
 struct env
  *q = (struct env*) g_pop1(f),
  **c = &q;
 // lots of variables :(
 g_num nom = g_nil, def = g_nil, lam = g_nil,
       v = g_nil, d = g_nil, e = g_nil;
 MM(f, &nom), MM(f, &def), MM(f, &lam);
 MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

 // collect vars and defs into two lists
 while (twop(exp) && twop(B(exp))) {
  for (d = A(exp), e = AB(exp); twop(d); e = g_pop1(f), d = A(d)) {
   f = gx2(f, e, g_nil);
   f = append(gx2(f, word(f->lambda), B(d)));
   if (!g_ok(f)) return forget(); }
  f = gx2(f, d, nom);
  f = gx2(f, e, def);
  if (!g_ok(f)) return forget();
  def = g_pop1(f), nom = g_pop1(f);
  // if it's a lambda compile it and record in lam list
  if (lambp(f, e)) {
   f = g_push(f, 2, d, lam);
   f = gxl(gxr(g_c0_lambda(f, c, g_nil, B(e))));
   if (!g_ok(f)) return forget();
   lam = g_pop1(f); }
  exp = BB(exp); }

 // if there's no body then evaluate the name of the last definition
 bool even = !twop(exp); // we check this again later to make global bindings at top level
 if (even) {
  f = gx2(f, A(nom), g_nil);
  if (!g_ok(f)) return forget();
  exp = g_pop1(f); }

 // find closures
 // for each function f with closure C(f)
 // for each function g with closure C(g)
 // if f in C(g) then C(g) include C(f)
 long j;
 do for (j = 0, d = lam; twop(d); d = B(d)) // for each bound function variable
  for (e = lam; twop(e); e = B(e)) // for each bound function variable
#define BBA(o) B(BA(o))
   if (d != e && memq(f, BBA(e), AA(d))) // if you need this function
    for (v = BBA(d); twop(v); v = B(v)) { // then you need its variables
     intptr_t vars = BBA(e), var = A(v);
     if (!memq(f, vars, var)) { // only add if it's not already there
      f = gx2(f, var, vars);
      if (!g_ok(f)) return forget();
      j++, BBA(e) = g_pop1(f); } }
 while (j);

 // now delete defined functions from the closure variable lists
 // they will be bound lazily when the function runs
 for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

 (*c)->lams = lam;
 f = append(gxl(g_push(f, 3, f->lambda, nom, exp)));

 if (!g_ok(f)) return forget();
 exp = g_pop1(f);

 size_t ll = llen(nom);
 f = ll > 1 ? g_push(f, 2, g_c1apn, gputnum(ll)) :
              g_push(f, 1, g_c1ap);
 f = gx2(f, g_nil, (*b)->stack); // push function stack rep
 (*b)->stack = g_pop1(f);
 nom = reverse(f, nom); // put in literal order
 for (v = nom; g_ok(f) && twop(B(v)); v = B(v)) { // push initial variable stack reps
  f = gx2(f, A(v), (*b)->stack);
  if (g_ok(f)) (*b)->stack = g_pop1(f); }


 nom = reverse(f, nom); // back to reverse order
 for (; twop(nom); nom = B(nom), def = B(def)) {
  if (lambp(f, A(def))) {
   d = assq(f, lam, A(nom));
   f = g_c0_lambda(f, c, BB(d), BA(def));
   if (!g_ok(f)) return forget();
   A(def) = B(d) = g_pop1(f); }
  if (even && nilp((*b)->args)) {
   f = g_c0_ix(f, g_vm_defglob, A(nom));
   if (!g_ok(f)) return forget(); }
  f = analyze(f, b, A(def));
  (*b)->stack = B((*b)->stack); }

 f = analyze(f, b, exp);
 return forget(); }


static g_vm(g_vm_defglob) {
 Have(3);
 Sp -= 3;
 struct g_tab *t = dict_of(f);
 g_num k = Ip[1].x,
       v = Sp[3];
 Sp[0] = k;
 Sp[1] = v;
 Sp[2] = (g_num) t;
 Pack(f);
 f = g_tput(f);
 return !g_ok(f) ? f :
  (Unpack(f),
   Sp += 1,
   Ip += 2,
   Continue()); }

static g_vm(g_vm_drop1) { return Ip++, Sp++, Continue(); }

static g_vm(g_vm_freev) {
 g_num
  y = Ip[1].x,
  v = g_tget(f, y, f->dict, y); // see if it's defined now...
 return Ip[0].ap = g_vm_pushk,
        Ip[1].x = v,
        Continue(); }

static g_vm(g_vm_lazyb) {
 intptr_t v = AB(Ip[1].x);
 return Ip[0].ap = g_vm_pushk,
        Ip[1].x = v,
        Continue(); }

static g_vm(g_vm_data) {
 intptr_t x = word(Ip);
 return Sp += 1,
        Ip = cell(Sp[0]),
        Sp[0] = x,
        Continue(); }


// push a value from the stack
static g_vm(g_vm_pushr) {
 Have1();
 return Sp[-1] = Sp[ggetnum(Ip[1].x)],
        Sp -= 1,
        Ip += 2,
        Continue(); }

// call and return
// apply function to one argument
static g_vm(g_vm_ap) {
 union u *k;
 if (odd(Sp[1])) Ip++, Sp++;
 else k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 return Continue(); }

// tail call
static g_vm(g_vm_tap) {
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

static struct g *g_tnew(struct g*f) {
 f = g_have(f, Width(struct g_tab) + 2);
 if (g_ok(f)) {
  struct g_tab *t = bump(f, Width(struct g_tab) + 1);
  *--f->sp = word(t);
  struct g_kvs **tab = (struct g_kvs**) (t + 1);
  tab[0] = 0, ini_tab(t, 0, 1, tab); }
 return f; }

// general g_hashing method...
static uintptr_t g_hash(struct g *f, intptr_t x) {
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
  uintptr_t len = vec_bytes(_), h = 2166136261;
  for (uint8_t *bs = _; len--; h ^= *bs++, h *= 16777619);
  return h; } }


// relies on table capacity being a power of 2
static g_inline uintptr_t index_of_key(struct g *f, struct g_tab *t, intptr_t k) {
 return (t->cap - 1) & g_hash(f, k); }

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

intptr_t g_tget(struct g *f, intptr_t zero, struct g_tab *t, intptr_t k) {
 uintptr_t i = index_of_key(f, t, k);
 struct g_kvs *e = t->tab[i];
 while (e && !eql(f, k, e->key)) e = e->next;
 return e ? e->val : zero; }

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


static g_vm(nomsym) {
 Have(Width(struct g_atom));
 struct g_atom *y;
 Pack(f);
 y = g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols),
 Unpack(f);
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }

static g_vm(g_vm_gensym) {
 if (g_strp(Sp[0])) return Ap(nomsym, f);
 uintptr_t const req = Width(struct g_atom) - 2;
 Have(req);
 struct g_atom *y = (struct g_atom*) Hp;
 Hp += req;
 ini_anon(y, g_clock());
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }

static g_vm(g_vm_symnom) {
 intptr_t y = Sp[0];
 y = symp(y) && sym(y)->nom ? word(sym(y)->nom) : g_nil;
 Sp[0] = y;
 Ip += 1;
 return Continue(); }

static g_vm(g_vm_symp) { return
 Sp[0] = symp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_tabp) { return
 Sp[0] = tabp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_slen) { return
 Sp[0] = g_strp(Sp[0]) ? gputnum(len(Sp[0])) : g_nil,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_ssub) {
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
   inistr(t, j - i);
   memcpy(txt(t), txt(s) + i, j - i);
   Sp[2] = (intptr_t) t; } }
 return Ip += 1,
        Sp += 2,
        Continue(); }

static g_vm(g_vm_sget) {
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

static g_vm(g_vm_scat) {
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
  inistr(z, len);
  memcpy(txt(z), txt(x), len(x));
  memcpy(txt(z) + len(x), txt(y), len(y));
  Sp[1] = word(z); }
 return Ip++, Continue(); }

static g_vm(g_vm_strp) { return
 Sp[0] = g_strp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_car) { return
 Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0],
 Ip++,
 Continue(); }

static g_vm(g_vm_cdr) { return
 Sp[0] = twop(Sp[0]) ? B(Sp[0]) : g_nil,
 Ip++,
 Continue(); }

static g_vm(g_vm_cons) {
 Have(Width(struct g_pair));
 struct g_pair *w = (struct g_pair*) Hp;
 ini_two(w, Sp[0], Sp[1]);
 Hp += Width(struct g_pair);
 Sp[1] = word(w);
 Sp++;
 Ip++;
 return Continue(); }

static g_vm(g_vm_twop) { return
 Sp[0] = twop(Sp[0]) ? gputnum(-1) : g_nil,
 Ip++,
 Continue(); }

static g_noinline bool eql_cont(struct g *f, intptr_t a, intptr_t b) {
 if (!(even(a | b) && cell(a)->ap == g_vm_data && cell(b)->ap == g_vm_data && typ(a) == typ(b))) return false;
 intptr_t t = typ(a);
 // FIXME could overflow the stack -- use off pool for this
 if (t == two_class) return eql(f, A(a), A(b)) && eql(f, B(a), B(b));
 if (t == vec_class) return 0 == memcmp(vec(a), vec(b), vec_bytes(vec(a)));
 return false; }

static g_inline bool eql(struct g *f, intptr_t a, intptr_t b) {
 return a == b || eql_cont(f, a, b); }

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
  else g->cp += b2w(vec_bytes((struct g_vec*) g->cp));
 return g; }

static g_noinline struct g *please(struct g *f, uintptr_t req0) {
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
   uintptr_t bytes = vec_bytes(src);
   struct g_vec *dst = bump(f, b2w(bytes));
   src->ap = memcpy(dst, src, bytes);
   return word(dst); } }

static g_vm(g_vm_clock) { return
 Sp[0] = gputnum(g_clock()),
 Ip += 1,
 Continue(); }

static g_vm(g_vm_nilp) { return
 Sp[0] = nilp(Sp[0]) ? gputnum(-1) : g_nil,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_putc) { return
 gputc(f, ggetnum(*Sp)),
 Ip += 1,
 Continue(); }

static g_vm(g_vm_info) {
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

static int g_putn(struct g *f, struct g_out *o, intptr_t n, uintptr_t base) {
 if (n < 0) o->putc(f, '-', o), n = -n;
 uintptr_t q = n / base, r = n % base;
 if (q) g_putn(f, o, q, base);
 return o->putc(f, g_digits[r], o); }
static g_vm(putn) {
  uintptr_t n = ggetnum(Sp[0]), b = ggetnum(Sp[1]);
  g_putn(f, &g_stdout, n, b);
  Sp[1] = Sp[0];
  Sp += 1;
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

static g_vm(g_vm_read) {
 Pack(f);
 f = g_read1(f, &g_stdin);
 if (g_code_of(f) == g_status_eof) return // no error but end of file
  f = g_core_of(f),
  Unpack(f),
  Ip += 1,
  Continue();
 f = gxl(f);
 if (!g_ok(f)) return f;
 return Unpack(f),
        Ip += 1,
        Continue(); }

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
 _(bif_putc, "putc", S1(g_vm_putc)) _(bif_prn, "putn", S2(putn)) _(bif_puts, "puts", S1(gputs))\
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
 f = g_symof(g_symof(f, "\\"), "`");
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



static int gfputx(struct g *f, struct g_out *o, intptr_t x) {
 if (odd(x))
  return gfprintf(f, o, "%d", (g_num) ggetnum(x));
 else if (!datp(x))
  return gfprintf(f, o, "#%lx", (long) x);
 int r = 0;

 if (typ(x) == two_class) {
  if (A(x) == word(f->quote) && twop(B(x))) return
   o->putc(f, '\'', o),
   gfputx(f, o, AB(x));
  for (o->putc(f, '(', o);; o->putc(f, ' ', o)) {
   gfputx(f, o, A(x));
   if (!twop(x = B(x))) return o->putc(f, ')', o); } }
 else if (typ(x) == tbl_class) {
   struct g_tab *t = tbl(x);
   return gfprintf(f, o, "#tab:%d/%d@%x", t->len, t->cap, x); }
 else if (typ(x) == sym_class) {
  struct g_vec * s = sym(x)->nom;
  if (s && vec_strp(s)) for (intptr_t i = 0; i < len(s); r = o->putc(f, txt(s)[i++], o));
  else r = gfprintf(f, o, "#sym@%x", (intptr_t) x);
  return r; }
 else {
  struct g_vec *v = vec(x);
  if (!vec_strp(v)) {
   intptr_t rank = v->rank, *shape = v->shape;
   r = gfprintf(f, o, "#vec@%x:%d.%d", (intptr_t) x, (intptr_t) v->type, (intptr_t) v->rank);
   for (intptr_t i = rank, *j = shape; i--; r = gfprintf(f, o, ".%d", (intptr_t) *j++)); }
  else {
   uintptr_t len = vlen(v);
   char *text = vtxt(v);
   o->putc(f, '"', o);
   for (char c; len--; o->putc(f, c, o))
    if ((c = *text++) == '\\' || c == '"') o->putc(f, '\\', o);
   r = o->putc(f, '"', o); }
  return r; } }

static g_vm(g_vm_seek) { return
 Sp[1] = word(cell(Sp[1]) + ggetnum(Sp[0])),
 Sp += 1,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_peek) { return
 Sp[0] = cell(Sp[0])->x,
 Ip += 1,
 Continue(); }

static g_vm(g_vm_poke) { return
 cell(Sp[1])->x = Sp[0],
 Sp += 1,
 Ip += 1,
 Continue(); }

static g_vm(thda) {
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

static g_vm(trim) {
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
  inivecv(v, type, rank, xs);
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

struct g *g_push(struct g *f, intptr_t m, ...) {
 if (!g_ok(f)) return f;
 va_list xs;
 va_start(xs, m);
 intptr_t n = 0;
 if (avail(f) < m) f = g_pushr(f, m, n, xs);
 else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, intptr_t));
 va_end(xs);
 return f; }

int gvfprintf(struct g*f, struct g_out*o, char const *fmt, va_list xs) {
 int c;
 while ((c = *fmt++)) {
  if (c != '%') o->putc(f, c, o);
  else pass: switch ((c = *fmt++)) {
   case 0: return c;
   case 'l': goto pass;
   case 'b': c = g_putn(f, o, va_arg(xs, uintptr_t), 2); continue;
   case 'o': c = g_putn(f, o, va_arg(xs, uintptr_t), 8); continue;
   case 'd': c = g_putn(f, o, va_arg(xs, uintptr_t), 10); continue;
   case 'x': c = g_putn(f, o, va_arg(xs, uintptr_t), 16); continue;
   default: c = o->putc(f, c, o); } }
 return c; }

int gfprintf(struct g *f, struct g_out *o, char const *fmt, ...) {
 va_list xs;
 va_start(xs, fmt);
 int r = gvfprintf(f, o, fmt, xs);
 va_end(xs);
 return r; }

struct ti { struct g_in in; char const *t; uintptr_t i; } ;
static int p_text_eof(struct g*f, struct g_in *j) {
 struct ti *i = (struct ti*) j;
 return !i->t[i->i]; }

static int p_text_getc(struct g*f, struct g_in *j) {
 struct ti *i = (struct ti*) j;
 char c = i->t[i->i];
 if (c) i->i++;
 return c ? c : EOF; }

static int p_text_ungetc(struct g*f, int _, struct g_in *j) {
 struct ti *i = (struct ti*) j;
 uintptr_t idx = i->i;
 return i->t[i->i = idx ? idx - 1 : idx]; }

g_noinline struct g *g_evals_(struct g*f, char const*s) {
 static char const *t = "((:(e a b)(? b(e(ev'ev(A b))(B b))a)e)0)";
 struct ti i = {{p_text_getc, p_text_ungetc, p_text_eof}, t, 0};
 f = g_eval(g_reads(f, (void*) &i));
 f = g_push(f, 3, g_nil, g_ok(f) ? f->quote : NULL, g_nil);
 i.t = s, i.i = 0;
 f = g_eval(gxr(gxl(gxr(gxl(g_reads(f, (void*) &i))))));
 if (g_ok(f)) f->sp++;
 return f; }
