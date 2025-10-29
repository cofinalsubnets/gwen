#include "g.h"
#include <stdarg.h>
static g_inline size_t b2w(size_t b) {
  size_t q = b / sizeof(intptr_t), r = b % sizeof(intptr_t);
  return q + (r ? 1 : 0); }
static g_vm_t data;
enum g_ty { g_ty_two, g_ty_str, g_ty_sym, g_ty_tbl, };
static g_inline void ini_vecv(struct g_vec *v, uintptr_t type, uintptr_t rank, va_list xs) {
  uintptr_t *shape = v->shape;
  v->ap = data;
  v->typ = g_ty_str;
  v->type = type;
  v->rank = rank;
  while (rank--) *shape++ = va_arg(xs, uintptr_t); }
static struct g_atom *g_intern_r(struct g *v, struct g_vec *b, struct g_atom **y);

static g_inline struct g *g_have(struct g *f, uintptr_t n);
static void ini_vec(struct g_vec *v, uintptr_t type, uintptr_t rank, ...) {
  va_list xs;
  va_start(xs, rank);
  ini_vecv(v, type, rank, xs);
  va_end(xs); }
static g_inline void ini_str(struct g_vec *s, uintptr_t len) { ini_vec((struct g_vec*) s, g_vt_char, 1, len); }
struct g_pair {
  g_vm_t *ap;
  uintptr_t typ;
  intptr_t a, b; };
static g_inline void ini_pair(struct g_pair *w, intptr_t a, intptr_t b) {
  w->ap = data; w->typ = g_ty_two; w->a = a; w->b = b; }
static g_inline void ini_table(struct g_table *t, uintptr_t len, uintptr_t cap, struct entry**tab) {
  t->ap = data; t->typ = g_ty_tbl; t->len = len; t->cap = cap; t->tab = tab; }
static g_inline void ini_sym(struct g_atom *y, struct g_vec *nom, uintptr_t code) {
  y->ap = data; y->typ = g_ty_sym; y->nom = nom; y->code = code; y->l = y->r = 0; }
static g_inline void ini_anon(struct g_atom *y, uintptr_t code) {
  y->ap = data; y->typ = g_ty_sym; y->nom = 0; y->code = code; }

static g_inline bool vec_strp(struct g_vec *s) { return s->type == g_vt_char && s->rank == 1; }
#define odd(_) ((uintptr_t)(_)&1)
#define even(_) !odd(_)
#define cell(_) ((union x*)(_))
#define typ(_) cell(_)[1].typ
g_inline bool g_strp(intptr_t _) { return even(_) && typ(_) == g_ty_str && vec_strp((struct g_vec*)_); }
#if g_tco
#define g_status_yield g_status_ok
#else
#define g_status_yield g_status_eof
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#endif
#define symp g_symp
#define dict_of(_) (_)->dict
#define encode(f, s) ((struct g*)((intptr_t)(f)|(s)))
#define nilp(_) (word(_)==g_nil)
#define A(o) two(o)->a
#define B(o) two(o)->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define push1(f, _) (*--(f)->sp=(_))
#define sym(_) ((struct g_atom*)(_))
#define two(_) ((struct g_pair*)(_))
#define tbl(_) ((struct g_table*)(_))
#define vec(_) ((struct g_vec*)(_))
#define ptr(_) ((intptr_t*)(_))
#define word(_) ((intptr_t)(_))
#define datp(_) (cell(_)->ap==data)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->safe=&((struct g_mem_root){(intptr_t*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define mix ((uintptr_t)2708237354241864315)
#define Got(n) (((uintptr_t)(Sp - Hp)) >= (n))
#define Get(n) Ap(gc, f, n)
#define within(a, b, c) (word(a)<=word(b)&&word(b)<word(c))
#define owns(f, x) within((intptr_t*)f, x, (intptr_t*)f + f->len)
_Static_assert(sizeof(union x) == sizeof(intptr_t));
_Static_assert(-1 >> 1 == -1, "sign extended shift");


#define str_type_width (Width(struct g_vec) + 1)

#define vlen(_)((struct g_vec*)(_))->shape[0]
#define vtxt(_) ((char*)(((struct g_vec*)(_))->shape+1))
#define len(_) vlen(_)
#define txt(_) vtxt(_)
#define Width(_) b2w(sizeof(_))
#define avail(f) ((uintptr_t)(f->sp-f->hp))


static void gc_walk(struct g*g, intptr_t *p0, intptr_t *t0);

static struct g
  *please(struct g*, uintptr_t),
  *g_ana(struct g*, g_vm_t*),
  *g_hash_put(struct g*),
  *g_intern(struct g*),
  *g_tbl(struct g*);
static g_inline struct g *g_intern(struct g*f) {
  f = g_have(f, Width(struct g_atom));
  if (g_ok(f)) f->sp[0] = (intptr_t) g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols);
  return f; }
static struct g*g_putx(struct g*, struct g_out*, intptr_t);
static g_vm_t g_yield, sysinfo;
static void *g_libc_malloc(struct g *f, uintptr_t n) { return malloc(n); }
static void g_libc_free(struct g *f, void *n)        { return free(n); }
static void *g_static_malloc(struct g *f, size_t n)  { return NULL; }
static void g_static_free(struct g *f, void*x)       {}

static g_vm_t
  data, dot,
  symnom, read0, prc,
  gensym, pairp, fixnump, symbolp, stringp,
  band, bor, bxor, bsr, bsl, bnot,
  ssub, sget, slen, scat,
  cons, car, cdr,
  lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
  seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
  defglob, drop1, imm, ref,
  free_variable, ev0,
  cond, jump, ap, tap, apn, tapn, ret, late_bind;




enum g_status g_fin(struct g *f) {
  enum g_status s = g_code_of(f);
  if ((f = g_core_of(f))) f->free(f, f->pool);
  return s; }

#ifndef EOF
#define EOF -1
#endif
struct ti { struct g_in in; const char *t; uintptr_t i; } ;
#define ti(x) ((struct ti*)(x))
static int p_text_eof(struct g_in *i) {
  return !ti(i)->t[ti(i)->i]; }
static int p_text_getc(struct g_in *i) {
  char c = ti(i)->t[ti(i)->i];
  if (c) ti(i)->i++;
  return c ? c : EOF; }
static int p_text_ungetc(struct g_in *i, int _) {
  uintptr_t idx = ti(i)->i;
  idx = idx ? idx - 1 : idx;
  ti(i)->i = idx;
  return ti(i)->t[idx]; }

g_inline struct g *g_eval(struct g *f) {
  f = g_ana(f, g_yield);
#if g_tco
  if (g_ok(f)) f = f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  while (g_ok(f)) f = f->ip->ap(f);
  f = g_code_of(f) == g_status_eof ? g_core_of(f) : f;
#endif
  return f; }

static g_inline struct g_tag { union x *null, *head, end[]; } *ttag(union x *k) {
  while (k->x) k++;
  return (struct g_tag*) k; }

g_inline bool g_twop(intptr_t _) { return even(_) && typ(_) == g_ty_two; }
g_inline bool g_tblp(intptr_t _) { return even(_) && typ(_) == g_ty_tbl; }
g_inline bool g_symp(intptr_t _) { return even(_) && typ(_) == g_ty_sym; }

static const size_t vt_size[] = {
  [g_vt_u8]  = 1, [g_vt_i8]  = 1, [g_vt_f8]  = 1,
  [g_vt_u16] = 2, [g_vt_i16] = 2, [g_vt_f16] = 2,
  [g_vt_u32] = 4, [g_vt_i32] = 4, [g_vt_f32] = 4,
  [g_vt_u64] = 8, [g_vt_i64] = 8, [g_vt_f64] = 8, };

g_inline uintptr_t g_fixed_size(enum g_vec_type t) {
  return vt_size[t]; }

struct d {
  int32_t type, rank;
  intptr_t shape[]; };
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
typedef uintptr_t g_xx_t(struct g*, intptr_t);
typedef intptr_t g_cp_t(struct g*, intptr_t, intptr_t*, intptr_t*);
typedef void g_wk_t(struct g*, intptr_t, intptr_t*, intptr_t*);
typedef bool g_id_t(struct g*, intptr_t, intptr_t);
typedef struct g
  *g_em_t(struct g*, struct g_out*, intptr_t),
  *g_pp_t(struct g*, intptr_t);

static int
  read_char(struct g_in *i);
static bool eql(struct g*, intptr_t, intptr_t);
static intptr_t
  gc_copy(struct g*,intptr_t,intptr_t*,intptr_t*),
  g_hash_get(struct g*,intptr_t, struct g_table*,intptr_t);
#define cp(...) gc_copy(__VA_ARGS__)
static uintptr_t hash(struct g*,intptr_t);


static struct g *g_buf_new(struct g *f) {
  f = g_have(f, str_type_width + 2);
  if (g_ok(f)) {
    union x *k = (union x*) f->hp;
    f->hp += str_type_width + 1;
    *--f->sp = word(k);
    struct g_vec *o = (struct g_vec*) k;
    ini_str(o, sizeof(intptr_t)); }
  return f; }

static struct g *g_buf_grow(struct g *f) {
  size_t len = len(f->sp[0]),
         req = str_type_width + 2 * b2w(len);
  f = g_have(f, req);
  if (g_ok(f)) {
    struct g_vec *o = (struct g_vec*) f->hp;
    f->hp += req;
    ini_str(o, 2 * len);
    memcpy(txt(o), txt(f->sp[0]), len);
    f->sp[0] = (intptr_t) o; }
  return f; }

struct g *g_read1i(struct g*f, struct g_in* i) {
  if (!g_ok(f)) return f;
  int c = read_char(i);
  size_t n = 0;
  switch (c) {
    case '(':  return g_readsi(f, i);
    case ')':  return g_push(f, 1, g_nil);
    case EOF:  return encode(f, g_status_eof);
    case '\'': return g_quote(g_read1i(f, i));
    case '"':  
      f = g_buf_new(f);
      for (size_t lim = sizeof(intptr_t); g_ok(f); f = g_buf_grow(f), lim *= 2)
        for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
          if ((c = i->getc(i)) == EOF || c == '"' ||
               (c == '\\' && (c = i->getc(i)) == EOF))
            return len(b) = n, f;
      return f;
    default:
      i->ungetc(i, c);
      f = g_buf_new(f);
      for (uintptr_t lim = sizeof(intptr_t); g_ok(f); f = g_buf_grow(f), lim *= 2)
        for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
          switch (c = i->getc(i)) {
            default: continue;
            case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
            case '(': case ')': case '"': case '\'': case EOF:
              i->ungetc(i, c);
              len(b) = n;
              txt(b)[n] = 0; // zero terminate for strtol ; n < lim so this is safe
              char *e;
              long j = strtol(txt(b), &e, 0);
              if (*e == 0) f->sp[0] = g_putnum(j);
              else f = g_intern(f);
              return f; }
      return f; } }

struct g *g_readsi(struct g *f, struct g_in* i) {
  intptr_t n = 0;
  for (int c; g_ok(f); n++) {
    c = read_char(i);
    if (c == EOF || c == ')') break;
    i->ungetc(i, c);
    f = g_read1i(f, i); }
  for (f = g_push(f, 1, g_nil); n--; f = g_cons_r(f));
  return f; }

static g_vm(dot) {
  g_putx(f, f->out, Sp[0]);
  Ip += 1;
  return Continue(); }

static struct g *g_pushr(struct g *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (n == m) return please(f, m);
  intptr_t x = va_arg(xs, intptr_t);
  MM(f, &x);
  f = g_pushr(f, m, n + 1, xs);
  UM(f);
  if (g_ok(f)) *--f->sp = x;
  return f; }

static g_inline void *bump(struct g *f, uintptr_t n) {
  void *x = f->hp;
  f->hp += n;
  return x; }

static uintptr_t vector_data_bytes(struct g_vec *v) {
  uintptr_t len = vt_size[v->type],
            rank = v->rank,
            *shape = v->shape;
  while (rank--) len *= *shape++;
  return len; }
uintptr_t vector_total_bytes(struct g_vec *v) {
  return sizeof(struct g_vec) + v->rank * sizeof(intptr_t) + vector_data_bytes(v); }

#define twop g_twop
static intptr_t assq(struct g *f, intptr_t l, intptr_t k) {
  for (; twop(l); l = B(l)) if (eql(f, k, AA(l))) return A(l);
  return 0; }

static intptr_t memq(struct g *f, intptr_t l, intptr_t k) {
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
  while (i--) f = g_cons_r(f);
  if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
  return f; }

static intptr_t reverse(struct g *f, intptr_t l) {
  intptr_t n = g_nil;
  for (intptr_t m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
  return n; }

static g_inline struct g *g_have(struct g *f, uintptr_t n) {
  return !g_ok(f) || avail(f) >= n ? f : please(f, n); }

static struct g *enscope(struct g *f, struct env *par, intptr_t args, intptr_t imps) {
  f = g_push(f, 3, args, imps, par);
  uintptr_t n = Width(env);
  f = g_have(f, n + Width(struct g_tag) + 1);
  if (g_ok(f)) {
    union x *k = (union x*) f->hp;
    f->hp += n + Width(struct g_tag);
    *--f->sp = word(k);
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

static g_inline struct g *g_cons_2(struct g *f, intptr_t a, intptr_t b) {
  return g_cons_l(g_push(f, 2, a, b)); }

static g_inline struct g *g_cons_stack(struct g *f, int i, int j) {
  f = g_have(f, Width(struct g_pair));
  if (g_ok(f)) {
    struct g_pair *p = (struct g_pair*) f->hp;
    ini_pair(p, f->sp[i], f->sp[j]);
    f->hp += Width(struct g_pair);
    *++f->sp = (intptr_t) p; }
  return f; }
struct g *g_cons_l(struct g *f) { return g_cons_stack(f, 0, 1); }
struct g *g_cons_r(struct g *f) { return g_cons_stack(f, 1, 0); }

#define Ana(n, ...) struct g *n(struct g *f, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct g *n(struct g *f, struct env **c, size_t m, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, ana_if, ana_let, ana_ap_args;
static cata pull, cata_i, cata_ix, cata_var, cata_ap, cata_yield, cata_ret;

// generic instruction ana handlers
static struct g *ana_ix(struct g *f, g_vm_t *i, intptr_t x) {
  return g_push(f, 3, cata_ix, i, x); }

static g_noinline g_vm(gc, uintptr_t n) {
  Pack(f);
  f = please(f, n);
  if (g_ok(f)) return Unpack(f), Continue();
  return f; }

#define Have1() do { if (Sp == Hp) return Get(1); } while (0)
static g_vm(uncurry) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

#define Have(n) do { if (!Got(n)) return Get(n); } while (0)
static g_noinline g_vm(curry2) {
  union x *k = cell(Hp), *j = k;
  size_t S = 3 + Width(struct g_tag);
  Have(S);
  j[0].ap = uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k;
  Hp += S;
  Ip = cell(*Sp);
  Sp[0] = word(k);
  return Continue(); }

// FIXME curry should be different functions for n = 2 and n > 2.
g_vm(curry) {
  union x *k = cell(Hp), *j = k;
  uintptr_t n = g_getnum(Ip[1].x);
  if (n == 2) return Ap(curry2, f);
  size_t S = 5 + Width(struct g_tag);
  Have(S);
  j += 2;
  k[0].ap = curry;
  k[1].x = g_putnum(n - 1);

  j[0].ap = uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k;
  Hp += S;
  Ip = cell(*Sp);
  Sp[0] = word(k);
  return Continue(); }

static g_vm(jump) { return
  Ip = Ip[1].m,
  Continue(); }

static g_vm(cond) { return
  Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2,
  Continue(); }

// load instructions
//
// push an immediate value
static g_vm(imm) {
  Have1();
  Sp -= 1;
  Sp[0] = Ip[1].x;
  Ip += 2;
  return Continue(); }

static g_vm(g_yield) { return
  Ip = Ip[1].m,
  Pack(f),
  encode(f, g_status_yield); }

static g_vm(ev0) { return
  Ip += 1,
  Pack(f),
  f = g_ana(f, jump),
  !g_ok(f) ? f : (Unpack(f), Continue()); }


#define Kp (f->ip)
static Cata(cata_yield) {
  f = g_have(f, m + Width(struct g_tag));
  if (g_ok(f)) {
    union x *k = (union x*) f->hp;
    f->hp += m + Width(struct g_tag);
    struct g_tag *t = (struct g_tag*) (k + m);
    t->null = NULL;
    t->head = k; 
    memset(k, -1, m * sizeof(intptr_t));
    Kp = k + m; }
  return f; }

static Cata(cata_if_pop_exit) {
  f = pull(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(cata_if_pop_branch) {
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = cond,
    Kp[1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  return f; }

static Cata(cata_if_push_branch) {
  f = pull(f, c, m);
  f = g_cons_2(f, (intptr_t) Kp, (*c)->alts);
  if (g_ok(f)) (*c)->alts = g_pop1(f);
  return f; }

static Cata(cata_if_push_exit) {
  f = pull(f, c, m);
  f = g_cons_2(f, (intptr_t) Kp, (*c)->ends);
  if (g_ok(f)) (*c)->ends = g_pop1(f);
  return f; }

static Cata(cata_if_jump_out) {
  f = pull(f, c, m + 3);
  if (g_ok(f)) {
    union x *a = cell(A((*c)->ends));
    if (a->ap == ret || a->ap == tap)
      Kp = memcpy(Kp - 2, a, 2 * sizeof(intptr_t));
    else if (a->ap == tapn)
      Kp = memcpy(Kp - 3, a, 3 * sizeof(intptr_t));
    else
      Kp -= 2,
      Kp[0].ap = jump,
      Kp[1].x = (intptr_t) a; }
  return f; }

static Cata(cata_ap) {
  f = pull(f, c, m + 1);
  if (g_ok(f)) {
    if (Kp[0].ap == ret) Kp[0].ap = tap;
    else Kp -= 1, Kp[0].ap = ap; }
  return f; }

static Cata(cata_apn) {
  intptr_t arity = g_pop1(f);
  f = pull(f, c, m + 2);
  if (g_ok(f)) {
    if (Kp[0].ap == ret) Kp -= 1, Kp[0].ap = tapn, Kp[1].x = arity;
    else Kp -= 2, Kp[0].ap = apn, Kp[1].x = arity; }
  return f; }

static Cata(cata_ix_, g_vm_t *i, intptr_t x) {
  avec(f, x, f = pull(f, c, m + 2));
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = i,
    Kp[1].x = x;
  return f; }

static Cata(cata_var_2) {
  intptr_t var = g_pop1(f),
           stack = g_pop1(f),
           i = 0;
  while (twop(stack))
    if (eql(f, A(stack), var)) break;
    else stack = B(stack), i++;
  return cata_ix_(f, c, m, ref, g_putnum(i)); }

// emit stack reference instruction
static Cata(cata_var) {
  intptr_t v = g_pop1(f), // variable name
           i = llen(g_pop1(f)); // stack inset
  for (intptr_t l = (*c)->imps; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
  for (intptr_t l = (*c)->args; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
out:
  return cata_ix_(f, c, m, ref, g_putnum(i)); }

static g_inline Cata(pull) { return ((cata*) g_pop1(f))(f, c, m); }

static Cata(cata_i) {
  g_vm_t *i = (g_vm_t*) g_pop1(f);
  f = pull(f, c, m + 1);
  if (g_ok(f))
    Kp -= 1,
    Kp[0].ap = i;
  return f; }

static Cata(cata_ix) {
  g_vm_t *i = (g_vm_t*) g_pop1(f);
  intptr_t x = g_pop1(f);
  return cata_ix_(f, c, m, i, x); }

static Cata(cata_curry) {
  struct env *e = (struct env*) g_pop1(f);
  uintptr_t ar = llen(e->args) + llen(e->imps);
  return ar == 1 ?  pull(f, c, m) : cata_ix_(f, c, m, curry, g_putnum(ar)) ; }

static Cata(cata_ret) {
  struct env *e = (struct env*) g_pop1(f);
  uintptr_t ar = llen(e->args) + llen(e->imps);
  return cata_ix_(f, c, m, ret, g_putnum(ar)); }

static struct g *ana_lambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp),
                *ana_seq(struct g*, struct env**, intptr_t, intptr_t);

static g_inline struct g *g_swap(struct g *f) {
  if (g_ok(f)) {
    intptr_t _ = f->sp[0];
    f->sp[0] = f->sp[1];
    f->sp[1] = _; }
  return f; }

static Ana(analyze) {
  if (!g_ok(f)) return f;
  // is it a variable?
  if (g_symp(x)) for (struct env *d = *c;; d = d->par) {
    if (nilp(d)) { // free variable?
      intptr_t y = g_hash_get(f, 0, dict_of(f), x);
      if (y) return ana_ix(f, imm, y);
      f = g_cons_2(f, x, (*c)->imps);
      if (g_ok(f)) (*c)->imps = g_pop1(f),
                   f = ana_ix(f, free_variable, (*c)->imps);
      return f; }
    // defined as a function by a local let form?
    intptr_t y;
    if ((y = assq(f, d->lams, x))) return
      avec(f, y, f = ana_ap_args(f, c, BB(y))),
      ana_ix(f, late_bind, y);
    // non function definition from local let form?
    if (memq(f, d->stack, x)) return
      g_push(f, 3, cata_var_2, x, d->stack);
    // closure or positional argument?
    if (memq(f, d->imps, x) || memq(f, d->args, x)) {
      if (*c != d) { // if we have found the variable in an enclosing scope then import it
        f = g_cons_2(f, x, (*c)->imps);
        if (g_ok(f)) x = g_pop1(f), (*c)->imps = x, x = A(x); }
      return g_push(f, 3, cata_var, x, (*c)->stack); } }
  // if it's not a variable and it's not a list (pair) then it evals to itself
  if (!twop(x)) return ana_ix(f, imm, x);
  intptr_t a = A(x), b = B(x);
  // singleton list?
  if (!twop(b)) return analyze(f, c, a); // value of first element
  // special form?
  if (g_symp(a)) {
    struct g_atom *y = sym(a);
    if (y == f->quote) return ana_ix(f, imm, !twop(b) ? b : A(b));
    if (y == f->let) return !twop(B(b)) ? analyze(f, c, A(b)) :
                                          ana_let(f, c, b);
    if (y == f->begin) return ana_seq(f, c, A(b), B(b));
    if (y == f->lambda) return f = ana_lambda(f, c, g_nil, b),
                               g_ok(f) ? analyze(f, c, g_pop1(f)) : 0;
    if (y == f->cond) return !twop(B(b)) ? analyze(f, c, A(b)) :
                                           ana_if(f, c, b); }
  // macro?
  intptr_t mac = g_hash_get(f, 0, f->macro, a);
  if (mac) return
    f = g_eval(g_cons_l(g_quote(g_swap(g_enlist(g_quote(g_push(f, 2, b, mac))))))),
    g_ok(f) ? analyze(f, c, g_pop1(f)) : f;
  // application.
  return avec(f, a, f = ana_ap_args(f, c, b)),
         analyze(f, c, a); }

static struct g *ana_lambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp) {
  f = enscope(f, *c, exp, imps);
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
    while (n--) f = g_cons_r(f);
    UM(f);
    f = g_push(f, 1, A(x)); }

  union x*k, *ip;
  if (g_ok(f))
    exp = g_pop1(f),
    d->args = f->sp[0],
    f->sp[0] = word(cata_yield),
    avec(f, exp, f = g_push(f, 2, cata_ret, d)),
    f = analyze(f, &d, exp),
    f = g_push(f, 2, cata_curry, d),
    ip = f->ip,
    avec(f, ip, f = pull(f, &d, 0));

  if (g_ok(f))
    k = f->ip,
    ttag(k)->head = k,
    f->ip = ip,
    f = g_cons_2(f, word(k), d->imps);

  UM(f);
  return f; }

static Ana(ana_if_r) {
  avec(f, x, f =
    !twop(x) || !twop(B(x)) ?
      g_push(f, 1, cata_if_jump_out) :
      (f = ana_if_r(f, c, BB(x)),
       f = g_push(f, 2, cata_if_jump_out, cata_if_push_branch),
       f = analyze(f, c, AB(x)),
       g_push(f, 1, cata_if_pop_branch)));
  return analyze(f, c, twop(x) ? A(x) : g_nil); }

static Ana(ana_if) {
  avec(f, x, f = g_push(f, 1, cata_if_push_exit));
  return g_push(ana_if_r(f, c, x), 1, cata_if_pop_exit); }

static struct g *ana_seq(struct g*f, struct env* *c, intptr_t a, intptr_t b) {
  if (!g_ok(f)) return f;
  if (twop(b)) avec(f, a, f = ana_seq(f, c, A(b), B(b)),
                          f = g_push(f, 2, cata_i, drop1));
  return analyze(f, c, a); }

static struct g *ana_ap_args_r(struct g *f, struct env**c, intptr_t x) {
  if (!twop(x)) return f;
  avec(f, x, f = ana_ap_args_r(f, c, B(x)),
             f = g_push(f, 1, cata_ap));
  return analyze(f, c, A(x)); }

// evaluate function call arguments and apply
static struct g *ana_ap_args(struct g *f, struct env **c, intptr_t x) {
  avec(f, x, f = g_cons_2(f, g_nil, (*c)->stack));
  if (g_ok(f))
    (*c)->stack = g_pop1(f),
    f = ana_ap_args_r(f, c, x),
    (*c)->stack = B((*c)->stack);
  return f; }

static intptr_t ldels(struct g *f, intptr_t lam, intptr_t l) {
  if (!twop(l)) return g_nil;
  intptr_t m = ldels(f, lam, B(l));
  if (!assq(f, lam, A(l))) B(l) = m, m = l;
  return m; }

static g_inline bool lambp(struct g *f, intptr_t x) {
  return twop(x) && A(x) == (intptr_t) f->lambda; }

// this is the longest function in the whole C implementation :(
// it handles the let special form in a way to support sequential and recursive binding.
static struct g *ana_let(struct g *f, struct env **b, intptr_t exp) {
  struct g_mem_root *mm = f->safe;
#define forget() ((f)->safe=(mm),f)
  MM(f, &exp);
  f = enscope(f, *b, (*b)->args, (*b)->imps);
  if (!g_ok(f)) return forget();
  struct env *q = (struct env*) g_pop1(f),
             **c = &q;
  // lots of variables :(
  intptr_t nom = g_nil, def = g_nil, lam = g_nil,
           v = g_nil, d = g_nil, e = g_nil;
  MM(f, &nom), MM(f, &def), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  while (twop(exp) && twop(B(exp))) {
    for (d = A(exp), e = AB(exp); twop(d); e = g_pop1(f), d = A(d)) {
      f = g_cons_2(f, e, g_nil);
      f = append(g_cons_2(f, word(f->lambda), B(d)));
      if (!g_ok(f)) return forget(); }
    f = g_cons_2(f, d, nom);
    f = g_cons_2(f, e, def);
    if (!g_ok(f)) return forget();
    def = g_pop1(f);
    nom = g_pop1(f);
    // if it's a lambda compile it and record in lam list
    if (twop(e) && f->lambda == (struct g_atom*) A(e)) {
      f = g_push(f, 2, d, lam);
      f = g_cons_l(g_cons_r(ana_lambda(f, c, g_nil, B(e))));
      if (!g_ok(f)) return forget();
      lam = g_pop1(f); }
    exp = BB(exp); }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even) {
    f = g_cons_2(f, A(nom), g_nil);
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
            f = g_cons_2(f, var, vars);
            if (!g_ok(f)) return forget();
            j++, BBA(e) = g_pop1(f); } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  f = append(g_cons_l(g_push(f, 3, f->lambda, nom, exp)));

  if (!g_ok(f)) return forget();
  exp = g_pop1(f);

  size_t ll = llen(nom);
  f = ll > 1 ? g_push(f, 2, cata_apn, g_putnum(ll)) :
               g_push(f, 1, cata_ap);
  f = g_cons_2(f, g_nil, (*b)->stack); // push function stack rep
  (*b)->stack = g_pop1(f);
  nom = reverse(f, nom); // put in literal order
  for (v = nom; g_ok(f) && twop(B(v)); v = B(v)) { // push initial variable stack reps
    f = g_cons_2(f, A(v), (*b)->stack);
    if (g_ok(f)) (*b)->stack = g_pop1(f); }


  nom = reverse(f, nom); // back to reverse order
  for (; twop(nom); nom = B(nom), def = B(def)) {
    if (lambp(f, A(def))) {
      d = assq(f, lam, A(nom));
      f = ana_lambda(f, c, BB(d), BA(def));
      if (!g_ok(f)) return forget();
      A(def) = B(d) = g_pop1(f); }
    if (even && nilp((*b)->args)) {
      f = ana_ix(f, defglob, A(nom));
      if (!g_ok(f)) return forget(); }
    f = analyze(f, b, A(def));
    (*b)->stack = B((*b)->stack); }

  f = analyze(f, b, exp);
  return forget(); }


g_vm(defglob) {
  Have(3);
  Sp -= 3;
  struct g_table *t = dict_of(f);
  intptr_t k = Ip[1].x,
           v = Sp[3];
  Sp[0] = k;
  Sp[1] = v;
  Sp[2] = (intptr_t) t;
  Pack(f);
  f = g_hash_put(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Sp += 1;
  Ip += 2;
  return Continue(); }

static g_vm(drop1) { return
  Ip += 1,
  Sp += 1,
  Continue(); }

static g_vm(free_variable) {
  intptr_t y = Ip[1].x,
           v = g_hash_get(f, y, f->dict, y); // see if it's defined now...
  Ip[0].ap = imm;
  Ip[1].x = v;
  return Continue(); }

static g_vm(late_bind) {
  intptr_t v = AB(Ip[1].x);
  Ip[0].ap = imm;
  Ip[1].x = v;
  return Continue(); }

static g_vm(data) {
  intptr_t x = word(Ip);
  Sp += 1;
  Ip = cell(Sp[0]);
  Sp[0] = x;
  return Continue(); }


// push a value from the stack
g_vm(ref) {
  Have1();
  Sp[-1] = Sp[g_getnum(Ip[1].x)];
  Sp -= 1;
  Ip += 2;
  return Continue(); }

// call and return
// apply function to one argument
g_vm(ap) {
  if (odd(Sp[1])) return Ip++, Sp++, Continue();
  union x*k = cell(Sp[1]);
  Sp[1] = word(Ip + 1);
  Ip = k;
  return Continue(); }

// tail call
g_vm(tap) {
  intptr_t x = Sp[0], j = Sp[1];
  Sp += g_getnum(Ip[1].x) + 1;
  if (odd(j)) return
    Sp += 1,
    Ip = cell(Sp[0]),
    Sp[0] = j,
    Continue();
  return
    Ip = cell(j),
    Sp[0] = x,
    Continue(); }

// apply to multiple arguments
g_vm(apn) {
  size_t n = g_getnum(Ip[1].x);
  union x*ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = cell(Sp[n]);
  Ip += 2;
  Sp[n] = word(ra); // store return address
  return Continue(); }

// tail call
g_vm(tapn) {
  size_t n = g_getnum(Ip[1].x),
         r = g_getnum(Ip[2].x);
  Ip = cell(Sp[n]) + 2;
  intptr_t *o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
g_vm(ret) {
  intptr_t n = g_getnum(Ip[1].x) + 1;
  Ip = cell(Sp[n]);
  Sp[n] = Sp[0];
  Sp += n;
  return Continue(); }
g_vm(ret0) { return
  Ip = cell(Sp[1]),
  Sp[1] = Sp[0],
  Sp += 1,
  Continue(); }


#define op(nom, n, x) g_vm(nom) { intptr_t _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
op(add, 2, (Sp[0]+Sp[1]-1)|1)
op(sub, 2, (Sp[0]-Sp[1])|1)
op(mul, 2, g_putnum(g_getnum(Sp[0])*g_getnum(Sp[1])))
op(quot, 2, nilp(Sp[1]) ? g_nil : g_putnum(g_getnum(Sp[0])/g_getnum(Sp[1])))
op(rem, 2, nilp(Sp[1]) ? g_nil : g_putnum(g_getnum(Sp[0])%g_getnum(Sp[1])))
op(eq, 2, eql(f, Sp[0], Sp[1]) ? g_putnum(-1) : g_nil)
op(lt, 2, Sp[0] < Sp[1] ? g_putnum(-1) : g_nil)
op(le, 2, Sp[0] <= Sp[1] ? g_putnum(-1) : g_nil)
op(gt, 2, Sp[0] > Sp[1] ? g_putnum(-1) : g_nil)
op(ge, 2, Sp[0] >= Sp[1] ? g_putnum(-1) : g_nil)
op(bnot, 1, ~Sp[0] | 1)
op(band, 2, (Sp[0] & Sp[1]) | 1)
op(bor, 2, (Sp[0] | Sp[1]) | 1)
op(bxor, 2, (Sp[0] ^ Sp[1]) | 1)
op(bsr, 2, g_putnum(g_getnum(Sp[0]) >> g_getnum(Sp[1])))
op(bsl, 2, g_putnum(g_getnum(Sp[0]) << g_getnum(Sp[1])))
op(fixnump, 1, odd(Sp[0]) ? g_putnum(-1) : g_nil)

struct g*g_tbl(struct g*f) {
  f = g_have(f, Width(struct g_table) + 2);
  if (g_ok(f)) {
    struct g_table *t = (struct g_table*) f->hp;
    f->hp += Width(struct g_table) + 1;
    *--f->sp = word(t);
    struct entry **tab = (struct entry**) (t + 1);
    tab[0] = 0;
    ini_table(t, 0, 1, tab); }
  return f; }

// general hashing method...
uintptr_t hash(struct g *f, intptr_t x) {
  if (odd(x)) {
    const int shift = sizeof(intptr_t) * 4;
    return x *= mix, (x << shift) | (x >> shift); }
  void *_ = (void*) x;
  if (!datp(x)) {
    if (!owns(f, x)) return mix ^ (mix * x);
    // it's a function, hash by length
    struct g_tag *t = ttag(_);
    intptr_t len = (union x*) t - t->head;
    return mix ^ (mix * len); }
  else switch (typ(x)) {
    case g_ty_two: return mix ^ (hash(f, A(x)) * hash(f, B(x)));
    case g_ty_sym: return sym(x)->code;
    case g_ty_tbl: return mix;
    case g_ty_str: default: {
      uintptr_t len = vector_total_bytes(_), h = 2166136261;
      for (uint8_t *bs = _; len--; h ^= *bs++, h *= 16777619);
      return h; } } }


// relies on table capacity being a power of 2
static g_inline uintptr_t index_of_key(struct g *f, struct g_table *t, intptr_t k) {
  return (t->cap - 1) & hash(f, k); }

g_noinline struct g *g_hash_put(struct g *f) {
  struct g_table *t = (struct g_table*) f->sp[2];
  intptr_t v = f->sp[1],
           k = f->sp[0];
  uintptr_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;

  if (e) return e->val = v, f->sp += 2, f;

  f = g_have(f, Width(struct entry) + 1);
  if (!g_ok(f)) return f;
  *--f->sp = word(f->hp);
  f->hp += Width(struct entry);

  e = (struct entry*) g_pop1(f);
  t = (struct g_table*) f->sp[2];
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
  struct entry **tab0, **tab1;

  f = g_have(f, cap1 + 1);
  if (!g_ok(f)) return f;
  *--f->sp = word(f->hp);
  f->hp += cap1;
  tab1 = (struct entry**) g_pop1(f);
  t = (struct g_table*) f->sp[2];
  tab0 = t->tab;
  memset(tab1, 0, cap1 * sizeof(intptr_t));
  for (t->cap = cap1, t->tab = tab1; cap0--;)
    for (struct entry *e, *es = tab0[cap0]; es;
      e = es,
      es = es->next,
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  return f->sp += 2, f; }

  
static struct entry *table_delete_r(
  struct g *f,
  struct g_table *t,
  intptr_t k,
  intptr_t *v,
  struct entry *e) { return
    !e ? e :
    eql(f, e->key, k) ? (t->len--,
                         *v = e->val,
                         e->next) :
    (e->next = table_delete_r(f, t, k, v, e->next),
     e); }

static g_noinline intptr_t table_delete(struct g *f, struct g_table *t, intptr_t k, intptr_t v) {
  uintptr_t idx = index_of_key(f, t, k);
  t->tab[idx] = table_delete_r(f, t, k, &v, t->tab[idx]);
  if (t->cap > 1 && t->len / t->cap < 1) {
    intptr_t cap = t->cap;
    struct entry *coll = 0, *x, *y; // collect all entries in one list
    for (intptr_t i = 0; i < cap; i++)
      for (x = t->tab[i], t->tab[i] = 0; x;)
        y = x, x = x->next, y->next = coll, coll = y;
    t->cap = cap >>= 1;
    for (intptr_t i; coll;)
      i = (cap - 1) & hash(f, coll->key),
      x = coll->next,
      coll->next = t->tab[i],
      t->tab[i] = coll,
      coll = x; }
  return v; }

g_vm(tnew) {
  Have(Width(struct g_table) + 1);
  struct g_table *t = (struct g_table*) Hp;
  struct entry **tab = (struct entry**) (t + 1);
  Hp += Width(struct g_table) + 1;
  tab[0] = 0;
  ini_table(t, 0, 1, tab);
  Sp[0] = (intptr_t) t;
  Ip++;
  return Continue(); }

intptr_t g_hash_get(struct g *f, intptr_t zero, struct g_table *t, intptr_t k) {
  uintptr_t i = index_of_key(f, t, k);
  struct entry *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

g_vm(tget) { return
  Sp[2] = g_hash_get(f, Sp[0], tbl(Sp[1]), Sp[2]),
  Sp += 2,
  Ip += 1,
  Continue(); }

g_vm(tset) {
  if (g_tblp(Sp[0])) {
    intptr_t t = Sp[0],
             k = Sp[1],
             v = Sp[2];
    Sp[0] = k;
    Sp[1] = v;
    Sp[2] = t;
    Pack(f);
    f = g_hash_put(f);
    if (!g_ok(f)) return f;
    Unpack(f); }
  return Ip += 1,
         Continue(); }

g_vm(tdel) { return
  Sp[2] = !g_tblp(Sp[1]) ? g_nil : table_delete(f, (struct g_table*) Sp[1], Sp[2], Sp[0]),
  Sp += 2,
  Ip += 1,
  Continue(); }

g_vm(tlen) { return
  Sp[0] = g_tblp(Sp[0]) ? g_putnum(((struct g_table*)Sp[0])->len) : g_nil,
  Ip += 1,
  Continue(); }

g_vm(tkeys) {
  intptr_t list = g_nil;
  if (g_tblp(Sp[0])) {
    struct g_table *t = (struct g_table*) Sp[0];
    intptr_t len = t->len;
    Have(len * Width(struct g_pair));
    struct g_pair *pairs = (struct g_pair*) Hp;
    Hp += len * Width(struct g_pair);
    for (uintptr_t i = t->cap; i;)
      for (struct entry *e = t->tab[--i]; e; e = e->next)
        ini_pair(pairs, e->key, list),
        list = (intptr_t) pairs, pairs++; }
  Sp[0] = list;
  Ip += 1;
  return Continue(); }
static g_noinline struct g_atom *g_intern_r(struct g *v, struct g_vec *b, struct g_atom **y) {
  struct g_atom *z = *y;
  if (!z) return // found an empty spot, insert new symbol
    z = bump(v, Width(struct g_atom)),
    ini_sym(z, b, hash(v, g_putnum(hash(v, (intptr_t) b)))),
    *y = z;
  struct g_vec *a = z->nom;
  // compare operation should be its own thing
  int i = len(a) < len(b) ? -1 :
          len(a) > len(b) ? 1 :
          strncmp(txt(a), txt(b), len(a));
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

#define strp g_strp
g_vm(gensym) {
  if (g_strp(Sp[0])) return Ap(nomsym, f);
  const uintptr_t req = Width(struct g_atom) - 2;
  Have(req);
  struct g_atom *y = (struct g_atom*) Hp;
  Hp += req;
  ini_anon(y, g_clock());
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

g_vm(symnom) {
  intptr_t y = Sp[0];
  y = g_symp(y) && ((struct g_atom*)y)->nom ? word(((struct g_atom*)y)->nom) : g_nil;
  Sp[0] = y;
  Ip += 1;
  return Continue(); }



static g_vm(symbolp) { return
  Sp[0] = g_symp(Sp[0]) ? g_putnum(-1) : g_nil,
  Ip += 1,
  Continue(); }




g_vm(slen) { return
  Sp[0] = g_strp(Sp[0]) ? g_putnum(len(Sp[0])) : g_nil,
  Ip += 1,
  Continue(); }

g_vm(ssub) {
  if (!g_strp(Sp[0])) Sp[2] = g_nil;
  else {
    struct g_vec*s = ((struct g_vec*)Sp[0]), *t;
    intptr_t i = odd(Sp[1]) ? g_getnum(Sp[1]) : 0,
             j = odd(Sp[2]) ? g_getnum(Sp[2]) : 0;
    i = MAX(i, 0);
    i = MIN(i, (intptr_t) len(s));
    j = MAX(j, i);
    j = MIN(j, (intptr_t) len(s));
    if (i == j) Sp[2] = g_nil;
    else {
      size_t req = str_type_width + b2w(j - i);
      Have(req);
      t = ((struct g_vec*)Hp);
      Hp += req;
      ini_str(t, j - i);
      memcpy(txt(t), txt(s) + i, j - i);
      Sp[2] = (intptr_t) t; } }
  return Ip += 1,
         Sp += 2,
         Continue(); }

g_vm(sget) {
  if (!g_strp(Sp[0])) Sp[1] = g_nil;
  else {
    struct g_vec *s = (struct g_vec*) Sp[0];
    uintptr_t i = g_getnum(Sp[1]);
    i = MIN(i, len(s) - 1);
    i = MAX(i, 0);
    Sp[1] = g_putnum(txt(s)[i]); }
  return Ip += 1,
         Sp += 1,
         Continue(); }

g_vm(scat) {
  intptr_t a = Sp[0], b = Sp[1];
  if (!g_strp(a)) return Sp += 1,
                       Ip += 1,
                       Continue();
  if (!g_strp(b)) return Sp[1] = a,
                       Sp += 1,
                       Ip += 1,
                       Continue();

  struct g_vec *x = (struct g_vec*)a, *y = (struct g_vec*)b;
  uintptr_t len = len(x) + len(y),
            req = str_type_width + b2w(len);
  Have(req);
  struct g_vec *z = (struct g_vec*)Hp;
  return Hp += req,
         ini_str(z, len),
         memcpy(txt(z), txt(x), len(x)),
         memcpy(txt(z) + len(x), txt(y), len(y)),
         Sp[1] = word(z),
         Ip += 1,
         Continue(); }

g_vm(stringp) { return
  Sp[0] = g_strp(Sp[0]) ? g_putnum(-1) : g_nil,
  Ip += 1,
  Continue(); }


static g_vm(car) { return
  Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0],
  Ip++,
  Continue(); }
static g_vm(cdr) { return
  Sp[0] = twop(Sp[0]) ? B(Sp[0]) : g_nil,
  Ip++,
  Continue(); }

static g_vm(cons) {
  Have(Width(struct g_pair));
  struct g_pair *w = (struct g_pair*) Hp;
  ini_pair(w, Sp[0], Sp[1]);
  Hp += Width(struct g_pair);
  Sp[1] = word(w);
  Sp++;
  Ip++;
  return Continue(); }

static g_vm(pairp) { return
  Sp[0] = twop(Sp[0]) ? g_putnum(-1) : g_nil,
  Ip++,
  Continue(); }


static g_noinline bool eql_cont(struct g *f, intptr_t a, intptr_t b) {
  if (!(even(a | b) && cell(a)->ap == data && cell(b)->ap == data && typ(a) == typ(b))) return false;
  else switch (typ(a)) {
    default: return false;
// FIXME could overflow the stack -- use off pool for this
    case g_ty_two: return eql(f, A(a), A(b)) && eql(f, B(a), B(b));
    case g_ty_str: {
      struct g_vec *x = (struct g_vec*) a, *y = (struct g_vec*) b;
      return 0 == memcmp(x, y, vector_total_bytes(x)); } } }
static g_inline bool eql(struct g *f, intptr_t a, intptr_t b) {
  return a == b || eql_cont(f, a, b); }



static struct g *copy_core(struct g*g, intptr_t *p1, uintptr_t len1, struct g *f) {
  memcpy(g, f, sizeof(struct g));
  uintptr_t len0 = f->len;
  intptr_t
    *p0 = (intptr_t*) f,
    *t0 = (intptr_t*) f + len0, // source pool top
    *t1 = (intptr_t*) g + len1, // target pool top
    *sp0 = f->sp,
    h = t0 - sp0,
    *sp1 = t1 - h; // stack height
  g->pool = p1;
  g->len = len1;
  g->sp = sp1;
  g->hp = g->cp = g->end;
  g->symbols = 0;
  g->ip = cell(gc_copy(g, word(g->ip), p0, t0));
  for (uintptr_t i = 0; i < g_nvars; i++)
    g->v[i] = gc_copy(g, g->v[i], p0, t0);
  for (intptr_t n = 0; n < h; n++)
    sp1[n] = gc_copy(g, sp0[n], p0, t0);
  for (struct g_mem_root *s = g->safe; s; s = s->next)
    *s->ptr = gc_copy(g, *s->ptr, p0, t0);
  while (g->cp < g->hp)
    gc_walk(g, p0, t0);
  return g->t0 = g_clock(), g; }


// keep v between
#define v_lo 8
// and
#define v_hi (v_lo << 6)
// where
//   v = (t2 - t0) / (t2 - t1)
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static g_noinline struct g *please(struct g *f, uintptr_t req0) {
  uintptr_t t0 = f->t0, t1 = g_clock(),
            len0 = f->len;
  intptr_t *w = (intptr_t*) f, *w0 = f->pool;
  struct g *g = (struct g*) (w == w0 ? w0 + len0 : w0);
  f = copy_core(g, f->pool, f->len, f);
  uintptr_t t2 = f->t0,      // get and set last gc end time
            req = req0 + len0 - avail(f),
            v = t2 == t1 ?  v_hi : (t2 - t0) / (t2 - t1),
            len1 = len0;

#define too_small (len1 < req || v < v_lo)
#define grow() (len1<<=1,v<<=1)
#define too_big (len1 >> 1 > req && v > v_hi)
#define shrink() (len1>>=1,v>>=1)
  if      (too_big) do shrink(); while (too_big);   // too big -> calculate smaller size
  else if (too_small) do grow(); while (too_small); // too small -> calculate bigger size
  else return f;                                    // just right -> all done

  // allocate a new pool with target size
  g = f->malloc(f, len1 * 2 * sizeof(intptr_t));
  if (!g) {
    if (req <= len0) return f;
    return encode(f, g_status_oom); }
  g = copy_core(g, (intptr_t*) g, len1, f);
  f->free(f, f->pool);
  return g; }

static void gc_walk(struct g*g, intptr_t *p0, intptr_t *t0) {
  if (!datp(g->cp)) for (g->cp += 2; g->cp[-2]; g->cp++)
    g->cp[-2] = gc_copy(g, g->cp[-2], p0, t0);
  else switch (typ(g->cp)) {
    case g_ty_str: default:
      g->cp += b2w(vector_total_bytes((struct g_vec*) g->cp));
      return;
    case g_ty_sym:
      g->cp += Width(struct g_atom) - (sym(g->cp)->nom ? 0 : 2);
      return;
    case g_ty_two: {
      intptr_t x = (intptr_t) g->cp;
      g->cp += Width(struct g_pair);
      A(x) = gc_copy(g, A(x), p0, t0);
      B(x) = gc_copy(g, B(x), p0, t0);
      return; }
    case g_ty_tbl: {
      struct g_table *t = (struct g_table*) g->cp;
      g->cp += Width(struct g_table) + t->cap + t->len * Width(struct entry);
      for (intptr_t i = 0, lim = t->cap; i < lim; i++)
        for (struct entry*e = t->tab[i]; e;
          e->key = gc_copy(g, e->key, p0, t0),
          e->val = gc_copy(g, e->val, p0, t0),
          e = e->next); } } }

static g_noinline intptr_t gc_copy(struct g *f, intptr_t x, intptr_t *p0, intptr_t *t0) {
  // if it's a number or it's outside managed memory then return it
  if (odd(x) || !within(p0, x, t0)) return x;
  union x *src = (union x*) x;
  x = src->x; // get its contents
  // if it contains a pointer to the new space then return the pointer
  if (!odd(x) && within((intptr_t*) f, x, (intptr_t*) f + f->len)) return x;
  // if it's data then call the copy function
  if (x != (intptr_t) data) {
    // it's a thread, find the end to find the head
    struct g_tag *t = ttag(src);
    union x *ini = t->head,
            *d = bump(f, t->end - ini),
            *dst = d;
    // copy source contents to dest and write dest addresses to source
    for (union x*s = ini; (d->x = s->x); s++->x = (intptr_t) d++);
    ((struct g_tag*) d)->head = dst;
    return (intptr_t) (dst + (src - ini)); }
  x = (intptr_t) src;
  switch (typ(src)) {
    case g_ty_two: {
      struct g_pair *src = (struct g_pair*) x,
                    *dst = bump(f, Width(struct g_pair));
      ini_pair(dst, src->a, src->b);
      return word(src->ap = (g_vm_t*) dst); }
    case g_ty_sym: {
      struct g_atom *src = sym(x), *dst;
      if (src->nom) dst = g_intern_r(f, (struct g_vec*) gc_copy(f, word(src->nom), p0, t0), &f->symbols);
      else dst = bump(f, Width(struct g_atom) - 2),
           ini_anon(dst, src->code);
      return (intptr_t) (src->ap = (g_vm_t*) dst); }
    case g_ty_str: default: {
      struct g_vec *src = (struct g_vec*) x;
      uintptr_t bytes = vector_total_bytes(src);
      struct g_vec *dst = bump(f, b2w(bytes));
      src->ap = memcpy(dst, src, bytes);
      return (intptr_t) dst; }
    case g_ty_tbl: {
      struct g_table *src = (struct g_table*) x;
      uintptr_t len = src->len, cap = src->cap;
      struct g_table *dst = bump(f, Width(struct g_table) + cap + Width(struct entry) * len);
      struct entry **tab = (struct entry**) (dst + 1),
                   *dd = (struct entry*) (tab + cap);
      ini_table(dst, len, cap, tab);
      src->ap = (g_vm_t*) dst;
      for (struct entry *d, *s, *last; cap--; tab[cap] = last)
        for (s = src->tab[cap], last = NULL; s;
          d = dd++,
          d->key = s->key,
          d->val = s->val,
          d->next = last,
          last = d,
          s = s->next);
      return word(dst); } } }

static g_vm(sysclock) {
  Sp[0] = g_putnum(g_clock());
  Ip += 1;
  return Continue(); }

static g_vm(nullp) {
  Sp[0] = nilp(Sp[0]) ? g_putnum(-1) : g_nil;
  Ip += 1;
  return Continue(); }

static g_vm(prc) {
  f = g_putc(f, f->out, g_getnum(*Sp));
  Ip += 1;
  return Continue(); }

static g_vm(sysinfo) {
  const uintptr_t req = 5 * Width(struct g_pair);
  Have(req);
  struct g_pair *si = (struct g_pair*) Hp;
  Hp += req;
  Sp[0] = word(si);
  ini_pair(si, g_putnum(f), word(si + 1));
  ini_pair(si + 1, g_putnum(f->pool), word(si + 2));
  ini_pair(si + 2, g_putnum(f->len), word(si + 3));
  ini_pair(si + 3, g_putnum(Hp - (intptr_t*) f), word(si + 4));
  ini_pair(si + 4, g_putnum(topof(f) - Sp), g_nil);
  Ip += 1;
  return Continue(); }

static g_vm(putn) {
  uintptr_t n = g_getnum(Sp[0]), b = g_getnum(Sp[1]);
  f = g_putn(f, f->out, n, b);
  Sp[1] = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }
static g_vm(g_puts) {
  if (strp(Sp[0])) {
    struct g_vec *s = (struct g_vec*) Sp[0];
    for (uintptr_t i = 0; i < len(s); f = g_putc(f, f->out, txt(s)[i++])); }
  Ip += 1;
  return Continue(); }

static g_vm(read0) {
  Pack(f);
  f = g_read1i(f, f->in);
  if (g_code_of(f) == g_status_eof) return // no error but end of file
    f = g_core_of(f),
    Unpack(f),
    Ip += 1,
    Continue();
  f = g_cons_l(f);
  if (!g_ok(f)) return f;
  return Unpack(f),
         Ip += 1,
         Continue(); }

#define S1(i) {{i}, {ret0}}
#define S2(i) {{curry},{.x=g_putnum(2)},{i}, {ret0}}
#define S3(i) {{curry},{.x=g_putnum(3)},{i}, {ret0}}
#define bifs(_) \
  _(bif_clock, "clock", S1(sysclock))\
  _(bif_add, "+", S2(add)) _(bif_sub, "-", S2(sub)) _(bif_mul, "*", S2(mul)) _(bif_quot, "/", S2(quot)) _(bif_rem, "%", S2(rem)) \
  _(bif_lt, "<", S2(lt))  _(bif_le, "<=", S2(le)) _(bif_eq, "=", S2(eq)) _(bif_ge, ">=", S2(ge))  _(bif_gt, ">", S2(gt)) \
  _(bif_bnot, "~", S1(bnot)) _(bif_bsl, "<<", S2(bsl)) _(bif_bsr, ">>", S2(bsr))\
  _(bif_band, "&", S2(band)) _(bif_bor, "|", S2(bor)) _(bif_bxor, "^", S2(bxor))\
  _(bif_cons, "X", S2(cons)) _(bif_car, "A", S1(car)) _(bif_cdr, "B", S1(cdr)) \
  _(bif_sget, "sget", S2(sget)) _(bif_ssub, "ssub", S3(ssub)) _(bif_slen, "slen", S1(slen)) _(bif_scat, "scat", S2(scat)) \
  _(bif_dot, ".", S1(dot)) _(bif_read, "read", S1(read0)) _(bif_putc, "putc", S1(prc))\
  _(bif_prn, "putn", S2(putn)) _(bif_puts, "puts", S1(g_puts))\
  _(bif_sym, "sym", S1(gensym)) _(bif_nom, "nom", S1(symnom))\
  _(bif_addr, "sysinfo", S1(sysinfo))\
  _(bif_thd, "thd", S1(thda)) _(bif_peek, "peek", S1(peek)) _(bif_poke, "poke", S2(poke)) _(bif_trim, "trim", S1(trim)) _(bif_seek, "seek", S2(seek)) \
  _(bif_tnew, "tnew", S1(tnew)) _(bif_tkeys, "tkeys", S1(tkeys)) _(bif_tlen, "tlen", S1(tlen)) _(bif_tset, "tset", S3(tset)) _(bif_tget, "tget", S3(tget)) _(bif_tdel, "tdel", S3(tdel))\
  _(bif_twop, "twop", S1(pairp)) _(bif_strp, "strp", S1(stringp)) _(bif_symp, "symp", S1(symbolp)) _(bif_nump, "nump", S1(fixnump)) _(bif_nilp, "nilp", S1(nullp))\
  _(bif_ev, "ev", S1(ev0))
#define built_in_function(n, _, d) static const union x n[] = d;
#define biff(b, n, _) {n, b},
bifs(built_in_function);
static g_vm(g_stop) { return Pack(f), f; }
static union x bif_stop[] = { {g_stop} };
static const struct { const char *n; const union x *x; } bifff[] = { bifs(biff) };
#define insts(_) _(free_variable) _(ret) _(ap) _(tap) _(apn) _(tapn) _(jump) _(cond) _(ref) _(imm) _(drop1) _(curry) _(defglob) _(late_bind) _(ret0)
#define i_entry(i) {"i_"#i,i},
static const struct { const char *n; g_vm_t *i; } i_dict[] = { insts(i_entry) };

static struct g *g_symof(struct g *f, const char *nom) {
  return g_intern(g_strof(f, nom)); }


// this is the general initialization function. arguments are
// - f: core pointer
// - len0: initial semispace size in words (== total_space_size / 2)
// - ma: malloc function pointer
// - fr: free function pointer
static struct g *g_ini_0(
    struct g *f,
    uintptr_t words,
    void *(*ma)(struct g*, uintptr_t),
    void (*fr)(struct g*, void*),
    struct g_in *in,
    struct g_out*out,
    struct g_out*err) {
  if (!g_ok(f)) return f;
  if (f == NULL) return encode(NULL, g_status_oom);

  // least allowed size = 2 * size of struct g
  if (words * sizeof(intptr_t) < sizeof(struct g)) return encode(NULL, g_status_oom);

  memset(f, 0, sizeof(struct g));
  f->len = words;
  f->pool = (intptr_t*) f;
  f->malloc = ma ? ma : g_static_malloc;
  f->free = fr ? fr : g_static_free;
  f->in = in ? in : &g_stdin;
  f->out = out ? out : &g_stdout;
  f->err = err ? err : f->out;
  f->hp = f->end;
  f->sp = (intptr_t*) f + words;
  f->ip = bif_stop;
  f->t0 = g_clock(); // this goes right before first allocation so gc always sees initialized t0
  f = g_symof(f, ":");
  f = g_symof(f, "?");
  f = g_symof(f, "`");
  f = g_symof(f, ",");
  f = g_symof(f, "\\");
  if (g_ok(f)) // these must be in reverse order from above
    f->lambda = sym(g_pop1(f)),
    f->begin = sym(g_pop1(f)),
    f->quote = sym(g_pop1(f)),
    f->cond = sym(g_pop1(f)),
    f->let = sym(g_pop1(f));
  f = g_tbl(g_tbl(f)); // dict and macro tables
  if (g_ok(f))
    f->macro = tbl(f->sp[0]),
    f->dict = tbl(f->sp[1]),
    f = g_symof(f, "macros"),
    f = g_hash_put(f),
    f = g_hash_put(g_symof(g_push(f, 1, (intptr_t) f->dict), "globals"));
  for (size_t i = 0; i < LEN(bifff); i++)
    f = g_hash_put(g_symof(g_push(f, 1, (intptr_t) bifff[i].x), bifff[i].n));
  for (size_t i = 0; i < LEN(i_dict); i++)
    f = g_hash_put(g_symof(g_push(f, 1, (intptr_t) i_dict[i].i), i_dict[i].n));
  return g_pop(f, 1); }



////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(struct g_in *i) {
  for (int c;;) switch (c = i->getc(i)) {
    default: return c;
    case '#': case ';': while (!i->eof(i) && (c = i->getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static const char g_digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
struct g* g_putn(struct g*f, struct g_out*o, uintptr_t n, uintptr_t base) {
  uintptr_t d = n % base;
  if (n / base) f = g_putn(f, o, n / base, base);
  return g_putc(f, o, g_digits[d]); }

static struct g* g_putx(struct g *f, struct g_out *o, intptr_t x) {
  if (odd(x)) return g_printf(f, o, "%d", (uintptr_t) g_getnum(x));
  else if (!datp(x)) return g_printf(f, o, "#%lx", (long) x);
  else switch (typ(x)) {
    case g_ty_two:
      if (A(x) == word(f->quote) && twop(B(x))) return
        f = g_putc(f, o,'\''),
        g_putx(f, o, AB(x));
      for (f = g_putc(f, o, '(');; f = g_putc(f, o, ' ')) {
        f = g_putx(f, o, A(x));
        if (!twop(x = B(x))) return g_putc(f, o, ')'); }
    case g_ty_tbl: return
      g_printf(f, o, "#table:%d/%d@%x",
          (intptr_t) tbl(x)->len,
          (intptr_t) tbl(x)->cap,
          (intptr_t) x);
    case g_ty_sym: {
      struct g_vec * s = sym(x)->nom;
      if (s) for (uintptr_t i = 0; i < len(s); f = g_putc(f, o, txt(s)[i++]));
      else f = g_printf(f, o, "#sym@%x", (intptr_t) x);
      return f; }
    case g_ty_str: default: {
      struct g_vec *v = (struct g_vec*) x;
      if (!vec_strp(v)) {
        uintptr_t rank = v->rank, *shape = v->shape;
        f = g_printf(f, o, "#vec@%x:%d.%d", (intptr_t) x, (intptr_t) v->type, (intptr_t) v->rank);
        for (uintptr_t i = rank, *j = shape; i--; f = g_printf(f, o, ".%d", (intptr_t) *j++)); }
      else {
        uintptr_t len = vlen(v);
        char *text = vtxt(v);
        f = g_putc(f, o, '"');
        for (char c; len--; f = g_putc(f, o, c))
          if ((c = *text++) == '\\' || c == '"') f = g_putc(f, o, '\\');
        f = g_putc(f, o, '"'); }
      return f; } } }


static g_vm(seek) { return
  Sp[1] = word(((union x*) Sp[1]) + g_getnum(Sp[0])),
  Sp += 1,
  Ip += 1,
  Continue(); }

static g_vm(peek) { return
  Sp[0] = cell(Sp[0])->x,
  Ip += 1,
  Continue(); }

static g_vm(poke) { return
  cell(Sp[1])->x = Sp[0],
  Sp += 1,
  Ip += 1,
  Continue(); }

static g_vm(thda) {
  size_t n = g_getnum(Sp[0]);
  Have(n + Width(struct g_tag));
  union x *k = cell(Hp);
  struct g_tag *t = (struct g_tag*) (k + n);
  Hp += n + Width(struct g_tag);
  t->null = NULL;
  t->head = k;
  memset(k, -1, n * sizeof(intptr_t));
  Sp[0] = (intptr_t) k;
  Ip += 1;
  return Continue(); }

static g_vm(trim) {
  union x *k = (union x*) Sp[0];
  ttag(k)->head = k;
  Ip += 1;
  return Continue(); }

struct g_in g_stdin = { g_stdin_getc, g_stdin_ungetc, g_stdin_eof };
struct g_out g_stdout = { g_stdout_putc };
struct g *g_vec0(struct g*f, uintptr_t type, uintptr_t rank, ...) {
  uintptr_t len = vt_size[type];
  va_list xs;
  va_start(xs, rank);
  for (uintptr_t i = rank; i--; len *= va_arg(xs, uintptr_t));
  va_end(xs);
  uintptr_t nbytes = sizeof(struct g_vec) + rank * sizeof(intptr_t) + len,
            ncells = b2w(nbytes);
  f = g_have(f, ncells + 1);
  if (g_ok(f)) {
    struct g_vec *v = (struct g_vec*) f->hp;
    f->hp += ncells;
    *--f->sp = word(v);
    va_start(xs, rank);
    ini_vecv(v, type, rank, xs);
    memset(v->shape + rank, 0, len);
    va_end(xs); }
  return f; }

struct g *g_ini_static(uintptr_t nbytes, void *f) {
  uintptr_t len0 = nbytes / (2 * sizeof(intptr_t));
  return g_ini_0(f, len0, NULL, NULL, NULL, NULL, NULL); }

struct g *g_ini_dynamic(void *(*ma)(struct g*, uintptr_t), void (*fr)(struct g*, void*)) {
  uintptr_t len0 = 1 << 10;
  struct g *f = ma(NULL, 2 * len0 * sizeof(intptr_t));
  return g_ini_0(f, len0, ma, fr, NULL, NULL, NULL); }


g_noinline struct g *g_reads(struct g *f, const char*s) {
  struct ti t = {{p_text_getc, p_text_ungetc, p_text_eof}, s, 0};
  return g_readsi(f, (void*) &t); }

struct g *g_write1(struct g *f) { return g_putx(f, f->out, f->sp[0]); }
struct g *g_ini(void) { return g_ini_dynamic(g_libc_malloc, g_libc_free); }

struct g *g_defns(struct g*f, uintptr_t len, struct g_def *defs) {
  for (uintptr_t i = 0; i < len; i++) {
    const char *s = defs[i].n;
    f = g_intern(g_strof(g_push(f, 2, f->dict, defs[i].x), s));
    if (g_ok(f)) {
      intptr_t w = f->sp[1];
      f->sp[1] = f->sp[2];
      f->sp[2] = w;
      f = g_pop(g_hash_put(f), 1); } }
  return f; }

struct g *g_push(struct g *f, uintptr_t m, ...) {
  if (!g_ok(f)) return f;
  va_list xs;
  va_start(xs, m);
  uintptr_t n = 0;
  if (avail(f) < m) f = g_pushr(f, m, n, xs);
  else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, intptr_t));
  va_end(xs);
  return f; }

g_inline struct g *g_strof(struct g *f, const char *cs) {
  uintptr_t bytes = strlen(cs),
            words = b2w(bytes),
            req = str_type_width + words;
  f = g_have(f, req + 1);
  if (g_ok(f)) {
    struct g_vec *o = (struct g_vec*) f->hp;
    f->hp += req;
    *--f->sp = word(o);
    ini_str(o, bytes);
    memcpy(txt(o), cs, bytes); }
  return f; }

g_inline uintptr_t g_str_len(intptr_t x) { return len(x); }
g_inline char *g_str_txt(intptr_t x) { return txt(x); }
g_inline struct g *g_pop(struct g *f, uintptr_t m) {
  return !g_ok(f) ? f : (f->sp += m, f); }

// keep this separate and g_noinline so g_eval can be tail call optimized if possible
static g_noinline struct g *g_ana(struct g *f, g_vm_t *y) {
  f = enscope(f, (struct env*) g_nil, g_nil, g_nil);
  if (!g_ok(f)) return f;
  struct env *c = (struct env*) g_pop1(f);
  intptr_t x = f->sp[0];
  f->sp[0] = (intptr_t) cata_yield;
  avec(f, c,
    avec(f, x, f = ana_ix(f, y, (intptr_t) f->ip)),
    f = analyze(f, &c, x),
    f = pull(f, &c, 0));
  return f; }

g_inline struct g*g_putc(struct g*f, struct g_out *o, int c) { 
  return o->putc(f, o, c); }

static struct g* gvprintf(struct g*f, struct g_out*o, const char *fmt, va_list xs) {
  while (*fmt) {
    char c = *fmt++;
    if (c != '%') f = g_putc(f, o, c);
    else re:
      switch (c = *fmt++) {
      case 0: return f;
      case 'l': goto re;
      default: f = g_putc(f, o, c); break;
      case 'd': f = g_putn(f, o, va_arg(xs, uintptr_t), 10); break;
      case 'x': f = g_putn(f, o, va_arg(xs, uintptr_t), 16); break;
      case 'o': f = g_putn(f, o, va_arg(xs, uintptr_t), 8); break;
      case 'b': f = g_putn(f, o, va_arg(xs, uintptr_t), 2); break; } }
  return f; }

struct g*g_printf(struct g *f, struct g_out *o, const char*fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  f = gvprintf(f, o, fmt, xs);
  va_end(xs);
  return f; }
