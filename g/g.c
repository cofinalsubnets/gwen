#include "g.h"
#include <stdarg.h>
#ifndef EOF
#define EOF -1
#endif

static struct g *gread1i(struct g*f, struct g_in* i);

static g_inline struct g *gread1(struct g *f) {
 return gread1i(f, &g_stdin); }

static g_inline int gfgetc(struct g*f, struct g_in *i) {
 return i->getc(f, i); }

static g_inline int giungetc(struct g*f, struct g_in *i, int c) {
 return i->ungetc(f, c, i); }

static g_inline int gfeof(struct g*f, struct g_in *i) {
 return i->eof(f, i); }

static g_inline struct g *geval(struct g*);

enum g_vec_type {
  g_vt_u8,  g_vt_i8,
  g_vt_u16, g_vt_i16,
  g_vt_u32, g_vt_i32,
  g_vt_u64, g_vt_i64,
  g_vt_f8,  g_vt_f16,
  g_vt_f32, g_vt_f64, };

static struct g *gana(struct g *f, g_vm_t *y);

static g_inline struct g *g_enlist(struct g*f) {
  return gxr(gpush(f, 1, g_nil)); }

static g_inline struct g *gquote(struct g*f) {
 if (!g_ok(f = g_enlist(f))) return f;
 return gxl(gpush(f, 1, f->quote)); }

static g_inline struct g *g_enc(struct g*f, enum g_status s) {
  return (struct g*) ((uintptr_t) f | s); }

static g_vm_t gvmdata;
enum g_ty { g_ty_two, g_ty_str, g_ty_nom, g_ty_tbl, };
static void inivecv(struct g_vec *v, uintptr_t type, uintptr_t rank, va_list xs) {
 intptr_t *shape = v->shape;
 v->ap = gvmdata;
 v->typ = g_ty_str;
 v->type = type;
 v->rank = rank;
 while (rank--) *shape++ = va_arg(xs, uintptr_t); }

static struct g_atom *gintern_r(struct g*, struct g_vec*, struct g_atom**);

static g_inline struct g *g_have(struct g *f, uintptr_t n);
static void inivec(struct g_vec *v, uintptr_t type, uintptr_t rank, ...) {
  va_list xs;
  va_start(xs, rank);
  inivecv(v, type, rank, xs);
  va_end(xs); }

static void inistr(struct g_vec *s, uintptr_t len) {
  inivec((struct g_vec*) s, g_vt_char, 1, len); }

struct g_pair {
  g_vm_t *ap;
  uintptr_t typ;
  intptr_t a, b; };

static g_inline void twoini(struct g_pair *w, intptr_t a, intptr_t b) {
  w->ap = gvmdata; w->typ = g_ty_two; w->a = a; w->b = b; }
static g_inline void tabini(struct g_tab *t, uintptr_t len, uintptr_t cap, struct g_kvs**tab) {
  t->ap = gvmdata; t->typ = g_ty_tbl; t->len = len; t->cap = cap; t->tab = tab; }
static g_inline void nomini(struct g_atom *y, struct g_vec *nom, uintptr_t code) {
  y->ap = gvmdata; y->typ = g_ty_nom; y->nom = nom; y->code = code; y->l = y->r = 0; }
static g_inline void inianon(struct g_atom *y, uintptr_t code) {
  y->ap = gvmdata; y->typ = g_ty_nom; y->nom = 0; y->code = code; }

static g_inline bool vec_strp(struct g_vec *s) { return s->type == g_vt_char && s->rank == 1; }
#define odd(_) ((uintptr_t)(_)&1)
#define even(_) !odd(_)
#define cell(_) ((union u*)(_))
#define typ(_) cell(_)[1].typ
bool g_strp(intptr_t _) { return even(_) && typ(_) == g_ty_str && vec_strp((struct g_vec*)_); }
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
#define datp(_) (cell(_)->ap==gvmdata)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->root=&((struct g_root){(g_num*)(r),f->root})))
#define UM(f) (f->root=f->root->next)
#define mix ((uintptr_t)2708237354241864315)
#define within(a, b, c) (num(a)<=num(b)&&num(b)<num(c))
#define owns(f, x) within((intptr_t*)f, x, (intptr_t*)f + f->len)
_Static_assert(sizeof(union u) == sizeof(intptr_t));
_Static_assert(-1 >> 1 == -1, "sign extended shift");


#define str_type_width (Width(struct g_vec) + 1)

#define vlen(_)((struct g_vec*)(_))->shape[0]
#define vtxt(_) ((char*)(((struct g_vec*)(_))->shape+1))
#define len(_) vlen(_)
#define txt(_) vtxt(_)
#define avail(f) ((uintptr_t)(f->sp-f->hp))


static void gc_walk(struct g*g, intptr_t *p0, intptr_t *t0);

static struct g
  *please(struct g*, uintptr_t),
  *gtabput(struct g*),
  *gintern(struct g*),
  *gtabnew(struct g*);
static g_inline struct g *gintern(struct g*f) {
  f = g_have(f, Width(struct g_atom));
  if (g_ok(f)) f->sp[0] = (intptr_t) gintern_r(f, (struct g_vec*) f->sp[0], &f->symbols);
  return f; }
static int gfputx(struct g*, struct g_out*, intptr_t);

static g_vm_t
  gvminfo, gvmyieldk, gvmdot,
  symnom, gvmread, gvmputc,
  gensym, gvmtwop, gvmnump, gvmnomp, gvmstringp, gvmtabp,
  gvmband, gvmbor, gvmbxor, gvmbsr, gvmbsl, gvmbnot,
  gvmssub, gvmsget, gvmslen, gvmscat,
  gvmcons, gvmcar, gvmcdr,
  gvmlt, gvmle, gvmeq, gvmgt, gvmge,
  gvmtset, gvmtabget, gvmtabdel, gvmtabnew, gvmtabkeys, gvmtablen,
  seek, peek, poke, trim, thda, gvmadd, gvmsub, gvmmul, gvmquot, gvmrem,
  gvmdefglob, gvmdrop1, gvmpushk, gvmpushr,
  gvmfreev, gvmeval,
  gvmcond, gvmjump, gvmap, gvmtap, gvmapn, gvmtapn, gvmret, gvmlazyb;

enum g_status gfin(struct g *f) {
  enum g_status s = g_code_of(f);
  f = g_core_of(f);
  if (f) f->free(f->pool, f);
  return s; }

static g_inline struct g *geval(struct g *f) {
 f = gana(f, gvmyieldk);
#if g_tco
 return g_ok(f) ? f->ip->ap(f, f->ip, f->hp, f->sp) : f; }
#else
 while (g_ok(f)) f = f->ip->ap(f);
 return g_code_of(f) == g_status_eof ? g_core_of(f) : f; }
#endif

static g_inline struct g_tag { union u *null, *head, end[]; } *ttag(union u *k) {
  while (k->x) k++;
  return (struct g_tag*) k; }

static g_inline bool twop(intptr_t _) { return even(_) && typ(_) == g_ty_two; }
static g_inline bool tabp(intptr_t _) { return even(_) && typ(_) == g_ty_tbl; }
static g_inline bool nomp(intptr_t _) { return even(_) && typ(_) == g_ty_nom; }

static const size_t vt_size[] = {
  [g_vt_u8]  = 1, [g_vt_i8]  = 1, [g_vt_f8]  = 1,
  [g_vt_u16] = 2, [g_vt_i16] = 2, [g_vt_f16] = 2,
  [g_vt_u32] = 4, [g_vt_i32] = 4, [g_vt_f32] = 4,
  [g_vt_u64] = 8, [g_vt_i64] = 8, [g_vt_f64] = 8, };

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

static int
  gigetc(struct g*, struct g_in *i);
static bool eql(struct g*, intptr_t, intptr_t);
static intptr_t
  ggccp(struct g*,intptr_t,intptr_t*,intptr_t*),
  gtabget(struct g*,intptr_t, struct g_tab*,intptr_t);
#define cp(...) ggccp(__VA_ARGS__)
static uintptr_t hash(struct g*, g_num);

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

static g_inline struct g *gfreadatom(struct g*f, struct g_in*i, int c) {
 uintptr_t n = 1, lim = sizeof(intptr_t);
 f = grbufn(f);
 if (g_ok(f))
  for (txt(f->sp[0])[0] = c; g_ok(f); f = grbufg(f), lim *= 2)
   for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
    switch (c = gfgetc(f, i)) {
     default: continue;
     case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
     case '(': case ')': case '"': case '\'': case 0 : case EOF:
      giungetc(f, i, c);
      len(b) = n;
      txt(b)[n] = 0; // zero terminate for strtol ; n < lim so this is safe
      char *e;
      long j = strtol(txt(b), &e, 0);
      if (*e == 0) f->sp[0] = gputnum(j);
      else f = gintern(f);
      return f; }
 return f; }

static g_inline struct g *gfreadstring(struct g*f, struct g_in*i, int c) {
 size_t n = 0;
 f = grbufn(f);
 for (size_t lim = sizeof(g_word); g_ok(f); f = grbufg(f), lim *= 2)
  for (struct g_vec *b = (struct g_vec*) f->sp[0]; n < lim; txt(b)[n++] = c)
   if ((c = gfgetc(f, i)) == EOF || c == '"' ||
       (c == '\\' && (c = gfgetc(f, i)) == EOF))
    return len(b) = n, f;
 return f; }


////
/// " the parser "
//
//
// get the next significant character from the stream
static int gigetc(struct g*f, struct g_in *i) {
 for (int c;;) switch (c = gfgetc(f, i)) {
  default: return c;
  case '#': case ';': while (!gfeof(f, i) && (c = gfgetc(f, i)) != '\n' && c != '\r');
  case 0: case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }


static struct g *greadsi(struct g *f, struct g_in* i) {
 intptr_t n = 0;
 for (int c; g_ok(f); n++) {
  c = gigetc(f, i);
  if (c == EOF || c == ')') break;
  giungetc(f, i, c);
  f = gread1i(f, i); }
 for (f = gpush(f, 1, g_nil); n--; f = gxr(f));
 return f; }

static struct g *gread1i(struct g*f, struct g_in* i) {
 if (!g_ok(f)) return f;
 int c = gigetc(f, i);
 switch (c) {
  case '(':  return greadsi(f, i);
  case ')': case EOF:  return g_enc(f, g_status_eof);
  case '\'': return gquote(gread1i(f, i));
  case '"': return gfreadstring(f, i, c);
  default: return gfreadatom(f, i, c); } }

static g_vm(gvmdot) {
 gfputx(f, &g_stdout, Sp[0]);
 Ip += 1;
 return Continue(); }

static struct g *gpushr(struct g *f, uintptr_t m, uintptr_t n, va_list xs) {
 if (n == m) return please(f, m);
 intptr_t x = va_arg(xs, intptr_t);
 MM(f, &x);
 f = gpushr(f, m, n + 1, xs);
 UM(f);
 if (g_ok(f)) *--f->sp = x;
 return f; }

static uintptr_t vec_bytes(struct g_vec *v) {
 intptr_t len = vt_size[v->type],
          rank = v->rank,
          *shape = v->shape;
 while (rank--) len *= *shape++;
 return sizeof(struct g_vec) + v->rank * sizeof(intptr_t) + len; }

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
  f = gpush(f, 1, l);
 if (!g_ok(f)) return f;
 if (i == 0) return f->sp++, f;
 if (g_ok(f)) f->sp[0] = f->sp[i + 1];
 while (i--) f = gxr(f);
 if (g_ok(f)) f->sp[1] = f->sp[0], f->sp++;
 return f; }

static intptr_t reverse(struct g *f, intptr_t l) {
 intptr_t n = g_nil;
 for (intptr_t m; twop(l);) m = l, l = B(l), B(m) = n, n = m;
 return n; }

static g_inline struct g *g_have(struct g *f, uintptr_t n) {
 return !g_ok(f) || avail(f) >= n ? f : please(f, n); }

static struct g *enscope(struct g *f, struct env *par, intptr_t args, intptr_t imps) {
 f = gpush(f, 3, args, imps, par);
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

static g_inline struct g *gx2(struct g *f, intptr_t a, intptr_t b) {
  return gxl(gpush(f, 2, a, b)); }

static g_noinline struct g *gx_(struct g *f, int i, int j) {
 f = g_have(f, Width(struct g_pair));
 if (g_ok(f)) {
  struct g_pair *p = bump(f, Width(struct g_pair));
  twoini(p, f->sp[i], f->sp[j]);
  *++f->sp = (intptr_t) p; }
 return f; }

g_inline struct g *gxl(struct g *f) { return gx_(f, 0, 1); }
g_inline struct g *gxr(struct g *f) { return gx_(f, 1, 0); }

#define Ana(n, ...) struct g *n(struct g *f, struct env **c, intptr_t x, ##__VA_ARGS__)
#define Cata(n, ...) struct g *n(struct g *f, struct env **c, size_t m, ##__VA_ARGS__)
typedef Ana(ana);
typedef Cata(cata);
static ana analyze, ganaif, ganalet, ganaapargs;
static cata pull, gcatai, gcataix, gcatavar, gcataap, gcatayield, gcataret;

// generic instruction ana handlers
static struct g *ganaix(struct g *f, g_vm_t *i, intptr_t x) {
  return gpush(f, 3, gcataix, i, x); }

static g_noinline g_vm(gvmgc, uintptr_t n) {
  Pack(f);
  f = please(f, n);
  if (g_ok(f)) return Unpack(f), Continue();
  return f; }

#define Have1() if (Sp == Hp) return Ap(gvmgc, f, 1)
#define Have(n) if (Sp < Hp + n) return Ap(gvmgc, f, n)
static g_vm(gvmuncurry) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

g_vm(gvmcurry) {
 union u *k = (union u*) Hp, *j = k;
 uintptr_t n = ggetnum(Ip[1].x);
 size_t S = 3 + Width(struct g_tag);

 if (n == 2) {
  Have(S);
  j[0].ap = gvmuncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k; }

 else {
  S += 2;
  Have(S);
  j += 2;
  k[0].ap = gvmcurry;
  k[1].x = gputnum(n - 1);
  j[0].ap = gvmuncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k; }

 Hp += S;
 Ip = cell(*Sp);
 Sp[0] = word(k);
 return Continue(); }

static g_vm(gvmjump) {
 Ip = Ip[1].m;
 return Continue(); }

static g_vm(gvmcond) {
 Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2;
 return Continue(); }

// load instructions
//
// push an gvmpushkediate value
static g_vm(gvmpushk) {
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 Ip += 2;
 return Continue(); }

static g_vm(gvmyieldk) {
 Ip = Ip[1].m;
 Pack(f);
 return g_enc(f, g_status_yield); }

static g_vm(gvmeval) {
 Ip += 1;
 Pack(f);
 f = gana(f, gvmjump);
 if (!g_ok(f)) return f;
 Unpack(f);
 return Continue(); }

#define Kp (f->ip)
static Cata(gcatayield) {
  f = g_have(f, m + Width(struct g_tag));
  if (g_ok(f)) {
    union u *k = bump(f, m + Width(struct g_tag));
    struct g_tag *t = (struct g_tag*) (k + m);
    t->null = NULL;
    t->head = k; 
    memset(k, -1, m * sizeof(intptr_t));
    Kp = k + m; }
  return f; }

static Cata(gcataif_pop_exit) {
  f = pull(f, c, m);
  if (g_ok(f)) (*c)->ends = B((*c)->ends); // pops cond expression exit address off env stack ends
  return f; }

static Cata(gcataif_pop_branch) {
  f = pull(f, c, m + 2);
  if (g_ok(f))
    Kp -= 2,
    Kp[0].ap = gvmcond,
    Kp[1].x = A((*c)->alts);
  (*c)->alts = B((*c)->alts);
  return f; }

#define gpop1(f) (*(f)->sp++)
static Cata(gcataif_push_branch) {
  f = pull(f, c, m);
  f = gx2(f, (intptr_t) Kp, (*c)->alts);
  if (g_ok(f)) (*c)->alts = gpop1(f);
  return f; }

static Cata(gcataif_push_exit) {
  f = pull(f, c, m);
  f = gx2(f, (intptr_t) Kp, (*c)->ends);
  if (g_ok(f)) (*c)->ends = gpop1(f);
  return f; }

static Cata(gcataif_jump_out) {
 f = pull(f, c, m + 3);
 if (g_ok(f)) {
  union u *a = cell(A((*c)->ends));
  if (a->ap == gvmret || a->ap == gvmtap)
   Kp = memcpy(Kp - 2, a, 2 * sizeof(intptr_t));
  else if (a->ap == gvmtapn)
   Kp = memcpy(Kp - 3, a, 3 * sizeof(intptr_t));
  else
   Kp -= 2,
   Kp[0].ap = gvmjump,
   Kp[1].x = (intptr_t) a; }
 return f; }

static Cata(gcataap) {
 f = pull(f, c, m + 1);
 if (g_ok(f)) {
  if (Kp[0].ap == gvmret) Kp[0].ap = gvmtap;
  else Kp -= 1, Kp[0].ap = gvmap; }
 return f; }

static Cata(gcataapn) {
 intptr_t arity = gpop1(f);
 f = pull(f, c, m + 2);
 if (g_ok(f)) {
  if (Kp[0].ap == gvmret) Kp -= 1, Kp[0].ap = gvmtapn, Kp[1].x = arity;
  else Kp -= 2, Kp[0].ap = gvmapn, Kp[1].x = arity; }
 return f; }

static Cata(gcataix_, g_vm_t *i, intptr_t x) {
 avec(f, x, f = pull(f, c, m + 2));
 if (g_ok(f))
  Kp -= 2,
  Kp[0].ap = i,
  Kp[1].x = x;
 return f; }

static Cata(gcatavar_2) {
 intptr_t var = gpop1(f),
          stack = gpop1(f),
          i = 0;
 while (twop(stack))
  if (eql(f, A(stack), var)) break;
  else stack = B(stack), i++;
 return gcataix_(f, c, m, gvmpushr, gputnum(i)); }

// emit stack gvmpushrerence instruction
static Cata(gcatavar) {
 intptr_t v = gpop1(f), // variable name
          i = llen(gpop1(f)); // stack inset
 for (intptr_t l = (*c)->imps; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
 for (intptr_t l = (*c)->args; !nilp(l); l = B(l), i++) if (eql(f, v, A(l))) goto out;
out:
 return gcataix_(f, c, m, gvmpushr, gputnum(i)); }

static g_inline Cata(pull) {
 return ((cata*) gpop1(f))(f, c, m); }

static Cata(gcatai) {
 g_vm_t *i = (g_vm_t*) gpop1(f);
 f = pull(f, c, m + 1);
 if (g_ok(f))
  Kp -= 1,
  Kp[0].ap = i;
 return f; }

static Cata(gcataix) {
 g_vm_t *i = (g_vm_t*) gpop1(f);
 intptr_t x = gpop1(f);
 return gcataix_(f, c, m, i, x); }

static Cata(gcatacurry) {
 struct env *e = (struct env*) gpop1(f);
 uintptr_t ar = llen(e->args) + llen(e->imps);
 return ar == 1 ?  pull(f, c, m) :
  gcataix_(f, c, m, gvmcurry, gputnum(ar)) ; }

static Cata(gcataret) {
  struct env *e = (struct env*) gpop1(f);
  uintptr_t ar = llen(e->args) + llen(e->imps);
  return gcataix_(f, c, m, gvmret, gputnum(ar)); }

static struct g *ganalambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp),
                *ganaseq(struct g*, struct env**, intptr_t, intptr_t);

static Ana(analyze) {
  if (!g_ok(f)) return f;
  // is it a variable?
  if (nomp(x)) for (struct env *d = *c;; d = d->par) {
    if (nilp(d)) { // free variable?
      intptr_t y = gtabget(f, 0, dict_of(f), x);
      if (y) return ganaix(f, gvmpushk, y);
      f = gx2(f, x, (*c)->imps);
      if (g_ok(f)) (*c)->imps = gpop1(f),
                   f = ganaix(f, gvmfreev, (*c)->imps);
      return f; }
    // defined as a function by a local let form?
    intptr_t y;
    if ((y = assq(f, d->lams, x))) return
      avec(f, y, f = ganaapargs(f, c, BB(y))),
      ganaix(f, gvmlazyb, y);
    // non function definition from local let form?
    if (memq(f, d->stack, x)) return
      gpush(f, 3, gcatavar_2, x, d->stack);
    // closure or positional argument?
    if (memq(f, d->imps, x) || memq(f, d->args, x)) {
      if (*c != d) { // if we have found the variable in an enclosing scope then import it
        f = gx2(f, x, (*c)->imps);
        if (g_ok(f)) x = gpop1(f), (*c)->imps = x, x = A(x); }
      return gpush(f, 3, gcatavar, x, (*c)->stack); } }
  // if it's not a variable and it's not a list (pair) then it evals to itself
  if (!twop(x)) return ganaix(f, gvmpushk, x);
  intptr_t a = A(x), b = B(x);
  // singleton list?
  if (!twop(b)) return analyze(f, c, a); // value of first element
  // special form?
  if (nomp(a)) {
    struct g_atom *y = sym(a);
    if (y == f->quote) return ganaix(f, gvmpushk, !twop(b) ? b : A(b));
    if (y == f->let) return !twop(B(b)) ? analyze(f, c, A(b)) :
                                          ganalet(f, c, b);
    if (y == f->begin) return ganaseq(f, c, A(b), B(b));
    if (y == f->lambda) return f = ganalambda(f, c, g_nil, b),
                               g_ok(f) ? analyze(f, c, gpop1(f)) : 0;
    if (y == f->cond) return !twop(B(b)) ? analyze(f, c, A(b)) :
                                           ganaif(f, c, b); }
  // macro?
  intptr_t mac = gtabget(f, 0, f->macro, a);
  if (mac) return
   f = gpush(f, 5, b, g_nil ,f->quote, g_nil, mac),
   f = geval(gxr(gxl(gxr(gxl(f))))),
   g_ok(f) ? analyze(f, c, gpop1(f)) : f;
  // application.
  avec(f, a, f = ganaapargs(f, c, b));
  return analyze(f, c, a); }

static struct g *ganalambda(struct g *f, struct env **c, intptr_t imps, intptr_t exp) {
  f = enscope(f, *c, exp, imps);
  struct env *d = (struct env*) gpop1(f);
  MM(f, &d);
  intptr_t x = d->args;

  // push exp args onto stack
  if (!twop(x)) f = gpush(f, 2, g_nil, g_nil);
  else { // there's at least one argument
    MM(f, &x);
    int n = 0;
    while (twop(B(x))) f = gpush(f, 1, A(x)), n++, x = B(x);
    f = gpush(f, 1, g_nil);
    while (n--) f = gxr(f);
    UM(f);
    f = gpush(f, 1, A(x)); }

  union u*k, *ip;
  if (g_ok(f))
    exp = gpop1(f),
    d->args = f->sp[0],
    f->sp[0] = word(gcatayield),
    avec(f, exp, f = gpush(f, 2, gcataret, d)),
    f = analyze(f, &d, exp),
    f = gpush(f, 2, gcatacurry, d),
    ip = f->ip,
    avec(f, ip, f = pull(f, &d, 0));

  if (g_ok(f))
    k = f->ip,
    ttag(k)->head = k,
    f->ip = ip,
    f = gx2(f, word(k), d->imps);

  return UM(f), f; }

static Ana(ganaif_r) {
  avec(f, x, f =
    !twop(x) || !twop(B(x)) ?
      gpush(f, 1, gcataif_jump_out) :
      (f = ganaif_r(f, c, BB(x)),
       f = gpush(f, 2, gcataif_jump_out, gcataif_push_branch),
       f = analyze(f, c, AB(x)),
       gpush(f, 1, gcataif_pop_branch)));
  return analyze(f, c, twop(x) ? A(x) : g_nil); }

static Ana(ganaif) {
  avec(f, x, f = gpush(f, 1, gcataif_push_exit));
  return gpush(ganaif_r(f, c, x), 1, gcataif_pop_exit); }

static struct g *ganaseq(struct g*f, struct env**c, intptr_t a, intptr_t b) {
 if (g_ok(f) && twop(b)) avec(f, a, f = ganaseq(f, c, A(b), B(b)),
                                    f = gpush(f, 2, gcatai, gvmdrop1));
 return analyze(f, c, a); }

static struct g *ganaapargsr(struct g *f, struct env**c, intptr_t x) {
  if (!twop(x)) return f;
  avec(f, x, f = ganaapargsr(f, c, B(x)),
             f = gpush(f, 1, gcataap));
  return analyze(f, c, A(x)); }

// evaluate function call arguments and apply
static struct g *ganaapargs(struct g *f, struct env **c, intptr_t x) {
  avec(f, x, f = gx2(f, g_nil, (*c)->stack));
  if (g_ok(f))
    (*c)->stack = gpop1(f),
    f = ganaapargsr(f, c, x),
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
static struct g *ganalet(struct g *f, struct env **b, intptr_t exp) {
  struct g_root *mm = f->root;
#define forget() ((f)->root=(mm),f)
  MM(f, &exp);
  f = enscope(f, *b, (*b)->args, (*b)->imps);
  if (!g_ok(f)) return forget();
  struct env *q = (struct env*) gpop1(f),
             **c = &q;
  // lots of variables :(
  intptr_t nom = g_nil, def = g_nil, lam = g_nil,
           v = g_nil, d = g_nil, e = g_nil;
  MM(f, &nom), MM(f, &def), MM(f, &lam);
  MM(f, &d); MM(f, &e); MM(f, &v); MM(f, &q);

  // collect vars and defs into two lists
  while (twop(exp) && twop(B(exp))) {
    for (d = A(exp), e = AB(exp); twop(d); e = gpop1(f), d = A(d)) {
      f = gx2(f, e, g_nil);
      f = append(gx2(f, word(f->lambda), B(d)));
      if (!g_ok(f)) return forget(); }
    f = gx2(f, d, nom);
    f = gx2(f, e, def);
    if (!g_ok(f)) return forget();
    def = gpop1(f);
    nom = gpop1(f);
    // if it's a lambda compile it and record in lam list
    if (twop(e) && f->lambda == (struct g_atom*) A(e)) {
      f = gpush(f, 2, d, lam);
      f = gxl(gxr(ganalambda(f, c, g_nil, B(e))));
      if (!g_ok(f)) return forget();
      lam = gpop1(f); }
    exp = BB(exp); }

  // if there's no body then evaluate the name of the last definition
  bool even = !twop(exp); // we check this again later to make global bindings at top level
  if (even) {
    f = gx2(f, A(nom), g_nil);
    if (!g_ok(f)) return forget();
    exp = gpop1(f); }

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
            j++, BBA(e) = gpop1(f); } }
  while (j);

  // now delete defined functions from the closure variable lists
  // they will be bound lazily when the function runs
  for (e = lam; twop(e); BBA(e) = ldels(f, lam, BBA(e)), e = B(e));

  (*c)->lams = lam;
  f = append(gxl(gpush(f, 3, f->lambda, nom, exp)));

  if (!g_ok(f)) return forget();
  exp = gpop1(f);

  size_t ll = llen(nom);
  f = ll > 1 ? gpush(f, 2, gcataapn, gputnum(ll)) :
               gpush(f, 1, gcataap);
  f = gx2(f, g_nil, (*b)->stack); // push function stack rep
  (*b)->stack = gpop1(f);
  nom = reverse(f, nom); // put in literal order
  for (v = nom; g_ok(f) && twop(B(v)); v = B(v)) { // push initial variable stack reps
    f = gx2(f, A(v), (*b)->stack);
    if (g_ok(f)) (*b)->stack = gpop1(f); }


  nom = reverse(f, nom); // back to reverse order
  for (; twop(nom); nom = B(nom), def = B(def)) {
    if (lambp(f, A(def))) {
      d = assq(f, lam, A(nom));
      f = ganalambda(f, c, BB(d), BA(def));
      if (!g_ok(f)) return forget();
      A(def) = B(d) = gpop1(f); }
    if (even && nilp((*b)->args)) {
      f = ganaix(f, gvmdefglob, A(nom));
      if (!g_ok(f)) return forget(); }
    f = analyze(f, b, A(def));
    (*b)->stack = B((*b)->stack); }

  f = analyze(f, b, exp);
  return forget(); }


static g_vm(gvmdefglob) {
  Have(3);
  Sp -= 3;
  struct g_tab *t = dict_of(f);
  intptr_t k = Ip[1].x,
           v = Sp[3];
  Sp[0] = k;
  Sp[1] = v;
  Sp[2] = (intptr_t) t;
  Pack(f);
  f = gtabput(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Sp += 1;
  Ip += 2;
  return Continue(); }

static g_vm(gvmdrop1) {
 Ip += 1;
 Sp += 1;
 return Continue(); }

static g_vm(gvmfreev) {
 intptr_t y = Ip[1].x,
          v = gtabget(f, y, f->dict, y); // see if it's defined now...
 Ip[0].ap = gvmpushk;
 Ip[1].x = v;
 return Continue(); }

static g_vm(gvmlazyb) {
 intptr_t v = AB(Ip[1].x);
 Ip[0].ap = gvmpushk;
 Ip[1].x = v;
 return Continue(); }

static g_vm(gvmdata) {
 intptr_t x = word(Ip);
 Sp += 1;
 Ip = cell(Sp[0]);
 Sp[0] = x;
 return Continue(); }


// push a value from the stack
g_vm(gvmpushr) {
 Have1();
 Sp[-1] = Sp[ggetnum(Ip[1].x)];
 Sp -= 1;
 Ip += 2;
 return Continue(); }

// call and return
// apply function to one argument
g_vm(gvmap) {
 union u *k;
 if (odd(Sp[1])) Ip++, Sp++;
 else k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 return Continue(); }

// tail call
g_vm(gvmtap) {
 intptr_t x = Sp[0], j = Sp[1];
 Sp += ggetnum(Ip[1].x) + 1;
 if (even(j)) Ip = cell(j), Sp[0] = x;
 else Sp += 1, Ip = cell(Sp[0]), Sp[0] = j;
 return Continue(); }

// apply to multiple arguments
g_vm(gvmapn) {
  size_t n = ggetnum(Ip[1].x);
  union u*ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = cell(Sp[n]);
  Ip += 2;
  Sp[n] = word(ra); // store return address
  return Continue(); }

// tail call
g_vm(gvmtapn) {
 size_t n = ggetnum(Ip[1].x),
        r = ggetnum(Ip[2].x);
 Ip = cell(Sp[n]) + 2;
 intptr_t *o = Sp;
 for (Sp += r + 1; n--; Sp[n] = o[n]);
 return Continue(); }

// return
g_vm(gvmret) {
 intptr_t n = ggetnum(Ip[1].x) + 1;
 Ip = cell(Sp[n]);
 Sp[n] = Sp[0];
 Sp += n;
 return Continue(); }

g_vm(gvmret0) {
 Ip = cell(Sp[1]);
 Sp[1] = Sp[0];
 Sp += 1;
 return Continue(); }

#define op(nom, n, x) g_vm(nom) { intptr_t _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
op(gvmadd, 2, (Sp[0]+Sp[1]-1)|1)
op(gvmsub, 2, (Sp[0]-Sp[1])|1)
op(gvmmul, 2, gputnum(ggetnum(Sp[0])*ggetnum(Sp[1])))
op(gvmquot, 2, nilp(Sp[1]) ? g_nil : gputnum(ggetnum(Sp[0])/ggetnum(Sp[1])))
op(gvmrem, 2, nilp(Sp[1]) ? g_nil : gputnum(ggetnum(Sp[0])%ggetnum(Sp[1])))
op(gvmeq, 2, eql(f, Sp[0], Sp[1]) ? gputnum(-1) : g_nil)
op(gvmlt, 2, Sp[0] < Sp[1] ? gputnum(-1) : g_nil)
op(gvmle, 2, Sp[0] <= Sp[1] ? gputnum(-1) : g_nil)
op(gvmgt, 2, Sp[0] > Sp[1] ? gputnum(-1) : g_nil)
op(gvmge, 2, Sp[0] >= Sp[1] ? gputnum(-1) : g_nil)
op(gvmbnot, 1, ~Sp[0] | 1)
op(gvmband, 2, (Sp[0] & Sp[1]) | 1)
op(gvmbor, 2, (Sp[0] | Sp[1]) | 1)
op(gvmbxor, 2, (Sp[0] ^ Sp[1]) | 1)
op(gvmbsr, 2, gputnum(ggetnum(Sp[0]) >> ggetnum(Sp[1])))
op(gvmbsl, 2, gputnum(ggetnum(Sp[0]) << ggetnum(Sp[1])))
op(gvmnump, 1, odd(Sp[0]) ? gputnum(-1) : g_nil)

struct g *gtabnew(struct g*f) {
  f = g_have(f, Width(struct g_tab) + 2);
  if (g_ok(f)) {
    struct g_tab *t = bump(f, Width(struct g_tab) + 1);
    *--f->sp = word(t);
    struct g_kvs **tab = (struct g_kvs**) (t + 1);
    tab[0] = 0;
    tabini(t, 0, 1, tab); }
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
    intptr_t len = (union u*) t - t->head;
    return mix ^ (mix * len); }
  else switch (typ(x)) {
    case g_ty_two: return mix ^ (hash(f, A(x)) * hash(f, B(x)));
    case g_ty_nom: return sym(x)->code;
    case g_ty_tbl: return mix;
    case g_ty_str: default: {
      uintptr_t len = vec_bytes(_), h = 2166136261;
      for (uint8_t *bs = _; len--; h ^= *bs++, h *= 16777619);
      return h; } } }


// relies on table capacity being a power of 2
static g_inline uintptr_t index_of_key(struct g *f, struct g_tab *t, intptr_t k) {
 return (t->cap - 1) & hash(f, k); }

g_noinline struct g *gtabput(struct g *f) {
  struct g_tab *t = (struct g_tab*) f->sp[2];
  intptr_t v = f->sp[1],
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
      i = (cap1-1) & hash(f, e->key),
      e->next = tab1[i],
      tab1[i] = e);

  return f->sp += 2, f; }

  
static struct g_kvs *gtabdelr(
 struct g *f, struct g_tab *t, intptr_t k, intptr_t *v, struct g_kvs *e) {
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
      i = (cap - 1) & hash(f, coll->key),
      x = coll->next,
      coll->next = t->tab[i],
      t->tab[i] = coll,
      coll = x; }
  return v; }

g_vm(gvmtabnew) {
  Have(Width(struct g_tab) + 1);
  struct g_tab *t = (struct g_tab*) Hp;
  struct g_kvs **tab = (struct g_kvs**) (t + 1);
  Hp += Width(struct g_tab) + 1;
  tab[0] = 0;
  tabini(t, 0, 1, tab);
  Sp[0] = (intptr_t) t;
  Ip++;
  return Continue(); }

intptr_t gtabget(struct g *f, intptr_t zero, struct g_tab *t, intptr_t k) {
  uintptr_t i = index_of_key(f, t, k);
  struct g_kvs *e = t->tab[i];
  while (e && !eql(f, k, e->key)) e = e->next;
  return e ? e->val : zero; }

g_vm(gvmtabget) {
 Sp[2] = gtabget(f, Sp[0], tbl(Sp[1]), Sp[2]);
 Sp += 2;
 Ip += 1;
 return Continue(); }

g_vm(gvmtset) {
  if (tabp(Sp[0])) {
    intptr_t t = Sp[0],
             k = Sp[1],
             v = Sp[2];
    Sp[0] = k;
    Sp[1] = v;
    Sp[2] = t;
    Pack(f);
    f = gtabput(f);
    if (!g_ok(f)) return f;
    Unpack(f); }
  return Ip += 1,
         Continue(); }

g_vm(gvmtabdel) {
 if (tabp(Sp[1])) Sp[2] = gtabdel(f, (struct g_tab*) Sp[1], Sp[2], Sp[0]);
 Sp += 2;
 Ip += 1;
 return Continue(); }

g_vm(gvmtablen) {
 Sp[0] = tabp(Sp[0]) ? gputnum(((struct g_tab*)Sp[0])->len) : g_nil;
 Ip += 1;
 return Continue(); }

g_vm(gvmtabkeys) {
  intptr_t list = g_nil;
  if (tabp(Sp[0])) {
    struct g_tab *t = (struct g_tab*) Sp[0];
    intptr_t len = t->len;
    Have(len * Width(struct g_pair));
    struct g_pair *pairs = (struct g_pair*) Hp;
    Hp += len * Width(struct g_pair);
    for (uintptr_t i = t->cap; i;)
      for (struct g_kvs *e = t->tab[--i]; e; e = e->next)
        twoini(pairs, e->key, list),
        list = (intptr_t) pairs, pairs++; }
  Sp[0] = list;
  Ip += 1;
  return Continue(); }

static g_noinline struct g_atom *gintern_r(struct g *v, struct g_vec *b, struct g_atom **y) {
  struct g_atom *z = *y;
  if (!z) return // found an empty spot, insert new symbol
    z = bump(v, Width(struct g_atom)),
    nomini(z, b, hash(v, gputnum(hash(v, (intptr_t) b)))),
    *y = z;
  struct g_vec *a = z->nom;
  // compare operation should be its own thing
  int i = len(a) < len(b) ? -1 :
          len(a) > len(b) ? 1 :
          strncmp(txt(a), txt(b), len(a));
  return i == 0 ? z :
    gintern_r(v, b, i < 0 ? &z->l : &z->r); }


static g_vm(nomsym) {
  Have(Width(struct g_atom));
  struct g_atom *y;
  Pack(f);
  y = gintern_r(f, (struct g_vec*) f->sp[0], &f->symbols),
  Unpack(f);
  Sp[0] = word(y);
  Ip += 1;
  return Continue(); }

static g_vm(gensym) {
 if (g_strp(Sp[0])) return Ap(nomsym, f);
 const uintptr_t req = Width(struct g_atom) - 2;
 Have(req);
 struct g_atom *y = (struct g_atom*) Hp;
 Hp += req;
 inianon(y, g_clock());
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }

static g_vm(symnom) {
 intptr_t y = Sp[0];
 y = nomp(y) && sym(y)->nom ? word(sym(y)->nom) : g_nil;
 Sp[0] = y;
 Ip += 1;
 return Continue(); }

static g_vm(gvmnomp) {
 Sp[0] = nomp(Sp[0]) ? gputnum(-1) : g_nil;
 Ip += 1;
 return Continue(); }

static g_vm(gvmtabp) {
 Sp[0] = tabp(Sp[0]) ? gputnum(-1) : g_nil;
 Ip += 1;
 return Continue(); }

static g_vm(gvmslen) {
 Sp[0] = g_strp(Sp[0]) ? gputnum(len(Sp[0])) : g_nil;
 Ip += 1;
 return Continue(); }

static g_vm(gvmssub) {
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

static g_vm(gvmsget) {
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

static g_vm(gvmscat) {
  intptr_t a = Sp[0], b = Sp[1];
  if (!g_strp(a)) return Sp += 1,
                       Ip += 1,
                       Continue();
  if (!g_strp(b)) return Sp[1] = a,
                       Sp += 1,
                       Ip += 1,
                       Continue();

  struct g_vec *x = vec(a),
               *y = vec(b),
               *z;
  uintptr_t len = len(x) + len(y),
            req = str_type_width + b2w(len);
  Have(req);
  z = (struct g_vec*) Hp;
  Hp += req;
  inistr(z, len);
  memcpy(txt(z), txt(x), len(x));
  memcpy(txt(z) + len(x), txt(y), len(y));
  Sp[1] = word(z);
  Ip += 1;
  return Continue(); }

static g_vm(gvmstringp) {
 Sp[0] = g_strp(Sp[0]) ? gputnum(-1) : g_nil;
 Ip += 1;
 return Continue(); }

static g_vm(gvmcar) {
 Sp[0] = twop(Sp[0]) ? A(Sp[0]) : Sp[0];
 Ip++;
 return Continue(); }
static g_vm(gvmcdr) {
 Sp[0] = twop(Sp[0]) ? B(Sp[0]) : g_nil;
 Ip++;
 return Continue(); }

static g_vm(gvmcons) {
 Have(Width(struct g_pair));
 struct g_pair *w = (struct g_pair*) Hp;
 twoini(w, Sp[0], Sp[1]);
 Hp += Width(struct g_pair);
 Sp[1] = word(w);
 Sp++;
 Ip++;
 return Continue(); }

static g_vm(gvmtwop) {
 Sp[0] = twop(Sp[0]) ? gputnum(-1) : g_nil;
 Ip++;
 return Continue(); }

static g_noinline bool eql_cont(struct g *f, intptr_t a, intptr_t b) {
 if (!(even(a | b) && cell(a)->ap == gvmdata && cell(b)->ap == gvmdata && typ(a) == typ(b))) return false;
 else switch (typ(a)) {
  default: return false;
// FIXME could overflow the stack -- use off pool for this
  case g_ty_two: return eql(f, A(a), A(b)) && eql(f, B(a), B(b));
  case g_ty_str: {
   struct g_vec *x = vec(a), *y = vec(b);
   return 0 == memcmp(x, y, vec_bytes(x)); } } }

static g_inline bool eql(struct g *f, intptr_t a, intptr_t b) {
 return a == b || eql_cont(f, a, b); }

static struct g *ggccpg(struct g*g, intptr_t *p1, uintptr_t len1, struct g *f) {
  memcpy(g, f, sizeof(struct g));
  g->pool = (void*) p1;
  g->len = len1;
  uintptr_t len0 = f->len;
  g_word
    *p0 = ptr(f),
    *tp0 = ptr(f) + len0, // source top
    *sp0 = f->sp,
    h = tp0 - sp0; // stack height
  g->sp = ptr(g) + len1 - h;
  g->hp = g->cp = g->end;
  g->ip = cell(ggccp(g, word(g->ip), p0, tp0));
  g->symbols = 0;
  for (uintptr_t i = 0; i < g_nvars; i++)
    g->v[i] = ggccp(g, g->v[i], p0, tp0);
  for (intptr_t n = 0; n < h; n++)
    g->sp[n] = ggccp(g, sp0[n], p0, tp0);
  for (struct g_root *s = g->root; s; s = s->next)
    *s->ptr = ggccp(g, *s->ptr, p0, tp0);
  while (g->cp < g->hp)
    gc_walk(g, p0, tp0);
  return g->t0 = g_clock(), g; }


static g_noinline struct g *please(struct g *f, uintptr_t req0) {
  uintptr_t t0 = f->t0,
            t1 = g_clock(),
            len0 = f->len;
  intptr_t *w = (intptr_t*) f,
           *w0 = (void*) f->pool;
  struct g *g = (struct g*) (w == w0 ? w0 + len0 : w0);
  f = ggccpg(g, (void*) f->pool, f->len, f);
  // keep v between
#define v_lo 4
  // and
#define v_hi 8
  // where
  //   v = (t2 - t0) / (t2 - t1)
  //       non-gc running time     t1    t2
  //   ,.........................,/      |
  //   -----------------------------------
  //   |                          `------'
  //   t0                  gc time (this cycle)
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
  g = f->malloc(len1 * 2 * sizeof(g_num), f);
  if (!g) return g_enc(f, req <= len0 ? g_status_ok : g_status_oom);
  g = ggccpg(g, (g_num*) g, len1, f);
  f->free(f->pool, f);
  return g; }

static void gc_walk(struct g*g, intptr_t *p0, intptr_t *t0) {
  if (!datp(g->cp)) for (g->cp += 2; g->cp[-2]; g->cp++)
    g->cp[-2] = ggccp(g, g->cp[-2], p0, t0);
  else switch (typ(g->cp)) {
    case g_ty_str: default:
      g->cp += b2w(vec_bytes((struct g_vec*) g->cp));
      return;
    case g_ty_nom:
      g->cp += Width(struct g_atom) - (((struct g_atom*) g->cp)->nom ? 0 : 2);
      return;
    case g_ty_two: {
      struct g_pair *w = (void*) g->cp;
      g->cp += Width(struct g_pair);
      w->a = ggccp(g, w->a, p0, t0);
      w->b = ggccp(g, w->b, p0, t0);
      return; }
    case g_ty_tbl: {
      struct g_tab *t = (void*) g->cp;
      g->cp += Width(struct g_tab) + t->cap + t->len * Width(struct g_kvs);
      for (intptr_t i = 0, lim = t->cap; i < lim; i++)
        for (struct g_kvs*e = t->tab[i]; e;
          e->key = ggccp(g, e->key, p0, t0),
          e->val = ggccp(g, e->val, p0, t0),
          e = e->next); } } }

static g_noinline intptr_t ggccp(struct g *f, intptr_t x, intptr_t *p0, intptr_t *t0) {
  // if it's a number or it's outside managed memory then return it
  if (odd(x) || !within(p0, x, t0)) return x;
  union u *src = cell(x);
  x = src->x; // get its contents
  // if it contains a pointer to the new space then return the pointer
  if (!odd(x) && within(ptr(f), x, ptr(f) + f->len)) return x;
  // if it's data then call the copy function
  if (x != (intptr_t) gvmdata) {
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
  switch (typ(src)) {
    case g_ty_two: {
      struct g_pair *src = two(x),
                    *dst = bump(f, Width(struct g_pair));
      twoini(dst, src->a, src->b);
      src->ap = (g_vm_t*) dst;
      return word(dst); }
    case g_ty_nom: {
      struct g_atom *src = sym(x), *dst;
      if (src->nom) dst = gintern_r(f, (struct g_vec*) ggccp(f, word(src->nom), p0, t0), &f->symbols);
      else dst = bump(f, Width(struct g_atom) - 2),
           inianon(dst, src->code);
      return (intptr_t) (src->ap = (g_vm_t*) dst); }
    case g_ty_str: default: {
      struct g_vec *src = vec(x);
      uintptr_t bytes = vec_bytes(src);
      struct g_vec *dst = bump(f, b2w(bytes));
      src->ap = memcpy(dst, src, bytes);
      return word(dst); }
    case g_ty_tbl: {
      struct g_tab *src = tbl(x);
      uintptr_t len = src->len, cap = src->cap;
      struct g_tab *dst = bump(f, Width(struct g_tab) + cap + Width(struct g_kvs) * len);
      struct g_kvs **tab = (struct g_kvs**) (dst + 1),
                   *dd = (struct g_kvs*) (tab + cap);
      tabini(dst, len, cap, tab);
      src->ap = (g_vm_t*) dst;
      for (struct g_kvs *d, *s, *last; cap--; tab[cap] = last)
        for (s = src->tab[cap], last = NULL; s;
          d = dd++,
          d->key = s->key,
          d->val = s->val,
          d->next = last,
          last = d,
          s = s->next);
      return word(dst); } } }

static g_vm(gvmclock) {
  Sp[0] = gputnum(g_clock());
  Ip += 1;
  return Continue(); }

static g_vm(gvmnilp) {
  Sp[0] = nilp(Sp[0]) ? gputnum(-1) : g_nil;
  Ip += 1;
  return Continue(); }

static g_inline int gfputc(struct g*f, int c, struct g_out *o) {
  return o->putc(f, c, o); }

static g_vm(gvmputc) {
  gfputc(f, ggetnum(*Sp), &g_stdout);
  Ip += 1;
  return Continue(); }

static g_vm(gvminfo) {
  const size_t req = 4 * Width(struct g_pair);
  Have(req);
  struct g_pair *si = (struct g_pair*) Hp;
  Hp += req;
  Sp[0] = word(si);
  twoini(si, gputnum(f), word(si + 1));
  twoini(si + 1, gputnum(f->len), word(si + 2));
  twoini(si + 2, gputnum(Hp - ptr(f)), word(si + 3));
  twoini(si + 3, gputnum(ptr(f) + f->len - Sp), g_nil);
  Ip += 1;
  return Continue(); }

static int gfputn(struct g*, struct g_out*, intptr_t, uintptr_t);
static g_vm(putn) {
  uintptr_t n = ggetnum(Sp[0]), b = ggetnum(Sp[1]);
  gfputn(f, &g_stdout, n, b);
  Sp[1] = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

static g_vm(gputs) {
  if (g_strp(Sp[0])) {
    struct g_vec *s = vec(Sp[0]);
    for (intptr_t i = 0; i < len(s); gfputc(f, txt(s)[i++], &g_stdout));
    gflush(f); }
  return Ip += 1,
         Continue(); }

static g_vm(gvmgetc) {
  Pack(f);
  int i = ggetc(f);
  Unpack(f);
  Sp[0] = gputnum(i);
  Ip += 1;
  return Continue(); }

static g_vm(gvmread) {
 Pack(f);
 f = gread1i(f, &g_stdin);
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

#define S1(i) {{i}, {gvmret0}}
#define S2(i) {{gvmcurry},{.x=gputnum(2)},{i}, {gvmret0}}
#define S3(i) {{gvmcurry},{.x=gputnum(3)},{i}, {gvmret0}}
#define bifs(_) \
  _(bif_clock, "clock", S1(gvmclock)) _(bif_addr, "gvminfo", S1(gvminfo))\
  _(bif_add, "+", S2(gvmadd)) _(bif_sub, "-", S2(gvmsub)) _(bif_mul, "*", S2(gvmmul)) _(bif_quot, "/", S2(gvmquot)) _(bif_rem, "%", S2(gvmrem)) \
  _(bif_lt, "<", S2(gvmlt))  _(bif_le, "<=", S2(gvmle)) _(bif_eq, "=", S2(gvmeq)) _(bif_ge, ">=", S2(gvmge))  _(bif_gt, ">", S2(gvmgt)) \
  _(bif_bnot, "~", S1(gvmbnot)) _(bif_bsl, "<<", S2(gvmbsl)) _(bif_bsr, ">>", S2(gvmbsr))\
  _(bif_band, "&", S2(gvmband)) _(bif_bor, "|", S2(gvmbor)) _(bif_bxor, "^", S2(gvmbxor))\
  _(bif_cons, "X", S2(gvmcons)) _(bif_gvmcar, "A", S1(gvmcar)) _(bif_gvmcdr, "B", S1(gvmcdr)) \
  _(bif_sget, "sget", S2(gvmsget)) _(bif_ssub, "ssub", S3(gvmssub)) _(bif_slen, "slen", S1(gvmslen)) _(bif_scat, "scat", S2(gvmscat)) \
  _(bif_gvmdot, ".", S1(gvmdot)) _(bif_read, "read", S1(gvmread)) _(bif_getc, "getc", S1(gvmgetc))\
  _(bif_putc, "putc", S1(gvmputc)) _(bif_prn, "putn", S2(putn)) _(bif_puts, "puts", S1(gputs))\
  _(bif_sym, "sym", S1(gensym)) _(bif_nom, "nom", S1(symnom)) _(bif_thd, "thd", S1(thda)) _(bif_peek, "peek", S1(peek)) _(bif_poke, "poke", S2(poke)) _(bif_trim, "trim", S1(trim)) _(bif_seek, "seek", S2(seek)) \
  _(bif_tabnew, "tnew", S1(gvmtabnew)) _(bif_tabkeys, "tkeys", S1(gvmtabkeys)) _(bif_tablen, "tlen", S1(gvmtablen)) _(bif_tset, "tset", S3(gvmtset)) _(bif_tabget, "tget", S3(gvmtabget)) _(bif_tabdel, "tdel", S3(gvmtabdel))\
  _(bif_twop, "twop", S1(gvmtwop)) _(bif_strp, "strp", S1(gvmstringp)) _(bif_nomp, "nomp", S1(gvmnomp)) _(bif_tabp, "tabp", S1(gvmtabp)) _(bif_nump, "nump", S1(gvmnump)) _(bif_nilp, "nilp", S1(gvmnilp))\
  _(bif_ev, "ev", S1(gvmeval))
#define built_in_function(n, _, d) static const union u n[] = d;
bifs(built_in_function);
#define insts(_) _(gvmfreev) _(gvmret) _(gvmap) _(gvmtap) _(gvmapn) _(gvmtapn) _(gvmjump) _(gvmcond) _(gvmpushr) _(gvmpushk) _(gvmdrop1) _(gvmcurry) _(gvmdefglob) _(gvmlazyb) _(gvmret0)
#define biff(b, n, _) {n, (intptr_t) b},
#define i_entry(i) {#i, (intptr_t) i},

static struct g *gnoms(const char *nom, struct g *f) {
  return gintern(gstrof(f, nom)); }

static g_vm(gvmyield) { return Pack(f), f; }
static union u yield[] = { {gvmyield} };
static const struct g_def g_defs0[] = { bifs(biff) insts(i_entry) };
// this is the general initialization function. arguments are
// - f: core pointer
// - len0: initial semispace size in words (== total_space_size / 2)
// - ma: malloc function pointer
// - fr: free function pointer
static struct g *gini0(
    struct g *restrict f,
    uintptr_t words,
    void *(*ma)(size_t, struct g*),
    void (*fr)(void*, struct g*)) {
  if (!g_ok(f)) return f;
  if (f == NULL || words * sizeof(g_word) < 2 * sizeof(struct g))
   return g_enc(f, g_status_oom);
  memset(f, 0, sizeof(struct g));
  f->len = words;
  f->pool = (void*) f;
  f->malloc = ma;
  f->free = fr;
  f->hp = f->end;
  f->sp = (intptr_t*) f + words;
  f->ip = yield;
  f->t0 = g_clock(); // this goes right before first allocation so gc always sees initialized t0
  f = gtabnew(gtabnew(f)); // dict and macro tables
  f = gnoms(":", gnoms("?", gnoms("`", gnoms(",", gnoms("\\", gnoms("ev", f))))));
  if (g_ok(f)) { // these must be in reverse order from above
    f->let = nom(gpop1(f));
    f->cond = nom(gpop1(f));
    f->quote = nom(gpop1(f));
    f->begin = nom(gpop1(f));
    f->lambda = nom(gpop1(f));
    f->eval = nom(gpop1(f));
    f->macro = tbl(gpop1(f));
    f->dict = tbl(gpop1(f));
    f = gdefs(f, LEN(g_defs0), (void*) g_defs0);
    struct g_def defs[] = {
      {"globals", (intptr_t) f->dict, },
      {"macros", (intptr_t) f->macro, }, };
    f = gdefs(f, 2, defs); }
  return f; }

static int gfputn(struct g *f, struct g_out *o, intptr_t n, uintptr_t base) {
 if (n < 0) gfputc(f, '-', o), n = -n;
 uintptr_t q = n / base, r = n % base;
 if (q) gfputn(f, o, q, base);
 return gfputc(f, g_digits[r], o); }

static int gfputx(struct g *f, struct g_out *o, intptr_t x) {
 if (odd(x)) return gfprintf(f, o, "%d", (g_num) ggetnum(x));
 if (!datp(x)) return gfprintf(f, o, "#%lx", (long) x);
 int r = 0;
 switch (typ(x)) {
  case g_ty_two:
   if (A(x) == word(f->quote) && twop(B(x))) return
    gfputc(f, '\'', o),
    gfputx(f, o, AB(x));
   for (gfputc(f, '(', o);; gfputc(f, ' ', o)) {
    gfputx(f, o, A(x));
    if (!twop(x = B(x))) return gfputc(f, ')', o); }
  case g_ty_tbl: {
   struct g_tab *t = tbl(x);
   return gfprintf(f, o, "#tab:%d/%d@%x", t->len, t->cap, x); }
  case g_ty_nom: {
   struct g_vec * s = sym(x)->nom;
   if (s && vec_strp(s)) for (intptr_t i = 0; i < len(s); r = gfputc(f, txt(s)[i++], o));
   else r = gfprintf(f, o, "#sym@%x", (intptr_t) x);
   return r; }
  case g_ty_str: default: {
   struct g_vec *v = vec(x);
   if (!vec_strp(v)) {
    intptr_t rank = v->rank, *shape = v->shape;
    r = gfprintf(f, o, "#vec@%x:%d.%d", (intptr_t) x, (intptr_t) v->type, (intptr_t) v->rank);
    for (intptr_t i = rank, *j = shape; i--; r = gfprintf(f, o, ".%d", (intptr_t) *j++)); }
   else {
    uintptr_t len = vlen(v);
    char *text = vtxt(v);
    gfputc(f, '"', o);
    for (char c; len--; gfputc(f, c, o))
     if ((c = *text++) == '\\' || c == '"') gfputc(f, '\\', o);
    r = gfputc(f, '"', o); }
   return r; } } }

static g_vm(seek) {
 Sp[1] = word(cell(Sp[1]) + ggetnum(Sp[0]));
 Sp += 1;
 Ip += 1;
 return Continue(); }

static g_vm(peek) {
 Sp[0] = cell(Sp[0])->x;
 Ip += 1;
 return Continue(); }

static g_vm(poke) {
 cell(Sp[1])->x = Sp[0];
 Sp += 1;
 Ip += 1;
 return Continue(); }

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
 ttag(k)->head = k;
 Ip += 1;
 return Continue(); }

struct g_in g_stdin = {
 (void*) ggetc,
 (void*) gungetc,
 (void*) geof };
struct g_out g_stdout = {
 (void*) gputc,
 (void*) gflush };

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

struct g *gstrof(struct g *f, const char *cs) {
 uintptr_t len = strlen(cs);
 f = g_vec0(f, g_vt_char, 1, len);
 if (g_ok(f)) memcpy(txt(f->sp[0]), cs, len);
 return f; }

struct g *ginid(void *(*ma)(size_t, struct g*), void (*fr)(void*, struct g*)) {
 uintptr_t len0 = 1 << 10;
 struct g *f = ma(2 * len0 * sizeof(g_word), NULL);
 return gini0(f, len0, ma, fr); }

struct ti { struct g_in in; const char *t; uintptr_t i; } ;
#define ti(x) ((struct ti*)(x))

static int p_text_eof(struct g*f, struct g_in *i) {
 return !ti(i)->t[ti(i)->i]; }

static int p_text_getc(struct g*f, struct g_in *i) {
 char c = ti(i)->t[ti(i)->i];
 if (c) ti(i)->i++;
 return c ? c : EOF; }

static int p_text_ungetc(struct g*f, int _, struct g_in *i) {
 uintptr_t idx = ti(i)->i;
 return ti(i)->t[ti(i)->i = idx ? idx - 1 : idx]; }

static g_noinline struct g *greads(struct g *f, const char*s) {
 struct ti t = {{p_text_getc, p_text_ungetc, p_text_eof}, s, 0};
 return greadsi(f, (void*) &t); }

int gfwrite1(struct g *f, struct g_out *o) {
 if (!g_ok(f)) return EOF;
 return gfputx(f, o, f->sp[0]); }

struct g *gini(void) {
 return ginid((void*) malloc, (void*) free); }

struct g *gdef1(struct g*f, const char*s) {
 if (!g_ok(f)) return f;
 struct g_def d = {s, gpop1(f)};
 return gdefs(f, 1, &d); }

struct g *gdefs(struct g*f, uintptr_t n, struct g_def *defs) {
 if (!g_ok(f)) return f;
 f = gpush(f, 1, f->dict);
 while (n--)
  f = gtabput(gintern(gstrof(gpush(f, 1, defs[n].x), defs[n].n)));
 return f; }

struct g *gpush(struct g *f, uintptr_t m, ...) {
 if (!g_ok(f)) return f;
 va_list xs;
 va_start(xs, m);
 uintptr_t n = 0;
 if (avail(f) < m) f = gpushr(f, m, n, xs);
 else for (f->sp -= m; n < m; f->sp[n++] = va_arg(xs, intptr_t));
 va_end(xs);
 return f; }

// don't inline this so callers can tail call optimize
static g_noinline struct g *gana(struct g *f, g_vm_t *y) {
 f = enscope(f, (struct env*) g_nil, g_nil, g_nil);
 if (!g_ok(f)) return f;
 struct env *c = (struct env*) ptr(gpop1(f));
 intptr_t x = f->sp[0];
 f->sp[0] = (intptr_t) gcatayield;
 avec(f, c,
  avec(f, x, f = ganaix(f, y, word(f->ip))),
  f = analyze(f, &c, x),
  f = pull(f, &c, 0));
 return f; }

int gvfprintf(struct g*f, struct g_out*o, const char *fmt, va_list xs) {
 int c;
 while ((c = *fmt++)) {
  if (c != '%') gfputc(f, c, o);
  else pass: switch ((c = *fmt++)) {
   case 0: return c;
   case 'l': goto pass;
   case 'b': c = gfputn(f, o, va_arg(xs, uintptr_t), 2); continue;
   case 'o': c = gfputn(f, o, va_arg(xs, uintptr_t), 8); continue;
   case 'd': c = gfputn(f, o, va_arg(xs, uintptr_t), 10); continue;
   case 'x': c = gfputn(f, o, va_arg(xs, uintptr_t), 16); continue;
   default: c = gfputc(f, c, o); } }
 return c; }

int gfprintf(struct g *f, struct g_out *o, const char*fmt, ...) {
 va_list xs;
 va_start(xs, fmt);
 int r = gvfprintf(f, o, fmt, xs);
 va_end(xs);
 return r; }

struct g *gevals(struct g *f, const char *s) {
 f = geval(greads(f, "((:(e a b)(? b(e(ev'ev(A b))(B b))a)e)0)"));
 f = gpush(f, 3, g_nil, g_ok(f) ? f->quote : NULL, g_nil);
 return geval(gxr(gxl(gxr(gxl(greads(f, s)))))); }

static struct g *g_lat = NULL;
enum g_status glisp(char const *p, ...) {
 g_lat = !g_lat || !g_ok(g_lat) ? (gfin(g_lat), gini()) : g_lat;
 va_list xs;
 va_start(xs, p);
 struct g_def defs[1] = { {p, va_arg(xs, g_num)} };
 g_lat = p ?
  gdefs(g_lat, 1, defs) :
  gevals_(g_lat, va_arg(xs, char const*));
 return g_code_of(g_lat); }
