// thanks !!
#include "gw.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

struct g_core {
  g_word *hp, *sp;
  union {
    struct {
      union g_cell *ip;
      struct g_table *dict, *macro;
      struct g_symbol *quote, *begin, *let, *cond, *lambda; };
    g_word vars[g_var_N]; };

  // symbol tree
  struct g_symbol *symbols;

  // memory management
  void *(*malloc)(g_core*, size_t),
       (*free)(g_core*, void*);
  uintptr_t len; // memory pool size
  g_word *pool;
  struct root { g_word *ptr; struct root *next; } *safe;
  union { uintptr_t t0; g_word *cp; }; // gc copy pointer
  struct dtor {
    g_word x;
    void (*d)(g_core*, g_word);
    struct dtor *next; } *dtors;
  g_word end[]; };

// built in type method tables
struct g_type {
  g_word (*cp)(g_core*, g_word, g_word*, g_word*); // for gc
  void (*wk)(g_core*, g_word, g_word*, g_word*);
  bool (*eq)(g_core*, g_word, g_word);
  void (*em)(g_core*, FILE*, g_word);
  uintptr_t (*xx)(g_core*, g_word);
  g_core (*show)(g_core*, g_word); };

typedef struct g_pair {
  g_vm *ap;
  g_type *typ;
  g_word a, b;
} g_pair, pair;

typedef struct g_symbol {
  g_vm *ap;
  g_type *typ;
  struct g_string *nom;
  uintptr_t code;
  struct g_symbol *l, *r;
} g_symbol, symbol;

typedef struct g_table {
  g_vm *ap;
  g_type *typ;
  uintptr_t len, cap;
  struct entry { g_word key, val; struct entry *next; } **tab;
} g_table, table;

typedef struct g_string {
  g_vm *ap;
  g_type *typ;
  uintptr_t len;
  char text[];
} g_string, string;


void
  g_write1f(g_core*, FILE*),
  *g_malloc(g_core*, size_t),
  g_free(g_core*, void*),
  transmit(g_core*, FILE*, g_word);
bool
  neql(g_core*, g_word, g_word),
  eql(g_core*, g_word, g_word);
uintptr_t
  hash(g_core*, g_word),
  g_clock(void);
g_word
  cp(g_core*, g_word, g_word*, g_word*),
  g_hash_get(g_core*, g_word, g_table*, g_word);
g_core
  *g_read1f(g_core*, FILE*),
  *g_have(g_core*, uintptr_t),
  *g_cells(g_core*, size_t),
  *g_push(g_core*, uintptr_t, ...),
  *g_cons_l(g_core*),
  *g_cons_r(g_core*),
  *g_ini_m(void *(*)(g_core*, size_t), void (*)(g_core*, void*)),
  *g_intern(g_core*),
  *g_hash_put(g_core*),
  *g_hash_put_2(g_core*),
  *g_ana(g_core*, g_vm*),
  *p_readsp(g_core*, g_string*);

g_vm
  data, bnot, rng, nullp, sysclock, symnom, dot,
  gensym, pairp, fixnump, symbolp, stringp,
  ssub, sget, slen, scat, prc, cons, car, cdr,
  lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
  seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
  read0, readf, p_isatty, g_yield, defglob, drop1, imm, ref,
  free_variable, curry, ev0, ret0,
  cond, jump, ap, tap, apl, apn, tapn, ret, late_bind;

#define Vm(n, ...) g_core *n(g_core *f, g_cell* Ip, g_word* Hp, g_word* Sp, ##__VA_ARGS__)
Vm(gc, uintptr_t);

#define putnum(_) (((g_word)(_)<<1)|1)
#define getnum(_) ((g_word)(_)>>1)
#define nil putnum(0)

_Static_assert(-1 >> 1 == -1, "sign extended shift");
_Static_assert(sizeof(g_cell) == sizeof(g_cell*));
_Static_assert(nil == g_nil);

#define encode(f, s) ((g_core*)((g_word)(f)|(s)))
#define core_of g_core_of
#define code_of g_code_of

#define min(a, b) ((a)<(b)?(a):(b))

#define str(x) ((g_string*)(x))
#define sym(x) ((g_symbol*)(x))
#define two(x) ((g_pair*)(x))
#define cell(x) ((g_cell*)(x))
#define tbl(x) ((g_table*)(x))
#define word(x) ((g_word)(x))
#define Width(_) b2w(sizeof(_))
#define within(a, b, c) (word(a)<=word(b)&&word(b)<word(c))
#define owns(f, x) within(f->pool, x, f->pool + f->len)
#define datp(_) (cell(_)->ap==data)
#define typ(_) cell(_)[1].typ
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->safe=&((struct root){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define pop1(f) (*(f)->sp++)
#define push1(f, x) (*--(f)->sp=(x))
#define nump(_) (word(_)&1)
#define celp(_) (!nump(_))
#define homp celp

#define nilp(_) (word(_)==nil)
#define A(o) two(o)->a
#define B(o) two(o)->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))

#define mix ((uintptr_t)2708237354241864315)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)

#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ip->ap(f, Ip, Hp, Sp)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
static Inline void *bump(g_core *f, size_t n) {
  void *x = f->hp;
  f->hp += n;
  return x; }

extern g_type str_type, two_type, str_type, sym_type, tbl_type, num_type, cel_type;
static Inline bool twop(g_word _) { return celp(_) && typ(_) == &two_type; }
static Inline bool strp(g_word _) { return celp(_) && typ(_) == &str_type; }
static Inline bool tblp(g_word _) { return celp(_) && typ(_) == &tbl_type; }
static Inline bool symp(g_word _) { return celp(_) && typ(_) == &sym_type; }
static Inline void ini_pair(g_pair *w, g_word a, g_word b) {
  w->ap = data;
  w->typ = &two_type;
  w->a = a;
  w->b = b; }

static Inline void ini_table(g_table *t, uintptr_t len, uintptr_t cap, struct entry**tab) {
  t->ap = data;
  t->typ = &tbl_type;
  t->len = len;
  t->cap = cap;
  t->tab = tab; }

static Inline void ini_str(g_string *s, uintptr_t len) {
  s->ap = data;
  s->typ = &str_type;
  s->len = len; }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(g_word),
         r = b % sizeof(g_word);
  return q + (r ? 1 : 0); }

static Inline struct g_tag { g_cell *null, *head, end[]; } *ttag(g_cell*k) {
  while (k->x) k++;
  return (struct g_tag*) k; }

static Inline g_core *g_eva(g_core *f, g_vm *y) {
  return g_run(g_ana(f, y)); }

typedef g_word word;
typedef g_cell cell;
