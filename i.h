#include "gw.h"
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#define encode(f, s) ((g_core*)((g_word)(f)|s))
#define core_of g_core_of
#define code_of g_code_of

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

#define g_malloc malloc
#define g_free free

#define avail(f) (f->sp-f->hp)
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))

#define Eof g_status_eof
#define Oom g_status_oom
#define Ok g_status_ok

#define W(_) ((word)(_))
#define Z W
#define R(_) ((cell*)(_))
#define Width(_) b2w(sizeof(_))
#define CP(x) cp(f, W(x), p0, t0)
#define within(a, b, c) (W(a)<=W(b)&&W(b)<W(c))
#define owns(f, x) within(f->pool, x, f->pool + f->len)
#define datp(_) (R(_)->ap==data)
#define typof(_) R(_)[1].typ
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->safe=&((struct root){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define pop1(f) (*(f)->sp++)
#define push1(f, x) (*--(f)->sp=(word)(x))
#define nump(_) (W(_)&1)
#define homp(_) (!nump(_))

#define str(x) ((g_string*)(x))
#define sym(x) ((g_symbol*)(x))
#define two(x) ((g_pair*)(x))
#define ptr(x) ((g_word*)(x))
#define tbl(x) ((g_table*)(x))
#define word(x) W(x)

#define putnum(_) (((word)(_)<<1)|1)
#define nil putnum(0)
#define getnum(_) ((word)(_)>>1)
#define nilp(_) (W(_)==nil)
#define A(o) two(o)->a
#define B(o) two(o)->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))

#define mix ((uintptr_t)2708237354241864315)
#define Have(n) if (Sp - Hp < n) return Ap(gc, f, n)
#define Have1() if (Sp == Hp) return Ap(gc, f, 1)


#ifndef g_version
#define g_version ""
#endif
// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifdef NTCO
#define YieldStatus g_status_eof
#define Vm(n, ...) int n(core *f, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define Continue() g_status_ok
#else
#define YieldStatus g_status_ok
#define Vm(n, ...) int n(core *f, thread* Ip, word* Hp, word* Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ap(Ip->ap, f)
#endif

typedef struct g_string g_string;
typedef struct g_pair g_pair;
typedef struct g_symbol g_symbol;
typedef struct g_table g_table;
typedef union g_cell g_cell;
char *g_gs2cs(g_string*);
g_core *g_cs2gs(char*);

// thanks !!
typedef g_core p_core, core;
typedef g_word word;
typedef struct methods methods, type, g_type;
typedef union g_cell cell, thread;

typedef Vm(vm);
typedef Vm(MVm, int);
union g_cell { vm *ap; word x; cell *m; methods *typ; };

#define DataHeader vm *ap; methods *typ
typedef struct g_string {
  DataHeader;
  uintptr_t len;
  char text[];
} string;
typedef struct g_symbol {
  DataHeader;
  string *nom;
  word code;
  struct g_symbol *l, *r;
} symbol;
typedef struct g_table {
  DataHeader;
  uintptr_t len, cap;
  struct entry { word key, val; struct entry *next; } **tab;
} table;
typedef struct g_pair {
  DataHeader;
  word a, b;
} pair;

// runtime core data structure -- 1 core = 1 thread of execution
struct g_core {
  // vm registers
  cell *ip;
  word *hp, *sp;

  // variables
  table *dict, *macro;
  symbol *eval, *quote, *begin, *let, *cond, *lambda;

  // symbol tree
  symbol *symbols;

  // memory management
  uintptr_t len; // memory pool size
  word *pool, *loop; // on and off pool
  struct root { word *ptr; struct root *next; } *safe;
  union { uintptr_t t0;  // end time of last gc
          word *cp; }; // gc copy pointer
  struct dtor {
    g_word x;
    void (*d)(g_core*, g_word);
    struct dtor *next; } *dtors; };

typedef bool g_mtd_eq_t(g_core*, g_word, g_word);
typedef g_word g_mtd_gc_cp_t(g_core*, g_word, g_word*, g_word*);
typedef void
  g_mtd_gc_wk_t(g_core*, g_word, g_word*, g_word*),
  g_mtd_em_t(g_core*, FILE*, g_word);
typedef uintptr_t g_mtd_xx_t(g_core*, g_word);
typedef g_core *g_mtd_show_t(g_core*, g_word);
// primitive type method tables
struct methods {
  g_mtd_gc_cp_t *cp; // for gc
  g_mtd_gc_wk_t *wk; // for gc
  g_mtd_eq_t *eq;         // check equality with another object of same type
  g_mtd_em_t *em;        // print it // replace this with stringify...
  g_mtd_xx_t *xx;               // hash it
  g_mtd_show_t *show; };

typedef struct g_input {
  int (*getc)(struct g_input*),
      (*ungetc)(struct g_input*, int),
      (*eof)(struct g_input*);
} input;

typedef struct file_input { input in; FILE *file; } file_input;
typedef struct text_input { input in; const char *text; int i; } text_input;


bool neql(g_core*, g_word, g_word),
     eql(core*, word, word);
void transmit(g_core*, FILE*, g_word);
uintptr_t hash(g_core*, g_word),
          g_clock(void);
g_word cp(core*, word, word*, word*);
g_core *g_have(g_core*, uintptr_t),
       *g_cells(g_core*, size_t),
       *vpushc(core*, uintptr_t, va_list),
       *pushc(g_core*, uintptr_t, ...),
       *g_tget(g_core*),
       *g_cons_stack(g_core*, int, int),
       *g_intern_c(g_core*),
       *g_hash_put_c(g_core*),
       *g_eval_c(g_core*, vm*),
       *p_readcs(g_core*, const char*);

Vm(gc, uintptr_t);
vm data, bnot, rng, nullp, sysclock, symnom, dot,
   gensym, pairp, fixnump, symbolp, stringp,
   ssub, sget, slen, scat, prc, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   read0, readf, p_isatty, yieldi, defglob, drop1, imm, ref,
   free_variable, curry, ev0, ret0,
   cond, jump, ap, tap, apn, tapn, ret, late_bind;

extern type str_type, two_type, str_type, sym_type, tbl_type;
static Inline bool twop(word _) { return homp(_) && typof(_) == &two_type; }
static Inline bool strp(word _) { return homp(_) && typof(_) == &str_type; }
static Inline bool tblp(word _) { return homp(_) && typof(_) == &tbl_type; }
static Inline bool symp(word _) { return homp(_) && typof(_) == &sym_type; }


static Inline void *bump(g_core *f, size_t n) {
  void *x = f->hp;
  return f->hp += n,
         x; }

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
  size_t q = b / sizeof(word), r = b % sizeof(word);
  return q + (r ? 1 : 0); }

static Inline struct tag { cell *null, *head, end[]; } *ttag(cell*k) {
  while (k->x) k++;
  return (struct tag*) k; }
// align bytes up to the nearest word
_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(cell*) == sizeof(cell), "cell is 1 word wide");
