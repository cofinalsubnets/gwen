#include "gw.h"
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#define encode(f, s) ((g_core*)((g_word)(f)|(s)))
#define core_of g_core_of
#define code_of g_code_of

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

#define g_malloc malloc
#define g_free free

#define min(a, b) ((a)<(b)?(a):(b))

#define Eof g_status_eof
#define Oom g_status_oom
#define Ok g_status_ok

#define putnum(_) (((word)(_)<<1)|1)
#define getnum(_) ((word)(_)>>1)
#define nil putnum(0)

#define str(x) ((g_string*)(x))
#define sym(x) ((g_symbol*)(x))
#define two(x) ((g_pair*)(x))
#define cell(x) ((cell*)(x))
#define tbl(x) ((g_table*)(x))
#define word(x) ((g_word)(x))
#define W word
#define R cell
#define Width(_) b2w(sizeof(_))
#define within(a, b, c) (W(a)<=W(b)&&W(b)<W(c))
#define owns(f, x) within(f->pool, x, f->pool + f->len)
#define datp(_) (cell(_)->ap==data)
#define typ(_) cell(_)[1].typ
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->safe=&((struct root){(word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define pop1(f) (*(f)->sp++)
#define push1(f, x) (*--(f)->sp=(word)(x))
#define nump(_) (W(_)&1)
#define celp(_) (!nump(_))
#define homp celp


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
#define Vm(n, ...) g_core *n(core *f, cell* Ip, word* Hp, word* Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ap(Ip->ap, f)

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

enum g_var {
  g_var_ip,
  g_var_dict,
  g_var_mac,
  g_var_ev,
  g_var_qt,
  g_var_do,
  g_var_de,
  g_var_if,
  g_var_la,
  g_var_N, };

struct g_core {
  // vm registers
  word *hp, *sp;
  union {
    struct {
      cell *ip;
      table *dict, *macro;
      symbol *eval, *quote, *begin, *let, *cond, *lambda;
    };
    g_word vars[g_var_N]; };

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

// primitive type method tables
typedef struct methods {
  g_word (*cp)(g_core*, g_word, g_word*, g_word*); // for gc
  void (*wk)(g_core*, g_word, g_word*, g_word*);
  bool (*eq)(g_core*, g_word, g_word);
  void (*em)(g_core*, FILE*, g_word);
  uintptr_t (*xx)(g_core*, g_word);
  g_core (*show)(g_core*, g_word);
} g_type;

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
       *g_push(g_core*, uintptr_t, ...),
       *g_cons_l(g_core*),
       *g_cons_r(g_core*),
       *g_intern(g_core*),
       *g_hash_get(g_core*),
       *g_hash_put(g_core*),
       *g_ana(g_core*, vm*),
       *g_read_cs(g_core*, const char*);

Vm(gc, uintptr_t);
vm data, bnot, rng, nullp, sysclock, symnom, dot,
   gensym, pairp, fixnump, symbolp, stringp,
   ssub, sget, slen, scat, prc, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   read0, readf, p_isatty, g_yield, defglob, drop1, imm, ref,
   free_variable, curry, ev0, ret0,
   cond, jump, ap, tap, apn, tapn, ret, late_bind;

static Inline void *bump(g_core *f, size_t n) {
  void *x = f->hp;
  f->hp += n;
  return x; }

extern type str_type, two_type, str_type, sym_type, tbl_type, num_type, cel_type;
static Inline bool twop(word _) { return celp(_) && typ(_) == &two_type; }
static Inline bool strp(word _) { return celp(_) && typ(_) == &str_type; }
static Inline bool tblp(word _) { return celp(_) && typ(_) == &tbl_type; }
static Inline bool symp(word _) { return celp(_) && typ(_) == &sym_type; }
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
  size_t q = b / sizeof(word),
         r = b % sizeof(word);
  return q + (r ? 1 : 0); }

static Inline struct tag { cell *null, *head, end[]; } *ttag(cell*k) {
  while (k->x) k++;
  return (struct tag*) k; }

// align bytes up to the nearest word
_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(cell*) == sizeof(cell), "cell is 1 word wide");
