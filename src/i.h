// thanks !!
#ifndef _g_i_h
#define _g_i_h
#include "gw.h"
#include <stdbool.h>

#define g_target_host 0
#define g_target_pd 1
#define g_target_free 2
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#if g_target == g_target_host || g_target == g_target_free
#define g_tco 1
#elif g_target == g_target_pd
#define g_tco 0
#endif
#if g_tco
#define Vm(n, ...) g_core *n(g_core *f, g_cell *Ip, g_word *Hp, g_word *Sp, ##__VA_ARGS__)
#define YieldStatus g_status_ok
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ap(Ip->ap, f)
#else
#define Vm(n, ...) g_core *n(g_core *f, ##__VA_ARGS__)
#define YieldStatus g_status_eof
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define Continue() f
#endif
typedef Vm(g_vm);

union g_cell {
  g_vm *ap;
  g_word x;
  g_cell *m;
  struct g_type *typ; };

_Static_assert(sizeof(g_cell) == sizeof(g_word));

enum g_var {
  g_var_ip,
  g_var_dict,
  g_var_mac,
  g_var_qt,
  g_var_do,
  g_var_de,
  g_var_if,
  g_var_la,
  g_var_N, };

// cores occur in pairs located 2^len words away from each other.
// the address of the lower core is a pointer to a 2^(len+1) word size
// block returned by the allocator. the upper core is located 2^len words
// above the lower core. during operation the active core switches between upper
// and lower cores, and sometimes is transparently relocated to the lower core
// of a different pair.
struct g_core {
  g_word *hp, *sp; // heap and stack pointers
  struct g_symbol *symbols; // symbol tree
  union { // concrete state variables
    g_word vars[g_var_N];
    struct {
      union g_cell *ip;
      struct g_table *dict, *macro;
      struct g_symbol *quote, *begin, *let, *cond, *lambda; }; };
  // memory pool size
  uintptr_t len;
  g_word *pool; // lower core address
  struct root { // linked list of pointers to values saved by C functions
    g_word *ptr; // pointer to C stack value
    struct root *next; } *safe;
  union { // gc state
    uintptr_t t0; // end of last gc timestamp
    g_word *cp; }; // copy pointer
  g_malloc_t *malloc;
  g_free_t *free;
  g_word end[]; }; // end of struct == initial heap pointer for this core
                   //
static Inline g_core *g_run(g_core *f) {
#if g_tco
  f = !g_ok(f) ? f : f->ip->ap(f, f->ip, f->hp, f->sp);
#else
  while (g_ok(f)) f = f->ip->ap(f);
  f = g_code_of(f) == g_status_eof ? g_core_of(f) : f;
#endif
  return f; }

#if g_target == g_target_host
#include "../host/sys.h"
#elif g_target == g_target_pd
#include "../pd/sys.h"
#elif g_target == g_target_free
typedef int g_file;
#define g_stdin 0
#define g_stdout 1
#define g_stderr 2
#define g_fprintf(...) ((void)0)
#define g_fputc(...) ((void)0)
#define EOF (-1)
#endif

void *memcpy(void *restrict, const void*restrict, size_t),
     *memset(void*, int, size_t);
long strtol(const char*restrict, char**restrict, int);
size_t strlen(const char*);
int strncmp(const char*, const char*, size_t);

// built in type method tables
typedef struct g_type {
  g_word (*cp)(g_core*, g_word, g_word*, g_word*); // for gc
  void (*wk)(g_core*, g_word, g_word*, g_word*);
  bool (*eq)(g_core*, g_word, g_word);
  g_core *(*em)(g_core*, g_file, g_word);
  uintptr_t (*xx)(g_core*, g_word);
  g_vm *ap;
  g_core *(*show)(g_core*, g_word);
} g_type;

typedef struct g_pair {
  g_vm *ap;
  g_type *typ;
  g_word a, b;
} g_pair;

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

g_malloc_t g_malloc;
g_free_t g_free;

void
  transmit(g_core*, g_file, g_word);

bool
  neql(g_core*, g_word, g_word),
  eql(g_core*, g_word, g_word);

uintptr_t
  hash(g_core*, g_word),
  g_clock(void);

g_word
  cp(g_core*, g_word, g_word*, g_word*),
  g_hash_get(g_core*, g_word, g_table*, g_word);

Vm(gc, uintptr_t);
g_core
  *g_ana(g_core *, g_vm*),
  *g_have(g_core*, uintptr_t),
  *g_cells(g_core*, size_t),
  *g_hash_put(g_core*);

g_vm
  data, nullp, sysclock, symnom, dot, self,
  gensym, pairp, fixnump, symbolp, stringp,
  band, bor, bxor, bsr, bsl, bnot,
  ssub, sget, slen, scat, prc, cons, car, cdr,
  lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
  seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
  g_yield, defglob, drop1, imm, ref,
  free_variable, curry, ev0, ret0,
  cond, jump, ap, tap, apn, tapn, ret, late_bind;

#define putnum g_putnum
#define getnum g_getnum
#define nil g_nil

_Static_assert(-1 >> 1 == -1, "sign extended shift");

#define encode(f, s) ((g_core*)((g_word)(f)|(s)))

#define str(x) ((g_string*)(x))
#define sym(x) ((g_symbol*)(x))
#define two(x) ((g_pair*)(x))
#define cell(x) ((g_cell*)(x))
#define tbl(x) ((g_table*)(x))
#define ptr(x) ((g_word*)(x))
#define word(x) ((g_word)(x))
#define Width(_) b2w(sizeof(_))
#define within(a, b, c) (word(a)<=word(b)&&word(b)<word(c))
#define owns(f, x) within((g_word*)f, x, (g_word*)f + f->len)
#define datp(_) (cell(_)->ap==data)
#define typ(_) cell(_)[1].typ
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->safe=&((struct root){(g_word*)(r),f->safe})))
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
#define Have(n) do { if ((size_t) (Sp - Hp) < n) return Ap(gc, f, n); } while (0)
#define Have1() do { if (Sp == Hp) return Ap(gc, f, 1); } while (0)

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

typedef struct g_input {
  int (*getc)(struct g_input*),
      (*ungetc)(struct g_input*, int),
      (*eof)(struct g_input*);
} g_input, input;
static Inline int p_in_getc(input *i) { return i->getc(i); }
static Inline int p_in_ungetc(input *i, int c) { return i->ungetc(i, c); }
static Inline int p_in_eof(input *i) { return i->eof(i); }
g_core *g_read1i(g_core*, g_input*),
       *g_run(g_core*),
       *g_readsi(g_core*, input*);

void g_dbg(g_core*);
#endif
