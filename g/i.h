// thanks !!
#ifndef _g_i_h
#define _g_i_h
#include "g.h"
#include <stdbool.h>
typedef Vm(g_vm);

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define LEN(x) (sizeof(x)/sizeof(*x))
#define MIN(x,y) ((x)<(y)?(x):(y))
#define MAX(x,y) ((x)>(y)?(x):(y))

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

struct d {
  int32_t type, rank;
  intptr_t shape[]; };

// cores occur in pairs located 2^len words away from each other.
// the address of the lower core is a pointer to a 2^(len+1) word size
// block returned by the allocator. the upper core is located 2^len words
// above the lower core. during operation the active core switches between upper
// and lower cores, and sometimes is transparently relocated to the lower core
// of a different pair.
struct g {
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

#include "sys.h"

typedef intptr_t g_cp_t(struct g*, intptr_t, intptr_t*, intptr_t*);
typedef void g_wk_t(struct g*, intptr_t, intptr_t*, intptr_t*);
typedef bool g_id_t(struct g*, intptr_t, intptr_t);
typedef struct g *g_em_t(struct g*, g_file, intptr_t);
typedef uintptr_t g_xx_t(struct g*, intptr_t);
typedef struct g *g_pp_t(struct g*, intptr_t);
// built in type method tables
typedef struct g_type {
  g_cp_t *cp;
  g_wk_t *wk;
  g_id_t *eq;
  g_em_t *em;
  g_xx_t *xx;
  g_vm *ap;
  g_pp_t *show;
} g_type;

g_cp_t cp_two, cp_tbl, cp_str, cp_sym;
g_wk_t wk_two, wk_tbl, wk_str, wk_sym;
g_id_t neql, eq_two, eq_str;
g_em_t em_two, em_sym, em_str, em_tbl;
g_xx_t xx_two, xx_tbl, xx_sym, xx_str;


extern g_cp_t *t_cp[];
extern g_wk_t *t_wk[];
extern g_em_t *t_em[];
extern g_xx_t *t_xx[];
extern g_vm *t_ap[];
extern g_pp_t *t_pp[];
extern g_id_t *t_id[];

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

typedef struct g_input {
  int (*getc)(struct g_input*),
      (*ungetc)(struct g_input*, int),
      (*eof)(struct g_input*);
} g_input, input;


void
  transmit(g_core*, g_file, g_word);

bool
  neql(g_core*, g_word, g_word),
  eql(g_core*, g_word, g_word);

uintptr_t
  hash(g_core*, g_word),
  g_sys_clock(void);

g_word
  cp(g_core*, g_word, g_word*, g_word*),
  g_hash_get(g_core*, g_word, g_table*, g_word);

g_core
  *please(g_core*, uintptr_t),
  *g_ana(g_core *, g_vm*),
  *g_have(g_core*, uintptr_t),
  *g_cells(g_core*, size_t),
  *g_hash_put(g_core*),
  *g_read1i(g_core*, g_input*),
  *g_readsi(g_core*, input*);

Vm(gc, uintptr_t);
g_vm
  data, symnom, dot, self,
  gensym, pairp, fixnump, symbolp, stringp,
  band, bor, bxor, bsr, bsl, bnot,
  ssub, sget, slen, scat, prc, cons, car, cdr,
  lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
  seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
  defglob, drop1, imm, ref,
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

enum g_ty {
  g_ty_two,
  g_ty_str,
  g_ty_sym,
  g_ty_tbl, };

extern g_type str_type, two_type, sym_type, tbl_type;
bool twop(intptr_t), strp(intptr_t), tblp(intptr_t), symp(intptr_t);

void ini_pair(g_pair*, intptr_t, intptr_t),
     ini_table(g_table*, uintptr_t, uintptr_t, struct entry**),
     ini_sym(g_symbol*, g_string*, uintptr_t),
     ini_anon(g_symbol*, uintptr_t),
     ini_str(g_string*, uintptr_t);
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(g_word),
         r = b % sizeof(g_word);
  return q + (r ? 1 : 0); }

static Inline struct g_tag { g_cell *null, *head, end[]; } *ttag(g_cell*k) {
  while (k->x) k++;
  return (struct g_tag*) k; }


void g_dbg(g_core*);
extern const char g_boot[];

// libc functions used internally...
// declare them here so the kernel can refer to them
// without including any non-freestanding header files.
void *malloc(size_t), free(void*),
     *memcpy(void *restrict, const void*restrict, size_t),
     *memset(void*, int, size_t);
long strtol(const char*restrict, char**restrict, int);
size_t strlen(const char*);
int strncmp(const char*, const char*, size_t);
#endif
