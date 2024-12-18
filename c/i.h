#ifndef _g_i_h
#define _g_i_h

#include "gwen.h"
#include <stdarg.h> // this is a freestanding header

// these are not freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>

typedef intptr_t GwenWord, *GwenStack, *GwenHeap;

// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifndef TCO
#define TCO 1 // on by default
#endif

#if TCO
#define Vm(n, ...) GwenStatus n(GwenCore *f, GwenCell* Ip, GwenHeap Hp, GwenStack Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define YieldStatus GwenStatusOk
#define Continue() Ip->ap(f, Ip, Hp, Sp)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)
#else
#define Vm(n, ...) GwenStatus n(GwenCore *f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define YieldStatus GwenStatusEof
#define Continue() Ok
#define Have(n) if (Sp - Hp < n) return gc(f, n)
#define Have1() if (Sp == Hp) return gc(f, 1)
#endif


// thanks !!
typedef GwenWord *GwenStack, *GwenHeap;
typedef union GwenCell GwenCell;
typedef Vm(GwenVm);
union GwenCell {
  GwenVm *ap;
  GwenWord x;
  GwenCell *m; };

typedef bool
  gwen_equal_t(GwenCore*, GwenWord, GwenWord);
typedef GwenWord
  gwen_copy_t(GwenCore*, GwenWord, GwenWord*, GwenWord*),
  gwen_hash_t(GwenCore*, GwenWord);
typedef void
  gwen_evac_t(GwenCore*, GwenWord, GwenWord*, GwenWord*, GwenHeap*),
  gwen_print_t(GwenCore*, gwen_file, GwenWord);

// basic data type method table
typedef struct GwenType {
  gwen_copy_t *copy;
  gwen_evac_t *evac;
  gwen_equal_t *equal;
  gwen_print_t *emit;
  gwen_hash_t *hash;
} GwenType;

#define GwenDataHeader GwenVm *ap; GwenType *typ
typedef struct GwenPair {
  GwenDataHeader;
  GwenWord a, b;
} GwenPair;


typedef struct GwenString {
  GwenDataHeader;
  uintptr_t len;
  char text[];
} GwenString;
typedef struct GwenSymbol {
  GwenDataHeader;
  GwenString *nom;
  GwenWord code;
  struct GwenSymbol *l, *r;
} GwenSymbol;

typedef struct GwenTableEntry {
  GwenWord key, val;
  struct GwenTableEntry *next;
} GwenTableEntry;
typedef struct GwenTable {
  GwenDataHeader;
  uintptr_t len, cap;
  GwenTableEntry **tab;
} GwenTable;
typedef struct GwenMm {
  GwenWord *addr;
  struct GwenMm *next;
} GwenMm;

// runtime core data structure -- 1 core = 1 thread of execution
struct GwenCore {
  // vm registers
  GwenCell *ip;
  GwenHeap  hp; // heap pointer
  GwenStack sp; // stack pointer
  // environment
  GwenTable *dict,
            *macro; // global environment and macros
  GwenSymbol *symbols; // internal symbols
  // memory management
  GwenWord len,
           *pool,
           *loop; // memory pool size and pointers
  uintptr_t t0; // end time of last gc
  GwenMm *safe; };

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
// functions are laid out in memory like this
//
// *|*|*|*|*|*|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
static struct tag { GwenCell *null, *head, end[]; } *ttag(GwenCell *k) {
  while (k->x) k++;
  return (struct tag*) k; }

static Inline GwenCell* mo_ini(GwenCell* _, uintptr_t len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }


static gwen_hash_t hash_two, hash_string, hash_symbol, hash_table;
static gwen_copy_t cp_two, copy_string, copy_symbol, copy_table;
static gwen_evac_t wk_two, walk_string, walk_symbol, walk_table;
static gwen_equal_t eq_two, string_equal, not_equal;
static gwen_print_t print_string, print_two, print_table, print_symbol;

extern GwenType pair_type, string_type, symbol_type, table_type;

// allocated data types other than threads
static GwenVm data;

static Inline GwenPair *ini_pair(GwenPair *w, GwenWord a, GwenWord b) {
  return w->ap = data, w->typ = &pair_type, w->a = a, w->b = b, w; }

static Inline GwenString* ini_str(GwenString* s, uintptr_t len) {
  return s->ap = data, s->typ = &string_type, s->len = len, s; }

static Inline GwenSymbol* ini_sym(GwenSymbol* y, GwenString*nom, uintptr_t code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = nom, y->code = code, y->l = y->r = 0, y; }
static Inline GwenSymbol* ini_anon(GwenSymbol* y, GwenWord code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = 0, y->code = code, y; }

static Inline GwenTable *ini_table(GwenTable *t, uintptr_t len, uintptr_t cap, GwenTableEntry **tab) {
  return t->ap = data, t->typ = &table_type, t->len = len, t->cap = cap, t->tab = tab, t; }

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(GwenWord) == sizeof(GwenCell), "cell is 1 word wide");

static GwenPair
  *pairof(GwenCore*, GwenWord, GwenWord);
static GwenTable
  *new_table(GwenCore*),
  *table_set(GwenCore*, GwenTable*, GwenWord, GwenWord);
static GwenSymbol
  *literal_symbol(GwenCore*, const char*),
  *intern(GwenCore*, GwenString*);
static void
  *bump(GwenCore*, uintptr_t),
  *cells(GwenCore*, uintptr_t),
  copy_from(GwenCore*, GwenWord*, uintptr_t),
  transmit(GwenCore*, GwenFile*, GwenWord);
static Vm(gc, uintptr_t s);
static bool
  gwen_please(GwenCore*, uintptr_t),
  eql(GwenCore*, GwenWord, GwenWord);
static GwenWord
  table_get(GwenCore*, GwenTable*, GwenWord, GwenWord),
  pushs(GwenCore*, uintptr_t, ...),
  hash(GwenCore*, GwenWord),
  cp(GwenCore*, GwenWord, GwenWord*, GwenWord*); // for recursive use by evac functions
static GwenVm display, bnot, rng, data,
   gensym, ev0, pairp, fixnump, symbolp, stringp, defmacro,
   ssub, sget, slen, scat, prc, error, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   curry;

#define Oom GwenStatusOom
#define Ok GwenStatusOk
#define Eof GwenStatusEof

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((GwenWord)(_)>>1)
#define putnum(_) (((GwenWord)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((GwenMm){(GwenWord*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((GwenPair*)(o))->a
#define B(o) ((GwenPair*)(o))->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))
#define nilp(_) ((_)==nil)
#define nump(_) ((GwenWord)(_)&1)
#define homp(_) (!nump(_))
#define datp(_) (ptr(_)->ap==data)
#define mix ((uintptr_t)2708237354241864315)
#define bind(n, x) if (!(n = (x))) return 0
#define ptr(o) ((GwenCell*)(o))
#define dtyp(x) ((GwenType*)ptr(x)[1].m)
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))

static Inline bool strp(GwenWord _) { return homp(_) && dtyp(_) == &string_type; }
static Inline bool twop(GwenWord _) { return homp(_) && dtyp(_) == &pair_type; }
static Inline bool tblp(GwenWord _) { return homp(_) && dtyp(_) == &table_type; }
static Inline bool symp(GwenWord _) { return homp(_) && dtyp(_) == &symbol_type; }

// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(GwenWord), r = b % sizeof(GwenWord);
  return q + (r ? 1 : 0); }

static Inline GwenWord pop1(GwenCore *f) { return *f->sp++; }
static Inline size_t stack_height(GwenCore *f) { return f->pool + f->len - f->sp; }
size_t gwen_drop(GwenCore *f, size_t n) {
  size_t h = stack_height(f);
  n = min(n, h);
  f->sp += n;
  return n; }
#endif
