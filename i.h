#ifndef _p_i_h
#define _p_i_h
#include "p.h"

#include <stdarg.h> // this is a freestanding header

// these are not freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>

typedef intptr_t PWord, *PStack, *PHeap;

// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifndef TCO
#define TCO 1 // on by default
#endif

#if TCO
#define Vm(n, ...) PStatus n(PCore *f, PCell* Ip, PHeap Hp, PStack Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define YieldStatus PStatusOk
#define Continue() Ip->ap(f, Ip, Hp, Sp)
#define Jump(v) v(f, Ip, Hp, Sp)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)
#else
#define Vm(n, ...) PStatus n(PCore *f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define YieldStatus PStatusEof
#define Continue() Ok
#define Jump(v) v(f)
#define Have(n) if (Sp - Hp < n) return gc(f, n)
#define Have1() if (Sp == Hp) return gc(f, 1)
#endif


// thanks !!
typedef PWord *PStack, *PHeap;
typedef union PCell PCell;
typedef Vm(PVm);
union PCell {
  PVm *ap;
  PWord x;
  PCell *m; };

typedef bool
  p_equal_t(PCore*, PWord, PWord);
typedef PWord
  p_copy_t(PCore*, PWord, PWord*, PWord*),
  p_hash_t(PCore*, PWord);
typedef void
  p_evac_t(PCore*, PWord, PWord*, PWord*, PHeap*),
  p_print_t(PCore*, p_file, PWord);

// basic data type method table
typedef struct PType {
  p_copy_t  *copy;
  p_evac_t  *evac;
  p_equal_t *equal;
  p_print_t *emit;
  p_hash_t  *hash;
  PVm       *apply;
} PType;

#define PDataHeader PVm *ap; PType *typ
typedef struct PPair {
  PDataHeader;
  PWord a, b;
} PPair;


typedef struct PString {
  PDataHeader;
  uintptr_t len;
  char text[];
} PString;
typedef struct PSymbol {
  PDataHeader;
  PString *nom;
  PWord code;
  struct PSymbol *l, *r;
} PSymbol;

typedef struct PTableEntry {
  PWord key, val;
  struct PTableEntry *next;
} PTableEntry;
typedef struct PTable {
  PDataHeader;
  uintptr_t len, cap;
  PTableEntry **tab;
} PTable;
typedef struct PMm {
  PWord *addr;
  struct PMm *next;
} PMm;

// runtime core data structure -- 1 core = 1 thread of execution
struct PCore {
  // vm registers
  PCell *ip;
  PHeap  hp; // heap pointer
  PStack sp; // stack pointer
             //
  // environment
  PTable *dict, *macro; // global environment and macros
  PSymbol *symbols; // internal symbols
                    //
  // memory management
  uintptr_t len;
  PWord *pool, *loop;
  uintptr_t t0; // end time of last gc
  PMm *safe;
};

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
struct tag { PCell *null, *head, end[]; } *ttag(PCell*);
static Inline PCell* mo_ini(PCell* _, uintptr_t len) {
  struct tag *t = (struct tag*) (_ + len);
  return t->null = NULL, t->head = _; }

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(PWord) == sizeof(PCell), "cell is 1 word wide");

PCell
  *mo_n(PCore*, size_t);
PPair
  *pairof(PCore*, PWord, PWord);
PTable
  *new_table(PCore*),
  *table_set(PCore*, PTable*, PWord, PWord);
PSymbol
  *literal_symbol(PCore*, const char*),
  *intern(PCore*, PString*);
void
  trim_thread(PCell*),
  *bump(PCore*, uintptr_t),
  *cells(PCore*, uintptr_t),
  transmit(PCore*, PFile*, PWord);
bool
  p_please(PCore*, uintptr_t),
  eql(PCore*, PWord, PWord);
p_equal_t not_equal;
PWord
  table_get(PCore*, PTable*, PWord, PWord),
  pushs(PCore*, uintptr_t, ...),
  hash(PCore*, PWord),
  cp(PCore*, PWord, PWord*, PWord*); // for recursive use by evac functions
Vm(gc, uintptr_t s);
PVm display, bnot, rng, data,
    defmacro,
    ret, ap1, ap, apn, tap, tap1, tapn,
    jump, cond, pushp, pushk, yield,
   gensym, ev0, pairp, fixnump, symbolp, stringp,
   ssub, sget, slen, scat, prc, error, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   curry
   ;

#define Oom PStatusOom
#define Ok PStatusOk
#define Eof PStatusEof

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((PWord)(_)>>1)
#define putnum(_) (((PWord)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((PMm){(PWord*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((PPair*)(o))->a
#define B(o) ((PPair*)(o))->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))
#define Z(x) ((intptr_t)(x))
#define nilp(_) (Z(_)==nil)
#define nump(_) ((PWord)(_)&1)
#define homp(_) (!nump(_))
#define datp(_) (ptr(_)->ap==data)
#define mix ((uintptr_t)2708237354241864315)
#define ptr(o) ((PCell*)(o))
#define dtyp(x) ((PType*)ptr(x)[1].m)
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))

extern PType pair_type, string_type, symbol_type, table_type;

static Inline bool hstrp(PWord _) { return dtyp(_) == &string_type; }
static Inline bool htwop(PWord _) { return dtyp(_) == &pair_type; }
static Inline bool htblp(PWord _) { return dtyp(_) == &table_type; }
static Inline bool hsymp(PWord _) { return dtyp(_) == &symbol_type; }

static Inline bool strp(PWord _) { return homp(_) && hstrp(_); }
static Inline bool twop(PWord _) { return homp(_) && htwop(_); }
static Inline bool tblp(PWord _) { return homp(_) && htblp(_); }
static Inline bool symp(PWord _) { return homp(_) && hsymp(_); }

static Inline PPair *ini_pair(PPair *w, PWord a, PWord b) {
  return w->ap = data, w->typ = &pair_type, w->a = a, w->b = b, w; }

static Inline PString* ini_str(PString* s, uintptr_t len) {
  return s->ap = data, s->typ = &string_type, s->len = len, s; }

static Inline PSymbol* ini_sym(PSymbol* y, PString*nom, uintptr_t code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = nom, y->code = code, y->l = y->r = 0, y; }
static Inline PSymbol* ini_anon(PSymbol* y, PWord code) {
  return y->ap = data, y->typ = &symbol_type, y->nom = 0, y->code = code, y; }

static Inline PTable *ini_table(PTable *t, uintptr_t len, uintptr_t cap, PTableEntry **tab) {
  return t->ap = data, t->typ = &table_type, t->len = len, t->cap = cap, t->tab = tab, t; }

// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(PWord), r = b % sizeof(PWord);
  return q + (r ? 1 : 0); }

static Inline PWord pop1(PCore *f) { return *f->sp++; }
static Inline size_t stack_height(PCore *f) { return f->pool + f->len - f->sp; }

#define bounded(a, b, c) ((PWord)(a)<=(PWord)(b)&&(PWord)(b)<(PWord)(c))
#define op(n, x) (Ip = (PCell*) Sp[n], Sp[n] = (x), Sp += n, Continue())


#endif
