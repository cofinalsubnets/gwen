#ifndef _p_i_h
#define _p_i_h
#include "p.h"

#include <stdarg.h> // this is a freestanding header

// these are not freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>

typedef intptr_t PWord, *PStack, *PHeap, Word, *Heap, *Stack;
typedef PCore Core;
typedef PStatus Status;

// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifndef TCO
#define TCO 1 // on by default
#endif

#if TCO
#define Vm(n, ...) Status n(Core *f, Thread* Ip, Heap Hp, Stack Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define YieldStatus PStatusOk
#define Continue() Ip->ap(f, Ip, Hp, Sp)
#define Jump(v) v(f, Ip, Hp, Sp)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)
#else
#define Vm(n, ...) Status n(Core *f, ##__VA_ARGS__)
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
typedef union PCell PCell, Thread, Cell;
typedef struct PType PType, Type;
typedef Vm(PVm);
union PCell {
  PVm *ap;
  Word x;
  Cell *m;
  Type *typ; };

typedef bool
  p_equal_t(Core*, PWord, PWord);

typedef Word
  p_copy_t(Core*, Word, Word*, Word*),
  p_hash_t(Core*, PWord);

typedef void
  p_evac_t(Core*, Word, Word*, Word*),
  p_print_t(Core*, p_file, Word);

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
} PPair, Pair;


typedef struct PString {
  PDataHeader;
  uintptr_t len;
  char text[];
} PString, String;
typedef struct PSymbol {
  PDataHeader;
  PString *nom;
  PWord code;
  struct PSymbol *l, *r;
} PSymbol, Symbol;

typedef struct PTableEntry {
  PWord key, val;
  struct PTableEntry *next;
} PTableEntry;
typedef struct PTable {
  PDataHeader;
  uintptr_t len, cap;
  PTableEntry **tab;
} PTable, Table;
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
  union {
    uintptr_t t0; // end time of last gc
    Heap cp; }; // gc copy pointer
  PMm *safe;
};

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
struct tag { Thread *null, *head, end[]; } *ttag(Thread*);

Thread
  *mo_ini(Thread*, uintptr_t),
  *trim_thread(Thread*),
  *mo_n(PCore*, size_t);
Pair
  *ini_pair(Pair*, Word, Word),
  *pairof(PCore*, PWord, PWord);
Table
  *ini_table(Table*, uintptr_t, uintptr_t, PTableEntry**),
  *new_table(PCore*),
  *table_set(PCore*, PTable*, PWord, PWord);
Symbol
  *ini_sym(Symbol*, String*, uintptr_t),
  *literal_symbol(PCore*, const char*),
  *intern(PCore*, PString*);
String *ini_str(String*, uintptr_t);
void
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
    ret, ap, apn, tap, tapn,
    jump, cond, pushp, pushk, yield,
    gensym, ev0, pairp, fixnump, symbolp, stringp,
    ssub, sget, slen, scat, prc, error, cons, car, cdr,
    lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
    seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
    curry ;

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
#define R(_) ((Cell*)(_))
#define nilp(_) (Z(_)==nil)
#define nump(_) ((PWord)(_)&1)
#define thdp(_) (!nump(_))
#define datp(_) (ptr(_)->ap==data)
#define dtyp(_) R(_)[1].typ
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
#define mix ((uintptr_t)2708237354241864315)

#define homp thdp
#define ptr R

#define pop1(f) (*(f)->sp++)

extern Type pair_type, string_type, symbol_type, table_type;
static Inline bool strp(Word _) { return homp(_) && dtyp(_) == &string_type; }
static Inline bool twop(Word _) { return homp(_) && dtyp(_) == &pair_type; }
static Inline bool tblp(Word _) { return homp(_) && dtyp(_) == &table_type; }
static Inline bool symp(Word _) { return homp(_) && dtyp(_) == &symbol_type; }

// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(PWord), r = b % sizeof(PWord);
  return q + (r ? 1 : 0); }

#define bounded(a, b, c) ((PWord)(a)<=(PWord)(b)&&(PWord)(b)<(PWord)(c))
#define op(n, x) (Ip = (PCell*) Sp[n], Sp[n] = (x), Sp += n, Continue())

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(PWord) == sizeof(Thread), "cell is 1 word wide");
#endif
