#ifndef _g_i_h
#define _g_i_h
// thanks !!
#include "g.h"
#include <stdbool.h>
struct g {
  intptr_t *hp, *sp; // heap and stack pointers
  union x *ip;
  struct g_symbol *symbols;
  intptr_t *pool; // lower core address
  uintptr_t len;  // length of half of pool
  struct g_mem_root *safe;
  union { uintptr_t t0;    // end of last gc timestamp
          intptr_t *cp; }; // copy pointer
  void *(*malloc)(struct g*, size_t),
        (*free)(struct g*, void*);
  struct g_table *dict, *macro;
  struct g_symbol *quote, *begin, *let, *cond, *lambda;
  intptr_t end[]; }; // end of struct == initial heap pointer for this core
#if !defined(g_tco) || g_tco != 0
#define Vm(n, ...) struct g *n(struct g *f, union x *Ip, intptr_t *Hp, intptr_t *Sp, ##__VA_ARGS__)
#define YieldStatus g_status_ok
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define Continue() Ap(Ip->ap, f)
#else
#define Vm(n, ...) struct g *n(struct g *f, ##__VA_ARGS__)
#define YieldStatus g_status_eof
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define Continue() f
#endif
typedef Vm(g_vm_t);
union x { g_vm_t *ap; intptr_t x; union x *m; uintptr_t typ; };
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#if !defined(g_tco) || g_tco != 0
static Inline struct g *g_run(struct g *f) {
  return !g_ok(f) ? f : f->ip->ap(f, f->ip, f->hp, f->sp); }
#else
static Inline struct g*g_run(struct g*f) {
  while (g_ok(f)) f = f->ip->ap(f);
  return g_code_of(f) == g_status_eof ? g_core_of(f) : f; }
#endif
struct g_in {
  int (*getc)(struct g_in*),
      (*ungetc)(struct g_in*, int),
      (*eof)(struct g_in*); };
struct g_string {
  g_vm_t *ap;
  uintptr_t typ;
  uintptr_t len;
  char text[]; };
#include "sys.h"
uintptr_t g_sys_clock(void); // used by garbage collector
struct g *g_read1i(struct g*, struct g_in*),
         *g_readsi(struct g*, struct g_in*);
g_vm_t ret0, curry, data, dot, self;
bool twop(intptr_t), strp(intptr_t), tblp(intptr_t), symp(intptr_t);
void *malloc(size_t), free(void*),
     *memcpy(void *restrict, const void*restrict, size_t),
     *memset(void*, int, size_t);
long strtol(const char*restrict, char**restrict, int);
size_t strlen(const char*);
int strncmp(const char*, const char*, size_t);
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(_,__) ((_)<(__)?(_):(__))
#define MAX(_,__) ((_)>(__)?(_):(__))
#define pop1(f) (*(f)->sp++)
#endif
