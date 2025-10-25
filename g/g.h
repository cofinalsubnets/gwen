#ifndef _g_h
#define _g_h
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// thanks !!
struct g
  *g_ini(void),
  *g_ini_static(uintptr_t, void*),
  *g_ini_dynamic(void *(*)(struct g*, uintptr_t), void (*)(struct g*, void*));

enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_fin(struct g*);

struct g_def {
  const char *n;
  union x *x; };

struct g
  *g_gc(struct g*),
  *g_read1(struct g*),
  *g_write1(struct g*),
  *g_evals(struct g*, const char*),
  *g_defns(struct g*, uintptr_t, struct g_def*),
  *g_apply(struct g*),
  *g_push(struct g*, uintptr_t, ...),
  *g_pop(struct g*, uintptr_t),
  *g_strof(struct g*, const char*),
  *g_cons_l(struct g*),
  *g_cons_r(struct g*);

bool g_twop(intptr_t),
     g_strp(intptr_t),
     g_tblp(intptr_t),
     g_symp(intptr_t);

#ifndef g_tco
#define g_tco 1
#endif

#if g_tco
#define g_vm(n, ...) struct g *n(struct g *f, union x *Ip, intptr_t *Hp, intptr_t *Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, f)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#else
#define g_vm(n, ...) struct g *n(struct g *f, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Continue() f
#endif
typedef g_vm(g_vm_t);
struct g_in {
  int (*getc)(struct g_in*),
      (*ungetc)(struct g_in*, int),
      (*eof)(struct g_in*); };
struct g_out {
  void (*printf)(struct g_out*, const char*, ...),
       (*putc)(struct g_out*, int); };
union x {
  g_vm_t *ap;
  intptr_t x;
  union x *m;
  uintptr_t typ; };

struct g {
  // vm state
  union x *ip;
  intptr_t *hp, *sp;
  struct g_symbol *symbols; // symbol tree
  // memory state
  uintptr_t len;  // length of core data
  intptr_t *pool; // lower core address
  struct g_mem_root { intptr_t *ptr; struct g_mem_root *next; } *safe;
  union { uintptr_t t0; intptr_t *cp; }; // copy pointer
  void *(*malloc)(struct g*, size_t),
        (*free)(struct g*, void*);
  // main i/o connections
  struct g_in *in;
  struct g_out *out, *err;
  // variables
  struct g_table *dict, *macro;
  struct g_symbol *quote, *begin, *let, *cond, *lambda;
  intptr_t end[]; }; // end of struct == initial heap pointer for this core

struct g_string {
  g_vm_t *ap;
  uintptr_t typ;
  uintptr_t len;
  char text[]; };

#define g_core_of(f) ((struct g*)((intptr_t)(f)&~(sizeof(intptr_t)-1)))
#define g_code_of(f) ((enum g_status)((intptr_t)(f)&(sizeof(intptr_t)-1)))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#define g_putnum(_) (((intptr_t)(_)<<1)|1)
#define g_getnum(_) ((intptr_t)(_)>>1)
#define g_nil g_putnum(0)
#define g_pop1(f) (*(f)->sp++)
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(_,__) ((_)<(__)?(_):(__))
#define MAX(_,__) ((_)>(__)?(_):(__))
#define g_inline inline __attribute__((always_inline))
#define g_noinline __attribute__((noinline))

// user gets these
g_vm_t ret0, curry;

// user gives these
uintptr_t g_clock(void); // used by garbage collector
struct g *g_read1i(struct g*, struct g_in*),
         *g_readsi(struct g*, struct g_in*);
int g_getc(struct g_in*),
    g_ungetc(struct g_in*, int),
    g_eof(struct g_in*);
void g_printf(struct g_out*, const char*, ...),
     g_putc(struct g_out*, int);
void *malloc(size_t), free(void*),
     *memcpy(void *restrict, const void*restrict, size_t),
     *memset(void*, int, size_t);
long strtol(const char*restrict, char**restrict, int);
size_t strlen(const char*);
int strncmp(const char*, const char*, size_t);
#endif
