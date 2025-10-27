#ifndef _g_h
#define _g_h
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
// thanks !!
struct g {
  union x  *ip;                          // 0
  intptr_t *hp,                          // 1
           *sp;                          // 2
  struct g_symbol *symbols;              // 3 symbol tree
  uintptr_t len;                         // 4 length of core data
  intptr_t *pool;                        // 5 lower core address
  struct g_mem_root *safe;               // 6
  union { uintptr_t t0; intptr_t *cp; }; // 7 copy pointer / timestamp
  void *(*malloc)(struct g*, size_t),    // 8
       (*free)(struct g*, void*);        // 9
  struct g_in  *in;                      // 10
  struct g_out *out,                     // 11
               *err;                     // 12
  struct g_table *dict,                  // 13
                 *macro;                 // 14
  struct g_symbol *quote,                // 15
                  *begin,                // 16
                  *let,                  // 17
                  *cond,                 // 18
                  *lambda;               // 19
  intptr_t end[]; };                     // 20 end of struct == initial heap pointer

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

struct g_in {
  int (*getc)(struct g_in*),
      (*ungetc)(struct g_in*, int),
      (*eof)(struct g_in*); };

struct g_out {
  void (*printf)(struct g_out*, const char*, ...),
       (*putc)(struct g_out*, int); };

struct g_mem_root {
  intptr_t *ptr;
  struct g_mem_root *next; };

typedef g_vm(g_vm_t);
struct g_def {
  const char *n;
  union x *x; };

union x {
  g_vm_t *ap;
  intptr_t x;
  union x *m;
  intptr_t typ; };

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

// user gives these
uintptr_t g_clock(void); // used by garbage collector
struct g *g_readsi(struct g*, struct g_in*);
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
int strncmp(const char*, const char*, size_t),
    memcmp(const void*, const void*, size_t);


// user gets these
g_vm_t ret0, curry;

bool g_twop(intptr_t),
     g_strp(intptr_t),
     g_tblp(intptr_t),
     g_symp(intptr_t);

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
uintptr_t g_str_len(intptr_t);
char *g_str_txt(intptr_t);
struct g *g_vec0(struct g*f, uintptr_t type, uintptr_t rank, ...);

enum g_vec_type {
  g_vt_u8,  g_vt_i8,
  g_vt_u16, g_vt_i16,
  g_vt_u32, g_vt_i32,
  g_vt_u64, g_vt_i64,
  g_vt_f8,  g_vt_f16,
  g_vt_f32, g_vt_f64, };
#define g_vt_char g_vt_i8
uintptr_t g_fixed_size(enum g_vec_type);

struct g_vec {
  g_vm_t *ap;
  uintptr_t typ,
            type,
            rank,
            shape[]; };
uintptr_t vector_total_bytes(struct g_vec *);
#endif
