#ifndef _g_h
#define _g_h
// thanks !!
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>

#define Width(_) b2w(sizeof(_))
#define g_width Width
#define g_core_of(f) ((struct g*)((intptr_t)(f)&~(sizeof(intptr_t)-1)))
#define g_code_of(f) ((enum g_status)((intptr_t)(f)&(sizeof(intptr_t)-1)))
#define g_ok(f) (g_code_of(f) == g_status_ok)

#define g_putnum(_) (((g_num)(_)<<1)|1)
#define g_getnum(_) ((g_num)(_)>>1)

#define gputnum g_putnum
#define ggetnum g_getnum

#define g_nil gputnum(0)
#define gnil g_nil
#define g_inline inline __attribute__((always_inline))
#define g_noinline __attribute__((noinline))
#define g_digits "0123456789abcdefghijklmnopqrstuvwxyz"
#define g_vt_char g_vt_u8
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(p,q) ((p)<(q)?(p):(q))
#define MAX(p,q) ((p)>(q)?(p):(q))

#ifndef g_tco
#define g_tco 1
#endif
#ifndef g_float
#define g_float 0
#endif

#if g_tco
#define g_vm(n, ...) struct g *n(struct g *restrict f, union u *Ip, g_num *Hp, g_num *restrict Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, f)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#else
#define g_vm(n, ...) struct g *n(struct g *restrict f, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Continue() f
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#endif

// ok

// integer default
typedef intptr_t
 g_num, g_word;

// any type
union u;

typedef g_vm(g_vm_t);
enum g_status {
 g_status_ok  = 0,
 g_status_oom = 1,
 g_status_err = 2,
 g_status_eof = 3, };

struct g {
 union u {
  g_vm_t *ap;
  g_num x, typ;
  union u *m; } *ip;
 g_num *hp, *sp;
 struct g_atom {
  g_vm_t *ap;
  g_num typ;
  struct g_vec {
   g_vm_t *ap;
   g_num typ, type, rank, shape[]; } *nom;
 uintptr_t code;
 struct g_atom *l, *r; } *symbols;
 uintptr_t len;
 struct g *pool;
 struct g_root {
  intptr_t *ptr;
  struct g_root *next; } *root;
 union { uintptr_t t0; g_num *cp; };
 void *(*malloc)(size_t, struct g*),
      (*free)(void*, struct g*);
 union {
#define g_nvars 16
  intptr_t v[g_nvars];
  struct {
   struct g_tab { g_vm_t *ap; intptr_t typ; uintptr_t len, cap;
    struct g_kvs { intptr_t key, val; struct g_kvs *next; } **tab;
   } *dict,
     *macro;
   struct g_atom
    *quote,
    *lambda;
   intptr_t u[]; }; };
 intptr_t end[]; };

struct g_def { char const *n; intptr_t x; };

struct g_in {
 int (*getc)(struct g*, struct g_in*),
     (*ungetc)(struct g*, int, struct g_in*),
     (*eof)(struct g*, struct g_in*); };

struct g_out {
 int (*putc)(struct g*, int, struct g_out*),
     (*flush)(struct g*); };

// some libc functions we use
int
 strncmp(char const*, char const*, size_t),
 memcmp(void const*, void const*, size_t);

void
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);

long
 strtol(char const*restrict, char**restrict, int);

g_vm_t g_vm_ret0, g_vm_curry;

uintptr_t
 g_clock(void); // used by garbage collector

int
 gvfprintf(struct g*, struct g_out*, char const*, va_list),
 gfprintf(struct g*, struct g_out*, const char*, ...),
 ggetc(struct g*),
 gungetc(struct g*, int),
 geof(struct g*),
 gputc(struct g*, int),
 gflush(struct g*);

struct g
 *g_inid(void *(*)(size_t, struct g*), void (*)(void*, struct g*)),
 *g_evals_(struct g*, const char*),
 *g_def1(struct g*, const char*),
 *g_defs(struct g*, struct g_def const*),
 *g_push(struct g*, uintptr_t, ...),
 *g_strof(struct g*, const char*),
 *gxl(struct g*),
 *gxr(struct g*);

enum g_status gfin(struct g*);

static g_inline struct g *g_ini(void) {
 return g_inid((void*) malloc, (void*) free); }

static g_inline size_t b2w(size_t b) {
 size_t q = b / sizeof(g_num),
        r = b % sizeof(g_num);
 return q + (r ? 1 : 0); }
#endif
