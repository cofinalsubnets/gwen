#ifndef _g_h
#define _g_h
// thanks !!
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#define Width(_) b2w(sizeof(_))
#define g_core_of(f) ((struct g*)((intptr_t)(f)&~(sizeof(intptr_t)-1)))
#define g_code_of(f) ((enum g_status)((intptr_t)(f)&(sizeof(intptr_t)-1)))
#define gokp(f) (g_code_of(f) == g_status_ok)
#define g_ok gokp
#define gputnum(_) (((intptr_t)(_)<<1)|1)
#define ggetnum(_) ((intptr_t)(_)>>1)
#define gnil gputnum(0)
#define g_putnum gputnum
#define g_getnum ggetnum
#define g_nil gnil
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(p,q) ((p)<(q)?(p):(q))
#define MAX(p,q) ((p)>(q)?(p):(q))

#ifndef g_tco
#define g_tco 1
#endif
#ifndef g_float
#define g_float 0
#endif

typedef intptr_t g_num, g_word;
union u;
#if g_tco
#define g_vm(n, ...) struct g *n(struct g *restrict f, union u *Ip, g_num *Hp, g_num *restrict Sp, ##__VA_ARGS__)
#else
#define g_vm(n, ...) struct g *n(struct g *restrict f, ##__VA_ARGS__)
#endif
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
    struct g_atom *l, *r; } *symbols;    // 3 
  uintptr_t len;                         // 4 length of core data
  struct g *pool;                        // 5 lower core address
  struct g_root {
    intptr_t *ptr;
    struct g_root *next; } *root;    // 6
  union { uintptr_t t0; g_num *cp; }; // 7 copy pointer / timestamp
  void *(*malloc)(size_t, struct g*),    // 8
       (*free)(void*, struct g*);        // 9
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
        *begin,
        *let,
        *cond,
        *lambda,
        *eval;
      intptr_t u[]; }; };
  intptr_t end[]; };

struct g_in {
  int (*getc)(struct g*, struct g_in*),
      (*ungetc)(struct g*, int, struct g_in*),
      (*eof)(struct g*, struct g_in*); };

struct g_out {
  int (*putc)(struct g*, int, struct g_out*),
      (*flush)(struct g*); };

struct g_def { char const *n; intptr_t x; };

enum g_status gfin(struct g*);

g_vm_t gvmret0, gvmcurry;

#define g_vt_char g_vt_u8
uintptr_t g_clock(void); // used by garbage collector
int
  gfprintf(struct g*, struct g_out*, const char*, ...),
  gfwrite1(struct g*, struct g_out*),
  ggetc(struct g*),
  gungetc(struct g*, int),
  geof(struct g*),
  gputc(struct g*, int),
  gflush(struct g*),
  strncmp(char const*, char const*, size_t),
  memcmp(void const*, void const*, size_t);
void
  *malloc(size_t),
  free(void*),
  *memcpy(void*restrict, void const*restrict, size_t),
  *memset(void*, int, size_t);
long strtol(char const*restrict, char**restrict, int);
size_t strlen(char const*);
struct g
  *gini(void),
  *ginid(void *(*)(size_t, struct g*), void (*)(void*, struct g*)),
  *gevals(struct g*, const char*),
  *gdef1(struct g*, const char*),
  *gdefs(struct g*, uintptr_t, struct g_def*),
  *gpush(struct g*, uintptr_t, ...),
  *gstrof(struct g*, const char*),
  *gconsl(struct g*),
  *gconsr(struct g*);

#define g_inline inline __attribute__((always_inline))
#define g_noinline __attribute__((noinline))
#define g_digits "0123456789abcdefghijklmnopqrstuvwxyz"

static g_inline struct g *gpop(struct g *f, uintptr_t m) {
  if (g_ok(f)) f->sp += m;
  return f; }

static g_inline struct g *gevals_(struct g*f, const char*s) {
 return gpop(gevals(f, s), 1); }

static g_inline size_t b2w(size_t b) {
  size_t q = b / sizeof(g_num),
         r = b % sizeof(g_num);
  return q + (r ? 1 : 0); }

#if g_float
#define g_gnum(_) (_)
#define g_pnum(_) (_)
#else
static g_inline g_num g_gnum(g_num n) {
  return n >> 1; }
static g_inline g_num g_pnum(g_num n) {
  return (n << 1) | 1; }
#endif

#if g_tco
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, f)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#else
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Continue() f
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#endif
#endif
