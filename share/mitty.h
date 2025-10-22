#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>

typedef intptr_t g_word;
typedef union g_cell g_cell;
typedef struct g_core g_core;
typedef void
  *g_malloc_t(g_core*, size_t),
   g_free_t(g_core*, void*);

g_core
  *g_ini(void),
  *g_ini_dynamic(g_malloc_t*, g_free_t*),
  *g_ini_static(size_t, void*),
  *g_read1(g_core*),
  *g_write1(g_core*),
  *g_read1s(g_core*, const char*),
  *g_readss(g_core*, const char*),
  *g_evals(g_core*, const char*),
  *g_evals_(g_core*, const char*),
  *g_define(g_core*, const char*),
  *g_eval(g_core*),
  *g_eval_(g_core*),
  *g_apply(g_core*),
  *g_push(g_core*, uintptr_t, ...),
  *g_pop(g_core*, uintptr_t),
  *g_strof(g_core*, const char*),
  *g_intern(g_core*),
  *g_tbl(g_core*),
  *g_cons_l(g_core*),
  *g_cons_r(g_core*);

enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_fin(g_core*);

extern const char g_boot_sequence[];

#define g_target_host 0
#define g_target_pd 1
#define g_target_os 2
#define g_target_pico 3

#ifndef g_target
#define g_target g_target_host
#endif

#if g_target == g_target_host || g_target == g_target_os
#define g_tco 1
#else
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

#define g_core_of(f) ((struct g_core*)((g_word)(f)&~(sizeof(g_word)-1)))
#define g_code_of(f) ((enum g_status)((g_word)(f)&(sizeof(g_word)-1)))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#define g_putnum(_) (((g_word)(_)<<1)|1)
#define g_getnum(_) ((g_word)(_)>>1)
#define g_nil g_putnum(0)
#endif
