#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>
#define g_core_of(f) ((struct g_core*)((g_word)(f)&~3))
#define g_code_of(f) ((enum g_status)((g_word)(f)&3))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#define g_putnum(_) (((g_word)(_)<<1)|1)
#define g_getnum(_) ((g_word)(_)>>1)
#define g_nil g_putnum(0)
typedef intptr_t g_word;
typedef union g_cell g_cell;
typedef struct g_core g_core;
typedef void *g_malloc_t(g_core*, size_t),
              g_free_t(g_core*, void*);
g_malloc_t g_malloc;
g_free_t g_free;
g_core
  *g_ini_m(g_malloc_t*, g_free_t*),
  *g_read1s(g_core*, const char*),
  *g_readss(g_core*, const char*),
  *g_eval(g_core*),
  *g_evals(g_core*, const char*),
  *g_apply(g_core*),
  *g_read1(g_core*),
  *g_write1(g_core*),
  *g_push(g_core*, uintptr_t, ...),
  *g_pop(g_core*, uintptr_t),
  *g_strof(g_core*, const char*),
  *g_intern(g_core*),
  *g_tbl(g_core*),
  *g_cons_l(g_core*),
  *g_cons_r(g_core*);


#define g_ini() g_ini_m(g_malloc, g_free)
enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_fin(g_core*);
#endif
