#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>
struct g;
enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_fin(struct g*);
typedef void
  g_free_t(struct g*, void*),
  *g_malloc_t(struct g*, uintptr_t);
struct g
  *g_ini(void),
  *g_ini_dynamic(g_malloc_t*, g_free_t*),
  *g_ini_static(uintptr_t, void*),
  *g_read1(struct g*),
  *g_write1(struct g*),
  *g_read1s(struct g*, const char*),
  *g_readss(struct g*, const char*),
  *g_evals(struct g*, const char*),
  *g_evals_(struct g*, const char*),
  *g_define(struct g*, const char*),
  *g_eval(struct g*),
  *g_eval_(struct g*),
  *g_apply(struct g*),
  *g_push(struct g*, uintptr_t, ...),
  *g_pop(struct g*, uintptr_t),
  *g_strof(struct g*, const char*),
  *g_intern(struct g*),
  *g_tbl(struct g*),
  *g_gc(struct g*),
  *g_cons_l(struct g*),
  *g_cons_r(struct g*);
#define g_core_of(f) ((struct g*)((intptr_t)(f)&~(sizeof(intptr_t)-1)))
#define g_code_of(f) ((enum g_status)((intptr_t)(f)&(sizeof(intptr_t)-1)))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#define g_putnum(_) (((intptr_t)(_)<<1)|1)
#define g_getnum(_) ((intptr_t)(_)>>1)
#define g_nil g_putnum(0)
#endif
