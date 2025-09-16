#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>

typedef intptr_t g_word;
typedef struct g_core g_core;

typedef enum g_status {
  g_status_ok = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_status;
void
  *g_malloc(g_core*, size_t),
  g_free(g_core*, void*),
  g_fin(g_core*);
g_core
  *g_ini_m(void *(*)(g_core*, size_t), void (*)(g_core*, void*)),
  *g_ini(void),
  *g_main(g_core*, const char*, const char**),
  *g_run(g_core*);

#define g_core_of(f) ((g_core*)((g_word)(f)&~3))
#define g_code_of(f) ((g_status)((g_word)(f)&3))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#endif
