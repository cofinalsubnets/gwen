#ifndef gw_h
#define gw_h
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef intptr_t g_word;

typedef struct g_core g_core;

typedef enum g_status {
  g_status_ok,
  g_status_oom,
  g_status_eof,
} g_status;

void
  g_fin(g_core*);

g_core
  *g_ini(void),
  *g_run(g_core*, const char*, const char**);

#define g_core_of(f) ((g_core*)((g_word)(f)&~3))
#define g_code_of(f) ((g_status)((g_word)(f)&3))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#endif
