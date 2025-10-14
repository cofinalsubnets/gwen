#ifndef _g_k_h
#define _g_k_h
#include <limine.h>
#include "i.h"
#include "libc_internal.h"


bool
  boot_ok(void);

void
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_dbg(g_core*),
  k_stop(void),
  k_reset(void)
  ;

void
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_dbg(g_core*);

void k_init(void);

typedef struct g_task {
  void *sp;
  uint32_t id, state;
  struct g_task *next;
} g_task;

#endif
