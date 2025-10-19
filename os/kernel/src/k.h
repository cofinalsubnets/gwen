#ifndef _g_k_h
#define _g_k_h
#include <limine.h>
#include "i.h"
#include "libc_internal.h"
#include "fb.h"

#define G_FREQ 100
#define G_TIMER_HZ G_FREQ

extern g_fb32 k_fb;
void
  k_stop(void),
  k_reset(void),
  k_dbg(g_core*),
  k_sleep_ticks(uintptr_t),
  k_init(void);

#define k_sleep(n) k_sleep_ticks((n)*G_FREQ)

typedef struct g_task {
  uintptr_t *sp, id, wait;
  struct g_task *next;
} g_task;

void resume(g_task*);

extern uintptr_t g_ticks;
uintptr_t g_new_id(void);

//extern volatile struct limine_stack_size_request stack_req;
extern volatile struct limine_memmap_request memmap_req;

bool boot_check(void);

#endif
