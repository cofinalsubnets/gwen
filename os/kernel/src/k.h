#ifndef _g_k_h
#define _g_k_h
#include "limine.h"
#include "i.h"
#include "libc_internal.h"
#include "fb.h"

#define G_FREQ 100
#define G_TIMER_HZ G_FREQ
void
  k_stop(void),
  k_reset(void),
  k_init(void);

typedef struct g_task {
  uintptr_t *sp, id, wait;
  struct g_task *next;
} g_task;

void resume(g_task*);

//extern volatile struct limine_stack_size_request stack_req;
extern volatile struct limine_memmap_request memmap_req;

bool boot_check(void);

#endif
