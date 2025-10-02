#include "k.h"
#include "fb.h"
static void k_log_memmap(void);
static const char boot[] =
#include "boot.h"
;
#define g_static_size  (1<<23)
static g_word g_static_pool[2][g_static_size];
void k_ini(void) {
  if (!boot_ok()) k_fin();

  g_fb32_ini(&k_fb);
  g_fb32_test_pattern(&k_fb);

  k_log("g_ini()");
  g_core *f = g_ini_static(g_static_size, g_static_pool);
  f = g_evals_(f, boot);
  if (!g_ok(f)) k_log(" XXX f=0x"),
                k_log_n((uintptr_t) f, 16),
                k_fin();
  k_log(".");
  k_dbg(f);
  k_log("\ngarbage collect"), f = please(f, 1), k_log(".");
  k_dbg(f); 

  k_log_memmap();

//  k_log("\nisr_0()"), isr_0(), k_log(".");
//  k_log("\nint $0"); asm ("int $0"); k_log(".");

  g_fin(f);
  k_fin(); }

static void k_log_memmap(void) {
  k_log("\nmemmap:");
  for (size_t i = 0; i < memmap_req.response->entry_count; i++)
    k_log("\n "), k_log_n(memmap_req.response->entries[i]->type, 10),
    k_log(" 0x"), k_log_n(memmap_req.response->entries[i]->base, 16),
    k_log(" 0x"), k_log_n(memmap_req.response->entries[i]->length, 16); }


void k_fin(void) { for (;;) asm (
#if defined (__x86_64__)
"hlt"
#elif defined (__aarch64__) || defined (__riscv)
"wfi"
#elif defined (__loongarch64)
"idle 0"
#endif
); }

uintptr_t g_clock(void) { return 0; }
