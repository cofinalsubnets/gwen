#include "k.h"
static g_core *k_g_ini(void);
static void k_log_memmap(void);
void k_ini(void) {

  if (!boot_ok()) k_fin();

  g_fb32_ini(&k_fb);
  g_fb32_test_pattern(&k_fb);

  k_log("g_ini()");
  g_core *f = k_g_ini();
  if (!g_ok(f)) k_log(" XXX f=0x"),
                k_log_n((uintptr_t) f, 16),
                k_fin();
  k_log(".");
  k_dbg(f);
  k_log("\ngarbage collect"), f = please(f, 1), k_log(".");
  k_dbg(f); 
  k_log("\nload interrupt descriptor table"), idt_ini(), k_log(".");
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

#define g_static_size  (1<<23)
static void *g_static_kmalloc(g_core *f, size_t n) {
  static g_word g_static_pool[2][g_static_size];
  return f ? NULL : g_static_pool; }
static void g_static_kfree(g_core *f, void *_) {}
static g_core *k_g_ini(void) {
  static const char boot[] =
#include "boot.h"
  ;
  return g_evals_(g_ini_m_n(g_static_kmalloc, g_static_kfree, g_static_size), boot); }

void k_fin(void) { for (;;) asm (
#if defined (__x86_64__)
"hlt"
#elif defined (__aarch64__) || defined (__riscv)
"wfi"
#elif defined (__loongarch64)
"idle 0"
#endif
); }
