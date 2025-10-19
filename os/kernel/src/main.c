#include "k.h"
#include "log.h"

uintptr_t g_ticks = 0;

void k_sleep_ticks(uintptr_t ticks) {
  uintptr_t t0 = g_ticks;
  while (g_ticks - t0 < ticks) k_stop(); }

uintptr_t g_new_id(void) {
  static uintptr_t i = 0;
  return i++; }

uintptr_t g_sys_clock(void) {
  return g_ticks; }

static void show_memmap(void) {
  uint64_t n = memmap_req.response->entry_count;
  struct limine_memmap_entry **r = memmap_req.response->entries;
  k_log("\nmemmap: ");
  k_log_n(n, 10);
  k_log(" regions:");
  for (uint64_t i = 0; i < n; i++)
    k_log("\n"),
    k_log_n(r[i]->type, 10),
    k_log(" 0x"),
    k_log_n(r[i]->base, 16),
    k_log("-0x"),
    k_log_n(r[i]->base + r[i]->length, 16),
    k_log(" "),
    k_log_n(r[i]->length>>10, 10),
    k_log("KiB"); }

static void show_fb(void) {
  g_fb32_test_pattern(&k_fb);
  k_log("\nfb[0] ");
  k_log_n(k_fb.width, 10);
  k_log("x");
  k_log_n(k_fb.height, 10); }

g_task *next_task(g_task *cur) {
  for (g_task *next = cur->next; next != cur; next = next->next)
    if (g_ticks >= next->wait) return next;
  return cur; }

static g_task
  main_task = { .wait = 0, .next = &main_task },
  *cur = &main_task;

#define g_static_size (1<<20)
static g_word g_static_pool[g_static_size];
void kmain(void) {
  if (!boot_check()) for (;;) k_stop();
  struct limine_framebuffer *fb = framebuffer_request.response->framebuffers[0];
  g_fb32_ini(&k_fb, fb->address, fb->width, fb->height, fb->pitch);
  show_fb();
  show_memmap();
  k_log("\nk_init()"), k_init(), k_log(".");
  k_log("\ng_ini_static("), k_log_n(g_static_size, 10), k_log(", 0x"), k_log_n((uintptr_t) g_static_pool, 16), k_log(")");
  g_core *f = g_ini_static(sizeof(g_static_pool), g_static_pool);
  k_log(".");
  k_log("\nboot");
  f = g_evals_(f,
#include "boot.h"
  );
  k_log(" in "), k_log_n(g_ticks, 10), k_log(" ticks.");
  k_log("\ngarbage collect"), f = please(f, 1), k_log(".");
  k_dbg(f); 

  for (g_task *next;;) {
    for (next = cur->next; next != cur; next = next->next)
      if (g_ticks >= next->wait) break;
    if (next == &main_task) k_stop();
    else resume(cur = next); } }
