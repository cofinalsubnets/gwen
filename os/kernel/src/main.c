#include "k.h"
#include "log.h"
#include "limine.h"
extern uintptr_t g_ticks;

g_fb32 k_fb;
#define g_static_size (1<<20)

__attribute__((used, section(".limine_requests")))
static volatile LIMINE_BASE_REVISION(3);
__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
__attribute__((used, section(".limine_requests"))) volatile struct limine_memmap_request memmap_req = { .id = LIMINE_MEMMAP_REQUEST, .revision = 0 };
__attribute__((used, section(".limine_requests"))) volatile struct limine_hhdm_request hhdm_req = { .id = LIMINE_HHDM_REQUEST, .revision = 0 };
/*
__attribute__((used, section(".limine_requests")))
volatile struct limine_stack_size_request stack_req = {
  .id = LIMINE_STACK_SIZE_REQUEST,
  .revision = 0,
  .stack_size = 1<<23, };
  */
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;

typedef struct mem_region {
  uintptr_t len;
  struct mem_region *next;
  uint8_t mem[];
} mem_region;


static void fb_init(void) {
  if (!framebuffer_request.response ||
      !framebuffer_request.response->framebuffer_count)
    for (;;) k_stop();
  struct limine_framebuffer *fb = framebuffer_request.response->framebuffers[0];
  g_fb32_ini(&k_fb, fb->address, fb->width, fb->height, fb->pitch);
  g_fb32_test_pattern(&k_fb); }

static mem_region *mem_init(void) {
  if (!memmap_req.response || !hhdm_req.response)
    for (;;) k_stop();
  mem_region *memory = NULL;
  uint64_t hhdm = hhdm_req.response->offset;
  uint64_t n = memmap_req.response->entry_count, nrs = 0, nbs = 0;
  struct limine_memmap_entry **r = memmap_req.response->entries;
  for (uint64_t i = 0; i < n; i++)
    if (r[i] &&
        r[i]->type == 0 &&
        r[i]->length >= sizeof(mem_region) &&
        r[i]->base + r[i]->length - 1 < (1l<<32)) {
      mem_region *reg = (mem_region*) (hhdm + r[i]->base);
      reg->next = memory;
      memory = reg;
      nrs += 1;
      nbs += reg->len = r[i]->length; }
  k_log("\n");
  k_log_n(nbs >> 10, 10);
  k_log("KiB in ");
  k_log_n(nrs, 10);
  k_log(" regions:");
  for (mem_region *r = memory; r; r = r->next) {
    k_log("\n ");
    k_log_n(r->len>>10, 10);
    k_log("KiB@0x");
    k_log_n((uintptr_t)r, 16); }

  return memory; }


g_core *lisp_init(void) {
  static g_word g_static_pool[g_static_size];
  g_core *f = please(g_evals_(g_ini_static(sizeof(g_static_pool), g_static_pool),
#include "boot.h"
  ), 0);
  k_log("\nf@0x"), k_log_n((uintptr_t) f, 16);
  if (f && g_ok(f)) {
    k_log("\n ip=0x"), k_log_n((uintptr_t) f->ip, 16);
    k_log("\n len="), k_log_n(f->len, 10);
    k_log("\n allocd="), k_log_n(f->hp - f->end, 10);
    k_log("\n stackd="), k_log_n((g_word*) f + f->len - f->sp, 10); }
  return f; }

void kmain(void) {
  k_init();
  fb_init();
  mem_region *memory = mem_init();
  g_core *f = lisp_init();
  k_log("\n");
  k_log_n(g_ticks, 10);
  k_log(" ticks ok.\n");
  for (;;) k_stop(); }
