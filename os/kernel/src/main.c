#include "k.h"
#include "log.h"

#define g_static_size (1<<20)

__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
#define _L __attribute__((used, section(".limine_requests"))) static volatile
_L LIMINE_BASE_REVISION(3);
_L struct limine_memmap_request memmap_req = { .id = LIMINE_MEMMAP_REQUEST, .revision = 0 };
_L struct limine_hhdm_request hhdm_req = { .id = LIMINE_HHDM_REQUEST, .revision = 0 };
_L struct limine_framebuffer_request fb_req = { .id = LIMINE_FRAMEBUFFER_REQUEST, .revision = 0 };
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;


static struct mem *kgetmem(uint64_t hhdm, uint64_t n, struct limine_memmap_entry **rr, uint64_t *nbs, uint64_t *nrs) {
  if (!n) return NULL;
  struct limine_memmap_entry *r = *rr;
  struct mem *next = kgetmem(hhdm, n-1, rr+1, nbs, nrs);
  if (r && r->type == 0 && r->length >= sizeof(struct mem) && r->base + r->length - 1 < 1l<<32) {
    struct mem *m = (struct mem*) (hhdm + r->base);
    *nrs += 1;
    *nbs += m->len = r->length;
    m->next = next;
    return m; }
  return next; }


struct k K;
void kmain(void) {
  arch_init(); // arch specific init

  // framebuffer init
  if (!fb_req.response || !fb_req.response->framebuffer_count) for (;;) k_stop();

  struct limine_framebuffer *fb0 = fb_req.response->framebuffers[0];
  g_fb32_ini(&K.fb, fb0->address, fb0->width, fb0->height, fb0->pitch);
  intptr_t x0 = K.fb.width / 2,
           y0 = K.fb.height / 2,
           rad = MIN(x0, y0);
  for (uint32_t color = 0xff0000, step = color / rad; rad; color -= step, rad--)
    for (intptr_t x = x0 - rad; x <= x0 + rad; x++)
      for (intptr_t y = y0 - rad; y <= y0 + rad; y++) {
        intptr_t dx = x - x0, dy = y - y0;
        if (dx * dx + dy * dy < rad * rad)
          g_fb32_px(&K.fb, y, x, color * (uintptr_t) &K); }
  k_logf("%dx%dp %dx%dc\n", K.fb.width, K.fb.height, K.fb.width >> 3, K.fb.height >> 3);

  // mem init
  if (!memmap_req.response || !hhdm_req.response) for (;;) k_stop();

  struct limine_memmap_entry **rr = memmap_req.response->entries;
  uint64_t hhdm = hhdm_req.response->offset,
           n = memmap_req.response->entry_count,
           nrs = 0,
           nbs = 0;
  K.free = kgetmem(hhdm, n, rr, &nbs, &nrs);
  K.used = NULL;
  k_logf("%dK %dr\n", nbs >> 10, nrs);

  // lisp init
  static g_word g_static_pool[g_static_size];
  g_core *f = K.f = please(g_evals_(g_ini_static(sizeof(g_static_pool), g_static_pool),
#include "boot.h"
  ), 0);
  k_logf("f@0x%x\n", (uintptr_t) f);
  if (f && g_ok(f)) k_logf(
    " pool=0x%x\n len=%d\n ip=0x%x\n allocd=%d\n stackd=%d\n",
    f->pool, f->len, f->ip, (uintptr_t) (f->hp - f->end), (g_word*) f + f->len - f->sp);

  k_logf("%d ticks ok.\n", K.ticks);
  for (;;) {
    k_log_char(key_get());
    k_stop(); } }
