#include <limits.h>
#include "k.h"
#include <limine.h>
#include "fb.h"

__attribute__((used, section(".limine_requests")))
static volatile LIMINE_BASE_REVISION(3);


__attribute__((used, section(".limine_requests")))
volatile struct limine_memmap_request memmap_req = {
  .id = LIMINE_MEMMAP_REQUEST,
  .revision = 0 };

__attribute__((used, section(".limine_requests")))
volatile struct limine_stack_size_request stack_req = {
  .id = LIMINE_STACK_SIZE_REQUEST,
  .revision = 0,
  .stack_size = 1<<23, };


__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;


void  k_dbg(g_core *f) {
  k_log("\nf@0x"), k_log_n((uintptr_t) f, 16);
  if (f && g_ok(f)) {
    k_log("\n  ip=0x"), k_log_n((uintptr_t) f->ip, 16);
    k_log("\n  len="), k_log_n(f->len, 10);
    k_log("\n  allocd="), k_log_n(f->hp - f->end, 10);
    k_log("\n  stackd="), k_log_n((g_word*) f + f->len - f->sp, 10); } }

void k_log(const char *msg) {
  g_fb32_cur_msg(&k_fb, 0xffeeddcc, msg); }

static void k_log_n_r(uintptr_t n, uintptr_t base) { if (n) k_log_n(n, base); }
void k_log_n(uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  k_log_n_r(n / base, base);
  const char d[2] = {digits[dig], 0};
  k_log(d); }

  /*
static void mandelbrot(void) {
  static uint32_t colors[5] = { 0xffaaffaa, 0xaaccaacc, 0x77447744, 0x44114411, 0 };
  const size_t width = k_fb.width, height = k_fb.height;
  for (size_t px = 0; px < width; px++)
    for (size_t py = 0; py < height; py++) {
      float x0 = (float) px / (float) width * 2.47f - 2.0f,
            y0 = (float) py / (float) height * 2.24f - 1.12f,
            x = 0.0,
            y = 0.0;
      int iter = 0, max = 1000;
      while (x*x + y*y <= 4 && iter < max) {
        float z = x*x - y*y + x0;
        y = 2 * x * y + y0;
        x = z;
        iter++; }
      g_fb32_px(&k_fb, y, x, colors[iter/250]);
      
    }
}*/

//extern volatile struct limine_stack_size_request stack_req;
extern volatile struct limine_memmap_request memmap_req;

uintptr_t g_sys_clock(void) { return 0; }

void k_step(void) {
}

void k_ini(void) {

  if (!LIMINE_BASE_REVISION_SUPPORTED ||
      !framebuffer_request.response ||
      !framebuffer_request.response->framebuffer_count ||
      !memmap_req.response)
    for (;;) k_stop();

  g_fb32_ini(&k_fb);
  g_fb32_test_pattern(&k_fb);

  for (uint64_t i = 0; i < framebuffer_request.response->framebuffer_count; i++)
    k_log("\nfb"), k_log_n(i, 10), k_log(": "), k_log_n(k_fb.width, 10), k_log("x"), k_log_n(k_fb.height, 10);
  k_log("\nmemmap: "), k_log_n(memmap_req.response->entry_count, 10), k_log(" regions");

  k_log("\ng_ini()");
  static g_word g_static_pool[1<<17];
  g_core *f = g_ini_static(sizeof(g_static_pool), g_static_pool);
  k_log(".");
  k_dbg(f);
  k_log("\nboot");
  f = g_evals_(f,
#include "boot.h"
  );
  k_log(".");
  k_dbg(f);
  k_log("\ngarbage collect"), f = please(f, 1), k_log(".");
  k_dbg(f); 
  k_log("\nload interrupt descriptor table"), k_init(), k_log(".");
  k_log("\nawaiting interrupts...");
  for (;;) k_step(), k_stop(); }
