#ifndef _g_k_h
#define _g_k_h
#include <limine.h>
#include "i.h"
#include "libc_internal.h"

extern volatile struct limine_framebuffer_request framebuffer_request;
extern volatile struct limine_memmap_request memmap_req;
extern volatile struct limine_stack_size_request stack_req;

typedef struct g_fb32 {
  size_t width, height, pitch, cur_x, cur_y;
  volatile uint32_t *_; } g_fb32;

bool limine_ok(void);
extern g_fb32 k_fb;
void
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_px_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint32_t px, uint8_t *bmp),
  g_fb32_px_msg(g_fb32 *fb, size_t row, size_t col, uint32_t px, const char *msg),
  g_fb32_cur_px_msg(g_fb32 *fb, uint32_t px, const char *msg),
  k_fb_ini(void),
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_set_cursor(size_t, size_t),
  k_dbg(g_core*)
  ;

__attribute__((packed))
struct idt_entry {
  uint16_t offset_1,
           selector;
  uint8_t ist,
          type_attributes;
  uint16_t offset_2;
  uint32_t offset_3,
           zero; };
extern struct idt_entry idt[256];
_Static_assert(sizeof(struct idt_entry) == 16);
_Static_assert(sizeof(idt) == 256 * sizeof(struct idt_entry));

void load_idt(uint16_t, uint64_t);
void idt_ini(void);

__attribute__((packed))
struct limit_base {
  uint16_t limit;
  uint64_t base; };

extern struct limit_base idtr;
#endif
