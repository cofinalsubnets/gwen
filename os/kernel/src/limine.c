#include "k.h"

__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;

__attribute__((used, section(".limine_requests")))
static volatile LIMINE_BASE_REVISION(3);

__attribute__((used, section(".limine_requests")))
volatile struct limine_framebuffer_request framebuffer_request = {
    .id = LIMINE_FRAMEBUFFER_REQUEST,
    .revision = 0 };

__attribute__((used, section(".limine_requests")))
volatile struct limine_memmap_request memmap_req = {
  .id = LIMINE_MEMMAP_REQUEST,
  .revision = 0 };

__attribute__((used, section(".limine_requests")))
volatile struct limine_stack_size_request stack_req = {
  .id = LIMINE_STACK_SIZE_REQUEST,
  .revision = 0,
  .stack_size = 1<<23, };

__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;

bool limine_ok(void) { return
  LIMINE_BASE_REVISION_SUPPORTED &&
  framebuffer_request.response &&
  framebuffer_request.response->framebuffer_count &&
  memmap_req.response; }
