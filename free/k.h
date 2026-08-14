// Freestanding-kernel glue shared by kmain.c and the per-arch backends.
// Combines the former free/k.h (kernel logging) and free/k_boot.h (the boot
// hand-off struct).
#pragma once
#include "love.h"

void
 k_logf(char const*, ...),
 k_vlogf(char const*, va_list),
 k_log_clear(void),
 k_log_char(char);

// k_boot -- the small struct the bootloader hand-off populates before
// kmain() runs. kmain.c reads this in meminit/fbinit; the Limine path
// fills it in limine_to_kboot(). Adding a new backend is just "write
// something that fills kboot, then jump to kmain."
#include <stdint.h>
#include <stdbool.h>

// khhdm -- the higher-half direct-map offset (physical P is reachable at
// khhdm + P). Defined in kmain.c. 0 means identity-mapped.
extern uintptr_t khhdm;

#define k_boot_ram_max 64

struct k_boot {
  uint32_t ram_n;
  struct { uintptr_t base, len; } ram[k_boot_ram_max];
  uintptr_t hhdm;                     // 0 means identity-mapped (UEFI path)
  struct {
    void    *base;                    // framebuffer linear address
    uint16_t w, h;                    // pixels
    uint32_t pitch_px;                // pixels per scanline (not bytes)
  } fb;
  bool has_fb;
  // the wall date at boot, UNIX SECONDS -- what makes ai_clock a clock and not an
  // uptime. Limine answers it; the doors that do not (-kernel, UEFI) fall back to
  // the machine's RTC in kmain. ⚠ 0 is "nobody knew", not midnight 1970: a stat
  // then reads as its own uptime, which is wrong but at least visibly so.
  uint64_t date;
  // the boot command line, copied whole at hand-off (limine's request, PVH's
  // start_info, the DTB's /chosen bootargs). "" is a plain boot: the love-side
  // split leaves cmdline seatless and the console shell takes over.
  char cmdline[256];
};

extern struct k_boot kboot;

// copy a hand-off cmdline into kboot; n bounds a source that may not be
// NUL-terminated (the DTB prop), ~0 for the C-string doors.
static inline void k_cmdline(char const *s, uintptr_t n) {
  uintptr_t i = 0;
  for (; i < n && i + 1 < sizeof kboot.cmdline && s[i]; i++) kboot.cmdline[i] = s[i];
  kboot.cmdline[i] = 0; }
