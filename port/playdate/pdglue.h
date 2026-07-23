// the SDK boundary, word-only: pdglue.c (gcc-compiled, owns pd_api.h) exports
// this surface to the mooncc side (main.c and everything under it). every
// crossing is an int/pointer -- no floats (the crank angle arrives as whole
// degrees), no variadics (pdg_log takes one string), so the moon<->gcc ABI
// seam stays inside what the differential gates prove.
#pragma once
#include <stddef.h>

// the 1-bit LCD geometry (pdglue.c static-asserts these against the SDK's)
#define PDG_ROWSIZE 52
#define PDG_ROWS    240

int pdg_crank_docked(void);
int pdg_crank_deg(void);                 // 0..359, whole degrees
void pdg_poll_buttons(void);             // latch this frame's button state
unsigned pdg_pushed(void);               // the latched fresh-press bits
void *pdg_realloc(void *p, size_t n);    // the SDK heap (16 MB on device + sim)
unsigned pdg_ms(void);                   // milliseconds since boot
void pdg_log(const char *s);             // one line to the SDK console
unsigned char *pdg_frame(void);          // the LCD frame buffer
void pdg_mark_updated(void);             // mark every row dirty
void pdg_set_update(int (*cb)(void *));  // install the per-frame callback
int pdg_file_read(const char *path, void *buf, unsigned cap);  // whole bundled file -> buf; -1 = absent

// the mooncc side (main.c): pdglue's eventHandler calls it once at init
void love_init(void);
