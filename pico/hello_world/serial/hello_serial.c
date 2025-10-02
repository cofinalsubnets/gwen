/**
 * Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <stdio.h>
#include "pico/stdlib.h"
#include "pico/time.h"
#include "i.h"
static const char boot[] =
#include "boot.h"
;

uintptr_t g_clock(void) {
  return get_absolute_time(); }

#define g_static_size  (1<<14)
static g_word g_static_pool[2][g_static_size];

int main() {
    stdio_init_all();
    printf("Hello, world!\n");
    g_core *f = g_ini_static(g_static_size, g_static_pool);
    printf("got f@0x%x", (unsigned int) f);
    if (g_ok(f)) f = g_evals_(f, boot),
                 printf("post boot got f@0x%x\n", (unsigned int) f);
    else printf("not ok, skip boot sequence\n");

    printf("entering loop ...\n");
    while (true) {

        printf("Hello from in the loop\n");
        sleep_ms(1000);
    }
}
