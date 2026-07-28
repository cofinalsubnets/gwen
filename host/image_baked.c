// host/image_baked.c -- the in-binary home of the post-boot heap image (doc/snapshot.md).
// `love --bake` dumps its own warmed heap into the .image section of its own file on disk, and the
// binary loads ITS OWN dump at startup (main.c) -- identical layout by construction, so the codec's
// same-binary +delta relocation just works. Sentinel-initialized (not {0}) so it lands in PROGBITS,
// patchable in place, never .bss.
// The section is laid LAST -- alone in the highest segment on the host (host/build.mk's
// --section-start), riding the tail of the single segment holo lays -- so the bake GROWS it: the
// blob is appended at the tail of the file and the phdr is rewritten to reach it. Nothing is
// pre-allocated and there is no ceiling; this stub exists only to give the section an address.
#include <stdint.h>
#define RESERVE_WORDS 2u
__attribute__((section(".image"))) uint64_t ai_baked_image[RESERVE_WORDS] = {1};
uintptr_t ai_baked_image_len = RESERVE_WORDS * 8u;
