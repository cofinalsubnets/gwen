// host/image_baked.c -- the in-binary home of the post-boot heap image (doc/snapshot.md).
// `love --bake` dumps its own warmed heap into the .image section of its own file on disk, and the
// binary loads ITS OWN dump at startup (main.c) -- identical layout by construction, so the codec's
// same-binary +delta relocation just works. Sentinel-initialized (not {0}) so it lands in PROGBITS,
// patchable in place, never .bss.
// AI_IMAGE_TAIL says the linker laid .image LAST, alone in the highest segment (host/build.mk's
// --section-start), so the bake GROWS it: the blob is appended at the tail of the file and the phdr
// is rewritten to reach it. Nothing to pre-allocate, no ceiling -- the stub only has to give the
// section an address. Without it (the mooncc/holo lane) .image is a FIXED RESERVE the bake fills in
// place, and an image bigger than the reserve errors telling you to bump RESERVE_WORDS.
#include <stdint.h>
#ifdef AI_IMAGE_TAIL
#define RESERVE_WORDS 2u                                       /* a stub: an address, not a reserve */
#else
#define RESERVE_WORDS 786432u                                  /* 6 MiB */
#endif
__attribute__((section(".image"))) uint64_t ai_baked_image[RESERVE_WORDS] = {1};
uintptr_t ai_baked_image_len = RESERVE_WORDS * 8u;
