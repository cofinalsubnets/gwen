/* __attribute__((aligned(N))) on a data declarator is HONORED, not skipped --
 * a skipped aligned(N) silently misaligns the object (page tables, DMA), which
 * is worse than the old parse error. exercises: the array-declarator trailing
 * run (the shape that refused), a scalar between neighbours that would land it
 * off-grain, an initialized aligned global (bytes AND boundary), and bare
 * `aligned` (gcc's biggest-useful, 16). gcc agrees on every exit. */
typedef unsigned long u64;

static char crumb1 = 1;                                   /* off-grain neighbours: the pad is real */
static u64 pt[512] __attribute__((aligned(4096)));        /* a page table's shape */
static char crumb2 = 2;
static int mid __attribute__((aligned(64))) = 7;
char pub[5] __attribute__((aligned(256))) = {9, 8};       /* exported + initialized */
static int bare __attribute__((aligned)) = 3;             /* no N: 16 */

int main(void) {
  if ((u64) pt & 4095) return 1;
  if ((u64) &mid & 63) return 2;
  if ((u64) pub & 255) return 3;
  if ((u64) &bare & 15) return 4;
  if (crumb1 != 1 || crumb2 != 2 || mid != 7 || bare != 3) return 5;
  if (pub[0] != 9 || pub[1] != 8 || pub[4] != 0) return 6;
  pt[0] = 30; pt[511] = 12;
  if (pt[0] + pt[511] != 42) return 7;
  return 42;
}
