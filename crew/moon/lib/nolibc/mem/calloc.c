#include "../impl.h"

/* the multiply CHECKED, as reallocarray's is: a wrap hands back a block smaller
 * than the caller is about to fill. And a block that came from a mapping of its
 * OWN arrives zeroed from the kernel -- only the arena's blocks carry the last
 * owner's leavings, so only they are worth walking. */
void *calloc(size_t n, size_t sz) {
  if (sz && n > (size_t) -1 / sz) { __errno_v = ENOMEM; return 0; }
  size_t t = n * sz;
  void *p = malloc(t);
  if (p && ((__mhdr *) p - 1)->next != __MDirect) memset(p, 0, t);
  return p; }
