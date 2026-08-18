#include "../impl.h"

/* a block worth its own mapping gets one, and free hands that back whole: the
   arena list below never returns a page, so a heap that peaks through a few
   large blocks would keep every one of them resident for the run. ⚠ the tag
   rides `next', which an allocated block does not otherwise use -- so malloc
   writes it on EVERY block, or free reads whatever the old payload left there. */
#define MDirect ((__mhdr *) 1)
#define MBig ((size_t) 128 * 1024)

static __mhdr __mbase;
static __mhdr *__mfree;
void free(void *p) {
  if (!p) return;
  __mhdr *b = (__mhdr *) p - 1, *q = __mfree;
  if (b->next == MDirect) { munmap(b, (long) (b->size * sizeof(__mhdr))); return; }
  for (; !(b > q && b < q->next); q = q->next)
    if (q >= q->next && (b > q || b < q->next)) break;   /* at the arena's wrap point */
  if (b + b->size == q->next) { b->size += q->next->size; b->next = q->next->next; }
  else b->next = q->next;
  if (q + q->size == b) { q->size += b->size; q->next = b->next; }
  else q->next = b;
  __mfree = q; }
static __mhdr *__mcore(size_t nu) {
  size_t need = (nu + 1) * sizeof(__mhdr);
  size_t len = need < (1UL << 20) ? (1UL << 20) : ((need + 4095UL) & ~4095UL);
  void *m = mmap(0, (long) len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (m == (void *) -1) return 0;
  __mhdr *u = m;
  u->size = len / sizeof(__mhdr);
  u->next = 0;
  free((void *) (u + 1));
  return __mfree; }
static void *__mbig(size_t nu) {                        /* its own mapping, given back whole */
  size_t len = (nu * sizeof(__mhdr) + 4095UL) & ~4095UL;
  void *m = mmap(0, (long) len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (m == (void *) -1) { __errno_v = ENOMEM; return 0; }
  __mhdr *u = m;
  u->size = len / sizeof(__mhdr);
  u->next = MDirect;
  return (void *) (u + 1); }
void *malloc(size_t n) {
  size_t nu = (n + sizeof(__mhdr) - 1) / sizeof(__mhdr) + 1;
  if (n >= MBig) return __mbig(nu);
  __mhdr *prev = __mfree;
  if (!prev) { __mbase.next = __mfree = prev = &__mbase; __mbase.size = 0; }
  for (__mhdr *q = prev->next; ; prev = q, q = q->next) {
    if (q->size >= nu) {
      if (q->size == nu) prev->next = q->next;
      else { q->size -= nu; q += q->size; q->size = nu; }
      __mfree = prev;
      q->next = 0;                                      /* the tag, written before any payload can */
      return (void *) (q + 1); }
    if (q == __mfree)
      if (!(q = __mcore(nu))) { __errno_v = ENOMEM; return 0; } } }
