#include <sys/event.h>
#include "../impl.h"

/* the canonical record is freebsd's; netbsd repacks to __kevent50's 40 bytes
 * with the positive filter (= -canon - 1, an involution). EVFILT_SIGNAL is
 * the one filter whose ident is a signal number, so it alone rides the
 * permutation -- in through sigfb, out through sigcan; a signal with no BSD
 * twin (STKFLT, PWR) refuses EINVAL. */
int kevent(int kq, struct kevent const *ch, int nch,
           struct kevent *ev, int nev, struct timespec const *ts) {
  if (__ai_osv < 2) { __errno_v = ENOSYS; return -1; }
  int nb = __ai_osv == 3;
  long nr = nb ? 435 : NR_fb_kevent;   /* __kevent50 */
  /* changes first (chunked through the translation buffer, no timeout leg),
   * then the wait -- the order one kernel call keeps. */
  for (int at = 0; at < nch; at += 8) {
    struct kevent fc[8]; struct __nb_kevent nc[8];
    int k = nch - at > 8 ? 8 : nch - at;
    for (int i = 0; i < k; i++) {
      struct kevent const *c = ch + at + i;
      long id = (long) c->ident;
      if (c->filter == EVFILT_SIGNAL && (id = __ai_sigfb(id)) < 0) {
        __errno_v = EINVAL; return -1; }
      if (nb) {
        nc[i].ident = (unsigned long) id;
        nc[i].filter = (unsigned int) (-(long) c->filter - 1);
        nc[i].flags = c->flags; nc[i].fflags = c->fflags; nc[i]._p0 = 0;
        nc[i].data = c->data; nc[i].udata = c->udata; }
      else fc[i] = *c, fc[i].ident = (unsigned long) id; }
    if (er(fb6(nr, kq, (long) (nb ? (void*) nc : (void*) fc), k, 0, 0, 0)) < 0)
      return -1; }
  if (nev <= 0) return 0;
  long got;
  if (!nb) {
    got = er(fb6(nr, kq, 0, 0, (long) ev, nev, (long) ts));
    if (got < 0) return -1;
    for (long i = 0; i < got; i++)
      if (ev[i].filter == EVFILT_SIGNAL)
        ev[i].ident = (unsigned long) __ai_sigcan((long) ev[i].ident); }
  else {
    /* through the repack buffer: up to 8 a call (fewer than asked is a legal
     * kevent answer; a hungrier caller loops) */
    struct __nb_kevent no[8];
    got = er(fb6(nr, kq, 0, 0, (long) no, nev > 8 ? 8 : nev, (long) ts));
    if (got < 0) return -1;
    for (long i = 0; i < got; i++) {
      long f = -(long) no[i].filter - 1;
      long id = (long) no[i].ident;
      if (f == EVFILT_SIGNAL) id = __ai_sigcan(id);
      ev[i].ident = (unsigned long) id; ev[i].filter = (short) f;
      ev[i].flags = (unsigned short) no[i].flags; ev[i].fflags = no[i].fflags;
      ev[i].data = no[i].data; ev[i].udata = no[i].udata;
      ev[i].ext[0] = ev[i].ext[1] = ev[i].ext[2] = ev[i].ext[3] = 0; } }
  return (int) got; }
