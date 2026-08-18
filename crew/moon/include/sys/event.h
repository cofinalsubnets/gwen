#ifndef _AI_SYS_EVENT_H
#define _AI_SYS_EVENT_H
/* kqueue, the BSD door (linux answers ENOSYS; signalfd is the twin there).
 * the canonical face is freebsd's (struct + the negative filters); netbsd's
 * 40-byte record and positive filters translate in the member. EVFILT_SIGNAL
 * idents speak CANONICAL signal numbers both ways (SIGCHLD is 17 here, never
 * the kernel's 20) -- the one filter whose ident the permutation rides. */
struct kevent {
  unsigned long ident;
  short filter;
  unsigned short flags;
  unsigned int fflags;
  long data;
  void *udata;
  unsigned long ext[4];
};
#define EVFILT_READ    (-1)
#define EVFILT_WRITE   (-2)
#define EVFILT_VNODE   (-4)
#define EVFILT_PROC    (-5)
#define EVFILT_SIGNAL  (-6)
#define EVFILT_TIMER   (-7)
#define EV_ADD     0x1
#define EV_DELETE  0x2
#define EV_ENABLE  0x4
#define EV_DISABLE 0x8
#define EV_ONESHOT 0x10
#define EV_CLEAR   0x20
#define EV_EOF     0x8000
#define EV_ERROR   0x4000
#define EV_SET(ev, a, b, c, d, e, f) do { struct kevent *__ev = (ev); \
  __ev->ident = (unsigned long) (a); __ev->filter = (short) (b); \
  __ev->flags = (unsigned short) (c); __ev->fflags = (unsigned int) (d); \
  __ev->data = (long) (e); __ev->udata = (void*) (f); } while (0)
struct timespec;
int kqueue(void);
int kevent(int kq, struct kevent const *ch, int nch,
           struct kevent *ev, int nev, struct timespec const *ts);
#endif
