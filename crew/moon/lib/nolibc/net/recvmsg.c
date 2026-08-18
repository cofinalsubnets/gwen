#include "../impl.h"

/* sendmsg's mirror: the kernel fills the BSD twins in our scratch, the heads
 * rewrite back to the canon in the caller's buffers. control past the scratch
 * comes back MSG_CTRUNC -- the kernel's own word for it. */
long recvmsg(int fd, struct msghdr *m, int fl) {
  if (__ai_osv < 2) return er(sc3(NR_recvmsg, fd, (long) m, fl));
  struct sockaddr_storage sa;
  unsigned char cb[256];
  struct __fb_msghdr f = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  f.iov = m->msg_iov; f.iovlen = (int) m->msg_iovlen;
  if (m->msg_name) { f.name = &sa; f.namelen = sizeof sa; }
  unsigned long cap = m->msg_controllen;
  if (m->msg_control && cap)
    f.control = cb, f.controllen = cap > sizeof cb ? (unsigned int) sizeof cb : (unsigned int) cap;
  long r = er(sc3(NR_recvmsg, fd, (long) &f, (long) __ai_msgfb(fl)));
  if (r < 0) return r;
  if (m->msg_name) {
    unsigned int n = f.namelen;
    __ai_saout(&sa, n);
    memcpy(m->msg_name, &sa, n > m->msg_namelen ? m->msg_namelen : n);
    m->msg_namelen = n; }
  if (f.control) {
    unsigned long cn = f.controllen, at = 0;
    unsigned char *o = m->msg_control;
    while (at + 12 <= cn) {
      unsigned long l = *(unsigned int*) (cb + at);
      int lv = *(int*) (cb + at + 4), ty = *(int*) (cb + at + 8);
      if (l < 12 || l > cn - at) break;
      if (lv == 0xffff) lv = 1;
      *(unsigned long*) (o + at) = l;
      *(int*) (o + at + 8) = lv;
      *(int*) (o + at + 12) = ty;
      if (l > 16) memcpy(o + at + 16, cb + at + 16, l - 16);
      at += (l + 7) & ~7UL; }
    m->msg_controllen = cn; }
  else m->msg_controllen = 0;
  m->msg_flags = (int) __ai_msgcan(f.flags);
  return r; }
