#include "../impl.h"

/* both BSDs share one msghdr/cmsghdr shape (48 bytes, int-wide lengths, the
 * 12-byte cmsg head): the cmsg DATA offset (16) and the CMSG_LEN values agree
 * with the canon, so translation rewrites only the heads. under SOL_SOCKET
 * only SCM_RIGHTS is spelled; an unmapped type refuses loudly. control rides
 * a fixed scratch -- a bigger one refuses, never a torn walk. */
long sendmsg(int fd, struct msghdr const *m, int fl) {
  if (__ai_osv < 2) return er(sc3(NR_sendmsg, fd, (long) m, fl));
  struct sockaddr_storage sa;
  unsigned char cb[256];
  struct __fb_msghdr f = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  f.iov = m->msg_iov; f.iovlen = (int) m->msg_iovlen;
  if (m->msg_name) {
    if (m->msg_namelen > sizeof sa) return er(-EINVAL);
    f.namelen = __ai_sain(m->msg_name, m->msg_namelen, &sa);
    f.name = &sa; }
  unsigned long cn = m->msg_controllen;
  if (cn) {
    unsigned char const *c = m->msg_control;
    unsigned long at = 0;
    if (cn > sizeof cb || !c) return er(-EINVAL);
    memset(cb, 0, cn);
    while (at + 16 <= cn) {
      unsigned long l = *(unsigned long const*) (c + at);
      int lv = *(int const*) (c + at + 8), ty = *(int const*) (c + at + 12);
      if (l < 16 || l > cn - at) return er(-EINVAL);
      if (lv == 1) { if (ty != 1) return er(-EINVAL); lv = 0xffff; }
      *(unsigned int*) (cb + at) = (unsigned int) l;
      *(int*) (cb + at + 4) = lv;
      *(int*) (cb + at + 8) = ty;
      memcpy(cb + at + 16, c + at + 16, l - 16);
      at += (l + 7) & ~7UL; }
    f.control = cb; f.controllen = (unsigned int) cn; }
  return er(sc3(NR_sendmsg, fd, (long) &f, (long) __ai_msgfb(fl))); }
