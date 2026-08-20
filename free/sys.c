// free/sys.c -- inle's syscall door, and the whole of it. nolibc's 76 sys/*
// members reach __ai_sys and nothing else (impl.h's sc0..sc6 -> __ai_call, no
// inline asm anywhere in that C), so answering it here is what lets that libc --
// and every lane written against it -- stand on this kernel instead of a hosted
// one. On a hosted seat the same symbol is a mksys.l lay that issues `syscall`
// or `svc`; here it is a C function, because the kernel it would have called is
// this file's other side.
//
// The numbers are LINUX'S, per arch, straight off impl.h's NR_* -- the tree
// carries those tables for x86_64 and aarch64 already, and inle owes no
// compatibility to anyone, so taking them costs nothing and translates nothing.
// os.c hands __ai_call a 1 under __inle__: the no-translation lane riscv takes.
//
// ⚠ AN UNMAPPED NUMBER ANSWERS -ENOSYS, and that is the refusal protocol, not a
// gap to be ashamed of -- the same one mount and unshare wear off linux. A lane
// asks its libc what it carries, never which kernel it is standing on.
#include "../crew/moon/lib/nolibc/impl.h"

// errno lives here rather than by linking nolibc's core.c: that member also
// carries malloc, environ, stdio and the process entry, every one of which this
// kernel already owns. Its other halves belong to a seat, and inle is the seat.
int __errno_v;
int *__errno_location(void) { return &__errno_v; }

// the kernel side (kmain.c): a raw fd through the k_sources row, no port above
// it -- and SEAT-BLIND, which is the law and not a gap. The seat is a property
// of the PORT layer: k_fd_eff is called from fd_readn, fd_writen, ai_fd_close
// and k_procseat, and from nowhere else, so an fd spelled in love is already an
// absolute row and only a port's own fd is ever remapped. A syscall sits under
// the port by construction, exactly as on a real kernel, where the number the
// trap carries is already the calling process's own.
// ⚠ THE DIVERGENCE THIS BUYS, named so it is not rediscovered as a bug: a
// SEATED task spelling `write(1, ..)` reaches row 1, where POSIX would reach
// whatever its parent seated. Nothing does -- love's stdio goes through the
// folded ports, and host/posix.c touches an implicit fd at three terminal-
// control calls and no data I/O at all. Closing it means per-task row tables
// (a real fd table), not an ambient g: `g` MOVES under collection, so the
// running task cannot be cached, only threaded.
extern long k_fd_write(int fd, void const *b, long n);
extern long k_fd_read(int fd, void *b, long n);

long __ai_sys(long n, long a, long b, long c, long d, long e, long f) {
  (void) d, (void) e, (void) f;
  switch (n) {
    case NR_write: return k_fd_write((int) a, (void const *) b, c);
    case NR_read:  return k_fd_read((int) a, (void *) b, c);
    default:       return -38; } }                       // ENOSYS, canonically
