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
// it. ⚠ SEAT-BLIND, and knowingly: k_fd_eff maps a task's 0/1/2 through its
// seat and needs the running task, which it reads off `g` -- and a syscall
// arrives from inside nolibc with no g threaded to it. Nothing seated calls
// this yet; when host/posix.c's nifs do, they hold a g at the nif and the seat
// has to reach __ai_sys from there or not at all.
extern long k_fd_write(int fd, void const *b, long n);
extern long k_fd_read(int fd, void *b, long n);

long __ai_sys(long n, long a, long b, long c, long d, long e, long f) {
  (void) d, (void) e, (void) f;
  switch (n) {
    case NR_write: return k_fd_write((int) a, (void const *) b, c);
    case NR_read:  return k_fd_read((int) a, (void *) b, c);
    default:       return -38; } }                       // ENOSYS, canonically
