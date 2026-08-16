#ifndef _AI_SYS_MMAN_H
#define _AI_SYS_MMAN_H
#include <sys/types.h>
#define PROT_NONE  0
#define PROT_READ  1
#define PROT_WRITE 2
#define PROT_EXEC  4
#define MAP_SHARED    1
#define MAP_PRIVATE   2
#define MAP_FIXED     16
#if defined(__FreeBSD__)
#define MAP_ANONYMOUS 4096
#define MAP_ANON      4096
#define MAP_POPULATE  0        /* freebsd has no populate; 0 = no-op */
#else
#define MAP_ANONYMOUS 32
#define MAP_ANON      32
#define MAP_POPULATE  32768
#endif
#define MAP_FAILED ((void*)(-1))
#if !defined(__FreeBSD__)
#define MADV_NORMAL    0
#define MADV_DONTFORK 10
#define MADV_DOFORK   11
#endif
int madvise(void*, long, int);
void *mmap(void*, long, int, int, int, long);
int munmap(void*, long);
int mprotect(void*, long, int);
#define MFD_CLOEXEC 1
int memfd_create(char const*, unsigned int);
#endif
