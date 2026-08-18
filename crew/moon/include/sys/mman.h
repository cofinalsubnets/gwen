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
#define MAP_ANONYMOUS 32
#define MAP_ANON      32
#define MAP_POPULATE  32768
#define MAP_FAILED ((void*)(-1))
#define MADV_NORMAL    0
#define MADV_DONTFORK 10
#define MADV_DOFORK   11
int madvise(void*, long, int);
void *mmap(void*, long, int, int, int, long);
int munmap(void*, long);
int mprotect(void*, long, int);
#define MFD_CLOEXEC 1
int memfd_create(char const*, unsigned int);
#endif
