#ifndef _AI_SYS_SYSCTL_H
#define _AI_SYS_SYSCTL_H
/* freebsd's kernel mib walk (stable/14); linux has none -- the member is freebsd-only */
#include <stddef.h>
#define CTL_KERN            1
#define KERN_PROC          14
#define KERN_PROC_PATHNAME 12
int sysctl(int const*, unsigned int, void*, size_t*, void const*, size_t);
#endif
