#ifndef _AI_SYS_SENDFILE_H
#define _AI_SYS_SENDFILE_H
#include <sys/types.h>
/* ours rather than glibc's: that one declares sendfile64 in terms of __off64_t,
 * a name only glibc's own <sys/types.h> carries -- and ours wins the search. */
ssize_t sendfile(int, int, off_t*, size_t);
#endif
