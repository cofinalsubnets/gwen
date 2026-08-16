#ifndef _AI_ERRNO_H
#define _AI_ERRNO_H
/* glibc's errno is thread-local behind a call */
int *__errno_location(void);
#define errno (*__errno_location())
#define EPERM            1
#define ENOENT           2
#define ESRCH            3
#define EINTR            4
#define EIO              5
#define ENXIO            6
#define E2BIG            7
#define ENOEXEC          8
#define EBADF            9
#define ECHILD          10
/* 1..10 are V7's, one table; from 11 the kernels part ways (freebsd:
 * stable/14 sys/errno.h) */
#if defined(__FreeBSD__)
#define EDEADLK         11
#define EAGAIN          35
#define EWOULDBLOCK     35
#else
#define EAGAIN          11
#define EWOULDBLOCK     11
#endif
#define ENOMEM          12
#define EACCES          13
#define EFAULT          14
#define ENOTBLK         15
#define EBUSY           16
#define EEXIST          17
#define EXDEV           18
#define ENODEV          19
#define ENOTDIR         20
#define EISDIR          21
#define EINVAL          22
#define ENFILE          23
#define EMFILE          24
#define ENOTTY          25
#define EFBIG           27
#define ENOSPC          28
#define ESPIPE          29
#define EROFS           30
#define EMLINK          31
#define EPIPE           32
#define EDOM            33
#define ERANGE          34
#define ETXTBSY         26
#if defined(__FreeBSD__)
#define ENAMETOOLONG    63
#define ENOLCK          77
#define ENOSYS          78
#define ENOTEMPTY       66
#define ELOOP           62
#define ECONNRESET      54
#define ENOTCONN        57
#define ETIMEDOUT       60
#define ECONNREFUSED    61
#define EINPROGRESS     36
#else
#define EDEADLK         35
#define ENAMETOOLONG    36
#define ENOLCK          37
#define ENOSYS          38
#define ENOTEMPTY       39
#define ELOOP           40
#define ECONNRESET     104
#define ENOTCONN       107
#define ETIMEDOUT      110
#define ECONNREFUSED   111
#define EINPROGRESS    115
#endif
#endif
