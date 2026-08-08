#ifndef _AI_SYS_TYPES_H
#define _AI_SYS_TYPES_H
typedef unsigned long size_t;
typedef long   ssize_t;
typedef int    pid_t;
typedef unsigned int uid_t;
typedef unsigned int gid_t;
typedef unsigned int mode_t;
typedef long   off_t;
typedef unsigned long dev_t;
typedef unsigned long ino_t;
typedef unsigned long nlink_t;
typedef long   time_t;
typedef long   suseconds_t;
typedef long   blksize_t;
typedef long   blkcnt_t;
typedef long   clock_t;
/* the BSD spellings glibc carries under _DEFAULT_SOURCE -- darkhttpd casts a
 * PF_ constant through (u_char), and a missing type name reads as a parse error
 * at the token AFTER the cast, which names nothing. */
typedef unsigned char  u_char;
typedef unsigned short u_short;
typedef unsigned int   u_int;
typedef unsigned long  u_long;
typedef unsigned short ushort;
typedef unsigned int   uint;
typedef unsigned long  ulong;
typedef char          *caddr_t;
#include <sys/select.h>   /* glibc pulls it in from here, and sources lean on that */
#endif
