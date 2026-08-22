#ifndef _AI_STDALIGN_H
#define _AI_STDALIGN_H
/* C11 7.15. _Alignas rides gcc's __attribute__((aligned(N))) door: honored at FILE
   SCOPE only -- on a local it is still skipped (doc/misc/moon-c-gaps). */
#define alignas _Alignas
#define alignof _Alignof
#define __alignas_is_defined 1
#define __alignof_is_defined 1
#endif
