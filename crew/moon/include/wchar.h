#ifndef _AI_WCHAR_H
#define _AI_WCHAR_H
#include <stddef.h>
/* C11 7.29, the types and their limits only: the carried libc has no wide string or
   wide stream functions, so a program that calls one wants a link, not this file. */
typedef int wint_t;
#define WEOF      ((wint_t) -1)
#define WCHAR_MIN (-2147483647 - 1)
#define WCHAR_MAX 2147483647
#endif
