#ifndef _AI_CTYPE_H
#define _AI_CTYPE_H
/* the C character classes -- declarations only; nolibc/libc owns the bodies.
   love.c itself never needs these (it nets bytes directly), so this header exists
   for THIRD-PARTY C (the LFS-userland ladder) -- without it
   a `#include <ctype.h>` falls through to glibc's GNU-laden one and mooncc chokes. */
int isalnum(int);
int isalpha(int);
int isblank(int);
int iscntrl(int);
int isdigit(int);
int isgraph(int);
int islower(int);
int isprint(int);
int ispunct(int);
int isspace(int);
int isupper(int);
int isxdigit(int);
int tolower(int);
int toupper(int);
#endif
