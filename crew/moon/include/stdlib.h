#ifndef _AI_STDLIB_H
#define _AI_STDLIB_H
#ifndef NULL
#define NULL ((void*)0)
#endif
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
typedef unsigned long size_t;
void *malloc(size_t);
void *calloc(size_t, size_t);
void *realloc(void*, size_t);
void *reallocarray(void*, size_t, size_t);
void  free(void*);
const char *getprogname(void);   /* glibc >= 2.33 home (gnulib also reaches unistd.h for it) */
void  exit(int);
void  abort(void);
int   atexit(void (*)(void));
void  qsort(void*, size_t, size_t, int (*)(void const*, void const*));
void *bsearch(void const*, void const*, size_t, size_t, int (*)(void const*, void const*));
char *getenv(char const*);
char *mktemp(char*);
int   setenv(char const*, char const*, int);
int   unsetenv(char const*);
int   atoi(char const*);
long  atol(char const*);
double atof(char const*);
long  strtol(char const*, char**, int);
unsigned long strtoul(char const*, char**, int);
long  strtoll(char const*, char**, int);
unsigned long strtoull(char const*, char**, int);
double strtod(char const*, char**);
int   system(char const*);
int   abs(int);
long  labs(long);
/* the pty quartet lives here in glibc (stdlib.h, not pty.h) */
int   posix_openpt(int);
int   grantpt(int);
int   unlockpt(int);
char *ptsname(int);
#endif
