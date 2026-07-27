#ifndef _AI_STRING_H
#define _AI_STRING_H
#ifndef NULL
#define NULL ((void*)0)
#endif
typedef unsigned long size_t;
void *memcpy(void*, void const*, size_t);
void *memmove(void*, void const*, size_t);
void *memset(void*, int, size_t);
int   memcmp(void const*, void const*, size_t);
void *memchr(void const*, int, size_t);
void *mempcpy(void*, void const*, size_t);      /* GNU extension: memcpy returning the END */
void *rawmemchr(void const*, int);              /* GNU extension: memchr with no length bound */
size_t strlen(char const*);
int   strcmp(char const*, char const*);
int   strncmp(char const*, char const*, size_t);
int   strcasecmp(char const*, char const*);
int   strncasecmp(char const*, char const*, size_t);
char *strcpy(char*, char const*);
char *strncpy(char*, char const*, size_t);
char *strcat(char*, char const*);
char *strchr(char const*, int);
char *strrchr(char const*, int);
char *strstr(char const*, char const*);
char *strdup(char const*);
char *strerror(int);
size_t strspn(char const*, char const*);
size_t strcspn(char const*, char const*);
char *strpbrk(char const*, char const*);
char *strtok(char*, char const*);
char *strncat(char*, char const*, size_t);
int strcoll(char const*, char const*);
#endif
