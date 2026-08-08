#ifndef _AI_ARPA_INET_H
#define _AI_ARPA_INET_H
#include <netinet/in.h>
int inet_pton(int, char const*, void*);
char const *inet_ntop(int, void const*, char*, socklen_t);
char *inet_ntoa(struct in_addr);
int inet_aton(char const*, struct in_addr*);
unsigned int inet_addr(char const*);
#endif
