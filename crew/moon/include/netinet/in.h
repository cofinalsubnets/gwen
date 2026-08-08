#ifndef _AI_NETINET_IN_H
#define _AI_NETINET_IN_H
#include <sys/socket.h>
typedef unsigned short in_port_t;
typedef unsigned int   in_addr_t;
struct in_addr { in_addr_t s_addr; };
struct sockaddr_in {
  sa_family_t    sin_family;
  in_port_t      sin_port;      /* network byte order */
  struct in_addr sin_addr;
  char           sin_zero[8];
};
struct in6_addr { unsigned char s6_addr[16]; };
struct sockaddr_in6 {
  sa_family_t     sin6_family;
  in_port_t       sin6_port;
  unsigned int    sin6_flowinfo;
  struct in6_addr sin6_addr;
  unsigned int    sin6_scope_id;
};
#define INADDR_ANY       0
#define INADDR_LOOPBACK  2130706433
#define INADDR_NONE      0xffffffffU
#define INADDR_BROADCAST 0xffffffffU
#define IPPROTO_IP   0
#define IPPROTO_TCP 6
#define IPPROTO_UDP 17
#define IPPROTO_IPV6 41
#define IPV6_V6ONLY      26
#define IPV6_JOIN_GROUP  20
#define IPV6_LEAVE_GROUP 21
#define INET_ADDRSTRLEN  16
#define INET6_ADDRSTRLEN 46
extern struct in6_addr const in6addr_any, in6addr_loopback;
/* the address predicates, glibc's non-__GNUC__ spelling: an in6_addr read as
 * four network-order words. htonl of a constant folds, so these stay constant. */
#define IN6_IS_ADDR_UNSPECIFIED(a) \
  (((unsigned const*)(a))[0] == 0 && ((unsigned const*)(a))[1] == 0 \
   && ((unsigned const*)(a))[2] == 0 && ((unsigned const*)(a))[3] == 0)
#define IN6_IS_ADDR_LOOPBACK(a) \
  (((unsigned const*)(a))[0] == 0 && ((unsigned const*)(a))[1] == 0 \
   && ((unsigned const*)(a))[2] == 0 && ((unsigned const*)(a))[3] == htonl(1))
#define IN6_IS_ADDR_V4MAPPED(a) \
  (((unsigned const*)(a))[0] == 0 && ((unsigned const*)(a))[1] == 0 \
   && ((unsigned const*)(a))[2] == htonl(0xffff))
#define IN6_IS_ADDR_V4COMPAT(a) \
  (((unsigned const*)(a))[0] == 0 && ((unsigned const*)(a))[1] == 0 \
   && ((unsigned const*)(a))[2] == 0 && ntohl(((unsigned const*)(a))[3]) > 1)
#define IN6_IS_ADDR_LINKLOCAL(a) \
  ((((unsigned const*)(a))[0] & htonl(0xffc00000)) == htonl(0xfe800000))
#define IN6_IS_ADDR_SITELOCAL(a) \
  ((((unsigned const*)(a))[0] & htonl(0xffc00000)) == htonl(0xfec00000))
#define IN6_IS_ADDR_MULTICAST(a) (((unsigned char const*)(a))[0] == 0xff)
#define IN6_ARE_ADDR_EQUAL(a, b) \
  (((unsigned const*)(a))[0] == ((unsigned const*)(b))[0] \
   && ((unsigned const*)(a))[1] == ((unsigned const*)(b))[1] \
   && ((unsigned const*)(a))[2] == ((unsigned const*)(b))[2] \
   && ((unsigned const*)(a))[3] == ((unsigned const*)(b))[3])
unsigned short htons(unsigned short);
unsigned short ntohs(unsigned short);
unsigned int   htonl(unsigned int);
unsigned int   ntohl(unsigned int);
#endif
