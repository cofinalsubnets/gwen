#ifndef _AI_SYS_SOCKET_H
#define _AI_SYS_SOCKET_H
typedef unsigned int socklen_t;
typedef unsigned short sa_family_t;
struct sockaddr { sa_family_t sa_family; char sa_data[14]; };
/* big enough for any address the seam speaks (glibc's is 128 bytes) */
struct sockaddr_storage { sa_family_t ss_family; char __pad[126]; };
#define AF_UNSPEC 0
#define AF_UNIX   1
#define AF_LOCAL  1
#define AF_INET   2
#define AF_INET6 10
#define SOCK_STREAM 1
#define SOCK_DGRAM  2
#define SOCK_NONBLOCK 2048
#define SOCK_CLOEXEC 524288
#define SOL_SOCKET 1
#define SO_REUSEADDR 2
#define SO_ERROR 4
#define SO_KEEPALIVE 9
#define SHUT_RD   0
#define SHUT_WR   1
#define SHUT_RDWR 2
#define MSG_NOSIGNAL 16384
#define MSG_DONTWAIT 64
#define MSG_CMSG_CLOEXEC 1073741824
int socket(int, int, int);
int bind(int, struct sockaddr const*, socklen_t);
int listen(int, int);
int accept(int, struct sockaddr*, socklen_t*);
int connect(int, struct sockaddr const*, socklen_t);
long send(int, void const*, unsigned long, int);
long recv(int, void*, unsigned long, int);
int setsockopt(int, int, int, void const*, socklen_t);
int getsockopt(int, int, int, void*, socklen_t*);
int shutdown(int, int);
long recvfrom(int, void*, unsigned long, int, struct sockaddr*, socklen_t*);
long sendto(int, void const*, unsigned long, int, struct sockaddr const*, socklen_t);
/* scatter-gather + ancillary (SCM_RIGHTS fd passing), glibc x86-64 layout */
struct iovec { void *iov_base; unsigned long iov_len; };
struct msghdr {
  void *msg_name; socklen_t msg_namelen;
  struct iovec *msg_iov; unsigned long msg_iovlen;
  void *msg_control; unsigned long msg_controllen;
  int msg_flags;
};
struct cmsghdr { unsigned long cmsg_len; int cmsg_level; int cmsg_type; };
#define SCM_RIGHTS 1
#define CMSG_ALIGN(n) (((n) + 7) & ~((unsigned long) 7))
#define CMSG_SPACE(n) (CMSG_ALIGN(n) + CMSG_ALIGN(sizeof(struct cmsghdr)))
#define CMSG_LEN(n)   (CMSG_ALIGN(sizeof(struct cmsghdr)) + (n))
#define CMSG_DATA(c)  ((unsigned char*)((struct cmsghdr*)(c) + 1))
#define CMSG_FIRSTHDR(m) ((m)->msg_controllen >= sizeof(struct cmsghdr)   ? (struct cmsghdr*)(m)->msg_control : (struct cmsghdr*) 0)
#define CMSG_NXTHDR(m, c)   ((unsigned char*)(c) + CMSG_ALIGN((c)->cmsg_len) + sizeof(struct cmsghdr)      > (unsigned char*)(m)->msg_control + (m)->msg_controllen    ? (struct cmsghdr*) 0    : (struct cmsghdr*)((unsigned char*)(c) + CMSG_ALIGN((c)->cmsg_len)))
long recvmsg(int, struct msghdr*, int);
long sendmsg(int, struct msghdr const*, int);

#endif
