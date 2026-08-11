/* crew/moon/lib/nolibc/dns.c -- getaddrinfo: the numeric slice, then the NAME
 * half over /etc/hosts and a UDP A query to /etc/resolv.conf's server. */
#include "impl.h"

/* ---- getaddrinfo, the numeric slice: dotted-quad IPv4 + localhost + a decimal
 * port -- exactly what the host seam speaks (host/sock.c resolves numbers; DNS
 * stays a post-rung nicety). one malloc'd block carries result + address. ---- */
struct __sain { unsigned short fam; unsigned short port; unsigned int addr; char pad[8]; };   /* sockaddr_in, 16 bytes */
struct __gai { struct addrinfo ai; struct __sain sa; };
static int __quad(char const *s, unsigned int *out) {
  unsigned int a = 0;
  for (int i = 0; i < 4; i++) {
    unsigned int b = 0, any = 0;
    while (*s >= 48 && *s <= 57) { b = b * 10 + (unsigned) (*s++ - 48); any = 1; if (b > 255) return -1; }
    if (!any) return -1;
    a = (a << 8) | b;
    if (i < 3 && *s++ != 46) return -1; }
  if (*s) return -1;
  *out = a;
  return 0; }
/* ---- the NAME half: /etc/hosts, then a UDP A query to /etc/resolv.conf's
 * nameservers -- the smallest resolver that keeps `connect host port` (ain)
 * and seed's http pull real on the raw default binary. IPv4 A records only,
 * first answer wins; 2 tries x ~2.5s per nameserver, up to 3 nameservers,
 * 127.0.0.1 when resolv.conf names none (musl's fallback). all addresses
 * move in HOST order here; getaddrinfo's htonl is the one wire flip. ---- */
static int __hline(char *ln, char const *host, unsigned int *out) {
  char *p = ln;
  while (*p == ' ' || *p == 9) p++;
  if (*p == '#' || !*p) return -1;
  char *a = p;
  while (*p && *p != ' ' && *p != 9) p++;
  if (!*p) return -1;
  *p++ = 0;
  unsigned int addr;
  if (__quad(a, &addr) < 0) return -1;       /* an IPv6 line falls out here */
  for (;;) {
    while (*p == ' ' || *p == 9) p++;
    if (!*p || *p == '#') return -1;
    char *n = p;
    while (*p && *p != ' ' && *p != 9) p++;
    int end = !*p;
    *p = 0;
    if (!strcmp(n, host)) { *out = addr; return 0; }
    if (end) return -1;
    p++; } }
static int __lines(char const *path, char const *host, unsigned int *out,
                   int (*one)(char*, char const*, unsigned int*)) {
  int fd = open(path, 0);
  if (fd < 0) return -1;
  char buf[1024], ln[512];
  long n; int li = 0, hit = -1;
  while (hit < 0 && (n = read(fd, buf, sizeof buf)) > 0)
    for (long i = 0; i < n; i++) {
      if (buf[i] != '\n') { if (li < (int) sizeof ln - 1) ln[li++] = buf[i]; continue; }
      ln[li] = 0, li = 0;
      if (one(ln, host, out) == 0) { hit = 0; break; } }
  if (hit < 0 && li) { ln[li] = 0; if (one(ln, host, out) == 0) hit = 0; }
  close(fd);
  return hit; }
/* a resolv.conf "nameserver A.B.C.D" line; host carries a (char) slot index */
static int __rline(char *ln, char const *slot, unsigned int *out) {
  char *p = ln;
  while (*p == ' ' || *p == 9) p++;
  if (strncmp(p, "nameserver", 10) != 0) return -1;
  p += 10;
  if (*p != ' ' && *p != 9) return -1;
  while (*p == ' ' || *p == 9) p++;
  char *a = p;
  while (*p && *p != ' ' && *p != 9 && *p != '#') p++;
  *p = 0;
  unsigned int addr;
  if (__quad(a, &addr) < 0) return -1;
  out[(int) *slot] = addr;
  return ++*(char*) slot >= 3 ? 0 : -1; }   /* keep scanning until 3 or eof */
static int __dnskip(unsigned char const *r, long rn, int pos) {
  while (pos < rn) {
    int l = r[pos];
    if (!l) return pos + 1;
    if ((l & 192) == 192) return pos + 2;    /* a compression pointer ends the name */
    pos += l + 1; }
  return -1; }
static int __dnsq(unsigned int ns, char const *host, unsigned int *out) {
  unsigned char q[300]; int qn = 12;
  struct timespec ts;
  clock_gettime(1, &ts);                     /* CLOCK_MONOTONIC seeds the id */
  unsigned short id = (unsigned short) (ts.tv_nsec ^ (ts.tv_nsec >> 16));
  memset(q, 0, 12);
  q[0] = (unsigned char) (id >> 8), q[1] = (unsigned char) id;
  q[2] = 1;                                  /* RD */
  q[5] = 1;                                  /* one question */
  for (char const *p = host; *p; ) {         /* labels */
    char const *d = p;
    while (*d && *d != '.') d++;
    long l = d - p;
    if (l < 1 || l > 63 || qn + l + 2 > (int) sizeof q - 5) return -1;
    q[qn++] = (unsigned char) l;
    memcpy(q + qn, p, (size_t) l), qn += (int) l;
    p = *d ? d + 1 : d; }
  q[qn++] = 0;
  q[qn++] = 0, q[qn++] = 1;                  /* QTYPE A */
  q[qn++] = 0, q[qn++] = 1;                  /* QCLASS IN */
  int fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (fd < 0) return -1;
  struct timeval tv = {2, 500000};
  setsockopt(fd, SOL_SOCKET, 20, &tv, sizeof tv);   /* SO_RCVTIMEO */
  struct __sain sa = {AF_INET, htons(53), htonl(ns), {0}};
  int got = -1;
  for (int try = 0; got < 0 && try < 2; try++) {
    if (sendto(fd, q, (unsigned long) qn, 0, (struct sockaddr *) &sa, 16) < 0) break;
    unsigned char r[512];
    long rn = recvfrom(fd, r, sizeof r, 0, 0, 0);
    if (rn < 12 || r[0] != q[0] || r[1] != q[1]) continue;
    if ((r[3] & 15) != 0) break;             /* NXDOMAIN &c: this server answered no */
    int an = (r[6] << 8) | r[7];
    int pos = __dnskip(r, rn, 12);           /* the echoed question */
    if (pos < 0) break;
    pos += 4;
    while (an-- > 0 && pos < rn) {           /* first A record wins (CNAMEs ride ahead of it) */
      pos = __dnskip(r, rn, pos);
      if (pos < 0 || pos + 10 > rn) break;
      int ty = (r[pos] << 8) | r[pos + 1], cl = (r[pos + 2] << 8) | r[pos + 3];
      int rdl = (r[pos + 8] << 8) | r[pos + 9];
      pos += 10;
      if (pos + rdl > rn) break;
      if (ty == 1 && cl == 1 && rdl == 4) {
        *out = ((unsigned int) r[pos] << 24) | ((unsigned int) r[pos + 1] << 16)
             | ((unsigned int) r[pos + 2] << 8) | r[pos + 3];
        got = 0;
        break; }
      pos += rdl; } }
  close(fd);
  return got; }
static int __dnslook(char const *host, unsigned int *out) {
  unsigned int ns[3] = {0, 0, 0};
  char slot = 0;
  __lines("/etc/resolv.conf", &slot, ns, __rline);
  if (!slot) ns[0] = 2130706433U, slot = 1;  /* no nameserver line: 127.0.0.1 */
  for (int i = 0; i < slot; i++)
    if (__dnsq(ns[i], host, out) == 0) return 0;
  return -1; }
int getaddrinfo(char const *host, char const *serv, struct addrinfo const *hints, struct addrinfo **res) {
  unsigned int a4 = 2130706433U;             /* 127.0.0.1 */
  if (host) {
    if (strcmp(host, "localhost") != 0 && __quad(host, &a4) < 0
        && __lines("/etc/hosts", host, &a4, __hline) < 0
        && __dnslook(host, &a4) < 0) return -2;                               /* EAI_NONAME */
  } else if (hints && (hints->ai_flags & AI_PASSIVE)) a4 = 0;                 /* INADDR_ANY */
  unsigned int port = 0;
  if (serv) {
    char const *s = serv;
    if (!*s) return -2;
    while (*s >= 48 && *s <= 57) port = port * 10 + (unsigned) (*s++ - 48);
    if (*s || port > 65535) return -2; }
  struct __gai *g = malloc(sizeof(struct __gai));
  if (!g) return -10;                        /* EAI_MEMORY */
  memset(g, 0, sizeof *g);
  g->sa.fam = AF_INET;
  g->sa.port = htons((unsigned short) port);
  g->sa.addr = htonl(a4);
  g->ai.ai_family = AF_INET;
  g->ai.ai_socktype = hints && hints->ai_socktype ? hints->ai_socktype : SOCK_STREAM;
  g->ai.ai_protocol = hints ? hints->ai_protocol : 0;
  g->ai.ai_addrlen = 16;
  g->ai.ai_addr = (struct sockaddr *) &g->sa;
  *res = &g->ai;
  return 0; }
void freeaddrinfo(struct addrinfo *r) { free(r); }

