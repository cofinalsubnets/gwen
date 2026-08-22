// host/sock.c -- every socket nif, both address families: TCP/UDP (ain's
// netcat core and inle's oracle wire), unix-domain connect (lux's X display
// door) and listen (the shore lux moors at). Host-only, auto-globbed +
// AiNif-registered (no
// love.c/love.h/main.c edit). Every stream nif mirrors main.c's lvm_open:
// produce an OS fd, hand it to ai_io_alloc (love.c) -> a heap port carrying a
// close finalizer. Once an fd is a port, READ AND WRITE COME FREE through the
// existing fgetc/fputc machinery (the fgetc read path even yields
// cooperatively on a not-ready fd), so a socket nif only has to make the fd.
//
// EVERY nif here PARKS rather than blocking (love.h's nif park: leave Ip
// unadvanced and yield, so the op re-runs on reschedule) -- accept and udp-recv
// on their fd, and connect on its HANDSHAKE, which is the write-direction wait
// doc/misc/io.md held back as rung 7 "until something asks". NOTHING IN THIS FILE
// WAITS. getaddrinfo is what used to make connect the exception, and it is gone:
// `connect` takes a dotted quad, and a NAME resolves one layer up in love, where
// the lookup itself can park. doc/misc/io.md, the nif floor.
#define _GNU_SOURCE     // SOCK_CLOEXEC
#include "love.h"
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

// Every socket fd is CLOSE-ON-EXEC. A run/exec/spawn child must never inherit
// these, and a server that re-execs onto a new binary must not carry its own
// listener across: the old fd stays bound and the fresh bind fails, since
// SO_REUSEADDR does not permit two live listeners. CLOEXEC releases the port at
// exec so the next generation binds clean.
#define cloexec(fd) do { if ((fd) >= 0) fcntl((fd), F_SETFD, FD_CLOEXEC); } while (0)

// a datagram caps at one ethernet MTU.
#define DgMax 1472

// Pull a live OS fd out of a port arg, or -1 if it isn't a port. Same inline
// "is x a port" as main.c's lvm_close: an even (heap) word whose first slot is
// the lvm_port_io discriminator (declared in love.h). A closed port carries the
// -3 sentinel; we hand that straight back and the syscall answers EBADF.
static intptr_t port_fd(ai_word x) {
 if ((x & 1) == 0 && ((union u*) x)->ap == lvm_port_io)
    return ai_io_fd((struct ai_io*) x);
 return -1; }

// a cask's (or string's) backing bytes, or 0 -- the wl lanes take either.
static struct ai_str *cask_bytes(ai_word x) {
 if (x & 1) return 0;
 if (((union u*) x)->ap == lvm_cask) return ((struct ai_cask*) x)->str;
 return ai_strp(x) ? (struct ai_str*) x : 0; }

// A DOTTED QUAD and nothing else -> the address in host order, or -1. This is
// the whole of what `connect` accepts now: getaddrinfo left this file with the
// nif floor's last rung, because it is not a syscall with an O_NONBLOCK to set
// but a config read that may speak DNS, and there is no nonblocking form of it.
// Names resolve one layer UP, in love, where a lookup can park -- lib/dns.l's
// `dial`. (nolibc's own resolver, crew/moon/lib/nolibc.c, is what this used to
// reach on a mooncc-built binary: /etc/hosts then a UDP A query, up to 2 tries x
// 2.5 s per nameserver across 3 of them. Fifteen seconds of dead vm, on the
// default build.)
static int quad(struct ai_str *hv, uint32_t *out) {
 if (hv->len < 7 || hv->len > 15) return -1;      // "0.0.0.0" .. "255.255.255.255"
 char s[16];
 memcpy(s, hv->bytes, hv->len);
 s[hv->len] = 0;
 uint32_t a = 0;
 char const *p = s;
 for (int i = 0; i < 4; i++) {
  uint32_t b = 0, any = 0;
  while (*p >= '0' && *p <= '9') {
   b = b * 10 + (uint32_t) (*p++ - '0'), any = 1;
   if (b > 255) return -1; }
  if (!any) return -1;
  a = (a << 8) | b;
  if (i < 3 && *p++ != '.') return -1; }
 if (*p) return -1;
 return *out = a, 0; }

// (connect quad port) -- TCP client, as a TWO-AP NIF BODY for hark's reason: the
// handshake has to park, and the op is not re-runnable at the park because it has
// already made a socket and sent a SYN. So the first ap makes the socket and
// starts the handshake, the second waits for it, and the fd rides the stack
// between them (a charm -- the GC walks that slot as an ordinary word).
// Any failure -- bad args, not a quad, refused, unreachable -> ().
ai_noinline static int call_connect(struct ai_str *hv, int port) {
 uint32_t a;
 if (port < 0 || port > 65535 || quad(hv, &a) < 0) return -1;
 int fd = socket(AF_INET, SOCK_STREAM, 0);
 if (fd < 0) return -1;
 cloexec(fd);
 // ⚠ AND IT STAYS NONBLOCKING. The handshake needs it, and afterwards love wraps
 // the fd as a heap port whose reads and writes toggle the flag per call anyway.
 int fl = fcntl(fd, F_GETFL);
 if (fl >= 0) fcntl(fd, F_SETFL, fl | O_NONBLOCK);
 struct sockaddr_in sa = {0};
 sa.sin_family = AF_INET;
 sa.sin_addr.s_addr = htonl(a);
 sa.sin_port = htons((uint16_t) port);
 int r;
 do r = connect(fd, (struct sockaddr*) &sa, sizeof sa); while (r < 0 && errno == EINTR);
 // ⚠ EINPROGRESS and no EALREADY: this is the FIRST connect on a fresh socket, so
 // "a previous one is still going" cannot be the answer. (nolibc has no EALREADY
 // either, and mooncc said so by name -- the undeclared-identifier diagnostic.)
 if (r == 0 || errno == EINPROGRESS) return fd;   // in hand, or in flight
 close(fd);
 return -1; }

static lvm(lvm_connect) {
 int fd = ai_strp(Sp[0]) && oddp(Sp[1])
        ? call_connect((struct ai_str*) Sp[0], (int) getcharm(Sp[1])) : -1;
 Sp[0] = putcharm(fd);                    // over `host`; -1 rides through to the waiter
 ai_musttail return Next(1); }

// The second ap: the handshake, waited on by the SCHEDULER. ⚠ readiness is the
// question here, not a leftover pre-guard of the kind rung 2 deleted from the read
// path -- there is no read to answer it, and SO_ERROR reads 0 on a socket that is
// merely still trying. POLLOUT first, then the error, is the one order that tells
// "connected" from "refused".
static lvm(lvm_connectw) {
 int fd = (int) getcharm(Sp[0]);
 if (fd < 0) goto fail;
 if (!ai_ready(fd, ai_wait_out)) {
  g->next_wait_fd = fd;
  g->next_wait_events = ai_wait_out;
  ai_musttail return Ap(lvm_yield_sw, g); }
 int err = 0;
 socklen_t el = sizeof err;
 if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &el) || err) { close(fd); goto fail; }
 Pack(g);
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) { close(fd); goto fail; }
 g = r;
 Unpack(g);
 // stack: [port, fd, port#, ...] -> [port, ...]
 Sp[2] = Sp[0];
 Sp += 2; Ip += 1;
 ai_musttail return Continue();
 fail:                                    // [fd, port#, ret] -> [(), ret]
 Sp[1] = ZeroPoint;
 Sp += 1; Ip += 1;
 ai_musttail return Continue(); }

// (listen port) -- TCP server socket: socket()+SO_REUSEADDR+bind(INADDR_ANY,
// port)+listen(). Returns the listening port object, or () on any failure.
// IPv4 only (enough for a loopback demo); `accept` gives the connection.
#define ai_listen_backlog 512
ai_noinline static int call_listen(int port) {
 if (port < 0 || port > 65535) return -1;
 int fd = socket(AF_INET, SOCK_STREAM, 0);
 if (fd < 0) return -1;
 int one = 1;
 setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
 struct sockaddr_in a = {0};
 a.sin_family = AF_INET;
 a.sin_addr.s_addr = htonl(INADDR_ANY);
 a.sin_port = htons((uint16_t) port);
 // The backlog is the ACCEPT QUEUE -- connections the kernel has already shaken
 // hands on and is holding for us -- and a server that twirls a task per client is
 // off serving them, not sitting in accept. At 1 the queue overflows on the second
 // simultaneous arrival, the kernel drops the SYN, and the client waits out an
 // exponential retry (measured against kiosko: 1s at 25 arrivals, 30s at 100),
 // which reads as our latency and is not ours.
 // ⚠ IT IS A CONSTANT AND NOT AN OPERAND, deliberately: `listen` is 1-ary across
 // the tree and out of it, and a second operand would turn every `(listen port)`
 // into a closure -- truthy, so every "did it listen?" test would read the failure
 // as a success.
 // ⚠ AND test/host/nifpark.l KNOWS THIS NUMBER: its law 5 fills the queue to make a
 // connect stall, which is the only way to reach the write-direction park offline.
 // Moving this without moving that leaves the fill one arrival short and reddens
 // there. 512 is the measured floor for a flat arrival curve at 400 simultaneous
 // clients, and small enough that filling it costs the law about 20ms.
 if (bind(fd, (struct sockaddr*) &a, sizeof a) || listen(fd, ai_listen_backlog)) {
  close(fd);
  return -1; }
 cloexec(fd);
 return fd; }

static lvm(lvm_listen) {
 if (!oddp(Sp[0])) goto fail;
 int fd = call_listen((int) getcharm(Sp[0]));
 if (fd < 0) goto fail;
 Pack(g);
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) { close(fd); goto fail; }
 g = r;
 Unpack(g);
 // stack: [port, port#, ...] -> [port, ...]
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue();
 fail:
 Sp[0] = ZeroPoint; Ip += 1;
 ai_musttail return Continue(); }

// accept(2) without waiting, in readn's three terms: >=0 the fd, -2 nobody is there
// yet, -1 gone. The O_NONBLOCK toggle is per call for main.c's reason -- the flags
// ride the OPEN FILE DESCRIPTION, which a forked child shares, and a listener left
// nonblocking is a surprise for whoever inherits it. errno is read before the
// restore, which is an fcntl and may set its own.
ai_noinline static int call_accept(int lfd) {
 int fl = fcntl(lfd, F_GETFL), off = fl >= 0 && !(fl & O_NONBLOCK);
 if (off) fcntl(lfd, F_SETFL, fl | O_NONBLOCK);
 int fd;
 do fd = accept(lfd, NULL, NULL); while (fd < 0 && errno == EINTR);
 int busy = fd < 0 && (errno == EAGAIN || errno == EWOULDBLOCK);
 if (off) fcntl(lfd, F_SETFL, fl);
 return fd >= 0 ? fd : busy ? -2 : -1; }

// (accept l) -- take the next client on listener port `l` and wrap its fd as a port.
// An empty backlog PARKS the task on the listener's fd (love.h's nif park: leave Ip
// unadvanced and yield), so the scheduler folds this listener into the same wait as
// every other quiet fd and one core is not burnt waiting for a first connection.
// Re-running the op is exact: nothing is consumed before the park. () on misuse or
// a real accept() failure.
static lvm(lvm_accept) {
 int lfd = (int) port_fd(Sp[0]);
 if (lfd < 0) goto fail;
 int fd = call_accept(lfd);
 if (fd == -2) { g->next_wait_fd = lfd; ai_musttail return Ap(lvm_yield_sw, g); }
 if (fd < 0) goto fail;
 cloexec(fd);
 Pack(g);
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) { close(fd); goto fail; }
 g = r;
 Unpack(g);
 // stack: [conn, l, ...] -> [conn, ...]
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue();
 fail:
 Sp[0] = ZeroPoint; Ip += 1;
 ai_musttail return Continue(); }

// (shutdown s how) -- half-close a socket port. `how` is the POSIX SHUT_*
// fixnum: 0 = read, 1 = write, 2 = both. The load-bearing case is (shutdown s 1)
// after a stdin-EOF, so the peer sees EOF on its read instead of a hung
// half-open socket. Returns the port (chainable); a no-op on misuse.
// ⚠ SHUTTING THE WRITE HALF MUST LAND THE WRITE RUN FIRST. love.h always said
// "close/seal call it first" and seal never did -- harmless while a write
// delivered by blocking, a truncated response the moment the door could answer
// short (rung 4). kiosko's own shape is `(say c body) (seal c 1) (close c)`.
static lvm(lvm_shutdown) {
 int fd = (int) port_fd(Sp[0]);
 if (fd >= 0 && oddp(Sp[1])) {
  intptr_t how = getcharm(Sp[1]);
  if (how >= 1 && how <= 2) {                 // fd >= 0 already proved it a port
   struct ai_io *io = (struct ai_io*) Sp[0];
   g->io = io;
   Pack(g);
   g = ai_io_wflush(g, io);
   if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
   if (ai_io_wpending(g, (struct ai_io*) g->sp[0])) {   // park; nothing shut yet
    Unpack(g);
    g->next_wake_at = ai_clock() + 1;
    ai_musttail return Ap(lvm_yield_sw, g); }
   Unpack(g); }
  if (how >= 0 && how <= 2) shutdown(fd, (int) how); }
 // stack: [s, how, ...] -> [s, ...]
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue(); }

// --- UDP (inle's milestone-5 oracle wire) ---------------------------------
// The TCP nifs above can't talk to inle: inle speaks UDP DATAGRAMS, each
// carrying its own sender address to reply to, and a connected byte-stream port
// (the fgetc/fputc free-read path) can't express that. So UDP gets three nifs
// that recvfrom/sendto directly off a bound port's fd and marshal the peer as a
// fixnum -- (host-order ipv4 << 16) | port, 48 bits, comfortably inside a fixnum:
//   (udp-bind port)            -> a port on a bound UDP socket | ()
//   (udp-recv p)               -> (peerfix . datagram-bytes) | ()  [PARKS]
//   (udp-send p peerfix bytes) -> p (chainable) | ()
// A quiet socket PARKS the task on its fd, like accept above -- the oracle is
// one-at-a-time, but "nothing else to do" is the SCHEDULER's judgement to make,
// not this nif's, and while it blocked no peer task could run at all.

ai_noinline static int call_udpbind(int port) {
 if (port < 0 || port > 65535) return -1;
 int fd = socket(AF_INET, SOCK_DGRAM, 0);
 if (fd < 0) return -1;
 int one = 1;
 setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
 struct sockaddr_in a = {0};
 a.sin_family = AF_INET;
 a.sin_addr.s_addr = htonl(INADDR_ANY);
 a.sin_port = htons((uint16_t) port);
 if (bind(fd, (struct sockaddr*) &a, sizeof a)) { close(fd); return -1; }
 cloexec(fd);
 return fd; }

static lvm(lvm_udpbind) {
 if (!oddp(Sp[0])) goto fail;
 int fd = call_udpbind((int) getcharm(Sp[0]));
 if (fd < 0) goto fail;
 Pack(g);
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) { close(fd); goto fail; }
 g = r;
 Unpack(g);
 // stack: [port#, ...] -> [port, ...]
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue();
 fail:
 Sp[0] = ZeroPoint; Ip += 1;
 ai_musttail return Continue(); }

// recvfrom + peer marshaling; the &-taken sockaddr lives here so the lvm
// wrapper stays TCO-clean. Returns by value (16 bytes -> registers).
// ⚠ THE STRUCT MUST STAY TWO WORDS. At 24 bytes the ABI returns it through memory,
// which puts an address-taken slot in the CALLER's frame -- and the caller is an
// lvm_, where a frame turns the tail Continue() into a ret (make vmret). So the
// would-block answer rides `n` as a third term rather than a third field: >=0 bytes,
// -2 nothing waiting, -1 gone. MSG_DONTWAIT asks for it without touching the flags.
struct dgram { ssize_t n; uintptr_t peerfix; };
ai_noinline static struct dgram call_udprecv(int fd, char *buf, size_t cap) {
 struct sockaddr_in peer; memset(&peer, 0, sizeof peer);
 socklen_t plen = sizeof peer;
 ssize_t n;
 do n = recvfrom(fd, buf, cap, MSG_DONTWAIT, (struct sockaddr*) &peer, &plen);
 while (n < 0 && errno == EINTR);
 if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) n = -2;
 return (struct dgram) { n, ((uintptr_t) ntohl(peer.sin_addr.s_addr) << 16)
                          | (uintptr_t) ntohs(peer.sin_port) }; }

static lvm(lvm_udprecv) {
 int fd = (int) port_fd(Sp[0]);
 if (fd < 0) goto fail;
 // ⚠ a stack buffer is safe in an lvm_ only while its address never reaches the TAIL:
 // every exit here unwinds the frame before it jumps. ai_musttail is owed rather than
 // opportunistic, so a shape that could not tail-jump refuses at compile.
 char buf[DgMax];
 struct dgram d = call_udprecv(fd, buf, sizeof buf);
 // no datagram yet -> PARK on the socket, exactly as accept does. Nothing has been
 // taken off the wire, so the op re-runs whole.
 if (d.n == -2) { g->next_wait_fd = fd; ai_musttail return Ap(lvm_yield_sw, g); }
 ssize_t n = d.n;
 if (n < 0) goto fail;
 uintptr_t peerfix = d.peerfix;
 Pack(g);                                            // bytes + chain allocate -> Pack
 if (n > 0) {                                        // datagram -> a fresh love string
  g = str0(g, (uintptr_t) n);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  memcpy(txt(g->sp[0]), buf, (uintptr_t) n);
  len(g->sp[0]) = (uintptr_t) n;
 } else {                                            // empty datagram -> the singleton
  g = ai_push(g, 1, (uintptr_t) EmptyString);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g); }
 g = ai_have(g, Width(struct ai_chain));             // (peerfix . bytes)
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 struct ai_chain *w = bump(g, Width(struct ai_chain));
 ini_chain(w, putcharm(peerfix), g->sp[0]);          // read sp[0] AFTER ai_have (may move)
 g->sp[0] = word(w);
 Unpack(g);
 // stack: [port, ...] -> [(peerfix . bytes), ...]
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue();
 fail:
 Sp[0] = ZeroPoint; Ip += 1;
 ai_musttail return Continue(); }

// sendto with the peer unmarshaled from its fixnum; the &-taken sockaddr
// lives here so the lvm wrapper stays TCO-clean.
ai_noinline static ssize_t call_udpsend(int fd, uintptr_t peerfix, void const *p, size_t n) {
 struct sockaddr_in a; memset(&a, 0, sizeof a);
 a.sin_family = AF_INET;
 a.sin_addr.s_addr = htonl((uint32_t) (peerfix >> 16));
 a.sin_port = htons((uint16_t) (peerfix & 0xffff));
 ssize_t w;
 do w = sendto(fd, p, n, 0, (struct sockaddr*) &a, sizeof a);
 while (w < 0 && errno == EINTR);
 return w; }

static lvm(lvm_udpsend) {
 if (!oddp(Sp[1]) || !ai_strp(Sp[2])) goto fail;
 int fd = (int) port_fd(Sp[0]);
 if (fd < 0) goto fail;
 struct ai_str *s = str(Sp[2]);
 ssize_t w = call_udpsend(fd, getcharm(Sp[1]), txt(s), len(s));
 if (w < 0) goto fail;
 // stack: [p, peerfix, bytes, ...] -> [p, ...]
 Sp[2] = Sp[0];
 Sp += 2; Ip += 1;
 ai_musttail return Continue();
 fail:
 Sp[2] = ZeroPoint;
 Sp += 2; Ip += 1;
 ai_musttail return Continue(); }

static union u const
 nif_connect[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_connect}, {lvm_connectw}, {lvm_ret0}},
 nif_listen[]   = {{lvm_listen}, {lvm_ret0}},
 nif_accept[]   = {{lvm_accept}, {lvm_ret0}},
 nif_shutdown[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_shutdown}, {lvm_ret0}},
 nif_udpbind[]  = {{lvm_udpbind}, {lvm_ret0}},
 nif_udprecv[]  = {{lvm_udprecv}, {lvm_ret0}},
 nif_udpsend[]  = {{lvm_cur}, {.x = putcharm(3)}, {lvm_udpsend}, {lvm_ret0}};
AiNif("connect",  nif_connect);
AiNif("listen",   nif_listen);
AiNif("accept",   nif_accept);
AiNif("seal", nif_shutdown);
AiNif("udp-bind", nif_udpbind);
AiNif("udp-recv", nif_udprecv);
AiNif("udp-send", nif_udpsend);
// --- unix-domain connect: lux's X display door ----------------------------------
// (connectu path) -- connect to a unix-domain stream socket and wrap the fd as a
// port | (). The load-bearing case is an X display socket (/tmp/.X11-unix/X<n>):
// real X servers listen only there, so lux's wire codec (doc/misc/proto/x11.l lineage)
// needs this one door the TCP nifs can't open.
ai_noinline static int call_connectu(struct ai_str *pv) {
 struct sockaddr_un a;
 if (pv->len == 0 || pv->len >= sizeof a.sun_path) return -1;
 memset(&a, 0, sizeof a);
 a.sun_family = AF_UNIX;
 memcpy(a.sun_path, pv->bytes, pv->len);
 int fd = socket(AF_UNIX, SOCK_STREAM, 0);
 if (fd < 0) return -1;
 if (connect(fd, (struct sockaddr*) &a, sizeof a)) { close(fd); return -1; }
 cloexec(fd);
 return fd; }

static lvm(lvm_connectu) {
 if (!ai_strp(Sp[0])) goto fail;
 int fd = call_connectu((struct ai_str*) Sp[0]);
 if (fd < 0) goto fail;
 Pack(g);
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) { close(fd); goto fail; }
 g = r;
 Unpack(g);
 // stack: [port, path, ...] -> [port, ...]
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue();
 fail:
 Sp[0] = ZeroPoint; Ip += 1;
 ai_musttail return Continue(); }

static union u const nif_connectu[] = {{lvm_connectu}, {lvm_ret0}};
AiNif("connectu", nif_connectu);
// --- the unix listener ----------------------------------------------------------
//   (shore path)          -> a listening unix port | () ; unlinks stale first
//                            (accept/await/close ride the core port nifs)

// (shore path): bind + listen a unix stream socket at path.
// leaves EXACTLY ONE net value above the path on every non-OOM path (the
// port, or the zero point), so lvm_shore collapses uniformly -- pty.c's law.
ai_noinline static struct ai *hv_shore(struct ai *g, ai_word pw) {
 struct ai_str *p = cask_bytes(pw);
 struct sockaddr_un a = {0};
 if (!p || p->len + 1 > sizeof a.sun_path) return ai_push(g, 1, ZeroPoint);
 a.sun_family = AF_UNIX;
 memcpy(a.sun_path, p->bytes, p->len);
 unlink(a.sun_path);
 int fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
 if (fd < 0) return ai_push(g, 1, ZeroPoint);
 if (bind(fd, (struct sockaddr*) &a, sizeof a) || listen(fd, 8)) {
  close(fd);
  return ai_push(g, 1, ZeroPoint); }
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) close(fd);
 return r; }

static lvm(lvm_shore) {
 Pack(g);
 g = hv_shore(g, g->sp[0]);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

static union u const nif_shore[] = {{lvm_shore}, {lvm_ret0}};
AiNif("shore", nif_shore);
