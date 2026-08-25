// src/seat.c -- the seat two frontends share: src/main.c's binary and the
// inle kernel both link this file, so what lives here exists ONCE where it
// used to exist twice. the bodies bottom out in libc calls, and on inle those
// land in src/sys.c's arms (__ai_call's negative-osv door), so most need no
// branch of their own.
#include "love.h"
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stddef.h>      // offsetof (the struct ai_wait_fd / struct pollfd assert)
#include <stdio.h>
#include <stdnoreturn.h>
#include <time.h>
#include <unistd.h>
#include <string.h>   // memcpy, for the argv marshal at the foot of this file

// __ai_osv, "which kernel this binary stands on", rides love.h: os.c defines
// it hosted, love.c carries the weak zero for links with no nolibc at all.

// CLOCK_REALTIME in milliseconds -- the one scale for the scheduler's
// deadlines, (clock t), and every mtime. on inle the call lands in the
// clock_gettime arm, which reads the kernel's kboot/kticks scale.
ai_noinline uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_REALTIME, &ts) ? (uintptr_t) -1
       : (uintptr_t) (ts.tv_sec * 1000 + ts.tv_nsec / 1000000); }

// the kernel's port lanes (src/kmain.c): the seat translation, then the rows
// -- a protocol read(2) cannot carry, busy and end being distinct answers, so
// the vt branches here rather than riding the syscall door. weak refusals so a
// hosted link, which never takes the branch, closes without them.
__attribute__((weak)) struct ai *k_port_flush(struct ai *g) { return g; }
__attribute__((weak)) intptr_t k_port_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  (void) fp, (void) src, (void) n; return -1; }
__attribute__((weak)) intptr_t k_port_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  (void) g, (void) dst, (void) n; return -1; }

// SIGPIPE is ignored (main) and the console re-raises it by hand: a runtime that answers
// "the device is gone" must not be killed before it reads the answer, but a shell tool must
// still die on a closed pipe or `love ... | head` runs to completion writing into nothing.
// so a heap port reports (writen answers -1, io_wdrain drops the run) and a static
// re-raises. re-raising rather than exiting keeps the wait status a signal death, so the
// shell's reporting and every `$?` downstream read as they always did.
static noreturn void console_hangup(void) {
 signal(SIGPIPE, SIG_DFL);
 raise(SIGPIPE);
 _exit(128 + SIGPIPE); }               // unreached unless someone caught it

static struct ai *fd_flush(struct ai *g) {
 if (__ai_osv < 0) return k_port_flush(g);
 if (g->io == &ai_stdout.io && fflush(stdout) && errno == EPIPE) console_hangup();
 return g; }

// land every byte, waiting on the device as long as it takes. answers how many
// got there, so a caller can tell a full write from a dead fd. (main.c's stdin
// pumper borrows it, hence the export.)
uintptr_t ai_fd_write_all(int fd, unsigned char const *src, uintptr_t n) {
 uintptr_t i = 0;
 while (i < n) {
  ssize_t k = write(fd, src + i, n - i);
  if (k < 0) { if (errno == EINTR) continue; break; }
  i += (uintptr_t) k; }
 return i; }

// the bulk lanes (contract in love.h). stdout rides stdio -- the static port has
// no buffer of love's own (nothing traces a static), so without fwrite every
// byte of every print would be its own write(2). one door, so there is no ordering to keep.
//
// nonblocking where a residue can be kept, and only there. a heap port carries love's write
// run behind it and io_wdrain re-offers whatever this call refuses, so the door answers what
// one stroke took and the writing task goes on rather than a peer that never reads stopping
// the whole vm. the three statics have no such run (nothing traces a static) and their
// per-byte lane prints from inside a structural printer, with nowhere to park mid-shape, so
// a refusal there would be a byte on the floor: their door lands what it takes and is the
// one place in this frontend still allowed to wait -- bounded, because a console drains.
//
// the O_NONBLOCK toggle is per-call for every fd we did not take. the flags ride the open
// file description, which a pty child and the shell that launched us both share, and leaving
// a terminal nonblocking at exit hands the user's shell back broken ("resource temporarily
// unavailable" on their next line) -- so an fd we merely inherited gets its flags read and
// put back around each call and we cache nothing. the pair is skipped where it already says
// nonblocking, which is free and covers the fds love opens itself. per call is the whole
// story: at 953 KB it would be 2.9M fcntls, which is why the run above pays the pair once
// per 4096 and a pipe -- whose bit is taken for the session (`inflag`) -- skips it outright.
static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
 if (__ai_osv < 0) return k_port_writen(fp, src, n);
 struct ai_io *io = (*fp)->io;
 intptr_t fd = ai_io_fd(io);
 if (io == &ai_stdout.io || io == &ai_stdin.io || io == &ai_stderr.io) {
  uintptr_t k = io == &ai_stdout.io ? fwrite(src, 1, n, stdout)
                                 : ai_fd_write_all((int) fd, src, n);
  if (k < n && errno == EPIPE) console_hangup();
  return (intptr_t) k; }
 int fl = fcntl((int) fd, F_GETFL), off = fl >= 0 && !(fl & O_NONBLOCK);
 if (off) fcntl((int) fd, F_SETFL, fl | O_NONBLOCK);
 ssize_t k;
 do k = write((int) fd, src, n); while (k < 0 && errno == EINTR);
 if (off) fcntl((int) fd, F_SETFL, fl);
 return k > 0 ? (intptr_t) k
      : (errno == EAGAIN || errno == EWOULDBLOCK) ? 0 : -1; }   // busy vs gone
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
 if (__ai_osv < 0) return k_port_readn(g, dst, n);
 intptr_t fd = ai_io_fd(g->io);
 ssize_t k;
 if (fd == STDIN_FILENO && ai_core_of(g)->inflag) k = read((int) fd, dst, n);   // the bit is already ours
 else {
  int fl = fcntl((int) fd, F_GETFL), off = fl >= 0 && !(fl & O_NONBLOCK);
  if (off) fcntl((int) fd, F_SETFL, fl | O_NONBLOCK);
  k = read((int) fd, dst, n);
  if (off) fcntl((int) fd, F_SETFL, fl); }
 return k > 0 ? (intptr_t) k
      : k == 0 ? -1
      : (errno == EAGAIN || errno == EWOULDBLOCK) ? 0 : -1; }

struct ai_port_vt const ai_fd_port_vt =
 { fd_flush, fd_writen, fd_readn, NULL };

struct ai_fio
 ai_stdin = { { lvm_port_io, &ai_fd_port_vt, putcharm(EOF) }, putcharm(STDIN_FILENO) },
 ai_stdout = { { lvm_port_io, &ai_fd_port_vt, putcharm(EOF) }, putcharm(STDOUT_FILENO) },
 ai_stderr = { { lvm_port_io, &ai_fd_port_vt, putcharm(EOF) }, putcharm(STDERR_FILENO) };

// the GC-context drain (a collected port's unflushed write run): raw write(2),
// no g machinery -- safe inside run_finalizers. on inle the write lands in
// k_fd_write's row, which is that port's absolute fd by the seat law.
void ai_fd_drain(int fd, void const *p, uintptr_t n) { ai_fd_write_all(fd, p, n); }

// waiting -- the frontier's fd-keyed park/wake and the fd close, one
// definition each (plan C2). hosted, one poll(2) covers a whole block
// (love.h lays struct ai_wait_fd as poll's own struct for exactly this); on
// inle the rows and the tick clock answer, through the weak k_ hooks kmain
// overrides -- conservative no-ops here, so a hosted link closes without them.
__attribute__((weak)) void k_row_close(int fd) { (void) fd; }
__attribute__((weak)) bool k_ready(int fd, int events) { (void) fd, (void) events; return true; }
__attribute__((weak)) void k_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  (void) fds, (void) n, (void) ms; }
__attribute__((weak)) void k_sleep(uintptr_t ms) { (void) ms; }

// shared EINTR-retry skeleton for poll-based wait. ms=0 means infinite.
// returns only when poll succeeds (data ready / deadline elapsed) or fails
// for a non-EINTR reason.
static void poll_wait(struct pollfd *fds, nfds_t nfds, uintptr_t ms) {
  uintptr_t deadline = ms == 0 ? 0 : ai_clock() + ms;
  for (;;) {
    int t = ms == 0 ? -1 :
            ms > (uintptr_t) __INT_MAX__ ? __INT_MAX__ : (int) ms;
    if (poll(fds, nfds, t) >= 0 || errno != EINTR) return;
    if (!deadline) continue;
    uintptr_t now = ai_clock();
    if (now >= deadline) return;
    ms = deadline - now; } }

void ai_sleep(uintptr_t ms) {
  if (__ai_osv < 0) return k_sleep(ms);
  poll_wait(NULL, 0, ms); }

static ai_noinline int poll_wrap(int fd, int events) {
  struct pollfd p = { .fd = fd, .events = (short) events };
  return poll(&p, 1, 0); }

bool ai_ready(int fd, int events) {
  if (__ai_osv < 0) return k_ready(fd, events);
  return fd < 0 || poll_wrap(fd, events) > 0; }

// love.h lays the block out as poll(2)'s own struct, so there is nothing to copy
// and no vector of ours to size -- which is the whole reason the count needs no
// ceiling, and why `revents` comes back to the scheduler for free.
_Static_assert(sizeof(struct ai_wait_fd) == sizeof(struct pollfd)
            && offsetof(struct ai_wait_fd, fd) == offsetof(struct pollfd, fd)
            && offsetof(struct ai_wait_fd, events) == offsetof(struct pollfd, events),
               "struct ai_wait_fd must be this platform's struct pollfd");
// ... and the two directions must be poll's own bits, for the same reason.
_Static_assert(ai_wait_in == POLLIN && ai_wait_out == POLLOUT,
               "ai_wait_in/out must be this platform's POLLIN/POLLOUT");

// the events come in filled, per fd -- the scheduler knows each task's park
// direction and a blanket mask would wake readers on writable. poll(2) fills
// `revents` on the way back out and the scheduler reads it (love.h).
void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  if (__ai_osv < 0) return k_wait_fds(fds, n, ms);
  if (n <= 0) { ai_sleep(ms); return; }
  poll_wait((struct pollfd*) fds, (nfds_t) n, ms); }

// the same block, asked and not waited on -- one poll(2) for the whole parked ring,
// where the weak default would spend one per fd. that is what lets the scheduler sweep
// the parked tasks on a fairness yield at all (love.c, over sweep_interval).
// no EINTR retry: a zero timeout means poll returns at once, and a signal that beats
// it is answered by leaving every revents zero -- "none ready", asked again next sweep.
// retrying would be the one thing this call must never do, which is block.
// on inle the sweep is per row (love.c's weak default's law: every slot filled,
// so "none ready" never reads as "nobody answered").
void ai_ready_fds(struct ai_wait_fd *fds, int n) {
  if (__ai_osv < 0) {
    for (int i = 0; i < n; i++)
      fds[i].revents = k_ready(fds[i].fd, fds[i].events) ? fds[i].events : 0;
    return; }
  if (n <= 0) return;
  if (poll((struct pollfd*) fds, (nfds_t) n, 0) >= 0) return;
  for (int i = 0; i < n; i++) fds[i].revents = 0; }

// override the weak g.c default with the real close. called by the finalizer
// that ai_io_alloc registers, so it runs when a heap port becomes unreachable.
// static stdin/stdout don't go through this path -- they live outside the l
// heap and the GC never visits them. on inle the row's own close method runs.
void ai_fd_close(int fd) {
  if (__ai_osv < 0) return k_row_close(fd);
  close(fd); }

// a love port -> the fd under it, or -1 for anything that is not an fd port. the fd
// port is this file's (ai_fd_port_vt below), so the question belongs here too.
intptr_t ai_port_fd(ai_word x) {
 if (!charmp(x) && ((union u*) x)->ap == lvm_port_io)
    return ai_io_fd((struct ai_io*) x);
 return -1; }

// argv: the chain of strings at g->sp[0] -> a NUL-terminated char** laid in the
// uncommitted heap gap at Hp. GC-invisible, holds no l pointers, and valid across a
// fork -- host_spawn_guard (src/posix.c) leaves the window above hp mapped for exactly
// this, so what execvp reads must live here and not in the strings themselves.
// consumed before any further allocation; never bumps Hp.
//
// -> g, and *cavp is the vector or NULL. the two failures are told apart by the g:
// argv not a chain of strings, or empty, leaves g OK (each caller says what a misuse
// answers -- they do not agree), and a failed reserve leaves it not ok.
struct ai *ai_argv_marshal(struct ai *g, char ***cavp) {
 *cavp = NULL;
 ai_word argv = g->sp[0];
 uintptr_t argc = 0, total = 0;
 for (ai_word p = argv; chainp(p); p = B(p)) {
  if (!ai_strp(A(p))) return g;                              // misuse: non-string argv
  argc++, total += len(A(p)) + 1; }                          // +1 for the NUL
 if (!argc) return g;                                        // empty argv
 if (!ai_ok(g = ai_have(g, argc + 1 + b2w(total)))) return g;
 argv = g->sp[0];                            // ai_have may have GC'd; argv is the only
                                             // root, at sp[0], so it is forwarded there
 char **cav = (char**) g->hp,                                // at Hp: aligned
      *blob = (char*) (g->hp + (argc + 1));                  // whole words after
 uintptr_t off = 0, i = 0;
 for (ai_word p = argv; chainp(p); p = B(p), i++) {
  struct ai_str *s = str(A(p));
  memcpy(blob + off, txt(s), len(s));
  blob[off + len(s)] = 0;
  cav[i] = blob + off;
  off += len(s) + 1; }
 cav[argc] = NULL;
 *cavp = cav;
 return g; }
