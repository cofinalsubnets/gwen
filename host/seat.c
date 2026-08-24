// host/seat.c -- the seat two frontends share: host/main.c's binary and the
// inle kernel both link this file, so what lives here exists ONCE where it
// used to exist twice. the bodies bottom out in libc calls, and on inle those
// land in free/sys.c's arms (__ai_call's negative-osv door), so most need no
// branch of their own.
#include "love.h"
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <time.h>
#include <unistd.h>

// which kernel this binary stands on: nolibc's os.c defines it (0 unprobed;
// negative says we ARE the kernel). weak so a foreign-libc link (love0) still
// resolves it -- nothing overrides there, and zero reads as hosted, which such
// a link always is.
__attribute__((weak)) long __ai_osv;

// CLOCK_REALTIME in milliseconds -- the one scale for the scheduler's
// deadlines, (clock t), and every mtime. on inle the call lands in the
// clock_gettime arm, which reads the kernel's kboot/kticks scale.
ai_noinline uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_REALTIME, &ts) ? (uintptr_t) -1
       : (uintptr_t) (ts.tv_sec * 1000 + ts.tv_nsec / 1000000); }

// the kernel's port lanes (free/kmain.c): the seat translation, then the rows
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

// the source library: each frontend keeps its own table -- the baked cat is
// the seat's -- and the one ai_libs picks by the same value. weak NULLs so
// either link closes with only its own table on board.
__attribute__((weak)) struct ai_lib const *k_libs(void) { return NULL; }
__attribute__((weak)) struct ai_lib const *host_libs(void) { return NULL; }
struct ai_lib const *ai_libs(void) { return __ai_osv < 0 ? k_libs() : host_libs(); }
