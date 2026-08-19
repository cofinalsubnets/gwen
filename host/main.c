#include "love.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <time.h>
#include <poll.h>
#include <errno.h>
#include <math.h>
#include <stddef.h>      // offsetof (the struct ai_wait_fd / struct pollfd assert)
#if defined(AiNolibc)
extern long __ai_osv;    // which kernel this run met: 1 linux, 2 freebsd, 3 netbsd (love-os)
#endif
extern void host_spawn_guard(struct ai*, int);   // host/posix.c (exec-bound forks drop the pools)
#include <stdnoreturn.h>
#include <signal.h>
#include <sys/wait.h>

ai_noinline uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_REALTIME, &ts) ? (uintptr_t) -1
       : (uintptr_t) (ts.tv_sec * 1000 + ts.tv_nsec / 1000000); }

// the fine clock's real source (the weak default in love.c degrades to ms*1e6)
// FIXME this seems useless on 32 bit. who uses it? maybe squash back to just
// one ai_clock() and pick resolution at compile time based on INTPTR_MAX
ai_noinline intptr_t ai_nclock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? -1
       : (intptr_t) ts.tv_sec * 1000000000 + ts.tv_nsec; }


// --- fd 0, and the price of taking it ---
// A bare port reads ONE BYTE PER readn (love.c's io_refill) and fd_readn pays 3 fcntl
// beside each one, so a 953 KB corpus down stdin cost 3.8M syscalls against a file
// argument's 23K. Both halves of that are BORROWINGS of the same fd, and what a door can
// lend decides which it gets:
//   seekable -- a heap bio parked in `inport`, which love.c's rbio_of reads THROUGH the
//               static, plus the seek back below. 3.8M -> 23K.
//   a pipe   -- the same run, plus the TOGGLE: O_NONBLOCK on for the whole session, the
//               old flags in `inflag`, so the gulp costs no fcntl at all. It cannot be
//               seeked back, so its residue is DELIVERED instead (stdin_hand).
//   a tty    -- neither. A human types, so syscalls-per-byte buys nothing, and a terminal
//               handed back nonblocking is the one version of this that breaks the
//               user's shell ("resource temporarily unavailable" on their next line).
//               With no run, `reads` keeps trickling -- which is what a prompt wants.
// ⚠ ONE PORT, ONE POSITION -- that is what makes the run safe. Every in-process reader
// goes through zgetc, which drains the run before the device, so an in-form (slurp in)
// still sees exactly the bytes our reader has not taken. What runs ahead is only the
// KERNEL's fd offset, and only an inheritor can see that -- hence the seek.
// ⚠ NOTHING PUTS A PIPE BACK -- so the residue is not UNDONE, it is DELIVERED (stdin_hand),
// which is why this door can hold a run at all; bash, with no fork to spare at the handoff,
// pays per byte instead. test_stdinbuf runs one program down each door and diffs, so a lane
// that starts running ahead without delivering fails there.
// ⚠ TWO PLACES HOLD UNREAD BYTES: the borrowed run, and `in`'s OWN pushback -- the ungetc
// stays on the static because that is the port everyone above reads.
static void stdin_give(struct ai *g) {
 if (!g || !ai_ok(g)) return;
 struct ai *fc = ai_core_of(g);
 if (fc->inflag)                                          // its blocking bit was ours: back it goes
  fcntl(STDIN_FILENO, F_SETFL, (int) getcharm(fc->inflag)), fc->inflag = 0;
 if (!fc->inport || lseek(STDIN_FILENO, 0, SEEK_CUR) < 0) return;   // an unseekable door: stdin_hand's
 uintptr_t n = ai_io_pending(g, (struct ai_io*) fc->inport)
             + (getcharm(ai_stdin.io.ungetc_buf) != EOF ? 1 : 0);
 if (n) lseek(STDIN_FILENO, -(off_t) n, SEEK_CUR); }
// ⚠ `in` IS NOT REBOUND -- it stays the static, and the run is BORROWED behind it (love.c's
// rbio_of). Rebinding would be unsound: bao's `reads` folded its own `in` at egg-compile
// time, so a fresh object fails its (id? p in) test and it would gulp the stream it means
// to trickle. Neither borrowing is ever named in the book at all.
static struct ai *stdin_take(struct ai *g) {
 if (!ai_ok(g)) return g;
 if (lseek(STDIN_FILENO, 0, SEEK_CUR) < 0) {              // not seekable: a tty, or a pipe
  if (isatty(STDIN_FILENO)) return g;
  int fl = fcntl(STDIN_FILENO, F_GETFL);                  // a pipe: take the bit AND the bytes
  if (fl >= 0 && ((fl & O_NONBLOCK) || fcntl(STDIN_FILENO, F_SETFL, fl | O_NONBLOCK) >= 0))
   ai_core_of(g)->inflag = putcharm(fl); }                // already-nonblocking restores to itself
 if (!ai_ok(g = ai_io_alloc(g, STDIN_FILENO))) return g;
 struct ai *fc = ai_core_of(g);
 fc->inport = fc->sp[0], fc->sp++;
 return g; }

// for (;;): the standard noreturn-defensive shape -- moon's stdnoreturn.h defines
// `noreturn` empty, so mooncc can't cut the fall-through tail itself; the loop
// leaves no ret for vmret to flag (gcc emits identical code either way).
static noreturn lvm(lvm_exit) { for (;;) stdin_give(g), exit(getcharm(Sp[0])); }
// Shared EINTR-retry skeleton for poll-based wait. ms=0 means infinite.
// Returns only when poll succeeds (data ready / deadline elapsed) or fails
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

void ai_sleep(uintptr_t ms) { poll_wait(NULL, 0, ms); }

static ai_noinline int poll_wrap(int fd, int events) {
  struct pollfd p = { .fd = fd, .events = (short) events };
  return poll(&p, 1, 0); }

bool ai_ready(int fd, int events) { return fd < 0 || poll_wrap(fd, events) > 0; }

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

// ⚠ THE EVENTS COME IN FILLED, per fd -- the scheduler knows each task's park
// direction and a blanket mask would wake readers on writable. poll(2) fills
// `revents` on the way back out and the scheduler reads it (love.h).
void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  poll_wait((struct pollfd*) fds, (nfds_t) n, ms); }

// The same block, asked and not waited on -- ONE poll(2) for the whole parked ring,
// where the weak default would spend one per fd. That is what lets the scheduler sweep
// the parked tasks on a fairness yield at all (love.c, over sweep_interval).
// ⚠ NO EINTR RETRY: a zero timeout means poll returns at once, and a signal that beats
// it is answered by leaving every revents zero -- "none ready", asked again next sweep.
// Retrying would be the one thing this call must never do, which is block.
void ai_ready_fds(struct ai_wait_fd *fds, int n) {
  if (n <= 0) return;
  if (poll((struct pollfd*) fds, (nfds_t) n, 0) >= 0) return;
  for (int i = 0; i < n; i++) fds[i].revents = 0; }

// ⚠ SIGPIPE IS IGNORED (main), AND THE CONSOLE RE-RAISES IT BY HAND. a runtime
// that ANSWERS "the device is gone" cannot be killed before it reads the answer
// -- kiosko died whenever a client hung up mid-response, which is the ordinary
// thing a browser does. but a SHELL TOOL must still die on a closed pipe, or
// `love ... | head` runs to completion writing into nothing. the line between the
// two is the one rung 4 already drew: a HEAP port reports (writen answers -1 and
// io_wdrain drops the run), a STATIC re-raises. re-raising rather than exiting
// keeps the wait status a signal death, so the shell's own reporting and every
// `$?` downstream are byte-for-byte what they always were.
static noreturn void console_hangup(void) {
 signal(SIGPIPE, SIG_DFL);
 raise(SIGPIPE);
 _exit(128 + SIGPIPE); }               // unreached unless someone caught it

static struct ai *fd_flush(struct ai *g) {
 if (g->io == &ai_stdout.io && fflush(stdout) && errno == EPIPE) console_hangup();
 return g; }

// land every byte, waiting on the device as long as it takes. Answers how many
// got there, so a caller can tell a full write from a dead fd.
static uintptr_t fd_write_all(int fd, unsigned char const *src, uintptr_t n) {
 uintptr_t i = 0;
 while (i < n) {
  ssize_t k = write(fd, src + i, n - i);
  if (k < 0) { if (errno == EINTR) continue; break; }
  i += (uintptr_t) k; }
 return i; }

// the bulk lanes (contract in love.h). stdout rides stdio -- the static port has
// no buffer of love's own (nothing traces a static), so without fwrite every
// byte of every print would be its own write(2). ⚠ there used to be an
// `fflush(stdout)` here and a per-byte `fputc` in a `putc` slot beside it,
// because two paths wrote one stream and the direct one had to land after the
// buffered one. One door, no ordering to keep.
//
// ⚠ NONBLOCKING WHERE A RESIDUE CAN BE KEPT, AND ONLY THERE. A heap port carries
// love's write run behind it, and io_wdrain re-offers whatever this call
// refuses -- so the door answers what one stroke took and the writing task goes
// on. That is the rung: a peer that never reads used to stop the whole vm, not
// the one task writing to it. The three STATICS have no such run (nothing
// traces a static) and their per-byte lane prints from inside a structural
// printer, with nowhere to park mid-shape, so a refusal there would be a byte
// on the floor. Their door lands what it takes and is the one place in this
// frontend still allowed to wait -- bounded, because a console drains.
//
// ⚠ THE O_NONBLOCK TOGGLE IS PER-CALL FOR EVERY FD WE DID NOT TAKE. The flags ride
// the OPEN FILE DESCRIPTION, which a pty child and the shell that launched us both
// share -- leaving a terminal nonblocking at exit is the classic way to hand the
// user's shell back broken ("resource temporarily unavailable" on their next line),
// so an fd we merely inherited gets its flags read and put back around each call and
// we cache nothing. We skip the pair when it already says nonblocking, which is free
// and covers the fds love opens itself. The measured price, now that readn is the sole
// read door: 3 fcntls + 1 read per CALL where it used to be 1 poll + 1 read (love's
// readiness pre-guard, deleted with getc). ⚠ PER CALL is the whole story -- it was
// priced when a call meant a byte, and at 953 KB that is 2.9M fcntls. The answer is the run
// above: it pays the pair once per 4096, and a pipe -- whose bit is TAKEN for the session
// (`inflag`) -- skips it outright, which is the one exemption below.
static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
 struct ai_io *io = (*fp)->io;
 intptr_t fd = ai_io_fd(io);
 if (io == &ai_stdout.io || io == &ai_stdin.io || io == &ai_stderr.io) {
  uintptr_t k = io == &ai_stdout.io ? fwrite(src, 1, n, stdout)
                                 : fd_write_all((int) fd, src, n);
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
// Override the weak g.c default with the real POSIX close. Called by the
// finalizer that ai_io_alloc registers, so it runs when a heap port becomes
// unreachable. Static stdin/stdout don't go through this path -- they live
// outside the l heap and the GC never visits them.
void ai_fd_close(int fd) { close(fd); }
// the GC-context drain (a collected port's unflushed write run): raw write(2),
// no g machinery -- safe inside run_finalizers.
void ai_fd_drain(int fd, void const *p, uintptr_t n) { fd_write_all(fd, p, n); }

// --- handing fd 0 to a child: the unseekable half of stdin_give, up top ---
// A forked pumper writes the residue into a fresh pipe, splices whatever the old fd 0 still
// brings, and the read end becomes fd 0 -- one fork per handoff, only when a residue exists.
// ⚠ ONLY WHERE A CHILD TAKES fd 0. At our own exit nothing of ours is left to pump and the
// dup2 would be private to a process about to vanish, so lvm_exit and main's tail call
// stdin_give alone. A peer holding fd 0 from BEFORE us (`cat f | { love a.l; love b.l; }`)
// is out of reach on a pipe however we hand off -- the one thing this door cannot promise.
// ⚠ stdin_give FIRST: the pumper reads fd 0 itself, and an EAGAIN on the bit we borrowed
// would read there as an end and cut the stream short.
// ⚠ THE PUSHBACK BYTE LEADS -- `in`'s ungetc is the earlier of the two places holding unread
// bytes, so it goes in front of the run, as chug_str splits it.
// ⚠ THE PUMPER IS A FORK: it copies every fd love had open and nobody reaps it. Narrow while
// exec is the only caller, but a live pipe love still held would keep a second writer on it.
static void stdin_hand(struct ai *g) {
 stdin_give(g);
 if (!g || !ai_ok(g)) return;
 struct ai *fc = ai_core_of(g);
 if (!fc->inport || lseek(STDIN_FILENO, 0, SEEK_CUR) >= 0) return;   // seekable: the seek said it all
 unsigned char res[ai_iobuf + 1];
 uintptr_t n = 0;
 if (getcharm(ai_stdin.io.ungetc_buf) != EOF)
  res[n++] = (unsigned char) getcharm(ai_stdin.io.ungetc_buf),
  ai_stdin.io.ungetc_buf = putcharm(EOF);
 n += ai_io_read_drain(g, (struct ai_io*) fc->inport, res + n, sizeof res - n);
 if (!n) return;                                                    // nothing owed: the fd is already exact
 int p[2];
 if (pipe(p)) return;
 pid_t pid = fork();
 if (pid < 0) { close(p[0]); close(p[1]); return; }
 if (!pid) {                                                        // the pumper: residue, then the rest
  close(p[0]);
  if (fd_write_all(p[1], res, n) == n)
   for (;;) {
    unsigned char buf[ai_iobuf];
    ssize_t k = read(STDIN_FILENO, buf, sizeof buf);
    if (k < 0 && errno == EINTR) continue;                          // ⚠ a signal is not an end
    if (k <= 0 || fd_write_all(p[1], buf, (uintptr_t) k) < (uintptr_t) k) break; }
  _exit(0); }                                                       // ⚠ _exit: no atexit, no flush, no love
 close(p[1]);
 if (p[0] != STDIN_FILENO) dup2(p[0], STDIN_FILENO), close(p[0]); }

// (open path mode) — open a file with mode "r"/"w"/"a"; returns a heap port
// (closed on GC) or zero on error or misuse. mode is a l string; only the
// first byte is consulted.
//   r = read-only
//   w = write-only, truncate-or-create
//   a = write-only, append-or-create
// Errors (path too long, unknown mode, open(2) failure) all return zero.

static ai_noinline int call_open(struct ai_str *pv, struct ai_str *mv) {
  uintptr_t plen = pv->len;
  char path[4096];
  if (plen >= sizeof path || mv->len == 0) return -1;
  memcpy(path, pv->bytes, plen);
  path[plen] = 0;
  int flags;
  switch (mv->bytes[0]) {
    case 'r': flags = O_RDONLY; break;
    case 'w': flags = O_WRONLY | O_CREAT | O_TRUNC; break;
    case 'a': flags = O_WRONLY | O_CREAT | O_APPEND; break;
    default: return -1; }
  return open(path, flags, 0644); }

static lvm(lvm_open) {
  if (!ai_strp(Sp[0]) || !ai_strp(Sp[1])) goto fail;
  struct ai_str *pv = (struct ai_str*) Sp[0];
  struct ai_str *mv = (struct ai_str*) Sp[1];
  int fd = call_open(pv, mv);
  if (fd < 0) goto fail;
  Pack(g);
  struct ai *r = ai_io_alloc(g, fd);
  if (!ai_ok(r)) { close(fd); goto fail; }
  g = r;
  Unpack(g);
  // stack: [port, path, mode, ...] -> [port, ...]
  Sp[2] = Sp[0];
  Sp += 2;
  Ip += 1;
  ai_musttail return Continue();
 fail:
  Sp[1] = ZeroPoint;
  Sp += 1;
  Ip += 1;
  ai_musttail return Continue(); }

// (close p) — close a port and HAND IT THE CLOSED VT, so every later read,
// write and flush finds the door that does nothing and the finalizer, which
// asks the vt for an fd, skips. Returns (). No-op on misuse, matching the
// existing fputc/etc. convention.
static lvm(lvm_close) {
  // inline "is x a port": heap pointer whose discriminator is lvm_port_io.
  if ((Sp[0] & 1) == 0 && ((union u*) Sp[0])->ap == lvm_port_io) {
    struct ai_io *io = (struct ai_io*) Sp[0];
    intptr_t fd = ai_io_fd(io);
    if (fd >= 0) {
      g->io = io;
      Pack(g);
      g = ai_io_wflush(g, io);   // buffered bytes land before the fd dies
      if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
      // the device would not take the whole run: PARK and come back. nothing has
      // been mutated yet -- the fd is open and Ip unadvanced -- so the re-run is
      // this same close from the top. it used to deliver by blocking, which stops
      // every other task for a peer that is only slow.
      if (ai_io_wpending(g, (struct ai_io*) g->sp[0])) {
        Unpack(g);
        g->next_wake_at = ai_clock() + 1;
        ai_musttail return Ap(lvm_yield_sw, g); }
      Unpack(g);
      close(fd);
      ((struct ai_io*) Sp[0])->vt = &ai_closed_vt; } }   // ⚠ re-read: wflush may collect
  Sp[0] = ZeroPoint;
  Ip += 1;
  ai_musttail return Continue(); }

// --- subprocess (hark) + environment (getenv) ---------------------------
// Both are host-only nifs (POSIX fork/exec/wait, getenv), like open/close.
// No malloc: argv is marshalled into the uncommitted l heap gap and the
// child's stdout is captured into a growing l string (the reader's
// str0 + grow + len-fixup pattern). See core/io.c ioread1str / grbufg.

// Best-effort write-through for the tee mode below: loop over a partial write,
// but let a failed/closed stdout pass silently -- a broken pipe on the ECHO of a
// child's output must not fail the child, which ran fine.
static void host_teeout(char const *p, size_t n) {
 while (n) {
  ssize_t w = write(STDOUT_FILENO, p, n);
  if (w < 0) { if (errno == EINTR) continue; return; }
  p += w, n -= (size_t) w; } }

// (hark argv) / (herald argv) are a TWO-AP NIF BODY -- {{start}, {drain}, {ret0}} --
// because the op is not re-runnable where it has to park. love.h's nif park says
// "leave Ip unadvanced and yield, the op re-runs", and a hark that re-ran from the
// top would fork a SECOND child. So the fork and the capture are two ops, and the
// park lives in the second one, which re-runs as often as the child is slow.
//
// The whole park state is FIVE STACK SLOTS, which the yield snapshots and the GC
// traces for free -- no C local survives a turn, and the capture string is free
// to move between them:
//
//    sp[0] out    the growing capture string -- or, when fd is -1, the whole answer
//    sp[1] n      bytes filled so far (a charm)
//    sp[2] fd     >= 0 draining | -2 drained, reaping | -1 done, out IS the answer
//    sp[3] pid    the child (a charm)
//    sp[4] tee    0/1 -- argv's own slot, which argv is done with by then
//    sp[5]        the return ip lvm_ret0 wants
//
// `tee` picks WHEN the captured output reaches stdout, not whether it is
// captured: 0 (hark) holds it until the child exits and hands the whole string
// back for the caller to do as it likes; 1 (herald) ALSO write(2)s each chunk
// through as it is read, so a long-running child STREAMS -- which is what make
// does (it pipes the child too, then relays each chunk rather than hoarding it).
// A capture-and-reprint caller passes 1 and skips its own reprint; a caller that
// consumes the text -- $(shell ..), a glob, an mtime probe -- passes 0. Because
// the tee bypasses the l-level `out` buffer, a teeing caller must (flush out)
// first or its own echoed lines land after the child's bytes.
//
// &locals (pipes/pid/status) are fine in both helpers: they return normally,
// they are not VM-dispatch tail-call sites (cf. call_open vs lvm_open).

// Lay the four state slots over argv, so every exit from the spawn -- a misuse,
// a failed pipe, a failed fork, a failed exec, a live child -- hands the drain
// ONE shape to read. The capture string (or the errno answer) goes on top after.
static struct ai *host_harkst(struct ai *g, intptr_t fd, intptr_t pid, int tee) {
 g = ai_push(g, 3, putcharm(0), putcharm(fd), putcharm(pid));
 if (ai_ok(g)) g->sp[3] = putcharm(tee);
 return g; }

// The first ap: marshal argv, fork, and confirm the exec. Called with g Packed;
// argv is at sp[0]. Returns a not-ok g only on OOM.
ai_noinline static struct ai *host_harkstart(struct ai *g, int tee) {
 // pass 1: validate every element is a string; size the arg-byte blob.
 ai_word argv = g->sp[0];
 intptr_t argc = 0;
 uintptr_t total = 0;
 for (ai_word p = argv; chainp(p); p = B(p)) {
  if (!ai_strp(A(p)))                                     // misuse
   return ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(-1));
  argc++, total += len(A(p)) + 1; }                       // +1 for the NUL
 if (!argc)                                               // empty argv
  return ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(-1));

 // Reserve gap for cav (argc+1 pointers, word-aligned) + the byte blob.
 // Written into the uncommitted region at Hp -- invisible to GC, holds no
 // l pointers, consumed before any further allocation. Never bump Hp.
 if (!ai_ok(g = ai_have(g, (uintptr_t) argc + 1 + b2w(total)))) return g;
 argv = g->sp[0];          // ai_have may have GC'd; argv (the only root, at sp[0])
                           // is forwarded there -- the C local is now stale.
 char **cav = (char**) g->hp;                             // at Hp: aligned
 char *blob = (char*) (g->hp + (argc + 1));               // whole words after
 { uintptr_t off = 0; intptr_t i = 0;
   for (ai_word p = argv; chainp(p); p = B(p), i++) {         // re-walk post-ai_have
    struct ai_str *s = str(A(p));
    memcpy(blob + off, txt(s), len(s));
    blob[off + len(s)] = 0;
    cav[i] = blob + off;
    off += len(s) + 1; }
   cav[argc] = NULL; }

 // spawn: stdout pipe + a close-on-exec error pipe. On a successful exec the
 // kernel closes ep[1] -> parent reads EOF; on failure the child writes errno
 // -> parent distinguishes "couldn't spawn" from "ran and exited 127".
 // ⚠ THAT HANDSHAKE STILL BLOCKS, and it is the one wait left here: it is
 // bounded by the child's exec(2), not by the child's life, which is the whole
 // difference this rung is about. (Every push below happens after the fork, so
 // growing the stack over the cav/blob gap is the parent's business alone.)
 int op[2], ep[2];
 // ⚠ errno into a local BEFORE the state push, on every one of these: the push
 // may collect, and a collection that grows the pool makes syscalls of its own.
 if (pipe(op)) { int e = errno;
  return ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(e)); }
 if (pipe(ep)) { int e = errno; close(op[0]); close(op[1]);
  return ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(e)); }
 fcntl(ep[1], F_SETFD, FD_CLOEXEC);
 fflush(stdout);
 host_spawn_guard(g, 1);
 pid_t pid = fork();
 if (pid) host_spawn_guard(g, 0);   // parent (a failed fork included); the child's g is unmapped
 if (pid < 0) { int e = errno;
  close(op[0]); close(op[1]); close(ep[0]); close(ep[1]);
  return ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(e)); }
 if (!pid) {                                              // child
  signal(SIGPIPE, SIG_DFL);                               // the ignore must not ride the exec
  dup2(op[1], STDOUT_FILENO);
  // DETACH stdin from the controlling terminal: this is a CAPTURE spawn (we want the child's
  // output, never interactive input), so give it /dev/null. Otherwise a child that touches the
  // tty -- e.g. qemu `-serial stdio` doing tcsetattr -- gets SIGTTOU/SIGTTIN as a background
  // process and STOPS, producing no output (the `make test_kernel` hang under an interactive
  // shell; invisible when run with no controlling tty). stdout is already the pipe, also non-tty.
  int nul = open("/dev/null", O_RDONLY);
  if (nul >= 0) { dup2(nul, STDIN_FILENO); if (nul > 2) close(nul); }
  close(op[0]); close(op[1]); close(ep[0]);
  execvp(cav[0], cav);
  int e = errno; ssize_t w = write(ep[1], &e, sizeof e); (void) w;
  _exit(127); }
 close(op[1]); close(ep[1]);                              // parent
 int childerr = 0; ssize_t r;
 do r = read(ep[0], &childerr, sizeof childerr); while (r < 0 && errno == EINTR);
 close(ep[0]);
 if (childerr) {                                          // exec failed
  close(op[0]);
  int st; while (waitpid(pid, &st, 0) < 0 && errno == EINTR) {}
  return ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(childerr)); }

 // The read end never blocks. It is a fresh fd the child does not share, so the
 // flag just STAYS on -- none of fd_readn's per-call toggle dance, which exists
 // for fds whose open file description a forked child holds too.
 { int fl = fcntl(op[0], F_GETFL); if (fl >= 0) fcntl(op[0], F_SETFL, fl | O_NONBLOCK); }
 return str0(host_harkst(g, op[0], pid, tee), 1u << 16); }  // capture -> sp[0]

// The second ap, once per scheduled turn: take what the pipe has (growing the
// string when it fills), tee it through if asked, then leave the state where the
// next turn finds it. Nothing here holds a pointer across an allocation -- the
// capture string is re-read off sp[0] every time, because a park may have moved it.
ai_noinline static struct ai *host_harkdrain(struct ai *g) {
 intptr_t fd = getcharm(g->sp[2]);
 if (fd == -1) return g;                        // nothing was spawned: sp[0] IS the answer
 pid_t pid = (pid_t) getcharm(g->sp[3]);
 if (fd >= 0) {
  int tee = getcharm(g->sp[4]) != 0;
  uintptr_t n = (uintptr_t) getcharm(g->sp[1]);
  for (;;) {
   uintptr_t lim = len(g->sp[0]);
   if (n == lim) {                                        // full -> double it and retry
    if (ai_ok(g = grbufg(g, lim))) continue;
    // ⚠ OOM mid-capture: close the pipe and KILL the child rather than wait on
    // it. A bounded reap of a killed child is not the wait this rung deletes.
    close((int) fd);
    kill(pid, SIGKILL);
    { int st; while (waitpid(pid, &st, 0) < 0 && errno == EINTR) {} }
    return g; }
   ssize_t r = read((int) fd, txt(g->sp[0]) + n, lim - n);
   if (r > 0) {
    if (tee) host_teeout(txt(g->sp[0]) + n, (size_t) r);  // ..before the buffer can move
    n += (uintptr_t) r;
    continue; }
   if (r < 0 && errno == EINTR) continue;
   g->sp[1] = putcharm((intptr_t) n);
   if (r < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
    g->next_wait_fd = (int) fd;                           // the scheduler owns the wait
    return g; }
   break; }                                               // EOF, or a read error we cannot use
  close((int) fd);
  g->sp[2] = putcharm(-2); }                              // drained; now reap
 // ⚠ THE REAP IS A POLL, for the reason the `wait` nif is one: SIGCHLD is not in
 // the scheduler's wait set and a pid is not an fd. It almost always answers on
 // the first ask -- the child closed its stdout on the way out -- so the tick is
 // what a child that closes stdout early and keeps computing costs, nothing more.
 { int st; pid_t w;
   do w = waitpid(pid, &st, WNOHANG); while (w < 0 && errno == EINTR);
   if (!w) { g->next_wake_at = ai_clock() + 1; return g; }
   uintptr_t n = (uintptr_t) getcharm(g->sp[1]);
   if (n) len(g->sp[0]) = n;                              // fix logical length
   else g->sp[0] = EmptyString;                           // empty output -> the singleton
   int status = w < 0 ? -1
              : WIFEXITED(st) ? WEXITSTATUS(st)
              : WIFSIGNALED(st) ? 128 + WTERMSIG(st) : -1;
   if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
   struct ai_chain *c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                  putcharm(status), g->sp[0]);
   g->sp[0] = word(c);
   g->sp[2] = putcharm(-1); }                             // done
 return g; }

static lvm(lvm_hark) {
 Pack(g);
 g = host_harkstart(g, 0);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// (herald argv) -- hark, TEEING: identical to (hark argv), same (status . output)
// answer, but the child's stdout is relayed as it arrives instead of only at
// exit. For a caller that just reprints what it captured; see the `tee` note above.
static lvm(lvm_herald) {
 Pack(g);
 g = host_harkstart(g, 1);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// The shared second ap. It PARKS -- Ip unadvanced, so the whole op re-runs on
// reschedule and reads its state back off the stack.
static lvm(lvm_harkdrain) {
 Pack(g);
 g = host_harkdrain(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 if (Sp[2] != putcharm(-1)) ai_musttail return Ap(lvm_yield_sw, g);
 Sp[4] = Sp[0];                                           // the answer over the state
 Sp += 4; Ip += 1;
 ai_musttail return Continue(); }

// (exec argv) -> REPLACE this process with argv[0], inheriting stdio (the real
// terminal). Unlike (hark argv) -- which forks, pipes the child's stdout into a
// captured string, and waits -- exec hands the tty straight to the child, so an
// INTERACTIVE program drives the terminal. On success it never returns; on a bad
// argv or a failed exec it returns an errno (or -1) fixnum, exactly like hark's
// spawn-failure path. cook execs its terminal recipe steps this way (the repl,
// gdb, the qemu run targets). Marshals argv into cav like hark, then execvp
// in place -- no allocation between the build and the exec, so cav stays valid.
ai_noinline static struct ai *host_exec(struct ai *g, ai_word argv) {
 intptr_t argc = 0;
 uintptr_t total = 0;
 for (ai_word p = argv; chainp(p); p = B(p)) {
  if (!ai_strp(A(p))) return ai_push(g, 1, putcharm(-1));   // misuse
  argc++, total += len(A(p)) + 1; }
 if (!argc) return ai_push(g, 1, putcharm(-1));            // empty argv
 if (!ai_ok(g = ai_have(g, (uintptr_t) argc + 1 + b2w(total)))) return g;
 argv = g->sp[0];                                          // re-root post-ai_have
 char **cav = (char**) g->hp;
 char *blob = (char*) (g->hp + (argc + 1));
 { uintptr_t off = 0; intptr_t i = 0;
   for (ai_word p = argv; chainp(p); p = B(p), i++) {
    struct ai_str *s = str(A(p));
    memcpy(blob + off, txt(s), len(s));
    blob[off + len(s)] = 0;
    cav[i] = blob + off;
    off += len(s) + 1; }
   cav[argc] = NULL; }
 fflush(stdout); fflush(stderr);
 signal(SIGPIPE, SIG_DFL);                                 // ... nor this one
 stdin_hand(g);                                            // the child inherits fd 0: hand it over exact
 execvp(cav[0], cav);
 return ai_push(g, 1, putcharm(errno)); }                  // exec failed -> errno

static lvm(lvm_exec) {
 Pack(g);
 g = host_exec(g, Sp[0]);                                  // returns only on failure
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 Sp[1] = Sp[0];                                            // errno fixnum over argv
 Sp += 1; Ip += 1;
 ai_musttail return Continue(); }

// Copy the name to a C string and look it up. Factored out (ai_noinline) so the
// memcpy(&name,...) escape can't defeat lvm_getenv's tail call (cf. call_open).
ai_noinline static char const *host_getenv(struct ai_str *nv) {
 char name[4096];
 if (nv->len >= sizeof name) return NULL;
 memcpy(name, nv->bytes, nv->len);
 name[nv->len] = 0;
 return getenv(name); }

// (getenv name) -> string, or zero if unset / misused. zero = absent, not an
// error; the run fixnum-error convention does not apply here.
static lvm(lvm_getenv) {
 char const *v = ai_strp(Sp[0]) ? host_getenv((struct ai_str*) Sp[0]) : NULL;
 if (!v) { Sp[0] = ZeroPoint; Ip += 1; ai_musttail return Continue(); }
 Pack(g);
 if (!ai_ok(g = ai_strof(g, v))) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 ai_musttail return Continue(); }

// (getpid x) -> the running process id (x ignored). main.c is linked into love0
// too, so unlike the host/*.c glob nifs this one exists in the bootstrap as well.
static lvm(lvm_getpid) { ai_musttail return Answer(putcharm(getpid())); }

static union u const
 nif_exit[] = {{lvm_exit}, {lvm_ret0}},
 nif_open[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_open}, {lvm_ret0}},
 nif_close[] = {{lvm_close}, {lvm_ret0}},
 nif_hark[] = {{lvm_hark}, {lvm_harkdrain}, {lvm_ret0}},
 nif_herald[] = {{lvm_herald}, {lvm_harkdrain}, {lvm_ret0}},
 nif_exec[] = {{lvm_exec}, {lvm_ret0}},
 nif_getenv[] = {{lvm_getenv}, {lvm_ret0}},
 nif_getpid[] = {{lvm_getpid}, {lvm_ret0}};
// Register in the ai_nifs section (drained in main below). An app thread adds its
// own nifs the same way in its OWN host/<app>.c -- auto-globbed, AiNif-registered,
// NO edit here or to love.c/love.h:
//   #include "love.h"                                       // the nif-writing surface
//   static lvm(lvm_foo) { ... ai_musttail return Answer(<v>); }
//   static union u const nif_foo[] = {{lvm_foo}, {lvm_ret0}};  // 1-arg; curry for more
//   AiNif("foo", nif_foo);
AiNif("quit", nif_exit);
AiNif("open", nif_open);
AiNif("close", nif_close);
AiNif("hark", nif_hark);
AiNif("herald", nif_herald);
AiNif("exec", nif_exec);
AiNif("getenv", nif_getenv);
AiNif("getpid", nif_getpid);

// --- the boot script ---------------------------------------------------
// Everything the two builds disagree about lives in this ONE conditional
// region: the baked lisp text plus a `boot` entry that main tail-calls after
// the universal setup (argv pins + the host nif defs above).
// LOVE_BUDGET_MB: cap the whole GC footprint (2*minor + 2*major) at N megabytes -- the runtime
// face of the ai_budget tunable (the field is set-at-runtime by design). The BENCH use: pin
// the pool so an A/B compares identical GC schedules -- the resize controller can't wander
// across a pool boundary between the two sides (the pool-cliff contamination class). Applied
// to the LIVE g in main, after boot or image wake, so both boot paths honor it.
static struct ai *env_budget(struct ai *g) {
  char const *b = getenv("LOVE_BUDGET_MB");
  if (g && b && atol(b) > 0) { g->budget = (uintptr_t) atol(b) * (1024 * 1024 / sizeof(ai_word)); return g; }
  // the DEFAULT is half the machine, not infinity: an unbounded resize
  // controller on a small swapless box asks the kernel past what it will
  // overcommit, and the refusal is a bare failed op. env wins above; a device
  // pins -Dai_budget; 0 stays unbounded only where the machine cannot say its
  // size.
  if (g && !g->budget) {
    // raw read + hand parse, no stdio: nolibc's fscanf speaks no width and no
    // %lu, and the default must fire in both libcs. MemTotal leads the file;
    // the first digit run is the kB count. ⚠ NOT gated on a kernel: the open
    // fails where there is no procfs, which leaves the budget unbounded --
    // exactly what naming the kernel bought, and it asks the box instead.
    int fd = open("/proc/meminfo", O_RDONLY);
    if (fd >= 0) { char mb[64]; long n = (long) read(fd, mb, sizeof mb - 1);
      close(fd);
      if (n > 8 && !memcmp(mb, "MemTotal", 8)) { mb[n] = 0;
        char *p = mb; while (*p && (*p < '0' || *p > '9')) p++;
        uintptr_t kb = 0; while (*p >= '0' && *p <= '9') kb = kb * 10 + (uintptr_t)(*p++ - '0');
        g->budget = kb * 1024 / 2 / sizeof(ai_word); } }
  }
  return g; }

// bake [PATH] / wake PATH: the heap-image snapshot (doc/snapshot.md) -- declared
// ABOVE the bootstrap split, because love0 links host/image.c too now: it bakes
// image FILES (the `bake` nif) and wakes them (wake), which is how the self-host
// build gets a warm mooncc under love0. The .image-section self-patch stays the
// full binary's lane.
extern int image_dump(struct ai*, char const*);          // host/image.c (file I/O around love.c's codec)
extern int image_bake(struct ai*);                       // host/image.c (the self-bake)
extern int image_bake_layers(struct ai*, void *const *, uintptr_t const *, char *const *, int);  // ..and the layered array
extern int image_freeze(struct ai*, void**, uintptr_t*);                                         // ..whose two codec doors
extern int image_save_over(struct ai*, void *const *, uintptr_t const *, uintptr_t,
                           void**, uintptr_t*, void**, uintptr_t*);                              // ..wear the wake guard there
extern int ai_baked_pick(char const*, void const**, uintptr_t*, void const**, uintptr_t*);       // which entry this command line wants
extern struct ai *image_load(char const*);
extern uint64_t ai_baked_image[];
extern uintptr_t ai_baked_image_len;

#ifdef LoveBoot
// love0: the CLI driver is the sed-wrapped raw text (it can't lcat its own arg
// ap). Self-test: the whole test corpus, baked in (sed-wrapped), run
// twice -- once compiled by the C bootstrap compiler (c0), once by the
// self-hosted ev installed from ev.l -- so one love0 invocation exercises both
// compilers (and -Dai_tco=0 makes it the trampoline path). s2cldef installs
// s2cl (string -> charlist); runner drinks the baked corpus (the global
// `tests`) through reads (the shell core, love/bao.l), whose `(ev 'ev r)` indirection
// late-binds to whatever `ev` is now, so the same shell drives the c0 pass and
// (after the egg) the self-hosted pass.
static char const cli[] =
#include "cli0.h"
 , runner[] = "(reads (tap (s2cl tests)))"   // the stream shell (love/bao.l) drinks the corpus
 , src0_bao[] =
#include "bao0.h"
 , src0_rng[] =
#include "rng0.h"
 , src0_kanren[] =
#include "kanren0.h"
 , src0_pat[] =
#include "pat0.h"
 , src0_uu[] =
#include "uu0.h"
 , src0_coin[] =
#include "coin0.h"
 , src0_q[] =
#include "q0.h"
 , src0_peg[] =
#include "peg0.h"
 , src0_overlay[] =
#include "overlay0.h"
 , src0_holo[] =
#include "holo0.h"
#include "x640.h"
#include "arm640.h"
 , src0_verbs[] =
#include "verbs0.h"
 ;

// With args, run the build tool (lcat / gen_data) through the CLI driver.
// With no args, self-test: eval prel, load bao (the shell core) as a module, and run
// the baked corpus via c0, then bootstrap the self-hosted ev (egg) and run the corpus
// again through it.
// the source library: both lanes load bao by name, and BOTH get the whole table -- a
// build tool's (use 'x) (the mooncc cat's (use 'holo)) resolves the same as the
// self-test's. overlay and peg are listed, never used here: each consumer opens with
// its own (use ..), the boot owes nothing. an unlisted-for entry costs a row, nothing more.
static struct ai_lib const libs0[] = {
  {"bao", src0_bao}, {"rng", src0_rng}, {"kanren", src0_kanren}, {"pat", src0_pat}, {"uu", src0_uu},
  {"coin", src0_coin}, {"q", src0_q}, {"overlay", src0_overlay}, {"peg", src0_peg},
  {"holo", src0_holo},                                 // which the mooncc cat's cpp/gen read (the self-host build lane)
  {"verbs", src0_verbs},                               // the verb registry: love0 runs the same cli.l rail
  {NULL, NULL} };
struct ai_lib const *ai_libs(void) { return libs0; }

static struct ai *boot(struct ai *g, bool argp) {
  if (argp) {                                        // a build tool (lcat etc.): bake prel + bao FIRST so the CLI's
    g = ai_evals_(g,                                   // own loader/printer (eval1/bye reach for map/jot/tap/puts/putc)
#include "p10.h"                                       // have the prel surface before they load the first file -- else
    );                                                 // loading prel.l ITSELF misses every prel fn its loader uses.
    g = ai_evals_(g,                                   // ⚠ ITS OWN CALL: readtext picks its reader ONCE per text, and
#include "prel0.h"                                     // p1 seals hook 0 only when the call above EVALUATES
    " "
#include "pat0.h"                                      // ⚠ pat RIDES THE POST TEXT: post is written in @, and a macro
    " "                                                //   reaches a reader only once it is in the book. the `use`
#include "post0.h"                                     //   below still registers the MODULE, for cli and uu
        
    "(use 'bao)"                                       // p1 goes FIRST: this lane never hatches an egg, and prel's
    "(use 'kanren)"                                    // loader folds `sound` at its own compile; kanren splices
                                                       //   because the corpus reads unify/ufail bare
    "(use 'pat)"                                       // ⚠ pat BEFORE cli (cli.l is written in @, and a macro
                                                       //   reaches a reader only once its layer is spliced) and
                                                       //   before verbs: the unsplice below pops the LAST splice,
                                                       //   which has to stay verbs
    "(use 'verbs)"                                     // the verb registry the cli rail walks -- registered, then
    );                                                 //   unspliced below: this lane runs the SAME cli.l
    g = ai_unsplice_(g);
    return ai_evals_(g, cli); }
  g = ai_evals_(g,                                    // p1 FIRST: prel's loader reads `sound`, and a
#include "p10.h"                                      // global folds at its reader's compile, so the
  );                                                  // reader in love has to exist before prel compiles
  g = ai_evals_(g,
#include "prel0.h"                                    // prel, read by p1 now that hook 0 is sealed
    " "
#include "pat0.h"                                     // ⚠ pat RIDES THE POST TEXT (see the argp lane above)
    " "
#include "post0.h"                                    // ..and the printer, which pass 1 below already needs
  );
  g = ai_evals_(g,
    "(use 'bao)"                                       // bao (the shell core): loaded, registered, spliced
    "(use 'holo)");                                    // the assembler service: load + register..
  g = ai_unsplice_(g);                                 //   ..and the C unsplice keeps it non-ambient, like the host
  g = ai_evals_(g,
    "(use 'pat)"                                       // ⚠ pat FIRST: uu.l is written in @, and a macro reaches a
    "(use 'uu) (: uu (from 'uu))"                      //   reader only once its layer is spliced. then the library
                                                       //   layers, all by name in the old eval order (uu's
    "(use 'coin)"                                      //   one-name surface rebinds like the host); every layer, splice
    "(use 'rng)"                                       //   and registry entry persists across the egg warm below, so one
    "(use 'q)"                                         //   load serves both corpus passes
    "(use 'kanren)"
  );
  g = ai_evals_(g, "(: (s2cl s) ((: (g i) (? (< i (tally s)) (link (peep s i 0) (g (+ 1 i))))) 0))");   // string -> charlist, for the runner
  // THE CORPUS IS READ, NOT BAKED. It used to ride as a C string through out/lib/tests0.h, which
  // put every test file in love0's dependency graph: editing one relinked the bootstrap and
  // rebuilt every object behind it, ~100 s for a one-line assert. Nothing needed it -- the nifs
  // are drained in main() before boot() runs, so file io is live here, and out/lib/corpus.list
  // already holds the ordered set (it exists because make cannot watch a wildcard's MEMBERSHIP).
  // ⚠ love0 IS NOT A RELEASE ARTIFACT (host/build.mk stamps it "bootstrap" for the same reason),
  // and the one thing that runs it is test_love0, from the tree that just built it.
  // ⚠ A MISSING FILE DIES BY NAME. Answering () would run a SHORTER corpus and still print
  // "tests pass" -- a green gate over tests that never ran, which is the one failure this must
  // not have. presence is the wrapper, so the read tests the OPEN and never the byte count.
  g = ai_evals_(g,
    "(: (c0read p) (: q (open p \"r\")"
    "               (? q (: s (slurp q) _ (close q) s)"
    "                  (: _ (say err (\"love0: corpus: cannot open \" + p)) _ (put err 10) (quit 1))))"
    "   (c0split s) (: n (tally s)"
    "                  (go i j acc) (? (n <= i) (rev (? (< j i) (link (snip s j i) acc) acc))"
    "                                 (: c (peep s i 0)"
    "                                    (? (|| (= c 32) (= c 10))"
    "                                       (go (+ i 1) (+ i 1) (? (< j i) (link (snip s j i) acc) acc))"
    "                                       (go (+ i 1) j acc))))"
    "                  (go 0 0 ()))"
    "   fs (c0split (c0read \"out/lib/corpus.list\"))"
    "   _ (? (two? fs) 0 (: _ (say err \"love0: corpus: out/lib/corpus.list names nothing\")"
    "                       _ (put err 10) (quit 1)))"
    "   tests (foldl (\\ a f (a + c0read f)) \"\" fs))");
  g = ai_evals_(g, runner);                           // pass 1: corpus via ev = the c0 nif
  g = ai_egg_(g,                                      // bootstrap: install the self-hosted ev
#include "egg0.h"
    ,
#include "p10.h"
    ,
#include "prel0.h"
    " "
#include "ev0.h"
    ,
#include "pat0.h"                                     // ⚠ pat rides the post text here too: the egg compiles post
    " "                                               //   after the hatch and before the mop, and @ must be in hand
#include "post0.h"
);
  return ai_evals_(g, runner); }                      // pass 2: corpus via the self-hosted ev

#else
// the full love: raw terminal mode for the interactive REPL (love0 never needs
// it -- a build tool / self-test is non-interactive); the CLI driver is the
// canonicalized lcat header.
#if defined(__x86_64__) || defined(__aarch64__)
#define AiGlazed 1                                      // the native JIT exists on this arch
#endif
// the tty is ONE terminal, so its cooked baseline and its atexit live in one
// place -- posix.c's, which the (raw on) nif already drives. this is the same
// call, and the capture-once latch there is what makes a repl that raws after
// bao already did restore the true baseline rather than a raw one.
extern int ai_raw_mode(intptr_t on);
#define raw_mode() ((void) ai_raw_mode(1))

static char const cli[] =
#include "cli.h"
 ;

// `bake` boots fully, then lays the post-warm image back into the binary's OWN
// .image section (host/image.c's copy + patch + atomic-rename -- no objcopy,
// ETXTBSY-proof) and exits; `bake PATH` writes a plain image file instead (the
// debug/inspection lane). `wake PATH` boots from an image file (any mismatch
// falls back to a normal egg boot). Opt-in flags; a normal run is the same code path.
// The baked post-boot image: a reserve in its own .image section (host/image.c), filled by
// `love bake` (the binary boots, snapshots itself, and lays the result back into its own body).
// Loaded at startup when its magic validates; else a normal egg boot.
// the post-warm dispatch (shared by boot() and the wake path, which skips the warm).
static struct ai *run_program(struct ai *g, bool argp, bool replp) {
  // THE SESSION LAYER. Boot is over; from here the base (orth -- prel/ev, the nifs,
  // every module the frontend warmed) is READ-ONLY, and it is read-only for the
  // plainest possible reason: it is never the HEAD again. lvm_defglob writes
  // A(g->book) and nothing else, so a top-level definition -- a script's, a repl
  // line's, the corpus's -- lands here instead of in the base. Reads still walk
  // down (bookget, head-first), so prel resolves exactly as before.
  //
  // Pushed here because this is where boot() and the wake path converge, so both
  // get it; and it is never popped, because its lifetime IS the session. That is
  // what keeps a CATTED app working: lux's eight files, the kore cat's fifteen and
  // the whole test corpus each arrive as ONE stream, so they share this layer and
  // the cross-file leaking they are built on (crew/lux/core.l's "every binding
  // LEAKS ... so the other files see this vocabulary") still resolves.
  //
  // bake exits before run_program, so the image carries the base with no session
  // layer on top; each woken session pushes its own. C-side: enter is a mopped nom
  // now, and a stashless layer is exactly what a session is.
  g = ai_layer_(g);
#ifdef AiGlazed
  // LOVE_NO_GLAZE: a pure-interpreter session -- ev back to base-ev (kept in the glaze
  // module book) and the natjit creation hook cleared. The forensics twin of LOVE_NO_IMAGE.
  // Checked here, the convergence of the egg-boot and image-wake paths: a body-less
  // top-level : pins even where the book nom is sealed away (an image). bake never
  // sees it -- the knob governs a session, not the baked artifact.
  if (getenv("LOVE_NO_GLAZE")) g = ai_evals_(g, "(: ev (from 'glaze 'base-ev) natjit ())");
#endif
  // the ARGV[0] DOOR of the verb rail (love/cli.l has the positional door): when the
  // binary was invoked under a verb's name -- a `seed` symlink onto the dist artifact
  // -- that verb fires on the args, even at argc 1 (a bare `seed` wants its usage),
  // which is exactly where the cli never runs. the basename decides, so no shadow rule
  // applies here; the whole walk lives in the module (love/verbs.l's `seat`) and this is
  // the call. ⚠ tablet?, never a bare truth test -- an unregistered module reads () and
  // (() 'seat) is the church const 1, which would answer argv[0] itself and dispatch it.
  // a verb ANSWERS a status charm and we quit with it; one that quits internally never
  // returns here. that is kore's convention, and it is why `love kore sed ..` nests.
  g = ai_evals_(g,
    "(: V (from 'verbs)"
    "   f (? (tablet? V) (V 'seat (cap cmdline)) ())"
    "   (? f (: r (f (cup cmdline)) (quit (? (charm? r) r 0))) 0))");
  if (argp) return ai_evals_(g, cli);
  if (!replp) return ai_evals_(g, "(reads in)");         // non-tty stdin: the stream shell (love/bao.l) drinks the in port
  return ai_evals_(g, "((from 'bao 'bao) 0)"); }                      // a tty: bao (the baked shell core) is DEFINE-ONLY -- installs
                                                         //   (bao _)/shell/... but never launches, so one image serves a
                                                         //   pipe and the self-test too; the frontend fires it here.

// the MODULE sources, name-keyed (the love0 twins above): registered in the source
// library and loaded by `use` -- one layer per load, leave registers, the splice
// serves the bare names. The lib entries ride the image too, so a woken session
// keeps the same registry.
static char const src_coin[] =
#include "coin.h"
 ;
static char const src_rng[] =
#include "rng.h"
 ;
static char const src_q[] =
#include "q.h"
 ;
static char const src_kanren[] =
#include "kanren.h"
 ;
static char const src_peg[] =
#include "peg.h"
 ;
static char const src_overlay[] =
#include "overlay.h"
 ;
static char const src_pat[] =
#include "pat.h"
 ;
static char const src_uu[] =
#include "uu.h"
 ;
static char const src_bao[] =
#include "bao.h"
 ;
static char const src_verbs[] =
#include "verbs.h"
 ;
// holo, the crew/holo/ assembler: ONE entry = the arch-neutral core plus the NATIVE
// backend (C string concatenation; the glaze emits for the running arch only --
// mooncc's cat joins the cross backends at its own build, and love0 bakes x64+arm64
// so the corpus's cross-arch asserts run under both its compilers).
// The LINKER half rides the same entry, in load order: elf.l wraps assembled bytes in an
// executable, obj.l lays a relocatable .o, link.l links a set. ⚠ they read holo's internals
// BARE (catall, le*, lay, laylax, patch-with), which is why they sit INSIDE the module text
// rather than loading over it -- one layer, so there is no splice to arrange. The cats keep
// their own copies: love0's holo carries no linker, and it bakes mooncc0.image.
static char const src_holo[] =
#include "holo.h"
#if defined(__x86_64__)
#include "x64.h"
#elif defined(__aarch64__)
#include "arm64.h"
#elif defined(__riscv)
#include "riscv.h"
#endif
#include "holo-elf.h"
#include "holo-obj.h"
#include "holo-link.h"
 ;

#ifdef AiGlazed
// the glaze, ONE module in two files: emit.l (the SSE/native emitter) then auto.l (ev's
// source recognizer), which reads emit's names bare -- so the order here is the module.
// hook.l is deliberately not in it; see the (use 'glaze) block in boot().
static char const src_glaze[] =
#include "emit.h"
#include "auto.h"
 ;
#endif

// the source library (love.h): .rodata, so an entry nothing loads costs a row and no
// heap at all -- overlay and peg are here for consumers that open with their own (use ..).
static struct ai_lib const libs[] = {
  {"coin", src_coin}, {"rng", src_rng}, {"q", src_q}, {"kanren", src_kanren},
  {"overlay", src_overlay}, {"peg", src_peg}, {"pat", src_pat}, {"uu", src_uu}, {"bao", src_bao},
  {"holo", src_holo}, {"verbs", src_verbs},
#ifdef AiGlazed
  {"glaze", src_glaze},
#endif
  {NULL, NULL} };
struct ai_lib const *ai_libs(void) { return libs; }

// read-eval one .l file into the booting session, loudly: a bake's cat has no shell help,
// so a raise in it must end the bake rather than seal a half-built artifact.
// the path is a value, never spliced into the source: bound as a name the text stays data
// whatever it holds, and the eval'd form is a constant.
// it closes q, because an open heap port registers a finalizer and would still be
// reachable at the seal -- carrying its fd into the image.
// the name is rebound to (), not pulled: the seal ends with (pull book 'book 0), so the
// book is already off the book here and naming it answers `;; missing book`. either way
// the name must stop holding the path, or an absolute one bakes the baker's directory in.
static struct ai *bake_eval_file(struct ai *g, char const *path) {
  uintptr_t xn = strlen(path);
  if (!ai_ok(g = str0(g, xn))) return g;
  if (xn) memcpy(txt(g->sp[0]), path, xn);
  g = ai_defv(g, "bake-load");
  if (!ai_ok(g)) return g;
  ai_core_of(g)->sp++;
  g = ai_evals_(g,
    "(: q (open bake-load \"r\")"
    " (? q (: _ (reads q) (close q))"
    "      (: _ (say err (\"love: bake: cannot open \" + bake-load)) _ (put err 10) (quit 1))))");
  return ai_ok(g) ? ai_evals_(g, "(: bake-load ())") : g; }

// auto.l's self-tests fill the `memo` compile cache with native nif closures whose ap is a
// W^X mmap address, and those cannot serialize. empty it, so the image boots with a clean
// cache and natives re-JIT lazily on the woken runtime's first ev.
static struct ai *bake_empty_glaze(struct ai *g) {
#ifdef AiGlazed
  return ai_evals_(g, "(: c (from 'glaze 'cache) (map (\\ k (pull c k 0)) (keys c)))");
#else
  return g;
#endif
}

// the layered bake. each spec is `CAT` or `CAT:verb,verb`; the cats are evaluated in order
// into one session and every layer but the last is frozen after its cat, pinning its words
// at their offsets so the final blob begins with each in turn. the last dump is the only
// one carrying a token stream; the rest ride as derived records.
// a derive that does not fit fails the bake loudly: it means a frozen prefix did not
// survive as one, which is the assumption the whole shape rests on.
static int bake_layers(struct ai *g, char const *const *spec, int n) {
  void *rec[8], *sub[8], *full = NULL;
  uintptr_t reclen[8], sublen[8], fulllen = 0;
  char *verbs[8];
  char path[8][4096];
  int i, rc = -6;
  for (i = 0; i < n; i++) {                              // split each spec, then eval its cat
    char const *c = strrchr(spec[i], ':');
    size_t pl = c && !strchr(c, '/') ? (size_t)(c - spec[i]) : strlen(spec[i]);
    if (pl >= sizeof path[0]) return -6;
    memcpy(path[i], spec[i], pl), path[i][pl] = 0;
    verbs[i] = c && !strchr(c, '/') ? (char*) c + 1 : NULL;
    if (!ai_ok(g = bake_eval_file(g, path[i]))) return -2;
    g = bake_empty_glaze(g);                             // ..before every dump, not just the last
    if (i + 1 < n && (rc = image_freeze(g, &rec[i], &reclen[i]))) return rc; }
  if ((rc = image_save_over(g, rec, reclen, (uintptr_t)(n - 1), sub, sublen, &full, &fulllen)))
    return rc;
  for (i = 0; i + 1 < n; i++) if (!sub[i]) {
    fprintf(stderr, "love: bake -L: layer %d (%s) did not survive as a prefix\n", i, path[i]);
    return -3; }
  sub[n - 1] = full, sublen[n - 1] = fulllen;
  for (i = 0; i + 1 < n; i++)                            // what each derived layer costs against its own words
    fprintf(stderr, "  bake -L: layer %d %s: %lu KB of prefix -> %lu B derived\n",
            i, path[i], (unsigned long)(reclen[i] >> 10), (unsigned long) sublen[i]);
  fprintf(stderr, "  bake -L: layer %d %s: %lu KB whole\n",
          n - 1, path[n - 1], (unsigned long)(fulllen >> 10));
  rc = image_bake_layers(g, sub, sublen, verbs, n);
  for (i = 0; i + 1 < n; i++) g->alloc(g, rec[i], 0), g->alloc(g, sub[i], 0);
  g->alloc(g, full, 0);
  return rc;
}

// bake: NULL = no snapshot; "" = `love bake` (patch the binary's own .image); else `bake PATH` (an image file).
static struct ai *boot(struct ai *g, bool argp, char const *bake, char const *bake_load,
                       char const *const *layer, int nlayer) {
  bool replp = !argp && isatty(STDIN_FILENO);
  if (replp) raw_mode();
  // THE DEBUG DOOR: LOVE_NO_MOP keeps the compiler's internals on the book (peek/poke/
  // seek/feels/dis and the raw cell nifs) for introspection -- egg.l skips the birth mop
  // when `nomop` is set. reaches only a FRESH egg warm, so pair it with LOVE_NO_IMAGE
  // (a baked image is already swept): `LOVE_NO_MOP=1 LOVE_NO_IMAGE=1 love`. off by
  // default, so the shipped surface and every gate stay swept.
  { char const *nm = getenv("LOVE_NO_MOP");
    if (nm && *nm) g = ai_evals_(g, "(: nomop 1)"); }
  g = ai_egg_(g,
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    " "
#include "ev.h"
    ,
#include "pat.h"                                        // ⚠ pat RIDES THE POST TEXT: post is written in @, and the egg
    " "                                                 //   is the FIRST thing this lane runs -- there is no seam to
#include "post.h"                                       //   splice a layer into. (use 'pat) below still registers it.
    );
  g = ai_evals_(g,
    "(use 'coin)"                                        // the library layers, ALL modules now, in the old eval order: coin
    "(use 'rng)"                                         //   (ring/monoid over the C coin lane), rng (the random stream), q
    "(use 'q)"                                           //   (rationals), then kanren (unification) -- registered BEFORE
    "(use 'kanren)"                                      //   overlay, whose engine reads subst through the registry
    "(use 'overlay)"                                     // overlay for the ev seam, whose HOOK lands in ORTH -- a module
    "(: overlay (from 'overlay)"                         //   layer cannot write it, the boot can. peg is registered above
    "   ev ((from 'overlay 'ov-hook) ev))"               //   and used by its consumers, not here.
    "(use 'pat)"                                         // ⚠ pat BEFORE uu: uu.l is written in @, and a macro reaches
    "(use 'uu)"                                          //   a reader only once its layer is spliced
                                                         // uu's NbE kernel: (: uu (from 'uu)) keeps the one-name surface --
    "(: uu (from 'uu))"                                  //   the corpus + an overlay reach (uu 'vof) through it
    "(use 'holo)"                                        // the crew/holo/ assembler, a post-egg language SERVICE: load + register,
  );                                                     //   then the C unsplice below keeps it NON-AMBIENT -- (use 'holo)
  g = ai_unsplice_(g);                                   //   splices it, (from 'holo 'assemble) probes it. a test that wants a
                                                         //   cross backend joins it at runtime ((use 'holo) <backend.l> -- the
                                                         //   test_glaze/test_raw_arm64 recipes), mooncc's cat joins ALL of them
  g = ai_evals_(g,
    "(use 'bao)"                                         // the shell core: loaded, registered, spliced (read/reads/welp/wrap bare)
    "(use 'verbs)"                                       // the verb registry, holo's shape: registered, then NON-AMBIENT below --
  );                                                     //   tab/word/seat are not names to reach bare, and `get` would shadow half
  g = ai_unsplice_(g);                                   //   the tree. a plain binary carries wake+bake and nothing else; a dist
                                                         //   bake's cat pins the rest into (from 'verbs 'tab)
#ifdef AiGlazed
  // the glaze, in three moves. (use 'glaze) loads emit.l + auto.l into their own layer and
  // registers it -- ~415 codegen names the book never sees. holo is spliced UNDER that layer
  // so `assemble` folds at the glaze's compile, and both come off after.
  g = ai_evals_(g, "(use 'holo)" "(use 'glaze)");
  g = ai_unsplice_(g);                                   // the glaze layer: registered, non-ambient
  // then ORTH's two names, which a module layer cannot write and the boot can: `ev` becomes
  // auto-native, `member?` its glazed self. Both carry their own re-load/trampoline gates.
  // Last the ala creation hook (love/glaze/hook.l), which is NOT in the module -- it leaks
  // natjit/fires/fired?/bake onto the book on purpose, and test_glaze re-cats it standalone.
  g = ai_evals_(g,
      "(: ev (from 'glaze 'ev) member? (from 'glaze 'member?))"
#include "hook.h"
      );
  g = ai_unsplice_(g);                                   // holo back to non-ambient
#endif

  // THE SEAL, and it runs on EVERY boot -- an egg boot and a woken image must differ in
  // startup time and NOTHING else. `book` goes so a program cannot reassign the globals
  // under everyone (the same reason a module book hands out a lookup closure, not its
  // tablet), and nif/nifx go because they install raw bytes as executable code: the glaze
  // folded them into its closures at the load above, and keeps them as module members
  // (love/glaze/emit.l), so (from 'glaze 'nif) is the one door left onto that.
  // ⚠ AND `born` COMES OFF WHEN BAKING, because it is the one thing above that a
  // snapshot must not carry: it is the HATCH DURATION (egg.l's (clock now)), so a
  // baked one would freeze one machine's ~220 ms into every future wake and report it
  // as this run's. It is also the last content in the image that varies run to run,
  // which is what makes a bake reproducible at all. The waker re-pins it below, at
  // its own cost -- per invocation, which is the only reading that was ever true.
  g = ai_evals_(g, bake
    ? "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) _ (pull book 'born 0) (pull book 'book 0))"
    : "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) (pull book 'book 0))");

  if (bake) {                                            // the bake verb: snapshot the post-warm heap, then exit
    // `bake -l FILE`: read-eval one more .l file before the snapshot -- the dist
    // artifact's door (crew/build.mk): the crew cats + the verb table go in WARM,
    // ahead of the same cache-empty + seal every bake gets, and the image still
    // carries no session layer. a raise in the cat finds nothing heard here (no shell
    // help), so a broken cat is a LOUD failed bake, never a quiet artifact.
    // ⚠ it was an ENVIRONMENT VARIABLE, and this tree spends exactly one of those
    // (HOME). An argument is visible in the command that ran, survives being read
    // back out of a log, and cannot be inherited by something that never asked.
    if (bake_load && !ai_ok(g = bake_eval_file(g, bake_load))) return g;
    if (nlayer) {                                        // THE LAYERED BAKE: see image_bake_layers
      int rc = bake_layers(g, layer, nlayer);
      if (rc) fprintf(stderr, "love: bake -L failed (rc=%d)\n", rc);
      exit(rc ? 1 : 0); }
    g = bake_empty_glaze(g);
    int rc = *bake ? image_dump(g, bake) : image_bake(g);
    if (rc) fprintf(stderr, "love: bake failed (rc=%d)\n", rc);
    exit(rc ? 1 : 0); }
  return run_program(g, argp, replp); }
#endif

// Marshal a word list onto the stack as ONE chain, left on top. `skip` drops that many
// words AFTER argv[0] -- the prime verb's own, which belong to the command line and not
// to the program. Each string is pushed before any is consed, so the ones already there
// are rooted on the stack through every ai_strof that might collect.
ai_noinline static struct ai *argv_chain(struct ai *g, char const **v, int argc, int skip) {
  int n = 0;
  if (argc > 0) g = ai_strof(g, v[0]), n++;                  // argv[0] is always the program
  for (int i = 1 + skip; i < argc; i++) g = ai_strof(g, v[i]), n++;
  for (g = ai_push(g, 1, ai_zero); n--; g = gxr(g));
  return g; }

int main(int argc, char const **argv) {
  signal(SIGPIPE, SIG_IGN);        // a hung-up peer is an ANSWER, not a death (fd_writen)
  struct ai *g = NULL;
  // THE PRIME VERBS. `bake [PATH]` / `wake PATH` must LEAD the command line, and by
  // physics rather than habit: wake decides which heap there is (image_load precedes
  // ai_ini), and bake must snapshot before run_program pushes the session layer. So they
  // are read here, in C, before any love exists to read them -- which is exactly why they
  // are the two the registry cannot own. love/verbs.l holds their rows anyway, so one
  // manifest answers for help and the shadow rule and a MISPLACED one is an honest error
  // instead of falling through to "run the file named bake".
  // Bare words, not flags: a verb is a verb. The escape hatches are the registry's own --
  // `love ./bake` and `love -- bake` are the file, since neither is this strcmp.
  // Both lanes now: love0 links host/image.c too, so it wakes an image FILE
  // (its own mooncc0.image bake -- the self-host build's ~ms compiler starts);
  // `bake` (the self-patch) stays host-only (love0 lays no .image section rule,
  // and its file bakes ride the `bake` nif from -e).
  // `skip` counts the words that are the PRIME's and not the program's. It is a count
  // and not a shift because the shift used to CLOBBER (argv[2] = argv[0]) -- which is
  // how the line came to be a command line nobody typed. Nothing is written now, so
  // both readings stay available: the whole invocation, and the program's view of it.
  char const *image_load_path = NULL, *bake = NULL;  // see boot(): "" = self-bake, a path = image file
#ifndef LoveBoot
  char const *bake_load = NULL;                     // bake -l CAT: read-eval it before the seal
  char const *layer[8];                             // bake -L: the chain, smallest first
  int nlayer = 0;
#endif
  int skip = 0;

#ifndef LoveBoot
  if (argc >= 2 && !strcmp(argv[1], "bake")) {
   int i = 2;                                      // bake [-l CAT | -L CAT:verbs ..] [PATH]
   // `bake -L CAT[:verbs] ..` is the layered bake (doc/plan/image-chain.md): one session,
   // the cats evaluated in inclusion order, frozen between, and the array laid into our
   // own section. every -L but the last rides as a derived record.
   while (i + 1 < argc && !strcmp(argv[i], "-L") && nlayer < (int) countof(layer))
    layer[nlayer++] = argv[i + 1], i += 2;
   if (!nlayer && i + 1 < argc && !strcmp(argv[i], "-l")) bake_load = argv[i + 1], i += 2;
   bake = i < argc ? argv[i] : "";
   // a loud refusal: the layered bake always patches this binary's own section, so a
   // leftover word is either a layer past the cap or a path that would be ignored.
   if (nlayer && *bake)
    return fprintf(stderr, "love: bake -L takes up to %d layers and no output path (got `%s')\n",
                   (int) countof(layer), bake), 2;
   skip = (i < argc ? i + 1 : i) - 1; }
  else
#endif
  if (argc >= 3 && !strcmp(argv[1], "wake"))
   image_load_path = argv[2], skip = 2;
  // a LEADING wake with nothing to wake. Its arity error is C's because its parse is:
  // the registry's row would say "must lead the command line", which is the one thing
  // this invocation got right.
  else if (argc == 2 && !strcmp(argv[1], "wake"))
   return fprintf(stderr, "love: wake needs an image path\n"), 2;
  if (image_load_path && !(g = image_load(image_load_path))) image_load_path = NULL;   // NULL -> normal boot
  // AUTO-LOAD: with no image flag, wake the image baked into the binary's own .image section, so a
  // plain `love` is glazed-by-default at ~4 ms cold start instead of the ~230 ms egg eval. Opt out with
  // LOVE_NO_IMAGE (the bench does, to control glazed-vs-interp itself). An EMPTY value is nothing
  // (unset) -- so a recipe under a caller's exported LOVE_NO_IMAGE can hand ONE
  // command its image back with the sh idiom `LOVE_NO_IMAGE= cmd` (the dist artifact running as
  // $(CC): its mooncc verb lives in the baked image, and an egg boot would read "mooncc" as a
  // filename). Any problem -- unbaked, stale, truncated -- makes the load return NULL, so we fall
  // through to the normal egg boot. Never wrong.
  // (love0's reserve is 2 words and never baked, so its auto-load always falls through.)
  char const *noimg = getenv("LOVE_NO_IMAGE");
  uintptr_t woke_ms = 0;                       // what the wake cost, for `born` below
  if (!g && !bake && !(noimg && *noimg)) {
   uintptr_t t0 = ai_clock();
   // ..and WHICH image: the section may carry an array, in which case the first
   // entry claiming this command's verb wins and the largest is the fallback. one
   // pass over a directory, before anything is woken -- it has to be, since the
   // verb table lives in the image we are choosing.
   uintptr_t blen = 0, slen = 0;
   void const *bimg = NULL, *bsub = NULL;
   if (ai_baked_pick(argc > 1 ? argv[1] : NULL, &bimg, &blen, &bsub, &slen)
       && (g = bsub ? ai_image_load_over(bimg, blen, bsub, slen) : ai_image_load(bimg, blen)))
    woke_ms = ai_clock() - t0,
    image_load_path = "<baked>"; }                                     // a loaded image is the booted state: skip the egg warm
  if (!g) g = ai_ini();
  g = env_budget(g);                               // the LOVE_BUDGET_MB cap, on whichever g won (fresh or woken image)
  bool argp = argc - skip > 1;
  // TWO chains, because there are two honest readings and they differ by the primes:
  //   cmdline  the WHOLE invocation, exactly as typed -- `wake IMAGE` included
  //   argv     the PROGRAM's view: argv[0], then the words past the prime
  // cli.l drops argv's head for its own use and rebinds argv again to the program's
  // own argv; cmdline is never rebound by anyone, which is what makes it the thing a
  // seat scan can read in any load order (love/verbs.l's `unprime` steps the primes).
  // ⚠ NEITHER IS PINNED UNDER A BAKE, and that absence is what makes a bare read safe.
  // A baked consumer folds its bare globals at its own compile, so a nom pinned while
  // the -l cat compiles would ride THIS line into every future wake. Unpinned it cannot
  // fold: the read stays the lookup it has to be, and every non-bake invocation pins
  // both before a line of love runs. The bake lane reads nothing -- love/verbs.l's
  // `fire` carries the one presence guard, so the app feet go quiet instead of scaring.
  if (!bake) {
    g = argv_chain(g, argv, argc, 0);               // cmdline, first: it ends up deeper
    g = argv_chain(g, argv, argc, skip); }          // argv, on top -- sp[0]
  if (ai_ok(g)) {
    // the static nifs (exit/open/close/run/getenv + any host/*.c app nifs) come
    // from the ai_nifs section -- immortal addresses, so the array door serves.
    // (This also re-pins them into a loaded image's book.)
    g = ai_defn(g, __start_ai_nifs, __stop_ai_nifs - __start_ai_nifs, 0);
    // ..and the MODULE tables (ai_mods, one ai_defn call per row): an app's nifs
    // land under its module, off the bare book. Same re-pin over a woken image --
    // the registry rides the image, so the drain finds the tablet and refreshes it.
    for (struct ai_mod const *mt = __start_ai_mods; mt < __stop_ai_mods; mt++)
      g = ai_defn(g, mt->defs, mt->n, mt->mod);
    // ⚠ NEITHER CHAIN LEAVES THE STACK. They are live heap values, so they cannot ride
    // a struct ai_def: C cannot re-root what it holds in an array, and the defn above
    // interns a hundred names -- a hundred chances to move them. ai_defv reads sp[0]
    // and leaves it, so each pop hands the next chain up.
    if (!bake) {
      g = ai_defv(g, "argv");
      if (ai_ok(g)) ai_core_of(g)->sp++;            // the book holds argv; the line is sp[0] now
      g = ai_defv(g, "cmdline");
      if (ai_ok(g)) ai_core_of(g)->sp++; }          // the book holds it now
    // `love-image`: WHICH image this session woke, by path -- here because here is
    // the only place it is knowable. The wake strips it from argv, so a consumer
    // keyed on its own compiler's identity (mooncc's runtime cache) can ask no
    // other way, and the alternative it settled for was every *.image beside the
    // binary, which makes a stranger's rebuild a miss. "<baked>" is the binary's
    // own .image section, whose identity is the binary's.
    // ⚠ PINNED ONLY WHEN THERE IS ONE, and absence is the answer for the rest: a
    // plain global is a pure global, and a baked consumer FOLDS its bare refs at
    // its own compile, so a nom pinned in the egg-booting bake session would ride
    // that session's value into the image forever. Unpinned, it cannot fold, and
    // the read stays the lookup it has to be. (`argv` and `cmdline` answer the same
    // trap the same way -- unpinned under a bake, see below.) Ask with
    // (member? 'love-image (names ())) -- presence out of band, never (lit? ..).
    if (image_load_path && ai_ok(g = ai_strof(g, image_load_path))) {
      g = ai_defv(g, "love-image");
      if (ai_ok(g)) ai_core_of(g)->sp++; }
    // `love-os`: WHICH KERNEL this invocation stands on -- "linux", "freebsd" or
    // "netbsd". A per-run fact like argv and NOT a build one: one binary answers
    // all three, so the compile that made it cannot say and only the run can. Our
    // libc probed it at entry (__ai_osv); a foreign one is built for one kernel
    // and its predefine is the whole answer.
    // ⚠ UNPINNED WHERE NOTHING CAN TELL, absence being the honest answer -- a
    // consumer that must know owes a diagnostic, never a guess at linux.
    // ⚠ AND UNPINNED UNDER A BAKE, for argv's reason above: a baked consumer would
    // fold the baking machine's kernel in and carry it onto every other.
    if (!bake) {
      char const *osn =
#if defined(AiNolibc)
        __ai_osv == 1 ? "linux" : __ai_osv == 2 ? "freebsd" : __ai_osv == 3 ? "netbsd" : 0;
#elif defined(__linux__)
        "linux";
#elif defined(__FreeBSD__)
        "freebsd";
#elif defined(__NetBSD__)
        "netbsd";
#else
        0;
#endif
      if (osn && ai_ok(g = ai_strof(g, osn))) {
        g = ai_defv(g, "love-os");
        if (ai_ok(g)) ai_core_of(g)->sp++; } }
    // `born` -- WHAT THIS INVOCATION COST TO START, in ms, and it belongs to the run and
    // not to the heap. The egg pins the HATCH duration (egg.l) and the bake pulls it back
    // off (the seal), so a woken love arrives without one and re-pins the WAKE duration
    // here. Both readings answer the same question; only the number differs, and the gap
    // between them is the whole point of baking (~220 ms hatched against ~4 ms woken).
    if (image_load_path && ai_ok(g = ai_push(g, 1, putcharm((intptr_t) woke_ms)))) {
      g = ai_defv(g, "born");
      if (ai_ok(g)) ai_core_of(g)->sp++; }
    // take what fd 0 can lend -- a read run, or its blocking bit (above). ⚠ NEVER UNDER
    // a bake: the image would carry a heap port, and a run's state belongs to the run, not the egg.
    if (!bake) g = stdin_take(g);
#ifdef LoveBoot
    if (!image_load_path) g = boot(g, argp);
    else g = ai_evals_(ai_layer_(g), cli);   // woken: the image carries the warm base; push the session layer, run the CLI
#else
    if (!image_load_path) g = boot(g, argp, bake, bake_load, layer, nlayer);
    else {              // wake: skip the egg warm, dispatch straight to the program
      bool replp = !argp && isatty(STDIN_FILENO);
      if (replp) raw_mode();
      g = run_program(g, argp, replp); }
#endif
  }
  if (ai_code_of(g) == ai_status_scare) ai_scare_face_(g);   // the honest face: ";; a b", or ";; oom@len=N" bare
  stdin_give(g);                                    // whoever shares this fd gets it back exact
  return ai_fin(g); }
