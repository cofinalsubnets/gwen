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
#include <sys/mman.h>    // the first boot's inflate buffer (mmap, no malloc)

// ai_clock lives in host/seat.c, one body for this frontend and the kernel's.
// the fine clock's real source (the weak default in love.c degrades to ms*1e6)
// FIXME this seems useless on 32 bit. who uses it? maybe squash back to just
// one ai_clock() and pick resolution at compile time based on INTPTR_MAX
ai_noinline intptr_t ai_nclock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? -1
       : (intptr_t) ts.tv_sec * 1000000000 + ts.tv_nsec; }


// --- fd 0, and the price of taking it ---
// a bare port reads one byte per readn (love.c's io_refill) and fd_readn pays 3 fcntl
// beside each one, so a 953 KB corpus down stdin cost 3.8M syscalls against a file
// argument's 23K. both halves of that are borrowings of the same fd, and what a door can
// lend decides which it gets:
//   seekable -- a heap bio parked in `inport`, which love.c's rbio_of reads through the
//               static, plus the seek back below. 3.8M -> 23K.
//   a pipe   -- the same run, plus the toggle: O_NONBLOCK on for the whole session, the
//               old flags in `inflag`, so the gulp costs no fcntl at all. it cannot be
//               seeked back, so its residue is delivered instead (stdin_hand).
//   a tty    -- neither. a human types, so syscalls-per-byte buys nothing, and a terminal
//               handed back nonblocking is the one version of this that breaks the
//               user's shell ("resource temporarily unavailable" on their next line).
//               with no run, `reads` keeps trickling -- which is what a prompt wants.
// one port, one position -- that is what makes the run safe. every in-process reader goes
// through zgetc, which drains the run before the device, so an in-form (slurp in) still
// sees exactly the bytes our reader has not taken. what runs ahead is only the kernel's fd
// offset, and only an inheritor can see that -- hence the seek. nothing puts a pipe back,
// so its residue is delivered rather than undone (stdin_hand), which is what lets this door
// hold a run at all. test_stdinbuf runs one program down each door and diffs, so a lane
// that starts running ahead without delivering fails there.
// two places hold unread bytes: the borrowed run, and `in`'s own pushback -- the ungetc
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
// `in` is not rebound -- it stays the static, and the run is borrowed behind it (love.c's
// rbio_of). rebinding would be unsound: bao's `reads` folded its own `in` at egg-compile
// time, so a fresh object fails its (id? p in) test and it would gulp the stream it means
// to trickle. neither borrowing is ever named in the book at all.
static struct ai *stdin_take(struct ai *g) {
 if (!ai_ok(g)) return g;
 if (lseek(STDIN_FILENO, 0, SEEK_CUR) < 0) {              // not seekable: a tty, or a pipe
  if (isatty(STDIN_FILENO)) return g;
  int fl = fcntl(STDIN_FILENO, F_GETFL);                  // a pipe: take the bit and the bytes
  if (fl >= 0 && ((fl & O_NONBLOCK) || fcntl(STDIN_FILENO, F_SETFL, fl | O_NONBLOCK) >= 0))
   ai_core_of(g)->inflag = putcharm(fl); }                // already-nonblocking restores to itself
 if (!ai_ok(g = ai_io_alloc(g, STDIN_FILENO))) return g;
 struct ai *fc = ai_core_of(g);
 fc->inport = fc->sp[0], fc->sp++;
 return g; }

// for (;;): the standard noreturn-defensive shape -- moon's stdnoreturn.h defines
// `noreturn` empty, so mooncc can't cut the fall-through tail itself; the loop
// leaves no ret for vmret to flag (gcc emits identical code either way).
// the kernel's task-aware twins (free/kmain.c) take these on a negative osv:
// quit is the seat/task door there -- seat-aware, machine-resetting unseated --
// and getpid the TASK pid, where this process's answers would be wrong. weak
// ghelp bodies so a kernel-less link closes; hosted never takes the branch.
__attribute__((weak)) lvm(k_lvm_quit) { ai_musttail return Ap(_lvm_ghelp, g); }
__attribute__((weak)) lvm(k_lvm_getpid) { ai_musttail return Ap(_lvm_ghelp, g); }

static lvm(lvm_exit) {
  if (__ai_osv < 0) ai_musttail return Ap(k_lvm_quit, g);
  for (;;) stdin_give(g), exit(getcharm(Sp[0])); }
// the wait cluster -- ai_sleep, ai_ready, ai_wait_fds, ai_ready_fds -- lives
// in host/seat.c, one definition for this frontend and the kernel's.

// the fd port -- ai_fd_port_vt, the three statics, ai_fd_drain, ai_fd_close --
// lives in host/seat.c, one definition for this frontend and the kernel's.

// --- handing fd 0 to a child: the unseekable half of stdin_give, up top ---
// a forked pumper writes the residue into a fresh pipe, splices whatever the old fd 0 still
// brings, and the read end becomes fd 0 -- one fork per handoff, only when a residue exists.
// only where a child takes fd 0. at our own exit nothing of ours is left to pump and the
// dup2 would be private to a process about to vanish, so lvm_exit and main's tail call
// stdin_give alone. a peer holding fd 0 from before us (`cat f | { love a.l; love b.l; }`)
// is out of reach on a pipe however we hand off -- the one thing this door cannot promise.
// stdin_give first: the pumper reads fd 0 itself, and an EAGAIN on the bit we borrowed
// would read there as an end and cut the stream short.
// the pushback byte leads -- `in`'s ungetc is the earlier of the two places holding unread
// bytes, so it goes in front of the run, as chug_str splits it.
// the pumper is a fork: it copies every fd love had open and nobody reaps it. narrow while
// exec is the only caller, but a live pipe love still held would keep a second writer on it.
extern uintptr_t ai_fd_write_all(int, unsigned char const*, uintptr_t);   // host/seat.c
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
  if (ai_fd_write_all(p[1], res, n) == n)
   for (;;) {
    unsigned char buf[ai_iobuf];
    ssize_t k = read(STDIN_FILENO, buf, sizeof buf);
    if (k < 0 && errno == EINTR) continue;                          // a signal is not an end
    if (k <= 0 || ai_fd_write_all(p[1], buf, (uintptr_t) k) < (uintptr_t) k) break; }
  _exit(0); }                                                       // _exit: no atexit, no flush, no love
 close(p[1]);
 if (p[0] != STDIN_FILENO) dup2(p[0], STDIN_FILENO), close(p[0]); }

// the open/close nifs live in host/posix.c now (plan C2): they are the posix
// surface's, and on inle their open(2)/close(2) land in free/sys.c's arms.

// --- subprocess (hark) + environment (getenv) ---------------------------
// both are host-only nifs (POSIX fork/exec/wait, getenv), like open/close.
// no malloc: argv is marshalled into the uncommitted l heap gap and the
// child's stdout is captured into a growing l string (the reader's
// str0 + grow + len-fixup pattern). see core/io.c ioread1str / grbufg.

// best-effort write-through for the tee mode below: loop over a partial write,
// but let a failed/closed stdout pass silently -- a broken pipe on the ECHO of a
// child's output must not fail the child, which ran fine.
static void host_teeout(char const *p, size_t n) {
 while (n) {
  ssize_t w = write(STDOUT_FILENO, p, n);
  if (w < 0) { if (errno == EINTR) continue; return; }
  p += w, n -= (size_t) w; } }

// (hark argv) / (herald argv) are a two-ap nif body -- {{start}, {drain}, {ret0}} --
// because the op is not re-runnable where it has to park. love.h's nif park says
// "leave Ip unadvanced and yield, the op re-runs", and a hark that re-ran from the
// top would fork a second child. so the fork and the capture are two ops, and the
// park lives in the second one, which re-runs as often as the child is slow.
//
// the whole park state is five stack slots, which the yield snapshots and the GC
// traces for free -- no C local survives a turn, and the capture string is free
// to move between them:
//
//    sp[0] out    the growing capture string -- or, when fd is -1, the whole answer
//    sp[1] n      bytes filled so far (a charm)
//    sp[2] fd     >= 0 draining | -2 drained, reaping | -1 done, out is the answer
//    sp[3] pid    the child (a charm)
//    sp[4] tee    0/1 -- argv's own slot, which argv is done with by then
//    sp[5]        the return ip lvm_ret0 wants
//
// `tee` picks when the captured output reaches stdout, not whether it is
// captured: 0 (hark) holds it until the child exits and hands the whole string
// back for the caller to do as it likes; 1 (herald) also write(2)s each chunk
// through as it is read, so a long-running child streams -- which is what make
// does (it pipes the child too, then relays each chunk rather than hoarding it).
// a capture-and-reprint caller passes 1 and skips its own reprint; a caller that
// consumes the text -- $(shell ..), a glob, an mtime probe -- passes 0. because
// the tee bypasses the l-level `out` buffer, a teeing caller must (flush out)
// first or its own echoed lines land after the child's bytes.
//
// &locals (pipes/pid/status) are fine in both helpers: they return normally,
// they are not VM-dispatch tail-call sites (cf. call_open vs lvm_open).

// lay the four state slots over argv, so every exit from the spawn -- a misuse,
// a failed pipe, a failed fork, a failed exec, a live child -- hands the drain
// one shape to read. the capture string (or the errno answer) goes on top after.
static struct ai *host_harkst(struct ai *g, intptr_t fd, intptr_t pid, int tee) {
 g = ai_push(g, 3, putcharm(0), putcharm(fd), putcharm(pid));
 if (ai_ok(g)) g->sp[3] = putcharm(tee);
 return g; }

// the first ap: marshal argv, fork, and confirm the exec. called with g Packed;
// argv is at sp[0]. returns a not-ok g only on oom.
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

 // reserve gap for cav (argc+1 pointers, word-aligned) + the byte blob.
 // written into the uncommitted region at Hp -- invisible to GC, holds no
 // l pointers, consumed before any further allocation. never bump Hp.
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

 // spawn: stdout pipe + a close-on-exec error pipe. on a successful exec the
 // kernel closes ep[1] -> parent reads EOF; on failure the child writes errno
 // -> parent distinguishes "couldn't spawn" from "ran and exited 127".
 // that handshake still blocks, and it is the one wait left here: it is
 // bounded by the child's exec(2), not by the child's life, which is the whole
 // difference this rung is about. (Every push below happens after the fork, so
 // growing the stack over the cav/blob gap is the parent's business alone.)
 int op[2], ep[2];
 // errno into a local before the state push, on every one of these: the push
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
  // detach stdin from the controlling terminal: this is a capture spawn (we want the child's
  // output, never interactive input), so give it /dev/null. otherwise a child that touches the
  // tty -- e.g. qemu `-serial stdio` doing tcsetattr -- gets SIGTTOU/SIGTTIN as a background
  // process and stops, producing no output (the `make test_kernel` hang under an interactive
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

 // the read end never blocks. it is a fresh fd the child does not share, so the
 // flag just stays on -- none of fd_readn's per-call toggle dance, which exists
 // for fds whose open file description a forked child holds too.
 { int fl = fcntl(op[0], F_GETFL); if (fl >= 0) fcntl(op[0], F_SETFL, fl | O_NONBLOCK); }
 return str0(host_harkst(g, op[0], pid, tee), 1u << 16); }  // capture -> sp[0]

// the second ap, once per scheduled turn: take what the pipe has (growing the
// string when it fills), tee it through if asked, then leave the state where the
// next turn finds it. nothing here holds a pointer across an allocation -- the
// capture string is re-read off sp[0] every time, because a park may have moved it.
ai_noinline static struct ai *host_harkdrain(struct ai *g) {
 intptr_t fd = getcharm(g->sp[2]);
 if (fd == -1) return g;                        // nothing was spawned: sp[0] is the answer
 pid_t pid = (pid_t) getcharm(g->sp[3]);
 if (fd >= 0) {
  int tee = getcharm(g->sp[4]) != 0;
  uintptr_t n = (uintptr_t) getcharm(g->sp[1]);
  for (;;) {
   uintptr_t lim = len(g->sp[0]);
   if (n == lim) {                                        // full -> double it and retry
    if (ai_ok(g = grbufg(g, lim))) continue;
    // oom mid-capture: close the pipe and kill the child rather than wait on
    // it. a bounded reap of a killed child is not the wait this rung deletes.
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
 // the reap is a poll, for the reason the `wait` nif is one: SIGCHLD is not in
 // the scheduler's wait set and a pid is not an fd. it almost always answers on
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

// (herald argv) -- hark, teeing: identical to (hark argv), same (status . output)
// answer, but the child's stdout is relayed as it arrives instead of only at
// exit. for a caller that just reprints what it captured; see the `tee` note above.
static lvm(lvm_herald) {
 Pack(g);
 g = host_harkstart(g, 1);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// the shared second ap. it parks -- Ip unadvanced, so the whole op re-runs on
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

// (exec argv) -> replace this process with argv[0], inheriting stdio (the real
// terminal). unlike (hark argv) -- which forks, pipes the child's stdout into a
// captured string, and waits -- exec hands the tty straight to the child, so an
// interactive program drives the terminal. on success it never returns; on a bad
// argv or a failed exec it returns an errno (or -1) fixnum, exactly like hark's
// spawn-failure path. cook execs its terminal recipe steps this way (the repl,
// gdb, the qemu run targets). marshals argv into cav like hark, then execvp
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

// copy the name to a C string and look it up. factored out (ai_noinline) so the
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
static lvm(lvm_getpid) {
  if (__ai_osv < 0) ai_musttail return Ap(k_lvm_getpid, g);
  ai_musttail return Answer(putcharm(getpid())); }

static union u const
 nif_exit[] = {{lvm_exit}, {lvm_ret0}},
 nif_hark[] = {{lvm_hark}, {lvm_harkdrain}, {lvm_ret0}},
 nif_herald[] = {{lvm_herald}, {lvm_harkdrain}, {lvm_ret0}},
 nif_exec[] = {{lvm_exec}, {lvm_ret0}},
 nif_getenv[] = {{lvm_getenv}, {lvm_ret0}},
 nif_getpid[] = {{lvm_getpid}, {lvm_ret0}};
// register in the ai_nifs section (drained in main below). an app thread adds its
// own nifs the same way in its own host/<app>.c -- auto-globbed, AiNif-registered,
// no edit here or to love.c/love.h:
//   #include "love.h"                                       // the nif-writing surface
//   static lvm(lvm_foo) { ... ai_musttail return Answer(<v>); }
//   static union u const nif_foo[] = {{lvm_foo}, {lvm_ret0}};  // 1-arg; curry for more
//   AiNif("foo", nif_foo);
AiNif("quit", nif_exit);
AiNif("hark", nif_hark);
AiNif("herald", nif_herald);
AiNif("exec", nif_exec);
AiNif("getenv", nif_getenv);
AiNif("getpid", nif_getpid);

// --- the boot script ---------------------------------------------------
// everything the two builds disagree about lives in this one conditional
// region: the baked lisp text plus a `boot` entry that main tail-calls after
// the universal setup (argv pins + the host nif defs above).
// LOVE_BUDGET_MB: cap the whole GC footprint (2*minor + 2*major) at N megabytes -- the runtime
// face of the ai_budget tunable (the field is set-at-runtime by design). the bench use: pin
// the pool so an A/B compares identical GC schedules -- the resize controller can't wander
// across a pool boundary between the two sides (the pool-cliff contamination class). applied
// to the live g in main, after boot or image wake, so both boot paths honor it.
static struct ai *env_budget(struct ai *g) {
  char const *b = getenv("LOVE_BUDGET_MB");
  if (g && b && atol(b) > 0) { g->budget = (uintptr_t) atol(b) * (1024 * 1024 / sizeof(ai_word)); return g; }
  // the default is half the machine, not infinity: an unbounded resize
  // controller on a small swapless box asks the kernel past what it will
  // overcommit, and the refusal is a bare failed op. env wins above; a device
  // pins -Dai_budget; 0 stays unbounded only where the machine cannot say its
  // size.
  if (g && !g->budget) {
    // raw read + hand parse, no stdio: nolibc's fscanf speaks no width and no
    // %lu, and the default must fire in both libcs. MemTotal leads the file;
    // the first digit run is the kB count. not gated on a kernel: the open
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

// bake [PATH] / wake PATH: the heap-image snapshot (doc/misc/snapshot.md) -- declared
// above the bootstrap split, because love0 links host/image.c too now: it bakes
// image files (the `bake` nif) and wakes them (wake), which is how the self-host
// build gets a warm mooncc under love0. the .image-section self-patch stays the
// full binary's lane.
extern int image_dump(struct ai*, char const*);          // host/image.c (file I/O around love.c's codec)
extern int image_bake(struct ai*);                       // host/image.c (the self-bake)
extern int ai_baked_pick(void const**, uintptr_t*);      // the carried image, if one is baked in
extern struct ai *image_load(char const*);
extern uint64_t ai_baked_image[];
extern uintptr_t ai_baked_image_len;

#ifdef LoveBoot
// love0: the CLI driver is the sed-wrapped raw text (it can't lcat its own arg
// ap). self-test: the whole test corpus, baked in (sed-wrapped), run
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

// with args, run the build tool (lcat / gen_data) through the CLI driver.
// with no args, self-test: eval prel, load bao (the shell core) as a module, and run
// the baked corpus via c0, then bootstrap the self-hosted ev (egg) and run the corpus
// again through it.
// the source library: both lanes load bao by name, and both get the whole table -- a
// build tool's (use 'x) (the mooncc cat's (use 'holo)) resolves the same as the
// self-test's. overlay and peg are listed, never used here: each consumer opens with
// its own (use ..), the boot owes nothing. an unlisted-for entry costs a row, nothing more.
static struct ai_lib const libs0[] = {
  {"bao", src0_bao}, {"rng", src0_rng}, {"kanren", src0_kanren}, {"uu", src0_uu},
  {"coin", src0_coin}, {"q", src0_q}, {"overlay", src0_overlay}, {"peg", src0_peg},
  {"holo", src0_holo},                                 // which the mooncc cat's cpp/gen read (the self-host build lane)
  {"verbs", src0_verbs},                               // the verb registry: love0 runs the same cli.l rail
  {NULL, NULL} };
struct ai_lib const *host_libs(void) { return libs0; }   // ai_libs picks (host/seat.c)

static struct ai *boot(struct ai *g, bool argp) {
  if (argp) {                                        // a build tool (lcat etc.): bake prel + bao first so the CLI's
    g = ai_evals_(g,                                   // own loader/printer (eval1/bye reach for map/jot/tap/puts/putc)
#include "p10.h"                                       // have the prel surface before they load the first file -- else
    );                                                 // loading prel.l itself misses every prel fn its loader uses.
    g = ai_evals_(g,                                   // its own call: readtext picks its reader once per text, and
#include "prel0.h"                                     // p1 seals hook 0 only when the call above evaluates
    " "
#include "post0.h"                                     // the printer, and `@` with it -- post's first half
        
    "(use 'bao)"                                       // p1 goes first: this lane never hatches an egg, and prel's
    "(use 'kanren)"                                    // loader folds `sound` at its own compile; kanren splices
                                                       //   because the corpus reads unify/ufail bare
    "(use 'verbs)"                                     // the verb registry the cli rail walks -- registered, then
    );                                                 //   unspliced below, which pops the LAST splice: verbs is it
    g = ai_unsplice_(g);
    g = ai_evals_(g, cli);                             // defines; cli-line is the dispatch
    return ai_evals_(g, "(cli-line cmdline 0)"); }     // a build tool is never a repl
  g = ai_evals_(g,                                    // p1 first: prel's loader reads `sound`, and a
#include "p10.h"                                      // global folds at its reader's compile, so the
  );                                                  // reader in love has to exist before prel compiles
  g = ai_evals_(g,
#include "prel0.h"                                    // prel, read by p1 now that hook 0 is sealed
    " "
#include "post0.h"                                    // ..and the printer, which pass 1 below already needs
  );
  g = ai_evals_(g,
    "(use 'bao)"                                       // bao (the shell core): loaded, registered, spliced
    "(use 'holo)");                                    // the assembler service: load + register..
  g = ai_unsplice_(g);                                 //   ..and the C unsplice keeps it non-ambient, like the host
  g = ai_evals_(g,
    "(use 'uu) (: uu (from 'uu))"                      // the uu kernel, then the library layers, all by name in the
                                                       //   old eval order (uu's one-name surface rebinds like the
    "(use 'coin)"                                      //   host); every layer, splice
    "(use 'rng)"                                       //   and registry entry persists across the egg warm below, so one
    "(use 'q)"                                         //   load serves both corpus passes
    "(use 'kanren)"
  );
  g = ai_evals_(g, "(: (s2cl s) ((: (g i) (? (< i (tally s)) (link (peep s i 0) (g (+ 1 i))))) 0))");   // string -> charlist, for the runner
  // the corpus is read, not baked: the nifs are drained in main() before boot() runs, so file
  // io is live here, and out/lib/corpus.list already holds the ordered set (it exists because
  // make cannot watch a wildcard's membership). baking it instead would put every test file
  // in love0's dependency graph, and love0 is not a release artifact anyway.
  // a missing file dies by name. answering () would run a shorter corpus and still print
  // "tests pass" -- a green gate over tests that never ran, which is the one failure this
  // must not have. presence is the wrapper, so the read tests the open, never the byte count.
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
// the tty is one terminal, so its cooked baseline and its atexit live in one
// place -- posix.c's, which the (raw on) nif already drives. this is the same
// call, and the capture-once latch there is what makes a repl that raws after
// bao already did restore the true baseline rather than a raw one.
extern int ai_raw_mode(intptr_t on);
#define raw_mode() ((void) ai_raw_mode(1))

static char const cli[] =
#include "cli.h"
 ;

// `bake` boots fully, then lays the post-warm image back into the binary's own
// .image section (host/image.c's copy + patch + atomic-rename -- no objcopy,
// ETXTBSY-proof) and exits; `bake PATH` writes a plain image file instead (the
// debug/inspection lane). `wake PATH` boots from an image file (any mismatch
// falls back to a normal egg boot). opt-in flags; a normal run is the same code path.
// the baked post-boot image: a reserve in its own .image section (host/image.c), filled by
// `love bake` (the binary boots, snapshots itself, and lays the result back into its own body).
// loaded at startup when its magic validates; else a normal egg boot.
// the post-warm dispatch (shared by boot() and the wake path, which skips the warm).
static struct ai *run_program(struct ai *g, bool replp) {
  // the session layer. boot is over; from here the base (orth -- prel/ev, the nifs,
  // every module the frontend warmed) is read-only, and it is read-only for the
  // plainest possible reason: it is never the head again. lvm_defglob writes
  // A(g->book) and nothing else, so a top-level definition -- a script's, a repl
  // line's, the corpus's -- lands here instead of in the base. reads still walk
  // down (bookget, head-first), so prel resolves exactly as before.
  //
  // pushed here because this is where boot() and the wake path converge, so both
  // get it; and it is never popped, because its lifetime is the session. that is
  // what keeps a catted app working: lux's eight files, the kore cat's fifteen and
  // the whole test corpus each arrive as one stream, so they share this layer and
  // the cross-file leaking they are built on (crew/lux/core.l's "every binding
  // leaks ... so the other files see this vocabulary") still resolves.
  //
  // bake exits before run_program, so the image carries the base with no session
  // layer on top; each woken session pushes its own. C-side: enter is a mopped nom
  // now, and a stashless layer is exactly what a session is.
  g = ai_layer_(g);
#ifdef AiGlazed
  // LOVE_NO_GLAZE: a pure-interpreter session -- ev back to base-ev (kept in the glaze
  // module book) and the natjit creation hook cleared. the forensics twin of LOVE_NO_IMAGE.
  // checked here, the convergence of the egg-boot and image-wake paths: a body-less
  // top-level : pins even where the book nom is sealed away (an image). bake never
  // sees it -- the knob governs a session, not the baked artifact.
  if (getenv("LOVE_NO_GLAZE")) g = ai_evals_(g, "(: ev (from 'glaze 'base-ev) natjit ())");
#endif
  // love/cli.l DEFINES rather than runs -- a body-less top-level `:` -- and `cli-line`
  // is this tail entire: the argv[0] verb door, the positional rail, the repl, the
  // stdin drink. one call, and the only thing C still owns is the isatty answer it
  // carries in. a forked lush child reaches the same door with the same line, which is
  // what lets a word whose PATH winner is this binary run without an exec.
  g = ai_evals_(g, cli);
  return ai_evals_(g, replp ? "(cli-line cmdline 1)" : "(cli-line cmdline 0)"); }

// the module sources, name-keyed (the love0 twins above): registered in the source
// library and loaded by `use` -- one layer per load, leave registers, the splice
// serves the bare names. the lib entries ride the image too, so a woken session
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
static char const src_uu[] =
#include "uu.h"
 ;
static char const src_bao[] =
#include "bao.h"
 ;
static char const src_verbs[] =
#include "verbs.h"
 ;
// holo, the crew/holo/ assembler: one entry = the arch-neutral core plus the native
// backend (C string concatenation; the glaze emits for the running arch only --
// mooncc's cat joins the cross backends at its own build, and love0 bakes x64+arm64
// so the corpus's cross-arch asserts run under both its compilers).
// the linker half rides the same entry, in load order: elf.l wraps assembled bytes in an
// executable, obj.l lays a relocatable .o, link.l links a set. they read holo's internals
// the LINKER HALF IS NOT HERE, and none of the three holos carries it: love0's is
// holo0+x64+arm64, the kernel's is holo plus its native backend, and this one matches.
// the egg's holo exists to feed the glaze, which emits for the machine it runs on and
// never writes a file; elf/obj/link come off the crew cat, laid at bake with the glaze
// live. a copy here was 48K of .rodata nothing called.
static char const src_holo[] =
#include "holo.h"
#if defined(__x86_64__)
#include "x64.h"
#elif defined(__aarch64__)
#include "arm64.h"
#elif defined(__riscv)
#include "riscv.h"
#endif
 ;

#ifdef AiGlazed
// the glaze, one module in two files: emit.l (the SSE/native emitter) then auto.l (ev's
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
  {"overlay", src_overlay}, {"peg", src_peg}, {"uu", src_uu}, {"bao", src_bao},
  {"holo", src_holo}, {"verbs", src_verbs},
#ifdef AiGlazed
  {"glaze", src_glaze},
#endif
  {NULL, NULL} };
struct ai_lib const *host_libs(void) { return libs; }    // ai_libs picks (host/seat.c)

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

static struct ai *boot(struct ai *g, bool argp, char const *bake, char const *bake_load) {
  bool replp = !argp && isatty(STDIN_FILENO);
  if (replp) raw_mode();
  // the debug door: LOVE_NO_MOP keeps the compiler's internals on the book (peek/poke/
  // seek/feels/dis and the raw cell nifs) for introspection -- egg.l skips the birth mop
  // when `nomop` is set. reaches only a fresh egg warm, so pair it with LOVE_NO_IMAGE
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
#include "post.h"                                       // the printer, and `@` with it -- post's first half
    );
  g = ai_evals_(g,
    "(use 'coin)"                                        // the library layers, all modules now, in the old eval order: coin
    "(use 'rng)"                                         //   (ring/monoid over the C coin lane), rng (the random stream), q
    "(use 'q)"                                           //   (rationals), then kanren (unification) -- registered before
    "(use 'kanren)"                                      //   overlay, whose engine reads subst through the registry
    "(use 'overlay)"                                     // overlay for the ev seam, whose hook lands in orth -- a module
    "(: overlay (from 'overlay)"                         //   layer cannot write it, the boot can. peg is registered above
    "   ev ((from 'overlay 'ov-hook) ev))"               //   and used by its consumers, not here.
    "(use 'uu)"                                          // uu.l is written in @, off the book's own macro table
                                                         // uu's NbE kernel: (: uu (from 'uu)) keeps the one-name surface --
    "(: uu (from 'uu))"                                  //   the corpus + an overlay reach (uu 'vof) through it
    "(use 'holo)"                                        // the crew/holo/ assembler, a post-egg language service: load + register,
  );                                                     //   then the C unsplice below keeps it non-ambient -- (use 'holo)
  g = ai_unsplice_(g);                                   //   splices it, (from 'holo 'assemble) probes it. a test that wants a
                                                         //   cross backend joins it at runtime ((use 'holo) <backend.l> -- the
                                                         //   test_glaze/test_raw_arm64 recipes), mooncc's cat joins all of them
  g = ai_evals_(g,
    "(use 'bao)"                                         // the shell core: loaded, registered, spliced (read/reads/welp/wrap bare)
    "(use 'verbs)"                                       // the verb registry, holo's shape: registered, then non-ambient below --
  );                                                     //   tab/word/seat are not names to reach bare, and `get` would shadow half
  g = ai_unsplice_(g);                                   //   the tree. a plain binary carries wake+bake and nothing else; a dist
                                                         //   bake's cat pins the rest into (from 'verbs 'tab)
  // kanren, overlay and uu come off the book. overlay and uu already have the
  // accessor the boot binds just above -- (: uu (from 'uu)) and overlay's -- so the
  // splice on top bought nothing but ambient names, and `C`, `Q`, `src`, `glob`,
  // `walk`, `var`, `con`, `est` are what this tree calls its locals. a binding you
  // forgot to write resolved instead of raising.
  // kanren keeps a surface, named here rather than inherited, the line its own
  // header draws: unify and its ufail/ufail? contract are the door, and `hoist`
  // takes the goal macros, which ride the layer's table and not the tablet's noms.
  // unsplice drops one link at a time, so pat and bao come off with them and go
  // straight back on: @ for every later compile, read/reads for cli.
  for (int i = 0; i < 4; i++) g = ai_unsplice_(g);       // bao, uu, overlay, kanren
  g = ai_evals_(g, "(use 'bao)"
    "(hoist 'kanren ())"                                 // \\\, &&&, |||, zz
    "(: unify (from 'kanren 'unify)  ufail (from 'kanren 'ufail)"
    "   ufail? (from 'kanren 'ufail?)  var (from 'kanren 'var)"
    "   s_plus (from 'kanren 's_plus)  s_star (from 'kanren 's_star)"
    "   === (from 'kanren '===)  =/= (from 'kanren '=/=))");           // ..and back: @ for every later compile, read/reads for cli
#ifdef AiGlazed
  // the glaze, in three moves. (use 'glaze) loads emit.l + auto.l into their own layer and
  // registers it -- ~415 codegen names the book never sees. holo is spliced under that layer
  // so `assemble` folds at the glaze's compile, and both come off after.
  g = ai_evals_(g, "(use 'holo)" "(use 'glaze)");
  g = ai_unsplice_(g);                                   // the glaze layer: registered, non-ambient
  // then orth's two names, which a module layer cannot write and the boot can: `ev` becomes
  // auto-native, `member?` its glazed self. both carry their own re-load/trampoline gates.
  // last the ala creation hook (love/glaze/hook.l), which is not in the module -- it leaks
  // natjit/fires/fired?/bake onto the book on purpose, and test_glaze re-cats it standalone.
  g = ai_evals_(g,
      "(: ev (from 'glaze 'ev) member? (from 'glaze 'member?))"
#include "hook.h"
      );
  g = ai_unsplice_(g);                                   // holo back to non-ambient
#endif

  // the seat doors: the process quartet, wrapped to read the LIVE door off a
  // tablet per call. a crew bake captures the wrappers, so a baked closure's
  // spawn still lands wherever the running seat aims the tablet -- the metal
  // wake (free/kmain.c) pins its task shim in; here the slots hold the posix
  // nifs the drain just laid. slot order: spawn spawnio spawnmap wait.
  g = ai_evals_(g,
    "(: spawn0 spawn  spawnio0 spawnio  spawnmap0 spawnmap  wait0 wait"
    "   seat-doors (: t (tablet 4) _ (pin t 0 spawn0) _ (pin t 1 spawnio0)"
    "                 _ (pin t 2 spawnmap0) _ (pin t 3 wait0) t)"
    "   (spawn argv) ((peep seat-doors 0 0) argv)"
    "   (spawnio argv i o e cl pg fg) ((peep seat-doors 1 0) argv i o e cl pg fg)"
    "   (spawnmap argv fdm cl pg fg) ((peep seat-doors 2 0) argv fdm cl pg fg)"
    "   (wait p) ((peep seat-doors 3 0) p))");

  // the seal, and it runs on every boot -- an egg boot and a woken image must differ in
  // startup time and nothing else. `book` goes so a program cannot reassign the globals
  // under everyone (the same reason a module book hands out a lookup closure, not its
  // tablet), and nif/nifx go because they install raw bytes as executable code: the glaze
  // folded them into its closures at the load above, and keeps them as module members
  // (love/glaze/emit.l), so (from 'glaze 'nif) is the one door left onto that.
  // and `born` comes off when baking, because it is the one thing above that a
  // snapshot must not carry: it is the hatch duration (egg.l's (clock now)), so a
  // baked one would freeze one machine's ~220 ms into every future wake and report it
  // as this run's. it is also the last content in the image that varies run to run,
  // which is what makes a bake reproducible at all. the waker re-pins it below, at
  // its own cost -- per invocation, which is the only reading that was ever true.
  g = ai_evals_(g, bake
    ? "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) _ (pull book 'born 0) (pull book 'book 0))"
    : "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) (pull book 'book 0))");

  if (bake) {                                            // the bake verb: snapshot the post-warm heap, then exit
    // `bake -l FILE`: read-eval one more .l file before the snapshot -- the dist
    // artifact's door (crew/build.mk): the crew cats + the verb table go in warm,
    // ahead of the same cache-empty + seal every bake gets, and the image still
    // carries no session layer. a raise in the cat finds nothing heard here (no shell
    // help), so a broken cat is a loud failed bake, never a quiet artifact.
    // an argument, not an environment variable: it is visible in the command that ran,
    // survives being read back out of a log, and cannot be inherited by something that
    // never asked. this tree spends exactly one env var (HOME).
    if (bake_load && !ai_ok(g = bake_eval_file(g, bake_load))) return g;
    g = bake_empty_glaze(g);
    int rc = *bake ? image_dump(g, bake) : image_bake(g);
    if (rc) fprintf(stderr, "love: bake failed (rc=%d)\n", rc);
    exit(rc ? 1 : 0); }
  return run_program(g, replp); }
#endif

// marshal a word list onto the stack as one chain, left on top. `skip` drops that many
// words after argv[0] -- the prime verb's own, which belong to the command line and not
// to the program. each string is pushed before any is consed, so the ones already there
// are rooted on the stack through every ai_strof that might collect.
ai_noinline static struct ai *argv_chain(struct ai *g, char const **v, int argc, int skip) {
  int n = 0;
  if (argc > 0) g = ai_strof(g, v[0]), n++;                  // argv[0] is always the program
  for (int i = 1 + skip; i < argc; i++) g = ai_strof(g, v[i]), n++;
  for (g = ai_push(g, 1, ai_zero); n--; g = gxr(g));
  return g; }

#if !defined(LoveBoot) && !defined(__wasm__)
// THE FIRST BOOT: an unbaked binary that carries its source finishes ITSELF --
// the dist roster members catted off the carried blob into a sibling file, a
// child `bake -l` over it (the one bake path, so the bytes are the tree's own,
// byte-identical to make's), and a re-exec on the patched binary. the cross
// seed's egg becomes the full artifact on first contact: no tree, no emulator,
// nothing foreign. any refusal prints its story and the egg boots -- slower,
// never fatal. dead on the kernel link (kmain is that entry) and absent from
// love0 (LoveBoot) and wasm (no processes).
extern intptr_t ai_inflate_raw(const unsigned char*, uintptr_t, unsigned char*, uintptr_t);
extern const unsigned char ai_srcgz[];
extern const uintptr_t ai_srcgz_len;
extern size_t host_selfpath(char*, size_t);
static char const src_distlist[] =
#include "distlist.h"
 ;
static uintptr_t fb_octal(unsigned char const *p, int n) {
  uintptr_t v = 0;
  for (int i = 0; i < n && p[i] >= '0' && p[i] <= '7'; i++) v = v * 8 + (uintptr_t)(p[i] - '0');
  return v; }
// join a symlink's target against the link's own directory, ".." and "." squashed
// (free/kmain.c k_lnk_canon's law) -- the tree keeps crew modules behind lib/ links
static uintptr_t fb_canon(char const *at, char const *ln, char *out, uintptr_t cap) {
  uintptr_t n = 0;
  if (ln[0] != '/') {
    uintptr_t d = strlen(at);
    while (d && at[d - 1] != '/') d--;
    if (d && d <= cap) memcpy(out, at, n = d - 1); }
  for (uintptr_t i = 0; ln[i];) {
    while (ln[i] == '/') i++;
    uintptr_t j = i;
    while (ln[j] && ln[j] != '/') j++;
    uintptr_t k = j - i;
    if (!k) break;
    if (k == 1 && ln[i] == '.') { i = j; continue; }
    if (k == 2 && ln[i] == '.' && ln[i + 1] == '.') {
      while (n && out[n - 1] != '/') n--;
      if (n) n--;
      i = j; continue; }
    if (n && n < cap - 1) out[n++] = '/';
    while (i < j && n < cap - 1) out[n++] = ln[i++]; }
  return n; }
// inflate the carried blob (gzip: skip the header fields, ISIZE names the tar)
static unsigned char *fb_untar(uintptr_t *outn) {
  unsigned char const *z = ai_srcgz; uintptr_t zn = ai_srcgz_len;
  if (zn < 18 || z[0] != 0x1f || z[1] != 0x8b || z[2] != 8) return NULL;
  uintptr_t o = 10; unsigned f = z[3];
  if (f & 4) o += 2 + (uintptr_t) z[o] + ((uintptr_t) z[o + 1] << 8);
  if (f & 8) { while (o < zn && z[o]) o++; o++; }
  if (f & 16) { while (o < zn && z[o]) o++; o++; }
  if (f & 2) o += 2;
  if (o + 8 >= zn) return NULL;
  uintptr_t un = (uintptr_t) z[zn - 4] | (uintptr_t) z[zn - 3] << 8
               | (uintptr_t) z[zn - 2] << 16 | (uintptr_t) z[zn - 1] << 24;
  unsigned char *t = mmap(NULL, un ? un : 1, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (t == MAP_FAILED) return NULL;
  if (ai_inflate_raw(z + o, zn - o - 8, t, un) != (intptr_t) un)
    return munmap(t, un), NULL;
  return *outn = un, t; }
// find a tree-relative path in the ustar block. the archive's paths carry a TOP
// component (the tree looks the same from inside as a checkout), so match past
// it; a symlink member chases its target against its own directory.
static unsigned char const *fb_find(unsigned char const *t, uintptr_t n,
                                    char const *path, uintptr_t *len, int hop) {
  uintptr_t pl = strlen(path);
  if (hop > 3 || !pl) return NULL;
  for (uintptr_t o = 0; o + 512 <= n && t[o];) {
    unsigned char const *h = t + o;
    uintptr_t sz = fb_octal(h + 124, 12);
    if ((h[156] == '0' || h[156] == 0 || h[156] == '2') && !memcmp(h + 257, "ustar", 5)) {
      char nm[256]; uintptr_t ln = 0;
      for (int i = 345; i < 500 && h[i] && ln < 254; i++) nm[ln++] = (char) h[i];
      if (ln) nm[ln++] = '/';
      for (int i = 0; i < 100 && h[i] && ln < 255; i++) nm[ln++] = (char) h[i];
      uintptr_t cut = 0;
      while (cut < ln && nm[cut] != '/') cut++;
      cut = cut < ln ? cut + 1 : 0;
      if (ln - cut == pl && !memcmp(nm + cut, path, pl)) {
        if (h[156] != '2') return *len = sz, t + o + 512;
        char tgt[101], cn[256]; uintptr_t tn = 0;
        while (tn < 100 && h[157 + tn]) { tgt[tn] = (char) h[157 + tn]; tn++; }
        tgt[tn] = 0;
        uintptr_t cl = fb_canon(path, tgt, cn, sizeof cn - 1);
        cn[cl] = 0;
        return fb_find(t, n, cn, len, hop + 1); } }
    o += 512 + ((sz + 511) & ~(uintptr_t) 511); }
  return NULL; }
static void first_boot(char const **argv) {
  if (ai_srcgz_len < 18) return;                       // no carried source: a dev link, egg on
  if (getenv("LOVE_FIRST_BOOT")) {                     // the latch: one try per exec chain
    fprintf(stderr, "; first boot: still unbaked after a bake -- running from source\n");
    return; }
  char exe[4096], cat[sizeof exe + 40];              // + ".firstboot.<pid>.l" and its NUL
  if (!host_selfpath(exe, sizeof exe)) return;
  uintptr_t un = 0;
  unsigned char *t = fb_untar(&un);
  if (!t) {
    fprintf(stderr, "; first boot: the carried source will not inflate -- running from source\n");
    return; }
  // per-process, for the reason the bake's scratch is (host/image.c): concurrent first
  // boots on one name write the cat into each other and unlink it under each other.
  snprintf(cat, sizeof cat, "%s.firstboot.%ld.l", exe, (long) getpid());
  int fd = open(cat, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd < 0) {                                        // a read-only seat: the honest story, no bake
    fprintf(stderr, "; first boot: %s is not writable -- running from source this session\n", cat);
    munmap(t, un);
    return; }
  for (char const *p = src_distlist; *p;) {
    while (*p == ' ' || *p == '\n') p++;
    char w[256]; size_t wl = 0;
    while (*p && *p != ' ' && *p != '\n' && wl < 255) w[wl++] = *p++;
    if (!wl) break;
    w[wl] = 0;
    uintptr_t ml = 0;
    unsigned char const *m = fb_find(t, un, w, &ml, 0);
    if (!m || (ml && write(fd, m, ml) != (ssize_t) ml)) {
      fprintf(stderr, "; first boot: %s %s -- running from source\n", w,
              m ? "would not write" : "is not in the carried source");
      close(fd), unlink(cat), munmap(t, un);
      return; } }
  close(fd), munmap(t, un);
  fprintf(stderr, "; first boot -- baking the crew from the carried source (about a minute, once)\n");
  pid_t p = fork();
  if (!p) { char *args[] = { exe, (char*) "bake", (char*) "-l", cat, NULL };
            execv(exe, args); _exit(127); }
  int st = -1;
  if (p > 0) waitpid(p, &st, 0);
  unlink(cat);
  if (p < 0 || !WIFEXITED(st) || WEXITSTATUS(st)) {
    fprintf(stderr, "; first boot: the bake failed -- running from source\n");
    return; }
  setenv("LOVE_FIRST_BOOT", "1", 1);
  execv(exe, (char *const *)(void *) argv);            // the patched file: same path, new inode
  fprintf(stderr, "; first boot: cannot re-exec -- running from source\n"); }
#endif

int main(int argc, char const **argv) {
  signal(SIGPIPE, SIG_IGN);        // a hung-up peer is an answer, not a death (fd_writen)
  struct ai *g = NULL;
  // the prime verbs. `bake [PATH]` / `wake PATH` must lead the command line, and by
  // physics rather than habit: wake decides which heap there is (image_load precedes
  // ai_ini), and bake must snapshot before run_program pushes the session layer. so they
  // are read here, in C, before any love exists to read them -- which is exactly why they
  // are the two the registry cannot own. love/verbs.l holds their rows anyway, so one
  // manifest answers for help and the shadow rule and a misplaced one is an honest error
  // instead of falling through to "run the file named bake".
  // bare words, not flags: a verb is a verb. the escape hatches are the registry's own --
  // `love ./bake` and `love -- bake` are the file, since neither is this strcmp.
  // both lanes: love0 links host/image.c too, so it wakes an image file (its own
  // mooncc0.image bake -- the self-host build's ~ms compiler starts); `bake` (the
  // self-patch) stays host-only, since love0 lays no .image section rule.
  // `skip` counts the words that are the prime's and not the program's -- a count and not
  // a shift, which would clobber argv[0] and leave a command line nobody typed. nothing is
  // written, so both readings stay available: the whole invocation, and the program's view.
  char const *image_load_path = NULL, *bake = NULL;  // see boot(): "" = self-bake, a path = image file
#ifndef LoveBoot
  char const *bake_load = NULL;                     // bake -l CAT: read-eval it before the seal
#endif
  int skip = 0;

#ifndef LoveBoot
  if (argc >= 2 && !strcmp(argv[1], "bake")) {
   int i = 2;                                      // bake [-l CAT] [PATH]
   if (i + 1 < argc && !strcmp(argv[i], "-l")) bake_load = argv[i + 1], i += 2;
   bake = i < argc ? argv[i] : "";
   skip = (i < argc ? i + 1 : i) - 1; }
  else
#endif
  if (argc >= 3 && !strcmp(argv[1], "wake"))
   image_load_path = argv[2], skip = 2;
  // a leading wake with nothing to wake. its arity error is C's because its parse is:
  // the registry's row would say "must lead the command line", which is the one thing
  // this invocation got right.
  else if (argc == 2 && !strcmp(argv[1], "wake"))
   return fprintf(stderr, "love: wake needs an image path\n"), 2;
  if (image_load_path && !(g = image_load(image_load_path))) image_load_path = NULL;   // NULL -> normal boot
  // auto-load: with no image flag, wake the image baked into the binary's own .image section, so a
  // plain `love` is glazed-by-default at ~4 ms cold start instead of the ~230 ms egg eval. opt out with
  // LOVE_NO_IMAGE (the bench does, to control glazed-vs-interp itself). an empty value is nothing
  // (unset) -- so a recipe under a caller's exported LOVE_NO_IMAGE can hand one
  // command its image back with the sh idiom `LOVE_NO_IMAGE= cmd` (the dist artifact running as
  // $(CC): its mooncc verb lives in the baked image, and an egg boot would read "mooncc" as a
  // filename). any problem -- unbaked, stale, truncated -- makes the load return NULL, so we fall
  // through to the normal egg boot. never wrong.
  // (love0's reserve is 2 words and never baked, so its auto-load always falls through.)
  char const *noimg = getenv("LOVE_NO_IMAGE");
  uintptr_t woke_ms = 0;                       // what the wake cost, for `born` below
  if (!g && !bake && !(noimg && *noimg)) {
   uintptr_t t0 = ai_clock();
   uintptr_t blen = 0;
   void const *bimg = NULL;
   if (ai_baked_pick(&bimg, &blen) && (g = ai_image_load(bimg, blen)))
    woke_ms = ai_clock() - t0,
    image_load_path = "<baked>"; }                                     // a loaded image is the booted state: skip the egg warm
#if !defined(LoveBoot) && !defined(__wasm__)
  // unbaked, with source aboard, and nothing explicit asked for: finish first.
  // a `wake` names its own image and a `bake` is the finishing move itself.
  if (!g && !bake && !(noimg && *noimg) && !(argc >= 2 && !strcmp(argv[1], "wake")))
    first_boot(argv);                                  // returns only on refusal; success re-execs
#endif
  if (!g) g = ai_ini();
  g = env_budget(g);                               // the LOVE_BUDGET_MB cap, on whichever g won (fresh or woken image)
  bool argp = argc - skip > 1;
  // two chains, because there are two honest readings and they differ by the primes:
  //   cmdline  the whole invocation, exactly as typed -- `wake image` included
  //   argv     the program's view: argv[0], then the words past the prime
  // cli.l drops argv's head for its own use and rebinds argv again to the program's
  // own argv; cmdline is never rebound by anyone, which is what makes it the thing a
  // seat scan can read in any load order (love/verbs.l's `unprime` steps the primes).
  // neither is pinned under a bake, and that absence is what makes a bare read safe: a
  // baked consumer folds its bare globals at its own compile, so a nom pinned while the -l
  // cat compiles would ride this line into every future wake. unpinned it cannot fold, and
  // every non-bake invocation pins both before a line of love runs. love/verbs.l's `fire`
  // carries the one presence guard, so the app feet go quiet instead of scaring.
  if (!bake) {
    g = argv_chain(g, argv, argc, 0);               // cmdline, first: it ends up deeper
    g = argv_chain(g, argv, argc, skip); }          // argv, on top -- sp[0]
  if (ai_ok(g)) {
    // the static nifs (exit/open/close/run/getenv + any host/*.c app nifs) come
    // from the ai_nifs section -- immortal addresses, so the array door serves.
    // (This also re-pins them into a loaded image's book.)
    g = ai_defn(g, __start_ai_nifs, __stop_ai_nifs - __start_ai_nifs, 0);
    // ..and the module tables (ai_mods, one ai_defn call per row): an app's nifs
    // land under its module, off the bare book. same re-pin over a woken image --
    // the registry rides the image, so the drain finds the tablet and refreshes it.
    for (struct ai_mod const *mt = __start_ai_mods; mt < __stop_ai_mods; mt++)
      g = ai_defn(g, mt->defs, mt->n, mt->mod);
    // neither chain leaves the stack. they are live heap values, so they cannot ride
    // a struct ai_def: C cannot re-root what it holds in an array, and the defn above
    // interns a hundred names -- a hundred chances to move them. ai_defv reads sp[0]
    // and leaves it, so each pop hands the next chain up.
    if (!bake) {
      g = ai_defv(g, "argv");
      if (ai_ok(g)) ai_core_of(g)->sp++;            // the book holds argv; the line is sp[0] now
      g = ai_defv(g, "cmdline");
      if (ai_ok(g)) ai_core_of(g)->sp++; }          // the book holds it now
    // `love-image`: which image this session woke, by path -- here because here is
    // the only place it is knowable. the wake strips it from argv, so a consumer
    // keyed on its own compiler's identity (mooncc's runtime cache) can ask no
    // other way, and the alternative it settled for was every *.image beside the
    // binary, which makes a stranger's rebuild a miss. "<baked>" is the binary's
    // own .image section, whose identity is the binary's.
    // pinned only when there is one, absence being the answer for the rest -- and unpinned
    // under a bake, for the reason `argv` and `cmdline` give below. ask with
    // (member? 'love-image (names ())): presence out of band, never (lit? ..).
    if (image_load_path && ai_ok(g = ai_strof(g, image_load_path))) {
      g = ai_defv(g, "love-image");
      if (ai_ok(g)) ai_core_of(g)->sp++; }
    // `love-os`: which kernel this invocation stands on -- "linux", "freebsd" or
    // "netbsd". a per-run fact like argv and not a build one: one binary answers
    // all three, so the compile that made it cannot say and only the run can. our
    // libc probed it at entry (__ai_osv); a foreign one is built for one kernel
    // and its predefine is the whole answer.
    // unpinned where nothing can tell, absence being the honest answer -- a consumer that
    // must know owes a diagnostic, never a guess at linux. unpinned under a bake too, or a
    // baked consumer folds the baking machine's kernel in and carries it onto every other.
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
    // `born` -- what this invocation cost to start, in ms, and it belongs to the run and
    // not to the heap. the egg pins the hatch duration (egg.l) and the bake pulls it back
    // off (the seal), so a woken love arrives without one and re-pins the wake duration
    // here. both readings answer the same question; only the number differs, and the gap
    // between them is the whole point of baking (~220 ms hatched against ~4 ms woken).
    if (image_load_path && ai_ok(g = ai_push(g, 1, putcharm((intptr_t) woke_ms)))) {
      g = ai_defv(g, "born");
      if (ai_ok(g)) ai_core_of(g)->sp++; }
    // take what fd 0 can lend -- a read run, or its blocking bit (above). never under
    // a bake: the image would carry a heap port, and a run's state belongs to the run, not the egg.
    if (!bake) g = stdin_take(g);
#ifdef LoveBoot
    if (!image_load_path) g = boot(g, argp);
    else g = ai_evals_(ai_evals_(ai_layer_(g), cli), "(cli-line cmdline 0)");   // woken: the image carries the warm base; push the session layer, run the CLI
#else
    if (!image_load_path) g = boot(g, argp, bake, bake_load);
    else {              // wake: skip the egg warm, dispatch straight to the program
      bool replp = !argp && isatty(STDIN_FILENO);
      if (replp) raw_mode();
      g = run_program(g, replp); }
#endif
  }
  if (ai_code_of(g) == ai_status_scare) ai_scare_face_(g);   // the honest face: ";; a b", or ";; oom@len=N" bare
  stdin_give(g);                                    // whoever shares this fd gets it back exact
  return ai_fin(g); }
