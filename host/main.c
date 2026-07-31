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
#include <stdnoreturn.h>
#include <sys/wait.h>

ai_noinline uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_REALTIME, &ts) ? (uintptr_t) -1
       : (uintptr_t) (ts.tv_sec * 1000 + ts.tv_nsec / 1000000); }

// the fine clock's real source (the weak default in love.c degrades to ms*1e6)
ai_noinline intptr_t ai_nclock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? -1
       : (intptr_t) ts.tv_sec * 1000000000 + ts.tv_nsec; }


// for (;;): the standard noreturn-defensive shape -- moon's stdnoreturn.h defines
// `noreturn` empty, so mooncc can't cut the fall-through tail itself; the loop
// leaves no ret for vmret to flag (gcc emits identical code either way).
static noreturn lvm(lvm_exit) { for (;;) exit(getcharm(Sp[0])); }
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

static ai_noinline int poll_wrap(int fd) {
  struct pollfd p = { .fd = fd, .events = POLLIN };
  return poll(&p, 1, 0); }

bool ai_ready(int fd) { return fd < 0 || poll_wrap(fd) > 0; }

void ai_wait_fds(int const *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  if (n > ai_wait_fds_max) __builtin_trap();
  struct pollfd p[ai_wait_fds_max];
  for (int i = 0; i < n; i++) p[i].fd = fds[i], p[i].events = POLLIN;
  poll_wait(p, n, ms); }

static struct ai *fd_getc(struct ai *g) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = g->io;
  if (getcharm(i->ungetc_buf) != EOF) {
    fc->b = getcharm(i->ungetc_buf);
    i->ungetc_buf = putcharm(EOF);
    return g; }
  uint8_t b;
  ssize_t n = read(getcharm(i->fd), &b, 1);
  if (n <= 0) { i->eof_seen = putcharm(true); fc->b = EOF; }
  else fc->b = b;
  return g; }

static struct ai *fd_ungetc(struct ai *g, int c) {
 struct ai *fc = ai_core_of(g);
 struct ai_io *i = fc->io;
 i->ungetc_buf = putcharm(c);
 i->eof_seen = putcharm(false);
 return fc->b = c, g; }

static struct ai *fd_eof(struct ai *g) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = fc->io;
  return fc->b = (getcharm(i->ungetc_buf) == EOF) && getcharm(i->eof_seen), g; }

static struct ai *fd_putc(struct ai *g, int c) {
 uint8_t b = c;
 if (g->io->fd == putcharm(STDOUT_FILENO)) fputc(b, stdout);
 else write(getcharm(g->io->fd), &b, 1);
 return g; }

static struct ai *fd_flush(struct ai *g) {
 if (g->io->fd == putcharm(STDOUT_FILENO)) fflush(stdout);
 return g; }

// the bulk lanes (contract in love.h). writen drains stdio first when the fd is
// stdout -- per-byte puts ride stdio there, and the direct write(2) must land
// AFTER them or the stream interleaves. readn is one nonblocking gulp: the
// O_NONBLOCK toggle is per-call because fd flags ride the open file description,
// which a pty child shares.
static intptr_t fd_writen(struct ai *g, unsigned char const *src, uintptr_t n) {
 intptr_t fd = getcharm(g->io->fd);
 if (fd == STDOUT_FILENO) fflush(stdout);
 uintptr_t i = 0;
 while (i < n) {
  ssize_t k = write((int) fd, src + i, n - i);
  if (k < 0) { if (errno == EINTR) continue; break; }
  i += (uintptr_t) k; }
 return (intptr_t) i; }
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
 intptr_t fd = getcharm(g->io->fd);
 // FAULT INJECTOR for doc/io.md defect 5 -- LOVE_FAULT_EAGAIN=<n> makes the nth
 // readn ON A TTY model a COMPETING READER on a shared fd: swallow whatever is waiting and
 // answer "would block". that is the poll-said-ready-then-read-said-no race, which
 // NO in-process schedule can reach (lvm_fgetc's guard and its refill are one op,
 // and the vm yields only at an Ap) but a second process sharing the open file
 // description can. before the fix this hung the whole VM; test/host/pty.l gates it.
 // ⚠ it fires only on a TTY, and must: on a regular file poll always says
 // readable, so the retry succeeds and nothing is proved. targeting the tty
 // rather than an nth-call count keeps the gate independent of how many times
 // the script itself was read.
 static int fault = -2;
 if (fault == -2) { char const *s = getenv("LOVE_FAULT_EAGAIN"); fault = s ? atoi(s) : 0; }
 if (fault > 0 && isatty((int) fd) && --fault == 0) {
  int fl0 = fcntl((int) fd, F_GETFL);
  fcntl((int) fd, F_SETFL, fl0 | O_NONBLOCK);
  unsigned char sink[256];
  while (read((int) fd, sink, sizeof sink) > 0);   // the other reader takes it all
  fcntl((int) fd, F_SETFL, fl0);
  return 0; }
 int fl = fcntl((int) fd, F_GETFL);
 fcntl((int) fd, F_SETFL, fl | O_NONBLOCK);
 ssize_t k = read((int) fd, dst, n);
 fcntl((int) fd, F_SETFL, fl);
 return k > 0 ? (intptr_t) k
      : k == 0 ? -1
      : (errno == EAGAIN || errno == EWOULDBLOCK) ? 0 : -1; }

struct ai_port_vt const ai_fd_port_vt =
 { fd_getc, fd_ungetc, fd_eof, fd_putc, fd_flush, fd_writen, fd_readn };

struct ai_io ai_stdin = { lvm_port_io, putcharm(STDIN_FILENO), putcharm(EOF), putcharm(false) };
struct ai_io ai_stdout = { lvm_port_io, putcharm(STDOUT_FILENO), putcharm(EOF), putcharm(false) };
struct ai_io ai_stderr = { lvm_port_io, putcharm(STDERR_FILENO), putcharm(EOF), putcharm(false) };
// Override the weak g.c default with the real POSIX close. Called by the
// finalizer that ai_io_alloc registers, so it runs when a heap port becomes
// unreachable. Static stdin/stdout don't go through this path -- they live
// outside the l heap and the GC never visits them.
void ai_fd_close(int fd) { close(fd); }
// the GC-context drain (a collected port's unflushed write run): raw write(2),
// no g machinery -- safe inside run_finalizers.
void ai_fd_drain(int fd, void const *p, uintptr_t n) {
 unsigned char const *src = p;
 uintptr_t i = 0;
 while (i < n) {
  ssize_t k = write(fd, src + i, n - i);
  if (k < 0) { if (errno == EINTR) continue; break; }
  i += (uintptr_t) k; } }

// (open path mode) — open a file with mode "r"/"w"/"a"; returns a heap port
// (closed on GC) or nil on error or misuse. mode is a l string; only the
// first byte is consulted.
//   r = read-only
//   w = write-only, truncate-or-create
//   a = write-only, append-or-create
// Errors (path too long, unknown mode, open(2) failure) all return nil.

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
  return Continue();
 fail:
  Sp[1] = ZeroPoint;
  Sp += 1;
  Ip += 1;
  return Continue(); }

// (close p) — close a port, mark its fd as the closed-sentinel (-3) so
// subsequent reads/writes/flush go to the noop slot, and the finalizer
// (which checks fd >= 0) skips. Returns (). No-op on misuse, matching
// the existing fputc/etc. convention.
static lvm(lvm_close) {
  // inline "is x a port": heap pointer whose discriminator is lvm_port_io.
  if ((Sp[0] & 1) == 0 && ((union u*) Sp[0])->ap == lvm_port_io) {
    struct ai_io *io = (struct ai_io*) Sp[0];
    intptr_t fd = getcharm(io->fd);
    if (fd >= 0) {
      g->io = io;
      Pack(g);
      g = ai_io_wflush(g, io);   // buffered bytes land before the fd dies
      Unpack(g);
      close(fd);
      io->fd = putcharm(-3); } }
  Sp[0] = ZeroPoint;
  Ip += 1;
  return Continue(); }

// --- subprocess (run) + environment (getenv) ---------------------------
// Both are host-only nifs (POSIX fork/exec/wait, getenv), like open/close.
// No malloc: argv is marshalled into the uncommitted l heap gap and the
// child's stdout is captured into a growing l string (the reader's
// str0 + grow + len-fixup pattern). See core/io.c ioread1str / grbufg.

// Local copy of core/io.c's grbufg (static there): grow the string on sp[0]
// to 2*len, copying the old `len` bytes in. str0 is the public allocator.
static struct ai *host_grbufg(struct ai *g, uintptr_t len) {
 if (ai_ok(g = str0(g, 2 * len)))
  memcpy(txt(g->sp[0]), txt(g->sp[1]), len),
  g->sp[1] = g->sp[0], g->sp++;
 return g; }

// Best-effort write-through for the tee mode below: loop over a partial write,
// but let a failed/closed stdout pass silently -- a broken pipe on the ECHO of a
// child's output must not fail the child, which ran fine.
static void host_teeout(char const *p, size_t n) {
 while (n) {
  ssize_t w = write(STDOUT_FILENO, p, n);
  if (w < 0) { if (errno == EINTR) continue; return; }
  p += w, n -= (size_t) w; } }

// Workhorse for (run argv) / (runt argv). Called with g Packed; argv is the
// single arg. Pushes EXACTLY ONE net value above argv on every path so the
// lvm_run shell collapses uniformly: success -> [(status . output), argv],
// failure -> [errno-or-(-1) fixnum, argv]. Returns a not-ok g only on OOM.
// &locals (pipes/pid/status) are fine here: this returns normally, it is
// not a VM-dispatch tail-call site (cf. call_open vs lvm_open).
//
// `tee` picks WHEN the captured output reaches stdout, not whether it is
// captured: 0 (run) holds it until the child exits and hands the whole string
// back for the caller to do as it likes; 1 (runt) ALSO write(2)s each chunk
// through as it is read, so a long-running child STREAMS -- which is what make
// does (it pipes the child too, then relays each chunk rather than hoarding it).
// A capture-and-reprint caller passes 1 and skips its own reprint; a caller that
// consumes the text -- $(shell ..), a glob, an mtime probe -- passes 0. Because
// the tee bypasses the l-level `out` buffer, a teeing caller must (flush out)
// first or its own echoed lines land after the child's bytes.
ai_noinline static struct ai *host_run(struct ai *g, ai_word argv, int tee) {
 // pass 1: validate every element is a string; size the arg-byte blob.
 intptr_t argc = 0;
 uintptr_t total = 0;
 for (ai_word p = argv; chainp(p); p = B(p)) {
  if (!ai_strp(A(p))) return ai_push(g, 1, putcharm(-1));   // misuse
  argc++, total += len(A(p)) + 1; }                       // +1 for the NUL
 if (!argc) return ai_push(g, 1, putcharm(-1));            // empty argv

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
 int op[2], ep[2];
 if (pipe(op)) return ai_push(g, 1, putcharm(errno));
 if (pipe(ep)) { int e = errno; close(op[0]); close(op[1]); return ai_push(g, 1, putcharm(e)); }
 fcntl(ep[1], F_SETFD, FD_CLOEXEC);
 fflush(stdout);
 pid_t pid = fork();
 if (pid < 0) { int e = errno;
  close(op[0]); close(op[1]); close(ep[0]); close(ep[1]);
  return ai_push(g, 1, putcharm(e)); }
 if (!pid) {                                              // child
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
  return ai_push(g, 1, putcharm(childerr)); }

 // drain stdout into a growing l string (bulk reads; stderr inherited). Under
 // tee, each chunk is ALSO written straight through as it arrives -- the read
 // loop is already incremental, so streaming costs one write per chunk.
 uintptr_t n = 0, lim = 1u << 16;
 g = str0(g, lim);                                        // capture -> sp[0]
 while (ai_ok(g)) {
  if (n == lim) { g = host_grbufg(g, lim); lim *= 2; continue; }
  r = read(op[0], txt(g->sp[0]) + n, lim - n);
  if (r < 0) { if (errno == EINTR) continue; break; }
  if (!r) break;                                          // EOF
  if (tee) host_teeout(txt(g->sp[0]) + n, (size_t) r);    // ..before the buffer can move
  n += (uintptr_t) r; }
 close(op[0]);
 { int st; while (waitpid(pid, &st, 0) < 0 && errno == EINTR) {}          // reap
   if (!ai_ok(g)) return g;                                // OOM mid-drain
   if (n) len(g->sp[0]) = n;                              // fix logical length
   else g->sp[0] = EmptyString;                             // empty output -> the singleton
   int status = WIFEXITED(st) ? WEXITSTATUS(st)
              : WIFSIGNALED(st) ? 128 + WTERMSIG(st) : -1;
   if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
   struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                              putcharm(status), g->sp[0]);
   g->sp[0] = word(w); }                                  // [(status.output), argv]
 return g; }

static lvm(lvm_run) {
 Pack(g);
 g = host_run(g, Sp[0], 0);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[1] = Sp[0];                                           // result over argv
 Sp += 1; Ip += 1;
 return Continue(); }

// (runt argv) -- run, TEEING: identical to (run argv), same (status . output)
// answer, but the child's stdout is relayed as it arrives instead of only at
// exit. For a caller that just reprints what it captured; see host_run's `tee`.
static lvm(lvm_runt) {
 Pack(g);
 g = host_run(g, Sp[0], 1);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[1] = Sp[0];                                           // result over argv
 Sp += 1; Ip += 1;
 return Continue(); }

// (exec argv) -> REPLACE this process with argv[0], inheriting stdio (the real
// terminal). Unlike (run argv) -- which forks, pipes the child's stdout into a
// captured string, and waits -- exec hands the tty straight to the child, so an
// INTERACTIVE program drives the terminal. On success it never returns; on a bad
// argv or a failed exec it returns an errno (or -1) fixnum, exactly like run's
// spawn-failure path. cook execs its terminal recipe steps this way (the repl,
// gdb, the qemu run targets). Marshals argv into cav like host_run, then execvp
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
 execvp(cav[0], cav);
 return ai_push(g, 1, putcharm(errno)); }                  // exec failed -> errno

static lvm(lvm_exec) {
 Pack(g);
 g = host_exec(g, Sp[0]);                                  // returns only on failure
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[1] = Sp[0];                                            // errno fixnum over argv
 Sp += 1; Ip += 1;
 return Continue(); }

// Copy the name to a C string and look it up. Factored out (ai_noinline) so the
// memcpy(&name,...) escape can't defeat lvm_getenv's tail call (cf. call_open).
ai_noinline static char const *host_getenv(struct ai_str *nv) {
 char name[4096];
 if (nv->len >= sizeof name) return NULL;
 memcpy(name, nv->bytes, nv->len);
 name[nv->len] = 0;
 return getenv(name); }

// (getenv name) -> string, or nil if unset / misused. nil = absent, not an
// error; the run fixnum-error convention does not apply here.
static lvm(lvm_getenv) {
 char const *v = ai_strp(Sp[0]) ? host_getenv((struct ai_str*) Sp[0]) : NULL;
 if (!v) { Sp[0] = ZeroPoint; Ip += 1; return Continue(); }
 Pack(g);
 if (!ai_ok(g = ai_strof(g, v))) return ghelp(g);
 Unpack(g);
 Sp[1] = Sp[0];
 Sp += 1; Ip += 1;
 return Continue(); }

// (getpid x) -> the running process id (x ignored). main.c is linked into love0
// too, so unlike the host/*.c glob nifs this one exists in the bootstrap as well.
static lvm(lvm_getpid) { return Sp[0] = putcharm(getpid()), Ip++, Continue(); }

static union u const
 nif_exit[] = {{lvm_exit}, {lvm_ret0}},
 nif_open[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_open}, {lvm_ret0}},
 nif_close[] = {{lvm_close}, {lvm_ret0}},
 nif_run[] = {{lvm_run}, {lvm_ret0}},
 nif_runt[] = {{lvm_runt}, {lvm_ret0}},
 nif_exec[] = {{lvm_exec}, {lvm_ret0}},
 nif_getenv[] = {{lvm_getenv}, {lvm_ret0}},
 nif_getpid[] = {{lvm_getpid}, {lvm_ret0}};
// Register in the ai_nifs section (drained in main below). An app thread adds its
// own nifs the same way in its OWN host/<app>.c -- auto-globbed, AI_NIF-registered,
// NO edit here or to love.c/love.h:
//   #include "love.h"                                       // the nif-writing surface
//   static lvm(lvm_foo) { ... return Sp[0] = <v>, Ip++, Continue(); }
//   static union u const nif_foo[] = {{lvm_foo}, {lvm_ret0}};  // 1-arg; curry for more
//   AI_NIF("foo", nif_foo);
AI_NIF("quit", nif_exit);
AI_NIF("open", nif_open);
AI_NIF("close", nif_close);
AI_NIF("run", nif_run);
AI_NIF("runt", nif_runt);
AI_NIF("exec", nif_exec);
AI_NIF("getenv", nif_getenv);
AI_NIF("getpid", nif_getpid);

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
  if (g && b && atol(b) > 0) g->budget = (uintptr_t) atol(b) * (1024 * 1024 / sizeof(ai_word));
  return g; }

// --bake [PATH] / --wake PATH: the heap-image snapshot (doc/snapshot.md) -- declared
// ABOVE the bootstrap split, because love0 links host/image.c too now: it bakes
// image FILES (the `bake` nif) and wakes them (--wake), which is how the self-host
// build gets a warm mooncc under love0. The .image-section self-patch stays the
// full binary's lane.
extern int image_dump(struct ai*, char const*);          // host/image.c (file I/O around love.c's codec)
extern int image_bake(struct ai*);                       // host/image.c (the self-bake)
extern struct ai *image_load(char const*);
extern uint64_t ai_baked_image[];
extern uintptr_t ai_baked_image_len;

#ifdef GL_BOOTSTRAP
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
 ;
static char const tests0[] =
#include "tests0.h"
 ;
static char const runner[] = "(reads (tap (s2cl tests)))";   // the stream shell (love/bao.l) drinks the baked corpus
// the MODULE sources, name-keyed: registered in the source library (ai_lib_, love.h)
// and loaded by `use` -- the loader wraps each in its own layer, leave registers it,
// the splice serves the bare names. bao/rng/kanren carry no brackets of their own now.
static char const src0_bao[] =
#include "bao0.h"
 ;
static char const src0_rng[] =
#include "rng0.h"
 ;
static char const src0_kanren[] =
#include "kanren0.h"
 ;
static char const src0_uu[] =
#include "uu0.h"
 ;
static char const src0_coin[] =
#include "coin0.h"
 ;
static char const src0_q[] =
#include "q0.h"
 ;
static char const src0_post[] =
#include "post0.h"
 ;
// holo with BOTH cross backends (x64 + arm64), one entry -- the corpus's cross-arch
// asserts run under both of love0's compilers.
static char const src0_holo[] =
#include "holo0.h"
#include "x640.h"
#include "arm640.h"
 ;

// With args, run the build tool (lcat / gen_data) through the CLI driver.
// With no args, self-test: eval prel, load bao (the shell core) as a module, and run
// the baked corpus via c0, then bootstrap the self-hosted ev (egg) and run the corpus
// again through it.
static struct ai *boot(struct ai *g, bool argp) {
  g = ai_lib_(g, "bao", src0_bao);                     // the source library: both lanes load bao by name;
  g = ai_lib_(g, "rng", src0_rng);                     //   BOTH lanes get the whole registry -- a build tool's (use 'x)
  g = ai_lib_(g, "kanren", src0_kanren);               //   (the mooncc cat's (use 'holo)) resolves the same as the
  g = ai_lib_(g, "uu", src0_uu);                       //   self-test's; an unused entry costs a registration, nothing more
  g = ai_lib_(g, "coin", src0_coin);
  g = ai_lib_(g, "q", src0_q);
  g = ai_lib_(g, "post", src0_post);                   //   post rides love0 now: its splice serves revcat/parse/bake bare,
  g = ai_lib_(g, "holo", src0_holo);                   //   which the mooncc cat's cpp/gen read (the self-host build lane)
  if (argp) {                                          // a build tool (lcat etc.): bake prel + bao FIRST so the CLI's
    g = ai_evals_(g,                                   // own loader/printer (eval1/bye reach for map/jot/tap/puts/putc)
#include "p10.h"                                       // have the prel surface before they load the first file -- else
#include "prel0.h"                                     // loading prel.l ITSELF misses every prel fn its loader uses.
    "(use 'bao)"                                       // p1 goes FIRST: this lane never hatches an egg, and prel's
    "(use 'kanren)"                                    // loader folds `sound` at its own compile. kanren before post,
    "(use 'post)"                                      //   reads unify/ufail bare; post serves revcat/parse/bake
    "(: verbs ())"                                     // the CLI's verb rail reads `verbs`: bound-empty = no verbs, quietly
    );
    return ai_evals_(g, cli); }
  g = ai_strof(g, tests0);                            // the baked corpus, as a string
  struct ai_def td[] = {{"tests", ai_pop1(g)}};
  g = ai_defn(g, td, countof(td));
  g = ai_evals_(g,                                    // p1 FIRST: prel's loader reads `sound`, and a
#include "p10.h"                                      // global folds at its reader's compile, so the
                                                      // reader in love has to exist before prel compiles
#include "prel0.h"                                    // prel, compiled by c0
  );
  g = ai_evals_(g,
    "(use 'bao)"                                       // bao (the shell core): loaded, registered, spliced
    "(use 'holo)");                                    // the assembler service: load + register..
  g = ai_unsplice_(g);                                 //   ..and the C unsplice keeps it non-ambient, like the host
  g = ai_evals_(g,
    "(use 'uu) (: uu (from 'uu))"                      // the library layers, all by name in the old eval order (uu's
    "(use 'coin)"                                      //   one-name surface rebinds like the host); every layer, splice
    "(use 'rng)"                                       //   and registry entry persists across the egg warm below, so one
    "(use 'q)"                                         //   load serves both corpus passes
    "(use 'kanren)"
  );
  g = ai_evals_(g, "(: (s2cl s) ((: (g i) (? (< i (tally s)) (link (peep s i 0) (g (+ 1 i))))) 0))");   // string -> charlist, for the runner
  g = ai_evals_(g, runner);                           // pass 1: corpus via ev = the c0 nif
  g = ai_egg_(g,                                      // bootstrap: install the self-hosted ev
#include "egg0.h"
    ,
#include "p10.h"
    ,
#include "prel0.h"
    ,
#include "ev0.h"
    );
  return ai_evals_(g, runner); }                      // pass 2: corpus via the self-hosted ev

#else
// the full love: raw terminal mode for the interactive REPL (love0 never needs
// it -- a build tool / self-test is non-interactive); the CLI driver is the
// canonicalized lcat header.
#if defined(__x86_64__) || defined(__aarch64__)
#define AI_GLAZED 1                                      // the native JIT exists on this arch
#endif
static struct termios saved_termios;
static void restore_termios(void) {
  tcsetattr(STDIN_FILENO, TCSANOW, &saved_termios); }

static void raw_mode(void) {
  tcgetattr(STDIN_FILENO, &saved_termios);
  atexit(restore_termios);                 // restore on normal exit
  struct termios raw = saved_termios;
  raw.c_lflag &= ~(ICANON | ECHO | ISIG | IEXTEN);  // no line buffering/echo
  raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
  raw.c_cc[VMIN] = 1;                      // block for one byte
  raw.c_cc[VTIME] = 0;
  tcsetattr(STDIN_FILENO, TCSANOW, &raw); }
  // c_oflag is left alone, so '\n' on output still becomes CR-LF.

static char const cli[] =
#include "cli.h"
 ;

// `--bake` boots fully, then lays the post-warm image back into the binary's OWN
// .image section (host/image.c's copy + patch + atomic-rename -- no objcopy,
// ETXTBSY-proof) and exits; `--bake PATH` writes a plain image file instead (the
// debug/inspection lane). `--wake PATH` boots from an image file (any mismatch
// falls back to a normal egg boot). Opt-in flags; a normal run is the same code path.
// The baked post-boot image: a reserve in its own .image section (host/image.c), filled by
// `love --bake` (the binary boots, snapshots itself, and lays the result back into its own body).
// Loaded at startup when its magic validates; else a normal egg boot.
// the post-warm dispatch (shared by boot() and the --wake path, which skips the warm).
static struct ai *run_program(struct ai *g, bool argp, bool replp) {
  // THE SESSION LAYER. Boot is over; from here the base (orth -- prel/ev, the nifs,
  // every module the frontend warmed) is READ-ONLY, and it is read-only for the
  // plainest possible reason: it is never the HEAD again. lvm_defglob writes
  // A(g->book) and nothing else, so a top-level definition -- a script's, a repl
  // line's, the corpus's -- lands here instead of in the base. Reads still walk
  // down (bookget, head-first), so prel resolves exactly as before.
  //
  // Pushed here because this is where boot() and the --wake path converge, so both
  // get it; and it is never popped, because its lifetime IS the session. That is
  // what keeps a CATTED app working: lux's eight files, the kore cat's fifteen and
  // the whole test corpus each arrive as ONE stream, so they share this layer and
  // the cross-file leaking they are built on (crew/lux/core.l's "every binding
  // LEAKS ... so the other files see this vocabulary") still resolves.
  //
  // --bake exits before run_program, so the image carries the base with no session
  // layer on top; each woken session pushes its own. C-side: enter is a mopped nom
  // now, and a stashless layer is exactly what a session is.
  g = ai_layer_(g);
#ifdef AI_GLAZED
  // LOVE_NO_GLAZE: a pure-interpreter session -- ev back to base-ev (kept in the glaze
  // module book) and the natjit creation hook cleared. The forensics twin of LOVE_NO_IMAGE.
  // Checked here, the convergence of the egg-boot and image-wake paths: a body-less
  // top-level : pins even where the book nom is sealed away (an image). --bake never
  // sees it -- the knob governs a session, not the baked artifact.
  if (getenv("LOVE_NO_GLAZE")) g = ai_evals_(g, "(: ev (glaze 'base-ev) natjit ())");
#endif
  // the ARGV[0] DOOR of the verb rail (love/cli.l has the positional door): when the
  // binary was invoked under a verb's name -- a `seed` symlink onto the dist artifact
  // -- that verb fires on the args, even at argc 1 (a bare `seed` wants its usage),
  // which is exactly where the cli never runs. `verbs` is pinned () by every boot, so
  // a plain binary walks an empty table and falls straight through; only a bake whose
  // cat rebound the table (crew/seed/up.l) ever dispatches here.
  g = ai_evals_(g,
    "(: s (cap cmdline) n (tally s)"
    "   (sx w) (: m (tally w)"                       // s ends in w, whole or at a / boundary
    "      (? (< n m) ()"
    "         (: (go i) (? (= i m) 1 (? (= (s (+ (- n m) i)) (w i)) (go (+ i 1)) ()))"
    "            (? (go 0) (? (= n m) 1 (= (s (- (- n m) 1)) 47)) ()))))"
    "   (fnd t) (? (two? t) (? (sx (cap (cap t))) (cap (cup (cap t))) (fnd (cup t))) ())"
    "   f (fnd verbs)"
    "   (? f (: _ (f (cup cmdline)) (quit 0)) 0))");
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
static char const src_post[] =
#include "post.h"
 ;
static char const src_uu[] =
#include "uu.h"
 ;
static char const src_bao[] =
#include "bao.h"
 ;
// holo, the crew/holo/ assembler: ONE entry = the arch-neutral core plus the NATIVE
// backend (C string concatenation; the glaze emits for the running arch only --
// mooncc's cat joins the cross backends at its own build, and love0 bakes x64+arm64
// so the corpus's cross-arch asserts run under both its compilers).
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

// bake: NULL = no snapshot; "" = --bake (patch the binary's own .image); else --bake PATH (write an image file).
static struct ai *boot(struct ai *g, bool argp, char const *bake) {
  bool replp = !argp && isatty(STDIN_FILENO);
  if (replp) raw_mode();
  g = ai_lib_(g, "coin", src_coin);
  g = ai_lib_(g, "rng", src_rng);
  g = ai_lib_(g, "q", src_q);
  g = ai_lib_(g, "kanren", src_kanren);
  g = ai_lib_(g, "post", src_post);
  g = ai_lib_(g, "uu", src_uu);
  g = ai_lib_(g, "bao", src_bao);
  g = ai_lib_(g, "holo", src_holo);
  g = ai_egg_(g,
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    ,
#include "ev.h"
    );
  g = ai_evals_(g,
    "(use 'coin)"                                        // the library layers, ALL modules now, in the old eval order: coin
    "(use 'rng)"                                         //   (ring/monoid over the C coin lane), rng (the random stream), q
    "(use 'q)"                                           //   (rationals), then kanren (unification) -- registered BEFORE post,
    "(use 'kanren)"                                      //   whose overlay half reads subst through the registry
    "(use 'post)"                                        // the parser combinators + the ev-seam overlay: the accessors rebind
    "(: parse (from 'post) overlay (from 'post)"         //   to the union (every reach is by key), and the ev HOOK lands in
    "   ev ((from 'post 'ov-hook) ev))"                  //   ORTH here -- a module layer cannot write it, the boot can
    "(use 'uu)"                                          // uu's NbE kernel: (: uu (from 'uu)) keeps the one-name surface --
    "(: uu (from 'uu))"                                  //   the corpus + an overlay reach (uu 'vof) through it
    "(use 'holo)"                                        // the crew/holo/ assembler, a post-egg language SERVICE: load + register,
  );                                                     //   then the C unsplice below keeps it NON-AMBIENT -- (use 'holo)
  g = ai_unsplice_(g);                                   //   splices it, (from 'holo 'assemble) probes it. a test that wants a
                                                         //   cross backend joins it at runtime ((use 'holo) <backend.l> -- the
                                                         //   test_glaze/test_raw_arm64 recipes), mooncc's cat joins ALL of them
  g = ai_evals_(g,
    "(use 'bao)"                                         // the shell core: loaded, registered, spliced (read/reads/welp/wrap bare)
    "(: verbs ())"                                       // the CLI's verb rail reads `verbs`: bound-EMPTY here, so a plain binary
  );                                                     //   answers no verbs quietly; a dist bake's cat rebinds it with the table
#ifdef AI_GLAZED
  g = ai_evals_(g,
      "(use 'holo)"
#include "emit.h"
#include "auto.h"
#include "gexport.h"
#include "hook.h"
      );
  g = ai_unsplice_(g);
#endif

  if (bake) {                                            // --bake: snapshot the post-warm heap, then exit
    // LOVE_BAKE_LOAD: read-eval one more .l file before the snapshot -- the dist
    // artifact's door (crew/build.mk): the crew cats + the verb table go in WARM,
    // ahead of the same cache-empty + seal every bake gets, and the image still
    // carries no session layer. a raise in the cat is helpless here (no shell
    // help), so a broken cat is a LOUD failed bake, never a quiet artifact.
    char const *xtra = getenv("LOVE_BAKE_LOAD");
    if (xtra) {
      char xb[4352];
      snprintf(xb, sizeof xb,
        "(: q (open \"%s\" \"r\")"
        " (? q (reads q) (: _ (say err \"love: --bake: cannot open %s\") _ (put err 10) (quit 1))))",
        xtra, xtra);
      g = ai_evals_(g, xb); }
#ifdef AI_GLAZED
    // auto.l's self-tests ran auto-ev, filling the `memo` compile cache with native nif
    // closures (ap = a W^X mmap addr) that can't be serialized. Empty it: the image boots
    // with a clean cache (natives JIT lazily on the loaded runtime's first ev, as designed).
    g = ai_evals_(g, "(: c ((peep book 'glaze 0) 'cache) (map (\\ k (pull c k 0)) (keys c)))");
#endif
    // HIDE the raw machine-code-execution seam from USERS (who boot this image): the glaze folded
    // `nif` into its closures, so pulling it off the book is safe. nif/nifx off, then seal `book`.
    // The no-image dev/test binary keeps them (egg.l defers book-removal) as the test knob.
    g = ai_evals_(g, "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) (pull book 'book 0))");
    int rc = *bake ? image_dump(g, bake) : image_bake(g);
    if (rc) fprintf(stderr, "love: bake failed (rc=%d)\n", rc);
    exit(rc ? 1 : 0); }
  return run_program(g, argp, replp); }
#endif

int main(int argc, char const **argv) {
  struct ai *g = NULL;
  // --bake [PATH] / --wake PATH must lead the args; strip them (keep argv[0]).
  // Both lanes now: love0 links host/image.c too, so it wakes an image FILE
  // (its own mooncc0.image bake -- the self-host build's ~ms compiler starts);
  // --bake (the self-patch) stays host-only (love0 lays no .image section rule,
  // and its file bakes ride the `bake` nif from -e).
  char const *image_load_path = NULL, *bake = NULL; // see boot(): "" = self-bake, a path = image file
#ifndef GL_BOOTSTRAP
  if (argc >= 2 && !strcmp(argv[1], "--bake")) {
   if (argc >= 3) bake = argv[2], argv[2] = argv[0], argv += 2, argc -= 2;
   else bake = "", argv[1] = argv[0], argv += 1, argc -= 1; }
  else
#endif
  if (argc >= 3 && !strcmp(argv[1], "--wake"))
   image_load_path = argv[2], argv[2] = argv[0], argv += 2, argc -= 2;
  if (image_load_path && !(g = image_load(image_load_path))) image_load_path = NULL;   // NULL -> normal boot
  // AUTO-LOAD: with no image flag, wake the image baked into the binary's own .image section, so a
  // plain `love` is glazed-by-default at ~4 ms cold start instead of the ~230 ms egg eval. Opt out with
  // LOVE_NO_IMAGE (the bench does, to control glazed-vs-interp itself). An EMPTY value is nothing
  // (unset) -- so a recipe under the Makefile's blanket `export LOVE_NO_IMAGE := 1` can hand ONE
  // command its image back with the sh idiom `LOVE_NO_IMAGE= cmd` (the dist artifact running as
  // $(CC): its mooncc verb lives in the baked image, and an egg boot would read "mooncc" as a
  // filename). Any problem -- unbaked, stale, truncated -- makes the load return NULL, so we fall
  // through to the normal egg boot. Never wrong.
  // (love0's reserve is 2 words and never baked, so its auto-load always falls through.)
  char const *noimg = getenv("LOVE_NO_IMAGE");
  if (!g && !bake && !(noimg && *noimg)) {
   if (ai_baked_image_len && (g = ai_image_load(ai_baked_image, ai_baked_image_len)))
    image_load_path = "<baked>"; }                                     // a loaded image is the booted state: skip the egg warm
  if (!g) g = ai_ini();
  g = env_budget(g);                               // the LOVE_BUDGET_MB cap, on whichever g won (fresh or woken image)
  bool argp = argc > 1;
  // The WHOLE C argv (incl. argv[0]/program name): cli.l drops the head for its own
  // use, while `cmdline` keeps the full list, pinned for user visibility.
  char const **av = argv;
  int ac = argc;
  for (; *av; g = ai_strof(g, *av++));
  for (g = ai_push(g, 1, ai_nil); ac--; g = gxr(g));
  if (ai_ok(g)) {
    ai_word full_argv = ai_pop1(g);                // shared by `argv` and `cmdline`
    // the static nifs (exit/open/close/run/getenv + any host/*.c app nifs) come
    // from the ai_nifs section; argv/cmdline are runtime values, defined here.
    g = ai_defn(g, __start_ai_nifs, __stop_ai_nifs - __start_ai_nifs);
    struct ai_def d[] = {{"argv", full_argv}, {"cmdline", full_argv}};
    g = ai_defn(g, d, countof(d));      // re-pins the host nifs (live addresses) into the loaded book too
#ifdef GL_BOOTSTRAP
    if (!image_load_path) g = boot(g, argp);
    else g = ai_evals_(ai_layer_(g), cli);   // woken: the image carries the warm base; push the session layer, run the CLI
#else
    if (!image_load_path) g = boot(g, argp, bake);
    else {              // --wake: skip the egg warm, dispatch straight to the program
      bool replp = !argp && isatty(STDIN_FILENO);
      if (replp) raw_mode();
      g = run_program(g, argp, replp); }
#endif
  }
  switch (ai_code_of(g)) {
   default: break;
   case ai_status_scare:               // the honest face: ";; a b" when the scare
    if (!ai_scare_face_(g))            // said something; bare (no data) = oom
     fprintf(stderr, ";; oom@len=%ld\n", (long) ai_core_of(g)->len);
    break; }
  return ai_fin(g); }
