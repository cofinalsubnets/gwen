// host/posix.c -- the POSIX surface, in one place: process (spawn/reap/wait/
// signal, the pid-1 supervisor's primitives and the shell's job control), fs
// effects and values (stat/readdir/rename/chmod/..), the environment, pipes and
// raw-fd plumbing, and the pty wrapper (bao's rlwrap/debugger muscle). Host-only,
// auto-globbed + AI_NIF-registered (no love.c/love.h/main.c edit). The
// conventions, kept throughout:
//   effect ops answer () ok | a POSITIVE errno | EINVAL misuse
//   value ops answer the value | () absence (or a NEGATIVE -errno where a
//   pid/fd/offset result must stay tellable from failure)
//
// argv marshaling mirrors host_exec (main.c): a chain of strings -> a NUL-
// terminated char** in the uncommitted heap gap at Hp (GC-invisible, holds no l
// pointers, consumed before any further alloc), valid across the fork -- ONE
// copy here (argv_marshal) shared by spawn, spawnio and mind; main.c keeps its
// own (main.c is CORE, an app file can't reach in).
#define _GNU_SOURCE     // unshare / CLONE_* (newns), posix_openpt/grantpt/unlockpt/ptsname
#include "love.h"
#include <unistd.h>     // fork execvp _exit read close getuid/getgid symlink readlink chown
#include <stdio.h>      // fflush, rename
#include <stdlib.h>     // setenv/unsetenv, posix_openpt grantpt unlockpt ptsname
#include <string.h>     // memcpy
#include <errno.h>
#include <signal.h>     // sigprocmask kill, SIGCHLD/SIGTERM (sigfd)
#include <fcntl.h>      // open, O_*, AT_FDCWD, FD_CLOEXEC
#include <sys/stat.h>   // mkdir, stat, chmod, umask, utimensat UTIME_NOW
#include <sys/wait.h>   // waitpid, WIF* (proc_status)
#include <sys/ioctl.h>  // ioctl TIOCSCTTY TIOC[GS]WINSZ struct winsize
#include <termios.h>    // tcgetattr tcsetattr ECHO TCSANOW (ptyecho, raw)
#include <dirent.h>     // opendir/readdir/closedir
#if defined(__linux__)
#include <sys/signalfd.h>   // signalfd, struct signalfd_siginfo (Linux only)
#include <sys/mount.h>      // mount(2)
#include <sched.h>          // unshare, CLONE_NEWUSER/NEWNS (newns)
#endif

// A wait(2) status word -> the value a reaper hands back: the exit code, or
// 128+signal for a signalled death (the shell convention), or -1 for the
// (shouldn't-happen) neither case. The way host_run (main.c) decodes it -- the
// one copy every reaper here shares, so they agree on what an exit code MEANS.
static inline int proc_status(int st) {
 return WIFEXITED(st) ? WEXITSTATUS(st)
       : WIFSIGNALED(st) ? 128 + WTERMSIG(st) : -1; }

// Pull a live OS fd out of a port arg, or -1 if it isn't a port. Same inline
// "is x a port" as main.c's lvm_close: a heap word whose discriminator is the
// port vtable. A closed port carries the -3 sentinel; we hand that straight back
// to the syscall, which fails with EBADF -- the honest answer.
static intptr_t port_fd(ai_word x) {
 if ((x & 1) == 0 && ((union u*) x)->ap == lvm_port_io)
    return getcharm(((struct ai_io*) x)->fd);
 return -1; }

// copy a love string into a NUL-terminated C buffer; false on non-string / too long.
static bool str_cbuf(ai_word x, char *buf, size_t cap) {
 if (!ai_strp(x)) return false;
 struct ai_str *s = (struct ai_str*) x;
 if ((size_t) s->len >= cap) return false;
 memcpy(buf, s->bytes, s->len);
 buf[s->len] = 0;
 return true; }

// The argv marshal: the chain of strings at g->sp[0] -> argc+1 char** + the
// NUL-joined byte blob, laid in the uncommitted heap gap at Hp -- GC-invisible,
// holds no l pointers, valid across a fork, consumed (execvp'd) before any
// further allocation. Called with g Packed. Two failure faces: a misuse (non-
// string element / empty argv) pushes putcharm(-1) and leaves *cavp NULL (the
// caller returns g as-is, the -1 already the net value); OOM returns !ok g
// (*cavp NULL too, so `if (!*cavp) return g` covers both).
static struct ai *argv_marshal(struct ai *g, char ***cavp) {
 *cavp = NULL;
 ai_word argv = g->sp[0];
 intptr_t argc = 0; uintptr_t total = 0;
 for (ai_word p = argv; chainp(p); p = B(p)) {
  if (!ai_strp(A(p))) return ai_push(g, 1, putcharm(-1));   // misuse: non-string argv
  argc++, total += len(A(p)) + 1; }
 if (!argc) return ai_push(g, 1, putcharm(-1));              // empty argv
 if (!ai_ok(g = ai_have(g, (uintptr_t) argc + 1 + b2w(total)))) return g;
 argv = g->sp[0];                                            // re-root post-ai_have
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
 *cavp = cav;
 return g; }

// --- the supervisor pair: spawn without waiting, reap any dead child ------------
// (spawn argv)  -> child pid (a fixnum) | a NEGATIVE fixnum (-errno / -1 misuse)
// (hear _)      -> (pid . status) of one reaped child
//                | ()                 none pending
//                | a NEGATIVE fixnum  (-errno, e.g. -ECHILD: no children left)
// init/init.l drives REAL processes with these plus the generic `still` (kill):
// spawn returns a pid to track, hear is the SIGCHLD core (poll it, map the pid
// back to a unit, restart per policy). On a real pid1 hear also collects
// reparented orphans (waitpid(-1)).

// the child side of the ignore dance: a disposition set to SIG_IGN SURVIVES exec,
// so a shell that ignores the job-control signals must undo that in every child
// between fork and exec -- or ^C could never kill anything it launches.
static void sig_dfl_job(void) {
 signal(SIGINT, SIG_DFL); signal(SIGQUIT, SIG_DFL);
 signal(SIGTSTP, SIG_DFL); signal(SIGTTIN, SIG_DFL); signal(SIGTTOU, SIG_DFL); }

// (spawn argv) -> the child pid, or a negated errno (negative, so a caller tells
// a pid (positive) from a failure (negative) without a second value). fork +
// execvp; the parent returns immediately -- NON-BLOCKING, unlike run (waits +
// captures) and exec (replaces in place). The child inherits init's stdio (a real
// pid1 redirects to the journal); a failed exec _exit(127)s, seen by the next hear.
ai_noinline static struct ai *host_spawn(struct ai *g) {
 char **cav;
 g = argv_marshal(g, &cav);
 if (!cav) return g;                                         // misuse pushed -1, or OOM
 fflush(NULL);                                               // flush now, not twice in the child
 pid_t pid = fork();
 if (pid < 0) return ai_push(g, 1, putcharm(-errno));
 if (!pid) { sig_dfl_job(); execvp(cav[0], cav); _exit(127); }   // child: default signals, exec or die 127
 return ai_push(g, 1, putcharm(pid)); }                      // parent: the live pid

static lvm(lvm_spawn) {
 Pack(g);
 g = host_spawn(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[1] = Sp[0];                                              // pid over argv
 Sp += 1; Ip += 1;
 return Continue(); }

// (hear _) -> (pid . status) of one reaped child, () if none are pending, or a
// negated errno (e.g. -ECHILD when no children remain). The pid is the CAR so the
// supervisor maps it back to a unit; status is proc_status (exit code / 128+sig).
// waitpid(-1, WNOHANG) reaps ANY child -- incl. reparented orphans on a real pid1.
// The arg is a dummy (ignored), so a bare (hear) curries; call it (hear 0).
ai_noinline static struct ai *host_reapany(struct ai *g) {
 int st;
 pid_t r = waitpid(-1, &st, WNOHANG);
 if (r == 0) { g->sp[0] = ZeroPoint; return g; }            // none pending -> the real () (not charm 0)
 if (r < 0)  { g->sp[0] = putcharm(-errno); return g; }     // error (ECHILD = none alive)
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
 struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm(r), putcharm(proc_status(st)));
 g->sp[0] = word(w);
 return g; }

static lvm(lvm_reapany) {
 Pack(g);
 g = host_reapany(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Ip += 1; return Continue(); }

// --- the signal perceive source (Linux signalfd) --------------------------------
// (sigfd sigs)  -> a PORT over a signalfd watching `sigs` (a list of signal numbers;
//                  a non-list keeps the supervisor default SIGCHLD + SIGTERM), those signals
//                  first BLOCKED (sigprocmask) so they QUEUE to the fd instead of
//                  their default disposition -- SIGCHLD's discard, SIGTERM's KILL.
//                  That queuing is exactly what turns a TERM into a graceful EVENT,
//                  not a death. () on failure. SIGINT is left unblocked so ^C bails.
// (sigtake port) -> (signo . pid) of ONE pending signal, or () if none ready.
// The supervisor PARKS with the core `(await sig)` (cooperative -- the scheduler
// merges the sigfd with a heartbeat task's timer in one ai_wait_fds, the {nic, clock}
// story for {signals, clock}), then sigtake reads the record. SIGCHLD coalesces, so a
// 'chld wake still loops `hear` to harvest every zombie.
#if defined(__linux__)
// the arg may be a LIST of signal numbers to watch; anything else (the dummy-0
// convention) keeps the supervisor's classic pair, SIGCHLD + SIGTERM.
ai_noinline static struct ai *host_sigfd(struct ai *g) {
 sigset_t m;
 sigemptyset(&m);
 ai_word a = g->sp[0];
 if (chainp(a))
  for (ai_word p = a; chainp(p); p = B(p)) {
  if (A(p) & 1) sigaddset(&m, (int) getcharm(A(p))); }
 else { sigaddset(&m, SIGCHLD); sigaddset(&m, SIGTERM); }
 if (sigprocmask(SIG_BLOCK, &m, NULL)) return g->sp[0] = ZeroPoint, g;
 int fd = signalfd(-1, &m, SFD_NONBLOCK | SFD_CLOEXEC);
 if (fd < 0) return g->sp[0] = ZeroPoint, g;
 struct ai *r = ai_io_alloc(g, fd);
 if (!ai_ok(r)) return close(fd), g->sp[0] = ZeroPoint, g;    // OOM -> nil (cf. net.c lvm_listen)
 g = r;
 return g->sp[1] = g->sp[0], g->sp += 1, g; }                 // port over the dummy arg
static lvm(lvm_sigfd) {
 Pack(g); g = host_sigfd(g); Unpack(g);     // host_sigfd folds every failure to nil, so no ghelp
 return Ip++, Continue(); }

// read one signalfd_siginfo (non-blocking) into (signo . pid). signo is the raw
// number (Linux: SIGCHLD 17, SIGTERM 15); pid is ssi_pid (the dead child on SIGCHLD).
ai_noinline static struct ai *host_sigtake(struct ai *g, int fd) {
 struct signalfd_siginfo si;
 ssize_t n = (fd >= 0) ? read(fd, &si, sizeof si) : -1;
 if (n != (ssize_t) sizeof si) { g->sp[0] = ZeroPoint; return g; }   // none ready -> the real () (not charm 0)
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
 struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                putcharm((intptr_t) si.ssi_signo),
                                putcharm((intptr_t) si.ssi_pid));
 g->sp[0] = word(w);
 return g; }

static lvm(lvm_sigtake) {
 int fd = (int) port_fd(Sp[0]);
 if (fd < 0) { Sp[0] = ZeroPoint; return Ip++, Continue(); }
 Pack(g);
 g = host_sigtake(g, fd);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Ip += 1; return Continue(); }
#else
// signalfd is Linux-only; keep the names present (so init.l loads) but inert.
static lvm(lvm_sigfd)   { Sp[0] = ZeroPoint; return Ip++, Continue(); }
static lvm(lvm_sigtake) { Sp[0] = ZeroPoint; return Ip++, Continue(); }
#endif

// --- foreground job control + cwd (the muscle a real shell needs) ---------------
// (wait pid)   -> BLOCK until pid exits OR STOPS: an exit is its proc_status (exit /
//                 128+sig), a stop (^Z: SIGTSTP/SIGSTOP) is 256 + the stopping signal
//                 -- a charm above every exit status, so a shell tells "stopped, job
//                 it" (< 255 st) from "done". -errno on failure. the foreground wait:
//                 spawn (inherited stdio) then wait, so a command owns the terminal
//                 and the prompt returns only when it is done or parked.
// (signal sig disp) -> sigaction: disp 0 = default, 1 = ignore. () | positive errno |
//                 EINVAL misuse (the effect convention). the shell ignores INT/QUIT/
//                 TSTP so the tty's ^C/^Z reach only the foreground child; spawn's
//                 child side resets them (an IGNORED disposition survives exec).
// (chdir path) -> () ok | -errno | -1 misuse. the `cd` builtin.
// (cwd _)      -> the current directory as a string, or () on failure. for the prompt.
// The syscall body lives in an ai_noinline helper so the lvm_ wrapper stays a pure tail-jump (no ret):
// the syscall + any stack buffer would otherwise block the sibcall to Continue() and trip `make vmret`.
ai_noinline static ai_word host_waitpid(ai_word arg) {
 intptr_t pid = (arg & 1) ? getcharm(arg) : 0;
 int st;
 pid_t r;
 do r = waitpid((pid_t) pid, &st, WUNTRACED); while (r < 0 && errno == EINTR);
 if (r < 0) return putcharm(-errno);
 if (WIFSTOPPED(st)) return putcharm(256 + WSTOPSIG(st));
 return putcharm(proc_status(st)); }
static lvm(lvm_waitpid) { Sp[0] = host_waitpid(Sp[0]); return Ip++, Continue(); }

ai_noinline static ai_word host_posix_signal(ai_word sigw, ai_word dw) {
 if (!(sigw & 1) || !(dw & 1)) return putcharm(EINVAL);
 struct sigaction sa;
 memset(&sa, 0, sizeof sa);
 sa.sa_handler = getcharm(dw) ? SIG_IGN : SIG_DFL;
 sigemptyset(&sa.sa_mask);
 return sigaction((int) getcharm(sigw), &sa, NULL) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_signal) {
 Sp[1] = host_posix_signal(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }


ai_noinline static ai_word host_chdir(ai_word arg) {
 char buf[4096];
 if (!str_cbuf(arg, buf, sizeof buf)) return putcharm(-1);
 return chdir(buf) ? putcharm(-errno) : ZeroPoint; }
static lvm(lvm_chdir) { Sp[0] = host_chdir(Sp[0]); return Ip++, Continue(); }

ai_noinline static struct ai *host_cwd(struct ai *g) {
 char buf[4096];
 if (!getcwd(buf, sizeof buf)) return g->sp[0] = ZeroPoint, g;
 if (!ai_ok(g = ai_strof(g, buf))) return g;            // OOM -> !ok, wrapper ghelps
 return g->sp[1] = g->sp[0], g->sp += 1, g; }           // cwd string over the dummy arg
static lvm(lvm_cwd) {
 Pack(g); g = host_cwd(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

// --- pipes + redirects (the fd plumbing a shell pipeline needs) ------------------
// (pipe _)       -> (readfd . writefd) of a fresh pipe (raw fds), or -errno.
// (openfd path m) -> a raw fd opening `path`: m 0 = read, 1 = write/create/trunc,
//                   2 = write/create/append. -errno on failure, -1 on a bad path.
// (spawnio argv in out err closes pg fg) -> pid. fork; in the child: the JOB-CONTROL
//                   dance first -- pg < 0 stays in the parent's pgrp (the legacy /
//                   non-tty lane), pg = 0 LEADS a fresh process group, pg > 0 JOINS
//                   that group (pipeline members join their stage-0 leader) -- and fg
//                   nonzero hands the child's group the TERMINAL (tcsetpgrp on fd 0
//                   BEFORE the dup2s, TTOU ignored for the handoff; the parent
//                   setpgids too, closing the race). A job in its OWN pgrp is what
//                   makes ^Z real: a stop signal to an ORPHANED group is discarded
//                   by POSIX, and the shell's own group is exactly that under a
//                   nested session. then dup2 `in`/`out`/`err` (each >=0) onto fd
//                   0/1/2, close every fd in the list `closes` (the pipe ends the
//                   child must not leak, so a downstream reader sees EOF), reset the
//                   job signals, execvp. The parent keeps its fds and closes the
//                   pipe ends itself with fdclose. -errno on a fork/marshal failure.
// (ttyfg pg)     -> give the terminal (fd 0) to process group pg; pg <= 0 takes it
//                   BACK to the caller's own group (the shell reclaiming the tty
//                   after a foreground job ends or stops). () | positive errno.
// (fdclose fd)    -> close a raw fd (the parent's pipe ends). () ok | -errno.
ai_noinline static struct ai *host_pipe(struct ai *g) {
 int fds[2];
 if (pipe(fds)) return g->sp[0] = putcharm(-errno), g;
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return close(fds[0]), close(fds[1]), g;   // OOM -> !ok
 struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                putcharm(fds[0]), putcharm(fds[1]));
 return g->sp[0] = word(w), g; }
static lvm(lvm_pipe) {
 Pack(g); g = host_pipe(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

static lvm(lvm_openfd) {
 char buf[4096];
 if (!str_cbuf(Sp[0], buf, sizeof buf)) { Sp[1] = putcharm(-1); Sp += 1; return Ip++, Continue(); }
 intptr_t m = (Sp[1] & 1) ? getcharm(Sp[1]) : 0;
 int flags = m == 1 ? (O_WRONLY | O_CREAT | O_TRUNC)
           : m == 2 ? (O_WRONLY | O_CREAT | O_APPEND)
           : O_RDONLY;
 int fd = open(buf, flags, 0644);
 Sp[1] = (fd < 0) ? putcharm(-errno) : putcharm(fd);
 Sp += 1; return Ip++, Continue(); }

ai_noinline static struct ai *host_spawnio(struct ai *g, int in, int out, int err,
                                            intptr_t pg, intptr_t fg) {
 char **cav;
 g = argv_marshal(g, &cav);
 if (!cav) return g;                         // misuse pushed -1, or OOM
 ai_word closes = g->sp[4];                  // re-read post-marshal (ai_have may have GC'd)
 fflush(NULL);
 pid_t pid = fork();
 if (pid < 0) return ai_push(g, 1, putcharm(-errno));
 if (!pid) {
  if (pg >= 0) {
   setpgid(0, (pid_t) pg);                     // 0 leads a fresh group, >0 joins it
   if (fg) { signal(SIGTTOU, SIG_IGN);          // the handoff, from the background
    tcsetpgrp(0, pg ? (pid_t) pg : getpid()); } }
  if (in  >= 0) dup2(in, 0);
  if (out >= 0) dup2(out, 1);
  if (err >= 0) dup2(err, 2);
  for (ai_word p = closes; chainp(p); p = B(p)) {
   intptr_t fd = getcharm(A(p));
   if (fd > 2) close((int) fd); }
  sig_dfl_job();                                // undo the shell's ignores (TTOU too)
  execvp(cav[0], cav);
  _exit(127); }
 if (pg >= 0) setpgid(pid, (pid_t) (pg ? pg : pid));   // parent side too: no race window
 return ai_push(g, 1, putcharm(pid)); }

static lvm(lvm_spawnio) {
 int in  = (Sp[1] & 1) ? (int) getcharm(Sp[1]) : -1;
 int out = (Sp[2] & 1) ? (int) getcharm(Sp[2]) : -1;
 int err = (Sp[3] & 1) ? (int) getcharm(Sp[3]) : -1;
 intptr_t pg = (Sp[5] & 1) ? getcharm(Sp[5]) : -1;
 intptr_t fg = (Sp[6] & 1) ? getcharm(Sp[6]) : 0;
 Pack(g);
 g = host_spawnio(g, in, out, err, pg, fg);  // argv at sp[0], closes at sp[4]
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[7] = Sp[0];                              // pid over the 7 args
 Sp += 7; Ip += 1;
 return Continue(); }

ai_noinline static ai_word host_posix_ttyfg(ai_word pgw) {
 pid_t pg = ((pgw & 1) && getcharm(pgw) > 0) ? (pid_t) getcharm(pgw) : getpgrp();
 return tcsetpgrp(0, pg) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_ttyfg) { Sp[0] = host_posix_ttyfg(Sp[0]); return Ip++, Continue(); }

static lvm(lvm_shutfd) {
 intptr_t fd = (Sp[0] & 1) ? getcharm(Sp[0]) : -1;
 Sp[0] = (fd >= 0 && close((int) fd)) ? putcharm(-errno) : ZeroPoint;
 return Ip++, Continue(); }

// (fdopen fd) -> a PORT over a raw fd -- pipe/openfd's other half, so love reads
// and writes its own plumbing (a command substitution drains a pipe with slurp, a
// heredoc body pours in with say). () on a non-charm / negative fd or OOM. The
// port's GC finalizer owns the fd from here: hand it over, don't fdclose it too.
ai_noinline static struct ai *host_fdopen(struct ai *g) {
 ai_word a = g->sp[0];
 intptr_t fd = (a & 1) ? getcharm(a) : -1;
 if (fd < 0) return g->sp[0] = ZeroPoint, g;
 struct ai *r = ai_io_alloc(g, (int) fd);
 if (!ai_ok(r)) return g->sp[0] = ZeroPoint, g;               // OOM -> nil (cf. sigfd)
 g = r;
 return g->sp[1] = g->sp[0], g->sp += 1, g; }                 // port over the fd arg
static lvm(lvm_fdopen) {
 Pack(g); g = host_fdopen(g); Unpack(g);     // every failure folds to nil, so no ghelp
 return Ip++, Continue(); }

// (spawnmap argv fdmap closes pg fg) -> pid | -errno. spawnio generalized: instead
// of the hardwired in/out/err triple, `fdmap` is a list of (childfd . srcfd) pairs
// applied IN ORDER in the child -- dup2(srcfd, childfd) for a charm srcfd >= 0,
// close(childfd) for () -- and each srcfd reads the fd table AS REMAPPED SO FAR,
// which is exactly the POSIX left-to-right redirection law (`>f 2>&1` maps
// ((1 . f) (2 . 1)) and the second entry sees the first's work). pg/fg and the
// closes list ride unchanged from spawnio (the job-control dance + the pipe ends
// the child must not leak). spawnio stays for its callers; this is the shell's lane.
ai_noinline static struct ai *host_spawnmap(struct ai *g, intptr_t pg, intptr_t fg) {
 char **cav;
 g = argv_marshal(g, &cav);
 if (!cav) return g;                         // misuse pushed -1, or OOM
 ai_word fdmap = g->sp[1], closes = g->sp[2];   // re-read post-marshal (ai_have may have GC'd)
 fflush(NULL);
 pid_t pid = fork();
 if (pid < 0) return ai_push(g, 1, putcharm(-errno));
 if (!pid) {
  if (pg >= 0) {
   setpgid(0, (pid_t) pg);                     // 0 leads a fresh group, >0 joins it
   if (fg) { signal(SIGTTOU, SIG_IGN);          // the handoff, from the background
    tcsetpgrp(0, pg ? (pid_t) pg : getpid()); } }
  for (ai_word p = fdmap; chainp(p); p = B(p)) {
   ai_word e = A(p);
   if (!chainp(e)) continue;
   intptr_t cfd = (A(e) & 1) ? getcharm(A(e)) : -1;
   if (cfd < 0) continue;
   ai_word sw = B(e);
   if ((sw & 1) && getcharm(sw) >= 0) dup2((int) getcharm(sw), (int) cfd);
   else close((int) cfd); }                    // () (or a negative) srcfd closes childfd
  for (ai_word p = closes; chainp(p); p = B(p)) {
   intptr_t fd = getcharm(A(p));
   if (fd > 2) close((int) fd); }
  sig_dfl_job();                                // undo the shell's ignores (TTOU too)
  execvp(cav[0], cav);
  _exit(127); }
 if (pg >= 0) setpgid(pid, (pid_t) (pg ? pg : pid));   // parent side too: no race window
 return ai_push(g, 1, putcharm(pid)); }
static lvm(lvm_spawnmap) {
 intptr_t pg = (Sp[3] & 1) ? getcharm(Sp[3]) : -1;
 intptr_t fg = (Sp[4] & 1) ? getcharm(Sp[4]) : 0;
 Pack(g);
 g = host_spawnmap(g, pg, fg);               // argv at sp[0], fdmap sp[1], closes sp[2]
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[5] = Sp[0];                              // pid over the 5 args
 Sp += 5; Ip += 1;
 return Continue(); }

// (getuid _) -> the real uid, a charm. the shell's # vs $ prompt; always succeeds.
static lvm(lvm_getuid) { Sp[0] = putcharm((intptr_t) getuid()); return Ip++, Continue(); }

// --- pid1 bringup: mount the early filesystems + cgroup dirs ----------------------
// (mkdir path mode) -> mkdir(2). () | -errno | -1 misuse. mode is octal (493 = 0755).
// also makes cgroup dirs (cgroup-v2 placement is then `open` + `say` the control file).
// (mount src tgt type) -> mount(2), flags 0 / no data (enough for proc/sysfs/tmpfs).
//   () | -errno | -1 misuse. needs privilege: run as pid1/root, or after (newns 0).
// (newns _) -> unshare a private USER+MOUNT namespace and selfmap to root-in-ns, so
//   (mount ...) works UNPRIVILEGED (the standard setgroups-deny + uid_map/gid_map).
//   () | -errno. a real pid1 skips this -- it already IS root.
// () on success, a POSITIVE errno on failure (so `!`/truthiness tells them apart --
// the pty/net convention; -errno would net falsey like the () success).
static lvm(lvm_mkdir) {
 char p[4096];
 if (!str_cbuf(Sp[0], p, sizeof p)) { Sp[1] = putcharm(EINVAL); Sp += 1; return Ip++, Continue(); }
 intptr_t mode = (Sp[1] & 1) ? getcharm(Sp[1]) : 0755;
 Sp[1] = mkdir(p, (mode_t) mode) ? putcharm(errno) : ZeroPoint;
 Sp += 1; return Ip++, Continue(); }

#if defined(__linux__)
ai_noinline static ai_word host_mount(ai_word a, ai_word b, ai_word c) {
 char src[1024], tgt[1024], typ[64];
 if (!str_cbuf(a, src, sizeof src) || !str_cbuf(b, tgt, sizeof tgt) || !str_cbuf(c, typ, sizeof typ))
  return putcharm(EINVAL);
 return mount(src, tgt, typ, 0, NULL) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_mount) { Sp[2] = host_mount(Sp[0], Sp[1], Sp[2]); Sp += 2; return Ip++, Continue(); }

static int ns_write(char const *path, char const *s) {
 int fd = open(path, O_WRONLY);
 if (fd < 0) return -1;
 ssize_t n = write(fd, s, strlen(s));
 return close(fd), (n < 0 ? -1 : 0); }
static lvm(lvm_newns) {
 long uid = (long) getuid(), gid = (long) getgid();
 if (unshare(CLONE_NEWUSER | CLONE_NEWNS)) { Sp[0] = putcharm(errno); return Ip++, Continue(); }
 char b[64];
 ns_write("/proc/self/setgroups", "deny");                       // required before gid_map
 snprintf(b, sizeof b, "0 %ld 1\n", uid); ns_write("/proc/self/uid_map", b);
 snprintf(b, sizeof b, "0 %ld 1\n", gid); ns_write("/proc/self/gid_map", b);
 Sp[0] = ZeroPoint; return Ip++, Continue(); }
#else
static lvm(lvm_mount) { Sp[2] = putcharm(ENOSYS); Sp += 2; return Ip++, Continue(); }   // Linux-only
static lvm(lvm_newns) { Sp[0] = putcharm(ENOSYS); return Ip++, Continue(); }
#endif

// --- the general POSIX fs surface (the posix_ symbol namespace; doc/posix.md L0,
// staging step 1) -- these serve any program, not just the supervisor, so their C
// symbols wear the posix_ prefix; the love names stay the plain POSIX words.
// (stat path)    -> (size mtime mode) | () -- absence (or unreadability) is nothing.
//                   size in bytes, mtime in MILLISECONDS (the (clock t) scale), mode
//                   the raw st_mode charm: kind reads off the S_IFMT bits in love
//                   ((& mode 61440): 32768 file, 16384 dir, 40960 link) and the
//                   permission bits ride along.
// (readdir path) -> the entry names, a list of strings ("." and ".." dropped), or ()
//                   on failure. NO order promised (readdir order, prepended) -- sort in love.
// (unlink path)  -> () ok | a POSITIVE errno | EINVAL misuse (the mkdir convention:
//                   an effect op nets truthy exactly when something went wrong).
// (lseek fd off whence) -> the new offset | -errno | -1 misuse (the value-op
//                   convention: negative = failure, like spawn/wait). RAW fds, the
//                   openfd lane -- NOT ports (a port's read buffer would desync
//                   under a seek). whence: 0 SET, 1 CUR, 2 END.
ai_noinline static struct ai *host_posix_stat(struct ai *g) {
 char p[4096];
 struct stat st;
 if (!str_cbuf(g->sp[0], p, sizeof p) || stat(p, &st))
  return g->sp[0] = ZeroPoint, g;                             // absent -> the real ()
#if defined(__APPLE__)
 intptr_t ms = (intptr_t) st.st_mtimespec.tv_sec * 1000 + st.st_mtimespec.tv_nsec / 1000000;
#else
 intptr_t ms = (intptr_t) st.st_mtim.tv_sec * 1000 + st.st_mtim.tv_nsec / 1000000;
#endif
 if (!ai_ok(g = ai_have(g, 3 * Width(struct ai_chain)))) return g;
 struct ai_chain *c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                putcharm((intptr_t) st.st_mode), ZeroPoint);
 c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)), putcharm(ms), word(c));
 c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
               putcharm((intptr_t) st.st_size), word(c));
 g->sp[0] = word(c);
 return g; }
static lvm(lvm_posix_stat) {
 Pack(g); g = host_posix_stat(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

ai_noinline static struct ai *host_posix_readdir(struct ai *g) {
 char p[4096];
 if (!str_cbuf(g->sp[0], p, sizeof p)) return g->sp[0] = ZeroPoint, g;
 DIR *d = opendir(p);
 if (!d) return g->sp[0] = ZeroPoint, g;
 g->sp[0] = ZeroPoint;                                        // the accumulator, over the path
 for (struct dirent *e; (e = readdir(d));) {
  if (e->d_name[0] == '.' && (!e->d_name[1] || (e->d_name[1] == '.' && !e->d_name[2])))
   continue;                                                  // "." and ".."
  if (!ai_ok(g = ai_strof(g, e->d_name))) return closedir(d), g;   // pushes: name over acc
  if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return closedir(d), g;
  struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 g->sp[0], g->sp[1]);         // (name . acc), slots re-read post-GC
  g->sp[1] = word(w);
  g->sp += 1; }                                               // pop the name
 closedir(d);
 return g; }
static lvm(lvm_posix_readdir) {
 Pack(g); g = host_posix_readdir(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

ai_noinline static ai_word host_posix_unlink(ai_word arg) {
 char p[4096];
 if (!str_cbuf(arg, p, sizeof p)) return putcharm(EINVAL);
 return unlink(p) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_unlink) { Sp[0] = host_posix_unlink(Sp[0]); return Ip++, Continue(); }

// (setenv name val) -> () | positive errno | EINVAL misuse; a NON-STRING val UNSETS
// (the absence lane: (setenv n ()) clears n from the environment).
// (environ _)       -> the environment as a list of "NAME=value" strings (the raw
//                      POSIX shape -- split at the first '=' in love; no order promised).
ai_noinline static ai_word host_posix_setenv(ai_word nw, ai_word vw) {
 char n[1024], v[4096];
 if (!str_cbuf(nw, n, sizeof n)) return putcharm(EINVAL);
 if (!ai_strp(vw)) return unsetenv(n) ? putcharm(errno) : ZeroPoint;
 if (!str_cbuf(vw, v, sizeof v)) return putcharm(EINVAL);
 return setenv(n, v, 1) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_setenv) {
 Sp[1] = host_posix_setenv(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }

extern char **environ;
ai_noinline static struct ai *host_posix_environ(struct ai *g) {
 g->sp[0] = ZeroPoint;                                        // the accumulator, over the dummy arg
 for (char **e = environ; e && *e; e++) {
  if (!ai_ok(g = ai_strof(g, *e))) return g;                  // pushes: entry over acc
  if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
  struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 g->sp[0], g->sp[1]);
  g->sp[1] = word(w);
  g->sp += 1; }
 return g; }
static lvm(lvm_posix_environ) {
 Pack(g); g = host_posix_environ(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

ai_noinline static ai_word host_posix_lseek(ai_word fdw, ai_word offw, ai_word whw) {
 if (!(fdw & 1) || !(offw & 1)) return putcharm(-1);
 int wh = (whw & 1) ? (int) getcharm(whw) : 0;
 wh = wh == 1 ? SEEK_CUR : wh == 2 ? SEEK_END : SEEK_SET;
 off_t r = lseek((int) getcharm(fdw), (off_t) getcharm(offw), wh);
 return r < 0 ? putcharm(-errno) : putcharm((intptr_t) r); }
static lvm(lvm_posix_lseek) {
 Sp[2] = host_posix_lseek(Sp[0], Sp[1], Sp[2]);
 Sp += 2; return Ip++, Continue(); }

static union u const
  nif_spawn[]   = {{lvm_spawn}, {lvm_ret0}},
  nif_reapany[] = {{lvm_reapany}, {lvm_ret0}},
  nif_sigfd[]   = {{lvm_sigfd}, {lvm_ret0}},
  nif_sigtake[] = {{lvm_sigtake}, {lvm_ret0}},
  nif_waitpid[] = {{lvm_waitpid}, {lvm_ret0}},
  nif_chdir[]   = {{lvm_chdir}, {lvm_ret0}},
  nif_cwd[]     = {{lvm_cwd}, {lvm_ret0}},
  nif_pipe[]    = {{lvm_pipe}, {lvm_ret0}},
  nif_openfd[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_openfd}, {lvm_ret0}},
  nif_spawnio[] = {{lvm_cur}, {.x = putcharm(7)}, {lvm_spawnio}, {lvm_ret0}},
  nif_shutfd[]  = {{lvm_shutfd}, {lvm_ret0}},
  nif_fdopen[]  = {{lvm_fdopen}, {lvm_ret0}},
  nif_spawnmap[] = {{lvm_cur}, {.x = putcharm(5)}, {lvm_spawnmap}, {lvm_ret0}},
  nif_getuid[]  = {{lvm_getuid}, {lvm_ret0}},
  nif_mkdir[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_mkdir}, {lvm_ret0}},
  nif_mount[]   = {{lvm_cur}, {.x = putcharm(3)}, {lvm_mount}, {lvm_ret0}},
  nif_newns[]   = {{lvm_newns}, {lvm_ret0}},
  nif_posix_stat[]    = {{lvm_posix_stat}, {lvm_ret0}},
  nif_posix_readdir[] = {{lvm_posix_readdir}, {lvm_ret0}},
  nif_posix_unlink[]  = {{lvm_posix_unlink}, {lvm_ret0}},
  nif_posix_lseek[]   = {{lvm_cur}, {.x = putcharm(3)}, {lvm_posix_lseek}, {lvm_ret0}},
  nif_posix_signal[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_signal}, {lvm_ret0}},
  nif_posix_ttyfg[]   = {{lvm_posix_ttyfg}, {lvm_ret0}},
  nif_posix_setenv[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_setenv}, {lvm_ret0}},
  nif_posix_environ[] = {{lvm_posix_environ}, {lvm_ret0}};
AI_NIF("spawn", nif_spawn);
AI_NIF("hear",  nif_reapany);
AI_NIF("sigfd", nif_sigfd);
AI_NIF("sigtake", nif_sigtake);
AI_NIF("wait",  nif_waitpid);
AI_NIF("chdir", nif_chdir);
AI_NIF("cwd",   nif_cwd);
AI_NIF("pipe",  nif_pipe);
AI_NIF("openfd", nif_openfd);
AI_NIF("spawnio", nif_spawnio);
AI_NIF("fdclose", nif_shutfd);
AI_NIF("fdopen", nif_fdopen);
AI_NIF("spawnmap", nif_spawnmap);
AI_NIF("getuid", nif_getuid);
AI_NIF("mkdir", nif_mkdir);
AI_NIF("mount", nif_mount);
AI_NIF("newns", nif_newns);
AI_NIF("stat",    nif_posix_stat);
AI_NIF("readdir", nif_posix_readdir);
AI_NIF("unlink",  nif_posix_unlink);
AI_NIF("lseek",   nif_posix_lseek);
AI_NIF("signal",  nif_posix_signal);
AI_NIF("ttyfg",   nif_posix_ttyfg);
AI_NIF("setenv",  nif_posix_setenv);
AI_NIF("environ", nif_posix_environ);
// --- the rest of the fs surface: the effect ops the fs tools ride ---------------
// (mv, ln, touch, chmod, chown -- crew/kore/fs.l and friends).
//   (rename old new)      -> () | errno | EINVAL   (mv's heart; same filesystem)
//   (symlink target path) -> () | errno | EINVAL   (path becomes a link TO target)
//   (readlink path)       -> the target string | ()
//   (chmod path mode)     -> () | errno | EINVAL   (mode the raw permission charm)
//   (chown path uid gid)  -> () | errno | EINVAL   (-1 leaves that id alone)
//   (utime path ms)       -> () | errno | EINVAL   (mtime AND atime on the stat
//                            scale, MILLISECONDS; a non-charm ms reads "now")
//   (umask mask)          -> the PREVIOUS mask | -1 misuse (always succeeds)
//   (rmdir path)          -> () | errno | EINVAL   (the empty-directory unlink)
//   (hardlink old new)    -> () | errno | EINVAL   (link(2); `link` the word is
//                            the chain ctor, the most spoken name in the prel,
//                            so the nif wears the long form)
ai_noinline static ai_word host_posix_rename(ai_word ow, ai_word nw) {
 char o[4096], n[4096];
 if (!str_cbuf(ow, o, sizeof o) || !str_cbuf(nw, n, sizeof n)) return putcharm(EINVAL);
 return rename(o, n) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_rename) {
 Sp[1] = host_posix_rename(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }

ai_noinline static ai_word host_posix_symlink(ai_word tw, ai_word pw) {
 char t[4096], p[4096];
 if (!str_cbuf(tw, t, sizeof t) || !str_cbuf(pw, p, sizeof p)) return putcharm(EINVAL);
 return symlink(t, p) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_symlink) {
 Sp[1] = host_posix_symlink(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }

ai_noinline static struct ai *host_posix_readlink(struct ai *g) {
 char p[4096], b[4096];
 if (!str_cbuf(g->sp[0], p, sizeof p)) return g->sp[0] = ZeroPoint, g;
 ssize_t n = readlink(p, b, sizeof b - 1);
 if (n < 0) return g->sp[0] = ZeroPoint, g;
 b[n] = 0;
 if (!ai_ok(g = ai_strof(g, b))) return g;                    // pushes: target over path
 g->sp[1] = g->sp[0];
 g->sp += 1;
 return g; }
static lvm(lvm_posix_readlink) {
 Pack(g); g = host_posix_readlink(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 return Ip++, Continue(); }

ai_noinline static ai_word host_posix_chmod(ai_word pw, ai_word mw) {
 char p[4096];
 if (!str_cbuf(pw, p, sizeof p) || !(mw & 1)) return putcharm(EINVAL);
 return chmod(p, (mode_t) getcharm(mw)) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_chmod) {
 Sp[1] = host_posix_chmod(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }

ai_noinline static ai_word host_posix_chown(ai_word pw, ai_word uw, ai_word gw) {
 char p[4096];
 if (!str_cbuf(pw, p, sizeof p) || !(uw & 1) || !(gw & 1)) return putcharm(EINVAL);
 return chown(p, (uid_t) getcharm(uw), (gid_t) getcharm(gw)) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_chown) {
 Sp[2] = host_posix_chown(Sp[0], Sp[1], Sp[2]);
 Sp += 2; return Ip++, Continue(); }

ai_noinline static ai_word host_posix_utime(ai_word pw, ai_word msw) {
 char p[4096];
 if (!str_cbuf(pw, p, sizeof p)) return putcharm(EINVAL);
 struct timespec ts[2];
 if (msw & 1) {
  intptr_t ms = getcharm(msw);
  ts[0].tv_sec = ts[1].tv_sec = (time_t) (ms / 1000);
  ts[0].tv_nsec = ts[1].tv_nsec = (long) (ms % 1000) * 1000000;
 } else
  ts[0].tv_sec = ts[1].tv_sec = 0, ts[0].tv_nsec = ts[1].tv_nsec = UTIME_NOW;
 return utimensat(AT_FDCWD, p, ts, 0) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_utime) {
 Sp[1] = host_posix_utime(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }

ai_noinline static ai_word host_posix_rmdir(ai_word pw) {
 char p[4096];
 if (!str_cbuf(pw, p, sizeof p)) return putcharm(EINVAL);
 return rmdir(p) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_rmdir) { Sp[0] = host_posix_rmdir(Sp[0]); return Ip++, Continue(); }

ai_noinline static ai_word host_posix_hardlink(ai_word ow, ai_word nw) {
 char o[4096], n[4096];
 if (!str_cbuf(ow, o, sizeof o) || !str_cbuf(nw, n, sizeof n)) return putcharm(EINVAL);
 return link(o, n) ? putcharm(errno) : ZeroPoint; }
static lvm(lvm_posix_hardlink) {
 Sp[1] = host_posix_hardlink(Sp[0], Sp[1]);
 Sp += 1; return Ip++, Continue(); }

static lvm(lvm_posix_umask) {
 Sp[0] = (Sp[0] & 1) ? putcharm((intptr_t) umask((mode_t) getcharm(Sp[0])))
                     : putcharm(-1);
 return Ip++, Continue(); }

static union u const
  nif_posix_rename[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_rename}, {lvm_ret0}},
  nif_posix_symlink[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_symlink}, {lvm_ret0}},
  nif_posix_readlink[] = {{lvm_posix_readlink}, {lvm_ret0}},
  nif_posix_chmod[]    = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_chmod}, {lvm_ret0}},
  nif_posix_chown[]    = {{lvm_cur}, {.x = putcharm(3)}, {lvm_posix_chown}, {lvm_ret0}},
  nif_posix_utime[]    = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_utime}, {lvm_ret0}},
  nif_posix_umask[]    = {{lvm_posix_umask}, {lvm_ret0}},
  nif_posix_rmdir[]    = {{lvm_posix_rmdir}, {lvm_ret0}},
  nif_posix_hardlink[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_posix_hardlink}, {lvm_ret0}};
AI_NIF("rename",   nif_posix_rename);
AI_NIF("symlink",  nif_posix_symlink);
AI_NIF("readlink", nif_posix_readlink);
AI_NIF("chmod",    nif_posix_chmod);
AI_NIF("chown",    nif_posix_chown);
AI_NIF("utime",    nif_posix_utime);
AI_NIF("umask",    nif_posix_umask);
AI_NIF("rmdir",    nif_posix_rmdir);
AI_NIF("hardlink", nif_posix_hardlink);
// --- the pty wrapper: bao's rlwrap/debugger muscle ------------------------------
// spawn a program on a fresh pseudo-terminal, reap it without blocking, signal
// it, and read/write its window size. The keystone, (mind argv), is host_run
// (main.c) with the stdout PIPE swapped for a pty pair: the same argv marshal +
// close-on-exec errno-pipe handshake, but the child's 0/1/2 become the pty SLAVE
// and the parent keeps the MASTER as a heap port (ai_io_alloc). So bao's editor
// talks to any program over the master the way a terminal would.
//
//   (mind argv)      -> (pid . master-port) | a fixnum (errno, or -1 = misuse)
//   (reap pid)         -> (status)   exited (a PAIR, truthy even at status 0)
//                       | ()         still running
//                       | errno      waitpid error (e.g. ECHILD)
//   (kill pid sig)     -> () ok | errno   (caller passes (0 - pid) for the group)
//   (winsize _)        -> (rows . cols) of the controlling tty (stdout), or ()
//   (setwinsize p r c) -> () ok | errno   push a size onto a master port
//
// (winsize) takes a dummy arg (ignored, like getpid): a bare (winsize) is the
// function itself -- (f) == f at zero operands -- so the call is (winsize 0).

// Workhorse for (mind argv), called with g Packed; argv is the single arg and
// the sole GC root at g->sp[0]. Leaves EXACTLY ONE net value above argv on every
// non-OOM path (so lvm_ptyrun collapses uniformly, cf. host_run): the
// (pid . master-port) chain on success, an errno/-1 fixnum otherwise. Returns a
// not-ok g only on OOM (lvm_ptyrun routes that to ghelp).
ai_noinline static struct ai *host_ptyrun(struct ai *g) {
  // NO l allocation between the marshal and the fork: openpt/grantpt/unlockpt/
  // ptsname/pipe don't touch the heap, so the uncommitted gap holds.
 char **cav;
 g = argv_marshal(g, &cav);
 if (!cav) return g;                               // misuse pushed -1, or OOM

  // open the master, unlock the slave, copy the slave path (ptsname's buffer is
  // static -- snapshot it for the child, which inherits the snapshot across fork).
 int mfd = posix_openpt(O_RDWR | O_NOCTTY);
 if (mfd < 0) return ai_push(g, 1, putcharm(errno));
 if (grantpt(mfd) || unlockpt(mfd)) { int e = errno; close(mfd); return ai_push(g, 1, putcharm(e)); }
 char sname[128];
 { char const *p = ptsname(mfd);
  if (!p || strlen(p) >= sizeof sname) { close(mfd); return ai_push(g, 1, putcharm(p ? ENAMETOOLONG : errno)); }
  memcpy(sname, p, strlen(p) + 1); }

  // close-on-exec errno pipe: child writes its setup/exec errno here; a clean
  // exec closes the write end -> parent reads EOF (childerr stays 0).
 int ep[2];
 if (pipe(ep)) { int e = errno; close(mfd); return ai_push(g, 1, putcharm(e)); }
 fcntl(ep[1], F_SETFD, FD_CLOEXEC);

 pid_t pid = fork();
 if (pid < 0) { int e = errno; close(mfd); close(ep[0]); close(ep[1]); return ai_push(g, 1, putcharm(e)); }
 if (!pid) {                                       // child
  close(mfd); close(ep[0]);
  int e;
  if (setsid() < 0) { e = errno; goto childfail; }
  int sfd = open(sname, O_RDWR);                  // opening a tty in a fresh session claims it as ctty
  if (sfd < 0) { e = errno; goto childfail; }
  ioctl(sfd, TIOCSCTTY, 0);                       // belt-and-braces; harmless if already ctty
  dup2(sfd, 0); dup2(sfd, 1); dup2(sfd, 2);
  if (sfd > 2) close(sfd);
  execvp(cav[0], cav);
  e = errno;
  childfail:
  { ssize_t w = write(ep[1], &e, sizeof e); (void) w; }
  _exit(127); }

 close(ep[1]);                                     // parent
 int childerr = 0; ssize_t r;
 do r = read(ep[0], &childerr, sizeof childerr); while (r < 0 && errno == EINTR);
 close(ep[0]);
 if (childerr) {                                   // setup/exec failed in the child
  close(mfd);
  int st; while (waitpid(pid, &st, 0) < 0 && errno == EINTR) {}
  return ai_push(g, 1, putcharm(childerr)); }

  // success: master -> heap port (pushes it to sp[0]; argv slides to sp[1]).
 struct ai *io = ai_io_alloc(g, mfd);
 if (!ai_ok(io)) {                                 // OOM: tear the child down, then ghelp
  kill(pid, SIGKILL);
  int st; while (waitpid(pid, &st, 0) < 0 && errno == EINTR) {}
  close(mfd);
  return io; }
 g = io;
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;   // port at sp[0] kept as a root
 struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm(pid), g->sp[0]);
 g->sp[0] = word(w);                               // [(pid . port), argv]
 return g; }

static lvm(lvm_ptyrun) {
 Pack(g);
 g = host_ptyrun(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Sp[1] = Sp[0];                                    // result over argv
 Sp += 1; Ip += 1;
 return Continue(); }

// Workhorse for (reap pid), called with g Packed and pid at g->sp[0]. The &st
// waitpid + the chain alloc live here (off the wrapper's frame so lvm_reap's
// Continue() tail-jumps, cf. host_ptyrun). Leaves exactly one net value at sp[0]:
// the (status) one-element list, () still-running, or an errno fixnum. Returns a
// not-ok g only on OOM (lvm_reap routes that to ghelp).
ai_noinline static struct ai *host_reap(struct ai *g, ai_word pidw) {
 intptr_t pid = (pidw & 1) ? getcharm(pidw) : 0;
 int st;
 pid_t r = waitpid((pid_t) pid, &st, WNOHANG);
 if (r == 0) { g->sp[0] = ai_nil; return g; }            // still running
 if (r < 0)  { g->sp[0] = putcharm(errno); return g; }   // waitpid error
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
 struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm(proc_status(st)), ZeroPoint);   // a REAL ()-tailed list, not the charm-0 fossil
 g->sp[0] = word(w);
 return g; }

// (reap pid): non-blocking wait. A reaped child returns its decoded status as a
// ONE-ELEMENT LIST so the result is a present chain even at status 0 -- a caller
// polling in a loop tells "exited 0" (a pair) from "still running" (()) without
// the two collapsing to the same blue. A bare fixnum means waitpid itself erred.
static lvm(lvm_reap) {
 Pack(g);
 g = host_reap(g, Sp[0]);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Ip += 1; return Continue(); }

// (kill pid sig): POSIX kill(2). A negative pid (the caller writes (0 - pid),
// never -pid -- that lexes as a kebab name) signals the process group. Returns
// () on success, the errno fixnum on failure.
static lvm(lvm_kill) {
 intptr_t pid = (Sp[0] & 1) ? getcharm(Sp[0]) : 0;
 intptr_t sig = (Sp[1] & 1) ? getcharm(Sp[1]) : 0;
 Sp[1] = kill((pid_t) pid, (int) sig) ? putcharm(errno) : ai_nil;
 Sp += 1; Ip += 1; return Continue(); }

// Workhorse for (winsize), called with g Packed (the dummy arg sits at sp[0]).
// The &ws ioctl + the chain alloc live here so lvm_winsize's Continue() tail-jumps
// (cf. host_ptyrun). Overwrites sp[0] with (rows . cols), or () if stdout isn't a
// tty. Returns a not-ok g only on OOM (lvm_winsize routes that to ghelp).
ai_noinline static struct ai *host_winsize(struct ai *g) {
 struct winsize ws;
 if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) < 0) { g->sp[0] = ai_nil; return g; }
 if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
 struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm(ws.ws_row), putcharm(ws.ws_col));
 g->sp[0] = word(w);
 return g; }

// (winsize): the controlling tty's size as (rows . cols), read off stdout; () if
// stdout isn't a tty (ioctl fails). The size to MIRROR onto a wrapped child.
static lvm(lvm_winsize) {
 Pack(g);
 g = host_winsize(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Ip += 1; return Continue(); }

// (setwinsize port rows cols): push a window size onto a master port; the kernel
// raises SIGWINCH on the slave's foreground group. () on success, errno on
// failure (incl. a non-port / closed port -> EBADF).
// The &ws ioctl for (setwinsize), off lvm_setwinsize's frame so its Continue()
// tail-jumps. Returns 0 or the errno.
ai_noinline static int host_setwinsize(intptr_t fd, intptr_t row, intptr_t col) {
 struct winsize ws = {0};
 ws.ws_row = (unsigned short) row;
 ws.ws_col = (unsigned short) col;
 return ioctl((int) fd, TIOCSWINSZ, &ws) ? errno : 0; }

static lvm(lvm_setwinsize) {
 intptr_t fd  = port_fd(Sp[0]);
 intptr_t row = (Sp[1] & 1) ? getcharm(Sp[1]) : 0;
 intptr_t col = (Sp[2] & 1) ? getcharm(Sp[2]) : 0;
 int rc = host_setwinsize(fd, row, col);
 Sp[2] = rc ? putcharm(rc) : ai_nil;
 Sp += 2; Ip += 1; return Continue(); }

// (ptyecho port on): toggle the pty's input ECHO. on = 0 / () clears it so a
// line-editing wrapper (bao's edraw) owns the echo and the child's cooked-mode
// echo doesn't double it; a truthy `on` restores it. ICANON is left intact -- the
// child still reads whole lines and sees VEOF. tcsetattr on the master fd sets the
// shared pty termios. () on success, errno on failure (non-port / closed -> EBADF).
// The &t tcget/tcsetattr for (ptyecho), off lvm_ptyecho's frame so its Continue()
// tail-jumps. Returns 0 or the errno (EBADF for a non-port / closed fd).
ai_noinline static int host_ptyecho(intptr_t fd, intptr_t on) {
 struct termios t;
 if (fd < 0) return EBADF;
 if (tcgetattr((int) fd, &t)) return errno;
 if (on) t.c_lflag |= ECHO; else t.c_lflag &= ~(tcflag_t) ECHO;
 return tcsetattr((int) fd, TCSANOW, &t) ? errno : 0; }

static lvm(lvm_ptyecho) {
 intptr_t fd = port_fd(Sp[0]);
 intptr_t on = (Sp[1] & 1) ? getcharm(Sp[1]) : 0;
 int rc = host_ptyecho(fd, on);
 Sp[1] = rc ? putcharm(rc) : ai_nil;
 Sp += 1; Ip += 1; return Continue(); }

// (raw on): own the interactive terminal discipline on stdin (fd 0). A truthy
// `on` puts the tty in raw mode (no ICANON/ECHO/ISIG, VMIN=1) so bao's editor is
// the SOLE echo; on = 0 / () restores the cooked termios captured at the first
// raw-on. bao's (shell _) calls (raw 1) because the bin/bao launch
// (love -l bao.l -e "(bao 0)") passes argv, so main.c's argp path skips raw_mode --
// without this the kernel tty echo doubles every line the editor draws. The cooked
// baseline is captured ONCE (a re-raw, e.g. main.c's no-arg path already raw'd,
// never re-saves a raw state) and restored on exit via atexit. () on success,
// errno on failure (stdin not a tty).
static struct termios raw_cooked;
static int raw_have_cooked = 0;
static void raw_restore(void) {
 if (raw_have_cooked) tcsetattr(STDIN_FILENO, TCSANOW, &raw_cooked); }
// All the &t termios work + the capture-once/atexit state for (raw on), off
// lvm_raw's frame so its Continue() tail-jumps. Returns 0 or the errno.
ai_noinline static int host_raw(intptr_t on) {
 struct termios t;
 if (tcgetattr(STDIN_FILENO, &t)) return errno;
 if (!on) { raw_restore(); return 0; }
 if (!raw_have_cooked) { raw_cooked = t; raw_have_cooked = 1; atexit(raw_restore); }
 t.c_lflag &= ~(tcflag_t) (ICANON | ECHO | ISIG | IEXTEN);
 t.c_iflag &= ~(tcflag_t) (IXON | ICRNL | BRKINT | INPCK | ISTRIP);
 t.c_cc[VMIN] = 1; t.c_cc[VTIME] = 0;
 return tcsetattr(STDIN_FILENO, TCSANOW, &t) ? errno : 0; }
static lvm(lvm_raw) {
 intptr_t on = (Sp[0] & 1) ? getcharm(Sp[0]) : 0;
 int rc = host_raw(on);
 Sp[0] = rc ? putcharm(rc) : ai_nil;
 Ip += 1; return Continue(); }

static union u const
  nif_raw[]        = {{lvm_raw}, {lvm_ret0}},
  nif_ptyrun[]     = {{lvm_ptyrun}, {lvm_ret0}},
  nif_reap[]       = {{lvm_reap}, {lvm_ret0}},
  nif_kill[]       = {{lvm_cur}, {.x = putcharm(2)}, {lvm_kill}, {lvm_ret0}},
  nif_winsize[]    = {{lvm_winsize}, {lvm_ret0}},
  nif_setwinsize[] = {{lvm_cur}, {.x = putcharm(3)}, {lvm_setwinsize}, {lvm_ret0}},
  nif_ptyecho[]    = {{lvm_cur}, {.x = putcharm(2)}, {lvm_ptyecho}, {lvm_ret0}};
AI_NIF("mind", nif_ptyrun);
AI_NIF("gather", nif_reap);
AI_NIF("still", nif_kill);
AI_NIF("winsize", nif_winsize);
AI_NIF("setwinsize", nif_setwinsize);
AI_NIF("ptyecho", nif_ptyecho);
AI_NIF("raw", nif_raw);
