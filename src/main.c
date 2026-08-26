#include "love.h"
#include "cats.h"
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
extern void host_spawn_guard(struct ai*, int);   // src/posix.c (exec-bound forks drop the pools)
#include <stdnoreturn.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/mman.h>    // the first boot's inflate buffer (mmap, no malloc)
extern struct ai *ai_argv_marshal(struct ai*, char***);   // src/seat.c: argv -> char** in the heap gap

// ai_clock lives in src/seat.c, one body for this frontend and the kernel's.
// the fine clock's real source (the weak default in love.c degrades to ms*1e6)
ai_noinline intptr_t ai_nclock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? -1
       : (intptr_t) ts.tv_sec * 1000000000 + ts.tv_nsec; }


static void stdin_give(struct ai *g) {
 if (!g || !ai_ok(g)) return;
 struct ai *fc = ai_core_of(g);
 if (fc->inflag)                                          // its blocking bit was ours: back it goes
  fcntl(STDIN_FILENO, F_SETFL, (int) getcharm(fc->inflag)), fc->inflag = 0;
 if (!fc->inport || lseek(STDIN_FILENO, 0, SEEK_CUR) < 0) return;   // an unseekable door: stdin_hand's
 uintptr_t n = ai_io_pending(g, (struct ai_io*) fc->inport)
             + (getcharm(ai_stdin.io.ungetc_buf) != EOF ? 1 : 0);
 if (n) lseek(STDIN_FILENO, -(off_t) n, SEEK_CUR); }

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

__attribute__((weak)) lvm(k_lvm_quit) { ai_musttail return Ap(_lvm_ghelp, g); }
__attribute__((weak)) lvm(k_lvm_getpid) { ai_musttail return Ap(_lvm_ghelp, g); }

static lvm(lvm_exit) {
  if (__ai_osv < 0) ai_musttail return Ap(k_lvm_quit, g);
  for (;;) stdin_give(g), exit(getcharm(Sp[0])); }

extern uintptr_t ai_fd_write_all(int, unsigned char const*, uintptr_t);   // src/seat.c
                                                                          //
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
 if (pid < 0) return close(p[0]), (void) close(p[1]);
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

static void host_teeout(char const *p, size_t n) {
 while (n) {
  ssize_t w = write(STDOUT_FILENO, p, n);
  if (w < 0) { if (errno == EINTR) continue; return; }
  p += w, n -= (size_t) w; } }

static struct ai *host_harkst(struct ai *g, intptr_t fd, intptr_t pid, int tee) {
 g = ai_push(g, 3, putcharm(0), putcharm(fd), putcharm(pid));
 if (ai_ok(g)) g->sp[3] = putcharm(tee);
 return g; }

// the first ap: marshal argv, fork, and confirm the exec. called with g Packed;
// argv is at sp[0]. returns a not-ok g only on oom.
ai_noinline static struct ai *host_harkstart(struct ai *g, int tee) {
 char **cav;
 g = ai_argv_marshal(g, &cav);
 if (!cav)                                                // a misuse, or the reserve
  return ai_ok(g) ? ai_push(host_harkst(g, -1, 0, tee), 1, putcharm(-1)) : g;

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

 int fl = fcntl(op[0], F_GETFL);
 if (fl >= 0) fcntl(op[0], F_SETFL, fl | O_NONBLOCK);
 return str0(host_harkst(g, op[0], pid, tee), 1u << 16); }  // capture -> sp[0]

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
                                                          //
 int st; pid_t w;
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
 g->sp[2] = putcharm(-1);                             // done
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

ai_noinline static struct ai *host_exec(struct ai *g) {
 char **cav;
 g = ai_argv_marshal(g, &cav);
 if (!cav) return ai_ok(g) ? ai_push(g, 1, putcharm(-1)) : g;
 fflush(stdout);
 fflush(stderr);
 signal(SIGPIPE, SIG_DFL);                                 // ... nor this one
 stdin_hand(g);                                            // the child inherits fd 0: hand it over exact
 execvp(cav[0], cav);
 return ai_push(g, 1, putcharm(errno)); }                  // exec failed -> errno

static lvm(lvm_exec) {
 Pack(g);
 g = host_exec(g);                                         // returns only on failure
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 Sp[1] = Sp[0];                                            // errno fixnum over argv
 ai_musttail return Nextp(1, 1); }

// (getenv name) -> string, or zero if unset / misused. zero = absent, not an error.
// the name goes to getenv where it lies: a love string's bytes[len] is always a NUL.
static lvm(lvm_getenv) {
 char const *v = ai_strp(Sp[0]) ? getenv(txt(Sp[0])) : NULL;
 if (!v) { Sp[0] = ZeroPoint; ai_musttail return Next(1); }
 Pack(g);
 if (!ai_ok(g = ai_strof(g, v))) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 Sp[1] = Sp[0];
 ai_musttail return Nextp(1, 1); }

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
AiNif("quit", nif_exit);
AiNif("hark", nif_hark);
AiNif("herald", nif_herald);
AiNif("exec", nif_exec);
AiNif("getenv", nif_getenv);
AiNif("getpid", nif_getpid);

static struct ai *env_budget(struct ai *g) {
  char const *b = getenv("LOVE_BUDGET_MB");
  if (g && b && atol(b) > 0) {
    g->budget = (uintptr_t) atol(b) * (1024 * 1024 / sizeof(ai_word));
    return g; }
  if (g && !g->budget) {
    int fd = open("/proc/meminfo", O_RDONLY);
    if (fd >= 0) { char mb[64]; long n = (long) read(fd, mb, sizeof mb - 1);
      close(fd);
      if (n > 8 && !memcmp(mb, "MemTotal", 8)) { mb[n] = 0;
        char *p = mb; while (*p && (*p < '0' || *p > '9')) p++;
        uintptr_t kb = 0; while (*p >= '0' && *p <= '9') kb = kb * 10 + (uintptr_t)(*p++ - '0');
        g->budget = kb * 1024 / 2 / sizeof(ai_word); } } }
  return g; }

extern int image_dump(struct ai*, char const*),          // src/image.c (file I/O around love.c's codec)
           image_bake(struct ai*),                       // src/image.c (the self-bake)
           ai_baked_pick(void const**, uintptr_t*);      // the carried image, if one is baked in
extern struct ai *image_load(char const*);
extern uint64_t ai_baked_image[];
extern uintptr_t ai_baked_image_len;

#ifdef LoveBoot
static char const
 runner[] = "(reads(tap(s2cl tests)))"   // the stream shell (love/bao.l) drinks the corpus
 , cli[] =
#include "cli0.h"
 , src0_mods[] =
#include "coin0.h"
#include "rng0.h"
#include "q0.h"
#include "glob0.h"
#include "kanren0.h"
#include "overlay0.h"
#include "uu0.h"
#include "holo0.h"
#include "x640.h"
#include "arm640.h"
#include "bao0.h"
#include "verbs0.h"
#include "peg0.h"
;

// love0 is never interactive -- a build tool or the self-test -- so replp is the full
// love's word and this lane only takes it to share main's one dispatch. love0 wakes an
// image file (its own mooncc0.image bake); the .image self-patch is the full binary's.
// mooncc0.image is the `bake` nif's, called from a -e, so it seals the session layer with
// cli0 already on it -- and every build-time object compile is one wake of it.
static struct ai *run_program(struct ai *g, bool replp, bool owed) {
  (void) replp;
  g = ai_layer_(g);
  if (owed) g = ai_evals_(g, cli);
  return ai_evals_(g, "(cli-line cmdline 0)"); }

// with args, run the build tool (lcat / gen_data) through the CLI driver.
// with no args, self-test: eval prel, load bao (the shell core) as a module, and run
// the baked corpus via c0, then bootstrap the self-hosted ev (egg) and run the corpus
// again through it. bake/bake_load are the full love's; they land here so one call
// serves both lanes.
static struct ai *boot(struct ai *g, bool argp, char const *bake, char const *bake_load) {
  (void) bake, (void) bake_load;
  if (argp) {
    g = ai_evals_(g,
#include "p10.h"
    );
    g = ai_evals_(g,
#include "prel0.h"
#include "post0.h"
    );
    g = ai_evals_(g, src0_mods);
    g = ai_evals_(g, "(use 'bao)(use 'kanren)(use 'verbs)");
    g = ai_unsplice_(g);
    g = ai_evals_(g, cli);
    return ai_evals_(g, "(cli-line cmdline 0)"); }
  g = ai_evals_(g,                                    // its own call: readtext picks its reader
#include "p10.h"                                     // once per text, and p1 seals hook 0 only
  );                                                 // when this call evaluates
  g = ai_evals_(g,
#include "prel0.h"
#include "post0.h"
  );
  g = ai_evals_(g, src0_mods);
  g = ai_evals_(g, "(use 'bao)(use 'holo)");
  g = ai_unsplice_(g);
  g = ai_evals_(g,
    "(use 'uu)(: uu (from 'uu))(use 'coin)(use 'rng)(use 'q)(use 'kanren)"
    "(: (s2cl s) ((: (g i) (? (< i (tally s)) (link (peep s i 0) (g (+ 1 i))))) 0)"
    "   (c0read p) (: q (open p \"r\")"
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
  g = ai_evals_(g, runner);          // pass 1: corpus via ev = the c0 nif
  g = ai_egg_(g,                                      // bootstrap: install the self-hosted ev
#include "egg0.h"
    ,
#include "p10.h"
    ,
#include "prel0.h"
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

// the glaze, in one text. emit.l (the native emitter) then auto.l (ev's source
// recognizer), which reads emit's names bare -- so the order here is the module,
// and the pair declares it since neither file is it alone. holo leads because the
// glaze folds `assemble` at its own compile, and orth's two names plus the ala
// creation hook trail it -- hook.l leaks natjit/fires/fired?/bake on purpose, so it
// stays outside the module. ai_evals_ evals form by form, so one text is one order.
// not in src_mods: an unglazed build must not pay for it, and the empty twins below let
// every eval site stand unconditional -- only the unsplice after the load still asks.
#ifdef AiGlazed
static char const src_glaze[] =
  "(use 'holo)"
  "(module 'glaze "
#include "emit.h"
#include "auto.h"
  ")"
  "(: ev (from 'glaze 'ev) member? (from 'glaze 'member?))"
#include "hook.h"
  ,
  // LOVE_NO_GLAZE: a pure-interpreter session -- ev back to base-ev (kept in the glaze
  // module book) and the natjit hook cleared. the forensics twin of LOVE_NO_IMAGE, and a
  // session knob: it governs a run, never the baked artifact.
  glaze_off[] = "(: ev (from 'glaze 'base-ev) natjit ())",
  // auto.l's self-tests fill `memo` with native closures whose ap is a W^X mmap address,
  // and those cannot serialize. empty it, so a woken runtime re-JITs lazily.
  glaze_cache[] = "(: c (from 'glaze 'cache) (map (\\ k (pull c k 0)) (keys c)))";
#else
static char const src_glaze[] = "", glaze_off[] = "", glaze_cache[] = "";
#endif

// the session layer: boot is over, and from here the base (prel/ev, the nifs, every
// warmed module) is never the head again, so a top-level definition lands here instead.
// never popped -- its lifetime is the session, which is what lets a catted app's files
// share one vocabulary. both the egg boot and the image wake converge here.
// love/cli.l DEFINES rather than runs, and `cli-line` is this tail entire: the argv[0]
// verb door, the positional rail, the repl, the stdin drink. the isatty answer is the
// only thing C still owns. the bake below carries it compiled, so `owed` is the egg
// lane alone -- a wake that re-evals it pays a 2 KB compile to reach two names it has.
static struct ai *run_program(struct ai *g, bool replp, bool owed) {
  if (replp) raw_mode();
  g = ai_layer_(g);
  if (getenv("LOVE_NO_GLAZE")) g = ai_evals_(g, glaze_off);
  if (owed) g = ai_evals_(g, cli);
  return ai_evals_(g, replp ? "(cli-line cmdline 1)" : "(cli-line cmdline 0)"); }

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

static struct ai *boot(struct ai *g, bool argp, char const *bake, char const *bake_load) {
  // the debug door: LOVE_NO_MOP keeps the compiler's internals on the book (peek/poke/
  // seek/feels/dis and the raw cell nifs) for introspection -- egg.l skips the birth mop
  // when `nomop` is set. reaches only a fresh egg warm, so pair it with LOVE_NO_IMAGE
  // (a baked image is already swept): `LOVE_NO_MOP=1 LOVE_NO_IMAGE=1 love`. off by
  // default, so the shipped surface and every gate stay swept.
  { char const *nm = getenv("LOVE_NO_MOP");
    if (nm && *nm) g = ai_evals_(g, "(: nomop 1)"); }
  g = ai_egg_(g, ai_cat_egg, ai_cat_p1, ai_cat_prel,    // prel, then ev's half
                 ai_cat_post);                           // the printer, and `@` with it
  g = ai_evals_(g, ai_cat_mods);                         // register every baked module; the uses below are splices
  g = ai_evals_(g,
    "(use 'coin)"
    "(use 'rng)"
    "(use 'q)"
    "(use 'kanren)"
    "(use 'overlay)"
    "(: overlay (from 'overlay)"
    "   ev ((from 'overlay 'ov-hook) ev))"
    "(use 'uu)"
    "(: uu (from 'uu))"
    "(use 'holo)"
  );
  g = ai_unsplice_(g);
  g = ai_evals_(g, "(use 'bao)(use 'verbs)");
  g = ai_unsplice_(g);
  // kanren, overlay and uu come off: both of the latter already have the accessor bound
  // above, so the splice bought only ambient names, and `C`, `Q`, `src`, `glob`, `walk`,
  // `var`, `con`, `est` are what this tree calls its locals. kanren keeps a surface, named
  // rather than inherited. unsplice drops one link at a time, so bao comes off with them
  // and goes straight back on: read/reads for cli, `@` for every later compile.
  for (int i = 0; i < 4; i++) g = ai_unsplice_(g);       // bao, uu, overlay, kanren
  g = ai_evals_(g, "(use 'bao)"
    "(hoist 'kanren ())"                                 // \\\, &&&, |||, zz -- macros, not names
    "(: unify (from 'kanren 'unify)  ufail (from 'kanren 'ufail)"
    "   ufail? (from 'kanren 'ufail?)  var (from 'kanren 'var)"
    "   s_plus (from 'kanren 's_plus)  s_star (from 'kanren 's_star)"
    "   === (from 'kanren '===)  =/= (from 'kanren '=/=))");
  g = ai_evals_(g, src_glaze);                           // "" on an unglazed arch
#ifdef AiGlazed
  g = ai_unsplice_(g);                                   // holo back to non-ambient
#endif

  g = ai_evals_(g,
    "(: spawn0 spawn  spawnio0 spawnio  spawnmap0 spawnmap  wait0 wait"
    "   seat-doors (: t (tablet 4) _ (pin t 0 spawn0) _ (pin t 1 spawnio0)"
    "                 _ (pin t 2 spawnmap0) _ (pin t 3 wait0) t)"
    "   (spawn argv) ((peep seat-doors 0 0) argv)"
    "   (spawnio argv i o e cl pg fg) ((peep seat-doors 1 0) argv i o e cl pg fg)"
    "   (spawnmap argv fdm cl pg fg) ((peep seat-doors 2 0) argv fdm cl pg fg)"
    "   (wait p) ((peep seat-doors 3 0) p))");

  g = ai_evals_(g, bake
    ? "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) _ (pull book 'born 0) (pull book 'book 0))"
    : "(: _ (pull book 'nif 0) _ (pull book 'nifx 0) (pull book 'book 0))");

  if (bake) {                                            // the bake verb: snapshot the post-warm heap, then exit
    if (bake_load && !ai_ok(g = bake_eval_file(g, bake_load))) return g;
    // the CLI driver rides the image too, last so it sits over the crew exactly as the
    // session-layer eval it replaces did. pure definition: cli-line reads argv and the
    // verb registry when CALLED, so nothing of this session is folded in.
    g = ai_evals_(g, cli);
    g = ai_evals_(g, glaze_cache);
    int rc = *bake ? image_dump(g, bake) : image_bake(g);
    if (rc) fprintf(stderr, "love: bake failed (rc=%d)\n", rc);
    exit(rc ? 1 : 0); }
  return run_program(g, !argp && isatty(STDIN_FILENO), 1); }
#endif

ai_noinline static struct ai *argv_chain(struct ai *g, char const **v, int argc, int skip) {
  int n = 0;
  if (argc > 0) g = ai_strof(g, v[0]), n++;                  // argv[0] is always the program
  for (int i = 1 + skip; i < argc; i++) g = ai_strof(g, v[i]), n++;
  for (g = ai_push(g, 1, ai_zero); n--; g = gxr(g));
  return g; }

#if !defined(LoveBoot) && !defined(__wasm__)
extern intptr_t ai_inflate_raw(const unsigned char*, uintptr_t, unsigned char*, uintptr_t);
#include "ustar.h"
extern const unsigned char ai_srcgz[];
extern const uintptr_t ai_srcgz_len;
extern size_t host_selfpath(char*, size_t);
static char const src_distlist[] =
#include "distlist.h"
 ;

// inflate the carried blob (gzip: skip the header fields, ISIZE names the tar)
static unsigned char *fb_untar(uintptr_t *outn) {
  uintptr_t o = 0, un = 0;
  if (!ai_gz_body(ai_srcgz, ai_srcgz_len, &o, &un)) return NULL;
  unsigned char *t = mmap(NULL, un ? un : 1, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (t == MAP_FAILED) return NULL;
  if (ai_inflate_raw(ai_srcgz + o, ai_srcgz_len - o - 8, t, un) != (intptr_t) un)
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
    uintptr_t sz = ai_ustar_octal(h + 124, 12);
    if (ai_ustar_member(h)) {
      char nm[256];
      uintptr_t ln = ai_ustar_name(h, nm, sizeof nm);
      if (ln == pl && !memcmp(nm, path, pl)) {
        if (!ai_ustar_islink(h)) return *len = sz, t + o + 512;
        char tgt[101], cn[256];
        tgt[ai_ustar_link(h, tgt, sizeof tgt - 1)] = 0;
        cn[ai_lnk_canon(path, tgt, cn, sizeof cn - 1)] = 0;
        return fb_find(t, n, cn, len, hop + 1); } }
    o += 512 + ((sz + 511) & ~(uintptr_t) 511); }
  return NULL; }

static void first_boot(char const **argv) {
  if (ai_srcgz_len < 18) return;                     // src/src.c's weak zero: this link carries no source
  // the latch, and it is an env var precisely because the state it guards spans an exec:
  // the re-exec below sets it, so the binary that comes back knows it already tried. a
  // bake that leaves the .image section unfilled would otherwise loop forever.
  if (getenv("LOVE_FIRST_BOOT")) {
    fprintf(stderr, "; first boot: still unbaked after a bake -- running from source\n");
    return; }
  char exe[4096], cat[sizeof exe + 40];              // + ".firstboot.<pid>.l" and its NUL
  if (!host_selfpath(exe, sizeof exe)) return;
  uintptr_t un = 0;
  unsigned char *t = fb_untar(&un);
  if (!t) {                                          // truncated or not gzip; or the mmap failed
    fprintf(stderr, "; first boot: the carried source will not inflate -- running from source\n");
    return; }
  // per-process, for the reason the bake's scratch is (src/image.c): concurrent first
  // boots on one name write the cat into each other and unlink it under each other.
  snprintf(cat, sizeof cat, "%s.firstboot.%ld.l", exe, (long) getpid());
  int fd = open(cat, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd < 0) {                                      // a read-only seat -- /usr/bin, a container layer
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
    // a roster name the archive does not carry (a stale distlist), or a full filesystem
    if (!m || (ml && write(fd, m, ml) != (ssize_t) ml)) {
      fprintf(stderr, "; first boot: %s %s -- running from source\n", w,
              m ? "would not write" : "is not in the carried source");
      close(fd), unlink(cat), munmap(t, un);
      return; } }
  close(fd), munmap(t, un);
  fprintf(stderr, ";; baking heap image\n");
  // a child, so the bake gets a clean process: it snapshots its own heap and exits, and
  // this one still has a session to fall back to if it fails.
  pid_t p = fork();
  if (!p) { char *args[] = { exe, (char*) "bake", (char*) "-l", cat, NULL };
            execv(exe, args); _exit(127); }
  int st = -1;
  if (p > 0) waitpid(p, &st, 0);
  unlink(cat);
  if (p < 0 || !WIFEXITED(st) || WEXITSTATUS(st)) {
    fprintf(stderr, ";; bake failed, running from source\n");
    return; }
  setenv("LOVE_FIRST_BOOT", "1", 1);
  execv(exe, (void*) argv);                          // the patched file: same path, new inode
  fprintf(stderr, "; first boot: cannot re-exec -- running from source\n"); }
#else
#define first_boot(argv) ((void) 0)                      // love0, or no processes to fork
#endif

int main(int argc, char const **argv) {
  signal(SIGPIPE, SIG_IGN);
  struct ai *g = NULL;
  char const *image_load_path = NULL, *bake = NULL;   // see boot(): "" = self-bake, a path = an image file
  char const *bake_load = NULL;                      // bake -l CAT: read-eval it before the seal
  int skip = 0;                                      // words that are the prime's, not the program's
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
  char const *noimg = getenv("LOVE_NO_IMAGE");
  uintptr_t woke_ms = 0;                       // what the wake cost, for `born` below
  if (!g && !bake && !(noimg && *noimg)) {
   uintptr_t t0 = ai_clock();
   uintptr_t blen = 0;
   void const *bimg = NULL;
   if (ai_baked_pick(&bimg, &blen) && (g = ai_image_load(bimg, blen)))
    woke_ms = ai_clock() - t0,
    image_load_path = "<baked>"; }                                     // a loaded image is the booted state: skip the egg warm
  // unbaked, with source aboard, and nothing explicit asked for: finish first.
  // a `wake` names its own image and a `bake` is the finishing move itself.
  if (!g && !bake && !(noimg && *noimg) && !(argc >= 2 && !strcmp(argv[1], "wake")))
    first_boot(argv);                                  // returns only on refusal; success re-execs
  if (!g) g = ai_ini();
  g = env_budget(g);                               // the LOVE_BUDGET_MB cap, on whichever g won (fresh or woken image)
  bool argp = argc - skip > 1;
  if (!bake) {
    g = argv_chain(g, argv, argc, 0);               // cmdline, first: it ends up deeper
    g = argv_chain(g, argv, argc, skip); }          // argv, on top -- sp[0]
  if (ai_ok(g)) {
    g = ai_defn(g, __start_ai_nifs, __stop_ai_nifs - __start_ai_nifs, 0);
    for (struct ai_mod const *mt = __start_ai_mods; mt < __stop_ai_mods; mt++)
      g = ai_defn(g, mt->defs, mt->n, mt->mod);
    if (!bake) {
      g = ai_defv(g, "argv");
      if (ai_ok(g)) ai_core_of(g)->sp++;            // the book holds argv; the line is sp[0] now
      g = ai_defv(g, "cmdline");
      if (ai_ok(g)) ai_core_of(g)->sp++; }          // the book holds it now
    if (image_load_path && ai_ok(g = ai_strof(g, image_load_path))) {
      g = ai_defv(g, "love-image");
      if (ai_ok(g)) ai_core_of(g)->sp++; }
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
    if (image_load_path && ai_ok(g = ai_push(g, 1, putcharm((intptr_t) woke_ms)))) {
      g = ai_defv(g, "born");
      if (ai_ok(g)) ai_core_of(g)->sp++; }
    if (!bake) g = stdin_take(g);
    // an egg warm, or a woken image straight to the program -- the wake skips the warm
    g = image_load_path ? run_program(g, !argp && isatty(STDIN_FILENO), 0)
                        : boot(g, argp, bake, bake_load);
  }
  if (ai_code_of(g) == ai_status_scare) ai_scare_face_(g);
  stdin_give(g);
  return ai_fin(g); }
