#!/bin/sh
# test/gate/freebsd.sh -- the multi-OS gate (doc/plan/seed-universal.md, rung
# UV): ONE default-lane x64 binary -- no -os, born branded EI_OSABI=9 --
# answers BOTH kernels with the same text and status. the legs: UV1 (entry,
# carry, sigsetjmp), UV2 (the whole compat battery: open flags, stat, dirent,
# signals, fork), UV-net (the socket family: sockaddr heads, sockopt names,
# msg flags, over loopback TCP + UDP + unix), and -- FBSD_SEED=1, minutes --
# the trophy: `love seed` ON THE BOX answers the tree's own bytes.
# ⚠ NOT here on purpose: termios proper (a gate that needs a tty).
#
# the box arrives by env: FBSD_SSH is a command prefix ("ssh -p 2222 -i key
# root@host"); without one the gate skips loudly, the house rule for a gate
# whose instrument is not on this machine.
#
# conjuring a box (what gated this 2026-08-16): the BASIC-CLOUDINIT qcow2 from
# download.freebsd.org/releases/VM-IMAGES/<rel>/amd64/Latest/, a NoCloud seed
# iso (mkisofs -V cidata user-data meta-data: disable_root false + an
# authorized key + PermitRootLogin prohibit-password), then
#   qemu-system-x86_64 -enable-kvm -m 2048 -drive file=img.qcow2,if=virtio \
#     -cdrom seed.iso -nic user,hostfwd=tcp:127.0.0.1:2222-:22 -display none
# first boot runs freebsd-update; sshd answers a few minutes in.
# usage: freebsd.sh OUTDIR LOVE0
set -u

ho=$1
love0=$2
d=$ho/fbsd

[ -n "${FBSD_SSH:-}" ] || { echo "test_freebsd: skipped (no FBSD_SSH box)"; exit 0; }

fail() { echo "FAIL test_freebsd: $*" >&2; exit 1; }

mkdir -p "$d"
cat > "$d/rung2.c" <<'EOF'
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/wait.h>

static int step = 0;
static void ok(int cond) {
  step++;
  if (!cond) { char b[3] = {'F', (char)('A' + step - 1), '\n'}; write(2, b, 3); _exit(step); } }

static volatile sig_atomic_t got = 0;
static void take(int sig) { got = sig; }

int main(void) {
  /* rung 2: the raw floor */
  ok(write(1, "hello, freebsd\n", 15) == 15);
  ok(getpid() > 0 && kill(getpid(), 0) == 0);
  ok(write(-1, "x", 1) == -1 && errno == EBADF);      /* er() saw the carry */
  struct timespec ts;
  ok(clock_gettime(0, &ts) == 0 && ts.tv_sec > 0);
  struct timespec tn = {0, 1000000};
  ok(nanosleep(&tn, 0) == 0);
  int fds[2]; char c = 0;
  ok(pipe(fds) == 0 && write(fds[1], "x", 1) == 1 && read(fds[0], &c, 1) == 1 && c == 'x');
  sigjmp_buf env;
  int r = sigsetjmp(env, 1);
  if (r == 0) siglongjmp(env, 7);
  ok(r == 7);

  /* rung 3: the forked tables */
  ok(open("/no/such/file", O_RDONLY) == -1 && errno == ENOENT);
  int fd = open("/tmp/rung3.txt", O_CREAT | O_WRONLY | O_TRUNC, 0644);   /* O_* freebsd's now */
  ok(fd >= 0 && write(fd, "five!", 5) == 5 && close(fd) == 0);
  struct stat st;
  ok(stat("/tmp/rung3.txt", &st) == 0 && st.st_size == 5 && S_ISREG(st.st_mode));
  fd = open("/tmp/rung3.txt", O_RDONLY); char buf[8];
  ok(fd >= 0 && read(fd, buf, 8) == 5 && memcmp(buf, "five!", 5) == 0);
  int fd2 = dup2(fd, 17);
  ok(fd2 == 17 && lseek(17, 0, 0) == 0 && close(17) == 0 && close(fd) == 0);
  ok(unlink("/tmp/rung3.txt") == 0);
  ok(mkdir("/tmp/rung3.d", 0755) == 0 && rmdir("/tmp/rung3.d") == 0);   /* AT_REMOVEDIR freebsd's */
  char *m = malloc(100000);                          /* mmap MAP_ANON freebsd's */
  ok(m != 0 && (m[0] = 1) && (m[99999] = 2));
  char cwd[256];
  ok(getcwd(cwd, sizeof cwd) != 0 && cwd[0] == '/');
  DIR *dp = opendir("/etc"); int n = 0;
  struct dirent *e;
  while (dp && (e = readdir(dp))) if (e->d_name[0]) n++;
  ok(dp != 0 && closedir(dp) == 0 && n > 2);         /* getdirentries fills our dirent */
  ok(isatty(0) == 0);                                /* stdin is ssh's pipe */
  printf("printf rides: %d\n", 42);                  /* stdio over the freebsd floor */
  fflush(stdout);

  /* signals: sigaction without a restorer, the mask through the C door */
  struct sigaction sa; memset(&sa, 0, sizeof sa);
  sa.sa_handler = take;
  ok(sigaction(SIGUSR1, &sa, 0) == 0);
  ok(kill(getpid(), SIGUSR1) == 0 && got == SIGUSR1);   /* the kernel's own trampoline returned */
  got = 0;
  sigset_t bs; sigemptyset(&bs); sigaddset(&bs, SIGUSR1);
  ok(sigprocmask(SIG_BLOCK, &bs, 0) == 0);
  ok(kill(getpid(), SIGUSR1) == 0 && got == 0);         /* parked */
  ok(sigprocmask(SIG_UNBLOCK, &bs, 0) == 0 && got == SIGUSR1);   /* lands on unblock */

  /* fork is real here */
  int pid = fork();
  if (pid == 0) _exit(5);
  int stx = 0;
  ok(pid > 0 && waitpid(pid, &stx, 0) == pid && WIFEXITED(stx) && WEXITSTATUS(stx) == 5);

  write(1, "all\n", 4);
  return 42;
}
EOF

moon0() { "$love0" wake "$ho/mooncc0.image" mooncc "$@"; }

# ---- rung UV1: ONE binary, both kernels ----
# the DEFAULT lane, no -os: canonical numbers dispatched at runtime (os.c's
# probe + map), the dual crt0, the dual sigprocmask leaves, the errno row,
# and the brand BORN in (every x64 static exe leaves the linker EI_OSABI=9;
# freebsd's loader requires the byte and linux's never reads it). the SAME
# file must answer the SAME text and status on both kernels.
# ⚠ -e/argv LANES RUN MANY TIMES ON PURPOSE: the freebsd kernel hands the
# vector base in %rdi and [rsp] may hold a pad word below argc, so a crt0
# reading the wrong door flips by stack address, not by input -- one green
# run proves nothing.
cat > "$d/uv1.c" <<'EOF'
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <string.h>
#include <stdio.h>

int main(int argc, char **argv) {
  (void) argv;
  sigjmp_buf b;
  int j = sigsetjmp(b, 1);
  if (!j) siglongjmp(b, 7);
  int bad = write(-1, "x", 1) == -1 && errno == EBADF;
  int k = kill(getpid(), 0) == 0;
  char m[96];
  sprintf(m, "uv1: argc=%d jmp=%d ebadf=%s kill=%s\n",
          argc, j, bad ? "ok" : "NO", k ? "ok" : "NO");
  write(1, m, strlen(m));
  return 42; }
EOF
moon0 -t x64 "$d/uv1.c" -o "$d/uv1" || fail "uv1: the default-lane compile"
[ "$(dd if="$d/uv1" bs=1 skip=7 count=1 2>/dev/null | od -An -tu1 | tr -d ' ')" = 9 ] \
  || fail "uv1: not born branded EI_OSABI=9"
lout=$(for i in 1 2 3 4; do "$d/uv1" one two; echo "rc=$?"; done)
fout=$($FBSD_SSH 'cat > /tmp/uv1 && chmod +x /tmp/uv1 && for i in 1 2 3 4; do /tmp/uv1 one two; echo "rc=$?"; done' < "$d/uv1") \
  || fail "uv1: the box could not take or run it"
[ "$lout" = "$fout" ] || fail "uv1: the kernels disagree -- linux[$lout] freebsd[$fout]"
echo "$lout" | grep -q "uv1: argc=3 jmp=7 ebadf=ok kill=ok" || fail "uv1 body -- got: $lout"
echo "$lout" | grep -q "rc=42" || fail "uv1 exit -- got: $lout"

echo "test_freebsd: UV1 -- ONE default-lane binary answered both kernels the same"

# ---- rung UV2: the compat members -- the WHOLE rung2 battery, one binary ----
# the same source the -os leg runs, compiled in the DEFAULT lane: open flags,
# stat, dirent, dup2, mkdir/rmdir, mmap, getcwd, readdir, sigaction + the
# handler's number, sigprocmask's bits, fork + the wait status -- every one
# translated at runtime, and the SAME file answers the SAME text on both
# kernels. (the linux run answers here; the box answers over ssh.)
moon0 -t x64 "$d/rung2.c" -o "$d/uv2" || fail "uv2: the default-lane compile"
l2=$("$d/uv2" < /dev/null; echo "rc=$?")   # stdin pinned: the battery asserts isatty(0)==0
f2=$($FBSD_SSH 'cat > /tmp/uv2 && chmod +x /tmp/uv2 && /tmp/uv2; echo "rc=$?"' < "$d/uv2") \
  || fail "uv2: the box could not take or run it"
[ "$l2" = "$f2" ] || fail "uv2: the kernels disagree -- linux[$l2] freebsd[$f2]"
echo "$l2" | grep -q "rc=42" || fail "uv2 exit -- got: $l2"
echo "$l2" | grep -q "all" || fail "uv2 battery -- got: $l2"

echo "test_freebsd: UV2 -- the whole rung2 battery, one binary, both kernels"

# ---- rung UV-net: the socket family, one binary ----
# sockaddr heads rebuilt (the BSD length byte where linux's 16-bit family
# sits), sockopt names permuted under SOL_SOCKET's move, msg flags mapped --
# proven over loopback TCP (bind/listen/getsockname/connect/accept), UDP
# (sendto/recvfrom + the peer's translated head), and a unix-path pair.
cat > "$d/uvnet.c" <<'EOF'
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>

static int step = 0;
static void ok(int cond) {
  step++;
  if (!cond) { char b[3] = {'F', (char)('A' + step - 1), '\n'}; write(2, b, 3); _exit(step); } }

int main(void) {
  /* TCP over loopback: the whole shape */
  int ls = socket(AF_INET, SOCK_STREAM, 0);
  ok(ls >= 0);
  int one = 1;
  ok(setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one) == 0);
  struct sockaddr_in a; memset(&a, 0, sizeof a);
  a.sin_family = AF_INET; a.sin_port = 0; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  ok(bind(ls, (struct sockaddr*) &a, sizeof a) == 0 && listen(ls, 4) == 0);
  struct sockaddr_in got; socklen_t gn = sizeof got; memset(&got, 0, sizeof got);
  ok(getsockname(ls, (struct sockaddr*) &got, &gn) == 0 && got.sin_family == AF_INET && got.sin_port != 0);
  int pid = fork();
  if (pid == 0) {
    int c = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in t; memset(&t, 0, sizeof t);
    t.sin_family = AF_INET; t.sin_port = got.sin_port; t.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (c < 0 || connect(c, (struct sockaddr*) &t, sizeof t)) _exit(99);
    char m[8];
    if (write(c, "ping", 4) != 4 || read(c, m, 8) != 4 || memcmp(m, "pong", 4)) _exit(98);
    _exit(5); }
  struct sockaddr_in peer; socklen_t pn = sizeof peer; memset(&peer, 0, sizeof peer);
  int cf = accept(ls, (struct sockaddr*) &peer, &pn);
  ok(cf >= 0 && peer.sin_family == AF_INET);
  char m[8];
  ok(read(cf, m, 8) == 4 && memcmp(m, "ping", 4) == 0 && write(cf, "pong", 4) == 4);
  int err = -1; socklen_t el = sizeof err;
  ok(getsockopt(cf, SOL_SOCKET, SO_ERROR, &err, &el) == 0 && err == 0);
  int stx = 0;
  ok(waitpid(pid, &stx, 0) == pid && WIFEXITED(stx) && WEXITSTATUS(stx) == 5);
  ok(close(cf) == 0 && close(ls) == 0);

  /* UDP: sendto/recvfrom, the peer head translated on the way out */
  int u = socket(AF_INET, SOCK_DGRAM, 0);
  memset(&a, 0, sizeof a);
  a.sin_family = AF_INET; a.sin_port = 0; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  ok(u >= 0 && bind(u, (struct sockaddr*) &a, sizeof a) == 0);
  gn = sizeof got; memset(&got, 0, sizeof got);
  ok(getsockname(u, (struct sockaddr*) &got, &gn) == 0 && got.sin_port != 0);
  ok(sendto(u, "dgram", 5, 0, (struct sockaddr*) &got, sizeof got) == 5);
  pn = sizeof peer; memset(&peer, 0, sizeof peer);
  char db[8];
  ok(recvfrom(u, db, 8, 0, (struct sockaddr*) &peer, &pn) == 5
     && memcmp(db, "dgram", 5) == 0 && peer.sin_family == AF_INET && peer.sin_port == got.sin_port);
  ok(recvfrom(u, db, 8, MSG_DONTWAIT, 0, 0) == -1 && errno == EAGAIN);   /* the flag moved, the errno came back canonical */
  ok(close(u) == 0);

  /* a unix-path pair */
  unlink("/tmp/uvnet.sock");
  int us = socket(AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un ua; memset(&ua, 0, sizeof ua);
  ua.sun_family = AF_UNIX; strcpy(ua.sun_path, "/tmp/uvnet.sock");
  ok(us >= 0 && bind(us, (struct sockaddr*) &ua, sizeof ua) == 0 && listen(us, 2) == 0);
  int uc = socket(AF_UNIX, SOCK_STREAM, 0);
  ok(uc >= 0 && connect(uc, (struct sockaddr*) &ua, sizeof ua) == 0);
  int ua2 = accept(us, 0, 0);
  ok(ua2 >= 0 && write(uc, "x", 1) == 1 && read(ua2, m, 1) == 1 && m[0] == 'x');
  ok(close(uc) == 0 && close(ua2) == 0 && close(us) == 0 && unlink("/tmp/uvnet.sock") == 0);

  write(1, "net all\n", 8);
  return 42;
}
EOF
moon0 -t x64 "$d/uvnet.c" -o "$d/uvnet" || fail "uvnet: the default-lane compile"
ln=$("$d/uvnet" < /dev/null; echo "rc=$?")
fn=$($FBSD_SSH 'cat > /tmp/uvnet && chmod +x /tmp/uvnet && /tmp/uvnet; echo "rc=$?"' < "$d/uvnet") \
  || fail "uvnet: the box could not take or run it"
[ "$ln" = "$fn" ] || fail "uvnet: the kernels disagree -- linux[$ln] freebsd[$fn]"
echo "$ln" | grep -q "net all" || fail "uvnet battery -- got: $ln"
echo "$ln" | grep -q "rc=42" || fail "uvnet exit -- got: $ln"

echo "test_freebsd: UV-net -- the socket family, one binary, both kernels"

# ---- the trophy, opt-in by name (FBSD_SEED=1, minutes): the seed builds the
# seed ON THE BOX, and the bytes are the tree's own. the bake is budget-
# invariant now, so the box's budget (RAM economics) does not move the answer.
if [ -n "${FBSD_SEED:-}" ]; then
  out=$($FBSD_SSH 'rm -rf /tmp/seedrun && mkdir /tmp/seedrun && cd /tmp/seedrun \
    && cat > love && chmod +x love \
    && env LOVE_BUDGET_MB=512 ./love seed > seed.log 2>&1; tail -3 seed.log' < "$ho/love") \
    || fail "trophy: the box could not run the seed"
  echo "$out" | grep -q "fixpoint ok" || fail "trophy: the on-box seed missed the fixpoint -- got: $out"
  echo "test_freebsd: THE TROPHY -- the seed built the seed on freebsd, to the byte"
fi
