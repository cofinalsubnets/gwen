#!/bin/sh
# test/gate/freebsd.sh -- rungs 2+3's gate (doc/plan/seed-universal.md): mooncc
# -os freebsd lays a static freebsd/amd64 binary -- the impl.h table, the
# mksys-freebsd twin (CF -> -errno), crt0-fbsd, the EI_OSABI brand, and rung
# 3's forked tables (O_*/MAP_*/SA_*/signal numbers/errno tail, the stat and
# dirent structs, the sigaction/sigprocmask/fork/dup2/readdir bodies) -- and a
# REAL freebsd runs it. ⚠ NOT here on purpose: termios proper and the socket
# family (sa_len, another constant set) -- rung 4's, with gates that need a
# tty and a wire.
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
moon0 -os freebsd -t x64 "$d/rung2.c" -o "$d/rung2" || fail "mooncc -os freebsd link"

# one byte says freebsd: e_ident[7] = 9
[ "$(dd if="$d/rung2" bs=1 skip=7 count=1 2>/dev/null | od -An -tu1 | tr -d ' ')" = 9 ] \
  || fail "EI_OSABI is not 9 -- the brand did not land"

out=$($FBSD_SSH 'cat > /tmp/rung2 && chmod +x /tmp/rung2 && /tmp/rung2; echo "rc=$?"' < "$d/rung2") \
  || fail "the box could not take or run the binary"
echo "$out" | grep -q "hello, freebsd" || fail "no greeting -- got: $out"
echo "$out" | grep -q "printf rides: 42" || fail "stdio did not ride -- got: $out"
echo "$out" | grep -q "rc=42" || fail "wrong exit -- got: $out"

echo "test_freebsd: a mooncc-laid static freebsd/amd64 binary ran on the box -- rungs 2+3 hold"

# ---- rung 4/5: the WHOLE love, -os freebsd, runs on the box ----
# every TU through mooncc (the host lane's flags wearing -os freebsd), mksys-freebsd
# the machine tail, our link -- then the -e lane, say, and the stdin repl answer there.
mkdir -p "$d/love"
moon0 -os freebsd -t x64 -D ai_tco=1 -D AiHaveVersionH -fno-ir -I"$ho" -I. -Icore -Iout/lib -c core/love.c "$d/love/love.o" || fail "-os freebsd core/love.c"
for f in host/*.c; do
  b=$(basename "$f" .c)
  moon0 -os freebsd -t x64 -D ai_tco=1 -I"$ho" -I. -Icore -Iout/lib -c "$f" "$d/love/host_$b.o" || fail "-os freebsd $f"
done
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  moon0 -os freebsd -t x64 -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/love/m_$b.o" || fail "-os freebsd $f"
done
"$love0" -l "$ho/.mksys-cat.l" -n -e "((from 'moon 'mksys-freebsd) \"$d/love/sys.o\")" || fail "mksys-freebsd"
moon0 -os freebsd -t x64 "$d/love"/*.o -o "$d/love/love-fbsd" || fail "the love link came up short"

# ⚠ -e RUNS MANY TIMES ON PURPOSE: argv must arrive whole on every exec. the
# freebsd kernel hands the vector base in %rdi and [rsp] may hold a pad word
# below argc, so a crt0 reading the wrong door flips by stack address, not by
# input -- one green run proves nothing.
$FBSD_SSH 'cat > /tmp/love-fbsd && chmod +x /tmp/love-fbsd' < "$d/love/love-fbsd" \
  || fail "the box could not take love"
out=$($FBSD_SSH 'for i in 1 2 3 4 5 6 7 8; do /tmp/love-fbsd -e "(quit 7)" < /dev/null; printf "%s" "$?"; done; echo
echo "(say out (show 42))" | /tmp/love-fbsd
echo
echo "(say out (show (sort (L 3 1 2))))" | /tmp/love-fbsd') \
  || fail "the box could not run love"
echo "$out" | grep -q "77777777" || fail "-e quit did not carry on every exec -- got: $out"
echo "$out" | grep -q "42" || fail "the say lane -- got: $out"
echo "$out" | grep -q "(1 2 3)" || fail "the stdin repl -- got: $out"

echo "test_freebsd: the WHOLE love (egg) answers on the box -- -e, say, and the repl"
