#!/bin/sh
# test/gate/freebsd.sh -- rung 2's gate (doc/plan/seed-universal.md): mooncc
# -os freebsd lays a static freebsd/amd64 binary -- the impl.h table, the
# mksys-freebsd twin (CF -> -errno), crt0-fbsd, the EI_OSABI brand -- and a
# REAL freebsd runs it. the exerciser stays inside rung 2's proven surface:
# raw write/read/openat(O_RDONLY)/lseek/pipe/kill/clock/nanosleep, the er()
# convention (EBADF is 9 on both), and the sigsetjmp/siglongjmp mask dance
# (sigprocmask 340, twice). ⚠ NOT here on purpose: open-for-write (O_* values
# fork, rung 3), malloc (MAP_ANONYMOUS forks, rung 3), sigaction/readdir/fork
# (poisoned members, rung 3).
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

static int step = 0;
static void ok(int cond) {
  step++;
  if (!cond) { char b[3] = {'F', (char)('0' + step), '\n'}; write(2, b, 3); _exit(step); } }

int main(void) {
  ok(write(1, "hello, freebsd\n", 15) == 15);
  ok(getpid() > 0);
  ok(kill(getpid(), 0) == 0);
  ok(write(-1, "x", 1) == -1 && errno == 9);          /* EBADF: er() saw the carry */
  struct timespec ts;
  ok(clock_gettime(0, &ts) == 0 && ts.tv_sec > 0);
  struct timespec tn = {0, 1000000};
  ok(nanosleep(&tn, 0) == 0);
  int fds[2]; char c = 0;
  ok(pipe(fds) == 0 && write(fds[1], "x", 1) == 1 && read(fds[0], &c, 1) == 1 && c == 'x');
  int fd = open("/COPYRIGHT", 0); char buf[16];
  ok(fd >= 0 && read(fd, buf, 16) == 16 && lseek(fd, 0, 0) == 0 && close(fd) == 0);
  sigjmp_buf env;
  int r = sigsetjmp(env, 1);
  if (r == 0) siglongjmp(env, 7);
  ok(r == 7);
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
echo "$out" | grep -q "rc=42" || fail "wrong exit -- got: $out"

echo "test_freebsd: a mooncc-laid static freebsd/amd64 binary ran on the box -- the tables hold"
