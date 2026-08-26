#!/bin/sh
# test/gate/raw.sh -- the GCC-FREE fixpoint, for one target. Everything test_selfhost
# builds, PLUS our own raw libc (crew/moon/lib/nolibc.c: raw-syscall wrappers, mini
# stdio, mmap malloc), the math floor (crew/moon/lib/math/am.c, ours), and sys.o (the
# syscall trampoline + our sigsetjmp/longjmp, laid by crew/moon/lib/mksys.l) -- then
# OUR OWN static linker (crew/holo/link.l, via `mooncc a.o..`) binds them. No gcc, no
# glibc, no ld anywhere: the whole chain is love. Corpus green over the fresh egg.
#
# THREE targets, ONE procedure: x64 native, riscv64 and arm64 under qemu-user. They
# were three near-identical recipes; what actually differs is four things -- the -t
# flag, whether the holo backend has to be loaded for mksys (the host bake carries
# only the native one), which mksys entry lays the syscall leaf, and the runner. A
# fourth target is a case here, not another copy.
#
# make owns the dependency graph, the corpus list AND the source lanes; this owns the
# procedure. NOT set -e: the corpus run captures $? for its own failure message.
# ⚠ the lanes arrive in the environment because the corpus already has the variadic
# tail -- gate_love_c / gate_host_c, mk/common.mk's own. one folder, named lanes: a
# gate that globs a directory instead is a second authority on what the binary is.
#
# usage: gate_love_c=.. gate_host_c=.. raw.sh TARGET OUTDIR LOVE CORPUS.l ..
set -u

target=$1
ho=$2
m=$3
shift 3

case $target in
  x64)     name=test_raw        ; tflag=""           ; sub=raw     ; bin=love-raw
           out=.test_raw.out    ; mksys=mksys        ; backend=""
           run=""               ; need=""            ; pretty=x64 ;;
  riscv64) name=test_raw_riscv  ; tflag="-t riscv64" ; sub=raw-rv  ; bin=love-raw-rv
           out=.test_raw_rv.out ; mksys=mksys-riscv  ; backend=crew/holo/rv64.l
           run=qemu-riscv64     ; need=qemu-riscv64  ; pretty=riscv64 ;;
  arm64)   name=test_raw_arm64  ; tflag="-t arm64"   ; sub=raw-a64 ; bin=love-raw-a64
           out=.test_raw_a64.out; mksys=mksys-arm64  ; backend=crew/holo/arm64.l
           run=qemu-aarch64     ; need=qemu-aarch64  ; pretty=aarch64 ;;
  *) echo "raw.sh: unknown target $target" >&2; exit 1 ;;
esac

fail() { echo "FAIL $name: $*" >&2; exit 1; }

# x64 is the native lane: it needs no emulator but mksys/nolibc/math are x64-only,
# so it is the host arch that gates it. The cross lanes need their qemu.
if [ -z "$need" ]; then
  arch=$(uname -m)
  if [ "$arch" != x86_64 ]; then
    echo "$name: x86-64 only, skipped on $arch"
    exit 0
  fi
elif ! command -v "$need" > /dev/null 2>&1; then
  echo "$name: no $need, skipped"
  exit 0
fi

echo "RAW $ho/$bin"
d=$ho/$sub
mkdir -p "$d"
rm -f "$d"/*.o

# shellcheck disable=SC2086  # $tflag is a word pair or empty, deliberately unquoted
moonc() { LOVE_NO_IMAGE= "$m" mooncc $tflag "$@"; }

for f in $gate_love_c $gate_host_c; do
  b=$(basename "$f" .c)
  moonc -D ai_tco=1 -I"$ho" -I. -Isrc -Iout/lib -c "$f" "$d/$b.o" || fail "mooncc $tflag -c $f"
done

# nolibc is NOT compiled here: the link below owes its symbols and the driver's
# runtime table supplies them member by need (crew/moon/lib/nolibc/, test_drv's
# lane). Naming the objects would take every member, dead areas included.
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  moonc -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || fail "mooncc $tflag -c $f"
done

# sys.o is laid by mksys.l rather than compiled: it is the syscall trampoline and
# our own sigsetjmp/longjmp, which have no C spelling. A cross target must JOIN the
# sealed holo module and load its backend first -- the host bake carries only the
# native one, where mooncc.image carries them all.
{ if [ -n "$backend" ]; then
    echo "(use 'holo)"
    cat "$backend"
  fi
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l \
      crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "((from 'moon '$mksys) \"$d/sys.o\")"
} | "$m" || fail "$mksys sys.o"

moonc "$d"/*.o -o "$ho/$bin" || fail "our-linker bind $bin"

# the binary carries no baked image, so LOVE_NO_IMAGE forces the fresh-egg boot -- under a
# CEILING, like every other emulated corpus here (ktest.l's 420 s): the cross lanes run this
# under qemu, where a wedge and a slow run look the same from outside. 124 is the timeout's.
# the corpus goes in as a FILE with stdin closed, not on a pipe, for test_host's reason
# (test/test.mk): the corpus TESTS stdin, and `reads` no longer drains stdin ahead of the
# first form, so a piped corpus has test/io.l's see/unsee poking the script it is riding on.
cat "$@" > "$ho/.corpus.l"
LOVE_NO_IMAGE=1 timeout 420 $run "$ho/$bin" "$ho/.corpus.l" </dev/null > "$ho/$out" 2>&1
s=$?
tail -1 "$ho/$out"
[ $s -eq 0 ] && grep -q "tests pass" "$ho/$out" || fail "corpus (exit $s)"

case $target in
  x64) echo "test_raw: the src/*.c lanes + nolibc + am math + sys.o, our linker, no gcc/glibc/ld -- corpus passes" ;;
  *)   echo "$name: the gcc-free $pretty love -- mooncc objects, $mksys, our linker, corpus under qemu" ;;
esac
