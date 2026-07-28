#!/bin/sh
# test/gate/raw.sh -- the GCC-FREE fixpoint, the rung-4 gate. Everything
# test_selfhost builds, PLUS our own raw libc (crew/moon/lib/nolibc.c: raw-syscall
# wrappers, mini stdio, mmap malloc), the math floor (crew/moon/lib/math/am.c, ours),
# and sys.o (the syscall trampoline + our sigsetjmp/longjmp, laid by
# crew/moon/lib/mksys.l) -- then OUR OWN static linker (crew/holo/link.l, via
# `mooncc a.o..`) binds them. No gcc, no glibc, no ld anywhere: the whole chain is
# love. Corpus green over the fresh egg.
#
# make owns the dependency and the corpus list; this owns the procedure.
# NOT set -e: the corpus run captures $? for its own failure message.
#
# usage: raw.sh OUTDIR LOVE CORPUS.l ..
set -u

ho=$1
m=$2
shift 2

arch=$(uname -m)
if [ "$arch" != x86_64 ]; then
  echo "test_raw: x86-64 only, skipped on $arch"
  exit 0
fi

fail() { echo "FAIL test_raw: $*" >&2; exit 1; }
moonc() { "$ho/mooncc" "$@"; }

echo "RAW $ho/love-raw"
d=$ho/raw
mkdir -p "$d"
rm -f "$d"/*.o

moonc -D ai_tco=1 -I"$ho" -I. -Iout/lib -c love.c "$d/love.o" || fail "mooncc -c love.c"

for f in host/*.c; do
  b=$(basename "$f" .c)
  moonc -D ai_tco=1 -I"$ho" -I. -Iout/lib -c "$f" "$d/$b.o" || fail "mooncc -c $f"
done

moonc -Icrew/moon/include -c crew/moon/lib/nolibc.c "$d/nolibc.o" || fail "mooncc -c nolibc.c"

for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  moonc -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || fail "mooncc -c $f"
done

# sys.o is laid by mksys.l rather than compiled: it is the syscall trampoline
# and our own sigsetjmp/longjmp, which have no C spelling.
{ cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l \
      crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "(mksys \"$d/sys.o\")"
} | "$m" || fail "mksys sys.o"

moonc "$d"/*.o -o "$ho/love-raw" || fail "our-linker bind love-raw"

# the binary carries no baked image, so LOVE_NO_IMAGE forces the fresh-egg boot
out=$ho/.test_raw.out
cat "$@" | LOVE_NO_IMAGE=1 "$ho/love-raw" > "$out" 2>&1
s=$?
tail -1 "$out"
[ $s -eq 0 ] && grep -q "tests pass" "$out" || fail "all-raw corpus (exit $s)"

echo "test_raw: love.c + host/*.c + nolibc + am math + sys.o, our linker, no gcc/glibc/ld -- corpus passes"
