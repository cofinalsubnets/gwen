#!/bin/sh
# moon-gzip.sh -- build gzip 1.2.4 with mooncc + nolibc + the holo linker (no
# gcc/glibc/ld) and prove it RUNS: round-trips over four shapes of input, `-t`
# integrity, the `-l` listing, and -- the one that matters -- FORMAT ACCURACY
# both ways against the system gzip. The second moon-userland rung
#, after bzip2.
#
# 1.2.4 is the pre-gnulib gzip: 14 plain C89/K&R files, no gnulib link tree and
# no C23 header cascade, which is why it is the version that reaches a RUNNABLE
# binary. (1.13 compiles its 8 core files but its far end is ~100 gnulib TUs.)
#
# Point GZIPSRC at a CONFIGURED gzip-1.2.4 tree (its configure writes the
# Makefile this reads DEFS from -- we pass the same three by hand). Without one
# the check SKIPS. To make one:
#   curl -O https://ftp.gnu.org/gnu/gzip/gzip-1.2.4.tar.gz
#   tar xzf gzip-1.2.4.tar.gz && cd gzip-1.2.4 && CC="gcc -std=gnu89" ./configure
#   make moon-gzip GZIPSRC=$PWD/gzip-1.2.4
# and the sources are CACHED, so a bare `make moon-gzip` finds `gzip-1.2.4*`
# under dl/ then under $MOONSRC (~/src when unset). An explicit GZIPSRC= still
# outranks both, and a missing tree is a clean SKIP rather than a failure.
#
# ⚠ configure is not load-bearing here: DEFS is three macros this script passes
# anyway, so an UNconfigured tree builds too. It stays the witness because it is
# the one file that says the tree was prepared.
#
# THREE TARGETS, one procedure (raw.sh's shape): `moon-gzip.sh arm64` cross
# compiles and runs under qemu, SKIPPING cleanly without it.
#
# ⚠ THE ONE APP-SIDE EDIT, and it is a real 64-bit portability bug in gzip, not
# a mooncc gap: gzip.c calls `ctime` with no declaration in scope. On the 32-bit
# machines of 1993 the implicit `int` return was the same width as the pointer;
# on x86-64 it TRUNCATES the returned char*. We prepend `#include <time.h>` to a
# COPY, leaving the imported tree pristine.
set -e

target=${1:-x64}
case $target in
  x64)   name=moon-gzip       ; tflag=""            ; sub=moongzip
         mksys=mksys       ; backend=""               ; run=""            ; need="" ;;
  arm64) name=moon-gzip-arm64 ; tflag="-t arm64"    ; sub=moongzip-a64
         mksys=mksys-arm64 ; backend=crew/holo/arm64.l ; run=qemu-aarch64 ; need=qemu-aarch64 ;;
  riscv64) name=moon-gzip-riscv ; tflag="-t riscv64" ; sub=moongzip-rv
         mksys=mksys-riscv ; backend=crew/holo/riscv.l ; run=qemu-riscv64 ; need=qemu-riscv64 ;;
  *) echo "moon-gzip.sh: unknown target $target (x64 | arm64 | riscv64)" >&2; exit 1 ;;
esac

# where a package's sources may live, first hit wins: the tree-local dl, then
# the cache -- $MOONSRC, or ~/src when that is unset. Answers EMPTY when nothing
# matches, which is what the skip branch reads, so a missing tree is never an
# error and never a `set -e` abort.
pkgfind() {                        # pkgfind <dir-glob> <witness-file>
  for c in dl/$1 "${MOONSRC:-$HOME/src}"/$1; do
    [ -f "$c/$2" ] && { printf '%s\n' "$c"; return 0; }
  done
  return 0
}

ho=out/host
mc="$ho/love mooncc"
love=$ho/love
GZIPSRC=${GZIPSRC:-$(pkgfind 'gzip-1.2.4*' gzip.c)}

if [ -n "$need" ] && ! command -v "$need" > /dev/null 2>&1; then
  echo "$name: no $need, skipped"
  exit 0
fi
if [ ! -f "$GZIPSRC/gzip.c" ]; then
  echo "$name: no gzip-1.2.4 tree found (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "           set GZIPSRC=<a gzip-1.2.4 tree> to run (see tools/moon-gzip.sh)."
  exit 0
fi
[ -x "$love" ] || { echo "$name: missing $love -- run 'make host'"; exit 1; }

d=$ho/$sub
rm -rf "$d"; mkdir -p "$d/src"

# the imported tree stays pristine: build from a copy, and prepend <time.h> to
# the one file that needs it (see the header note).
cp "$GZIPSRC"/*.c "$GZIPSRC"/*.h "$d/src/"
printf '#include <time.h>\n' > "$d/src/gzip.c.new"
cat "$GZIPSRC/gzip.c" >> "$d/src/gzip.c.new"
mv "$d/src/gzip.c.new" "$d/src/gzip.c"

# match.S is the hand-written 8086 asm accelerator -- OFF (ASMV undefined), which
# is how every non-i386 build of 1.2.4 has always been configured. crypt.c is the
# stripped encryption stub and compiles to nothing, which is a CPP lane in its own
# right (an empty-after-cpp TU is a valid empty TU, not a failure).
SRC="gzip zip deflate trees bits unzip inflate util crypt lzw unlzw unpack unlzh getopt"
# exactly the DEFS its own configure writes on Linux.
CFLAGS="-DSTDC_HEADERS=1 -DHAVE_UNISTD_H=1 -DDIRENT=1 -Icrew/moon/include -I$d/src"

echo "MOON-GZIP  $GZIPSRC  ($target: mooncc + nolibc + holo, no gcc/glibc/ld)"

objs=""
for b in $SRC; do
  $mc $tflag $CFLAGS -c "$d/src/$b.c" "$d/$b.o" || { echo "FAIL mooncc -c $b.c"; exit 1; }
  objs="$objs $d/$b.o"
done

# the rung-4 libc floor: am math + the syscall leaf (mksys lays sys.o). ⚠ NO nolibc
# object -- the link owes its symbols and the driver's runtime table pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED (src/build.mk says the same thing about love
# itself). Naming an object would take every member instead.
for f in crew/moon/lib/math/*.c; do
  b=`basename "$f" .c`
  $mc $tflag -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
# sys.o is LAID, not compiled -- and a CROSS lay needs holo's backend loaded
# first (the host bake carries only the native one), exactly as raw.sh does it.
{ if [ -n "$backend" ]; then echo "(use 'holo)"; cat "$backend"; fi
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "((from 'moon '$mksys) \"$d/sys.o\")"; } | $love || { echo "FAIL $mksys sys.o"; exit 1; }

$mc $tflag $objs "$d"/m_*.o "$d/sys.o" -o "$d/gzip" || { echo "FAIL holo link gzip"; exit 1; }
echo "  linked $(wc -c < "$d/gzip") bytes -> $d/gzip"

# ---- prove it runs (absolute path -- the checks cd into a work dir) ----
gz=$(cd "$d" && pwd)/gzip
$run "$gz" --version >/dev/null 2>&1 || { echo "FAIL gzip --version"; exit 1; }

w=$d/work; rm -rf "$w"; mkdir -p "$w"

# four shapes, because the interesting failures are at the edges: nothing to
# compress, one byte, text that compresses hard, and bytes that do not compress
# at all (a stored/incompressible block is a different code path).
: > "$w/empty"
printf 'x' > "$w/one"
i=0; while [ $i -lt 400 ]; do printf 'the quick brown fox jumps over the lazy dog\n'; i=$((i+1)); done > "$w/text"
head -c 65536 /dev/urandom > "$w/rand"
cat "$GZIPSRC"/*.c > "$w/src.c"

for f in empty one text rand src.c; do
  cp "$w/$f" "$w/$f.keep"
  ( cd "$w" && $run "$gz" -9 "$f" ) || { echo "FAIL gzip -9 $f"; exit 1; }
  [ -f "$w/$f.gz" ] || { echo "FAIL gzip -9 $f produced no $f.gz"; exit 1; }
  ( cd "$w" && $run "$gz" -t "$f.gz" ) || { echo "FAIL gzip -t $f.gz"; exit 1; }
  ( cd "$w" && $run "$gz" -d "$f.gz" ) || { echo "FAIL gzip -d $f.gz"; exit 1; }
  cmp "$w/$f" "$w/$f.keep" || { echo "FAIL roundtrip not byte-identical: $f"; exit 1; }
done
echo "  OK roundtrip byte-identical (empty, 1 byte, text, incompressible, source)"

# the listing columns: -l reads its own header back, and it is the one place the
# printf field widths in nolibc's __fmt are load-bearing.
( cd "$w" && $run "$gz" -9 -c text > text.gz && $run "$gz" -l text.gz ) > "$w/list.out" 2>&1 \
  || { echo "FAIL gzip -l"; exit 1; }
grep -q 'uncompressed' "$w/list.out" || { echo "FAIL gzip -l wrote no header row"; cat "$w/list.out"; exit 1; }
echo "  OK -t integrity and -l listing"

# ---- FORMAT ACCURACY: the system gzip is the oracle, both directions ----
if command -v gzip >/dev/null 2>&1; then
  gzip -t "$w/text.gz" || { echo "FAIL system gzip cannot verify our .gz"; exit 1; }
  gzip -dc "$w/text.gz" | cmp - "$w/text" || { echo "FAIL system gzip decodes our .gz wrongly"; exit 1; }
  gzip -9 -c "$w/src.c" > "$w/theirs.gz"
  ( cd "$w" && $run "$gz" -dc theirs.gz ) | cmp - "$w/src.c" \
    || { echo "FAIL our gzip decodes the system .gz wrongly"; exit 1; }
  echo "  OK format-accurate both ways against the system gzip"
fi

echo "$name: gzip 1.2.4 built by mooncc + nolibc + holo$([ -n "$run" ] && echo " for $target"), runs + round-trips -- ok"
