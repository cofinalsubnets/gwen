#!/bin/sh
# moon-m4.sh -- build GNU m4 1.4 with mooncc + nolibc + the holo linker (no
# gcc/glibc/ld) and prove it RUNS: the package's OWN check suite (57 checks
# lifted from the m4 manual) green against our binary, plus a direct battery
# (define/eval/divert/esyscmd through popen/format floats). The fourth
# moon-userland rung (doc/archive/moon-userland.md), after bzip2, gzip and tar.
#
# m4's source is the one imported artifact. Point M4SRC at a CONFIGURED
# m4-1.4 tree (./configure already run, so config.h exists). Without one the
# check SKIPS (like moon-tar without TARSRC). To make one:
#   curl -O https://ftp.gnu.org/gnu/m4/m4-1.4.tar.gz
#   tar xzf m4-1.4.tar.gz && (cd m4-1.4 && ./configure)
#   make moon-m4       M4SRC=$PWD/m4-1.4
#   make moon-m4-arm64 M4SRC=$PWD/m4-1.4
#
# THE SOURCES ARE CACHED, so none of that is needed twice: this looks for
# `m4-1.4*` under dl/ and then under $MOONSRC -- ~/src when that is unset --
# so a bare `make moon-m4` finds a cached tree with no variable at all. An
# explicit M4SRC= still outranks both, and a missing tree is a clean SKIP
# rather than a failure, so this gate stays opt-in either way.
#
# TWO TARGETS, one procedure (raw.sh's shape, as moon-lua.sh and moon-sqlite.sh
# do it): `moon-m4.sh arm64` cross-compiles the same sources with `mooncc -t
# arm64` and runs everything under qemu-aarch64, SKIPPING cleanly without it.
# config.h is reused as configure wrote it, which is sound here and worth
# saying why: both targets are little-endian LP64, and the answers that differ
# between them are the ones the header already corrects by hand.
#
# â  THE CHECK SUITE NEEDS A WRAPPER on the cross target. check-them is a shell
# script that finds `m4` on PATH and execs it, and an aarch64 binary is not
# executable here (no binfmt_misc registration for qemu). So the cross lane
# puts a one-line `m4` script on PATH that execs qemu with the real binary --
# the suite then runs completely unmodified, which is the point of running it.
#
# Nothing here needs gcc EXCEPT the one-time ./configure probe that emits
# config.h (the accepted precedent -- mooncc COMPILES every object). Two of
# configure's answers describe GLIBC, not our target libc, so the build
# corrects them in place (config.h is a generated file; this IS configuration):
#   HAVE_EFGCVT  -- nolibc has no ecvt/fcvt/gcvt; format.c's sprintf branch
#                   is the right lane (and the better code).
#   USE_STACKOVF -- stack-overflow detection needs sigaltstack + sys/resource.h
#                   headers we don't carry yet; a nicety, off.
set -e

target=${1:-x64}
case $target in
  x64)   name=moon-m4       ; tflag=""         ; sub=moonm4
         mksys=mksys       ; backend=""              ; run=""            ; need="" ;;
  arm64) name=moon-m4-arm64 ; tflag="-t arm64" ; sub=moonm4-a64
         mksys=mksys-arm64 ; backend=crew/holo/arm64.l ; run=qemu-aarch64 ; need=qemu-aarch64 ;;
  riscv64) name=moon-m4-riscv ; tflag="-t riscv64" ; sub=moonm4-rv
         mksys=mksys-riscv ; backend=crew/holo/riscv.l ; run=qemu-riscv64 ; need=qemu-riscv64 ;;
  *) echo "moon-m4.sh: unknown target $target (x64 | arm64 | riscv64)" >&2; exit 1 ;;
esac

# where a package's sources may live, first hit wins: the tree-local dl,
# then the cache -- $MOONSRC, or ~/src when that is unset. An explicit *SRC=
# on the make line still outranks both. Answers EMPTY when nothing matches,
# which is what the skip branch below reads, so a missing tree is never an
# error and never a `set -e` abort.
pkgfind() {                        # pkgfind <dir-glob> <witness-file>
  for c in dl/$1 "${MOONSRC:-$HOME/src}"/$1; do
    [ -f "$c/$2" ] && { printf '%s\n' "$c"; return 0; }
  done
  return 0
}

ho=out/host
mc=$ho/mooncc
love=$ho/love
M4SRC=${M4SRC:-$(pkgfind 'm4-1.4*' config.h)}

if [ -n "$need" ] && ! command -v "$need" > /dev/null 2>&1; then
  echo "$name: no $need, skipped"
  exit 0
fi
if [ ! -f "$M4SRC/config.h" ]; then
  echo "$name: no configured m4-1.4 found (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "         set M4SRC=<a ./configure'd m4-1.4 tree> to run (see tools/moon-m4.sh)."
  exit 0
fi
for bin in "$mc" "$love"; do
  [ -x "$bin" ] || { echo "$name: missing $bin -- run 'make $ho/mooncc host'"; exit 1; }
done

# the target-libc corrections (idempotent; see the header comment)
sed -i 's|^#define HAVE_EFGCVT 2$|/* #undef HAVE_EFGCVT */|;s|^#define USE_STACKOVF 1$|/* #undef USE_STACKOVF */|' "$M4SRC/config.h"

d=$ho/$sub
rm -rf "$d"; mkdir -p "$d"

# m4's link set, as its src/Makefile OBJECTS + lib/Makefile OBJECTS chose --
# minus stackovf.o (USE_STACKOVF off) and alloca.o (HAVE_ALLOCA: nolibc's).
SRC="m4 builtin debug eval format freeze input macro output path symtab"
LIB="regex getopt getopt1 error obstack xmalloc xstrdup"
CFLAGS="-DSTDC_HEADERS=1 -DHAVE_CONFIG_H -Icrew/moon/include -I$M4SRC -I$M4SRC/src -I$M4SRC/lib"

echo "MOON-M4  $M4SRC  ($target: mooncc + nolibc + holo, no gcc/glibc/ld)"

objs=""
for b in $SRC; do
  $mc $tflag $CFLAGS -c "$M4SRC/src/$b.c" "$d/src_$b.o" || { echo "FAIL mooncc -c src/$b.c"; exit 1; }
  objs="$objs $d/src_$b.o"
done
for b in $LIB; do
  $mc $tflag $CFLAGS -c "$M4SRC/lib/$b.c" "$d/lib_$b.o" || { echo "FAIL mooncc -c lib/$b.c"; exit 1; }
  objs="$objs $d/lib_$b.o"
done

# the rung-4 libc floor: am math + the syscall leaf (mksys lays sys.o). ⚠ NO nolibc
# object -- the link owes its symbols and the driver's runtime table pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED (host/build.mk says the same of love itself).
# Naming an object would take every member instead.
for f in crew/moon/lib/math/*.c; do
  b=`basename "$f" .c`
  $mc $tflag -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
# sys.o is LAID, not compiled -- and a CROSS lay needs holo's backend loaded
# first (the host bake carries only the native one), exactly as raw.sh does it.
{ if [ -n "$backend" ]; then echo "(use 'holo)"; cat "$backend"; fi
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "($mksys \"$d/sys.o\")"; } | $love || { echo "FAIL $mksys sys.o"; exit 1; }

$mc $tflag $objs "$d"/m_*.o "$d/sys.o" -o "$d/m4" || { echo "FAIL holo link m4"; exit 1; }
echo "  linked $(wc -c < "$d/m4") bytes -> $d/m4"

# ---- prove it runs (absolute binary path -- the checks cd into work dirs) ----
m4bin=$(cd "$d" && pwd)/m4
# the name the check suite must find on PATH: the binary itself natively, or a
# wrapper that hands it to qemu (see the header -- check-them execs `m4`).
m4dir=$(cd "$d" && pwd)
if [ -n "$run" ]; then
  m4dir=$(cd "$d" && pwd)/bin
  mkdir -p "$m4dir"
  # â  -0 m4 matters: m4 prints its own argv[0] in every error message, and two
  # of the suite's checks compare stderr against a text that names it. without
  # it the wrapper's full path lands there and those two fail for a reason that
  # has nothing to do with the compiler.
  printf '#!/bin/sh\nexec %s -0 m4 %s "$@"\n' "$run" "$m4bin" > "$m4dir/m4"
  chmod +x "$m4dir/m4"
fi
$run "$m4bin" --version >/dev/null 2>&1 || { echo "FAIL m4 --version"; exit 1; }

# the direct battery: expansion, eval, diversions (tmpfile/rewind), esyscmd
# (popen), integer + float format
t=$(printf 'define(foo, bar)foo eval(7*6)\n' | $run "$m4bin")
[ "$t" = "bar 42" ] || { echo "FAIL define/eval: '$t'"; exit 1; }
t=$(printf 'divert(1)w\ndivert(0)h\ndivert\nundivert(1)' | $run "$m4bin" | tr -d '\n')
[ "$t" = "hw" ] || { echo "FAIL divert: '$t'"; exit 1; }
t=$(printf "esyscmd(\`echo pipe-ok')" | $run "$m4bin")
[ "$t" = "pipe-ok" ] || { echo "FAIL esyscmd: '$t'"; exit 1; }
t=$(printf "format(\`%%05d %%.2f %%e', 7, 3.14159, 12345.678)\n" | $run "$m4bin")
[ "$t" = "00007 3.14 1.234568e+04" ] || { echo "FAIL format: '$t'"; exit 1; }
echo "  OK define/eval + divert + esyscmd + format"

# the package's own check suite: 57 manual-derived checks, stdout AND stderr
# compared (the stderr legs read strerror texts -- nolibc's table matters)
( cd "$M4SRC/checks" && PATH="$m4dir:$PATH" sh ./check-them [0-9]* ) | tail -1 | grep -q "All checks successful" \
  || { echo "FAIL m4's own check suite"; exit 1; }
echo "  OK m4's own check suite (57 checks from the manual)"

echo "$name: GNU m4 1.4 built by mooncc + nolibc + holo$([ -n "$run" ] && echo " for $target"), runs + full check suite -- ok"
