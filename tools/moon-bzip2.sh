#!/bin/sh
# moon-bzip2.sh -- build bzip2 1.0.8 with mooncc + nolibc + the holo linker (no
# gcc/glibc/ld) and prove it RUNS: round-trips at both block-size ends, `-t`
# integrity, and format accuracy both ways against the system bzip2. The FIRST
# moon-userland rung, and still the best-shaped one.
#
# bzip2 is the ideal first package and the reason is structural, not historical:
# ~7.3k lines of plain C89 and NO ./configure, so it isolates mooncc's C coverage
# from the shell-and-autotools bootstrap entirely. There is nothing to prepare --
# an EXTRACTED tree is a ready tree.
#
# Point BZIP2SRC at an extracted bzip2-1.0.8 tree, or let it find one:
#   curl -LO https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz
#   tar xzf bzip2-1.0.8.tar.gz
#   make moon-bzip2 BZIP2SRC=$PWD/bzip2-1.0.8
# a bare `make moon-bzip2` looks under dl/ then under $MOONSRC (~/src when unset).
# A missing tree is a clean SKIP rather than a failure -- these stay opt-in, and a
# green `make test_slow` says nothing about them.
#
# THREE TARGETS, one procedure (raw.sh's shape): `moon-bzip2.sh arm64` cross
# compiles and runs the round-trips under qemu, SKIPPING cleanly without it.
#
# ⚠ bzip2's own Makefile passes -D_FILE_OFFSET_BITS=64, and it is load-bearing
# rather than decoration: bzip2.c reaches for fopen64/fseeko64 without it on a
# glibc header set. Ours is not glibc, but the macro is what its sources expect
# to be true, so it is passed here exactly as its Makefile passes it.
set -e

target=${1:-x64}
case $target in
  x64)   name=moon-bzip2       ; tflag=""            ; sub=moonbz2
         mksys=mksys       ; backend=""               ; run=""            ; need="" ;;
  arm64) name=moon-bzip2-arm64 ; tflag="-t arm64"    ; sub=moonbz2-a64
         mksys=mksys-arm64 ; backend=crew/holo/arm64.l ; run=qemu-aarch64 ; need=qemu-aarch64 ;;
  riscv64) name=moon-bzip2-riscv ; tflag="-t riscv64" ; sub=moonbz2-rv
         mksys=mksys-riscv ; backend=crew/holo/riscv.l ; run=qemu-riscv64 ; need=qemu-riscv64 ;;
  *) echo "moon-bzip2.sh: unknown target $target (x64 | arm64 | riscv64)" >&2; exit 1 ;;
esac

pkgfind() {                        # pkgfind <dir-glob> <witness-file>
  for c in dl/$1 "${MOONSRC:-$HOME/src}"/$1; do
    [ -f "$c/$2" ] && { printf '%s\n' "$c"; return 0; }
  done
  return 0
}

ho=out/host
mc="$ho/love mooncc"
love=$ho/love
BZIP2SRC=${BZIP2SRC:-$(pkgfind 'bzip2-1.0.8*' bzlib.c)}

if [ -n "$need" ] && ! command -v "$need" > /dev/null 2>&1; then
  echo "$name: no $need, skipped"
  exit 0
fi
if [ ! -f "$BZIP2SRC/bzlib.c" ]; then
  echo "$name: no bzip2-1.0.8 tree found (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "            set BZIP2SRC=<an extracted bzip2-1.0.8 tree> to run (see tools/moon-bzip2.sh)."
  exit 0
fi
[ -x "$love" ] || { echo "$name: missing $love -- run 'make host'"; exit 1; }

d=$ho/$sub
rm -rf "$d"; mkdir -p "$d"

# the library's seven, plus the driver. bzip2recover/dlltest/spewG/unzcrash are
# its own side tools and not part of the binary.
SRC="blocksort huffman crctable randtable compress decompress bzlib bzip2"
CFLAGS="-D_FILE_OFFSET_BITS=64 -DBZ_UNIX=1 -Icrew/moon/include -I$BZIP2SRC"

echo "MOON-BZIP2  $BZIP2SRC  ($target: mooncc + nolibc + holo, no gcc/glibc/ld)"

objs=""
for b in $SRC; do
  $mc $tflag $CFLAGS -c "$BZIP2SRC/$b.c" "$d/$b.o" || { echo "FAIL mooncc -c $b.c"; exit 1; }
  objs="$objs $d/$b.o"
done

# the rung-4 libc floor: am math + the syscall leaf (mksys lays sys.o). ⚠ NO nolibc
# object -- the link owes its symbols and the driver's runtime table pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED (src/build.mk says the same of love itself).
# Naming an object would take every member instead.
for f in crew/moon/lib/math/*.c; do
  b=`basename "$f" .c`
  $mc $tflag -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
# sys.o is LAID, not compiled -- and a CROSS lay needs holo's backend loaded
# first (the host bake carries only the native one), exactly as raw.sh does it.
{ if [ -n "$backend" ]; then echo "(use 'holo)"; cat "$backend"; fi
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "((from 'moon '$mksys) \"$d/sys.o\")"; } | $love || { echo "FAIL $mksys sys.o"; exit 1; }

$mc $tflag $objs "$d"/m_*.o "$d/sys.o" -o "$d/bzip2" || { echo "FAIL holo link bzip2"; exit 1; }
echo "  linked $(wc -c < "$d/bzip2") bytes -> $d/bzip2"

# ---- prove it runs (absolute path -- the checks cd into a work dir) ----
bz=$(cd "$d" && pwd)/bzip2
w=$d/work; rm -rf "$w"; mkdir -p "$w"

# four shapes: nothing, one byte, text that compresses hard, and bytes that do
# not compress at all -- the edges are where a block coder goes wrong.
: > "$w/empty"
printf 'x' > "$w/one"
i=0; while [ $i -lt 800 ]; do printf 'the quick brown fox jumps over the lazy dog\n'; i=$((i+1)); done > "$w/text"
head -c 65536 /dev/urandom > "$w/rand"
cat "$BZIP2SRC"/*.c > "$w/src.c"

# -1 and -9 are different block sizes, so both ends of the sort get walked.
for lvl in 1 9; do
  for f in empty one text rand src.c; do
    cp "$w/$f" "$w/$f.keep"
    ( cd "$w" && $run "$bz" -$lvl -f "$f" ) || { echo "FAIL bzip2 -$lvl $f"; exit 1; }
    [ -f "$w/$f.bz2" ] || { echo "FAIL bzip2 -$lvl $f produced no $f.bz2"; exit 1; }
    ( cd "$w" && $run "$bz" -t "$f.bz2" ) || { echo "FAIL bzip2 -t $f.bz2"; exit 1; }
    ( cd "$w" && $run "$bz" -d -f "$f.bz2" ) || { echo "FAIL bzip2 -d $f.bz2"; exit 1; }
    cmp "$w/$f" "$w/$f.keep" || { echo "FAIL roundtrip not byte-identical: $f at -$lvl"; exit 1; }
  done
done
echo "  OK roundtrip byte-identical at -1 and -9 (empty, 1 byte, text, incompressible, source)"

# ---- FORMAT ACCURACY: the system bzip2 is the oracle, both directions ----
if command -v bzip2 >/dev/null 2>&1; then
  ( cd "$w" && $run "$bz" -9 -c src.c > ours.bz2 ) || { echo "FAIL bzip2 -c"; exit 1; }
  bzip2 -t "$w/ours.bz2" || { echo "FAIL system bzip2 cannot verify our .bz2"; exit 1; }
  bzip2 -dc "$w/ours.bz2" | cmp - "$w/src.c" || { echo "FAIL system bzip2 decodes our .bz2 wrongly"; exit 1; }
  bzip2 -9 -c "$w/src.c" > "$w/theirs.bz2"
  ( cd "$w" && $run "$bz" -dc theirs.bz2 ) | cmp - "$w/src.c" \
    || { echo "FAIL our bzip2 decodes the system .bz2 wrongly"; exit 1; }
  echo "  OK format-accurate both ways against the system bzip2"
fi

echo "$name: bzip2 1.0.8 built by mooncc + nolibc + holo$([ -n "$run" ] && echo " for $target"), runs + round-trips -- ok"
