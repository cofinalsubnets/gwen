#!/bin/sh
# moon-tar.sh -- build GNU tar 1.13 with mooncc + nolibc + the holo linker (no
# gcc/glibc/ld) and prove it RUNS: a cf/xf roundtrip byte-identical to the tree
# it archived, a czf/xzf roundtrip (tar forks gzip through a pipe), and interop
# with the system tar reading our archive. The third moon-userland rung
# (doc/archive/moon-userland.md), after bzip2 and gzip.
#
# tar's source is the one imported artifact. Point TARSRC at a CONFIGURED
# tar-1.13 tree (./configure already run, so config.h exists). Without one the
# check SKIPS (like test_raw_arm64 without qemu). To make one:
#   curl -O https://ftp.gnu.org/gnu/tar/tar-1.13.tar.gz
#   tar xzf tar-1.13.tar.gz && cd tar-1.13
#   cp /usr/share/automake-*/config.{sub,guess} .   # 1999 vintage: no x86_64
#   CC="gcc -std=gnu89" ./configure                 # its probes are implicit-int
#   make moon-tar       TARSRC=$PWD/tar-1.13
#   make moon-tar-arm64 TARSRC=$PWD/tar-1.13
#
# THE SOURCES ARE CACHED, so none of that is needed twice: this looks for
# `tar-1.13*` under dl/ and then under $MOONSRC -- ~/src when that is unset --
# so a bare `make moon-tar` finds a cached tree with no variable at all. An
# explicit TARSRC= still outranks both, and a missing tree is a clean SKIP
# rather than a failure, so this gate stays opt-in either way.
#
# (Those two lines are about CONFIGURE, not about us: tar 1.13 predates x86-64,
# and modern gcc makes the implicit-int `main(){return(0);}` of its probes a
# hard error. mooncc compiles every actual source either way.)
#
# TWO TARGETS, one procedure (raw.sh's shape, as moon-lua/sqlite/m4 do it):
# `moon-tar.sh arm64` cross-compiles with `mooncc -t arm64` and runs the
# roundtrips under qemu-aarch64, SKIPPING cleanly without it. config.h is
# reused as configure wrote it for the host -- sound here because both targets
# are little-endian LP64.
#
# Nothing here needs gcc EXCEPT the one-time ./configure probe that emits
# config.h (the accepted precedent -- mooncc COMPILES every object). The system
# tar/gzip are used only to VERIFY our binary, never to build it.
set -e

target=${1:-x64}
case $target in
  x64)   name=moon-tar       ; tflag=""         ; sub=moontar
         mksys=mksys       ; backend=""              ; run=""            ; need="" ;;
  arm64) name=moon-tar-arm64 ; tflag="-t arm64" ; sub=moontar-a64
         mksys=mksys-arm64 ; backend=crew/holo/arm64.l ; run=qemu-aarch64 ; need=qemu-aarch64 ;;
  riscv64) name=moon-tar-riscv ; tflag="-t riscv64" ; sub=moontar-rv
         mksys=mksys-riscv ; backend=crew/holo/riscv.l ; run=qemu-riscv64 ; need=qemu-riscv64 ;;
  *) echo "moon-tar.sh: unknown target $target (x64 | arm64 | riscv64)" >&2; exit 1 ;;
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
TARSRC=${TARSRC:-$(pkgfind 'tar-1.13*' config.h)}

if [ -n "$need" ] && ! command -v "$need" > /dev/null 2>&1; then
  echo "$name: no $need, skipped"
  exit 0
fi
if [ ! -f "$TARSRC/config.h" ]; then
  echo "$name: no configured tar-1.13 found (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "          set TARSRC=<a ./configure'd tar-1.13 tree> to run (see tools/moon-tar.sh)."
  exit 0
fi
for bin in "$mc" "$love"; do
  [ -x "$bin" ] || { echo "$name: missing $bin -- run 'make $ho/mooncc host'"; exit 1; }
done
command -v tar  >/dev/null 2>&1 || { echo "$name: no system tar to verify against -- skipped"; exit 0; }

d=$ho/$sub
rm -rf "$d"; mkdir -p "$d"

# tar's link set, exactly as its configure chose: the 15 binary objects + the
# 21 libtar.a objects. STDC_HEADERS is what config.h defines; pre-C89 gnulib
# TUs (argmatch.c) omit <config.h>, so pass it on the line to pull <string.h>.
SRC="arith buffer compare create delete extract incremen list mangle misc names open3 rtapelib tar update"
# fnmatch: tar's OWN bundled lib/fnmatch.c (configure drops it from libtar.a
# only because it found a system fnmatch; mooncc compiles it clean).
LIB="addext argmatch backupfile basename error exclude fnmatch full-write getdate getopt getopt1 modechange msleep quotearg safe-read xgetcwd xmalloc xstrdup xstrtol xstrtoul xstrtoumax mktime"
CFLAGS="-DSTDC_HEADERS=1 -DHAVE_CONFIG_H -Icrew/moon/include -I$TARSRC -I$TARSRC/src -I$TARSRC/lib -I$TARSRC/intl"

echo "MOON-TAR  $TARSRC  ($target: mooncc + nolibc + holo, no gcc/glibc/ld)"

objs=""
for b in $SRC; do
  $mc $tflag $CFLAGS -c "$TARSRC/src/$b.c" "$d/src_$b.o" || { echo "FAIL mooncc -c src/$b.c"; exit 1; }
  objs="$objs $d/src_$b.o"
done
for b in $LIB; do
  $mc $tflag $CFLAGS -c "$TARSRC/lib/$b.c" "$d/lib_$b.o" || { echo "FAIL mooncc -c lib/$b.c"; exit 1; }
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

$mc $tflag $objs "$d"/m_*.o "$d/sys.o" -o "$d/tar" || { echo "FAIL holo link tar"; exit 1; }
echo "  linked $(wc -c < "$d/tar") bytes -> $d/tar"

# ---- prove it runs (absolute binary path -- the checks cd into work dirs) ----
tarbin=$(cd "$d" && pwd)/tar
$run "$tarbin" --version >/dev/null 2>&1 || { echo "FAIL tar --version"; exit 1; }

w=$d/work; rm -rf "$w"; mkdir -p "$w/src/sub"
printf 'hello from love-tar\n'          > "$w/src/a.txt"
printf 'second file, some content\n'  > "$w/src/sub/b.txt"
head -c 4096 /dev/urandom             > "$w/src/blob.bin"
ln -s a.txt "$w/src/link"
chmod 0644 "$w/src/a.txt"; chmod 0600 "$w/src/sub/b.txt"

# plain cf/xf roundtrip
( cd "$w" && $run "$tarbin" cf out.tar src ) || { echo "FAIL tar cf"; exit 1; }
mkdir -p "$w/ex" && ( cd "$w/ex" && $run "$tarbin" xf ../out.tar ) || { echo "FAIL tar xf"; exit 1; }
diff -r "$w/src" "$w/ex/src" || { echo "FAIL cf/xf roundtrip not byte-identical"; exit 1; }
echo "  OK cf/xf roundtrip byte-identical (perms + symlink preserved)"

# system tar reads our archive (interop)
tar tf "$w/out.tar" >/dev/null 2>&1 || { echo "FAIL system tar cannot read our archive"; exit 1; }
echo "  OK system tar reads our archive"

# gzip'd roundtrip: tar forks gzip through a pipe (fork/execvp/pipe/wait)
if command -v gzip >/dev/null 2>&1; then
  ( cd "$w" && $run "$tarbin" czf out.tgz src ) || { echo "FAIL tar czf"; exit 1; }
  mkdir -p "$w/exz" && ( cd "$w/exz" && $run "$tarbin" xzf ../out.tgz ) || { echo "FAIL tar xzf"; exit 1; }
  diff -r "$w/src" "$w/exz/src" || { echo "FAIL czf/xzf roundtrip not byte-identical"; exit 1; }
  echo "  OK czf/xzf roundtrip (forked gzip through a pipe)"
fi

echo "$name: GNU tar 1.13 built by mooncc + nolibc + holo$([ -n "$run" ] && echo " for $target"), runs + roundtrips -- ok"
