#!/bin/sh
# test/gate/fat.sh -- the fat container (doc/plan/seed-universal.md U1): the ONE
# file answers on the native machine through its prefix + content-named cache,
# the cache holds (a second run lays nothing new), the pack is deterministic to
# the byte, and the foreign member extracts and answers under qemu-user (that
# leg skips loudly without one). HOME points into scratch so the cache the gate
# exercises is its own, never the nest's.
#
# usage: fat.sh FAT NATIVE_ARCH XARCH XQEMU BOOT_LOVE HO XD
set -u

fat=$1; a=$2; xa=$3; xqemu=$4; boot=$5; ho=$6; xd=$7
d=$ho/.fattest

fail() { echo "FAIL test_fat: $*" >&2; exit 1; }

rm -rf "$d" && mkdir -p "$d" || fail "scratch"

# the native lane, through the prefix and a fresh cache
HOME=$d "$fat" -e '(quit 7)'
[ $? -eq 7 ] || fail "native member did not answer through the prefix"
n1=$(ls "$d/.love/fat" | wc -l)
HOME=$d "$fat" -e '(quit 7)'
[ $? -eq 7 ] || fail "second run"
n2=$(ls "$d/.love/fat" | wc -l)
[ "$n1" = "$n2" ] || fail "the cache did not hold ($n1 -> $n2 entries)"

# determinism: the same members answer the same bytes
# ⚠ the member list MIRRORS crew/build.mk's dist-fat recipe (the drift trap).
"$boot" mk/tools/fatpack.l "$d/fat2" "$a" "$ho/love" "$xa" "$xd/love" >/dev/null \
  || fail "repack"
cmp -s "$fat" "$d/fat2" || fail "repack answered different bytes"

# the foreign member: read its case arm off the prefix, extract, run under qemu
if command -v "$xqemu" >/dev/null 2>&1; then
  set -- $(sed -n "s/^$xa) b=\([0-9]*\) n=\([0-9]*\) s=\([0-9]*\);;\$/\1 \2 \3/p" "$fat")
  [ $# -eq 3 ] || fail "no $xa arm in the prefix"
  dd if="$fat" of="$d/x.m" bs=4096 skip="$1" count="$2" 2>/dev/null || fail "extract"
  head -c "$3" "$d/x.m" > "$d/x.elf" && chmod +x "$d/x.elf"
  "$xqemu" "$d/x.elf" -e '(quit 7)'
  [ $? -eq 7 ] || fail "foreign member did not answer under $xqemu"
  echo "test_fat: ok -- native + cache + determinism + $xa under $xqemu"
else
  echo "test_fat: ok -- native + cache + determinism ($xa leg skipped: no $xqemu)"
fi
rm -rf "$d"
