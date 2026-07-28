#!/bin/sh
# test/gate/riscv.sh -- the riscv64 codegen rung end to end: the whole test/cc battery
# compiled `mooncc -t riscv64` (EM_RISCV static ELF, the holo riscv backend), run under
# qemu-riscv64 user mode, and DIFFERENTIAL against the native x64 build of the same
# file. mooncc is its own reference here -- the frontend is shared, so only codegen can
# diverge. Three battery files are x64-only features and are excluded exactly as the
# arm64 lane refuses them.
#
# NOT set -e: both halves capture $? to compare them.
#
# usage: riscv.sh OUTDIR LOVE
set -u

ho=$1
m=$2

fail() { echo "FAIL $*" >&2; exit 1; }
moonrun() { "$m" --wake "$ho/mooncc.image" -e '(moon-main (cuup (cup cmdline)))' "$@"; }

echo "RISCV test/cc battery (mooncc -t riscv64 vs native x64, under qemu-riscv64)"
if ! command -v qemu-riscv64 > /dev/null 2>&1 || [ "$(uname -m)" != x86_64 ]; then
  echo "test_riscv: skipped (needs qemu-riscv64 + an x86_64 host)"
  exit 0
fi

d=$ho/riscv
mkdir -p "$d"
p=0

for f in test/cc/*.c; do
  b=$(basename "$f" .c)
  case $b in 100-complex|101-vla|102-bigstruct) continue;; esac

  moonrun -t riscv64 "$f" "$d/rv_$b" > /dev/null 2>&1 || fail "riscv compile $f"
  qemu-riscv64 "$d/rv_$b"; a=$?

  moonrun "$f" "$d/x_$b" > /dev/null 2>&1 || fail "x64 compile $f"
  "$d/x_$b"; x=$?

  [ "$a" -eq "$x" ] || fail "riscv battery $f (rv $a x64 $x)"
  p=$((p + 1))
done

echo "test_riscv: $p/$p battery files agree riscv-vs-x64"
