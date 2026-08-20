#!/bin/sh
# test/gate/riscv.sh -- the riscv64 codegen rung end to end: the whole test/cc battery
# compiled `mooncc -t riscv64` (EM_RISCV static ELF, the holo riscv backend), run under
# qemu-riscv64 user mode, and DIFFERENTIAL against the native x64 build of the same
# file. mooncc is its own reference here -- the frontend is shared, so only codegen can
# diverge.
#
# THE UNSUPPORTED FILES ARE ASSERTED, NOT SKIPPED, exactly as ccarch.sh does it: a
# listed program must REFUSE -- nonzero, no signal, a diagnostic naming both the file
# and the CAUSE. A silent skip list is where a regression hides. The day riscv64 grows
# one of these lanes its build starts succeeding, this check fails, and the name comes
# off the list.
#
# NOT set -e: both halves capture $? to compare them.
#
# usage: riscv.sh OUTDIR LOVE
set -u

ho=$1
m=$2

fail() { echo "FAIL $*" >&2; exit 1; }
moonrun() { LOVE_NO_IMAGE= "$m" mooncc "$@"; }

echo "RISCV test/cc battery (mooncc -t riscv64 vs native x64, under qemu-riscv64)"
if ! command -v qemu-riscv64 > /dev/null 2>&1 || [ "$(uname -m)" != x86_64 ]; then
  echo "test_riscv: skipped (needs qemu-riscv64 + an x86_64 host)"
  exit 0
fi

d=$ho/riscv
mkdir -p "$d"
p=0
r=0

# the features riscv64 has no lane for -- kept in step with ccarch.sh's riscv64 case
unsupported="100-complex 101-vla 102-bigstruct 111-int128 117-vastruct"

for f in test/cc/*.c; do
  b=$(basename "$f" .c)

  case " $unsupported " in
    *" $b "*)
      moonrun -t riscv64 "$f" "$d/rv_$b" > "$d/$b.log" 2>&1; st=$?
      [ "$st" -ne 0 ] \
        || fail "$b: mooncc -t riscv64 BUILT a program listed as unsupported -- take it off the list in this script"
      [ "$st" -lt 128 ] || fail "$b: mooncc died on a signal ($st) where a refusal was expected"
      grep -q "$f" "$d/$b.log" \
        || { cat "$d/$b.log" >&2; fail "$b: the refusal does not name the file"; }
      if grep -q 'cause unnamed' "$d/$b.log"; then
        cat "$d/$b.log" >&2
        fail "$b: refuses without naming a cause -- pin the site with nolane"
      fi
      r=$((r + 1))
      continue ;;
  esac

  moonrun -t riscv64 "$f" "$d/rv_$b" > /dev/null 2>&1 || fail "riscv compile $f"
  qemu-riscv64 "$d/rv_$b"; a=$?

  moonrun "$f" "$d/x_$b" > /dev/null 2>&1 || fail "x64 compile $f"
  "$d/x_$b"; x=$?

  [ "$a" -eq "$x" ] || fail "riscv battery $f (rv $a x64 $x)"
  p=$((p + 1))
done

echo "test_riscv: $p/$p battery files agree riscv-vs-x64, and $r unsupported ones refuse cleanly"
