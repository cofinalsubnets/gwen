#!/bin/sh
# test/gate/ccarm64.sh -- the C battery's AARCH64 twin: every test/cc/*.c built by
# `mooncc -t arm64` AND by an aarch64 cross-gcc, both run under qemu-user, exit
# code and stdout required to agree.
#
# WHY IT EXISTS. test_moon has run this battery against gcc -O0 since the driver
# was born, and its own comment said "x86-64 only until arm64 parity" -- so for
# four backends out of five the differential simply did not exist. That is not a
# small gap: mooncc's x64 lane and its arm64 lane share the whole front end and
# most of gen.l, and a fault in the SHARED model can be masked on one target by a
# lane the other does not have.
#
# That is exactly what the first run of this gate found. `(x * 0x076be629) >> 27`
# on a `unsigned int` x -- test/cc/104-u32wrap.c's own de Bruijn ctz, the musl
# mallocng shape -- did not wrap to 32 bits on arm64. The bug was in the shared
# rule (a bare literal's value tuple is typed 'long, and u32bin? disqualifies on a
# long operand), and x64 was right only because it has a mul-IMMEDIATE form whose
# lane passes 'int by hand. arm64 has no such form, fell to the register lane, and
# read the literal's 'long. One rule, two targets, and only the second one told
# the truth.
#
# ⚠ it compares stdout as well as the exit code. An exit code is eight bits; for a
# battery whose programs return a count of passing checks that is enough to say
# THAT something drifted and never which check.
#
# THE THREE EXCLUSIONS are asserted, not skipped. 100-complex, 101-vla and
# 102-bigstruct use features the arm64 lane does not implement, and this gate
# requires mooncc to REFUSE them -- nonzero exit, a diagnostic naming the file.
# A silent skip would let a real regression hide behind the list, and a clean
# refusal turning into a crash is itself a bug. When arm64 gains one of these, its
# build starts succeeding, this check fails, and the name comes off the list --
# which is the reminder working as intended.
#
# Needs qemu-aarch64 + an aarch64 cross-gcc with a static libc; point AARCH64_CC
# at one, or let the auto-find try the usual names and the local Nerves toolchain.
# Skips cleanly (exit 0, a note) without either -- like test_arm64 / test_kernel.
# make owns the dependency graph; this owns the procedure.
# NOT set -e: the checks report their own failures with context.
#
# usage: ccarm64.sh OUTDIR LOVE
set -u

ho=$1
m=$2
d=$ho/ccarm64
mkdir -p "$d"

fail() { echo "FAIL test_ccarm64: $*" >&2; exit 1; }
moonrun() { "$m" --wake "$ho/mooncc.image" -e '(moon-main (cuup (cup cmdline)))' "$@"; }

# the arm64 lane does not implement these yet -- refusal is the asserted behaviour
unsupported="100-complex 101-vla 102-bigstruct"

QEMU=$(command -v qemu-aarch64 2>/dev/null || true)
GCC="${AARCH64_CC:-}"
[ -z "$GCC" ] && GCC=$(command -v aarch64-linux-gnu-gcc 2>/dev/null || true)
[ -z "$GCC" ] && GCC=$(command -v aarch64-nerves-linux-gnu-gcc 2>/dev/null || true)
[ -z "$GCC" ] && GCC=$(ls /usr/local/data/*/.nerves/artifacts/nerves_toolchain_aarch64*/bin/aarch64-nerves-linux-gnu-gcc 2>/dev/null | head -1)
if [ -z "$QEMU" ] || [ -z "$GCC" ]; then
  echo "test_ccarm64: skipped (need qemu-aarch64 + an aarch64 cross-gcc; set AARCH64_CC)"
  exit 0
fi

n=0
nref=0
for f in test/cc/*.c; do
  b=$(basename "$f" .c)

  case " $unsupported " in
    *" $b "*)
      # must refuse, and refuse CLEANLY: a diagnostic naming the file, not a crash
      if moonrun -t arm64 -o "$d/$b.m" "$f" > "$d/$b.mlog" 2>&1; then
        fail "$b: mooncc -t arm64 BUILT a program listed as unsupported -- take it off the list in this script"
      fi
      st=$?
      [ $st -lt 128 ] || fail "$b: mooncc died on a signal ($st) where a refusal was expected"
      grep -q "$f" "$d/$b.mlog" \
        || { cat "$d/$b.mlog" >&2; fail "$b: the refusal does not name the file"; }
      nref=$((nref + 1))
      continue ;;
  esac

  moonrun -t arm64 -o "$d/$b.m" "$f" > "$d/$b.mlog" 2>&1 \
    || { cat "$d/$b.mlog" >&2; fail "$b: mooncc -t arm64 could not build it"; }

  # -w: the battery is about the ANSWERS, and gcc warns about deliberate edges
  $GCC -O0 -w -static -o "$d/$b.g" "$f" 2> "$d/$b.glog" \
    || { cat "$d/$b.glog" >&2; fail "$b: the cross gcc could not build it"; }

  timeout 60 "$QEMU" "$d/$b.m" > "$d/$b.mout" 2>&1; ra=$?
  timeout 60 "$QEMU" "$d/$b.g" > "$d/$b.gout" 2>&1; rg=$?

  [ $ra -ne 124 ] || fail "$b: our binary timed out under qemu"
  [ $ra -eq $rg ] || fail "$b: exit ours $ra, $GCC $rg (aarch64, under qemu)"
  if ! cmp -s "$d/$b.mout" "$d/$b.gout"; then
    echo "--- $b: ours vs the cross gcc on aarch64 (first 20 differing lines) ---" >&2
    diff "$d/$b.gout" "$d/$b.mout" 2>/dev/null | head -20 >&2
    fail "$b: our aarch64 codegen and the cross gcc's disagree"
  fi
  n=$((n + 1))
done

[ $n -gt 0 ] || fail "no programs ran from test/cc/"

echo "test_ccarm64: $n programs agree with the cross gcc on aarch64 under qemu, and $nref unsupported ones refuse cleanly"
