#!/bin/sh
# test/gate/ccarch.sh -- the C battery on a CROSS TARGET: every test/cc/*.c built
# by `mooncc -t <arch>`, run under qemu-user, and required to answer exactly what
# the same source answers on x86-64. TWO targets, ONE procedure (raw.sh's shape).
#
# WHY, and why the ORACLE is x86-64 rather than gcc. test_moon has run this battery
# against gcc -O0 since the driver was born, but only ever natively, and its own
# comment said "x86-64 only until arm64 parity" -- so for four of the five backends
# the differential did not exist. That gap is not incidental: mooncc's targets share
# the whole front end and most of gen.l, which is exactly the arrangement where a
# fault in the SHARED model is masked on one target by a lane the other lacks.
#
# The first arm64 run proved it. `(x * 0x076be629) >> 27` on an `unsigned int` x --
# test/cc/104-u32wrap.c's own de Bruijn ctz, the musl mallocng shape -- did not wrap
# to 32 bits. The faulty rule was shared by every target (a bare literal's value
# tuple is typed 'long, and u32bin? disqualifies on a long operand); x64 was right
# only because it has a mul-IMMEDIATE lane that passes 'int by hand, and arm64,
# having no such form, fell to the register lane and read the 'long. One rule, two
# targets, and only the second told the truth.
#
# So the reference is THE X86-64 BUILD OF THE SAME SOURCE, which test_moon already
# pins against gcc. The two gates compose into a complete argument: gcc pins x64,
# and x64 pins every other target. That also makes this gate work where no cross
# gcc exists at all -- riscv64 has qemu here but no cross toolchain, and asking
# "do my targets agree?" needs no third compiler. Where a cross gcc IS available
# (aarch64, via AARCH64_CC or the local Nerves toolchain) it is used as an
# ADDITIONAL oracle, because agreeing with x64 cannot catch a bug both share.
#
# ⚠ stdout is compared, not just the exit code. These programs return a COUNT of
# passing checks; eight bits can say THAT something moved and never which one.
#
# THE EXCLUSIONS ARE ASSERTED, NOT SKIPPED, and the list is PER TARGET. A program
# using a feature this target has no lane for must be REFUSED: nonzero exit, no
# signal, a diagnostic naming the file. A silent skip list is where a regression
# hides; and the day a target grows one of these its build starts succeeding, this
# check fails, and the name comes off that target's list -- which is exactly how
# 101-vla left arm64's (the lane was one `and` mask and three sp moves away from
# neutral, and the C99 VLA has ridden both backends since).
#
# Skips whole (exit 0, a note) without the target's qemu -- like test_raw_arm64 /
# test_riscv. make owns the dependency graph; this owns the procedure.
# NOT set -e: the checks report their own failures with context.
#
# usage: ccarch.sh ARCH OUTDIR LOVE     (ARCH: arm64 | riscv64)
set -u

arch=$1
ho=$2
m=$3

case $arch in
  arm64)   name=test_ccarm64 ; qemu=qemu-aarch64 ; pretty=aarch64
           ccenv=${AARCH64_CC:-}
           ccnames="aarch64-linux-gnu-gcc aarch64-nerves-linux-gnu-gcc"
           ccglob="/usr/local/data/*/.nerves/artifacts/nerves_toolchain_aarch64*/bin/aarch64-nerves-linux-gnu-gcc"
           ccvar=AARCH64_CC
           unsupported="100-complex 102-bigstruct 117-vastruct" ;;
  riscv64) name=test_ccriscv ; qemu=qemu-riscv64 ; pretty=riscv64
           ccenv=${RISCV64_CC:-}
           ccnames="riscv64-linux-gnu-gcc riscv64-unknown-linux-gnu-gcc riscv64-unknown-elf-gcc"
           ccglob=""
           ccvar=RISCV64_CC
           unsupported="100-complex 102-bigstruct 111-int128 117-vastruct 151-w128fuzz" ;;
  *) echo "ccarch.sh: unknown target $arch" >&2; exit 1 ;;
esac

# ⚠ ONE RUN'S WORTH, and no more: each case leaves a .g (a STATIC gcc binary, ~3.2 MB), a
# .glog, a .gout, a .t and a .tout, and nothing ever read them again -- 1192 files and 420 MB
# for arm64 alone, 94 MB for riscv, growing with every run. Clearing at the START rather than
# the end keeps the last run's artifacts for a post-mortem, which is the only time anyone wants
# them, while bounding the pile to a single run.
d=$ho/cc-$arch
rm -rf "$d"
mkdir -p "$d"

fail() { echo "FAIL $name: $*" >&2; exit 1; }
moonrun() { LOVE_NO_IMAGE= "$m" mooncc "$@"; }

# $unsupported comes from the case above: the features THIS target has no lane
# for yet -- refusal is the asserted behaviour, per target, not per gate.

QEMU=$(command -v "$qemu" 2>/dev/null || true)
if [ -z "$QEMU" ]; then
  echo "$name: skipped (need $qemu)"
  exit 0
fi
if [ "$(uname -m)" != x86_64 ]; then
  echo "$name: skipped (the reference build is native x86-64)"
  exit 0
fi

# the OPTIONAL extra oracle: a cross gcc with a static libc, if this box has one
GCC="$ccenv"
for c in $ccnames; do
  [ -n "$GCC" ] && break
  GCC=$(command -v "$c" 2>/dev/null || true)
done
if [ -z "$GCC" ] && [ -n "$ccglob" ]; then
  GCC=$(ls $ccglob 2>/dev/null | head -1)
fi

n=0
nref=0
ngcc=0
for f in test/cc/*.c; do
  b=$(basename "$f" .c)

  case " $unsupported " in
    *" $b "*)
      if moonrun -t "$arch" -o "$d/$b.t" "$f" > "$d/$b.tlog" 2>&1; then
        fail "$b: mooncc -t $arch BUILT a program listed as unsupported -- take it off the list in this script"
      fi
      st=$?
      [ $st -lt 128 ] || fail "$b: mooncc died on a signal ($st) where a refusal was expected"
      grep -q "$f" "$d/$b.tlog" \
        || { cat "$d/$b.tlog" >&2; fail "$b: the refusal does not name the file"; }
      nref=$((nref + 1))
      continue ;;
  esac

  # the reference: the SAME source on x86-64, which test_moon pins against gcc
  moonrun -o "$d/$b.x" "$f" > "$d/$b.xlog" 2>&1 \
    || { cat "$d/$b.xlog" >&2; fail "$b: mooncc could not build the x86-64 reference"; }
  timeout 60 "$d/$b.x" > "$d/$b.xout" 2>&1; rx=$?

  moonrun -t "$arch" -o "$d/$b.t" "$f" > "$d/$b.tlog" 2>&1 \
    || { cat "$d/$b.tlog" >&2; fail "$b: mooncc -t $arch could not build it"; }
  timeout 60 "$QEMU" "$d/$b.t" > "$d/$b.tout" 2>&1; rt=$?

  [ $rt -ne 124 ] || fail "$b: our $pretty binary timed out under qemu"
  [ $rt -eq $rx ] || fail "$b: exit $pretty $rt, x86-64 $rx -- the same source, two of our targets"
  if ! cmp -s "$d/$b.tout" "$d/$b.xout"; then
    echo "--- $b: our $pretty vs our x86-64 (first 20 differing lines) ---" >&2
    diff "$d/$b.xout" "$d/$b.tout" 2>/dev/null | head -20 >&2
    fail "$b: our $pretty codegen disagrees with our x86-64"
  fi

  # and against a real cross gcc where the box has one (catches a shared fault,
  # which agreeing with x64 cannot)
  if [ -n "$GCC" ]; then
    # -w: the battery is about the ANSWERS, and gcc warns about deliberate edges
    if $GCC -O0 -w -static -o "$d/$b.g" "$f" 2> "$d/$b.glog"; then
      timeout 60 "$QEMU" "$d/$b.g" > "$d/$b.gout" 2>&1; rg=$?
      [ $rt -eq $rg ] || fail "$b: exit ours $rt, $GCC $rg (on $pretty)"
      cmp -s "$d/$b.tout" "$d/$b.gout" || {
        echo "--- $b: ours vs the cross gcc on $pretty (first 20 lines) ---" >&2
        diff "$d/$b.gout" "$d/$b.tout" 2>/dev/null | head -20 >&2
        fail "$b: our $pretty codegen and the cross gcc's disagree"; }
      ngcc=$((ngcc + 1))
    else
      cat "$d/$b.glog" >&2
      fail "$b: the cross gcc could not build it"
    fi
  fi

  # ⚠ A PASSED CASE IS DEAD WEIGHT. `fail` exits, so reaching here means this program agreed
  # on every leg -- and the diffs are printed INLINE at the moment they disagree, so nothing
  # downstream ever reads these again. The .g is a STATICALLY LINKED cross binary, 3.3 MB, one
  # per program: 137 of them made cc-arm64 436 MB, 73% of the whole out/ tree, for a gate that
  # only runs in test_extra. A FAILING case keeps everything, which is the only time anyone
  # has ever wanted it.
  rm -f "$d/$b.g" "$d/$b.t" "$d/$b.tout" "$d/$b.gout" "$d/$b.glog" \
        "$d/$b.x" "$d/$b.xout" "$d/$b.xlog" "$d/$b.tlog"
  n=$((n + 1))
done

[ $n -gt 0 ] || fail "no programs ran from test/cc/"

if [ -n "$GCC" ]; then
  echo "$name: $n programs answer on $pretty exactly as on x86-64, $ngcc of them cross-checked against $(basename "$GCC"), and $nref unsupported ones refuse cleanly"
else
  echo "$name: $n programs answer on $pretty exactly as on x86-64 (no cross gcc here -- x64 is the oracle, and test_moon pins it), and $nref unsupported ones refuse cleanly"
fi
