#!/bin/sh
# test/gate/ulp.sh -- the MATH FLOOR differential: crew/moon/lib/math/am.c
# measured against the host libm, and -- the half that matters -- measured
# TWICE, once compiled by mooncc and once by the system cc, with the two
# reports required to agree BYTE FOR BYTE.
#
# WHY THE SECOND HALF EXISTS. `make ulp` has measured am.c since it was
# written, but always the $(CC) build of it, so it answered "is the ALGORITHM
# accurate" and never "does OUR COMPILER build it". am.c's own header claimed
# "the mooncc-compiled object measures IDENTICAL to gcc's" -- true when a hand
# check made it so, and quietly false afterwards. two mooncc bugs were living
# in that gap on 2026-07-29, both invisible to a green test_slow:
#
#   * the 4th integer parameter could be lost outright (its arrival register
#     taken as scratch after the ride analysis licensed it). am.c's mul64 lost
#     its `lo` out-pointer, so am_sin/am_cos SEGFAULTED for every |x| >= 2^19
#     -- reachable from the shipping binary as `(sine 1e20)`.
#   * an unsigned 64-bit value converted to double SIGNED, so (double)~0ull
#     was -1. rbig's G1 word crosses 2^63 on most inputs and sin/cos drifted
#     to 1609 ulp past the Payne-Hanek handoff -- while every argument BELOW
#     the handoff stayed exact, which is why nothing else noticed.
#
# neither is a math bug and neither shows in a value the corpus asserts. float
# BITS are where a codegen fault hides best: an ordinary C test compares ints
# and a wrong low mantissa bit still prints "0.1". so the differential is the
# instrument, and the mooncc side is the point of it.
#
# the ACCURACY half is gated too, against am.c's documented stance -- so a
# change to the algorithm that loses precision fails here rather than being
# discovered by a later differential. the bounds are the measured maxima; they
# are a CEILING, and beating one is free (lower the bound in the same commit).
#
# hosted only: the oracle IS the system libm, so this cannot run on the kernel
# or wasm -- exactly like `make ulp`, whose procedure this owns.
# make owns the dependency graph; this owns the procedure.
# NOT set -e: the checks report their own failures with context.
#
# usage: ulp.sh OUTDIR LOVE
set -u

ho=$1
m=$2
d=$ho/ulpgate            # NOT $ho/ulp -- that name is `make ulp`'s binary
mkdir -p "$d"

fail() { echo "FAIL test_ulp: $*" >&2; exit 1; }
moonrun() { "$m" --wake "$ho/mooncc.image" -e '(moon-main (cuup (cup cmdline)))' "$@"; }

arch=$(uname -m)
if [ "$arch" != x86_64 ]; then
  echo "test_ulp: x86-64 only (mooncc emits x64), skipped on $arch"
  exit 0
fi
cc_g=$(command -v gcc || command -v cc) || true
if [ -z "${cc_g:-}" ]; then
  echo "test_ulp: no system cc for the oracle, skipped"
  exit 0
fi

am=crew/moon/lib/math/am.c

# -- the two objects: same source, same harness, different compiler --
moonrun -c -o "$d/am_moon.o" "$am" > "$d/moon.build" 2>&1 \
  || { cat "$d/moon.build" >&2; fail "mooncc could not build $am"; }
$cc_g -O2 -c -o "$d/am_sys.o" "$am" 2> "$d/sys.build" \
  || { cat "$d/sys.build" >&2; fail "$cc_g could not build $am"; }

# the HARNESS is always the system cc's: it calls the libm oracle, and holding
# it fixed keeps the comparison about am.c's object and nothing else.
for w in moon sys; do
  $cc_g -O2 -o "$d/ulp_$w" tools/ulp.c "$d/am_$w.o" -lm 2> "$d/link_$w" \
    || { cat "$d/link_$w" >&2; fail "could not link the $w harness"; }
done

# -- both modes. `reduce` is the Payne-Hanek scan (|x| up to 2^1020); it is
# what caught the u64->double fault, since the default sweep stops at 2^8 and
# never reaches the reduction at all. --
for mode in sweep reduce; do
  case $mode in sweep) a= ;; reduce) a=reduce ;; esac
  for w in moon sys; do
    "$d/ulp_$w" $a > "$d/$mode.$w" 2>&1
    st=$?
    [ $st -eq 0 ] || fail "the $w harness died in the $mode run (exit $st)"
  done
  if ! cmp -s "$d/$mode.moon" "$d/$mode.sys"; then
    echo "--- $mode: mooncc's am.o vs $cc_g's (first 20 differing lines) ---" >&2
    diff "$d/$mode.sys" "$d/$mode.moon" 2>/dev/null | head -20 >&2
    fail "$mode: our compiler and $cc_g build am.c into different math"
  fi
done

# -- the accuracy ceiling: am.c's documented stance, per function.
# ⚠ `pow` sweeps x in [2^-40, 2^40] against y up to 2^8, so |y ln x| reaches
# the representable rim and its 7 is NOT the header's "<= 2 ulp typical" --
# that claim is about small |y ln x| and this sweep does not isolate it.
# `powrim` is deliberately at the rim and carries the documented ~40. --
nf=0
while read -r fn lim; do
  [ -n "$fn" ] || continue
  got=$(awk -v f="$fn" '$1 == f { print $3; exit }' "$d/sweep.sys")
  [ -n "$got" ] || fail "no '$fn' line in the report -- tools/ulp.c changed shape"
  awk -v g="$got" -v l="$lim" 'BEGIN { exit !(g + 0 <= l + 0) }' \
    || fail "$fn measured $got ulp, over its documented ceiling of $lim"
  nf=$((nf + 1))
done <<'EOF'
sqrt 0
exp 1
exp2 1
log 1
logn1 1
sin 2
cos 2
atan2 3
pow 7
powrim 37
EOF

echo "test_ulp: mooncc and $cc_g build am.c into byte-identical math (2 modes), and all $nf functions hold their ulp ceiling"
