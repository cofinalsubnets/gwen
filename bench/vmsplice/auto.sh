#!/bin/sh
# auto.sh -- the splice JIT pipeline, end to end on live closures (rung 1 of the
# automation; run.sh's body.c half was the hand-made proof of the same shape):
#   dis (love/ev.l) -> compose.l -> mooncc -> lift.l -> nif -> differential + timing.
# Run from the repo root: sh bench/vmsplice/auto.sh
set -e
R=$(cd "$(dirname "$0")/../.." && pwd)
cd "$R"
o=out/bench/vmsplice
m=out/host/love
mc=out/host/mooncc
mkdir -p $o
[ -x $m ]  || { echo "auto: no $m -- run make host first" >&2; exit 1; }
[ -x $mc ] || { echo "auto: no $mc -- run make host first" >&2; exit 1; }

# ⚠ LOVE_NO_GLAZE everywhere: a glazed sample is a native cell dis cannot read, and
# a glazed twin would make the interp row read ~9x too fast (run.sh's trap).
LOVE_NO_GLAZE=1 $m -l bench/vmsplice/samples.l bench/vmsplice/compose.l $o

for c in $o/comp-*.c; do
  nm=$(basename $c .c | sed s/^comp-//)
  $mc -I. -Iout/lib -c $c $o/comp-$nm.o
  # check.l binds the .o against THIS process (bind.l's bindcode, rung 2) and runs the
  # differential -- all in one love, the only place the bound addresses are valid.
  LOVE_NO_GLAZE=1 $m -l bench/vmsplice/samples.l -l bench/vmsplice/bind.l \
    bench/vmsplice/check.l $o/comp-$nm.o $nm
done
