#!/bin/sh
# vmsplice -- price the VM-splice JIT shape, and what mooncc's slot traffic costs it.
# Run from the repo root: sh bench/vmsplice/run.sh
#
# Two halves:
#   splice.c  composed-vs-dispatched over a non-folding chain walk, built by BOTH
#             compilers -- the cc column is what the shape is worth, the mooncc column
#             is what we get today. The difference is the lever.
#   body.c    a self-contained composed body: mooncc it, lift the bytes out of the .o
#             with holo's own reader (lift.l), install through `nif`, check it agrees
#             with its interp twin and time it (install.l).
set -e
R=$(cd "$(dirname "$0")/../.." && pwd)
cd "$R"
o=out/bench/vmsplice
m=out/host/love
mc=out/host/mooncc
CC=${CC:-cc}
mkdir -p $o
[ -x $m ] || { echo "vmsplice: no $m -- run make host first" >&2; exit 1; }
[ -x $mc ] || { echo "vmsplice: no $mc -- run make host first" >&2; exit 1; }

echo "== splice.c: composed vs dispatched =="
# core/love.c per compiler: the probe links against the real VM, so each column is
# self-consistent. liblove.a carries the am_* math floor.
if command -v $CC >/dev/null 2>&1; then
  $CC -O2 -std=c2x -I. -Iout/lib -c core/love.c            -o $o/love-cc.o
  $CC -O2 -std=c2x -I. -Iout/lib -c bench/vmsplice/splice.c -o $o/splice-cc.o
  $CC $o/splice-cc.o $o/love-cc.o out/host/liblove.a -o $o/splice-cc -lm 2>/dev/null
  echo "-- $CC --"; $o/splice-cc
else
  echo "-- $CC not on PATH, skipped --"
fi
$mc -I. -Iout/lib -c core/love.c                  $o/love-mc.o
$mc -I. -Iout/lib -c bench/vmsplice/splice.c $o/splice-mc.o
$CC $o/splice-mc.o $o/love-mc.o out/host/liblove.a -o $o/splice-mc -lm 2>/dev/null
echo "-- mooncc --"; $o/splice-mc

echo
echo "== body.c: mooncc -> lift -> nif, in a live love =="
$mc -I. -Iout/lib -c bench/vmsplice/body.c $o/body.o
$m bench/vmsplice/lift.l $o/body.o jitbody $o/jitcode.l
echo "-- glaze on (the specializing tier is the baseline) --"
$m -l $o/jitcode.l bench/vmsplice/install.l
echo "-- glaze off (the true interpreter baseline) --"
LOVE_NO_GLAZE=1 $m -l $o/jitcode.l bench/vmsplice/install.l
