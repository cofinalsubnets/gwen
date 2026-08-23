#!/bin/sh
# tools/moon-ablate.sh -- price the residency knobs (the pare plan's rung 0 harness).
# For each configuration: recompile ALL of love under MOON_ABLATE through the fixpoint
# gate (a configuration that cannot rebuild itself never reaches the timer), then read
# the love1 binary three ways -- perf cycles + instructions over the corpus (boot
# subtracted, median of N), and .text bytes. Deltas against the base row.
#   sh tools/moon-ablate.sh [samples] [conf ..]
# no confs: base ralloc tpool cs tpool,cs -- the gauge's A/B/C/D points.
# knob names: ralloc tpool cs lhome homes pcs (gen.l's ablenv).
set -u
R=$(cd "$(dirname "$0")/.." && pwd)
cd "$R" || exit 1
SAMPLES=${1:-3}
[ $# -gt 0 ] && shift
CONFS=${*:-"base ralloc tpool cs tpool,cs"}
WORK=$R/out/bench/ablate
mkdir -p "$WORK"
command -v perf >/dev/null || { echo "moon-ablate: no perf here"; exit 1; }

# the corpus as ONE file, fed by REDIRECT (ccbench's law: only a seekable fd 0 gets a
# read run; a pipe reads one byte at a time and dilutes every lane identically)
cat test/00-init.l test/spec.l test/uu.l \
    $(ls test/*.l | grep -vE '/(00-init|spec|glaze-x86|uu)\.l$' | LC_ALL=C sort) \
    > "$WORK/corpus.l"

# perf medians for one binary: EV1=cycles EV2=instructions, corpus minus boot
pmeasure() { # $1=bin -> "cycles insns"
  bin=$1; cf=$WORK/p.$$; : > "$cf.c"; : > "$cf.i"; : > "$cf.bc"; : > "$cf.bi"
  i=0
  while [ $i -lt "$SAMPLES" ]; do
    LOVE_NO_IMAGE=1 perf stat -x, -e cycles:u,instructions:u -o "$cf" \
      "$bin" < "$WORK/corpus.l" > /dev/null 2>&1 || { echo "dnf dnf"; return; }
    awk -F, '/cycles/{print $1}' "$cf" >> "$cf.c"
    awk -F, '/instructions/{print $1}' "$cf" >> "$cf.i"
    LOVE_NO_IMAGE=1 perf stat -x, -e cycles:u,instructions:u -o "$cf" \
      "$bin" < /dev/null > /dev/null 2>&1
    awk -F, '/cycles/{print $1}' "$cf" >> "$cf.bc"
    awk -F, '/instructions/{print $1}' "$cf" >> "$cf.bi"
    i=$((i + 1))
  done
  med() { sort -n "$1" | awk '{a[NR]=$1} END{print a[int((NR+1)/2)]}'; }
  awk -v c="$(med "$cf.c")" -v b="$(med "$cf.bc")" \
      -v ci="$(med "$cf.i")" -v bi="$(med "$cf.bi")" 'BEGIN{print c-b, ci-bi}'
  rm -f "$cf" "$cf.c" "$cf.i" "$cf.bc" "$cf.bi"
}

printf '%-12s %14s %8s %16s %8s %10s %7s\n' conf cycles d% insns d% text d%
BC=; BI=; BT=
for conf in $CONFS; do
  env=$conf; [ "$conf" = base ] && env=
  # every configuration compiles all of love and must close the fixpoint
  rm -rf out/host/moon out/host/fix
  if ! MOON_ABLATE=$env make test_fixpoint > "$WORK/fix.$conf.log" 2>&1; then
    printf '%-12s FAILS THE FIXPOINT (out/bench/ablate/fix.%s.log)\n' "$conf" "$conf"
    continue
  fi
  bin=$WORK/love1.$(printf '%s' "$conf" | tr , +)
  cp out/host/fix/love1 "$bin"
  txt=$(size -A "$bin" | awk '$1==".text"{print $2}')
  set -- $(pmeasure "$bin")
  cyc=$1; ins=$2
  if [ "$cyc" = dnf ]; then printf '%-12s DNF on the corpus\n' "$conf"; continue; fi
  if [ -z "$BC" ]; then BC=$cyc; BI=$ins; BT=$txt; fi
  awk -v n="$conf" -v c="$cyc" -v i="$ins" -v t="$txt" -v bc="$BC" -v bi="$BI" -v bt="$BT" \
    'BEGIN{printf "%-12s %14d %+7.1f%% %16d %+7.1f%% %10d %+6.1f%%\n",
           n, c, (c/bc-1)*100, i, (i/bi-1)*100, t, (t/bt-1)*100}'
done
echo "moon-ablate: deltas vs the FIRST row; +-0.7% is the cycle floor on this box (moon-gauge)"
# the last configuration left out/host ablated: dirty the moon objects so the next
# make rebuilds clean -- a seed against an ablated artifact reads FIXPOINT NOT OK
rm -rf "$R"/out/host/moon "$R"/out/host/fix
echo "moon-ablate: out/host/moon cleared -- the next make rebuilds the artifact clean"
