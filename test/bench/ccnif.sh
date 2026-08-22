#!/bin/sh
# ccnif.sh -- mooncc's codegen against gcc's and clang's, on the three files in
# host/ that ask the most of it. NOT A GATE and deliberately not wired into one:
# it is a development instrument, run by hand while working on gen.l, and what it
# prints is three readings of the same source, not a verdict.
#
# THE SUBJECT is host/hash.c, host/deflate.c and host/inflate.c -- sha-256, md5,
# crc32, cksum, DEFLATE and inflate. They are the widest C the tree owns and the
# least like the rest of it: 32-bit rotates, a wrapping add carried over eight
# registers, two table walks reading EIGHT INDEPENDENT lookups a step, a 64-bit
# accumulator shifted by a runtime count, a hash-chain match finder, an insertion
# sort over packed keys, and an eight-in-order copy that is deliberately not a
# word move. love.c has none of those shapes, so nothing else here reads them --
# and mooncc compiles all three into the shipped artifact.
#
# ⚠ THE HARNESSES INCLUDE THE .c. Every entry point in the three files is a
# static, so a program that includes the source sees the algorithm whole and no
# seam had to be cut into host/ to reach it. What the love-facing wrappers name is
# stubbed (nif/stub.h) -- main() enters at the algorithm and the lvm ops are never
# called, they only have to link. That trick works for any host/*.c nif.
#
# THREE READINGS, and they answer different questions:
#
#   answers -- every lane runs the same report and the reports are diffed. A
#              divergence is a MISCOMPILE and the only place in this script where
#              one compiler can be said to be wrong. gcc runs at -O0 as well as
#              -O2, and those two are compared to EACH OTHER first: where the
#              oracle disagrees with itself the fault is undefined behaviour in
#              OUR source and not a mooncc bug, which is a different repair.
#   text    -- .text bytes per lane. The whole-file number is exact, off the
#              section header, and it is the only SOUND total: gcc and clang
#              inline statics out of existence, so a sum over the names two lanes
#              share charges mooncc for a callee its opposite number already paid
#              for inside a caller. Under it, the widest per-function ratios --
#              which is what actually names a lane worth working on.
#   time    -- the algorithms run for real, one row each, median of SAMPLES. The
#              shell holds the clock: the two builds carry different libcs, so a
#              program reading its own would time the clock as much as the code.
#
# ⚠ READ THE TIME ROWS IN PAIRS, never as one number. crc32 and cksum are branch-
# free table walks; sha-256 and md5 are register pressure with no memory in the
# loop; deflate is pointer chasing; inflate is a branch per symbol. mooncc level
# on one and far behind on another names the lane that wants work -- which is the
# whole reason this prints six rows and not an average. (test/bench/ccbench.sh
# times three of these too, but through the SHIPPED BINARY's nifs, where the love
# runtime and the libc are in the picture; here nothing is but the code.)
#
# usage: ./ccnif.sh [reps] [samples]
#   reps    passes over the timed corpus per run (default 24)
#   samples timed runs per row, median reported (default 3)
#
# x86-64 only -- mooncc emits x64 here. Needs `make host` first (out/host/love IS
# the compiler under test) and whichever of gcc/clang are on PATH.
set -u

R=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
REPS=${1:-24}
SAMPLES=${2:-3}
W=$R/out/test/bench/nif
m=$R/out/host/love

# a distro often symlinks gcc/clang through ccache; the time rows are the compiled
# CODE and not the compile, but the build times below would clock a cache hit
export CCACHE_DISABLE=1

[ -x "$m" ] || { echo "ccnif: no $m -- run \`make host\` first" >&2; exit 1; }
[ "$(uname -m)" = x86_64 ] || { echo "ccnif: x86-64 only (mooncc emits x64)" >&2; exit 1; }

rm -rf "$W"; mkdir -p "$W"
inc="-Icore -Itest/libc"

# the lanes, in report order; mooncc first so it is the numerator everywhere
lanes="mooncc gcc-O2 clang-O2 gcc-O0"
build() {                                        # build LANE SRC OUT
  case $1 in
    mooncc)   ( cd "$R" && LOVE_NO_IMAGE= "$m" mooncc $inc -o "$3" "$2" ) ;;
    gcc-O2)   ( cd "$R" && gcc   -O2 -w $inc -o "$3" "$2" ) ;;
    gcc-O0)   ( cd "$R" && gcc   -O0 -w $inc -o "$3" "$2" ) ;;
    clang-O2) ( cd "$R" && clang -O2 -w $inc -o "$3" "$2" ) ;;
  esac
}
object() {                                       # compile LANE SRC OUT, -c only
  case $1 in
    mooncc)   ( cd "$R" && LOVE_NO_IMAGE= "$m" mooncc $inc -c -o "$3" "$2" ) ;;
    gcc-O2)   ( cd "$R" && gcc   -O2 -w $inc -c -o "$3" "$2" ) ;;
    gcc-O0)   ( cd "$R" && gcc   -O0 -w $inc -c -o "$3" "$2" ) ;;
    clang-O2) ( cd "$R" && clang -O2 -w $inc -c -o "$3" "$2" ) ;;
  esac
}

have=
for l in $lanes; do
  case $l in mooncc) c=$m ;; gcc-*) c=$(command -v gcc) ;; clang-*) c=$(command -v clang) ;; esac
  [ -n "${c:-}" ] && have="$have $l"
done
echo "ccnif: lanes:$have   reps=$REPS samples=$SAMPLES"
echo

# ---------------------------------------------------------------- answers
echo "== answers =="
for b in sum gz; do
  ok=1
  for l in $have; do
    if ! build "$l" "test/bench/nif/$b.c" "$W/$b.$l" > "$W/$b.$l.build" 2>&1; then
      echo "  $b $l: DID NOT BUILD"; sed -n 1,5p "$W/$b.$l.build"; ok=0; continue
    fi
    "$W/$b.$l" > "$W/$b.$l.out" 2>&1 || { echo "  $b $l: died (exit $?)"; ok=0; }
  done
  [ $ok = 1 ] || continue
  # the oracle against itself first: a -O0/-O2 split is UB in the nif source
  if [ -f "$W/$b.gcc-O0.out" ] && [ -f "$W/$b.gcc-O2.out" ] \
     && ! cmp -s "$W/$b.gcc-O0.out" "$W/$b.gcc-O2.out"; then
    echo "  $b: gcc -O0 and -O2 DISAGREE -- undefined behaviour in the nif source, not a mooncc fault"
    diff "$W/$b.gcc-O0.out" "$W/$b.gcc-O2.out" | head -10
  fi
  n=$(wc -l < "$W/$b.mooncc.out")
  same=
  for l in $have; do
    [ "$l" = mooncc ] && continue
    if cmp -s "$W/$b.mooncc.out" "$W/$b.$l.out"; then same="$same $l"
    else
      echo "  $b: mooncc DIVERGES from $l -- a miscompile in one of them:"
      diff "$W/$b.$l.out" "$W/$b.mooncc.out" | head -10
    fi
  done
  echo "  $b: $n answers, mooncc ==$same"
done
echo

# ---------------------------------------------------------------- text
# gap-derived per function, exact per file. ⚠ mooncc's ELF carries no st_size, so
# a comparison has to use the measure every lane answers -- the distance to the
# next text symbol, which includes inter-function padding (~1-2%). the whole-file
# number below it is off the section header and is exact.
echo "== text bytes (lower is better; ratio is against mooncc) =="
syms() {   # "<name> <bytes>" for an object's text symbols, gap-derived, section-ended
  end=$(objdump -h "$1" | awk '$2==".text"{print strtonum("0x" $3)}')
  nm -n --defined-only "$1" 2>/dev/null | awk -v e="$end" '
    $2 ~ /^[tT]$/ { na[++k]=$3; ad[k]=strtonum("0x" $1) }
    END { for (i = 1; i <= k; i++) print na[i], (i<k ? ad[i+1]-ad[i] : e-ad[i]) }' | sort
}
printf '%-16s' "file"; for l in $have; do printf '%12s' "$l"; done; echo
for f in hash deflate inflate; do
  printf '%-16s' "host/$f.c"
  base=
  for l in $have; do
    object "$l" "host/$f.c" "$W/$f.$l.o" > /dev/null 2>&1 \
      || { printf '%12s' dnf; continue; }
    t=$(objdump -h "$W/$f.$l.o" | awk '$2==".text"{print strtonum("0x" $3)}')
    [ -n "$base" ] || base=$t
    printf '%12s' "$t"
  done
  echo
  syms "$W/$f.mooncc.o" > "$W/$f.mooncc.syms"
  for l in $have; do
    [ "$l" = mooncc ] && continue
    [ -f "$W/$f.$l.o" ] || continue
    syms "$W/$f.$l.o" > "$W/$f.$l.syms"
    mine=$(wc -l < "$W/$f.mooncc.syms")
    theirs=$(wc -l < "$W/$f.$l.syms")
    # ⚠ NO SUM OVER THE INTERSECTION. gcc and clang inline statics out of
    # existence, so a callee mooncc emits separately lives INSIDE the other
    # lane's caller: totalling the shared names charges mooncc for a function
    # its opposite number already paid for inside another row. The whole-file
    # line above is the sound total; these are per-function readings, worst
    # first, and each is still generous to the lane that inlined into it.
    join "$W/$f.mooncc.syms" "$W/$f.$l.syms" | awk -v l="$l" -v a="$mine" -v b="$theirs" '
      $3 > 0 { r[++n] = $2 / $3; nm[n] = $1 }
      END { printf "    vs %-10s %d fns against %d, %d in both", l, a, b, n
            if (!n) { print "" ; exit }
            for (i = 1; i < n; i++) for (j = i + 1; j <= n; j++)
              if (r[j] > r[i]) { t = r[i]; r[i] = r[j]; r[j] = t
                                 u = nm[i]; nm[i] = nm[j]; nm[j] = u }
            printf ";  widest:"
            for (i = 1; i <= 3 && i <= n; i++) printf " %s %.2fx", nm[i], r[i]
            printf "\n" }'
  done
done
echo

# ---------------------------------------------------------------- time
echo "== time, ms (median of $SAMPLES, $REPS reps; lower is better) =="
ms() {   # median wall-clock ms of SAMPLES runs of "$@"
  i=0
  while [ $i -lt "$SAMPLES" ]; do
    s=$(date +%s%N)
    "$@" > /dev/null 2>&1 || { echo dnf; return; }
    e=$(date +%s%N)
    echo $(( (e - s) / 1000000 ))
    i=$((i + 1))
  done | sort -n | awk -v n="$SAMPLES" 'NR == int((n+1)/2) { print }'
}
printf '%-16s' "row"; for l in $have; do printf '%12s' "$l"; done; echo
for row in "sum s sha256" "sum m md5" "sum c crc32" "sum k cksum" \
           "gz d deflate" "gz i inflate"; do
  set -- $row
  b=$1; sel=$2; lbl=$3
  printf '%-16s' "$lbl"
  base=
  for l in $have; do
    [ -x "$W/$b.$l" ] || { printf '%12s' dnf; continue; }
    v=$(ms "$W/$b.$l" "$sel" "$REPS")
    [ -n "$base" ] || base=$v
    if [ "$l" = mooncc ] || [ "$v" = dnf ] || [ "$base" = dnf ] || [ "$v" = 0 ]; then
      printf '%12s' "$v"
    else
      printf '%12s' "$v($(awk -v a="$base" -v b="$v" 'BEGIN{printf "%.2fx", a/b}'))"
    fi
  done
  echo
done
echo
echo "ccnif: the ratio in a time cell is mooncc/that lane -- 1.00x is parity, 3.00x is three times the wall clock."
