#!/bin/sh
# ccsize.sh -- the SIZE half of the compiler differential, the table ccbench.sh's wall
# clock cannot answer: how much .text each lane lays, and how much of it is love's C
# rather than the libc underneath. Reads the binaries ccbench.sh leaves in out/bench/cc/
# (run it first), and writes two tables: the decomposition per lane, then mooncc against
# each native over the symbols both actually emit.
#
# The method, and every part of it is load-bearing:
#   * sizes are ADDRESS-GAP derived -- the distance to the next text symbol. Not
#     st_size, because mooncc's ELF does not carry it, and a comparison needs one
#     measure both sides answer. So every number here includes inter-fn padding (~1-2%).
#   * love's C is decided per lane by ITS OWN OBJECTS, never by the other binary. A
#     symbol the native binary lacks is NOT thereby mooncc runtime: gcc inlines 265 of
#     love's statics out of existence, and a set difference taken against the binary
#     reads every one of them as libc. That mistake put ~110 KB of love code in
#     mooncc's libc column for two fills of doc/archive/moon-diff.md.
#   * names are canonicalized first -- gcc ships `c0_lambda.isra.0` where mooncc ships
#     `c0_lambda`, and the clone's bytes belong to the parent. 53 symbols (7.5 KB) land
#     in the wrong column without it.
#
# ⚠ this counts what a lane SHIPS, which for a libc is half the question: what it can
# CALL is the other half, and ccdead.py answers that one.
#
# usage: ./ccsize.sh          (after ./ccbench.sh, or `make ccbench`)
R=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
W=$R/out/bench/cc
[ -d "$W" ] || { echo "ccsize: no $W -- run ./ccbench.sh first" >&2; exit 1; }

# the lanes ccbench built, in its own order; mooncc first so it is the numerator.
LANES=$(for l in mooncc gcc-musl clang-musl gcc clang; do
          [ -f "$W/love-$l" ] && echo "$l"; done)
[ -n "$LANES" ] || { echo "ccsize: no lane binaries under $W" >&2; exit 1; }

TD=${TMPDIR:-/tmp}/ccsize.$$
mkdir -p "$TD"; trap 'rm -rf "$TD"' EXIT INT TERM

# the clone suffixes gcc and clang bolt onto a specialized copy. Folded twice: a
# symbol can wear two (`.constprop.0.isra.0`).
canon='gsub(/\.(isra|part|constprop|cold|lto_priv|localalias|llvm)[._0-9]*$/,"",n)'

# "<name> <bytes>" for a binary's text symbols, gap-derived, clones summed into parent.
binsyms() { nm -n --defined-only "$1" 2>/dev/null | awk '
    toupper($2)=="T" { na[++k]=$3; ad[k]=strtonum("0x" $1) }
    END { for(i=1;i<=k;i++){ n=na[i]; '"$canon"'; '"$canon"'
                             s[n] += (i<k ? ad[i+1]-ad[i] : 0) }
          for(x in s) print x, s[x] }' | sort -k1,1; }
# the names a lane's own objects define -- its love C, everything else being libc/crt.
objsyms() { nm --defined-only "$@" 2>/dev/null | awk '
    toupper($2)=="T" { n=$3; '"$canon"'; '"$canon"'; print n }' | sort -u; }

for l in $LANES; do
  binsyms "$W/love-$l" > "$TD/bin.$l"
  if [ "$l" = mooncc ]; then
    # mooncc's own lane: love.o + m_am.o + the host objects (flat). ⚠ there is no
    # nolibc object to exclude -- the driver pulls those members itself, so they
    # reach the binary and never the object dir. The complement IS the libc.
    objsyms "$W/mooncc/love.o" "$W/mooncc/m_am.o" \
            $(ls "$W"/mooncc/*.o | grep -vE '/(love|nolibc|sys|m_[a-z0-9]+)\.o$') > "$TD/own.$l"
  else
    od=$W/o-love-$l
    objsyms "$od/love.o" "$od/am.o" "$od"/host/*.o > "$TD/own.$l"
  fi
done

echo ".text decomposed -- love's own C against the libc under it"
printf '%-12s %7s %11s %7s %11s %11s\n' lane own\# 'own bytes' libc\# 'libc bytes' total
for l in $LANES; do
  o=$(join    "$TD/bin.$l" "$TD/own.$l" | awk '{n++;s+=$2} END{printf "%d %d", n, s}')
  c=$(join -v1 "$TD/bin.$l" "$TD/own.$l" | awk '{n++;s+=$2} END{printf "%d %d", n, s}')
  echo "$l $o $c" | awk '{printf "%-12s %7d %11d %7d %11d %11d\n",$1,$2,$3,$4,$5,$3+$5}'
done

echo
echo "love's own C -- mooncc against each native"
[ -f "$TD/own.mooncc" ] || { echo "  (no mooncc lane)"; exit 0; }
for l in $LANES; do
  [ "$l" = mooncc ] && continue
  comm -12 "$TD/own.mooncc" "$TD/own.$l" > "$TD/both"
  a=$(join "$TD/bin.mooncc" "$TD/both" | awk '{s+=$2} END{print s+0}')
  b=$(join "$TD/bin.$l"     "$TD/both" | awk '{s+=$2} END{print s+0}')
  # what only one side emits: mooncc's are love statics the native inlined away.
  m=$(comm -23 "$TD/own.mooncc" "$TD/own.$l" | join "$TD/bin.mooncc" - | awk '{n++;s+=$2} END{printf "%d %d",n,s+0}')
  v=$(comm -13 "$TD/own.mooncc" "$TD/own.$l" | join "$TD/bin.$l"     - | awk '{n++;s+=$2} END{printf "%d %d",n,s+0}')
  echo "$l $(wc -l < "$TD/both") $a $b $m $v" | awk \
    '{printf "%-12s both emit %4d: %9d vs %9d = %.2fx | mooncc-only %4d/%8d  %s-only %3d/%7d\n",
             $1,$2,$3,$4,$3/$4,$5,$6,$1,$7,$8}'
done
