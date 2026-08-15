#!/bin/sh
# moon-sweep.sh -- measure the DISTANCE between mooncc and a gnulib-using GNU
# package: compile every lib/*.c, score against the object list automake
# actually builds on this platform, and bucket the failures by first cause.
# The before/after harness for; the method the tar rung used
# (nm-diff the link set) applied to compilation instead of linking.
#
# ⚠ THE DENOMINATOR IS THE POINT. A gnulib lib/ carries every platform's lane
# (Windows, MSVC, ports without pipe/fsync), so a raw `for f in lib/*.c` count
# is meaningless -- gzip-1.13 ships 118 .c of which automake builds 37 here.
# This scores against lib/Makefile's *_a_OBJECTS and reports the rest apart.
#
# Point PKGSRC at a CONFIGURED tree (./configure already run, so config.h and
# lib/Makefile exist). Without one the check SKIPS. To make one:
#   curl -O https://ftp.gnu.org/gnu/gzip/gzip-1.13.tar.gz
#   tar xzf gzip-1.13.tar.gz && (cd gzip-1.13 && ./configure)
#   mk/tools/moon-sweep.sh PKGSRC=$PWD/gzip-1.13
#
# gcc is used ONLY for the one-time ./configure probe that emits config.h (the
# accepted moon-userland precedent); mooncc compiles every object here.
set -e

ho=out/host
mc=$ho/mooncc
inc=crew/moon/include
PKGSRC=${PKGSRC:-dl/gzip-1.13}
[ $# -gt 0 ] && for a in "$@"; do case $a in PKGSRC=*) PKGSRC=${a#PKGSRC=} ;; esac; done

if [ ! -f "$PKGSRC/lib/Makefile" ]; then
  echo "moon-sweep: no configured package at '$PKGSRC' -- skipped."
  echo "            set PKGSRC=<a ./configure'd tree with lib/> to run (see mk/tools/moon-sweep.sh)."
  exit 0
fi
[ -x "$mc" ] || { echo "moon-sweep: missing $mc -- run 'make $ho/mooncc'"; exit 1; }

# absolute: the compile runs from inside lib/, and a relative -I silently
# mis-resolves the moment we cd (the bisection trap in)
G=$(pwd); MC=$G/$mc; INC=$G/$inc
T=$(cd "$PKGSRC" && pwd)
d=$ho/moonsweep
rm -rf "$d"; mkdir -p "$d/logs"

# --- the build set: automake's *_a_OBJECTS, continuation lines and all. ------
# names arrive as `libgzip_a-cloexec.$(OBJEXT)` (or with a subdir); strip the
# directory, the `libfoo_a-` per-target prefix, and the object suffix.
awk '
  /^(am_)?lib[A-Za-z0-9_]*_a_OBJECTS[ \t]*=/ { inobj = 1 }
  inobj {
    line = $0
    n = split(line, w, /[ \t]+/)
    for (i = 1; i <= n; i++)
      if (w[i] ~ /\$\(OBJEXT\)$/) {
        o = w[i]; sub(/\.\$\(OBJEXT\)$/, "", o)
        sub(/^.*\//, "", o); sub(/^lib[A-Za-z0-9_]*_a-/, "", o)
        print o
      }
    if (line !~ /\\$/) inobj = 0
  }
' "$T/lib/Makefile" | sort -u > "$d/buildset.txt"

nbuild=$(wc -l < "$d/buildset.txt")
[ "$nbuild" -gt 0 ] || { echo "moon-sweep: found no *_a_OBJECTS in $T/lib/Makefile"; exit 1; }

# --- compile every TU (the out-of-build-set ones are informational) ----------
: > "$d/results.tsv"
for f in "$T"/lib/*.c; do
  b=$(basename "$f" .c)
  ( cd "$T/lib" && timeout 60 "$MC" -DHAVE_CONFIG_H -I"$INC" -I"$T/lib" -I"$T" \
      -c "$f" -o "$d/$b.o" ) > "$d/logs/$b.log" 2>&1 && st=OK || st=FAIL
  rm -f "$d/$b.o"
  grep -q "^$b\$" "$d/buildset.txt" && inset=BUILT || inset=other
  printf '%s\t%s\t%s\n' "$st" "$inset" "$b" >> "$d/results.tsv"
done

# a failure's CAUSE = its first log line, paths stripped (per-file detail).
cause() { head -1 "$d/logs/$1.log" 2>/dev/null | sed -e 's#/[^ ]*/##g'; }
# its KIND = the cause with the varying identifier folded out, so the census
# counts shapes of failure. The parenthetical is the finding: today most kinds
# name a symptom and not a cause -- that gap is what closes.
kind() { cause "$1" |
         sed -e 's/^;; cgfn refuses .*/;; cgfn refuses <fn>   (undeclared identifier, unnamed)/' \
             -e 's/^cc: cannot resolve #include.*/cc: cannot resolve #include <hdr>/' \
             -e 's/^cc: \([a-z]*\) error in .*/cc: \1 error   (no cause reported)/' \
             -e 's/^cc: #error directive.*/cc: #error directive   (text not echoed)/'; }

echo
echo "=== moon-sweep: $(basename "$T") ==="
echo "lib/*.c present: $(ls "$T"/lib/*.c | wc -l)    automake builds here: $nbuild"
echo
ok=$(awk -F'\t' '$1=="OK"  && $2=="BUILT"' "$d/results.tsv" | wc -l)
no=$(awk -F'\t' '$1=="FAIL"&& $2=="BUILT"' "$d/results.tsv" | wc -l)
echo "--- the build set (what counts) ---"
echo "  compiles: $ok / $nbuild        fails: $no"
echo
if [ "$no" -gt 0 ]; then
  echo "--- failures, with first cause ---"
  awk -F'\t' '$1=="FAIL" && $2=="BUILT"{print $3}' "$d/results.tsv" |
    while read -r b; do printf '  %-20s %s\n' "$b" "$(cause "$b")"; done
  echo
  echo "--- buckets (by kind of failure) ---"
  awk -F'\t' '$1=="FAIL" && $2=="BUILT"{print $3}' "$d/results.tsv" |
    while read -r b; do kind "$b"; done | sort | uniq -c | sort -rn |
    sed 's/^/  /'
  echo
fi
oo=$(awk -F'\t' '$1=="FAIL" && $2=="other"' "$d/results.tsv" | wc -l)
echo "--- outside the build set (informational: other platforms' lanes) ---"
echo "  $oo of $(awk -F'\t' '$2=="other"' "$d/results.tsv" | wc -l) fail; a gnulib #error here"
echo "  usually means \"this platform lacks X\", i.e. configuration, not a mooncc gap."
echo
echo "logs: $d/logs/    scored: $d/results.tsv"
