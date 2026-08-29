#!/bin/sh
# gvnrun.sh -- the GVN + LICM oracle over the corpus TUs (gvn.tpl.l per file),
# then the totals: rows by kind and category, static count and loop weight.
# needs a CURRENT bake. usage: sh gvnrun.sh [outdir] [file ..]
S=$1
[ -n "$S" ] || S=out/ssagap
[ $# -gt 0 ] && shift
mkdir -p "$S"
: > "$S/gvn.txt"
if [ $# -gt 0 ]; then files="$*"; else
  files="src/*.c crew/moon/lib/math/am.c crew/moon/lib/nolibc/string/*.c \
         crew/moon/lib/nolibc/stdio/*.c crew/moon/lib/nolibc/fmt/*.c \
         crew/moon/lib/nolibc/os.c crew/moon/lib/nolibc/env/*.c crew/moon/lib/nolibc/proc/*.c"; fi
for f in $files; do
  sed "s|@FILE@|$f|" doc/misc/proto/ssagap/gvn.tpl.l > "$S/g1.l"
  out/host/love "$S/g1.l" >> "$S/gvn.txt" 2>&1 || echo "!! $f"
done
grep -E '^\((G|L) ' "$S/gvn.txt" > "$S/gvnrows.txt"
awk '{k=substr($1,2) " " $4; n[k]++; w[k]+=$6} END{for (k in n) printf "%-12s static %6d  loop-wt %8d\n", k, n[k], w[k]}' "$S/gvnrows.txt" | sort
echo "fns: $(grep -c '^==' "$S/gvn.txt") TUs, $(wc -l < "$S/gvnrows.txt") rows"
