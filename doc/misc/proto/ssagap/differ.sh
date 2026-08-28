#!/bin/sh
# differ.sh -- the rung-0 differential: crew/moon/val.l against the python
# oracle over the whole corpus, row for row (C census, D dead, H chains).
# needs a CURRENT bake (val.l rides the image). usage: sh differ.sh [outdir]
set -e
S=$1
[ -n "$S" ] || S=out/ssagap
mkdir -p "$S"
: > "$S/ir.txt"
for f in src/*.c crew/moon/lib/math/am.c crew/moon/lib/nolibc/string/*.c \
         crew/moon/lib/nolibc/stdio/*.c crew/moon/lib/nolibc/fmt/*.c \
         crew/moon/lib/nolibc/os.c crew/moon/lib/nolibc/env/*.c crew/moon/lib/nolibc/proc/*.c; do
  sed "s|@FILE@|$f|" doc/misc/proto/ssagap/valdiff.tpl.l > "$S/v1.l"
  out/host/love "$S/v1.l" >> "$S/ir.txt" || echo "!! $f"
done
grep -E '^\((C|D|H) ' "$S/ir.txt" > "$S/rows.love.txt"
python3 doc/misc/proto/ssagap/ssagap.py "$S/ir.txt" --rows > "$S/rows.py.txt"
diff -u "$S/rows.py.txt" "$S/rows.love.txt" && echo "DIFFERENTIAL OK: $(wc -l < "$S/rows.love.txt") rows"
