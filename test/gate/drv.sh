#!/bin/sh
# test/gate/drv.sh -- the cc-DRIVER conventions gate: `CC=mooncc` must drive a
# gcc-shaped recipe unchanged. Three laws, each cheap:
#   1. the advisory flag soup (the REAL $(ai_cflags), passed in by make) rides
#      through -c and the link ignored;
#   2. a link owing libc symbols pulls the runtime BY NEED -- nolibc + the am
#      math + the sys leaf, compiled from the sources beside us -- and the
#      binary RUNS;
#   3. the loud edges stay loud: -shared refuses (usage, exit 2) and -nostdlib
#      leaves the libc out (link-undef, exit 1) -- an ignored SEMANTIC flag
#      would be the silent-no-op trap wearing a cc face, and this gate keeps
#      that door shut.
#
# usage: drv.sh OUTDIR CFLAGS..
set -u
ho=$1; shift
d=$ho/drv
mkdir -p "$d"
fail() { echo "FAIL test_drv: $*" >&2; exit 1; }

cat > "$d/a.c" <<'EOF'
#include <stdio.h>
#include <string.h>
#include <math.h>
int side(void);
int main(void) { printf("%d\n", (int)sqrt(16.0) + (int)strlen("abc") + side()); return 0; }
EOF
cat > "$d/b.c" <<'EOF'
int side(void) { return 35; }
EOF

# 1+2: the flag soup through -c and the link; the runtime pull binds printf/
# strlen/sqrt from nothing but the tree's own sources.
"$ho/mooncc" "$@" -c "$d/a.c" -o "$d/a.o" || fail "-c under the cc flag soup"
"$ho/mooncc" "$@" -c "$d/b.c" -o "$d/b.o" || fail "-c b.c"
"$ho/mooncc" "$@" -o "$d/drv" "$d/a.o" "$d/b.o" || fail "link + runtime pull"
out=$("$d/drv") || fail "the pulled binary did not run"
[ "$out" = 42 ] || fail "answered '$out', wanted 42"

# 3a: -shared refuses loudly
"$ho/mooncc" -shared "$d/b.o" -o "$d/x.so" 2>/dev/null && fail "-shared did not refuse"
[ $? -eq 2 ] || fail "-shared refused with the wrong exit"

# 3b: -nostdlib turns the driver's libc off -- the owed symbols stay link-undef
"$ho/mooncc" -nostdlib "$d/a.o" "$d/b.o" -o "$d/no" 2>/dev/null && fail "-nostdlib still linked"

echo "test_drv: CC=mooncc -- the cc flag soup rides through, the runtime pulls by need, -shared/-nostdlib stay loud"
