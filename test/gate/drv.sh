#!/bin/sh
# test/gate/drv.sh -- the cc-DRIVER conventions gate: `CC=mooncc` must drive a
# gcc-shaped recipe unchanged. Three laws, each cheap:
#   1. the advisory flag soup (the REAL $(ai_cflags), passed in by make) rides
#      through -c and the link ignored;
#   2. a link owing libc symbols pulls the runtime BY NEED -- nolibc + the am
#      math + the sys leaf, compiled from the sources beside us -- and the
#      binary RUNS;
#   3. the loud edges stay loud: -shared refuses (usage, exit 2) and -nostdlib
#      leaves the libc out (exit 1) -- an ignored SEMANTIC flag would be the
#      silent-no-op trap wearing a cc face, and this gate keeps that door shut.
#      And -nostdlib's refusal is pinned by its SENTENCE, not just its exit
#      code: it used to arrive as `;; link-undef "printf"`, a love debug note
#      escaping from inside holo, and an exit-code-only check is exactly what
#      let that stand for as long as it did.
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

# 2b: -lm -ldl -L ride through. the driver pulls its runtime BY NEED, so the libc
# family a recipe asks for is already in the artifact before it asks -- and lua's own
# Makefile writes LIBS=-lm, which is the whole reason this matters. the object must be
# UNCHANGED by them: a tolerated flag that moved a byte would not be tolerated at all.
"$ho/mooncc" "$@" -c "$d/b.c" -o "$d/b2.o" -lm -ldl -L/usr/lib || fail "-l/-L did not ride through"
cmp -s "$d/b.o" "$d/b2.o" || fail "a tolerated -l/-L changed the object"
"$ho/mooncc" "$@" -o "$d/drv2" "$d/a.o" "$d/b.o" -lm || fail "link with -lm"
[ "$("$d/drv2")" = 42 ] || fail "the -lm-linked binary did not answer 42"
# ..but a BARE -l is still a refusal: taking it would eat the next word as a library
# name and the word after it as an input, which is the silent no-op wearing a cc face.
"$ho/mooncc" -c "$d/b.c" -o "$d/b3.o" -l 2>/dev/null && fail "a bare -l did not refuse"

# 3a: -shared refuses loudly
"$ho/mooncc" -shared "$d/b.o" -o "$d/x.so" 2>/dev/null && fail "-shared did not refuse"
[ $? -eq 2 ] || fail "-shared refused with the wrong exit"

# 3b: -nostdlib turns the driver's libc off, and the owed symbols are an
# UNDEFINED REFERENCE -- named, on stderr, with a cc: prefix like every other
# diagnostic. printf is owed by a.c and nothing supplies it under -nostdlib.
msg=$("$ho/mooncc" -nostdlib "$d/a.o" "$d/b.o" -o "$d/no" 2>&1) && fail "-nostdlib still linked"
case $msg in
  "cc: undefined reference to "*"'printf'"*) : ;;
  *) fail "-nostdlib refused, but said: $msg" ;;
esac

# 4: THE CONFIGURE PROBE -- how a build asks a cc anything before trusting it: a source on
# stdin, its language named by -x because there is no suffix left to read. every
# autoconf-shaped build writes some version of this line before it believes a flag, so a cc
# that cannot be ASKED is one that gets answered by the fallback instead.
printf 'int main(void){return 0;}' | "$ho/mooncc" -std=gnu23 -x c -c -o /dev/null - \
  || fail "the configure probe (-x c with the source on stdin) did not compile"
printf 'int main(void){return 0;}' | "$ho/mooncc" -xc -c -o "$d/in.o" - \
  || fail "glued -xc did not compile"
[ -s "$d/in.o" ] || fail "a source on stdin wrote no object"
# ..and -x stays LOUD on a language we are not: taking c++ for C is -std='s own hazard.
"$ho/mooncc" -x c++ -c "$d/b.c" -o "$d/x.o" 2>/dev/null && fail "-x c++ did not refuse"

echo "test_drv: CC=mooncc -- the cc flag soup rides through, the runtime pulls by need, the configure probe answers, -shared/-nostdlib stay loud"
