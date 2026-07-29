#!/bin/sh
# test/gate/nest.sh -- the install nest, three ways, one shape.
#
#   A: make install   -- GNU make + the host's install/sed/ln (the reference)
#   B: cook install   -- cook imports mk/install.mk itself (env import, $(call),
#                        $(origin), @D) and runs the same recipes through sh
#   C: cook install with PATH FRONTED by kore symlinks -- install/sed/ln/cat/
#      chmod/mkdir/tr resolve to the kore applets (argv0 dispatch), so the
#      inner steps run on our own tools: the self-hosted lane
#
# A == B in every dimension: entries, types, modes, symlink targets, contents.
# C == B except the two `install -s` targets (kore notes -s and copies
# unstripped), which must instead equal the UNSTRIPPED build artifacts exactly.
#
# every install here aims at a scratch DESTDIR, so the gate never touches ~.
# usage: nest.sh OUTDIR LOVE
set -u
ho=$1; love=$2
T=$ho/.nest
fail() { echo "FAIL nest: $1"; exit 1; }
rm -rf "$T"; mkdir -p "$T"

# the nest walk: entries + types + modes; symlink targets with the scratch root
# folded to NEST so A's and B's link bodies compare equal.
shape() { (cd "$1" && find . -exec stat -c '%n %F %a' {} \; | LC_ALL=C sort); }
links() { (cd "$1" && find . -type l | LC_ALL=C sort | while read -r l; do
             printf '%s -> %s\n' "$l" "$(readlink "$l")"; done | sed "s|$2|NEST|g"); }

make -s install DESTDIR="$T/A/" > /dev/null       || fail "make install"
"$love" -l crew/cook/cook.l -f Makefile install DESTDIR="$T/B/" > /dev/null \
                                                  || fail "cook install"

# the nest ANSWERS: run what was laid, on both boot paths (image wake + egg).
# a stripped mooncc/holo ELF once segfaulted here while the A/B trees compared
# EQUAL -- both lanes stripped it the same way. never only diff what you can run.
env -u LOVE_NO_IMAGE "$T/A/.love/bin/love" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' \
                                                  || fail "the installed love does not answer (wake)"
LOVE_NO_IMAGE=1 "$T/A/.love/bin/love" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' \
                                                  || fail "the installed love does not answer (egg)"

# and it SURVIVES binutils strip: strip rebuilds a file FROM its section
# headers, so holo must cover every loaded byte with one -- ai_rela (the -pie
# reloc table) was once uncovered, came back zeroed, and the stripped binary
# segfaulted at the first unrebased pointer. installs stay unstripped (the
# symtab is deliberate: nm/gdb); this keeps a user's own strip safe.
if command -v strip > /dev/null 2>&1; then
  cp "$T/A/.love/bin/love" "$T/love-stripped"
  strip "$T/love-stripped"
  env -u LOVE_NO_IMAGE "$T/love-stripped" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' \
                                                  || fail "the STRIPPED love does not answer (wake)"
  LOVE_NO_IMAGE=1 "$T/love-stripped" -e '(? (2 = (1 + 1)) (quit 0) (quit 1))' \
                                                  || fail "the STRIPPED love does not answer (egg)"
fi

shape "$T/A" > "$T/sa"; shape "$T/B" > "$T/sb"
cmp -s "$T/sa" "$T/sb"                            || fail "A vs B: entries/types/modes differ"
links "$T/A" "$T/A" > "$T/la"; links "$T/B" "$T/B" > "$T/lb"
cmp -s "$T/la" "$T/lb"                            || fail "A vs B: symlink targets differ"
diff -r "$T/A" "$T/B" > /dev/null                 || fail "A vs B: contents differ"

# lane C: every applet name a symlink onto the kore shim, PATH fronted. tr rides
# too ($(BINUP)'s $(shell ... tr [:lower:] [:upper:]) runs under this PATH).
K=$T/korebin; mkdir -p "$K"
for t in install sed ln cat chmod mkdir tr; do ln -sf "$(pwd)/$ho/kore" "$K/$t"; done
PATH="$(pwd)/$K:$PATH" "$love" -l crew/cook/cook.l -f Makefile install DESTDIR="$T/C/" \
  > /dev/null 2> "$T/cerr"                        || { cat "$T/cerr"; fail "cook install (kore lane)"; }

# C's only licensed deviation: kore's install -s lands liblove.so unstripped
# (bin/love installs unstripped in EVERY lane -- the symtab is deliberate, see
# mk/install.mk). everything else must match B byte for byte, and both loves
# must equal the build artifact.
diff -r "$T/B" "$T/C" > "$T/bc" 2>&1
grep -v -e 'liblove\.so differ' "$T/bc" | grep -q . \
  && { cat "$T/bc"; fail "B vs C: differ beyond liblove.so"; }
cmp -s "$ho/love" "$T/C/.love/bin/love"           || fail "C: bin/love is not the unstripped binary"
cmp -s "$ho/liblove.so" "$T/C/.love/lib/liblove.so" || fail "C: liblove.so is not the unstripped library"
links "$T/B" "$T/B" > "$T/lb2"; links "$T/C" "$T/C" > "$T/lc"
cmp -s "$T/lb2" "$T/lc"                           || fail "B vs C: symlink targets differ"
shape "$T/C" > "$T/sc"
cmp -s "$T/sb" "$T/sc"                            || fail "B vs C: entries/types/modes differ"

# THE INSTALLED TOOLCHAIN, from a foreign cwd -- the whole point of shipping
# lib/love/moon/. every path in the mooncc driver used to be cwd-relative, so
# outside a source tree `<stdio.h>` fell through to /usr/include (glibc's, whose
# stdio.h wants the compiler's own stddef.h -> "cannot resolve") and the link
# found no libc at all. run it from a scratch directory: our headers must serve
# the preprocessor and the implicit runtime must bind printf/strlen/sqrt.
# `cd` matters more than it looks -- from the repo root the DEV rung would serve
# and the seat rung would never be exercised.
A=$(cd "$T/A" && pwd)
mkdir -p "$T/away"
cat > "$T/away/h.c" <<'EOF'
#include <stdio.h>
#include <string.h>
#include <math.h>
int main(void) { printf("%d\n", (int) sqrt(16.0) + (int) strlen("abc")); return 0; }
EOF
# two steps, because they test the two rungs separately: -c resolves the
# HEADERS through the seat, the link pulls the RUNTIME through it.
# (the one-file `mooncc h.c -o h` lane is the tiny standalone emit -- no
# linker, so no libc, in a source tree just the same; see doc/moon.md.)
( cd "$T/away" && "$A/.love/bin/mooncc" -c h.c -o h.o ) || fail "installed mooncc: -c from a foreign cwd (our headers)"
( cd "$T/away" && "$A/.love/bin/mooncc" h.o -o h )      || fail "installed mooncc: link from a foreign cwd (the runtime pull)"
out=$( cd "$T/away" && ./h )                       || fail "installed mooncc: the binary does not run"
[ "$out" = 7 ]                                     || fail "installed mooncc: answered '$out', wanted 7"

rm -rf "$T"
echo "nest: make == cook, the kore lane installs it (install/sed/ln/cat/chmod/mkdir/tr), and the installed mooncc compiles from any cwd ok"
