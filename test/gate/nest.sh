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
# (bin/love installs unstripped in EVERY lane now -- binutils strip breaks the
# mooncc/holo ELF, see mk/install.mk). everything else must match B byte for
# byte, and both loves must equal the build artifact.
diff -r "$T/B" "$T/C" > "$T/bc" 2>&1
grep -v -e 'liblove\.so differ' "$T/bc" | grep -q . \
  && { cat "$T/bc"; fail "B vs C: differ beyond liblove.so"; }
cmp -s "$ho/love" "$T/C/.love/bin/love"           || fail "C: bin/love is not the unstripped binary"
cmp -s "$ho/liblove.so" "$T/C/.love/lib/liblove.so" || fail "C: liblove.so is not the unstripped library"
links "$T/B" "$T/B" > "$T/lb2"; links "$T/C" "$T/C" > "$T/lc"
cmp -s "$T/lb2" "$T/lc"                           || fail "B vs C: symlink targets differ"
shape "$T/C" > "$T/sc"
cmp -s "$T/sb" "$T/sc"                            || fail "B vs C: entries/types/modes differ"

rm -rf "$T"
echo "nest: make == cook, and the kore lane installs it (install/sed/ln/cat/chmod/mkdir/tr) ok"
