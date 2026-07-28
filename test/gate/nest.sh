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

# C's only licensed deviation: the -s targets land unstripped (bin/ai is the same
# binary seen through its symlink -- diff -r follows links). everything else must
# match B byte for byte, and the -s pair must equal the build artifacts.
diff -r "$T/B" "$T/C" > "$T/bc" 2>&1
grep -v -e 'bin/love differ' -e 'bin/ai differ' -e 'liblove\.so differ' "$T/bc" | grep -q . \
  && { cat "$T/bc"; fail "B vs C: differ beyond the -s targets"; }
cmp -s "$ho/love" "$T/C/.love/bin/love"           || fail "C: bin/love is not the unstripped binary"
cmp -s "$ho/liblove.so" "$T/C/.love/lib/liblove.so" || fail "C: liblove.so is not the unstripped library"
links "$T/B" "$T/B" > "$T/lb2"; links "$T/C" "$T/C" > "$T/lc"
cmp -s "$T/lb2" "$T/lc"                           || fail "B vs C: symlink targets differ"
shape "$T/C" > "$T/sc"
cmp -s "$T/sb" "$T/sc"                            || fail "B vs C: entries/types/modes differ"

rm -rf "$T"
echo "nest: make == cook, and the kore lane installs it (install/sed/ln/cat/chmod/mkdir/tr) ok"
