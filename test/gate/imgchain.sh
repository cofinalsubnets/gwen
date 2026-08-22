#!/bin/sh
# test/gate/imgchain.sh -- the pinned prefix under a collector that collects at every
# opportunity (doc/misc/plan/image-chain.md).
#
# `love bake -L` is the only thing that sets g->froze, so nothing in the ordinary corpus
# can reach the pin: the branch in gcp, the verbatim block in gen_major, the terminator
# fixup in evac_thread and the forged finalizer nodes are all dead code outside a layered
# bake. This drives a three-layer bake under AiGcStress, where every allocation collects
# and a major rides every 32nd, so the prefix is pinned across hundreds of collections
# before it is dumped.
#
# what it asserts, in the order a failure would appear:
#   * the bake completes
#   * each derived record stays SMALL -- a pin that stopped holding offsets shows up as a
#     patch per word, not as a crash
#   * each entry wakes carrying exactly its own layer and nothing above it
#
# usage: imgchain.sh LOVE     (the binary is copied; a bake patches its own file)
set -u

love=$1
name=test_imgchain
d=out/host/.imgchain
cap=65536                        # a derived record past this means the prefix was not shared

fail() { echo "FAIL $name: $*" >&2; exit 1; }

rm -rf "$d"; mkdir -p "$d"
labs=$(CDPATH= cd -- "$(dirname -- "$love")" && pwd)/$(basename -- "$love")

# three layers by inclusion. each adds a name and some churn, so the layer after it runs
# a few hundred collections over the frozen prefix before the next dump.
cat > "$d/a.l" <<'L'
(: ca (\ x (x + 1)))
(: pa (map (\ i (list i (show i))) ^200))
L
cat > "$d/b.l" <<'L'
(: cb (\ x (x * 2)))
(: pb (map (\ i (list i (show i) (i * i))) ^300))
L
cat > "$d/c.l" <<'L'
(: cc (\ x (x - 3)))
(: pc (map (\ i (list i (show i))) ^200))
L

# ..and bake it twice, because only argv[1] picks an entry: each run lets a different
# derived layer claim `-e`, which is the one verb reaching a session we can ask questions of.
bake() {                          # bake WHICH -> $d/love with layer WHICH claiming -e
  cp "$labs" "$d/love" || fail "cannot copy $love"
  case $1 in
  0) "$d/love" bake -L "$d/a.l:-e" -L "$d/b.l" -L "$d/c.l" > "$d/bake$1.out" 2>&1 ;;
  1) "$d/love" bake -L "$d/a.l" -L "$d/b.l:-e" -L "$d/c.l" > "$d/bake$1.out" 2>&1 ;;
  esac
  [ $? -eq 0 ] || fail "the layered bake failed: $(tail -2 "$d/bake$1.out")"
  # every derived record is reported; none of them may be anywhere near a whole image
  sed -n 's/.*-> \([0-9]*\) B derived.*/\1/p' "$d/bake$1.out" | while read -r n; do
    [ "$n" -lt $cap ] || { echo "FAIL $name: a derived record is $n bytes (>= $cap) -- the prefix was not shared" >&2; exit 1; }
  done || exit 1
  grep -q 'B derived' "$d/bake$1.out" || fail "the bake reported no derived layer at all"
}

ask() { env -u LOVE_NO_IMAGE "$d/love" -e "$1" 2>&1 | tail -1; }

# layer a alone: ca, and neither of the two above it
bake 0
got=$(ask '(list (ca 1) (tally pa) (member? (quote cb) (names ())) (member? (quote cc) (names ())))')
[ "$got" = "(2 200 0 0)" ] || fail "the first derived entry woke as \`$got' (wanted (2 200 0 0))"

# layer b: ca and cb, not cc
bake 1
got=$(ask '(list (ca 1) (cb 3) (tally pb) (member? (quote cc) (names ())))')
[ "$got" = "(2 6 300 0)" ] || fail "the second derived entry woke as \`$got' (wanted (2 6 300 0))"

# ..and the whole entry, which is the default. it is reached through a FILE rather than
# -e: argv[1] is what picks, and a path claims no verb, so the fallback (the largest
# entry) is what wakes.
cat > "$d/q.l" <<'L'
(: _ (puts (show (list (ca 1) (cb 3) (cc 10) (tally pc)))) _ (putc 10) 0)
L
got=$(env -u LOVE_NO_IMAGE "$d/love" "$d/q.l" 2>&1 | tail -1)
[ "$got" = "(2 6 7 200)" ] || fail "the whole entry woke as \`$got' (wanted (2 6 7 200))"

echo "$name: a three-layer bake under AiGcStress -- the pinned prefix survives, and each entry wakes carrying its own layer and nothing above it"
