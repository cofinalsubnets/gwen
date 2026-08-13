#!/bin/sh
# test/gate/targz.sh -- lib/tar.l + lib/gz.l against the two programs they replace.
#
# test/host/gz.l proves the laws that need nothing outside the tree: crc32 against
# its published vector, both coders against each other, the ustar header field by
# field. THIS gate proves the half that only the outside world can say -- that GNU
# tar and GNU gzip AGREE with us, in both directions, over a real tree.
#
# ⚠ AGREEING WITH OURSELVES PROVES NOTHING HERE. A coder and a decoder written by
# one hand share a model, and a round trip through both is green for any pair of
# functions that invert each other -- including a pair that agree on a format
# nobody else speaks. The system tools are the only oracle that can catch that,
# which is why this gate exists separately rather than as more asserts.
#
# The tree is chosen for what it puts in the header rather than for size: a
# SYMLINK (typeflag 2, and a target in the linkname field), a file with a MODE
# that is not the default (0600), an EMPTY file, a file whose size is an exact
# multiple of 512 (so the body padding is zero bytes -- the off-by-one lives
# there), a deep path, and incompressible bytes beside compressible ones.
#
# Skips cleanly where either tool is missing, and takes the love binary as $1.
set -e

love=${1:-out/host/love}
[ -x "$love" ] || { echo "targz: no $love -- run 'make host'"; exit 1; }
command -v tar  >/dev/null 2>&1 || { echo "targz: no system tar, skipped";  exit 0; }
command -v gzip >/dev/null 2>&1 || { echo "targz: no system gzip, skipped"; exit 0; }

w=$(mktemp -d)
trap 'rm -rf "$w"' EXIT
r=$(pwd)

mkdir -p "$w/tree/sub/deep"
printf 'alpha\n'                        > "$w/tree/a.txt"
: >                                       "$w/tree/empty"
printf 'beta beta beta beta beta beta\n' > "$w/tree/sub/b.txt"
head -c 512  /dev/urandom               > "$w/tree/sub/exact512.bin"
head -c 4096 /dev/urandom               > "$w/tree/sub/deep/blob.bin"
cat lib/gz.l lib/tar.l                  > "$w/tree/text.l"
ln -s a.txt "$w/tree/link"
chmod 0600 "$w/tree/sub/b.txt"

fail() { echo "FAIL targz: $*"; exit 1; }

# ---- 1. we WRITE, they READ ------------------------------------------------
cat > "$w/pack.l" <<EOF
(use 'tar)
(use 'gz)
(: g (tar-gather "$w/tree" "")
   _ (? (! g) (: _ (say err "gather failed\n") (quit 1)) 0)
   a (tar-pack (<(>g)))
   _ (? (! a) (: _ (say err "pack failed\n") (quit 1)) 0)
   z (gz-zip a "" 0)
   q (open "$w/ours.tar.gz" "w") _ (say q z) _ (close q)
   p (open "$w/ours.tar" "w") _ (say p a) _ (close p)
   0)
EOF
"$love" "$w/pack.l" || fail "love could not write the archive"
[ -s "$w/ours.tar.gz" ] || fail "love wrote an empty .tar.gz"

gzip -t "$w/ours.tar.gz" || fail "system gzip rejects our .gz container"
tar tzf "$w/ours.tar.gz" > /dev/null || fail "system tar cannot list our .tar.gz"
mkdir -p "$w/theirs"
( cd "$w/theirs" && tar xzf "$w/ours.tar.gz" ) || fail "system tar cannot extract our .tar.gz"
diff -r "$w/tree" "$w/theirs" || fail "system tar's extraction of our archive differs"
[ -L "$w/theirs/link" ] || fail "the symlink came out as a regular file"
[ "$(readlink "$w/theirs/link")" = a.txt ] || fail "the symlink target is wrong"
m=$(stat -c %a "$w/theirs/sub/b.txt")
[ "$m" = 600 ] || fail "mode not preserved through our writer (got $m, want 600)"
echo "  OK we write, GNU tar + gzip read -- tree identical, symlink and mode intact"

# ---- 2. they WRITE, we READ ------------------------------------------------
( cd "$w/tree" && tar czf "$w/theirs.tar.gz" . )
mkdir -p "$w/ours"
cat > "$w/unpack.l" <<EOF
(use 'tar)
(use 'gz)
(: q (open "$w/theirs.tar.gz" "r") z (: s (slurp q) _ (close q) (s + ""))
   u (gz-unzip z)
   _ (? (! u) (: _ (say err "gunzip failed\n") (quit 1)) 0)
   r (tar-unpack (<(>u)))
   _ (? (! r) (: _ (say err "untar failed\n") (quit 1)) 0)
   w (tar-scatter "$w/ours" (<(>r)))
   _ (? (! w) (: _ (say err "scatter failed\n") (quit 1)) 0)
   0)
EOF
"$love" "$w/unpack.l" || fail "love could not read the system's .tar.gz"
diff -r "$w/tree" "$w/ours" || fail "our extraction of the system archive differs"
echo "  OK GNU tar + gzip write, we read -- tree identical"

# ---- 3. the gzip container alone, both ways, over shapes that break coders --
for f in tree/text.l tree/sub/deep/blob.bin tree/empty; do
  src="$w/$f"
  cat > "$w/one.l" <<EOF
(use 'gz)
(: q (open "$src" "r") s (: t (slurp q) _ (close q) (t + ""))
   z (gz-zip s "" 0)
   o (open "$w/one.gz" "w") _ (say o z) _ (close o)
   0)
EOF
  "$love" "$w/one.l" || fail "love could not gzip $f"
  gzip -dc "$w/one.gz" | cmp - "$src" || fail "system gunzip disagrees on $f"
  # ..and the reverse, at -9, which is where gzip writes a DYNAMIC code -- the
  # branch our own coder never emits and so never exercises from this side.
  gzip -9 -c "$src" > "$w/theirs.gz"
  cat > "$w/one2.l" <<EOF
(use 'gz)
(: q (open "$w/theirs.gz" "r") z (: t (slurp q) _ (close q) (t + ""))
   u (gz-unzip z)
   _ (? (! u) (: _ (say err "unzip failed\n") (quit 1)) 0)
   o (open "$w/back" "w") _ (say o (<(>u))) _ (close o)
   0)
EOF
  "$love" "$w/one2.l" || fail "love could not gunzip gzip -9's output for $f"
  cmp "$w/back" "$src" || fail "we disagree with gzip -9 on $f"
done
echo "  OK gzip container both ways (text, incompressible, empty; -9 dynamic codes read)"

echo "targz: lib/tar.l + lib/gz.l agree with GNU tar and GNU gzip both ways -- ok"
