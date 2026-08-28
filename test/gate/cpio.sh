#!/bin/sh
# test/gate/cpio.sh -- lib/cpio.l + crew/cpio/cpiocmd.l against the program they replace.
#
# The newc archive is what the kernel unpacks an initramfs from, and `make
# distro-initramfs` is the caller: `find | cpio -o -H newc | gzip -9`, all three of
# them ours now. So the oracle is GNU cpio, in BOTH directions, over a tree chosen
# for the format's edges rather than for size.
#
# ⚠ AGREEING WITH OURSELVES PROVES NOTHING. A packer and an unpacker written by one
# hand invert each other for any format, including one nobody else speaks -- the same
# argument test/gate/targz.sh makes, and the reason this gate is separate from the
# laws in test/host/.
#
# The tree's edges, and what each is for: newc pads the NAME to a 4-byte boundary
# measured from the header's start (110 + namesize), and the BODY to another, so the
# names run 1..4 charms past a multiple of four and the bodies 0..3 -- every pad
# length appears. A SYMLINK's target is its body and is NOT NUL-terminated. An EMPTY
# file and an empty DIRECTORY both carry filesize 0 while meaning different things.
#
# Skips cleanly where cpio is missing, and takes the love binary as $1.
set -e

love=${1:-out/host/love}
[ -x "$love" ] || { echo "cpio: no $love -- run 'make host'"; exit 1; }
command -v cpio >/dev/null 2>&1 || { echo "cpio: no system cpio, skipped"; exit 0; }
r=$(pwd)
L=$r/$love

w=$(mktemp -d)
trap 'rm -rf "$w"' EXIT
fail() { echo "FAIL cpio: $*"; exit 1; }

t=$w/tree
mkdir -p "$t/sub/deep"
printf 'a'      > "$t/n1"          # name and body both short
printf 'ab'     > "$t/nn2"
printf 'abc'    > "$t/nnn3"
printf 'abcd'   > "$t/nnnn4"       # body an exact multiple of 4: no padding at all
: >              "$t/empty"        # filesize 0, and it is a FILE
printf 'beta\n' > "$t/sub/b.txt"
head -c 3000 /dev/urandom > "$t/sub/deep/blob.bin"
ln -s ../b.txt "$t/sub/deep/link"  # the target is the body, no NUL
chmod 0600 "$t/sub/b.txt"
chmod 0755 "$t/nnnn4"

( cd "$t" && find . | LC_ALL=C sort ) > "$w/names"

# ---- 1. we WRITE, they READ ------------------------------------------------
( cd "$t" && "$L" cpio -o --quiet < "$w/names" ) > "$w/ours.cpio"
[ -s "$w/ours.cpio" ] || fail "we wrote an empty archive"
[ $(( $(stat -c %s "$w/ours.cpio") % 512 )) -eq 0 ] || fail "our archive is not padded to 512"
mkdir -p "$w/x1"
( cd "$w/x1" && cpio -id --quiet < "$w/ours.cpio" ) || fail "system cpio cannot extract ours"
diff -r "$t" "$w/x1" || fail "system cpio's extraction of our archive differs"
[ -L "$w/x1/sub/deep/link" ] || fail "the symlink came out as a regular file"
[ "$(readlink "$w/x1/sub/deep/link")" = ../b.txt ] || fail "the symlink target is wrong"
# ⚠ diff -r COMPARES BYTES, NOT MODES -- the blind spot that shipped a real bug in the
# tar extractor. The modes are their own list, both directions.
( cd "$t"    && find . -type f | LC_ALL=C sort | xargs stat -c '%a %n' ) > "$w/m.want"
( cd "$w/x1" && find . -type f | LC_ALL=C sort | xargs stat -c '%a %n' ) > "$w/m.got"
diff "$w/m.want" "$w/m.got" || fail "system cpio did not get our modes"
echo "  OK we write, GNU cpio reads -- tree identical, symlink and modes intact"

# ---- 2. they WRITE, we READ ------------------------------------------------
( cd "$t" && cpio --quiet -o -H newc < "$w/names" ) > "$w/gnu.cpio"
mkdir -p "$w/x2"
( cd "$w/x2" && "$L" cpio -i --quiet < "$w/gnu.cpio" ) || fail "we cannot extract GNU's"
diff -r "$t" "$w/x2" || fail "our extraction of the system archive differs"
( cd "$w/x2" && find . -type f | LC_ALL=C sort | xargs stat -c '%a %n' ) > "$w/m.got2"
diff "$w/m.want" "$w/m.got2" || fail "we did not preserve the modes"
[ -L "$w/x2/sub/deep/link" ] || fail "we wrote the symlink as a regular file"
echo "  OK GNU cpio writes, we read -- tree identical, modes preserved"

# ---- 3. the two listings, byte for byte, over both archives ------------------
# the names are the archive's own contents; a listing that agrees on both files is
# both halves of the wire agreeing about where every name starts and ends.
cpio -t --quiet < "$w/ours.cpio" > "$w/l1"
"$L" cpio -t --quiet < "$w/ours.cpio" > "$w/l2"
cpio -t --quiet < "$w/gnu.cpio" > "$w/l3"
"$L" cpio -t --quiet < "$w/gnu.cpio" > "$w/l4"
cmp -s "$w/l1" "$w/l2" || { diff "$w/l1" "$w/l2"; fail "-t on our archive"; }
cmp -s "$w/l3" "$w/l4" || { diff "$w/l3" "$w/l4"; fail "-t on GNU's archive"; }
cmp -s "$w/l1" "$w/l3" || { diff "$w/l1" "$w/l3"; fail "the two archives name different things"; }
# ⚠ the stored name loses a leading "./" -- GNU cpio drops it where GNU tar keeps it
grep -q '^\./' "$w/l1" && fail "a stored name kept its ./"
echo "  OK -t listings byte-identical, both archives, both tools"

# ---- 4. the flags ------------------------------------------------------------
run() { e=0; "$@" > /dev/null 2>&1 || e=$?; }
# -F / -I / -O name the archive instead of the streams
( cd "$t" && "$L" cpio -o --quiet -O "$w/byO.cpio" < "$w/names" ) || fail "cpio -O"
cmp -s "$w/ours.cpio" "$w/byO.cpio" || fail "-O wrote something else than stdout would"
"$L" cpio -t --quiet -I "$w/byO.cpio" > "$w/l5" || fail "cpio -I"
cmp -s "$w/l1" "$w/l5" || fail "-I read something else than stdin would"
"$L" cpio -t --quiet -F "$w/byO.cpio" > "$w/l6" || fail "cpio -F"
cmp -s "$w/l1" "$w/l6" || fail "-F read something else"
# -H takes newc and refuses the rest BY NAME (a format that lied would be worse)
( cd "$t" && "$L" cpio -o --quiet -H newc < "$w/names" ) > /dev/null || fail "-H newc"
run sh -c "cd '$t' && '$L' cpio -o -H odc < '$w/names'"
[ $e -ne 0 ] || fail "-H odc was not refused"
# the block count, and --quiet silencing it
"$L" cpio -t -I "$w/byO.cpio" 2> "$w/blocks" > /dev/null || fail "cpio -t block count"
grep -q '^[0-9][0-9]* blocks$' "$w/blocks" || fail "no 'N blocks' line on err"
"$L" cpio -t --quiet -I "$w/byO.cpio" 2> "$w/quiet" > /dev/null || fail "cpio -t --quiet"
[ ! -s "$w/quiet" ] || fail "--quiet still said something"
# -v names every member on err as it packs, and only on err (⚠ -t's own -v is GNU's
# ls -l LISTING, a layout of its own, and is not here -- so -v is asked of -o)
( cd "$t" && "$L" cpio -o -v --quiet < "$w/names" ) > /dev/null 2> "$w/verb" || fail "cpio -ov"
cmp -s "$w/names" "$w/verb" || fail "-v did not name every member on err"
# -u overwrites; without it a newer file on disk stays. ⚠ the archive's mtimes are the
# tree's own, so "newer" here is arranged with touch and not with luck
mkdir -p "$w/x3" && ( cd "$w/x3" && "$L" cpio -i --quiet < "$w/ours.cpio" )
printf 'MINE\n' > "$w/x3/n1"; touch -d '2030-01-01' "$w/x3/n1"
( cd "$w/x3" && "$L" cpio -i --quiet < "$w/ours.cpio" ) || fail "cpio -i over a tree"
grep -q MINE "$w/x3/n1" || fail "a newer file on disk was overwritten without -u"
( cd "$w/x3" && "$L" cpio -iu --quiet < "$w/ours.cpio" ) || fail "cpio -iu"
grep -q MINE "$w/x3/n1" && fail "-u did not overwrite"
# a torn archive is an error and not a half-written tree
head -c 200 "$w/ours.cpio" > "$w/torn.cpio"
run sh -c "cd '$w' && '$L' cpio -t -I '$w/torn.cpio'"
[ $e -eq 1 ] || fail "a torn archive did not fail (rc $e)"
echo "  OK the flags -- -F/-I/-O, -H's refusal, the block count, -v, -u, a torn read"

# ---- 5. the initramfs shape --------------------------------------------------
# what the kernel actually unpacks: find's order kept (a directory before what is
# under it), and the whole thing through our own gzip, read back by the system's.
( cd "$t" && "$L" cpio -o --quiet < "$w/names" ) | "$L" gzip > "$w/img.cpio.gz"
gzip -t "$w/img.cpio.gz" || fail "system gzip rejects the image"
mkdir -p "$w/x4"
( cd "$w/x4" && gunzip -c "$w/img.cpio.gz" | cpio -id --quiet ) || fail "the image will not unpack"
diff -r "$t" "$w/x4" || fail "the image's tree differs"
awk 'NR==1 && $0 != "." { exit 1 }' "$w/l1" || fail "the root entry is not first"
echo "  OK the initramfs shape -- find's order kept, our cpio + our gzip, GNU unpacks it"

echo "cpio: lib/cpio.l agrees with GNU cpio both ways over newc -- ok"
