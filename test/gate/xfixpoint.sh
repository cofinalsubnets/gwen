#!/bin/sh
# test/gate/xfixpoint.sh -- the CROSS-MACHINE fixpoint, in effigy (doc/plan/
# seed-universal.md U0). dist_cross's twin objects -- this machine's mooncc,
# -t the other arch -- link love1: machine A's bytes for machine B. Then love1
# itself, under qemu-user standing in for machine B, bakes its own mooncc
# image, rebuilds every TU natively (mooncc's default target is the ground it
# stands on -- no -t below, an arm64 machine would pass none), links love2,
# and love1 must equal love2 TO THE BYTE. One cmp says two things at once:
# the twin machine reproduces this machine's bytes (what "same fixpoint
# everywhere" asks for), and mooncc's output does not depend on the arch
# mooncc runs on.
#
# ⚠ the TU flags MIRROR crew/build.mk's x-lane (AI_HAVE_VERSION_H on love.o,
# no $(moon_fir) -- the cross lane never carried a record), the fixpoint.sh
# drift trap wearing its cross face. this gate's first run caught the version
# flag MISSING from the x-lane: the twin named itself "unknown".
#
# usage: xfixpoint.sh OUTDIR LOVE0 QEMU XTGT MKSYS TCO XD OBJ...
set -u

ho=$1
love0=$2
qemu=$3
xtgt=$4
mks=$5
tco=$6
xd=$7
shift 7
d=$xd/fix
cat=$ho/.mooncc-cat.l

command -v "$qemu" >/dev/null 2>&1 || { echo "test_xfixpoint: skipped (needs $qemu)"; exit 0; }

fail() { echo "FAIL test_xfixpoint: $*" >&2; exit 1; }

mkdir -p "$d"
rm -f "$d"/*.o "$d"/love1 "$d"/love2 "$d"/mooncc1.image

# love1: machine A's link of the twin -- exactly the dist link, before the bake
# mutates it (the object list arrives FROM make, $(xobjs), never globbed).
moon0() { "$love0" wake "$ho/mooncc0.image" mooncc "$@"; }
moon0 -t "$xtgt" -pie "$@" -o "$d/love1" || fail "love1 cross link"

echo "XFIX $d/love1 rebuilds itself under $qemu"

# machine B: love1 bakes its own compiler image...
LOVE_NO_IMAGE=1 "$qemu" "$d/love1" -l "$cat" -e "(? ((bake \"$d/mooncc1.image\") = 1) (quit 0) (quit 1))" \
  || fail "love1 bakes mooncc1.image under $qemu"

# ...and rebuilds every TU with it, natively, in the order make links them
moon1() { "$qemu" "$d/love1" wake "$d/mooncc1.image" mooncc "$@"; }
moon1 -D ai_tco="$tco" -D AI_HAVE_VERSION_H -I"$ho" -I. -Icore -Iout/lib -c core/love.c "$d/love.o" || fail "love1 mooncc -c core/love.c"
for f in host/*.c; do
  b=$(basename "$f" .c)
  moon1 -D ai_tco="$tco" -I"$ho" -I. -Icore -Iout/lib -c "$f" "$d/host_$b.o" || fail "love1 mooncc -c $f"
done
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  moon1 -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || fail "love1 mooncc -c $f"
done
LOVE_NO_IMAGE=1 "$qemu" "$d/love1" -l "$ho/.mksys-cat.l" -n -e "($mks \"$d/sys.o\")" >/dev/null || fail "love1 mksys"
test -s "$d/sys.o" || fail "love1 mksys laid an empty sys.o"

o2=; for o in "$@"; do o2="$o2 $d/${o##*/}"; done
moon1 -pie $o2 -o "$d/love2" || fail "love2 link"

cmp "$d/love1" "$d/love2" || fail "love2 differs from love1 -- machine B does not reproduce machine A's bytes"

echo "test_xfixpoint: bee's mooncc -t $xtgt -> love1; love1 under $qemu -> love2; byte-identical -- the twin machine reproduces this machine's bytes"
