#!/bin/sh
# test/gate/kore.sh -- kore, the multi-call toolbox (crew/kore/), against GNU coreutils
# as the oracle. The laws first, then ~350 checks whose shape is almost always the same
# one: run the system tool, run OUR applet the same way, and require byte-identical
# stdout -- and, where the exit code carries meaning (grep's 0/1/2, sed's 1/2, xargs'
# 123/127, expr's 0/1/2, patch's 0/1), that too. GNU is not assumed correct, only
# independent. Three checks cannot take that shape and say so where they sit: the
# walks whose order is the file system's business (find, du) compare SETS, split and
# patch compare the TREE they leave, and mktemp's answer is random, so its shape and
# its effect are what is asked.
#
# That one shape is `both`/`pipe` below; it is why 161 lines of recipe fit here without
# repeating `cmp -s` ninety times. Where a check is genuinely its own thing -- the fs
# mutations, the kill/wait pair -- it is written out.
#
# NOT set -e: the exit-code comparisons need $? to survive.
#
# usage: kore.sh OUTDIR LOVE
set -u

ho=$1
m=$2

fail() { echo "FAIL $*" >&2; exit 1; }
korerun() { LOVE_NO_IMAGE= "$m" kore "$@"; }
# the love and the out dir, spelled ABSOLUTELY: $m and $ho are relative (the root
# Makefile sets R := .), and the checks that cd somewhere -- split's output directory,
# patch's tree -- cannot use either. ⚠ a `< $ho/p.diff` INSIDE a cd'd subshell opens
# after the cd, so a relative one silently hands patch an empty stdin -- and both sides
# then do nothing and match, which reads exactly like a pass.
case $ho in /*) HO=$ho;; *) HO=$PWD/$ho;; esac
K=$PWD/$m

g=$ho/.kore-g
o=$ho/.kore-o
same() { cmp -s "$g" "$o" || fail "kore $1 vs GNU"; }
# the workhorse: the system tool, then ours, then compare stdout
both() { n=$1; shift; "$@" > "$g" 2>/dev/null; korerun "$@" > "$o" 2>/dev/null; same "$n"; }
# ..and the stdin face
pipe() { n=$1; i=$2; shift 2
         printf '%s' "$i" | "$@" > "$g" 2>/dev/null
         printf '%s' "$i" | korerun "$@" > "$o" 2>/dev/null
         same "$n"; }

# ------------------------------------------------------------------- the laws
echo "UTILS crew/kore/{text,core,fs,re,sed,awk,expr,find,diff,patch,law}.l"
out=$ho/.test_kore.out
# ⚠ lush's job.l + glob.l ride along because find.l captures sh-match at its define
{ cat test/00-init.l crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l \
      crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
      crew/vi/core.l crew/vi/vi.l crew/kore/diff.l crew/kore/patch.l crew/lush/job.l crew/lush/glob.l \
      crew/kore/find.l; \
  echo "(use 'kore)"; \
  cat crew/kore/law.l; } | "$m" > "$out" 2>&1
r=$?
cat "$out"
[ $r -eq 0 ] && grep -q "crew/kore/law: myers" "$out" || fail "utils (exit $r)"

# ------------------------------------------- diff, argv0 dispatch, usage, as
printf 'a\nb\nc\n' > "$ho/.au1"; printf 'a\nX\nc\n' > "$ho/.au2"
korerun diff "$ho/.au1" "$ho/.au1" > "$ho/.kore-same.out" 2>&1; r=$?
[ $r -eq 0 ] && [ ! -s "$ho/.kore-same.out" ] || fail "kore diff same (exit $r)"
korerun diff "$ho/.au1" "$ho/.au2" > "$ho/.kore-diff.out" 2>&1; r=$?
[ $r -eq 1 ] || fail "kore diff differ (exit $r)"
# the unified body must match GNU's; the ---/+++ header lines carry timestamps
diff -u "$ho/.au1" "$ho/.au2" | tail -n +3 > "$g"
tail -n +3 "$ho/.kore-diff.out" > "$o"
same "diff"
# kore dispatches on argv[0], so a link named `diff` IS diff. the shim is the distro's
# own shape (a script reading basename $0, tool names symlinked onto it) -- the build
# tree carries no kore binary anymore, the crew riding love's own image.
printf '#!/bin/sh\nn=$(basename -- "$0")\nLOVE_NO_IMAGE= exec "%s" kore "$n" "$@"\n' "$PWD/$m" > "$ho/.koreshim"
chmod 755 "$ho/.koreshim"
ln -sf .koreshim "$ho/diff"
"$ho/diff" "$ho/.au1" "$ho/.au2" > "$ho/.kore-sym.out" 2>&1; r=$?
[ $r -eq 1 ] && cmp -s "$ho/.kore-diff.out" "$ho/.kore-sym.out" \
  || fail "kore argv0 symlink (exit $r)"
korerun bogus > /dev/null 2>&1; r=$?
[ $r -eq 2 ] || fail "kore usage (exit $r)"
if [ "$(uname -m)" = x86_64 ]; then
  printf '(li r0 60) (li r6 7) (sys)\n' > "$ho/.kore-as.l"
  korerun as x64 "$ho/.kore-as.l" "$ho/.kore-as.elf" > /dev/null 2>&1 || fail "kore as"
  chmod +x "$ho/.kore-as.elf"; "$ho/.kore-as.elf"; r=$?
  [ $r -eq 7 ] || fail "kore as run (exit $r)"
fi
# ar + ld, over mooncc objects (x86_64; both need the mooncc shim beside us).
# ar: GNU-shape TO THE BYTE -- same members through binutils ar (D = deterministic,
# our only mode) and ours, whole archives cmp'd; `ar t` lists alike. ld: lay the
# same crt0 object mooncc's own link lane synthesizes (crt0/objelf leak from the
# mooncc cat), then our applet must bind crt0+main+f BYTE-IDENTICAL to mooncc's
# whole-program link, and the exe must run: 35 + 7 = exit 42.
if [ "$(uname -m)" = x86_64 ] && [ -x "$ho/mooncc" ]; then
  printf 'int f(void){return 35;}\n' > "$ho/.kore-arf.c"
  printf 'int f(void);\nint main(void){return f()+7;}\n' > "$ho/.kore-arm.c"
  "$ho/mooncc" -c "$ho/.kore-arf.c" "$ho/.kore-arf.o" >/dev/null 2>&1 || fail "kore ar: mooncc -c f.c"
  "$ho/mooncc" -c "$ho/.kore-arm.c" "$ho/.kore-arm.o" >/dev/null 2>&1 || fail "kore ar: mooncc -c main.c"
  if command -v ar >/dev/null 2>&1; then
    rm -f "$ho/.kore-gnu.a" "$ho/.kore-our.a"
    ar rcsD "$ho/.kore-gnu.a" "$ho/.kore-arf.o" "$ho/.kore-arm.o"
    korerun ar rcs "$ho/.kore-our.a" "$ho/.kore-arf.o" "$ho/.kore-arm.o" || fail "kore ar rcs"
    cmp -s "$ho/.kore-gnu.a" "$ho/.kore-our.a" || fail "kore ar vs GNU (archive bytes)"
    ar t "$ho/.kore-gnu.a" > "$g"; korerun ar t "$ho/.kore-our.a" > "$o"; same "ar t"
  fi
  "$m" -l "$ho/.mooncc-cat.l" -e '(write-bytes "'"$ho"'/.kore-crt0.o" (objelf (intern "x64") crt0 () (link "__ai_start" ()) () (link "__ai_start" ()) () () () () ()))' >/dev/null 2>&1
  [ -s "$ho/.kore-crt0.o" ] || fail "kore ld: crt0 lay"
  "$ho/mooncc" "$ho/.kore-arm.o" "$ho/.kore-arf.o" -o "$ho/.kore-mc.elf" >/dev/null 2>&1 || fail "kore ld: mooncc link"
  korerun ld "$ho/.kore-crt0.o" "$ho/.kore-arm.o" "$ho/.kore-arf.o" -o "$ho/.kore-ld.elf" || fail "kore ld"
  # ⚠ BYTE-IDENTICAL, and it is `.comment` that lets it be: both doors drive the SAME linker,
  # so the file they write is the same file, producer record included. It briefly was not --
  # mooncc stamped "mooncc" and kore ld stamped "holo", which shifted every header after it and
  # cost this check ten lines of objcopy to look past. The distinction carried nothing: one
  # linker, and the only caller of the holo door was this test.
  cmp -s "$ho/.kore-mc.elf" "$ho/.kore-ld.elf" || fail "kore ld vs mooncc link (bytes)"
  "$ho/.kore-ld.elf"; r=$?
  [ $r -eq 42 ] || fail "kore ld run (exit $r)"
  # and the archive as a LINK INPUT: `mooncc main.o libf.a` must bind the exe the
  # .o link binds, byte for byte -- which is the proof that members come in BY NEED
  # through the ranlib index, since the library also carries one nothing calls. our
  # ar writes it, our linker reads it (crew/holo/link.l's ld-arsyms).
  printf 'int unused(void){return 99;}\n' > "$ho/.kore-arz.c"
  "$ho/mooncc" -c "$ho/.kore-arz.c" "$ho/.kore-arz.o" >/dev/null 2>&1 || fail "kore ar: mooncc -c unused.c"
  rm -f "$ho/.kore-arl.a"
  korerun ar rcs "$ho/.kore-arl.a" "$ho/.kore-arf.o" "$ho/.kore-arz.o" || fail "kore ar rcs (library)"
  "$ho/mooncc" "$ho/.kore-arm.o" "$ho/.kore-arl.a" -o "$ho/.kore-ara.elf" >/dev/null 2>&1 || fail "kore ld: archive input"
  cmp -s "$ho/.kore-mc.elf" "$ho/.kore-ara.elf" || fail "kore ld archive vs .o link (bytes -- an unneeded member rode in?)"
  "$ho/.kore-ara.elf"; r=$?
  [ $r -eq 42 ] || fail "kore ld archive run (exit $r)"
  # objcopy over the exe we just linked. byte-equality with the real objcopy is
  # test_objcopy's job; what this row is for is the DISPATCH -- that the applet is
  # reachable off the registry and takes the arguments it advertises.
  korerun objcopy -O binary "$ho/.kore-ld.elf" "$ho/.kore-oc.bin" || fail "kore objcopy -O binary"
  korerun objcopy -O ihex "$ho/.kore-ld.elf" "$ho/.kore-oc.hex" || fail "kore objcopy -O ihex"
  [ -s "$ho/.kore-oc.bin" ] && [ -s "$ho/.kore-oc.hex" ] || fail "kore objcopy wrote nothing"
  grep -q '^:00000001' "$ho/.kore-oc.hex" || fail "kore objcopy: no ihex end record"
  korerun objcopy -O srec "$ho/.kore-ld.elf" "$ho/.kore-oc.x" 2>/dev/null \
    && fail "kore objcopy took an unknown format"
  # nm over holo's own ELF reader. the differential is against LC_ALL=C nm: the
  # BYTE order is ours, and a desk with a locale set gets a collated one from GNU,
  # so an uncollated `nm` here would fail on the machine and not in the tree.
  # ⚠ the executable is in the roster on purpose -- ld-read is ET_REL by contract
  # and ld-syms is the door that is not, so a regression that hands nm to ld-read
  # shows up here rather than the day someone reads a linked file.
  if command -v nm >/dev/null 2>&1; then
    for f in .kore-arm.o .kore-arf.o .kore-ld.elf; do
      LC_ALL=C nm "$ho/$f" > "$g" 2>/dev/null
      korerun nm "$ho/$f" > "$o" || fail "kore nm $f"
      same "nm $f"
    done
  fi
  korerun nm -u "$ho/.kore-arm.o" > "$o" || fail "kore nm -u"
  [ "$(cat "$o")" = "                 U f" ] || fail "kore nm -u (want the one undefined nom)"
  korerun nm -g "$ho/.kore-arf.o" > "$o" || fail "kore nm -g"
  grep -q ' T f$' "$o" || fail "kore nm -g (want T f)"
  korerun nm "$ho/.kore-arf.o" "$ho/.kore-arm.o" > "$o" || fail "kore nm (two files)"
  grep -q '\.kore-arm\.o:$' "$o" || fail "kore nm: no per-file header past one file"
  korerun nm "$ho/.kore-arf.c" >/dev/null 2>&1 && fail "kore nm read a non-ELF"
fi
echo "kore: diff (GNU-identical) + argv0 symlink + usage + as + ar + ld + objcopy + nm ok"

# ------------------------------------------------------------- the line tools
printf 'b\na\nc\nb\n' > "$ho/.cu1"; printf 'x y\nz\n' > "$ho/.cu2"
LC_ALL=C sort "$ho/.cu1" > "$g"; korerun sort "$ho/.cu1" > "$o"; same "sort"
LC_ALL=C sort -u "$ho/.cu1" > "$g"; korerun sort -u "$ho/.cu1" > "$o"; same "sort -u"
LC_ALL=C sort "$ho/.cu1" | uniq -c > "$g"
korerun sort "$ho/.cu1" | korerun uniq -c > "$o"; same "uniq -c"
both "head"     head -n 2 "$ho/.cu1" "$ho/.cu2"
both "tail"     tail -n 2 "$ho/.cu1" "$ho/.cu2"
both "wc"       wc "$ho/.cu1" "$ho/.cu2"
wc -l < "$ho/.cu1" > "$g"; korerun wc -l < "$ho/.cu1" > "$o"; same "wc -l stdin"
both "cat"      cat "$ho/.cu1" "$ho/.cu2"
both "seq"      seq 5
both "echo"     echo hi there
both "basename" basename /a/b.txt .txt
# tee writes twice: the file half is checked as well as the stream half
printf 'q\nq\nr\n' | tee "$ho/.cu-g2" > "$g"
printf 'q\nq\nr\n' | korerun tee "$ho/.cu-o2" > "$o"
cmp -s "$g" "$o" && cmp -s "$ho/.cu-g2" "$ho/.cu-o2" || fail "kore tee vs GNU"
echo "kore: line tools (sort/uniq/head/tail/wc/cat/seq/echo/basename/tee GNU-identical) ok"

# ------------------------------------------------------------ the field tools
printf 'a:b:c\nnodelim\nx:y\n' > "$ho/.fu1"
both "cut -f"    cut -d: -f1,3    "$ho/.fu1"
both "cut -s"    cut -d: -f1,3 -s "$ho/.fu1"
both "cut -f2-"  cut -d: -f2-     "$ho/.fu1"
pipe "cut -c"    'hello
hi
'                cut -c2-4
pipe "tr"        'hi there
'                tr a-z A-Z
pipe "tr class"  'Mixed Case 123
'                tr '[:lower:]' '[:upper:]'
pipe "tr pad"    'abcd
'                tr abcd xy
pipe "tr -d"     'hello world
'                tr -d aeiou
pipe "tr -s"     'aa  bb   cc
'                tr -s ' '
pipe "nl"        'a

b
'                nl
pipe "rev"       'abc
de
'                rev
echo "kore: field tools (cut/tr/nl/rev GNU-identical) ok"

# --------------------------------------------------------------- the fs tools
P=$ho/.fsplay
rm -rf "$P"; mkdir "$P"
korerun mkdir -p "$P/a/b/c" && [ -d "$P/a/b/c" ] || fail "kore mkdir -p"
printf 'hi there\n' > "$P/f1"
korerun cp "$P/f1" "$P/f2" && cmp -s "$P/f1" "$P/f2" || fail "kore cp"
korerun cp "$P/f1" "$P/a" && cmp -s "$P/f1" "$P/a/f1" || fail "kore cp into dir"
korerun mv "$P/f2" "$P/f3" && [ ! -e "$P/f2" ] && cmp -s "$P/f1" "$P/f3" || fail "kore mv"
korerun ln -s f1 "$P/l1" && [ "$(readlink "$P/l1")" = f1 ] || fail "kore ln -s"
korerun ln "$P/f1" "$P/h1" && [ "$P/h1" -ef "$P/f1" ] || fail "kore ln"
korerun touch "$P/new" "$P/.hidden" && [ -f "$P/new" ] && [ -f "$P/.hidden" ] || fail "kore touch"
korerun chmod 600 "$P/f1" && [ "$(stat -c %a "$P/f1")" = 600 ] || fail "kore chmod"
LC_ALL=C ls -1 "$P" > "$g"; korerun ls "$P" > "$o"; same "ls"
# ours shows dotfiles but never . / .. , which is GNU's -A
LC_ALL=C ls -A -1 "$P" > "$g"; korerun ls -a "$P" > "$o"; same "ls -a vs GNU -A"
[ "$(korerun pwd)" = "$(pwd)" ] || fail "kore pwd"
korerun rm "$P/f3" && [ ! -e "$P/f3" ] || fail "kore rm"
korerun rm -r "$P/a" && [ ! -e "$P/a" ] || fail "kore rm -r"
korerun mkdir "$P/empty" && korerun rmdir "$P/empty" && [ ! -e "$P/empty" ] || fail "kore rmdir"
korerun rm "$P/nope" > /dev/null 2>&1; r=$?; [ $r -eq 1 ] || fail "kore rm miss exit"
korerun rm -f "$P/nope" > /dev/null 2>&1; r=$?; [ $r -eq 0 ] || fail "kore rm -f quiet"
# the build's fs verbs: install lays parents + mode, cmp answers 0/1/2, readlink
# chases -f to GNU's canonical answer
korerun install -D -m 644 "$P/f1" "$P/i/n/dst" && [ "$(stat -c %a "$P/i/n/dst")" = 644 ] \
  && cmp -s "$P/f1" "$P/i/n/dst" || fail "kore install -D -m"
korerun install -d "$P/i/d1/d2" && [ -d "$P/i/d1/d2" ] || fail "kore install -d"
korerun cmp -s "$P/f1" "$P/i/n/dst"; [ $? -eq 0 ] || fail "kore cmp same"
printf 'other\n' > "$P/i/o"
korerun cmp -s "$P/f1" "$P/i/o"; [ $? -eq 1 ] || fail "kore cmp differ"
korerun cmp -s "$P/f1" "$P/i/nope" 2> /dev/null; [ $? -eq 2 ] || fail "kore cmp trouble"
[ "$(korerun readlink "$P/l1")" = "$(readlink "$P/l1")" ] || fail "kore readlink"
ln -sf l1 "$P/l2"
[ "$(korerun readlink -f "$P/l2")" = "$(readlink -f "$P/l2")" ] || fail "kore readlink -f"
echo "kore: fs tools (mkdir/cp/mv/ln/touch/chmod/ls/pwd/rm/rmdir/install/cmp/readlink) ok"

# ------------------------------------------------------------------- the greps
printf 'abc\nxbz\nzzz\n+q\n*r\n' > "$ho/.gr1"; printf 'nope\nbc here\n' > "$ho/.gr2"
both "grep"          grep b "$ho/.gr1"
both "grep -n multi" grep -n b "$ho/.gr1" "$ho/.gr2"
both "grep -c"       grep -c b "$ho/.gr1" "$ho/.gr2"
both "grep -v"       grep -v b "$ho/.gr1"
both "grep -l"       grep -l b "$ho/.gr1" "$ho/.gr2"
pipe "grep -l stdin" 'q
'                    grep -l q
both "grep empty pattern" grep '' "$ho/.gr1"
# the BRE battery: star, anchors, classes, +/? , groups, and the two shapes where a
# leading + or * is a LITERAL because there is nothing to repeat
for p in 'ab*c' '^x' 'z$' '[abx]b' '[^a]b' 'b\+' 'xb\?z' '\(zz\)*z' '.z' '^\+q' '^*r' 'x[b-z]z' \
         'a\|z' 'abc\|zzz\|nope' '\(a\|x\)b' 'a\|b\|c' 'a\|' '\|a' \
         'b\{2\}' 'z\{2,\}' 'z\{1,2\}' '\{2\}' '\+q' 'a\{3,2\}' 'a\{2' \
         '[[:digit:]]' '[[:alpha:]][[:digit:]]' '[^[:alnum:]]' '[[:space:]]' \
         '[[:upper:]]' '[[:punct:]]' '[[:xdigit:]]' '[]a]' '[a-]' '[[:nope:]]'; do
  grep -c "$p" "$ho/.gr1" > "$g" 2>/dev/null; a=$?
  korerun grep -c "$p" "$ho/.gr1" > "$o" 2>/dev/null; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore grep BRE '$p' vs GNU"
done
# the ERE battery. ⚠ THE RAGGED EDGES ARE THE POINT: a loose repeat is DROPPED in
# ERE where BRE keeps it as ink, and an unclosed brace / stray ) are literals here
# and malformed there -- all four read off GNU, none of them guessable
for p in 'a|z' '(a|x)b' 'a{2}' 'z{2,}' 'z{1,2}' '[[:digit:]]+' 'a+' 'ab?c' '(a|b)+' \
         '^(a|x)' '(a|b|z)+$' 'a{1,2}b' '[[:digit:]]{2}' '*r' '+q' '?x' '{2}' \
         'a{' 'a)' 'a||z' 'a{3,2}' '(a'; do
  grep -Ec "$p" "$ho/.gr1" > "$g" 2>/dev/null; a=$?
  korerun grep -Ec "$p" "$ho/.gr1" > "$o" 2>/dev/null; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore grep ERE '$p' vs GNU"
done
# the flag matrix, CLUSTERED as this tree's own scripts write them (-qF, -qiw, -oE):
# a walk that reads only whole words takes `-qF` for a pattern and passes every
# single-flag test while doing it
printf 'abc\nxbz\nzzz\n+q\n*r\nFoo Bar\nab_cd\nx{2}y\na1b2\n' > "$ho/.gr3"
# ⚠ set -f FIRST: the word split below is deliberate, the PATHNAME EXPANSION that
# rides along with it is not -- `[a-z]*` and `*r` are globs, and an unguarded split
# hands grep whatever files happen to sit in the cwd instead of the pattern
set -f
for fl in '-c b' '-i FOO' '-i foo' '-w ab' '-w abc' '-x zzz' '-x zz' '-x ' \
          '-F a\|z' '-F *r' '-F ab' '-o b' '-oE [a-z]+' '-n -i foo' '-h b' '-a b' \
          '-c -m 2 b' '-m 1 z' '-v b' '-iw foo' '-nv b' '-co b' '-iF foo' \
          '-ow [a-z]*' '-o ' '-oE [0-9]|[A-Z]'; do
  # shellcheck disable=SC2086
  set -- $fl
  grep "$@" "$ho/.gr3" > "$g" 2>/dev/null; a=$?
  korerun grep "$@" "$ho/.gr3" > "$o" 2>/dev/null; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore grep flags '$fl' vs GNU"
done
set +f
# -e stacks and, once given, every positional word is a FILE
grep -e abc -e zzz "$ho/.gr3" > "$g"; korerun grep -e abc -e zzz "$ho/.gr3" > "$o"
same "grep -e stacking"
# -q is output-free and speaks only in the exit code (61 uses of it in this tree)
korerun grep -q b "$ho/.gr3" > "$o"; r=$?
[ $r -eq 0 ] && [ ! -s "$o" ] || fail "kore grep -q hit (exit $r, or it spoke)"
korerun grep -q qqq "$ho/.gr3" > "$o"; r=$?
[ $r -eq 1 ] && [ ! -s "$o" ] || fail "kore grep -q miss"
korerun grep b "$ho/.gr1" > /dev/null; r=$?; [ $r -eq 0 ] || fail "kore grep hit exit"
korerun grep qqq "$ho/.gr1" > /dev/null; r=$?; [ $r -eq 1 ] || fail "kore grep miss exit"
grep b "$ho/.gr-nope" 2> "$g"; a=$?
korerun grep b "$ho/.gr-nope" 2> "$o"; b=$?
cmp -s "$g" "$o" && [ $a -eq 2 ] && [ $b -eq 2 ] || fail "kore grep missing file vs GNU"
korerun grep b "$ho/.gr1" "$ho/.gr-nope" > /dev/null 2>&1; r=$?
[ $r -eq 2 ] || fail "kore grep err beats match exit"
echo "kore: grep (BRE + ERE batteries + the clustered flag matrix GNU-identical, the exit triple) ok"

# --------------------------------------------------------------------- the sed
printf 'abc\nxbz\nzzz\nq4\nw5\n' > "$ho/.sd1"
for sc in 's/b/X/' 's/z/Q/g' '2d' '/x/,/q/d' '$d' '2q' 's/x*/-/g' 's/\(b*\)z/[\1]/' \
          's/b/[&]/' 's|z|_|g' 's/a/1/; s/b/2/' 's/q\(.\)/<\1>/' \
          's/a\|z/Y/g' 's/[[:digit:]]/#/g' 's/b\{2\}/B/' '/a\|q/d' 's/\(a\|x\)b/@/'; do
  sed "$sc" "$ho/.sd1" > "$g"; a=$?
  korerun sed "$sc" "$ho/.sd1" > "$o"; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore sed '$sc' vs GNU"
done
# -E moves the backslashes; the dialect must reach BOTH an address and an s
for sc in 's/(a|x)b/@/' 's/a|z/Y/g' 's/[[:digit:]]+/#/' '/a|q/d' 's/b{1,2}/B/'; do
  sed -E "$sc" "$ho/.sd1" > "$g"; a=$?
  korerun sed -E "$sc" "$ho/.sd1" > "$o"; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore sed -E '$sc' vs GNU"
done
# -e stacks in order, and clusters (-ne is how this tree writes it)
sed -n -e 1p -e 3p "$ho/.sd1" > "$g"; korerun sed -n -e 1p -e 3p "$ho/.sd1" > "$o"
same "sed -e stacking"
sed -ne 2p "$ho/.sd1" > "$g"; korerun sed -ne 2p "$ho/.sd1" > "$o"; same "sed -ne clustered"
sed -e 's/a/1/' -e 's/b/2/' "$ho/.sd1" > "$g"
korerun sed -e 's/a/1/' -e 's/b/2/' "$ho/.sd1" > "$o"; same "sed -e twice"
# ⚠ -i IS A DIFFERENT STREAM MODEL, not just a different sink: each file is its own
# stream, so line numbers restart and $ is per-file. TWO files is the only test that
# can tell that from the joined lane -- with one file the two models agree
for t in 's/b/X/g' '1d' '$d' 's/[[:digit:]]/#/g'; do
  cp "$ho/.sd1" "$ho/.sdg"; cp "$ho/.sd1" "$ho/.sdo"
  sed -i "$t" "$ho/.sdg"; korerun sed -i "$t" "$ho/.sdo"
  cmp -s "$ho/.sdg" "$ho/.sdo" || fail "kore sed -i '$t' vs GNU"
done
cp "$ho/.sd1" "$ho/.sdga"; cp "$ho/.sd1" "$ho/.sdgb"
cp "$ho/.sd1" "$ho/.sdoa"; cp "$ho/.sd1" "$ho/.sdob"
sed -i '1d;$d' "$ho/.sdga" "$ho/.sdgb"
korerun sed -i '1d;$d' "$ho/.sdoa" "$ho/.sdob"
cmp -s "$ho/.sdga" "$ho/.sdoa" && cmp -s "$ho/.sdgb" "$ho/.sdob" \
  || fail "kore sed -i over TWO files (the per-file model) vs GNU"
for sc in '2,4p' '/z/p' 's/b/X/p' '/x/,/q/p'; do
  sed -n "$sc" "$ho/.sd1" > "$g"
  korerun sed -n "$sc" "$ho/.sd1" > "$o"
  cmp -s "$g" "$o" || fail "kore sed -n '$sc' vs GNU"
done
pipe "sed stdin" 'ab
'                sed 's/a/1/'
printf 'a\n' | sed 's/a' > /dev/null 2>&1; a=$?
printf 'a\n' | korerun sed 's/a' > /dev/null 2>&1; b=$?
[ $a -eq 1 ] && [ $b -eq 1 ] || fail "kore sed bad-script exit (gnu $a ours $b)"
sed p "$ho/.sd-nope" "$ho/.sd1" > "$g" 2>&1; a=$?
korerun sed p "$ho/.sd-nope" "$ho/.sd1" > "$o" 2>&1; b=$?
cmp -s "$g" "$o" && [ $a -eq 2 ] && [ $b -eq 2 ] || fail "kore sed missing file vs GNU"
# ⚠ A MISSING FINAL NEWLINE IS DATA. GNU drops it after the LAST WRITE and not after
# every one, so `-n 'p;p'` keeps the inner newline and loses only the outer -- which is
# why the line rides a jug rather than a per-write flag. The whole line lane answers to
# this: sed, rev and the two clips (head that CUT before the last line does not).
printf 'a\nx' > "$ho/.nonl"
for sc in '' 'p' 's/a/A/' 's/x/Y/'; do
  sed "$sc" "$ho/.nonl" > "$g"; korerun sed "$sc" "$ho/.nonl" > "$o"
  same "sed '$sc' on a source with no final newline"
done
for sc in 'p' 'p;p'; do
  sed -n "$sc" "$ho/.nonl" > "$g"; korerun sed -n "$sc" "$ho/.nonl" > "$o"
  same "sed -n '$sc' on a source with no final newline"
done
for t in "rev" "head -n 5" "tail -n 5" "head -n 1" "head -1" "tail -1" "head -2"; do
  # shellcheck disable=SC2086
  $t "$ho/.nonl" > "$g"; korerun $t "$ho/.nonl" > "$o"
  same "$t on a source with no final newline"
done
rm -f "$ho/.nonl"
echo "kore: sed (s///gp + d/p/q + addresses + -E/-e/-i, the per-file model, GNU-identical, exits 1/2) ok"
echo "kore: the missing final newline is data (sed/rev/head/tail, and head -N) ok"

# --------------------------------------------------------- the process tools
pipe "xargs"     'a b
c
'                xargs
pipe "xargs -n 2" '1
2
3
4
5
'                 xargs -n 2 echo
pipe "xargs empty" '' xargs echo
printf 'x\n' | korerun xargs false; r=$?
[ $r -eq 123 ] || fail "kore xargs fail exit (rc $r)"
printf 'x\n' | korerun xargs /no/such/cmd 2>/dev/null; r=$?
[ $r -eq 127 ] || fail "kore xargs 127 (rc $r)"
env AUP=44 sh -c 'printf %s "$AUP"' > "$g"
korerun env AUP=44 sh -c 'printf %s "$AUP"' > "$o"; same "env assign"
# _= is the shell's own last-argument variable and differs by who was exec'd
# the oracle wears korerun's own LOVE_NO_IMAGE= prefix, so the two children
# compare the same environment
LOVE_NO_IMAGE= env | grep -v '^_=' | LC_ALL=C sort > "$g"
korerun env | grep -v '^_=' | LC_ALL=C sort > "$o"; same "env print"
korerun env sh -c 'exit 3'; r=$?; [ $r -eq 3 ] || fail "kore env child exit (rc $r)"
korerun sleep 0.1 || fail "kore sleep"
korerun sleep xx 2>/dev/null; r=$?; [ $r -eq 1 ] || fail "kore sleep bad exit (rc $r)"
sleep 3 & sp=$!
korerun kill -9 $sp || fail "kore kill send"
wait $sp; r=$?; [ $r -eq 137 ] || fail "kore kill effect (rc $r)"
korerun kill -0 999999 2>/dev/null; r=$?; [ $r -eq 1 ] || fail "kore kill dead pid (rc $r)"
echo "kore: process tools (env/sleep/kill/xargs -- GNU-identical output, the exit faces) ok"

# ------------------------------------------------------------------ the shell
# lush rides the kore cat: `kore sh` (and an sh symlink) IS the shell -- the
# distro's /bin/sh. one -c through the image wake proves the whole ride:
# dispatch, compounds, cmdsub.
korerun sh -c 'if true; then echo "kore-sh $(echo ok)"; fi' > "$o" 2>&1; r=$?
[ $r -eq 0 ] && [ "$(cat "$o")" = "kore-sh ok" ] || fail "kore sh (exit $r)"
ln -sf .koreshim "$ho/sh"
"$ho/sh" -c 'echo via-symlink' > "$o" 2>&1; r=$?
[ $r -eq 0 ] && [ "$(cat "$o")" = "via-symlink" ] || fail "kore sh symlink (exit $r)"
echo "kore: sh (lush aboard -- kore sh + the argv0 symlink) ok"

# ------------------------------------------------------------------ awk
# gawk is the oracle and every check is byte-identical stdout. the input is a
# small table so fields, numbers and text all have something to bite on.
awkin=$ho/.kore-awkin
printf 'alice 30 engineer\nbob 25 baker\ncarol 41 engineer\n' > "$awkin"
aw() { n=$1; shift
       awk "$@" < "$awkin" > "$g" 2>/dev/null
       korerun awk "$@" < "$awkin" > "$o" 2>/dev/null
       same "awk $n"; }
aw fields   '{print $1, $3}'
aw nr-nf    '{print NR, NF, $NF}'
aw arith    'BEGIN{print 1+2, 7/2, 7%3, 2^10, -2^2, int(-3.7)}'
aw concat   'BEGIN{x="a"; y=1; print x y 2}'
aw numfmt   'BEGIN{print 1/3, 1e20, 0.00001, 100000, 3.0}'
aw strnum   '{if ($2 > 30) print $1}'
aw regex    '/engineer/{print $1}'
aw match    '{if ($0 ~ /^b/) print "b:" NR}'
aw rebuild  'BEGIN{OFS="-"}{$1=$1; print}'
aw setfield '{$2="X"; print}'
aw nf-set   '{NF=2; print; print NF}'
aw fs       -F' ' '{print NF}'
aw substr   'BEGIN{print substr("hello",2,3), substr("hello",0,3), substr("hello",4)}'
aw strfns   'BEGIN{print index("hello","ll"), length("hello"), toupper("aBc"), tolower("aBc")}'
aw split    'BEGIN{n=split("a:b:c",A,":"); print n, A[1], A[3]}'
aw gsub     '{n=gsub(/e/,"3"); print n, $0}'
aw subamp   'BEGIN{s="abc"; sub(/b/,"[&]",s); print s}'
aw matchfn  'BEGIN{print match("hello","l+"), RSTART, RLENGTH}'
aw printf   'BEGIN{printf "%s|%d|%5.2f|%-4s|%05d|%+d|%e|%g|%c|%x\n","a",42,3.14159,"b",42,7,1234.5,0.0000123,65,255}'
aw bignum   'BEGIN{print 1e20, 1e23, 2^60; printf "%d|%.0f\n", 1e20, 1e20}'
aw math     'BEGIN{printf "%.6f %.6f %.6f %.6f %.6f\n", atan2(1,1), atan2(1,-1), sin(1), cos(1), exp(2)}'
aw arrays   'BEGIN{a["x"]=1; a["y"]=2; n=0; for(k in a) n++; print n, ("x" in a), ("z" in a)}'
aw delete   'BEGIN{a[1]=1;a[2]=2; delete a[1]; print (1 in a), (2 in a), length(a)}'
aw subsep   'BEGIN{a[1,2]=5; print ((1,2) in a), ((1,3) in a)}'
aw loops    'BEGIN{for(i=0;i<6;i++){if(i==2)continue; if(i==4)break; printf "%d",i}; print ""}'
aw doloop   'BEGIN{i=0; do{printf "%d",i;i++}while(i<3); print ""}'
aw func     'function f(a,b){return a+b} BEGIN{print f(2,3)}'
aw funcarr  'function g(arr){arr["k"]=9} BEGIN{g(A); print A["k"]}'
aw funcloc  'function h(n,  i,s){for(i=1;i<=n;i++)s=s i; return s} BEGIN{print h(4)}'
aw recurse  'function fac(n){return n<=1?1:n*fac(n-1)} BEGIN{print fac(6)}'
aw next     '{if(NR==1) next; print "kept", $1}'
aw range    '/alice/,/bob/{print "R:" NR}'
aw uninit   'BEGIN{print x+0, "["x"]", length(x), !x}'
aw ofmt     'BEGIN{OFMT="%.2f"; print 3.14159, 3}'
aw convfmt  'BEGIN{CONVFMT="%.2g"; x=3.14159; print (x "")}'
aw vflag    -v x=7 'BEGIN{print x, x+1}'
aw dynre    'BEGIN{r="^b"; if("bob" ~ r) print "dyn-ok"}'
aw endonly  'END{print NR, $0}'
# the exit code is awk's own, and exit outside END still runs the END rules
korerun awk '{exit 3} END{print "end ran"}' < "$awkin" > "$o" 2>/dev/null; r=$?
[ $r -eq 3 ] && [ "$(cat "$o")" = "end ran" ] || fail "kore awk exit (exit $r): $(cat "$o")"
# a bad program is a diagnosed refusal, not a crash and not silence
korerun awk 'BEGIN{' < /dev/null > "$o" 2>"$g"; r=$?
[ $r -eq 2 ] && [ -s "$g" ] || fail "kore awk syntax error (exit $r)"
# -f takes the program off a file, and several concatenate
printf 'BEGIN{x=1}\n' > "$ho/.kore-awk1"; printf 'BEGIN{print x+1}\n' > "$ho/.kore-awk2"
awk -f "$ho/.kore-awk1" -f "$ho/.kore-awk2" < "$awkin" > "$g" 2>/dev/null
korerun awk -f "$ho/.kore-awk1" -f "$ho/.kore-awk2" < "$awkin" > "$o" 2>/dev/null
same "awk -f"
echo "kore: awk (41 checks byte-identical to gawk, the exit code, -f, the refusal) ok"

# ------------------------------------------------------------------ find
# ⚠ THE ORDER IS SORTED ON BOTH SIDES. find hands out readdir order, which is the
# file system's business and repeats for nobody; ours sorts each directory on
# purpose (a build wants the same tree to cut the same image twice), so the only
# honest comparison is of the SETS. everything else here is byte-identical.
ft=$ho/.kore-ftree
rm -rf "$ft"; mkdir -p "$ft/a/b" "$ft/c"
: > "$ft/f1.txt"; : > "$ft/a/f2.txt"; : > "$ft/a/b/f3.log"; : > "$ft/c/f4.txt"
ln -sf f1.txt "$ft/link1"
fd() { n=$1; shift
       find "$@" 2>/dev/null | LC_ALL=C sort > "$g"
       korerun find "$@" 2>/dev/null | LC_ALL=C sort > "$o"
       same "find $n"; }
fd plain     "$ft"
fd name      "$ft" -name '*.txt'
fd nameq     "$ft" -name 'f?.txt'
fd typef     "$ft" -type f
fd typed     "$ft" -type d
fd typel     "$ft" -type l
fd not       "$ft" '!' -type d
fd and       "$ft" -type f -name '*.txt'
fd or        "$ft" -name '*.log' -o -name '*.txt'
fd parens    "$ft" '(' -name '*.log' -o -name link1 ')'
fd maxdepth0 "$ft" -maxdepth 0
fd maxdepth1 "$ft" -maxdepth 1
fd mindepth2 "$ft" -mindepth 2
fd path      "$ft" -path '*/a/*'
fd prune     "$ft" -path '*/a' -prune -o -print
fd twopaths  "$ft/a" "$ft/c"
fd explicit  "$ft" -name '*.txt' -print
fd notname   "$ft" '!' -name '*.txt'
# -exec runs the command once per name; the output is ours to compare directly
korerun find "$ft" -name '*.log' -exec echo FOUND '{}' ';' > "$o" 2>/dev/null
[ "$(cat "$o")" = "FOUND $ft/a/b/f3.log" ] || fail "kore find -exec: $(cat "$o")"
# a path that is not there complains and the code remembers; the others still walk
korerun find "$ft/nope" "$ft/c" > "$o" 2>"$g"; r=$?
[ $r -eq 1 ] && [ -s "$g" ] && grep -q 'f4.txt' "$o" || fail "kore find missing path (exit $r)"
# a malformed expression is a refusal, not a walk
korerun find "$ft" -name > /dev/null 2>&1; r=$?
[ $r -eq 1 ] || fail "kore find bad expression (exit $r)"
echo "kore: find (18 walks set-identical to the system find, -exec, the two refusals) ok"

# --------------------------------------------------------- the record tools
# paste / comm / join / split / od, against the GNU tools. `both` carries most of
# it; split is checked by its EFFECT (the pieces it writes), which is the only
# thing it produces at all.
rt=$ho/.kore-rec
rm -rf "$rt"; mkdir -p "$rt"
printf 'a\nb\nc\n' > "$rt/p1"; printf '1\n2\n' > "$rt/p2"; printf 'X\nY\nZ\nW\n' > "$rt/p3"
both "paste"       paste "$rt/p1" "$rt/p2"
both "paste 3"     paste "$rt/p1" "$rt/p2" "$rt/p3"
both "paste -d"    paste -d: "$rt/p1" "$rt/p3"
# ⚠ the delimiter LIST cycles per gap and starts over each row -- a two-delimiter
# list over three columns is the only shape that can tell that from "the first one"
both "paste -d2"   paste -d':|' "$rt/p1" "$rt/p2" "$rt/p3"
both "paste -s"    paste -s "$rt/p1" "$rt/p2"
both "paste -s -d" paste -s -d, "$rt/p1" "$rt/p3"
printf 'apple\nbanana\ncherry\n' > "$rt/c1"; printf 'banana\ndate\n' > "$rt/c2"
for fl in '' -1 -2 -3 -12 -13 -23 -123; do
  # shellcheck disable=SC2086
  both "comm $fl" comm $fl "$rt/c1" "$rt/c2"
done
printf 'a 1 x\nb 2 y\nc 3 z\nc 4 w\n' > "$rt/j1"; printf 'a A\nc C\nc D\nd E\n' > "$rt/j2"
printf 'a:1:x\nb:2:y\nc:3:z\n' > "$rt/j3"; printf 'a:A\nc:C\n' > "$rt/j4"
printf '  a   1  \nb 2\n' > "$rt/j5"; printf 'a A\nb B\n' > "$rt/j6"
both "join"         join "$rt/j1" "$rt/j2"
both "join -a1"     join -a 1 "$rt/j1" "$rt/j2"
both "join -a1 -a2" join -a 1 -a 2 "$rt/j1" "$rt/j2"
both "join -v1"     join -v 1 "$rt/j1" "$rt/j2"
both "join -v2"     join -v 2 "$rt/j1" "$rt/j2"
both "join -t:"     join -t: "$rt/j3" "$rt/j4"
both "join -t: -a1" join -t: -a 1 "$rt/j3" "$rt/j4"
both "join -1 -2"   join -1 2 -2 1 "$rt/j1" "$rt/j2"
both "join blanks"  join "$rt/j5" "$rt/j6"
# ⚠ a key repeated on BOTH sides is the whole cross product, in file-1-outer order
both "join cross"   join "$rt/j1" "$rt/j1"
# split writes files and says nothing: the pieces are the comparison
seq 1 25 > "$rt/sq"; printf 'a\nb' > "$rt/nonl"; : > "$rt/none"
sp() { n=$1; shift
       rm -rf "$rt/sg" "$rt/so"; mkdir -p "$rt/sg" "$rt/so"
       ( cd "$rt/sg" && split "$@" ) 2>/dev/null
       ( cd "$rt/so" && LOVE_NO_IMAGE= "$K" kore split "$@" ) 2>/dev/null
       diff -r "$rt/sg" "$rt/so" > /dev/null 2>&1 || fail "kore split $n vs GNU"; }
sp "-l 10"   -l 10 ../sq
sp "-l 7 pre" -l 7 ../sq pre
sp "-b 13"   -b 13 ../sq
sp "-a 3"    -l 5 -a 3 ../sq
sp "-d"      -l 9 -d ../sq
sp "no final newline" -l 1 ../nonl
sp "empty writes nothing" -l 5 ../none
# od: the address radices, the readings, the limits, and the `*` a repeat collapses to
printf 'hello\nworld\n\001\002\377' > "$rt/o1"
printf 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB' > "$rt/odup"
printf '\0\1\7\10\11\12\13\14\15\134\177\377abc' > "$rt/oesc"
: > "$rt/oempty"
for f in o1 odup oesc oempty; do
  for fl in '' -c -b -x -a -d -s -i -tx1 -tx4 -to1 -to4 -tu1 -tu4 -td1 -td4 -tc -ta \
            -Ad -Ax -An '-v -c' '-N 5 -c' '-j 3 -c' '-j 3 -N 4 -c'; do
    # shellcheck disable=SC2086
    both "od $fl $f" od $fl "$rt/$f"
  done
done
both "od -An -tx1"  od -An -tx1 "$rt/o1"
both "od two files" od -c "$rt/o1" "$rt/oesc"
both "od -Ax -to2"  od -Ax -to2 "$rt/o1"
echo "kore: record tools (paste/comm/join/split/od GNU-identical -- od over 4 files x 24 readings) ok"

# ------------------------------------------------------------------- expr
# ⚠ EXPR SPEAKS IN THE EXIT CODE as much as on stdout (0 the answer is neither ""
# nor "0", 1 it is, 2 the expression will not do), so `both` comparing both is the
# whole check. the arithmetic ones are here because C's TRUNCATING division and
# love's FLOORING // disagree on every negative pair.
for e in '1 + 2' '10 / 3' '10 % 3' '3 * 4' '1 + 2 * 3' '( 1 + 2 ) * 3' '5 - 8' \
         '-7 / 2' '-7 % 2' '7 / -2' 'abc = abc' 'abc = abd' '2 < 10' '2 < 10a' \
         'abc < abd' '3 >= 3' '3 != 4' '1 | 2' '0 | 3' '0 & 2' '1 & 2' \
         'abc : a.c' 'abcd : a.c' 'abc : x' 'length abcde' 'substr abcdef 2 3' \
         'substr abcdef 0 3' 'substr abcdef 5 99' 'index abcdef cd' 'index abcdef z' \
         '+ length' '0' 'foo' '1 / 0' 'a + 1' '1 +'; do
  # shellcheck disable=SC2086
  set -- $e
  expr "$@" > "$g" 2>/dev/null; a=$?
  korerun expr "$@" > "$o" 2>/dev/null; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore expr '$e' vs GNU (gnu $a ours $b)"
done
# the two the loop cannot carry: a group in the pattern, and the empty operand
expr abc : 'a\(.\)c' > "$g"; korerun expr abc : 'a\(.\)c' > "$o"; same "expr group"
expr abc : 'a\(x\)c' > "$g" 2>/dev/null; a=$?
korerun expr abc : 'a\(x\)c' > "$o" 2>/dev/null; b=$?
cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore expr empty group vs GNU"
expr '' '|' foo > "$g"; korerun expr '' '|' foo > "$o"; same "expr empty | foo"
echo "kore: expr (36 expressions + the groups, stdout AND the 0/1/2 exit, GNU-identical) ok"

# ------------------------------------------------- stat, du, date, id, mktemp, chown
# ⚠ TZ=UTC: this love has no tz database (localtime IS gmtime), so `date` and stat's
# %y are UTC and only UTC. GNU reads TZ, so the oracle has to be told.
export TZ=UTC
dt=$ho/.kore-dt
rm -rf "$dt"; mkdir -p "$dt/a/b" "$dt/c"
printf '0123456789' > "$dt/f1"
head -c 9000 /dev/urandom > "$dt/a/f2"
: > "$dt/a/b/f3"
ln -sf f1 "$dt/lk"
for f in '%n' '%s' '%a' '%A' '%F' '%u' '%U' '%g' '%G' '%h' '%i' '%Y' '%b' '%B' '%f' \
         '%N' '%y' '%n|%s|%a' 'x%%y' 'a\tb\n'; do
  both "stat -c $f" stat -c "$f" "$dt/f1"
done
both "stat dir"      stat -c '%n %F %A %a' "$dt/a"
# ⚠ the bare face does NOT follow a link and -L does -- one stat call apart, and the
# only check that can tell lstat from stat at all
both "stat link"     stat -c '%n %F %A' "$dt/lk"
both "stat -L link"  stat -L -c '%n %F %A' "$dt/lk"
both "stat many"     stat -c '%s %n' "$dt/f1" "$dt/a/f2"
both "stat empty"    stat -c '%F' "$dt/a/b/f3"
# -c adds a newline and reads no escapes; --printf reads them and adds none
both "stat --printf" stat --printf='a\t%s\n' "$dt/f1"
korerun stat -c %s "$dt/nope" > /dev/null 2>&1; r=$?
[ $r -eq 1 ] || fail "kore stat missing file (exit $r)"
# du: the walk hands out readdir order, so the tree comparisons are of the SETS
# (find's honesty, and for the same reason); the single-path ones are byte-identical
dusort() { n=$1; shift
           "$@" 2>/dev/null | LC_ALL=C sort > "$g"
           korerun "$@" 2>/dev/null | LC_ALL=C sort > "$o"
           same "du $n"; }
dusort "plain" du "$dt"
dusort "-a"    du -a "$dt"
dusort "-d 1"  du -d 1 "$dt"
dusort "-ab"   du -ab "$dt"
both "du -s"   du -s "$dt"
both "du file" du "$dt/f1"
both "du -c"   du -c "$dt/f1" "$dt/a/f2"
both "du -sb"  du -sb "$dt"
both "du -sh"  du -sh "$dt"
both "du -sk"  du -sk "$dt"
both "du -h"   du -h "$dt/a/f2"
# a hard link is counted ONCE per run, which is the whole reason du reads inodes
rm -rf "$dt/hl"; mkdir "$dt/hl"; head -c 9000 /dev/urandom > "$dt/hl/one"
ln "$dt/hl/one" "$dt/hl/two"
both "du hard link" du -s "$dt/hl"
# ..and -h's three significant figures, rounded UP, over a scale that reaches G
rm -rf "$dt/hs"; mkdir "$dt/hs"
for sz in 1 5000 11000 100000 1500000 20000000; do head -c $sz /dev/zero > "$dt/hs/f$sz"; done
dusort "-ah over a scale" du -ah "$dt/hs"
# date: -d @SECONDS is what makes this gateable at all -- `now` differs by the second
for s in 0 1 1000000000 1700000000 1234567890 951782400 2147483647 4102444800; do
  for f in '' '+%Y-%m-%d %H:%M:%S' \
           '+%a %A %b %B %j %y %C %e %F %T %D %s %H %I %p %u %w %Z %z' '+%%|%n|%t|'; do
    if [ -n "$f" ]; then
      LC_ALL=C date -u -d @$s "$f" > "$g"; korerun date -u -d @$s "$f" > "$o"
    else
      LC_ALL=C date -u -d @$s > "$g"; korerun date -u -d @$s > "$o"
    fi
    same "date -d @$s '$f'"
  done
done
LC_ALL=C date -u -r "$dt/f1" '+%Y-%m-%d %H:%M:%S' > "$g"
korerun date -u -r "$dt/f1" '+%Y-%m-%d %H:%M:%S' > "$o"; same "date -r FILE"
[ "$(korerun date '+%Y')" = "$(date -u '+%Y')" ] || fail "kore date (now)"
# id: the numeric and named faces byte-identical, groups included -- the supplementary
# list is read out of /etc/group here (no getgroups, no NSS), so it is a real check
for fl in -u -g -un -gn -G ''; do
  # shellcheck disable=SC2086
  both "id $fl" id $fl
done
# mktemp answers a name nobody had, so the SHAPE and the effect are the check
t=$(korerun mktemp) || fail "kore mktemp"
case $t in /tmp/tmp.??????????) [ -f "$t" ] || fail "kore mktemp made no file";;
           *) fail "kore mktemp name: $t";; esac
[ "$(stat -c %a "$t")" = 600 ] || fail "kore mktemp mode: $(stat -c %a "$t")"
rm -f "$t"
t=$(korerun mktemp -d); [ -d "$t" ] || fail "kore mktemp -d"; rmdir "$t"
t=$(korerun mktemp -p "$dt" wooXXXXXX)
case $t in "$dt"/woo??????) [ -f "$t" ] || fail "kore mktemp -p made no file";;
           *) fail "kore mktemp -p name: $t";; esac
rm -f "$t"
t=$(korerun mktemp -u); [ -e "$t" ] && fail "kore mktemp -u left the file behind"
# chown: unprivileged, so the honest checks are the no-op and the refusal
korerun chown "$(id -un):$(id -gn)" "$dt/f1" || fail "kore chown to our own ids"
korerun chown nosuchuser000 "$dt/f1" 2>/dev/null; r=$?
[ $r -eq 1 ] || fail "kore chown unknown user (exit $r)"
echo "kore: stat/du/date/id/mktemp/chown (GNU-identical, the tree sums, the UTC clock) ok"

# ------------------------------------------------------------------ patch
# ⚠ THE ORACLE IS THE TREE, not the message. GNU patch's chatter has moved between
# releases; what has not is what it leaves on disk, so every check here runs GNU
# and ours over two identical copies and requires the copies to still match.
pw=$HO/.kore-pw
pset() { rm -rf "$pw"; mkdir -p "$pw/g/sub" "$pw/o/sub"
         printf 'one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\n' > "$pw/base"
         cp "$pw/base" "$pw/g/sub/f.txt"; cp "$pw/base" "$pw/o/sub/f.txt"; }
pmk() { cp "$pw/base" "$pw/new"; sed -i "$1" "$pw/new"
        ( cd "$pw" && diff -u base new \
            | sed -e '1s|^--- base.*|--- a/sub/f.txt|' -e '2s|^+++ new.*|+++ b/sub/f.txt|' ) > "$pw/p.diff"; }
prun() { n=$1; shift
         ( cd "$pw/g" && patch "$@" < "$pw/p.diff" ) >/dev/null 2>&1; a=$?
         ( cd "$pw/o" && LOVE_NO_IMAGE= "$K" kore patch "$@" < "$pw/p.diff" ) >/dev/null 2>&1; b=$?
         diff -r "$pw/g" "$pw/o" > /dev/null 2>&1 && [ $a -eq $b ] \
           || { diff -r "$pw/g" "$pw/o" | head -5; fail "kore patch $n (gnu $a ours $b)"; }; }
pset; pmk 's/three/THREE/; s/seven/SEVEN/'; prun "two hunks" -p1
pset; pmk 's/three/THREE/; s/seven/SEVEN/'
( cd "$pw/g" && patch -p1 < "$pw/p.diff" ) >/dev/null 2>&1
( cd "$pw/o" && LOVE_NO_IMAGE= "$K" kore patch -p1 < "$pw/p.diff" ) >/dev/null 2>&1
prun "-R puts it back" -p1 -R
# an offset: a line inserted ahead of the hunk moves it, and both must find it there
pset; pmk 's/three/THREE/; s/seven/SEVEN/'
for s in g o; do ( cd "$pw/$s/sub" && printf 'zero\n' > t && cat f.txt >> t && mv t f.txt ); done
prun "offset (and the .orig a mismatch leaves)" -p1
pset; pmk 's/three/THREE/'; prun "--dry-run touches nothing" -p1 --dry-run
# no -p at all: the basename, which is why `patch < p` works from inside the directory
pset; pmk 's/four/FOUR/'
( cd "$pw/g/sub" && patch < "$pw/p.diff" ) >/dev/null 2>&1; a=$?
( cd "$pw/o/sub" && LOVE_NO_IMAGE= "$K" kore patch < "$pw/p.diff" ) >/dev/null 2>&1; b=$?
diff -r "$pw/g" "$pw/o" > /dev/null 2>&1 && [ $a -eq $b ] || fail "kore patch (no -p)"
# a create (--- /dev/null), whose -0,0 seat is the one the search has to reach
pset; printf 'x\ny\nz\n' > "$pw/new"
( cd "$pw" && diff -u /dev/null new | sed -e '2s|^+++ new.*|+++ b/sub/new.txt|' ) > "$pw/p.diff"
prun "creates a file" -p1
# ⚠ the missing final newline, BOTH directions -- the `\ No newline` line carries no
# count of its own, so the one closing a hunk arrives after the counts are spent
pset; printf 'a\nb\nc' > "$pw/base"
cp "$pw/base" "$pw/g/sub/f.txt"; cp "$pw/base" "$pw/o/sub/f.txt"
pmk 's/c/C/'; prun "a source with no final newline" -p1
pset; printf 'a\nb\nc\n' > "$pw/base"
cp "$pw/base" "$pw/g/sub/f.txt"; cp "$pw/base" "$pw/o/sub/f.txt"
printf 'a\nb\nC' > "$pw/new"
( cd "$pw" && diff -u base new \
    | sed -e '1s|^--- base.*|--- a/sub/f.txt|' -e '2s|^+++ new.*|+++ b/sub/f.txt|' ) > "$pw/p.diff"
prun "the patch takes the newline away" -p1
# many hunks over a longer file, so the running delta gets exercised
pset; seq 1 200 > "$pw/base"
cp "$pw/base" "$pw/g/sub/f.txt"; cp "$pw/base" "$pw/o/sub/f.txt"
pmk 's/^7$/SEVEN/; s/^70$/SEVENTY/; s/^133$/ONETHIRTYTHREE/; 40d; 100i\INSERTED'
prun "many hunks" -p1
# -i names the patch, -p2 strips deeper, and two files ride one patch
pset; pmk 's/two/TWO/'
sed -i -e '1s|.*|--- x/y/sub/f.txt|' -e '2s|.*|+++ x/y/sub/f.txt|' "$pw/p.diff"
( cd "$pw/g" && patch -p2 -i "$pw/p.diff" ) >/dev/null 2>&1; a=$?
( cd "$pw/o" && LOVE_NO_IMAGE= "$K" kore patch -p2 -i "$pw/p.diff" ) >/dev/null 2>&1; b=$?
diff -r "$pw/g" "$pw/o" > /dev/null 2>&1 && [ $a -eq $b ] || fail "kore patch -p2 -i"
pset
printf 'aa\nbb\n' > "$pw/g/sub/g.txt"; cp "$pw/g/sub/g.txt" "$pw/o/sub/g.txt"
{ printf -- '--- a/sub/f.txt\n+++ b/sub/f.txt\n@@ -1,3 +1,3 @@\n one\n-two\n+TWO\n three\n'
  printf -- '--- a/sub/g.txt\n+++ b/sub/g.txt\n@@ -1,2 +1,2 @@\n aa\n-bb\n+BB\n'; } > "$pw/p.diff"
prun "two files in one patch" -p1
# a hunk with nowhere to go: exit 1, the file half-applied the same way, and a .rej
pset; pmk 's/three/THREE/; s/seven/SEVEN/'
for s in g o; do printf 'nope\nnope\nnope\nnope\nnope\nnope\nnope\nnope\n' > "$pw/$s/sub/f.txt"; done
( cd "$pw/g" && patch -p1 < "$pw/p.diff" ) >/dev/null 2>&1; a=$?
( cd "$pw/o" && LOVE_NO_IMAGE= "$K" kore patch -p1 < "$pw/p.diff" ) >/dev/null 2>&1; b=$?
[ $a -eq 1 ] && [ $b -eq 1 ] && cmp -s "$pw/g/sub/f.txt" "$pw/o/sub/f.txt" \
  && [ -f "$pw/o/sub/f.txt.rej" ] && [ -f "$pw/o/sub/f.txt.orig" ] \
  || fail "kore patch reject (gnu $a ours $b)"
cmp -s "$pw/g/sub/f.txt.rej" "$pw/o/sub/f.txt.rej" || fail "kore patch .rej vs GNU"
# ⚠ the .orig is the file AS IT WAS, which here is the unrelated one -- so the reject
# is re-applied to the tree the patch was cut against, and that is the real claim: a
# .rej we wrote is a patch our own reader takes back.
cp "$pw/base" "$pw/o/sub/f.txt"
( cd "$pw/o/sub" && LOVE_NO_IMAGE= "$K" kore patch f.txt < f.txt.rej ) >/dev/null 2>&1 \
  || fail "kore patch: the .rej does not re-apply"
cmp -s "$pw/o/sub/f.txt" "$pw/new" || fail "kore patch: the re-applied .rej lands elsewhere"
echo "kore: patch (13 applications leaving the same tree GNU patch does -- offsets, creates, rejects, the newline) ok"

# ------------------------------------------------------- the status charm
# every main ANSWERS its status (crew/kore/core.l's urun) instead of quitting, so
# a caller staying in the image lives through a tool that fails -- the property the
# seat hides, since the seat quits with the answer. one image, four tools whose
# statuses are 1, 2 (a udie from deep inside), 0 and 0: the run must reach the last
# say, and the charms must be exactly those. ⚠ nothing else here can catch this: a
# regression to `quit` still passes every check above.
LOVE_NO_IMAGE= "$m" -e '(: a (kore-main (list "kore" "false"))
                                    b (kore-main (list "kore" "basename"))
                                    c (kore-main (list "kore" "true"))
                                    d (kore-main (list "kore" "echo" "alive"))
                                    _ (say out (show a + " " + show b + " " + show c + " " + show d + "\n"))
                                    (quit 0))' > "$o" 2>/dev/null
r=$?
[ $r -eq 0 ] || fail "kore in-image: the process did not survive four tools (exit $r)"
[ "$(tail -1 "$o")" = "1 2 0 0" ] || fail "kore in-image statuses: $(tail -1 "$o")"
# ..and the unknown tool answers usage's 2 rather than ending anything
LOVE_NO_IMAGE= "$m" -e '(: r (kore-main (list "kore" "nosuchtool"))
                                    _ (say out ("after " + show r + "\n")) (quit 0))' > "$o" 2>/dev/null
[ "$(tail -1 "$o")" = "after 2" ] || fail "kore in-image unknown tool: $(tail -1 "$o")"
echo "kore: the status charm (mains answer, the image survives, the seat quits) ok"
