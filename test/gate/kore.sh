#!/bin/sh
# test/gate/kore.sh -- kore, the multi-call toolbox (crew/kore/), against GNU coreutils
# as the oracle. The laws first, then ~90 checks whose shape is almost always the same
# one: run the system tool, run OUR applet the same way, and require byte-identical
# stdout -- and, where the exit code carries meaning (grep's 0/1/2, sed's 1/2, xargs'
# 123/127), that too. GNU is not assumed correct, only independent.
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
korerun() { "$m" --wake "$ho/kore.image" -e '(kore-main (link "kore" (cuup (cup cmdline))))' "$@"; }

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
echo "UTILS crew/kore/{text,core,fs,re,sed,diff,law}.l"
out=$ho/.test_kore.out
cat test/00-init.l crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l \
    crew/kore/sed.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l \
    crew/vi/vi.l crew/kore/diff.l \
    crew/kore/law.l | "$m" > "$out" 2>&1
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
# kore dispatches on argv[0], so a symlink named `diff` IS diff
ln -sf kore "$ho/diff"
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
  "$m" -l "$ho/.mooncc-cat.l" -e '(write-bytes "'"$ho"'/.kore-crt0.o" (objelf (intern "x64") crt0 () (link "__ai_start" ()) () (link "__ai_start" ()) () () ()))' >/dev/null 2>&1
  [ -s "$ho/.kore-crt0.o" ] || fail "kore ld: crt0 lay"
  "$ho/mooncc" "$ho/.kore-arm.o" "$ho/.kore-arf.o" -o "$ho/.kore-mc.elf" >/dev/null 2>&1 || fail "kore ld: mooncc link"
  korerun ld "$ho/.kore-crt0.o" "$ho/.kore-arm.o" "$ho/.kore-arf.o" -o "$ho/.kore-ld.elf" || fail "kore ld"
  cmp -s "$ho/.kore-mc.elf" "$ho/.kore-ld.elf" || fail "kore ld vs mooncc link (bytes)"
  "$ho/.kore-ld.elf"; r=$?
  [ $r -eq 42 ] || fail "kore ld run (exit $r)"
fi
echo "kore: diff (GNU-identical) + argv0 symlink + usage + as + ar + ld ok"

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
for p in 'ab*c' '^x' 'z$' '[abx]b' '[^a]b' 'b\+' 'xb\?z' '\(zz\)*z' '.z' '^\+q' '^*r' 'x[b-z]z'; do
  grep -c "$p" "$ho/.gr1" > "$g" 2>/dev/null; a=$?
  korerun grep -c "$p" "$ho/.gr1" > "$o"; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore grep BRE '$p' vs GNU"
done
korerun grep b "$ho/.gr1" > /dev/null; r=$?; [ $r -eq 0 ] || fail "kore grep hit exit"
korerun grep qqq "$ho/.gr1" > /dev/null; r=$?; [ $r -eq 1 ] || fail "kore grep miss exit"
grep b "$ho/.gr-nope" 2> "$g"; a=$?
korerun grep b "$ho/.gr-nope" 2> "$o"; b=$?
cmp -s "$g" "$o" && [ $a -eq 2 ] && [ $b -eq 2 ] || fail "kore grep missing file vs GNU"
korerun grep b "$ho/.gr1" "$ho/.gr-nope" > /dev/null 2>&1; r=$?
[ $r -eq 2 ] || fail "kore grep err beats match exit"
echo "kore: grep (plain/-n/-c/-v/-l + BRE battery GNU-identical, the exit triple) ok"

# --------------------------------------------------------------------- the sed
printf 'abc\nxbz\nzzz\nq4\nw5\n' > "$ho/.sd1"
for sc in 's/b/X/' 's/z/Q/g' '2d' '/x/,/q/d' '$d' '2q' 's/x*/-/g' 's/\(b*\)z/[\1]/' \
          's/b/[&]/' 's|z|_|g' 's/a/1/; s/b/2/' 's/q\(.\)/<\1>/'; do
  sed "$sc" "$ho/.sd1" > "$g"; a=$?
  korerun sed "$sc" "$ho/.sd1" > "$o"; b=$?
  cmp -s "$g" "$o" && [ $a -eq $b ] || fail "kore sed '$sc' vs GNU"
done
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
echo "kore: sed (s///gp + d/p/q + number/\$/regex/range addresses GNU-identical, exits 1/2) ok"

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
env | grep -v '^_=' | LC_ALL=C sort > "$g"
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
ln -sf kore "$ho/sh"
"$ho/sh" -c 'echo via-symlink' > "$o" 2>&1; r=$?
[ $r -eq 0 ] && [ "$(cat "$o")" = "via-symlink" ] || fail "kore sh symlink (exit $r)"
echo "kore: sh (lush aboard -- kore sh + the argv0 symlink) ok"
