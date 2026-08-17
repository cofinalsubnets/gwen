#!/bin/sh
# moon-parity.sh -- one C feature per row, every mooncc target per column: which
# lanes exist where. doc/moon-c-gaps.md's parity table is this script's output,
# and `moon-parity.sh check` holds the doc to it.
#
# moon-sweep.sh measures one target against a real package; moon-reject.sh measures
# the refusal surface against gcc. This measures the TARGETS AGAINST EACH OTHER,
# which neither of those can see: a lane that x64 has and riscv64 does not is
# invisible to any single-target sweep, and invisible to gcc, because gcc has them
# all. Cross-target drift is the failure this tree actually ships -- core/love.c compiles
# everywhere, so the gaps live in the C that core/love.c never writes.
#
# THREE VERDICTS per cell, and the third is the interesting one:
#   ok       -- an object came out, referencing nothing the probe did not declare.
#               The lane is ours.
#   libgcc   -- an object came out, and it CALLS OUT: a lane that exists but is
#               borrowed. On a freestanding seat that is a link-time dependency,
#               not a compile-time one, so only the object's symbols reveal it.
#   —        -- refused. The cause is printed by `moon-parity.sh why`.
#
# The borrow test is "an undefined symbol the probe's own source never declared",
# not a grep for __aeabi_ -- so it catches an x64 or riscv64 lane reaching for
# libgcc's own spellings (__divti3, __muldc3) as readily as arm's.
#
# ⚠ NO FOREIGN TOOL: the symbols come from `kore nm -u`, which is holo's ELF reader
# (crew/holo/link.l's ld-syms) behind nm's surface -- one wake over every object the
# sweep laid, not one readelf per cell. Everything this script needs, the tree built.
#
# ⚠ A cell is a COMPILE, not a run. `ok` means the lane exists, never that it is
# right -- test_cts and the cross gates are what say that. Read this table for the
# SHAPE of the coverage and go elsewhere for its depth.
#
# ⚠ thumb2 and thumb2sp get their own columns, and the reason is one row: thumb2sp
# is ARMv7E-M with an SP-only FPU (the Playdate's STM32F746), so f64 softens to
# __aeabi_* where thumb2's fpv5-d16 does it in hardware. A merged t32 column hides
# exactly that, which is how the hand-written table got it wrong.
#
# usage: moon-parity.sh [table | check | why]
#   table  (default) the markdown table, ready to paste into doc/moon-c-gaps.md
#   check  diff the measured matrix against the table the doc carries; nonzero on drift
#   why    every refusal with the cause the compiler gave, and every borrow with its symbol
set -e

# mooncc and kore are love's own verbs (the layered bake); MOONCC/KORE still override.
# ⚠ `env`, not a bare assignment prefix: $mc expands AFTER assignment-recognition, so a
# literal `LOVE_NO_IMAGE=` in the expansion would run as a command name.
love=${LOVE:-out/host/love}
mc=${MOONCC:-env LOVE_NO_IMAGE= $love mooncc}
kore=${KORE:-env LOVE_NO_IMAGE= $love kore}
doc=${DOC:-doc/moon-c-gaps.md}
mode=${1:-table}
d=${TMPDIR:-/tmp}/moon-parity.$$
[ -x "$love" ] || { echo "moon-parity: no $love -- run make host"; exit 0; }
mkdir -p "$d"
trap 'rm -rf "$d"' EXIT

targets='x64 arm64 riscv64 thumb2 thumb2sp thumb1'

# p LABEL SOURCE [DECLARED..] -- one feature, compiled on every target. LABEL is the
# doc's row spelling and the join key; keep the two identical or `check` reports drift
# that is only a rename. DECLARED names the externs the SOURCE itself declares, which
# are undefined symbols that mean nothing about the lane; anything else undefined is
# the borrow.
n=0
p() {
  n=$((n+1))
  printf '%s\n' "$2" > "$d/p$n.c"
  printf '%s' "$1" > "$d/p$n.label"
  shift 2
  : > "$d/p$n.allow"
  for s in "$@"; do printf '%s\n' "$s" >> "$d/p$n.allow"; done
  for t in $targets; do
    if $mc -t "$t" -c "$d/p$n.c" -o "$d/p$n.$t.o" 2>"$d/p$n.$t.log"; then
      printf 'ok' > "$d/p$n.$t.v"
      printf '%s\n' "$d/p$n.$t.o" >> "$d/objs"
    else printf '%s' '—' > "$d/p$n.$t.v"; fi
  done
}

# --- the probes. Each is the smallest program that reaches ONE lane, and each
# DEFINES what it exercises: a bare prototype compiles on every target and would
# score a lane that is not there (doc/moon-c-gaps, the 16B-return trap).
p '`__int128`' \
'unsigned __int128 f(unsigned long a,unsigned long b){return (unsigned __int128)a*b;}'
p '`_Complex` arithmetic' \
'_Complex double f(_Complex double a,_Complex double b){return a*b;}'
p 'variable-length array' \
'int f(int n){int a[n];a[0]=1;return a[0];}'
p 'by-value composite arg, ≤16B, registers free' \
'typedef struct{int a,b,c,d;}S; static int t(S s){return s.a+s.d;} int g(void){S s={1,2,3,4};return t(s);}'
p 'by-value composite arg, MEMORY class' \
'typedef struct{long a[10];}S; static int t(S s){return (int)s.a[9];} int g(void){S s={{0}};return t(s);}'
p 'composite passed at a variadic call site' \
'typedef struct{int a,b;}S; int v(int n,...); int g(void){S s={1,2};return v(1,s);}' v
p 'composite NAMED in a variadic parameter list' \
'typedef struct{int a,b;}S; int v(int n,S s,...){return s.a+n;}'
p 'composite return, 16B all-int' \
'typedef struct{int a,b,c,d;}R; static R mk(int x){R r={x,x,x,x};return r;} int g(void){return mk(3).a;}'
p 'composite return, MEMORY class' \
'typedef struct{long a[10];}R; static R mk(long x){R r={{0}};r.a[9]=x;return r;} int g(void){return (int)mk(3).a[9];}'
p '`__builtin_bswap64`' \
'unsigned long long f(unsigned long long x){return __builtin_bswap64(x);}'
p '`__sync` spin-lock pair' \
'static volatile int L; int f(void){int o=__sync_lock_test_and_set(&L,1);__sync_lock_release(&L);return o;}'
p 'signed 64-bit `/` and `%`' \
'long long f(long long a,long long b){return a/b+a%b;}'
p '64-bit `*` and shifts' \
'long long f(long long a,int n){return (a*a)+(a<<n)+(a>>n);}'
p '`double`/`float` arithmetic' \
'double f(double a,double b){return a*b/(a+b);} float g(float a,float b){return a*b/(a+b);}'

# --- ONE wake over every object the sweep laid: `kore nm -u` prints the undefined
# symbols of each, under a `path:` header. An object whose every lane is ours shows
# its header and no rows, which is the answer, not a silence.
if [ -s "$d/objs" ]; then
  # shellcheck disable=SC2046
  $kore nm -u $(cat "$d/objs") 2>/dev/null > "$d/undef.txt" || true
  one=$(head -1 "$d/objs")
  awk -v one="$one" '
    /^\/.*:$/ { f=substr($0,1,length($0)-1); next }
    $1=="U" && NF==2 { print (f==""?one:f), $2 }
  ' "$d/undef.txt" > "$d/pairs.txt"
  while read -r f s; do
    b=${f##*/}; b=${b%.o}                 # p3.arm64
    i=${b%%.*}; i=${i#p}                  # 3
    t=${b#*.}                             # arm64
    grep -qx -- "$s" "$d/p$i.allow" 2>/dev/null && continue
    printf 'libgcc' > "$d/p$i.$t.v"
    printf '%s\n' "$s" >> "$d/p$i.$t.borrow"
  done < "$d/pairs.txt"
fi

cell() { v=$(cat "$d/p$1.$2.v"); [ "$v" = ok ] && printf '✓' || printf '%s' "$v"; }

emit_table() {
  echo '| lane | x64 | arm64 | riscv64 | thumb2 | thumb2sp | thumb1 |'
  echo '|---|:-:|:-:|:-:|:-:|:-:|:-:|'
  i=0
  while [ $i -lt $n ]; do
    i=$((i+1))
    printf '| %s | %s | %s | %s | %s | %s | %s |\n' "$(cat "$d/p$i.label")" \
      "$(cell $i x64)" "$(cell $i arm64)" "$(cell $i riscv64)" \
      "$(cell $i thumb2)" "$(cell $i thumb2sp)" "$(cell $i thumb1)"
  done
}

case "$mode" in
table) emit_table ;;

why)
  i=0
  while [ $i -lt $n ]; do
    i=$((i+1))
    for t in $targets; do
      v=$(cat "$d/p$i.$t.v")
      [ "$v" = ok ] && continue
      if [ "$v" = libgcc ]; then
        printf '  %-46s %-9s borrows %s\n' "$(cat "$d/p$i.label")" "$t" \
          "$(tr '\n' ' ' < "$d/p$i.$t.borrow")"
      else
        printf '  %-46s %-9s %s\n' "$(cat "$d/p$i.label")" "$t" \
          "$(sed 's/.*: //; s/ (in .*//' "$d/p$i.$t.log" | head -1)"
      fi
    done
  done
  ;;

check)
  emit_table > "$d/measured.md"
  # the doc's copy: the table opening `| lane |` through the first line that is not a row
  awk '/^\| lane \| x64 \|/{on=1} on && !/^\|/{exit} on' "$doc" > "$d/doc.md"
  [ -s "$d/doc.md" ] || { echo "moon-parity: no parity table found in $doc"; exit 1; }
  if diff -u "$d/doc.md" "$d/measured.md" > "$d/drift.txt"; then
    echo "moon-parity: $doc matches the compiler on $n lanes × 6 targets."
  else
    echo "moon-parity: DRIFT -- $doc disagrees with the compiler."
    echo "             -doc  +measured; regenerate with 'mk/tools/moon-parity.sh table'."
    sed '1,2d' "$d/drift.txt"
    exit 1
  fi
  ;;

*) echo "usage: moon-parity.sh [table | check | why]"; exit 2 ;;
esac
