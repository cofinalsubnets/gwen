#!/bin/sh
# test/gate/refuzz.sh -- kore's grep and sed against GNU's, over SEEDED RANDOM
# patterns and subjects, byte-for-byte on stdout and on the exit code.
#
# WHY THIS EXISTS BESIDE test_kore's HAND BATTERY. the hand battery is a list
# someone thought of, so it passes exactly the cases its author already had in
# mind. every bug this engine actually shipped -- the ERE loose-repeat drop, -F
# walking a string with `two?` (false for one, so the pattern came out empty and
# matched everything), the zero-length -o match printing a blank line -- was a
# shape nobody would have written down. a random draw does not know what we meant
# either, which is exactly its value: it asks GNU instead of asking us.
#
# ⚠ THE ORACLE MUST BE THE REAL GNU. an interactive `grep` on a dev box may be a
# ugrep shim whose BRE differs (it ERRORS where GNU takes a loose repeat as ink),
# so the oracle is an ABSOLUTE PATH, its --version is checked, and the lane SKIPS
# loudly where GNU is absent. a differential with no independent oracle is not a
# weaker test, it is not a test.
#
# ⚠ GNU grep AND GNU sed DO NOT SHARE AN ERE DIALECT, which is why the atom sets
# below are per-tool. `grep -E ')'` matches a literal paren; `sed -E 's/)/X/'`
# quits with "Unmatched ) or \)". same for an unclosed `{`. our engine follows
# GREP there (crew/kore/re.l's `loose`), so the sed lane simply does not draw the
# two shapes -- rather than pretend one answer is right for both tools.
#
# ⚠ SEEDED, NOT RANDOM: the draw is a pure function of $seed, so a red run
# reproduces exactly and a green one means the same thing tomorrow. raise
# REFUZZ_ROUNDS by hand when hunting; never make either time-dependent.
#
# usage: refuzz.sh LOVE KOREIMAGE
set -u

m=$1
img=$2
seed=${REFUZZ_SEED:-20260815}
rounds=${REFUZZ_ROUNDS:-150}

GREP=/usr/bin/grep
SED=/usr/bin/sed
[ -x "$GREP" ] || GREP=$(command -v grep 2>/dev/null || true)
[ -x "$SED" ]  || SED=$(command -v sed  2>/dev/null || true)
if [ ! -x "${GREP:-}" ] || [ ! -x "${SED:-}" ]; then
  echo "refuzz: no grep/sed on this box -- SKIPPED (no oracle to ask)"; exit 0
fi
case $("$GREP" --version 2>&1 | head -1) in *GNU*) ;;
  *) echo "refuzz: $GREP is not GNU grep -- SKIPPED (wrong oracle)"; exit 0 ;; esac
case $("$SED" --version 2>&1 | head -1) in *GNU*) ;;
  *) echo "refuzz: $SED is not GNU sed -- SKIPPED (wrong oracle)"; exit 0 ;; esac

w=${TMPDIR:-/tmp}/refuzz.$$
mkdir -p "$w" || exit 1
trap 'rm -rf "$w"' EXIT INT TERM
korerun() { "$m" wake "$img" kore "$@"; }

# ⚠ THE DRAW IS MADE IN awk, ONCE, not in the shell: `$(rnd)` runs in a SUBSHELL,
# so an LCG kept in a shell variable never advances in the parent and every round
# draws the same pattern -- a fuzz that runs 150 rounds of one case and says ok.
# fields: BRE \t ERE(grep) \t ERE(sed) \t the six subject lines joined by | .
# `|` is not in the subject alphabet, so the join is unambiguous.
awk -v seed="$seed" -v n="$rounds" '
BEGIN {
  srand(seed)
  # ⚠ THE SEPARATOR IS `;` AND MAY NOT APPEAR IN AN ATOM. it was `|` here once,
  # and BRE atoms carry `\|` -- so \(a\|b\) split into \(a\ and b\), the draw
  # emitted shapes no dialect has, and the lane spent its rounds on nonsense.
  nb = split("a;b;c;.;a*;.*;[ab];[^a];[a-c];b\\+;c\\?;\\(ab\\)*;\\(a\\|b\\);a\\|b;" \
             "b\\{2\\};a\\{1,2\\};[[:alpha:]];[[:digit:]];^;$;\\{2\\};\\+;*", B, ";")
  # the grep-ERE set carries the ragged shapes: ) and { as ink, a loose repeat dropped
  # NOTE: no apostrophes anywhere in this awk block -- it is single-quoted shell.
  # ⚠ AT MOST ONE bare repeat, and the draw puts it FIRST (see mkere below): GNU
  # stacks them into shapes POSIX calls undefined (a leading star-then-brace
  # matches only the empty text, for reasons no spec licenses) and
  # bug-compatibility with that is not a goal. one loose repeat IS drawn, because
  # "GNU drops it" is the documented edge our own parser leans on
  ng = split("a;b;c;.;a*;.*;[ab];[^a];[a-c];b+;c?;(ab)*;(a|b);a|b;b{2};a{1,2};" \
             "[[:alpha:]];[[:digit:]];^;$;);{", G, ";")
  # ⚠ `*` and `+` only. a LEADING `{2}` is left out because GNU is not
  # self-consistent about it: `-E {2}` alone behaves as the empty pattern (every
  # line), but `-E {2}a` does NOT behave as `a`. POSIX calls the shape undefined,
  # nothing in this tree writes it, and we drop the whole interval (see re.l).
  # a divergence there is one with a GNU corner, not a bug of ours
  nl = split("+;*", L, ";")
  # the sed-ERE set drops ) { and the BARE repeats {2} + * -- GNU sed REJECTS all
  # five where GNU grep takes them (a loose repeat is ink to grep, "nothing to
  # repeat" to sed). the ATTACHED repeats (a*, b+, a{1,2}) stay: both agree there
  ns = split("a;b;c;.;a*;.*;[ab];[^a];[a-c];b+;c?;(ab)*;(a|b);a|b;b{2};a{1,2};" \
             "[[:alpha:]];[[:digit:]];^;$", S, ";")
  nc = split("a b c 1 _ . * ", C, " "); C[nc] = " "
  for (r = 0; r < n; r++) {
    printf "%s\t%s\t%s\t", pat(B, nb), mkere(), pat(S, ns)
    for (i = 0; i < 6; i++) printf "%s%s", (i ? "|" : ""), subj()
    printf "\n"
  }
}
function mkere(   o) { o = pat(G, ng)
  return (rand() < 0.2) ? L[1 + int(rand() * nl)] o : o }   # one loose repeat, up front
function pat(A, k,   j, m, o) { m = 1 + int(rand() * 4); o = ""
  for (j = 0; j < m; j++) o = o A[1 + int(rand() * k)]; return o }
function subj(   j, m, o) { m = int(rand() * 7); o = ""
  for (j = 0; j < m; j++) o = o C[1 + int(rand() * nc)]; return o }
' > "$w/draws"

bad=0
r=0
cmp1() { # LABEL TOOL argv..  -- oracle then ours; stdout AND exit must agree
  nm=$1; shift; tool=$1; shift
  case $tool in grep) or=$GREP ;; sed) or=$SED ;; esac
  "$or" "$@" "$w/in" > "$w/g" 2>/dev/null; a=$?
  korerun "$tool" "$@" "$w/in" > "$w/o" 2>/dev/null; b=$?
  if ! cmp -s "$w/g" "$w/o" || [ "$a" -ne "$b" ]; then
    bad=$((bad + 1))
    echo "REFUZZ DIFF ($nm) seed=$seed round=$r: $tool $* (gnu $a ours $b)" >&2
    echo "-- subject:" >&2; cat -A "$w/in" >&2
    diff "$w/g" "$w/o" >&2 | head -6
  fi
}

while IFS='	' read -r bp gp sp subj; do
  r=$((r + 1))
  printf '%s\n' "$subj" | tr '|' '\n' > "$w/in"
  cmp1 bre grep -c  "$bp";  cmp1 bre grep     "$bp";  cmp1 bre grep -n "$bp"
  cmp1 bre grep -v  "$bp";  cmp1 bre grep -o  "$bp";  cmp1 bre grep -w "$bp"
  cmp1 bre grep -x  "$bp";  cmp1 bre grep -i  "$bp"
  cmp1 ere grep -Ec "$gp";  cmp1 ere grep -E  "$gp";  cmp1 ere grep -oE "$gp"
  cmp1 ere grep -nE "$gp";  cmp1 ere grep -vE "$gp"
  cmp1 sub sed "s/$bp/</";  cmp1 sub sed "s/$bp/</g";  cmp1 sub sed "s/$bp/[&]/g"
  cmp1 adr sed -n "/$bp/p"; cmp1 sub sed -E "s/$sp/</g"; cmp1 adr sed -nE "/$sp/p"
done < "$w/draws"

if [ "$bad" -gt 0 ]; then
  echo "refuzz: $bad divergence(s) over $r rounds (seed $seed) -- FAILED" >&2
  exit 1
fi
echo "refuzz: grep + sed vs GNU over $r seeded rounds (seed $seed), byte-identical ok"
