#!/bin/sh
# test/gate/cli.sh -- the CLI's EXIT STATUS, which nothing else pinned.
#
# love/cli.l's every failure lane ends in a status: 1 for a file it cannot open, 2
# for a malformed flag or a torn -e, 0 for -v/-h, and a verb's own charm for the
# verb rail. Not one of them was gated -- test_seat drives the file seat and reads
# what it SAYS, the corpus never runs the flags at all, and every other gate calls
# the binary in a way that would not notice a status going wrong. So the six exits
# could have been renumbered, or silently turned into 0, under a green test_slow.
#
# That matters more than it looks: `love -e ... || die` in a script, and cook's and
# make's exit-status contract, both read this and nothing else. The lane got rebuilt
# when cli.l moved off `quit` onto a scare + one trap (kore's uleave/urun shape, so
# a caller in the same image can catch a leave instead of dying), and the whole
# point of that change is that the numbers do NOT move.
#
# usage: sh test/gate/cli.sh LOVE
set -u
love=${1:-out/host/love}
d=out/host/.cli && mkdir -p $d
fails=0
torn=$d/torn.l; printf '(: x (foo\n' > $torn

# want-status, want-substring (empty = must say NOTHING), then the argv.
try() {
  wst=$1; wsay=$2; shift 2
  out=$("$love" "$@" </dev/null 2>&1); st=$?
  [ "$st" = "$wst" ] || {
    echo "FAIL cli: 'love $*' exit $st, wanted $wst"; fails=$((fails+1)); }
  case $wsay in
    "") [ -z "$out" ] || { echo "FAIL cli: 'love $*' should be SILENT, said: $(echo "$out" | head -1)"
                           fails=$((fails+1)); } ;;
    *)  case $out in
          *"$wsay"*) ;;
          *) echo "FAIL cli: 'love $*' missing '$wsay', said: $(echo "$out" | head -1)"
             fails=$((fails+1)) ;;
        esac ;;
  esac
}

# the lanes that WORK answer 0
try 0 3        -e '1 + 2'
try 0 262144   -e '2 3 4'                 # the tower: many datums fold to one application
try 0 ''       -n -e '1 + 2'              # -n mutes the print and keeps the 0
try 0 usage    -h
try 0 'love '  -v
try 0 1        -m 8m -e 1

# ..and every failure lane answers ITS OWN number, which is the whole gate
try 2 'not a size'        -m zz -e 1
try 2 'needs an argument' -e
try 2 'needs an argument' -l
try 2 'needs an argument' -m
try 2 'unfinished form'   -e '(1 +'
try 1 'cannot open'       /nonexistent.l
try 1 'cannot open'       -l /nonexistent.l -e 1

# the VERB rail answers a charm and the rail leaves with it -- the one lane whose
# status is a value rather than a literal. ⚠ NOT `bake`/`wake`: those are the PRIME
# verbs, taken by main.c before this file is ever evaluated (its run_program), so
# their status says nothing about the rail here. `verbs` is one that does reach it.
try 0 bake verbs                          # the rail's own listing, off the tab

[ $fails -eq 0 ] || { echo "FAIL cli ($fails)"; exit 1; }
echo "cli: every exit lane keeps its status -- 0 working, 1 unopenable, 2 malformed, and the verb's own"
