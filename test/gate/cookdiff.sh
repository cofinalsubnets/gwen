#!/bin/sh
# test/gate/cookdiff.sh -- cook against GNU make, differentially. The oracle is a
# SECOND IMPLEMENTATION, not our own expectations: each case is a tiny Makefile run
# by both, and the outputs must agree byte for byte.
#
# ⚠ THE FAILURE THIS EXISTS FOR IS SILENCE. An unknown `$(name args)` is a VARIABLE
# reference in make's grammar, not an error -- so a builtin cook has never implemented
# expands to nothing and the build carries on with an empty value. That is how
# `$(error ..)` guards were no-ops here, and it is invisible to any test that only
# asks whether cook agrees with itself. Six functions and two real bugs came out of
# the first sweep of this file.
#
# usage: cookdiff.sh LOVE     (LOVE = a binary whose `kore make` verb is cook)
set -u
K=${1:?usage: cookdiff.sh LOVE}
K=$(cd "$(dirname "$K")" && pwd)/$(basename "$K")

command -v make >/dev/null 2>&1 || { echo "cookdiff: no ambient make -- skipped"; exit 0; }
case "$(make --version 2>/dev/null | head -1)" in
  *"GNU Make"*) ;;
  *) echo "cookdiff: ambient make is not GNU make -- skipped"; exit 0 ;;
esac

# ⚠ THE ORACLE RUNS TOP-LEVEL. under `make -jN` the outer make exports MAKEFLAGS with
# its jobserver, and the make below would inherit it -- a parallel oracle reorders any
# case whose target has two independent prerequisites, so `all: a | b` prints [b] before
# [a] about 1 run in 12 and the gate reports COOK as failing when the oracle moved. cook
# is serial, so only one side of the differential was drifting. a differential is worth
# exactly what its oracle's determinism is worth.
unset MAKEFLAGS MFLAGS GNUMAKEFLAGS MAKELEVEL

work=$(mktemp -d) || exit 1
trap 'rm -rf "$work"' EXIT
fail=0; ran=0; known=0

# case NAME KIND  <<'E' ... E     KIND: same = must agree | known = a recorded difference
# ⚠ EACH CASE RUNS EXACTLY ONCE PER MAKE. A case may have side effects on purpose
# (shell_once counts them), so re-running one to print its diff would report a
# different world than the one that failed. Capture both outputs, then decide.
case_() {
  nm=$1; kind=$2
  d=$work/$nm; mkdir -p "$d"; cat > "$d/Makefile"
  g=$(cd "$d" && make -s 2>&1)
  c=$(cd "$d" && LOVE_NO_IMAGE= "$K" kore make 2>&1 | grep -v 'is already up to date')
  ran=$((ran + 1))
  [ "$g" = "$c" ] && return 0
  if [ "$kind" = known ]; then
    known=$((known + 1)); echo "cookdiff: KNOWN  $nm"; return 0
  fi
  echo "cookdiff: FAIL   $nm"
  echo "  gnu : $(printf '%s' "$g" | tr '\n' '|')"
  echo "  cook: $(printf '%s' "$c" | tr '\n' '|')"
  fail=$((fail + 1))
}

# ---- functions -------------------------------------------------------------
case_ subst same      <<'E'
all:;@echo "$(subst ee,EE,feet street)"
E
case_ patsubst same   <<'E'
all:;@echo "$(patsubst %.c,%.o,a.c b.c c.h)"
E
case_ strip same      <<'E'
all:;@echo "[$(strip   a   b   )]"
E
case_ findstring same <<'E'
all:;@echo "[$(findstring a,b a c)][$(findstring z,b a c)]"
E
case_ filter same     <<'E'
all:;@echo "$(filter %.c %.h,a.c b.o c.h) / $(filter-out %.o,a.c b.o)"
E
case_ sort same       <<'E'
all:;@echo "$(sort c b a b)"
E
case_ words same      <<'E'
all:;@echo "[$(word 2,a b c)][$(words a b c)][$(wordlist 2,3,a b c d)]"
E
case_ firstlast same  <<'E'
all:;@echo "[$(firstword a b)][$(lastword a b)]"
E
case_ paths same      <<'E'
all:;@echo "[$(dir a/b c)][$(notdir a/b c)][$(basename a.c)][$(suffix a.c)]"
E
case_ addfix same     <<'E'
all:;@echo "[$(addprefix p-,a b)][$(addsuffix -s,a b)]"
E
case_ join same       <<'E'
all:;@echo "[$(join a b,1 2)][$(join a b c,1)]"
E
case_ if same         <<'E'
all:;@echo "[$(if ,yes,no)][$(if x,yes,no)]"
E
case_ orand same      <<'E'
all:;@echo "[$(or ,,x)][$(and x,y)][$(and x,,y)]"
E
case_ foreach same    <<'E'
all:;@echo "[$(foreach v,a b c,<$(v)>)]"
E
case_ foreach_scope same <<'E'
v = outer
all:;@echo "[$(foreach v,a b,$(v))][$(v)]"
E
case_ call same       <<'E'
rev = $(2) $(1)
all:;@echo "[$(call rev,a,b)]"
E
case_ flavor same     <<'E'
A = 1
B := 2
all:;@echo "[$(flavor A)][$(flavor B)][$(flavor NOPE)]"
E
case_ origin same     <<'E'
A = 1
all:;@echo "[$(origin A)][$(origin NOPE)]"
E
case_ shell same      <<'E'
all:;@echo "[$(shell echo hi)]"
E
case_ info same       <<'E'
all:;@echo "[$(info side)done]"
E
case_ nested_var same <<'E'
A = B
B = deep
all:;@echo "[$($(A))]"
E

# ---- assignment and expansion ----------------------------------------------
case_ flavors same    <<'E'
A = 1
A += 2
B := x
B += y
C ?= d
all:;@echo "[$(A)][$(B)][$(C)]"
E
case_ recursive same  <<'E'
X = $(Y)
Y = late
all:;@echo "[$(X)]"
E
case_ simple same     <<'E'
Y = early
X := $(Y)
Y = late
all:;@echo "[$(X)]"
E
case_ substref same   <<'E'
S = a.c b.c
all:;@echo "[$(S:.c=.o)][$(S:%.c=%.x)]"
E

# ---- rules and recipes -----------------------------------------------------
case_ autovars same   <<'E'
all: dep1 dep2
	@echo "[$@][$<][$^]"
dep1 dep2:;@:
E
case_ patrule same    <<'E'
%.o: %.c ; @echo "[$@ from $< stem=$*]"
all: a.o
a.c: ;@touch a.c
E
# ⚠ A PATTERN WHOSE PREREQUISITE CANNOT BE MADE DOES NOT APPLY. make rejects the
# rule and tries the next; with none left, a target that exists on disk is a source
# leaf and is done. cook took the unmatchable rule as a LAST RESORT instead and
# demanded a file nobody makes -- and since mk/common.mk cancels make's lex rule
# (`%.c: %.l`, this tree being full of `<name>.l` beside `<name>.c`), EVERY .c in
# the tree matched: `make -f mk/cook.mk all` died on "no recipe to make core/love.l".
# Cook's whole job is this tree's own Makefile, and none of the cases above noticed.
case_ patrule_reject same <<'E'
$(shell printf 'int x;\n' > src.c)
%.c: %.l ; @echo "LEXED $<"
all: src.c
	@echo "made $<"
E
# ..and the other side of it: a prerequisite that CAN be made still applies
case_ patrule_chain same <<'E'
%.c: %.l ; @echo "[lex $< -> $@]"
gen.l: ;@touch gen.l
all: gen.c
E
case_ inline_semi same <<'E'
all:;@echo one ; echo two
E
case_ orderonly same  <<'E'
all: a | b
	@echo "[built]"
a:;@echo "[a]"
b:;@echo "[b]"
E
case_ dollardollar same <<'E'
all:;@echo "[$$(echo nested)]" ; echo "[$${HOME:+set}]"
E
case_ conditionals same <<'E'
V = x
ifeq ($(V),x)
R = eq
else
R = ne
endif
ifneq (a,b)
Q = differ
endif
ifdef V
D = def
endif
ifndef NOPE
N = ndef
endif
all:;@echo "[$(R)][$(Q)][$(D)][$(N)]"
E
case_ defaultgoal same <<'E'
first:;@echo "[first is default]"
second:;@echo "[second]"
E
case_ export same     <<'E'
export FOO = bar
all:;@echo "[$$FOO]"
E
case_ phony same      <<'E'
.PHONY: all
all:;@echo "[phony ok]"
E

# ⚠ SIDE EFFECTS ARE THE POINT OF THIS ONE. A recipe line expanded twice runs its
# $(shell ..) twice, and only the second value is ever used -- so the duplicate work
# and its side effects are invisible in the output. Count the runs instead.
case_ shell_once same <<'E'
all:
	@echo "[$(shell echo R >> ./n.txt; wc -l < ./n.txt)]" ; rm -f ./n.txt
E

# ---- recorded differences, reported and not failed -------------------------
# ⚠ each of these is a KNOWN divergence with a reason, not a shrug. Promote one to
# `same` the moment it is fixed; never add a row here to make the gate quiet.
#
# continuation: make folds \<newline> AND the next line's indent into one space in a
# variable, but a RECIPE's backslash-newline is passed to the shell with the indent
# intact. cook's uncont runs over the whole file before recipe lines are told apart,
# so the two wants collide -- fixing the variable side alone would regress recipes.
case_ continuation known <<'E'
A = one \
    two
all:;@echo "[$(A)]"
E
# comment: make keeps the space before a trailing `#`, cook trims it. Cosmetic in
# every consumer that splits on whitespace, which is nearly all of them.
case_ comment known <<'E'
A = 1 # trailing
all:;@echo "[$(A)]"
E
# ignored failure: under -s make says nothing about a `-cmd` that failed; cook echoes
# the command and names the ignored status. Cook's is the more useful report, and
# saying less would be the regression.
case_ ignored_fail known <<'E'
all:
	@echo "[at]"
	-false
	@echo "[after]"
E

echo "cookdiff: $ran cases, $fail failed, $known recorded differences"
[ $fail -eq 0 ] || exit 1
exit 0
