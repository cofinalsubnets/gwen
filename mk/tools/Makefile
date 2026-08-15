# The gates for the tools that ride beside the build rather than in it. Three shapes: a
# GENERATED file diffed against a fresh regeneration (vmret against its frozen python
# reference under py/), a generator's output weighed for shape rather than byte-compared
# (hue, whose file the build owns), and a library's own law file (cook, tele). Each checks
# only what is already built.
#
# `make test_tools` from the repo root, or `make -C tools` here. Needs python3 for the
# vmret reference, and objdump/llvm-objdump for vmret itself.

R := ../..
include $(R)/mk/common.mk

# the WARM love (test/test.mk's $(mw), same bargain): the baked image woken rather than the
# egg compiled from source. The root exports LOVE_NO_IMAGE so the EGG gates boot cold;
# these tools are not the egg, and test/host/cook.l spawns a subprocess per fixture that would
# inherit it.
mw = env -u LOVE_NO_IMAGE $(m)
# the same warm love spelled from the REPO ROOT, for the recipes that `cd $(R)` first
# (where $(m)'s ../ prefix would point one level above the root).
mwr = env -u LOVE_NO_IMAGE out/host$(hsuf)/love

.PHONY: all test_vmret test_cook test_tele test_xor test_hue
all: test_vmret test_cook test_tele test_xor test_hue

# Build the host love on demand (it runs the .l tools); only if it's missing.
$(m):
	@$(MAKE) -C $(R)/host

# vmret.l vs py/vmret.py over the host binary plus any kernel ELFs present (each arch
# exercises a different ret matcher). ⚠ love0 is skipped: it builds -Dai_tco=0, the
# trampoline, where handler rets are expected and the vmret signal is noise.
test_vmret: | $(m)
	@echo TEST vmret.l
	@for e in $(R)/out/host/love `find $(R)/out/free -name 'love-*.elf' 2>/dev/null`; do \
	  python3 $(R)/tools/py/vmret.py $$e > $(R)/out/host/.vmret.py 2>&1; \
	  $(mw) $(R)/tools/vmret.l $$e > $(R)/out/host/.vmret.l 2>&1; \
	  cmp -s $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l && echo "  ok   $$e" \
	    || { echo "  FAIL $$e"; diff $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l; rm -f $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l; exit 1; }; \
	done; rm -f $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l


# the syntax file is BUILT (crew/build.mk) out of crew/vi/hue.l's class table and the
# vocabulary the host answers to, so there is no checked-in second copy to drift from and
# nothing left to diff -- a rename that moves (names ()) now just rebuilds it. What is
# still worth asking is whether the generator ran to the END and meant what it emitted.
# ⚠ THE EXIT CODE CANNOT ANSWER THAT. the cat rides STDIN, where a scare prints `;; a b`
# and CARRIES ON at 0, so a condition can land in the middle of the file and still leave a
# green make. The terminal b:current_syntax is the summary line here -- hue2vim.l writes
# it last -- and the two shape checks below are what a diff used to say the long way.
test_hue: | $(m)
	@echo TEST hue2vim.l
	@$(MAKE) --no-print-directory -C $(R) out/host$(hsuf)/syntax.vim
	@f=$(R)/out/host$(hsuf)/syntax.vim; \
	  grep -q '^let b:current_syntax = "love"$$' $$f \
	    || { echo "  FAIL $$f: no terminal b:current_syntax -- the generator stopped short"; exit 1; }; \
	  grep -n '^;;' $$f && { echo "  FAIL $$f: a scare printed into the syntax file"; exit 1; }; \
	  grep -q '^syn keyword LoveBook ' $$f \
	    || { echo "  FAIL $$f: no LoveBook keywords -- the vocabulary probe answered nothing"; exit 1; }; \
	  for g in `grep -oE '^syn (keyword|match|region) Love[A-Za-z0-9]+' $$f \
	              | awk '{print $$3}' | grep -v '^LoveList' | sort -u`; do \
	    grep -q "^hi def link $$g " $$f \
	      || { echo "  FAIL $$f: $$g is painted but never linked to a highlight group"; exit 1; }; \
	  done; \
	  echo "  ok   $$f"

# cook's Makefile-importer suite: test/host/cook.l writes fixture Makefiles to a temp dir and
# runs cook on them as a subprocess, asserting over the captured output. Paths are relative
# to the repo root, so it runs from there.
# ⚠ the SHELL pair needs the BUILT lush (a fixture sets `SHELL := out/host/lush` to prove
# cook honors it), and that rule lives in the ROOT makefile -- so the prerequisite sits on
# test_tools, not here. ⚠ and the built love goes on PATH FIRST: lush is a catted script
# under `#!/usr/bin/env -S love`, so PATH picks the love, and without this the gate runs
# whatever love is INSTALLED. a one-entry .bin/, NOT out/host itself -- that holds
# `sh -> kore`, and shadowing /bin/sh costs the two .SHELLFLAGS tests.
# Through run.sh for the summary line: the exit code alone passes a reader that stopped early.
test_cook: | $(m)
	@echo TEST test/host/cook.l
	@mkdir -p $(R)/out/host$(hsuf)/.bin && ln -sf ../love $(R)/out/host$(hsuf)/.bin/love
	@cd $(R) && PATH="$(abspath $(R)/out/host$(hsuf)/.bin):$$PATH" \
	  sh test/gate/run.sh -a cook "$(mwr)" "cook tests pass" test/host/cook.l

# tele, the autograd library (pure love over the constellation algebra): test/tele/grad.l
# grad-checks every op, analytic gradient vs central finite difference, so the oracle is
# self-validating. tele loads as a user `-l` and is NOT in the corpus -- this is its gate.
# Through run.sh for the summary line: the exit code alone passes a reader that stopped early.
test_tele: | $(m)
	@echo TEST test/tele/grad.l
	@cd $(R) && sh test/gate/run.sh -a tele "$(mwr) -l crew/tele/tele.l" "tele tests pass" test/tele/grad.l

# the integration gate over that same library: a 2-8-1 MLP learns XOR, seeded so it converges
# identically every run -- autograd + nn + optim composed into a real net. test/tele/grad.l
# grad-checks the ops one at a time; this asks whether they compose. Corpus-shaped -- 00-init
# for `assert`, zz-fin for the count -- but tele is no module, so it cannot ride the test/*.l
# glob and gets this lane instead. Same two checks as its neighbours -- exit 0 AND the summary
# line, since a reader that stops early leaves the asserts unrun and exits 0 -- but inline
# rather than through run.sh, because the subject is three files catted, not one handed over.
test_xor: | $(m)
	@echo TEST test/tele/xor.l
	@cd $(R) && cat test/00-init.l test/tele/xor.l test/zz-fin.l > out/host/.test_xor.l; \
	  $(mwr) -l crew/tele/tele.l out/host/.test_xor.l </dev/null > out/host/.test_xor.out 2>&1; \
	  r=$$?; cat out/host/.test_xor.out; \
	  [ $$r -eq 0 ] || { echo "FAIL test_xor (exit $$r)"; exit 1; }; \
	  grep -q "tests pass" out/host/.test_xor.out || { echo "FAIL test_xor (exit 0, no summary)"; exit 1; }
