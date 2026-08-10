# The gates for the tools that ride beside the build rather than in it. Two shapes: a
# GENERATED file diffed against a fresh regeneration (vmret against its frozen python
# reference under py/, syntax.vim against crew/vi/hue.l), and a library's own law file
# (cook, tele). Each checks only what is already built.
#
# `make test_tools` from the repo root, or `make -C tools` here. Needs python3 for the
# vmret reference, and objdump/llvm-objdump for vmret itself.

R := ..
include $(R)/common.mk

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


# vim/syntax.vim is GENERATED from crew/vi/hue.l's class table and the vocabulary the host
# answers to, so this regenerates and diffs: a rename that moves (names ()), or a new row
# in the class table, reddens here until the file is re-run. It diffs against THE HOST YOU
# BUILT -- a differently-configured build answers a different (names ()) and will ask you
# to regenerate, which is right: your syntax file should describe your love.
# ⚠ LOVE_NO_IMAGE is CLEARED here. Under it the egg's mop never runs and the compiler's
# internals (`book` among them) are still on the book; the syntax file describes the
# SHIPPED language, so the generator gets the shipped boot.
test_hue: | $(m)
	@echo TEST hue2vim.l
	@cd $(R) && cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l \
	  | $(mwr) > out/host/.syntax.vim 2>&1
	@cmp -s $(R)/out/host/.syntax.vim $(R)/vim/syntax.vim \
	  || { echo "  FAIL vim/syntax.vim is stale -- regenerate:"; \
	       echo "    cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l | out/host/love > vim/syntax.vim"; \
	       diff $(R)/vim/syntax.vim $(R)/out/host/.syntax.vim; \
	       rm -f $(R)/out/host/.syntax.vim; exit 1; }
	@rm -f $(R)/out/host/.syntax.vim; echo "  ok   vim/syntax.vim"

# cook's Makefile-importer suite: test/host/cook.l writes fixture Makefiles to a temp dir and
# runs cook on them as a subprocess, asserting over the captured output. Paths are relative
# to the repo root, so it runs from there.
# ⚠ the SHELL pair needs the BUILT lush (a fixture sets `SHELL := out/host/lush` to prove
# cook honors it), and that rule lives in the ROOT makefile -- so the prerequisite sits on
# test_tools, not here. ⚠ and the built love goes on PATH FIRST: lush is a catted script
# under `#!/usr/bin/env -S love`, so PATH picks the love, and without this the gate runs
# whatever love is INSTALLED. a one-entry .bin/, NOT out/host itself -- that holds
# `sh -> kore`, and shadowing /bin/sh costs the two .SHELLFLAGS tests.
test_cook: | $(m)
	@echo TEST cook.l
	@mkdir -p $(R)/out/host$(hsuf)/.bin && ln -sf ../love $(R)/out/host$(hsuf)/.bin/love
	@cd $(R) && PATH="$(abspath $(R)/out/host$(hsuf)/.bin):$$PATH" $(mwr) test/host/cook.l

# tele, the autograd library (pure love over the constellation algebra): test/host/tele.l
# grad-checks every op, analytic gradient vs central finite difference, so the oracle is
# self-validating. tele loads as a user `-l` and is NOT in the corpus -- this is its gate.
test_tele: | $(m)
	@echo TEST tele.l
	@cd $(R) && $(mwr) -l crew/tele/tele.l test/host/tele.l

# the flagship integration demo: a 2-8-1 MLP learns XOR, seeded so it converges identically
# every run, nonzero if the four outputs do not round right -- autograd + nn + optim on a
# real net.
test_xor: | $(m)
	@echo TEST xor.l
	@cd $(R) && $(mwr) -l crew/tele/tele.l crew/tele/xor.l
