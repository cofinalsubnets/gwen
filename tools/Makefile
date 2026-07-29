# Validation gates for the love rewrites of the old Python build tools.
#
# Each love tool in this directory (gen_data.l, vmret.l) must
# produce output identical to the frozen Python reference under py/ (see
# py/README.md). These targets diff the two over every relevant build artifact
# present in the tree -- so they only check what you've actually built. Run all
# of them with `make test_tools` from the repo root, or `make -C tools` here.
#
# Needs: python3 (the references), and objdump/llvm-objdump for vmret.

R := ..
include $(R)/common.mk

.PHONY: all test_vmret test_cook test_tele test_xor test_hue
all: test_vmret test_cook test_tele test_xor test_hue

# Build the host love on demand (it runs the .l tools); only if it's missing.
$(m):
	@$(MAKE) -C $(R)/host

# vmret.l vs py/vmret.py over the host binary plus any kernel arch ELFs
# present (each arch exercises a different ret matcher + the objdump choice).
# love0 is skipped: it builds -Dai_tco=0 (trampoline), where handler rets are
# expected -- the vmret signal is noise there.
test_vmret: | $(m)
	@echo TEST vmret.l
	@for e in $(R)/out/host/love `find $(R)/out/free -name 'love-*.elf' 2>/dev/null`; do \
	  python3 $(R)/tools/py/vmret.py $$e > $(R)/out/host/.vmret.py 2>&1; \
	  $(m) $(R)/tools/vmret.l $$e > $(R)/out/host/.vmret.l 2>&1; \
	  cmp -s $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l && echo "  ok   $$e" \
	    || { echo "  FAIL $$e"; diff $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l; rm -f $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l; exit 1; }; \
	done; rm -f $(R)/out/host/.vmret.py $(R)/out/host/.vmret.l


# tools/hue2vim.l: vim/syntax.vim is GENERATED from crew/vi/hue.l's class table
# and the vocabulary the host answers to, so regenerate it and diff -- the same
# bargain test_vmret strikes. this is the gate that keeps the editor in the tree
# and the editor outside it reading one .l: a rename that moves (names ()), or a
# new row in the class table, fails here until the file is re-run.
#
# it diffs against THE HOST YOU BUILT. a differently-configured build answers a
# different (names ()) and will ask you to regenerate -- which is right: your
# syntax file should describe your love.
#
# ⚠ LOVE_NO_IMAGE is CLEARED. the root Makefile exports it, and under it love
# compiles the corpus at boot instead of waking the baked image -- so the egg's
# mop never runs and the compiler's internals (`book` among them) are still on
# the book. the syntax file describes the SHIPPED language, so the generator
# gets the shipped boot. hue2vim.l refuses an unmopped book on its own too.
test_hue: | $(m)
	@echo TEST hue2vim.l
	@cd $(R) && cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l \
	  | env -u LOVE_NO_IMAGE out/host/love > out/host/.syntax.vim 2>&1
	@cmp -s $(R)/out/host/.syntax.vim $(R)/vim/syntax.vim \
	  || { echo "  FAIL vim/syntax.vim is stale -- regenerate:"; \
	       echo "    cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l | out/host/love > vim/syntax.vim"; \
	       diff $(R)/vim/syntax.vim $(R)/out/host/.syntax.vim; \
	       rm -f $(R)/out/host/.syntax.vim; exit 1; }
	@rm -f $(R)/out/host/.syntax.vim; echo "  ok   vim/syntax.vim"

# crew/cook/cook.l: run its Makefile-importer test suite. cooktest.l writes fixture
# Makefiles to a temp dir and runs cook on them as a subprocess, asserting over
# the captured output (variable expansion, the make functions, conditionals,
# include, pattern + static-pattern rules). It uses paths relative to the repo
# root, so run it from there.
test_cook: | $(m)
	@echo TEST cook.l
	@cd $(R) && out/host/love crew/cook/cooktest.l

# crew/tele/tele.l: the PyTorch-style autograd library (pure love over the constellation
# algebra). teletest.l grad-checks every op -- the analytic gradient (from `back`)
# vs the central finite difference -- so the oracle is self-validating. tele loads
# as a user `-l` (NOT in the corpus), so `make test` is untouched; this is its gate.
test_tele: | $(m)
	@echo TEST tele.l
	@cd $(R) && out/host/love -l crew/tele/tele.l crew/tele/teletest.l

# crew/tele/xor.l: the flagship integration demo -- a 2-8-1 MLP learns XOR (the
# not-linearly-separable problem a single linear layer can't solve). seeded, so it
# converges identically every run; exits nonzero if the four outputs don't round
# right. proves the whole stack (autograd + nn + optim) trains a real net.
test_xor: | $(m)
	@echo TEST xor.l
	@cd $(R) && out/host/love -l crew/tele/tele.l crew/tele/xor.l
