# cook.mk -- a make-shaped front door to cook (crew/cook/cook.l + crew/cook/Cookfile).
#
# `make` is muscle memory; cook is the build tool written in love. This STUB
# bridges them: `make -f cook.mk <goal>` makes sure the love binary exists, then
# hands the goal to cook, which reads Cookfile. The real Makefile is left untouched
# and stays the source of truth for the irreducible C bootstrap -- cook RUNS on
# love, so it cannot build love; that one rung is delegated to make, and cook
# ports everything above it (see Cookfile).
#
# usage:  make -f cook.mk            # the default goal (all = the host build)
#         make -f cook.mk test
#         make -f cook.mk clean
#
# anything cook can't drive -- qemu, a tty repl, the wasm/kernel sub-makes, perf
# (cook's `run` captures stdout and waits, so no streaming and no tty) -- is passed
# straight to the real Makefile below.

LOVE := out/host/love
COOK := $(LOVE) -l crew/cook/cook.l crew/cook/Cookfile

.DEFAULT_GOAL := all

# The bootstrap rung cook can't climb. No prerequisites: this fires ONLY when the
# binary is absent (a fresh checkout), handing the C build to the real Makefile.
# Source-change rebuilds are handled by Cookfile's 'host card (a `make host` that
# no-ops when current), so this is not a staleness gate, just an existence one.
$(LOVE):
	@$(MAKE) host

# The verbs cook owns: it shells out to the toolchain itself. Ensure the binary
# exists first, then let cook (via Cookfile) take over.
COOKED := all test clean install bench vmret valg
.PHONY: $(COOKED)
$(COOKED): $(LOVE)
	@$(COOK) $@

# The verbs cook can't drive (interactive / streaming / sub-make): pass through to
# the real Makefile verbatim.
PASSED := host love0 kernel wasm lib hooks uninstall \
          run run-hdd run-headless repl gdb disasm perf flame cloc \
          test_slow test_host test_love0 test_tools test_kernel test_wasm \
          cat cata catav
.PHONY: $(PASSED)
$(PASSED):
	@$(MAKE) $@
