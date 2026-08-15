# mk/cook.mk -- a make-shaped front door to cook (crew/cook/cook.l over this tree's Makefile).
# `make -f mk/cook.mk <goal>` makes sure the love binary exists, then hands the goal to cook.
#
# The real Makefile stays the source of truth for the irreducible C bootstrap: cook RUNS on
# love, so it cannot build love. That one rung is make's, cook ports everything above it,
# and anything cook cannot drive -- qemu, a tty repl, a sub-make, perf, since cook's `run`
# captures stdout and waits -- passes straight through to make below.

LOVE := out/host/love
# ⚠ cook reads the Makefile ITSELF, never a transpiled snapshot: a snapshot freezes the
# $(wildcard) lists at emit time, so it goes quietly stale the next time a source lands.
COOK := $(LOVE) -l crew/cook/cook.l -f Makefile

.DEFAULT_GOAL := all

# The bootstrap rung cook cannot climb. No prerequisites, so it fires only when the binary
# is ABSENT: an existence gate, not a staleness one -- cook's own `host` card rebuilds.
$(LOVE):
	@$(MAKE) host

# the verbs cook owns: make the binary exist, then let cook take over.
COOKED := all test clean install bench vmret valg
.PHONY: $(COOKED)
$(COOKED): $(LOVE)
	@$(COOK) $@

# the verbs cook cannot drive -- interactive, streaming, or a sub-make -- passed verbatim.
PASSED := host love0 kernel wasm lib hooks uninstall \
          run run-hdd run-headless repl gdb disasm perf flame cloc \
          test_slow test_host test_love0 test_tools test_kernel test_wasm \
          cat cata catav
.PHONY: $(PASSED)
$(PASSED):
	@$(MAKE) $@
