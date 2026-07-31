# Project root. This file holds the cross-cutting tasks (test, clean, install,
# ...) and INCLUDES one fragment per build area, each living in the folder it
# builds: mk/lib.mk (out/lib headers), host/build.mk (the POSIX CLI), crew/build.mk
# (the crew app scripts), port/inle/kernel.mk (the freestanding kernel), test/test.mk
# (the gates), mk/install.mk. wasm keeps its own Makefile. Device ports (playdate,
# rp2040) live in the separate l-ports repo. Build output lands under out/
# (out/host, out/free, out/lib, out/dl). Shared vars live in common.mk.
R := .
include common.mk

CCACHE ?= $(shell command -v ccache 2>/dev/null)

# love0 -- the bootstrap interpreter, PINNED to the canonical out/host tree (never
# $(hsuf)'d like $(ho)): it bakes the lcat headers every frontend shares. Defined
# UP HERE, not by the host-build block below, because test_love0's prerequisite uses
# it -- and make expands prerequisites at PARSE time, so a later definition reads as
# empty, silently dropping test_love0's dep on the binary. Serially that hides (the
# earlier test_host builds love0 transitively via host -> lib_h -> love0); under -j,
# test_love0 then races ahead of the love0 link ("out/host/love0: No such file").
love0 = out/host/love0

# every love run UNDER make boots deterministically: the test gate must exercise the freshly-built egg
# (not a stale baked image), and the bench controls glazed-vs-interp itself. So suppress the startup
# image auto-load for all recipes. The USER's binary (run outside make) still wakes its baked image.
export LOVE_NO_IMAGE := 1

.PHONY: all install uninstall clean distclean
.PHONY: host kernel wasm love0 site site-serve
.PHONY: test test_host test_slow test_tools test_love0 test_wasm test_proof test_gen test_uugen test_uuwm uuwm test_gc test_gcheck test_hostnif test_doc test_glaze test_sat test_holo test_as test_holofuzz test_glazefuzz test_encver test_lux test_extract test_big test_mx test_clay test_arm64 test_thumb1 test_thumb2 test_virt test_wake
.PHONY: valg disasm flame cat cata catav perf repl gdb vmret bench nettest lint fmt fmt-check ccdb

# `make` with no target is `make test` -- pinned EXPLICITLY because the includes
# below precede the test rule, so make's "first explicit target is the default"
# would otherwise pick a fragment's first rule (mk/lib.mk's `lib`).
.DEFAULT_GOAL := test

# --- build fragments, pushed down into the folders they build (see each file) ---
include mk/lib.mk
include host/build.mk
include crew/build.mk
include mk/distro.mk
include port/inle/kernel.mk
include test/test.mk
include mk/install.mk

# `make test` is the DEV GATE -- what you run on every edit: the two egg self-tests (the
# host binary `love` from-source under LOVE_NO_IMAGE, and love0 -- c0 + the self-hosted ev,
# twice) plus vmret. It does NOT build the image (the --bake step), nor run
# coqc/lean/glaze/gc/tools, which are slow and/or need extra toolchains. ~20 s settled on
# this box, serial by design: no -j races, ctrl-C responsive.
#
# `make test_slow` is the MERGE GATE -- run it before publishing, not per edit. It is
# MINUTES (~17 on this box, less on a settled tree, more right after a commit), and a
# fifth of that is three doors booting the corpus under emulation (2026-07-30, warm:
# test_kernel 62s, test_uefi 64s, test_kernel_arm64 88s) beside test_arm64's 66s
# qemu-user corpus. It was `test_all` until the name stopped being true -- test_kdiff,
# test_selfhost, test_riscv, test_up and the test_raw_* cross lanes are all opt-in, so
# this is the SLOW gate, not the whole one. While DEVELOPING, run `make test` and the
# individual test_* targets that cover what you touched -- most are under a second
# ($(mw), test/test.mk).
JOBS  ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
osync := $(if $(filter output-sync,$(.FEATURES)),--output-sync=target,)
test_phases = test_host test_love0 vmret
test:
	@$(MAKE) --no-print-directory $(test_phases)
# vmret rides the fast `test`: the TCO gate (every lvm_* VM ap must tail-jump, never
# emit a `ret`) so a sibcall regression breaks the gate the moment it lands. It no-ops
# with a message when no disassembler (objdump/llvm-objdump) is on PATH, like the
# proof/kernel/wasm tests. The rest of test_tools (cook/tele/xor) + the crew apps stay
# in test_slow.
# test_kernel + test_wasm are in test_slow but NOT the fast `test`: each needs an
# extra toolchain (qemu, x86_64-only; emcc + node) and no-ops when that is
# absent. See their rules below.
test_slow: test_host test_love0 test_front test_proof test_gen test_uugen test_uulean test_uuwm test_uukind test_gc test_gcheck test_extract test_big test_mx test_tools test_hostnif test_doc test_glaze test_sat test_holo test_as test_holofuzz test_glazefuzz test_encver test_lux test_kore test_nest test_seed test_vi test_moon test_clay test_ccarm64 test_ccriscv test_libc test_ulp test_raw test_drv test_asmops test_fixpoint test_dist nettest test_arm64 test_thumb1 test_thumb2 test_thumb2sp test_virt test_mps2 test_mps2_t1 test_mps2_wake test_teensy41 test_nucleo446 test_playdate test_kernel test_uefi test_kernel_arm64 test_vec test_wasm test_wake
all: host kernel wasm

# lint: paren/bracket/brace balance + unclosed strings across every tracked .l
# (libra ⚖ -- crew/libra/libra.l over lib/lint.l, a .l-aware scan: ; and #!
# comments, ' and ` are reader ops). Balance is libra's DEFAULT verb, so the bare
# file list is the whole invocation. QUIET when clean, path:line:col: warnings +
# exit 1 on any imbalance; tabs warn but don't fail. NOT in the test gate (it's an
# editing aid, not a semantic check).
lint: $(ho)/love
	@$(ho)/love $R/crew/libra/libra.l $$(git ls-files '*.l') && echo "lint: .l balance clean"

# ccdb: emit compile_commands.json so clangd sees the flags the build actually uses.
# without it clangd guesses, misses love.h, and the fatal include error cascades into a
# flood of undeclared-name noise that says nothing about the code. the generated headers
# under out/ must exist, so build first. machine-specific (absolute paths), gitignored.
ccdb:
	@python3 $R/tools/ccdb.py

# fmt: reformat the HOUSE-STYLE C in place with moonfmt (crew/moon/fmt.l) -- reindent
# to 1-space, respace glued operators, normalize known-type pointer declarators. it is
# content-preserving + idempotent, so it only ever touches whitespace. SCOPED on purpose:
# just the core + the host CLI. test/cc are compiler test INPUTS (deliberate formatting),
# crew/moon/include are libc-shaped headers, port/* is board code -- none house style, so
# none are swept. `fmt-check` is the read-only variant (exit 1 if anything is unformatted).
FMT_FILES := love.c love.h $(wildcard host/*.c) $(wildcard host/*.h)
fmt: $(ho)/love
	@$(ho)/love $R/crew/moon/fmt.l -w $(FMT_FILES) && echo "fmt: formatted $(words $(FMT_FILES)) file(s)"
fmt-check: $(ho)/love
	@bad=; for f in $(FMT_FILES); do \
	  $(ho)/love $R/crew/moon/fmt.l "$$f" | diff -q "$$f" - >/dev/null || bad="$$bad $$f"; done; \
	  if [ -n "$$bad" ]; then echo "fmt-check: needs formatting:$$bad" >&2; exit 1; else echo "fmt-check: clean"; fi

# NB: there is NO git pre-commit hook -- committed artifacts (wasm/love.js, bench/
# bench.html) are rebuilt MANUALLY (`make wasm`, `make -C bench html`) and staged
# by hand. An auto-rebuild hook re-ran the benchmarks on every commit (minutes);
# it was removed deliberately. Rebuild before committing artifact-affecting code.

# crew/cook/Cookfile: this Makefile transpiled into a resolved cook recipe by
# `cook --emit` (crew/cook/cook.l). cook reads this Makefile directly too, but the
# emitted Cookfile is the build with every $(shell)/$(wildcard)/var/pattern
# RESOLVED -- a flat, self-documenting snapshot. Regenerate it whenever the
# Makefile changes. (A baked snapshot: re-run `make crew/cook/Cookfile` after adding
# a source/test file, since the wildcard lists are frozen at emit time.)
crew/cook/Cookfile: $(MAKEFILE_LIST) crew/cook/cook.l $(ho)/love
	@echo AI	$@
	@$(ho)/love -l crew/cook/cook.l --emit Makefile > $@

# site: this tree's own docs as a browsable site -- README.md + doc/*.md through
# papel (crew/papel/papel.l), which is lapiz for the markdown and cook for the
# staleness, so a second `make site` writes nothing. `make site-serve` builds it
# and hands out/site to kiosko, regenerating before each request: edit a .md,
# reload, see it. There is no out/site rule -- papel IS the incremental build,
# and make must not second-guess which pages are stale.
site: host
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site README.md doc
site-serve: host
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site -s $(SITEPORT) README.md doc
# NOT `PORT` -- test/test.mk already claims that one for nettest (7390), is included
# first, and `?=` on a set variable is a no-op, so `make site-serve` quietly served
# the site on nettest's port.
SITEPORT ?= 8080

# ====================================================================
# wasm (own Makefile)
# ====================================================================
wasm:
	@$(MAKE) -C wasm

clean:
	rm -rf out
	@rm -f proof/rocq/*.vo proof/rocq/*.vok proof/rocq/*.vos proof/rocq/*.glob proof/rocq/.*.aux
	@$(MAKE) -C wasm clean
distclean: clean

valg: host
	cat $t | valgrind --error-exitcode=1 --suppressions=$R/tools/valgrind.supp $m
# the math floor's differential: am.c vs the host libm, max-ulp per fn, and the
# REPORT -- `./out/host/ulp reduce` adds the reduction scan. this lane is the
# eyeball one (opt-in like valg: needs a hosted oracle); test_ulp is the GATE,
# and it builds am.c with mooncc as well, which this never did.
.PHONY: ulp
ulp:
	@mkdir -p out/host
	@$(CC) -O2 -o out/host/ulp $R/tools/ulp.c $R/crew/moon/lib/math/am.c -lm
	@out/host/ulp
out/host/perf.data: host
	cat $t | perf record -o $@ $m
perf: out/host/perf.data
	exec perf report -i $<
out/host/flamegraph.svg: out/host/perf.data
	flamegraph -o $@ --perfdata $<
repl: host
	@exec $m
cloc:
	cloc --by-file love love.c love.h main.c port tools test vim crew
cat: clean all test
cata: clean all test_slow
# Full clean rebuild, every frontend, all tests, then the corpus under valgrind.
catav: clean all test_slow valg

disasm: host
	exec rizin -A $m
gdb: host
	exec gdb $m
# no-op with a message when no disassembler is present, so the fast `test` stays
# portable (like test_proof/coqc). tools/vmret.l disassembles $m and flags any
# lvm_* VM ap that emits a `ret` instead of tail-jumping to the next.
OBJDUMP_ANY := $(shell command -v objdump 2>/dev/null || command -v llvm-objdump 2>/dev/null)
ifeq ($(OBJDUMP_ANY),)
vmret: host
	@echo "vmret: skipped (needs objdump or llvm-objdump)"
else
vmret: host
	@$m tools/vmret.l $m
endif

bench: host
	$(MAKE) -C bench bench

