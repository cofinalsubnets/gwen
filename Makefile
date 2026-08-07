# project root makefile
R := .
include common.mk

CCACHE ?= $(shell command -v ccache 2>/dev/null)

# bootstrap interpreter
love0 = out/host/love0

# maybe we can change this
export LOVE_NO_IMAGE := 1

.PHONY: all install uninstall clean distclean
.PHONY: host kernel wasm love0 site site-serve
.PHONY: test test_host test_slow test_extra test_tools test_love0 test_wasm test_proof test_gen test_uugen test_uuwm uuwm test_gc test_gcheck test_gcstress test_hostnif test_doc test_glaze test_hook test_sat test_holo test_as test_elf32 test_objcopy test_holofuzz test_glazefuzz test_encver test_lux test_extract test_big test_mx test_clay test_moonfuzz test_arm64 test_thumb1 test_thumb2 test_virt test_wake test_embed test_rp2040
.PHONY: valg disasm flame cat cata catav perf repl gdb vmret waits bench nettest lint ccdb

.DEFAULT_GOAL := test

# avoid creating empty artifacts with fresh mtime
.DELETE_ON_ERROR:

# --- build fragments, pushed down into the folders they build (see each file) ---
include mk/lib.mk
include host/build.mk
include crew/build.mk
include mk/distro.mk
include port/inle/kernel.mk
include test/test.mk
include mk/install.mk

.PHONY: test test_slow test_extra

JOBS  ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
osync := $(if $(filter output-sync,$(.FEATURES)),--output-sync=target,)
test_phases = test_host test_love0
# fast gate
test:
	@$(MAKE) --no-print-directory $(test_phases)

# slow gate. test_embed rides here rather than in `test`: ~7 s on a tree nothing touched,
# and it earns them only when love.h or a frontend moved (a love.h edit puts it at ~2.5 min,
# because linking every frontend means compiling love.c once per target). What it adds HERE
# is the frontends the booting lanes below never reach -- mps2, teensy41, nucleo446,
# playdate, and kmain.c at aarch64 -- which otherwise wait for test_extra.
test_slow: test_host test_love0 vmret test_wasm test_kernel test_virt test_embed
	

# really really really slow gate. test_embed is here too, cheap insurance: the thumb lanes
# below SKIP without arm-none-eabi, so on a bare box this tier would otherwise compile none
# of them either.
test_extra: test_embed test_filemode waits test_kernel_arm64 test_mps2 test_mps2_t1 \
	test_mps2_wake test_teensy41 test_nucleo446 test_nucleo446_smoke test_rp2040 test_playdate test_arm64 \
	test_vec test_front test_proof test_gen test_uugen test_uulean test_uuwm \
	test_uukind test_gc test_gcheck test_gcstress test_extract test_big test_mx \
	test_tools test_hostnif test_doc test_glaze test_hook test_sat test_holo test_as test_elf32 test_objcopy \
	test_holofuzz test_glazefuzz test_encver test_lux test_kore test_nest test_seed test_vi \
	test_moon test_clay test_moonfuzz test_ccarm64 test_ccriscv test_libc test_ulp test_raw \
	test_drv test_asmops test_fixpoint test_dist nettest test_thumb1 test_thumb2 test_thumb2sp \
	test_virt test_kernel test_uefi test_wasm test_wake

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

# NB: there is NO git pre-commit hook -- committed artifacts (wasm/love.js, bench/
# bench.html) are rebuilt MANUALLY (`make wasm`, `make -C bench html`) and staged
# by hand. An auto-rebuild hook re-ran the benchmarks on every commit (minutes);
# it was removed deliberately. Rebuild before committing artifact-affecting code.
#
# ⚠ wasm/love.js CANNOT be gated by cmp-against-a-rebuild, and the reason is not
# non-determinism -- the link is exactly reproducible. it BAKES `git describe`
# (love_version.h, mapped -dirty -> -wasm), so it embeds the revision it was built
# AT, which is necessarily the parent of the commit that carries it. a rebuild at
# HEAD therefore always differs, in exactly those 8 bytes, after ANY commit -- not
# only one that touches love.c. so it is one revision behind by construction; that
# is the cost of baking the id, and the alternative is not baking it. test_wasm
# links out of tree so at least it stops DIRTYING the file on every run.

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
# papel (crew/papel/papel.l)
site: host
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site README.md doc
SITEPORT ?= 8080
site-serve: host
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site -s $(SITEPORT) README.md doc

wasm:
	@$(MAKE) -C wasm

# clean takes everything this tree BUILDS. dl/ is everything it DOWNLOADED -- limine,
# OVMF, a package's sources -- which is minutes of network for bytes no edit invalidates,
# so it sits outside out/ and survives. distclean is the one that asks for it back.
clean:
	rm -rf out
	@rm -f proof/rocq/*.vo proof/rocq/*.vok proof/rocq/*.vos proof/rocq/*.glob proof/rocq/.*.aux
	@$(MAKE) -C wasm clean
distclean: clean
	rm -rf dl

# the memory lane. ⚠ THE CORPUS IS A FILE ARGUMENT, NEVER STDIN -- the corpus TESTS
# stdin (test/io.l's see/unsee roundtrip), so piping it in has those asserts eating the
# script they ride on, and the run dies reading its own comments as code. e77e0e8c moved
# four other lanes off the same pattern and missed this one; `</dev/null` is what the
# asserts should find.
valg: host
	@cat $t > $(ho)/.valg-corpus.l
	valgrind --error-exitcode=1 --suppressions=$R/tools/valgrind.supp $m $(ho)/.valg-corpus.l </dev/null
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

# waits rides the fast `test` beside vmret, for the same reason: it pins an
# invariant whose only failure mode is a HANG, which no assert can catch after
# the fact. the device floor's rule is that the only code in the tree that blocks
# is the scheduler, and tools/waits.l carries the roster of every wait plus the
# sentence that earns it -- a new one reddens here instead of arriving as a wedged
# gate. it needs no toolchain (it reads the C, never the ELF; see the file for why
# a disassembly could only answer green), but it does need the tracked file list,
# so it no-ops outside a git checkout the way vmret does without objdump.
WAITS_C := $(shell git ls-files '*.c' 2>/dev/null)
ifeq ($(WAITS_C),)
waits: host
	@echo "waits: skipped (needs a git checkout to enumerate the .c files)"
else
waits: host
	@$m tools/waits.l $(WAITS_C)
endif

bench: host
	$(MAKE) -C bench bench

