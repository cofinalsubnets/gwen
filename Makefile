# project root makefile
R := .
include mk/common.mk

CCACHE ?= $(shell command -v ccache 2>/dev/null)

# ==== who compiles this tree (doc/misc/dist.md) ====
# ⚠ MAKE DOES NOT GUESS, and it used to: a `bin/love` laid beside the source WAS the
# toolchain by being there, so a plain `make` preferred it over the machine's own compiler
# -- the weaker claim, picked by a file existing. Which mode a build is in belongs to
# whoever DRIVES it. A bare make has no love, so it can only mean the ambient cc, which is
# what $(CC) already says. A love driving (lib/source.l's `seed`) knows its own selfpath
# and names CC outright: a working ambient cc where it probed one -- the STRONGER claim,
# since a foreign compiler holding the scaffold is the one thing a self build cannot say
# -- and its own mooncc where it did not. Nothing here needs to ask.
# `cc_named` is `make CC=gcc` or CC in the environment -- PATH is not consulted, and `CC ?=`
# could not ask it (make defines CC itself, so `?=` never fires). ⚠ CCACHE goes when CC is
# two words: ccache takes the compiler as argv[1], and `love mooncc` is not one.
cc_named := $(filter command line environment override,$(origin CC))
ifneq ($(words $(CC)),1)
CCACHE :=
endif

# bootstrap interpreter
love0 = out/host/love0

# the gates run the image by default: warm is what ships, and a gate whose subject is the
# fresh egg says LOVE_NO_IMAGE=1 itself. the recipes' LOVE_NO_IMAGE= clears guard against a
# user's exported egg.

# every verb here is phony, one roster (a fragment's own gates are rostered in that fragment).
.PHONY: all install uninstall clean distclean host kernel wasm love0 site site-serve test test_host \
  test_hdiff test_slow test_extra test_tools test_love0 test_wasm test_proof test_gen test_uugen \
  test_gc test_gcheck test_gcstress test_hostegg test_hostnif test_doc test_glaze test_hook test_sat test_cli \
  test_holo test_as test_elf32 test_objcopy test_holofuzz test_glazefuzz test_encver test_lux \
  test_extract test_big test_mx test_clay test_moonfuzz test_arm64 test_thumb1 test_thumb2 \
  test_virt test_wake test_embed test_rp2040 valg disasm flame cat cata catav perf repl gdb \
  vmret waits bench nettest lint ccdb ulp

# an unpacked release builds the PRODUCT: `tar xzf .. && make -C love-<ver>` has to end in a
# seed binary, which is what whoever ran it came for. a checkout keeps the fast gate.
ifeq ($(in_git),)
.DEFAULT_GOAL := dist
else
.DEFAULT_GOAL := test
endif

# avoid creating empty artifacts with fresh mtime
.DELETE_ON_ERROR:

# --- build fragments, pushed down into the folders they build ---
include mk/lib.mk
include src/build.mk
include crew/build.mk
include mk/distro.mk
include src/kernel.mk
include test/test.mk
include mk/install.mk

JOBS  ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
osync := $(if $(filter output-sync,$(.FEATURES)),--output-sync=target,)
test_phases = test_host test_love0
# fast gate
test:
	@$(MAKE) --no-print-directory $(test_phases)

# slow gate -- the MERGE gate, headlined by test_seed: the artifact lays its own source and
# rebuilds itself byte-identically, which is the product's whole claim. the embedded lanes
# are off the roster and run by name; the kernel's six are one name, `make test_inle`.
# ⚠ test_seed IS NOT test_distboot's little brother, and reading it that way is how the
# seed's cc deference died unnoticed for eleven days. They prove DIFFERENT things and
# neither contains the other -- test/test.mk says which is which at each recipe.
test_slow: test_host test_love0 vmret test_bakerep test_stdinbuf test_stdincorpus test_seat test_cli test_cookdiff test_dist test_seed


# really really really slow gate: the depth behind the seed -- the proofs, the gc lanes, the
# moon and holo batteries, the crew apps. the embedded, board and cross-arch lanes are by
# name only (src/kernel.mk, port/, wasm/), as are test_fixpoint and test_distboot.
test_extra: test_filemode waits test_front test_proof test_gen test_uugen test_uulean test_uuwm \
	test_uukind test_gc test_gcheck test_gcstress test_extract test_big test_mx \
	test_tools test_hostnif test_doc test_glaze test_hook test_sat test_holo test_as \
	test_holofuzz test_glazefuzz test_encver test_lux test_kore test_refuzz test_sb test_vi \
	test_moon test_clay test_moonfuzz test_forge \
	test_cts test_libc test_ulp test_raw \
	test_drv test_hdiff nettest test_wake test_gz test_cpio

all: host kernel wasm dist

# lint: libra ⚖ over every tracked .l, and BALANCE is the only thing that refuses -- its
# other rules print as an editing aid the exit code ignores, so no roster of exempt files is
# owed. not in the test gate.
lint: $(ho)/love
	@$(ho)/love $R/crew/libra/libra.l $$(git ls-files '*.l') && echo "lint: parens balance"

# ccdb: compile_commands.json, so clangd reads the flags the build actually uses. build
# first (it needs the generated headers under out/); the absolute paths make it this box's.
ccdb: $(ho)/love
	@$(ho)/love $R/tools/ccdb.l

# wasm/love.js is the one committed build artifact: `make wasm` refreshes it by hand, so do
# that before committing anything that moves it. it bakes ./VERSION and nothing of the
# revision, so a rebuild at HEAD is byte-identical and test_wasm may cmp it once regenerated.

# this tree's own docs as a browsable site: README.md + doc/*.md through papel, plus one page
# per crew tool (its header comment IS its documentation) and the annotated source beside it.
#   libra doc   lifts a .l header out as markdown -- the only thing in the tree that reads .l
#               for prose, so papel never learns what a .l is
#   hue2web     paints the source out of crew/vi/hue.l's class table and crew/vi/config.l's
#               theme, the same table the editor and the vim syntax read
# a tool with a doc/*.md of its own is skipped for the doc page and still gets a source page.
# both doc/ and doc/misc/ are asked: doc/ is the three man sources, the rest live under misc.
crewtools = $(foreach d,$(wildcard crew/*),$(wildcard $d/$(notdir $d).l))
sitetools = $(foreach f,$(crewtools),\
  $(if $(wildcard doc/$(notdir $(basename $f)).md doc/misc/$(notdir $(basename $f)).md),,$f))
out/toolmd.stamp: $(sitetools) crew/libra/libra.l $(ho)/love
	@rm -rf out/toolmd && mkdir -p out/toolmd
	@for f in $(sitetools); do n=$${f##*/}; n=$${n%.l}; \
	   { $(ho)/love $R/crew/libra/libra.l doc $$f && echo && echo "[the source]($$n.src.html)"; } \
	     > out/toolmd/$$n.md || exit 1; done
	@echo "  toolmd: $(words $(sitetools)) crew headers -> out/toolmd/"
	@touch $@
# the source pages and their stylesheet, written into the site papel just built
huesrc = $(crewtools) crew/vi/hue.l crew/vi/config.l tools/hue2web.l $(ho)/love
site: host out/toolmd.stamp
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site README.md doc out/toolmd
	@$(MAKE) --no-print-directory out/site/hue.css
# LOVE_NO_IMAGE is cleared: the painter asks THIS host for its vocabulary, and an egg boot
# would answer with the compiler's internals rather than the shipped language.
out/site/hue.css: $(huesrc)
	@env -u LOVE_NO_IMAGE $(ho)/love $R/tools/hue2web.l css > $@
	@for f in $(crewtools); do n=$${f##*/}; n=$${n%.l}; \
	   env -u LOVE_NO_IMAGE $(ho)/love $R/tools/hue2web.l src $$f > out/site/$$n.src.html \
	     || exit 1; done
	@echo "  hue2web: $(words $(crewtools)) sources painted -> out/site/*.src.html"
SITEPORT ?= 8080
site-serve: host out/toolmd.stamp
	@$(ho)/love -l crew/papel/papel.l -t love -o out/site -s $(SITEPORT) README.md doc out/toolmd

wasm:
	@$(MAKE) -C wasm

# clean takes what this tree BUILDS; dl/ is what it downloaded, so it survives to distclean.
clean:
	rm -rf out
	@rm -f test/proof/rocq/*.vo test/proof/rocq/*.vok test/proof/rocq/*.vos test/proof/rocq/*.glob test/proof/rocq/.*.aux
	@# wasm/ does not ride the release (crew/build.mk's dist_drop), so an unpacked
	@# tree has no such directory to clean and must not fail trying.
	@[ -d wasm ] && $(MAKE) -C wasm clean || :
distclean: clean
	rm -rf dl
# the memory lane. the corpus is a FILE ARGUMENT, never stdin: it tests stdin itself
# (test/io.l's see/unsee roundtrip), so piping it in has those asserts eat the script they
# ride on. `</dev/null` is what they should find.
valg: host
	@cat $t > $(ho)/.valg-corpus.l
	valgrind --error-exitcode=1 --suppressions=$R/tools/valgrind.supp $m $(ho)/.valg-corpus.l </dev/null
# the math floor's differential: am.c vs the host libm, max-ulp per fn (`ulp reduce` adds the
# reduction scan). the eyeball lane, opt-in like valg since it needs a hosted oracle; test_ulp
# is the gate, and it builds am.c with mooncc too.
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
	cloc --by-file love src/love.c src/love.h main.c port tools test vim crew
cat: clean all test
cata: clean all test_slow
# full clean rebuild, every frontend, all tests, then the corpus under valgrind
catav: clean all test_slow valg

disasm: host
	exec rizin -A $m
gdb: host
	exec gdb $m
# tools/vmret.l flags any lvm_* VM ap that emits a `ret` instead of tail-jumping. the sibcall
# pass only gripes over a call MARKED ai_musttail, so a forgotten mark rets silently. ~2 s,
# and a no-op with a message where no disassembler is present.
OBJDUMP_ANY := $(shell command -v objdump 2>/dev/null || command -v llvm-objdump 2>/dev/null)
ifeq ($(OBJDUMP_ANY),)
vmret: host
	@echo "vmret: skipped (needs objdump or llvm-objdump)"
else
vmret: host
	@$m tools/vmret.l $m
endif

# waits pins an invariant whose only failure mode is a HANG, which no assert catches after the
# fact: the only code here that blocks is the scheduler, and tools/waits.l carries every wait
# plus the sentence earning it. it reads the C, never the ELF, so it needs no toolchain -- but
# it does need the tracked file list, and no-ops outside a git checkout.
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

