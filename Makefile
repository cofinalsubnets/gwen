# project root makefile
R := .
include mk/common.mk

CCACHE ?= $(shell command -v ccache 2>/dev/null)

# ==== THE FULL-FAT ARTIFACT'S OWN TOOLCHAIN (doc/misc/dist.md) ====
# A release tarball that ships `bin/love` carries its whole C toolchain in that one
# file: love wears a `mooncc` verb, and mooncc drives gcc-shaped recipes unchanged
# (test_drv). So when the bundled binary is here and the user named no compiler, IT
# is the compiler -- `make` on a seed-laid tree touches no ambient cc.
# ⚠ `CC ?=` CANNOT SAY THIS. make defines CC=cc itself, so `?=` never fires and the
# ambient compiler would win silently; $(origin CC) is the only way to ask whether a
# HUMAN set it. An explicit CC= still outranks the bundle, which is what the lean
# artifact's whole point is and what the DDC leg needs.
# ⚠ AND CCACHE MUST GO WITH IT. The compile lanes spell $(CCACHE) $(CC), and ccache
# takes the compiler as its FIRST argument -- handing it a two-word `love mooncc`
# makes it run `love` and treat `mooncc` as a source-file argument. Nothing here is
# cacheable by it anyway: mooncc is not a compiler ccache knows how to hash.
# ⚠ and the test is NOT `origin CC == default`: mk/common.mk says `CC = clang`, so by the
# time this runs the origin is "file" and a default-only guard never fires. What we
# actually mean is "unless a HUMAN named a compiler for this run" -- command line or
# environment. The tree's own clang default is exactly what the bundle should displace.
# ⚠ AND A BUNDLED love IS ALREADY PAST THE SELF-HOST CIRCLE. love0 exists for exactly one
# reason: to be *some* love that can wake mooncc0.image, because the default love is
# mooncc-built and so cannot drive its own build. A tree the seed laid
# has no such circle -- that binary IS a love with mooncc baked in as a verb. So it
# compiles with the binary it shipped with, and love0 / mooncc0.image / the sed-laid 0.h
# twins are never built at all. Not an optimisation: building them is how a bootstrap that
# has nothing to bootstrap goes wrong (distboot found the mooncc-built lane segfaulting
# laying prel0.h), and the lane simply has no reason to exist here.
bundled_love := $(if $(wildcard $(R)/bin/love),$(abspath $(R)/bin/love),)

ifeq ($(filter command line environment override,$(origin CC)),)
ifneq ($(bundled_love),)
CC := $(bundled_love) mooncc
CCACHE :=
endif
endif

# bootstrap interpreter
love0 = out/host/love0

# the gates run the image by default: warm is what ships. a gate whose subject is the
# fresh egg says so itself with LOVE_NO_IMAGE=1; love0 is always the egg. the build
# recipes keep their LOVE_NO_IMAGE= clears as the guard against a user's exported egg.

# every verb here is phony: one roster, so adding one is one line and not two. (the gates
# each fragment owns are rostered in that fragment.)
.PHONY: all install uninstall clean distclean host kernel wasm love0 site site-serve test test_host \
  test_hdiff test_slow test_extra test_tools test_love0 test_wasm test_proof test_gen test_uugen \
  test_gc test_gcheck test_gcstress test_hostegg test_hostnif test_doc test_glaze test_hook test_sat test_cli \
  test_holo test_as test_elf32 test_objcopy test_holofuzz test_glazefuzz test_encver test_lux \
  test_extract test_big test_mx test_clay test_moonfuzz test_arm64 test_thumb1 test_thumb2 \
  test_virt test_wake test_embed test_rp2040 valg disasm flame cat cata catav perf repl gdb \
  vmret waits bench nettest lint ccdb ulp cacheclean

# ⚠ THE UNPACKED RELEASE BUILDS THE PRODUCT. `tar xzf love-<ver>.tar.gz && make -C love-<ver>`
# has to end in a seed binary, because that is what whoever ran it came for -- our gates and
# bootstrap rungs are the tree's business, not theirs. A checkout keeps the fast gate: there the
# edit loop is the thing, and `make dist` is one word away.
ifeq ($(in_git),)
.DEFAULT_GOAL := dist
else
.DEFAULT_GOAL := test
endif

# avoid creating empty artifacts with fresh mtime
.DELETE_ON_ERROR:

# --- build fragments, pushed down into the folders they build (see each file) ---
include mk/lib.mk
include host/build.mk
include crew/build.mk
include mk/distro.mk
include free/kernel.mk
include test/test.mk
include mk/install.mk

JOBS  ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
osync := $(if $(filter output-sync,$(.FEATURES)),--output-sync=target,)
test_phases = test_host test_love0
# fast gate
test:
	@$(MAKE) --no-print-directory $(test_phases)

# slow gate -- the MERGE gate. test_seed is the headline: the artifact lays its own
# source and rebuilds itself byte-identically (`love seed`), which is the product's
# whole claim. the embedded lanes (kernel, boards, wasm, cross arches) are off the
# rosters: out of scope for now, each still runs by name when its surface moves.
# the kernel's set has one name -- `make test_inle` (free/kernel.mk) -- since it is
# six lanes and typing five of them is how the sixth goes unrun.
test_slow: test_host test_love0 vmret test_bakerep test_stdinbuf test_stdincorpus test_seat test_cli test_cookdiff test_dist test_seed


# really really really slow gate: the depth behind the seed -- the proofs, the gc lanes,
# the moon and holo batteries, the crew apps. the embedded and cross-arch gates left this
# roster with the slow gate's (test_embed*, test_kernel*, test_disk, test_uefi, test_virt,
# test_vec, test_asmops, the boards, the thumb lanes, test_elf32, test_objcopy, test_wasm,
# test_arm64, test_cc/cts arm64+riscv); test_fixpoint and test_distboot retired to
# by-name as well -- test_seed proves the circle in the slow gate.
test_extra: test_filemode waits test_front test_proof test_gen test_uugen test_uulean test_uuwm \
	test_uukind test_gc test_gcheck test_gcstress test_extract test_big test_mx \
	test_tools test_hostnif test_doc test_glaze test_hook test_sat test_holo test_as \
	test_holofuzz test_glazefuzz test_encver test_lux test_kore test_refuzz test_sb test_vi \
	test_moon test_clay test_moonfuzz test_forge \
	test_cts test_libc test_ulp test_raw \
	test_drv test_hdiff nettest test_wake test_gz test_cpio

all: host kernel wasm dist

# lint: libra ⚖ over every tracked .l -- BALANCE, and nothing else refuses. libra's
# other rules print as an editing aid and the exit code ignores them, so no roster
# of exempt files is owed and none is kept: every tracked .l answers for its parens
# and for nothing about how it is laid out. NOT in the test gate.
lint: $(ho)/love
	@$(ho)/love $R/crew/libra/libra.l $$(git ls-files '*.l') && echo "lint: parens balance"

# ccdb: emit compile_commands.json so clangd reads the flags the build actually uses --
# without it the missed core/love.h cascades into a flood of undeclared-name noise. ⚠ the
# generated headers under out/ must exist, so build first. Machine-specific: it carries
# absolute paths, so it is this box's and rides in no archive.
ccdb: $(ho)/love
	@$(ho)/love $R/tools/ccdb.l

# ⚠ there is deliberately NO pre-commit hook: the committed artifacts (wasm/love.js,
# bench/bench.html) are rebuilt by hand (`make wasm`, `make -C bench html`) and staged, so
# rebuild before committing anything that affects them. An auto-rebuild hook re-ran the
# benchmarks on every commit, minutes each.
#
# ⚠ and wasm/love.js CANNOT be gated by cmp-against-a-rebuild -- not for want of
# determinism, the link is exact, but because it BAKES `git describe`. It therefore
# embeds the revision it was built AT, necessarily the parent of the commit carrying it,
# so a rebuild at HEAD always differs in those 8 bytes after ANY commit. It is one
# revision behind by construction: the cost of baking the id. test_wasm links out of tree
# so at least it stops DIRTYING the file on every run.

# this tree's own docs as a browsable site: README.md + doc/*.md through papel -- plus
# one page per CREW TOOL, whose header comment IS its documentation, and the ANNOTATED
# SOURCE of every crew tool beside it.
#
#   libra doc   lifts a .l file's header out as markdown (it is the only thing in the
#               tree that reads .l for prose), and papel builds the site from markdown
#               exactly as it always has -- papel never learns what a .l is.
#   hue2web     paints the source itself, out of crew/vi/hue.l's class table and
#               crew/vi/config.l's theme -- the same table the editor and the vim
#               syntax read, so the site wears the editor's colours by construction.
#
# a tool with a doc/*.md of its own is skipped for the DOC page (that page is the one
# someone wrote) but still gets its source page. ⚠ BOTH doc/ AND doc/misc/ are asked:
# doc/ is the three man sources now and every other hand-written page is under misc.
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
# ⚠ LOVE_NO_IMAGE is CLEARED (the guard against an exported egg): the painter asks THIS
# host for its vocabulary, and under the egg boot that vocabulary is the compiler's own
# internals rather than the shipped language.
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

# clean takes everything this tree BUILDS; dl/ is what it DOWNLOADED, minutes of network
# for bytes no edit invalidates, so it survives. distclean is what asks for it again.
clean:
	rm -rf out
	@rm -f test/proof/rocq/*.vo test/proof/rocq/*.vok test/proof/rocq/*.vos test/proof/rocq/*.glob test/proof/rocq/.*.aux
	@# wasm/ does not ride the release (crew/build.mk's dist_drop), so an unpacked
	@# tree has no such directory to clean and must not fail trying.
	@[ -d wasm ] && $(MAKE) -C wasm clean || :
distclean: clean
	rm -rf dl
# ⚠ THE COMPILER'S CACHE LIVES IN HOME, so `clean` cannot reach it and should not try -- ~/.love
# also holds the INSTALLED nest that `make install` put there, and a clean that ate someone's
# installation would be a surprise of the worst kind. It bounds itself (crew/moon/moon.l's
# mcsweep: the last 32 archives, ~13 MB, swept on every link); this is the door for wanting it
# gone anyway.
cacheclean:
	rm -rf $(HOME)/.love/cache

# the memory lane. ⚠ THE CORPUS IS A FILE ARGUMENT, NEVER STDIN: the corpus TESTS stdin
# (test/io.l's see/unsee roundtrip), so piping it in has those asserts eating the script
# they ride on, and the run dies reading its own comments as code. `</dev/null` is what
# the asserts should find.
valg: host
	@cat $t > $(ho)/.valg-corpus.l
	valgrind --error-exitcode=1 --suppressions=$R/tools/valgrind.supp $m $(ho)/.valg-corpus.l </dev/null
# the math floor's differential: am.c vs the host libm, max-ulp per fn (`ulp reduce` adds
# the reduction scan). The EYEBALL lane, opt-in like valg since it needs a hosted oracle;
# test_ulp is the gate, and it builds am.c with mooncc too, which this never did.
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
	cloc --by-file love core/love.c core/love.h main.c port tools test vim crew
cat: clean all test
cata: clean all test_slow
# Full clean rebuild, every frontend, all tests, then the corpus under valgrind.
catav: clean all test_slow valg

disasm: host
	exec rizin -A $m
gdb: host
	exec gdb $m
# tools/vmret.l disassembles $m and flags any lvm_* VM ap that emits a `ret` instead of
# tail-jumping. the sibcall pass only gripes over a call that is MARKED ai_musttail, so a
# forgotten mark rets silently -- this is the check that the discipline is COMPLETE, ~2 s.
# No-op with a message when no disassembler is present, so the gate stays portable.
OBJDUMP_ANY := $(shell command -v objdump 2>/dev/null || command -v llvm-objdump 2>/dev/null)
ifeq ($(OBJDUMP_ANY),)
vmret: host
	@echo "vmret: skipped (needs objdump or llvm-objdump)"
else
vmret: host
	@$m tools/vmret.l $m
endif

# waits pins an invariant whose
# only failure mode is a HANG, which no assert catches after the fact. The device floor's
# rule is that the only code here that blocks is the scheduler, and tools/waits.l carries
# the roster of every wait plus the sentence earning it -- a new one reddens here instead
# of arriving as a wedged gate. It reads the C, never the ELF, so it needs no toolchain,
# but it does need the tracked file list and so no-ops outside a git checkout.
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

