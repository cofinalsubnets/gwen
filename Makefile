# project root makefile
# R is the project root every path here hangs off. CONDITIONAL, so a makefile outside the
# tree -- a port's -- can set it and include this file to reach these recipes; mk/common.mk
# spells it the same way for the same reason. ⚠ that also means an R in the ENVIRONMENT is
# read, where a plain := would have shadowed it. A command line R already won either way.
R ?= .
include $(R)/mk/common.mk

CCACHE ?= $(shell command -v ccache 2>/dev/null)

# ==== who compiles this tree (doc/misc/dist.md) ====
# ⚠ MAKE DOES NOT GUESS, and it used to: a `bin/love` laid beside the source WAS the
# toolchain by being there, so a plain `make` preferred it over the machine's own compiler
# -- the weaker claim, picked by a file existing. Which mode a build is in belongs to
# whoever DRIVES it. A bare make has no love, so it can only mean the ambient cc, which is
# what $(CC) already says. A love driving (crew/source/source.l's `seed`) knows its own selfpath
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
  test_gc test_gcheck test_gcstress test_hostegg test_hostnif test_doc test_glaze test_hook test_letrec test_sat test_cli \
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

# ==== the lcat'd headers and the generated lib ====
# project root; shared vars are mk/common.mk.
#
# Each love/*.l is serialized to a C string literal by tools/lcat.l, run on the bootstrap
# love0. Frontends #include these and assemble the bootstrap with G_EGG_PRE/POST. Drop a .l
# into love/ and it is picked up -- no rule to edit.
lib_h = $(patsubst love/%.l,out/lib/%.h,$(wildcard love/*.l))
# the crew/holo/ assembler, baked into both runtimes as a core language service. Every
# backend is arch-neutral (they produce machine-code bytes as DATA and never execute them),
# but the HOST bakes its NATIVE one only, while love0 keeps them all so the corpus's
# cross-arch asserts run under both its compilers. Both flavors are generated either way:
# holo_h is the lcat header, asm0_h the sed-wrapped raw source love0 needs.
holo_h = out/lib/holo.h  out/lib/amd64.h  out/lib/arm64.h  out/lib/rv64.h
# holo's LINKER half is NOT baked: elf/obj/link ride the crew cat, laid at bake with the
# glaze live. no egg carries them -- the egg's holo feeds the glaze, which emits for the
# machine it runs on and never writes a file.
asm0_h = out/lib/holo0.h out/lib/amd640.h out/lib/arm640.h
# the glaze (native JIT): raw-text headers, no lcat round-trip. Evaled ONLY before a
# `love bake`, so a normal boot never pays the ~810 ms and the baked snapshot carries an
# always-on JIT at zero startup. doc/misc/snapshot.md.
glaze_h = out/lib/emit.h out/lib/auto.h out/lib/hook.h out/lib/walk.h
# love0's bootstrap headers: raw source wrapped as a C literal by four substitutions,
# since love0 cannot lcat the very sources it is assembled from. The whole concatenated
# corpus rides along so love0 self-tests both compilers in one run.
# ⚠ AMBIENT sed WHILE BOOTSTRAPPING, OURS ONCE WE HAVE ONE -- the same discipline as $(CC)
# and $(lcat_love). These headers are INPUTS to love0, so a from-scratch tree has no love
# to lay them with, and none is laid beside one either -- so this is ambient sed, always.
# ⚠ THE ORDER OF THE FOUR IS THE CORRECTNESS: backslash first, or the escapes it writes
# get escaped again by the quote pass.
sed_lit = sed \
  -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/'
boot_h = out/lib/cli0.h out/lib/egg0.h out/lib/post0.h out/lib/p10.h out/lib/prel0.h out/lib/ev0.h out/lib/bao0.h out/lib/uu0.h out/lib/coin0.h out/lib/rng0.h out/lib/q0.h out/lib/glob0.h out/lib/kanren0.h out/lib/overlay0.h out/lib/peg0.h out/lib/verbs0.h $(asm0_h)
.PHONY: lib
lib: $(lib_h) $(boot_h)
# ⚠ lcat a .l into its header ATOMICALLY -- temp, require non-empty, then mv. A bare `> $@`
# truncates first, so a broken love0 leaves a 0-byte header make calls up to date, which
# SILENTLY drops a baked service (an empty holo.h => `assemble` unbound => the glaze emits
# nothing => a corrupt native). ⚠ and the temp takes the PID: the ports RECURSE onto these,
# so -j runs the recipe twice at once and one shared temp is renamed out from under the other.
# ⚠ AND THE MOVE ASKS `cmp` FIRST. A recipe fires on an MTIME, so a bare touch re-derives
# the header either way; REWRITING it there would bump a mtime no content earned and rebuild
# every frontend that includes it. Re-derive, compare, move only on a difference -- the
# discipline out/lib/corpus.list and out/host/0/.love0cc already keep. The tag line rides
# the move, so what prints is what changed.
# ⚠ the lcat is run by love0, and only ever by love0: the tree carries no love of its own,
# so the bootstrap is the bootstrap wherever this builds (see ./Makefile).
# ⚠ AND THE PRELOAD BELONGS TO LOVE0 ALONE. `-l love/prel.l` feeds prel's SOURCE to a
# pre-egg love, which is the only kind that can read it: prel.l:19 calls `(tray 0)`, and
# `tray` is one of the raw ctors THE EGG MOPS AT BIRTH -- so a baked love handed its own
# prel source dies `;; missing tray`. A baked love does not need it either, having prel in
# the image already. Both lanes then lcat the same bytes.
lcat_love = $(love0) -l love/prel.l
lcat_h = @mkdir -p out/lib; t=$@.$$$$.tmp; \
  $(lcat_love) tools/lcat.l $< > $$t && test -s $$t \
    || { rm -f $$t; echo "FAIL: $@ empty (lcat failed -- broken bootstrap?)"; exit 1; }; \
  if cmp -s $$t $@ 2>/dev/null; then rm -f $$t; else mv -f $$t $@; echo 'LOVE	'$@; fi
$(lib_h): out/lib/%.h: love/%.l tools/lcat.l   # + $(love0), stated below
	$(lcat_h)
# the sed twin of the lcat lay: a text->C-literal that needs no interpreter, so the tag
# says SED -- it is sed running, ambient or ours, and never the lcat love.
# ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads, for the same reason $(hcc) does: the root
# Makefile exports it=1 in a tree with no bundled love, a seed-laid tree INHERITS it,
# and an egg-booted love has no verb table -- so `love sed` would read as a filename.
sed_h = @mkdir -p out/lib; t=$@.$$$$.tmp; LOVE_NO_IMAGE= $(sed_lit) $< > $$t; \
  if cmp -s $$t $@ 2>/dev/null; then rm -f $$t; else mv -f $$t $@; echo 'SED	'$@; fi
# ⚠ every rule below is a STATIC pattern -- their sources live outside love/, so the
# wildcard misses them, and an implicit pattern would make these headers INTERMEDIATE.
# holo rides the same lcat pipeline as the egg (the glaze is its client); rune is the CAS,
# for device frontends that bake it behind the egg.
$(holo_h): out/lib/%.h: crew/holo/%.l tools/lcat.l
	$(lcat_h)
out/lib/rune.h: crew/rune/rune.l tools/lcat.l
	$(lcat_h)
# love0's raw-source twins of the same backends, so the corpus tests the assembler under
# BOTH compilers, and the generic love/*.l twin beside them.
$(asm0_h): out/lib/%0.h: crew/holo/%.l
	$(sed_h)
out/lib/%0.h: love/%.l
	$(sed_h)
# the glaze is sigil-heavy, so it skips the lcat reader round-trip and bakes verbatim.
$(glaze_h): out/lib/%.h: love/glaze/%.l
	$(sed_h)
# ⚠ the corpus SET stamp: ktests.l aggregates $t, a wildcard, so a DELETED test leaves every
# remaining prereq older than the target and make keeps baking the ghost. Depend on the LIST:
# rewritten only when membership changes, so it re-lays on add OR delete. love0 READS this file
# at run time to find the corpus (src/main.c), which is what took the corpus out of its
# dependency graph -- so the list is a manifest now, not only a stamp.
.PHONY: force_corpus_list
force_corpus_list: ;
out/lib/corpus.list: force_corpus_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$t' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi

# love_version.h: the build's version, surfaced as the `love-version` global.
# ./VERSION is the WHOLE id -- an arbitrary string, ours to bump deliberately, and no
# version control is consulted: the artifact's bytes are the tree's, so the tree's own
# file is the only honest name for a build. It compiles into love.o, so it rewrites
# only when VERSION moves and a frontend without it falls back to "unknown".
out/lib/love_version.h: $(R)/VERSION
	@mkdir -p out/lib
	@printf '#define AiVersion "%s"\n' "$$(cat $(R)/VERSION)" > $@
	@echo 'SH	'$@

# the page a reader lands on -- `readelf -p .love.README` -- IS the command line's own
# answer, so the two cannot drift: `love -v -h`, spelled as the two halves it prints.
# love0 is what answers, because the readme is an input to the link that builds love and
# only the bootstrap exists that early. the version comes off VERSION rather than out of
# love0, which pins its own to "bootstrap"; the verb line is still love0's short roster.
out/lib/readme.bin: $(love0) $(R)/love/cli.l $(R)/VERSION
	@mkdir -p out/lib
	@printf 'love %s\n' "$$(cat $(R)/VERSION)" > $@
	@$(love0) -h </dev/null >> $@
	@echo 'LOVE	'$@

# the lcat'd headers are PRODUCED BY running love0, so re-lay them whenever it moves.
$(lib_h) $(holo_h) out/lib/rune.h: $(love0)

# ==== the host build ====
# the project root, so paths resolve from there; shared vars are mk/common.mk.
#
# the default flavor owns out/host and HCC builds in its own hsuf'd tree, so the $(CC)
# lane never overwrites a mooncc object -- the two disagree on nothing a linker can see, and
# a mixed relink would read as an up-to-date binary. the .hostcc stamp catches in-place
# flips. love0 and the generated out/lib/*.h stay pinned to canonical out/host paths.
ho = out/host$(hsuf)
h_o = $(love_c:$(R)/%.c=$(ho)/%.o)
# host_c (mk/common.mk): the per-app host-nif files, auto-registered via AiNif. linked
# directly into the binary, never via liblove.a, so the love_nifs section is not
# archive-collected. drop a src/<app>.c in and it builds -- no rule edit.
host_o = $(host_c:$(R)/%.c=$(ho)/%.o)
# love0 and the lib tools ride this too, HCC or not.
# (-I$(ho) -Iout/lib reach the generated egg/cli headers.)
host_cc = $(CC)
# GCDBG is the GC debug lanes' knob and deliberately not $(EXTRA_CFLAGS): it must reach
# the shipped love under both compilers and never love0. EXTRA_CFLAGS does not reach
# $(moon0) at all, so test_gcheck would run a corpus on a binary that never had the check
# in it; and love0 is shared and unsuffixed, so a flag reaching it leaks out of the debug
# lane -- a stress-built love0 segfaults baking mooncc0.image and takes the tree with it.
# LOVE_NO_IMAGE= (empty = unset) leads, and it is load-bearing whenever CC is the dist
# artifact's own `love mooncc` verb: under a caller's exported egg a love has no verb
# table, and `mooncc` then reads as a filename ("love: cannot open mooncc"). the love0
# lane already leads with it; this puts it on every $(hcc) site at once.
hcc = LOVE_NO_IMAGE= $(host_cc) $(ai_cflags) $(GCDBG) -Dai_tco=$(tco) -fpic -I$(ho) -I. -Isrc -Iout/lib
# the boot image gets its own segment at the top of the address space so `love bake` can
# grow it: the blob appends at the tail of the file and only that phdr + shdr are rewritten,
# nothing else moving (src/image.c's bake_tail). --section-start is what buys it -- ld
# gives a section at a far address a PT_LOAD to itself, above .bss and alone in it.
# 0x2000000 clears .bss with room to grow and is page-aligned, which the loader's
# offset/vaddr congruence needs; an overlap is a loud ld error. holo lays the same shape
# its own way, so both toolchains bake alike.
image_ldflags = -Wl,--section-start=.love.image=0x2000000
# .hostcc -- the tree's compiler+link identity, content-stamped (cmp keeps the mtime when
# nothing moved). every host object and the link depend on it, so an in-place flavor flip
# rebuilds the tree instead of relinking mixed-libc objects.
.PHONY: force_hostcc
force_hostcc: ;
$(ho)/.hostcc: force_hostcc
	@mkdir -p $(ho)
	@tf=$@.$$$$.tmp; printf '%s\n' '$(host_cc) $(image_ldflags)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
# liblove.a is not here, and that is the whole point: `ar` is a foreign tool, so a
# target that owed it could never `make host` without an ambient toolchain, however
# self-hosting everything else became. nothing in the default lane links it either --
# `love` comes from $(moon_o). its two consumers name it themselves: the HCC lane
# below, and test/front (test.mk), which is the only thing that links love as a
# library at all.
host: $(ho)/love $(ho)/love.baked $(ho)/love.1 $(ho)/cook.1
love0: $(love0)

# the boot image -- the crew bake (doc/misc/plan/one-binary.md): `$< bake -l ..` boots
# the fresh binary, evals the whole dist cat, and lays ONE image into that binary's own
# .image section -- src/image.c copies the exe, pwrites the blob and renames over the
# original, so a new inode leaves anyone still executing on the old one. the tree's love
# then is the artifact's shape: `love kore|mooncc|sh|libra ..` with no shim and no
# sibling image (the corpus never wakes it, riding LOVE_NO_IMAGE). the load is an
# optimization: main.c falls back to an egg boot on any mismatch, so a stale bake is
# slower, never fatal. the .baked stamp carries the dependency, since the bake mutates
# the binary itself -- and it watches the cat too, so a crew edit rebakes without
# relinking.
$(ho)/love.baked $(ho)/love.cand.baked: %.baked: % $(ho)/.dist-cat.l
	@echo 'LOVE	'$< "(bake)"
	@$< bake -l $(ho)/.dist-cat.l
	@touch $@


# candidate: build + bake the next generation at a side path nothing executes, so the
# in-place bake can never hit ETXTBSY whoever is running `love`. gate it with
# `make test m=$(ho)/love.cand`, then promote on green with an atomic rename -- the dock's
# `adopt`. on a red gate the canonical binary is untouched and the candidate dies at the
# side path, like a to-space that never flips.
.PHONY: candidate
candidate: $(ho)/love.cand.baked


# rm the archive first: `ar r` replaces and adds but never removes, so a renamed or
# dropped source leaves a stale .o behind and the link dies on multiple definitions.
$(ho)/liblove.a: $(h_o)
	@echo 'AR	'$@
	@mkdir -p $(dir $@)
	@rm -f $@; ar rcs $@ $^

# the bootstrap interpreter: -DLoveBoot against the fallback top-level data.h (no
# -I$(ho)), and -Dai_tco=0, which is also the trampoline-coverage lane. it runs the .l
# tools that generate the lcat headers, so it cannot depend on them -- it #includes the
# sed-wrapped $(boot_h) instead, produced without an interpreter. it links the whole
# the host_c glob: the posix nifs and src/image.c's bake/wake are what let love0 bake and
# wake mooncc0.image and so drive the mooncc-built default `love`.
# -DAiVersion='$(love_base)+bootstrap' on purpose, and both halves earn their place.
# the suffix: love0 bakes the lcat headers every frontend shares, so a love0 that relinks
# re-lays all of them and rebuilds every object behind them -- a ~25 s cascade fired by
# nothing but a new commit hash. the bootstrap is not a release artifact; the shipped
# `love` carries the real id (the love.o dep below). the base, though, must be the real
# one: `.comment` writes the pre-+ half of love-version, so love1 (built by love0's
# mooncc) and love2 (built by love1's) agree only if love0 names the same release.
# it moves when ./version moves -- a release, not a commit -- so the cascade stays away.
# -Dai_data_section=0: the bootstrap asks the sentinels by name and owes no linker script.
# both ai_typ bodies answer the same enum d for the same ap, and no layout crosses binaries
# (love.h: the heap image carries an ap as its index).
# -fPIE is the compile half of the -pie link below, said out loud rather than inherited:
# gcc on linux defaults to it, the BSDs' clang does not, and the mismatch is an
# R_X86_64_32S the linker refuses at the very end of a from-scratch bootstrap.
boot_cc = $(CCACHE) $(CC) $(ai_cflags) -fPIE -DLoveBoot -Dai_tco=0 -Dai_data_section=0 -DAiVersion='"$(love_base)+bootstrap"' -I. -Isrc -Iout/lib
# ⚠ src/cats.c is NOT love0's: it bakes the out/lib/*.h headers love0 itself lays, so a
# from-scratch tree has none of them to compile against. love0's boot reads the sed-wrapped
# 0.h twins instead (src/main.c, #ifdef LoveBoot).
love0_host_o = $(patsubst $(R)/%.c,out/host/0/%.o,$(filter-out $(R)/src/cats.c,$(host_c)))
love0_o = $(love0_host_o) $(love_c:$(R)/%.c=out/host/0/%.o)   # pinned (not $(ho)/0)
out/host/0/src/main.o: $(boot_h)
out/host/0/src/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
# the LOVE_NO_IMAGE= prefix (empty = unset) hands the compiler its baked image back from
# under the blanket corpus export: when CC is the dist artifact's own mooncc verb, the verb
# table lives in that image and an egg boot would read "mooncc" as a filename.
# .love0cc content-stamps this compile line, .hostcc's trick one lane over, and the base
# version is why it had to exist: love0's id is now load-bearing (it must name the same
# release a real love does, or .comment differs and test_fixpoint fails at a byte offset
# with nothing to say about the cause). make tracks files, not flag strings, so a ./version
# bump would otherwise leave love0 stamped with the previous release forever.
.PHONY: force_love0cc
force_love0cc: ;
out/host/0/.love0cc: force_love0cc
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; printf '%s\n' '$(boot_cc)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
out/host/0/%.o: $(R)/%.c $(love_h) out/host/0/.love0cc
	@echo 'CC	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(boot_cc) -c $< -o $@
# -pie is load-bearing: love0 bakes mooncc0.image, and the image codec refuses a binary
# whose text sits in its index range -- a PIE loads high and clears it. gcc/clang default
# to PIE anyway; mooncc, the download door's CC, does not.
$(love0): $(love0_o)
	@echo 'LD	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(CC) $(ai_cflags) -pie -o $@ $(love0_o)

# src/love.c -> out/host/*.o
$(ho)/%.o: $(R)/%.c $(love_h) $(ho)/.hostcc
	@echo 'CC	'$@
	@mkdir -p $(dir $@)
	@$(hcc) -c $< -o $@

# l.o carries the version string; recompile it when the id changes. love0's twin is
# deliberately not here -- see the -DAiVersion note on boot_cc.
$(ho)/love.o: out/lib/love_version.h
# the lcat'd headers the frontends bake inline -- src/cats.c takes the egg and the module
# set, src/main.c the CLI and the glaze. one roster for both: the mooncc twin and the HCC
# link below read the same name, and three spellings is how they drift.
baked_h = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/glob.h out/lib/kanren.h out/lib/overlay.h out/lib/scan.h out/lib/re.h out/lib/peg.h out/lib/uu.h out/lib/verbs.h out/lib/distlist.h $(holo_h) $(glaze_h)
$(ho)/src/main.o $(ho)/src/cats.o: $(baked_h)
# the carried-blob reader both the first boot and the kernel's ram fs decode with
$(ho)/src/main.o $(ho)/src/ustar.o: $(R)/src/ustar.h
# src/cb.c rides the crew/quay sources by unity include -- recompile when they move.
$(ho)/src/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h

# ==== the default love is mooncc-built (self-host rung 2) ====
# every TU compiles under love0 waking mooncc0.image, our own nolibc + am math + mksys
# sys.o replace glibc, and holo links it -pie. CC's remaining jobs here are love0 and the
# liblove.a/.so lane, since a shared object wants PIC codegen and a dynamic section holo
# does not lay. HCC=1 takes the $(CC) link below instead -- the foreign-cc differential,
# opt-in, and the only lane that puts one on the vm at ai_tco=1 where ai_musttail is live
# (mk/common.mk says why). one link rule, two names -- `love` and the candidate.
# ⚠ ONE SHAPE, and it was two: a love laid beside the tree compiled it directly, so love0,
# mooncc0.image and the sed-laid 0.h twins were never made there. Nothing lays one now
# (crew/source/source.l), so the self-host circle is always there to break and love0 always wakes
# mooncc0.image to break it. $(CC) builds love0 -- the ambient compiler, or whatever a
# driving love named -- and mooncc builds every object after it.
# boot_love: whoever runs a build-time .l tool. every such site asks by this name rather
# than spelling love0, so the one place that answers stays one place.
boot_love = $(love0)
moon0 = $(love0) wake out/host/mooncc0.image mooncc $(GCDBG)
moon0_dep = out/host/mooncc0.image
moon_d = $(ho)/moon
moon_host_o = $(host_c:$(R)/src/%.c=$(moon_d)/host_%.o)
moon_math_o = $(patsubst crew/moon/lib/math/%.c,$(moon_d)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
# no nolibc object: the link owes its symbols, so the driver's runtime table pulls
# crew/moon/lib/nolibc/ member by need -- a love asking for no calendar and no
# resolver links neither. naming an object would take every member instead.
moon_love_o = $(love_tu:%.c=$(moon_d)/%.o)
moon_o = $(moon_love_o) $(moon_host_o) $(moon_math_o) $(moon_d)/sys.o
# -D AiHaveVersionH + the love_version.h dep: this TU carries the version id into the
# shipped binary, and mooncc has no __has_include for src/love.c's fallback probe to use.
$(moon_love_o): $(moon_d)/%.o: $(R)/src/%.c $(love_h) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -D AiHaveVersionH -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(moon_d)/love.o: out/lib/love_version.h        # only this TU carries the version id
$(moon_d)/host_%.o: $(R)/src/%.c $(love_h) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(moon_d)/host_main.o $(moon_d)/host_cats.o: $(baked_h)
$(moon_d)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(moon_d)/m_%.o: crew/moon/lib/math/%.c $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
# sys.o is laid, not compiled: the syscall trampoline and our sigsetjmp/longjmp have no C
# spelling. love0 runs the lay, its holo carrying every backend. the entry is picked by
# $(hosta), the host's arch, never $a -- a cross lane overrides $a, and this object is
# out/host's, so it is the host's or it is wrong (an aarch64 sys.o laid here dies at the
# link with `link-machine`, one remove from its cause).
ifeq ($(hosta),aarch64)
mksys_e = mksys-arm64
else
mksys_e = mksys
endif
# the backends ride the CAT, all three: sys.o is laid for an arch the cat is
# asked for, and only the baked book's own backend is aboard otherwise -- so a
# cross lay for the third ISA answered `obj-no-backend`. asbook.l first, then
# the backends join the module, then elf/obj: asbook.l's own stated order.
mksys_l = crew/kore/text.l crew/kore/u.l crew/kore/asbook.l \
          crew/holo/amd64.l crew/holo/arm64.l crew/holo/rv64.l \
          crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
# pinned to out/host, like the src.o that reads it: the cat is $(mksys_l) verbatim and
# $(mksys_l) is flavour-neutral, so one cut serves every hsuf. templated on $(ho) it would
# be re-cut per flavour at a fresh path, and out/host/src.o -- a fixed target -- would go
# out of date under each one, leaving the default love stale and the next relink unbaked.
# pinning also keeps this rule reachable from a sub-make whose hsuf is not the default.
out/host/.mksys-cat.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(mksys_l)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
out/host/.mksys-cat.l: $(mksys_l) out/host/.mksys-cat.list
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@cat $(mksys_l) > $@
# the last love0 in the default lane. sys.o is laid by running a love over the mksys cat,
# and naming love0 here was enough to drag the whole bootstrap back in -- love0 wants $(boot_h),
# which used to want tests0.h, the whole corpus through one stdin, where distboot kept dying at
# 139. (The corpus is read now, not baked, so that particular tail is gone.) A bundled love
# lays it just as well: the cat carries holo itself, so the layer needs nothing of the bootstrap.
$(moon_d)/sys.o: out/host/.mksys-cat.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(boot_love) -l out/host/.mksys-cat.l -q -e "((from 'moon '$(mksys_e)) \"$@\")" && test -s $@
ifneq ($(HCC),)
# the HCC flavor is a foreign-cc differential, not the artifact: it links no
# source blob and no readme ($(hcc) knows neither), and dist refuses it.
$(ho)/love $(ho)/love.cand: $(host_o) $(ho)/liblove.a $(ho)/.hostcc $(R)/src/love_data.ld $(baked_h)
	@echo 'LD	'$@
	@mkdir -p $(dir $@)
	@$(hcc) -o $@ $(host_o) $(ho)/liblove.a $(image_ldflags) $(data_ld)
else
# the nolibc sources are a dep of the link, not of any object: the driver compiles
# the members it pulls, so an edit there changes this binary with no .o to notice.
# and the tree is two deep since it went one-function-to-a-file -- all but a handful
# sit under ctype/ dirent/ env/ fmt/ mem/ net/ stdio/ sys/ .., so a one-level glob names
# five of them and every libc edit that matters relinks nothing. it reads as an
# up-to-date binary carrying the code from before the edit. (mk/install.mk's moon_srcs
# is the same glob for the same reason -- keep the two in step.)
nolibc_src = $(wildcard crew/moon/lib/nolibc/*.c crew/moon/lib/nolibc/*.h \
                        crew/moon/lib/nolibc/*/*.c crew/moon/lib/nolibc/*/*.h)
# this link is the seed (doc/misc/dist.md, seed-universal U2): the default binary
# carries its own source blob and readme, and once baked it is the artifact --
# there is no leaner host build for it to subsume anymore.
# the layout stays load-bearing: .image must end the segment for `bake` to
# grow it at the tail (src/image.c's bake_tail refuses otherwise), which it
# does, the blob riding .rodata well below it.
# -freadme rides only this link, so test_fixpoint's relink of $(moon_o) needs
# no mirror of it. out/lib/readme.bin is the page a reader lands on --
# `readelf -p .love.README`, mapped by nothing.
# $(kart_o), the SHIPPED KERNEL's objects (the kernel section below owns the list and
# their rules): the artifact is the fused binary now (plan C2) -- what boots
# on metal is tools/kproject.l's projection of exactly this file.
$(ho)/love $(ho)/love.cand: $(moon_o) out/host/src.o out/host/rt.o out/lib/readme.bin $(nolibc_src)
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moon0) -pie $(moon_o) $(kart_o) out/host/src.o out/host/rt.o -freadme=out/lib/readme.bin -o $@
endif

# the man pages are written in doc/*.md and generated here through the lapiz lens: one
# source, so the roff cannot drift from the prose. a static pattern -- an implicit one
# would make these intermediate and re-run the lens on every build. mkman takes the version
# header as its second word and fills @version@ itself, so the roff needs no sed after.
$(ho)/love.1 $(ho)/cook.1 $(ho)/lush.1: $(ho)/%.1: doc/%.md tools/mkman.l crew/lapiz/lapiz.l out/lib/love_version.h $(ho)/love
	@echo 'LOVE	'$@
	@mkdir -p $(dir $@)
	@$(ho)/love tools/mkman.l doc/$*.md out/lib/love_version.h > $@

# ==== the crew bake and the dist ====
# the layered bake above runs lays the whole crew into out/host/love's own image, so
# `love kore|mooncc|sh|..` is the build tree's spelling exactly as it is the artifact's.
# What remains here: the cat rosters, mooncc0.image (love0's own -- an image keeps its
# binary's layout), the lush/sb PATH scripts, and the dist artifact. It follows the host
# build above, so $(ho) is already spelled; shared vars are mk/common.mk.

# kore: the diff engines, the text/tool surface, the line tools, and `kore` itself -- the
# multi-call toolbox picking its util off the command line or an argv[0] symlink. lush
# rides the cat too, so `kore sh` and a /bin/sh symlink are the shell and the distro's
# one-binary userland closes over its own console. ⚠ lush goes BEFORE cook.l, whose
# $(wildcard) presence-guard then reads sh-glob bare.
# ⚠ the rosters a LATER roster splices sit above it: a prerequisite list expands when
# make READS the rule, so a $(..) still undefined there expands to nothing and the cat
# comes out short a file -- silently, the members that remain being well-formed.
lushfiles = crew/lush/job.l crew/lush/lex.l crew/lush/gram.l crew/lush/glob.l crew/lush/word.l crew/lush/eval.l crew/lush/line.l crew/lush/main.l
# ⚠ awk.l sits with sed.l because it rides re.l; find.l sits AFTER $(lushfiles)
# because it rides lush's fnmatch (sh-match) and a body captures its free names at
# its define -- the same law that keeps kore.l last.
korefiles =crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/sum.l crew/kore/re.l crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/bc.l crew/kore/proc.l crew/kore/less.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l crew/kore/patch.l tools/ain.l $(lushfiles) crew/kore/find.l crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l crew/kore/kore.l
# mooncc is its OWN app, NOT in the kore cat: a cc edit rebuilds only mooncc, so a kore
# rebuild in another session cannot tear the compiler. ⚠ member order is the scope -- the
# u-floor, then asbook splices the boot-registered holo and the CROSS BACKENDS join it
# (defbackend mutates holo's own table, so mooncc cross-compiles every target whichever
# single backend the host image baked), the writers, the compiler proper, then moon.l
# whose tail SEAT fires.
moonfiles = crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/amd64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/rv64.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/moon/val.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l
# ⚠ THE MEMBERSHIP IS AN INPUT AND MAKE CANNOT SEE IT -- the same trap $(ho)/.dist.list and
# out/lib/corpus.list already guard. Moving a file BETWEEN these lists changes what the cat
# holds while every file make watches keeps its mtime, so the cat is "up to date" and the image
# is built from the old set: silently, and it reads exactly like the edit not working. Splitting
# the u-floor out of core.l cost four debug rounds to this, across five different cats.
# Depend on the LIST: rewritten only when membership moves, so the cat re-lays on add OR drop.
$(ho)/.mooncc-cat.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(moonfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
$(ho)/.mooncc-cat.l: $(moonfiles) $(ho)/.mooncc-cat.list
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@cat $(filter %.l,$^) > $@
# sb 🌱 the patch-set vcs (svalbard), and lush 🐚 the love shell -- also the distro's console shell,
# whose SEAT in main.l fires on its own basename. Both are catted shebang scripts, PATH
# picking the love that runs them.
sbfiles = crew/kore/text.l crew/kore/diff.l lib/dns.l crew/sb/merge.l crew/sb/http.l crew/sb/sb.l
$(ho)/sb: $(sbfiles)
$(ho)/lush: $(lushfiles)
$(ho)/sb $(ho)/lush:
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@{ echo '#!/usr/bin/env -S love'; cat $^; } > $@
	@chmod 755 $@
# mooncc0.image: the mooncc cat baked by LOVE0, the build-time compiler that breaks the
# self-host circle -- the default love is mooncc-built, so its own image cannot drive its
# build, and love0 waking this one can. PINNED to out/host like love0 itself. The ONLY
# standalone image left: the default love's crew rides its own .image (the layered bake,
# above), and an image cannot cross binaries anyway.
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo 'LOVE	'$@
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

# ==== dist: the ONE artifact (self-host rung 3; seed-universal U2) ====
# the seed IS the default binary: out/host/love links the moon objects plus its
# own source blob and readme (the host build section carries the link), and the layered
# bake lays the crew warm -- cook + kore + lush (vi and ain ride its cat) +
# mooncc (all five backends) + sb + kiosko -- each pinning its own name into the
# verb table love/cli.l's rail reads: `love sb|cook|kore|kiosko|mooncc ..` are
# the same binary being multi-call. there is no leaner host build beside it and
# no love-<arch> twin: one tree, one binary, and `make dist` is that binary
# plus the source tarball. the per-ISA bytes remain (a binary is for one
# machine until U1's container); it is the artifact NAMES that dissolved.
# member order is the scope: kore's floor first, asbook before the backends
# (defbackend mutates the spliced holo), every main before kore.l's applet table.
distfiles = crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/sum.l crew/kore/re.l \
            crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/bc.l crew/kore/proc.l crew/kore/less.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
            crew/vi/core.l crew/vi/vi.l \
            crew/kore/diff.l crew/kore/patch.l lib/dns.l tools/ain.l $(lushfiles) crew/kore/find.l \
            crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/amd64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/rv64.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/holo/copy.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
            crew/moon/val.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l crew/kore/kore.l crew/sb/merge.l \
            crew/sb/http.l crew/sb/sb.l crew/kiosko/kiosko.l \
            lib/gz.l lib/tar.l crew/tar/tarcmd.l crew/gz/gzcmd.l lib/cpio.l \
            crew/cpio/cpiocmd.l crew/source/source.l crew/lapiz/lapiz.l \
            lib/salt.l crew/libra/libra.l lib/hueweb.l lib/serve.l
# ⚠ THE MEMBERSHIP IS AN INPUT, and make cannot see it. Adding a file to distfiles
# changes what the artifact CARRIES while every file make watches keeps its mtime, so
# a cat older than the new member is "up to date" and the binary links without it --
# silently, and it looks exactly like the feature not working. (the lib section's
# corpus.list is the same guard for $t, and for the same reason.) Depend on the LIST:
# rewritten only when membership moves, so the cat re-lays on an add OR a delete.
# ⚠ the cat lives in $(ho): it is the DEFAULT binary's own bake load now
# (love.baked, above), and the dist lanes read the same file -- one roster,
# one set of bytes, so the tree binary and the artifact cannot drift.
.PHONY: force_dist_list
force_dist_list: ;
$(ho)/.dist.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(distfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
$(ho)/.dist-cat.l: $(distfiles) $(ho)/.dist.list
	@echo 'CAT	'$@
	@mkdir -p $(dir $@)
	@cat $(distfiles) > $@
# the same roster, baked into the binary: the FIRST BOOT (src/main.c) cats the
# members off the carried source blob exactly as the rule above does off the tree.
# ⚠ CONTENT-STAMPED, because the roster's home is now this file: a prerequisite on the
# whole Makefile would otherwise relay on every edit to it, and korelist.h below is a
# prerequisite of every kernel object. cmp keeps the mtime where the bytes did not move.
out/lib/distlist.h: Makefile
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; printf '"%s"\n' '$(distfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
.PHONY: dist dist-source dist-seed

# ==== THE RELEASE ARTIFACTS (doc/misc/dist.md) ====
# A release is TWO THINGS, and they sit on the one axis that actually matters to
# somebody who just downloaded one: do you have a C toolchain?
#
#   SOURCE   love-<ver>.tar.gz   sources only. `make` bootstraps through the local
#                                cc, which builds love0 and NOTHING else.
#   SEED     love                one executable that CARRIES its own source and IS
#                                its own toolchain -- the tree's own out/host/love,
#                                baked. `love source` lays the tree with bin/love
#                                already in it; `make` there calls no ambient
#                                compiler at all.
#
# ⚠ AND THEY ANSWER THE SAME BINARY, which is the whole claim and is not a thing we
# had to engineer: the local cc only ever builds `love0` (the host build section), and every
# object in the shipped binary is mooncc's, compiled by love0 waking mooncc0.image.
# So the bootstrap compiler is a scaffold that leaves no trace in the product --
# which is exactly what test_fixpoint already asserts to the byte, and what its DDC
# leg (a foreign-compiled love0) was audited for. `make test_distboot` is that claim
# stated over the artifacts rather than over the tree.
#
# ⚠ THERE WAS A THIRD, and it is retired: a FULL tarball, the source tree with a
# baked love laid into bin/. The seed does that job strictly better -- one file
# instead of an archive, nothing to unpack it with, and the same bytes at the far
# end -- so the fat tarball was a second way to say what the seed already says, and
# a third leg of every release gate to keep honest.
#
# The archive is OURS end to end -- lib/tar.l and lib/gz.l -- so cutting a release
# needs neither `tar` nor `gzip` on the box, and crew/gz/gzcmd.l wears their flags for a
# hand. Our coder costs every block three ways and writes the cheapest; it lands a
# few percent above `gzip -9` (lib/gz.l carries the numbers).
#
# ⚠ REPRODUCIBLE BY CONSTRUCTION: the pack pins every mtime/uid/gid to $(dist_stamp)
# and the gzip header's own MTIME is 0, so two cuts of one tree are the same bytes
# and "this is that release" is something anyone can check with sha256sum.
# the id is ./VERSION, the whole of it -- the tarball is named for it AND ships it,
# and no version control is consulted anywhere in the cut: the tree on disk is the
# source, and the archive's bytes are a function of it and nothing else.
dist_ver  := $(love_base)
dist_stamp ?= 0
dist_source = out/dist/love-$(dist_ver).tar.gz
dist-source: $(dist_source)
# the seed is the tree's own binary, baked -- one file, no second name. the HCC
# flavor is a foreign-cc differential, not the artifact, so dist refuses it.
ifneq ($(HCC),)
dist-seed:
	$(error dist: the HCC flavor is a differential, not the artifact -- drop HCC=)
else
dist-seed: $(ho)/love.baked
endif
dist: dist-source dist-seed   # a release is both

# ⚠ wasm/love.js is EMSCRIPTEN'S OUTPUT, committed so github pages can serve a repl
# (wasm/Makefile calls it "the COMMITTED artifact"). It stays in the repo for exactly
# that, and stays OUT of the release: 103 KB gz in one file -- more than both generated
# proof terms together -- and it is the one thing here a reader could not regenerate
# without installing a foreign toolchain, inside an artifact whose whole claim is that
# it needs none. Dropping it from the TARBALL costs the repl nothing.
#
# bench/ goes the same way, and for the same reason read from the other side: it is
# 209 files and ~1 MB of OTHER LANGUAGES -- one implementation of each workload in
# go, rust, java, julia, lua, python, js, elixir, scheme and lisp -- to time love
# against, plus a committed results page. Nothing in the build reaches it and nobody
# rebuilding love needs it; a reader who wants the numbers wants the repo. ⚠ the drop
# is a top-segment name, so the WALK prunes it and the tree under it is never read.
#
# port/ and wasm/ leave on the same test, applied to a RELEASE and not to a checkout:
# what does `make` in the unpacked tree reach? the default goal there is `dist`, which
# is dist-source + love.baked, and neither reads either directory. port/ is six
# bare-metal boards whose gates are opt-in BY NAME (test_mps2, test_playdate, ..) and
# which want a cross toolchain and hardware the tarball's reader has not got; wasm/ is
# an emscripten build plus its committed output, and emscripten is precisely the
# foreign toolchain this artifact exists to not need. ⚠ `wasm/love.js` came off the
# list because the directory holding it now goes -- one name, not two.
#
# ⚠ doc/ IS NOT ON THIS LIST and cannot be, because of one rule: the man pages are
# WRITTEN in doc/{love,cook,lush}.md and generated from them (the host build section), and
# `install: $(installs)` names all three -- so an unpacked release with no doc/ builds
# its binary and then dies on `make install` with no rule to make doc/love.md. so doc/
# is the three man sources and nothing else, and everything that used to sit beside
# them -- the design record, the arcs, the sketches -- is doc/misc, which selfpack.l
# skips on its own (sp-inner). a directory named misc does not need a release policy
# to know it is not the product.
dist_drop = bench port wasm
# THE ARCHIVE IS THE TREE: selfpack walks the root and skips only what is not
# source -- out bin dl, everything HIDDEN at the root (.git .sb .claude .cache
# .gitignore .., the machine's and the checkout's), and $(dist_drop) -- pins every
# mtime to $(dist_stamp) and sorts, so the bytes are a function of the tree's
# CONTENT and of nothing else. No index, no stage, no version control: the files
# you are looking at are the files the artifact carries, checkout and seed-laid
# tree alike -- which is what lets the rebuild answer the same bytes anywhere.
# ⚠ a FORCE rule, the corpus.list pattern: selfpack runs every make, walks and
# hashes (cheap, and the hash sees a delete or rename that leaves no mtime), and
# writes the archive ONLY when the tree's content moved -- so its mtime holds and
# nothing downstream re-links on a touch or a no-op.
# the runner is $(boot_love) -- the bundled love, or the egg this make built.
# The archive is f(tree) and never f(runner), so the only question is which
# binary runs it, and the egg wins twice over: gcc's codegen through deflate's
# and sha's array loops takes a THIRD of the instructions mooncc's does, and
# glibc hands back the pages that nolibc's free never munmaps -- 1.2 s / 148 MB
# against the tree's own love at 2.2 s / 627 MB, same bytes out.
# ⚠ and it is always CURRENT, which the tree's own love need not be: a love
# older than a nif the packer reaches for answers `missing', and the tree cannot
# relink its way out, since src.o carries this archive -- the binary that would
# fix the runner would need the runner first.
# Never a dependency edge: the seed EMBEDS this archive, so the archive must
# exist before the binary can link.
.PHONY: force_src
force_src: ;
$(dist_source): force_src $(love0)
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= LOVE_BUDGET_MB=256 $(boot_love) tools/selfpack.l $@ love-$(dist_ver) $(dist_stamp) $(dist_drop)

# THE SOURCE BLOB: the source tarball laid into an object (tools/mksrc.l), so the
# artifact hands out its own source with no second download and no `tar xf` -- love
# `source` inflates it. src/src.c defines the pair WEAK and empty, so this object's
# STRONG definitions override them at the link and a plain `make host` needs none of
# it. ⚠ holo names its arches (uname's and holo's disagree on x86_64).
# ⚠ THE BLOB FOLLOWS $(hosta), NOT $a. Only the host link takes this object, and the
# path is arch-neutral -- so reading $a lets `make kernel a=aarch64` lay an arm64 blob
# at out/host/src.o and every later host link dies on link-machine. The x-lane cuts
# its own $(xd)/src.o for the cross road.
# ⚠ AND THESE TWO RULES MUST SIT BELOW $(dist_source)'s DEFINITION. A prerequisite
# list is expanded where it is WRITTEN: above the definition it expands to nothing,
# make never builds the tarball, and only the recipe -- expanded later, when the
# variable is set -- names a file that was never cut. It fails as a missing archive,
# which reads as the tarball rule being broken rather than this line being early.
# ⚠ flat ifeqs, no else-chain: cook reads `else ifeq` as a bare else and drops the
# condition, so a chain picks the wrong arch under the seed's own make.
src_arch = amd64
ifeq ($(hosta),aarch64)
src_arch = arm64
endif
ifeq ($(hosta),riscv64)
src_arch = rv64
endif
# ⚠ mksrc rides the mksys cat (kore + holo elf/obj), NOT (use 'holo): the
# module walk resolves off a NEST, and a fresh seed tree has none. same lane
# as sys.o, live in both worlds. PINNED to out/host: the blob is the tree's,
# not a compiler flavor's, and only the moon link takes it.
out/host/src.o: $(dist_source) tools/mksrc.l out/host/.mksys-cat.l $(love0)
	@$(boot_love) -l out/host/.mksys-cat.l tools/mksrc.l $(dist_source) $@ $(src_arch)

# THE CARRIED RUNTIME: each hosted ISA's compiled nolibc archive, raw and
# laid beside the source blob (tools/mkrt.l lays, moon.l's rtcarried
# consumes), so a bare `love cc` links without compiling 197 members first.
# RIDES THE MOONCC IMAGE by wake -- the archives ARE mooncc compiles, so the
# whole compiler must be aboard -- and the image dep also re-cuts them when
# the COMPILER moves, keeping a binary and its carried archives cut together.
# deterministic all the way down, so the fixpoint carries the object unchanged.
rt_slice = $(wildcard crew/moon/include/*.h crew/moon/include/*/*.h \
                      crew/moon/lib/*.l \
                      crew/moon/lib/nolibc/*.c crew/moon/lib/nolibc/*.h \
                      crew/moon/lib/nolibc/*/*.c crew/moon/lib/nolibc/*/*.h \
                      crew/moon/lib/math/*.c)
# ⚠ TWO LANES, like moon0's: the bundled seed rides its own baked moon (and
# its rtarch consult finds the binary's own carried archives -- the laid tree
# hashes to their stamp, so a fresh-box seed COPIES them instead of compiling
# 3 x 197 members); only the git-clone lane wakes mooncc0.image, which is the
# one lane love0 was ever owed. dragging that dep into the bundled lane cost
# both box trophies once: love0's C compile met netbsd's gcc and its own
# linux-isms.
out/host/rt.o: $(rt_slice) tools/mkrt.l out/host/mooncc0.image $(love0)
	@$(love0) wake out/host/mooncc0.image tools/mkrt.l $@ $(src_arch)

# ==== the x-lane: test_xfixpoint's objects (seed-universal U0) ====
# there is ONE artifact; this lane builds no second one. it compiles the tree's
# TUs through `mooncc -t` for another arch so the cross-machine fixpoint gate
# can link them and prove, under qemu-user, that mooncc's output does not
# depend on the arch mooncc runs on. gate machinery, never a product.
# THE ROSTER, one row per arch the gate can effigy: the mooncc target, the
# qemu-user that runs it, and the mksys leaf that lays its machine tail.
# the triple word make speaks (a cross toolchain's prefix is spelled that way) -> the
# canonical ISA nom everything above make uses. love/prel.l's arch-canon, in make's clothes.
xtgt_x86_64   = amd64
xtgt_aarch64  = arm64
xtgt_riscv64  = rv64
xqemu_x86_64  = qemu-x86_64
xqemu_aarch64 = qemu-aarch64
xqemu_riscv64 = qemu-riscv64
xmksys_x86_64  = mksys
xmksys_aarch64 = mksys-arm64
xmksys_riscv64 = mksys-riscv
# which arch: `make xa=riscv64 test_xfixpoint` names one, and the default is
# the other member of the two the host is not.
xa ?= $(if $(filter aarch64,$a),x86_64,aarch64)
xtgt   = $(xtgt_$(xa))
xqemu  = $(xqemu_$(xa))
xmksys = $(xmksys_$(xa))
ifeq ($(xtgt),)
$(error x-lane: no such arch `$(xa)' -- the roster carries x86_64 aarch64 riscv64)
endif
# ⚠ PER-ARCH, because the objects are: one shared dir let a riscv64 love.o stand as
# up-to-date for an aarch64 link, and the mismatch shows only at the far end.
xd = out/x-$(xa)
moonx = $(moon0) -t $(xtgt)
xhost_o = $(host_c:$(R)/src/%.c=$(xd)/host_%.o)
xmath_o = $(patsubst crew/moon/lib/math/%.c,$(xd)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
# no nolibc.o: the link owes its symbols and the driver pulls the members by need
# (crew/moon/lib/nolibc/), so a dist takes no calendar and no resolver.
# ⚠ ALL SEVEN love TUs, the host lane's moon_love_o worn at $(xa): love.c became seven
# files and a rule naming one of them links 191 undefined noms -- the twin has to take the
# set, not the name that used to be the set.
xlove_o = $(love_tu:%.c=$(xd)/%.o)
xobjs = $(xlove_o) $(xhost_o) $(xmath_o) $(xd)/sys.o
# -D AiHaveVersionH like the host lane (build.mk's love.o): mooncc has no
# __has_include, so the flag is the only door to the version header.
$(xlove_o): $(xd)/%.o: $(R)/src/%.c $(love_h) out/host/mooncc0.image
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -D AiHaveVersionH -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(xd)/love.o: out/lib/love_version.h            # only this TU carries the version id
# ..and the lcat headers these two #include, which the host lane names and this one did
# not: from a FRESH tree the cross target reached cats.c before out/lib/egg.h existed.
$(xd)/host_main.o $(xd)/host_cats.o: $(baked_h)
$(xd)/host_%.o: $(R)/src/%.c $(love_h) out/host/mooncc0.image
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Isrc -Iout/lib -c $< $@
$(xd)/host_main.o: $(baked_h)
$(xd)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(xd)/m_%.o: crew/moon/lib/math/%.c out/host/mooncc0.image
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@$(moonx) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
$(xd)/sys.o: out/host/.mksys-cat.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p $(dir $@)
	@$(love0) -l out/host/.mksys-cat.l -q -e "((from 'moon '$(xmksys)) \"$@\")" && test -s $@

# ==== the fat container (seed-universal U1) ====
# the twin SEED: the x-lane link wearing the artifact's clothes -- its own src
# blob and readme, so the member answers `love source` like the native one.
$(xd)/src.o: $(dist_source) tools/mksrc.l out/host/.mksys-cat.l $(love0)
	@$(boot_love) -l out/host/.mksys-cat.l tools/mksrc.l $(dist_source) $@ $(xtgt)
$(xd)/rt.o: $(rt_slice) tools/mkrt.l out/host/mooncc0.image $(love0)
	@$(love0) wake out/host/mooncc0.image tools/mkrt.l $@ $(xtgt)
# $(xkart_o), the twin's kernel objects, in the host link's own order -- the
# artifact is fused, so an egg without them is not the binary the far machine
# rebuilds. the kernel section below owns the list and the prereq line; a recipe expands
# late, so reading it here is enough.
$(xd)/love: $(xobjs) $(xd)/src.o $(xd)/rt.o out/lib/readme.bin
	@echo 'MOON	'$@
	@$(moonx) -pie $(xobjs) $(xkart_o) $(xd)/src.o $(xd)/rt.o -freadme=out/lib/readme.bin -o $@
# dist-fat -- OPT-IN: ONE file, both texts, behind fatpack's sh prefix and its
# content-named cache under ~/.love/fat. the native member rides baked; the
# twin is an egg until U1.2 moves the bake to the extraction.
fat = out/dist/love-fat
.PHONY: dist-fat
dist-fat: $(ho)/love.baked $(xd)/love tools/fatpack.l
	@mkdir -p out/dist
	@$(boot_love) tools/fatpack.l $(fat) $a $(ho)/love $(xa) $(xd)/love
	@chmod +x $(fat)

# ==== the vim syntax for .l -- GENERATED, so there is no copy to keep up to date ====
# tools/hue2vim.l reads crew/vi/hue.l's class table the other way round (one table, two
# readers: the painter in vframe and vim) and asks THIS host for its vocabulary -- so the
# file describes the love you built, which makes it an artifact like any other. It lives
# under out/ for that reason: a checked-in copy can be stale, a built one cannot.
# mk/install.mk installs it beside assets/vim/'s two hand-written siblings.
# ⚠ LOVE_NO_IMAGE is CLEARED. Under it the egg's mop never runs and the compiler's own
# internals (`book` among them) are still on the book; the syntax file describes the
# SHIPPED language, so the generator gets the shipped boot -- and hue2vim.l refuses
# outright rather than freeze build state and call it the language.
# ⚠ atomic, for $(lcat_h)'s reason: a bare `> $@` truncates first, so a broken generator
# would leave a 0-byte syntax file make calls up to date.
huefiles = crew/vi/config.l crew/vi/hue.l tools/hue2vim.l
$(ho)/syntax.vim: $(huefiles) $(m)
	@echo 'HUE	'$@
	@mkdir -p $(dir $@); t=$@.$$$$.tmp; \
	  cat $(huefiles) | env -u LOVE_NO_IMAGE $(m) > $$t && test -s $$t && mv -f $$t $@ \
	    || { rm -f $$t; echo "FAIL: $@ empty (hue2vim.l failed)"; exit 1; }
.PHONY: syntax
syntax: $(ho)/syntax.vim
include $(R)/mk/distro.mk

# ==== the freestanding kernel ====
# ./Makefile from the project root, so paths resolve from there; shared vars are mk/common.mk.
#
# Arch-independent glue is free/{kmain.c,k.h}, per-arch code free/<a>/. Each
# arch brings itself up under `qemu -kernel` with no bootloader or firmware at all (the
# PVH stub on x86_64, the EL1 MMU stub on aarch64, both laid by mkboot.l); free/uefi/'s
# own BOOTX64.EFI is the second door, and the one that hands over a framebuffer.
ko = out/free
# downloaded, not built -- so it lives OUTSIDE out/ and `make clean` leaves it standing.
# `make distclean` is the one that asks for the network again.
dl = dl

# every gate and verb below is phony: one roster, so adding one is one line and not two.
.PHONY: force_kfs_list kmain_o run run-$a run-sh run-headless init-container \
  uefi test_arm64 test_kernel test_disk test_uefi test_uefi_arm64 test_kboot test_kverb test_kernel_arm64 \
  test_inle test_wasm

# K_TEST=1 builds a headless serial test kernel (batch read-eval over COM1, with an
# `exit` nif that quits qemu) into its own odir and elf, so it never clobbers the
# normal interactive one.
ifdef K_TEST
ksuf := -test
endif

# The COMPILER is ours, and only ours: mooncc compiles every TU, holo lays the assembly
# and links. KCC names WHICH love drives it, not which compiler -- a foreign cc has no
# lane here (dropped 2026-08-19: HCC covers foreign-cc on the host and ccbench races them
# over the same TUs, so a second kernel compiler earned nothing it did not already cost).
# ⚠ mooncc is love's own verb now (the layered bake, doc/misc/plan/one-binary.md), and the
# LOVE_NO_IMAGE= clear is load-bearing (the guard against an exported egg): an
# egg-booted love has no verb table -- `mooncc` would read as a filename.
KCC ?= LOVE_NO_IMAGE= $(ho)/love mooncc

k_arch_c = $(wildcard $(R)/src/$a_*.c)
k_free_c = $R/src/kmain.c $R/src/blk.c $R/src/sys.c
# THE WHOLE HOST SURFACE rides the kernel now (plan C2): the fused -pie link
# is one object set, host frontend included -- posix.c's nifs (A3), seat.c's
# plumbing (B2), and main.c with the rest, every libc call bottoming out in
# src/sys.c's table. the spawn family registers and refuses at runtime, the
# boot text's task shim shadows those names, and quit/getpid branch to their
# k_lvm_ twins on a negative osv. ⚠ no quay.c here: cb.c carries it by unity
# include, exactly as the host link does.
k_host_c = $(patsubst %,$R/src/%.c,main cats cb image mem hash sock tls deflate inflate src posix seat ustar)
k_quay_c = $R/crew/quay/cga_8x8.c $R/crew/quay/moderndos_8x16.c $R/crew/quay/paint.c
k_shared_c = $(love_c) $(k_quay_c) $(c_c)
k_h = $(love_h) $(R)/src/k.h $(R)/src/ustar.h $(wildcard *.h $(R)/src/$a_*.h)

k_odir = $(ko)/$a$(ksuf)
k_elf = $(ko)/love-$a$(ksuf).elf
k_pie = $(k_odir)/love.pie

k_shared_o = $(k_shared_c:$(R)/%.c=$(k_odir)/%.o)
k_arch_o = $(k_arch_c:$(R)/%.c=$(k_odir)/%.o)
k_free_o = $(k_free_c:$(R)/%.c=$(k_odir)/%.o)
k_host_o = $(k_host_c:$(R)/%.c=$(k_odir)/%.o)
# the two LAYS (holo IR written in love, free/mk{boot,vec}.l -- no assembler
# runs in this build). vec.o is an ordinary object and rides the pie; boot.o
# is the bring-up, and its 32-bit stub carries abs32 sites a pie cannot
# slide -- it stays out of the link and the PROJECTION lays and patches it.
k_lay_o = $(k_odir)/$a/vec.o
k_boot_o = $(k_odir)/$a/boot.o
# the mksys machine tail, the same object the hosted link carries: __ai_call
# compiles with both doors now, so the raw `syscall`/`svc` leaf must resolve --
# dead on metal (a negative osv takes __ai_inle first), and it brings the seat
# symbols the stubs used to fake (__ai_sigret, the netbsd leaves).
k_tail_o = $(k_odir)/$a/sys.o
# the runtime archive slice (crt0 and kin), laid per arch like the cross lane's
k_rt_o = $(k_odir)/rt.o
# the shipped odir pie (a CROSS arch's kernel) carries its own arch-tagged
# source blob -- the blob IS the initrd now (kmain.c's k_untar). the K_TEST
# pie stays blobless (src.c's weak zero) and keeps the lcatfs bake instead.
ifndef K_TEST
k_src_o = $(k_odir)/src.o
endif
k_o = $(k_shared_o) $(k_arch_o) $(k_free_o) $(k_host_o) $(k_lay_o) $(k_tail_o) $(k_rt_o) $(k_src_o) $(k_doom_o)

# The kernel runs the GENERATIONAL collector bounded by g->budget: kmain sums the boot
# memmap into kram_words and sets budget = kram_words/8 after ai_ini (the Appel knob).
# ⚠ unbounded, the nursery's copy-overhead resizer grows until gen_major's all-survive
# sizing asks kmallocw for a block bigger than any physical RAM range. gen_please, src/love.c.
# NO FLAGS BEYOND -I/-D/-t, and none are missing (plan C1): mooncc hears
# -c -o -I -D -t -os -std= -Ttext/-Tdata -fno-inline -pie -freadme and
# -ffreestanding, and tolerates-and-discards the traditional soup (-g -O -W*
# and the -f family) -- verified by byte-identical objects. even
# -ffreestanding is gone: the kernel compiles HOSTED, the same line as the
# host's moon lane, because love.c's one hosted/metal fork (the W^X code
# arena) branches on __ai_osv at run time now, and the mmap family it then
# links answers -ENOSYS through the same door as everything else.
kcflags =
kcppflags := \
  -I$(k_odir) \
  -I. -Isrc -I$(R)/out/host -Iout/lib -I$(R)/crew/quay -I$(R) \
  -I$(R)/crew/moon/include \
  $(kcppflags)
ifdef K_TEST
# tail-threaded, matching the real kernel and the host; love0 stays the trampoline lane.
kcppflags += -DK_TEST -Dai_tco=1
endif
# no machine flags: `-t` names the backend and the -m* soup is vacuous for our codegen --
# nothing of ours ever lives below sp (no red zone to disable), and we emit abs64 and
# pc-relative relocations and nothing else, so the top 2 GiB needs no code model.
# ⚠ mooncc REFUSES a -m flag rather than ignoring it (dropping one silently would be the
# no-op wearing a cc face), which is the other reason there are none to pass.
kcc = $(KCC) $(kcflags) $(kcppflags) -t $(k_be_$a)
# ours has to exist before it can compile anything.
kcc_dep = $(ho)/love.baked

kernel: $(k_elf)

# THE LINK IS THE HOST'S OWN (plan C2): one mooncc -pie over the whole object
# set, the same lane that links out/host/love. what the doors eat is the
# PROJECTION of that pie -- tools/kproject.l re-bases every PT_LOAD at the
# kernel base, applies the love_rela table there (the law nolibc's __ai_reloc
# runs at a hosted start, run ahead of time), lays and patches boot.o below
# the image, writes k_image_top, and emits the flat ELF all three doors have
# always booted -- the note, the entry by symbol, paddr = vaddr.
# cb.o rides the quay sources by unity include; the baked cats reach every object
# through the pattern rule below.
$(k_odir)/src/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(k_odir)/rt.o: $(rt_slice) tools/mkrt.l $m
	@echo 'LOVE	'$@
	@mkdir -p "$(dir $@)"
	@$m tools/mkrt.l $@ $(k_be_$a)
$(k_odir)/src.o: $(dist_source) tools/mksrc.l out/host/.mksys-cat.l $m
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $m -l out/host/.mksys-cat.l tools/mksrc.l $(dist_source) $@ $(k_be_$a)
$(k_pie): $(k_o) $m
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(KCC) -pie -t $(k_be_$a) $(k_o) -o $@
kproject_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/tools/kproject.l
$(k_odir)/kproject.list: force_dist_list
	@mkdir -p "$(dir $@)"
	@tf=$@.$$$$.tmp; echo '$(kproject_l)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
$(k_odir)/kproject.l: $(kproject_l) $(k_odir)/kproject.list
	@echo 'CAT	'$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(kproject_l)); } > $@

# THE SHIPPED KERNEL IS THE ARTIFACT'S PROJECTION -- one binary, worn two
# ways; test_kboot boots exactly what `make` installs. the TEST kernel keeps
# its own pie (the corpus rides its kmain), and a cross arch keeps the odir
# pie (no artifact of that arch stands here).
k_pie_in = $(k_pie)
k_pie_dep =
ifndef K_TEST
ifeq ($a,$(hosta))
k_pie_in = $(ho)/love
# the projection carries the baked image, so it must follow the in-place bake
k_pie_dep = $(kcc_dep)
endif
endif
$(k_elf): $(k_odir)/kproject.l $(k_pie_in) $(k_pie_dep) $(k_boot_o) $m
	@echo 'KPROJ	'$@
	@mkdir -p "$(dir $@)"
	@$m $(k_odir)/kproject.l $(k_pie_in) $(k_boot_o) $@ $a && test -s $@

# --- the initrd ------------------------------------------------------
# lib/*.l baked per-file into .rodata as {path, bytes, len} rows (tools/lcatfs.l), which
# the ramfs in kmain.c serves reads off. Paths are baked RELATIVE, exactly as the readers
# spell them: test/kernel's disk.l and ramfs.l say (use "lib/fat.l") and (use "lib/json.l"),
# which the ramfs answers the moment `open` sits in defs[]. ⚠ the .list stamp is corpus.list's idiom -- a wildcard aggregate leaves every
# remaining prereq older than the target when a file is DELETED, and make bakes the ghost.
kfs = $(sort $(wildcard $R/lib/*.l))
force_kfs_list: ;
out/lib/kfs.list: force_kfs_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$(kfs)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
out/lib/kfs.h: $(kfs) out/lib/kfs.list $(love0) tools/lcatfs.l love/prel.l
	@mkdir -p out/lib
	@echo 'LOVE	'$@
	@$(love0) -l love/prel.l tools/lcatfs.l $(kfs:$R/%=%) > $@

# --- the kore roster (rung 3) ----------------------------------------
# the blob initrd carries every member, so the SHIPPED kernel bakes only the
# ORDER: $(korefiles) (the crew section, folded in first) as one love string, and
# the boot text cats the members off the ramfs. the K_TEST kernel skips it --
# its corpus bakes the kore subset it drives.
out/lib/korelist.h: Makefile
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; printf '"%s"\n' '$(korefiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi

# Shared C sources (src/love.c, crew/quay/, nolibc's six) + per-arch free/<a>/.
# Under K_TEST kmain.c #includes the baked corpus out/lib/ktests.h.
$(k_odir)/%.o: $(R)/%.c $(k_h) $(kcc_dep) $(baked_h) $(if $(K_TEST),out/lib/kfs.h out/lib/ktests.h,out/lib/korelist.h)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

# kmain_o -- the kernel frontend, COMPILED AND NOTHING MORE, at whatever arch and face the
# caller's `a=` / `K_TEST=` say. test_embed asks for it three ways; the odir is spelled here
# so a caller never re-derives it (KCC decides half of it).
kmain_o: $(k_free_o)

# --- THE ARTIFACT CARRIES THE KERNEL (plan C2, the unification) --------------
# out/host/love's link gains the SHIPPED kernel for the HOST's arch: kmain
# (the kore cats aboard), the ramfs and rows, the syscall door, the arch
# bring-up and the vector lay. boot.o stays out (the projection's). these ride
# the MOON LANE -- moon0-compiled, the vec lay under boot_love -- because a
# seed builds the artifact before any $m exists; KCC has no seat here.
# spelled at $(hosta), never $a: a cross `make kernel a=..` must not move the
# host artifact. the prereqs live here (kernel.mk owns the shape); the link
# the host link recipe reads $(kart_o) at run time, where it is defined.
kart_inc = -I$(ho) -I. -Isrc -Iout/lib -I$R \
  -I$R/crew/quay -I$R/crew/moon/include
kart_h = $(love_h) $(R)/src/k.h $(R)/src/ustar.h $(wildcard $(R)/src/$(hosta)_*.h)
# kmain.c's own bake is the kore ROSTER now; the egg and the module set are src/cats.c's,
# and that object rides the host lane above.
kart_bake = out/lib/korelist.h
kart_arch_o = $(patsubst $R/src/%.c,$(moon_d)/k_%.o,$(wildcard $R/src/$(hosta)_*.c))
# the console's painter and its fonts: kernel-only draws the host link never had
kart_quay_o = $(patsubst %,$(moon_d)/k_q_%.o,paint cga_8x8 moderndos_8x16)
# the twin link takes the same set at $(xa) -- $(xkart_o), below the lays
kart_o = $(moon_d)/k_kmain.o $(moon_d)/k_blk.o $(moon_d)/k_sys.o $(kart_arch_o) $(kart_quay_o) $(moon_d)/kvec.o

# --- the doom lane (DOOM=1) -------------------------------------------------
# OPT-IN and absent from every default build: doomgeneric is not ours and not in
# this tree, so the lane wants the source at dl/doomgeneric and the IWAD at
# dl/doom1.wad -- test_cts's dl/c-testsuite posture, for the same reason. absent
# either, the lane names the missing file as a missing prerequisite.
#
#   git clone --depth 1 https://github.com/ozkl/doomgeneric.git dl/doomgeneric
#   curl -Lo dl/doom1.wad <the shareware IWAD>
#   make run DOOM=1
#
# src/doom.c answers doomgeneric's four platform doors off the framebuffer, the
# scancode tap and the clock; the WAD rides kmain.c's k_baked hook into the
# ramfs, so doom's own fopen finds it. ⚠ the FRAMEBUFFER door is the ESP one:
# `qemu -kernel` hands over none, so `make run DOOM=1` (UEFI) is the lane and
# run-sh is not. the vendored platform backends carry their own main and stay
# out of the build.
ifdef DOOM
doom_d = $R/dl/doomgeneric/doomgeneric
# ⚠ ONE `%` per filter-out pattern, so the drops are spelled as a wildcard of
# their own: the platform backends (each carries a main) and the two library
# sound lanes, which want allegro and SDL headers nothing here has.
doom_drop = $(wildcard $(doom_d)/doomgeneric_*.c $(doom_d)/i_allegro*.c $(doom_d)/i_sdl*.c)
doom_c = $(filter-out $(doom_drop),$(wildcard $(doom_d)/*.c))
k_doom_o = $(patsubst $(doom_d)/%.c,$(k_odir)/doom/%.o,$(doom_c)) $(k_odir)/doom/wad.o
k_free_c += $R/src/doom.c
kcppflags += -I$(doom_d)
$(k_odir)/doom/%.o: $(doom_d)/%.c $(kcc_dep)
	@echo 'DOOM	'$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
$(k_odir)/doom/wad.o: $R/dl/doom1.wad tools/mkblob.l out/host/.mksys-cat.l $m
	@echo 'MKBLOB	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $m -l out/host/.mksys-cat.l tools/mkblob.l $< $@ doom_wad $(k_be_$a)
# and the same set on the KART lane, which is where the host's own kernel is
# built (plan C2: the artifact carries it) -- so `make kernel DOOM=1` at $(hosta)
# rides these and the cross odir rides the rows above.
kart_inc += -I$(doom_d)
kart_doom_o = $(patsubst $(doom_d)/%.c,$(moon_d)/kd_%.o,$(doom_c)) \
  $(moon_d)/kd_wad.o $(moon_d)/k_doom.o
kart_o += $(kart_doom_o)
$(moon_d)/kd_%.o: $(doom_d)/%.c $(moon0_dep)
	@echo 'DOOM	'$@
	@mkdir -p "$(dir $@)"
	@$(moon0) $(kart_inc) -c $< $@
$(moon_d)/kd_wad.o: $R/dl/doom1.wad tools/mkblob.l out/host/.mksys-cat.l $(love0)
	@echo 'MKBLOB	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $(boot_love) -l out/host/.mksys-cat.l tools/mkblob.l $< $@ doom_wad $(k_be_$(hosta))
endif

$(moon_d)/k_%.o: $R/src/%.c $(kart_h) $(kart_bake) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moon0) $(kart_inc) -c $< $@
$(moon_d)/k_q_%.o: $R/crew/quay/%.c $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moon0) $(kart_inc) -c $< $@
# the vector lay, under whatever love a fresh tree has (mksys's own idiom)
$(moon_d)/kvec.o: $(ko)/$(hosta)/mkvec.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $(boot_love) -l $< -q -e '(lay-vec "$@" "$(hosta)")' && test -s $@
$(ho)/love $(ho)/love.cand: $(kart_o)

# l.o carries the version string; recompile it when the id changes. ⚠ the -D is what MAKES
# it carry one -- mooncc has no __has_include for src/love.c's fallback probe, so without it
# the dep tracks a header the object cannot read and the kernel answers "unknown".
$(k_odir)/src/love.o: out/lib/love_version.h
$(k_odir)/src/love.o: kcppflags += -DAiHaveVersionH

# The two LAYS. holo's object writer (obj.l's objsecs) takes a list of NAMED sections --
# .boot, .note.pvh, the 2 KiB-aligned vector table, .bss -- which is what the kernel needs
# and a compiler never emits. ⚠ the cat joins the TARGET's backend text explicitly: a
# frontend bakes holo with the NATIVE one only, and this build must not care where it runs.
k_be_x86_64 = amd64
k_be_aarch64 = arm64
klay_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/$(k_be_$a).l $R/crew/holo/elf.l $R/crew/holo/obj.l
# kproject.l's cat shape, twice. ⚠ STATIC pattern, never an implicit one: a pattern-MADE
# prerequisite is an INTERMEDIATE make deletes after the link, and the cat would then run
# again on every build. naming the targets keeps them ordinary files.
$(k_odir)/mkvec.l $(k_odir)/mkboot.l: $(k_odir)/%.l: $R/src/%.l $(klay_l)
	@echo 'CAT	'$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(klay_l)) $<; } > $@

# --- THE TWIN CARRIES IT TOO (the cross road) --------------------------------
# an egg laid for another machine must BE the binary that machine's own `love
# seed` builds, so it takes the kernel at $(xa) exactly as the host link takes it
# at $(hosta) -- an egg short of it first-boots fine and then fails the fixpoint,
# a whole machine away from the lane that laid it. spelled here rather than in
# the crew section because the kernel section owns the shape and is included second:
# $(kart_bake) expands to nothing up there. an arch with no free/<a>/ carries
# none, which is what the $(if) reads.
xkart_inc = -I$(ho) -I. -Isrc -Iout/lib -I$R \
  -I$R/crew/quay -I$R/crew/moon/include
xkart_h = $(love_h) $(R)/src/k.h $(wildcard $(R)/src/$(xa)_*.h)
xkart_arch_o = $(patsubst $R/src/%.c,$(xd)/k_%.o,$(wildcard $R/src/$(xa)_*.c))
xkart_quay_o = $(patsubst %,$(xd)/k_q_%.o,paint cga_8x8 moderndos_8x16)
xkart_o = $(if $(xkart_arch_o),$(xd)/k_kmain.o $(xd)/k_blk.o $(xd)/k_sys.o $(xkart_arch_o) $(xkart_quay_o) $(xd)/kvec.o,)
$(xd)/k_%.o: $R/src/%.c $(xkart_h) $(kart_bake) $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moonx) $(xkart_inc) -c $< $@
$(xd)/k_q_%.o: $R/crew/quay/%.c $(moon0_dep)
	@echo 'MOON	'$@
	@mkdir -p "$(dir $@)"
	@$(moonx) $(xkart_inc) -c $< $@
# the twin's own cat, the shape above worn at $(xa): the kernel's is cut at $a
# and this is the other machine. one target, so an ordinary rule serves.
xklay_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/$(k_be_$(xa)).l $R/crew/holo/elf.l $R/crew/holo/obj.l
$(xd)/mkvec.l: $R/src/mkvec.l $(xklay_l)
	@echo 'CAT	'$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(xklay_l)) $<; } > $@
$(xd)/kvec.o: $(xd)/mkvec.l $(love0)
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@LOVE_NO_IMAGE= $(boot_love) -l $< -q -e '(lay-vec "$@" "$(xa)")' && test -s $@
$(xd)/love: $(xkart_o)

# `test -s`: an empty object is the failure this build cannot see -- it links, and the
# kernel boots into nothing.
$(k_lay_o) $(k_boot_o): $(k_odir)/$a/%.o: $(k_odir)/mk%.l $m
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@$m -l $< -q -e '(lay-$* "$@" "$a")' && test -s $@

# the machine tail rides the host's own cat (flavour-neutral, one cut for every
# consumer); only the entry names the arch.
k_mksys_x86_64 = mksys
k_mksys_aarch64 = mksys-arm64
$(k_tail_o): out/host/.mksys-cat.l $m
	@echo 'HOLO	'$@
	@mkdir -p "$(dir $@)"
	@$m -l out/host/.mksys-cat.l -q -e "((from 'moon '$(k_mksys_$a)) \"$@\")" && test -s $@

# --- qemu run targets ------------------------------------------------
# KVM where the host offers it: TCG costs 6x on the boot (22s to the prompt against
# 5s) and 7x on the corpus. A box without /dev/kvm falls to TCG and answers the same,
# which is what lets the GATES take it too (tools/ktest.l, tools/kboot.l, vec.sh).
# ⚠ x86_64-on-x86_64 only, not any arch match: qemu's arm `virt` is asked for
# gic-version=2 here, and a host whose GIC cannot back v2 REFUSES the pairing.
k_kvm = $(if $(and $(wildcard /dev/kvm),$(filter x86_64,$a),$(filter x86_64,$(shell uname -m))),-enable-kvm -cpu host,)
k_qemu_x86_64 = -M q35 -serial stdio
k_qemu_risc = -device ramfb -device qemu-xhci -device usb-kbd -device usb-mouse
k_qemu_aarch64 = -M virt,gic-version=2 -cpu cortex-a72 -serial stdio -semihosting $(k_qemu_risc)
k_qemu = qemu-system-$a -m 256M $(k_qemu_$a) $(k_kvm)
# ⚠ the FIRMWARE rides the ESP door ALONE. `qemu -kernel` enters our PVH stub with the
# machine bare; hand it OVMF as well and the firmware boots first and takes the door.
# tools/ktest.l draws the same line, which is why its -kernel lane names no pflash.
k_fw = -drive if=pflash,unit=0,format=raw,file=$(dl)/edk2-ovmf/ovmf-code-$a.fd,readonly=on

# THE TWO DOORS, and they trade: our own BOOTX64.EFI hands over a framebuffer and
# carries no command line, `qemu -kernel` carries one (-append) and hands over no
# framebuffer -- PVH has nothing to hand. so `run` is the graphical one and
# `run-sh` the one that seats lush. ⚠ x86_64 only for the ESP: BOOTAA64.EFI is
# not ours to lay yet, so aarch64 takes the -kernel door for both.
ifeq ($a,x86_64)
run: run-$a
run-$a: $(ko)/esp-$a/EFI/BOOT/$(k_efiname) $(ko)/esp-$a/love.elf $(dl)/edk2-ovmf/ovmf-code-$a.fd
	exec $(k_qemu) $(k_fw) -drive format=raw,file=fat:rw:$(ko)/esp-$a
else
run: run-$a
run-$a: $(k_elf)
	exec $(k_qemu) -kernel $<
endif
# the serial doors: no firmware, nothing downloaded, and a command line.
run-sh: $(k_elf)
	exec $(k_qemu) -kernel $< -append "sh"
run-headless: $(k_elf)
	exec $(k_qemu) -kernel $< -display none -no-reboot

# Boot init AS PID 1 in a container -- love at the Linux altitude of "the system". An
# unprivileged pid+user+mount namespace: --pid --fork makes the entrypoint pid 1, --user
# --map-root-user makes it root-in-ns so mount works, --mount-proc gives it a fresh /proc.
# love then IS init: getpid 1, mounts the early filesystems, reaps a reparented orphan.
# (pid1 0) is the deterministic tour, (perceive 0) the live signalfd supervisor.
init-container: host
	@command -v unshare >/dev/null || { echo "init-container: needs unshare (util-linux)"; exit 1; }
	@echo "-- love as PID 1 in a pid+user+mount namespace --"
	unshare --pid --fork --mount-proc --user --map-root-user -- $m -l crew/init/init.l -e "(pid1 0)"

# --- headless serial test (wired into test_slow; x86_64 + qemu only) ------------
# The K_TEST corpus: the host $t minus what this seat cannot run, plus the laws that can
# only run HERE. It bakes into out/lib/ktests.h and boots through the self-hosted ev,
# printing the usual summary over serial -- the freestanding kernel held to the same
# corpus test_host and test_love0 hold the host to. tools/ktest.l drives it.
#
# Dropped: run.l wants host-OS nifs (subprocess), bell.l's Bell-number bignums are too
# heavy for an emulated kernel. Added, in order: ramfs.l (the baked initrd, which on the
# host would just be `open` on the real tree), fs.l and wfs.l (the writable tree), kore0.l
# then the kore cat then kore.l (the fs tools over the cat's own prefix), pipe.l (rung 4:
# pipes, the spawn/wait shim, the stdio seat), sys.l (the syscall seam -- nolibc's write
# through src/sys.c to a row, which only this seat can ask), lush's engine parts in cat order as
# test/host/sh.l reads them (sh0.l pins what they mention and the seat lacks) and sh.l,
# rung 4's gate -- a real pipeline through sh-line -- then disk.l (rung 5: the virtio raw
# door + lib/fat.l on the real device, guarded on (disk ()) so a seat without one stays
# green) and svm.l (the AMD-V spike, guarded twice: the nom is x86_64-only and the
# silicon may be Intel's). zz-fin.l goes last: it prints the summary and quits.
kt = $(filter-out %/run.l %/bell.l %/zz-fin.l,$t) \
  $R/test/kernel/ramfs.l $R/test/kernel/fs.l $R/test/kernel/wfs.l \
  $R/test/kernel/kore0.l $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/core.l $R/crew/kore/fs.l \
  $R/test/kernel/kore.l $R/test/kernel/pipe.l $R/test/kernel/sys.l \
  $R/test/kernel/sh0.l $R/crew/lush/job.l $R/crew/lush/lex.l $R/crew/lush/gram.l \
  $R/crew/lush/glob.l $R/crew/lush/word.l $R/crew/lush/eval.l $R/test/kernel/sh.l \
  $R/test/kernel/disk.l $R/test/kernel/svm.l $R/test/kernel/vmx.l \
  $R/test/zz-fin.l
# out/lib/corpus.list carries the MEMBERSHIP, rewritten only when the set changes
# (the lib section) -- so an edit to any makefile in the tree does not relay this header.
out/lib/ktests.list: force_dist_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$(kt)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo 'SH	'$@; fi
out/lib/ktests.l: $(kt) out/lib/corpus.list out/lib/ktests.list
	@echo 'CAT	'$@
	@mkdir -p out/lib
	@cat $(kt) > $@
# the two VERBATIM bakes, one shape (lcatv, not lcat: an inspect-reprint diverges
# when the corpus is read back incrementally through a strin port).
out/lib/ktests.h: out/lib/%.h: out/lib/%.l $(love0) tools/lcatv.l love/prel.l
	@echo 'LOVE	'$@
	@$(love0) -l love/prel.l tools/lcatv.l $< > $@

# arm64 EXECUTION validator: cross-build `love` for aarch64 and run the corpus under
# qemu-aarch64 -- test/holo/golden.l proves the byte encodings, this proves they run.
test_arm64: host
	@./tools/arm64check.sh

# The x86_64 gate boots the ELF DIRECT: `qemu -kernel` reads the PVH note and enters our
# own bring-up -- page tables, GDT, long mode, kboot -- with NOTHING in dl/ involved.
ifeq ($a,x86_64)
test_kernel: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test.elf
	@echo TEST $(ko)/love-$a-test.elf "(serial, headless, -kernel; ~60s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test.elf - $a

# test_disk -- the rung-5 gate: write a file, RESET the machine, read it back. Two boots
# of the K_TEST kernel over one FRESH scratch image -- the first finds no filesystem and
# formats, the second must mount what the first wrote; ktest.l's 4th arg demands the kept
# line on top of the green summary.
test_disk: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test.elf
	@rm -f $(ko)/love-$a-test.elf.disk
	@echo TEST $(ko)/love-$a-test.elf "(two boots, one disk: the reset-persistence gate)"
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test.elf - $a
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test.elf - $a "disk: fat kept across the reset"
	@echo "test_disk: the machine remembered"

# test_kboot -- inle rung 3's gate: the SHIPPED kernel (no K_TEST) booted direct with a
# boot command line, the baked kore cat dispatching off the program seat, running the tool
# and quitting through the reset door. Four boots at a cold cat eval each (~minutes under
# TCG), so OPT-IN -- run it when the kernel or the kore cat moves. vi
# stays the interactive smoke, under run-* -- `-append "vi lib/json.l"`.
# test_kverb -- the artifact emits its own boot image (`love kernel`, from
# nothing but what it carries), and the answer is BYTE-IDENTICAL to this
# makefile's projection: one derivation, two drivers, no drift possible
# between what a box gets and what the tree builds. run from out/free, so a
# cwd dependence would fail it.
test_kverb: host
	@$(MAKE) -s $(k_elf)
	@echo TEST love kernel "(the projection verb; byte-identical to make's)"
	@rm -f $(ko)/.kverb.elf
	@cd $(ko) && $(abspath $m) kernel .kverb.elf > /dev/null
	@cmp $(ko)/.kverb.elf $(k_elf)
	@rm -f $(ko)/.kverb.elf

test_kboot: host $(R)/tools/kboot.l
	@$(MAKE) -s $(k_elf)
	@echo TEST $(k_elf) "(the kore cat off cmdline; 4 boots, ceiling 420s each)"
	@$m $(R)/tools/kboot.l $(k_elf) "kore ls lib" "json.l"
	@$m $(R)/tools/kboot.l $(k_elf) "kore wc lib/json.l" "lib/json.l" $$(wc -c < $(R)/lib/json.l)
	@$m $(R)/tools/kboot.l $(k_elf) "sh -c \"cd lib; pwd\"" "/lib"
	@$m $(R)/tools/kboot.l $(k_elf) "sh -c \"kore ls lib | kore wc -l\"" $$(ls $(R)/lib | wc -l)
else
test_kernel test_disk test_kboot:
	@echo "$@: skipped (host arch $a is not x86_64)"
endif

# --- the UEFI door: our own BOOTX64.EFI ------------------------------------
# No gnu-efi, no foreign toolchain, no bootloader we did not write: mooncc compiles the
# loader (loader.c reads love.elf off the ESP, fills kboot from the UEFI memmap + GOP,
# ExitBootServices, page tables, jumps kmain), mkefi.l lays the ms_abi<->SysV seam in holo
# IR, and holo's PE lane links the PE32+ the firmware runs. This is the LAPTOP door and the
# only one that hands over a framebuffer; the ESP is two files.
# ⚠ it carries NO command line -- `run-sh` is the -append door. adding one means a second
# file on the ESP for the loader to read, and nothing here reads one yet.
uefi_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/crew/holo/pe.l \
  $R/src/uefi_mkefi.l
# the removable-media path firmware looks for, per arch -- it is the FILENAME that
# picks the loader, so the two ESPs differ in nothing else.
k_efiname_x86_64 = BOOTX64.EFI
k_efiname_aarch64 = BOOTAA64.EFI
k_efiname = $(k_efiname_$a)
k_uefid = $(ko)/uefi-$a$(ksuf)
k_espd = $(ko)/esp-$a$(ksuf)
$(k_uefid)/loader.o: $R/src/uefi_loader.c $(ho)/love.baked
	@echo 'MOON	'$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(ho)/love mooncc -t $(k_be_$a) -c $< $@
$(k_uefid)/$(k_efiname): $(k_uefid)/loader.o $(uefi_l) $m
	@echo 'HOLO	'$@
	@mkdir -p $(dir $@)
	@{ echo "(use 'holo)"; cat $(uefi_l); echo '(mkboot "$@" "$a" (list "$<"))'; } | $m
# the ESP: the loader at that path, and the kernel beside it (the loader opens
# "love.elf" on its own volume).
$(k_espd)/EFI/BOOT/$(k_efiname): $(k_uefid)/$(k_efiname)
$(k_espd)/love.elf: $(ko)/love-$a$(ksuf).elf
$(k_espd)/EFI/BOOT/$(k_efiname) $(k_espd)/love.elf:
	@echo 'CP	'$@
	@mkdir -p $(dir $@)
	@cp $< $@
uefi: $(ko)/esp-$a/EFI/BOOT/$(k_efiname) $(ko)/esp-$a/love.elf
	@echo "uefi: $(ko)/esp-$a is an ESP -- copy it to a FAT32 partition, or"
	@echo "      qemu-system-$a -drive format=raw,file=fat:rw:$(ko)/esp-$a ..."

# test_uefi -- the whole laptop door under qemu, and the only gate that exercises the
# HAND-OVER (the loader off the ESP, kboot from the memmap, ExitBootServices, the jump);
# everything past it is the artifact test_kernel already gates. Gated on the firmware
# being PRESENT, never downloaded, since test_slow must fetch nothing -- `make
# dl/edk2-ovmf/ovmf-code-x86_64.fd` once and the lane starts running. ⚠ it is also the
# only gate handed a framebuffer, so kmain.c's fbdraw runs nowhere else: if it costs
# MINUTES where test_kernel does not, the console is repainting, not the door faulting.
OVMF_X64 := $(wildcard $(dl)/edk2-ovmf/ovmf-code-x86_64.fd)
ifeq ($(and $(filter x86_64,$a),$(OVMF_X64)),)
test_uefi:
	@echo "test_uefi: skipped (x86_64 + $(dl)/edk2-ovmf/ovmf-code-x86_64.fd needed)"
else
test_uefi: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/esp-x86_64-test/EFI/BOOT/BOOTX64.EFI $(ko)/esp-x86_64-test/love.elf
	@echo TEST $(ko)/esp-x86_64-test "(serial, headless, our own BOOTX64.EFI; ~64s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/esp-x86_64-test $(OVMF_X64) x86_64
endif

# test_uefi_arm64 -- the same door on the other arch, and the ONLY one that gives
# aarch64 a loader of ours: `qemu -kernel` is a hypervisor protocol, so until this
# lane runs, arm64 has never met firmware.
OVMF_A64 := $(wildcard $(dl)/edk2-ovmf/ovmf-code-aarch64.fd)
QEMU_A64U ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
ifeq ($(and $(OVMF_A64),$(QEMU_A64U)),)
test_uefi_arm64:
	@echo "test_uefi_arm64: skipped (qemu-system-aarch64 + $(dl)/edk2-ovmf/ovmf-code-aarch64.fd needed)"
else
test_uefi_arm64: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/esp-aarch64-test/EFI/BOOT/BOOTAA64.EFI $(ko)/esp-aarch64-test/love.elf
	@echo TEST $(ko)/esp-aarch64-test "(serial, headless, our own BOOTAA64.EFI; TCG, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/esp-aarch64-test $(OVMF_A64) aarch64
endif

# test_inle -- the kernel's whole roster, in one word. Every lane below prints its own
# skip where the seat cannot run it (not x86_64, no qemu), so this is safe to type
# anywhere; cheapest first, so a break says so early. SEQUENTIAL sub-makes: as plain
# prerequisites a -j would land two of them in one object tree at once.
# ⚠ NOT on test_slow -- it is minutes of qemu, and the merge gate's subject is the seed.
# This is the gate to type when free/ or the kore cat moves.
test_inle:
	@$(MAKE) -s test_kernel
	@$(MAKE) -s test_disk
	@$(MAKE) -s test_uefi
	@$(MAKE) -s test_kboot
	@$(MAKE) -s test_kverb
	@$(MAKE) -s test_kernel_arm64
	@$(MAKE) -s test_uefi_arm64
	@echo "test_inle: boot, disk, command line, firmware -- both arches"

# The aarch64 twin of test_kernel, same corpus under full-TCG (~45s). In test_slow
# because the lane needs a gate that RUNS it: the aarch64 kernel is otherwise reached
# only by lanes test_kernel's x86_64 gate skips. Our own cc crosses by name (-t), so the
# only thing that can be missing is the emulator.
QEMU_A64 ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
ifeq ($(QEMU_A64),)
test_kernel_arm64:
	@echo "test_kernel_arm64: skipped (need qemu-system-aarch64)"
else
test_kernel_arm64: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/love-aarch64-test.elf
	@echo TEST $(ko)/love-aarch64-test.elf "(serial, headless, TCG, -kernel; ~90s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/love-aarch64-test.elf - aarch64
endif

# --- wasm headless test (BY NAME: `make test_wasm`; needs emcc + node) ------
# Build love.js and run the SAME $t corpus through it under node -- a third runtime after
# the host and love0, exercising wasm's <data.h> override (sentinel-ap data kinds, no flat
# code-address space). The harness evals the corpus in one ai_eval and greps the drained
# output for the zz-fin summary, exactly as test_host greps `cat $t | love`.
# ⚠ it links OUT OF TREE (wasm's `gate` target -> out/wasm/love.js) and never over the
# committed wasm/love.js: a gate must not rewrite the working tree.
NODE ?= $(shell command -v node 2>/dev/null)
EMCC ?= $(or $(shell command -v emcc 2>/dev/null),/usr/lib/emscripten/emcc)
ifeq ($(and $(NODE),$(wildcard $(EMCC))),)
test_wasm:
	@echo "test_wasm: skipped (needs emcc + node)"
else
test_wasm:
	@$(MAKE) -s -C $(R)/wasm gate
	@echo TEST out/wasm/love.js "(node)"
	@$(NODE) $(R)/wasm/test.mjs --love $(R)/out/wasm/love.js $t
endif

# --- downloads -------------------------------------------------------
$(dl)/edk2-ovmf/ovmf-code-%.fd:
	@echo 'MK	'ovmf
	@mkdir -p $(dl)
	@curl -L https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/edk2-ovmf.tar.gz | gunzip | tar -C $(dl) -xf -
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
	esac
include $(R)/test/test.mk
include $(R)/mk/install.mk

JOBS  ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
osync := $(if $(filter output-sync,$(.FEATURES)),--output-sync=target,)
test_phases = test_host test_love0 test_letrec
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
# name only (the kernel section, port/, wasm/), as are test_fixpoint and test_distboot.
test_extra: test_filemode waits test_front test_proof test_gen test_uugen test_uulean test_uuwm \
	test_uukind test_gc test_gcheck test_gcstress test_extract test_big test_mx \
	test_tools test_hostnif test_doc test_glaze test_hook test_sat test_holo test_as \
	test_holofuzz test_glazefuzz test_encver test_lux test_kore test_refuzz test_sb test_vi \
	test_moon test_clay test_moonfuzz test_forge \
	test_cts test_libc test_ulp test_raw \
	test_drv test_hdiff test_tco0 nettest test_wake test_gz test_cpio

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
	@# wasm/ does not ride the release (dist_drop, above), so an unpacked
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

