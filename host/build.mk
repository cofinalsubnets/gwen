# host/build.mk -- the host (POSIX CLI) build, into out/host. Included by ./Makefile from
# the project root, so paths resolve from there; shared vars are mk/common.mk.
#
# ⚠ the DEFAULT flavor owns out/host and HCC builds in its own hsuf'd tree, so the $(CC)
# lane never overwrites a mooncc object -- the two disagree on nothing a linker can see, and
# a mixed relink would read as an up-to-date binary. The .hostcc stamp catches IN-PLACE
# flips. love0 and the generated out/lib/*.h stay pinned to canonical out/host paths.
ho = out/host$(hsuf)
h_o = $(love_c:$(R)/%.c=$(ho)/%.o)
# host/*.c: per-app host-nif files, auto-globbed and auto-registered via AiNif. ⚠ linked
# DIRECTLY into the binary, never via liblove.a, so the ai_nifs section is not
# archive-collected. Drop a host/<app>.c in and it builds -- no rule edit.
host_o = $(patsubst host/%.c,$(ho)/host/%.o,$(wildcard host/*.c))
# love0 and the lib tools ride this too, HCC or not.
# (-I$(ho) -Iout/lib reach the generated egg/cli headers.)
host_cc = $(CC)
# ⚠ GCDBG is the GC debug lanes' knob and deliberately NOT $(EXTRA_CFLAGS): it must reach
# the SHIPPED love under both compilers and never love0. EXTRA_CFLAGS does not reach
# $(moon0) at all, so test_gcheck would run a corpus on a binary that never had the check
# in it; and love0 is shared and unsuffixed, so a flag reaching it leaks out of the debug
# lane -- a stress-built love0 segfaults baking mooncc0.image and takes the tree with it.
# ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads, and it is load-bearing whenever CC is the dist
# artifact's own `love mooncc` verb: under a caller's exported egg a love has no verb
# table, and `mooncc` then reads as a FILENAME ("love: cannot open mooncc"). The love0
# lane already leads with it; this puts it on every $(hcc) site at once.
hcc = LOVE_NO_IMAGE= $(host_cc) $(ai_cflags) $(GCDBG) -Dai_tco=$(tco) -fpic -I$(ho) -I. -Icore -Iout/lib
# the whole-archive flag differs by linker, and mach-o takes no core/love_data.ld either -- it
# spells sections `segment,section`, so core/kinds.h's roster asks the sentinels by name.
ifeq ($(shell uname -s),Darwin)
so_archive = -Wl,-force_load,$(ho)/liblove.a       # ld64's whole-archive
# ⚠ the host contract (ai_clock, ai_fd_port_vt, ai_stdin/out/err: in host/main.c, linked
# into `love` itself and not the archive) is UNRESOLVED in the .so by design -- the loading
# executable provides it. GNU ld allows that; ld64 must be told to defer.
so_undef = -Wl,-undefined,dynamic_lookup
else
so_archive = -Wl,--whole-archive $(ho)/liblove.a -Wl,--no-whole-archive
endif
# the boot image gets its OWN segment at the top of the address space so `love bake` can
# GROW it: the blob appends at the tail of the file and only that phdr + shdr are rewritten,
# nothing else moving (host/image.c's bake_tail). --section-start is what buys it -- ld
# gives a section at a far address a PT_LOAD to itself, above .bss and alone in it.
# 0x2000000 clears .bss with room to grow and is page-aligned, which the loader's
# offset/vaddr congruence needs; an overlap is a LOUD ld error. holo lays the same shape
# its own way, so both toolchains bake alike. ⚠ no mach-o branch because nothing here
# builds there: host/image.c wants <link.h> and dl_iterate_phdr, and on Apple silicon a
# self-patching binary would have to re-sign itself before it could exec again.
image_ldflags = -Wl,--section-start=.love_image=0x2000000
# .hostcc -- the tree's compiler+link identity, content-stamped (cmp keeps the mtime when
# nothing moved). Every host object and the link depend on it, so an in-place flavor flip
# rebuilds the tree instead of relinking mixed-libc objects.
.PHONY: force_hostcc
force_hostcc: ;
$(ho)/.hostcc: force_hostcc
	@mkdir -p $(ho)
	@tf=$@.$$$$.tmp; printf '%s\n' '$(host_cc) $(image_ldflags)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
# ⚠ liblove.so is NOT here, and that is the whole point: it was the one thing on this
# target that a foreign toolchain had to build. holo lays no dynamic section, so a shared
# object is genuinely CC's job (and the archive is `ar`'s) -- which meant `make host` could
# never run without an ambient compiler, however self-hosting everything else became.
# Nothing in the default lane links against it: `love` comes from $(moon_o), and liblove is
# an EMBEDDING product -- for a C program linking love, installed by mk/install.mk, which
# builds its own glibc-tree copies anyway. `make embed` when you want them.
host: $(ho)/love $(ho)/love.baked $(ho)/love.1 $(ho)/cook.1
.PHONY: embed
embed: $(ho)/liblove.so
love0: $(love0)

# dock: the steering dock, launched from a stable COPY so `adopt` can relink the canonical
# out/host/love in place without ETXTBSY. Loads the probe ladder, the server and the
# self-modify loop. ⚠ bind loopback and firewall it -- it evals what it reads.
.PHONY: dock
DOCK_PORT ?= 7620
dock: host
	@cp $(ho)/love $(ho)/dock
	exec $(ho)/dock -l free/judge.l -l free/serve.l -l free/drive.l -l free/patch.l -e "(dock $(DOCK_PORT))"
# the BOOT IMAGE -- the LAYERED CREW BAKE (doc/plan/one-binary.md): `$< bake -L ..` boots
# the fresh binary, evals the docs layer, FREEZES, evals the rest of the crew, and lays
# the chain into that binary's OWN .image section -- host/image.c copies the exe, pwrites
# the blob and renames over the original, so a new inode leaves anyone still executing on
# the old one. The tree's love then IS the artifact's shape: `love kore|mooncc|sh|libra ..`
# with no shim and no sibling image, a verb waking only its layer (libra ~22 ms, the full
# crew ~104 ms -- and no verb picks the LARGEST entry, so a bare `love` pays the full
# wake; the corpus never does, riding LOVE_NO_IMAGE). The load is an OPTIMIZATION: main.c
# falls back to an egg boot on any mismatch, so a stale bake is slower, never fatal. The
# .baked STAMP carries the dependency, since the bake mutates the binary itself -- and it
# now watches the cats too, so a crew edit rebakes (~12 s) without relinking.
$(ho)/love.baked $(ho)/love.cand.baked: %.baked: % $(ho)/.docs-cat.l $(ho)/.rest-cat.l
	@echo LOVE	$< "(bake -L)"
	@$< bake -L $(ho)/.docs-cat.l:libra,help -L $(ho)/.rest-cat.l
	@touch $@


# candidate: build + bake the NEXT GENERATION at a side path nothing executes, so the
# in-place bake can never hit ETXTBSY whoever is running `love`. Gate it with
# `make test m=$(ho)/love.cand`, then promote on green with an atomic rename -- the dock's
# `adopt`. On a red gate the canonical binary is untouched and the candidate dies at the
# side path, like a to-space that never flips.
.PHONY: candidate
candidate: $(ho)/love.cand.baked


# ⚠ rm the archive first: `ar r` replaces and adds but never REMOVES, so a renamed or
# dropped source leaves a stale .o behind and the link dies on multiple definitions.
$(ho)/liblove.a: $(h_o)
	@echo AR	$@
	@mkdir -p $(dir $@)
	@rm -f $@; ar rcs $@ $^

$(ho)/liblove.so: $(ho)/liblove.a $(R)/core/love_data.ld
	@echo LD	$@
	@mkdir -p $(dir $@)
	@$(hcc) -shared -o $@ $(so_archive) $(so_undef) $(data_ld)

# The bootstrap interpreter: -DLoveBoot against the fallback top-level data.h (no
# -I$(ho)), and -Dai_tco=0, which is also the trampoline-coverage lane. It RUNS the .l
# tools that generate the lcat headers, so it cannot depend on them -- it #includes the
# sed-wrapped $(boot_h) instead, produced without an interpreter. It links the whole
# host/*.c glob: the posix nifs and host/image.c's bake/wake are what let love0 bake and
# wake mooncc0.image and so drive the mooncc-built default `love`.
# ⚠ -DAiVersion='$(love_base)+bootstrap' on purpose, and BOTH halves earn their place.
# The suffix: love0 bakes the lcat headers every frontend shares, so a love0 that relinks
# re-lays all of them and rebuilds every object behind them -- a ~25 s cascade fired by
# nothing but a new commit hash. The bootstrap is not a release artifact; the shipped
# `love` carries the real id (the love.o dep below). The BASE, though, must be the real
# one: `.comment` writes the pre-+ half of love-version, so love1 (built by love0's
# mooncc) and love2 (built by love1's) agree only if love0 names the same release.
# It moves when ./VERSION moves -- a release, not a commit -- so the cascade stays away.
# ⚠ -Dai_data_section=0: the bootstrap asks the sentinels BY NAME and owes no linker
# script. Both ai_typ bodies answer the same enum d for the same ap, and the one place a
# data object crosses between differently-built binaries -- the heap image -- carries an ap
# as its INDEX, never an address. So the layout never crosses.
boot_cc = $(CCACHE) $(CC) $(ai_cflags) -DLoveBoot -Dai_tco=0 -Dai_data_section=0 -DAiVersion='"$(love_base)+bootstrap"' -I. -Icore -Iout/lib
love0_host_o = $(patsubst host/%.c,out/host/0/host/%.o,$(wildcard host/*.c))
love0_o = $(love0_host_o) $(love_c:$(R)/%.c=out/host/0/%.o)   # PINNED (not $(ho)/0)
out/host/0/host/main.o: $(boot_h)
out/host/0/host/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
# ⚠ the LOVE_NO_IMAGE= prefix (empty = unset) hands the compiler its baked image back from
# under the blanket corpus export: when CC is the dist artifact's own mooncc verb, the verb
# table lives in that image and an egg boot would read "mooncc" as a filename.
# ⚠ .love0cc content-stamps THIS compile line, .hostcc's trick one lane over, and the base
# version is why it had to exist: love0's id is now load-bearing (it must name the same
# release a real love does, or .comment differs and test_fixpoint fails at a byte offset
# with nothing to say about the cause). make tracks files, not flag strings, so a ./VERSION
# bump would otherwise leave love0 stamped with the previous release forever.
.PHONY: force_love0cc
force_love0cc: ;
out/host/0/.love0cc: force_love0cc
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; printf '%s\n' '$(boot_cc)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
out/host/0/%.o: $(R)/%.c $(love_h) out/host/0/.love0cc
	@echo CC	$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(boot_cc) -c $< -o $@
# ⚠ -pie is LOAD-BEARING: love0 bakes mooncc0.image, and the image codec refuses a binary
# whose text sits in its index range -- a PIE loads high and clears it. gcc/clang default
# to PIE anyway; mooncc, the download door's CC, does not.
$(love0): $(love0_o)
	@echo LD	$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(CC) $(ai_cflags) -pie -o $@ $(love0_o)

# core/love.c -> out/host/*.o
$(ho)/%.o: $(R)/%.c $(love_h) $(ho)/.hostcc
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(hcc) -c $< -o $@

# l.o carries the version string; recompile it when the id changes. love0's twin is
# deliberately NOT here -- see the -DAiVersion note on boot_cc.
$(ho)/love.o: out/lib/love_version.h
# the lcat'd headers host/main.c bakes inline. ONE roster: the mooncc twin and the
# HCC link below read the same name, and three spellings is how they drift.
baked_h = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h out/lib/overlay.h out/lib/peg.h out/lib/pat.h out/lib/uu.h out/lib/verbs.h $(holo_h) $(ld_h) $(glaze_h)
$(ho)/host/main.o: $(baked_h)
# host/cb.c rides the crew/quay sources by unity include -- recompile when they move.
$(ho)/host/cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h

# ==== the DEFAULT love is MOONCC-BUILT (self-host rung 2) ====
# Every TU compiles under love0 waking mooncc0.image, our own nolibc + am math + mksys
# sys.o replace glibc, and holo links it -pie. CC's remaining jobs here are love0 and the
# liblove.a/.so lane, since a shared object wants PIC codegen and a dynamic section holo
# does not lay. HCC=1 takes the $(CC) link below instead -- the foreign-cc differential,
# opt-in, and the only lane that puts one on the vm at ai_tco=1 where ai_musttail is live
# (mk/common.mk says why). One link rule, two names -- `love` and the candidate.
# ⚠ TWO SHAPES, and the second builds no bootstrap at all. Normally love0 wakes
# mooncc0.image, the image that breaks the self-host circle. With a BUNDLED love beside the
# tree (the binary a seed laid beside itself -- ./Makefile's bundled_love) there is no circle: that
# binary already carries mooncc as a verb, so it compiles the tree directly and love0,
# mooncc0.image and the sed-laid 0.h twins are never made. moon0_dep carries the difference
# into the rules below, so nothing names an image that will not exist.
# boot_love: whoever runs a build-time .l tool -- love0 normally, the bundled artifact
# when one is here. Every such site must ask for it by this name, or it resurrects love0.
boot_love = $(if $(bundled_love),$(bundled_love),$(love0))
ifneq ($(bundled_love),)
moon0 = LOVE_NO_IMAGE= $(bundled_love) mooncc $(GCDBG)
moon0_dep =
else
moon0 = $(love0) wake out/host/mooncc0.image mooncc $(GCDBG)
moon0_dep = out/host/mooncc0.image
endif
moon_d = $(ho)/moon
moon_host_o = $(patsubst host/%.c,$(moon_d)/host_%.o,$(wildcard host/*.c))
moon_math_o = $(patsubst crew/moon/lib/math/%.c,$(moon_d)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
# no nolibc object: the link owes its symbols, so the driver's runtime table pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED -- a love asking for no calendar and no
# resolver links neither. Naming an object would take every member instead.
moon_o = $(moon_d)/love.o $(moon_host_o) $(moon_math_o) $(moon_d)/sys.o
# -D AiHaveVersionH + the love_version.h dep: this TU carries the version id into the
# SHIPPED binary, and mooncc has no __has_include for core/love.c's fallback probe to use.
# THE RECORD, and it is OFF: `-fir` lays the machine-form IR of every function into
# .rodata (per-TU `ai_ir_<basename>`), ~1.5 MB, +12.8% on the artifact. It was on for
# exactly one commit, and the argument that took it off again is the good one: THE
# SOURCE IS ALREADY IN HERE. A second description of the same program, at a level
# almost nobody reads, when what a reader lacks is orientation -- and .README buys that
# for 3 KB. ⚠ the cost is the splice JIT: with no record it declines every op and the
# natjit lane is dead weight, so `make moon_fir=-fir` is how you get it back (and
# `rm -rf out/host/moon` first -- make tracks files, not flag strings).
moon_fir = -fno-ir
$(moon_d)/love.o: core/love.c $(love_h) $(moon0_dep) out/lib/love_version.h
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -D AiHaveVersionH $(moon_fir) -I$(ho) -I. -Icore -Iout/lib -c $< $@
$(moon_d)/host_%.o: host/%.c $(love_h) $(moon0_dep)
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) $(moon_fir) -I$(ho) -I. -Icore -Iout/lib -c $< $@
$(moon_d)/host_main.o: $(baked_h)
$(moon_d)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(moon_d)/m_%.o: crew/moon/lib/math/%.c $(moon0_dep)
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) $(moon_fir) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
# sys.o is LAID, not compiled: the syscall trampoline and our sigsetjmp/longjmp have no C
# spelling. love0 runs the lay, its holo carrying every backend. ⚠ the entry is picked by
# $(hosta), the HOST's arch, never $a -- a cross lane overrides $a, and this object is
# out/host's, so it is the host's or it is wrong (an aarch64 sys.o laid here dies at the
# link with `link-machine`, one remove from its cause).
ifeq ($(hosta),aarch64)
mksys_e = mksys-arm64
else
mksys_e = mksys
endif
# ⚠ THE BACKENDS RIDE THE CAT, all three: sys.o is laid for an arch the cat is
# asked for, and only the baked book's own backend is aboard otherwise -- so a
# cross lay for the third ISA answered `obj-no-backend`. asbook.l first, then
# the backends join the module, then elf/obj: asbook.l's own stated order.
mksys_l = crew/kore/text.l crew/kore/u.l crew/kore/asbook.l \
          crew/holo/x64.l crew/holo/arm64.l crew/holo/riscv.l \
          crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
$(ho)/.mksys-cat.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(mksys_l)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
$(ho)/.mksys-cat.l: $(mksys_l) $(ho)/.mksys-cat.list
	@echo CAT	$@
	@mkdir -p $(dir $@)
	@cat $(mksys_l) > $@
# ⚠ THE LAST love0 IN THE DEFAULT LANE. sys.o is LAID by running a love over the mksys cat,
# and naming love0 here was enough to drag the whole bootstrap back in -- love0 wants $(boot_h),
# which used to want tests0.h, the whole corpus through one stdin, where distboot kept dying at
# 139. (The corpus is READ now, not baked, so that particular tail is gone.) A bundled love
# lays it just as well: the cat carries holo itself, so the layer needs nothing of the bootstrap.
$(moon_d)/sys.o: $(ho)/.mksys-cat.l $(if $(bundled_love),,$(love0))
	@echo HOLO	$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(boot_love) -l $(ho)/.mksys-cat.l -n -e "((from 'moon '$(mksys_e)) \"$@\")" && test -s $@
ifneq ($(HCC),)
# ⚠ the HCC flavor is a foreign-cc DIFFERENTIAL, not the artifact: it links no
# source blob and no readme ($(hcc) knows neither), and dist refuses it.
$(ho)/love $(ho)/love.cand: $(host_o) $(ho)/liblove.a $(ho)/.hostcc $(R)/core/love_data.ld $(baked_h)
	@echo LD	$@
	@mkdir -p $(dir $@)
	@$(hcc) -o $@ $(host_o) $(ho)/liblove.a $(image_ldflags) $(data_ld)
else
# ⚠ the nolibc sources are a dep of the LINK, not of any object: the driver compiles
# the members it pulls, so an edit there changes this binary with no .o to notice.
# ⚠ AND THE TREE IS TWO DEEP since it went one-function-to-a-file -- all but a handful
# sit under ctype/ dirent/ env/ fmt/ mem/ net/ stdio/ sys/ .., so a one-level glob names
# five of them and every libc edit that matters relinks NOTHING. It reads as an
# up-to-date binary carrying the code from before the edit. (mk/install.mk's moon_srcs
# is the same glob for the same reason -- keep the two in step.)
nolibc_src = $(wildcard crew/moon/lib/nolibc/*.c crew/moon/lib/nolibc/*.h \
                        crew/moon/lib/nolibc/*/*.c crew/moon/lib/nolibc/*/*.h)
# THIS LINK IS THE SEED (doc/dist.md, seed-universal U2): the default binary
# carries its own source blob and readme, and once baked it IS the artifact --
# there is no leaner host build for it to subsume anymore.
# ⚠ the layout stays load-bearing: .image must END the segment for `bake` to
# grow it at the tail (host/image.c's bake_tail refuses otherwise), which it
# does, the blob riding .rodata well below it.
# ⚠ -freadme rides only THIS link, so test_fixpoint's relink of $(moon_o) needs
# no mirror of it. assets/readme.bin is the page a reader lands on --
# `readelf -p .README`, mapped by nothing.
$(ho)/love $(ho)/love.cand: $(moon_o) out/host/src.o out/host/rt.o assets/readme.bin $(nolibc_src)
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -pie $(moon_o) out/host/src.o out/host/rt.o -freadme=assets/readme.bin -o $@
endif

# the man pages are WRITTEN in doc/*.md and generated here through the lapiz lens: one
# source, so the roff cannot drift from the prose. ⚠ a STATIC pattern -- an implicit one
# would make these intermediate and re-run the lens on every build. mkman takes the version
# header as its second word and fills @VERSION@ itself, so the roff needs no sed after.
$(ho)/love.1 $(ho)/cook.1 $(ho)/lush.1: $(ho)/%.1: doc/%.md mk/tools/mkman.l crew/lapiz/lapiz.l out/lib/love_version.h $(ho)/love
	@echo LOVE	$@
	@mkdir -p $(dir $@)
	@$(ho)/love mk/tools/mkman.l doc/$*.md out/lib/love_version.h > $@

