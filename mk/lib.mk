# mk/lib.mk -- the out/lib/*.h egg + service headers. Included by ./Makefile from the
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
holo_h = out/lib/holo.h  out/lib/x64.h  out/lib/arm64.h  out/lib/riscv.h
# holo's LINKER half is NOT baked: elf/obj/link ride the crew cat, laid at bake with the
# glaze live. no egg carries them -- the egg's holo feeds the glaze, which emits for the
# machine it runs on and never writes a file.
asm0_h = out/lib/holo0.h out/lib/x640.h out/lib/arm640.h
# the glaze (native JIT): raw-text headers, no lcat round-trip. Evaled ONLY before a
# `love bake`, so a normal boot never pays the ~810 ms and the baked snapshot carries an
# always-on JIT at zero startup. doc/misc/snapshot.md.
glaze_h = out/lib/emit.h out/lib/auto.h out/lib/hook.h
# love0's bootstrap headers: raw source wrapped as a C literal by four substitutions,
# since love0 cannot lcat the very sources it is assembled from. The whole concatenated
# corpus rides along so love0 self-tests both compilers in one run.
# ⚠ AMBIENT sed WHILE BOOTSTRAPPING, OURS ONCE WE HAVE ONE -- the same discipline as $(CC)
# and $(lcat_love). These headers are INPUTS to love0, so a from-scratch tree has no love
# to lay them with; a seed-laid tree has a bundled love whose sed is the artifact's own,
# and builds none of the 0.h twins anyway (./Makefile's bundled_love).
# ⚠ THE ORDER OF THE FOUR IS THE CORRECTNESS: backslash first, or the escapes it writes
# get escaped again by the quote pass.
sed_lit = $(if $(bundled_love),$(bundled_love) sed,sed) \
  -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/'
boot_h = out/lib/cli0.h out/lib/egg0.h out/lib/post0.h out/lib/p10.h out/lib/prel0.h out/lib/ev0.h out/lib/bao0.h out/lib/uu0.h out/lib/coin0.h out/lib/rng0.h out/lib/q0.h out/lib/kanren0.h out/lib/overlay0.h out/lib/peg0.h out/lib/verbs0.h $(asm0_h)
.PHONY: lib
lib: $(lib_h) $(boot_h)
# ⚠ ONE LCAT RUN LAYS THEM ALL, and that is where the build time went: ~98% of an
# invocation is love0 booting (2.45G insns of the 2.52G a 2.2K file costs), so fifteen
# runs paid fifteen startups to serialize 300K of text. One run, a scratch dir, and the
# loop below moves only what CHANGED -- which is also what keeps an edit to one .l from
# restamping every header and rebuilding every frontend.
# ⚠ lcat lays into a SCRATCH DIR and the move is here, so a broken love0 cannot leave a
# 0-byte header make calls up to date -- which SILENTLY drops a baked service (an empty
# holo.h => `assemble` unbound => the glaze emits nothing => a corrupt native).
# ⚠ and the scratch takes the PID: the ports RECURSE onto these, so -j runs the recipe
# twice at once and one shared temp is renamed out from under the other.
# ⚠ the lcat is run by love0 NORMALLY and by the BUNDLED love when a seed laid one
# beside the tree: love0 is not built at all there (see ./Makefile's bundled_love).
# ⚠ AND THE PRELOAD BELONGS TO LOVE0 ALONE. `-l love/prel.l` feeds prel's SOURCE to a
# pre-egg love, which is the only kind that can read it: prel.l:19 calls `(tray 0)`, and
# `tray` is one of the raw ctors THE EGG MOPS AT BIRTH -- so a baked love handed its own
# prel source dies `;; missing tray`. A baked love does not need it either, having prel in
# the image already. Both lanes then lcat the same bytes.
lcat_love = $(if $(bundled_love),$(bundled_love),$(love0) -l love/prel.l)
# the sources and the headers they lay, in step: lib_h is love/*.l by construction, and
# the two outsiders (holo's four, rune) name their own.
lcat_src = $(wildcard love/*.l) $(patsubst out/lib/%.h,crew/holo/%.l,$(holo_h)) crew/rune/rune.l
lcat_out = $(lib_h) $(holo_h) out/lib/rune.h
# ⚠ THE STAMP IS THE WORK and the headers only depend on it -- a rule per header would be
# a run per header again. Each header keeps a recipe rather than none, because a
# prerequisite-less rule fires only when the target is MISSING (port/port.mk learned the
# same thing); the recipe never WRITES the file, so an unchanged header keeps its mtime
# and nothing downstream rebuilds. What it does check is the one hole a stamp opens: a
# header deleted by hand under a current stamp would otherwise be served as up to date
# and fail at the cc, which is exactly how out/lib/rune.h once froze (port/playdate).
out/lib/.lcat.stamp: $(lcat_src) tools/lcat.l $(if $(bundled_love),,$(love0))
	@mkdir -p out/lib; d=out/lib/.lcat.$$$$; rm -rf $$d; mkdir -p $$d; \
	 $(lcat_love) tools/lcat.l $$d $(lcat_src) \
	   || { rm -rf $$d; echo "FAIL: lcat laid nothing (broken bootstrap?)"; exit 1; }; \
	 for h in $(notdir $(lcat_out)); do \
	   test -s $$d/$$h \
	     || { rm -rf $$d; echo "FAIL: out/lib/$$h empty (lcat failed -- broken bootstrap?)"; exit 1; }; \
	   cmp -s $$d/$$h out/lib/$$h || { echo 'LOVE	out/lib/'$$h; mv -f $$d/$$h out/lib/$$h; }; \
	 done; rm -rf $$d; touch $@
$(lcat_out): out/lib/.lcat.stamp
	@test -s $@ || { echo "FAIL: $@ is missing but $< is current -- rm $< and re-make"; exit 1; }
# the sed twin of the lcat lay: a text->C-literal that needs no interpreter, so the tag
# says SED -- it is sed running, ambient or ours, and never the lcat love.
# ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads, for the same reason $(hcc) does: the root
# Makefile exports it=1 in a tree with no bundled love, a seed-laid tree INHERITS it,
# and an egg-booted love has no verb table -- so `love sed` would read as a filename.
sed_h = @mkdir -p out/lib; echo 'SED	'$@; LOVE_NO_IMAGE= $(sed_lit) $< > $@
# holo rides the same lcat run as the egg (the glaze is its client); rune is the CAS, for
# device frontends that bake it behind the egg. Both are in $(lcat_src) above -- their
# sources live outside love/, where the wildcard misses them.
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
# at run time to find the corpus (host/main.c), which is what took the corpus out of its
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

# the lcat'd headers are PRODUCED BY running the lcat love, so the stamp above names it as
# a prerequisite and re-lays whenever it moves. ⚠ EMPTY when a seed bundled one: love0 is
# never built there, and naming it would build it for no reason -- the lane the artifact
# exists to skip.
