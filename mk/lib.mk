# mk/lib.mk -- the out/lib/*.h egg + service headers. Included by ./Makefile from the
# project root; shared vars are common.mk.
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
# holo's LINKER half, baked beside the backends: elf.l wraps assembled bytes in an
# executable, obj.l lays a relocatable .o, link.l links a set (ldkern is the kernel's door).
# ⚠ the holo- prefix is load-bearing: a quoted #include "link.h" sits next to glibc's
# <link.h>, and a missing header would find that one instead of failing.
ld_h = out/lib/holo-elf.h out/lib/holo-obj.h out/lib/holo-link.h
asm0_h = out/lib/holo0.h out/lib/x640.h out/lib/arm640.h
# the glaze (native JIT): raw-text headers, no lcat round-trip. Evaled ONLY before a
# `love bake`, so a normal boot never pays the ~810 ms and the baked snapshot carries an
# always-on JIT at zero startup. doc/snapshot.md.
glaze_h = out/lib/emit.h out/lib/auto.h out/lib/hook.h
# love0's bootstrap headers: raw source wrapped by tools/lit.c, a text->C-literal needing
# no interpreter, since love0 cannot lcat the very sources it is assembled from. The whole
# concatenated corpus rides along so love0 self-tests both compilers in one run.
# ⚠ PINNED to out/host beside love0, and built by the same plain $(CC): a BUILD tool, so it
# is the host's even when the tree is laying a cross artifact, and it never goes musl.
lit = out/host/lit
$(lit): tools/lit.c
	@mkdir -p $(dir $@)
	@echo CC	$@
	@LOVE_NO_IMAGE= $(CC) -std=$(ai_std) -O2 -Wall -Wextra -Werror -o $@ $<
gl0_h = out/lib/cli0.h out/lib/egg0.h out/lib/post0.h out/lib/p10.h out/lib/prel0.h out/lib/ev0.h out/lib/bao0.h out/lib/pat0.h out/lib/uu0.h out/lib/coin0.h out/lib/rng0.h out/lib/q0.h out/lib/kanren0.h out/lib/overlay0.h out/lib/peg0.h out/lib/verbs0.h out/lib/tests0.h $(asm0_h)
.PHONY: lib
lib: $(lib_h) $(gl0_h)
# ⚠ lcat a .l into its header ATOMICALLY -- temp, require non-empty, then mv. A bare `> $@`
# truncates first, so a broken love0 leaves a 0-byte header make calls up to date, which
# SILENTLY drops a baked service (an empty holo.h => `assemble` unbound => the glaze emits
# nothing => a corrupt native). ⚠ and the temp takes the PID: the ports RECURSE onto these,
# so -j runs the recipe twice at once and one shared temp is renamed out from under the other.
# ⚠ the lcat is run by love0 NORMALLY and by the BUNDLED love when a seed laid one
# beside the tree: love0 is not built at all there (see ./Makefile's bundled_love).
# ⚠ AND THE PRELOAD BELONGS TO LOVE0 ALONE. `-l love/prel.l` feeds prel's SOURCE to a
# pre-egg love, which is the only kind that can read it: prel.l:19 calls `(tray 0)`, and
# `tray` is one of the raw ctors THE EGG MOPS AT BIRTH -- so a baked love handed its own
# prel source dies `;; missing tray`. A baked love does not need it either, having prel in
# the image already. Both lanes then lcat the same bytes.
lcat_love = $(if $(bundled_love),$(bundled_love),$(love0) -l love/prel.l)
lcat_h = @mkdir -p out/lib; echo LOVE	$@; t=$@.$$$$.tmp; \
  $(lcat_love) tools/lcat.l $< > $$t && test -s $$t && mv -f $$t $@ \
    || { rm -f $$t; echo "FAIL: $@ empty (lcat failed -- broken bootstrap?)"; exit 1; }
$(lib_h): out/lib/%.h: love/%.l tools/lcat.l   # + $(love0), stated below
	$(lcat_h)
# the lit twin of $(lcat_h): a text->C-literal that needs no interpreter.
lit_h = @mkdir -p out/lib; echo CAT	$@; $(lit) $< > $@
# ⚠ every rule below is a STATIC pattern -- their sources live outside love/, so the
# wildcard misses them, and an implicit pattern would make these headers INTERMEDIATE.
# holo rides the same lcat pipeline as the egg (the glaze is its client); rune is the CAS,
# for device frontends that bake it behind the egg.
$(holo_h): out/lib/%.h: crew/holo/%.l tools/lcat.l
	$(lcat_h)
$(ld_h): out/lib/holo-%.h: crew/holo/%.l tools/lcat.l
	$(lcat_h)
out/lib/rune.h: crew/rune/rune.l tools/lcat.l
	$(lcat_h)
# love0's raw-source twins of the same backends, so the corpus tests the assembler under
# BOTH compilers, and the generic love/*.l twin beside them.
$(asm0_h): out/lib/%0.h: crew/holo/%.l $(lit)
	$(lit_h)
out/lib/%0.h: love/%.l $(lit)
	$(lit_h)
# the glaze is sigil-heavy, so it skips the lcat reader round-trip and bakes verbatim.
$(glaze_h): out/lib/%.h: love/glaze/%.l $(lit)
	$(lit_h)
# ⚠ the corpus SET stamp: tests0.h and ktests.l aggregate $t, a wildcard, so a DELETED test
# leaves every remaining prereq older than the target and make keeps baking the ghost.
# Depend on the LIST: rewritten only when membership changes, so they re-lay on add OR delete.
.PHONY: force_corpus_list
force_corpus_list: ;
out/lib/corpus.list: force_corpus_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$t' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
out/lib/tests0.h: $t out/lib/corpus.list $(lit)
	@mkdir -p out/lib
	@echo CAT	$@
	@cat $t | $(lit) > $@

# love_version.h: the build's version, surfaced as the `love-version` global.
#
# TWO PARTS, and the split is the point: the checked-in ./VERSION is the BASE -- an
# arbitrary string, ours to bump deliberately -- and the VCS only ever adds a SUFFIX.
# A version-control id alone cannot say whether one build is newer than another, or
# what it is; a base alone cannot say which revision you have. So `0.1` in a release
# tarball, `0.1+g701f864d-dirty` in a working checkout.
#
# ⚠ AND A TARBALL IS VCS-INDEPENDENT BY CONSTRUCTION, which is the whole reason the base
# is a FILE rather than a tag: an unpacked release has no .git to describe, and this id
# compiles into love.o, so a tarball that could not name itself would differ from the
# tree it was cut from by exactly one string -- and the release claim rests on those two
# being the same bytes (doc/dist.md). The dist stage freezes the FULL computed id into
# the staged VERSION, so an extracted build reproduces it exactly with no VCS present.
#
# VCS-agnostic: darcs stamps its patch hash, git describes, neither leaves the base bare.
# ⚠ rewritten only when the id CHANGES, so l.o relinks on a new revision and not on every
# build. A frontend without it on the include path falls back to "unknown".
.PHONY: force_version
force_version: ;
out/lib/love_version.h: force_version
	@mkdir -p out/lib
	@b="$$(cat $(R)/VERSION 2>/dev/null || echo 0)"; \
	if [ -d $(R)/_darcs ]; then \
	  s="+darcs.$$(darcs log --repodir $(R) --last 1 2>/dev/null | awk '/^patch/{print substr($$2,1,12)}')"; \
	  darcs whatsnew --repodir $(R) >/dev/null 2>&1 && s="$$s.dirty"; \
	  v="$$b$$s"; \
	elif [ -e $(R)/.git ]; then \
	  s="$$(git -C $(R) describe --always --dirty 2>/dev/null)"; \
	  v="$$b$${s:++g$$s}"; \
	else \
	  v="$$b"; \
	fi; tf=$@.$$$$.tmp; printf '#define AI_VERSION "%s"\n' "$$v" > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi

# the lcat'd headers are PRODUCED BY running the lcat love, so re-lay them whenever it
# moves. ⚠ EMPTY when a seed bundled one: love0 is never built there, and naming
# it as a prerequisite would build it for no reason -- the lane the artifact exists to skip.
$(lib_h) $(holo_h) $(ld_h) out/lib/rune.h: $(if $(bundled_love),,$(love0))
