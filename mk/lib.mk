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

# the lcat'd headers are PRODUCED BY running love0, so re-lay them whenever it moves.
$(lib_h) $(holo_h) out/lib/rune.h: $(love0)
