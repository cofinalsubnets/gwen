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
asm0_h = out/lib/holo0.h out/lib/x640.h out/lib/arm640.h
# the glaze (native JIT): raw-text headers, no lcat round-trip. Evaled ONLY before a
# --bake, so a normal boot never pays the ~810 ms and the baked snapshot carries an
# always-on JIT at zero startup. doc/snapshot.md.
glaze_h = out/lib/emit.h out/lib/auto.h out/lib/hook.h
# love0's bootstrap headers: sed-wrapped raw source, a text->C-literal needing no
# interpreter, since love0 cannot lcat the very sources it is assembled from. The whole
# concatenated corpus rides along so love0 self-tests both compilers in one run.
sed_lit = sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/'
gl0_h = out/lib/cli0.h out/lib/egg0.h out/lib/post0.h out/lib/p10.h out/lib/prel0.h out/lib/ev0.h out/lib/bao0.h out/lib/uu0.h out/lib/coin0.h out/lib/rng0.h out/lib/q0.h out/lib/kanren0.h out/lib/overlay0.h out/lib/peg0.h out/lib/tests0.h $(asm0_h)
.PHONY: lib
lib: $(lib_h) $(gl0_h)
# ⚠ lcat a .l into its header ATOMICALLY -- temp, require non-empty, then mv. A bare `> $@`
# truncates first, so a broken love0 leaves a 0-byte header make calls up to date, which
# SILENTLY drops a baked service (an empty holo.h => `assemble` unbound => the glaze emits
# nothing => a corrupt native). ⚠ and the temp takes the PID: the ports RECURSE onto these,
# so -j runs the recipe twice at once and one shared temp is renamed out from under the other.
lcat_h = @mkdir -p out/lib; echo LOVE	$@; t=$@.$$$$.tmp; \
  $(love0) -l love/prel.l tools/lcat.l $< > $$t && test -s $$t && mv -f $$t $@ \
    || { rm -f $$t; echo "FAIL: $@ empty (love0 lcat failed -- broken bootstrap?)"; exit 1; }
$(lib_h): out/lib/%.h: love/%.l tools/lcat.l   # + $(love0), stated below
	$(lcat_h)
# the sed twin of $(lcat_h): a text->C-literal that needs no interpreter.
sed_h = @mkdir -p out/lib; echo AI	$@; $(sed_lit) $< > $@
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
# ⚠ the corpus SET stamp: tests0.h and ktests.l aggregate $t, a wildcard, so a DELETED test
# leaves every remaining prereq older than the target and make keeps baking the ghost.
# Depend on the LIST: rewritten only when membership changes, so they re-lay on add OR delete.
.PHONY: force_corpus_list
force_corpus_list: ;
out/lib/corpus.list: force_corpus_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$t' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
out/lib/tests0.h: $t out/lib/corpus.list
	@mkdir -p out/lib
	@echo AI	$@
	@cat $t | $(sed_lit) > $@

# love_version.h: the build's version-control id, surfaced as the `love-version` global.
# VCS-agnostic -- a _darcs/ repo stamps its patch hash, else git describe, else "unknown" --
# so a darcs snapshot import carries this rule verbatim and stamps itself. ⚠ rewritten only
# when the id CHANGES, so l.o relinks on a new revision and not on every build. A frontend
# without it on the include path falls back to "unknown".
.PHONY: force_version
force_version: ;
out/lib/love_version.h: force_version
	@mkdir -p out/lib
	@if [ -d $(R)/_darcs ]; then \
	  v="darcs-$$(darcs log --repodir $(R) --last 1 2>/dev/null | awk '/^patch/{print substr($$2,1,12)}')"; \
	  darcs whatsnew --repodir $(R) >/dev/null 2>&1 && v="$$v-dirty"; \
	else \
	  v="$$(git -C $(R) describe --always --dirty 2>/dev/null || echo unknown)"; \
	fi; tf=$@.$$$$.tmp; printf '#define AI_VERSION "%s"\n' "$$v" > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH $@; fi

# the lcat'd headers are PRODUCED BY running love0, so re-lay them whenever love0 moves.
$(lib_h) $(holo_h) out/lib/rune.h: $(love0)
