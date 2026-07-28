# mk/lib.mk -- the out/lib/*.h egg + service headers (lcat + sed_lit)
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk. Every recipe here is unchanged from the single-file Makefile.

# Static lisp headers: each love/*.l is serialized to a C string literal in
# out/lib/*.h by tools/lcat.l (run on the bootstrap interpreter love0). Frontends
# #include these and assemble the bootstrap with G_EGG_PRE/POST (love.h).
# Drop a .l into love/ and it is picked up automatically -- no rule to edit.
lib_h = $(patsubst love/%.l,out/lib/%.h,$(wildcard love/*.l))
# the crew/holo/ assembler baked into BOTH runtimes as a core language service. The
# backends are pure love (produce machine-code bytes as DATA, never execute them), so
# every one is arch-neutral -- but the HOST bakes its NATIVE backend only (main.c
# arch-gates the include; mooncc.image carries ALL backends via its cat, and a test
# that wants a cross backend runtime-loads it: (enter ()) (use 'holo) <backend.l>
# (leave ())). love0 keeps every backend so the corpus's cross-arch asserts run under
# both its compilers. Both .h flavors are still GENERATED for both backends (love0
# needs them; the host simply includes one). asm_h = lcat headers (host love);
# asm0_h = sed-wrapped raw source (love0, the bootstrap -- can't lcat its own sources).
holo_h = out/lib/holo.h  out/lib/x64.h  out/lib/arm64.h  out/lib/riscv.h
asm0_h = out/lib/holo0.h out/lib/x640.h out/lib/arm640.h
# the glaze (native JIT, love/glaze/{emit,auto}.l): baked to raw-text headers (sed_lit,
# like asm0 -- no lcat reader round-trip). Evaled ONLY before a --bake (x86-gated
# in main.c), so a normal boot never pays the ~810 ms; the baked snapshot then carries
# an always-on JIT at zero startup (Phase 4, doc/snapshot.md). Their self-test asserts
# native-compile transient closures -- the bake's gen_major drops them before serializing.
glaze_h = out/lib/emit.h out/lib/auto.h out/lib/gexport.h out/lib/hook.h
# love0's bootstrap headers: sed-wrapped raw source (a text->C-literal needing no
# interpreter -- the l reader strips the ; comments at read time), since love0
# can't lcat the very sources it is assembled from (chicken/egg). cli.l doubles as
# love0's CLI arg handler; prel/ev/egg/repl + the whole concatenated test corpus
# are baked in so love0 self-tests both compilers in one run (see main.c). The final
# l uses the canonicalized lcat headers from the rule below instead.
sed_lit = sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$$/\\n"/'
gl0_h = out/lib/cli0.h out/lib/egg0.h out/lib/prel0.h out/lib/ev0.h out/lib/bao0.h out/lib/uu0.h out/lib/coin0.h out/lib/rng0.h out/lib/q0.h out/lib/kanren0.h out/lib/post0.h out/lib/tests0.h $(asm0_h)
.PHONY: lib
lib: $(lib_h) $(gl0_h)
# lcat a .l source into its C-string header, ATOMICALLY: generate to a temp, require it
# non-empty, then mv into place. A bare `> $@` truncates first, so a broken love0 (or any
# lcat failure) would leave a 0-byte header that make then treats as up-to-date -- which
# SILENTLY drops a baked service (e.g. an empty holo.h => `assemble` unbound => the glaze's
# map lane emits nothing => a corrupt native => crash/hang). Fail loudly instead.
lcat_h = @mkdir -p out/lib; echo LOVE	$@; \
  $(love0) -l love/prel.l tools/lcat.l $< > $@.tmp && test -s $@.tmp && mv -f $@.tmp $@ \
    || { rm -f $@.tmp; echo "FAIL: $@ empty (love0 lcat failed -- broken bootstrap?)"; exit 1; }
$(lib_h): out/lib/%.h: love/%.l tools/lcat.l   # + $(love0), stated below
	$(lcat_h)
# the crew/holo/ assembler (crew/holo/holo.l + crew/holo/x64.l) rides the SAME lcat pipeline into the
# post-egg layer -- a core language service (the glaze is its client). Explicit rules
# (their sources live in crew/holo/, not love/, so the pattern rule above misses them).
out/lib/holo.h: crew/holo/holo.l tools/lcat.l
	$(lcat_h)
out/lib/x64.h: crew/holo/x64.l tools/lcat.l
	$(lcat_h)
out/lib/arm64.h: crew/holo/arm64.l tools/lcat.l
	$(lcat_h)
out/lib/riscv.h: crew/holo/riscv.l tools/lcat.l
	$(lcat_h)
# crew/rune/ the CAS rides the same lcat pipeline for device frontends that
# bake it behind the egg (port/playdate's workbench). rune.l seals itself at its
# foot, so there is no second header for the module boundary.
out/lib/rune.h: crew/rune/rune.l tools/lcat.l
	$(lcat_h)
# love0's sed-wrapped raw source of the same three (no interpreter -- the l reader
# strips ; comments at read time), baked into the bootstrap so the corpus can test
# the assembler under BOTH compilers (c0 + the self-hosted ev), like prel/ev/bao.
out/lib/holo0.h: crew/holo/holo.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
out/lib/x640.h: crew/holo/x64.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
out/lib/arm640.h: crew/holo/arm64.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
out/lib/%0.h: love/%.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
# the glaze headers (love/glaze/ -- outside the love/*.l wildcard, so explicit). Raw sed_lit:
# the glaze source is sigil-heavy, so skip the lcat reader round-trip and bake it verbatim.
out/lib/emit.h: love/glaze/emit.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
out/lib/auto.h: love/glaze/auto.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
out/lib/gexport.h: love/glaze/export.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
out/lib/hook.h: love/glaze/hook.l
	@mkdir -p out/lib
	@echo AI	$@
	@$(sed_lit) $< > $@
# the corpus SET stamp: tests0.h (and the kernel's ktests.l) aggregate $t, a wildcard --
# so a DELETED test file leaves every remaining prereq older than the target and make
# keeps baking the ghost. Depend on the LIST itself: regenerated every make, rewritten
# only when membership changes (the love_version.h idiom below), so the aggregates
# re-lay on an add OR a delete.
.PHONY: force_corpus_list
force_corpus_list: ;
out/lib/corpus.list: force_corpus_list
	@mkdir -p out/lib
	@echo '$t' > $@.tmp
	@if cmp -s $@.tmp $@ 2>/dev/null; then rm -f $@.tmp; else mv $@.tmp $@; echo SH	$@; fi
out/lib/tests0.h: $t out/lib/corpus.list
	@mkdir -p out/lib
	@echo AI	$@
	@cat $t | $(sed_lit) > $@

# love_version.h: the build's version-control id, surfaced in the runtime as the `love-version`
# global (love.c ai_ini_0). VCS-AGNOSTIC: a _darcs/ repo stamps darcs-<12-hex patch hash>
# (-dirty when darcs whatsnew is non-empty), else git describe, else "unknown" -- so the
# darcs snapshot import carries this rule verbatim and stamps itself. Regenerated every
# make but only rewritten when the id changes, so l.o relinks on a new revision, not on
# every build. Frontends without it on the include path fall back to "unknown" (love.c
# uses __has_include).
.PHONY: force_version
force_version: ;
out/lib/love_version.h: force_version
	@mkdir -p out/lib
	@if [ -d $(R)/_darcs ]; then \
	  v="darcs-$$(darcs log --repodir $(R) --last 1 2>/dev/null | awk '/^patch/{print substr($$2,1,12)}')"; \
	  darcs whatsnew --repodir $(R) >/dev/null 2>&1 && v="$$v-dirty"; \
	else \
	  v="$$(git -C $(R) describe --always --dirty 2>/dev/null || echo unknown)"; \
	fi; printf '#define AI_VERSION "%s"\n' "$$v" > $@.tmp
	@if cmp -s $@.tmp $@ 2>/dev/null; then rm -f $@.tmp; else mv $@.tmp $@; echo SH $@; fi

# The lcat'd lib headers (egg.h et al) are PRODUCED BY running love0, so re-lay
# them whenever love0 changes. (The old "edit a .h => make clean or love0 hangs" gum is
# cleaned: love0's own objects already depend on $(love_h), so love0 can't go stale.)
$(lib_h) $(holo_h) out/lib/rune.h: $(love0)
