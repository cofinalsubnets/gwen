# crew/build.mk -- the crew app builds: the kore/mooncc/sb/lush scripts, their baked
# images, and the dist artifact. Included by ./Makefile after host/build.mk, so $(ho) is
# already spelled; shared vars are common.mk.

# kore: the diff engines, the text/tool surface, the line tools, and `kore` itself -- the
# multi-call toolbox picking its util off the command line or an argv[0] symlink. lush
# rides the cat too, so `kore sh` and a /bin/sh symlink are the shell and the distro's
# one-binary userland closes over its own console. ⚠ lush goes BEFORE cook.l, whose
# $(wildcard) presence-guard then reads sh-glob bare.
# ⚠ the rosters a LATER roster splices sit above it: a prerequisite list expands when
# make READS the rule, so a $(..) still undefined there expands to nothing and the cat
# comes out short a file -- silently, the members that remain being well-formed.
lushfiles = crew/lush/job.l crew/lush/lex.l crew/lush/gram.l crew/lush/glob.l crew/lush/word.l crew/lush/eval.l crew/lush/line.l crew/lush/main.l
korefiles =crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l crew/kore/sed.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l tools/ain.l $(lushfiles) crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l crew/kore/kore.l
# mooncc is its OWN app, NOT in the kore cat: a cc edit rebuilds only mooncc, so a kore
# rebuild in another session cannot tear the compiler. ⚠ member order is the scope -- the
# u-floor, then asbook splices the boot-registered holo and the CROSS BACKENDS join it
# (defbackend mutates holo's own table, so mooncc cross-compiles every target whichever
# single backend the host image baked), the writers, the compiler proper, then moon.l
# whose tail SEAT fires.
moonfiles = crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l
# the build-tree kore/mooncc bins are WAKE SHIMS over their sibling images, the exact shape
# mk/install.mk installs: `#!/bin/sh` resolving its own directory, then exec'ing the
# SIBLING love on the SIBLING image. ⚠ the interpreter is never PATH's, so a tree-fresh cat
# can never run under a foreign older-baked binary -- the skew that once laid EMPTY .text
# when obj.l read a `holo` book key an installed love's bake lacked. make keeps cat, image
# and shim fresh together, so consistency is structural rather than checked at runtime.
# (a cold invocation then wakes in ~ms instead of re-evaling the cat, ~1.3s.) kore's shim
# threads basename($0) through, so the argv[0]-symlink dispatch still lands.
$(ho)/.kore-cat.l: $(korefiles)
$(ho)/.mooncc-cat.l: $(moonfiles)
$(ho)/.kore-cat.l $(ho)/.mooncc-cat.l:
	@echo CAT	$(abspath $@)
	@mkdir -p $(dir $@)
	@cat $^ > $@
$(ho)/kore: $(ho)/kore.image
	@echo CAT	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'n=$$(basename -- "$$0")'; \
	   echo 'exec "$$h/love" wake "$$h/kore.image" "$$n" "$$@"'; } > $@
	@chmod 755 $@
$(ho)/mooncc: $(ho)/mooncc.image
	@echo CAT	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$0")" && pwd)'; \
	   echo 'exec "$$h/love" wake "$$h/mooncc.image" mooncc "$$@"'; } > $@
	@chmod 755 $@
# sb 🌱 the patch-set vcs (svalbard), and lush 🐚 the love shell -- also the distro's console shell,
# whose SEAT in main.l fires on its own basename. Both are catted shebang scripts, PATH
# picking the love that runs them.
sbfiles = crew/kore/text.l crew/kore/diff.l lib/dns.l crew/sb/merge.l crew/sb/http.l crew/sb/sb.l
$(ho)/sb: $(sbfiles)
$(ho)/lush: $(lushfiles)
$(ho)/sb $(ho)/lush:
	@echo CAT	$(abspath $@)
	@mkdir -p $(dir $@)
	@{ echo '#!/usr/bin/env -S love'; cat $^; } > $@
	@chmod 755 $@
# the two WARM images (the live bake, doc/snapshot.md): each cat loads under a NEUTRAL name
# so its tail SEAT stays quiet, then the bake nif snapshots the session. LOVE_NO_IMAGE
# rides the recipe, so the bake session itself egg-boots -- same warm state every time.
$(ho)/mooncc.image $(ho)/kore.image: $(ho)/%.image: $(ho)/.%-cat.l $m
	@echo LOVE	$(abspath $@)
	@$m -l $< -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'
# mooncc0.image: the SAME cat baked by LOVE0, the build-time compiler that breaks the
# self-host circle -- the default love is mooncc-built, so its own image cannot drive its
# build, and love0 waking this one can. PINNED to out/host like love0 itself.
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo LOVE	$(abspath $@)
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

# ==== dist: the ONE artifact (self-host rung 3) ====
# out/dist/love-<arch> is the download door whole: the default love (mooncc-built,
# static PIE, nolibc) re-baked with the crew warm -- cook + kore + lush (vi and ain ride
# its cat) + mooncc (all five backends) + sb + kiosko -- and crew/sb/up.l's
# verb table, which love/cli.l's verb rail reads: `love up URL` syncs ~/.love/src
# and cook-installs the nest; `love sb|cook|kore|kiosko|mooncc ..` are the same
# binary being multi-call. the bake rides `love bake`'s own lane (main.c's
# `bake -l CAT` evals it ahead of the cache-empty + seal), so the artifact
# is the default binary with a bigger image -- no session layer, same sealing.
# member order is the scope: kore's floor first, asbook before the backends
# (defbackend mutates the spliced holo), every main before kore.l's applet
# table, up.l LAST so the verbs close over the lot. DIST_ORIGIN pins the
# default `love up` origin URL ahead of up.l (unset: up asks for a URL).
distfiles = crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l \
            crew/kore/sed.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
            crew/vi/core.l crew/vi/vi.l \
            crew/kore/diff.l lib/dns.l tools/ain.l $(lushfiles) crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/holo/copy.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
            crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l crew/kore/kore.l crew/sb/merge.l \
            crew/sb/http.l crew/sb/sb.l crew/kiosko/kiosko.l crew/sb/up.l \
            lib/gz.l lib/tar.l lib/tarcmd.l lib/source.l
DIST_ORIGIN ?=
# ⚠ THE MEMBERSHIP IS AN INPUT, and make cannot see it. Adding a file to distfiles
# changes what the artifact CARRIES while every file make watches keeps its mtime, so
# a cat older than the new member is "up to date" and the binary links without it --
# silently, and it looks exactly like the feature not working. (mk/lib.mk's
# corpus.list is the same guard for $t, and for the same reason.) Depend on the LIST:
# rewritten only when membership moves, so the cat re-lays on an add OR a delete.
.PHONY: force_dist_list
force_dist_list: ;
out/dist/.dist.list: force_dist_list
	@mkdir -p out/dist
	@tf=$@.$$$$.tmp; echo '$(distfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
out/dist/.dist-cat.l: $(distfiles) out/dist/.dist.list
	@echo CAT	$(abspath $@)
	@mkdir -p out/dist
	@{ echo '(: origin "$(DIST_ORIGIN)")'; cat $(distfiles); } > $@
# the artifact is named for its arch ($a = uname -m): love-x86_64 here,
# love-aarch64 on a pi -- the moon lane is native on both (mooncc defaults to
# the ground it stands on), so `make dist` anywhere bakes that machine's door.
.PHONY: dist dist-source dist-seed

# ==== THE RELEASE ARTIFACTS (doc/dist.md) ====
# A release is TWO THINGS, and they sit on the one axis that actually matters to
# somebody who just downloaded one: do you have a C toolchain?
#
#   SOURCE   love-<ver>.tar.gz   sources only. `make` bootstraps through the local
#                                cc, which builds love0 and NOTHING else.
#   SEED     love-<arch>         one executable that CARRIES its own source and IS
#                                its own toolchain. `love source` lays the tree with
#                                bin/love already in it; `make` there calls no
#                                ambient compiler at all.
#
# ⚠ AND THEY ANSWER THE SAME BINARY, which is the whole claim and is not a thing we
# had to engineer: the local cc only ever builds `love0` (host/build.mk), and every
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
# needs neither `tar` nor `gzip` on the box. ⚠ our coder writes the FIXED Huffman
# code, ~24% above `gzip -9` (lib/gz.l carries the numbers): a real cost on a
# download, and the reason a dynamic coder is the next rung.
#
# ⚠ REPRODUCIBLE BY CONSTRUCTION: the pack pins every mtime/uid/gid to $(dist_stamp)
# and the gzip header's own MTIME is 0, so two cuts of one revision are the same
# bytes and "this is that release" is something anyone can check with sha256sum.
# the SAME two-part id mk/lib.mk computes -- base from ./VERSION, VCS only a suffix --
# because the tarball is named for it AND ships it, and a release whose filename and
# `love --version` disagreed would be its own kind of lie.
# ⚠ AND THE .git MUST BE THIS TREE'S: `git -C DIR` walks UP, so an extracted tree sitting
# inside a checkout -- which is where `love seed` puts one, the cwd -- described the
# ENCLOSING repo. A second +g on an id that already carried one, and a stage cut from the
# wrong index: a 158-byte tarball and an artifact with no source in it. mk/lib.mk's
# love_version has always guarded on exactly this, and the two ids must agree.
in_git    := $(wildcard $(R)/.git)
dist_base := $(shell cat $(R)/VERSION 2>/dev/null || echo 0)
dist_vcs  := $(if $(in_git),$(shell git -C $(R) describe --always --dirty 2>/dev/null),)
dist_ver  := $(dist_base)$(if $(dist_vcs),+g$(dist_vcs),)
dist_stamp ?= 0
dist_stage = out/dist/stage
dist_source = out/dist/love-$(dist_ver).tar.gz
dist_seed   = out/dist/love-$a
dist-source: $(dist_source)
dist-seed:   $(dist_seed)
dist:        dist-source dist-seed   # a release is both

# ⚠ wasm/love.js is EMSCRIPTEN'S OUTPUT, committed so github pages can serve a repl
# (wasm/Makefile calls it "the COMMITTED artifact"). It stays in the repo for exactly
# that, and stays OUT of the release: 103 KB gz in one file -- more than both generated
# proof terms together -- and it is the one thing here a reader could not regenerate
# without installing a foreign toolchain, inside an artifact whose whole claim is that
# it needs none. Dropping it from the TARBALL costs the repl nothing.
dist_drop = wasm/love.js
# the stage: every TRACKED file, minus dl/ (third-party downloads that `make
# distclean` fetches again -- shipping them would triple the tarball and stale them).
# ⚠ checkout-index reads the INDEX, so a release is cut from what is tracked, not
# from whatever is lying in the working tree.
# ⚠ and the staged VERSION is OVERWRITTEN with the fully computed id -- base AND vcs
# suffix -- because an extracted tarball has no .git to describe. The checked-in VERSION
# carries only the base; freezing the whole id here is what lets a build from the tarball
# stamp the same string the tree stamped, and that string compiles into love.o.
# ⚠ ALWAYS RE-STAGED, never cached on a stamp. The stage's real input is the INDEX, and
# make cannot depend on that: a `git add` changes what a release contains while every file
# make watches keeps its mtime, so a stamped stage happily serves a tarball cut before the
# edit you are trying to test. That is silent, and it looks exactly like the fix not
# working -- it cost three distboot rounds here before the artifact was opened and found to
# predate the change. A checkout-index of the tree is a second; correctness is worth it.
.PHONY: force_stage
force_stage: ;
out/dist/.staged-$(dist_ver): force_stage $(ho)/love
	@echo STAGE	$(abspath $(dist_stage))/love-$(dist_ver)
	@# ⚠ SAY SO WHEN THE INDEX AND THE WORKING TREE DISAGREE. Cutting from the index is
	@# right for a release -- it is what makes an artifact reproducible from a revision --
	@# but it means an uncommitted edit is NOT in what you just built, and a gate run
	@# against it is testing the old code while you read the new. That failure is silent
	@# and looks exactly like the fix not working; it cost two full distboot rounds here.
	@# ⚠ THE WORKTREE COLUMN IS THE ONE THAT MATTERS. `status --porcelain` reports a
	@# STAGED edit as dirty too, and those are exactly the ones that DO ride -- warning
	@# on them cries wolf on every correct release and teaches you to read past it. The
	@# second column is the worktree against the index: ` M` and `??` are absent from
	@# the artifact, `M ` is in it.
	@out=$$(git -C $(R) status --porcelain 2>/dev/null | awk 'substr($$0,2,1) != " "'); \
	 if [ -n "$$out" ]; then \
	   echo "  /warn this artifact comes from the INDEX and these are NOT in it --"; \
	   echo "  /warn 'git add' them first:"; \
	   printf '%s\n' "$$out" | sed 's/^/        /' | head -8; fi
	@rm -rf $(dist_stage)
	@mkdir -p $(dist_stage)/love-$(dist_ver)
	@git -C $(R) checkout-index -a --prefix=$(abspath $(dist_stage))/love-$(dist_ver)/
	@rm -rf $(dist_stage)/love-$(dist_ver)/dl
	@rm -f $(dist_stage)/love-$(dist_ver)/$(dist_drop)
	@printf '%s\n' "$(dist_ver)" > $(dist_stage)/love-$(dist_ver)/VERSION
	@touch $@

# ⚠ AN EXTRACTED TREE CANNOT CUT ONE. The stage is `git checkout-index`, and a tree laid
# by `love source` has no .git -- so there the tarball is not built, it is ALREADY THERE:
# the artifact wrote the bytes it carried to exactly this path. Reusing them is what makes
# a seed binary rebuilt out there byte-identical rather than merely equivalent, since the
# blob it embeds is the same archive and not a re-pack that has to coincide.
ifneq ($(in_git),)
$(dist_source): out/dist/.staged-$(dist_ver) lib/tar.l lib/gz.l tools/tgz.l
	@echo TGZ	$(abspath $@)
	@rm -f $@
	@$(ho)/love tools/tgz.l c $@ $(dist_stage) $(dist_stamp)
else
$(dist_source):
	@echo "dist: no .git here and no $@ --" >&2
	@echo "dist: an extracted tree rebuilds from the archive 'love source' laid;" >&2
	@echo "dist: re-extract if it went missing." >&2
	@exit 1
endif

# THE SOURCE BLOB: the source tarball laid into an object (tools/mksrc.l), so the
# artifact hands out its own source with no second download and no `tar xf` -- love
# `source` inflates it. host/src.c defines the pair WEAK and empty, so this object's
# STRONG definitions override them at the link and a plain `make host` needs none of
# it. ⚠ holo names its arches ($a is uname's, and they disagree on x86_64).
# ⚠ AND THESE TWO RULES MUST SIT BELOW $(dist_source)'s DEFINITION. A prerequisite
# list is expanded where it is WRITTEN: above the definition it expands to nothing,
# make never builds the tarball, and only the recipe -- expanded later, when the
# variable is set -- names a file that was never cut. It fails as a missing archive,
# which reads as the tarball rule being broken rather than this line being early.
ifeq ($a,aarch64)
src_arch = arm64
else
src_arch = x64
endif
out/dist/src-$a.o: $(dist_source) tools/mksrc.l $(ho)/love
	@$(ho)/love tools/mksrc.l $(dist_source) $@ $(src_arch)
# ⚠ THIS LINKS, where it used to `cp` the host binary. A section cannot be injected
# into a finished ELF, so the artifact is now its own link -- $(moon_o) plus the blob
# -- and only then baked. The layout stays load-bearing the other way: .image must
# still END the segment for `bake` to grow it at the tail (host/image.c's bake_tail
# refuses otherwise), which it does, the blob riding .rodata well below it.
$(dist_seed): $(moon_o) out/dist/src-$a.o out/dist/.dist-cat.l $(ho)/love
	@echo DIST	$(abspath $@)
	@mkdir -p $(dir $@)
	@$(moon0) -pie $(moon_o) out/dist/src-$a.o -o $@
	@./$@ bake -l out/dist/.dist-cat.l
	@echo "  dist: $$(du -h $@ | cut -f1) -> $@"

# ==== dist_cross: the TWIN artifact (the other elf arch) ====
# the same door for the machine you are not on: every TU through `mooncc -t`,
# the twin's mksys leaf, our -pie link -- and the bake RUNS the twin under
# qemu-user (the one foreign tool here, and only at build time: `bake` boots
# the egg, warms, and seals the twin's own heap, glaze emitting the twin's
# native code the whole way). so one x86 laptop bakes the pi's download, and
# a pi with qemu-user bakes the laptop's -- each host can serve both doors.
ifeq ($a,aarch64)
xarch = x86_64
xtgt = x64
xqemu = qemu-x86_64
xmksys = mksys
else
xarch = aarch64
xtgt = arm64
xqemu = qemu-aarch64
xmksys = mksys-arm64
endif
xd = out/dist/x
moonx = $(moon0) -t $(xtgt)
xhost_o = $(patsubst host/%.c,$(xd)/host_%.o,$(wildcard host/*.c))
xmath_o = $(patsubst crew/moon/lib/math/%.c,$(xd)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
# no nolibc.o: the link owes its symbols and the driver pulls the members by need
# (crew/moon/lib/nolibc/), so a dist takes no calendar and no resolver.
xobjs = $(xd)/love.o $(xhost_o) $(xmath_o) $(xd)/sys.o
$(xd)/love.o: love.c $(love_h) out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c $< $@
$(xd)/host_%.o: host/%.c $(love_h) out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c $< $@
$(xd)/host_main.o: $(baked_h)
$(xd)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
$(xd)/m_%.o: crew/moon/lib/math/%.c out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moonx) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
$(xd)/sys.o: $(ho)/.mksys-cat.l $(love0)
	@echo HOLO	$@
	@mkdir -p $(dir $@)
	@$(love0) -l $(ho)/.mksys-cat.l -n -e '($(xmksys) "$@")' && test -s $@
out/dist/love-$(xarch): $(xobjs) out/dist/.dist-cat.l
	@echo DIST	$(abspath $@)
	@$(moonx) -pie $(xobjs) -o $@
	@$(xqemu) ./$@ bake -l out/dist/.dist-cat.l
	@echo "  dist: $$(du -h $@ | cut -f1) -> $@ (the $(xarch) twin, baked under $(xqemu))"
.PHONY: dist_cross
dist_cross: out/dist/love-$(xarch)

# ==== the vim syntax for .l -- GENERATED, so there is no copy to keep up to date ====
# tools/hue2vim.l reads crew/vi/hue.l's class table the other way round (one table, two
# readers: the painter in vframe and vim) and asks THIS host for its vocabulary -- so the
# file describes the love you built, which makes it an artifact like any other. It lives
# under out/ for that reason: a checked-in copy can be stale, a built one cannot.
# mk/install.mk installs it beside vim/'s two hand-written siblings.
# ⚠ LOVE_NO_IMAGE is CLEARED. Under it the egg's mop never runs and the compiler's own
# internals (`book` among them) are still on the book; the syntax file describes the
# SHIPPED language, so the generator gets the shipped boot -- and hue2vim.l refuses
# outright rather than freeze build state and call it the language.
# ⚠ atomic, for $(lcat_h)'s reason: a bare `> $@` truncates first, so a broken generator
# would leave a 0-byte syntax file make calls up to date.
huefiles = crew/vi/config.l crew/vi/hue.l tools/hue2vim.l
$(ho)/syntax.vim: $(huefiles) $(m)
	@echo HUE	$@
	@mkdir -p $(dir $@); t=$@.$$$$.tmp; \
	  cat $(huefiles) | env -u LOVE_NO_IMAGE $(m) > $$t && test -s $$t && mv -f $$t $@ \
	    || { rm -f $$t; echo "FAIL: $@ empty (hue2vim.l failed)"; exit 1; }
.PHONY: syntax
syntax: $(ho)/syntax.vim
