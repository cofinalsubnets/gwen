# crew/build.mk -- the crew rides IN the default binary (doc/plan/one-binary.md): the
# layered bake host/build.mk runs lays the whole crew into out/host/love's own image, so
# `love kore|mooncc|sh|..` is the build tree's spelling exactly as it is the artifact's.
# What remains here: the cat rosters, mooncc0.image (love0's own -- an image keeps its
# binary's layout), the lush/sb PATH scripts, and the dist artifact. Included by
# ./Makefile after host/build.mk, so $(ho) is already spelled; shared vars are
# mk/common.mk.

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
korefiles =crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l crew/kore/patch.l mk/tools/ain.l $(lushfiles) crew/kore/find.l crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l crew/kore/kore.l
# mooncc is its OWN app, NOT in the kore cat: a cc edit rebuilds only mooncc, so a kore
# rebuild in another session cannot tear the compiler. ⚠ member order is the scope -- the
# u-floor, then asbook splices the boot-registered holo and the CROSS BACKENDS join it
# (defbackend mutates holo's own table, so mooncc cross-compiles every target whichever
# single backend the host image baked), the writers, the compiler proper, then moon.l
# whose tail SEAT fires.
moonfiles = crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l
# ⚠ THE MEMBERSHIP IS AN INPUT AND MAKE CANNOT SEE IT -- the same trap $(ho)/.dist.list and
# out/lib/corpus.list already guard. Moving a file BETWEEN these lists changes what the cat
# holds while every file make watches keeps its mtime, so the cat is "up to date" and the image
# is built from the old set: silently, and it reads exactly like the edit not working. Splitting
# the u-floor out of core.l cost four debug rounds to this, across five different cats.
# Depend on the LIST: rewritten only when membership moves, so the cat re-lays on add OR drop.
$(ho)/.mooncc-cat.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(moonfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
$(ho)/.mooncc-cat.l: $(moonfiles) $(ho)/.mooncc-cat.list
	@echo CAT	$(abspath $@)
	@mkdir -p $(dir $@)
	@cat $(filter %.l,$^) > $@
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
# mooncc0.image: the mooncc cat baked by LOVE0, the build-time compiler that breaks the
# self-host circle -- the default love is mooncc-built, so its own image cannot drive its
# build, and love0 waking this one can. PINNED to out/host like love0 itself. The ONLY
# standalone image left: the default love's crew rides its own .image (the layered bake,
# host/build.mk), and an image cannot cross binaries anyway.
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo LOVE	$(abspath $@)
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

# ==== dist: the ONE artifact (self-host rung 3; seed-universal U2) ====
# the seed IS the default binary: out/host/love links the moon objects plus its
# own source blob and readme (host/build.mk carries the link), and the layered
# bake lays the crew warm -- cook + kore + lush (vi and ain ride its cat) +
# mooncc (all five backends) + sb + kiosko -- and crew/sb/up.l's verb table,
# which love/cli.l's verb rail reads: `love sb|cook|kore|kiosko|mooncc ..` are
# the same binary being multi-call. there is no leaner host build beside it and
# no love-<arch> twin: one tree, one binary, and `make dist` is that binary
# plus the source tarball. the per-ISA bytes remain (a binary is for one
# machine until U1's container); it is the artifact NAMES that dissolved.
# member order is the scope: kore's floor first, asbook before the backends
# (defbackend mutates the spliced holo), every main before kore.l's applet
# table, up.l LAST so the verbs close over the lot. DIST_ORIGIN pins the
# default `love up` origin URL ahead of up.l (unset: up asks for a URL).
distfiles = crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l \
            crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
            crew/vi/core.l crew/vi/vi.l \
            crew/kore/diff.l crew/kore/patch.l lib/dns.l mk/tools/ain.l $(lushfiles) crew/kore/find.l \
            crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/holo/copy.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
            crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l crew/kore/kore.l crew/sb/merge.l \
            crew/sb/http.l crew/sb/sb.l crew/kiosko/kiosko.l crew/sb/up.l \
            lib/gz.l lib/tar.l lib/tarcmd.l lib/source.l crew/lapiz/lapiz.l \
            lib/salt.l lib/infix.l crew/libra/libra.l lib/hueweb.l lib/serve.l
# THE DOCS LANE -- the small image of the array below. a one-shot `love libra ..`
# wants the .l reader, the config door, the factor pass and the document lens, and
# nothing else: it is 1.8 MB against the full image's 7.6, and the wake is linear in
# that (~17 ms/MB measured), so the command starts in a quarter of the time.
# ⚠ ITS MEMBERSHIP IS AN INPUT, exactly as distfiles' is -- same list guard below.
docsfiles = lib/lint.l lib/salt.l lib/infix.l crew/lapiz/lapiz.l crew/libra/libra.l
# ..and the REST of the dist, which is the second layer of the bake below. filter-out
# keeps distfiles' order, so the two cats together are the same tree in the same
# sequence -- only the docs half now goes in FIRST, which is what makes it a prefix.
restfiles = $(filter-out $(docsfiles),$(distfiles))
DIST_ORIGIN ?=
# ⚠ THE MEMBERSHIP IS AN INPUT, and make cannot see it. Adding a file to distfiles
# changes what the artifact CARRIES while every file make watches keeps its mtime, so
# a cat older than the new member is "up to date" and the binary links without it --
# silently, and it looks exactly like the feature not working. (mk/lib.mk's
# corpus.list is the same guard for $t, and for the same reason.) Depend on the LIST:
# rewritten only when membership moves, so the cat re-lays on an add OR a delete.
# ⚠ the cats live in $(ho): they are the DEFAULT binary's own bake layers now
# (host/build.mk's love.baked), and the dist lanes read the same files -- one roster,
# one set of bytes, so the tree binary and the artifact cannot drift.
.PHONY: force_dist_list
force_dist_list: ;
$(ho)/.dist.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(distfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
$(ho)/.docs.list: force_dist_list
	@mkdir -p $(dir $@)
	@tf=$@.$$$$.tmp; echo '$(docsfiles)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
$(ho)/.docs-cat.l: $(docsfiles) $(ho)/.docs.list
	@echo CAT	$(abspath $@)
	@mkdir -p $(dir $@)
	@cat $(docsfiles) > $@
$(ho)/.dist-cat.l: $(distfiles) $(ho)/.dist.list
	@echo CAT	$(abspath $@)
	@mkdir -p $(dir $@)
	@{ echo '(: origin "$(DIST_ORIGIN)")'; cat $(distfiles); } > $@
# the second layer: everything the docs layer is not. `origin` rides here because it is
# the artifact's own (love up), and the docs image has no use for it.
$(ho)/.rest-cat.l: $(restfiles) $(ho)/.dist.list $(ho)/.docs.list
	@echo CAT	$(abspath $@)
	@mkdir -p $(dir $@)
	@{ echo '(: origin "$(DIST_ORIGIN)")'; cat $(restfiles); } > $@
.PHONY: dist dist-source dist-seed

# ==== THE RELEASE ARTIFACTS (doc/dist.md) ====
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
dist_base := $(love_base)
dist_vcs  := $(if $(in_git),$(shell git -C $(R) describe --always --dirty 2>/dev/null),)
dist_ver  := $(dist_base)$(if $(dist_vcs),+g$(dist_vcs),)
dist_stamp ?= 0
dist_stage = out/dist/stage
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
dist_drop = wasm/love.js
# the stage: every TRACKED file, minus dl/ (third-party downloads that `make
# distclean` fetches again -- shipping them would triple the tarball and stale them).
# ⚠ checkout-index reads the INDEX, so a release is cut from what is tracked, not
# from whatever is lying in the working tree.
# ⚠ and the staged VERSION is OVERWRITTEN with the fully computed id -- base AND vcs
# suffix -- because an extracted tarball has no .git to describe. The checked-in VERSION
# carries only the base; freezing the whole id here is what lets a build from the tarball
# stamp the same string the tree stamped, and that string compiles into love.o.
# THE STAGE'S REAL INPUT IS THE INDEX, and the stamp holds the index's own CONTENT HASH --
# `git write-tree`, 2 ms, the same tree object a commit would name. So this is a content
# stamp like $(ho)/.hostcc and out/lib/corpus.list, not a date stamp: a `git add` changes the
# hash and re-stages, and nothing else can make it skip.
# ⚠ THE OLD RULE RE-STAGED UNCONDITIONALLY and said make could not depend on the index. It
# can, through the hash; what it cannot depend on is an mtime, which was the true objection --
# a `git add` moves no file make watches, so a DATE-stamped stage serves a tarball cut before
# the edit you are testing, silently, looking exactly like the fix not working (three distboot
# rounds went that way). The hash closes that hole and costs 43 s less on every no-op.
# ⚠ the dirty WARNING stays outside the skip: `write-tree` hashes the index, so a worktree
# edit leaves it unchanged -- correctly, the artifact does not carry that edit -- and the one
# time you need telling is exactly then.
.PHONY: force_stage
force_stage: ;
out/dist/.staged-$(dist_ver): force_stage
	@mkdir -p out/dist
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
	@t=$$(git -C $(R) write-tree 2>/dev/null); \
	 if [ -n "$$t" ] && [ -f $@ ] && [ -d $(dist_stage)/love-$(dist_ver) ] \
	    && [ "$$t" = "$$(cat $@ 2>/dev/null)" ]; then :; else \
	   echo "STAGE	$(abspath $(dist_stage))/love-$(dist_ver)"; \
	   rm -rf $(dist_stage); \
	   mkdir -p $(dist_stage)/love-$(dist_ver); \
	   git -C $(R) checkout-index -a --prefix=$(abspath $(dist_stage))/love-$(dist_ver)/; \
	   rm -rf $(dist_stage)/love-$(dist_ver)/dl; \
	   rm -f $(dist_stage)/love-$(dist_ver)/$(dist_drop); \
	   printf '%s\n' "$(dist_ver)" > $(dist_stage)/love-$(dist_ver)/VERSION; \
	   printf '%s\n' "$$t" > $@; fi

# ⚠ AN EXTRACTED TREE CANNOT CUT ONE. The stage is `git checkout-index`, and a tree laid
# by `love source` has no .git -- so there the tarball is not built, it is ALREADY THERE:
# the artifact wrote the bytes it carried to exactly this path. Reusing them is what makes
# a seed binary rebuilt out there byte-identical rather than merely equivalent, since the
# blob it embeds is the same archive and not a re-pack that has to coincide.
ifneq ($(in_git),)
# KEEP THE LAST N, and nothing fancier: every cut is named for its revision, so they pile up
# one per commit you happened to build -- 35 of them and 200 MB here before anyone looked. The
# newest $(dist_keep) survive (by mtime, so the one you are working with is never the casualty)
# and the stage stamps follow the same rule, being the same generations by another name.
dist_keep ?= 3
# ⚠ the runner is $(boot_love), never the seed itself: the seed EMBEDS this
# archive, so the archive must exist before the binary can link.
$(dist_source): out/dist/.staged-$(dist_ver) lib/tar.l lib/gz.l mk/tools/tgz.l $(if $(bundled_love),,$(love0))
	@echo TGZ	$(abspath $@)
	@rm -f $@
	@$(boot_love) mk/tools/tgz.l c $@ $(dist_stage) $(dist_stamp)
	@ls -t out/dist/love-*.tar.gz 2>/dev/null | tail -n +$$(($(dist_keep)+1)) | xargs -r rm -f
	@ls -t out/dist/.staged-* 2>/dev/null | tail -n +$$(($(dist_keep)+1)) | xargs -r rm -f
else
# an extracted tree has no index to cut from -- but it needs the archive, since
# the seed it builds embeds one. a seed-laid tree already holds the very bytes
# it carried (reusing them is what makes its rebuild byte-identical); a bare
# source tree re-cuts them from itself -- mk/tools/selfpack.l mirrors the stage
# cut (same walk, same sort, same stamp), and test_distboot's binary compare is
# what holds the re-cut to the byte.
$(dist_source): $(if $(bundled_love),,$(love0))
	@if [ -f $@ ]; then :; else \
	   echo "TGZ	$(abspath $@)"; mkdir -p $(dir $@); \
	   $(boot_love) mk/tools/selfpack.l $@ love-$(dist_ver) $(dist_stamp); fi
endif

# THE SOURCE BLOB: the source tarball laid into an object (mk/tools/mksrc.l), so the
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
else ifeq ($a,riscv64)
src_arch = riscv64
else
src_arch = x64
endif
# ⚠ mksrc rides the mksys cat (kore + holo elf/obj), NOT (use 'holo): the
# module walk resolves off a NEST, and a fresh seed tree has none. same lane
# as sys.o, live in both worlds. PINNED to out/host: the blob is the tree's,
# not a compiler flavor's, and only the moon link takes it.
out/host/src.o: $(dist_source) mk/tools/mksrc.l $(ho)/.mksys-cat.l $(if $(bundled_love),,$(love0))
	@$(boot_love) -l $(ho)/.mksys-cat.l mk/tools/mksrc.l $(dist_source) $@ $(src_arch)

# ==== the x-lane: test_xfixpoint's objects (seed-universal U0) ====
# there is ONE artifact; this lane builds no second one. it compiles the tree's
# TUs through `mooncc -t` for another arch so the cross-machine fixpoint gate
# can link them and prove, under qemu-user, that mooncc's output does not
# depend on the arch mooncc runs on. gate machinery, never a product.
# THE ROSTER, one row per arch the gate can effigy: the mooncc target, the
# qemu-user that runs it, and the mksys leaf that lays its machine tail.
xtgt_x86_64   = x64
xtgt_aarch64  = arm64
xtgt_riscv64  = riscv64
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
xhost_o = $(patsubst host/%.c,$(xd)/host_%.o,$(wildcard host/*.c))
xmath_o = $(patsubst crew/moon/lib/math/%.c,$(xd)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
# no nolibc.o: the link owes its symbols and the driver pulls the members by need
# (crew/moon/lib/nolibc/), so a dist takes no calendar and no resolver.
xobjs = $(xd)/love.o $(xhost_o) $(xmath_o) $(xd)/sys.o
# -D AiHaveVersionH like the host lane (build.mk's love.o): mooncc has no
# __has_include, so the flag is the only door to the version header.
$(xd)/love.o: core/love.c $(love_h) out/host/mooncc0.image out/lib/love_version.h
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -D AiHaveVersionH -I$(ho) -I. -Icore -Iout/lib -c $< $@
$(xd)/host_%.o: host/%.c $(love_h) out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Icore -Iout/lib -c $< $@
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

# ==== the vim syntax for .l -- GENERATED, so there is no copy to keep up to date ====
# mk/tools/hue2vim.l reads crew/vi/hue.l's class table the other way round (one table, two
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
huefiles = crew/vi/config.l crew/vi/hue.l mk/tools/hue2vim.l
$(ho)/syntax.vim: $(huefiles) $(m)
	@echo HUE	$@
	@mkdir -p $(dir $@); t=$@.$$$$.tmp; \
	  cat $(huefiles) | env -u LOVE_NO_IMAGE $(m) > $$t && test -s $$t && mv -f $$t $@ \
	    || { rm -f $$t; echo "FAIL: $@ empty (hue2vim.l failed)"; exit 1; }
.PHONY: syntax
syntax: $(ho)/syntax.vim
