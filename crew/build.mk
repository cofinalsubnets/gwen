# crew/build.mk -- the crew rides IN the default binary (doc/misc/plan/one-binary.md): the
# layered bake src/build.mk runs lays the whole crew into out/host/love's own image, so
# `love kore|mooncc|sh|..` is the build tree's spelling exactly as it is the artifact's.
# What remains here: the cat rosters, mooncc0.image (love0's own -- an image keeps its
# binary's layout), the lush/sb PATH scripts, and the dist artifact. Included by
# ./Makefile after src/build.mk, so $(ho) is already spelled; shared vars are
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
korefiles =crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/fs.l crew/kore/sum.l crew/kore/re.l crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l crew/kore/patch.l tools/ain.l $(lushfiles) crew/kore/find.l crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l crew/kore/kore.l
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
# src/build.mk), and an image cannot cross binaries anyway.
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo 'LOVE	'$@
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

# ==== dist: the ONE artifact (self-host rung 3; seed-universal U2) ====
# the seed IS the default binary: out/host/love links the moon objects plus its
# own source blob and readme (src/build.mk carries the link), and the layered
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
            crew/kore/sed.l crew/kore/awk.l crew/kore/expr.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
            crew/vi/core.l crew/vi/vi.l \
            crew/kore/diff.l crew/kore/patch.l lib/dns.l tools/ain.l $(lushfiles) crew/kore/find.l \
            crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/amd64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/rv64.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/holo/copy.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
            crew/moon/val.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l crew/kore/kore.l crew/sb/merge.l \
            crew/sb/http.l crew/sb/sb.l crew/kiosko/kiosko.l \
            lib/gz.l lib/tar.l lib/tarcmd.l lib/gzcmd.l lib/cpio.l lib/cpiocmd.l \
            lib/source.l crew/lapiz/lapiz.l \
            lib/salt.l crew/libra/libra.l lib/hueweb.l lib/serve.l
# ⚠ THE MEMBERSHIP IS AN INPUT, and make cannot see it. Adding a file to distfiles
# changes what the artifact CARRIES while every file make watches keeps its mtime, so
# a cat older than the new member is "up to date" and the binary links without it --
# silently, and it looks exactly like the feature not working. (mk/lib.mk's
# corpus.list is the same guard for $t, and for the same reason.) Depend on the LIST:
# rewritten only when membership moves, so the cat re-lays on an add OR a delete.
# ⚠ the cat lives in $(ho): it is the DEFAULT binary's own bake load now
# (src/build.mk's love.baked), and the dist lanes read the same file -- one roster,
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
out/lib/distlist.h: crew/build.mk
	@echo 'SH	'$@
	@mkdir -p out/lib
	@printf '"%s"\n' '$(distfiles)' > $@
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
# had to engineer: the local cc only ever builds `love0` (src/build.mk), and every
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
# needs neither `tar` nor `gzip` on the box, and lib/gzcmd.l wears their flags for a
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
# WRITTEN in doc/{love,cook,lush}.md and generated from them (src/build.mk), and
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
# rebuilds. src/kernel.mk owns the list and the prereq line; a recipe expands
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
