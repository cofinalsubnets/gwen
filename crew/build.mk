# crew/build.mk -- the crew app builds (kore/mooncc/seed scripts + mooncc.image)
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk. Every recipe here is unchanged from the single-file Makefile.

# kore (crew/kore/): the myers + patience diff engines, the text/tool surface,
# the line tools (crew/kore/core.l: cat head tail wc sort uniq tee + the trivia),
# and `kore`, the multi-call toolbox (busybox's trick -- one binary, the util picked
# off the command line or an argv[0] symlink; crew/kore/kore.l is the dispatcher).
# The law file holds the engines to their projections, to an O(nm) LCS oracle
# (minimality), the u-floor to its GNU faces, and seeded fuzzes; the kore smokes then
# drive the BUILT artifact: diff + the line tools byte-identical to GNU (LC_ALL=C
# for sort), the exit triple, argv[0] dispatch through a `diff` symlink, usage at 2,
# and (x86_64) `kore as` assembling an exit(7) ELF that RUNS. Gate = the law sentinel
# AND exit 0 AND the smokes.
# lush rides the cat too (before cook.l: cook's $(wildcard) presence-guard then
# reads sh-glob bare): `kore sh` and a /bin/sh argv0 symlink are the shell --
# the distro's one-binary userland closes over its own console.
korefiles = crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l crew/kore/sed.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l tools/ain.l $(lushfiles) crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/kore/kore.l
# mooncc: the C compiler is its OWN app, NOT baked into the kore cat -- a cc edit rebuilds
# only mooncc (never kore), so an kore rebuild in another session can't tear the compiler.
# Its own cat: the u-floor (text+core), then asbook splices the boot-registered holo
# and the CROSS BACKENDS join it (their defs ride the session layer; defbackend
# mutates holo's own table -- so mooncc cross-compiles every target regardless of
# which single backend the host image baked), elf/obj/link writers, then
# crew/moon/{lex,cpp,parse,gen}.l, lib/mksys.l (the sys leaf the driver's
# runtime pull lays in-memory), and moon.l whose tail SEAT fires.
# (crew/holo/text.l = the neutral-text assembler front end gen.l's inline asm
# parses templates with; it leaks stream globals incl. a `parse` rebind -- fine
# here, no later cat member reads them bare.)
moonfiles = crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l
# (`ho` is defined further down, after this rule is READ -- target/prereq names
# expand at parse time, so these lines spell out/host$(hsuf) themselves.)
#
# the build-tree kore/mooncc bins are WAKE SHIMS over their sibling images -- the
# exact shape mk/install.mk installs: `#!/bin/sh` resolving its own directory, then
# exec'ing the SIBLING love on the SIBLING image. the interpreter is never PATH's, so
# a tree-fresh cat can never run under a foreign (older-baked) binary -- the skew
# that once laid EMPTY .text when obj.l read a `holo` book key an installed love's
# bake lacked. the image is anchor-checked to that binary, and make keeps cat,
# image and shim fresh together: consistency is structural, not checked at runtime.
# (bonus: every cold invocation -- the m4/tar builds pay dozens -- wakes in ~ms
# instead of re-evaling the cat ~1.3s.) kore's shim threads basename($0) through as
# the program name, so the argv[0]-symlink dispatch (`diff` -> kore) still lands.
out/host$(hsuf)/.kore-cat.l: $(korefiles)
	@echo AI	$(abspath $@)
	@mkdir -p $(dir $@)
	@cat $(korefiles) > $@
out/host$(hsuf)/kore: out/host$(hsuf)/kore.image
	@echo AI	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'n=$$(basename -- "$$0")'; \
	   echo 'exec "$$h/love" --wake "$$h/kore.image" -e "(kore-main (link \"$$n\" (cuup (cup cmdline))))" "$$@"'; } > $@
	@chmod 755 $@
out/host$(hsuf)/.mooncc-cat.l: $(moonfiles)
	@echo AI	$(abspath $@)
	@mkdir -p $(dir $@)
	@cat $(moonfiles) > $@
out/host$(hsuf)/mooncc: out/host$(hsuf)/mooncc.image
	@echo AI	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$0")" && pwd)'; \
	   echo 'exec "$$h/love" --wake "$$h/mooncc.image" -e "(moon-main (cuup (cup cmdline)))" "$$@"'; } > $@
	@chmod 755 $@
# seed: the patch-set vcs (crew/seed/seed.l over the kore text+diff floor;
# doc/seed.md). its own catted shebang script, the mooncc precedent.
seedfiles = crew/kore/text.l crew/kore/diff.l crew/seed/merge.l crew/seed/http.l crew/seed/seed.l
out/host$(hsuf)/seed: $(seedfiles)
	@echo AI	$(abspath $@)
	@mkdir -p $(dir $@)
	@{ echo '#!/usr/bin/env -S love'; cat $(seedfiles); } > $@
	@chmod 755 $@
# lush 🐚: the love shell (crew/lush/) -- job control, pipes, redirects over the
# host/posix.c nifs; also the distro's console shell (mk/distro.mk cats these
# same parts to /lib/sh.l). its own catted shebang script, the seed precedent;
# the SEAT in main.l fires on its own basename.
lushfiles = crew/lush/job.l crew/lush/lex.l crew/lush/gram.l crew/lush/glob.l crew/lush/word.l crew/lush/eval.l crew/lush/line.l crew/lush/main.l
out/host$(hsuf)/lush: $(lushfiles)
	@echo AI	$(abspath $@)
	@{ echo '#!/usr/bin/env -S love'; cat $(lushfiles); } > $@
	@chmod 755 $@
# the mooncc image: the compiler baked WARM (the live bake, doc/snapshot.md). The
# cat loads under a NEUTRAL name so moon.l's tail SEAT stays quiet, then the bake
# nif snapshots the session. LOVE_NO_IMAGE rides the recipe (exported above), so
# the bake session itself egg-boots -- same warm state, deterministically.
$(ho)/mooncc.image: $(ho)/.mooncc-cat.l $m
	@echo AI	$(abspath $@)
	@$m -l $(ho)/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'
# mooncc0.image: the SAME cat baked by LOVE0 (anchor-checked to love0) -- the
# build-time compiler that breaks the self-host circle: the default out/host/love
# is mooncc-built now (host/build.mk's moon lane), so its own image cannot drive
# its build; love0 waking this one can, at the same ~ms cold start. PINNED to
# out/host like love0 itself.
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo AI	$(abspath $@)
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'
# the kore image: the multi-call toolbox baked WARM, the mooncc.image precedent. the
# cat loads under a NEUTRAL name so kore.l's SEAT me? is false and stays quiet, then
# the bake snapshots. test_kore wakes it per tool (`--wake kore.image -e '(kore-main
# (link "kore" (cuup (cup cmdline))))'`) -- ~0.02s vs ~0.75s cold, across its ~77 spawns.
$(ho)/kore.image: $(ho)/.kore-cat.l $m
	@echo AI	$(abspath $@)
	@$m -l $(ho)/.kore-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

# ==== dist: the ONE artifact (self-host rung 3) ====
# out/dist/love-<arch> is the download door whole: the default love (mooncc-built,
# static PIE, nolibc) re-baked with the crew warm -- cook + kore + lush (vi and ain ride
# its cat) + mooncc (all five backends) + seed + kiosko -- and crew/seed/up.l's
# verb table, which love/cli.l's verb rail reads: `love up URL` syncs ~/.love/src
# and cook-installs the nest; `love seed|cook|kore|kiosko|mooncc ..` are the same
# binary being multi-call. the bake rides --bake's own lane (main.c's
# LOVE_BAKE_LOAD evals the cat ahead of the cache-empty + seal), so the artifact
# is the default binary with a bigger image -- no session layer, same sealing.
# member order is the scope: kore's floor first, asbook before the backends
# (defbackend mutates the spliced holo), every main before kore.l's applet
# table, up.l LAST so the verbs close over the lot. DIST_ORIGIN pins the
# default `love up` origin URL ahead of up.l (unset: up asks for a URL).
distfiles = crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l \
            crew/kore/sed.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l \
            crew/vi/core.l crew/vi/vi.l \
            crew/kore/diff.l tools/ain.l $(lushfiles) crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
            crew/moon/gen.l crew/moon/lib/mksys.l crew/moon/moon.l crew/kore/kore.l crew/seed/merge.l \
            crew/seed/http.l crew/seed/seed.l crew/kiosko/kiosko.l crew/seed/up.l
DIST_ORIGIN ?=
out/dist/.dist-cat.l: $(distfiles)
	@echo AI	$(abspath $@)
	@mkdir -p out/dist
	@{ echo '(: origin "$(DIST_ORIGIN)")'; cat $(distfiles); } > $@
# the artifact is named for its arch ($a = uname -m): love-x86_64 here,
# love-aarch64 on a pi -- the moon lane is native on both (mooncc defaults to
# the ground it stands on), so `make dist` anywhere bakes that machine's door.
out/dist/love-$a: $(ho)/love $(ho)/love.baked out/dist/.dist-cat.l
	@echo DIST	$(abspath $@)
	@cp $(ho)/love $@
	@LOVE_BAKE_LOAD=out/dist/.dist-cat.l ./$@ --bake
	@echo "  dist: $$(du -h $@ | cut -f1) -> $@"
.PHONY: dist
dist: out/dist/love-$a

# ==== dist_cross: the TWIN artifact (the other elf arch) ====
# the same door for the machine you are not on: every TU through `mooncc -t`,
# the twin's mksys leaf, our -pie link -- and the bake RUNS the twin under
# qemu-user (the one foreign tool here, and only at build time: --bake boots
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
xobjs = $(xd)/love.o $(xhost_o) $(xd)/nolibc.o $(xmath_o) $(xd)/sys.o
$(xd)/love.o: love.c $(love_h) out/host/mooncc0.image
	@echo MOONX	$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c $< $@
$(xd)/host_%.o: host/%.c $(love_h) out/host/mooncc0.image
	@echo MOONX	$@
	@mkdir -p $(dir $@)
	@$(moonx) -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c $< $@
$(xd)/host_main.o: out/lib/egg.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h out/lib/post.h out/lib/uu.h $(holo_h) $(glaze_h)
$(xd)/host_cb.o: crew/quay/quay.c crew/quay/quay.h
$(xd)/nolibc.o: crew/moon/lib/nolibc.c out/host/mooncc0.image
	@echo MOONX	$@
	@mkdir -p $(dir $@)
	@$(moonx) -Icrew/moon/include -c $< $@
$(xd)/m_%.o: crew/moon/lib/math/%.c out/host/mooncc0.image
	@echo MOONX	$@
	@mkdir -p $(dir $@)
	@$(moonx) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
$(xd)/sys.o: $(ho)/.mksys-cat.l $(love0)
	@echo MOONX	$@
	@mkdir -p $(dir $@)
	@$(love0) -l $(ho)/.mksys-cat.l -e '($(xmksys) "$@")' && test -s $@
out/dist/love-$(xarch): $(xobjs) out/dist/.dist-cat.l
	@echo DIST	$(abspath $@)
	@$(moonx) -pie $(xobjs) -o $@
	@LOVE_BAKE_LOAD=out/dist/.dist-cat.l $(xqemu) ./$@ --bake
	@echo "  dist: $$(du -h $@ | cut -f1) -> $@ (the $(xarch) twin, baked under $(xqemu))"
.PHONY: dist_cross
dist_cross: out/dist/love-$(xarch)
