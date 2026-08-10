# crew/build.mk -- the crew app builds: the kore/mooncc/seed/lush scripts, their baked
# images, and the dist artifact. Included by ./Makefile after host/build.mk, so $(ho) is
# already spelled; shared vars are common.mk.

# kore: the diff engines, the text/tool surface, the line tools, and `kore` itself -- the
# multi-call toolbox picking its util off the command line or an argv[0] symlink. lush
# rides the cat too, so `kore sh` and a /bin/sh symlink are the shell and the distro's
# one-binary userland closes over its own console. ⚠ lush goes BEFORE cook.l, whose
# $(wildcard) presence-guard then reads sh-glob bare.
korefiles = crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l crew/kore/sed.l crew/kore/proc.l lib/lint.l crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l tools/ain.l $(lushfiles) crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l crew/kore/kore.l
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
	@echo AI	$(abspath $@)
	@mkdir -p $(dir $@)
	@cat $^ > $@
$(ho)/kore: $(ho)/kore.image
	@echo AI	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'n=$$(basename -- "$$0")'; \
	   echo 'exec "$$h/love" --wake "$$h/kore.image" -e "(kore-main (link \"$$n\" (cuup (cup cmdline))))" "$$@"'; } > $@
	@chmod 755 $@
$(ho)/mooncc: $(ho)/mooncc.image
	@echo AI	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$0")" && pwd)'; \
	   echo 'exec "$$h/love" --wake "$$h/mooncc.image" -e "(moon-main (cuup (cup cmdline)))" "$$@"'; } > $@
	@chmod 755 $@
# seed 🌱 the patch-set vcs, and lush 🐚 the love shell -- also the distro's console shell,
# whose SEAT in main.l fires on its own basename. Both are catted shebang scripts, PATH
# picking the love that runs them.
seedfiles = crew/kore/text.l crew/kore/diff.l lib/dns.l crew/seed/merge.l crew/seed/http.l crew/seed/seed.l
lushfiles = crew/lush/job.l crew/lush/lex.l crew/lush/gram.l crew/lush/glob.l crew/lush/word.l crew/lush/eval.l crew/lush/line.l crew/lush/main.l
$(ho)/seed: $(seedfiles)
$(ho)/lush: $(lushfiles)
$(ho)/seed $(ho)/lush:
	@echo AI	$(abspath $@)
	@mkdir -p $(dir $@)
	@{ echo '#!/usr/bin/env -S love'; cat $^; } > $@
	@chmod 755 $@
# the two WARM images (the live bake, doc/snapshot.md): each cat loads under a NEUTRAL name
# so its tail SEAT stays quiet, then the bake nif snapshots the session. LOVE_NO_IMAGE
# rides the recipe, so the bake session itself egg-boots -- same warm state every time.
$(ho)/mooncc.image $(ho)/kore.image: $(ho)/%.image: $(ho)/.%-cat.l $m
	@echo AI	$(abspath $@)
	@$m -l $< -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'
# mooncc0.image: the SAME cat baked by LOVE0, the build-time compiler that breaks the
# self-host circle -- the default love is mooncc-built, so its own image cannot drive its
# build, and love0 waking this one can. PINNED to out/host like love0 itself.
out/host/mooncc0.image: out/host/.mooncc-cat.l $(love0)
	@echo AI	$(abspath $@)
	@$(love0) -l out/host/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'

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
            crew/kore/diff.l lib/dns.l tools/ain.l $(lushfiles) crew/cook/cook.l crew/kore/asbook.l \
            crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l \
            crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l \
            crew/holo/link.l crew/holo/copy.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
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
$(xd)/host_main.o: $(baked_h)
$(xd)/host_cb.o: crew/quay/quay.c crew/quay/nif.c crew/quay/quay.h
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
	@$(love0) -l $(ho)/.mksys-cat.l -n -e '($(xmksys) "$@")' && test -s $@
out/dist/love-$(xarch): $(xobjs) out/dist/.dist-cat.l
	@echo DIST	$(abspath $@)
	@$(moonx) -pie $(xobjs) -o $@
	@LOVE_BAKE_LOAD=out/dist/.dist-cat.l $(xqemu) ./$@ --bake
	@echo "  dist: $$(du -h $@ | cut -f1) -> $@ (the $(xarch) twin, baked under $(xqemu))"
.PHONY: dist_cross
dist_cross: out/dist/love-$(xarch)
