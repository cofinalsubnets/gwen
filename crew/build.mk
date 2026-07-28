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
korefiles = crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l crew/kore/sed.l crew/kore/proc.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l tools/ain.l crew/cook/cook.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/kore/kore.l
# mooncc: the C compiler is its OWN app, NOT baked into the kore cat -- a cc edit rebuilds
# only mooncc (never kore), so an kore rebuild in another session can't tear the compiler.
# Its own cat: the u-floor (text+core), then asbook splices the boot-registered holo
# and the CROSS BACKENDS join it (their defs ride the session layer; defbackend
# mutates holo's own table -- so mooncc cross-compiles every target regardless of
# which single backend the host image baked), elf/obj/link writers, then
# crew/moon/{lex,cpp,parse,gen,cc}.l whose tail SEAT fires.
# (crew/holo/text.l = the neutral-text assembler front end gen.l's inline asm
# parses templates with; it leaks stream globals incl. a `parse` rebind -- fine
# here, no later cat member reads them bare.)
moonfiles = crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/moon/gen.l crew/moon/moon.l
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
	@cat $(korefiles) > $@
out/host$(hsuf)/kore: out/host$(hsuf)/kore.image
	@echo AI	$(abspath $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$0")" && pwd)'; \
	   echo 'n=$$(basename -- "$$0")'; \
	   echo 'exec "$$h/love" --wake "$$h/kore.image" -e "(kore-main (link \"$$n\" (cuup (cup cmdline))))" "$$@"'; } > $@
	@chmod 755 $@
out/host$(hsuf)/.mooncc-cat.l: $(moonfiles)
	@echo AI	$(abspath $@)
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
	@{ echo '#!/usr/bin/env -S love'; cat $(seedfiles); } > $@
	@chmod 755 $@
# the mooncc image: the compiler baked WARM (the live bake, doc/snapshot.md). The
# cat loads under a NEUTRAL name so moon.l's tail SEAT stays quiet, then the bake
# nif snapshots the session. LOVE_NO_IMAGE rides the recipe (exported above), so
# the bake session itself egg-boots -- same warm state, deterministically.
$(ho)/mooncc.image: $(ho)/.mooncc-cat.l $m
	@echo AI	$(abspath $@)
	@$m -l $(ho)/.mooncc-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'
# the kore image: the multi-call toolbox baked WARM, the mooncc.image precedent. the
# cat loads under a NEUTRAL name so kore.l's SEAT me? is false and stays quiet, then
# the bake snapshots. test_kore wakes it per tool (`--wake kore.image -e '(kore-main
# (link "kore" (cuup (cup cmdline))))'`) -- ~0.02s vs ~0.75s cold, across its ~77 spawns.
$(ho)/kore.image: $(ho)/.kore-cat.l $m
	@echo AI	$(abspath $@)
	@$m -l $(ho)/.kore-cat.l -e '(? ((bake "$@") = 1) (quit 0) (quit 1))'
