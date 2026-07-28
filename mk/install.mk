# mk/install.mk -- install / uninstall
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk.
#
# the NEST: the default install is ~/.love, a self-implying home -- the loader's
# seat walk (love/prel.l) derives <seat>/../lib and its love/ subfolder from the
# running binary's own path, so ~/.love/bin/love finds ~/.love/lib/love/<x>.l
# with no configuration and no env vars. ~/.local/bin gets a compat SYMLINK per
# bin (PATH already knows it; the user edits nothing). a distro overrides
# PREFIX/DESTDIR (PREFIX=usr/ DESTDIR=$$pkgdir) and the same lib/love/ layout
# is the walk's FHS rung: /usr/bin/love finds /usr/lib/love/<x>.l -- one
# layout, both worlds; the compat links are nest-only.

# --- install / uninstall --------------------------------------------
PREFIX ?= .love/
VIMPREFIX ?= .vim/
DESTDIR ?= $(HOME)/
# BIN -- the name the interpreter installs under, and the ONE knob for the LÖVE
# collision: Arch's extra/love owns /usr/bin/love AND man1/love.1 outright, Debian
# sidesteps it as love-11.5. Nothing below hardcodes the command name any more, so
# `make install BIN=lovelang` moves the binary, both shim scripts, all six shebangs
# and the man page together. The default installs byte-identically to before.
# The PROJECT is still love: lib/love/, liblove, love.h and love/*.l keep the name
# (data paths, not PATH entries -- the shims find lib/love/ relative to themselves).
BIN ?= love
BINUP = $(shell echo '$(BIN)' | tr '[:lower:]' '[:upper:]')
d = $(DESTDIR)/$(PREFIX)
v = $(DESTDIR)/$(VIMPREFIX)

# A shebang-carrying source tool installs as a SYMLINK to the source under the
# default name, so edits land without a reinstall. Under a renamed BIN the file's
# own `#!/usr/bin/env -S love -l` would re-exec the wrong interpreter, so it
# installs as a COPY with line 1 rewritten -- which is what a package wants anyway
# (a symlink into a source tree is useless off this machine). The pattern matches
# both shebang forms, leaving a trailing ` -l` alone.
ifeq ($(BIN),love)
instool = ln -sf $(abspath $1) $2
instag = LN
else
instool = sed '1s|env -S love|env -S $(BIN)|' $1 > $2 && chmod 755 $2
instag = CP
endif

# the module sources the seat walk serves: (use 'cook) etc from ANY session of
# this love resolves here. installed DEREFERENCED (install(1) follows the repo
# lib/ symlinks), so the nest stands alone; lib/seed.l is the assembly and its
# seed/ parts ride the slashed-include rung the same way.
libmods = cook json kiosko lapiz papel rune seed seed/text seed/diff seed/merge seed/http seed/core
installs = \
  $d/bin/$(BIN) \
  $d/bin/ai \
  $d/bin/kore \
  $d/bin/seed \
  $d/bin/mooncc \
  $d/bin/moonfmt \
  $d/bin/cook \
  $d/bin/papel \
  $d/bin/kiosko \
  $d/bin/ain \
  $d/bin/lux \
  $d/bin/bao \
  $d/share/man/man1/$(BIN).1 \
  $d/share/man/man1/cook.1 \
  $d/lib/love/prel.l \
  $d/lib/love/ev.l \
  $d/lib/love/bao.l \
  $d/lib/love/mooncc.image \
  $(patsubst %,$d/lib/love/%.l,$(libmods)) \
  $d/lib/liblove.a \
  $d/lib/liblove.so \
  $d/include/love.h \
  $v/ftdetect/love.vim \
  $v/syntax/love.vim \
  $v/ftplugin/love.vim

# the PATH door, nest-only: each bin (and man page) gets a ~/.local compat
# symlink, since ~/.local/bin is already on PATH and ~/.local/share/man on
# manpath. a real PREFIX (a distro) skips these.
ifeq ($(PREFIX),.love/)
compat = $(DESTDIR)/.local
binnames = $(BIN) ai kore seed mooncc moonfmt cook papel kiosko ain lux bao
installs += $(patsubst %,$(compat)/bin/%,$(binnames)) \
  $(compat)/share/man/man1/$(BIN).1 $(compat)/share/man/man1/cook.1
$(compat)/bin/%: $d/bin/%
	@echo LN	$(abspath $@)
	@mkdir -p $(@D)
	@ln -sf $(abspath $<) $@
$(compat)/share/man/man1/%.1: $d/share/man/man1/%.1
	@echo LN	$(abspath $@)
	@mkdir -p $(@D)
	@ln -sf $(abspath $<) $@
endif

install: $(installs)
uninstall:
	@echo RM	$(abspath $(installs))
	@rm -f $(installs)

$d/include/love.h: love.h
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$d/lib/love/%.l: love/%.l
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

# the crew module sources ride the repo lib/ (this rule is SECOND, so love/%.l
# wins where both could match -- prel/ev/bao come from love/, the crew set from
# lib/'s symlinks, dereferenced by install(1))
$d/lib/love/%.l: lib/%.l
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

# the embeddable libs install GLIBC always: a musl-compiled archive poisons a
# glibc link (the sigsetjmp note in the host block), and a musl .so is useless
# to a dynamic (glibc) consumer. Under STATIC=1 the canonical out/host (glibc)
# tree is built on demand by a sub-make.
glibc_ho = out/host
ifneq ($(glibc_ho),$(ho))
$(glibc_ho)/liblove.a $(glibc_ho)/liblove.so: force_hostcc
	@$(MAKE) --no-print-directory STATIC=0 $@
endif

$d/lib/liblove.a: $(glibc_ho)/liblove.a
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$d/lib/liblove.so: $(glibc_ho)/liblove.so
	@echo CP	$(abspath $@)
	@install -D -m 755 -s $< $@

# UNSTRIPPED, deliberately: stripping would drop the symbol table holo lays on
# purpose (nm and gdb read it), and it costs ~2% of a baked binary. binutils
# strip is SAFE on our ELF now -- every loaded byte has a covering section
# header (link.l's ai_rela; nest.sh strips a copy and runs it to keep it so) --
# so a user who wants it smaller can strip their own.
$d/bin/$(BIN): $(ho)/love $(ho)/love.baked
	@echo CP	$(abspath $@)
	@install -D -m 755 $< $@
# compat: `ai` was the name from 2026-06-15 until the reversion to `love`. A
# script on a user's disk carrying `#!/usr/bin/env -S ai -l` keeps working.
# (Unclaimed in Debian and the Arch repos as of 2026-07; the AUR's terminal-ai
# already declares Provides: ai, so this alias is the first to drop in a package.)
$d/bin/ai: $d/bin/$(BIN)
	@echo LN	$(abspath $@)
	@ln -sf $(BIN) $@
# the boot image travels INSIDE the binary (.image is an allocated PROGBITS
# section), so the plain-copy install keeps the ~4ms wake.

# cook: the build tool (crew/cook/cook.l) installed as an executable `cook` on PATH.
# Its `#!/usr/bin/env -S love -l` shebang re-execs the installed `love` to load it,
# then it discovers a Makefile/Cookfile/Cards.l in the cwd. Installed as a SYMLINK
# to the source so edits to crew/cook/cook.l are picked up without a reinstall.
$d/bin/cook: crew/cook/cook.l
	@echo $(instag)	$(abspath $@)
	@mkdir -p $(@D)
	@$(call instool,$<,$@)

# papel: the static site generator (crew/papel/papel.l), and kiosko the static web
# server (crew/kiosko/kiosko.l). Same shebang + SYMLINK mechanism as cook. papel READS
# its siblings (lapiz for the markdown, cook for the staleness) rather than being -l'd
# alongside them -- two tool files cannot both be -l'd, since each one's seat would fire
# on the other's command line -- and it finds them by READLINK'ing this very symlink back
# to the source tree, so the link on PATH and the crew directory need not be neighbours.
$d/bin/papel: crew/papel/papel.l
	@echo $(instag)	$(abspath $@)
	@mkdir -p $(@D)
	@$(call instool,$<,$@)

$d/bin/kiosko: crew/kiosko/kiosko.l
	@echo $(instag)	$(abspath $@)
	@mkdir -p $(@D)
	@$(call instool,$<,$@)

# moonfmt: the C formatter (crew/moon/fmt.l). Same single-file shebang mechanism as
# cook (`#!/usr/bin/env -S love -l` re-execs the installed `love`; the SEAT inside fires
# on its own name and quits). Installed as a SYMLINK to the source, so edits to
# crew/moon/fmt.l are picked up without a reinstall.
$d/bin/moonfmt: crew/moon/fmt.l
	@echo $(instag)	$(abspath $@)
	@mkdir -p $(@D)
	@$(call instool,$<,$@)

# ain: the netcat clone (tools/ain.l). Same shebang-script mechanism as cook
# (`#!/usr/bin/env -S love -l` re-execs the installed `love` to load it); the SEAT
# form inside the file finds its own name on the command line and fires.
# (a COPY, not a symlink like cook -- so it takes the shebang rewrite unconditionally;
# at the default BIN the sed is an identity and the bytes are unchanged.)
$d/bin/ain: tools/ain.l
	@echo CP	$(abspath $@)
	@install -d $(@D)
	@sed '1s|env -S love|env -S $(BIN)|' $< > $@
	@chmod 755 $@

# kore: the multi-call toolbox -- ONE catted script (busybox's trick), the util
# picked off the command line (`kore diff A B`, `kore nc H P`, `kore make`, `kore as ..`)
# or off argv[0] through a tool-named symlink. Shadows nothing on the host: only
# `kore` lands on PATH; the distro symlinks the tool names when shadowing is the
# point. The tool files' SEATs stay quiet inside the cat (no file of theirs sits
# in the program seat), so crew/kore/kore.l's dispatcher is the one thing firing.
$d/bin/kore: $(korefiles)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN)'; sed 's|^#!/usr/bin/env -S love|#!/usr/bin/env -S $(BIN)|' $(korefiles); } > $@
	@chmod 755 $@

# seed: the patch-set vcs -- its own catted script (out/host/seed's shape; the
# sources carry no shebangs, so the interpreter line then the plain cat). its
# SEAT fires on the installed name, the same as the build-tree script's.
$d/bin/seed: $(seedfiles)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN)'; cat $(seedfiles); } > $@
	@chmod 755 $@

# mooncc: the C compiler, ITS OWN app (doc/moon.md). The installed bin is a WAKE SHIM:
# it boots the baked mooncc IMAGE next door (--wake, ~ms) and fires moon-main on the
# args -- the whole-cat re-eval (~1.3 s at every compile) is paid ONCE, at bake.
# The image is baked by the build binary against the build cat (below); strip
# keeps .text/.rodata vaddrs, so the stripped installed love wakes it fine -- but
# it IS binary-specific (anchor-checked), so image and binary always install
# from the same build. Kept OUT of the kore cat so a cc edit never forces an kore
# rebuild and vice versa.
# the home comes off the CHASED path (readlink -f): invoked through a ~/.local
# compat symlink, $0's own dir has no lib/ sibling -- the nest does.
$d/bin/mooncc: $(MAKEFILE_LIST)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'exec "$$h/$(BIN)" --wake "$$h/../lib/love/mooncc.image" -e "(moon-main (cuup (cup cmdline)))" "$$@"'; } > $@
	@chmod 755 $@
$d/lib/love/mooncc.image: $(ho)/mooncc.image
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

# lux: the window manager (crew/lux/*.l), the seven modules catted into one shebang
# script. DISPLAY picks the socket, ~/.Xauthority the cookie (crew/lux/config.l);
# mod+q restarts in place by exec'ing this same script.
luxfiles = crew/lux/core.l crew/lux/layout.l crew/lux/wire.l crew/lux/ewmh.l crew/lux/manage.l crew/lux/keys.l crew/lux/config.l crew/lux/lux.l
$d/bin/lux: $(luxfiles)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN) -l'; cat $(luxfiles); } > $@
	@chmod 755 $@

# bao: the interactive shell. Unlike crew/cook/ain, love/bao.l is DEFINE-ONLY (the
# launch `(bao 0)` is normally fired by main.c on a tty), so the bin is a tiny
# relocatable launcher: it loads the installed bao.l next door and fires it.
$d/bin/bao: $(MAKEFILE_LIST)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'exec "$$h/$(BIN)" -l "$$h/../lib/love/bao.l" -e "((from '\''bao '\''bao) 0)" "$$@"'; } > $@
	@chmod 755 $@

# the .TH command name follows BIN too (`man lovelang` should not head LOVE(1));
# the other `love`s on that line are the PROJECT and the version string, so they stay.
$d/share/man/man1/$(BIN).1: $(ho)/love.1
	@echo CP	$(abspath $@)
	@install -d $(@D)
	@sed '1s|"LOVE"|"$(BINUP)"|' $< > $@
	@chmod 644 $@

$d/share/man/man1/cook.1: $(ho)/cook.1
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$v/ftdetect/love.vim: vim/ftdetect.vim
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$v/syntax/love.vim: vim/syntax.vim
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$v/ftplugin/love.vim: vim/ftplugin.vim
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@
