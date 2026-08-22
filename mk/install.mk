# mk/install.mk -- install / uninstall. Included by ./Makefile from the project root;
# shared vars are mk/common.mk.
#
# THE NEST: the default install is ~/.love, a self-implying home, and what lands in it
# is the BINARY and the things a person reads -- man pages and the vim files. ⚠ nothing
# else needs to: the loader resolves modules out of the baked image (the modules arc,
# rung 3) and mooncc carries its own headers and runtime (the bare cc door), so a nest
# holding copies of either would be serving nobody.
# ~/.local/bin gets a compat SYMLINK per bin, since PATH already knows it.

# --- install / uninstall --------------------------------------------
PREFIX ?= .love/
VIMPREFIX ?= .vim/
DESTDIR ?= $(HOME)/
# BIN -- the name the interpreter installs under, and the ONE knob for the LÖVE collision
# (Arch's extra/love owns /usr/bin/love and man1/love.1 outright). Nothing below hardcodes
# the command name, so `make install BIN=lovelang` moves the binary, both shims, every
# shebang and the man page together. ⚠ the PROJECT is still love: lib/love/, liblove,
# core/love.h and love/*.l keep the name -- data paths, not PATH entries.
BIN ?= love
BINUP = $(shell echo '$(BIN)' | tr '[:lower:]' '[:upper:]')
d = $(DESTDIR)/$(PREFIX)
v = $(DESTDIR)/$(VIMPREFIX)

# A shebang-carrying source tool installs as a SYMLINK to the source under the default
# name, so edits land without a reinstall. Under a renamed BIN its own
# `#!/usr/bin/env -S love -l` would re-exec the wrong interpreter, so it installs as a COPY
# with line 1 rewritten -- which is what a package wants anyway. The pattern matches both
# shebang forms, leaving a trailing ` -l` alone.
# the sed these recipes spawn is OURS: kore is the installed binary's own verb now
# (the layered bake, doc/misc/plan/one-binary.md). ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads,
# for $(hcc)'s reason: the root exports it=1 for the corpus, and an egg-booted love has
# no verbs -- `kore` would read as a filename.
korecmd = LOVE_NO_IMAGE= $(ho)/love kore
ifeq ($(BIN),love)
# ⚠ the chmod repairs the target when the source came through svalbard, which does not carry
# the executable bit -- without it the link resolves to a 644 file and every exec EACCESes.
instool = ln -sf $(abspath $1) $2 && chmod 755 $(abspath $1)
instag = LN
else
instool = $(korecmd) sed '1s|env -S love|env -S $(BIN)|' $1 > $2 && chmod 755 $2
instag = CP
endif

# ⚠ ONE roster each: the compat-symlink block below reads the same two names, and two
# spellings of a list is how they drift.
binnames = $(BIN) kore sb mooncc cook papel kiosko libra ain lux bao lush
mannames = $(BIN) cook lush
installs = $(patsubst %,$d/bin/%,$(binnames)) \
  $(patsubst %,$d/share/man/man1/%.1,$(mannames)) \
  $v/ftdetect/love.vim $v/syntax/love.vim $v/ftplugin/love.vim

# the plain data install, spelled once -- a dozen rules below wear it.
inst644 = @echo CP	$(abspath $@); install -D -m 644 $< $@

# the PATH door, nest-only: each bin and man page gets a ~/.local compat symlink, since
# those are already on PATH and manpath. A real PREFIX (a distro) skips them.
ifeq ($(PREFIX),.love/)
compat = $(DESTDIR)/.local
installs += $(patsubst %,$(compat)/bin/%,$(binnames)) \
  $(patsubst %,$(compat)/share/man/man1/%.1,$(mannames))
inln = @echo LN	$(abspath $@); mkdir -p $(@D); ln -sf $(abspath $<) $@
$(compat)/bin/%: $d/bin/%
	$(inln)
$(compat)/share/man/man1/%.1: $d/share/man/man1/%.1
	$(inln)
endif

install: $(installs)
uninstall:
	@echo RM	$(abspath $(installs))
	@rm -f $(installs)

# UNSTRIPPED deliberately: stripping drops the symbol table holo lays on purpose, for ~2%
# of a baked binary. binutils strip IS safe on our ELF (every loaded byte has a covering
# section header), so a user who wants it smaller can strip their own.
$d/bin/$(BIN): $(ho)/love $(ho)/love.baked
	@echo CP	$(abspath $@)
	@install -D -m 755 $< $@
# the boot image travels INSIDE the binary (.image is an allocated PROGBITS section, the
# layered crew chain riding it), so the plain-copy install keeps the warm wake and every verb.

# the single-file shebang tools, one shape: the `#!/usr/bin/env -S love -l` line re-execs
# the installed interpreter, and each file's own SEAT fires on its name. papel and libra
# READ their siblings rather than being -l'd beside them -- two tool files cannot both be
# -l'd, since each one's seat would fire on the other's command line -- and they find them
# by READLINK'ing this very symlink back to the source tree, so the link on PATH and the
# crew directory need not be neighbours. libra's siblings are named ((use 'lint),
# (use 'salt), and (use 'lapiz) on the doc verb alone) and ride the baked image.
# ⚠ each source sits FIRST on its own line: instool reads $<, and a prerequisite added on
# the grouped line below lands ahead of it -- which installs the kore shim as `cook`.
$d/bin/cook:    crew/cook/cook.l    $(ho)/love.baked
$d/bin/papel:   crew/papel/papel.l  $(ho)/love.baked
$d/bin/kiosko:  crew/kiosko/kiosko.l $(ho)/love.baked
$d/bin/libra:   crew/libra/libra.l  $(ho)/love.baked
$d/bin/cook $d/bin/papel $d/bin/kiosko $d/bin/libra:
	@echo $(instag)	$(abspath $@)
	@mkdir -p $(@D)
	@$(call instool,$<,$@)

# ain, the netcat clone: the same shebang mechanism, but installed as a COPY rather than a
# symlink, so it takes the rewrite unconditionally. At the default BIN the substitution is
# an identity and the bytes are unchanged.
$d/bin/ain: tools/ain.l $(ho)/love.baked
	@echo CP	$(abspath $@)
	@install -d $(@D)
	@$(korecmd) sed '1s|env -S love|env -S $(BIN)|' $< > $@
	@chmod 755 $@

# kore, the multi-call toolbox: the util picked off the command line or off argv[0] through
# a tool-named symlink. It shadows nothing here -- only `kore` lands on PATH, and the distro
# symlinks the tool names where shadowing is the point.
# A VERB SHIM: the installed binary carries the crew in its own layered image
# (doc/misc/plan/one-binary.md), so there is no sibling image and no wake spelling -- the
# picker wakes the crew layer off the `kore` verb, same warm start as ever.
# ⚠ `n` comes off $0 UNCHASED where `h` is the chased path: a tool symlink must arrive as its
# own name for the argv[0] door, and only the real file's dir has the $(BIN) sibling.
$d/bin/kore: $(MAKEFILE_LIST)
	@echo CAT	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'n=$$(basename -- "$$0")'; \
	   echo 'case "$$n" in kore) LOVE_NO_IMAGE= exec "$$h/$(BIN)" kore "$$@";; *) LOVE_NO_IMAGE= exec "$$h/$(BIN)" kore "$$n" "$$@";; esac'; } > $@
	@chmod 755 $@

# sb 🌱 and lush 🐚, each its own catted script: their sources carry no shebangs, so the
# interpreter line then a plain cat. Each SEAT fires on the installed name -- lush's on its
# basename, so `sh` through a symlink lands too.
$d/bin/sb: $(sbfiles)
$d/bin/lush: $(lushfiles)
$d/bin/sb $d/bin/lush:
	@echo CAT	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN)'; cat $^; } > $@
	@chmod 755 $@

# mooncc: the same verb-shim shape -- the compiler is the installed binary's own verb,
# its layer woken by the picker (~ms, the whole-cat re-eval long gone). ⚠ the home comes
# off the CHASED path (readlink -f): invoked through a ~/.local compat symlink, $0's own
# dir has no $(BIN) sibling -- the nest does.
$d/bin/mooncc: $(MAKEFILE_LIST)
	@echo CAT	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'LOVE_NO_IMAGE= exec "$$h/$(BIN)" mooncc "$$@"'; } > $@
	@chmod 755 $@

# lux, the window manager: its modules catted into one shebang script. Settings ride salt
# (~/.love/etc/lux.l then ./.lux.l), which also names the display and the cookie when
# DISPLAY/XAUTHORITY will not do; mod+q restarts in place by exec'ing this script.
luxfiles = crew/lux/core.l crew/lux/layout.l crew/lux/wire.l crew/lux/ewmh.l crew/lux/manage.l crew/lux/keys.l crew/lux/config.l crew/lux/lux.l
$d/bin/lux: $(luxfiles)
	@echo CAT	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN) -l'; cat $(luxfiles); } > $@
	@chmod 755 $@

# bao, the interactive shell. Unlike cook and ain, love/bao.l is DEFINE-ONLY -- main.c
# fires `(bao 0)` on a tty -- so the bin is a tiny launcher that fires it. ⚠ the module
# rides the binary, so there is nothing to -l and no nest path to get wrong.
$d/bin/bao: $(MAKEFILE_LIST)
	@echo CAT	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'exec "$$h/$(BIN)" -e "((from '\''bao '\''bao) 0)" "$$@"'; } > $@
	@chmod 755 $@

# the .TH command name follows BIN too (`man lovelang` should not head LOVE(1));
# the other `love`s on that line are the PROJECT and the version string, so they stay.
$d/share/man/man1/$(BIN).1: $(ho)/love.1 $(ho)/love.baked
	@echo CP	$(abspath $@)
	@install -d $(@D)
	@$(korecmd) sed '1s|"LOVE"|"$(BINUP)"|' $< > $@
	@chmod 644 $@

# the man pages BIN does not rename, and the two hand-written vim files. ⚠ static
# patterns: an implicit rule would make these intermediate.
$d/share/man/man1/cook.1 $d/share/man/man1/lush.1: $d/share/man/man1/%.1: $(ho)/%.1
	$(inst644)
$v/ftdetect/love.vim $v/ftplugin/love.vim: $v/%/love.vim: assets/vim/%.vim
	$(inst644)
# the syntax is GENERATED (crew/build.mk) out of crew/vi/hue.l's class table and the
# vocabulary this host answers to, so it is installed from out/ like any other artifact.
$v/syntax/love.vim: $(ho)/syntax.vim
	$(inst644)
