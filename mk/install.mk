# mk/install.mk -- install / uninstall. Included by ./Makefile from the project root;
# shared vars are mk/common.mk.
#
# THE NEST: the default install is ~/.love, a self-implying home. ⚠ the loader's SEAT
# WALK IS RETIRED (the modules arc, rung 3): the binary's modules are baked or
# image-carried, and nothing resolves ~/.love/lib/love/<x>.l at runtime anymore. the
# lib tree below still installs -- source on disk for reading and for tools that open
# it by PATH -- and retires with the self-host arc's `love up` story, not before.
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
# (the layered bake, doc/plan/one-binary.md). ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads,
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

# the module sources the seat walk serves: (use 'cook) and friends from ANY session of this
# love resolve here. Installed DEREFERENCED, since install(1) follows the repo lib/
# symlinks, so the nest stands alone.
libmods = cook dns json lint salt libra kiosko lapiz papel hueweb serve rune sb sb/text sb/diff sb/merge sb/http sb/core lush lush/job lush/lex lush/gram lush/glob lush/word lush/eval lush/line lush/main
# ⚠ ONE roster each: the compat-symlink block below reads the same two names, and two
# spellings of a list is how they drift.
binnames = $(BIN) kore sb mooncc cook papel kiosko libra ain lux bao lush
mannames = $(BIN) cook lush
installs = $(patsubst %,$d/bin/%,$(binnames)) \
  $(patsubst %,$d/share/man/man1/%.1,$(mannames)) \
  $d/lib/love/prel.l $d/lib/love/ev.l $d/lib/love/bao.l \
  $(patsubst %,$d/lib/love/%.l,$(libmods)) \
  $v/ftdetect/love.vim $v/syntax/love.vim $v/ftplugin/love.vim

# the embeddable goods are the cc lane's: a shared object wants PIC codegen and a dynamic
# section holo does not lay. `love up` installs EMBED=0, its nest having no ambient cc.
EMBED ?= 1
override EMBED := $(filter-out 0,$(EMBED))
ifneq ($(EMBED),)
installs += \
  $d/lib/liblove.a \
  $d/lib/liblove.so \
  $d/include/love.h \
  $d/include/kinds.h
endif

# the plain data install, spelled once -- a dozen rules below wear it.
inst644 = @echo CP	$(abspath $@); install -D -m 644 $< $@

# THE TOOLCHAIN's own files: mooncc's headers (ours, glibc-ABI-faithful, NOT glibc's) and
# the runtime sources its implicit-libc link pulls. They land under the SEAT, at the root
# moon.l walks to, which is what lets an installed mooncc work from ANY cwd -- every path
# in the driver was cwd-relative, so outside a source tree `<stdio.h>` fell through to
# /usr/include and the link found no libc at all. ⚠ NOT read out of ~/.love/src: a package
# has no src tree and `love down` takes one away, so the copy is what makes it stand alone.
moon_hdrs = $(wildcard crew/moon/include/*.h crew/moon/include/*/*.h)
# ⚠ the nolibc tree is TWO deep since it went one-function-to-a-file: 185 of its 189
# sources sit under ctype/ dirent/ env/ fmt/ mem/ net/ sys/ .., and a one-level glob
# installs core.c alone. that reads as a working nest right up to the link -- mhome
# finds its root by core.c and then owes every member the glob left behind.
moon_srcs = $(wildcard crew/moon/lib/nolibc/*.c crew/moon/lib/nolibc/*.h \
                       crew/moon/lib/nolibc/*/*.c crew/moon/lib/nolibc/*/*.h \
                       crew/moon/lib/math/*.c)
installs += $(patsubst crew/moon/%,$d/lib/love/moon/%,$(moon_hdrs) $(moon_srcs))
$d/lib/love/moon/include/%: crew/moon/include/%
	$(inst644)
$d/lib/love/moon/lib/%: crew/moon/lib/%
	$(inst644)

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

# --- the SOURCE nest: ~/.love/src/love-<ver>/ + the tarball it came from ------------
# An installed love keeps its own source, and the archive it was cut from beside it: a
# pristine baseline to diff a working tree against, and the thing `love up` can rebuild
# from without asking the network. The tarball's top directory is already love-<ver>, so
# the version keying costs nothing -- it IS the archive's own name.
#
# ⚠ IT NEVER CLOBBERS ANOTHER VERSION. Installing 0.2 must leave 0.1 exactly where it
# stands, so an existing love-<ver>/ is LEFT ALONE rather than written into -- a
# half-overwritten source tree is worse than either version. Re-installing the same
# version is therefore a no-op on the tree; delete it by hand to force a fresh lay.
# ⚠ and there is deliberately NO `current` symlink yet: which version is live is a
# decision we have not made, and quietly picking one here would make it by accident.
.PHONY: install-src
install-src: $(dist_source)
	@mkdir -p $d/pkg $d/src
	@cp -p $(dist_source) $d/pkg/
	@echo INSTALL	$(abspath $d)/pkg/$(notdir $(dist_source))
	@if [ -d "$d/src/love-$(dist_ver)" ]; then \
	   echo "  install-src: $d/src/love-$(dist_ver) exists -- left alone (delete it to re-lay)"; \
	 else \
	   $(ho)/love tools/tgz.l x $(dist_source) $d/src >/dev/null \
	     && echo "  install-src: source laid at $d/src/love-$(dist_ver)"; fi

install: $(installs)
uninstall:
	@echo RM	$(abspath $(installs))
	@rm -f $(installs)

# the embedding surface is TWO files: core/love.h includes core/kinds.h (the kind lattice,
# laid by core/mx.l), so an installed core/love.h without it does not compile.
$d/include/%.h: core/%.h
	$(inst644)

$d/lib/love/%.l: love/%.l
	$(inst644)
# ⚠ the crew module sources ride the repo lib/, and this rule is SECOND so love/%.l wins
# where both could match: prel/ev/bao come from love/, the crew set from lib/'s symlinks.
$d/lib/love/%.l: lib/%.l
	$(inst644)

# ⚠ the embeddable libs install from the CANONICAL tree always -- an HCC build lays its own
# out/host-cc, and what a consumer links against should not depend on which cc we were
# differentialling that day. Built on demand by a sub-make when we are not in it.
glibc_ho = out/host
ifneq ($(glibc_ho),$(ho))
$(glibc_ho)/liblove.a $(glibc_ho)/liblove.so: force_hostcc
	@$(MAKE) --no-print-directory HCC=0 $@
endif

$d/lib/liblove.a: $(glibc_ho)/liblove.a
	$(inst644)

$d/lib/liblove.so: $(glibc_ho)/liblove.so
	@echo CP	$(abspath $@)
	@install -D -m 755 -s $< $@

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
# crew directory need not be neighbours. libra's are named ((use 'lint), (use 'salt), and
# (use 'lapiz) on the doc verb alone) and ride libmods above instead.
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
# (doc/plan/one-binary.md), so there is no sibling image and no wake spelling -- the
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
# fires `(bao 0)` on a tty -- so the bin is a tiny launcher that loads it and fires it.
$d/bin/bao: $(MAKEFILE_LIST)
	@echo CAT	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'exec "$$h/$(BIN)" -l "$$h/../lib/love/bao.l" -e "((from '\''bao '\''bao) 0)" "$$@"'; } > $@
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
