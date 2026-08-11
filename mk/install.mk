# mk/install.mk -- install / uninstall. Included by ./Makefile from the project root;
# shared vars are common.mk.
#
# THE NEST: the default install is ~/.love, a self-implying home -- the loader's seat walk
# derives <seat>/../lib and its love/ subfolder from the running binary's own path, so
# ~/.love/bin/love finds ~/.love/lib/love/<x>.l with no configuration and no env vars.
# ~/.local/bin gets a compat SYMLINK per bin, since PATH already knows it. A distro
# overrides PREFIX/DESTDIR and the same lib/love/ layout is the walk's FHS rung
# (/usr/bin/love finds /usr/lib/love/<x>.l) -- one layout, both worlds, and the compat
# links are nest-only.

# --- install / uninstall --------------------------------------------
PREFIX ?= .love/
VIMPREFIX ?= .vim/
DESTDIR ?= $(HOME)/
# BIN -- the name the interpreter installs under, and the ONE knob for the LÖVE collision
# (Arch's extra/love owns /usr/bin/love and man1/love.1 outright). Nothing below hardcodes
# the command name, so `make install BIN=lovelang` moves the binary, both shims, every
# shebang and the man page together. ⚠ the PROJECT is still love: lib/love/, liblove,
# love.h and love/*.l keep the name -- data paths, not PATH entries.
BIN ?= love
BINUP = $(shell echo '$(BIN)' | tr '[:lower:]' '[:upper:]')
d = $(DESTDIR)/$(PREFIX)
v = $(DESTDIR)/$(VIMPREFIX)

# A shebang-carrying source tool installs as a SYMLINK to the source under the default
# name, so edits land without a reinstall. Under a renamed BIN its own
# `#!/usr/bin/env -S love -l` would re-exec the wrong interpreter, so it installs as a COPY
# with line 1 rewritten -- which is what a package wants anyway. The pattern matches both
# shebang forms, leaving a trailing ` -l` alone.
ifeq ($(BIN),love)
# ⚠ the chmod repairs the target when the source came through seed, which does not carry
# the executable bit -- without it the link resolves to a 644 file and every exec EACCESes.
instool = ln -sf $(abspath $1) $2 && chmod 755 $(abspath $1)
instag = LN
else
instool = $(ho)/kore sed '1s|env -S love|env -S $(BIN)|' $1 > $2 && chmod 755 $2
instag = CP
endif

# the module sources the seat walk serves: (use 'cook) and friends from ANY session of this
# love resolve here. Installed DEREFERENCED, since install(1) follows the repo lib/
# symlinks, so the nest stands alone.
libmods = cook dns json lint salt libra kiosko lapiz papel rune seed seed/text seed/diff seed/merge seed/http seed/core lush lush/job lush/lex lush/gram lush/glob lush/word lush/eval lush/line lush/main
# ⚠ ONE roster each: the compat-symlink block below reads the same two names, and two
# spellings of a list is how they drift.
binnames = $(BIN) kore seed mooncc moonfmt cook papel kiosko libra ain lux bao lush
mannames = $(BIN) cook lush
installs = $(patsubst %,$d/bin/%,$(binnames)) \
  $(patsubst %,$d/share/man/man1/%.1,$(mannames)) \
  $d/lib/love/prel.l $d/lib/love/ev.l $d/lib/love/bao.l $d/lib/love/mooncc.image \
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
moon_srcs = crew/moon/lib/nolibc.c $(wildcard crew/moon/lib/math/*.c)
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

install: $(installs)
uninstall:
	@echo RM	$(abspath $(installs))
	@rm -f $(installs)

# the embedding surface is TWO files: love.h includes kinds.h (the kind lattice,
# laid by mx.l), so an installed love.h without it does not compile.
$d/include/%.h: %.h
	$(inst644)

$d/lib/love/%.l: love/%.l
	$(inst644)
# ⚠ the crew module sources ride the repo lib/, and this rule is SECOND so love/%.l wins
# where both could match: prel/ev/bao come from love/, the crew set from lib/'s symlinks.
$d/lib/love/%.l: lib/%.l
	$(inst644)

# ⚠ the embeddable libs install GLIBC always: a musl-compiled archive poisons a glibc link
# (the sigsetjmp note in host/build.mk) and a musl .so is useless to a dynamic consumer.
# Under STATIC=1 the canonical out/host tree is built on demand by a sub-make.
glibc_ho = out/host
ifneq ($(glibc_ho),$(ho))
$(glibc_ho)/liblove.a $(glibc_ho)/liblove.so: force_hostcc
	@$(MAKE) --no-print-directory STATIC=0 $@
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
# the boot image travels INSIDE the binary (.image is an allocated PROGBITS section), so
# the plain-copy install keeps the ~4ms wake.

# the single-file shebang tools, one shape: the `#!/usr/bin/env -S love -l` line re-execs
# the installed interpreter, and each file's own SEAT fires on its name. papel and libra
# READ their siblings rather than being -l'd beside them -- two tool files cannot both be
# -l'd, since each one's seat would fire on the other's command line -- and they find them
# by READLINK'ing this very symlink back to the source tree, so the link on PATH and the
# crew directory need not be neighbours. libra's are named ((use 'json), (use 'lint)) and
# ride libmods above instead.
# ⚠ each source sits FIRST on its own line: instool reads $<, and a prerequisite added on
# the grouped line below lands ahead of it -- which installs the kore shim as `cook`.
$d/bin/cook:    crew/cook/cook.l    $(ho)/kore
$d/bin/papel:   crew/papel/papel.l  $(ho)/kore
$d/bin/kiosko:  crew/kiosko/kiosko.l $(ho)/kore
$d/bin/libra:   crew/libra/libra.l  $(ho)/kore
$d/bin/moonfmt: crew/moon/fmt.l     $(ho)/kore
$d/bin/cook $d/bin/papel $d/bin/kiosko $d/bin/libra $d/bin/moonfmt:
	@echo $(instag)	$(abspath $@)
	@mkdir -p $(@D)
	@$(call instool,$<,$@)

# ain, the netcat clone: the same shebang mechanism, but installed as a COPY rather than a
# symlink, so it takes the rewrite unconditionally. At the default BIN the substitution is
# an identity and the bytes are unchanged.
$d/bin/ain: tools/ain.l $(ho)/kore
	@echo CP	$(abspath $@)
	@install -d $(@D)
	@$(ho)/kore sed '1s|env -S love|env -S $(BIN)|' $< > $@
	@chmod 755 $@

# kore, the multi-call toolbox: ONE catted script, the util picked off the command line or
# off argv[0] through a tool-named symlink. It shadows nothing here -- only `kore` lands on
# PATH, and the distro symlinks the tool names where shadowing is the point. The member
# SEATs stay quiet inside the cat, so kore.l's dispatcher is the one thing firing.
$d/bin/kore: $(korefiles) $(ho)/kore
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN)'; $(ho)/kore sed 's|^#!/usr/bin/env -S love|#!/usr/bin/env -S $(BIN)|' $(korefiles); } > $@
	@chmod 755 $@

# seed 🌱 and lush 🐚, each its own catted script: their sources carry no shebangs, so the
# interpreter line then a plain cat. Each SEAT fires on the installed name -- lush's on its
# basename, so `sh` through a symlink lands too.
$d/bin/seed: $(seedfiles)
$d/bin/lush: $(lushfiles)
$d/bin/seed $d/bin/lush:
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN)'; cat $^; } > $@
	@chmod 755 $@

# mooncc, its own app: the installed bin is a WAKE SHIM booting the baked image next door,
# so the whole-cat re-eval (~1.3s per compile) is paid ONCE at bake. ⚠ the image is
# anchor-checked to its binary, so the two always install from the same build. ⚠ and the
# home comes off the CHASED path (readlink -f): invoked through a ~/.local compat symlink,
# $0's own dir has no lib/ sibling -- the nest does.
$d/bin/mooncc: $(MAKEFILE_LIST)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'exec "$$h/$(BIN)" --wake "$$h/../lib/love/mooncc.image" -e "(moon-main (cuup (cup cmdline)))" "$$@"'; } > $@
	@chmod 755 $@
$d/lib/love/mooncc.image: $(ho)/mooncc.image
	$(inst644)

# lux, the window manager: its modules catted into one shebang script. Settings ride salt
# (~/.love/etc/lux.l then ./.lux.l), which also names the display and the cookie when
# DISPLAY/XAUTHORITY will not do; mod+q restarts in place by exec'ing this script.
luxfiles = crew/lux/core.l crew/lux/layout.l crew/lux/wire.l crew/lux/ewmh.l crew/lux/manage.l crew/lux/keys.l crew/lux/config.l crew/lux/lux.l
$d/bin/lux: $(luxfiles)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/usr/bin/env -S $(BIN) -l'; cat $(luxfiles); } > $@
	@chmod 755 $@

# bao, the interactive shell. Unlike cook and ain, love/bao.l is DEFINE-ONLY -- main.c
# fires `(bao 0)` on a tty -- so the bin is a tiny launcher that loads it and fires it.
$d/bin/bao: $(MAKEFILE_LIST)
	@echo AI	$(abspath $@)
	@install -d $(dir $@)
	@{ echo '#!/bin/sh'; \
	   echo 'h=$$(CDPATH= cd -- "$$(dirname -- "$$(readlink -f -- "$$0")")" && pwd)'; \
	   echo 'exec "$$h/$(BIN)" -l "$$h/../lib/love/bao.l" -e "((from '\''bao '\''bao) 0)" "$$@"'; } > $@
	@chmod 755 $@

# the .TH command name follows BIN too (`man lovelang` should not head LOVE(1));
# the other `love`s on that line are the PROJECT and the version string, so they stay.
$d/share/man/man1/$(BIN).1: $(ho)/love.1 $(ho)/kore
	@echo CP	$(abspath $@)
	@install -d $(@D)
	@$(ho)/kore sed '1s|"LOVE"|"$(BINUP)"|' $< > $@
	@chmod 644 $@

# the man pages BIN does not rename, and the three vim files. ⚠ static patterns: an
# implicit rule would make these intermediate.
$d/share/man/man1/cook.1 $d/share/man/man1/lush.1: $d/share/man/man1/%.1: $(ho)/%.1
	$(inst644)
$v/ftdetect/love.vim $v/syntax/love.vim $v/ftplugin/love.vim: $v/%/love.vim: vim/%.vim
	$(inst644)
	@install -D -m 644 $< $@
