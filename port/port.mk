# port/port.mk -- the shape every bare-metal port shares. Included by port/<x>/Makefile,
# each of which is run from its own folder (`make -C port/<x>`); $(R) is the project root,
# and mooncc's system include dir is CWD-relative, so every compile cd's to $(R) first.
#
# Set before including: p_tgt (mooncc's -t backend) and p_be (the holo backend .l the lays
# splice in; p_link_be if the link wants a different set). Optional: lib_h + p_hdrs, the
# lcat'd headers to delegate to the root.
#
# Answers: R o MOONCC mc lv, .DELETE_ON_ERROR, clean, FORCE and its three delegations, the
# lay_l/link_l/copy_l cats, the am.o and ocopy.l rules, and the p_obj/p_lay/p_link shapes.

R := ../..
p_dir = $(notdir $(CURDIR))
o = out/$(p_dir)
# mooncc is love's own verb (the layered bake, doc/plan/one-binary.md). MOONCC is the
# command as run FROM $(R); mc is the file the verb needs, the baked-stamp's sibling.
# ⚠ LOVE_NO_IMAGE= leads (the guard against an exported egg): an egg-booted love has no verbs.
MOONCC = LOVE_NO_IMAGE= out/host/love mooncc
mc = $(R)/out/host/love.baked
lv = $(R)/out/host/love

# a failed recipe takes its half-written target with it -- else a 0-byte artifact carries a
# fresh mtime and the next make calls it up to date. the root does not include these
# makefiles, so it is said here rather than inherited. see ../../Makefile.
.DELETE_ON_ERROR:

# the include sits above the port's own rules, so name the goal rather than letting the
# first target win it. every port points `default` at its own artifact.
.DEFAULT_GOAL := default
.PHONY: default clean FORCE

clean:
	rm -rf $(R)/$(o)

# ⚠ FORCE, never a bare prerequisite-less rule: that fires only when the target is MISSING,
# so a stale header or binary is served forever. ⚠ and the explicit binary rules also block
# make's builtin `%: %.o` -- out/host/love.o sits beside the binary, and a bare prerequisite
# let the builtin "relink" love from that lone object, then delete the half-made result.
FORCE:
# lib_hR is what an OBJECT depends on; p_hdrs only widens what gets delegated, for a
# header some faces want and the rest must not rebuild for.
lib_hR = $(addprefix $(R)/,$(lib_h))
ifneq ($(strip $(lib_h) $(p_hdrs)),)
$(sort $(lib_hR) $(addprefix $(R)/,$(p_hdrs))): FORCE
	@$(MAKE) -C $(R) $(patsubst $(R)/%,%,$@)
endif
$(lv): FORCE
	@$(MAKE) -C $(R) out/host/love
$(mc): FORCE
	@$(MAKE) -C $(R) out/host/love.baked

# the holo cats. ⚠ the backend text is named explicitly: a frontend bakes holo with the
# NATIVE backend only, and a port must not care which machine it is building on.
p_link_be ?= $(p_be)
p_be_l    = $(addprefix $(R)/crew/holo/,$(addsuffix .l,$(p_be)))
p_lnbe_l  = $(addprefix $(R)/crew/holo/,$(addsuffix .l,$(p_link_be)))
# ⚠ THE FLOOR IS CATTED, THEN SPLICED. text.l and u.l reopen module 'kore, so their
# names (uread, udie ..) do not walk for whoever comes after -- and every driver below
# reads them bare. So the cats emit (use 'kore) once the two files have registered it,
# free/kernel.mk's klink recipe exactly.
kore_l = $(R)/crew/kore/text.l $(R)/crew/kore/u.l
lay_l  = $(kore_l) $(R)/crew/kore/asbook.l \
  $(R)/crew/holo/elf.l $(R)/crew/holo/obj.l
link_l = $(kore_l) $(R)/crew/kore/asbook.l \
  $(p_lnbe_l) $(R)/crew/holo/elf.l $(R)/crew/holo/obj.l $(R)/crew/holo/link.l
copy_l = $(link_l) $(R)/crew/holo/copy.l
# the same lists spelled from $(R), which is where the cats run
lay_lc  = $(subst $(R)/,,$(lay_l))
kore_lc = $(subst $(R)/,,$(kore_l))
be_lc   = $(subst $(R)/,,$(p_be_l))

# nolibc's pure members: the libc a bare-metal seat gets, the same six the kernel takes
# (mk/common.mk) out of the same source -- there is no second libc in this tree. A port
# lays them with a foreach over libc_m under its own <x>_cc. ⚠ -Icrew/moon/include is
# owed: the members open with impl.h, whose hosted declarations cost compile time and
# nothing else -- the six owe ONE symbol between them (memmove's memcpy), and it is one
# of the six.
libc_m    = memchr memcmp memcpy memmove memset strlen
libc_dep  = $(R)/crew/moon/lib/nolibc/impl.h $(mc)
libc_o    = $(addprefix $(R)/$(o)/,$(addsuffix .o,$(libc_m)))

# the am math floor: the one object every port compiles exactly alike.
$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t $(p_tgt) -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o

# p_ocopy -- the flatten, for the ports that ship a .bin/.hex: crew/holo/copy.l reads the
# linked ELF and writes objcopy's two output formats, byte for byte (`kore objcopy` is the
# same code with a name). Takes no argument; a port that links its own ELF and stops there
# (virt, mps2) never asks for it.
define p_ocopy
$$(R)/$$(o)/ocopy.l: $$(copy_l)
	@echo CAT	$$@
	@mkdir -p $$(R)/$$(o)
	@{ echo "(use 'holo)"; cat $$(kore_l); echo "(use 'kore)"; \
	   cat $$(filter-out $$(kore_l),$$(copy_l)); echo '(objcopy >argv)'; } > $$@
endef

# p_obj -- one compiled object. $1 stem, $2 the source from $(R), $3 the prerequisites,
# $4 the compile command (a port's own <x>_cc, plus any -D this object alone wants).
define p_obj
$$(R)/$$(o)/$1.o: $3
	@echo MOON	$$@
	@mkdir -p $$(R)/$$(o)
	@cd $$(R) && $4 -c $2 $$(o)/$1.o
endef

# p_lay -- one object laid from holo IR, so no assembler runs. $1 stem, $2 the mk*.l driver
# (whose name is the function too), $3 that function's arguments.
define p_lay
$$(R)/$$(o)/$1.o: $2.l $$(p_be_l) $$(lay_l) $$(lv)
	@echo HOLO	$$@
	@mkdir -p $$(R)/$$(o)
	@cd $$(R) && { echo "(use 'holo)"; cat $$(be_lc) $$(kore_lc); echo "(use 'kore)"; \
	  cat $$(filter-out $$(kore_lc),$$(lay_lc)) port/$$(p_dir)/$2.l; \
	  echo '($2 $3)'; } | out/host/love
endef

# p_link -- the link driver's cat. $1 its stem. ⚠ an explicit target, never a pattern rule:
# a pattern-MADE prerequisite is an INTERMEDIATE make deletes after the link, and the cat
# would then run again on every build.
define p_link
$$(R)/$$(o)/$1.l: $1.l $$(link_l)
	@echo CAT	$$@
	@mkdir -p $$(R)/$$(o)
	@{ echo "(use 'holo)"; cat $$(kore_l); echo "(use 'kore)"; \
	   cat $$(filter-out $$(kore_l),$$(link_l)) $$<; } > $$@
endef
