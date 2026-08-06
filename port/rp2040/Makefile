# Raspberry Pi Pico (RP2040) build -- Cortex-M0+ / ARMv6-M, compiled by
# mooncc -t thumb1. No Pico SDK, no CMake, no vendor headers. Run from the
# project root via `make -C port/rp2040` (needs out/host/mooncc, built by the
# root Makefile). mooncc's system include dir is CWD-relative, so every compile
# runs from $(R). Output lands in $(R)/out/rp2040/.
#
# ⚠ THIS PORT COMPILES BUT DOES NOT LINK, and the default target says so rather
# than pretending. Three things are missing, none of them this file's:
#   * rp2040.c hangs the vector table off __attribute__((section(".vectors"))),
#     and holo's 32-bit object writer has no section list -- moon SCARES on it
#     (obj-section-arm32) instead of dropping it in silence.
#   * boot2 wants its CRC stamped and the .uf2 packed; tools/ still speaks the
#     pre-rename .g dialect and emits gas source for the stamp.
#   * thumb relocations are not in crew/holo/link.l, so nothing of ours binds
#     the ELF either.
# What IS gated today is the frontend: `make test_embed` compiles main.c here
# with every other frontend, so this port cannot rot against love.h again.
R := ../..
o = out/rp2040
MOONCC = out/host/mooncc
mc = $(R)/out/host/mooncc

rp_cc = $(MOONCC) -t thumb1 -D __STDC_HOSTED__=0 -I.

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

.PHONY: default objs clean
default:
	@echo "rp2040: compiles, does not link yet -- see the head of this Makefile."
	@echo "        \`make objs\` builds what mooncc can take; \`make test_embed\` gates main.c."

# what mooncc can take today. rp2040.o is NOT here: see the head.
objs: $(R)/$(o)/main.o $(R)/$(o)/am.o

clean:
	rm -rf $(R)/$(o)

# delegate the host binaries to the root, always (a rule with no prerequisites
# fires only when its target is MISSING, so a stale one would be served forever).
.PHONY: FORCE
FORCE:
$(mc): FORCE
	@$(MAKE) -C $(R) out/host/mooncc

$(R)/$(o)/main.o: main.c rp2040.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(rp_cc) -c port/rp2040/main.c $(o)/main.o

$(R)/$(o)/rp2040.o: rp2040.c rp2040.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(rp_cc) -c port/rp2040/rp2040.c $(o)/rp2040.o

$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t thumb1 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o
