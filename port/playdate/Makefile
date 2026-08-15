# Playdate build. Run from the project root via `make -C port/playdate`, or
# directly via `cd port/playdate && make`. Output lands in $(R)/out/playdate/
# ($n.pdx at the top). Requires PLAYDATE_SDK_PATH (pdc + C_API + simulator);
# the DEVICE half is compiled by mooncc -t thumb2sp (the STM32F746's FPU is
# single-precision: every f64 op softens to the __aeabi_* libgcc helpers,
# gated bit-exact by test_thumb2sp) with only pdglue.c + the SDK's setup.c on
# arm-none-eabi-gcc -- pdglue owns pd_api.h and flattens the SDK to a
# word-only seam. la rides pooled ABS32 words on this target (the loader
# relocates words, never a MOVW/MOVT pair). The simulator pdex.so stays an
# ordinary host gcc shared object, so `make sim` runs with no cross tools.
#
# Both halves bake the same program: the egg bootstrap (out/lib lcat headers,
# delegated to the root), crew/rune/ as a registered module, and cas.l (this
# folder, lcat'd locally) driving the workbench. See main.c + pdglue.c.
R := ../..
include $(R)/common.mk

o = $(R)/out/playdate
n = love
pd_sdk = $(PLAYDATE_SDK_PATH)
CCACHE ?= $(shell command -v ccache 2>/dev/null)
MOONCC = $(R)/out/host/mooncc
pd_cc = $(CCACHE) arm-none-eabi-gcc -g3
pd_opt = -Os -falign-functions=16 -fomit-frame-pointer
# the device M7's FPU is single-precision; gcc-side doubles ride the libgcc
# soft helpers -- the same __aeabi_* calls mooncc's soft-double pass emits.
pd_fpu = -mfloat-abi=hard -mfpu=fpv5-sp-d16 -D__FPU_USED=1
pd_mcflags = -mthumb -mcpu=cortex-m7 $(pd_fpu)
pd_incdir = -I. -Icore -I$(o) -I$(R) -I$(R)/out/lib -I$(R)/crew/quay -I$(pd_sdk)/C_API
# ai_data_section=0: the sentinel tiling core/love.h's ai_typ reads needs the link to
# place love_data.NN, and this seat links through the SDK's own link_map.ld --
# not ours to write a block into. core/love.h asks the sentinels by name here.
pd_defs = -DTARGET_PLAYDATE=1 -DTARGET_EXTENSION=1 -Dai_data_section=0
pd_cpflags = $(pd_mcflags) $(pd_opt) $(pd_defs) -std=gnu2x -ffreestanding \
  -gdwarf-2 -Wall -Wno-unused -Wstrict-prototypes -Wno-unknown-pragmas \
  -Wdouble-promotion -mword-relocations -fno-common \
  -ffunction-sections -fdata-sections $(pd_incdir)
pd_lds = $(pd_sdk)/C_API/buildsupport/link_map.ld
pd_ldflags = -nostartfiles $(pd_mcflags) -T$(pd_lds) \
  -Wl,-Map=$(o)/pdex.map,--cref,--gc-sections,--no-warn-mismatch,--emit-relocs

# mooncc's flags for the device C (run from $(R): its system include dir is
# CWD-relative). TARGET_PLAYDATE gates main.c's newlib stubs.
mc_flags = -t thumb2sp -D ai_tco=1 -D __STDC_HOSTED__=0 -D TARGET_PLAYDATE=1 -D TARGET_EXTENSION=1 -D ai_data_section=0 \
  -Iport/playdate -Iout/playdate -I. -Icore -Iout/lib -Icrew/quay

# the lcat lib headers this frontend bakes (produced by the root's rules on
# love0, delegated below so a standalone make works from a clean tree)
lib_hs = $(patsubst %,$(R)/out/lib/%.h,egg prel ev q kanren rune)
pd_h = $(love_h) $(wildcard $(R)/crew/quay/*.h) $(o)/cas.h $(lib_hs)

# the split: mooncc carries core/love.c + am + quay + main.c; gcc carries pdglue.c
# (the pd_api.h owner) + the SDK's setup.c. the sim build compiles everything
# hosted, so pd_src keeps the full wildcard.
pd_local_c = $(wildcard *.c)
pd_shared_c = $(love_c) $(f_c)
pd_sdk_c = $(pd_sdk)/C_API/buildsupport/setup.c
pd_src = $(pd_local_c) $(pd_shared_c) $(pd_sdk_c)
mc_shared_o = $(pd_shared_c:$(R)/%.c=$(o)/%.o)
pd_o = $(o)/main.o $(mc_shared_o) $(o)/pdglue.o $(o)/setup.o

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

.PHONY: default sim clean
default: $(o)/$n.pdx

sim: $(o)/$n.pdx
	@$(pd_sdk)/bin/PlaydateSimulator $^

clean:
	rm -rf $(o)

# delegate the lib headers and the host binary (which lcats cas.l) to the root.
# each one needs FORCE: a rule with NO prerequisites fires only when its target
# is MISSING, so an existing-but-STALE header was served forever and the root's
# own up-to-date check never got asked. out/lib/rune.h froze that way -- nothing
# else in the tree regenerates it -- and when crew/rune/rune.l grew the
# (leave ()) that registers the module, the baked image kept the sealless
# header and the workbench died on `;; no-module rune`. the root decides
# staleness; our job is only to ask it every time.
.PHONY: FORCE
FORCE:
$(lib_hs): FORCE
	@$(MAKE) -C $(R) out/lib/$(@F)
$(m): FORCE
	@$(MAKE) -C $(R) out/host/love
$(MOONCC): FORCE
	@$(MAKE) -C $(R) out/host/mooncc

# cas.l -> C string literal, atomically (a 0-byte header would silently drop
# the whole demo -- same discipline as the root's lcat_h)
$(o)/cas.h: cas.l $(R)/tools/lcat.l | $(m)
	@echo LOVE	$@
	@mkdir -p $(o)
	@$(m) $(R)/tools/lcat.l cas.l > $@.tmp && test -s $@.tmp && mv -f $@.tmp $@ \
	  || { rm -f $@.tmp; echo "FAIL: $@ empty"; exit 1; }

$(o)/$n.pdx: $(o)/Source/pdex.elf $(o)/Source/pdex.so $(o)/Source/love-pd.img
	@echo PDC	$@
	@$(pd_sdk)/bin/pdc -sdkpath $(pd_sdk) $(o)/Source $@

# the qemu-baked heap image rides the pdx as a data file; port/mps2's imgpd
# bakes AND wake-checks it (FORCE keeps the delegation live -- mps2's own
# deps decide whether the bake re-runs). An empty image ships too: the
# device just egg-boots (and the 64-bit sim always does).
$(R)/out/mps2/love-pd.img: FORCE
	@$(MAKE) --no-print-directory -C $(R)/port/mps2 imgpd
FORCE:

$(o)/Source/love-pd.img: $(R)/out/mps2/love-pd.img
	@echo CP	$@
	@mkdir -p $(dir $@)
	@cp $< $@

$(o)/Source/pdex.%: $(o)/pdex.%
	@echo CP	$@
	@mkdir -p $(dir $@)
	@cp $< $@

# the mooncc half: main.c + the shared runtime + quay
$(o)/main.o: main.c pdglue.h $(pd_h) | $(MOONCC)
	@echo MOON	$@
	@mkdir -p $(o)
	@cd $(R) && out/host/mooncc $(mc_flags) -c port/playdate/main.c out/playdate/main.o
$(o)/%.o: $(R)/%.c $(pd_h) | $(MOONCC)
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@cd $(R) && out/host/mooncc $(mc_flags) -c $*.c out/playdate/$*.o
# the gcc half: the SDK boundary
$(o)/pdglue.o: pdglue.c pdglue.h
	@echo CC	$@
	@mkdir -p $(o)
	@$(pd_cc) -c $(pd_cpflags) pdglue.c -o $@
$(o)/setup.o: $(pd_sdk)/C_API/buildsupport/setup.c
	@echo CC	$@
	@mkdir -p $(o)
	@$(pd_cc) -c $(pd_cpflags) $< -o $@

$(o)/pdex.elf: $(pd_o) $(pd_lds)
	@echo LD	$@
	@$(pd_cc) $(pd_o) $(pd_ldflags) -o $@

# the simulator .so: a single-shot hosted build of the same sources
$(o)/pdex.so: $(pd_src) $(o)/cas.h
	@echo CC	$@
	@mkdir -p $(o)
	@$(CCACHE) gcc -g -O1 -shared -fPIC -Dai_tco=0 -DTARGET_SIMULATOR=1 -DTARGET_EXTENSION=1 \
	  -I. -Icore -I$(o) -I$(R) -I$(R)/out/lib -I$(R)/crew/quay -I$(pd_sdk)/C_API \
	  -o $@ $(pd_src) -lm

.PRECIOUS: $(o)/%.elf
