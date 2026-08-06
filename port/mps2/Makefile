# qemu MPS2+ AN500 (Cortex-M7) build -- love compiled END TO END by mooncc.
# Run from the project root via `make -C port/mps2` (needs out/host/mooncc,
# built by the root Makefile). Every C file -- love.c, the am math floor,
# libc, main.c -- goes through `mooncc -t thumb2`, and start.o is laid from holo
# IR by mkstart.l, so the LINK is the only arm-none-eabi step left (crew/holo's
# 32-bit lane is what retires it). mooncc's system include dir is CWD-relative,
# so every compile runs from $(R).
# Boot it: qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel love.elf
# (exit 42 = the egg hatched and the driver tail's laws held; `make
# test_mps2` at the root gates exactly that).
R := ../..
o = out/mps2
MOONCC = out/host/mooncc
mc = $(R)/out/host/mooncc

mps2_cc = $(MOONCC) -t thumb2 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Iout/lib
lib_h = out/lib/egg.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h

objs = $(R)/$(o)/start.o $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o \
  $(R)/$(o)/str.o $(R)/$(o)/main.o

baker_objs = $(R)/$(o)/start.o $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o \
  $(R)/$(o)/str.o $(R)/$(o)/main-baker.o

# our own link: -Ttext 0 is the board's origin, -nostdlib says an owed nom is a
# diagnostic rather than a hunt for a runtime this board would not want.
mps2_ld = $(MOONCC) -t thumb2 -Ttext 0 -nostdlib

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

.PHONY: mps2 img imgpd clean
mps2: $(R)/$(o)/love.elf
default: mps2

# the BAKED HEAP IMAGE (love.img): main.c's -D BAKER face bakes the corpus on
# qemu's M7 and dumps the image through semihosting -- the bake as a build-time
# event (the teensy embeds the blob and wakes from it). No qemu -> an empty
# image, and the device's egg lane bakes from source as always.
img: $(R)/$(o)/love.img

$(R)/$(o)/main-baker.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D BAKER -c port/mps2/main.c $(o)/main-baker.o

$(R)/$(o)/baker.elf: $(baker_objs)
	@echo LINK	$@
	@cd $(R) && $(mps2_ld) $(baker_objs:$(R)/%=%) -o $(o)/baker.elf

waker_objs = $(R)/$(o)/start.o $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o \
  $(R)/$(o)/str.o $(R)/$(o)/main-waker.o

$(R)/$(o)/main-waker.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D WAKER -c port/mps2/main.c $(o)/main-waker.o

$(R)/$(o)/waker.elf: $(waker_objs)
	@echo LINK	$@
	@cd $(R) && $(mps2_ld) $(waker_objs:$(R)/%=%) -o $(o)/waker.elf

$(R)/$(o)/love.img: $(R)/$(o)/baker.elf
	@echo BAKE	$@ "(qemu M7)"
	@if command -v qemu-system-arm >/dev/null 2>&1; then \
	   cd $(R) && timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	     -kernel $(o)/baker.elf </dev/null && test -s $(o)/love.img; \
	 else : > $@; echo "img: no qemu-system-arm -- empty image (device egg-boots)"; fi

# the PLAYDATE image (love-pd.img): same baker skeleton, the rune+cas corpus
# (-D BAKER_RUNE), and a qemu wake-check (waker-pd) BEFORE the pdx ships it.
imgpd: $(R)/$(o)/love-pd.img

pd_lib_h = $(lib_h) out/lib/rune.h

$(R)/out/playdate/cas.h:
	@$(MAKE) -C $(R)/port/playdate ../../out/playdate/cas.h

$(R)/$(o)/main-baker-pd.o: main.c $(R)/love.h $(addprefix $(R)/,$(pd_lib_h)) $(R)/out/playdate/cas.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D BAKER -D BAKER_RUNE -Iout/playdate -c port/mps2/main.c $(o)/main-baker-pd.o

$(R)/$(o)/main-waker-pd.o: main.c $(R)/love.h $(addprefix $(R)/,$(pd_lib_h)) $(R)/out/playdate/cas.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D WAKER -D BAKER_RUNE -Iout/playdate -c port/mps2/main.c $(o)/main-waker-pd.o

$(R)/$(o)/baker-pd.elf: $(filter-out %/main-baker.o,$(baker_objs)) $(R)/$(o)/main-baker-pd.o
	@echo LINK	$@
	@cd $(R) && $(mps2_ld) $(^:$(R)/%=%) -o $(o)/baker-pd.elf

$(R)/$(o)/waker-pd.elf: $(filter-out %/main-waker.o,$(waker_objs)) $(R)/$(o)/main-waker-pd.o
	@echo LINK	$@
	@cd $(R) && $(mps2_ld) $(^:$(R)/%=%) -o $(o)/waker-pd.elf

$(R)/$(o)/love-pd.img: $(R)/$(o)/baker-pd.elf $(R)/$(o)/waker-pd.elf
	@echo BAKEPD	$@ "(qemu M7 + wake-check)"
	@if command -v qemu-system-arm >/dev/null 2>&1; then \
	   cd $(R) && timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	     -kernel $(o)/baker-pd.elf </dev/null && test -s $(o)/love-pd.img \
	   && { timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	          -kernel $(o)/waker-pd.elf </dev/null; [ $$? -eq 42 ] \
	        || { echo "FAIL pd image wake-check"; rm -f $(o)/love-pd.img; exit 1; }; }; \
	 else : > $@; echo "imgpd: no qemu-system-arm -- empty image (device egg-boots)"; fi

# --- the thumb1 face: the SAME port compiled `mooncc -t thumb1` (ARMv6-M, the
# RP2040's ISA). v6-M code is a strict subset of ARMv7E-M, so qemu's M7 runs it
# natively -- no RP2040 machine exists in qemu; the microbit differential legs
# prove the v6-M encoding floor, this proves the whole runtime. ai_tco=0: v6-M
# has no sibcalls (the pop-pc epilogue), so the VM rides the trampoline -- the
# same lane the clang rp2040 build uses. The __aeabi soft-float/64-bit helpers
# come from gcc's v6-m multilib (the exact set the rp2040 port links).
mps2t1_cc = $(MOONCC) -t thumb1 -D ai_tco=0 -D __STDC_HOSTED__=0 -I. -Iout/lib
t1_lg = `arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -print-libgcc-file-name`

t1_objs = $(R)/$(o)/start.o $(R)/$(o)/love-t1.o $(R)/$(o)/am-t1.o $(R)/$(o)/mem-t1.o \
  $(R)/$(o)/str-t1.o $(R)/$(o)/main-t1.o

.PHONY: mps2t1
mps2t1: $(R)/$(o)/love-t1.elf

$(R)/$(o)/love-t1.o: $(R)/love.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2t1_cc) -c love.c $(o)/love-t1.o

$(R)/$(o)/am-t1.o: $(R)/crew/moon/lib/math/am.c $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t thumb1 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am-t1.o

$(R)/$(o)/mem-t1.o: $(R)/libc/mem.c $(R)/love.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2t1_cc) -c libc/mem.c $(o)/mem-t1.o

$(R)/$(o)/str-t1.o: $(R)/libc/str.c $(R)/love.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2t1_cc) -c libc/str.c $(o)/str-t1.o

$(R)/$(o)/main-t1.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2t1_cc) -c port/mps2/main.c $(o)/main-t1.o

# our linker binds this one too: libgcc.a rides in as an ARCHIVE, resolved by
# need through its ranlib index (crew/holo/link.l's ld-arsyms). gcc's own v6-M
# members are the only foreign objects in the tree's boot images.
$(R)/$(o)/love-t1.elf: $(t1_objs)
	@echo LINK	$@
	@cd $(R) && $(MOONCC) -t thumb1 -Ttext 0 -nostdlib $(t1_objs:$(R)/%=%) $(t1_lg) -o $(o)/love-t1.elf

clean:
	rm -rf $(R)/$(o)

# the lcat lib headers come from the root's own rules (on love0), on demand.
# FORCE, and the pd set too: a prerequisite-less rule only fires when the target
# is MISSING, so a stale header was served forever -- and out/lib/rune.h was not
# even in the delegated set, so it had no rule at all (a missing one died with
# "No rule to make target", an existing one silently froze). see the same note in
# port/playdate/Makefile.
.PHONY: FORCE
FORCE:
$(addprefix $(R)/,$(sort $(lib_h) $(pd_lib_h))): FORCE
	@$(MAKE) -C $(R) $(patsubst $(R)/%,%,$@)

$(R)/$(o)/love.o: $(R)/love.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -c love.c $(o)/love.o

$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t thumb2 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o

$(R)/$(o)/%.o: $(R)/libc/%.c $(R)/love.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -c libc/$*.c $(o)/$*.o

$(R)/$(o)/main.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -c port/mps2/main.c $(o)/main.o

# start.o: the vector table, the FPU gate and the semihosting trampoline, laid from
# holo IR by mkstart.l -- no gas. The lay's cat is the thumb2 backend spliced into the
# sealed holo module, then the object writer; see port/virt/mkstart.l for the same
# shape at riscv64 and port/rp2040/mkboot2.l for the named-section one.
lay_l = $(R)/crew/kore/text.l $(R)/crew/kore/core.l $(R)/crew/kore/asbook.l \
  $(R)/crew/holo/elf.l $(R)/crew/holo/obj.l

$(R)/$(o)/start.o: mkstart.l $(R)/crew/holo/thumb2.l $(lay_l) $(R)/out/host/love
	@echo LAY	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && { echo "(use 'holo)"; cat crew/holo/thumb2.l $(subst $(R)/,,$(lay_l)) \
	  port/mps2/mkstart.l; \
	  echo '(mkstart "$(o)/start.o")'; } | out/host/love

$(R)/out/host/love: FORCE
	@$(MAKE) -C $(R) out/host/love

$(R)/$(o)/love.elf: $(objs)
	@echo LINK	$@
	@cd $(R) && $(mps2_ld) $(objs:$(R)/%=%) -o $(o)/love.elf
