# qemu MPS2+ AN500 (Cortex-M7) build -- love compiled END TO END by mooncc.
# Run from the project root via `make -C port/mps2` (needs out/host/mooncc,
# built by the root Makefile). Every C file -- love.c, the am math floor,
# libc, main.c -- goes through `mooncc -t thumb2`; only start.S (12
# instructions) and the final link are arm-none-eabi. mooncc's system
# include dir is CWD-relative, so every compile runs from $(R).
# Boot it: qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel love.elf
# (exit 42 = the egg hatched and the driver tail's laws held; `make
# test_mps2` at the root gates exactly that).
R := ../..
o = out/mps2
MOONCC = out/host/mooncc
AS = arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16
LD = arm-none-eabi-ld

mps2_cc = $(MOONCC) -t thumb2 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Iout/lib
lib_h = out/lib/egg.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h

objs = $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o $(R)/$(o)/str.o \
  $(R)/$(o)/ctype.o $(R)/$(o)/main.o $(R)/$(o)/start.o

baker_objs = $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o $(R)/$(o)/str.o \
  $(R)/$(o)/ctype.o $(R)/$(o)/main-baker.o $(R)/$(o)/start.o

.PHONY: mps2 img imgpd clean
mps2: $(R)/$(o)/love.elf
default: mps2

# the BAKED HEAP IMAGE (love.img): main.c's -D BAKER face bakes the corpus on
# qemu's M7 and dumps the image through semihosting -- the bake as a build-time
# event (the teensy embeds the blob and wakes from it). No qemu -> an empty
# image, and the device's egg lane bakes from source as always.
img: $(R)/$(o)/love.img

$(R)/$(o)/main-baker.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h))
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D BAKER -c port/mps2/main.c $(o)/main-baker.o

$(R)/$(o)/baker.elf: $(baker_objs) mps2.lds
	@echo LD	$@
	@$(LD) -T mps2.lds $(baker_objs) -o $@

waker_objs = $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o $(R)/$(o)/str.o \
  $(R)/$(o)/ctype.o $(R)/$(o)/main-waker.o $(R)/$(o)/start.o

$(R)/$(o)/main-waker.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h))
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D WAKER -c port/mps2/main.c $(o)/main-waker.o

$(R)/$(o)/waker.elf: $(waker_objs) mps2.lds
	@echo LD	$@
	@$(LD) -T mps2.lds $(waker_objs) -o $@

$(R)/$(o)/love.img: $(R)/$(o)/baker.elf
	@echo BAKE	$@ "(qemu M7)"
	@if command -v qemu-system-arm >/dev/null 2>&1; then \
	   cd $(R) && timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	     -kernel $(o)/baker.elf </dev/null && test -s $(o)/love.img; \
	 else : > $@; echo "img: no qemu-system-arm -- empty image (device egg-boots)"; fi

# the PLAYDATE image (love-pd.img): same baker skeleton, the rune+cas corpus
# (-D BAKER_RUNE), and a qemu wake-check (waker-pd) BEFORE the pdx ships it.
imgpd: $(R)/$(o)/love-pd.img

pd_lib_h = $(lib_h) out/lib/rune.h out/lib/runeseal.h

$(R)/out/playdate/cas.h:
	@$(MAKE) -C $(R)/port/playdate ../../out/playdate/cas.h

$(R)/$(o)/main-baker-pd.o: main.c $(R)/love.h $(addprefix $(R)/,$(pd_lib_h)) $(R)/out/playdate/cas.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D BAKER -D BAKER_RUNE -Iout/playdate -c port/mps2/main.c $(o)/main-baker-pd.o

$(R)/$(o)/main-waker-pd.o: main.c $(R)/love.h $(addprefix $(R)/,$(pd_lib_h)) $(R)/out/playdate/cas.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -D WAKER -D BAKER_RUNE -Iout/playdate -c port/mps2/main.c $(o)/main-waker-pd.o

$(R)/$(o)/baker-pd.elf: $(filter-out %/main-baker.o,$(baker_objs)) $(R)/$(o)/main-baker-pd.o mps2.lds
	@echo LD	$@
	@$(LD) -T mps2.lds $(filter-out %/main-baker.o mps2.lds,$^) -o $@

$(R)/$(o)/waker-pd.elf: $(filter-out %/main-waker.o,$(waker_objs)) $(R)/$(o)/main-waker-pd.o mps2.lds
	@echo LD	$@
	@$(LD) -T mps2.lds $(filter-out %/main-waker.o mps2.lds,$^) -o $@

$(R)/$(o)/love-pd.img: $(R)/$(o)/baker-pd.elf $(R)/$(o)/waker-pd.elf
	@echo BAKEPD	$@ "(qemu M7 + wake-check)"
	@if command -v qemu-system-arm >/dev/null 2>&1; then \
	   cd $(R) && timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	     -kernel $(o)/baker-pd.elf </dev/null && test -s $(o)/love-pd.img \
	   && { timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	          -kernel $(o)/waker-pd.elf </dev/null; [ $$? -eq 42 ] \
	        || { echo "FAIL pd image wake-check"; rm -f $(o)/love-pd.img; exit 1; }; }; \
	 else : > $@; echo "imgpd: no qemu-system-arm -- empty image (device egg-boots)"; fi

clean:
	rm -rf $(R)/$(o)

# the lcat lib headers come from the root's own rules (on love0), on demand
$(addprefix $(R)/,$(lib_h)):
	@$(MAKE) -C $(R) $(patsubst $(R)/%,%,$@)

$(R)/$(o)/love.o: $(R)/love.c $(R)/love.h $(addprefix $(R)/,$(lib_h))
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -c love.c $(o)/love.o

$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t thumb2 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o

$(R)/$(o)/%.o: $(R)/libc/%.c $(R)/love.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -c libc/$*.c $(o)/$*.o

$(R)/$(o)/main.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h))
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(mps2_cc) -c port/mps2/main.c $(o)/main.o

$(R)/$(o)/start.o: start.S
	@echo AS	$@
	@mkdir -p $(R)/$(o)
	@$(AS) -c start.S -o $@

$(R)/$(o)/love.elf: $(objs) mps2.lds
	@echo LD	$@
	@$(LD) -T mps2.lds $(objs) -o $@
