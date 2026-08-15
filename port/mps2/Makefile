# qemu MPS2+ AN500 (Cortex-M7) build -- love compiled END TO END by mooncc. `make -C
# port/mps2` from the project root. Every C file -- core/love.c, the am math floor, libc, main.c
# -- goes through `mooncc -t thumb2`, start.o is laid from holo IR by mkstart.l, and
# mooncc's own -Ttext/-Tdata drive the link over holo's 32-bit lane: no arm-none-eabi
# anywhere.
#
# Boot it: qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel love.elf
# (exit 42 = the egg hatched and the driver tail's laws held; `make test_mps2` at the root
# gates exactly that).
p_tgt  = thumb2
p_be   = thumb2
lib_h  = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h
p_hdrs = out/lib/rune.h
include ../port.mk

mps2_cc = $(MOONCC) -t thumb2 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Icore -Iout/lib
# our own link: -Ttext 0 is the board's origin, -nostdlib says an owed nom is a diagnostic
# rather than a hunt for a runtime this board would not want.
mps2_ld = $(MOONCC) -t thumb2 -Ttext 0 -Tdata 0x20000000 -nostdlib

# the five faces share a skeleton and differ in main.o alone: the plain kernel, the BAKER
# that dumps a heap image through semihosting, the WAKER that boots one back, and the two
# -pd twins carrying the rune+cas corpus.
skel = $(R)/$(o)/start.o $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o $(R)/$(o)/str.o
objs        = $(skel) $(R)/$(o)/main.o
baker_objs  = $(skel) $(R)/$(o)/main-baker.o
waker_objs  = $(skel) $(R)/$(o)/main-waker.o
pd_lib_h    = $(lib_h) out/lib/rune.h

.PHONY: mps2 mps2t1 img imgpd
default: mps2
mps2: $(R)/$(o)/love.elf
mps2t1: $(R)/$(o)/love-t1.elf

$(eval $(call p_obj,love,core/love.c,$(R)/core/love.c $(R)/core/love.h $(lib_hR) $(mc),$(mps2_cc)))
$(eval $(call p_obj,mem,libc/mem.c,$(R)/libc/mem.c $(R)/core/love.h $(mc),$(mps2_cc)))
$(eval $(call p_obj,str,libc/str.c,$(R)/libc/str.c $(R)/core/love.h $(mc),$(mps2_cc)))

# main.c, once per face -- the -D is the whole difference.
mainc = main.c $(R)/core/love.h $(lib_hR) $(mc)
$(eval $(call p_obj,main,port/mps2/main.c,$(mainc),$(mps2_cc)))
$(eval $(call p_obj,main-baker,port/mps2/main.c,$(mainc),$(mps2_cc) -D BAKER))
$(eval $(call p_obj,main-waker,port/mps2/main.c,$(mainc),$(mps2_cc) -D WAKER))

# start.o: the vector table, the FPU gate and the semihosting trampoline, laid from holo IR
# -- no gas. port/virt/mkstart.l is the same shape at riscv64, port/rp2040/mkboot2.l the
# named-section one.
$(eval $(call p_lay,start,mkstart,"$(o)/start.o"))

$(R)/$(o)/love.elf: $(objs)
	@echo MOON	$@
	@cd $(R) && $(mps2_ld) $(objs:$(R)/%=%) -o $(o)/love.elf
$(R)/$(o)/baker.elf: $(baker_objs)
	@echo MOON	$@
	@cd $(R) && $(mps2_ld) $(baker_objs:$(R)/%=%) -o $(o)/baker.elf
$(R)/$(o)/waker.elf: $(waker_objs)
	@echo MOON	$@
	@cd $(R) && $(mps2_ld) $(waker_objs:$(R)/%=%) -o $(o)/waker.elf

# the BAKED HEAP IMAGE (love.img): main.c's -D BAKER face bakes the corpus on qemu's M7 and
# dumps the image through semihosting -- the bake as a build-time event (the teensy embeds
# the blob and wakes from it). No qemu -> an empty image, and the device's egg lane bakes
# from source as always.
img: $(R)/$(o)/love.img
$(R)/$(o)/love.img: $(R)/$(o)/baker.elf
	@echo LOVE	$@ "(qemu M7)"
	@if command -v qemu-system-arm >/dev/null 2>&1; then \
	   cd $(R) && timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	     -kernel $(o)/baker.elf </dev/null && test -s $(o)/love.img; \
	 else : > $@; echo "img: no qemu-system-arm -- empty image (device egg-boots)"; fi

# the PLAYDATE image (love-pd.img): same baker skeleton, the rune+cas corpus
# (-D BAKER_RUNE), and a qemu wake-check (waker-pd) BEFORE the pdx ships it.
imgpd: $(R)/$(o)/love-pd.img
$(R)/out/playdate/cas.h:
	@$(MAKE) -C $(R)/port/playdate ../../out/playdate/cas.h

pdmainc = main.c $(R)/core/love.h $(addprefix $(R)/,$(pd_lib_h)) $(R)/out/playdate/cas.h $(mc)
$(eval $(call p_obj,main-baker-pd,port/mps2/main.c,$(pdmainc),$(mps2_cc) -D BAKER -D BAKER_RUNE -Iout/playdate))
$(eval $(call p_obj,main-waker-pd,port/mps2/main.c,$(pdmainc),$(mps2_cc) -D WAKER -D BAKER_RUNE -Iout/playdate))

$(R)/$(o)/baker-pd.elf: $(skel) $(R)/$(o)/main-baker-pd.o
	@echo MOON	$@
	@cd $(R) && $(mps2_ld) $(^:$(R)/%=%) -o $(o)/baker-pd.elf
$(R)/$(o)/waker-pd.elf: $(skel) $(R)/$(o)/main-waker-pd.o
	@echo MOON	$@
	@cd $(R) && $(mps2_ld) $(^:$(R)/%=%) -o $(o)/waker-pd.elf

$(R)/$(o)/love-pd.img: $(R)/$(o)/baker-pd.elf $(R)/$(o)/waker-pd.elf
	@echo LOVE	$@ "(qemu M7 + wake-check)"
	@if command -v qemu-system-arm >/dev/null 2>&1; then \
	   cd $(R) && timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	     -kernel $(o)/baker-pd.elf </dev/null && test -s $(o)/love-pd.img \
	   && { timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic \
	          -kernel $(o)/waker-pd.elf </dev/null; [ $$? -eq 42 ] \
	        || { echo "FAIL pd image wake-check"; rm -f $(o)/love-pd.img; exit 1; }; }; \
	 else : > $@; echo "imgpd: no qemu-system-arm -- empty image (device egg-boots)"; fi

# --- the thumb1 face: the SAME port compiled `mooncc -t thumb1` (ARMv6-M, the RP2040's
# ISA). v6-M code is a strict subset of ARMv7E-M, so qemu's M7 runs it natively -- no
# RP2040 machine exists in qemu; the microbit differential legs prove the v6-M encoding
# floor, this proves the whole runtime. ai_tco=0: v6-M has no sibcalls (the pop-pc
# epilogue), so the VM rides the trampoline -- the lane the clang rp2040 build uses. The
# __aeabi soft-float/64-bit helpers come from gcc's v6-m multilib.
mps2t1_cc = $(MOONCC) -t thumb1 -D ai_tco=0 -D __STDC_HOSTED__=0 -I. -Icore -Iout/lib
t1_lg = `arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -print-libgcc-file-name`
t1_objs = $(R)/$(o)/start.o $(R)/$(o)/love-t1.o $(R)/$(o)/am-t1.o $(R)/$(o)/mem-t1.o \
  $(R)/$(o)/str-t1.o $(R)/$(o)/main-t1.o

$(eval $(call p_obj,love-t1,core/love.c,$(R)/core/love.c $(R)/core/love.h $(lib_hR) $(mc),$(mps2t1_cc)))
$(eval $(call p_obj,mem-t1,libc/mem.c,$(R)/libc/mem.c $(R)/core/love.h $(mc),$(mps2t1_cc)))
$(eval $(call p_obj,str-t1,libc/str.c,$(R)/libc/str.c $(R)/core/love.h $(mc),$(mps2t1_cc)))
$(eval $(call p_obj,main-t1,port/mps2/main.c,$(mainc),$(mps2t1_cc)))
$(eval $(call p_obj,am-t1,crew/moon/lib/math/am.c,$(R)/crew/moon/lib/math/am.c $(mc),\
  $(MOONCC) -t thumb1 -Icrew/moon/lib/math -Icrew/moon/include))

# our linker binds this one too: libgcc.a rides in as an ARCHIVE, resolved by need through
# its ranlib index (crew/holo/link.l's ld-arsyms). gcc's own v6-M members are the only
# foreign objects in the tree's boot images.
$(R)/$(o)/love-t1.elf: $(t1_objs)
	@echo MOON	$@
	@cd $(R) && $(MOONCC) -t thumb1 -Ttext 0 -Tdata 0x20000000 -nostdlib $(t1_objs:$(R)/%=%) $(t1_lg) -o $(o)/love-t1.elf
