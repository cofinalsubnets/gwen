# Teensy 4.1 (i.MX RT1062) build -- love compiled END TO END by mooncc. `make -C
# port/teensy41` from the project root. Every C file -- the runtime's seven, the am
# floor, libc, teensy41.c, main.c -- goes through `mooncc -t thumb2`; boot.o (the
# ROM-facing FlexSPI/IVT/vector blocks + crt0 + the asm helpers) is laid from holo IR by
# mkboot.l, the baked heap image by mkimg.l, tlink.l LINKS over holo's ldbare32 and
# ocopy.l writes the .hex and .bin over holo's copy.l -- so the port owes no gas, no ld, no linker script and
# no objcopy. No libgcc either: mooncc's thumb2 lanes are self-contained (zero __aeabi_*).
# The one outside program left is qemu, and only to BAKE the heap image (port/mps2 runs the
# baker; no qemu leaves the blob empty and the device egg-boots).
#
# The default target also VERIFIES the boot image: FCFB tag at flash offset 0, IVT header
# at 0x1000, and a thumb-bit entry pointing into .startup -- the exact fields whose
# mislayout cost first silicon its boot.
p_tgt = thumb2
p_be  = thumb2
lib_h = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h
include ../port.mk

te_cc = $(MOONCC) -t thumb2 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Icore -Iout/lib

objs = $(love_o) $(R)/$(o)/am.o $(libc_o) \
  $(R)/$(o)/teensy41.o $(R)/$(o)/psram.o $(R)/$(o)/main.o $(R)/$(o)/boot.o \
  $(R)/$(o)/love-img.o

.PHONY: teensy41 flash psramtest flashpsram
default: teensy41
teensy41: $(R)/$(o)/love.hex

$(foreach u,$(love_m),$(eval $(call p_obj,$u,src/$u.c,\
  $(R)/src/$u.c $(love_dep),$(te_cc))))
$(foreach m,$(libc_m),$(eval $(call p_obj,$m,crew/moon/lib/nolibc/string/$m.c,\
  $(R)/crew/moon/lib/nolibc/string/$m.c $(libc_dep),$(te_cc) -Icrew/moon/include)))
$(eval $(call p_obj,teensy41,port/teensy41/teensy41.c,teensy41.c teensy41.h $(R)/src/love.h $(mc),$(te_cc)))
$(eval $(call p_obj,main,port/teensy41/main.c,main.c teensy41.h $(R)/src/love.h $(lib_hR) $(mc),$(te_cc)))

# boot.o: the ROM-facing FlexSPI/IVT/vector blocks, crt0 and the asm helpers, laid from
# holo IR -- no gas. port/mps2/mkstart.l is the same shape with one section,
# port/rp2040/mkboot2.l the named-section one.
$(eval $(call p_lay,boot,mkboot,"$(o)/boot.o"))

# the baked heap image: port/mps2's qemu baker dumps it (a build-time bake), and mkimg.l
# lays the blob as an object in place of `ld -r -b binary`. FORCE keeps the delegation
# live -- the mps2 makefile's own deps decide whether the qemu bake re-runs.
$(R)/out/mps2/love.img: FORCE
	@$(MAKE) --no-print-directory -C $(R)/port/mps2 img
$(eval $(call p_lay,love-img,mkimg,"out/mps2/love.img" "$(o)/love-img.o"))
$(R)/$(o)/love-img.o: $(R)/out/mps2/love.img

# the link is ours: tlink.l drives holo's ldbare32 with the XIP flash map, in place of
# arm-none-eabi-ld + teensy41.lds.
$(eval $(call p_link,tlink))
$(eval $(p_ocopy))

$(R)/$(o)/love.elf: $(objs) $(R)/$(o)/tlink.l $(lv)
	@echo HOLO	$@
	@cd $(R) && out/host/love $(o)/tlink.l $(o)/love.elf $(objs:$(R)/%=%)

# .hex (Intel HEX) is what teensy_loader_cli / the Teensy Loader GUI flash. The .bin is the
# verification surface: byte offsets ARE flash offsets.
$(R)/$(o)/love.hex: $(R)/$(o)/love.elf $(R)/$(o)/ocopy.l
	@echo LOVE	$@
	@cd $(R) && out/host/love $(o)/ocopy.l -O ihex $(o)/love.elf $(o)/love.hex
	@cd $(R) && out/host/love $(o)/ocopy.l -O binary $(o)/love.elf $(o)/love.bin
	@python3 -c "import sys,struct; b=open('$(R)/$(o)/love.bin','rb').read(); \
	  fcfb=struct.unpack_from('<I',b,0)[0]; ivt=struct.unpack_from('<I',b,0x1000)[0]; \
	  entry=struct.unpack_from('<I',b,0x1004)[0]; \
	  ok = fcfb==0x42464346 and ivt==0x432000D1 and (entry&1)==1 and 0x60000000<entry<0x60800000; \
	  print('boot image: FCFB@0 %08x IVT@1000 %08x entry %08x --'%(fcfb,ivt,entry), 'ok' if ok else 'BAD'); \
	  sys.exit(0 if ok else 1)" || { echo "FAIL boot image layout"; exit 1; }

# Flash a connected Teensy 4.1 (tap the on-board button to enter the bootloader):
flash: $(R)/$(o)/love.hex
	teensy_loader_cli --mcu=TEENSY41 -w -v $(R)/$(o)/love.hex

# PSRAM smoke test (bake-free fast iteration): probe + pattern-test the FlexSPI2 PSRAM
# chips, report over serial. `make psramtest` then `make flashpsram`.
pt_objs = $(R)/$(o)/psram.o $(R)/$(o)/psram-test.o $(R)/$(o)/teensy41.o $(R)/$(o)/boot.o
$(eval $(call p_obj,psram,port/teensy41/psram.c,psram.c psram.h teensy41.h $(mc),$(te_cc)))
$(eval $(call p_obj,psram-test,port/teensy41/psram-test.c,psram-test.c psram.h teensy41.h $(mc),$(te_cc)))
psramtest: $(R)/$(o)/psram-test.hex
flashpsram: $(R)/$(o)/psram-test.hex
	teensy_loader_cli --mcu=TEENSY41 -w -v $(R)/$(o)/psram-test.hex

$(R)/$(o)/psram-test.elf: $(pt_objs) $(R)/$(o)/tlink.l $(lv)
	@echo HOLO	$@
	@cd $(R) && out/host/love $(o)/tlink.l $(o)/psram-test.elf $(pt_objs:$(R)/%=%)
$(R)/$(o)/psram-test.hex: $(R)/$(o)/psram-test.elf $(R)/$(o)/ocopy.l
	@echo LOVE	$@
	@cd $(R) && out/host/love $(o)/ocopy.l -O ihex $(o)/psram-test.elf $(o)/psram-test.hex
