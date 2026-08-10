# Nucleo-F446RE build -- firmware compiled END TO END by mooncc -t thumb2sp (the F446's
# Cortex-M4 has the single-precision FPv4 FPU: the playdate's exact target -- f64 softens
# to __aeabi_* libgcc calls, so the link takes gcc's cortex-m4 hard-float multilib libgcc,
# a LIBRARY read by need, not a tool run). boot.o is laid from holo IR by mkboot.l, nlink.l
# links over holo's ldbare32 and ocopy.l flattens over holo's copy.l, so the port owes no
# gas, no ld, no linker script and no objcopy.
#
# Faces: `make` builds firm.bin/.hex (flash with `make flash` -- st-flash -- or copy
# firm.bin to the NODE_F446RE mass-storage drive), and that is what `make test_nucleo446`
# gates: mooncc compiled every C file and the boot image is valid. `make smoke` builds the
# -D QSMOKE twin that leaves through qemu semihosting with the self-check tally -- boot it
# by hand on netduinoplus2 and expect 28 -- `make test_nucleo446_smoke` does it.
p_tgt = thumb2sp
p_be  = thumb2
include ../port.mk

nu_cc = $(MOONCC) -t thumb2sp -D __STDC_HOSTED__=0 -I.
lg = `arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -print-libgcc-file-name`

objs  = $(R)/$(o)/nucleo446.o $(R)/$(o)/main.o $(R)/$(o)/am.o $(R)/$(o)/boot.o
qobjs = $(R)/$(o)/nucleo446-q.o $(R)/$(o)/main-q.o $(R)/$(o)/am.o $(R)/$(o)/boot.o

.PHONY: nucleo446 smoke flash
default: nucleo446
nucleo446: $(R)/$(o)/firm.hex
smoke: $(R)/$(o)/smoke.elf

$(eval $(call p_obj,nucleo446,port/nucleo446/nucleo446.c,nucleo446.c nucleo446.h $(mc),$(nu_cc)))
$(eval $(call p_obj,main,port/nucleo446/main.c,main.c nucleo446.h $(mc),$(nu_cc)))
# the -D QSMOKE twin, the same two sources through the same cc
$(eval $(call p_obj,nucleo446-q,port/nucleo446/nucleo446.c,nucleo446.c nucleo446.h $(mc),$(nu_cc) -D QSMOKE))
$(eval $(call p_obj,main-q,port/nucleo446/main.c,main.c nucleo446.h $(mc),$(nu_cc) -D QSMOKE))

# boot.o: the vector table, crt0, the fault shim and the asm helpers, laid from holo IR --
# no gas. port/teensy41/mkboot.l is the same shape with the i.MX ROM blocks on top.
$(eval $(call p_lay,boot,mkboot,"$(o)/boot.o"))

# the link is ours: nlink.l drives holo's ldbare32 with the F4's memory map, in place of
# arm-none-eabi-ld + nucleo446.lds.
$(eval $(call p_link,nlink))
$(eval $(p_ocopy))

$(R)/$(o)/firm.elf: $(objs) $(R)/$(o)/nlink.l $(lv)
	@echo HOLO	$@
	@cd $(R) && out/host/love $(o)/nlink.l $(o)/firm.elf $(objs:$(R)/%=%) $(lg)

$(R)/$(o)/smoke.elf: $(qobjs) $(R)/$(o)/nlink.l $(lv)
	@echo HOLO	$@
	@cd $(R) && out/host/love $(o)/nlink.l $(o)/smoke.elf $(qobjs:$(R)/%=%) $(lg)

# .bin byte offsets ARE flash offsets -- verify the boot surface: a valid initial SP inside
# SRAM and a thumb-bit reset entry inside flash (the two words a bricked-looking board
# always traces back to).
$(R)/$(o)/firm.hex: $(R)/$(o)/firm.elf $(R)/$(o)/ocopy.l
	@echo LOVE	$@
	@cd $(R) && out/host/love $(o)/ocopy.l -O ihex $(o)/firm.elf $(o)/firm.hex
	@cd $(R) && out/host/love $(o)/ocopy.l -O binary $(o)/firm.elf $(o)/firm.bin
	@python3 -c "import struct; b=open('$(R)/$(o)/firm.bin','rb').read(); \
	  sp=struct.unpack_from('<I',b,0)[0]; entry=struct.unpack_from('<I',b,4)[0]; \
	  ok = 0x20000000<sp<=0x20020000 and (entry&1)==1 and 0x08000000<entry<0x08080000; \
	  print('boot image: sp %08x entry %08x --'%(sp,entry), 'ok' if ok else 'BAD'); \
	  exit(0 if ok else 1)" || { echo "FAIL boot image layout"; exit 1; }

# Flash a connected Nucleo over its ST-LINK:
flash: $(R)/$(o)/firm.hex
	st-flash --reset write $(R)/$(o)/firm.bin 0x08000000
