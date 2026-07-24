# Nucleo-F446RE build -- firmware compiled END TO END by mooncc -t thumb2sp
# (the F446's Cortex-M4 has the single-precision FPv4 FPU: the playdate's
# exact target -- f64 arithmetic softens to __aeabi_* libgcc calls, so the
# link takes gcc's cortex-m4 hard-float multilib libgcc; boot.S is the only
# gas-assembled piece). mooncc's system include dir is CWD-relative, so every
# compile runs from $(R). Output lands in $(R)/out/nucleo446/.
#
# Faces: `make` builds firm.bin/.hex (the device firmware; flash with `make
# flash` -- st-flash -- or copy firm.bin to the NODE_F446RE mass-storage
# drive). `make smoke` builds the -D QSMOKE twin that leaves through qemu
# semihosting with the self-check tally; `make test_nucleo446` at the root
# boots it on qemu's STM32F405 (netduinoplus2) and expects 28.
R := ../..
o = out/nucleo446
MOONCC = out/host/mooncc
mc = $(R)/out/host/mooncc
AS = arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16
LD = arm-none-eabi-ld
OBJCOPY ?= $(shell command -v llvm-objcopy 2>/dev/null || echo arm-none-eabi-objcopy)
lg = `arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -print-libgcc-file-name`

nu_cc = $(MOONCC) -t thumb2sp -D __STDC_HOSTED__=0 -I.

objs = $(R)/$(o)/nucleo446.o $(R)/$(o)/main.o $(R)/$(o)/am.o $(R)/$(o)/boot.o
qobjs = $(R)/$(o)/nucleo446-q.o $(R)/$(o)/main-q.o $(R)/$(o)/am.o $(R)/$(o)/boot.o

.PHONY: nucleo446 smoke flash clean
nucleo446: $(R)/$(o)/firm.hex
default: nucleo446
smoke: $(R)/$(o)/smoke.elf

clean:
	rm -rf $(R)/$(o)

$(R)/$(o)/nucleo446.o: nucleo446.c nucleo446.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(nu_cc) -c port/nucleo446/nucleo446.c $(o)/nucleo446.o

$(R)/$(o)/main.o: main.c nucleo446.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(nu_cc) -c port/nucleo446/main.c $(o)/main.o

$(R)/$(o)/nucleo446-q.o: nucleo446.c nucleo446.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(nu_cc) -D QSMOKE -c port/nucleo446/nucleo446.c $(o)/nucleo446-q.o

$(R)/$(o)/main-q.o: main.c nucleo446.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(nu_cc) -D QSMOKE -c port/nucleo446/main.c $(o)/main-q.o

$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t thumb2sp -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o

$(R)/$(o)/boot.o: boot.S
	@echo AS	$@
	@mkdir -p $(R)/$(o)
	@$(AS) -c boot.S -o $@

$(R)/$(o)/firm.elf: $(objs) nucleo446.lds
	@echo LD	$@
	@$(LD) -T nucleo446.lds $(objs) $(lg) -o $@

$(R)/$(o)/smoke.elf: $(qobjs) nucleo446.lds
	@echo LD	$@
	@$(LD) -T nucleo446.lds $(qobjs) $(lg) -o $@

# .bin byte offsets ARE flash offsets -- verify the boot surface: a valid
# initial SP inside SRAM and a thumb-bit reset entry inside flash (the two
# words a bricked-looking board always traces back to).
$(R)/$(o)/firm.hex: $(R)/$(o)/firm.elf
	@echo HEX	$@
	@$(OBJCOPY) -O ihex $< $@
	@$(OBJCOPY) -O binary $< $(R)/$(o)/firm.bin
	@python3 -c "import struct; b=open('$(R)/$(o)/firm.bin','rb').read(); \
	  sp=struct.unpack_from('<I',b,0)[0]; entry=struct.unpack_from('<I',b,4)[0]; \
	  ok = 0x20000000<sp<=0x20020000 and (entry&1)==1 and 0x08000000<entry<0x08080000; \
	  print('boot image: sp %08x entry %08x --'%(sp,entry), 'ok' if ok else 'BAD'); \
	  exit(0 if ok else 1)" || { echo "FAIL boot image layout"; exit 1; }

# Flash a connected Nucleo over its ST-LINK:
flash: $(R)/$(o)/firm.hex
	st-flash --reset write $(R)/$(o)/firm.bin 0x08000000
