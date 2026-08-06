# Nucleo-F446RE build -- firmware compiled END TO END by mooncc -t thumb2sp
# (the F446's Cortex-M4 has the single-precision FPv4 FPU: the playdate's
# exact target -- f64 arithmetic softens to __aeabi_* libgcc calls, so the
# link takes gcc's cortex-m4 hard-float multilib libgcc -- a LIBRARY read by
# need, not a tool run). boot.o is laid from holo IR by mkboot.l and nlink.l does
# the link over holo's ldbare32, so the port owes neither gas nor ld nor a linker
# script. mooncc's system include dir is CWD-relative, so every compile runs from
# $(R). Output lands in $(R)/out/nucleo446/.
#
# Faces: `make` builds firm.bin/.hex (the device firmware; flash with `make
# flash` -- st-flash -- or copy firm.bin to the NODE_F446RE mass-storage
# drive), and that is what `make test_nucleo446` gates: mooncc compiled every
# C file and the boot image is valid. `make smoke` builds the -D QSMOKE twin
# that leaves through qemu semihosting with the self-check tally -- boot it by
# hand on netduinoplus2 and expect 28 -- `make test_nucleo446_smoke` does it.
R := ../..
o = out/nucleo446
MOONCC = out/host/mooncc
mc = $(R)/out/host/mooncc
OBJCOPY ?= $(shell command -v llvm-objcopy 2>/dev/null || echo arm-none-eabi-objcopy)
lg = `arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -print-libgcc-file-name`

nu_cc = $(MOONCC) -t thumb2sp -D __STDC_HOSTED__=0 -I.

objs = $(R)/$(o)/nucleo446.o $(R)/$(o)/main.o $(R)/$(o)/am.o $(R)/$(o)/boot.o
qobjs = $(R)/$(o)/nucleo446-q.o $(R)/$(o)/main-q.o $(R)/$(o)/am.o $(R)/$(o)/boot.o

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

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

# boot.o: the vector table, crt0, the fault shim and the asm helpers, laid from holo
# IR by mkboot.l -- no gas. The lay's cat is the thumb2 backend spliced into the sealed
# holo module, then the object writer; port/teensy41/mkboot.l is the same shape with
# the i.MX ROM blocks on top.
lay_l = $(R)/crew/kore/text.l $(R)/crew/kore/core.l $(R)/crew/kore/asbook.l \
  $(R)/crew/holo/elf.l $(R)/crew/holo/obj.l

$(R)/$(o)/boot.o: mkboot.l $(R)/crew/holo/thumb2.l $(lay_l) $(R)/out/host/love
	@echo LAY	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && { echo "(use 'holo)"; cat crew/holo/thumb2.l $(subst $(R)/,,$(lay_l)) \
	  port/nucleo446/mkboot.l; \
	  echo '(mkboot "$(o)/boot.o")'; } | out/host/love

$(R)/out/host/love: FORCE
	@$(MAKE) -C $(R) out/host/love


# the link is ours: nlink.l drives holo's ldbare32 with the F4's memory map, in
# place of arm-none-eabi-ld + nucleo446.lds. libgcc.a is named on the line and its
# members are pulled by need through the ranlib index, so the .a is a LIBRARY the
# link reads and not a tool it runs.
link_l = $(R)/crew/kore/text.l $(R)/crew/kore/core.l $(R)/crew/kore/asbook.l \
  $(R)/crew/holo/thumb2.l $(R)/crew/holo/elf.l $(R)/crew/holo/obj.l $(R)/crew/holo/link.l

$(R)/$(o)/nlink.l: nlink.l $(link_l)
	@echo AI	$@
	@mkdir -p $(R)/$(o)
	@{ echo "(use 'holo)"; cat $(link_l) $<; } > $@

$(R)/$(o)/firm.elf: $(objs) $(R)/$(o)/nlink.l $(R)/out/host/love
	@echo LINK	$@
	@cd $(R) && out/host/love $(o)/nlink.l $(o)/firm.elf $(objs:$(R)/%=%) $(lg)

$(R)/$(o)/smoke.elf: $(qobjs) $(R)/$(o)/nlink.l $(R)/out/host/love
	@echo LINK	$@
	@cd $(R) && out/host/love $(o)/nlink.l $(o)/smoke.elf $(qobjs:$(R)/%=%) $(lg)

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

.PHONY: FORCE
FORCE:
