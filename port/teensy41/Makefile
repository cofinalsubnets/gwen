# Teensy 4.1 (i.MX RT1062) build -- love compiled END TO END by mooncc.
# Run from the project root via `make -C port/teensy41` (needs out/host/mooncc,
# built by the root Makefile). Every C file -- love.c, the am math floor,
# libc, teensy41.c, main.c -- goes through `mooncc -t thumb2`; boot.S (the
# ROM-facing FlexSPI/IVT/vector blocks + crt0 + the asm helpers) is
# arm-none-eabi-gcc-assembled and arm-none-eabi-ld links against
# teensy41.lds. No libgcc: mooncc's thumb2 lanes are self-contained (zero
# __aeabi_*). mooncc's system include dir is CWD-relative, so every compile
# runs from $(R). Output lands in $(R)/out/teensy41/.
#
# The default target also VERIFIES the boot image: FCFB tag at flash offset
# 0, IVT header at 0x1000, and a thumb-bit entry pointing into .startup --
# the exact fields whose mislayout cost first silicon its boot.
R := ../..
o = out/teensy41
MOONCC = out/host/mooncc
AS = arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16
LD = arm-none-eabi-ld
OBJCOPY ?= $(shell command -v llvm-objcopy 2>/dev/null || echo arm-none-eabi-objcopy)

te_cc = $(MOONCC) -t thumb2 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Iout/lib
lib_h = out/lib/egg.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h

objs = $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o $(R)/$(o)/str.o \
  $(R)/$(o)/teensy41.o $(R)/$(o)/psram.o $(R)/$(o)/main.o $(R)/$(o)/boot.o \
  $(R)/$(o)/love-img.o

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

.PHONY: teensy41 flash clean
teensy41: $(R)/$(o)/love.hex
default: teensy41

clean:
	rm -rf $(R)/$(o)

# the lcat lib headers come from the root's own rules (on love0), on demand
$(addprefix $(R)/,$(lib_h)):
	@$(MAKE) -C $(R) $(patsubst $(R)/%,%,$@)

$(R)/$(o)/love.o: $(R)/love.c $(R)/love.h $(addprefix $(R)/,$(lib_h))
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(te_cc) -c love.c $(o)/love.o

$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t thumb2 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o

$(R)/$(o)/%.o: $(R)/libc/%.c $(R)/love.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(te_cc) -c libc/$*.c $(o)/$*.o

$(R)/$(o)/teensy41.o: teensy41.c teensy41.h $(R)/love.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(te_cc) -c port/teensy41/teensy41.c $(o)/teensy41.o

$(R)/$(o)/main.o: main.c teensy41.h $(R)/love.h $(addprefix $(R)/,$(lib_h))
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(te_cc) -c port/teensy41/main.c $(o)/main.o

$(R)/$(o)/boot.o: boot.S
	@echo AS	$@
	@mkdir -p $(R)/$(o)
	@$(AS) -c boot.S -o $@

# the baked heap image: port/mps2's qemu baker dumps it (build-time bake);
# ld -b binary wraps the blob with _binary_love_img_{start,end} symbols and
# teensy41.lds places it in flash (.image). FORCE keeps the delegation live
# (the mps2 makefile's own deps decide whether the qemu bake re-runs).
$(R)/out/mps2/love.img: FORCE
	@$(MAKE) --no-print-directory -C $(R)/port/mps2 img
FORCE:

$(R)/$(o)/love-img.o: $(R)/out/mps2/love.img
	@echo IMG	$@
	@mkdir -p $(R)/$(o)
	@cp $< $(R)/$(o)/love.img
	@cd $(R)/$(o) && $(LD) -r -b binary love.img -o love-img.o

$(R)/$(o)/love.elf: $(objs) teensy41.lds
	@echo LD	$@
	@$(LD) -T teensy41.lds $(objs) -o $@

# .hex (Intel HEX) is what teensy_loader_cli / the Teensy Loader GUI flash.
# The .bin is the verification surface: byte offsets ARE flash offsets.
$(R)/$(o)/love.hex: $(R)/$(o)/love.elf
	@echo HEX	$@
	@$(OBJCOPY) -O ihex -R .eh_frame $< $@
	@$(OBJCOPY) -O binary $< $(R)/$(o)/love.bin
	@python3 -c "import sys,struct; b=open('$(R)/$(o)/love.bin','rb').read(); \
	  fcfb=struct.unpack_from('<I',b,0)[0]; ivt=struct.unpack_from('<I',b,0x1000)[0]; \
	  entry=struct.unpack_from('<I',b,0x1004)[0]; \
	  ok = fcfb==0x42464346 and ivt==0x432000D1 and (entry&1)==1 and 0x60000000<entry<0x60800000; \
	  print('boot image: FCFB@0 %08x IVT@1000 %08x entry %08x --'%(fcfb,ivt,entry), 'ok' if ok else 'BAD'); \
	  sys.exit(0 if ok else 1)" || { echo "FAIL boot image layout"; exit 1; }

# Flash a connected Teensy 4.1 (tap the on-board button to enter the bootloader):
flash: $(R)/$(o)/love.hex
	teensy_loader_cli --mcu=TEENSY41 -w -v $(R)/$(o)/love.hex

# PSRAM smoke test (bake-free fast iteration): probe + pattern-test the
# FlexSPI2 PSRAM chips, report over serial. `make psramtest` then
# `make flashpsram` (or teensy_loader_cli by hand on psram-test.hex).
pt_objs = $(R)/$(o)/psram.o $(R)/$(o)/psram-test.o $(R)/$(o)/teensy41.o $(R)/$(o)/boot.o
$(R)/$(o)/psram.o: psram.c psram.h teensy41.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(te_cc) -c port/teensy41/psram.c $(o)/psram.o
$(R)/$(o)/psram-test.o: psram-test.c psram.h teensy41.h
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(te_cc) -c port/teensy41/psram-test.c $(o)/psram-test.o
$(R)/$(o)/psram-test.elf: $(pt_objs) teensy41.lds
	@echo LD	$@
	@$(LD) -T teensy41.lds $(pt_objs) -o $@
$(R)/$(o)/psram-test.hex: $(R)/$(o)/psram-test.elf
	@echo HEX	$@
	@$(OBJCOPY) -O ihex $< $@
.PHONY: psramtest flashpsram
psramtest: $(R)/$(o)/psram-test.hex
flashpsram: $(R)/$(o)/psram-test.hex
	teensy_loader_cli --mcu=TEENSY41 -w -v $(R)/$(o)/psram-test.hex
