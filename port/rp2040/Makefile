# Raspberry Pi Pico (RP2040) build -- Cortex-M0+ / ARMv6-M, compiled by
# mooncc -t thumb1. No Pico SDK, no CMake, no vendor headers. Run from the
# project root via `make -C port/rp2040` (needs out/host/mooncc, built by the
# root Makefile). mooncc's system include dir is CWD-relative, so every compile
# runs from $(R). Output lands in $(R)/out/rp2040/.
#
# NOTHING FOREIGN WRITES A BYTE OF THIS IMAGE. Every .o is mooncc's or holo's:
# the vector table and crt0 are C (rp2040.c), and boot2 -- the 256-byte stage
# the mask ROM checksums and runs -- is laid by mkboot2.l straight into a named
# section. This is the only thumb port with no .S, which is what the RP2040's
# C vector table buys: its cousins still assemble a boot.S with gas.
#
# ⚠ what is still foreign is the LINK: arm-none-eabi-ld binds this, because
# thumb relocations (R_ARM_ABS32, R_ARM_THM_CALL, R_ARM_THM_JUMP24) are not in
# crew/holo/link.l yet. That is the one thing between this port and a build
# with no toolchain but ours. ⚠ and the .uf2 packer (tools/elf2uf2.g) still
# speaks the pre-rename .g dialect, so `make bin` writes the raw flash image
# and a UF2 wants that port first.
#
# Faces: `make` builds love.elf + love.bin and verifies the boot surface (the
# boot2 CRC, the vector table's SP and reset entry) -- the three words a
# bricked-looking Pico always traces back to. `make objs` compiles only.
R := ../..
o = out/rp2040
MOONCC = out/host/mooncc
mc = $(R)/out/host/mooncc

rp_cc = $(MOONCC) -t thumb1 -D __STDC_HOSTED__=0 -I.

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

AS_NONE =
LD = arm-none-eabi-ld
OBJCOPY ?= $(shell command -v llvm-objcopy 2>/dev/null || echo arm-none-eabi-objcopy)
# the v6-m/nofp multilib: __aeabi_[u]idiv, the 64-bit pairs, the whole soft-float set
lg = `arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -print-libgcc-file-name`

objs = $(R)/$(o)/boot2.o $(R)/$(o)/rp2040.o $(R)/$(o)/main.o $(R)/$(o)/am.o

# the lay's cat: the thumb1 backend spliced into the sealed holo module, then
# the object writer. see port/virt/mkstart.l for the same shape at riscv64.
lay_l = $(R)/crew/kore/text.l $(R)/crew/kore/core.l $(R)/crew/kore/asbook.l \
  $(R)/crew/holo/elf.l $(R)/crew/holo/obj.l

.PHONY: default objs bin clean
default: bin
bin: $(R)/$(o)/love.bin

objs: $(objs)

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

# boot2: 240 payload bytes zero-padded to 252 and CRC-stamped, in .boot2. no
# gas -- the section name IS the interface, and the writer spells it now.
$(R)/$(o)/boot2.o: mkboot2.l boot2.bin $(R)/crew/holo/thumb1.l $(lay_l) $(R)/out/host/love
	@echo BOOT2	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && { echo "(use 'holo)"; cat crew/holo/thumb1.l $(subst $(R)/,,$(lay_l)) \
	  port/rp2040/mkboot2.l; \
	  echo '(mkboot2 "port/rp2040/boot2.bin" "$(o)/boot2.o")'; } | out/host/love

$(R)/out/host/love: FORCE
	@$(MAKE) -C $(R) out/host/love

$(R)/$(o)/love.elf: $(objs) rp2040.lds
	@echo LD	$@
	@$(LD) -T rp2040.lds $(objs) $(lg) -o $@

# .bin byte offsets ARE flash offsets, so the boot surface is readable straight
# out of it: the ROM checks boot2's CRC before anything else runs, then the
# vector table at 0x10000100 must hold an SP inside SRAM and a thumb-bit reset
# entry inside flash. Any one of the three wrong and the board just sits there.
$(R)/$(o)/love.bin: $(R)/$(o)/love.elf
	@echo BIN	$@
	@$(OBJCOPY) -O binary $< $@
	@python3 -c "import struct; b=open('$(R)/$(o)/love.bin','rb').read(); \
	  crc=struct.unpack_from('<I',b,252)[0]; sp,e=struct.unpack_from('<II',b,256); \
	  ok = crc==0x7a4eb274 and 0x20000000<sp<=0x20042000 and (e&1)==1 and 0x10000100<e<0x10200000; \
	  print('boot image: boot2 crc %08x sp %08x entry %08x --'%(crc,sp,e), 'ok' if ok else 'BAD'); \
	  exit(0 if ok else 1)" || { echo "FAIL rp2040 boot image"; exit 1; }
