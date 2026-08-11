# Raspberry Pi Pico (RP2040) build -- Cortex-M0+ / ARMv6-M, compiled by mooncc -t thumb1.
# No Pico SDK, no CMake, no vendor headers. `make -C port/rp2040` from the project root.
#
# NOTHING FOREIGN WRITES A BYTE OF THIS IMAGE, and nothing foreign reads it either: every
# .o is mooncc's or holo's (the vector table and crt0 are C in rp2040.c, and boot2 -- the
# 256-byte stage the mask ROM checksums and runs -- is laid by mkboot2.l straight into a
# named section), rlink.l binds it over holo's ldbare32, and ocopy.l flattens it. gcc's
# cortex-m0 libgcc is the one foreign FILE, read as an archive by need and never run.
#
# ⚠ the .uf2 packer (tools/elf2uf2.g) still speaks the pre-rename .g dialect, so `make bin`
# writes the raw flash image and a UF2 wants that port first.
#
# Faces: `make` builds love.elf + love.bin and verifies the segment permissions and the
# boot surface (the boot2 CRC, the vector table's SP and reset entry) -- the three words a
# bricked-looking Pico always traces back to. `make objs` compiles only.
p_tgt      = thumb1
p_be       = thumb1
p_link_be  = thumb1 thumb2
include ../port.mk

rp_cc = $(MOONCC) -t thumb1 -D __STDC_HOSTED__=0 -I.
# the v6-m/nofp multilib: __aeabi_[u]idiv, the 64-bit pairs, the whole soft-float set
lg = `arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -print-libgcc-file-name`

objs = $(R)/$(o)/boot2.o $(R)/$(o)/rp2040.o $(R)/$(o)/main.o $(R)/$(o)/am.o

.PHONY: objs bin
default: bin
bin: $(R)/$(o)/love.bin
objs: $(objs)

$(eval $(call p_obj,main,port/rp2040/main.c,main.c rp2040.h $(mc),$(rp_cc)))
$(eval $(call p_obj,rp2040,port/rp2040/rp2040.c,rp2040.c rp2040.h $(mc),$(rp_cc)))

# boot2: 240 payload bytes zero-padded to 252 and CRC-stamped, in .boot2. no gas -- the
# section name IS the interface, and the writer spells it now.
$(eval $(call p_lay,boot2,mkboot2,"port/rp2040/boot2.bin" "$(o)/boot2.o"))
$(R)/$(o)/boot2.o: boot2.bin

# the link is ours: rlink.l drives holo's ldbare32 with the Pico's flash map, in place of
# arm-none-eabi-ld + rp2040.lds. libgcc.a is named on the line and read as a LIBRARY, by
# need through the ranlib index -- never run.
$(eval $(call p_link,rlink))
$(eval $(p_ocopy))

# Flash R|X, SRAM R|W: rlink.l asserts the first and the sections answer for the second.
# Read back rather than trusted -- nothing here is both writable and executable.
$(R)/$(o)/love.elf: $(objs) $(R)/$(o)/rlink.l $(lv)
	@echo HOLO	$@
	@cd $(R) && out/host/love $(o)/rlink.l $(o)/love.elf $(objs:$(R)/%=%) $(lg)
	@python3 -c "import struct; b=open('$(R)/$(o)/love.elf','rb').read(); \
	  po,pe,pn = struct.unpack_from('<I',b,28)[0], struct.unpack_from('<H',b,42)[0], struct.unpack_from('<H',b,44)[0]; \
	  ph=[struct.unpack_from('<8I',b,po+i*pe) for i in range(pn)]; \
	  ld=[(p[2],p[6]) for p in ph if p[0]==1]; \
	  wx=[hex(a) for a,f in ld if (f&2) and (f&1)]; \
	  fl=[f for a,f in ld if a==0x10000000]; \
	  ok = fl==[5] and not wx; \
	  print('segments:', ' '.join('%08x %s'%(a,'RWE' if f==7 else 'R E' if f==5 else 'RW ' if f==6 else str(f)) for a,f in ld), '--', 'ok' if ok else 'BAD (w+x: %s)'%wx); \
	  exit(0 if ok else 1)" || { echo "FAIL rp2040 segment permissions"; exit 1; }

# .bin byte offsets ARE flash offsets, so the boot surface is readable straight out of it:
# the ROM checks boot2's CRC before anything else runs, then the vector table at 0x10000100
# must hold an SP inside SRAM and a thumb-bit reset entry inside flash. Any one of the
# three wrong and the board just sits there.
$(R)/$(o)/love.bin: $(R)/$(o)/love.elf $(R)/$(o)/ocopy.l
	@echo LOVE	$@
	@cd $(R) && out/host/love $(o)/ocopy.l -O binary $(o)/love.elf $(o)/love.bin
	@python3 -c "import struct; b=open('$(R)/$(o)/love.bin','rb').read(); \
	  crc=struct.unpack_from('<I',b,252)[0]; sp,e=struct.unpack_from('<II',b,256); \
	  ok = crc==0x7a4eb274 and 0x20000000<sp<=0x20042000 and (e&1)==1 and 0x10000100<e<0x10200000; \
	  print('boot image: boot2 crc %08x sp %08x entry %08x --'%(crc,sp,e), 'ok' if ok else 'BAD'); \
	  exit(0 if ok else 1)" || { echo "FAIL rp2040 boot image"; exit 1; }
