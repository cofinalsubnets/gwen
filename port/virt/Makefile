# qemu -M virt (riscv64) build -- love with NO foreign toolchain anywhere: every C file
# goes through `mooncc -t riscv64`, start.o is laid from holo IR (mkstart.l), and OUR
# linker binds the ELF. `make -C port/virt` from the project root.
#
# THE ORG: virt's mask ROM jumps to the DRAM base 0x80000000 -- not the ELF entry -- so the
# image lays at 0x80000000 - 120 (the one-segment header's size): the header falls in the
# unmapped hole below DRAM, where the loader's writes are discarded, and the FIRST TEXT
# BYTE lands exactly at 0x80000000. That first text is the driver's crt0, whose
# `call __ai_start` lands in start.o's STRONG override (sp, FPU, mtvec, then main) -- the
# same weak seam nolibc rides, doing bare-metal bring-up.
#
# Boot it: qemu-system-riscv64 -M virt -bios none -nographic -kernel love.elf
# (exit 42 = the egg hatched and the driver tail's laws held; 98 = a trap, with mcause/mepc
# named on the console; `make test_virt` at the root gates exactly that).
p_tgt = riscv64
p_be  = riscv
lib_h = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h
include ../port.mk

virt_cc = $(MOONCC) -t riscv64 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Iout/lib

objs = $(R)/$(o)/start.o $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o \
  $(R)/$(o)/str.o $(R)/$(o)/main.o

.PHONY: virt
default: virt
virt: $(R)/$(o)/love.elf

$(eval $(call p_lay,start,mkstart,"$(o)/start.o",START))

$(eval $(call p_obj,love,love.c,$(R)/love.c $(R)/love.h $(lib_hR) $(mc),$(virt_cc)))
$(eval $(call p_obj,mem,libc/mem.c,$(R)/libc/mem.c $(R)/love.h $(mc),$(virt_cc)))
$(eval $(call p_obj,str,libc/str.c,$(R)/libc/str.c $(R)/love.h $(mc),$(virt_cc)))
$(eval $(call p_obj,main,port/virt/main.c,main.c $(R)/love.h $(lib_hR) $(mc),$(virt_cc)))

$(R)/$(o)/love.elf: $(objs)
	@echo LINK	$@
	@cd $(R) && $(MOONCC) -t riscv64 -Ttext 0x7FFFFF88 $(objs:$(R)/%=%) -o $(o)/love.elf
