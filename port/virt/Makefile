# qemu -M virt (riscv64) build -- love with NO foreign toolchain anywhere:
# every C file goes through `mooncc -t riscv64`, start.o is laid from holo IR
# (mkstart.l, via the host love with the riscv backend spliced in), and OUR
# linker binds the ELF. Run from the project root via `make -C port/virt`
# (needs out/host/mooncc + out/host/love, built by the root Makefile).
# mooncc's system include dir is CWD-relative, so every compile runs from $(R).
#
# THE ORG: virt's mask ROM jumps to the DRAM base 0x80000000 -- not the ELF
# entry -- so the image lays at 0x80000000 - 120 (the one-segment header's
# size): the header falls in the unmapped hole below DRAM (the loader's
# writes there are discarded) and the FIRST TEXT BYTE lands exactly at
# 0x80000000. That first text is the driver's crt0, whose `call __ai_start`
# lands in start.o's STRONG override (sp, FPU, mtvec, then main) -- the same
# weak seam nolibc rides, doing bare-metal bring-up.
#
# Boot it: qemu-system-riscv64 -M virt -bios none -nographic -kernel love.elf
# (exit 42 = the egg hatched and the driver tail's laws held; 98 = a trap,
# with mcause/mepc named on the console; `make test_virt` at the root gates
# exactly that).
R := ../..
o = out/virt
MOONCC = out/host/mooncc
mc = $(R)/out/host/mooncc
lv = $(R)/out/host/love

virt_cc = $(MOONCC) -t riscv64 -D ai_tco=1 -D __STDC_HOSTED__=0 -I. -Iout/lib
lib_h = out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h out/lib/love_version.h

objs = $(R)/$(o)/start.o $(R)/$(o)/love.o $(R)/$(o)/am.o $(R)/$(o)/mem.o \
  $(R)/$(o)/str.o $(R)/$(o)/main.o

# a failed recipe takes its half-written target with it -- else a 0-byte artifact
# carries a fresh mtime and the next make calls it up to date. this makefile stands
# alone (the root does not include it), so it says so itself. see ../../Makefile.
.DELETE_ON_ERROR:

.PHONY: virt clean
virt: $(R)/$(o)/love.elf
default: virt

$(R)/$(o)/start.o: mkstart.l $(R)/crew/holo/riscv.l $(R)/crew/holo/obj.l $(R)/crew/holo/elf.l $(lv)
	@echo START	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && { echo "(use 'holo)"; cat crew/holo/riscv.l; \
	  cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l port/virt/mkstart.l; \
	  echo '(mkstart "$(o)/start.o")'; } | out/host/love

$(R)/$(o)/love.o: $(R)/love.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(virt_cc) -c love.c $(o)/love.o

$(R)/$(o)/am.o: $(R)/crew/moon/lib/math/am.c $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(MOONCC) -t riscv64 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $(o)/am.o

$(R)/$(o)/mem.o: $(R)/libc/mem.c $(R)/love.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(virt_cc) -c libc/mem.c $(o)/mem.o

$(R)/$(o)/str.o: $(R)/libc/str.c $(R)/love.h $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(virt_cc) -c libc/str.c $(o)/str.o

$(R)/$(o)/main.o: main.c $(R)/love.h $(addprefix $(R)/,$(lib_h)) $(mc)
	@echo MOON	$@
	@mkdir -p $(R)/$(o)
	@cd $(R) && $(virt_cc) -c port/virt/main.c $(o)/main.o

$(R)/$(o)/love.elf: $(objs)
	@echo LINK	$@
	@cd $(R) && $(MOONCC) -t riscv64 -Ttext 0x7FFFFF88 $(addprefix $(o)/,start.o love.o am.o mem.o str.o main.o) -o $(o)/love.elf

clean:
	rm -rf $(R)/$(o)

# the lcat lib headers come from the root's own rules (on love0), on demand.
# FORCE: a prerequisite-less delegation only fires when the target is MISSING,
# so a stale header would be served forever (the [[stale-delegated-header]]
# trap; same note in port/mps2 + port/playdate).
.PHONY: FORCE
FORCE:
$(addprefix $(R)/,$(sort $(lib_h))): FORCE
	@$(MAKE) -C $(R) $(patsubst $(R)/%,%,$@)
# the binaries delegate the same way -- and the explicit rule also BLOCKS make's
# builtin `%: %.o`: out/host/love.o (the root's liblove lane) sits beside the
# binary, and a bare prerequisite let the builtin "relink" love from that lone
# object -- undefined mains all the way down, and make DELETES the half-made
# binary on the way out.
$(lv): FORCE
	@$(MAKE) -C $(R) out/host/love
$(mc): FORCE
	@$(MAKE) -C $(R) out/host/mooncc
