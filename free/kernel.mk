# free/kernel.mk -- the inle kernel, freestanding, into out/free. Included by
# ./Makefile from the project root, so paths resolve from there; shared vars are mk/common.mk.
#
# Arch-independent glue is free/{kmain.c,k.h}, per-arch code free/<a>/. Each
# arch brings itself up under `qemu -kernel` with no bootloader or firmware at all (the
# PVH stub on x86_64, the EL1 MMU stub on aarch64, both laid by mkboot.l); free/uefi/'s
# own BOOTX64.EFI is the second door, and the one that hands over a framebuffer.
ko = out/free
# downloaded, not built -- so it lives OUTSIDE out/ and `make clean` leaves it standing.
# `make distclean` is the one that asks for the network again.
dl = dl

# every gate and verb below is phony: one roster, so adding one is one line and not two.
.PHONY: force_kfs_list kmain_o run run-$a run-sh run-headless init-container \
  uefi test_arm64 test_kernel test_disk test_uefi test_kboot test_kernel_arm64 \
  test_inle test_wasm

# K_TEST=1 builds a headless serial test kernel (batch read-eval over COM1, with an
# `exit` nif that quits qemu) into its own odir and elf, so it never clobbers the
# normal interactive one.
ifdef K_TEST
ksuf := -test
endif

# The COMPILER is ours, and only ours: mooncc compiles every TU, holo lays the assembly
# and links. KCC names WHICH love drives it, not which compiler -- a foreign cc has no
# lane here (dropped 2026-08-19: HCC covers foreign-cc on the host and ccbench races them
# over the same TUs, so a second kernel compiler earned nothing it did not already cost).
# ⚠ mooncc is love's own verb now (the layered bake, doc/plan/one-binary.md), and the
# LOVE_NO_IMAGE= clear is load-bearing (the guard against an exported egg): an
# egg-booted love has no verb table -- `mooncc` would read as a filename.
KCC ?= LOVE_NO_IMAGE= $(ho)/love mooncc

k_arch_c = $(wildcard $(R)/free/$a/*.c)
k_free_c = $R/free/kmain.c $R/free/blk.c
# paint.c is named rather than wildcarded (mk/common.mk): the console renders 32bpp,
# so this seat wants the shared painter. nif.c stays out until the kernel grows
# defs[] rows for it -- bodies nothing calls are bytes the image cannot spend.
k_shared_c = $(love_c) $(f_c) $R/crew/quay/paint.c $(c_c)
k_h = $(love_h) $(wildcard *.h $(R)/free/*.h $(R)/free/$a/*.h)

k_odir = $(ko)/$a$(ksuf)
k_elf = $(ko)/love-$a$(ksuf).elf

k_shared_o = $(k_shared_c:$(R)/%.c=$(k_odir)/%.o)
k_arch_o = $(k_arch_c:$(R)/%.c=$(k_odir)/%.o)
k_free_o = $(k_free_c:$(R)/%.c=$(k_odir)/%.o)
# the two LAYS: what used to be four .S files.
# boot.o is the bring-up, vec.o the interrupt tail; both are holo IR written in
# love (free/mk{boot,vec}.l), so no assembler runs in this build at all.
k_lay_o = $(k_odir)/free/$a/boot.o $(k_odir)/free/$a/vec.o
k_o = $(k_shared_o) $(k_arch_o) $(k_free_o) $(k_lay_o)

# The kernel runs the GENERATIONAL collector bounded by g->budget: kmain sums the boot
# memmap into kram_words and sets budget = kram_words/8 after ai_ini (the Appel knob).
# ⚠ unbounded, the nursery's copy-overhead resizer grows until gen_major's all-survive
# sizing asks kmallocw for a block bigger than any physical RAM range. gen_please, core/love.c.
kcflags = $(ai_cflags) -nostdinc -ffreestanding -fno-lto -fno-PIC \
  -ffunction-sections -fdata-sections
kcppflags := \
  -I$(k_odir) \
  -I. -Icore -I$(R)/out/host -Iout/lib -I$(R)/crew/quay -I$(R) -I$(R)/free \
  -I$(R)/free/$a \
  -I$(R)/crew/moon/include \
  $(kcppflags)
ifdef K_TEST
# tail-threaded, matching the real kernel and the host; love0 stays the trampoline lane.
kcppflags += -DK_TEST -Dai_tco=1
endif
# no machine flags: `-t` names the backend and the -m* soup is vacuous for our codegen --
# nothing of ours ever lives below sp (no red zone to disable), and we emit abs64 and
# pc-relative relocations and nothing else, so the top 2 GiB needs no code model.
# ⚠ mooncc REFUSES a -m flag rather than ignoring it (dropping one silently would be the
# no-op wearing a cc face), which is the other reason there are none to pass.
kcc = $(KCC) $(kcflags) $(kcppflags) -t $(k_be_$a)
# ours has to exist before it can compile anything.
kcc_dep = $(ho)/love.baked

kernel: $(k_elf)

# The LINK is ours, and only ours: holo's kernel lane (crew/holo/link.l's ldkern, driven
# by free/klink.l) lays the shape -- the note, five page-aligned PT_LOADs,
# p_paddr = p_vaddr - bias, entry by symbol, kimage_end -- and all three doors boot what
# it writes. The <a>.lds files and the KLINK=lld lane that read them went 2026-08-19: a
# linker twin nothing ran, and it was the one thing keeping the two kernel seats on
# core/mx.l's hand-spliced roster. No --gc-sections either -- the image carries some dead
# code, and it is RAM the kernel has plenty of.
klink_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/free/klink.l
$(k_odir)/klink.list: force_dist_list
	@mkdir -p "$(dir $@)"
	@tf=$@.$$$$.tmp; echo '$(klink_l)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
$(k_odir)/klink.l: $(klink_l) $(k_odir)/klink.list
	@echo CAT	$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(klink_l)); } > $@

$(k_elf): $(k_odir)/klink.l $(k_o) $m
	@echo HOLO	$@
	@mkdir -p "$(dir $@)"
	@$m $(k_odir)/klink.l $@ $a $(k_o)

# --- the initrd ------------------------------------------------------
# lib/*.l baked per-file into .rodata as {path, bytes, len} rows (mk/tools/lcatfs.l), which
# the ramfs in kmain.c serves reads off. Paths are baked RELATIVE, exactly as the walk
# asks: prel tries lib/<x>.l off the cwd, so `use` finds these the moment `open` sits in
# defs[]. ⚠ the .list stamp is corpus.list's idiom -- a wildcard aggregate leaves every
# remaining prereq older than the target when a file is DELETED, and make bakes the ghost.
kfs = $(sort $(wildcard $R/lib/*.l))
force_kfs_list: ;
out/lib/kfs.list: force_kfs_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$(kfs)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
out/lib/kfs.h: $(kfs) out/lib/kfs.list $(love0) mk/tools/lcatfs.l love/prel.l
	@mkdir -p out/lib
	@echo LOVE	$@
	@$(love0) -l love/prel.l mk/tools/lcatfs.l $(kfs:$R/%=%) > $@

# --- the kore cat (rung 3) -------------------------------------------
# the whole $(korefiles) userland (crew/build.mk, included first) baked VERBATIM for the
# SHIPPED kernel only: kmain.c evals it at boot and the cmdline's program seat picks the
# tool. the K_TEST kernel skips it -- its corpus bakes the kore subset it drives.
out/lib/korecat.l: $(korefiles)
	@echo CAT	$@
	@mkdir -p out/lib
	@cat $(korefiles) > $@

# Shared C sources (core/love.c, crew/quay/, nolibc's six) + per-arch free/<a>/.
# Under K_TEST kmain.c #includes the baked corpus out/lib/ktests.h.
$(k_odir)/%.o: $(R)/%.c $(k_h) $(kcc_dep) out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/verbs.h out/lib/pat.h out/lib/uu.h out/lib/bao.h out/lib/kfs.h $(if $(K_TEST),out/lib/ktests.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h,out/lib/korecat.h out/lib/holo.h out/lib/x64.h out/lib/arm64.h out/lib/peg.h)
	@echo MOON	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

# kmain_o -- the kernel frontend, COMPILED AND NOTHING MORE, at whatever arch and face the
# caller's `a=` / `K_TEST=` say. test_embed asks for it three ways; the odir is spelled here
# so a caller never re-derives it (KCC decides half of it).
kmain_o: $(k_free_o)

# l.o carries the version string; recompile it when the id changes. ⚠ the -D is what MAKES
# it carry one -- mooncc has no __has_include for core/love.c's fallback probe, so without it
# the dep tracks a header the object cannot read and the kernel answers "unknown".
$(k_odir)/love.o: out/lib/love_version.h
$(k_odir)/love.o: kcppflags += -DAiHaveVersionH

# The two LAYS. holo's object writer (obj.l's objsecs) takes a list of NAMED sections --
# .boot, .note.pvh, the 2 KiB-aligned vector table, .bss -- which is what the kernel needs
# and a compiler never emits. ⚠ the cat joins the TARGET's backend text explicitly: a
# frontend bakes holo with the NATIVE one only, and this build must not care where it runs.
k_be_x86_64 = x64
k_be_aarch64 = arm64
klay_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/$(k_be_$a).l $R/crew/holo/elf.l $R/crew/holo/obj.l
# klink.l's shape, twice. ⚠ STATIC pattern, never an implicit one: a pattern-MADE
# prerequisite is an INTERMEDIATE make deletes after the link, and the cat would then run
# again on every build. naming the targets keeps them ordinary files.
$(k_odir)/mkvec.l $(k_odir)/mkboot.l: $(k_odir)/%.l: $R/free/%.l $(klay_l)
	@echo CAT	$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $R/crew/kore/text.l $R/crew/kore/u.l; \
	   echo "(use 'kore)"; cat $(filter-out $R/crew/kore/text.l $R/crew/kore/u.l,$(klay_l)) $<; } > $@

# `test -s`: an empty object is the failure this build cannot see -- it links, and the
# kernel boots into nothing.
$(k_lay_o): $(k_odir)/free/$a/%.o: $(k_odir)/mk%.l $m
	@echo HOLO	$@
	@mkdir -p "$(dir $@)"
	@$m -l $< -n -e '(lay-$* "$@" "$a")' && test -s $@

# --- qemu run targets ------------------------------------------------
# KVM where the host offers it: TCG costs 6x on the boot (22s to the prompt against
# 5s) and 7x on the corpus. A box without /dev/kvm falls to TCG and answers the same,
# which is what lets the GATES take it too (mk/tools/ktest.l, mk/tools/kboot.l, vec.sh).
# ⚠ x86_64-on-x86_64 only, not any arch match: qemu's arm `virt` is asked for
# gic-version=2 here, and a host whose GIC cannot back v2 REFUSES the pairing.
k_kvm = $(if $(and $(wildcard /dev/kvm),$(filter x86_64,$a),$(filter x86_64,$(shell uname -m))),-enable-kvm -cpu host,)
k_qemu_x86_64 = -M q35 -serial stdio
k_qemu_risc = -device ramfb -device qemu-xhci -device usb-kbd -device usb-mouse
k_qemu_aarch64 = -M virt,gic-version=2 -cpu cortex-a72 -serial stdio -semihosting $(k_qemu_risc)
k_qemu = qemu-system-$a -m 256M $(k_qemu_$a) $(k_kvm)
# ⚠ the FIRMWARE rides the ESP door ALONE. `qemu -kernel` enters our PVH stub with the
# machine bare; hand it OVMF as well and the firmware boots first and takes the door.
# mk/tools/ktest.l draws the same line, which is why its -kernel lane names no pflash.
k_fw = -drive if=pflash,unit=0,format=raw,file=$(dl)/edk2-ovmf/ovmf-code-$a.fd,readonly=on

# THE TWO DOORS, and they trade: our own BOOTX64.EFI hands over a framebuffer and
# carries no command line, `qemu -kernel` carries one (-append) and hands over no
# framebuffer -- PVH has nothing to hand. so `run` is the graphical one and
# `run-sh` the one that seats lush. ⚠ x86_64 only for the ESP: BOOTAA64.EFI is
# not ours to lay yet, so aarch64 takes the -kernel door for both.
ifeq ($a,x86_64)
run: run-$a
run-$a: $(ko)/esp/EFI/BOOT/BOOTX64.EFI $(ko)/esp/love.elf $(dl)/edk2-ovmf/ovmf-code-$a.fd
	exec $(k_qemu) $(k_fw) -drive format=raw,file=fat:rw:$(ko)/esp
else
run: run-$a
run-$a: $(k_elf)
	exec $(k_qemu) -kernel $<
endif
# the serial doors: no firmware, nothing downloaded, and a command line.
run-sh: $(k_elf)
	exec $(k_qemu) -kernel $< -append "sh"
run-headless: $(k_elf)
	exec $(k_qemu) -kernel $< -display none -no-reboot

# Boot init AS PID 1 in a container -- love at the Linux altitude of "the system". An
# unprivileged pid+user+mount namespace: --pid --fork makes the entrypoint pid 1, --user
# --map-root-user makes it root-in-ns so mount works, --mount-proc gives it a fresh /proc.
# love then IS init: getpid 1, mounts the early filesystems, reaps a reparented orphan.
# (pid1 0) is the deterministic tour, (perceive 0) the live signalfd supervisor.
init-container: host
	@command -v unshare >/dev/null || { echo "init-container: needs unshare (util-linux)"; exit 1; }
	@echo "-- love as PID 1 in a pid+user+mount namespace --"
	unshare --pid --fork --mount-proc --user --map-root-user -- $m -l crew/init/init.l -e "(pid1 0)"

# --- headless serial test (wired into test_slow; x86_64 + qemu only) ------------
# The K_TEST corpus: the host $t minus what this seat cannot run, plus the laws that can
# only run HERE. It bakes into out/lib/ktests.h and boots through the self-hosted ev,
# printing the usual summary over serial -- the freestanding kernel held to the same
# corpus test_host and test_love0 hold the host to. mk/tools/ktest.l drives it.
#
# Dropped: run.l wants host-OS nifs (subprocess), bell.l's Bell-number bignums are too
# heavy for an emulated kernel. Added, in order: ramfs.l (the baked initrd, which on the
# host would just be `open` on the real tree), fs.l and wfs.l (the writable tree), kore0.l
# then the kore cat then kore.l (the fs tools over the cat's own prefix), pipe.l (rung 4:
# pipes, the spawn/wait shim, the stdio seat), lush's engine parts in cat order as
# test/host/sh.l reads them (sh0.l pins what they mention and the seat lacks) and sh.l,
# rung 4's gate -- a real pipeline through sh-line -- then disk.l (rung 5: the virtio raw
# door + lib/fat.l on the real device, guarded on (disk ()) so a seat without one stays
# green) and svm.l (the AMD-V spike, guarded twice: the nom is x86_64-only and the
# silicon may be Intel's). zz-fin.l goes last: it prints the summary and quits.
kt = $(filter-out %/run.l %/bell.l %/zz-fin.l,$t) \
  $R/test/kernel/ramfs.l $R/test/kernel/fs.l $R/test/kernel/wfs.l \
  $R/test/kernel/kore0.l $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/core.l $R/crew/kore/fs.l \
  $R/test/kernel/kore.l $R/test/kernel/pipe.l \
  $R/test/kernel/sh0.l $R/crew/lush/job.l $R/crew/lush/lex.l $R/crew/lush/gram.l \
  $R/crew/lush/glob.l $R/crew/lush/word.l $R/crew/lush/eval.l $R/test/kernel/sh.l \
  $R/test/kernel/disk.l $R/test/kernel/svm.l $R/test/kernel/vmx.l \
  $R/test/zz-fin.l
# out/lib/corpus.list carries the MEMBERSHIP, rewritten only when the set changes
# (mk/lib.mk) -- so an edit to any makefile in the tree does not relay this header.
out/lib/ktests.list: force_dist_list
	@mkdir -p out/lib
	@tf=$@.$$$$.tmp; echo '$(kt)' > $$tf; \
	 if cmp -s $$tf $@ 2>/dev/null; then rm -f $$tf; else mv $$tf $@; echo SH	$@; fi
out/lib/ktests.l: $(kt) out/lib/corpus.list out/lib/ktests.list
	@echo CAT	$@
	@mkdir -p out/lib
	@cat $(kt) > $@
# the two VERBATIM bakes, one shape (lcatv, not lcat: an inspect-reprint diverges
# when the corpus is read back incrementally through a strin port).
out/lib/korecat.h out/lib/ktests.h: out/lib/%.h: out/lib/%.l $(love0) mk/tools/lcatv.l love/prel.l
	@echo LOVE	$@
	@$(love0) -l love/prel.l mk/tools/lcatv.l $< > $@

# arm64 EXECUTION validator: cross-build `love` for aarch64 and run the corpus under
# qemu-aarch64 -- test/holo/golden.l proves the byte encodings, this proves they run.
test_arm64: host
	@./mk/tools/arm64check.sh

# The x86_64 gate boots the ELF DIRECT: `qemu -kernel` reads the PVH note and enters our
# own bring-up -- page tables, GDT, long mode, kboot -- with NOTHING in dl/ involved.
ifeq ($a,x86_64)
test_kernel: host $(R)/mk/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test.elf
	@echo TEST $(ko)/love-$a-test.elf "(serial, headless, -kernel; ~60s, ceiling 420s)"
	@$m $(R)/mk/tools/ktest.l $(ko)/love-$a-test.elf - $a

# test_disk -- the rung-5 gate: write a file, RESET the machine, read it back. Two boots
# of the K_TEST kernel over one FRESH scratch image -- the first finds no filesystem and
# formats, the second must mount what the first wrote; ktest.l's 4th arg demands the kept
# line on top of the green summary.
test_disk: host $(R)/mk/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test.elf
	@rm -f $(ko)/love-$a-test.elf.disk
	@echo TEST $(ko)/love-$a-test.elf "(two boots, one disk: the reset-persistence gate)"
	@$m $(R)/mk/tools/ktest.l $(ko)/love-$a-test.elf - $a
	@$m $(R)/mk/tools/ktest.l $(ko)/love-$a-test.elf - $a "disk: fat kept across the reset"
	@echo "test_disk: the machine remembered"

# test_kboot -- inle rung 3's gate: the SHIPPED kernel (no K_TEST) booted direct with a
# boot command line, the baked kore cat dispatching off the program seat, running the tool
# and quitting through the reset door. Four boots at a cold cat eval each (~minutes under
# TCG), so OPT-IN -- run it when the kernel or the kore cat moves. vi
# stays the interactive smoke, under run-* -- `-append "vi lib/json.l"`.
test_kboot: host $(R)/mk/tools/kboot.l
	@$(MAKE) -s $(k_elf)
	@echo TEST $(k_elf) "(the kore cat off cmdline; 4 boots, ceiling 420s each)"
	@$m $(R)/mk/tools/kboot.l $(k_elf) "kore ls lib" "json.l"
	@$m $(R)/mk/tools/kboot.l $(k_elf) "kore wc lib/json.l" "lib/json.l" $$(wc -c < $(R)/lib/json.l)
	@$m $(R)/mk/tools/kboot.l $(k_elf) "sh -c \"cd lib; pwd\"" "/lib"
	@$m $(R)/mk/tools/kboot.l $(k_elf) "sh -c \"kore ls lib | kore wc -l\"" $$(ls $(R)/lib/*.l | wc -l)
else
test_kernel test_disk test_kboot:
	@echo "$@: skipped (host arch $a is not x86_64)"
endif

# --- the UEFI door: our own BOOTX64.EFI ------------------------------------
# No gnu-efi, no foreign toolchain, no bootloader we did not write: mooncc compiles the
# loader (loader.c reads love.elf off the ESP, fills kboot from the UEFI memmap + GOP,
# ExitBootServices, page tables, jumps kmain), mkefi.l lays the ms_abi<->SysV seam in holo
# IR, and holo's PE lane links the PE32+ the firmware runs. This is the LAPTOP door and the
# only one that hands over a framebuffer; the ESP is two files.
# ⚠ it carries NO command line -- `run-sh` is the -append door. adding one means a second
# file on the ESP for the loader to read, and nothing here reads one yet.
uefi_l = $R/crew/kore/text.l $R/crew/kore/u.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/crew/holo/pe.l \
  $R/free/uefi/mkefi.l
$(ko)/uefi$(ksuf)/loader.o: $R/free/uefi/loader.c $(ho)/love.baked
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(ho)/love mooncc -c $< $@
$(ko)/uefi$(ksuf)/BOOTX64.EFI: $(ko)/uefi$(ksuf)/loader.o $(uefi_l) $m
	@echo HOLO	$@
	@mkdir -p $(dir $@)
	@{ echo "(use 'holo)"; cat $(uefi_l); echo '(mkboot "$@" (list "$<"))'; } | $m
# the ESP: BOOTX64.EFI at the removable-media path the firmware looks for, and the kernel
# beside it (the loader opens "love.elf" on its own volume).
$(ko)/esp$(ksuf)/EFI/BOOT/BOOTX64.EFI: $(ko)/uefi$(ksuf)/BOOTX64.EFI
$(ko)/esp$(ksuf)/love.elf: $(ko)/love-x86_64$(ksuf).elf
$(ko)/esp$(ksuf)/EFI/BOOT/BOOTX64.EFI $(ko)/esp$(ksuf)/love.elf:
	@echo CP	$@
	@mkdir -p $(dir $@)
	@cp $< $@
uefi: $(ko)/esp/EFI/BOOT/BOOTX64.EFI $(ko)/esp/love.elf
	@echo "uefi: out/free/esp is an ESP -- copy it to a FAT32 partition, or"
	@echo "      qemu-system-x86_64 -drive format=raw,file=fat:rw:$(ko)/esp ..."

# test_uefi -- the whole laptop door under qemu, and the only gate that exercises the
# HAND-OVER (the loader off the ESP, kboot from the memmap, ExitBootServices, the jump);
# everything past it is the artifact test_kernel already gates. Gated on the firmware
# being PRESENT, never downloaded, since test_slow must fetch nothing -- `make
# dl/edk2-ovmf/ovmf-code-x86_64.fd` once and the lane starts running. ⚠ it is also the
# only gate handed a framebuffer, so kmain.c's fbdraw runs nowhere else: if it costs
# MINUTES where test_kernel does not, the console is repainting, not the door faulting.
OVMF_X64 := $(wildcard $(dl)/edk2-ovmf/ovmf-code-x86_64.fd)
ifeq ($(and $(filter x86_64,$a),$(OVMF_X64)),)
test_uefi:
	@echo "test_uefi: skipped (x86_64 + $(dl)/edk2-ovmf/ovmf-code-x86_64.fd needed)"
else
test_uefi: host $(R)/mk/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/esp-test/EFI/BOOT/BOOTX64.EFI $(ko)/esp-test/love.elf
	@echo TEST $(ko)/esp-test "(serial, headless, our own BOOTX64.EFI; ~64s, ceiling 420s)"
	@$m $(R)/mk/tools/ktest.l $(ko)/esp-test $(OVMF_X64) x86_64
endif

# test_inle -- the kernel's whole roster, in one word. Every lane below prints its own
# skip where the seat cannot run it (not x86_64, no qemu), so this is safe to type
# anywhere; cheapest first, so a break says so early. SEQUENTIAL sub-makes: as plain
# prerequisites a -j would land two of them in one object tree at once.
# ⚠ NOT on test_slow -- it is minutes of qemu, and the merge gate's subject is the seed.
# This is the gate to type when free/ or the kore cat moves.
test_inle:
	@$(MAKE) -s test_kernel
	@$(MAKE) -s test_disk
	@$(MAKE) -s test_uefi
	@$(MAKE) -s test_kboot
	@$(MAKE) -s test_kernel_arm64
	@echo "test_inle: boot, disk, command line -- both arches"

# The aarch64 twin of test_kernel, same corpus under full-TCG (~45s). In test_slow
# because the lane needs a gate that RUNS it: the aarch64 kernel is otherwise reached
# only by lanes test_kernel's x86_64 gate skips. Our own cc crosses by name (-t), so the
# only thing that can be missing is the emulator.
QEMU_A64 ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
ifeq ($(QEMU_A64),)
test_kernel_arm64:
	@echo "test_kernel_arm64: skipped (need qemu-system-aarch64)"
else
test_kernel_arm64: host $(R)/mk/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/love-aarch64-test.elf
	@echo TEST $(ko)/love-aarch64-test.elf "(serial, headless, TCG, -kernel; ~90s, ceiling 420s)"
	@$m $(R)/mk/tools/ktest.l $(ko)/love-aarch64-test.elf - aarch64
endif

# --- wasm headless test (wired into test_slow; emcc + node) -----------------
# Build love.js and run the SAME $t corpus through it under node -- a third runtime after
# the host and love0, exercising wasm's <data.h> override (sentinel-ap data kinds, no flat
# code-address space). The harness evals the corpus in one ai_eval and greps the drained
# output for the zz-fin summary, exactly as test_host greps `cat $t | love`.
# ⚠ it links OUT OF TREE (wasm's `gate` target -> out/wasm/love.js) and never over the
# committed wasm/love.js: a gate must not rewrite the working tree.
NODE ?= $(shell command -v node 2>/dev/null)
EMCC ?= $(or $(shell command -v emcc 2>/dev/null),/usr/lib/emscripten/emcc)
ifeq ($(and $(NODE),$(wildcard $(EMCC))),)
test_wasm:
	@echo "test_wasm: skipped (needs emcc + node)"
else
test_wasm:
	@$(MAKE) -s -C $(R)/wasm gate
	@echo TEST out/wasm/love.js "(node)"
	@$(NODE) $(R)/wasm/test.mjs --love $(R)/out/wasm/love.js $t
endif

# --- downloads -------------------------------------------------------
$(dl)/edk2-ovmf/ovmf-code-%.fd:
	@echo MK	ovmf
	@mkdir -p $(dl)
	@curl -L https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/edk2-ovmf.tar.gz | gunzip | tar -C $(dl) -xf -
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
	esac

