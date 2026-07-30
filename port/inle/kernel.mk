# port/inle/kernel.mk -- the freestanding kernel build, out/free. Was free/Makefile.
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk. Every recipe here is unchanged from the single-file Makefile.

# ====================================================================
# kernel (freestanding) build -- outputs under out/free. Was free/Makefile.
# The inle kernel lives in port/inle/: arch-independent glue is kmain.c + k.h
# there, per-arch code in port/inle/<a>/ (arch.c, *.S, *.lds). Each arch
# carries its own `qemu -kernel` bring-up (x86_64's PVH stub (mkboot.l), what
# test_kernel rides; aarch64's EL1 MMU stub (same file), what test_kernel_arm64
# rides -- no bootloader/firmware on either), and the Limine iso/hdd lanes
# below serve the interactive run-* targets (framebuffer console).
# ====================================================================
ko = out/free
dl = out/dl

# K_TEST=1 builds a headless serial test kernel (batch read-eval over COM1, with an
# `exit` nif that quits qemu) into its own odir / elf / iso, so it never clobbers the
# normal interactive kernel. See the test_kernel target below.
ifdef K_TEST
ksuf := -test
endif

# The COMPILER is ours (doc/moon-kernel.md rung 5): mooncc compiles every TU,
# holo lays the assembly and links, so nothing foreign is left in this build.
# KCC=clang is the comparison lane -- exactly like CC on the host side, and the
# differential the clang-shaped kernel exists to serve. A GCC cross toolchain
# also works:
#   make kernel a=aarch64 KCC=aarch64-linux-gnu-gcc KLINK=lld KLD=aarch64-linux-gnu-ld
# KLD serves the KLINK=lld lane only; the default link is ours (see below).
KCC ?= $(ho)/mooncc
KLD ?= ld.lld
KCC_IS_CLANG := $(shell $(KCC) --version 2>/dev/null | grep -qiw clang && echo 1)
# ours by NAME: mooncc is a wake shim over an image, so `--version` would have to
# boot it just to answer a makefile question at parse time.
KCC_IS_MOON := $(if $(findstring mooncc,$(KCC)),1,)

k_arch_c = $(wildcard $(R)/port/inle/$a/*.c)
# aarch64/builtins.c supplies __clear_cache and __udivti3 -- the two calls a
# FOREIGN compiler's codegen emits and then has to be handed somewhere. ours
# emits neither: it lowers __builtin___clear_cache to the dc/ic sequence inline
# (gen.l) and never reaches for a 128-bit divide, so its objects reference no
# such symbol. so the file belongs to the clang lane, and the moon lane drops
# it -- which is just as well, since it is written in __int128, a type we do
# not carry.
ifeq ($(KCC_IS_MOON),1)
k_arch_c := $(filter-out %/builtins.c,$(k_arch_c))
endif
k_free_c = $R/port/inle/kmain.c
k_shared_c = $(love_c) $(f_c) $(c_c)
k_h = $(love_h) $(wildcard *.h $(R)/port/inle/*.h $(R)/port/inle/$a/*.h)

# the object tree and the ELF are per COMPILER as well as per K_TEST. ⚠ they
# were not, and switching KCC therefore REUSED the other compiler's objects --
# so the clang lane, whose whole job is to be the differential twin, silently
# re-ran the mooncc artifact and reported it green. ours keeps the bare name
# (it is the default and the shipped one); a foreign cc gets its own tree.
KLINK ?= holo
kccsuf = $(if $(KCC_IS_MOON),,-$(notdir $(KCC)))
klsuf = $(if $(filter holo,$(KLINK)),,-$(KLINK))
kvsuf = $(kccsuf)$(klsuf)
k_odir = $(ko)/$a$(ksuf)$(kccsuf)
k_elf = $(ko)/love-$a$(ksuf)$(kvsuf).elf

k_shared_o = $(k_shared_c:$(R)/%.c=$(k_odir)/%.o)
k_arch_o = $(k_arch_c:$(R)/%.c=$(k_odir)/%.o)
k_free_o = $(k_free_c:$(R)/%.c=$(k_odir)/%.o)
# the two LAYS: what used to be four .S files (doc/moon-kernel.md rung 4).
# boot.o is the bring-up, vec.o the interrupt tail; both are holo IR written in
# love (port/inle/mk{boot,vec}.l), so no assembler runs in this build at all.
k_lay_o = $(k_odir)/port/inle/$a/boot.o $(k_odir)/port/inle/$a/vec.o
k_o = $(k_shared_o) $(k_arch_o) $(k_free_o) $(k_lay_o)

# The kernel runs the GENERATIONAL collector (the host default), BOUNDED by g->budget: kmain sums the
# boot memmap into kram_words and sets budget = kram_words/8 after ai_ini (the Appel knob). Without
# that bound the nursery's copy-overhead resizer grows unbounded and gen_major's worst-case (all-survive)
# sizing then asks kmallocw for a contiguous block bigger than the largest physical RAM range -> OOM.
# See gen_please (love.c) and the budget wiring (kmain.c).
kcflags = $(ai_cflags) -nostdinc -ffreestanding -fno-lto -fno-PIC \
  -ffunction-sections -fdata-sections
kldflags := -static -nostdlib --gc-sections -T $(R)/port/inle/$a/$a.lds -z max-page-size=0x1000
kcppflags := \
  -I$(k_odir) \
  -I. -I$(R)/out/host -Iout/lib -I$(R)/crew/quay -I$(R) -I$(R)/port/inle \
  -I$(R)/port/inle/$a \
  -I$(R)/crew/moon/include \
  -Ilibc \
  $(kcppflags) \
  -DLIMINE_API_REVISION=3
ifdef K_TEST
# tail-threaded (ai_tco=1, matching the real kernel + host). This build was long
# PINNED to tco=0 because it "hung" at tco=1 -- ROOT-CAUSED 2026-06-29 (gdb on the
# qemu gdbstub): not a hang but a #PF, the GC's terminator scan following a tag-2
# young-pointing terminator off the heap (gcp gets a terminator as a field because
# range-gated tagp missed it). The kmallocw layout triggered it; glibc/host didn't.
# Fixed by range-independent terminator recognition (tagl/in_live_pool in love.c), so
# the test gate now exercises tco=1 like everything else. love0 stays the trampoline lane.
kcppflags += -DK_TEST -Dai_tco=1
endif
ifeq ($(KCC_IS_CLANG),1)
kcc_if_clang = -target $a-unknown-none-elf
endif

# the machine flags a FOREIGN cc needs to be told. mooncc is told none of them:
# `-t` names the backend, and the whole -m* soup is VACUOUS for our codegen --
# probed before the flip (doc/moon-kernel.md). -mno-red-zone: gen.l allocates
# its frame before addressing a slot and every scratch cell pre-decrements, so
# nothing of ours ever lives below sp (1257 functions scanned, not one).
# -mcmodel=kernel: we emit abs64 and pc-relative relocations and NOTHING else,
# where clang's kernel objects carry 5792 32-bit absolutes -- so linking in the
# top 2 GiB needs no code model at all. ⚠ and mooncc REFUSES a -m flag rather
# than ignoring it (moon.l's tolerance list: dropping one silently would be the
# no-op wearing a cc face), so these must not reach it.
kcflags_x86_64 = -m64 -march=x86-64 -mabi=sysv -mno-red-zone -mcmodel=kernel
kcflags_aarch64 = -mcpu=generic -march=armv8-a
kcflags_mach = $(if $(KCC_IS_MOON),,$(kcflags_$a))
kcc_tgt = $(if $(KCC_IS_MOON),-t $(k_be_$a),$(kcc_if_clang))

kldflags_x86_64 = -m elf_x86_64
kldflags_aarch64 = -m aarch64elf

kcc = $(KCC) $(kcflags) $(kcflags_mach) $(kcppflags) $(kcc_tgt)
# ours has to exist before it can compile anything.
kcc_dep = $(if $(KCC_IS_MOON),$(ho)/mooncc,)

kernel: $(k_elf)

# The LINK is ours by default: holo's kernel lane (crew/holo/link.l's ldkern,
# driven by port/inle/klink.l) lays the same shape <a>.lds asks for -- the
# note, five page-aligned PT_LOADs, p_paddr = p_vaddr - bias, entry by symbol,
# kimage_end -- and all three doors (`qemu -kernel`, our BOOTX64.EFI, limine)
# boot the file it writes. KLINK=lld puts ld.lld and the .lds back, the
# comparison lane; it stays exact, and the .lds files stay in the tree as its
# statement of the layout. --gc-sections has no twin here (the image carries
# some dead code; it is RAM, and the kernel has plenty). (KLINK is set above --
# the variant suffix needs it before the first rule names a target.)
klink_l = $R/crew/kore/text.l $R/crew/kore/core.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/port/inle/klink.l
$(k_odir)/klink.l: $(klink_l)
	@echo AI	$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $(klink_l); } > $@

ifeq ($(KLINK),holo)
$(k_elf): $(k_odir)/klink.l $(k_o) $m
	@echo HOLO	$@
	@mkdir -p "$(dir $@)"
	@$m $(k_odir)/klink.l $@ $a $(k_o)
else
$(k_elf): $(R)/port/inle/$a/$a.lds $(k_o)
	@echo LD	$@
	@mkdir -p "$(dir $@)"
	@$(KLD) $(kldflags) $(k_o) -o $@
endif

# Shared C sources (love.c, crew/quay/, libc/) + per-arch port/inle/<a>/.
# Under K_TEST kmain.c #includes the baked corpus out/lib/ktests.h.
$(k_odir)/%.o: $(R)/%.c $(k_h) $(kcc_dep) out/lib/egg.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/uu.h out/lib/bao.h $(if $(K_TEST),out/lib/ktests.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h)
	@echo CC	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

# l.o carries the version string (love_version.h); recompile it when the id changes. The
# -D is what MAKES it carry one: $(KCC) defaults to mooncc, which has no __has_include for
# love.c's fallback probe, so without this the dep tracked a header the object could not
# read and the kernel answered "unknown".
$(k_odir)/love.o: out/lib/love_version.h
$(k_odir)/love.o: kcppflags += -DAI_HAVE_VERSION_H

# The two LAYS. holo's object writer (crew/holo/obj.l's objsecs) takes a list of
# NAMED sections, which is what the kernel needs and a compiler never emits --
# .boot, .note.pvh, the 2 KiB-aligned vector table, .bss. The cat joins the
# TARGET's backend text explicitly: a frontend bakes holo with the NATIVE one
# only, and this build must not care which machine it is running on.
k_be_x86_64 = x64
k_be_aarch64 = arm64
klay_l = $R/crew/kore/text.l $R/crew/kore/core.l $R/crew/kore/asbook.l \
  $R/crew/holo/$(k_be_$a).l $R/crew/holo/elf.l $R/crew/holo/obj.l
# klink.l's shape, twice: an explicit rule each rather than one pattern, so the
# cats are ordinary targets. a pattern-made prerequisite is an INTERMEDIATE make
# deletes after the link, which would re-cat them on every build.
$(k_odir)/mkvec.l: $R/port/inle/mkvec.l $(klay_l)
	@echo AI	$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $(klay_l) $<; } > $@
$(k_odir)/mkboot.l: $R/port/inle/mkboot.l $(klay_l)
	@echo AI	$@
	@mkdir -p "$(dir $@)"
	@{ echo "(use 'holo)"; cat $(klay_l) $<; } > $@

# `test -s`: an empty object is the failure this build cannot see -- it links,
# and the kernel boots into nothing.
$(k_odir)/port/inle/$a/vec.o: $(k_odir)/mkvec.l $m
	@echo LAY	$@
	@mkdir -p "$(dir $@)"
	@$m -l $< -n -e '(lay-vec "$@" "$a")' && test -s $@

$(k_odir)/port/inle/$a/boot.o: $(k_odir)/mkboot.l $m
	@echo LAY	$@
	@mkdir -p "$(dir $@)"
	@$m -l $< -n -e '(lay-boot "$@" "$a")' && test -s $@

# --- ISO / HDD image rules -------------------------------------------
k_xorriso_x86_64 = \
  -b boot/limine/limine-bios-cd.bin \
  -no-emul-boot -boot-load-size 4 -boot-info-table
k_xorriso = xorriso -as mkisofs -quiet -R -r -J \
  -hfsplus -apm-block-size 2048 \
  --efi-boot boot/limine/limine-uefi-cd.bin \
  -efi-boot-part --efi-boot-image --protective-msdos-label \
  $(k_xorriso_$a)

# The Limine bootloader config is generated here rather than kept as a
# standalone source file (it is four static lines).
$(ko)/limine.conf:
	@mkdir -p $(dir $@)
	@printf 'timeout: 1\n/gk\n    protocol: limine\n    path: boot():/boot/kernel\n' > $@

$(ko)/love-$a$(ksuf)$(kvsuf).iso: $(k_elf) $(dl)/limine/limine $(ko)/limine.conf
	@echo MK $@
	@rm -rf $(ko)/iso_root
	@mkdir -p $(ko)/iso_root/boot
	@cp $< $(ko)/iso_root/boot/kernel
	@mkdir -p $(ko)/iso_root/boot/limine
	@cp $(ko)/limine.conf $(ko)/iso_root/boot/limine/
	@mkdir -p $(ko)/iso_root/EFI/BOOT
	@cp $(dl)/limine/limine-uefi-cd.bin $(ko)/iso_root/boot/limine/
	@cp $(dl)/limine/limine-bios.sys $(dl)/limine/limine-bios-cd.bin $(ko)/iso_root/boot/limine/
	@cp $(dl)/limine/BOOTX64.EFI $(dl)/limine/BOOTIA32.EFI $(ko)/iso_root/EFI/BOOT/
	@cp $(dl)/limine/BOOTAA64.EFI $(ko)/iso_root/EFI/BOOT/
	$(k_xorriso) $(ko)/iso_root -o $@
	@$(dl)/limine/limine bios-install $@
	@rm -rf $(ko)/iso_root

$(ko)/love-$a.hdd: $(ko)/love-$a.elf $(dl)/limine/limine $(ko)/limine.conf
	@echo MK $@
	@rm -f $@
	@dd if=/dev/zero bs=1M count=0 seek=64 of=$@
	@PATH=$$PATH:/usr/sbin:/sbin sgdisk $@ -n 1:2048 -t 1:ef00
	@mformat -i $@@@1M
	@mmd -i $@@@1M ::/EFI ::/EFI/BOOT ::/boot ::/boot/limine
	@mcopy -i $@@@1M $< ::/boot/kernel
	@mcopy -i $@@@1M $(ko)/limine.conf ::/boot/limine
	@mcopy -i $@@@1M $(dl)/limine/limine-bios.sys ::/boot/limine
	@mcopy -i $@@@1M $(dl)/limine/BOOTX64.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M $(dl)/limine/BOOTIA32.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M $(dl)/limine/BOOTAA64.EFI ::/EFI/BOOT

# --- qemu run targets ------------------------------------------------
k_qemu_x86_64 = -M q35 -serial stdio
k_qemu_risc = -device ramfb -device qemu-xhci -device usb-kbd -device usb-mouse
k_qemu_aarch64 = -M virt,gic-version=2 -cpu cortex-a72 -serial stdio -semihosting $(k_qemu_risc)
k_qemu = qemu-system-$a -m 256M $(k_qemu_$a) \
  -drive if=pflash,unit=0,format=raw,file=$(dl)/edk2-ovmf/ovmf-code-$a.fd,readonly=on

.PHONY: run run-hdd run-$a run-hdd-$a run-headless
run: run-$a
run-hdd: run-hdd-$a
run-$a: $(ko)/love-$a.iso $(dl)/edk2-ovmf/ovmf-code-$a.fd
	exec $(k_qemu) -cdrom $<
run-hdd-$a: $(ko)/love-$a.hdd $(dl)/edk2-ovmf/ovmf-code-$a.fd
	exec $(k_qemu) -hda $<
run-headless: $(ko)/love-$a.iso $(dl)/edk2-ovmf/ovmf-code-$a.fd
	exec $(k_qemu) -cdrom $< -display none -no-reboot


# Boot init AS PID 1 in a container -- the Linux altitude of "love as the system".
# A private pid+user+mount namespace (unprivileged, no daemon/image/root): --pid
# --fork makes the entrypoint pid 1, --user --map-root-user makes it root-in-ns so
# mount works, --mount-proc gives it a fresh /proc reflecting the namespace. love then
# IS init: getpid 1, mounts the early filesystems, and reaps a reparented orphan
# (pid 1's defining duty). (pid1 0) is the deterministic tour; swap in (perceive 0)
# for the live signalfd supervisor. Needs unshare (util-linux) + unprivileged userns.
.PHONY: init-container
init-container: host
	@command -v unshare >/dev/null || { echo "init-container: needs unshare (util-linux)"; exit 1; }
	@echo "-- love as PID 1 in a pid+user+mount namespace --"
	unshare --pid --fork --mount-proc --user --map-root-user -- $m -l init/init.l -e "(pid1 0)"

# --- headless serial test (wired into test_slow; x86_64 + qemu only) ------------
# The K_TEST kernel boots, runs the baked corpus through the self-hosted ev, and
# PASSES (1708/1708 in ~2.5s). Two bugs were behind the long-parked hang:
#  (1) the cooperative scheduler deadlocked -- a task blocked in `(wait p)` was
#      saved by yield_sw parked on the kernel's serial input fd (a stale
#      next_wait_fd), so find_runnable never rescheduled it (fixed in love.c
#      lvm_wait: clear next_wake_at/next_wait_fd before yielding);
#  (2) five float-sqrt asserts failed because libc/math.c pow(x,0.5) used
#      exp(0.5*log x) (drifts a few ULP) instead of the exact Newton sqrt(), and
#      cos_k's Taylor ran a couple terms short at the pi/4 boundary.
#
# A K_TEST kernel bakes the test corpus in (out/lib/ktests.h, baked VERBATIM by
# tools/lcatv.l -- lcat's inspect-reprint diverges when the corpus is read back
# incrementally via a strin port) and runs it through the self-hosted ev at boot,
# printing the usual summary over the serial console, then quits qemu (the `exit`
# nif -> isa-debug-exit). tools/ktest.l (run on
# the host l) boots it under qemu headless, captures the serial output, and checks
# it. So this exercises the freestanding kernel the way test_host/test_love0 exercise
# the host. x86_64 only (qemu + isa-debug-exit); a no-op on other hosts.
#
# Drop from the kernel corpus: io.l (host file open) and run.l (subprocess/getenv)
# need host-OS nifs the kernel lacks; bell.l's Bell-number bignums are too heavy
# for the emulated kernel. (math.l REJOINED when the math floor became am.c --
# the same <= 2 ulp seven everywhere, so the glibc-precision bands hold.)
kt = $(filter-out %/io.l %/run.l %/bell.l,$t)
# out/lib/corpus.list carries the MEMBERSHIP (mk/lib.mk: regenerated every make, rewritten
# only when the set changes), which is the whole job $(MAKEFILE_LIST) used to do here -- and
# it did it by re-laying this header, and so rebuilding all eleven kernel objects, on any
# edit to any makefile in the tree.
out/lib/ktests.l: $(kt) out/lib/corpus.list
	@mkdir -p out/lib
	@cat $(kt) > $@
out/lib/ktests.h: out/lib/ktests.l $(love0) tools/lcatv.l love/prel.l
	@echo AI	$@
	@$(love0) -l love/prel.l tools/lcatv.l out/lib/ktests.l > $@
# arm64 EXECUTION validator: cross-build `love` for aarch64 + run the corpus under
# qemu-aarch64 (the trustworthy check for the glaze's second target -- holotest
# proves byte encodings, this proves they run). No-ops without qemu + a cross-gcc.
.PHONY: test_arm64
test_arm64: host
	@./tools/arm64check.sh

# The x86_64 gate boots the ELF DIRECT: `qemu -kernel` reads the PVH ELF note
# and enters our own bring-up (mkboot.l) (page tables, GDT, long mode, kboot) -- no
# limine, no OVMF, no iso, NOTHING in out/dl. The limine/firmware machinery
# above stays for the interactive run-* lanes (they want the framebuffer
# console only a real bootloader hands over) and for test_kernel_arm64.
.PHONY: test_kernel
ifeq ($a,x86_64)
test_kernel: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test$(kvsuf).elf
	@echo TEST $(ko)/love-$a-test$(kvsuf).elf "(serial, headless, -kernel; ~60s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test$(kvsuf).elf - $a
else
test_kernel:
	@echo "test_kernel: skipped (host arch $a is not x86_64)"
endif

# --- the UEFI door: our own BOOTX64.EFI ------------------------------------
# No limine, no gnu-efi, no foreign toolchain: mooncc compiles the loader
# (port/inle/uefi/loader.c -- read love.elf off the ESP, fill kboot from the
# UEFI memmap + GOP framebuffer, ExitBootServices, page tables, jump kmain),
# mkefi.l lays the ms_abi<->SysV seam in holo IR, and holo's PE lane
# (crew/holo/pe.l) links the PE32+ the firmware runs. The ESP is two files.
# This is the LAPTOP door -- the one that replaces limine on real hardware.
uefi_l = $R/crew/kore/text.l $R/crew/kore/core.l $R/crew/kore/asbook.l \
  $R/crew/holo/elf.l $R/crew/holo/obj.l $R/crew/holo/link.l $R/crew/holo/pe.l \
  $R/port/inle/uefi/mkefi.l
$(ko)/uefi$(ksuf)/loader.o: $R/port/inle/uefi/loader.c $(ho)/mooncc
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(ho)/mooncc -c $< $@
$(ko)/uefi$(ksuf)/BOOTX64.EFI: $(ko)/uefi$(ksuf)/loader.o $(uefi_l) $m
	@echo PE	$@
	@mkdir -p $(dir $@)
	@{ echo "(use 'holo)"; cat $(uefi_l); echo '(mkboot "$@" (list "$<"))'; } | $m
# the ESP: BOOTX64.EFI at the removable-media path the firmware looks for, and
# the kernel beside it (the loader opens "love.elf" on its own volume).
$(ko)/esp$(ksuf)/EFI/BOOT/BOOTX64.EFI: $(ko)/uefi$(ksuf)/BOOTX64.EFI
	@echo CP	$@
	@mkdir -p $(dir $@)
	@cp $< $@
$(ko)/esp$(ksuf)/love.elf: $(ko)/love-x86_64$(ksuf)$(kvsuf).elf
	@echo CP	$@
	@mkdir -p $(dir $@)
	@cp $< $@
.PHONY: uefi
uefi: $(ko)/esp/EFI/BOOT/BOOTX64.EFI $(ko)/esp/love.elf
	@echo "uefi: out/free/esp is an ESP -- copy it to a FAT32 partition, or"
	@echo "      qemu-system-x86_64 -drive format=raw,file=fat:rw:$(ko)/esp ..."

# test_uefi -- the whole laptop door under qemu: OUR BOOTX64.EFI loads the
# K_TEST kernel, the corpus runs over serial. Needs firmware (any OVMF build);
# gated on the file being PRESENT so the gate never downloads -- rung 4's whole
# point is that `make test_slow` fetches nothing. Fetch it once by hand with
# `make out/dl/edk2-ovmf/ovmf-code-x86_64.fd` and this lane starts running.
#
# In test_slow, at ~64 s -- the same as the -kernel door. What is only HERE is the
# hand-over: the loader reading love.elf off the ESP, kboot filled from the UEFI memmap +
# GOP, ExitBootServices, page tables, the jump. Everything past it is the artifact
# test_kernel already gates. ⚠ this is also the ONLY gate that hands over a framebuffer,
# so kmain.c's fbdraw runs nowhere else -- if this lane costs MINUTES while test_kernel
# does not, the console is repainting more than it was asked to, not the door faulting.
OVMF_X64 := $(wildcard $(dl)/edk2-ovmf/ovmf-code-x86_64.fd)
.PHONY: test_uefi
ifeq ($(and $(filter x86_64,$a),$(OVMF_X64)),)
test_uefi:
	@echo "test_uefi: skipped (x86_64 + $(dl)/edk2-ovmf/ovmf-code-x86_64.fd needed)"
else
test_uefi: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/esp-test/EFI/BOOT/BOOTX64.EFI $(ko)/esp-test/love.elf
	@echo TEST $(ko)/esp-test "(serial, headless, our own BOOTX64.EFI; ~64s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/esp-test $(OVMF_X64) x86_64
endif

# test_kdiff -- the clang-vs-mooncc K_TEST DIFFERENTIAL (moon-kernel rung 5).
# The kernel is ours now, so clang's only remaining job in this tree is to be
# the twin: a second compiler over the same sources, so that when the kernel
# breaks you can ask whether it broke in the code or in our codegen. A twin
# nothing runs is a twin that rots, so this boots it -- the same corpus through
# the same three doors, out of its own object tree (kccsuf, without which the
# two lanes silently shared objects and this compared nothing).
# ~45s per arch on top of test_kernel, so it is OPT-IN, not in test_slow --
# test_kernel already gates the artifact we ship. Run it when the kernel moves.
.PHONY: test_kdiff
ifneq ($(shell command -v clang 2>/dev/null),)
test_kdiff:
	@$(MAKE) -s KCC=clang test_kernel
	@$(MAKE) -s KCC=clang test_kernel_arm64
	@echo "test_kdiff: the same corpus, both compilers, both arches"
else
test_kdiff:
	@echo "test_kdiff: skipped (no clang)"
endif

# The aarch64 twin of test_kernel: cross-build the K_TEST kernel and run the same
# corpus under full-TCG qemu-system-aarch64 (~45s). In test_slow because the lane
# needs a gate that RUNS it -- the aarch64 kernel silently stopped LINKING once,
# and nothing caught it precisely because test_kernel is x86_64-gated.
# Needs qemu-system-aarch64 and a CROSS-CAPABLE $(KCC) -- ours (which names the
# backend with -t, and is the default) or clang (-target $a-unknown-none-elf).
# A native gcc cannot, so that lane still skips. No-op without either (so a
# plain `make test_slow` stays green on a host lacking them), like test_wasm.
QEMU_A64 ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
.PHONY: test_kernel_arm64
ifeq ($(and $(QEMU_A64),$(or $(KCC_IS_MOON),$(filter 1,$(KCC_IS_CLANG)))),)
test_kernel_arm64:
	@echo "test_kernel_arm64: skipped (need qemu-system-aarch64 + a cross-capable KCC)"
else
test_kernel_arm64: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/love-aarch64-test$(kvsuf).elf
	@echo TEST $(ko)/love-aarch64-test$(kvsuf).elf "(serial, headless, TCG, -kernel; ~90s, ceiling 420s)"
	@$m $(R)/tools/ktest.l $(ko)/love-aarch64-test$(kvsuf).elf - aarch64
endif

# --- wasm headless test (wired into test_slow; emcc + node) -----------------
# Build love.js and run the SAME $t corpus through it under node -- a third
# runtime after the host and love0, exercising wasm's <data.h> override
# (sentinel-ap data kinds, no flat code-address space). The harness evals the
# whole corpus in one ai_eval and greps the drained output for the zz-fin
# summary, exactly as test_host greps `cat $t | love`. No-op when emcc or node
# is missing (so a plain `make test_slow` stays green on a host without them).
NODE ?= $(shell command -v node 2>/dev/null)
EMCC ?= $(or $(shell command -v emcc 2>/dev/null),/usr/lib/emscripten/emcc)
.PHONY: test_wasm
ifeq ($(and $(NODE),$(wildcard $(EMCC))),)
test_wasm:
	@echo "test_wasm: skipped (needs emcc + node)"
else
test_wasm: wasm
	@echo TEST wasm/love.js "(node)"
	@$(NODE) $(R)/wasm/test.mjs $t
endif

# --- downloads -------------------------------------------------------
$(dl)/edk2-ovmf/ovmf-code-%.fd:
	@echo MK ovmf
	@mkdir -p $(dl)
	@curl -L https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/edk2-ovmf.tar.gz | gunzip | tar -C $(dl) -xf -
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
	esac

$(dl)/limine/limine:
	@echo MK limine
	@rm -rf $(dl)/limine
	@git clone https://codeberg.org/Limine/Limine.git $(dl)/limine --branch=v10.x-binary --depth=1 > /dev/null 2>&1
	@$(MAKE) -sC $(dl)/limine

