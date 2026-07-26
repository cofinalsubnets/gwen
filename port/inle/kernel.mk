# port/inle/kernel.mk -- the freestanding kernel build, out/free. Was free/Makefile.
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk. Every recipe here is unchanged from the single-file Makefile.

# ====================================================================
# kernel (freestanding) build -- outputs under out/free. Was free/Makefile.
# The inle kernel lives in port/inle/: arch-independent glue is kmain.c + k.h
# there, per-arch code in port/inle/<a>/ (arch.c, *.S, *.lds). Boots via Limine.
# ====================================================================
ko = out/free
dl = out/dl

# K_TEST=1 builds a headless serial test kernel (batch read-eval over COM1, with an
# `exit` nif that quits qemu) into its own odir / elf / iso, so it never clobbers the
# normal interactive kernel. See the test_kernel target below.
ifdef K_TEST
ksuf := -test
endif

# Cross toolchain defaults to clang + lld (one multi-target pair covers every
# arch). Override for a GCC cross toolchain, e.g.
#   make kernel a=aarch64 KCC=aarch64-linux-gnu-gcc KLD=aarch64-linux-gnu-ld
KCC ?= clang
KLD ?= ld.lld
KCC_IS_CLANG := $(shell $(KCC) --version 2>/dev/null | grep -qiw clang && echo 1)

k_arch_c = $(wildcard $(R)/port/inle/$a/*.c)
k_asm = $(wildcard $(R)/port/inle/$a/*.asm)
k_free_c = $R/port/inle/kmain.c
k_shared_c = $(love_c) $(f_c) $(c_c)
k_S = $(wildcard $(R)/port/inle/$a/*.S)
k_h = $(love_h) $(wildcard *.h $(R)/port/inle/*.h $(R)/port/inle/$a/*.h)

k_odir = $(ko)/$a$(ksuf)

k_shared_o = $(k_shared_c:$(R)/%.c=$(k_odir)/%.o)
k_arch_o = $(k_arch_c:$(R)/%.c=$(k_odir)/%.o)
k_free_o = $(k_free_c:$(R)/%.c=$(k_odir)/%.o)
k_S_o = $(k_S:$(R)/%.S=$(k_odir)/%.o)
k_asm_o = $(k_asm:$(R)/%.asm=$(k_odir)/%.o)
k_o = $(k_shared_o) $(k_arch_o) $(k_free_o) $(k_S_o) $(k_asm_o)

# The kernel runs the GENERATIONAL collector (the host default), BOUNDED by g->budget: kmain sums the
# limine memmap into kram_words and sets budget = kram_words/8 after ai_ini (the Appel knob). Without
# that bound the nursery's copy-overhead resizer grows unbounded and gen_major's worst-case (all-survive)
# sizing then asks kmallocw for a contiguous block bigger than the largest physical RAM range -> OOM.
# See gen_please (love.c) and the budget wiring (kmain.c).
kcflags = $(ai_cflags) -nostdinc -ffreestanding -fno-lto -fno-PIC \
  -ffunction-sections -fdata-sections
kldflags := -static -nostdlib --gc-sections -T $(R)/port/inle/$a/$a.lds -z max-page-size=0x1000
kcppflags := \
  -I$(k_odir) \
  -I. -I$(R)/out/host -Iout/lib -I$(R)/crew/quay -I$(R) -I$(R)/port/inle \
  -Ilibc \
  -isystem c \
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

kcflags_x86_64 = -m64 -march=x86-64 -mabi=sysv -mno-red-zone -mcmodel=kernel
kcflags_aarch64 = -mcpu=generic -march=armv8-a

kldflags_x86_64 = -m elf_x86_64
kldflags_aarch64 = -m aarch64elf

kcc = $(KCC) $(kcflags) $(kcflags_$a) $(kcppflags) $(kcc_if_clang)
k_nasmflags := -f elf64 -g -F dwarf -Wall -w-reloc-abs-qword -w-reloc-abs-dword -w-reloc-rel-dword

kernel: $(ko)/love-$a$(ksuf).elf

$(ko)/love-$a$(ksuf).elf: $(R)/port/inle/$a/$a.lds $(k_o)
	@echo LD	$@
	@mkdir -p "$(dir $@)"
	@$(KLD) $(kldflags) $(k_o) -o $@

# Shared C sources (love.c, crew/quay/, libc/) + per-arch port/inle/<a>/.
# Under K_TEST kmain.c #includes the baked corpus out/lib/ktests.h.
$(k_odir)/%.o: $(R)/%.c $(k_h) out/lib/egg.h out/lib/prel.h out/lib/ev.h out/lib/uu.h out/lib/bao.h $(if $(K_TEST),out/lib/ktests.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h)
	@echo CC	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

# l.o carries the version string (love_version.h); recompile it when the id changes.
$(k_odir)/love.o: out/lib/love_version.h

$(k_odir)/%.o: $(R)/%.S $(k_h)
	@echo AS	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

$(k_odir)/%.o: $(R)/%.asm $(k_h)
	@echo AS	$@
	@mkdir -p "$(dir $@)"
	@nasm $< -o $@ $(k_nasmflags)

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

$(ko)/love-$a$(ksuf).iso: $(ko)/love-$a$(ksuf).elf $(dl)/limine/limine $(ko)/limine.conf
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

# --- headless serial test (wired into test_all; x86_64 + qemu only) ------------
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
out/lib/ktests.l: $(kt) $(MAKEFILE_LIST) out/lib/corpus.list
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

.PHONY: test_kernel
ifeq ($a,x86_64)
test_kernel: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 $(ko)/love-$a-test.iso $(dl)/edk2-ovmf/ovmf-code-$a.fd
	@echo TEST $(ko)/love-$a-test.iso "(serial, headless)"
	@$m $(R)/tools/ktest.l $(ko)/love-$a-test.iso $(dl)/edk2-ovmf/ovmf-code-$a.fd $a
else
test_kernel:
	@echo "test_kernel: skipped (host arch $a is not x86_64)"
endif

# The aarch64 twin of test_kernel: cross-build the K_TEST kernel and run the same
# corpus under full-TCG qemu-system-aarch64 (~45s). In test_all because the lane
# needs a gate that RUNS it -- the aarch64 kernel silently stopped LINKING once,
# and nothing caught it precisely because test_kernel is x86_64-gated.
# Needs qemu-system-aarch64 and a CLANG $(KCC): the cross target comes from
# -target $a-unknown-none-elf, which a native gcc cannot do. No-op without either
# (so a plain `make test_all` stays green on a host lacking them), like test_wasm.
QEMU_A64 ?= $(shell command -v qemu-system-aarch64 2>/dev/null)
.PHONY: test_kernel_arm64
ifeq ($(and $(QEMU_A64),$(filter 1,$(KCC_IS_CLANG))),)
test_kernel_arm64:
	@echo "test_kernel_arm64: skipped (need qemu-system-aarch64 + a clang KCC)"
else
test_kernel_arm64: host $(R)/tools/ktest.l
	@$(MAKE) -s K_TEST=1 a=aarch64 $(ko)/love-aarch64-test.iso $(dl)/edk2-ovmf/ovmf-code-aarch64.fd
	@echo TEST $(ko)/love-aarch64-test.iso "(serial, headless, TCG)"
	@$m $(R)/tools/ktest.l $(ko)/love-aarch64-test.iso $(dl)/edk2-ovmf/ovmf-code-aarch64.fd aarch64
endif

# --- wasm headless test (wired into test_all; emcc + node) -----------------
# Build love.js and run the SAME $t corpus through it under node -- a third
# runtime after the host and love0, exercising wasm's <data.h> override
# (sentinel-ap data kinds, no flat code-address space). The harness evals the
# whole corpus in one ai_eval and greps the drained output for the zz-fin
# summary, exactly as test_host greps `cat $t | love`. No-op when emcc or node
# is missing (so a plain `make test_all` stays green on a host without them).
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

