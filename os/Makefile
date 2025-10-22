.SUFFIXES:
n=mitty
a=$(shell uname -m)
ARCH=$a
override IMAGE_NAME := $n-$a
.PHONY: all
all: $n-$a.iso
.PHONY: all-hdd
all-hdd: $n-$a.hdd


$n-$a.iso: limine/limine kernel
	@echo MK $@
	@rm -rf iso_root
	@mkdir -p iso_root/boot
	@cp kernel/kernel.$a iso_root/boot/kernel
	@mkdir -p iso_root/boot/limine
	@cp limine.conf iso_root/boot/limine/
	@mkdir -p iso_root/EFI/BOOT
	@cp limine/limine-uefi-cd.bin iso_root/boot/limine/
ifeq ($a,x86_64)
	@cp limine/limine-bios.sys limine/limine-bios-cd.bin iso_root/boot/limine/
	@cp limine/BOOTX64.EFI limine/BOOTIA32.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -quiet -R -r -J -b boot/limine/limine-bios-cd.bin \
		-no-emul-boot -boot-load-size 4 -boot-info-table -hfsplus \
		-apm-block-size 2048 --efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
	@./limine/limine bios-install $@
endif
ifeq ($a,aarch64)
	@cp -v limine/BOOTAA64.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -quiet -R -r -J \
		-hfsplus -apm-block-size 2048 \
		--efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
endif
ifeq ($a,riscv64)
	@cp -v limine/BOOTRISCV64.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -R -r -J \
		-hfsplus -apm-block-size 2048 \
		--efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
endif
ifeq ($a,loongarch64)
	@cp -v limine/BOOTLOONGARCH64.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -R -r -J \
		-hfsplus -apm-block-size 2048 \
		--efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
endif
	@rm -rf iso_root

$n-$a.hdd: limine/limine kernel
	@echo MK $@
	@rm -f $@
	@dd if=/dev/zero bs=1M count=0 seek=64 of=$@
ifeq ($a,x86_64)
	@PATH=$$PATH:/usr/sbin:/sbin sgdisk $@ -n 1:2048 -t 1:ef00 -m 1
	@./limine/limine bios-install $@ 2>&1 >/dev/null
else
	@PATH=$$PATH:/usr/sbin:/sbin sgdisk $@ -n 1:2048 -t 1:ef00
endif
	@mformat -i $@@@1M
	@mmd -i $@@@1M ::/EFI ::/EFI/BOOT ::/boot ::/boot/limine
	@mcopy -i $@@@1M kernel/kernel.$a ::/boot/kernel
	@mcopy -i $@@@1M limine.conf ::/boot/limine
ifeq ($a,x86_64)
	@mcopy -i $@@@1M limine/limine-bios.sys ::/boot/limine
	@mcopy -i $@@@1M limine/BOOTX64.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M limine/BOOTIA32.EFI ::/EFI/BOOT
endif
ifeq ($a,aarch64)
	@mcopy -i $@@@1M limine/BOOTAA64.EFI ::/EFI/BOOT
endif
ifeq ($a,riscv64)
	@mcopy -i $@@@1M limine/BOOTRISCV64.EFI ::/EFI/BOOT
endif
ifeq ($a,loongarch64)
	@mcopy -i $@@@1M limine/BOOTLOONGARCH64.EFI ::/EFI/BOOT
endif

.PHONY: kernel
kernel:
	@make -C kernel

.PHONY: clean
clean:
	@make -C kernel clean
	@rm -rf iso_root $n-$a.iso $n-$a.hdd

.PHONY: distclean
distclean:
	@echo DISTCLEAN
	@make -C kernel distclean
	@rm -rf iso_root *.iso *.hdd kernel-deps limine ovmf

.PHONY: run run-hdd
run: run-$a
run-hdd: run-hdd-$a
QEMUFLAGS := -m 2G
qemu-x86_64=qemu-system-x86_64 $(QEMUFLAGS)\
	-M q35\
	-drive if=pflash,unit=0,format=raw,file=ovmf/ovmf-code-x86_64.fd,readonly=on
.PHONY: run-x86_64 run-hdd-x86_64
run-x86_64: ovmf/ovmf-code-$a.fd $n-$a.iso
	$(qemu-x86_64) -cdrom $n-$a.iso
run-hdd-x86_64: ovmf/ovmf-code-$a.fd $n-$a.hdd
	$(qemu-x86_64) -hda $n-$a.hdd

qemu-aarch64=qemu-system-aarch64 $(QEMUFLAGS)\
		-M virt \
		-cpu cortex-a72 \
		-device ramfb \
		-device qemu-xhci \
		-device usb-kbd \
		-device usb-mouse \
		-drive if=pflash,unit=0,format=raw,file=ovmf/ovmf-code-aarch64.fd,readonly=on

.PHONY: run-aarch64 run-hdd-aarch64
run-aarch64: ovmf/ovmf-code-aarch64.fd $n-aarch64.iso
	$(qemu-aarch64) -cdrom $n-aarch64.iso
run-hdd-aarch64: ovmf/ovmf-code-aarch64.fd $n-aarch64.hdd
	$(qemu-aarch64) -hda $n-aarch64.hdd

qemu-riscv64=qemu-system-riscv64 $(QEMUFLAGS)\
	-M virt \
	-cpu rv64 \
	-device ramfb \
	-device qemu-xhci \
	-device usb-kbd \
	-device usb-mouse \
	-drive if=pflash,unit=0,format=raw,file=ovmf/ovmf-code-riscv64.fd,readonly=on

.PHONY: run-riscv64 run-hdd-riscv64
run-riscv64: ovmf/ovmf-code-riscv64.fd $n-riscv64.iso
	$(qemu-riscv64) -cdrom $n-riscv64.iso
run-hdd-riscv64: ovmf/ovmf-code-riscv64.fd $n-riscv64.hdd
	$(qemu-riscv64) -hda $n-riscv64.hdd

qemu-loongarch64=qemu-system-loongarch64 $(QEMUFLAGS)\
	-M virt \
	-cpu la464 \
	-device ramfb \
	-device qemu-xhci \
	-device usb-kbd \
	-device usb-mouse \
	-drive if=pflash,unit=0,format=raw,file=ovmf/ovmf-code-loongarch64.fd,readonly=on

.PHONY: run-loongarch64 run-hdd-loongarch64
run-loongarch64: ovmf/ovmf-code-loongarch64.fd $n-loongarch64.iso
	$(qemu-loongarch64) -cdrom $n-$a.iso

run-hdd-loongarch64: ovmf/ovmf-code-loongarch64.fd $n-loongarch64.hdd
	$(qemu-loongarch64) -hda $n-$a.hdd

ovmf/ovmf-code-%.fd:
	@echo MK ovmf
	@mkdir -p ovmf
	@curl -sLo $@ https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/$(notdir $@)
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
		riscv64) dd if=/dev/zero of=$@ bs=1 count=0 seek=33554432 2>/dev/null;; \
	esac

# Toolchain for building the 'limine' executable for the host.
git_clone_limine=git clone\
	https://codeberg.org/Limine/Limine.git\
 	limine --branch=v10.x-binary --depth=1 > /dev/null 2>&1
limine/limine:
	@echo MK limine
	@rm -rf limine
	@$(git_clone_limine)
	@make -sC limine
