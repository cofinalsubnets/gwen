n=mitty
x=mi
m=bin/host/$n
l=bin/host/lcat
.PHONY: test
test: bin/host/$n
	@echo MI test
	@$m test/*.$x

#build
target ?= host
#build
# c files and headers
share_h=$(wildcard share/*.h)
share_c=$(wildcard share/*.c)
font_c=$(wildcard share/font/*.c)
font_h=$(wildcard share/font/*.h)
host_o=$(addprefix bin/host/, $(share_c:.c=.o) sys.o)
a=bin/host/$n.a
so=bin/host/lib$n.so
flags:= -std=gnu17 -g -O2 -pipe\
 	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
 	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-flto -fpic
cc=$(CC) $(flags) -Ishare -Ihost -Ibin\
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)

bin/host/$n: bin/host/main.o bin/host/$n.a
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^

bin/host/lcat: bin/host/lcat.o bin/host/$n.a
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^

bin/host/$n.a: $(host_o)
	@echo AR $@
	@mkdir -p $(dir $@)
	@ar rcs $@ $^

bin/host/lib$n.so: $(host_o)
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -shared -o $@ $^

bin/host/%.o: %.c $(share_h) Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c $< -o $@

bin/host/%.o: host/%.c $(share_h) Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c -Ibin/host $< -o $@

bin/host/main.o: host/main.c bin/host/main.h bin/boot.h $(share_h) Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c -Ibin/host $< -o $@

#.c.o:
#	@echo $@
#	@$(cc) -c $< -o $@

# sed command to escape lisp text into C string format
bin/boot.h: bin/host/lcat host/lcat.sed share/boot.$x
	@echo LC $@
	@$l < share/boot.$x | sed -f host/lcat.sed >$@

bin/host/main.h: bin/host/lcat host/lcat.sed host/main.$x
	@echo LC $@
	@$l < host/main.$x | sed -f host/lcat.sed > $@

bin/host/$n.1: bin/host/$n host/manpage.$x
	@echo MI $@
	@$m host/manpage.$x > $@


# installlation
# default install to home directory under ~/.local/
PREFIX ?= .local/
VIMPREFIX ?= .vim/
DESTDIR ?= $(HOME)/
dest=$(DESTDIR)/$(PREFIX)/
vimdest=$(DESTDIR)/$(VIMPREFIX)/
installs=\
 	$(dest)/bin/$n\
  $(dest)/share/man/man1/$n.1\
  $(dest)/lib/$n.a\
  $(dest)/lib/lib$n.so\
  $(dest)/include/$n.h\
  $(vimdest)/ftdetect/$n.vim\
  $(vimdest)/syntax/$n.vim
install: $(installs)
uninstall:
	@echo RM $(abspath $(installs))
	@rm -f $(installs)

$(dest)/include/$n.h: share/$n.h
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(dest)/lib/$n.a: bin/host/$n.a
	@echo IN $(abspath $@)
	@install -D -m 755 $< $@
$(dest)/lib/lib$n.so: bin/host/lib$n.so
	@echo IN $(abspath $@)
	@install -D -m 755 $< $@
$(dest)/bin/$n: bin/host/$n
	@echo IN $(abspath $@)
	@install -D -m 755 -s $< $@
$(dest)/share/man/man1/$n.1: bin/host/$n.1
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/ftdetect/$n.vim: vim/ftdetect/$n.vim
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/syntax/$n.vim: vim/syntax/$n.vim
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@

clean:
	@echo CLEAN
	@rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: bin/host/$n
	valgrind --error-exitcode=1 $m test/*.$x
# profiling on linux
perf.data: bin/host/$n
	perf record $m test/*.$x
perf: perf.data
	perf report
flamegraph.svg: perf.data
	flamegraph --perfdata $^
repl: bin/host/$n
	rlwrap $m
serve:
	darkhttpd .


ARCH=$(shell uname -m)
a=$(ARCH)
k_c=$(share_c) $(font_c) $(wildcard k/libc/*.c) $(wildcard k/src/*.c) $(wildcard k/src/$a/*.c)
k_h=$(share_h) $(font_h) $(wildcard k/libc/*.h) $(wildcard k/src/*.h) $(wildcard k/src/$a/*.h)
k_S=$(wildcard k/src/*.S) $(wildcard k/src/$a/*.S)
k_asm=$(wildcard k/src/*.asm) $(wildcard k/src/$a/*.asm)
k_o=$(addprefix bin/k_$a/, $(k_c:.c=.o) $(k_S:.S=.o) $(k_asm:.asm=.o))

NASMFLAGS := -g -F dwarf -Wall -w-reloc-abs-qword -w-reloc-abs-dword -w-reloc-rel-dword
kldflags := -static -nostdlib --gc-sections -T k/$a.lds -z max-page-size=0x1000
kcflags:=-std=gnu17 -g -O2 -pipe\
	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-nostdinc -ffreestanding -fno-lto -fno-PIC -ffunction-sections -fdata-sections
kcppflags := \
    -I k/src \
		-I bin \
		-I share \
		-I k/libc \
		-I share/font \
		-Dg_target=g_target_os \
    -isystem k/libc \
    $(kcppflags) \
    -DLIMINE_API_REVISION=3

kcc=$(CC) $(kcflags) $(kcppflags)
# Architecture specific internal flags.
ifeq ($(ARCH),x86_64)
    ifeq ($(CC_IS_CLANG),1)
        override kcc += \
            -target x86_64-unknown-none-elf
    endif
    override kcflags += \
        -m64 \
        -march=x86-64 \
        -mabi=sysv \
        -mno-80387 \
        -mno-mmx \
        -mno-sse \
        -mno-sse2 \
        -mno-red-zone \
        -mcmodel=kernel
    override kldflags += \
        -m elf_x86_64
    override NASMFLAGS := \
        -f elf64 \
        $(NASMFLAGS)
endif

ifeq ($(ARCH),aarch64)
    ifeq ($(CC_IS_CLANG),1)
        override kcc += \
            -target aarch64-unknown-none-elf
    endif
    override kcflags += \
        -mcpu=generic \
        -march=armv8-a+nofp+nosimd \
        -mgeneral-regs-only
    override kldflags += \
        -m aarch64elf
endif

ifeq ($(ARCH),riscv64)
    ifeq ($(CC_IS_CLANG),1)
        override kcc += \
            -target riscv64-unknown-none-elf
        override kcflags += \
            -march=rv64imac
    else
        override kcflags += \
            -march=rv64imac_zicsr_zifencei
    endif
    override kcflags += \
        -mabi=lp64 \
        -mno-relax
    override kldflags += \
        -m elf64lriscv \
        --no-relax
endif

ifeq ($(ARCH),loongarch64)
    ifeq ($(CC_IS_CLANG),1)
        override kcc += \
            -target loongarch64-unknown-none-elf
    endif
    override kcflags += \
        -march=loongarch64 \
        -mabi=lp64s \
        -mfpu=none \
        -msimd=none
    override kldflags += \
        -m elf64loongarch
endif

bin/$n-$a.k: Makefile k/$a.lds $(k_o)
	@echo LD $@
	@mkdir -p "$(dir $@)"
	@$(LD) $(kldflags) $(k_o) -o $@


bin/k_$a/%.o: %.c Makefile $(share_h) bin/boot.h
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
bin/k_$a/share/cga_8x8.o: share/font/cga_8x8.c
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
bin/k_$a/%.o: k/src/%.S $(share_h) Makefile bin/boot.h
	@echo AS $@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
bin/k_$a/k/src/$a/%.o: k/src/$a/%.asm
	@echo AS $@
	@mkdir -p "$(dir $@)"
	@nasm $(NASMFLAGS) $< -o $@


bin/$n-$a.iso: bin/$n-$a.k bin/limine/limine k/limine.conf
	@echo MK $@
	@rm -rf iso_root
	@mkdir -p iso_root/boot
	@cp $< iso_root/boot/kernel
	@mkdir -p iso_root/boot/limine
	@cp k/limine.conf iso_root/boot/limine/
	@mkdir -p iso_root/EFI/BOOT
	@cp bin/limine/limine-uefi-cd.bin iso_root/boot/limine/
ifeq ($a,x86_64)
	@cp bin/limine/limine-bios.sys bin/limine/limine-bios-cd.bin iso_root/boot/limine/
	@cp bin/limine/BOOTX64.EFI bin/limine/BOOTIA32.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -quiet -R -r -J -b boot/limine/limine-bios-cd.bin \
		-no-emul-boot -boot-load-size 4 -boot-info-table -hfsplus \
		-apm-block-size 2048 --efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
	@bin/limine/limine bios-install $@
endif
ifeq ($a,aarch64)
	@cp -v bin/limine/BOOTAA64.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -quiet -R -r -J \
		-hfsplus -apm-block-size 2048 \
		--efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
endif
ifeq ($a,riscv64)
	@cp -v bin/limine/BOOTRISCV64.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -R -r -J \
		-hfsplus -apm-block-size 2048 \
		--efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
endif
ifeq ($a,loongarch64)
	@cp -v bin/limine/BOOTLOONGARCH64.EFI iso_root/EFI/BOOT/
	@xorriso -as mkisofs -R -r -J \
		-hfsplus -apm-block-size 2048 \
		--efi-boot boot/limine/limine-uefi-cd.bin \
		-efi-boot-part --efi-boot-image --protective-msdos-label \
		iso_root -o $@
endif
	@rm -rf iso_root

bin/$n-$a.hdd: bin/$n-$a.iso limine/limine k/limine.conf
	@echo MK $@
	@rm -f $@
	@dd if=/dev/zero bs=1M count=0 seek=64 of=$@
ifeq ($a,x86_64)
	@PATH=$$PATH:/usr/sbin:/sbin sgdisk $@ -n 1:2048 -t 1:ef00 -m 1
	@bin/limine/limine bios-install $@ 2>&1 >/dev/null
else
	@PATH=$$PATH:/usr/sbin:/sbin sgdisk $@ -n 1:2048 -t 1:ef00
endif
	@mformat -i $@@@1M
	@mmd -i $@@@1M ::/EFI ::/EFI/BOOT ::/boot ::/boot/limine
	@mcopy -i $@@@1M $< ::/boot/kernel
	@mcopy -i $@@@1M k/limine.conf ::/boot/limine
ifeq ($a,x86_64)
	@mcopy -i $@@@1M bin/limine/limine-bios.sys ::/boot/limine
	@mcopy -i $@@@1M bin/limine/BOOTX64.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M bin/limine/BOOTIA32.EFI ::/EFI/BOOT
endif
ifeq ($a,aarch64)
	@mcopy -i $@@@1M bin/limine/BOOTAA64.EFI ::/EFI/BOOT
endif
ifeq ($a,riscv64)
	@mcopy -i $@@@1M bin/limine/BOOTRISCV64.EFI ::/EFI/BOOT
endif
ifeq ($a,loongarch64)
	@mcopy -i $@@@1M bin/limine/BOOTLOONGARCH64.EFI ::/EFI/BOOT
endif

.PHONY: run run-hdd
run: run-$a
run-hdd: run-hdd-$a
QEMUFLAGS := -m 2G
qemu-x86_64=qemu-system-x86_64 $(QEMUFLAGS)\
	-M q35\
	-drive if=pflash,unit=0,format=raw,file=bin/ovmf/ovmf-code-x86_64.fd,readonly=on
.PHONY: run-x86_64 run-hdd-x86_64
run-x86_64: bin/$n-$a.iso bin/ovmf/ovmf-code-$a.fd
	$(qemu-x86_64) -cdrom $<
run-hdd-x86_64: bin/$n-$a.hdd bin/ovmf/ovmf-code-$a.fd
	$(qemu-x86_64) -hda $<

qemu-aarch64=qemu-system-aarch64 $(QEMUFLAGS)\
		-M virt \
		-cpu cortex-a72 \
		-device ramfb \
		-device qemu-xhci \
		-device usb-kbd \
		-device usb-mouse \
		-drive if=pflash,unit=0,format=raw,file=bin/ovmf/ovmf-code-aarch64.fd,readonly=on

.PHONY: run-aarch64 run-hdd-aarch64
run-aarch64: bin/ovmf/ovmf-code-aarch64.fd $n-aarch64.iso
	$(qemu-aarch64) -cdrom $n-aarch64.iso
run-hdd-aarch64: bin/ovmf/ovmf-code-aarch64.fd $n-aarch64.hdd
	$(qemu-aarch64) -hda $n-aarch64.hdd

qemu-riscv64=qemu-system-riscv64 $(QEMUFLAGS)\
	-M virt \
	-cpu rv64 \
	-device ramfb \
	-device qemu-xhci \
	-device usb-kbd \
	-device usb-mouse \
	-drive if=pflash,unit=0,format=raw,file=bin/ovmf/ovmf-code-riscv64.fd,readonly=on

.PHONY: run-riscv64 run-hdd-riscv64
run-riscv64: bin/ovmf/ovmf-code-riscv64.fd $n-riscv64.iso
	$(qemu-riscv64) -cdrom $n-riscv64.iso
run-hdd-riscv64: bin/ovmf/ovmf-code-riscv64.fd $n-riscv64.hdd
	$(qemu-riscv64) -hda $n-riscv64.hdd

qemu-loongarch64=qemu-system-loongarch64 $(QEMUFLAGS)\
	-M virt \
	-cpu la464 \
	-device ramfb \
	-device qemu-xhci \
	-device usb-kbd \
	-device usb-mouse \
	-drive if=pflash,unit=0,format=raw,file=bin/ovmf/ovmf-code-loongarch64.fd,readonly=on

.PHONY: run-loongarch64 run-hdd-loongarch64
run-loongarch64: bin/ovmf/ovmf-code-loongarch64.fd bin/$n-loongarch64.iso
	$(qemu-loongarch64) -cdrom $n-$a.iso

run-hdd-loongarch64: bin/ovmf/ovmf-code-loongarch64.fd bin/$n-loongarch64.hdd
	$(qemu-loongarch64) -hda $n-$a.hdd

bin/ovmf/ovmf-code-%.fd:
	@echo MK ovmf
	@mkdir -p bin/ovmf
	@curl -sLo $@ https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/$(notdir $@)
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
		riscv64) dd if=/dev/zero of=$@ bs=1 count=0 seek=33554432 2>/dev/null;; \
	esac

# Toolchain for building the 'limine' executable for the host.
git_clone_limine=git clone\
	https://codeberg.org/Limine/Limine.git\
 	bin/limine --branch=v10.x-binary --depth=1 > /dev/null 2>&1
bin/limine/limine:
	@echo MK limine
	@rm -rf bin/limine
	@$(git_clone_limine)
	@make -sC bin/limine
