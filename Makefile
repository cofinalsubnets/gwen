n=mitty
x=mi
m=bin/host/$n
l=bin/host/lcat
ARCH=$(shell uname -m)
.PHONY: test all
test: bin/host/$n
	@echo MI test
	@$m test/*.$x

all: bin/$n-$(ARCH).k bin/host/$n bin/host/lib$n.so bin/pd/$n.pdx

#build
target ?= host
#build
# c files and headers
share_h=$(wildcard g/*.h)
share_c=$(wildcard g/*.c)
font_c=$(wildcard g/font/*.c)
font_h=$(wildcard g/font/*.h)
host_o=$(addprefix bin/host/, $(share_c:.c=.o) sys.o)
a=bin/host/$n.a
so=bin/host/lib$n.so
flags:= -std=gnu17 -g -O2 -pipe\
 	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
 	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-flto -fpic
cc=$(CC) $(flags) -Ig/ -Ihost/ -Ibin/ \
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
bin/boot.h: bin/host/lcat host/lcat.sed g/boot.$x
	@echo MI $@
	@$l < g/boot.$x | sed -f host/lcat.sed >$@

bin/host/main.h: bin/host/lcat host/lcat.sed host/main.$x
	@echo MI $@
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
  $(dest)/g/man/man1/$n.1\
  $(dest)/lib/$n.a\
  $(dest)/lib/lib$n.so\
  $(dest)/include/$n.h\
  $(vimdest)/ftdetect/$n.vim\
  $(vimdest)/syntax/$n.vim
install: $(installs)
uninstall:
	@echo RM $(abspath $(installs))
	@rm -f $(installs)

$(dest)/include/$n.h: g/$n.h
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
$(dest)/g/man/man1/$n.1: bin/host/$n.1
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/ftdetect/$n.vim: misc/vim/ftdetect.vim
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/syntax/$n.vim: misc/vim/syntax.vim
	@echo IN $(abspath $@)
	@install -D -m 644 $< $@

clean:
	@echo CLEAN
	@rm -rf bin
distclean:
	@echo DISTCLEAN
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


a=$(ARCH)
k_c=$(share_c) $(font_c) $(wildcard k/*.c) $(wildcard k/arch/$a/*.c)
k_h=$(share_h) $(font_h) $(wildcard k/*.h) $(wildcard k/arch/$a/*.h)
k_S=$(wildcard k/*.S) $(wildcard k/arch/$a/*.S)
k_asm=$(wildcard k/*.asm) $(wildcard k/arch/$a/*.asm)
k_o=$(addprefix bin/k_$a/, $(k_c:.c=.o) $(k_S:.S=.o) $(k_asm:.asm=.o))

kldflags := -static -nostdlib --gc-sections -T k/arch/$a/$a.lds -z max-page-size=0x1000
kcflags:=-std=gnu17 -g -O2 -pipe\
	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-nostdinc -ffreestanding -fno-lto -fno-PIC -ffunction-sections -fdata-sections
kcppflags := \
    -I k/ \
		-I k/include/ \
		-I bin/ \
		-I g/ \
		-I g/font/ \
		-Dg_target=g_target_os \
    -isystem k/include/ \
    $(kcppflags) \
    -DLIMINE_API_REVISION=3

ifeq ($(CC_IS_CLANG),1)
kcc_if_clang=-target $a-unknown-none-elf
else
endif
kcflags_x86_64=\
	-m64 \
	-march=x86-64 \
	-mabi=sysv \
	-mno-80387 \
	-mno-mmx \
	-mno-sse \
	-mno-sse2 \
	-mno-red-zone \
	-mcmodel=kernel
kcflags_aarch64=\
	-mcpu=generic \
	-march=armv8-a+nofp+nosimd \
	-mgeneral-regs-only
kcflags_riscv64=\
	-march=rv64imac\
	-mabi=lp64 \
	-mno-relax
kcflags_loongarch64=\
	-march=loongarch64 \
	-mabi=lp64s \
	-mfpu=none \
	-msimd=none
kldflags_x86_64=-m elf_x86_64
kldflags_aarch64=-m aarch64elf
kldflags_riscv64=-m elf64lriscv --no-relax
kldflags_loongarch64=-m elf64loongarch

kcc=$(CC) $(kcflags) $(kcflags_$a) $(kcppflags) $(kcc_if_clang)
kcc_loongarch64=-target loongarch64-unknown-none-elf
kcc_riscv64=-target riscv64-unknown-none-elf
kcc_aarch64=-target aarch64-unknown-none-elf
k_nasmflags := -f elf64 -g -F dwarf -Wall -w-reloc-abs-qword -w-reloc-abs-dword -w-reloc-rel-dword

bin/$n-$a.k: Makefile k/arch/$a/$a.lds $(k_o)
	@echo LD $@
	@mkdir -p "$(dir $@)"
	@$(LD) $(kldflags) $(k_o) -o $@

bin/k_$a/%.o: %.c Makefile $(share_h) bin/boot.h
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
bin/k_$a/g/cga_8x8.o: g/font/cga_8x8.c
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
bin/k_$a/%.o: k/%.S $(share_h) Makefile bin/boot.h
	@echo AS $@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@
bin/k_$a/k/arch/$a/%.o: k/arch/$a/%.asm
	@echo AS $@
	@mkdir -p "$(dir $@)"
	@nasm $< -o $@ $(k_nasmflags)

k_xorriso_x86_64=\
	 	-b boot/limine/limine-bios-cd.bin \
		-no-emul-boot -boot-load-size 4 -boot-info-table
k_xorriso=xorriso -as mkisofs -quiet -R -r -J\
	-hfsplus -apm-block-size 2048\
	--efi-boot boot/limine/limine-uefi-cd.bin\
	-efi-boot-part --efi-boot-image --protective-msdos-label\
	$(k_xorriso_$a)

bin/$n-$a.iso: bin/$n-$a.k dl/limine/limine k/limine/limine.conf
	@echo MK $@
	@rm -rf bin/iso_root
	@mkdir -p bin/iso_root/boot
	@cp $< bin/iso_root/boot/kernel
	@mkdir -p bin/iso_root/boot/limine
	@cp k/limine/limine.conf bin/iso_root/boot/limine/
	@mkdir -p bin/iso_root/EFI/BOOT
	@cp dl/limine/limine-uefi-cd.bin bin/iso_root/boot/limine/
	@cp dl/limine/limine-bios.sys dl/limine/limine-bios-cd.bin bin/iso_root/boot/limine/
	@cp dl/limine/BOOTX64.EFI dl/limine/BOOTIA32.EFI bin/iso_root/EFI/BOOT/
	@cp dl/limine/BOOTAA64.EFI bin/iso_root/EFI/BOOT/
	@cp dl/limine/BOOTRISCV64.EFI bin/iso_root/EFI/BOOT/
	@cp dl/limine/BOOTLOONGARCH64.EFI bin/iso_root/EFI/BOOT/
	$(k_xorriso) bin/iso_root -o $@
	@dl/limine/limine bios-install $@
	@rm -rf bin/iso_root

bin/$n-$a.hdd: bin/$n-$a.k dl/limine/limine k/limine/limine.conf
	@echo MK $@
	@rm -f $@
	@dd if=/dev/zero bs=1M count=0 seek=64 of=$@
	@PATH=$$PATH:/usr/sbin:/sbin sgdisk $@ -n 1:2048 -t 1:ef00
	@mformat -i $@@@1M
	@mmd -i $@@@1M ::/EFI ::/EFI/BOOT ::/boot ::/boot/limine
	@mcopy -i $@@@1M $< ::/boot/kernel
	@mcopy -i $@@@1M k/limine/limine.conf ::/boot/limine
	@mcopy -i $@@@1M dl/limine/limine-bios.sys ::/boot/limine
	@mcopy -i $@@@1M dl/limine/BOOTX64.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M dl/limine/BOOTIA32.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M dl/limine/BOOTAA64.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M dl/limine/BOOTRISCV64.EFI ::/EFI/BOOT
	@mcopy -i $@@@1M dl/limine/BOOTLOONGARCH64.EFI ::/EFI/BOOT

k_qemu_x86_64=-M q35
k_qemu_risc=-M virt -device ramfb -device qemu-xhci -device usb-kbd -device usb-mouse
k_qemu_loongarch64=$(k_qemu_risc) -cpu la464
k_qemu_aarch64=$(k_qemu_risc) -cpu=cortex-a72
k_qemu_riscv64=$(k_qemu_risc) -cpu=rv64
k_qemu=qemu-system-$a -m 2G $(k_qemu_$a)\
	-drive if=pflash,unit=0,format=raw,file=dl/ovmf/ovmf-code-$a.fd,readonly=on

.PHONY: run run-hdd run-$a run-hdd-$a
run: run-$a
run-hdd: run-hdd-$a
run-$a: bin/$n-$a.iso dl/ovmf/ovmf-code-$a.fd
	$(k_qemu) -cdrom $<
run-hdd-$a: bin/$n-$a.hdd dl/ovmf/ovmf-code-$a.fd
	$(k_qemu) -hda $<

dl/ovmf/ovmf-code-%.fd:
	@echo MK ovmf
	@mkdir -p dl/ovmf
	@curl -sLo $@ https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/$(notdir $@)
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
		riscv64) dd if=/dev/zero of=$@ bs=1 count=0 seek=33554432 2>/dev/null;; \
	esac

# Toolchain for building the 'limine' executable for the host.
git_clone_limine=git clone\
	https://codeberg.org/Limine/Limine.git\
 	dl/limine --branch=v10.x-binary --depth=1 > /dev/null 2>&1
dl/limine/limine:
	@echo MK limine
	@rm -rf dl/limine
	@$(git_clone_limine)
	@make -sC dl/limine
bin/pd/%:
	@make -C pd ../$@
