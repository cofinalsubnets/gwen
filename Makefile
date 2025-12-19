n=gl
x=g
b=b
lib=LD_LIBRARY_PATH=$b/h:$(LD_LIBRARY_PATH)
m=$(lib) $b/h/$n
l=$(lib) $b/h/lcat
a?=$(shell uname -m)

t=$(sort $(wildcard t/*.$x))

.PHONY: test all h k pd clean distclean valg perf repl cloc cat catr
test: $b/h/$n
	@echo TEST
	@cat $t | $m
all: h k pd
h: $b/h/$n $b/h/lib$n.so $b/h/$n.1
k: $b/$n-$a.elf
pd: $b/$n.pdx
clean:
	@echo RM	$b
	@rm -rf $b
distclean:
	@echo RM	$b dl
	@rm -rf `git check-ignore * */*`
valg: $b/h/$n
	@cat $t | $(lib) valgrind --error-exitcode=1 b/h/$n
$b/perf.data: $b/h/$n
	cat $t | $(lib) perf record -o $@ b/h/$n
perf: $b/perf.data
	perf report -i $<
$b/flamegraph.svg: $b/perf.data
	flamegraph -o $@ --perfdata $<
repl: $b/h/$n
	@which rlwrap >/dev/null && $(lib) rlwrap $< || $(lib) $<
cloc:
	cloc --by-file --force-lang=Lisp,$x g h js k p pd t vim
cat: clean all test

#build
# c files and headers
g_h=$(wildcard g/*.h)
g_c=$(wildcard g/*.c)
f_c=$(wildcard f/*.c)
h_o=$(addprefix $b/h/, $(g_c:.c=.o) sys.o)
g_cflags=-std=gnu23 -g -O2 -pipe\
 	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
 	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none
cc=$(CC) $(g_cflags) -fpic -I. -Ig/ -Ih/ -I$b/

k_c=$(g_c) $(f_c) $(wildcard k/*.c) $(wildcard k/arch/$a/*.c)
k_h=$(g_h) $(wildcard k/*.h) $(wildcard k/arch/$a/*.h)
k_S=$(wildcard k/*.S) $(wildcard k/arch/$a/*.S)
k_asm=$(wildcard k/*.asm) $(wildcard k/arch/$a/*.asm)
k_o=$(addprefix $b/k/$a/, $(k_c:.c=.o) $(k_S:.S=.o) $(k_asm:.asm=.o))

kcflags=$(g_cflags)	-nostdinc -ffreestanding -fno-lto -fno-PIC -ffunction-sections -fdata-sections
kldflags := -static -nostdlib --gc-sections -T k/arch/$a/$a.lds -z max-page-size=0x1000
kcppflags := \
	-Ik  -Ib -If -Ig\
	-I k/include/ \
	-isystem k/include/ \
	$(kcppflags) \
	-DLIMINE_API_REVISION=3

ifeq ($(CC_IS_CLANG),1)
kcc_if_clang=-target $a-unknown-none-elf
endif
kcflags_x86_64=-m64 -march=x86-64 -mabi=sysv -mno-80387 -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -mcmodel=kernel
kcflags_aarch64=-mcpu=generic -march=armv8-a+nofp+nosimd -mgeneral-regs-only
kcflags_riscv64=-march=rv64imac -mabi=lp64 -mno-relax
kcflags_loongarch64=-march=loongarch64 -mabi=lp64s  -mfpu=none -msimd=none

kldflags_x86_64=-m elf_x86_64
kldflags_aarch64=-m aarch64elf
kldflags_riscv64=-m elf64lriscv --no-relax
kldflags_loongarch64=-m elf64loongarch

kcc=$(CC) $(kcflags) $(kcflags_$a) $(kcppflags) $(kcc_if_clang)
kcc_loongarch64=-target loongarch64-unknown-none-elf
kcc_riscv64=-target riscv64-unknown-none-elf
kcc_aarch64=-target aarch64-unknown-none-elf
k_nasmflags := -f elf64 -g -F dwarf -Wall -w-reloc-abs-qword -w-reloc-abs-dword -w-reloc-rel-dword

pd_sdk=$(PLAYDATE_SDK_PATH)
pd_src=$(wildcard pd/*.c) $(g_c) $(f_c)
pd_gcc=arm-none-eabi-gcc
pd_cc=$(pd_gcc) -g3
pd_as=$(pd_gcc) -x assembler-with-cpp
pd_opt=-O2 -falign-functions=16 -fomit-frame-pointer
pd_lds=$(patsubst ~%,$(HOME)%,$(pd_sdk)/C_API/buildsupport/link_map.ld)
pd_fpu=-mfloat-abi=hard -mfpu=fpv5-sp-d16 -D__FPU_USED=1
pd_incdir=$(patsubst %,-I %, pd $(pd_sdk)/C_API g b f)
pd_defs =-DTARGET_PLAYDATE=1 -DTARGET_EXTENSION=1 -Dg_tco=0
pd_src +=$(pd_sdk)/C_API/buildsupport/setup.c
pd_o=$(addprefix $b/pd/, $(pd_src:.c=.o))
pd_mcflags=-mthumb -mcpu=cortex-m7 $(pd_fpu)
pd_cpflags=\
	$(pd_mcflags) $(pd_opt) $(pd_defs)\
 	-gdwarf-2 -Wall -Wno-unused -Wstrict-prototypes -Wno-unknown-pragmas\
 	-fverbose-asm -Wdouble-promotion -mword-relocations -fno-common\
  -ffunction-sections -fdata-sections -Wa,-ahlms=$b/pd/$(notdir $(<:.c=.lst))
pd_ldflags=\
	-nostartfiles $(pd_mcflags) -T$(pd_lds)\
 	-Wl,-Map=$b/pd/pdex.map,--cref,--gc-sections,--no-warn-mismatch,--emit-relocs
pd_asflags=$(pd_mcflags) $(pd_opt) -g3 -gdwarf-2 -Wa,-amhls=$(<:.s=.lst)\
  -D__HEAP_SIZE=8388208 \
 	-D__STACK_SIZE=4194304

$b/h/lcat: $b/h/lcat.o $b/h/lib$n.so
	@echo LD	$@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^

$b/h/lib$n.a: $(h_o)
	@echo AR	$@
	@mkdir -p $(dir $@)
	@ar rcs $@ $^

$b/h/lib$n.so: $b/h/lib$n.a
	@echo LD	$@
	@mkdir -p $(dir $@)
	@$(cc) -shared -o $@ -Wl,--whole-archive $^ -Wl,--no-whole-archive

$b/h/%.o: %.c $(g_h)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(cc) -c $< -o $@

$b/h/%.o: h/%.c $(g_h)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(cc) -c -I$b/h $< -o $@

$b/h/main.o: h/main.c $b/boot.h $(g_h)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(cc) -c -I$b/h $< -o $@

$b/h/$n: $b/h/main.o $b/h/lib$n.so
	@echo LD	$@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ -l$n -Lb/h $<

# sed command to escape lisp text into C string format
$b/boot.h: $b/h/lcat h/lcat.sed g/boot.$x
	@echo GEN	$@
	@$l < g/boot.$x | sed -f h/lcat.sed >$@

$b/h/$n.1: $b/h/$n h/manpage.$x
	@echo GEN	$@
	@$m < h/manpage.$x > $@

$b/$n-$a.elf: k/arch/$a/$a.lds $(k_o)
	@echo LD	$@
	@mkdir -p "$(dir $@)"
	@$(LD) $(kldflags) $(k_o) -o $@

$b/k/$a/%.o: %.c $(g_h) $b/boot.h
	@echo CC	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

$b/k/$a/g/cga_8x8.o: f/cga_8x8.c
	@echo CC	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

$b/k/$a/%.o: k/%.S $(g_h)
	@echo AS	$@
	@mkdir -p "$(dir $@)"
	@$(kcc) -c $< -o $@

$b/k/$a/k/arch/$a/%.o: k/arch/$a/%.asm
	@echo AS	$@
	@mkdir -p "$(dir $@)"
	@nasm $< -o $@ $(k_nasmflags)

$b/$n.pdx: $b/pd/Source/pdex.elf $b/pd/Source/pdex.so
	@echo PDC	$@
	@$(pd_sdk)/bin/pdc -sdkpath $(pd_sdk) $b/pd/Source $@

$b/pd/Source/pdex.%: $b/pd/pdex.%
	@echo CP	$@
	@mkdir -p $(dir $@)
	@cp $< $@

$b/pd/%.o : %.c | $b/boot.h
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(pd_cc) -c $(pd_cpflags) -I pd -I b -I f $(pd_incdir) $< -o $@

$b/pd/%.o : %.s
	@echo AS	$@
	@$(pd_as) -c $(pd_asflags) $< -o $@

$b/pd/pdex.elf: $(pd_o) $(pd_lds)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(pd_cc) $(pd_o) $(pd_ldflags) -o $@

$b/pd/pdex.so: $(pd_src)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@gcc -g -shared -fPIC -lm -Dg_tco=0 -DTARGET_SIMULATOR=1 -DTARGET_EXTENSION=1 $(pd_incdir) -o $b/pd/pdex.so $(pd_src)

.PRECIOUS: $b/pd/%elf

k_xorriso_x86_64=\
	-b boot/limine/limine-bios-cd.bin \
	-no-emul-boot -boot-load-size 4 -boot-info-table
k_xorriso=xorriso -as mkisofs -quiet -R -r -J\
	-hfsplus -apm-block-size 2048\
	--efi-boot boot/limine/limine-uefi-cd.bin\
	-efi-boot-part --efi-boot-image --protective-msdos-label\
	$(k_xorriso_$a)

$b/$n-$a.iso: $b/$n-$a.elf dl/limine/limine k/limine/limine.conf
	@echo MK $@
	@rm -rf $b/iso_root
	@mkdir -p $b/iso_root/boot
	@cp $< $b/iso_root/boot/kernel
	@mkdir -p $b/iso_root/boot/limine
	@cp k/limine/limine.conf $b/iso_root/boot/limine/
	@mkdir -p $b/iso_root/EFI/BOOT
	@cp dl/limine/limine-uefi-cd.bin $b/iso_root/boot/limine/
	@cp dl/limine/limine-bios.sys dl/limine/limine-bios-cd.bin $b/iso_root/boot/limine/
	@cp dl/limine/BOOTX64.EFI dl/limine/BOOTIA32.EFI $b/iso_root/EFI/BOOT/
	@cp dl/limine/BOOTAA64.EFI $b/iso_root/EFI/BOOT/
	@cp dl/limine/BOOTRISCV64.EFI $b/iso_root/EFI/BOOT/
	@cp dl/limine/BOOTLOONGARCH64.EFI $b/iso_root/EFI/BOOT/
	$(k_xorriso) $b/iso_root -o $@
	@dl/limine/limine bios-install $@
	@rm -rf $b/iso_root

$b/$n-$a.hdd: $b/$n-$a.elf dl/limine/limine k/limine/limine.conf
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
k_qemu_aarch64=$(k_qemu_risc) -cpu cortex-a72
k_qemu_riscv64=$(k_qemu_risc) -cpu rv64
k_qemu=qemu-system-$a -m 256M $(k_qemu_$a)\
	-drive if=pflash,unit=0,format=raw,file=dl/ovmf/ovmf-code-$a.fd,readonly=on

run: run-$a
run-hdd: run-hdd-$a
run-$a: $b/$n-$a.iso dl/ovmf/ovmf-code-$a.fd
	$(k_qemu) -cdrom $<
run-hdd-$a: $b/$n-$a.hdd dl/ovmf/ovmf-code-$a.fd
	$(k_qemu) -hda $<
.PHONY: run run-hdd run-$a run-hdd-$a

dl/ovmf/ovmf-code-%.fd:
	@echo MK ovmf
	@mkdir -p dl/ovmf
	@curl -sLo $@ https://github.com/osdev0/edk2-ovmf-nightly/releases/latest/download/$(notdir $@)
	@case "$a" in \
		aarch64) dd if=/dev/zero of=$@ bs=1 count=0 seek=67108864 2>/dev/null;; \
		riscv64) dd if=/dev/zero of=$@ bs=1 count=0 seek=33554432 2>/dev/null;; \
	esac

dl/limine/limine:
	@echo MK limine
	@rm -rf dl/limine
	@git clone https://codeberg.org/Limine/Limine.git dl/limine --branch=v10.x-binary --depth=1 > /dev/null 2>&1
	@make -sC dl/limine

# installlation
# default install to home directory under ~/.local/
PREFIX ?= .local/
VIMPREFIX ?= .vim/
DESTDIR ?= $(HOME)/
installs=\
 	$(DESTDIR)/$(PREFIX)/bin/$n\
  $(DESTDIR)/$(PREFIX)/g/man/man1/$n.1\
  $(DESTDIR)/$(PREFIX)/lib/lib$n.a\
  $(DESTDIR)/$(PREFIX)/lib/lib$n.so\
  $(DESTDIR)/$(PREFIX)/include/$x.h\
  $(DESTDIR)/$(VIMPREFIX)/ftdetect/$n.vim\
  $(DESTDIR)/$(VIMPREFIX)/syntax/$n.vim

.PHONY: install uninstall
install: $(installs)
uninstall:
	@echo RM	$(abspath $(installs))
	@rm -f $(installs)

$(DESTDIR)/$(PREFIX)/include/$x.h: g/$x.h
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$(DESTDIR)/$(PREFIX)/lib/lib$n.a: $b/h/lib$n.a
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$(DESTDIR)/$(PREFIX)/lib/lib$n.so: $b/h/lib$n.so
	@echo CP	$(abspath $@)
	@install -D -m 755 -s $< $@

$(DESTDIR)/$(PREFIX)/bin/$n: $b/h/$n
	@echo CP	$(abspath $@)
	@install -D -m 755 -s $< $@

$(DESTDIR)/$(PREFIX)/g/man/man1/$n.1: $b/h/$n.1
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$(DESTDIR)/$(VIMPREFIX)/ftdetect/$n.vim: vim/ftdetect.vim
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@

$(DESTDIR)/$(VIMPREFIX)/syntax/$n.vim: vim/syntax.vim
	@echo CP	$(abspath $@)
	@install -D -m 644 $< $@
