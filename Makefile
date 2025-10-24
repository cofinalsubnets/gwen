n=gl
x=g
m=bin/h/$n
l=bin/h/lcat
a?=$(shell uname -m)
test: bin/h/$n
	@echo GL test
	@$m t/*.$x
all: h k pd
h: bin/h/$n bin/h/lib$n.so bin/h/$n.1
k: bin/$n-$a.k
pd: bin/$n.pdx
.PHONY: test all h k pd

#build
target ?= host
#build
# c files and headers
share_h=$(wildcard g/*.h)
share_c=$(wildcard g/*.c)
font_c=$(wildcard g/font/*.c)
host_o=$(addprefix bin/h/, $(share_c:.c=.o) sys.o)
flags:= -std=gnu17 -g -O2 -pipe\
 	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
 	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-flto -fpic
cc=$(CC) $(flags) -I. -Ig/ -Ih/ -Ibin/ \
	 -D g_version='"$(shell git rev-parse HEAD)"'\
	 -D g_target=g_target_$(target)

bin/h/$n: bin/h/main.o bin/h/$n.a
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^

bin/h/lcat: bin/h/lcat.o bin/h/$n.a
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -o $@ $^

bin/h/$n.a: $(host_o)
	@echo AR $@
	@mkdir -p $(dir $@)
	@ar rcs $@ $^

bin/h/lib$n.so: $(host_o)
	@echo LD $@
	@mkdir -p $(dir $@)
	@$(cc) -shared -o $@ $^

bin/h/%.o: %.c $(share_h) Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c $< -o $@

bin/h/%.o: h/%.c $(share_h) Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c -Ibin/h $< -o $@

bin/h/main.o: h/main.c bin/h/main.h bin/boot.h $(share_h) Makefile
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(cc) -c -Ibin/h $< -o $@

# sed command to escape lisp text into C string format
bin/boot.h: bin/h/lcat h/lcat.sed g/boot.$x
	@echo GL $@
	@$< < g/boot.$x | sed -f h/lcat.sed >$@

bin/h/main.h: bin/h/lcat h/lcat.sed h/main.$x
	@echo GL $@
	@$< < h/main.$x | sed -f h/lcat.sed > $@

bin/h/$n.1: bin/h/$n h/manpage.$x
	@echo GL $@
	@$^ > $@

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
  $(dest)/include/$x.h\
  $(vimdest)/ftdetect/$n.vim\
  $(vimdest)/syntax/$n.vim
install: $(installs)
uninstall:
	@echo RM $(abspath $(installs))
	@rm -f $(installs)
.PHONY: install uninstall

$(dest)/include/$x.h: g/$x.h
	@echo CP $(abspath $@)
	@install -D -m 644 $< $@
$(dest)/lib/$n.a: bin/h/$n.a
	@echo CP $(abspath $@)
	@install -D -m 755 $< $@
$(dest)/lib/lib$n.so: bin/h/lib$n.so
	@echo CP $(abspath $@)
	@install -D -m 755 $< $@
$(dest)/bin/$n: bin/h/$n
	@echo CP $(abspath $@)
	@install -D -m 755 -s $< $@
$(dest)/g/man/man1/$n.1: bin/h/$n.1
	@echo CP $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/ftdetect/$n.vim: g/vim/ftdetect.vim
	@echo CP $(abspath $@)
	@install -D -m 644 $< $@
$(vimdest)/syntax/$n.vim: g/vim/syntax.vim
	@echo CP $(abspath $@)
	@install -D -m 644 $< $@

clean:
	@echo RM bin
	@rm -rf bin
distclean:
	@echo RM bin dl
	@rm -rf `git check-ignore * */*`
# valgrind detects some memory errors
valg: bin/h/$n
	valgrind --error-exitcode=1 $m t/*.$x
# profiling on linux
bin/perf.data: bin/h/$n
	perf record -o $@ $m t/*.$x
perf: bin/perf.data
	perf report -i $<
bin/flamegraph.svg: bin/perf.data
	flamegraph -o $@ --perfdata $<
repl: bin/h/$n
	rlwrap $m
serve:
	darkhttpd .
.PHONY: clean distclean valg perf repl serve

k_c=$(share_c) $(font_c) $(wildcard k/*.c) $(wildcard k/arch/$a/*.c)
k_h=$(share_h) $(wildcard k/*.h) $(wildcard k/arch/$a/*.h)
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
	-Dg_target=g_target_os \
	-isystem k/include/ \
	$(kcppflags) \
	-DLIMINE_API_REVISION=3

ifeq ($(CC_IS_CLANG),1)
kcc_if_clang=-target $a-unknown-none-elf
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
bin/k_$a/%.o: k/%.S $(share_h) Makefile
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
k_qemu_aarch64=$(k_qemu_risc) -cpu cortex-a72
k_qemu_riscv64=$(k_qemu_risc) -cpu rv64
k_qemu=qemu-system-$a -m 2G $(k_qemu_$a)\
	-drive if=pflash,unit=0,format=raw,file=dl/ovmf/ovmf-code-$a.fd,readonly=on

run: run-$a
run-hdd: run-hdd-$a
run-$a: bin/$n-$a.iso dl/ovmf/ovmf-code-$a.fd
	$(k_qemu) -cdrom $<
run-hdd-$a: bin/$n-$a.hdd dl/ovmf/ovmf-code-$a.fd
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

pd_sdk=$(PLAYDATE_SDK_PATH)
pd_src=$(wildcard pd/*.c) $(share_c) $(font_c)
pd_gcc=arm-none-eabi-gcc
pd_cc=$(pd_gcc) -g3
pd_as=$(pd_gcc) -x assembler-with-cpp
pd_opt=-O2 -falign-functions=16 -fomit-frame-pointer
pd_lds=$(patsubst ~%,$(HOME)%,$(pd_sdk)/C_API/buildsupport/link_map.ld)
pd_fpu=-mfloat-abi=hard -mfpu=fpv5-sp-d16 -D__FPU_USED=1
pd_incdir=$(patsubst %,-I %, pd $(pd_sdk)/C_API g bin)
pd_defs =-DTARGET_PLAYDATE=1 -DTARGET_EXTENSION=1 -Dg_tco=0
pd_heap =8388208
pd_stack=4194304
pd_adefs=-D__HEAP_SIZE=$(pd_heap) -D__STACK_SIZE=$(pd_stack)
pd_src +=$(pd_sdk)/C_API/buildsupport/setup.c
pd_o=$(addprefix bin/pd/, $(pd_src:.c=.o))
pd_mcflags=-mthumb -mcpu=cortex-m7 $(pd_fpu)
pd_asflags=$(pd_mcflags) $(pd_opt) -g3 -gdwarf-2 -Wa,-amhls=$(<:.s=.lst) $(pd_adefs)
pd_cpflags=\
	$(pd_mcflags) $(pd_opt) $(pd_defs)\
 	-gdwarf-2 -Wall -Wno-unused -Wstrict-prototypes -Wno-unknown-pragmas\
 	-fverbose-asm -Wdouble-promotion -mword-relocations -fno-common\
  -ffunction-sections -fdata-sections -Wa,-ahlms=bin/pd/$(notdir $(<:.c=.lst))
pd_ldflags=\
	-nostartfiles $(pd_mcflags) -T$(pd_lds)\
 	-Wl,-Map=bin/pd/pdex.map,--cref,--gc-sections,--no-warn-mismatch,--emit-relocs

bin/$n.pdx: bin/pd/Source/pdex.elf bin/pd/Source/pdex.so
	@echo PD $@
	@$(pd_sdk)/bin/pdc -sdkpath $(pd_sdk) bin/pd/Source $@
bin/pd/Source/pdex.%: bin/pd/pdex.%
	@echo CP $@
	@mkdir -p $(dir $@)
	@cp $< $@
bin/pd/%.o : %.c | bin/boot.h
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(pd_cc) -c $(pd_cpflags) -I pd -I bin $(pd_incdir) $< -o $@
bin/pd/%.o : %.s
	@echo AS $@
	@$(pd_as) -c $(pd_asflags) $< -o $@
bin/pd/pdex.elf: $(pd_o) $(pd_lds)
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(pd_cc) $(pd_o) $(pd_ldflags) -o $@
bin/pd/pdex.so: $(pd_src)
	@echo CC $@
	@mkdir -p $(dir $@)
	@gcc -g -shared -fPIC -lm -Dg_tco=0 -DTARGET_SIMULATOR=1 -DTARGET_EXTENSION=1 $(pd_incdir) -o bin/pd/pdex.so $(pd_src)
.PRECIOUS: bin/pd/%elf
