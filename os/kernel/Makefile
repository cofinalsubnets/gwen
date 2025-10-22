ARCH?=$(shell uname -m)
src_src=$(addprefix src/, $(notdir $(wildcard ../../src/*.c)))
font_src=$(addprefix font/, $(notdir $(wildcard ../../font/*.c)))
libc_src=$(addprefix libc/, $(notdir $(wildcard ../../libc/*.c)))
os_src=$(notdir $(wildcard src/*.c src/*.S src/*.asm src/arch/$(ARCH).c src/arch/$(ARCH).S src/arch/$(ARCH).asm))
src=$(src_src) $(font_src) $(libc_src) $(os_src)
o=$(addprefix obj-$(ARCH)/, $(patsubst %.S, %.o, $(patsubst %.asm, %.o, $(patsubst %.c, %.o, $(src)))))
NASMFLAGS := -g -F dwarf -Wall -w-reloc-abs-qword -w-reloc-abs-dword -w-reloc-rel-dword
LDFLAGS := -static -nostdlib --gc-sections -T $(ARCH).lds -z max-page-size=0x1000
CFLAGS:=-std=gnu17 -g -O2 -pipe\
	-Wall -Wextra -Wstrict-prototypes -Wno-unused-parameter -Wno-shift-negative-value\
	-falign-functions -fomit-frame-pointer -fno-stack-check -fno-stack-protector\
	-fno-exceptions -fno-asynchronous-unwind-tables -fno-stack-clash-protection\
 	-fcf-protection=none\
	-nostdinc -ffreestanding -fno-lto -fno-PIC -ffunction-sections -fdata-sections
CPPFLAGS := \
    -I src \
		-I ../../bin \
		-I ../../src \
		-I ../../libc \
		-I ../../font \
		-Dg_target=g_target_os \
    -isystem ../../libc \
    $(CPPFLAGS) \
    -DLIMINE_API_REVISION=3
# Architecture specific internal flags.
ifeq ($(ARCH),x86_64)
    ifeq ($(CC_IS_CLANG),1)
        override CC += \
            -target x86_64-unknown-none-elf
    endif
    override CFLAGS += \
        -m64 \
        -march=x86-64 \
        -mabi=sysv \
        -mno-80387 \
        -mno-mmx \
        -mno-sse \
        -mno-sse2 \
        -mno-red-zone \
        -mcmodel=kernel
    override LDFLAGS += \
        -m elf_x86_64
    override NASMFLAGS := \
        -f elf64 \
        $(NASMFLAGS)
endif
ifeq ($(ARCH),aarch64)
    ifeq ($(CC_IS_CLANG),1)
        override CC += \
            -target aarch64-unknown-none-elf
    endif
    override CFLAGS += \
        -mcpu=generic \
        -march=armv8-a+nofp+nosimd \
        -mgeneral-regs-only
    override LDFLAGS += \
        -m aarch64elf
endif
ifeq ($(ARCH),riscv64)
    ifeq ($(CC_IS_CLANG),1)
        override CC += \
            -target riscv64-unknown-none-elf
        override CFLAGS += \
            -march=rv64imac
    else
        override CFLAGS += \
            -march=rv64imac_zicsr_zifencei
    endif
    override CFLAGS += \
        -mabi=lp64 \
        -mno-relax
    override LDFLAGS += \
        -m elf64lriscv \
        --no-relax
endif

ifeq ($(ARCH),loongarch64)
    ifeq ($(CC_IS_CLANG),1)
        override CC += \
            -target loongarch64-unknown-none-elf
    endif
    override CFLAGS += \
        -march=loongarch64 \
        -mabi=lp64s \
        -mfpu=none \
        -msimd=none
    override LDFLAGS += \
        -m elf64loongarch
endif

kernel.$(ARCH): Makefile $(ARCH).lds $o
	@echo LD $@
	@mkdir -p "$(dir $@)"
	@$(LD) $(LDFLAGS) $o -o $@

../../bin/boot.h:
	@make -C ../.. bin/boot.h


obj-$(ARCH)/%.o: ../../%.c Makefile ../../bin/boot.h
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
obj-$(ARCH)/%.o: src/%.c Makefile ../../bin/boot.h
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
obj-$(ARCH)/%.o: src/%.c Makefile ../../bin/boot.h
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
obj-$(ARCH)/cga_8x8.o: ../../font/cga_8x8.c
	@echo CC $@
	@mkdir -p "$(dir $@)"
	@$(CC) -c $< -o $@
obj-$(ARCH)/%.S.o: %.S Makefile ../../bin/boot.h
	@echo AS $@
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
obj-$(ARCH)/$(ARCH).o: src/arch/$(ARCH).asm Makefile ../../bin/boot.h
	@echo AS $@
	@mkdir -p "$(dir $@)"
	@nasm $(NASMFLAGS) $< -o $@

.PHONY: clean distclean
clean:
	rm -rf bin-$(ARCH) obj-$(ARCH)
distclean:
	rm -rf `git check-ignore * */*`
