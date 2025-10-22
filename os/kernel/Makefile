ARCH?=$(shell uname -m)
OUTPUT=kernel.$(ARCH)
include toolchain.mk

# Obtain the object and header dependencies file names.
src_c=$(addprefix src/, $(notdir $(wildcard ../../src/*.c)))
font_c=$(addprefix font/, $(notdir $(wildcard ../../font/*.c)))
libc_c=$(addprefix libc/, $(notdir $(wildcard ../../libc/*.c)))
os_c_asm=$(notdir $(wildcard src/*.c src/*.S src/*.asm src/arch/$(ARCH).c src/arch/$(ARCH).S src/arch/$(ARCH).asm))
srcfiles=$(src_c) $(font_c) $(libc_c) $(os_c_asm)
OBJ=$(addprefix obj-$(ARCH)/, $(patsubst %.S, %.o, $(patsubst %.asm, %.o, $(patsubst %.c, %.o, $(srcfiles)))))

$(OUTPUT): Makefile $(ARCH).lds $(OBJ)
	@echo LD -o $@ $(OBJ)
	@mkdir -p "$(dir $@)"
	@$(LD) $(LDFLAGS) $(OBJ) -o $@


# Link rules for the final executable.

# Compilation rules for *.c files.
obj-$(ARCH)/%.o: ../../%.c Makefile ../../bin/boot.h $(DEPS)
	@echo CC -o $@ $<
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
obj-$(ARCH)/%.o: src/%.c Makefile ../../bin/boot.h $(DEPS)
	@echo CC -o $@ $<
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@
obj-$(ARCH)/%.o: src/%.c Makefile ../../bin/boot.h $(DEPS)
	@echo CC -o $@ $<
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

../../bin/boot.h:
	make -C ../.. bin/boot.h

obj-$(ARCH)/cga_8x8.o: ../../font/cga_8x8.c
	@echo CC -o $@ $<
	@mkdir -p "$(dir $@)"
	@$(CC) -c $< -o $@

# Compilation rules for *.S files.
obj-$(ARCH)/%.S.o: %.S Makefile ../../bin/boot.h
	@echo AS -o $@ $<
	@mkdir -p "$(dir $@)"
	@$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

# Compilation rules for *.asm (nasm) files.
obj-$(ARCH)/$(ARCH).o: src/arch/$(ARCH).asm Makefile ../../bin/boot.h
	@echo AS -o $@ $<
	@mkdir -p "$(dir $@)"
	@nasm $(NASMFLAGS) $< -o $@



.PHONY: clean distclean
clean:
	rm -rf bin-$(ARCH) obj-$(ARCH)
distclean:
	rm -rf `git check-ignore * */*`
