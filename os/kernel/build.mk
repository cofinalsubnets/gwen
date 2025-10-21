src_dirs=src cc-runtime/src ../../src ../../libc ../../font 

# Use "find" to glob all *.c, *.S, and *.asm files in the tree
# (except the src/arch/* directories, as those are gonna be added
# in the next step).
SRCFILES := \
	$(shell find -L $(src_dirs) -type f -not -path 'src/arch/*' 2>/dev/null | LC_ALL=C sort)\
  $(wildcard src/arch/$(ARCH).*)

# Obtain the object and header dependencies file names.
CFILES := $(filter %.c,$(SRCFILES))
ASFILES := $(filter %.S,$(SRCFILES))
NASMFILES := $(filter %.asm,$(SRCFILES))
OBJ := $(addprefix obj-$(ARCH)/,$(CFILES:.c=.c.o) $(ASFILES:.S=.S.o) $(NASMFILES:.asm=.asm.o))
HEADER_DEPS := $(addprefix obj-$(ARCH)/,$(CFILES:.c=.c.d) $(ASFILES:.S=.S.d))

# Default target. This must come first, before header dependencies.
.PHONY: all
all: $(OUTPUT)

# Include header dependencies.
-include $(HEADER_DEPS)

# Link rules for the final executable.
$(OUTPUT): GNUmakefile $(ARCH).lds $(OBJ)
	mkdir -p "$(dir $@)"
	$(LD) $(LDFLAGS) $(OBJ) -o $@

# Compilation rules for *.c files.
obj-$(ARCH)/%.c.o: %.c GNUmakefile ../../src/boot.h $(DEPS)
	mkdir -p "$(dir $@)"
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

../../src/boot.h:
	make -C ../.. src/boot.h

# Compilation rules for *.S files.
obj-$(ARCH)/%.S.o: %.S GNUmakefile ../../src/boot.h
	mkdir -p "$(dir $@)"
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

ifeq ($(ARCH),x86_64)
# Compilation rules for *.asm (nasm) files.
obj-$(ARCH)/%.asm.o: %.asm GNUmakefile ../../src/boot.h
	mkdir -p "$(dir $@)"
	nasm $(NASMFLAGS) $< -o $@

endif
