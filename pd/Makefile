pd_objdir=../bin/pd
pdx=$(pd_objdir)/mitty.pdx
default: $(pdx)

pd_src = $(wildcard *.c) $(wildcard ../g/*.c) $(wildcard ../g/font/*.c)

pd_simcompiler = gcc -g
pd_dylib_flags = -shared -fPIC

TRGT = arm-none-eabi-
GCC:=$(dir $(shell which $(TRGT)gcc))
ifeq ($(GCC),)
GCC = /usr/local/bin/
endif
OJBCPY:=$(dir $(shell which $(TRGT)objcopy))
ifeq ($(OJBCPY),)
OJBCPY = /usr/local/bin/
endif

# Locate the SDK
SDK = $(PLAYDATE_SDK_PATH)
ifeq ($(SDK),)
	SDK = $(shell egrep '^\s*SDKRoot' ~/.Playdate/config | head -n 1 | cut -c9-)
	ifeq ($(SDK),)
		$(error SDK path not found; set ENV value PLAYDATE_SDK_PATH)
	endif
endif

PDCFLAGS = -sdkpath $(SDK)
PDC = $(SDK)/bin/pdc
pd_cc   = $(GCC)$(TRGT)gcc -g3
pd_cp   = $(OJBCPY)$(TRGT)objcopy
pd_as   = $(GCC)$(TRGT)gcc -x assembler-with-cpp
pd_opt = -O2 -falign-functions=16 -fomit-frame-pointer

pd_ldscript=$(patsubst ~%,$(HOME)%,$(SDK)/C_API/buildsupport/link_map.ld)
FPU = -mfloat-abi=hard -mfpu=fpv5-sp-d16 -D__FPU_USED=1

pd_incdir  = $(patsubst %,-I %, . $(SDK)/C_API ../g ../g/font ../bin)
pd_defs=-DTARGET_PLAYDATE=1 -DTARGET_EXTENSION=1 -Dg_target=g_target_pd
HEAP_SIZE      = 8388208
STACK_SIZE     = 4194304
pd_adefs=\
  -D__HEAP_SIZE=$(HEAP_SIZE)\
 	-D__STACK_SIZE=$(STACK_SIZE)
pd_src += $(SDK)/C_API/buildsupport/setup.c
pd_objs    = $(addprefix $(pd_objdir)/, $(pd_src:.c=.o))
pd_mcflags = -mthumb -mcpu=cortex-m7 $(FPU)
pd_asflags  = $(pd_mcflags) $(pd_opt) -g3 -gdwarf-2 -Wa,-amhls=$(<:.s=.lst) $(pd_adefs)
pd_cpflags=\
	$(pd_mcflags) $(pd_opt) $(pd_defs)\
 	-gdwarf-2 -Wall -Wno-unused -Wstrict-prototypes -Wno-unknown-pragmas\
 	-fverbose-asm -Wdouble-promotion -mword-relocations -fno-common\
  -ffunction-sections -fdata-sections -Wa,-ahlms=$(pd_objdir)/$(notdir $(<:.c=.lst))
pd_ldflags=\
	-nostartfiles $(pd_mcflags) -T$(pd_ldscript)\
 	-Wl,-Map=$(pd_objdir)/pdex.map,--cref,--gc-sections,--no-warn-mismatch,--emit-relocs
Source=$(pd_objdir)/Source

$(pdx): $(Source)/pdex.elf $(Source)/pdex.so
	@echo PDC $@
	@$(PDC) $(PDCFLAGS) $(Source) $(pdx)

$(Source)/pdex.elf: $(pd_objdir)/pdex.elf
	@echo CP $@
	@mkdir -p $(dir $@)
	@cp $< $@

$(Source)/pdex.so: $(pd_objdir)/pdex.so
	@echo CP $@
	@mkdir -p $(dir $@)
	@cp $< $@

$(pd_objdir)/%.o : %.c | ../bin/boot.h
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(pd_cc) -c $(pd_cpflags) -I . -I ../bin $(pd_incdir) $< -o $@

$(pd_objdir)/%.o : %.s
	@echo AS $@
	@$(pd_as) -c $(pd_asflags) $< -o $@

.PRECIOUS: $(pd_objdir)/%elf $(pd_objdir)/%bin $(pd_objdir)/%hex
$(pd_objdir)/pdex.elf: $(pd_objs) $(pd_ldscript)
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(pd_cc) $(pd_objs) $(pd_ldflags) -o $@
$(pd_objdir)/pdex.hex: $(pd_objdir)/pdex.elf
	@echo CP $@
	@mkdir -p $(dir $@)
	@$(pd_cp) -O ihex $< $@
$(pd_objdir)/pdex.bin: $(pd_objdir)/pdex.elf
	@echo CP $@
	@mkdir -p $(dir $@)
	@$(pd_cp) -O binary $< $@
$(pd_objdir)/pdex.so: $(pd_src)
	@echo CC $@
	@mkdir -p $(dir $@)
	@$(pd_simcompiler) $(pd_dylib_flags) -lm -Dg_target=g_target_pd -DTARGET_SIMULATOR=1 -DTARGET_EXTENSION=1 $(pd_incdir) -o $(pd_objdir)/pdex.so $(pd_src)
clean:
	@echo CLEAN
	@rm -rf $(pd_objdir)
../bin/boot.h:
	@make -C .. bin/boot.h
