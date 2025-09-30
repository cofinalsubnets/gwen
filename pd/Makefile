HEAP_SIZE      = 8388208
STACK_SIZE     = 61800

PRODUCT = gw.pdx

# Locate the SDK
SDK = ${PLAYDATE_SDK_PATH}
ifeq ($(SDK),)
	SDK = $(shell egrep '^\s*SDKRoot' ~/.Playdate/config | head -n 1 | cut -c9-)
endif

ifeq ($(SDK),)
$(error SDK path not found; set ENV value PLAYDATE_SDK_PATH)
endif

######
# IMPORTANT: You must add your source folders to VPATH for make to find them
# ex: VPATH += src1:src2
######

VPATH += ../src/

# List C source files here
SRC = $(wildcard *.c) $(wildcard ../src/*.c)

# List all user directories here
UINCDIR = ../src/

# List user asm files
UASRC = 

# List all user C define here, like -D_DEBUG=1
UDEFS = -Dg_target=g_target_pd

# Define ASM defines here
UADEFS = 

# List the user directory to look for the libraries here
ULIBDIR =

# List all user libraries here
ULIBS =

include $(SDK)/C_API/buildsupport/common.mk

