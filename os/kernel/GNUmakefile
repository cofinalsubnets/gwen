# Nuke built-in rules.
.SUFFIXES:

# This is the name that our final executable will have.
# Change as needed.

# Target architecture to build for. Default to x86_64.
ARCH := x86_64
OUTPUT := kernel.$(ARCH)

include toolchain.mk
include build.mk

# Remove object files and the final executable.
.PHONY: clean
clean:
	rm -rf bin-$(ARCH) obj-$(ARCH)


# Remove everything built and generated including downloaded dependencies.
.PHONY: distclean
distclean:
	rm -rf `git check-ignore * */*`
