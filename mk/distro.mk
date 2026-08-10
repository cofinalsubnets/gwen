# mk/distro.mk -- the love-native Linux distro: an initramfs where love is /init. The LFS
# toolchain phase is already solved elsewhere (a gcc-free static love, `make test_raw`), so
# a bootable system is just PACKAGING what is green: love as pid 1, kore as the
# busybox-style userland, lush as the console shell.
#
#   make distro-initramfs   -> out/distro/initramfs.cpio.gz
#   make distro-run         -> boot it under qemu on a stock kernel
#
# The kernel stays the one imported artifact (BZIMAGE, default the host's).

distro_dir   = out/distro
distro_root  = $(distro_dir)/root
distro_img   = $(distro_dir)/initramfs.cpio.gz
# ⚠ the base love MUST be static -- a bare initramfs has no ld.so or glibc. Prefer the
# gcc-free love-raw, the true love base; fall back to a static-musl host love.
distro_love    = $(firstword $(wildcard out/host/love-raw out/host-musl/love))
# kore applets to expose as argv[0] symlinks (kore dispatches on the basename).
distro_applets = ls cat head tail wc sort uniq grep sed cut tr nl rev cp mv rm \
                 mkdir rmdir ln touch pwd chmod basename dirname seq yes true \
                 false env sleep kill xargs diff

# The host kernel is the default imported artifact; override with `make BZIMAGE=...`.
BZIMAGE ?= /boot/vmlinuz-linux

.PHONY: distro-initramfs distro-run distro-smoke
distro-initramfs: $(distro_img)
$(distro_img): init/boot.l $(lushfiles) $(korefiles) $(distro_love)
	@test -n "$(distro_love)" || { echo "distro: need a STATIC love -- run 'make test_raw' (love-raw) or 'make STATIC=1'"; exit 1; }
	@echo DISTRO	$(abspath $@)  '(base: $(distro_love))'
	@rm -rf $(distro_root)
	@mkdir -p $(distro_root)/bin $(distro_root)/lib $(distro_root)/proc $(distro_root)/sys $(distro_root)/dev $(distro_root)/tmp
	@cp init/boot.l $(distro_root)/init && chmod 755 $(distro_root)/init
	@cp $(distro_love) $(distro_root)/bin/love && chmod 755 $(distro_root)/bin/love
	@ln -sf love $(distro_root)/bin/ai
	@cat $(lushfiles) > $(distro_root)/lib/sh.l
	@{ echo '#!/bin/love'; cat $(korefiles); } > $(distro_root)/bin/kore && chmod 755 $(distro_root)/bin/kore
	@for a in $(distro_applets); do ln -sf kore $(distro_root)/bin/$$a; done
	@ln -sf kore $(distro_root)/bin/sh
	@ln -sf kore $(distro_root)/bin/lush
	@( cd $(distro_root) && find . | cpio --quiet -o -H newc ) | gzip -9 > $@
	@echo "  packed $$(gzip -l $@ | awk 'NR==2{print $$2}') bytes -> $@"

# Direct kernel boot, no bootloader: rdinit=/init makes love pid 1. KVM when the host
# offers it -- TCG is too slow to reach the console inside a smoke window.
distro_accel = $(shell test -e /dev/kvm && echo -enable-kvm -cpu host)
# ⚠ 2G: love reserves a two-space GC heap at startup, so pid1 love PLUS a forked child
# each need one -- 512M overflows (execve -> ENOMEM). Override with QMEM=.
QMEM ?= 2048
distro_qemu = qemu-system-x86_64 -m $(QMEM) $(distro_accel) -kernel $(BZIMAGE) -initrd $(distro_img) \
              -append "console=ttyS0 earlyprintk=serial,ttyS0 rdinit=/init panic=-1" \
              -serial stdio -display none -no-reboot
distro-run: $(distro_img)
	@test -r "$(BZIMAGE)" || { echo "distro-run: no kernel at $(BZIMAGE) -- set BZIMAGE=..."; exit 1; }
	exec $(distro_qemu)

# Non-interactive smoke: boot, feed `ls /proc` to the console, prove love came up as pid 1
# with /proc mounted and the kore userland running, then kill qemu. ⚠ the trailing sleep
# holds stdin open, keeping the shell out of an EOF-respawn loop.
distro-smoke: $(distro_img)
	@test -r "$(BZIMAGE)" || { echo "distro-smoke: no kernel at $(BZIMAGE)"; exit 1; }
	@echo "-- booting (10s capture) --"
	@( printf 'ls /proc\n'; sleep 8 ) | timeout 10 $(distro_qemu) 2>/dev/null | tee $(distro_dir)/boot.log || true
	@echo "-- checks --"
	@grep -q "love as PID 1" $(distro_dir)/boot.log && echo "  OK love is pid 1" || { echo "  FAIL not pid 1"; exit 1; }
	@grep -q "mount proc on /proc -> ok" $(distro_dir)/boot.log && echo "  OK /proc mounted" || { echo "  FAIL /proc"; exit 1; }
	@grep -q "start the console shell" $(distro_dir)/boot.log && echo "  OK reached shell handoff" || { echo "  FAIL no shell"; exit 1; }
	@grep -qE "kore ls /proc -> exit 0, [1-9]" $(distro_dir)/boot.log && echo "  OK kore userland runs (ls /proc, forked+captured)" || { echo "  FAIL userland self-check"; exit 1; }
	@echo "-- distro smoke passed --"
