# mk/distro.mk -- the love-native Linux distro: an initramfs where love is /init. The LFS
# toolchain phase is already solved elsewhere (a gcc-free static love, `make test_raw`), so
# a bootable system is just PACKAGING what is green: love as pid 1, kore as the
# busybox-style userland, lush as the console shell.
#
#   make distro-initramfs   -> out/distro/initramfs.cpio.gz
#   make distro-run         -> boot it under qemu on a stock kernel
#
# The kernel stays the one imported artifact (BZIMAGE, default the host's).

# ⚠ THE CUT IS OURS END TO END NOW -- kore's find, lib/cpio.l and lib/gz.l where the
# host's find | cpio | gzip -9 stood. $(mabs) because the pack runs INSIDE a `cd`, and
# $m is spelled relative to the tree root.
mabs         = $(abspath $m)
distro_dir   = out/distro
distro_root  = $(distro_dir)/root
distro_img   = $(distro_dir)/initramfs.cpio.gz
# ⚠ the base love MUST be static -- a bare initramfs has no ld.so or glibc. love-raw is it:
# gcc-free, our own linker over nolibc, and 935K against the baked love's ~11M, which is
# what an initramfs wants carried into RAM. `make test_raw` lays it.
distro_love    = $(wildcard out/host/love-raw)
# kore applets to expose as argv[0] symlinks (kore dispatches on the basename).
distro_applets = ls cat head tail wc sort uniq grep sed awk find cut tr nl rev cp mv rm \
                 mkdir rmdir ln touch pwd chmod basename dirname seq yes true \
                 false env sleep kill xargs diff

# The host kernel is the default imported artifact; override with `make BZIMAGE=...`.
BZIMAGE ?= /boot/vmlinuz-linux

.PHONY: distro-initramfs distro-run distro-smoke
distro-initramfs: $(distro_img)
$(distro_img): crew/init/boot.l $(lushfiles) $(korefiles) $(distro_love)
	@test -n "$(distro_love)" || { echo "distro: no out/host/love-raw -- run 'make test_raw' to lay it"; exit 1; }
	@echo DISTRO	$(abspath $@)  '(base: $(distro_love))'
	@rm -rf $(distro_root)
	@mkdir -p $(distro_root)/bin $(distro_root)/lib $(distro_root)/proc $(distro_root)/sys $(distro_root)/dev $(distro_root)/tmp
	@cp crew/init/boot.l $(distro_root)/init && chmod 755 $(distro_root)/init
	@cp $(distro_love) $(distro_root)/bin/love && chmod 755 $(distro_root)/bin/love
	@cat $(lushfiles) > $(distro_root)/lib/sh.l
# ⚠ lib/dns.l RIDES ALONG OR THE WHOLE TOOLBOX DIES: mk/tools/ain.l, a korefiles member,
# probes for the `dial` nif at load and says (use 'dns) when it is absent -- which it is
# in love-raw -- and an initramfs with no /lib/dns.l answers that with a scare that takes
# the whole cat down. The symptom is every applet gone, not a quiet nc.
	@cp lib/dns.l $(distro_root)/lib/dns.l
	@{ echo '#!/bin/love'; cat $(korefiles); } > $(distro_root)/bin/kore && chmod 755 $(distro_root)/bin/kore
	@for a in $(distro_applets); do ln -sf kore $(distro_root)/bin/$$a; done
	@ln -sf kore $(distro_root)/bin/sh
	@ln -sf kore $(distro_root)/bin/lush
	@( cd $(distro_root) && $(mabs) kore find . | $(mabs) cpio -o --quiet ) | $(mabs) gzip > $@
	@echo "  packed $$($(mabs) gzip -l $@ | $(mabs) kore awk 'NR==2{print $$2}') bytes -> $@"

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
