; Driver for the kernel boot gates: boot the headless serial test kernel under qemu,
; capture its serial console (the baked corpus runs at boot and prints the usual
; "N tests pass" summary, then (exit) quits qemu via isa-debug-exit), and report.
; Run on the host love, which has the `hark` subprocess nif.
; argv = (this file firmware arch), and the file picks the door: a .elf boots
; direct (`qemu -kernel`, the PVH door -- no bootloader, no firmware image at
; all, so pass "-" for the firmware); a directory is an esp served as a FAT
; volume (the uefi door -- our own BOOTX64.EFI + love.elf), and takes the
; firmware as pflash. there is no third door, so anything else is a refusal.

(: (sub-at s i t j) (? (>= j (tally t)) 1                    ; does t occur in s at index i?
       (? (= (peep s (+ i j) 0) (peep t j 0)) (sub-at s i t (+ 1 j)) 0))
   (idx s t) ((: (g i) (? (> (+ i (tally t)) (tally s)) -1     ; first index of t in s, else -1
       (? (sub-at s i t 0) i (g (+ 1 i))))) 0)
   (has? s t) (< -1 (idx s t))
   (bol s i)  (? (< i 1) 0 (? (= 10 (peep s (- i 1) 0)) i (bol s (- i 1))))
   (eol s i)  (? (>= i (tally s)) i (? (= 10 (peep s i 0)) i (eol s (+ 1 i))))
   (lineof s t) (: i (idx s t) (? (< i 0) "" (snip s (bol s i) (eol s i)))))

(: a    (cup argv)
   iso  (cap a)
   ovmf (cap (cup a))
   arch (: x (cup (cup a)) (? (two? x) (cap x) "x86_64"))   ; optional 3rd arg; x86_64 by default
   ; optional 4th arg: a line the serial output must also contain -- test_disk's
   ; second boot demands "disk: fat kept across the reset" on top of the summary.
   need (: x (cup (cup (cup a))) (? (two? x) (cap x) ""))
   ; a .elf boots direct: qemu's own loader reads the PVH note and enters our
   ; bring-up stub (mkboot.l) -- nothing downloaded, nothing vendored.
   elf? (: n (tally iso) (&& (> n 4) (sub-at iso (- n 4) ".elf" 0)))
   ; a directory is an esp: qemu fakes a FAT volume over it and the firmware
   ; runs efi/boot/bootx64.efi -- ours, the uefi door. (stat's mode is at 2;
   ; s_ifdir is 0040000 under the 0170000 type mask.)
   dir? (: st (stat iso)                          ; (size mtime mode ..) -- `at` is not bound here
           m (? (two? st) (cap (cup (cup st))) 0)
           (= 16384 (& m 61440)))
   ; machine + exit channel are per-arch. x86_64 quits qemu by writing the
   ; isa-debug-exit port; 'virt' has no such device, so aarch64 goes out through
   ; arm semihosting instead -- see k_qemu_exit in each port's arch.c.
   mach (? (arch = "aarch64")
           ["-M" "virt,gic-version=2" "-cpu" "cortex-a72" "-semihosting"]
           ["-M" "q35" "-device" "isa-debug-exit,iobase=0xf4,iosize=0x04"])
   ; 768M + 420s. the corpus outgrew 256M (a silent death mid-corpus) and then
   ; 512M (";; oom@len=N" at a deterministic dot) -- both the same wall: a major
   ; collection grows the pool by allocating a new contiguous 2x pair while the
   ; old pair still stands, so the ask (~78M at today's corpus) must fit a hole
   ; in ram the old pair fragments -- and whether one fits is a placement
   ; lottery any image-size change re-rolls. the margin here is the mitigation;
   ; the cure would be a pool pair in two blocks. a full green run is ~3min
   ; wall under tcg (the kernel's own summary clock reads ~17s -- don't trust
   ; it for the timeout).
   fw   ["-drive" (+ "if=pflash,unit=0,format=raw,file=" (+ ovmf ",readonly=on"))]
   ; the uefi door only: ovmf parks 64-bit bars above 4G, past every door's
   ; 0..4G map, and the driver then skips the disk (its honest fallback). this
   ; knob turns ovmf's 64-bit mmio window off, so the bar lands reachable and
   ; the disk laws run on this door too.
   below4g ["-fw_cfg" "opt/ovmf/X-PciMmio64Mb,string=0"]
   ; the esp door is the only one handed a framebuffer, and on `virt` nothing
   ; offers one unasked. ramfb, not virtio-gpu: edk2 drives that one through
   ; Blt() and its gop answers a mode with no address, which is no framebuffer
   ; at all. ramfb's lands in ordinary ram, so the console paints at memory
   ; speed and the lane costs nothing.
   gfx  (? (arch = "aarch64") ["-device" "ramfb"] ())
   media (? elf? ["-kernel" iso]
            dir? (fw + (below4g + (gfx + ["-drive" (+ "format=raw,file=fat:rw:" iso)])))
            (scare 'ktest-media iso))
   ; kvm where the host can back it: 7x on this corpus (50s of TCG against 7s).
   ; x86_64-on-x86_64 only -- `virt` is asked for gic-version=2 above, and an
   ; arm host whose gic cannot back v2 refuses the pairing rather than degrading.
   ; a box without /dev/kvm falls to tcg, so the verdict never rides on this.
   kvm  (? (&& (two? (stat "/dev/kvm"))
           (&& (arch = "x86_64") (has? (cup (hark ["uname" "-m"])) "x86_64")))
           ["-enable-kvm" "-cpu" "host"] ())
   ; the scratch disk (rung 5): a sparse raw image beside the artifact, virtio-blk
   ; to the kernel. truncate creates it and leaves an existing one's bytes alone
   ; (same size), so a marker written by one boot survives into the next -- the
   ; persistence gate boots twice on purpose, and test/kernel/disk.l reads which
   ; boot it is off the disk itself. aarch64 rides virtio-mmio, and qemu's virt
   ; slots default to the legacy layout -- force-legacy=false is what makes them
   ; speak version 2, the one blk.c drives.
   disk (+ iso ".disk")
   _    (hark ["truncate" "-s" "35840000" disk])
   dargs (: d ["-drive" (+ "file=" (+ disk ",if=none,format=raw,id=d0"))]
            (? (arch = "aarch64")
               (d + ["-device" "virtio-blk-device,drive=d0"
                     "-global" "virtio-mmio.force-legacy=false"])
               (d + ["-device" "virtio-blk-pci,drive=d0"])))
   r    (hark (cat ["timeout" "420" (+ "qemu-system-" arch) "-m" "768M"]
             (cat mach
             (cat kvm
             (cat dargs
             (cat ["-serial" "stdio" "-display" "none" "-no-reboot"] media))))))
   out  (cup r)
   line (lineof out "tests pass")
   ; "0 tests pass" is a corpus that died before its first assert wearing a
   ; green face (a stale ktests bake did exactly this) -- a summary only counts
   ; when it counted something.
   ok   (&& (has? out "tests pass")
            (&& (! (has? out "failed:"))
            (&& (! (sub-at line 0 "0 tests pass" 0))
                (? (tally need) (has? out need) 1))))
   (? ok (: _ (puts line) (putc 10))
         (: _ (say err "ktest: FAILED") _ (put err 10)
            _ (say err out) _ (put err 10) (quit 1))))
