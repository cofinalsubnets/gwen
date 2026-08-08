; --- test/kernel/disk.l: the disk (rung 5) -- the virtio driver's raw door,
; then lib/fat.l (off the ramfs, the module walk) mounted on the real device.
; GUARDED on (disk ()): a door whose bus hides the disk skips with a word and
; stays green -- the driver already said why at boot, on the serial console.
; ⚠ the raw probe writes sector 16: inside FAT32's reserved region, which the
; fs below never touches -- the two halves share one disk across REBOOTS, and
; that is the point: tools/kdisk.l boots this corpus twice on one image and
; greps the second log for "disk: fat kept" -- the reset-persistence gate.
(: dn (disk ())
   (? (! dn) (: _ (puts "disk: none here, laws skipped") (putc 10) ())
      (: pat ((: (go b i) (? (< i 512) (: _ (pin b i (% (i * 7) 256)) (go b (i + 1)))
                             (string b)))
              (cask 512) 0)
         _ (assert
            (< 65536 dn)                        ; the gate attaches ~70000 sectors
            (1 = (disk-write 16 pat))
            (pat = (disk-read 16 1))
            (1024 = (tally (disk-read 16 2)))
            !(disk-read dn 1)                   ; past the end: absence
            !(disk-write 3 "abc")               ; not whole sectors: refused
            !(disk-read 0 0))
         ())))
(use 'fat)
(: dev (fat-disk ())
   (? (! (two? dev)) ()
      (: f0 (fat-mount dev)
         seen (? (tablet? f0) (fat-read f0 "boot.txt") ())
         f (? (tablet? f0) f0 (: _ (fat-mkfs dev) (fat-mount dev)))
         _ (? (two? seen)
              (: _ (assert ("the machine remembers" = (cap (cup seen))))
                 _ (puts "disk: fat kept across the reset") (putc 10))
              (: _ (puts "disk: fat born") (putc 10) ()))
         _ (assert (tablet? f))
         _ (assert (1 = (fat-write f "boot.txt" "the machine remembers")))
         _ (assert (1 = (fat-write f "scratch.txt" "abc"))
                   ("abc" = (cap (cup (fat-read f "scratch.txt"))))
                   (two? (cap (cup (fat-ls f ""))))
                   (1 = (fat-rm f "scratch.txt"))
                   !(fat-stat f "scratch.txt"))
         ())))
