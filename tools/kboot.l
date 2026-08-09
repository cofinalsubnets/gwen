; Driver for `make test_kboot` (inle rung 3): boot the SHIPPED x86_64 kernel
; direct (`qemu -kernel`, the PVH door) with a boot command line, capture the
; serial console, and ask that every expected substring made it out. The seated
; tool runs off the baked kore cat and quits -- the reset door, which -no-reboot
; turns into a qemu exit; a boot that reaches the shell instead just times out
; and reports. Run on the host love (the `hark` nif). argv = (this elf append expect..).

; ⚠ the helpers live in their OWN form (ktest.l's structure) on purpose: folding
; them into the driver's `:` below segfaults the host -- defining sub-at/has? and
; CALLING has? inside one letrec blows the C stack (a core bug, 2026-08-07 open).
(: (sub-at s i t j) (? (>= j (tally t)) 1                    ; does t occur in s at index i?
       (? (= (peep s (+ i j) 0) (peep t j 0)) (sub-at s i t (+ 1 j)) 0))
   (has? s t) ((: (g i) (? (> (+ i (tally t)) (tally s)) 0
       (? (sub-at s i t 0) 1 (g (+ 1 i))))) 0))

(: a    (cup argv)
   elf  (cap a)
   app  (cap (cup a))
   want (cup (cup a))
   ; KVM where the host can back it -- this lane is x86_64 only, so the arch
   ; question is just the host's. Without /dev/kvm it falls to TCG, same verdict.
   kvm  (? (&& (two? (stat "/dev/kvm")) (has? (cup (hark `("uname" "-m"))) "x86_64"))
           `("-enable-kvm" "-cpu" "host") ())
   r    (hark (cat `("timeout" "420" "qemu-system-x86_64" "-m" "512M" "-M" "q35")
              (cat kvm
                   `("-kernel" elf "-append" app
                     "-serial" "stdio" "-display" "none" "-no-reboot"))))
   out  (cup r)
   miss ((: (g w acc) (? (! (two? w)) (rev acc)
                         (g (cup w) (? (has? out (cap w)) acc (link (cap w) acc))))) want ())
   (? (! (two? miss))
      (: _ (puts (+ "kboot ok: " app)) (putc 10))
      (: _ (say err (+ "test_kboot FAILED: " app)) _ (put err 10)
         _ (each miss (\ m (: _ (say err (+ "  missing: " m)) (put err 10))))
         _ (say err out) _ (put err 10) (quit 1))))
