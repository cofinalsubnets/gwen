; Driver for `make test_kernel`: boot the headless serial test kernel under qemu,
; capture its serial console (the baked corpus runs at boot and prints the usual
; "N tests pass" summary, then (exit) quits qemu via isa-debug-exit), and report.
; Run on the host love, which has the `run` subprocess nif.
; argv = (this file firmware arch): a .elf file boots DIRECT (`qemu -kernel`, the
; PVH door -- no bootloader, no firmware image, firmware arg ignored, pass "-");
; anything else is the iso lane (limine cdrom + the firmware as pflash).

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
   ; a .elf boots direct: qemu's own loader reads the PVH note and enters our
   ; boot.S stub -- nothing downloaded, nothing vendored.
   elf? (: n (tally iso) (&& (> n 4) (sub-at iso (- n 4) ".elf" 0)))
   ; machine + exit channel are per-arch. x86_64 quits qemu by writing the
   ; isa-debug-exit port; 'virt' has no such device, so aarch64 goes out through
   ; ARM semihosting instead -- see k_qemu_exit in each port's arch.c.
   mach (? (arch = "aarch64")
           `("-M" "virt,gic-version=2" "-cpu" "cortex-a72" "-semihosting")
           `("-M" "q35" "-device" "isa-debug-exit,iobase=0xf4,iosize=0x04"))
   ; 512M + 420s: the corpus outgrew the old 256M/90s budgets. under 256M the
   ; kernel died SILENTLY mid-corpus at a deterministic dot, no fault text, the
   ; crash point MOVING with unrelated image growth (a budget wall's face, not
   ; a logic bug); and a full green run is ~3min wall under TCG (the kernel's
   ; own summary clock reads ~17s -- don't trust it for the timeout).
   media (? elf?
            `("-kernel" iso)
            `("-drive" (+ "if=pflash,unit=0,format=raw,file=" (+ ovmf ",readonly=on"))
              "-cdrom" iso))
   r    (run (cat `("timeout" "420" (+ "qemu-system-" arch) "-m" "512M")
             (cat mach
             (cat `("-serial" "stdio" "-display" "none" "-no-reboot") media))))
   out  (cup r)
   ok   (&& (has? out "tests pass") (! (has? out "failed:")))
   (? ok (: _ (puts (lineof out "tests pass")) (putc 10))
         (: _ (say err "test_kernel: FAILED") _ (put err 10)
            _ (say err out) _ (put err 10) (quit 1))))
