; --- test/host/fat.l: FAT32 in love (lib/fat.l) over a cask device -- the same
; module the kernel mounts on its virtio disk (rung 5), proven here where the
; laws run in milliseconds and mtools can sit on the other side of the format.
; run standalone from the repo root:
;   cat test/00-init.l test/host/fat.l | out/host/love

(use 'fat)

; the device: 70000 sectors in one cask. rd answers fresh strings, wr pours.
(: img (cask (* 70000 512))
   (mkdev im) `((\ lba n (: b (cask (n * 512)) _ (pour b 0 im (lba * 512) (n * 512)) (string b)))
                (\ lba s (: _ (pour im (lba * 512) s 0 (tally s)) ((tally s) // 512)))
                (// (tally im) 512))
   dev (mkdev img))

; mkfs: a whole format answers 1; a device under FAT32's own floor refuses
(assert
 (1 = (fat-mkfs dev))
 !(fat-mkfs (mkdev (cask (* 1000 512)))))

(: f (fat-mount dev))

; mount: presence, and the honest refusals
(assert
 (tablet? f)
 !(fat-mount (mkdev (cask (* 70000 512))))            ; unformatted: no signature
 (two? (fat-ls f "")) (! (cap (cup (fat-ls f ""))))   ; the fresh root: (1 ())
 !(fat-ls f "nope") !(fat-read f "nope") !(fat-stat f "nope"))

; files: write, list, stat, read -- and every answer wears the (1 _) wrapper
(assert
 (1 = (fat-write f "hello.txt" "hello disk"))
 ('("hello.txt") = (cap (cup (fat-ls f ""))))
 ('(1 10 32) = (fat-stat f "hello.txt"))
 ("hello disk" = (cap (cup (fat-read f "hello.txt"))))
 ; the empty file is a real answer: (1 ""), never ()
 (1 = (fat-write f "empty" ""))
 (two? (fat-read f "empty")) ("" = (cap (cup (fat-read f "empty"))))
 ('(1 0 32) = (fat-stat f "empty")))

; replace: longer, then shorter; the size follows, the old chain is freed
(: big ((: (go i acc) (? (< i 200) (go (i + 1) (acc + "0123456789")) acc)) 0 ""))
(assert
 (1 = (fat-write f "hello.txt" big))                  ; 2000 bytes: a real chain
 (2000 = (cap (cup (fat-stat f "hello.txt"))))
 (big = (cap (cup (fat-read f "hello.txt"))))
 (1 = (fat-write f "hello.txt" "short"))
 ("short" = (cap (cup (fat-read f "hello.txt")))))

; long names: LFN written and read back, case-blind like everything here
(assert
 (1 = (fat-write f "limine.conf" "timeout: 1"))
 ("timeout: 1" = (cap (cup (fat-read f "LIMINE.CONF"))))
 (1 = (fat-write f "a rather long name indeed.text" "lfn"))
 ("lfn" = (cap (cup (fat-read f "a rather long name indeed.text"))))
 (member? "limine.conf" (cap (cup (fat-ls f "")))))

; directories: mkdir, files inside, nesting; rm insists a dir be empty first
(assert
 (1 = (fat-mkdir f "boot"))
 (1 = (fat-write f "boot/love.elf" "ELF!"))
 ("ELF!" = (cap (cup (fat-read f "/boot/love.elf"))))
 ('("love.elf") = (cap (cup (fat-ls f "boot"))))
 (1 = (fat-mkdir f "boot/deep"))
 (1 = (fat-write f "boot/deep/x" "y"))
 ("y" = (cap (cup (fat-read f "boot/deep/x"))))
 !(fat-write f "gone/x" "y")                          ; an absent parent refuses
 !(fat-rm f "boot")                                   ; not empty
 (1 = (fat-rm f "boot/deep/x"))
 (1 = (fat-rm f "boot/deep"))
 (1 = (fat-rm f "hello.txt"))
 !(fat-stat f "hello.txt"))

; persistence is the point: a FRESH mount over the same bytes reads it all back
(: f2 (fat-mount dev))
(assert
 (tablet? f2)
 ("timeout: 1" = (cap (cup (fat-read f2 "limine.conf"))))
 ("ELF!" = (cap (cup (fat-read f2 "boot/love.elf"))))
 ("" = (cap (cup (fat-read f2 "empty")))))

; --- the other side of the format: mtools reads what we wrote, we read what it
; wrote. guarded on mdir being installed (the hdd lane already leans on it).
(: (lines* s) (: n (tally s)
                 (go i w acc) (? (<= n i) (rev (? (tally w) (link w acc) acc))
                                 (? (= (s i) 10) (go (+ i 1) "" (link w acc))
                                    (go (+ i 1) (+ w (string `((s i)))) acc)))
                 (go 0 "" ()))
   have-mtools (: r (hark '("sh" "-c" "command -v mdir >/dev/null && echo yes"))
                  (&& (! (cap r)) (< 0 (tally (cup r)))))
   _ (? have-mtools
        (: ip "out/.fat-probe.img"
           _ (: p (open ip "w") _ (say p (string img)) (close p))
           md (hark `("mdir" "-i" ip "-b" "::/"))
           mt (hark `("mtype" "-i" ip "::/limine.conf"))
           lp "out/.fat-probe.txt"
           _ (: p (open lp "w") _ (say p "from the host side") (close p))
           mc (hark `("mcopy" "-i" ip lp "::/fromhost.txt"))
           s (: q (open ip "r") t (slurp q) _ (close q) t)
           im2 (cask (tally s))
           _ (pour im2 0 s 0 (tally s))
           f3 (fat-mount (mkdev im2))
           (assert
            (! (cap md)) (! (cap mt)) (! (cap mc))
            (member? "::/limine.conf" (lines* (cup md)))
            ("timeout: 1" = (cup mt))
            (tablet? f3)
            ("from the host side" = (cap (cup (fat-read f3 "fromhost.txt"))))
            (: _ (unlink ip) _ (unlink lp) 1)))
        (puts "  (mtools interop skipped: no mdir here)")))

(test_fin ())
(puts "fat: ok") (putc 10)
