; port/teensy41/mkimg.l -- wrap the baked heap image as an object, in place of
; `ld -r -b binary`. port/mps2's qemu baker dumps love.img (a fully-symbolic heap
; snapshot); the wake copies the blob into the PSRAM major pool, so it rides in
; flash whole and main.c reads it between two brackets.
;
; ld -b binary spells those brackets off the FILE NAME -- _binary_love_img_start /
; _end, mangled from `love.img`. this lays them by hand under the same two noms,
; because main.c says them and a rename is not what this rung is about.
;
; ⚠ the section is ALLOC-only PROGBITS on an 8-byte grain: the image header carries
; uint64 fields, and the wake reads them in place before any copy.
;
; runs after crew/holo/{elf,obj}.l in a cat with the thumb2 backend spliced into the
; sealed holo module; port/teensy41/Makefile drives it.
(: (mkimg src dst)
   (: r (uread src)
      _ (? (two? r) 0 (scare 'mkimg-no-image src))     ; presence out of band: a 0-byte
      s <>r  n (tally s)                               ; image reads the same as none
      _ (? (0 < n) 0 (scare 'mkimg-empty src))
      ir [['label '_binary_love_img_start]
          ['raw (map (\ i (s i)) (jot n))]
          ['label '_binary_love_img_end]]
      (write-bytes dst
       (objsecs32 'thumb2 [[".love.image" 1 2 8 ir]] ()
        '("_binary_love_img_start" "_binary_love_img_end") ()))))
