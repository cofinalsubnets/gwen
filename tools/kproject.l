; tools/kproject.l -- the metal projection (plan C2). the artifact is ONE
; hosted -pie love; a boot door eats a fixed-address image. this tool derives
; the second from the first: re-base every PT_LOAD at the kernel base, apply
; the ai_rela table there (the same law nolibc's __ai_reloc runs at a hosted
; start -- run ahead of time, since on metal nobody is there to run it), lay
; and patch boot.o below the image, write k_image_top into the file, and emit
; the flat ELF the doors have always eaten. cat after crew/holo/{elf,obj,link}.l:
;
;   love <cat> IN.pie BOOT.o OUT.elf ARCH
;
; (kproject pie bootb out arch) is the callable half -- pie and boot.o as
; BYTES -- which is how the `love kernel` verb projects the running binary
; over a boot.o it laid itself (lib/source.l).

(: (kparams arch)                       ; target, kernel base, entry symbol
    (? (arch = "x86_64")  ['x64   0x200000   "kmain"]
       (arch = "aarch64") ['arm64 0x40200000 "a64boot"]
       (scare 'kproject-arch arch))
   (kread p) (: r (uread p) (two? r ? [p <>r] (scare 'kproject-open p)))

   (kproject pie bootb out arch)
   (: kp (kparams arch)
      tgt (at kp 0) kbase (at kp 1) entnm (at kp 2)
      mach (peep elf-machines tgt 0)

      ; --- the pie, parsed: program headers, the named sections, the symtab --
      phoff (rle64 pie 32) shoff (rle64 pie 40)
      phnum (rle16 pie 56) shnum (rle16 pie 60) shstrx (rle16 pie 62)
      (pho i) (phoff + (i * 56))
      ; each load: (vaddr fileoff filesz memsz flags)
      loads ((: (go i acc)
                 (? (phnum <= i) (rev acc)
                    (go (i + 1)
                        (? (rle32 pie (pho i) = 1)
                           ([(rle64 pie (pho i + 16)) (rle64 pie (pho i + 8))
                             (rle64 pie (pho i + 32)) (rle64 pie (pho i + 40))
                             (rle32 pie (pho i + 4))] >< acc)
                           acc)))) 0 ())
      (sho i) (shoff + (i * 64))
      shstro (rle64 pie (sho shstrx + 24))
      (sname i) (ld-cstr pie (shstro + rle32 pie (sho i)))
      (shfind nm) ((: (go i) (? (shnum <= i) (scare 'kproject-section nm)
                                (sname i = nm) i (go (i + 1)))) 1)
      relo (: x (shfind ".love_rela") (rle64 pie (sho x + 24)))
      relz (: x (shfind ".love_rela") (rle64 pie (sho x + 32)))
      symo (: x (shfind ".symtab") (rle64 pie (sho x + 24)))
      symz (: x (shfind ".symtab") (rle64 pie (sho x + 32)))
      syminf (: x (shfind ".symtab") (rle32 pie (sho x + 44)))
      stro (: x (shfind ".strtab") (rle64 pie (sho x + 24)))
      strz (: x (shfind ".strtab") (rle64 pie (sho x + 32)))
      (pie-sym nm) ((: (go e)                    ; a defined global's value, or ()
                        (? ((symo + symz) <= e) ()
                           (: sx (rle16 pie (e + 6))
                              (? (&& (0 < sx) (sx < 0xff00)
                                     (ld-cstr pie (stro + rle32 pie e) = nm))
                                 (rle64 pie (e + 8))
                                 (go (e + 24)))))) symo)
      (va2off v) ((: (go l)                      ; a vaddr's file offset, through its load
                      (? !(two? l) (scare 'kproject-va v)
                         (: g <l
                            (? (&& ((at g 0) <= v) (v < (at g 0 + at g 2)))
                               (v - at g 0 + at g 1)
                               (go >l))))) loads)

      ; --- boot.o, laid below the image: ldkern's own head, one object -------
      objs `((ld-read "boot.o" bootb mach))
      ne (ldlay1 'note kbase objs)
      b0 (ld-align 4096 ne) b1 (ldlay1 'boot b0 objs)
      t1 (ldlayn 'text (ldlay1 'text b1 objs) objs)
      o1 (ldlayn 'rodata (ldlay1 'rodata t1 objs) objs)
      d1 (ldlayn 'data (ldlay1 'data o1 objs) objs)
      bootend (ldlayn 'bss (ldlay1 'bss d1 objs) objs)
      bias (ld-align 4096 bootend)
      top ((: (go l m) (? !(two? l) m
                          (go >l (: e (bias + at <l 0 + at <l 3) (? (m < e) e m)))))
           loads 0)
      glo (ldsyms objs)
      ; the pie's globals join the book (boot's own definitions stand), so a
      ; boot reloc that names a kernel symbol resolves at its metal address
      _ ((: (go e)
             (? ((symo + symz) <= e) 0
                (: sx (rle16 pie (e + 6)) bind ((pie (e + 4)) >> 4)
                   _ (? (&& (0 < sx) (sx < 0xff00) (member? bind '(1 2)))
                        (: nm (ld-cstr pie (stro + rle32 pie e))
                           (? (two? (peep glo nm ())) 0
                              (pin glo nm `((bias + rle64 pie (e + 8)) 0))))
                        0)
                   (go (e + 24))))) symo)
      eaddr (: r (peep glo entnm ())
               (? (two? r) <r (scare 'kproject-entry entnm)))
      cx `(glo b0 b0 b0 b0 () 0 () () (ld-extents objs))

      ; --- the patch list: every ai_rela site slides by bias, and k_image_top
      ; takes the image's far edge -- ascending, for the one-pass surgery below
      ; (the table already lays ascending; the walk reverses it back)
      (ins s l) (? !(two? l) `(s)
                   ((at <l 0) < (at s 0)) (<l >< (ins s >l))
                   (s >< l))
      sites (ins [(pie-sym "k_image_top") top]
                 (rev ((: (go k acc)
                           (? (relz <= k) acc
                              (: v (rle64 pie (relo + k))
                                 (go (k + 8) ([v (bias + rle64 pie (va2off v))] >< acc)))))
                      0 ())))
      ; one load's bytes with its patches spliced in
      (spliced go0 va fsz ss)
       ((: (go c l acc)
            (? !(two? l) (rev ((snip pie c (go0 + fsz)) >< acc))
               (: o (go0 + (at <l 0 - va))
                  (go (o + 8) >l ((le64 (at <l 1)) >< ((snip pie c o) >< acc))))))
        go0 (filter (\ s (&& (va <= at s 0) (at s 0 < (va + fsz)))) ss) ())

      ; --- the output shape: ldkern's -- header page (ehdr, phdrs, the note),
      ; boot at its page, then the re-based image; symtab past the bytes ------
      nsz (foldl (\ a ob (a + ld-lanesz ob 'note)) 0 objs)
      nph (2 + tally loads - (? (0 < nsz) 0 1))
      hdrsz (64 + (56 * nph))
      _ (? ((hdrsz + nsz) <= 4096) 0 (scare 'kproject-headers hdrsz))
      (fo a) ((a - kbase) + 4096)
      phs (: bs `(1 7 (fo b0) b0 b0 (d1 - b0) (bootend - b0) 4096)
             ps (map (\ g `(1 (at g 4) (fo (bias + at g 0)) (bias + at g 0)
                            (bias + at g 0) (at g 2) (at g 3) 4096))
                     loads)
             (? (0 < nsz) (`(4 4 hdrsz kbase kbase nsz nsz 4) >< (bs >< ps))
                (bs >< ps)))
      en (ldemit cx 'note kbase objs ())
      (gapz a b) (? (a < b) `((ld-zeros (b - a))) ())
      ; ⚠ the file is address-linear from kbase plus one page (fo), so the boot
      ; chain is seeded with the kbase..b0 gap -- without it every byte lands
      ; one page below where its phdr points. each stage takes the PIECES of
      ; the one before (never the [cur pieces] pair), bridged by its gap; bss
      ; owns address space and no bytes, so it is laid above and never emitted.
      eb (ldemit cx 'boot b0 objs `((ld-zeros (b0 - kbase))))
      et (ldemitn cx 'text objs (ldemit cx 'text b1 objs (>eb + (gapz <eb b1))))
      eo (ldemitn cx 'rodata objs (ldemit cx 'rodata t1 objs (>et + (gapz <et t1))))
      ed (ldemitn cx 'data objs (ldemit cx 'data o1 objs (>eo + (gapz <eo o1))))
      ; the image bytes, each load at its congruent file spot
      body ((: (go l c acc)
                (? !(two? l) [c acc]
                   (: g <l o (fo (bias + at g 0))
                      (go >l (o + at g 2)
                          (acc + (`((ld-zeros (o - c))) + spliced (at g 1) (at g 0) (at g 2) sites))))))
           loads (fo d1) ())
      fsz <body
      ; the symtab, values slid: the uefi loader reads `kboot` out of exactly this
      esy ((: (go e acc)
               (? ((symo + symz) <= e) (rev acc)
                  (: sx (rle16 pie (e + 6))
                     (go (e + 24)
                         (? (&& (0 < sx) (sx < 0xff00))
                            ((snip pie (e + 16) (e + 24)) ><
                             ((le64 (bias + rle64 pie (e + 8))) ><
                              ((snip pie e (e + 8)) >< acc)))
                            ((snip pie e (e + 24)) >< acc))))))
           symo ())
      shstr (o-strtab '(".symtab" ".strtab" ".shstrtab"))
      (snm k) (at <>shstr k)
      syo (ld-align 8 fsz)
      sto (syo + symz) ssto (sto + strz) sstz (tally <shstr)
      shto (ld-align 8 (ssto + sstz))
      sht (catall `((ld-shdr 0 0 0 0 0 0 0 0 0 0)
                    (ld-shdr (snm 0) 2 0 0 syo symz 2 syminf 8 24)
                    (ld-shdr (snm 1) 3 0 0 sto strz 0 0 1 0)
                    (ld-shdr (snm 2) 3 0 0 ssto sstz 0 0 1 0)))
      hdr (elf64-hdrn 2 eaddr mach `(shto 4 3)
                      (map (\ g (elf64-phdr <g (at g 1) (at g 2) (at g 3)
                                            (at g 4) (at g 5) (at g 6) (at g 7)))
                           phs))
      (ld-write out
        (hdr >< (>en + `((ld-zeros (4096 - (hdrsz + nsz)))) + >ed
                 + <>body
                 + `((ld-zeros (syo - fsz)))
                 + (esy + `((snip pie stro (stro + strz)) <shstr
                            (ld-zeros (shto - (ssto + sstz))) sht))))))

   ; the file driver (kernel.mk's KPROJ). arity-gated: the `love kernel` verb
   ; evals this file too (lib/source.l), and ITS argv is the verb's own.
   ; ⚠ a def, so the outer `:` stays define-mode and `kproject` survives the
   ; read -- the call below is its own form.
   (kdrive as)
    (? (= 4 (tally as))
       (kproject <>(kread <as) <>(kread <>as) (at as 2) (at as 3))
       0))

(kdrive >argv)
