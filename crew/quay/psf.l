; psf -- console fonts for the pier: PSF1/PSF2 files -> a quay font atlas.
;
; a PSF is the linux console's font: a header, glyph bitmaps in the same
; discipline the atlas speaks (MSB the leftmost pixel, scanlines padded to
; bytes), and usually a unicode table naming each glyph's codepoints.
; (psf path) reads one into the atlas cask blitrow takes: 256 glyphs in
; cp437 order -- each cell byte's codepoint (unfold) looked up in the
; font's own table, the '?' glyph standing in for what the font lacks.
; a font without a table is taken in file order. gzipped fonts ride a
; zcat child to a temp file: (psf-gz path).
;
;   (: ft (psf-gz "/usr/share/kbd/consolefonts/ter-132n.psf.gz")
;      (pier (L "sh") "/tmp/.X11-unix/X0" (xauth-cookie ...) ft))
;
; host-only (pour/unfold/tether ride host/*.c); gated by boot/font.l.

(:
 (psf-u32 f i) (| (f i) (| (<< (f (+ i 1)) 8) (| (<< (f (+ i 2)) 16) (<< (f (+ i 3)) 24))))
 (psf-u16 f i) (| (f i) (<< (f (+ i 1)) 8))

 ; utf-8: one codepoint at i -> (cp . next); only sizes 1-3 matter here
 (psf-cp f i) (: b (f i)
                (? (< b 128) (link b (+ i 1))
                   (< b 224) (link (| (<< (& 31 b) 6) (& 63 (f (+ i 1)))) (+ i 2))
                   (< b 240) (link (| (<< (& 15 b) 12)
                                      (| (<< (& 63 (f (+ i 1))) 6) (& 63 (f (+ i 2)))))
                                   (+ i 3))
                   (link 0 (+ i 4))))

 ; the psf2 unicode table: per glyph, utf-8 codepoints to 0xFF (0xFE opens
 ; combining sequences we skip). cp -> glyph#+1, first claim wins.
 (psf2-table f off n) (: t (tablet 512) tn (tally f)
                         (go g i) (? (? (<= n g) 1 (<= tn i)) t
                                    (= 255 (f i)) (go (+ g 1) (+ i 1))
                                    (= 254 (f i)) (: (skip j) (? (<= tn j) j
                                                                 (= 255 (f j)) (+ j 1)
                                                                 (skip (+ j 1)))
                                                     (go (+ g 1) (skip (+ i 1))))
                                    (: c (psf-cp f i)
                                       _ (? (peep t (cap c) 0) 0 (pin t (cap c) (+ g 1)))
                                       (go g (cup c))))
                         (go 0 off))

 ; the psf1 table: ucs2 words to 0xFFFF (0xFFFE opens sequences)
 (psf1-table f off n) (: t (tablet 512) tn (tally f)
                         (go g i) (? (? (<= n g) 1 (<= tn (+ i 1))) t
                                    (: v (psf-u16 f i)
                                       (? (= v 65535) (go (+ g 1) (+ i 2))
                                          (= v 65534) (: (skip j) (? (<= tn (+ j 1)) j
                                                                     (= 65535 (psf-u16 f j)) (+ j 2)
                                                                     (skip (+ j 2)))
                                                         (go (+ g 1) (skip (+ i 2))))
                                          (: _ (? (peep t v 0) 0 (pin t v (+ g 1)))
                                             (go g (+ i 2))))))
                         (go 0 off))

 ; lay the atlas: 256 cells in cp437 order, glyph bytes poured straight
 ; from the file (C copies); w/h must fit what blitrow can carry
 (psf-atlas f hdrsz n csz h w tbl)
  (: bpr (// (+ w 7) 8)
     (? (! (&& (< 0 w) (<= w 16) (< 0 h) (<= h 32) (= csz (* h bpr)))) ()
      (: at (cask (+ 4 (* 256 csz)))
         _ (pin at 0 w)
         _ (pin at 1 h)
         q (? tbl (: v (peep tbl 63 0) (? v v 1)) 64)   ; the stand-in: '?'
         _ ((: (go b) (? (< b 256)
                        (: cp (? (< b 128) b (unfold b))
                           gn (? tbl (: v (peep tbl cp 0) (? v v q)) (+ b 1))
                           gn2 (? (< n gn) 1 gn)
                           _ (pour at (+ 4 (* b csz)) f (+ hdrsz (* (- gn2 1) csz)) csz)
                           (go (+ b 1)))
                        0))
            0)
         at)))

 (psf-bytes f)
  (? (< (tally f) 32) ()
     (&& (= 114 (f 0)) (&& (= 181 (f 1)) (&& (= 74 (f 2)) (= 134 (f 3)))))
     (: hs (psf-u32 f 8) fl (psf-u32 f 12) n (psf-u32 f 16)
        csz (psf-u32 f 20) h (psf-u32 f 24) w (psf-u32 f 28)
        tbl (? (& 1 fl) (psf2-table f (+ hs (* n csz)) n) ())
        (psf-atlas f hs n csz h w tbl))
     (&& (= 54 (f 0)) (= 4 (f 1)))
     (: md (f 2) csz (f 3)
        n (? (& 1 md) 512 256)
        tbl (? (& 2 md) (psf1-table f (+ 4 (* n csz)) n) ())
        (psf-atlas f 4 n csz csz 8 tbl))
     ())

 (psf p) (: f (open p "r")
            (? (! (lit? f)) ()
             (: b (slurp f) _ (close f) (psf-bytes b))))

 ; a .gz font: zcat into a temp file (a pty would mangle the bytes), then
 ; read it back. the temp wears the clock so parallel piers don't collide.
 (psf-gz p) (: tmp (+ "/tmp/quay-font-" (+ (show (clock 0)) ".psf"))
               md (tether (L "sh" "-c" (+ "zcat '" (+ p (+ "' > " tmp)))))
               (? (! (two? md)) ()
                (: (wf q) (: g2 (gather q) (? (two? g2) g2 (: _ (rest 2) (wf q))))
                   _ (slurp (cup md))
                   _ (wf (cap md))
                   _ (close (cup md))
                   at (psf tmp)
                   md2 (tether (L "rm" "-f" tmp))
                   _ (? (two? md2) (: _ (slurp (cup md2)) _ (wf (cap md2)) (close (cup md2))) 0)
                   at))))
