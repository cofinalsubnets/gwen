; --- test/host/font.l: the font atlas, the builtins, and the PSF loader ---
; NOT in the test/*.l corpus (font/blitrow/pour/unfold are host/*.c). the
; atlas discipline and the builtin bake are gated directly; the PSF lane
; is gated against a font SYNTHESIZED right here (no system fonts assumed):
;
;   cat test/00-init.l test/host/font.l | out/host/love

(reads (open "crew/quay/psf.l" "r"))

; --- the builtins bake into atlases ---
(: ft (font (cask (font () 0)) 0)
 _ (assert
    (= 8 (peep ft 0 0)) (= 16 (peep ft 1 0))     ; moderndos 8x16
    (= (+ 4 (* 256 16)) (font () 0))             ; the size protocol
    (! (font (cask 8) 0))                        ; too small: refused
    (! (font (cask 64) 7)))                      ; no such builtin
 f8 (font (cask (font () 1)) 1)
 _ (assert (= 8 (peep f8 0 0)) (= 8 (peep f8 1 0)))
 ; blitrow through each: the A lights differently per face
 s (screen (cask (screen () 1 4)) 1 4)
 _ (scribe s "A")
 fb (cask (* 32 (* 32 4)))
 _ (blitrow fb 32 s 0 () ft)
 lit16 (foldl (\ a i (+ a (? (= 192 (peep fb i 0)) 1 0))) 0 (jot 4096))
 fb2 (cask (* 32 (* 32 4)))
 _ (blitrow fb2 32 s 0 () f8)
 lit8 (foldl (\ a i (+ a (? (= 192 (peep fb2 i 0)) 1 0))) 0 (jot 4096))
 _ (assert (= 126 lit16) (= 84 lit8))
 ; () font falls back to the builtin: same pixels as the baked atlas
 fb3 (cask (* 32 (* 32 4)))
 _ (blitrow fb3 32 s 0 () ())
 lit0 (foldl (\ a i (+ a (? (= 192 (peep fb3 i 0)) 1 0))) 0 (jot 4096))
 _ (assert (= 126 lit0)))

; --- a synthesized PSF1: 256 glyphs of 8x8, glyph 65 a solid band; no
; unicode table, so the file order IS the cp437 order ---
(: b (cask (+ 4 (* 256 8)))
 _ (pin b 0 54) _ (pin b 1 4)                    ; the PSF1 magic
 _ (pin b 2 0) _ (pin b 3 8)                     ; mode 0, charsize 8
 _ ((: (go i) (? (< i 8) (: _ (pin b (+ 4 (+ (* 65 8) i)) 255) (go (+ i 1))) 0)) 0)
 p "/tmp/quay-font-boot.psf"
 f (open p "w")
 _ (assert (lit? f))
 _ (say f b)
 _ (close f)
 at (psf p)
 _ (assert
    (! (! at))
    (= 8 (peep at 0 0)) (= 8 (peep at 1 0))
    (= 255 (peep at (+ 4 (* 65 8)) 0))           ; the A band landed at cell 65
    (= 0 (peep at (+ 4 (* 66 8)) 0)))            ; ... and 66 stayed dark
 s (screen (cask (screen () 1 4)) 1 4)
 _ (scribe s "A")
 fb (cask (* 32 (* 16 4)))
 _ (blitrow fb 32 s 0 () at)
 lit (foldl (\ a i (+ a (? (= 192 (peep fb i 0)) 1 0))) 0 (jot 2048))
 _ (assert (= (* 64 3) lit))                     ; a solid 8x8: every channel lit
 md (tether `("rm" "-f" "/tmp/quay-font-boot.psf"))
 _ (? (two? md) (: _ (slurp (cup md))
                   _ (: (wf q) (: g2 (gather q) (? (two? g2) g2 (: _ (rest 2) (wf q)))) (wf (cap md)))
                   (close (cup md)))
      0))

; --- a PSF2 with a unicode table: one glyph claiming U+2500 (the box line);
; the cell byte 196 unfolds to U+2500 and must find it; 'x' (no claim)
; falls to the '?' stand-in -- glyph 0 here, dark ---
(: nb (+ 4 (* 2 8))
 b (cask (+ 32 (+ 16 6)))                        ; header + 2 glyphs + table
 _ (pin b 0 114) _ (pin b 1 181) _ (pin b 2 74) _ (pin b 3 134)   ; PSF2 magic
 _ (pin b 8 32)                                  ; headersize 32
 _ (pin b 12 1)                                  ; flags: unicode table
 _ (pin b 16 2)                                  ; two glyphs
 _ (pin b 20 8)                                  ; charsize 8
 _ (pin b 24 8)                                  ; height 8
 _ (pin b 28 8)                                  ; width 8
 _ ((: (go i) (? (< i 8) (: _ (pin b (+ 40 i) 255) (go (+ i 1))) 0)) 0)   ; glyph 1 solid
 ; the table: glyph 0 -> "?" 0xFF ; glyph 1 -> U+2500 (e2 94 80) 0xFF
 _ (pin b 48 63) _ (pin b 49 255)
 _ (pin b 50 226) _ (pin b 51 148) _ (pin b 52 128) _ (pin b 53 255)
 at (psf-bytes (string b))
 _ (assert
    (! (! at))
    (= 255 (peep at (+ 4 (* 196 8)) 0))          ; cell 196 (the ─) wears glyph 1
    (= 0 (peep at (+ 4 (* 120 8)) 0))))          ; x falls to '?', glyph 0: dark

(puts "font: ok")
