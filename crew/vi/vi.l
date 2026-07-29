; crew/vi/vi.l -- the terminal face over crew/vi/core.l: read keys off `in`,
; blit vframe's escapes onto `out`, perform the write requests the ex commands
; leave on the state. a toolbox-style file (kore is its door: `kore vi FILE`);
; loads after core.l/re.l/vi core in the kore cat and rides the u-floor (uread,
; udie) and text.l's lines/unlines.
;
; keys: one byte at a time via (see in); ESC [ A/B/C/D decode to k/j/l/h, any
; other post-ESC byte is DELIVERED AFTER the bare 27 (a one-byte pushback), so
; `ESC x` interleaves right. port EOF quits -- a piped script (the smoke lane)
; ends with :wq and never dangles. the tty raws via the `raw` nif (cooked
; restores at exit, bao's atexit); the frame lives on the alternate screen,
; so the session's scrollback survives the visit.
(:
   (vi-main as)
    (: f (? (two? as) (cap as) "")
       r (? (< 0 (tally f)) (uread f) ())
       st (vnew (? (two? r) (lines (caup r)) ()) f)
       _ (? (&& (< 0 (tally f)) (! (two? r))) (vmsg st (+ f ": new file")) ())
       ; the pens are the FACE's to hand over -- they want $COLORTERM and the
       ; user's own theme file, and the engine reads neither
       _ (pin st 'pens (hue-wake ()))
       ws (winsize 0)
       _ (? (two? ws) (: _ (pin st 'h (cap ws)) (pin st 'w (cup ws))) ())
       e (+ "" 27)
       _ (raw 1)
       _ (puts (+ e "[?1049h"))
       ; the ex write requests: (name) means write, "" means the buffer's file
       (dow s2)
        (: rq (s2 'dow)
           (? (! (two? rq)) s2
              (: _ (pin s2 'dow ())
                 nm (cap rq)
                 (? (< 0 (tally nm))
                    (: q (open nm "w")
                       (? q (: _ (say q (unlines (s2 'lines))) _ (close q)
                               _ (pin s2 'dirty ())
                               (vmsg s2 (+ nm " written")))
                          (: _ (pin s2 'quit ())          ; a failed wq must NOT quit
                             (vmsg s2 (+ "cannot write " nm)))))
                    (: _ (pin s2 'quit ())
                       (vmsg s2 "no file name"))))))
       ; one key, arrows decoded, with the one-byte pushback: (byte next-pend)
       (rd pend)
        (? (<= 0 pend) (L pend -1)
           (: c (see in)
              (? (! (= c 27)) (L c -1)
                 (: c2 (see in)
                    (? (= c2 91)
                       (: c3 (see in)
                          (L (? (= c3 65) 107 (= c3 66) 106
                                (= c3 67) 108 (= c3 68) 104 27) -1))
                       (L 27 c2))))))
       (fin _) (: _ (puts (e + "[0m" + e + "[?1049l")) _ (flush out) (quit 0))
       (go pend)
        (: _ (puts (vframe st)) _ (flush out)
           kr (rd pend)
           c (cap kr)
           (? (< c 0) (fin ())
              (: _ (vstep st c)
                 _ (dow st)
                 (? (st 'quit) (fin ())
                    (go (caup kr))))))
       (go -1)))
