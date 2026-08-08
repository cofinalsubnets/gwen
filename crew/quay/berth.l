; berth -- quay's terminal emulator: where a program ties up.
;
; a child rides a pty (tether); its bytes pour through quay's cb screen -- the
; same VT parser the kernel console runs, hosted in a cask by host/cb.c --
; and the grid repaints the OUTER terminal as painted cells: tmux for one.
; keys pour down the other way; DSR/DA answers ride home off the screen's
; reply queue; the outer size is polled each second-ish and mirrored onto
; the pty (the kernel raises SIGWINCH on the child for us). a resize opens
; a fresh grid -- the full-screen guest repaints on its SIGWINCH, which is
; the one-buffer bargain throughout.
;
; run: out/host/love -l crew/quay/berth.l -e '(berth (L "sh"))'
;      out/host/love -l crew/quay/berth.l -e '(berth (L "vim" "README.md"))'
;
; host-only (screen/scribe + tether/gather + raw/winsize ride host/*.c), so
; berth never joins the corpus; the moor core needs no tty and is smoke-
; tested end to end over a real pty by boot/berth.l.

(:
 ; --- the painter: one frame of the grid, said into a jug as escape codes ---
 ; a pen is a cell's high bytes ((>> cell 8): fg, bg, font+faces); one SGR per
 ; pen CHANGE, respecified from zero each time -- cheap, and never stale
 (sgr p) (: fg (& 255 p) bg (& 255 (>> p 8)) fa (& 15 (>> p 20))
          (+ "\e[0"
           (+ (? (& 1 fa) ";1" "")
            (+ (? (& 2 fa) ";4" "")
             (+ (? (& 4 fa) ";7" "")
              (+ ";38;5;" (+ (show fg) (+ ";48;5;" (+ (show bg) "m")))))))))

 ; glyph bytes go OUT as utf-8: the grid holds cp437 (the fold), the outer
 ; terminal speaks unicode (the unfold), each face rendered once and cached
 glyphs #()
 (utf8 cp) (? (< cp 128) ("" + cp)
              (< cp 2048) (+ ("" + (| 192 (>> cp 6))) ("" + (| 128 (& 63 cp))))
              (+ ("" + (| 224 (>> cp 12)))
                 (+ ("" + (| 128 (& 63 (>> cp 6)))) ("" + (| 128 (& 63 cp))))))
 (gl b) (: g (peep glyphs b ())
           (? (string? g) g
            (: cp (? (&& (<= 32 b) (< b 127)) b (unfold b))
               s (utf8 (? (&& (charm? cp) (< 0 cp)) cp 63))
               _ (pin glyphs b s)
               s)))

 ; paint row i cell by cell, threading the pen; a blank cell is a space
 (prow j s c i q p) (? (< q c)
                     (: g (glass s (+ (* i c) q))
                        p2 (>> g 8)
                        _ (? (= p p2) 0 (say j (sgr p2)))
                        _ (say j (gl (: ch (& 255 g) (? (= 0 ch) 32 ch))))
                        (prow j s c i (+ q 1) p2))
                     p)
 (prows j s r c i p) (? (< i r)
                      (: p2 (prow j s c i 0 p)
                         _ (? (< (+ i 1) r) (say j "\r\n") 0)
                         (prows j s r c (+ i 1) p2))
                      p)
 (frame s) (: r (gaze s 1) c (gaze s 2) w (gaze s 0) f (gaze s 3)
              j (jug 0)
              _ (say j "\e[?25l\e[H")
              _ (prows j s r c 0 -1)             ; pen -1: the first cell respecs
              _ (say j (+ "\e[" (+ (show (+ 1 (// w c)))
                         (+ ";" (+ (show (+ 1 (% w c))) "H")))))
              _ (? (& 1 f) (say j "\e[?25h") 0)  ; the guest's cursor wish
              (slurp j))

 ; --- the moor: pump the master through the screen until the child hangs up.
 ; the screen lives in box bx (a resize swaps it); replies pour straight back;
 ; a dirty box (when given) cues the painter
 ; park on the first byte, then SWIG the rest of the gulp -- per-byte sees
 ; cap out near a megabyte a second, and a full-screen guest pours six
 (moor bx m dirty) (: ch (see m)
                    (? (= ch -1) ()
                     (: s (bx -> () 0)
                        _ (scribe s ch)
                        b (cask 8192)
                        _ (: (drain x) (: n (swig m b)
                                          (? (< 0 n)
                                             (: _ (scribe s (snip (string b) 0 n))
                                                (drain ()))
                                             0))
                           (drain ()))
                        rep (reply s)
                        ; ⚠ NOT (map (put m) rep): map applies RIGHT TO LEFT, so the
                        ; answer went out backwards -- "\e[1;1R" as "R1;1[\e". a reply
                        ; is a byte STREAM and its order is the whole content of it.
                        ; foldl is the left-to-right walk, and one say beats six puts.
                        _ (? rep (: _ (say m (foldl (\ a b (+ a ("" + b))) "" rep))
                                    (flush m))
                             0)
                        _ (? dirty (pin dirty () 1) 0)
                        (moor bx m dirty))))

 ; --- keys: raw stdin pours onto the master; VEOF follows the user's own eof ---
 (oars m) (: ch (see in)
            (? (= ch -1) (: _ (put m 4) (flush m))
             (: _ (put m ch) _ (flush m) (oars m))))

 ; --- the tide: mirror the outer terminal's size onto the pty. a change
 ; opens a fresh grid (content is the guest's to repaint on its SIGWINCH)
 (tide bx m dirty) (: ws (winsize 0)
                     (? (! (two? ws)) ()
                      (: r (cap ws) c (cup ws) s (bx -> () 0)
                         (? (&& (= r (gaze s 1)) (= c (gaze s 2))) ()
                          (: _ (bx <- () (screen (cask (screen () r c)) r c))
                             _ (setwinsize m r c)
                             _ (pin dirty () 1)
                             ())))))

 ; --- the swim: repaint on dirt at ~30fps, feel the tide every half second ---
 (swim bx m dirty tick) (: _ (? (dirty -> () 0)
                              (: _ (pin dirty () 0)
                                 f2 (frame (bx -> () 0))
                                 _ (say out f2)            ; say IS the bulk lane (and flushes)
                                 0)
                              0)
                           _ (? (= 0 (% tick 16)) (tide bx m dirty) 0)
                           _ (rest 33)
                           (swim bx m dirty (+ tick 1)))

 (waitfor pid) (: s (gather pid) (? (two? s) s (= 0 s) (do (rest 2) (waitfor pid)) s))

 ; --- berth: tie argv up. returns the child's status pair, or tether's errno ---
 (berth argv) (: ws (winsize 0)                  ; a fresh pty says 0x0: fall back
                 r (? (&& (two? ws) (< 5 (cap ws))) (cap ws) 24)
                 c (? (&& (two? ws) (< 19 (cup ws))) (cup ws) 80)
                 _ (setenv "TERM" "xterm-256color")   ; the dialect we actually speak
                 md (tether argv)
                 (? (! (two? md)) md
                  (: pid (cap md) m (cup md)
                     s0 (screen (cask (screen () r c)) r c)
                     bx #s0                         ; the screen, boxed: a resize swaps it
                     dirty #1                       ; born dirty: paint frame one
                     _ (setwinsize m r c)
                     _ (raw 1)
                     _ (say out "\e[?1049h\e[2J")
                     _ (flush out)
                     up (twirl (\ _ (oars m)) 0)
                     sw (twirl (\ _ (swim bx m dirty 1)) 0)
                     _ (moor bx m dirty)            ; ... until the child hangs up
                     _ (freeze sw)
                     _ (? (landed? up) (catch up) (freeze up))
                     _ (say out "\e[0m\e[?25h\e[?1049l")
                     _ (flush out)
                     _ (raw 0)
                     _ (close m)
                     (waitfor pid)))))
