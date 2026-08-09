; berth -- quay's terminal multiplexer: where programs tie up.
;
; each child rides its own pty (tether); its bytes pour through a quay cb screen
; -- the same VT parser the kernel console runs, hosted in a cask by host/cb.c --
; and the grids repaint the OUTER terminal as painted cells. keys pour down the
; other way; DSR/DA answers ride home off each screen's reply queue; the outer
; size is polled each second-ish and mirrored onto every pty (the kernel raises
; SIGWINCH on the children for us). a resize opens fresh grids -- the full-screen
; guests repaint on their SIGWINCH, which is the one-buffer bargain throughout.
;
; WHERE THE PANES COME FROM: lib/wharf.l, and nothing about it is berth's. it is
; a plan tree in and rectangles in CELLS out -- no nif, no pty, no framebuffer --
; so the kernel console can lay its own panes with the same arithmetic. that
; sharing is the point; berth is just the first seat to ask.
;
; C-a is the prefix, and the intercept sits in the key pump, BELOW every guest:
;   C-a s / C-a v   split the focused berth, stacked or beside
;   C-a h j k l     steer the focus
;   C-a x           shut the focused berth
;   C-a C-a         send a literal C-a through
; a guest never sees the prefix and never has to cooperate.
;
; run: out/host/love -l crew/quay/berth.l -e '(berth (L "sh"))'
;      out/host/love -l crew/quay/berth.l -e '(berth (L "vim" "README.md"))'
;
; host-only (screen/scribe + tether/gather + raw/winsize ride host/*.c), so
; berth never joins the corpus; the moor core needs no tty and is smoke-
; tested end to end over a real pty by test/host/berth.l.
(use 'wharf)

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
            (: cp (? ((<= 32 b) && (< b 127)) b (unfold b))
               s (utf8 (? (charm? cp && (< 0 cp)) cp 63))
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

 ; --- the moor: pump the master through the screen until the child hangs up.
 ; the screen lives in box bx (a relay swaps it); replies pour straight back;
 ; a dirty box (when given) cues the painter
 ; park on the first byte, then SWIG the rest of the gulp -- per-byte sees
 ; cap out near a megabyte a second, and a full-screen guest pours six
 ; ⚠ the box is re-read every gulp on purpose: that is how a resize reaches a
 ; running moor with no signal, and why a pane's screen may be swapped freely.
 (moor bx m dirty) (: ch (see m)
                    (? (= ch -1) ()
                     (: s (peep bx () 0)
                        _ (scribe s ch)
                        b (cask 8192)
                        _ (: (drain x) (: n (swig m b)
                                          (? (< 0 n)
                                             (: _ (scribe (peep bx () 0) (snip (string b) 0 n))
                                                (drain ()))
                                             0))
                           (drain ()))
                        rep (reply (peep bx () 0))
                        ; ⚠ NOT (map (put m) rep): map applies RIGHT TO LEFT, so the
                        ; answer went out backwards -- "\e[1;1R" as "R1;1[\e". a reply
                        ; is a byte STREAM and its order is the whole content of it.
                        ; foldl is the left-to-right walk, and one say beats six puts.
                        _ (? rep (: _ (say m (foldl (\ a b (+ a ("" + b))) "" rep))
                                    (flush m))
                             0)
                        _ (? dirty (pin dirty () 1) 0)
                        (moor bx m dirty))))

 (waitfor pid) (: s (gather pid) (? (two? s) s (= 0 s) (do (rest 2) (waitfor pid)) s))

 ; --- the harbour: every berth's screen, pty and child, under one wharf plan ---
 ; a pane is (box master pid). the harbour is a tablet, and the keys are the
 ; whole state: plan focus next rows cols panes lay dirt argv jobs.
 (hbox p) <p
 (hmas p) <>p
 (hpid p) <>>p
 (hpane hb id) (peep (hb 'panes) id ())
 (hfind bs id) (? (two? bs) (? (= (wharf-id <bs) id) <bs (hfind >bs id)) ())
 (hsoil hb) (: b (hb 'dirt) (pin b () 1))

 ; open a child on its own pty at r x c. ⚠ tether's errno rides back out on a
 ; refusal -- a charm, never two?, which is how the caller tells it from a pane.
 (hopen argv r c) (: md (tether argv)
                     (? !(two? md) md
                      (: m >md
                         s (screen (cask (screen () r c)) r c)
                         bx #s
                         _ (setwinsize m r c)
                         (L bx m <md))))

 ; --- the relay: the plan says where every berth is, so every screen is opened
 ; to its rectangle and every pty told its new size. content is the guest's to
 ; repaint on its own SIGWINCH -- the one-buffer bargain, now once per pane.
 ; ⚠ a berth with no room still needs a VALID screen (the nifs refuse a zero
 ; axis), so its grid is 1x1 and wharf-show? is what keeps it off the glass.
 (hrelay hb) (: bs (wharf-lay (hb 'plan) (hb 'rows) (hb 'cols))
                _ (map (\ b (: p (hpane hb (wharf-id b))
                               (? !(two? p) 0
                                (: nr (max 1 (wharf-rows b)) nc (max 1 (wharf-cols b))
                                   bx (hbox p)
                                   s (peep bx () 0)
                                   (? ((= nr (gaze s 1)) && (= nc (gaze s 2))) 0
                                    (: s2 (screen (cask (screen () nr nc)) nr nc)
                                       _ (pin bx () s2)
                                       (setwinsize (hmas p) nr nc)))))))
                       bs)
                _ (pin hb 'lay bs)
                (hsoil hb))

 ; --- the frame -------------------------------------------------------------
 (hgo row col) (+ "\e[" (+ (show (row + 1)) (+ ";" (+ (show (col + 1)) "H"))))
 hdown (utf8 9474)                      ; │
 hacross (utf8 9472)                    ; ─

 ; one pane: every grid row addressed at its own origin, which is the whole
 ; difference between painting a rectangle and painting a run of full lines
 (hrows j s r c row0 col0 i p)
  (? (< i r)
     (: _ (say j (hgo (row0 + i) col0))
        p2 (prow j s c i 0 p)
        (hrows j s r c row0 col0 (i + 1) p2))
     p)

 (hbar j n s) (? (< 0 n) (: _ (say j s) (hbar j (n - 1) s)) 0)
 (hbars j hb)
  (map (\ b (: d <b row <>b col <>>b n <>>>b
               (? (= d 'across) (: _ (say j (hgo row col)) (hbar j n hacross))
                (: (down i) (? (< i n)
                               (: _ (say j (hgo (row + i) col)) _ (say j hdown) (down (i + 1)))
                               0)
                   (down 0)))))
       (wharf-bars (hb 'plan) (hb 'rows) (hb 'cols)))

 ; ⚠ nothing is erased between frames, and nothing needs to be: panes and bars
 ; TILE the wharf exactly (lib/wharf.l's standing law), so every cell is written
 ; every frame and no stale glyph can survive a layout change.
 (hframe hb)
  (: j (jug 0)
     _ (say j "\e[?25l")
     _ (map (\ b (: p (hpane hb (wharf-id b))
                    (? (wharf-show? b && two? p)
                       (hrows j (peep (hbox p) () 0) (wharf-rows b) (wharf-cols b)
                                (wharf-row b) (wharf-col b) 0 -1)
                       0)))
            (hb 'lay))
     _ (hbars j hb)
     _ (: p (hpane hb (hb 'focus)) b (hfind (hb 'lay) (hb 'focus))
          (? (two? p && two? b)
             (: s (peep (hbox p) () 0) w (gaze s 0) c (gaze s 2) f (gaze s 3)
                _ (say j (hgo (wharf-row b + (// w c)) (wharf-col b + (% w c))))
                (? (& 1 f) (say j "\e[?25h") 0))          ; the guest's cursor wish
             0))
     (slurp j))

 ; --- the berths: opening, steering, shutting -------------------------------
 ; a pane's pump, twirled: moor runs until the child hangs up and the berth is
 ; shut for it. ⚠ the handle is kept in 'jobs so the teardown can reap it.
 (hmoor hb id) (: p (hpane hb id)
                  t (twirl (\ _ (: _ (moor (hbox p) (hmas p) (hb 'dirt)) (hshut hb id))) 0)
                  (pin hb 'jobs (t >< hb 'jobs)))

 ; split the focused berth and tie a fresh child up in the new half. it opens
 ; at 1x1 and the relay right after is what gives it its real size.
 (hsplit hb d) (: p (hopen (hb 'argv) 1 1)
                  (? !(two? p) 0
                   (: id (hb 'next)
                      _ (pin (hb 'panes) id p)
                      _ (pin hb 'next (id + 1))
                      _ (pin hb 'plan (wharf-split (hb 'plan) (hb 'focus) d 50 id))
                      _ (pin hb 'focus id)
                      _ (hrelay hb)
                      (hmoor hb id))))

 (hsteer hb d) (: id (wharf-steer (hb 'lay) (hb 'focus) d)
                  (? (= id (hb 'focus)) 0 (: _ (pin hb 'focus id) (hsoil hb))))

 ; shut a berth: its pty closes, the plan promotes its sibling over the bar they
 ; shared, and the focus moves on if it was the one that left. ⚠ shutting twice
 ; is safe -- the second finds no pane and closes nothing, which is what a child
 ; hanging up on a berth the user already shut looks like.
 (hshut hb id) (: p (hpane hb id)
                  _ (? (two? p) (close (hmas p)) 0)
                  _ (pin (hb 'panes) id ())
                  pl (wharf-shut (hb 'plan) id)
                  _ (pin hb 'plan pl)
                  _ (? (= id (hb 'focus))
                       (: bs (wharf-berths pl) (pin hb 'focus (? (two? bs) <bs ())))
                       0)
                  (hrelay hb))

 ; --- keys: raw stdin pours onto the FOCUSED pane, C-a first ----------------
 (hfeed hb ch) (: p (hpane hb (hb 'focus))
                  (? (two? p) (: _ (put (hmas p) ch) (flush (hmas p))) 0))
 (hkey hb) (: k (see in)
              (? (< k 0)   ()
                 (= k 1)   (hfeed hb 1)                 ; C-a C-a: a literal one
                 (= k 115) (hsplit hb 'stack)           ; s
                 (= k 118) (hsplit hb 'beside)          ; v
                 (= k 104) (hsteer hb 'left)            ; h
                 (= k 106) (hsteer hb 'down)            ; j
                 (= k 107) (hsteer hb 'up)              ; k
                 (= k 108) (hsteer hb 'right)           ; l
                 (= k 120) (hshut hb (hb 'focus))       ; x
                 0))
 (oars hb) (: ch (see in)
              (? (= ch -1) (hfeed hb 4)                 ; VEOF follows the user's own eof
               (: _ (? (= ch 1) (hkey hb) (hfeed hb ch))
                  (oars hb))))

 ; --- the tide: mirror the outer terminal's size onto the whole wharf -------
 ; ⚠ THE FLOOR IS NOT DECORATION, and it lives here so both callers share it: a
 ; fresh pty answers (0 . 0) -- two? and true and useless -- and a wharf of no
 ; cells lays every berth at zero extent, which paints a blank screen and never
 ; recovers. the opening had this floor; the tide did not, and quietly undid it
 ; half a second in.
 (hsize u) (: ws (winsize 0)
              (L (? (two? ws && (< 5 <ws)) <ws 24)
                 (? (two? ws && (< 19 >ws)) >ws 80)))
 (htide hb) (: sz (hsize 0) r <sz c <>sz
               (? ((= r (hb 'rows)) && (= c (hb 'cols))) ()
                (: _ (pin hb 'rows r) _ (pin hb 'cols c) (hrelay hb))))

 ; --- the swim: repaint on dirt at ~30fps, feel the tide every half second,
 ; and end when the last berth empties ---
 (hswim hb tick) (: b (hb 'dirt)
                    _ (? (peep b () 0)
                         (: _ (pin b () 0) _ (say out (hframe hb)) 0)   ; say IS the bulk lane
                         0)
                    _ (? (= 0 (% tick 16)) (htide hb) 0)
                    _ (rest 33)
                    (? (two? (wharf-berths (hb 'plan))) (hswim hb (tick + 1)) ()))

 ; --- berth: tie argv up. answers the FIRST child's status pair (the one the
 ; caller named), or tether's errno if it never started ---
 (berth argv) (: sz (hsize 0) r <sz c <>sz
                 _ (setenv "TERM" "xterm-256color")   ; the dialect we actually speak
                 p0 (hopen argv r c)
                 (? !(two? p0) p0
                  (: panes #()
                     _ (pin panes 1 p0)
                     hb #()
                     _ (pin hb 'plan 1) _ (pin hb 'focus 1) _ (pin hb 'next 2)
                     _ (pin hb 'rows r) _ (pin hb 'cols c)
                     _ (pin hb 'panes panes) _ (pin hb 'argv argv)
                     _ (pin hb 'dirt #1)          ; born dirty: paint frame one
                     _ (pin hb 'jobs ()) _ (pin hb 'lay ())
                     _ (hrelay hb)
                     _ (raw 1)
                     _ (say out "\e[?1049h\e[2J")
                     _ (flush out)
                     up (twirl (\ _ (oars hb)) 0)
                     _ (hmoor hb 1)
                     _ (hswim hb 1)               ; ... until the last berth empties
                     _ (map (\ t (? (landed? t) (catch t) (freeze t))) (hb 'jobs))
                     _ (? (landed? up) (catch up) (freeze up))
                     _ (say out "\e[0m\e[?25h\e[?1049l")
                     _ (flush out)
                     _ (raw 0)
                     (waitfor (hpid p0))))))
