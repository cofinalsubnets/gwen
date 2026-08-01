; --- test/host/pier.l: the X window frontend, end to end against a headless server ---
; NOT in the test/*.l corpus (connectu/tether/screen/blit are host/*.c). spawns its
; own Xvfb, sails a pier with a real child, finds the window with QueryTree, and
; reads the PAINTED PIXELS back through the server with GetImage -- the whole loop:
; pty -> parser -> grid -> blit -> PutImage -> X -> GetImage.
;
;   cat test/00-init.l test/host/pier.l | out/host/love
;
; no Xvfb on the box -> a polite skip (the sentinel still prints).

(reads (open "crew/lux/core.l" "r"))
(reads (open "crew/lux/wire.l" "r"))
(reads (open "crew/quay/berth.l" "r"))
(reads (open "crew/quay/pier.l" "r"))

; -displayfd + the camp, test/host/haven.l's discipline (see its header for the whole
; story): a fresh display every run so no stray server can become the test
; target, the number-on-its-own-line read, and every child struck on any
; failed claim so a failure leaves no debris to poison the next run.
(: xv (tether `("Xvfb" "-displayfd" "1" "-screen" "0" "2560x1200x24" "-nolisten" "tcp"))
   (wfx p) (: g2 (gather p) (? (two? g2) g2 (: _ (rest 2) (wfx p))))
   camp #()
   (enlist k pid) (pin camp k pid)
   (strike _) (map (\ k (: p (camp k)
                           _ (? (charm? p) (: _ (still p 15) (wfx p)) 0) 0))
                   (keys camp))
   (must tag v) (? v v (: _ (strike ())
                           _ (puts "pier: FAIL ") _ (putx tag) _ (putc 10)
                           (quit 1)))
 _ (? (! (two? xv))
      (: _ (puts "pier: ok (no Xvfb, skipped)") (quit 0))
      0)
 _ (enlist 'xv (cap xv))
 (rdnum p) ((: (go n st) (: c (see p)
                            (? (= c -1) ()
                               (= c 13) (go n st)
                               (= c 10) (? (= st 1) n (go 0 0))
                               (&& (<= 48 c) (< c 58))
                                (? (= st 2) (go 0 2) (go (+ (* 10 n) (- c 48)) 1))
                               (go 0 2))))
            0 0)
 dn (rdnum (cup xv))
 _ (must 'display (&& (charm? dn) (! (two? (gather (cap xv))))))
 dpy (+ "/tmp/.X11-unix/X" (show dn))

 ; GetImage (opcode 73, ZPixmap): w*h pixels back as a byte list
 (px-getimg s d x y w h)
  (: _ (x-w8 s 73) _ (x-w8 s 2) _ (x-w16 s 5)
     _ (x-w32 s d) _ (x-w16 s x) _ (x-w16 s y) _ (x-w16 s w) _ (x-w16 s h)
     _ (x-w32 s 4294967295) _ (x-flush s)
     hdr (x-reply s)
     (x-rbytes s (* 4 (u32 hdr 4))))
 (lit-in px) (foldl (\ a b (+ a (? (= 192 b) 1 0))) 0 px)

 ; the pier under test, on its own pty with a real child
 ; the child prints steadily, so a resize's FRESH grid refills on its own
 md (tether `("out/host/love" "-l" "crew/lux/core.l" "-l" "crew/lux/wire.l" "-l" "crew/quay/berth.l"
            "-l" "crew/quay/pier.l"
            "-e" (+ "(pier (L \"sh\" \"-c\" \"while true; do printf H; sleep 0.3; done\") \"" (+ dpy "\" () ())"))))
 _ (must 'pier-up (two? md))
 _ (enlist 'md (cap md))

 ; our own eyes: a second connection; poll QueryTree until the window docks
 s (connectu dpy)
 sc (x-setup s ())
 _ (must 'x-eyes (&& (two? sc) (= 24 (scr-depth sc))))
 (waitwin t) (? (< t 1) ()
                (: ws (x-querytree s (scr-root sc))               ; (1 windows) | () -- a FAILED
                   ws2 (? (two? ws) (caup ws) ())                 ; query keeps waiting, same as an
                   (? (two? ws2) (cap ws2) (: _ (rest 100) (waitwin (- t 1))))))   ; empty root did
 win (waitwin 100)
 _ (must 'pier-window (charm? win))
 ; GetImage on an unmapped window is a BadMatch -- wait for IsViewable
 (waitmap t) (? (< t 1) 0
                (? (= 2 (cap (x-attrs s win))) 1
                   (: _ (rest 50) (waitmap (- t 1)))))
 _ (must 'pier-mapped (waitmap 100))

 ; read the first cell back off the server until the H lands: an H in the
 ; default pen paints 0xc0c0c0 strokes on black -- count the 0xc0 bytes
 (waitlit t) (? (< t 1) 0
                (: n (lit-in (px-getimg s win 0 0 8 16))
                   (? (< 30 n) n (: _ (rest 100) (waitlit (- t 1))))))
 _ (must 'h-painted (< 30 (waitlit 100)))
 _ (must 'sea-dark (= 0 (lit-in (px-getimg s win 0 160 8 16))))  ; row ten: still sea

 ; lux's move: retile us WIDE. fat rows must still ship in legal PutImage
 ; bands -- one over-long request would corrupt pier's whole stream (the bug
 ; a real desktop found: prompt painted, then garble and silence)
 _ (x-configure s win 0 0 2560 1184)
 _ (x-flush s)
 (waitlit2 t) (? (< t 1) 0
                (: n (lit-in (px-getimg s win 0 0 8 16))
                   (? (< 30 n) n (: _ (rest 100) (waitlit2 (- t 1))))))
 _ (must 'wide-shipped (< 30 (waitlit2 100)))                        ; the wide frame shipped

 ; step away: the child dies, the pier follows
 pid (cap md)
 _ (still pid 15)
 st (wfx pid)
 _ (pull camp 'md 0)                         ; reaped: a second wfx would twirl
 _ (must 'pier-reaped (two? st))
 _ (close s)
 _ (strike ())                               ; the same teardown the failure path takes
 _ (close (cup md)) _ (close (cup xv))
 (puts "pier: ok"))
