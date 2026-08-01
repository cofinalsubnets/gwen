; --- test/host/luxui.l: lux end to end under a headless X ---
; NOT in the test/*.l corpus (connectu/tether are host/*.c). Spawns an Xvfb,
; sails the REAL lux driver (the same seven modules `make install` cats into
; bin/lux) as one child, then a PROBE (test/host/luxui-probe.l) as another: the
; probe drives synthetic input -- XTEST FakeInput for the super+drags, SendEvent
; ClientMessages for the EWMH requests -- and asserts every effect off
; GetGeometry/GetProperty/GetInputFocus, exiting 0 iff all held. TWO processes
; on purpose: lux's stream and the probe's never share a socket, so neither
; can desync the other (the one-process attempt kept mis-reading geometry;
; the test/host/haven.l shape is the cure).
;
;   cat test/00-init.l test/host/luxui.l | out/host/love
;
; no Xvfb on the box -> a polite skip (the sentinel still prints).

; the display number off Xvfb's -displayfd stdout: take the first line that is
; ENTIRELY digits (server chatter shares the pty -- test/host/haven.l's lesson).
(: xv (tether `("Xvfb" "-displayfd" "1" "-screen" "0" "640x480x24" "-nolisten" "tcp"))
   (wfx p) (: g2 (gather p) (? (two? g2) g2 (: _ (rest 2) (wfx p))))
   camp #()
   (enlist k pid) (pin camp k pid)
   (strike _) (map (\ k (: p (camp k)
                           _ (? (charm? p) (: _ (still p 15) (wfx p)) 0) 0))
                   (keys camp))
   (must tag v) (? v v (: _ (strike ())
                           _ (puts "luxui: FAIL ") _ (putx tag) _ (putc 10)
                           (quit 1)))
 _ (? (! (two? xv))
      (: _ (puts "luxui: ok (no Xvfb, skipped)") (quit 0))
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

 ; a dead moor socket in the rundir: lux's sweep at moor start must fell it
 ; (bind + close leaves the file behind, refusing connects -- the shape an
 ; exec'd restart or a dead Xephyr session leaves)
 rdir (: rd (getenv "XDG_RUNTIME_DIR") (? (< 1 (tally rd)) rd "/tmp"))
 stale "lux-luxui-stale.sock"
 _ (: p (shore (+ rdir (+ "/" stale))) (? p (close p) 0))
 _ (must 'stale-planted (any (\ n (= n stale)) (readdir rdir)))

 ; the real driver, the same module set bin/lux ships (DISPLAY rides env)
 ph (tether `("env" (+ "DISPLAY=:" (show dn))
            "out/host/love" "-l" "crew/lux/core.l" "-l" "crew/lux/layout.l"
            "-l" "crew/lux/wire.l" "-l" "crew/lux/ewmh.l"
            "-l" "crew/lux/manage.l" "-l" "crew/lux/keys.l"
            "-l" "crew/lux/config.l" "-l" "crew/lux/lux.l"))
 _ (must 'lux-up (two? ph))
 _ (enlist 'ph (cap ph))

 ; the probe: its own process, its own streams; it waits for lux itself
 ; (_NET_SUPPORTED on the root), drives, asserts, and exits 0 or names its
 ; failure. Its pty is echoed either way -- a red run tells its story here.
 pr (tether `("out/host/love" "-l" "crew/lux/core.l" "-l" "crew/lux/layout.l"
            "-l" "crew/lux/wire.l" "-l" "test/host/luxui-probe.l"
            "-e" (+ "(probe \"" (+ dpy "\")"))))
 _ (must 'probe-up (two? pr))
 _ (enlist 'pr (cap pr))
 g (wfx (cap pr))
 _ (pull camp 'pr 0)
 (echo p) (: c (see p) (? (= c -1) 0 (: _ (putc c) (echo p))))
 _ (echo (cup pr))
 _ (close (cup pr))
 _ (must 'probe-clean (= 0 (cap g)))
 _ (must 'swept (! (any (\ n (= n stale)) (readdir rdir))))

 _ (strike ())
 _ (close (cup ph)) _ (close (cup xv))
 (puts "luxui: ok"))
