; test/host/luxui-probe.l -- the lux UI probe: a SEPARATE process with its own
; clean X streams (the one-process gate desynced -- see test/host/luxui.l), driven
; by (probe dpy). It creates its own InputOutput toplevels (xterm needs fonts
; a bare Xvfb lacks), drives lux with SYNTHETIC input -- XTEST FakeInput for
; super+drag move/resize, SendEvent ClientMessages for the EWMH requests --
; and asserts every effect off GetGeometry/GetProperty/GetInputFocus:
;   manage -> tile, maximize add/remove (the _NET_WM_STATE stamp both ways),
;   super+btn1 move EXACT, super+btn3 resize EXACT, the float holds the input
;   focus, a second client tiles while the float rides, _NET_CLOSE_WINDOW
;   kills it (no WM_DELETE_WINDOW advertised -> the KillClient fallback),
;   a Qt-style withdraw (real unmap then the ICCCM synthetic) leaves a
;   fullscreen float UNMAPPED and a re-show re-manages it (the telegram
;   media-viewer zombie: the crossing's render must not resurrect it),
;   the unmap ledger: lux's own hides (switch, minimize) never evict, a
;   RUDE client unmap with no courtesy synthetic does (xmonad's rule).
; Exits 0 iff every assertion held; any failure names itself and exits 1.
; Load order: core -> layout -> wire, then this file; test/host/luxui.l spawns it.
(: (probe dpy)
   (: s (connectu dpy)
      sc (x-setup s ())
      root (scr-root sc)
      (line m) (: _ (say out m) _ (say out "\n") (flush out))
      (fail m) (: _ (line (+ "probe FAIL " m)) (quit 1))
      (must m v) (? v v (fail m))
      ; a patient waiter: poll th every 100ms until it answers true
      (waitp m th t) (? (< t 1) (fail m)
                        (? (th ()) 1 (: _ (rest 100) (waitp m th (- t 1)))))

      ; --- lux is up when ewmh-init has stamped the root ---
      _ (waitp "wm-up" (\ u (two? (x-getprop-atoms s root (x-intern s "_NET_SUPPORTED")))) 100)

      ; --- XTEST: the extension's major opcode, then the super keycode
      ;     (mod4 row of GetModifierMapping, first nonzero) ---
      qx (: n (tally "XTEST")
            _ (x-w8 s 98) _ (x-w8 s 0) _ (x-w16 s (+ 2 (/ (+ n (x-pad4 n)) 4)))
            _ (x-w16 s n) _ (x-w16 s 0) _ (x-wpad s "XTEST") _ (x-flush s)
            hdr (x-reply s) (nthl hdr 9))
      _ (must "xtest-present" (< 0 qx))
      sk (: _ (x-w8 s 119) _ (x-w8 s 0) _ (x-w16 s 1) _ (x-flush s)
            hdr (x-reply s) kpm (nthl hdr 1) data (x-rbytes s (* 8 kpm))
            (go j) (? (< j kpm) (: c (nthl data (+ (* 6 kpm) j)) (? (< 0 c) c (go (+ j 1)))) 0)
            (go 0))
      _ (must "super-keycode" (< 0 sk))
      ; one synthetic input event: xXTestFakeInputReq is 36 bytes (length 9),
      ; rootX/rootY at bytes 24/26; type 2/3 key, 4/5 button, 6 motion (abs)
      (fake ty dt rx ry)
       (: _ (x-w8 s qx) _ (x-w8 s 2) _ (x-w16 s 9) _ (x-w8 s ty) _ (x-w8 s dt) _ (x-w16 s 0)
          _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0)
          _ (x-w16 s rx) _ (x-w16 s ry) _ (x-w32 s 0) _ (x-w16 s 0) _ (x-w8 s 0) _ (x-w8 s 0)
          (x-flush s))
      ; an EWMH request: SendEvent (SubstructureRedirect|Notify) to the root
      (cmsg win ty d0 d1 d2)
       (: _ (x-w8 s 25) _ (x-w8 s 0) _ (x-w16 s 11) _ (x-w32 s root) _ (x-w32 s 1572864)
          _ (x-w8 s 33) _ (x-w8 s 32) _ (x-w16 s 0) _ (x-w32 s win) _ (x-w32 s ty)
          _ (x-w32 s d0) _ (x-w32 s d1) _ (x-w32 s d2) _ (x-w32 s 0) _ (x-w32 s 0)
          (x-flush s))
      (getfocus u) (: _ (x-w8 s 43) _ (x-w8 s 0) _ (x-w16 s 1) _ (x-flush s)
                      hdr (x-reply s) (u32 hdr 8))
      ; our own toplevel: InputOutput off the connection's id base, white back
      (mkwin s2 wid rt) (: _ (x-w8 s2 1) _ (x-w8 s2 0) _ (x-w16 s2 9)
                           _ (x-w32 s2 wid) _ (x-w32 s2 rt)
                           _ (x-w16 s2 5) _ (x-w16 s2 5) _ (x-w16 s2 200) _ (x-w16 s2 150)
                           _ (x-w16 s2 1) _ (x-w16 s2 1)
                           _ (x-w32 s2 0)
                           _ (x-w32 s2 2) _ (x-w32 s2 16777215)
                           _ (x-map s2 wid) (x-flush s2))
      a-st (x-intern s "_NET_WM_STATE")
      a-mv (x-intern s "_NET_WM_STATE_MAXIMIZED_VERT")
      a-mh (x-intern s "_NET_WM_STATE_MAXIMIZED_HORZ")
      a-cl (x-intern s "_NET_CLOSE_WINDOW")

      ; --- 1. manage: map -> lux tiles the lone window over the screen ---
      w1 (scr-idbase sc)
      _ (mkwin s w1 root)
      _ (waitp "w1-tiled" (\ u (< 600 (nthl (x-getgeom s w1) 2))) 100)
      _ (waitp "w1-focused" (\ u (= w1 (getfocus ()))) 50)

      ; --- 2. maximize add/remove while TILED (a float's toggle is a no-op
      ;     by design: float-state is the proxy) -- the stamp goes on and off ---
      _ (cmsg w1 a-st 1 a-mv a-mh)
      _ (waitp "maximized" (\ u (: g (x-getgeom s w1)
                                   (&& (= 0 (nthl g 0)) (< 600 (nthl g 2))))) 50)
      _ (waitp "max-stamped" (\ u (= 2 (tally (x-getprop-atoms s w1 a-st)))) 50)
      _ (cmsg w1 a-st 0 a-mv a-mh)
      _ (waitp "unmax-stamped" (\ u (! (x-getprop-atoms s w1 a-st))) 50)

      ; --- 3. super+btn1 drag: float + move, the delta lands EXACT ---
      g0 (x-getgeom s w1)
      cx (+ (nthl g0 0) (// (nthl g0 2) 2)) cy (+ (nthl g0 1) (// (nthl g0 3) 2))
      _ (fake 6 0 cx cy) _ (rest 80)
      _ (fake 2 sk 0 0) _ (rest 80)
      _ (fake 4 1 cx cy) _ (rest 100)
      _ (fake 6 0 (+ cx 40) (+ cy 20)) _ (rest 60)
      _ (fake 6 0 (+ cx 75) (+ cy 45)) _ (rest 100)
      _ (fake 5 1 (+ cx 75) (+ cy 45)) _ (rest 100)
      _ (fake 3 sk 0 0) _ (rest 80)
      _ (waitp "moved-exact" (\ u (: g (x-getgeom s w1)
                                     (&& (= (nthl g 0) (+ (nthl g0 0) 75))
                                         (&& (= (nthl g 1) (+ (nthl g0 1) 45))
                                             (= (nthl g 2) (nthl g0 2)))))) 50)

      ; --- 4. super+btn3 drag: resize, the delta lands EXACT ---
      g1 (x-getgeom s w1)
      rx (+ (nthl g1 0) (// (nthl g1 2) 2)) ry (+ (nthl g1 1) (// (nthl g1 3) 2))
      _ (fake 6 0 rx ry) _ (rest 80)
      _ (fake 2 sk 0 0) _ (rest 80)
      _ (fake 4 3 rx ry) _ (rest 100)
      _ (fake 6 0 (+ rx 50) (+ ry 30)) _ (rest 100)
      _ (fake 5 3 (+ rx 50) (+ ry 30)) _ (rest 100)
      _ (fake 3 sk 0 0) _ (rest 80)
      _ (waitp "resized-exact" (\ u (: g (x-getgeom s w1)
                                       (&& (= (nthl g 2) (+ (nthl g1 2) 50))
                                           (= (nthl g 3) (+ (nthl g1 3) 30))))) 50)
      _ (waitp "float-focused" (\ u (= w1 (getfocus ()))) 50)

      ; --- 5. a second client (its own connection) tiles alongside;
      ;     _NET_CLOSE_WINDOW fells it -- no WM_DELETE_WINDOW advertised, so
      ;     lux takes the KillClient fallback, verified through
      ;     FRESH eyes (the reply stream of a client whose window died
      ;     mid-poll echoes stale replies) ---
      s2 (connectu dpy)
      sc2 (x-setup s2 ())
      w2 (scr-idbase sc2)
      _ (mkwin s2 w2 root)
      _ (waitp "w2-tiled" (\ u (< 300 (nthl (x-getgeom s w2) 2))) 100)           ; w1 floats, so w2 owns the tiling alone
      _ (cmsg w2 a-cl 0 0 0)
      s3 (connectu dpy)
      sc3 (x-setup s3 ())
      _ (waitp "w2-felled" (\ u (! (any (\ k (= k w2)) (x-querytree s3 root)))) 100)

      ; --- 5b. the withdraw race (the telegram media-viewer zombie): a client
      ;     hides a fullscreen float Qt-style -- a REAL unmap, then the ICCCM
      ;     synthetic UnmapNotify. The real unmap's pointer crossing renders
      ;     while the float is still in the sheaf, so the render re-maps it;
      ;     the withdraw must leave the window UNMAPPED anyway, and a re-show
      ;     must arrive as a fresh MapRequest and re-manage fullscreen. ---
      a-fs (x-intern s "_NET_WM_STATE_FULLSCREEN")
      wt (+ w1 1)                                                                ; a TILE under the pointer: the crossing into a tile is the
      _ (mkwin s wt root)                                                        ; rendering branch (a float's crossing only fmarks)
      _ (waitp "wt-tiled" (\ u (< 600 (nthl (x-getgeom s wt) 2))) 100)
      wv (+ w1 2)
      _ (: _ (x-w8 s 1) _ (x-w8 s 0) _ (x-w16 s 9)                               ; create (unmapped): white 300x200 toplevel
           _ (x-w32 s wv) _ (x-w32 s root)
           _ (x-w16 s 0) _ (x-w16 s 0) _ (x-w16 s 300) _ (x-w16 s 200)
           _ (x-w16 s 1) _ (x-w16 s 1) _ (x-w32 s 0)
           _ (x-w32 s 2) _ (x-w32 s 16777215) (x-flush s))
      _ (: _ (x-w8 s 18) _ (x-w8 s 0) _ (x-w16 s 7) _ (x-w32 s wv)               ; _NET_WM_STATE = [FULLSCREEN] BEFORE the map (Qt showFullScreen)
           _ (x-w32 s a-st) _ (x-w32 s 4) _ (x-w8 s 32) _ (x-w8 s 0)
           _ (x-w16 s 0) _ (x-w32 s 1) _ (x-w32 s a-fs) (x-flush s))
      (mapv u) (: _ (x-w8 s 8) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s wv) (x-flush s))
      (mstv u) (cap (x-attrs s wv))                                              ; 2 = viewable, 0 = unmapped
      _ (fake 6 0 30 20) _ (rest 80)                                             ; park the pointer over the BARE tile (the w1 float, dragged
                                                                                 ; off its near-fullscreen tiled rect, leaves only this strip):
                                                                                 ; the withdraw's crossing must land in the TILE to render
      _ (mapv ())
      _ (waitp "wv-fullscreen" (\ u (< 600 (nthl (x-getgeom s wv) 2))) 50)
      (withdraw u) (: _ (x-unmap s wv)                                           ; the real unmap, then the courtesy synthetic to the root
                      _ (x-w8 s 25) _ (x-w8 s 0) _ (x-w16 s 11) _ (x-w32 s root) _ (x-w32 s 1572864)
                      _ (x-w8 s 18) _ (x-w8 s 0) _ (x-w16 s 0) _ (x-w32 s root) _ (x-w32 s wv)
                      _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0)
                      (x-flush s))
      _ (withdraw ())
      _ (waitp "wv-withdraw-unstamped" (\ u (! (two? (x-getprop-atoms s wv (x-intern s "WM_STATE"))))) 50)
      _ (rest 300)                                                               ; let any resurrecting render land before the read
      _ (waitp "wv-withdraw-unmapped" (\ u (= 0 (mstv ()))) 50)
      _ (mapv ())
      _ (waitp "wv-reshow-fullscreen" (\ u (&& (= 2 (mstv ()))
                                               (&& (< 600 (nthl (x-getgeom s wv) 2))
                                                   (two? (x-getprop-atoms s wv (x-intern s "WM_STATE")))))) 50)
      _ (withdraw ())                                                            ; leave it withdrawn: the moor leg leans on nothing here
      _ (waitp "wv-rewithdraw" (\ u (= 0 (mstv ()))) 50)

      ; --- 5c. the unmap ledger (xmonad's waitingUnmap): lux's OWN hides --
      ;     workspace switch, minimize -- echo back counted and must NOT evict;
      ;     a RUDE client unmap (no ICCCM courtesy synthetic at all) must ---
      a-cd (x-intern s "_NET_CURRENT_DESKTOP")
      a-aw (x-intern s "_NET_ACTIVE_WINDOW")
      a-ic (x-intern s "WM_CHANGE_STATE")
      a-li (x-intern s "_NET_CLIENT_LIST")
      wr (+ w1 3)
      _ (: _ (x-w8 s 1) _ (x-w8 s 0) _ (x-w16 s 9)                               ; a plain tile
           _ (x-w32 s wr) _ (x-w32 s root)
           _ (x-w16 s 0) _ (x-w16 s 0) _ (x-w16 s 300) _ (x-w16 s 200)
           _ (x-w16 s 1) _ (x-w16 s 1) _ (x-w32 s 0)
           _ (x-w32 s 2) _ (x-w32 s 16777215) (x-flush s))
      (mapr u) (: _ (x-w8 s 8) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s wr) (x-flush s))
      (mstr u) (cap (x-attrs s wr))
      (known? u) (any (\ k (= k wr)) (x-getprop-atoms s root a-li))
      _ (mapr ())
      _ (waitp "wr-tiled" (\ u (known? ())) 50)
      _ (cmsg root a-cd 1 0 0)                                                   ; our hide: the switch away...
      _ (waitp "wr-switch-hidden" (\ u (= 0 (mstr ()))) 50)
      _ (must "wr-switch-keeps" (known? ()))                                     ; ...counted, NOT a withdraw
      _ (cmsg root a-cd 0 0 0)
      _ (waitp "wr-switch-back" (\ u (= 2 (mstr ()))) 50)
      _ (cmsg wr a-ic 3 0 0)                                                     ; our hide: iconify (the layout drops it)...
      _ (waitp "wr-minimized" (\ u (= 0 (mstr ()))) 50)
      _ (must "wr-minimize-keeps" (known? ()))                                   ; ...counted too
      _ (cmsg wr a-aw 0 0 0)                                                     ; a pager activates: restore
      _ (waitp "wr-restored" (\ u (= 2 (mstr ()))) 50)
      _ (x-unmap s wr) _ (x-flush s)                                             ; the RUDE hide: a raw client unmap, NO synthetic --
      _ (waitp "wr-rude-evicts" (\ u (! (known? ()))) 50)                        ; an unowed real unmap IS the withdraw
      _ (waitp "wr-rude-unstamped" (\ u (! (two? (x-getprop-atoms s wr (x-intern s "WM_STATE"))))) 50)
      _ (rest 300)                                                               ; let any resurrecting render land before the read
      _ (must "wr-rude-unmapped" (= 0 (mstr ())))
      _ (mapr ())                                                                ; and the re-show re-manages
      _ (waitp "wr-remanaged" (\ u (&& (known? ()) (= 2 (mstr ())))) 50)

      ; --- 6. LAST, the moor: the live repl evals, survives a scare, CHANGES state
      ;     (msg! 'next-layout flips the ||| index -- read back through the
      ;     SNAPSHOT over the moor socket, so the assert leans on no window
      ;     and no X read), and REFUSES garbage ((lux! 0): (0 st) is 1, not
      ;     a sane 4-list). A queued change drains at lux's next X event; we
      ;     nudge with a no-op ClientMessage from a FRESH connection each time
      ;     (kept open: this Xvfb spuriously CloseDownClient's one client
      ;     ~10s in -- gdb'd to ospoll_wait -> CloseDownClient with the
      ;     victim's socket provably untouched -- so nothing here may lean on
      ;     s staying alive, and closing spares fds would re-arm it) ---
      rsock (: rd (getenv "XDG_RUNTIME_DIR")
               base (? (< 1 (tally rd)) rd "/tmp")
               (+ base (+ "/lux-" (+ (snip dpy 16 (tally dpy)) ".sock"))))
      rc (: (try t) (? (< t 1) ()
                       (: p (connectu rsock)
                          (? (lit? p) p (: _ (rest 100) (try (- t 1))))))
            (try 100))
      _ (must "moor-up" (lit? rc))
      (rdline u) (: (go acc) (: ch (see rc)
                                (? (|| (= ch -1) (= ch 10)) acc (go (+ acc (+ "" ch)))))
                    (go ""))
      (ask q) (: _ (say rc q) _ (say rc "\n") _ (flush rc) (rdline ()))
      _ (must "moor-evals" (= "3" (ask "(+ 1 2)")))
      _ (ask "(boom-unbound)")                                                   ; a scare answers a value; the session lives
      _ (must "moor-survives" (= "7" (ask "(+ 3 4)")))
      (nudge u) (: nf (connectu dpy) sf (x-setup nf ())
                   _ (x-w8 nf 25) _ (x-w8 nf 0) _ (x-w16 nf 11)
                   _ (x-w32 nf root) _ (x-w32 nf 1572864)
                   _ (x-w8 nf 33) _ (x-w8 nf 32) _ (x-w16 nf 0) _ (x-w32 nf root) _ (x-w32 nf 1)
                   _ (x-w32 nf 0) _ (x-w32 nf 0) _ (x-w32 nf 0) _ (x-w32 nf 0) _ (x-w32 nf 0)
                   _ (x-flush nf) (rest 100))
      ; the ||| index rides at minimize>smart>toggle>mirror>choice (config.l's stack)
      chidx "(nthl (nthl (nthl (nthl (nthl (caup (lux-st ())) 2) 1) 2) 2) 1)"
      i0 (ask chidx)
      _ (ask "(msg! 'next-layout)")
      _ (nudge ())
      _ (waitp "moor-changes" (\ u (! (= i0 (ask chidx)))) 50)
      _ (ask "(msg! 'next-layout)")                                              ; flip back (the ||| holds two layouts)
      _ (nudge ())
      _ (waitp "moor-cycles" (\ u (= i0 (ask chidx))) 50)
      _ (ask "(lux! 0)")                                                        ; garbage: refused, the wm stands
      _ (nudge ())
      _ (must "moor-refuses" (= "11" (ask "(+ 5 6)")))
      _ (must "moor-standing" (= i0 (ask chidx)))

      ; --- 7. keybindings under a lock modifier: super+space (next-layout,
      ;     read back through the moor's snapshot like leg 6) must fire with
      ;     NumLock OFF -- and just the same with NumLock ON: a lock bit rides
      ;     the KeyPress state, so the grab must cover the variant or every
      ;     chord goes dead while plain typing sails on (the 2026-07-27
      ;     symptom) ---
      kmn (x-keymap s (scr-mink sc) (scr-maxk sc) (L 32 65407))                   ; space, Num_Lock
      spk (peep kmn 32 0) nlk (peep kmn 65407 0)
      _ (must "space-keycode" (< 0 spk)) _ (must "numlock-keycode" (< 0 nlk))
      (chord u) (: _ (fake 2 sk 0 0) _ (rest 60) _ (fake 2 spk 0 0) _ (rest 60)
                   _ (fake 3 spk 0 0) _ (rest 60) _ (fake 3 sk 0 0) (rest 100))
      (numlock u) (: _ (fake 2 nlk 0 0) _ (rest 60) _ (fake 3 nlk 0 0) (rest 100))
      _ (chord ())
      _ (waitp "chord-flips" (\ u (! (= i0 (ask chidx)))) 50)                     ; the machinery works bare...
      _ (numlock ())                                                              ; ...now latch NumLock
      _ (chord ())
      _ (waitp "chord-under-numlock" (\ u (= i0 (ask chidx))) 50)                 ; and the chord must still land (flip back to i0)
      _ (numlock ())                                                              ; restore

      ; --- 8. MappingNotify: poke one unused keycode's mapping (a stand-in for
      ;     setxkbmap / a keyboard replug) -- lux re-resolves and regrabs, and
      ;     the chord still lands ---
      _ (: _ (x-w8 s 100) _ (x-w8 s 1) _ (x-w16 s 3) _ (x-w8 s 255) _ (x-w8 s 1)   ; ChangeKeyboardMapping: keycode 255 -> NoSymbol
           _ (x-w16 s 0) _ (x-w32 s 0) (x-flush s))
      _ (rest 200)                                                               ; let the regrab land
      _ (chord ())
      _ (waitp "chord-after-remap" (\ u (! (= i0 (ask chidx)))) 50)
      _ (chord ())
      _ (waitp "chord-after-remap-back" (\ u (= i0 (ask chidx))) 50)

      ; --- 9. the back door: (lux-restart ()) over the moor execs in place.
      ;     the old generation is marked through its lux-cell; the reborn moor
      ;     answers with a FRESH cell (the marker gone), the SAME pid (exec),
      ;     and the windows adopted. one connection per poll: a try that lands
      ;     on the dying generation just answers wrong and is retried ---
      (askonce q) (: p (connectu rsock)
                     (? (! (lit? p)) ()
                        (: _ (say p q) _ (say p "\n") _ (flush p)
                           r (: (go acc) (: ch (see p)
                                            (? (|| (= ch -1) (= ch 10)) acc (go (+ acc (+ "" ch)))))
                                (go ""))
                           _ (close p) r)))
      _ (ask "(pin lux-cell 'gen 'old)")
      pid0 (ask "(getpid 0)")
      _ (say rc "(lux-restart ())\n") _ (flush rc)
      _ (waitp "restart-reborn" (\ u (= "()" (askonce "(peep lux-cell 'gen ())"))) 100)
      _ (waitp "restart-same-pid" (\ u (= pid0 (askonce "(getpid 0)"))) 50)
      _ (waitp "restart-adopts" (\ u (any (\ k (= k wr)) (x-getprop-atoms s root a-li))) 100)
      _ (close rc)

      _ (close s3) _ (close s)
      _ (line "probe: manage + maximize + move + resize + focus + moor + close, all exact")
      (quit 0)))
