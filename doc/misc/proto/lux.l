; wm -- rungs 2+4 of the xmonad-in-ai port: BECOME the window manager, then
; TILE. Rides the x11.l wire codec (rung 1) over the REAL door -- the unix
; display socket via (connectu path) (src/sock.c). The WM handshake is one
; request: ChangeWindowAttributes on the ROOT selecting SubstructureRedirect|
; SubstructureNotify -- from then on no client maps/resizes a top-level without
; the server ASKING US. The event loop drives the pure core (test/wm.l's stack
; zipper, spike-local copy) and renders it with the Tall layout: master takes
; the left half, everyone else stacks in the right column; one window means
; the whole screen. MapRequest inserts, DestroyNotify unmanages, every change
; retiles, and the wm parks when its last client leaves. run against
; `Xephyr :7 -ac`:
;   out/host/love doc/misc/proto/wm.l &  then  DISPLAY=:7 xterm  (and another)
; the Access error (code 10) on the redirect means another WM owns the display.
(: s (connectu "/tmp/.X11-unix/X7")
   _ (? s 0 (: _ (say err "wm: no display socket /tmp/.X11-unix/X7 -- Xephyr up?\n") (quit 1)))
   (line m) (: _ (say out m) _ (say out "\n") (flush out))

   ; --- the codec (x11.l's helpers) ---
   (w8 v)   (put s v)
   (w16 v)  (: _ (put s (& 255 v)) (put s (& 255 (>> v 8))))
   (w32 v)  (: _ (w16 (& 65535 v)) (w16 (& 65535 (>> v 16))))
   (r8 u)   (see s)
   (r16 u)  (: a (see s) b (see s) (| a (<< b 8)))
   (r32 u)  (: a (r16 ()) b (r16 ()) (| a (<< b 16)))
   (skip n) (: (go i) (? (< i n) (: _ (see s) (go (+ i 1))) 0) (go 0))
   (rstr n) (: (go i acc) (? (< i n) (go (+ i 1) (+ acc (see s))) acc) (go 0 ""))
   (pad4 n) (% (- 4 (% n 4)) 4)

   ; --- handshake + setup parse (only what the wm needs) ---
   _ (w8 108) _ (w8 0) _ (w16 11) _ (w16 0) _ (w16 0) _ (w16 0) _ (w16 0)
   _ (flush s)
   st (r8 ())
   _ (? (= st 1) 0
        (: rlen (r8 ()) _ (skip 6) why (rstr rlen)
           _ (say err (+ "wm: setup refused: " (+ why "\n"))) (quit 1)))
   _ (r8 ()) _ (r16 ()) _ (r16 ())
   alen (r16 ())
   _ (r32 ()) idbase (r32 ()) idmask (r32 ()) _ (r32 ())
   vlen (r16 ()) _ (r16 ())
   nscr (r8 ()) nfmt (r8 ())
   _ (skip 4)                                       ; image/bitmap orders, scanline unit/pad
   minkc (r8 ()) maxkc (r8 ())
   _ (skip 4)
   _ (skip (+ vlen (pad4 vlen)))
   _ (skip (* 8 nfmt))
   root (r32 ()) _ (r32 ()) _ (r32 ()) _ (r32 ()) _ (r32 ())
   wpx (r16 ()) hpx (r16 ())
   _ (skip (- (* 4 alen) (+ 32 (+ (+ vlen (pad4 vlen)) (+ (* 8 nfmt) 24)))))

   ; --- request writers ---
   (cwa-events win mask)                            ; ChangeWindowAttributes: CWEventMask only
    (: _ (w8 2) _ (w8 0) _ (w16 4) _ (w32 win) _ (w32 2048) (w32 mask))
   (configure win x y w h)                          ; ConfigureWindow: x y w h border=0 (mask 0x1F)
    (: _ (w8 12) _ (w8 0) _ (w16 8) _ (w32 win) _ (w16 31) _ (w16 0)
       _ (w32 x) _ (w32 y) _ (w32 w) _ (w32 h) (w32 0))
   (mapwin win) (: _ (w8 8) _ (w8 0) _ (w16 2) (w32 win))

   ; --- become the wm: redirect the root's substructure; sync via InternAtom
   ;     (its reply = we are in; an Access error = another wm holds the root).
   _ (cwa-events root 1572864)                      ; SubstructureRedirect|SubstructureNotify
   _ (w8 16) _ (w8 0) _ (w16 5) _ (w16 12) _ (w16 0)
   _ (: (go i) (? (< i 12) (: _ (put s ("WM_PROTOCOLS" i)) (go (+ i 1))) 0) (go 0))
   _ (flush s)
   b0 (r8 ())
   _ (? (= b0 1) (skip 31)
        (: code (r8 ()) _ (skip 30)
           _ (say err (? (= code 10) "wm: another wm owns this display (Access)\n"
                         (+ "wm: X error at redirect, code " (+ (show code) "\n"))))
           (quit 1)))
   _ (line (+ "wm: managing root " (+ (show root) (+ " (" (+ (show wpx) (+ "x" (+ (show hpx) ")")))))))

   ; --- the keyboard: GetKeyboardMapping -> keycodes for the bindings, then
   ;     GrabKey mod1+j / mod1+k / mod1+Return on the root. A grab means the
   ;     server routes those chords to US no matter who has input focus.
   nkc (+ 1 (- maxkc minkc))
   _ (w8 101) _ (w8 0) _ (w16 2) _ (w8 minkc) _ (w8 nkc) _ (w16 0)
   _ (flush s)
   _ (r8 ()) kpc (r8 ())                            ; reply, keysyms-per-keycode
   _ (r16 ()) _ (r32 ()) _ (skip 24)
   km (tablet 8)                                    ; keysym -> its first keycode
   _ (: (go i) (? (< i (* nkc kpc))
                  (: sym (r32 ())
                     _ (? (&& (|| (= sym 106) (|| (= sym 107) (= sym 65293)))
                             (! (peep km sym 0)))
                          (pin km sym (+ minkc (// i kpc))) 0)
                     (go (+ i 1)))
                  0)
        (go 0))
   kj (peep km 106 0) kk (peep km 107 0) kret (peep km 65293 0)
   (grab kc) (? (< kc 1) 0                          ; GrabKey: mod1, async/async
                (: _ (w8 33) _ (w8 1) _ (w16 4) _ (w32 root)
                   _ (w16 8) _ (w8 kc) _ (w8 1) _ (w8 1)
                   _ (w8 0) _ (w8 0) (w8 0)))
   _ (grab kj) _ (grab kk) _ (grab kret)
   _ (flush s)
   _ (line (+ "wm: grabbed mod1+j/k/ret, keycodes " (+ (show kj) (+ " " (+ (show kk) (+ " " (show kret)))))))

   ; --- the pure core: the stack zipper (test/wm.l, spike-local copy) ---
   (stk f up dn) [f up dn]
   (whole zs) (? (two? zs) (cat (rev (caup zs)) (link (cap zs) (caup (cup zs)))) ())
   (srev zs) (? (two? zs) (stk (cap zs) (caup (cup zs)) (caup zs)) ())
   (fup zs) (? (! (two? zs)) ()
               (: up (caup zs)
                  (? (two? up) (stk (cap up) (cup up) (link (cap zs) (caup (cup zs))))
                     (: all (rev (link (cap zs) (caup (cup zs))))
                        (stk (cap all) (cup all) ())))))
   (fdn zs) (srev (fup (srev zs)))
   (wmaster zs) (? (! (two? zs)) () (stk (cap zs) () (cat (rev (caup zs)) (caup (cup zs)))))
   (sput w zs) (? (two? zs) (stk w (caup zs) (link (cap zs) (caup (cup zs)))) (stk w () ()))
   (snub w l) (filter (\ x (! (= x w))) l)
   (srm w zs) (? (! (two? zs)) ()
                 (: f (cap zs) up (snub w (caup zs)) dn (snub w (caup (cup zs)))
                    (? (! (= f w)) (stk f up dn)
                       (two? dn) (stk (cap dn) up (cup dn))
                       (two? up) (stk (cap up) (cup up) ())
                       ())))
   (has? zs w) (any (\ x (= x w)) (whole zs))
   (focusw win) (: _ (w8 42) _ (w8 1) _ (w16 3) _ (w32 win) (w32 0))   ; SetInputFocus, revert PointerRoot
   (refocus zs) (? (two? zs) (: _ (focusw (cap zs)) (flush s)) 0)

   ; --- Tall: the row (master first) -> ((win x y w h) ..) ---
   (nth l i) (? (< i 1) (cap l) (nth (cup l) (- i 1)))
   (tile row)
    (: n (tally row)
       (? (< n 1) ()
          (= n 1) [[(cap row) 0 0 wpx hpx]]
          (: mw (// wpx 2) k (- n 1) rh (// hpx k)
             (link [(cap row) 0 0 mw hpx]
                   (: (go i l) (? (! (two? l)) ()
                                  (link [(cap l) mw (* i rh) (- wpx mw)
                                         (? (= i (- k 1)) (- hpx (* (- k 1) rh)) rh)]
                                        (go (+ i 1) (cup l))))
                      (go 0 (cup row)))))))
   (retile zs)
    (: (go ts) (? (! (two? ts)) 0
                  (: t (cap ts)
                     _ (configure (nth t 0) (nth t 1) (nth t 2) (nth t 3) (nth t 4))
                     (go (cup ts))))
       _ (go (tile (whole zs)))
       (flush s))

   ; --- the event loop over the zipper. live = managed at least once, so the
   ;     wm parks cleanly when its last client leaves (the spike's exit; a real
   ;     session would run on).
   (loop zs live)
    (: b0 (r8 ())
       (? (= b0 0)
          (: code (r8 ()) _ (skip 30)
             _ (line (+ "wm: X error, code " (show code))) (loop zs live))
          (: code (& 127 b0)
             (? (= code 20)                          ; MapRequest: manage + tile + map
                (: _ (r8 ()) _ (r16 ()) _ (r32 ()) win (r32 ()) _ (skip 20)
                   zs2 (? (has? zs win) zs (sput win zs))
                   _ (retile zs2)
                   _ (mapwin win) _ (flush s)
                   _ (line (+ "wm: + " (+ (show win) (+ " row " (show (whole zs2))))))
                   (loop zs2 1))
                (= code 19)                          ; MapNotify: NOW it is viewable -- focus it
                (: _ (r8 ()) _ (r16 ()) _ (r32 ()) win (r32 ()) _ (skip 20)
                   _ (? (&& (two? zs) (= win (cap zs))) (refocus zs) 0)
                   (loop zs live))
                (= code 2)                           ; KeyPress: the grabbed chords drive the zipper
                (: kc (r8 ()) _ (skip 30)
                   (? (! (two? zs)) (loop zs live)
                      (= kc kj)                      ; mod1+j: focus down
                      (: zs2 (fdn zs) _ (refocus zs2)
                         _ (line (+ "wm: focus -> " (show (cap zs2))))
                         (loop zs2 live))
                      (= kc kk)                      ; mod1+k: focus up
                      (: zs2 (fup zs) _ (refocus zs2)
                         _ (line (+ "wm: focus -> " (show (cap zs2))))
                         (loop zs2 live))
                      (= kc kret)                    ; mod1+Return: the focused window becomes master
                      (: zs2 (wmaster zs) _ (retile zs2) _ (refocus zs2)
                         _ (line (+ "wm: master -> " (+ (show (cap zs2)) (+ " row " (show (whole zs2))))))
                         (loop zs2 live))
                      (loop zs live)))
                (= code 17)                          ; DestroyNotify: unmanage + tile
                (: _ (r8 ()) _ (r16 ()) _ (r32 ()) win (r32 ()) _ (skip 20)
                   (? (! (has? zs win)) (loop zs live)
                      (: zs2 (srm win zs)
                         _ (retile zs2)
                         _ (refocus zs2)
                         _ (line (+ "wm: - " (+ (show win) (+ " row " (show (whole zs2))))))
                         (? (&& live (! (two? zs2))) (line "wm: empty -- down")
                            (loop zs2 live)))))
                (= code 23)                          ; ConfigureRequest
                (: _ (r8 ()) _ (r16 ()) _ (r32 ()) win (r32 ()) _ (skip 4)
                   x (r16 ()) y (r16 ()) w (r16 ()) h (r16 ()) _ (r16 ())
                   _ (r16 ()) _ (skip 4)
                   _ (? (has? zs win) (retile zs)    ; managed: the layout answers, not the ask
                        (: _ (configure win x y w h) (flush s)))
                   (loop zs live))
                (: _ (skip 31) (loop zs live))))))   ; other Substructure noise: skip
   _ (loop () 0)
   _ (close s)
   (line "wm: done"))
