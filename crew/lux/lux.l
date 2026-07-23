; crew/lux/lux.l -- the driver: wire the pure core (crew/lux/core.l), the layout engine
; (crew/lux/layout.l), the manageHook (crew/lux/manage.l), the keymap (crew/lux/keys.l), and EWMH
; (crew/lux/ewmh.l) over the X protocol (crew/lux/wire.l), configured by crew/lux/config.l.
; Concatenate them:
;   cat crew/lux/{core,layout,wire,ewmh,manage,keys,config,lux}.l | love
; (against `Xephyr :7 -ac`, or a real server with lux-auth set). Rung 8: the
; driver reads the CONFIG -- border width and colors (focused/normal pixels),
; workspaces, the layout stack, hooks, keymap, startup spawns -- and runs until
; the display dies or (quit). The f_t hinge: a float-focused chord moves/
; resizes the float, a tile-focused one messages the layout / navigates. ffoc
; tracks the focused FLOAT (the zipper only holds tiles); hist is nextMatch's
; MRU focus ring.

; --- the lux repl: a unix-socket moor into the RUNNING wm (the dock's shape,
; crew-side; `ain <sock>` is the client). Forms eval in the book, so every
; global is a verb; the LIVE (sh lay hist ffoc) state rides lux-cell -- reads
; take the snapshot ((lux-st ())), changes queue a state function through
; (lux! f), and the driver's loop drains the queue at its next X event,
; GUARDED: a change must answer a 4-list whose sheaf is wssane? -- the proved
; invariant, standing as a live seatbelt -- or it is refused and the old
; state kept. ONE HAND ON THE WIRE: the repl never touches any X socket (the
; haven painter's law, absolute); with layout.l's open dispatch lane,
; (lay! (L my-closure state..)) swaps in a layout written at the prompt.
; The socket is an EVAL SERVER -- $XDG_RUNTIME_DIR (0700) when set, /tmp
; otherwise (a socket refuses connect without write permission).
(: lux-cell #()
   (lux-st u) (peep lux-cell 'st ())
   ; a change QUEUES; the driver's loop drains at its next X event. On a live
   ; desktop that is imminent (any motion, focus or property tick); a headless
   ; harness nudges with any event of its own (boot/luxui-probe.l sends a
   ; no-op ClientMessage). Deliberately NO wake wire from lux's side: a poke
   ; connection made Xvfb close-down an INNOCENT client whose id range rode a
   ; recycled slot (gdb: ospoll_wait -> CloseDownClient -> FreeClientResources
   ; with the victim's socket provably open) -- one hand on the wire, and the
   ; wire is the driver's alone.
   (lux! f) (: _ (pin lux-cell 'q (link f (peep lux-cell 'q ())))             ; (a peep+pin pair -- a concurrent drain in the gap can lose
                'queued)                                                         ;  a change; prompt-scale odds, and wssane? guards either way)
   ; the command sugar -- each a (lux! ..) over the snapshot shape (sh lay hist ffoc)
   (lay! l)  (lux! (\ st (L (cap st) l (nthl st 2) (nthl st 3))))
   (msg! m)  (lux! (\ st (: lay2 (lay-msg (caup st) m (wsget (cap st) (wscur (cap st))))
                             (L (cap st) (? lay2 lay2 (caup st)) (nthl st 2) (nthl st 3)))))
   (sink! w) (lux! (\ st (L (wssink (cap st) w) (caup st) (nthl st 2)
                             (: f (nthl st 3) (? (= f w) () f)))))
   (float! w r) (lux! (\ st (L (wsdofloat (cap st) w r) (caup st) (nthl st 2) w)))
   (row u) (wsrow (cap (lux-st ())))

   ; the moor: shore the socket, twirl a session per client -- read one form
   ; (sound keeps the read protocol), eval through the self-hosted ev, show
   ; the value back. deliberate orphans, like the dock's: lux runs forever.
   (lux-sock u) (: rd (getenv "XDG_RUNTIME_DIR")
                    base (? (< 1 (tally rd)) rd "/tmp")
                    (+ base (+ "/lux-" (+ (lux-dispnum ()) ".sock"))))
   ; the session reads LINES (see parks the TASK -- the reader's park law) and
   ; sounds them off a tap: a memory port keeps the read protocol and cannot
   ; block, where (sound c ..) on the socket itself stalls the WHOLE vm in the
   ; C parse the moment the fd runs dry mid-skip -- an idle attached client
   ; would freeze the wm between asks (haven's painter starved on exactly
   ; this). an unfinished shape carries to the next line; anything after the
   ; first form on a line is let go.
   (lux-session c)
    (: e (nom 0)
       (rdline acc) (: ch (see c)
                       (? (= ch -1) ()
                          (= ch 10) (rev (link 10 acc))
                          (rdline (link ch acc))))
       (go pend) (: cl (rdline ())
                    (? (! (two? cl)) 0
                       (: full (cat pend cl)
                          p (tap full)
                          r (sound p e)
                          (? (id? r e) (go ())
                             (id? r p) (go full)
                             (: v (ev 'ev r)
                                _ (say c (+ (show v) "\n"))
                                _ (flush c)
                                (go ()))))))
       (go ()))
   (lux-moor path)
    (: lp (shore path)
       (? (! (lit? lp)) (: _ (say out (+ "lux: no moor at " (+ path "\n"))) (flush out))
          (: _ (say out (+ "lux: moored at " (+ path "\n")))
             _ (flush out)
             (knock _) (: _ (await lp)
                          c (accept lp)
                          _ (? (lit? c) (twirl (\ _ (: _ (lux-session c) (close c))) 0) 0)
                          (knock ()))
             (knock ())))))

; a welp-face help so a scare returns a visible value (to the repl client, or
; at a driver raise site) instead of dropping the wm -- the dock's shape: keep
; the read protocol, answer ('scare a b) on a bare scare. a leaked top-level
; global, since the book is mopped post-egg.
(: (help s a b) (? (eof? s) b (more? s) a `('scare a b)))

(: s (x-connect lux-display)
   _ (? s 0 (: _ (say err "lux: no display -- Xephyr up?\n") (quit 1)))
   (line m) (: _ (say out m) _ (say out "\n") (flush out))
   sc (x-setup s lux-auth)
   _ (? sc 0 (: _ (say err "lux: setup refused (auth?)\n") (quit 1)))
   root (scr-root sc) wpx (scr-w sc) hpx (scr-h sc)
   _ (? (x-redirect s root) 0 (: _ (say err "lux: another WM owns this display\n") (quit 1)))
   _ (line (+ "lux: managing " (+ (show root) (+ " " (+ (show wpx) (+ "x" (show hpx)))))))
   _ (ewmh-init s root (scr-idbase sc) (tally lux-tags))                          ; the support window + _NET_* root properties

   ; --- the config's face: border pixels, the root cursor, the startup spawns ---
   npix (x-color s (scr-cmap sc) lux-normal-color)
   fpix (x-color s (scr-cmap sc) lux-focused-color)
   _ (line (+ "lux: colors " (+ (show npix) (+ " " (show fpix)))))
   _ (x-cursor s root (+ (scr-idbase sc) 1) (+ (scr-idbase sc) 2))               ; setDefaultCursor xC_left_ptr
   _ (map (\ cmd (spawn cmd)) lux-startup)

   ; --- keyboard: resolve the keymap's keysyms to keycodes, grab every chord ---
   km (x-keymap s (scr-mink sc) (scr-maxk sc) (keys-syms lux-keymap))
   ktab (filter (\ r (> (caup r) 0))                                             ; rows (mods keycode action); keysym absent -> dropped
                (map (\ r (L (kb-mods r) (peep km (kb-sym r) 0) (kb-act r))) lux-keymap))
   _ (map (\ r (x-grab s root (cap r) (caup r))) ktab)
   _ (x-flush s)
   _ (line (+ "lux: grabbed " (+ (show (tally ktab)) " chords")))

   ; --- rendering (the layout engine answers, X obeys) ---
   (nth l i) (? (< i 1) (cap l) (nth (cup l) (- i 1)))
   scrn (L 0 0 wpx hpx)
   (trect u) (ewmh-rect scrn wpx hpx)                                            ; the tiling rect: screen minus dock struts (sup+b flips)

   ; --- the unmap ledger (xmonad's mapped + waitingUnmap): map!/hide! track our
   ;     own INTENT at send time -- mapd holds the windows we have mapped, owed
   ;     counts the unmap echoes we're owed back. A real UnmapNotify with no
   ;     echo owed is the CLIENT's own unmap: a withdraw, whether or not the
   ;     ICCCM courtesy synthetic ever comes (a toolkit that skips it still
   ;     gets evicted instead of a window that can never disappear). ---
   mapd (tablet 8) owed (tablet 8)
   (map! w) (: _ (pin mapd w 1) (x-map s w))
   (hide! w) (: _ (? (peep mapd w 0) (pin owed w (+ 1 (peep owed w 0))) 0)       ; only a mapped window echoes -- an unmap of the
                _ (pull mapd w 0) (x-unmap s w))                                 ; already-hidden fires no event, so no count
   (unled! w) (: _ (pull mapd w 0) (pull owed w 0))                              ; the window left management: clear its ledger
   (render sh lay ff)                                                            ; ask the layout for placements, configure them, unmap the dropped
    (: z (wsget sh (wscur sh)) f (? ff ff (zfoc z))                              ; ff: the float holding the keyboard, else the tiled top
       pls (cap (lay-tile lay (trect ()) z)) placed (map cap pls)
       _ (map (\ t (: w (nth t 0) bw (? (< (tally t) 6) lux-border (nth t 5))   ; smartBorders sets 0 on a singleton/fullscreen
                      _ (x-border s w bw)
                      _ (x-borderpix s w (? (= w f) fpix npix))                  ; focusedBorderColor / normalBorderColor
                      (x-configure s w (nth t 1) (nth t 2)                       ; the border sits OUTSIDE the frame: shave it off
                                   (mx2 1 (- (nth t 3) (* 2 bw))) (mx2 1 (- (nth t 4) (* 2 bw)))))) pls)
       _ (map (\ w (? (any (\ p (= p w)) placed) 0 (hide! w))) (wsrow sh))       ; windows the layout dropped (Full/minimize): hide them
       _ (map (\ w (map! w)) placed)                                             ; (re)map placed -- a no-op on the already-mapped
       _ (map (\ fp (: w (cap fp) fr (caup fp)                                   ; floating windows AT HOME HERE: place, map, raise above the tiles
                        bw (? (= fr scrn) 0 lux-border)                         ; a fullscreen float (the whole screen is its rect) wears no border
                        _ (x-border s w bw) _ (x-borderpix s w (? (= w f) fpix npix))
                        _ (x-configure s w (nth fr 0) (nth fr 1) (nth fr 2) (nth fr 3))
                        _ (map! w) (x-raise s w))) (wsvisfl sh))
       (x-flush s))
   (refocus sh) (? (wsfoc sh) (: _ (x-focus s (wsfoc sh)) (x-flush s)) 0)        ; focus the current top -- only when it is mapped/viewable
   (fmark sh pf w) (: _ (x-borderpix s w fpix) p (? pf pf (wsfoc sh))            ; keyboard to float w sans re-render: it wears the focused
                      (? (&& p (! (= p w))) (x-borderpix s p npix) 0))           ; border, the previous effective focus hands it back
   (watch w) (: _ (x-cwa-events s w 16) (x-grabbtn s w))                         ; the mouse wires: EnterWindow selected (focus follows the
                                                                                 ; pointer in) + buttons 1-3 sync-grabbed (click-to-focus)
   (switch sh lay t)                                                             ; view: hide the leaving row + floats, show the arriving
    (? (|| (= t (wscur sh)) (! (wsknown? sh t))) sh
       (: old (wsrow sh) oldf (map cap (wsvisfl sh)) sh2 (wsview sh t)
          _ (map (\ w (map! w)) (wsrow sh2))
          _ (render sh2 lay ())
          _ (map (\ w (hide! w)) old)
          _ (map (\ w (hide! w)) oldf) _ (x-flush s)
          _ (line (+ "lux: view " (+ (show t) (+ " " (show (wsrow sh2))))))
          sh2))
   (msg-or old new) (? new new old)                                              ; keep the old layout when a message is unhandled (())
   (lux-self u)                                                                   ; our script path, either argv shape: plain `love lux`
    (: (go m) (? (! (two? m)) (cap argv)                                         ; gives (script ..); the -l shebang gives the RAW OS
                 (= (cap m) "-l") (? (two? (cup m)) (caup m) (cap argv))         ; argv (love -l script ..) -- take the -l operand
                 (go (cup m)))
       (go argv))

   ; --- do-act: interpret one keys.l action. Returns by looping. ---
   (do-act a sh lay hist ffoc)
    (: k (cap a) z (wsget sh (wscur sh)) fw (? ffoc ffoc (wsfoc sh))
       (relay sh2 lay2 f2) (: flf (? (&& f2 (wsfloating? sh2 f2)) f2 ())         ; a float focus takes the keyboard directly; else the tiled top
                              _ (render sh2 lay2 flf)
                              _ (? flf (: _ (x-focus s flf) (x-flush s)) (refocus sh2))
                              (loop sh2 lay2 hist f2))
       (cyc d) (: nx (rnext (wsring sh) fw d)                                    ; cycle the ring: tiles, then the floats at home
                  (? (! nx) (loop sh lay hist ffoc)
                     (wsfloating? sh nx) (relay sh lay nx)                        ; a float takes the keyboard (relay focuses it)
                     (relay (wsmap sh (zfocto nx)) lay ())))
       (? (= k 'msg) (relay sh (msg-or lay (lay-msg lay (caup a) z)) ffoc)
          (= k 'focus-dn) (cyc 1)
          (= k 'focus-up) (cyc -1)
          (= k 'focus-master) (relay (wsmap sh (\ zz (zfocto (? (two? (zrow zz)) (cap (zrow zz)) ()) zz))) lay ())
          (= k 'swap-dn) (relay (wsmap sh zswapdn) lay ffoc)
          (= k 'swap-up) (relay (wsmap sh zswapup) lay ffoc)
          (= k 'swap-master) (relay (wsmap sh zmaster) lay ffoc)
          (= k 'view) (: sh2 (switch sh lay (caup a))                            ; a float focus stays only if its home is the new view
                         (loop sh2 lay hist (? (&& ffoc (= (wscur sh2) (wstagof sh2 ffoc))) ffoc ())))
          (= k 'shift) (: t (caup a) sh2 (shiftwin sh fw t)                      ; the effective focus emigrates -- a float by its home
                          ff2 (? (&& (! (= sh2 sh)) (= ffoc fw)) () ffoc)
                          _ (render sh2 lay ff2)
                          _ (? (= sh2 sh) 0 (: _ (hide! fw) (x-flush s)))        ; it moved (always off this view): hide the emigrant
                          _ (line (+ "lux: sent " (+ (show fw) (+ " -> " (show t)))))
                          (loop sh2 lay hist ff2))
          (= k 'spawn) (: p (spawn (caup a)) _ (line (+ "lux: spawn " (+ (cap (caup a)) (+ " " (show p)))))
                          (loop sh lay hist ffoc))
          (= k 'exec) (: _ (line "lux: exec -- abdicating") _ (exec (caup a))     ; returns only on failure
                         (loop sh lay hist ffoc))
          (= k 'kill) (: _ (? fw (: _ (? (x-has-atom s fw "WM_PROTOCOLS" "WM_DELETE_WINDOW")   ; ask first (killWindow): KillClient
                                         (x-wmdel s fw) (x-kill s fw))           ; takes the whole CLIENT, every window it owns
                                     (x-flush s)) 0)                             ; DestroyNotify does the eviction
                         _ (line (+ "lux: kill " (show fw))) (loop sh lay hist ffoc))
          (= k 'quit) (line "lux: quit")                                          ; fall out of the loop
          (= k 'restart)                                                         ; restart-in-place: exec the SCRIPT (its shebang re-resolves
          (: _ (line (+ "lux: restart -- exec, pid " (show (getpid 0))))          ; love off PATH, so an upgraded binary is picked up -- exec'ing
             _ (exec (L (lux-self ())))                                           ; /proc/self/exe pins a deleted build forever); the CLOEXEC'd
             _ (exec (L "/proc/self/exe" (lux-self ())))                          ; socket drops, the redirect releases, the new generation
             _ (line "lux: restart failed") (loop sh lay hist ffoc))              ; ADOPTS the survivors (fallback: a dev run sans exec bit)
          (= k 'refresh) (relay sh lay ffoc)
          (= k 'reset-layout) (relay sh lux-layout ffoc)
          (= k 'toggle-struts) (: _ (ewmh-toggle ()) _ (line "lux: toggle struts") (relay sh lay ffoc))
          (= k 'sink) (? (wsfloating? sh fw) (relay (wssink sh fw) lay ()) (loop sh lay hist ffoc))
          (= k 'float-toggle)
          (? (! fw) (loop sh lay hist ffoc)
             (wsfloating? sh fw) (relay (wssink sh fw) lay ())
             (: _ (line (+ "lux: float " (show fw))) (relay (wsdofloat sh fw (x-getgeom s fw)) lay fw)))
          (= k 'next-match)
          (: t (nm-pick (wsring sh) fw hist (caup a))                            ; the whole ring -- floats too, anchored at the
             (? (! t) (loop sh lay hist ffoc)                                    ; EFFECTIVE focus
                (wsfloating? sh t) (relay sh lay t)
                (relay (wsmap sh (zfocto t)) lay ())))
          ; --- the directional trio: the f_t hinge ---
          (wsfloating? sh fw)                                                    ; float focused: move (go) or resize (swapd/resize)
          (: r (wsrect sh fw) d (nthl a 2) dx (cap d) dy (caup d)
             r2 (? (= k 'go) (L (+ (nth r 0) dx) (+ (nth r 1) dy) (nth r 2) (nth r 3))
                   (L (nth r 0) (nth r 1) (mx2 20 (+ (nth r 2) dx)) (mx2 20 (+ (nth r 3) dy))))
             (relay (wsdofloat sh fw r2) lay ffoc))
          (: dir (caup a) pls (cap (lay-tile lay (trect ()) z)) tgt (nav-target pls (wsfoc sh) dir)
             (? (= k 'go) (? tgt (relay (wsmap sh (zfocto tgt)) lay ()) (loop sh lay hist ffoc))
                (= k 'swapd) (? tgt (relay (wsmap sh (zswapw (wsfoc sh) tgt)) lay ffoc) (loop sh lay hist ffoc))
                (relay sh (msg-or lay (lay-msg lay (? (|| (= dir 'left) (= dir 'down)) 'shrink 'expand) z)) ffoc)))))

   ; --- pointer drag (xmonad's mouseBindings): the window follows the pointer
   ;     ('move) or its far corner does ('resize). g0 is the start rect, (rx0 ry0)
   ;     the start pointer; a release/motion carries the live pointer at bytes
   ;     20/22. The FINAL rect is recomputed from the release position, so no
   ;     rect need thread through the loop. Other events are dropped for the
   ;     drag's duration -- the pointer is grabbed, the user is not spawning. ---
   (drag-rect mode g0 rx0 ry0 rx ry)
    (: dx (- rx rx0) dy (- ry ry0)
       (? (= mode 'move) (L (+ (nth g0 0) dx) (+ (nth g0 1) dy) (nth g0 2) (nth g0 3))
          (L (nth g0 0) (nth g0 1) (mx2 40 (+ (nth g0 2) dx)) (mx2 40 (+ (nth g0 3) dy)))))
   (drag mode w g0 rx0 ry0)
    (: e (x-next s)
       (? (! e) g0                                                              ; display gone: keep the start rect
          (= (ev-code e) 6) (: pkt (ev-raw e)                                   ; MotionNotify: reconfigure live
                                g1 (drag-rect mode g0 rx0 ry0 (u16 pkt 20) (u16 pkt 22))
                                _ (x-configure s w (nth g1 0) (nth g1 1) (nth g1 2) (nth g1 3)) _ (x-flush s)
                                (drag mode w g0 rx0 ry0))
          (= (ev-code e) 5) (: pkt (ev-raw e) (drag-rect mode g0 rx0 ry0 (u16 pkt 20) (u16 pkt 22)))   ; ButtonRelease: settle
          (drag mode w g0 rx0 ry0)))                                            ; anything else mid-drag: ignore

   ; --- the repl gate: every pass through the loop publishes the snapshot and
   ;     drains (lux!) changes, each GUARDED -- a change must answer a 4-list
   ;     whose sheaf is wssane? (the proved invariant, live) or it is refused
   ;     and the old state kept. a scare inside a change answers ('scare a b)
   ;     through the help above -- not a 4-list, so it refuses the same way. ---
   (loop sh lay hist ffoc)
    (: q (pull lux-cell 'q ())
       (? (! (two? q)) (: _ (pin lux-cell 'st (L sh lay hist ffoc))
                          (loop2 sh lay hist ffoc))
          (: st (foldl (\ acc f (: r (f acc)
                                   (? (&& (two? r) (&& (= 4 (tally r)) (wssane? (cap r)))) r
                                      (: _ (line "lux: change refused") acc))))
                       (L sh lay hist ffoc) (rev q))
             _ (pin lux-cell 'st st)                                            ; the snapshot rides POST-drain, so a moor read sees the change
             sh2 (cap st) lay2 (caup st) f2 (nthl st 3)
             flf (? (&& f2 (wsfloating? sh2 f2)) f2 ())
             _ (render sh2 lay2 flf)
             _ (? flf (: _ (x-focus s flf) (x-flush s)) (refocus sh2))
             _ (line (+ "lux: changed x" (show (tally q))))
             (loop2 sh2 lay2 (nthl st 2) f2))))

   ; --- the event loop: (sheaf, layout, focus-history, focused-float).
   ;     the MRU param is hist0, the fresh ring hist: a `:` binding SHADOWS a
   ;     same-named parameter over the WHOLE form (one scope, no escape), so
   ;     `hist (?  .. hist ..)` read the zero point every pass -- the ring
   ;     never accumulated. name the parameter apart and the ring lives. ---
   (loop2 sh lay hist0 ffoc)
    (: f (? ffoc ffoc (wsfoc sh))                                                ; the MRU rides the EFFECTIVE focus: floats enter too
       hist (? (&& f (! (= f (? (two? hist0) (cap hist0) ())))) (link f (znub f hist0)) hist0)
       (relay sh2 lay2 f2) (: flf (? (&& f2 (wsfloating? sh2 f2)) f2 ())         ; render, focus the float or the tiled top, loop (do-act has its own)
                              _ (render sh2 lay2 flf)
                              _ (? flf (: _ (x-focus s flf) (x-flush s)) (refocus sh2))
                              (loop sh2 lay2 hist f2))
       _ (hear 0)                                                                ; hear one spawned zombie home, if any
       _ (ewmh-sync s sh)                                                        ; publish the sheaf to pagers (change-guarded)
       e (x-next s)
       (? (! e) (line "lux: display gone")
          (= (ev-code e) 0) (: _ (line (+ "lux: X error " (show (ev-detail e)))) (loop sh lay hist ffoc))
          (: code (ev-code e) win (ev-window e)
             (? (&& (= code 20) (wsknownw? sh win))                              ; MapRequest from a window we already manage: IGNORE it
                (: _ (line (+ "lux: remap " (show win))) (loop sh lay hist ffoc))   ; (mapping it here would paint it OVER a workspace it
                (= code 20)                                                      ; isn't on -- the "one window on two workspaces" bug)
                                                                                 ; MapRequest: admit tiled, run the manageHook, then place
                (: _ (? (ewmh-track s win) (line (+ "lux: strut " (show win))) 0) ; a dock's strut shrinks the tiling rect
                   sh1 (admit sh win)
                   c (mg-ctx s win scrn (x-getgeom s win))
                   sh2 (manage-window lux-manage c sh1)
                   ffoc2 (? (wsfloating? sh2 win) win ffoc)                      ; a hook-floated window takes the float focus
                   (? (wsknownw? sh2 win)                                        ; managed (tiled or floated) -> place it
                      (: _ (x-chprop32 s win (x-intern s "WM_STATE") (x-intern s "WM_STATE") (L 1 0))   ; the ICCCM stamp: a restart finds us even hidden
                         _ (watch win)
                         _ (render sh2 lay ffoc2) _ (map! win)
                         _ (? (wsfloating? sh2 win) (x-focus s win) 0)          ; a freshly-mapped float (a dialog) takes the keyboard at once
                         _ (x-flush s)
                         _ (line (+ "lux: + " (+ (show win) (+ " " (show (wsrow sh2)))))) (loop sh2 lay hist ffoc2))
                      (: _ (x-map s win)                                         ; ignored: map raw -- a desktop UNDER everything, a dock/notification on top
                         _ (? (x-has-atom s win "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DESKTOP")
                              (x-lower s win) (x-raise s win))
                         _ (x-flush s)
                         _ (line (+ "lux: ignore " (show win))) (loop sh2 lay hist ffoc))))
                (= code 19) (: _ (? (= win (wsfoc sh)) (: _ (x-focus s win) (x-flush s)) 0) (loop sh lay hist ffoc))   ; MapNotify: focus if it's the top
                (= code 17)                                                      ; DestroyNotify: unmanage + tile
                (: _ (ewmh-untrack win) _ (unled! win)
                   (? (! (wsknownw? sh win)) (loop sh lay hist ffoc)
                      (: sh2 (evict sh win) ffoc2 (? (= win ffoc) () ffoc)
                         _ (render sh2 lay ffoc2)
                         _ (line (+ "lux: - " (+ (show win) (+ " " (show (wsrow sh2))))))
                         (loop sh2 lay hist ffoc2))))
                (= code 18)                                                     ; UnmapNotify: xmonad's waitingUnmap rule. Our own hides echo
                (? (! (wsknownw? sh win)) (loop sh lay hist ffoc)                ; back COUNTED (the owed ledger); a synthetic (send-event bit
                   (: syn (< 127 (cap (ev-raw e))) p (peep owed win 0)           ; on the raw byte) is a withdraw (ICCCM 4.1.4) -- and so is a
                      (? (&& (! syn) (< 0 p))                                    ; real one we're owed NO echo for: the CLIENT unmapped it,
                         (: _ (pin owed win (- p 1)) (loop sh lay hist ffoc))    ; courtesy synthetic or not
                         (: _ (ewmh-untrack win)
                            _ (x-delprop s win (x-intern s "WM_STATE"))          ; unstamp: a restart must NOT re-adopt the withdrawn (the
                            sh2 (evict sh win) ffoc2 (? (= win ffoc) () ffoc)
                            _ (x-unmap s win)                                    ; a crossing's render in the event gap may have re-mapped it
                                                                                 ; (telegram's media viewer) -- withdrawn means UNMAPPED, so say so
                            _ (unled! win)
                            _ (render sh2 lay ffoc2) _ (x-flush s)               ; tray ghost); a re-map arrives as a fresh MapRequest
                            _ (line (+ "lux: withdraw " (show win)))
                            (loop sh2 lay hist ffoc2)))))
                (= code 7)                                                      ; EnterNotify: focus follows the pointer (normal crossings
                (: pkt (ev-raw e) w (u32 pkt 12)                                 ; only -- the window is the EVENT field, byte 12)
                   (? (|| (! (= (nthl pkt 30) 0)) (= (ev-detail e) 2))           ; mode Grab/Ungrab or detail Inferior: not a real entry
                      (loop sh lay hist ffoc)
                      (|| (! (wsknownw? sh w)) (= w (? ffoc ffoc (wsfoc sh))))
                      (loop sh lay hist ffoc)                                    ; unmanaged, or already the EFFECTIVE focus: still
                      (wsfloating? sh w) (: _ (fmark sh ffoc w) _ (x-focus s w) _ (x-flush s) (loop sh lay hist w))
                      (: sh2 (wsmap sh (zfocto w)) _ (render sh2 lay ()) _ (refocus sh2) (loop sh2 lay hist ()))))
                (= code 4)                                                      ; ButtonPress via the sync grab: super+drag moves/resizes a
                (: pkt (ev-raw e) w (u32 pkt 12) btn (ev-detail e) st (ev-state e)   ; float (xmonad mouseBindings); a plain click focuses
                   (? (&& (& st supm) (&& (wsknownw? sh w) (|| (= btn 1) (= btn 3))))   ; sup+btn1 move, sup+btn3 resize -- float first, then drag
                      (: sh2 (? (wsfloating? sh w) sh (wsdofloat sh w (x-getgeom s w)))
                         g0 (x-getgeom s w) rx0 (u16 pkt 20) ry0 (u16 pkt 22)
                         _ (x-focus s w) _ (x-raise s w)
                         _ (x-grabptr s w) _ (x-allow-async s) _ (x-flush s)     ; capture motion, thaw the sync freeze WITHOUT replaying
                         gf (drag (? (= btn 1) 'move 'resize) w g0 rx0 ry0)
                         _ (x-ungrabptr s) _ (x-flush s)
                         _ (line (+ (? (= btn 1) "lux: moved " "lux: resized ") (show w)))
                         (relay (wsdofloat sh2 w gf) lay w))
                      (&& (& st supm) (&& (wsknownw? sh w) (= btn 2)))           ; sup+btn2: promote the clicked window to master (shiftMaster)
                      (: _ (x-allow-async s) _ (x-flush s)
                         (relay (wsmap sh (\ zz (zmaster (zfocto w zz)))) lay ffoc))
                      (: _ (x-replay s) _ (x-flush s)                           ; a plain click: hand it to the app, then click-to-focus
                         (? (|| (! (wsknownw? sh w)) (= w (? ffoc ffoc (wsfoc sh)))) (loop sh lay hist ffoc)
                            (wsfloating? sh w) (: _ (fmark sh ffoc w) _ (x-focus s w) _ (x-raise s w) _ (x-flush s) (loop sh lay hist w))
                            (: sh2 (wsmap sh (zfocto w)) _ (render sh2 lay ()) _ (refocus sh2) (loop sh2 lay hist ()))))))
                (= code 2)                                                       ; KeyPress: the table answers
                (: kc (ev-detail e) stm (& (ev-state e) kmodmask)
                   hit (afind (\ r (&& (= (cap r) stm) (= (caup r) kc))) ktab)
                   (? (two? hit) (do-act (nthl hit 2) sh lay hist ffoc)
                      (loop sh lay hist ffoc)))
                (= code 23)                                                      ; ConfigureRequest: TILED -> the layout answers, and the
                (? (&& (wsknownw? sh win) (! (wsfloating? sh win)))              ; client is TOLD so -- a synthetic ConfigureNotify with the
                   (: _ (render sh lay ffoc) _ (x-confnotify s win) _ (x-flush s)   ; real geometry (silence leaves VTE drawing a phantom grid)
                      (loop sh lay hist ffoc))
                   (: pkt (ev-raw e) mask (u16 pkt 26) g (x-getgeom s win)       ; unmanaged (pre-map): GRANT it -- a client WAITS on
                      gx (? (& mask 1) (u16 pkt 16) (nth g 0))                   ; this configure before mapping (deny = no MapRequest,
                      gy (? (& mask 2) (u16 pkt 18) (nth g 1))                   ; the 1x1 deadlock); fields absent from the value-mask
                      gw (? (& mask 4) (u16 pkt 20) (nth g 2))                   ; keep their current geometry
                      gh (? (& mask 8) (u16 pkt 22) (nth g 3))
                      _ (x-configure s win gx gy gw gh) _ (x-flush s)
                      sh2 (? (wsfloating? sh win) (wsdofloat sh win (L gx gy gw gh)) sh)   ; a FLOAT's request is granted too, and the
                      (loop sh2 lay hist ffoc)))                                 ; sheaf's rect follows it (xmonad: grant + re-float)
                (= code 33)                                                      ; ClientMessage: EWMH requests ride SendEvent to the root
                (: pkt (ev-raw e) cw (ewmh-cm-win pkt) ty (ewmh-cm-type pkt)
                   (fs? d) (= d (ewmh-atom "_NET_WM_STATE_FULLSCREEN"))
                   (mx? d) (|| (= d (ewmh-atom "_NET_WM_STATE_MAXIMIZED_VERT"))   ; either maximize axis reads as "maximize"
                              (= d (ewmh-atom "_NET_WM_STATE_MAXIMIZED_HORZ")))
                   (state-toggle rect atoms word)                               ; _NET_WM_STATE add/remove/toggle -> float at rect / sink
                     (: act (ewmh-cm-d0 pkt) on (wsfloating? sh cw)
                        want (? (= act 1) 1 (= act 0) 0 (? on 0 1))             ; 0 remove / 1 add / 2 toggle
                        (? (= want (? on 1 0)) (loop sh lay hist ffoc)
                           want (: sh2 (wsdofloat sh cw rect)
                                   _ (x-chprop32 s cw (ewmh-atom "_NET_WM_STATE") 4 atoms)
                                   _ (render sh2 lay cw) _ (x-focus s cw) _ (x-flush s)
                                   _ (line (+ "lux: " (+ word (+ " " (show cw))))) (loop sh2 lay hist cw))
                           (: sh2 (wssink sh cw)
                              _ (x-chprop32 s cw (ewmh-atom "_NET_WM_STATE") 4 ())
                              _ (render sh2 lay ()) _ (line (+ "lux: un" (+ word (+ " " (show cw))))) (loop sh2 lay hist ()))))
                   (? (&& (= ty (ewmh-atom "_NET_WM_STATE"))                     ; _NET_WM_STATE add/remove/toggle FULLSCREEN (ewmhFullscreen)
                          (&& (wsknownw? sh cw) (|| (fs? (ewmh-cm-d1 pkt)) (fs? (ewmh-cm-d2 pkt)))))
                      (state-toggle scrn (L (ewmh-atom "_NET_WM_STATE_FULLSCREEN")) "fullscreen")   ; over the whole screen, struts and all
                      (&& (= ty (ewmh-atom "_NET_WM_STATE"))                     ; MAXIMIZED_VERT/HORZ -> float over the tiling rect (panel kept)
                          (&& (wsknownw? sh cw) (|| (mx? (ewmh-cm-d1 pkt)) (mx? (ewmh-cm-d2 pkt)))))
                      (state-toggle (trect ()) (L (ewmh-atom "_NET_WM_STATE_MAXIMIZED_VERT") (ewmh-atom "_NET_WM_STATE_MAXIMIZED_HORZ")) "maximize")
                      (&& (= ty (ewmh-atom "WM_CHANGE_STATE")) (= (ewmh-cm-d0 pkt) 3))   ; ICCCM iconify -> minimize (minimizeEventHook)
                      (: lay2 (msg-or lay (lay-msg lay (L 'minimize-window cw) (wsget sh (wscur sh))))
                         _ (render sh lay2 ffoc) _ (line (+ "lux: minimize " (show cw))) (loop sh lay2 hist ffoc))
                      (&& (= ty (ewmh-atom "_NET_CLOSE_WINDOW")) (wsknownw? sh cw))   ; a panel/pager closes a window -- gracefully if it can
                      (: _ (? (x-has-atom s cw "WM_PROTOCOLS" "WM_DELETE_WINDOW") (x-wmdel s cw) (x-kill s cw))
                         _ (x-flush s) _ (line (+ "lux: close " (show cw))) (loop sh lay hist ffoc))
                      (= ty (ewmh-atom "_NET_CURRENT_DESKTOP"))                  ; a pager switches the desktop
                      (loop (switch sh lay (+ (ewmh-cm-d0 pkt) 1)) lay hist ffoc)
                      (= ty (ewmh-atom "_NET_WM_DESKTOP"))                       ; a pager drags a window to another desktop (W.shiftWin)
                      (: t (+ (ewmh-cm-d0 pkt) 1) sh2 (shiftwin sh cw t)
                         (? (= sh2 sh) (loop sh lay hist ffoc)                   ; unknown window/tag (or sticky 0xffffffff): a no-op
                            (: ff2 (? (&& (= ffoc cw) (! (= t (wscur sh2)))) () ffoc)
                               _ (render sh2 lay ff2)                            ; arriving on the current view: render maps it...
                               _ (? (= t (wscur sh2)) 0 (: _ (hide! cw) (x-flush s)))   ; ...leaving it: hide the emigrant
                               _ (line (+ "lux: sent " (+ (show cw) (+ " -> " (show t)))))
                               (loop sh2 lay hist ff2))))
                      (= ty (ewmh-atom "_NET_ACTIVE_WINDOW"))                    ; a pager activates: restore if minimized, view its tag, focus
                      (: z (wsget sh (wscur sh))
                         lay2 (msg-or lay (lay-msg lay (L 'restore-window cw) z))
                         t (wstagof sh cw)
                         (? (! t) (: _ (render sh lay2 ffoc) (loop sh lay2 hist ffoc))
                            (: sh2 (wsmap (switch sh lay2 t) (zfocto cw))
                               _ (render sh2 lay2 ()) _ (refocus sh2) (loop sh2 lay2 hist ()))))
                      (loop sh lay hist ffoc)))
                (loop sh lay hist ffoc)))))

   ; adopt the windows already mapped before we grabbed the redirect (closes the startup race; survives a restart-in-place)
   (furniture? w) (|| (x-has-atom s w "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DESKTOP")   ; desktops and docks are furniture, never clients
                      (x-has-atom s w "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"))
   (ghostly? w) (two? (x-getprop-atoms s w (x-intern s "WM_STATE")))             ; the last generation's stamp: managed then, maybe hidden now
   kids (filter (\ w (! (furniture? w)))
                (filter (\ w (|| (x-manageable? s w) (ghostly? w))) (x-querytree s root)))
   _ (map (\ w (ewmh-track s w)) (x-querytree s root))                           ; a pre-existing dock's strut still shrinks the rect
   (adopt-tag w) (: d (x-getprop-atoms s w (x-intern s "_NET_WM_DESKTOP"))       ; the last generation wrote each window's tag on the window
                    t (? (two? d) (+ 1 (cap d)) 1)                               ; (floats wear their HOME tag too, so they come back to it,
                    (? (&& (< 0 t) (! (< (tally lux-tags) t))) t 1))            ; tiled; out of range -> tag 1)
   sh0 (foldl (\ ss w (wsview (admit (wsview ss (adopt-tag w)) w) (cap lux-tags))) (sheaf lux-tags) kids)
   _ (map (\ w (x-chprop32 s w (x-intern s "WM_STATE") (x-intern s "WM_STATE") (L 1 0))) kids)
   _ (map (\ w (watch w)) kids)                                                 ; survivors get the mouse wires too
   _ (map (\ w (? (x-manageable? s w) (pin mapd w 1) 0)) kids)                   ; seed the ledger: the viewable adoptees are mapped
   _ (? (two? kids) (: _ (render sh0 lux-layout ())
                       _ (map (\ w (? (any (\ x (= x w)) (wsrow sh0)) 0 (hide! w))) (wstiled sh0))   ; adopted onto a HIDDEN tag: hide it
                       _ (x-flush s)                                             ; now, or it lingers painted over workspace 1 (the WM_STATE
                       _ (refocus sh0) (line (+ "lux: adopted " (show kids)))) 0)   ; stamp keeps it adoptable through the NEXT restart)
   _ (twirl (\ u (lux-moor (lux-sock ()))) 0)                                   ; the repl moor: its own task, its own sockets
   _ (loop sh0 lux-layout () ())
   _ (close s)
   (line "lux: done"))
