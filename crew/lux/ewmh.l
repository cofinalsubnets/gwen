; crew/lux/ewmh.l -- EWMH + docks: the protocol tax that separates "manages my windows"
; from "my taskbar, pager, and fullscreen video work". Three pieces:
;   * a SUPPORT WINDOW advertising _NET_SUPPORTING_WM_CHECK + _NET_WM_NAME
;     ("LG3D (XMonad)" -- the Java hack, setWMName);
;   * root properties a pager reads (_NET_SUPPORTED/_NET_CLIENT_LIST/
;     _NET_ACTIVE_WINDOW/_NET_CURRENT_DESKTOP/_NET_WM_DESKTOP), re-published by
;     ewmh-sync whenever the sheaf changes (a change-guard skips the no-ops);
;   * STRUTS: a dock's _NET_WM_STRUT_PARTIAL shrinks the tiling rect
;     (avoidStruts); ewmh-toggle flips it live (ToggleStruts, sup+b).
; The strut arithmetic and the property encodings are PURE (jug-testable), so
; crew/lux/law.l asserts them; the ewst tablet is the module's one mutable cell (the
; evq pattern). Loads after crew/lux/{layout,wire}.l. A body-LESS `:` -- leaks all.
(:
   ; --- pure: strut arithmetic. A strut is (left right top bottom), root-
   ;     relative reserved widths; shrinking clamps the rect off each edge. ---
   (strut-parse vals) (#vals >= 4 ? (take 4 vals))                      ; _PARTIAL's 12 values: the first 4 are the strut
   (strut-shrink r st swp shp)
    (: x1 (mx2 (rx r) (st 0)) y1 (mx2 (ry r) (st 2))
       x2 (mn2 (rx r + rw r) (swp - st 1))
       y2 (mn2 (ry r + rh r) (shp - st 3))
       (rct x1 y1 (mx2 0 (x2 - x1)) (mx2 0 (y2 - y1))))

   ; --- the EWMH state: one tablet (the evq pattern) ---
   ewst {'avoid 1 'struts () 'last ()}
   (ewmh-atom nm) (peep (peep ewst 'atoms (tablet 1)) nm 0)
   (ewmh-rect scrn swp shp)                                                      ; the tiling rect: screen minus struts (when avoiding)
    (? !(peep ewst 'avoid 0) scrn
       (foldl (\ r p (strut-shrink r <>p swp shp)) scrn (ewst 'struts)))
   (ewmh-toggle u) (pin ewst 'avoid !(ewst 'avoid))
   (ewmh-track s win)                                                            ; read the window's strut; () if it has none
    (: vals (x-getprop-atoms s win (ewmh-atom "_NET_WM_STRUT_PARTIAL"))
       vals (two? vals ? vals (x-getprop-atoms s win (ewmh-atom "_NET_WM_STRUT")))
       st (strut-parse vals)
       (st ? (pin ewst 'struts ([win st] >< filter (p \ <p != win) (ewst 'struts)), st)))
   (ewmh-untrack win) (pin ewst 'struts (filter (p \ <p != win) (peep ewst 'struts ())))

   ; --- CreateWindow (opcode 1): the 1x1 InputOnly support window at (-1,-1) ---
   (x-mkwin s wid parent)
    (x-w8 s 1,       x-w8 s 0,      x-w16 s 8,     x-w32 s wid,
     x-w32 s parent, x-w16 s 65535, x-w16 s 65535, x-w16 s 1,
     x-w16 s 1,      x-w16 s 0,     x-w16 s 2,     x-w32 s 0, x-w32 s 0)                    ; border 0, class InputOnly, visual/values CopyFromParent

   ; --- ChangeProperty (opcode 18, mode Replace), format 32 and 8 ---
   (x-chprop32 s win prop type vals)
    (: n #vals (go (a >< u)) (x-w32 s a, go u)
     (x-w8 s 18, x-w8 s 0, x-w16 s (6 + n), x-w32 s win, x-w32 s prop,
      x-w32 s type, x-w8 s 32, x-w8 s 0, x-w16 s 0, x-w32 s n,
      go vals))           ; an ORDERED loop: map's effects run tail-first
   (x-chprop8 s win prop type txt)
    (: n #txt (go i) (i < x-pad4 n ? (put s 0, go (i + 1)))
     (x-w8 s 18, x-w8 s 0, x-w16 s (6 + (n + x-pad4 n) / 4), x-w32 s win, x-w32 s prop,
      x-w32 s type, x-w8 s 8, x-w8 s 0, x-w16 s 0, x-w32 s n,
      x-wname s txt, go 0))

   ; --- init: intern the vocabulary, hoist the support window, write the root ---
   ewmh-atoms '("_NET_SUPPORTED" "_NET_SUPPORTING_WM_CHECK" "_NET_WM_NAME" "_NET_CLIENT_LIST"
                 "_NET_CLIENT_LIST_STACKING" "_NET_ACTIVE_WINDOW" "_NET_CURRENT_DESKTOP" "_NET_NUMBER_OF_DESKTOPS" "_NET_WM_DESKTOP"
                 "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN" "_NET_WM_STATE_MODAL"
                 "_NET_WM_STATE_MAXIMIZED_VERT" "_NET_WM_STATE_MAXIMIZED_HORZ" "_NET_CLOSE_WINDOW"
                 "_NET_WM_STRUT" "_NET_WM_STRUT_PARTIAL" "_NET_WM_WINDOW_TYPE"
                 "_NET_WM_WINDOW_TYPE_DOCK" "_NET_WM_WINDOW_TYPE_DIALOG" "_NET_WM_WINDOW_TYPE_NOTIFICATION"
                 "WM_CHANGE_STATE")                                              ; ICCCM iconify (minimizeEventHook); interned, NOT advertised as _NET_
   (ewmh-init s root idbase ndesk)
    (: at (tablet 8)
       _ (each ewmh-atoms (nm \ pin at nm (x-intern s nm)))
       utf8 (x-intern s "UTF8_STRING")
       _ (pins ewst 'atoms at 'root root 'sup idbase)
       (a nm) (peep at nm 0)
       _ (x-mkwin s idbase root)
       _ (x-chprop32 s root (a "_NET_SUPPORTED") 4                                           ; type ATOM(4); only the _NET_ vocabulary
                     (map (nm \ peep at nm 0) (filter (nm \ snip nm 0 5 = "_NET_") ewmh-atoms)))
       _ (x-chprop32 s root (a "_NET_SUPPORTING_WM_CHECK") 33 [idbase])                     ; type WINDOW(33)
       _ (x-chprop32 s idbase (a "_NET_SUPPORTING_WM_CHECK") 33 [idbase])
       _ (x-chprop8 s idbase (a "_NET_WM_NAME") utf8 "LG3D (XMonad)")                        ; setWMName: the Java hack
       _ (x-chprop32 s root (a "_NET_NUMBER_OF_DESKTOPS") 6 [ndesk])                        ; type CARDINAL(6)
       (x-flush s))

   ; --- sync: publish the sheaf whenever it changed (pagers track us) ---
   (ewmh-sync s sh)
    (: wins (wswins sh) cur (wscur sh - 1) foc (wsfoc sh ? (wsfoc sh) 0)
       key [cur foc wins]
       (? (key = ewst 'last) 0
          (: _ (pin ewst 'last key) root (peep ewst 'root 0)
             _ (x-chprop32 s root (ewmh-atom "_NET_CLIENT_LIST") 33 wins)
             _ (x-chprop32 s root (ewmh-atom "_NET_CLIENT_LIST_STACKING") 33 wins)   ; libwnck CREATES its windows off THIS list (no stacking =
                                                                                     ; an empty taskbar); wswins is tiles-then-floats = bottom to top
             _ (x-chprop32 s root (ewmh-atom "_NET_CURRENT_DESKTOP") 6 [cur])
             _ (x-chprop32 s root (ewmh-atom "_NET_ACTIVE_WINDOW") 33 [foc])
             _ (map (p \ (: t (<p - 1)                                           ; tiled windows wear their workspace...
                            (map (w \ x-chprop32 s w (ewmh-atom "_NET_WM_DESKTOP") 6 [t]) (zrow <>p)))) (wsboard sh))
             _ (map (p \ x-chprop32 s <p (ewmh-atom "_NET_WM_DESKTOP") 6 [(p 2 - 1)]) (wsfloat sh))           ; ...floats wear their HOME tag (per-workspace now)
             (x-flush s))))

   ; --- ClientMessage (event 33) field reads, off the raw bytes ---
   (ewmh-cm-win pkt) (u32 pkt 4)  (ewmh-cm-type pkt) (u32 pkt 8)
   (ewmh-cm-d0 pkt) (u32 pkt 12)  (ewmh-cm-d1 pkt) (u32 pkt 16)  (ewmh-cm-d2 pkt) (u32 pkt 20))
