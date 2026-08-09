; crew/lux/manage.l -- the manageHook DSL: a new window's properties decide where it
; lands. A HOOK is (ctx sh -> sh): given the manage context and the sheaf (the
; window already admitted TILED by the driver), it may re-place the window --
; float it, center it, ignore it (evict) -- or pass the sheaf through unchanged.
; This mirrors xmonad: the core manages/tiles every window first, then the
; manageHook transforms. The COMBINATORS + float ACTIONS are pure (crew/lux/law.l
; asserts them with mock predicates); the PREDICATES query X (prop?/class-of?/
; transient) so they, do-raise, and the assembled default are host-only -- they
; call crew/lux/wire.l's GetProperty. A body-LESS `:` -- it all leaks to the driver.
;
; ctx = (socket window screen-rect win-geom): the socket for property queries,
; the new window, the screen rect (0 0 W H), and the window's current geometry.

; the two combinators are spelled as operators, so they read as the rules they
; are: `pred -?> act`, `p <||> q`. `fixity` is the one door onto the grammar left
; open after the hatch, and it must stand in its OWN form -- opfix factors a whole
; form at once, so a row pinned inside the `:` below would land too late for it.
; <||> sits in the logical band with ||, -?> just under it, both right-handed.
(fixity '<||> '(2 30 0))
(fixity '-?> '(2 25 0))
(:
   (mg-ctx s win screen geom) `(s win screen geom)
   (mg-sock c) (nthl c 0)  (mg-win c) (nthl c 1)  (mg-screen c) (nthl c 2)  (mg-geom c) (nthl c 3)

   ; --- actions: (ctx sh) -> sh. The float trio move the (already-tiled) window
   ;     into the floating half; ignore evicts it; raise restacks it (X). ---
   (do-full-float c sh) (wsdofloat sh (mg-win c) (mg-screen c))
   (do-float c sh) (wsdofloat sh (mg-win c) (mg-geom c))                         ; float at the window's own geometry
   (do-center-float c sh)                                                        ; center the window's size on the screen
    (: r (mg-screen c) g (mg-geom c) w (rw g) h (rh g)
       (wsdofloat sh (mg-win c) (rct (rx r + ((rw r - w) // 2)) (ry r + ((rh r - h) // 2)) w h)))
   (do-ignore c sh) (evict sh (mg-win c))                                        ; never manage it (docks/notifications): the driver maps it raw
   (do-raise c sh) (: _ (x-raise (mg-sock c) (mg-win c)) sh)                     ; restack above (X); returns the sheaf unchanged

   ; --- predicates: (ctx) -> bool. prop?/class-of? query X properties. ---
   (prop? propnm valnm) (\ c (x-has-atom (mg-sock c) (mg-win c) propnm valnm))   ; isInProperty ATOM VALUE
   (class-of? names) (? !(two? names) (\ c 0)                                    ; an empty roster asks X nothing
                        (\ c (: cl (x-getclass (mg-sock c) (mg-win c)) (any (\ n (n = cl)) names))))
   (<||> p q) (\ c (p c || q c))                                                 ; predicate OR

   ; --- combinators. A Maybe-hook (from -?>) is (ctx sh) -> (sh') | () : matched
   ;     yields a 1-list, else nothing. compose-one takes the FIRST match;
   ;     compose-all threads a list of plain hooks. ---
   (-?> pred act) (\ c sh (? (pred c) `((act c sh)) ()))
   (compose-one mhs) (\ c sh (: (go l) (? !(two? l) sh
                                          (: r (<l c sh) (? (two? r) <r (go >l)))) (go mhs)))
   (compose-all hs) (\ c sh (foldl (\ s h (h c s)) sh hs))

   ; --- xmonad's transience' and manageDocks as plain hooks (VALUES, not nullary
   ;     helpers -- (f) == f would hand back the wrapper unrun) ---
   transience (\ c sh (? (x-transient (mg-sock c) (mg-win c)) (do-center-float c sh) sh))   ; a dialog floats centered on its parent
   manage-docks (\ c sh (? ((prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"          ; checkDock: a dock OR a desktop --
                             <||> prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DESKTOP") c)     ; the file manager's wallpaper window
                           (do-ignore c sh) sh))                                                    ; is a client only by accident

   ; --- run a hook against a freshly-admitted window ---
   (manage-window hook c sh) (hook c sh)

   ; --- the class ROSTERS a config can name (crew/lux/config.l reads them out of
   ;     salt). they come in as symbols or strings; WM_CLASS is text, so spell
   ;     every entry as text once, here. ---
   (mg-classes ns) (map (\ n (? (name? n) (string n) n)) (filter (\ n (name? n || string? n)) ns))
   mg-center-def '("Xmessage" "Xscreensaver-settings" "feh" "vlc")               ; dialogs by nature, whatever they say they are
   mg-float-def  '("steam" "Gimp" "krita")                                       ; toolkits that place their own windows
   mg-panel-def  '("Xfce4-panel")

   ; --- the config's manageHook, assembled (host-only: it CALLS prop? at load,
   ;     which captures crew/lux/wire.l's X ops). composeAll [ def, transience',
   ;     manageDocks, composeOne [ notification/dock, fullscreen, dialog, modal ] ].
   ;     centers float centered, floats float in place, ignores are never managed. ---
   (manage-for centers floats ignores)
    (compose-all `(
       (\ c sh sh)                                                              ; manageHook def: tile (identity -- the driver already admitted it)
       transience
       manage-docks
       (compose-one `(
          (class-of? (mg-classes ignores) -?> (\ c sh (do-ignore c (do-raise c sh))))
          ((prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"
            <||> prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK")
           -?> (\ c sh (do-ignore c (do-raise c sh))))                          ; doRaise >> doIgnore
          (prop? "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN" -?> do-full-float)
          ((prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
            <||> class-of? (mg-classes centers))
           -?> do-center-float)
          ((prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY"            ; a panel's calendar popup, toolboxes,
            <||> prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_TOOLBAR"       ; splashes -- self-placed popups float
            <||> prop? "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
            <||> class-of? (mg-classes mg-panel-def))                            ; (the panel itself is a DOCK, ignored above)
           -?> do-float)
          ((prop? "_NET_WM_STATE" "_NET_WM_STATE_MODAL"
            <||> class-of? (mg-classes floats))
           -?> do-float)))))
   manage-default (manage-for mg-center-def mg-float-def ()))
