; crew/lux/config.l -- the CONFIG: ~/.xmonad/xmonad.hs transliterated, now with a
; door. Everything the Haskell record pinned, as plain values the driver
; (crew/lux/lux.l) reads -- the DSL underneath (core/layout/manage/keys/ewmh)
; already IS the xmonad vocabulary, so this file is short the way a config should
; be. A body-LESS `:`.
;
; THE DOOR is salt (lib/salt.l): ~/.love/etc/lux.l for your taste, then ./.lux.l
; for a tree's own, the second winning. Every setting falls back to the value that
; was hardcoded here, so no config file anywhere is today's lux to the byte.
; ⚠ salt is DATA, never code: nothing in a config file is evaluated, so nothing in
; it needs quoting.
;
; ⚠ a repeated setting REPLACES rather than appends -- so each roster takes ALL
; its entries in ONE form, and `()` as the lone operand spells the EMPTY roster
; (an operand-less `(startup)` is indistinguishable from no line at all).
;
;   (border 3)  (normal-color "Dark Slate Blue")  (focused-color "Light Cyan")
;   (tags 9)  (mod sup)  (nudge 20)
;   (startup ("setxkbmap" "-option" "ctrl:nocaps"))
;   (layout (rtall 1 0.02 0.5) (dwindle phi))
;   (layout-wrap minimize smart toggle-full mirror)
;   (center-class Xmessage feh vlc)  (float-class steam Gimp krita)  (ignore-class conky)
;   (bind ((sup Return) (spawn "alacritty")) ((sup shift F1) (view 1))
;         ((sup ",") (msg shrink))           ((sup 65027) (spawn "xdotool" "key" "x")))
;   (unbind (sup p) (sup z))
;   (display ":7")  (xauthority "/home/me/.Xauthority")
;
; a CHORD is a list, the last element the key and the rest modifiers: a symbol
; names it (`e`, `Return`), a one-char string IS that character (`","`, `"1"`),
; and a bare number is the keysym itself -- so the name table is a convenience,
; never a ceiling.
;
; the conf-* readers take the tablet, so crew/lux/law.l proves the defaults against
; an EMPTY one -- the gate must not change meaning the day you write a config file.
(use 'salt)
(:
   ; --- the readers. a roster entry is a name or a form; anything else (the lone
   ;     `()` that spells "empty") drops out, and an ABSENT roster takes d. ---
   (conf-live? e) (two? e || name? e || string? e)
   (conf-roster c k d) (: r (salt-all c k) (two? r ? (filter conf-live? r) d))
   (conf-mod c) (: m (kmod-of (salt-one c 'mod 'sup) supm) (m ? m supm))
   (conf-tags c) (: n (salt-one c 'tags 9) (charm? n && (0 < n) ? n 9))

   lux-conf (salt 'lux)

   ; borderWidth = 3, normalBorderColor / focusedBorderColor
   lux-border (salt-one lux-conf 'border 3)
   lux-normal-color (salt-one lux-conf 'normal-color "Dark Slate Blue")
   lux-focused-color (salt-one lux-conf 'focused-color "Light Cyan")

   ; workspaces = map show [1 .. 9]
   lux-tags (map (+ 1) (jot (conf-tags lux-conf)))

   ; layoutHook = minimize . smartBorders . avoidStruts . windowNavigation
   ;            . toggleLayouts Full . mkToggle (single MIRROR) $ l1 ||| l2
   ;   (avoidStruts lives in the driver as ewmh-rect; windowNavigation is
   ;    nav-target under the directional keys -- both always-on, not records)
   ;   l2 = Dwindle R CW phi 1.1 where phi = fib 32 / fib 31 -- the fib, verbatim
   (lux-fib m n i) (i < 1 ? m (lux-fib n (m + n) (i - 1)))
   lux-phi (lux-fib 0 1 32 / lux-fib 0 1 31)
   ; one ||| member as data: the head names the layout, the tail its operands, and
   ; `phi` stands for the golden ratio wherever a number would go.
   (lux-arg a i d) (: (go l k) (? !(two? l) d
                                  (0 < k) (go >l (k - 1))
                                  (: v <l (id? v 'phi ? lux-phi (constellation? v ? v d))))
                      (go a i))
   (lux-lay-of sp)
    (: h (two? sp ? <sp sp) a (two? sp ? >sp ())
       (? (id? h 'rtall)   (resizable-tall (lux-arg a 0 1) (lux-arg a 1 0.02) (lux-arg a 2 0.5))
          (id? h 'tall)    (tall (lux-arg a 0 1))
          (id? h 'dwindle) (dwindle (lux-arg a 0 lux-phi))
          (id? h 'full)    full
          ()))
   (lux-wrap-of w inner) (? (id? w 'minimize)    (minimize inner)
                            (id? w 'smart)       (smart-borders inner)
                            (id? w 'toggle-full) (toggle-full inner)
                            (id? w 'mirror)      (mirror inner)
                            inner)
   lux-lays (filter two? (map lux-lay-of
                              (conf-roster lux-conf 'layout '((rtall 1 0.02 0.5) (dwindle phi)))))
   lux-wraps (conf-roster lux-conf 'layout-wrap '(minimize smart toggle-full mirror))
   lux-layout (foldr lux-wrap-of (choice lux-lays) lux-wraps)

   ; manageHook / keys: assembled in manage.l / keys.l (composeAll and
   ;   `keys def UNION custom` are the DSL's own assembly points)
   lux-manage (manage-for (conf-roster lux-conf 'center-class mg-center-def)
                          (conf-roster lux-conf 'float-class mg-float-def)
                          (conf-roster lux-conf 'ignore-class ()))
   lux-mod (conf-mod lux-conf)
   lux-keymap (keys-build lux-mod (salt-one lux-conf 'nudge 20) (conf-tags lux-conf)
                          (conf-roster lux-conf 'bind ()) (conf-roster lux-conf 'unbind ()))

   ; startupHook = spawn "setxkbmap -option ctrl:nocaps -option compose:rctrl"
   ;   (setWMName "LG3D (XMonad)" rides ewmh-init; the cursor is x-cursor).
   ;   LUX_NOSTARTUP skips the spawns -- a headless harness wants no keyboard
   ;   churn on its bare Xvfb
   lux-startup (? (getenv "LUX_NOSTARTUP") ()
                  (conf-roster lux-conf 'startup
                               '(("setxkbmap" "-option" "ctrl:nocaps" "-option" "compose:rctrl"))))

   ; the display: DISPLAY (":0" / ":7.0" -> the unix socket) unless a config names
   ;   one, the cookie from the xauth file when the server wants one (an -ac Xephyr
   ;   has no entry -> ()). ⚠ XAUTHORITY names that file and WINS over ~/.Xauthority
   ;   -- every other X client reads it that way, and a display manager that parks
   ;   the live cookie elsewhere (ly's /run/user/<uid>/lyxauth) leaves a stale
   ;   ~/.Xauthority behind, whose cookie the server refuses
   (lux-dispof d) (? (#d < 2) "7"                                           ; unset: the Xephyr harness habit
                     (: b (snip d 1 #d)
                        (: (go i) ((i = tally b) || (b i = 46) ? (snip b 0 i) (go (i + 1)))
                           (go 0))))
   (lux-dispnum u) (lux-dispof (: d (salt-one lux-conf 'display ()) (string? d ? d (getenv "DISPLAY"))))
   (lux-xauth u) (: v (getenv "XAUTHORITY") (0 < tally v ? v (getenv "HOME" + "/.Xauthority")))
   (lux-auth-for disp) (xauth-cookie (salt-one lux-conf 'xauthority (lux-xauth ())) disp)
   lux-display ("/tmp/.X11-unix/X" + lux-dispnum ())
   lux-auth (lux-auth-for (lux-dispnum ())))
