; crew/lux/keys.l -- the keymap DSL + xmonad's DEFAULT keymap + the config's custom
; table. PURE DATA: a binding is (mods keysym action), an action a tagged list
; the driver (crew/lux/lux.l) interprets -- so the whole table rides the corpus and
; crew/lux/law.l asserts its shape, the union law, and the directional generator.
; The config is xmonad's `(. keys def) . M.union . M.fromList $ [custom]`:
; custom bindings UNION over the stock table, custom winning on (mods keysym).
; keys-build is the same union with a user's rows on top -- it takes plain
; operands, never a settings tablet, so this file compiles before crew/lux/config.l
; reads salt (a bare `salt-one` here would fold against nothing).
;
; The action vocabulary (what the driver's do-act cases on):
;   (msg M)            sendMessage M to the layout stack
;   (go DIR (dx dy))   f_t: float -> keysMoveWindow by (dx dy); tiled -> Go DIR
;   (swapd DIR (dx dy))     float -> keysResizeWindow;          tiled -> Swap DIR
;   (resize DIR (dx dy))    float -> keysResizeWindow;          tiled -> Shrink/Expand by DIR
;   (view T) (shift T) (spawn ARGV) (exec ARGV) (kill) (quit) (restart)
;   (refresh) (reset-layout) (sink) (float-toggle) (next-match FWD)
;   (focus-dn) (focus-up) (focus-master) (swap-dn) (swap-up) (swap-master)
; A body-LESS `:` -- the vocabulary leaks to the driver.
(:
   ; --- modifier masks (the X state bits) ---
   shiftm 1  ctrlm 4  altm 8  supm 64                                            ; sup = mod4, alt = mod1
   kmodmask 77                                                                   ; shift|ctrl|alt|sup: what a lookup masks state by

   ; --- keysyms (X11 core; letters/digits are ASCII) ---
   xk-ret 65293  xk-tab 65289  xk-left 65361  xk-up 65362  xk-right 65363  xk-down 65364

   ; --- a binding row ---
   (kb mods sym act) [mods sym act]
   (kb-mods r) <r  (kb-sym r) <>r  (kb-act r) (r 2)
   (keys-has t mods sym) (any (r \ (kb-mods r = mods) && (kb-sym r = sym)) t)
   (keys-find t mods sym) (: hit (afind (r \ (kb-mods r = mods) && (kb-sym r = sym)) t)
                             (two? hit ? (kb-act hit) ()))
   (keys-union custom def) (custom + (filter (r \ !(keys-has custom (kb-mods r) (kb-sym r))) def))
   (keys-syms t) (foldl (\ a r (any (x \ x = kb-sym r) a ? a (kb-sym r >< a))) () t)

   ; --- KEY SPECS: a LIST, `(sup shift e)` -- the LAST element is the key, every
   ;     one before it a modifier. a config writes these (crew/lux/config.l reads
   ;     them out of salt as plain data), so the reader hands over the structure
   ;     and nothing here parses a spelling.
   ;     the key field, four ways, each unambiguous:
   ;       e / Return    a symbol -- one char is its ascii keysym, longer names one below
   ;       "," / "1"     a one-char STRING is that character, punctuation and digits included
   ;       65027         a bare charm IS the keysym, so the table is a convenience,
   ;                     never a ceiling (⚠ 32 and up: a digit key is "1" or 49, not 1) ---
   xk-fkeys (map (i \ [("F" + show (i + 1)) (65470 + i)]) (jot 12))             ; F1..F12
   xk-names (xk-fkeys + [
     ["Return" xk-ret]   ["Tab" xk-tab]     '("space" 32)       '("Escape" 65307)
     ["Left" xk-left]    ["Up" xk-up]       ["Right" xk-right] ["Down" xk-down]
     '("BackSpace" 65288) '("Delete" 65535)   '("Insert" 65379)   '("Menu" 65383)
     '("Home" 65360)      '("End" 65367)      '("Page_Up" 65365)  '("Page_Down" 65366)
     '("Print" 65377)     '("Pause" 65299)
     '("grave" 96)  '("comma" 44)  '("period" 46) '("slash" 47)   '("backslash" 92)
     '("minus" 45)  '("equal" 61)  '("plus" 43)   '("asterisk" 42)
     '("semicolon" 59) '("apostrophe" 39) '("bracketleft" 91) '("bracketright" 93)
     '("XF86AudioMute" 269025042)        '("XF86AudioLowerVolume" 269025041)
     '("XF86AudioRaiseVolume" 269025043) '("XF86AudioPlay" 269025044)
     '("XF86AudioStop" 269025045)        '("XF86AudioPrev" 269025046)
     '("XF86AudioNext" 269025047)
     '("XF86MonBrightnessUp" 269025026)  '("XF86MonBrightnessDown" 269025027)])
   (xk-of k) (? (charm? k) (31 < k ? k ())                                     ; a keysym, verbatim -- the escape hatch past the table
                (: nm (name? k ? (string k) k)
                   (? !(string? nm) ()
                      (#nm = 1) (nm 0)                                      ; one char IS its ascii keysym
                      (: hit (afind (r \ <r = nm) xk-names)
                         (two? hit ? <>hit ())))))                             ; unknown name -> () (no keysym is 0, so nothing is lost)
   (kmod-of m modm) (? (id? m 'sup || id? m 'super || id? m 'mod4) supm
                       (id? m 'alt || id? m 'mod1) altm
                       (id? m 'ctrl || id? m 'control) ctrlm
                       (id? m 'shift) shiftm
                       (id? m 'mod) modm                                         ; `mod` follows whatever the config chose
                       ())
   ; a spec -> (mods keysym), or () if any field is unknown. ⚠ the mask ACCUMULATES
   ; through a `(1 mask)` wrapper: a bare mask of 0 (no modifiers) is a legitimate
   ; answer and would read as failure on its own.
   (kspec modm spec)
    (? !(two? spec) ()
       (: n #spec
          ks (xk-of (spec (n - 1)))
          ms (foldl (\ a m (: b (kmod-of m modm) (two? a && b ? [(<a | b)] ())))
                    '(0) (take (n - 1) spec))
          (ks && two? ms ? [<ms ks] ())))

   ; --- a config's own rows. `spawn`/`exec` carry their argv FLAT in a config
   ;     ((spawn "rofi" "-show" "drun")) and nested in a binding row, so they are
   ;     the two the reader re-wraps; every other action is already its own shape. ---
   (kb-act-of a) (? !(two? a) a
                    (id? <a 'spawn || id? <a 'exec) [<a >a]
                    a)
   (keys-rows modm rows)                                                         ; ((spec action) ..) -> binding rows; an unreadable spec is dropped
    (foldr (\ r acc (: k (two? r ? (kspec modm <r) ())
                       (two? k && two? >r ? (kb <k <>k (kb-act-of <>r) >< acc) acc)))
           () rows)
   (keys-drop modm t specs)                                                      ; remove every chord named in specs
    (: ks (filter two? (map (s \ kspec modm s) specs))
       (? !(two? ks) t
          (filter (r \ !(any (k \ (<k = kb-mods r) && (<>k = kb-sym r)) ks)) t)))

   ; --- the directional block: the .hs list comprehension as a map --
   ;     (dir keysyms delta) x {hjkl, arrows} x {mod, mod+shift, mod+ctrl} ---
   (kdirs-for nud) [['left  [104 xk-left]  [(0 - nud) 0]]
                     ['down  [106 xk-down]  [0 nud]]
                     ['up    [107 xk-up]    [0 (0 - nud)]]
                     ['right [108 xk-right] [nud 0]]]
   (kdir-rows modm d) (: dir <d delta (d 2)
                         (foldl cat () (map (sym \ [(kb modm sym ['go dir delta])
                                                     (kb (modm | shiftm) sym ['swapd dir delta])
                                                     (kb (modm | ctrlm) sym ['resize dir delta])])
                                            <>d)))
   (keys-dir-for modm nud) (foldl cat () (map (d \ kdir-rows modm d) (kdirs-for nud)))

   ; --- the custom command block, exactly the config's table ---
   (keys-cmd-for modm) [
     (kb modm 120 '(kill))                                                       ; mod+x
     (kb (modm | shiftm) 113 '(quit))                                            ; mod+shift+q
     (kb modm xk-ret '(spawn ("xfce4-terminal")))                                ; mod+return  term
     (kb modm 96 '(spawn ("rofi" "-show" "drun")))                               ; mod+`       menu
     (kb modm 32 '(msg next-layout))                                             ; mod+space
     (kb modm 98 '(toggle-struts))                                               ; mod+b       ToggleStruts (ewmh.l's avoid flag)
     (kb modm 109 '(msg mirror))                                                 ; mod+m       Toggle MIRROR
     (kb modm 102 '(msg toggle-layout))                                          ; mod+f       toggleLayouts Full
     (kb modm 112 '(spawn ("xfce4-screenshooter")))                              ; mod+p
     (kb modm 119 '(float-toggle))                                               ; mod+w       f_t sink float
     (kb (modm | shiftm) 101 '(spawn ("gvim" "/home/gwen/g/lux")))               ; mod+shift+e edit config
     (kb modm 101 '(spawn ("rofimoji")))                                         ; mod+e
     (kb modm 122 '(spawn ("xscreensaver-command" "-activate")))                 ; mod+z
     (kb (modm | shiftm) 111 '(exec ("openbox" "--replace")))                    ; mod+shift+o abdicate
     (kb (modm | shiftm) 122 '(spawn ("xscreensaver-command" "-lock")))          ; mod+shift+z
     (kb altm xk-tab '(next-match 1))                                            ; alt+tab     nextMatch Forward
     (kb modm xk-tab '(next-match 0))                                            ; mod+tab     nextMatch Backward
     (kb modm 44 '(msg inc-master))                                              ; mod+,       IncMasterN 1
     (kb modm 46 '(msg dec-master))                                              ; mod+.       IncMasterN -1
     (kb (modm | shiftm) 44 '(msg shrink))                                       ; mod+shift+,
     (kb (modm | shiftm) 46 '(msg expand))]                                      ; mod+shift+.

   ; --- xmonad's `keys def`, single-monitor subset (mod+{w,e,r} screens and
   ;     mod+shift+/ help are not ported; some rows are overridden by custom).
   ;     the digit rows follow the TAG COUNT, and stop at 9 -- there is no tenth
   ;     digit key to hang a tenth workspace on. ---
   (keys-def-for modm ntags) (: ds (map (+ 1) (jot (9 < ntags ? 9 ntags)))
    (cat [
     (kb (modm | shiftm) xk-ret '(spawn ("xterm")))                              ; mod+shift+return  terminal
     (kb modm 112 '(spawn ("dmenu_run")))                                        ; mod+p             (custom overrides)
     (kb (modm | shiftm) 99 '(kill))                                             ; mod+shift+c
     (kb modm 32 '(msg next-layout))                                             ; mod+space
     (kb (modm | shiftm) 32 '(reset-layout))                                     ; mod+shift+space
     (kb modm 110 '(refresh))                                                    ; mod+n
     (kb modm xk-tab '(focus-dn))                                                ; mod+tab           (custom overrides)
     (kb (modm | shiftm) xk-tab '(focus-up))
     (kb modm 106 '(focus-dn))                                                   ; mod+j
     (kb modm 107 '(focus-up))                                                   ; mod+k             (custom overrides j/k with Go)
     (kb modm 109 '(focus-master))                                               ; mod+m             (custom overrides)
     (kb modm xk-ret '(swap-master))                                             ; mod+return        (custom overrides)
     (kb (modm | shiftm) 106 '(swap-dn))                                         ; mod+shift+j
     (kb (modm | shiftm) 107 '(swap-up))                                         ; mod+shift+k
     (kb modm 104 '(msg shrink))                                                 ; mod+h             (custom overrides)
     (kb modm 108 '(msg expand))                                                 ; mod+l             (custom overrides)
     (kb modm 116 '(sink))                                                       ; mod+t             withFocused sink
     (kb modm 44 '(msg inc-master))
     (kb modm 46 '(msg dec-master))
     (kb (modm | shiftm) 113 '(quit))                                            ; mod+shift+q
     (kb modm 113 '(restart))]                                                   ; mod+q
     (cat (map (d \ kb modm (48 + d) ['view d]) ds)                           ; mod+1..9
          (map (d \ kb (modm | shiftm) (48 + d) ['shift d]) ds))))            ; mod+shift+1..9

   ; --- the STOCK table: what lux drives with no config file anywhere ---
   keys-dir (keys-dir-for supm 20)
   keys-cmd (keys-cmd-for supm)
   keys-custom (keys-dir + keys-cmd)
   keys-def (keys-def-for supm 9)
   the-keys (keys-union keys-custom keys-def)

   ; the same union with a user's rows on top: theirs beat the custom block, which
   ; beats the stock one, and `unbind` cuts last so a chord can be taken away.
   (keys-build modm nud ntags binds unbinds)
    (keys-drop modm
               (keys-union (keys-rows modm binds)
                           (keys-union ((keys-dir-for modm nud) + (keys-cmd-for modm))
                                       (keys-def-for modm ntags)))
               unbinds)

   ; --- nextMatch's MRU pick, pure: row = the workspace's windows, f = the
   ;     focus, hist = every focus ever, most recent first. Forward walks to
   ;     the most recently used OTHER window (the alt-tab flip); Backward to
   ;     the least. Windows never yet focused count as oldest. ---
   (nm-pick row f hist fwd)
    (: cands (filter (w \ !(w = f) && any (x \ x = w) row) hist)
       fresh (filter (w \ !(w = f) && !(any (x \ x = w) cands)) row)
       order (cands + fresh)
       (? !(two? order) () fwd <order <(rev order))))
