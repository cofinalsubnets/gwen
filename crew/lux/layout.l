; crew/lux/layout.l -- layouts as DATA records, messages as SYMBOLS. A layout answers
; two questions, both PURE (no X, no sockets -- so this rides the corpus on every
; target, crew/lux/law.l asserts the geometry):
;
;   (lay-tile lay rect z) -> (placements lay')   ; place the zipper's row in the rect
;   (lay-msg  lay msg  z) -> lay' | ()            ; () = "I don't take this message"
;
; rect is (x y w h); a placement is (win x y w h) -- the shape crew/lux/lux.l's render
; already configures. `z` is the focus zipper (crew/lux/core.l), so a layout sees BOTH
; the ordered row (zrow) and the focus (zfoc). Messages ride sendMessage: () back
; means unhandled, so a modifier stack (rung 4) falls through cleanly. This rung
; ships xmonad's ResizableTall (nmaster/ratio/per-slave heights), Full, and the
; ||| choice + NextLayout. A body-LESS `:` -- the vocabulary leaks to the driver.
(:
   ; --- geometry: a rect is (x y w h); splits return rect lists ---
   (rct x y w h) [x y w h]
   (rx r) <r  (ry r) <>r  (rw r) (r 2)  (rh r) (r 3)
   (splitv-eq n r)                                                               ; n equal vertical strips, last takes the remainder
    (? (n < 1) ()
       (: hh (rh r // n)
          (: (go i) (? (i < n) (link (rct (rx r) (ry r + (i * hh)) (rw r)
                                          (i = (n - 1) ? (rh r - ((n - 1) * hh)) hh))
                                     (go (i + 1))) ())
             (go 0))))
   (splitv-w fracs r)                                                            ; weighted vertical split (ResizableTall's slave heights)
    (: n #fracs tot (foldl (+) 0.0 fracs)
       (: (go i yy) (? (i < n)
                       (: fr (fracs i)
                          hh (i = (n - 1) ? ((ry r + rh r) - yy) (int (rh r * (fr / tot))))
                          (rct (rx r) yy (rw r) hh >< go (i + 1) (yy + hh)))
                       ())
          (go 0 (ry r))))
   (splith-by ratio r)                                                          ; split width by ratio -> (left right)
    (: mw (int (ratio * rw r))
       [(rct (rx r) (ry r) mw (rh r)) (rct (rx r + mw) (ry r) (rw r - mw) (rh r))])
   (placeat wins rects) (? (two? wins && two? rects)                            ; zip windows with rects -> (win x y w h)..
                           ((<wins >< <rects) >< placeat >wins >rects) ())
   (posof x l) (: (go i m) (!(two? m) ? -1 (<m = x ? i (go (i + 1) >m))) (go 0 l))
   (mx2 a b) (a < b ? b a)  (mn2 a b) (a < b ? a b)                          ; scalar max/min (max/min are array reducers)
   (padfr fr ns) (: (go i f) (? (i < ns) (link (two? f ? <f 1.0)              ; a length-ns slave-frac list, padded with 1.0
                                              (go (i + 1) (two? f ? >f ()))) ())
                    (go 0 fr))

   ; --- ResizableTall (nmaster delta frac slaves): xmonad's Tall + slave heights ---
   (resizable-tall nm delta frac) ['rtall nm frac delta ()]                    ; slaves start equal (())
   (tall nm) (resizable-tall nm 0.03 0.5)                                       ; plain Tall, 3% steps, half master
   (rt-nm lay n) (: m (lay 1) (m < 1 ? 1 (m > n ? n m)))               ; master count, clamped to [1,n]
   (rt-tile lay rect z)
    (: row (zrow z) n #row ratio (lay 2) fracs (lay 4) nm (rt-nm lay n)
       (? (n < 1) [() lay]
          (n <= nm) [(placeat row (splitv-eq n rect)) lay]                     ; all fit the master column
          (: hs (splith-by ratio rect) rl <hs rr <>hs
             ns (n - nm)
             ms (placeat (take nm row) (splitv-eq nm rl))
             ss (placeat (drop nm row) (splitv-w (padfr fracs ns) rr))
             [(ms + ss) lay])))
   (rt-set lay k v) (: (go i p) (!(two? p) ? () ((i = k ? v <p) >< go (i + 1) >p)) (go 0 lay))
   (rt-msg lay msg z)
    (: ratio (lay 2) delta (lay 3)
       (? (msg = 'shrink)     (rt-set lay 2 (mx2 0.05 (ratio - delta)))
          (msg = 'expand)     (rt-set lay 2 (mn2 0.95 (ratio + delta)))
          (msg = 'inc-master) (rt-set lay 1 (lay 1 + 1))
          (msg = 'dec-master) (rt-set lay 1 (mx2 1 (lay 1 - 1)))
          ((msg = 'mirror-shrink) || (msg = 'mirror-expand))                     ; nudge the FOCUSED slave's height
          (: row (zrow z) n #row nm (rt-nm lay n)
             slaves (drop nm row) idx (posof (zfoc z) slaves)
             (? (idx < 0) lay                                                    ; a master is focused: no slave to resize
                (: sf (padfr (lay 4) #slaves) d (msg = 'mirror-shrink ? (0.0 - delta) delta)
                   (rt-set lay 4 (rt-set sf idx (mx2 0.1 (sf idx + d)))))))
          ()))

   ; --- Full: the focused window owns the whole rect; the rest go unplaced
   ;     (the driver unmaps windows absent from the placements) ---
   full '(full)
   (full-tile lay rect z) (: f (zfoc z) [(f ? [(f >< rect)] ()) lay])

   ; --- Dwindle: the golden spiral. Each window takes a phi-fraction of the
   ;     remaining rect, the split axis alternating -- windows dwindle. Pure. ---
   (dwindle ratio) ['dwindle ratio]
   (dw-go rect wins ratio axis)
    (? !(two? wins) ()
       !(two? >wins) [(<wins >< rect)]                                          ; last window fills the remainder
       (: f (ratio / (1.0 + ratio))                                              ; the focused window's fraction (phi/(1+phi))
          hs (? (axis = 0)                                                        ; axis 0 = split left|right, 1 = top|bottom
                (: fw (int (f * rw rect)) [(rct (rx rect) (ry rect) fw (rh rect)) (rct (rx rect + fw) (ry rect) (rw rect - fw) (rh rect))])
                (: fh (int (f * rh rect)) [(rct (rx rect) (ry rect) (rw rect) fh) (rct (rx rect) (ry rect + fh) (rw rect) (rh rect - fh))]))
          ((<wins >< <hs) >< dw-go <>hs >wins ratio (1 - axis))))
   (dw-tile lay rect z) [(dw-go rect (zrow z) (lay 1) 0) lay]
   (dw-msg lay msg z) (? (msg = 'expand) (rt-set lay 1 (lay 1 * 1.1))       ; expand/shrink nudge the spiral ratio
                          (msg = 'shrink) (rt-set lay 1 (mx2 1.05 (lay 1 / 1.1))) ())

   ; --- modifiers: layout -> layout. A modifier record holds an inner layout;
   ;     it transforms the inner's placements and/or intercepts its own messages,
   ;     delegating the rest (() from lay-msg = unhandled, so the stack composes) ---
   (with-border p b) [(p 0) (p 1) (p 2) (p 3) (p 4) b]    ; a 6th field: the border width hint
   (keep-border src dst) (#src < 6 ? dst (dst + [(src 5)]))            ; carry a border hint through a transform

   ; smartBorders: a lone tiled window (or a fullscreen one) wears no border ---
   (smart-borders inner) ['smart inner]
   (sb-tile lay rect z) (: inner (lay 1) res (lay-tile inner rect z) pls <res
                           pls2 (? (#pls = 1) (map (p \ with-border p 0) pls)
                                   (map (p \ ? ((p 3 = rw rect) && (p 4 = rh rect)) (with-border p 0) p) pls))
                           [pls2 ['smart <>res]])
   (sb-msg lay msg z) (: r (lay-msg (lay 1) msg z) (r ? ['smart r] ()))

   ; MIRROR (mkToggle (single MIRROR)): transpose every output rect. Starts OFF ---
   (mirror inner) ['mirror 0 inner]
   (mir-place p rect) (: base [(p 0) (rx rect + (p 2 - ry rect)) (ry rect + (p 1 - rx rect)) (p 4) (p 3)]
                         (keep-border p base))                                    ; reflect across the rect's diagonal
   (mir-tile lay rect z)
    (: on (lay 1) inner (lay 2)
       (? !on (: res (lay-tile inner rect z) [<res ['mirror on <>res]])
          (: res (lay-tile inner (rct (rx rect) (ry rect) (rh rect) (rw rect)) z)   ; tile in the transposed rect
             [(map (p \ mir-place p rect) <res) ['mirror on <>res]])))
   (mir-msg lay msg z)
    (: on (lay 1) inner (lay 2)
       (? (msg = 'mirror) ['mirror (on ? 0 1) inner]                             ; Toggle MIRROR
          (: r (lay-msg inner msg z) (r ? ['mirror on r] ()))))

   ; toggleLayouts Full: hold the inner layout + a flag; ToggleLayout flips to
   ;     Full (focus owns the whole rect) and back. Starts OFF. ---
   (toggle-full inner) ['toggle-lay 0 inner]
   (tf-tile lay rect z)
    (: on (lay 1) inner (lay 2)
       (? on (: f (zfoc z) [(f ? [(f >< rect)] ()) lay])                        ; Full: only the focus, whole rect
          (: res (lay-tile inner rect z) [<res ['toggle-lay on <>res]])))
   (tf-msg lay msg z)
    (: on (lay 1) inner (lay 2)
       (? (msg = 'toggle-layout) ['toggle-lay (on ? 0 1) inner]
          (: r (lay-msg inner msg z) (r ? ['toggle-lay on r] ()))))

   ; minimize: filter the minimized windows out of the row before tiling (the
   ;     driver then unmaps them, since they fall out of the placements). The
   ;     message carries a window: (minimize-window w) hides, restore un-hides. ---
   (minimize inner) ['minimize () inner]
   (mn-tile lay rect z)
    (: mins (lay 1) inner (lay 2)
       z2 (foldl (\ zz w (zdel w zz)) z mins)
       res (lay-tile inner rect z2)
       [<res ['minimize mins <>res]])
   (mn-msg lay msg z)
    (: mins (lay 1) inner (lay 2)
       (? (two? msg && (<msg = 'minimize-window)) ['minimize (<>msg >< mins) inner]
          (two? msg && (<msg = 'restore-window))                                  ; un-hide a NAMED window (a taskbar activate)
          (? (any (w \ w = <>msg) mins)
             ['minimize (filter (w \ !(w = <>msg)) mins) inner] ())
          (msg = 'restore) ['minimize (two? mins ? >mins ()) inner]            ; un-hide the most recent
          (: r (lay-msg inner msg z) (r ? ['minimize mins r] ()))))

   ; windowNavigation: pure 2D geometry. nav-target names the window nearest the
   ;     focus in a direction ('left/'right/'up/'down); the DRIVER refocuses or
   ;     swaps to it (Go / Swap dir), since focus/swap are StackSet ops, not
   ;     layout state. nav-swap gives the (focus target) pair to exchange. ---
   (pcx p) (p 1 + (p 3 // 2))  (pcy p) (p 2 + (p 4 // 2))
   (in-dir dir fx fy px py) (? (dir = 'left) (px < fx) (dir = 'right) (px > fx) (dir = 'up) (py < fy) (py > fy))
   (dist2 ax ay bx by) (((ax - bx) * (ax - bx)) + ((ay - by) * (ay - by)))
   (nav-target pls focus dir)
    (: fp (afind (p \ p 0 = focus) pls)
       (? !(two? fp) ()
          (: fx (pcx fp) fy (pcy fp)
             cands (filter (p \ !(p 0 = focus) && in-dir dir fx fy (pcx p) (pcy p)) pls)
             (? !(two? cands) ()
                ((foldl (\ b p (!(two? b) ? p (dist2 fx fy (pcx p) (pcy p) < dist2 fx fy (pcx b) (pcy b) ? p b))) () cands) 0)))))

   ; --- ||| : a list of layouts + an index; NextLayout bumps it ---
   (choice subs) ['choice 0 subs]
   (ch-cur lay) ((lay 2) (lay 1))
   (ch-put lay cur2) (: idx (lay 1) subs (lay 2)                       ; replace the active sublayout
                        ['choice idx (map2 (\ i s (i = idx ? cur2 s)) (jot #subs) subs)])
   (ch-tile lay rect z) (: res (lay-tile (ch-cur lay) rect z)
                           [<res (ch-put lay <>res)])
   (ch-msg lay msg z)
    (? (msg = 'next-layout) ['choice ((lay 1 + 1) % tally (lay 2)) (lay 2)]
       (: r (lay-msg (ch-cur lay) msg z) (r ? (ch-put lay r) ())))

   ; map over two lists in lockstep (prel has none) ---
   (map2 f a b) (two? a && two? b ? (f <a <b >< map2 f >a >b) ())

   ; --- the dispatch: case on the record tag. The tag can also be a CLOSURE --
   ;     the OPEN lane: <lay a function answering ('tile lay rect z) ->
   ;     (placements lay') and ('msg lay msg z) -> lay' | (), so a layout
   ;     defined at the lux repl (or anywhere) enters the dispatch as plain
   ;     DATA, no table edit -- the stumpwm move. Modifiers wrap it like any
   ;     builtin (their delegation only calls lay-tile/lay-msg). ---
   (lay-tile lay rect z) (: k <lay (? (k = 'rtall) (rt-tile lay rect z)
                                            (k = 'full)  (full-tile lay rect z)
                                            (k = 'dwindle) (dw-tile lay rect z)
                                            (k = 'smart) (sb-tile lay rect z)
                                            (k = 'mirror) (mir-tile lay rect z)
                                            (k = 'toggle-lay) (tf-tile lay rect z)
                                            (k = 'minimize) (mn-tile lay rect z)
                                            (k = 'choice) (ch-tile lay rect z)
                                            (lit? k) (k 'tile lay rect z) [() lay]))
   (lay-msg lay msg z) (: k <lay (? (k = 'rtall) (rt-msg lay msg z)
                                          (k = 'full)  ()
                                          (k = 'dwindle) (dw-msg lay msg z)
                                          (k = 'smart) (sb-msg lay msg z)
                                          (k = 'mirror) (mir-msg lay msg z)
                                          (k = 'toggle-lay) (tf-msg lay msg z)
                                          (k = 'minimize) (mn-msg lay msg z)
                                          (k = 'choice) (ch-msg lay msg z)
                                          (lit? k) (k 'msg lay msg z) ())))
