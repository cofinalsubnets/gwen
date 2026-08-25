; crew/lux/law.l -- xmonad's QuickCheck properties for crew/lux/core.l, in exact arithmetic,
; plus a seeded fuzz. Reads the LEAKED globals from crew/lux/core.l (concatenate them:
; `cat test/00-init.l crew/lux/core.l crew/lux/law.l | love`, the `make test_lux` gate). Split
; into small independent top-level `:` forms -- each self-contained, nothing new
; leaks, and no single form grows big enough to stress the whole-form compiler.

; --- the zipper laws ---
(: z3 (zmk 2 '(1) '(3))                                                          ; row (1 2 3), 2 focused
   (assert
      (zrow (zfrom '(1 2 3)) = '(1 2 3)) !(zfrom ()) !(zrow ())
      (zrow z3 = '(1 2 3)) (zfoc z3 = 2)
      (zfocdn (zfocup z3) = z3) (zfocup (zfocdn z3) = z3)                        ; motions are inverses
      (zfoc (zfocup z3) = 1) (zfoc (zfocdn z3) = 3)
      (zfoc (zfocup (zfocup z3)) = 3)                                            ; wrap past the top -> the bottom
      (((3 zfocup) z3) = z3) (((3 zfocdn) z3) = z3)                              ; the church numeral IS the cycle law
      (zfocup (zmk 7 () ()) = zmk 7 () ())                                       ; a singleton is a fixed point
      (zswapdn (zswapup z3) = z3) (zswapup (zswapdn z3) = z3)                    ; swaps are inverses
      (zfoc (zswapup z3) = 2) (zrow (zswapup z3) = '(2 1 3))
      (zrow (zswapup (zfocup z3)) = '(2 3 1))                                    ; swapping the top rotates it down
      (zrow (zmaster z3) = '(2 1 3)) (zmaster (zmaster z3) = zmaster z3)         ; master fronts the focus, idempotent
      (zrow (zins 9 z3) = '(1 9 2 3)) (zfoc (zins 9 z3) = 9)
      (zdel 2 z3 = zmk 3 '(1) ()) (zrow (zdel 1 z3) = '(2 3))
      (zdel 9 z3 = z3) !(zdel 1 (zmk 1 () ()))))

; --- the sheaf laws ---
(: (sameset? a b) ((#a = tally b) && all (x \ any (y \ x = y) b) a)         ; equal as sets
   s0 (sheaf '(1 2 3)) s1 (admit s0 10) s2 (admit (admit s1 20) 30)             ; ws1 row (30 20 10)
   s4 (shiftto s2 2)
   (assert
      (wscur s0 = 1) !(wsrow s0) !(wsfoc s0) (wssane? s0)
      (wsfoc s1 = 10) (wsrow s1 = '(10)) (wsknownw? s1 10) (wssane? s1)
      (admit s1 10 = s1)                                                         ; managing a managed window is a no-op
      (wsrow s2 = '(30 20 10)) (wsfoc s2 = 30) (wssane? s2)
      (wscur (wsview s2 2) = 2) !(wsfoc (wsview s2 2))                           ; view: only the tag turns
      (wsview s2 9 = s2) (wsview (wsview s2 2) 1 = s2)
      (sameset? (wswins (wsview s2 2)) (wswins s2))                             ; view moves no windows
      (wscur s4 = 1) (wstagof s4 30 = 2) (wsfoc s4 = 20) (wsrow s4 = '(20 10)) (wssane? s4)
      (shiftto s2 1 = s2) (shiftto s2 9 = s2)                                   ; to self / to nowhere: no-ops
      (: s5 (shiftwin s2 10 3)                                                  ; shiftwin names its window: 10 leaves, focus stays 30
         ((wstagof s5 10 = 3) && (wsrow s5 = '(30 20)) && (wsfoc s5 = 30) && wssane? s5))
      (shiftwin s2 10 1 = s2) (shiftwin s2 99 2 = s2)                           ; to its own tag / an unknown window: no-ops
      !(wsknownw? (evict s2 20) 20) (wsrow (evict s2 20) = '(30 10))
      (wsfoc (evict s2 30) = 20) (wssane? (evict s2 30))
      (wstagof s2 10 = 1) !(wstagof s2 99)
      ; --- the focus ring (rnext walks it cyclically, either direction) ---
      (rnext '(1 2 3) 2 1 = 3) (rnext '(1 2 3) 2 -1 = 1)
      (rnext '(1 2 3) 3 1 = 1) (rnext '(1 2 3) 1 -1 = 3)                        ; wraps at either end
      (rnext '(1 2 3) 9 1 = 1) !(rnext () 9 1)                                  ; absent -> the head; an empty ring -> ()
      (wsring s2 = '(30 20 10))))                                               ; no floats: the ring IS the tiled row

; --- the floating half (W.float / W.sink) ---
(: s2 (admit (admit (admit (sheaf '(1 2 3)) 10) 20) 30) r99 '(20 20 200 100)
   (sameset? a b) ((#a = tally b) && all (x \ any (y \ x = y) b) a)
   f2 (wsdofloat s2 20 r99)
   (assert
      !(wsfloating? s2 20) (wsfloating? f2 20) (wsrect f2 20 = r99)              ; float lifts a window
      (wsknownw? f2 20) (wsrect f2 30 = ())                                      ; still managed; a tiled window has no rect
      !(any (x \ x = 20) (wstiled f2))                                         ; float removed 20 from the tiled stack
      (sameset? (wswins f2) (wswins s2))                                         ; float moves it between sets, not out
      !(wsfloating? (wssink f2 20) 20) (sameset? (wswins (wssink f2 20)) (wswins s2))        ; sink is float's inverse
      ; --- the HOME TAG: a float lives on ONE workspace (xmonad's model) ---
      (1 = wsftag f2 20)                                                         ; floated while viewing tag 1: home is 1
      (1 = wsftag (wsview f2 2) 20)                                              ; viewing elsewhere doesn't move it
      (two? (wsvisfl f2)) !(two? (wsvisfl (wsview f2 2)))                        ; visible at home, hidden abroad
      (1 = wstagof f2 20)                                                        ; wstagof answers a float's home
      (1 = wsftag (wsdofloat (wsview f2 2) 20 '(0 0 9 9)) 20)                    ; re-floating abroad (a drag) keeps the home
      (2 = wsftag (shiftwin f2 20 2) 20)                                         ; shiftwin retags a float's home...
      (r99 = wsrect (shiftwin f2 20 2) 20)                                       ; ...still floating, rect kept
      (wssane? (shiftwin f2 20 2)) (shiftwin f2 20 1 = f2)                       ; to its own home: a no-op
      (wsring f2 = '(30 10 20))                                                  ; the ring reaches the float, after the tiles
      !(any (x \ x = 20) (wsring (wsview f2 2)))))                             ; abroad, the float is off the ring

; --- the seeded fuzz: the whole op vocabulary, 200 steps, invariant per step ---
(: r99 '(20 20 200 100)
   (fzop k w s)
    (? (k = 0) (admit s w)     (k = 1) (evict s w)
       (k = 2) (wsmap s zfocup) (k = 3) (wsmap s zfocdn)
       (k = 4) (wsmap s zswapup) (k = 5) (wsmap s zswapdn)
       (k = 6) (wsmap s zmaster) (k = 7) (wsview s (1 + (w % 3)))
       (k = 8) (shiftto s (1 + (w % 3)))
       (: fw (wsfoc s) (fw ? (wsfloating? s fw ? (wssink s fw) (wsdofloat s fw r99)) s)))   ; toggle-float the focus
   (fz i st s)
    (? (i < 1) s
       (: r1 (random st 10) r2 (random >r1 40)
          s2 (fzop <r1 (1 + <r2) s)
          (wssane? s2 ? (fz (i - 1) >r2 s2) ['broke <r1 (1 + <r2)])))
   sF (fz 200 (wheel 7) (sheaf '(1 2 3)))
   (assert (wssane? sF) (all (w \ wsfloating? sF w || wsknown? sF (wstagof sF w)) (wswins sF))))

; --- the layout engine (crew/lux/layout.l): geometry is a partition ---
(: (area p) (p 3 * p 4)                                                ; a placement's w*h
   (sumarea ps) (foldl (\ a p (a + area p)) 0 ps)
   r (rct 0 0 800 600) full-area 480000
   z1 (zfrom '(10)) z3 (zfrom '(10 20 30))                                       ; z3 focuses the master (10)
   L1 (resizable-tall 1 0.02 0.5)
   t1 <(lay-tile L1 r z1) t3 <(lay-tile L1 r z3)
   (assert
      ; ResizableTall partitions the rect exactly, master-left / slaves-right
      (#t3 = 3) (sumarea t3 = full-area)
      (<t3 = '(10 0 0 400 600))                                                  ; master owns the left half
      (<>t3 = '(20 400 0 400 300)) (t3 2 = '(30 400 300 400 300))           ; two slaves stack in the right half
      (#t1 = 1) (<t1 = '(10 0 0 800 600))                                   ; a lone window fills the screen
      ; Shrink / Expand nudge the master ratio by delta (master width, was 400)
      (: mw- (<<(lay-tile (lay-msg L1 'shrink z3) r z3) 3)
         mw+ (<<(lay-tile (lay-msg L1 'expand z3) r z3) 3)
         ((mw- < 400) && mw- > 380 && mw+ > 400 && (mw+ < 420)))
      ((lay-msg (lay-msg L1 'expand z3) 'shrink z3) 2 = 0.5)               ; expand then shrink is a round trip
      ; IncMasterN grows the master column (2 masters left, 1 slave right)
      (: L2 (lay-msg L1 'inc-master z3) p <(lay-tile L2 r z3)
         ((<p 1 = 0) && (<>p 1 = 0) && ((p 2) 1 = 400)))                   ; two masters at x=0, slave at x=400
      ((lay-msg (lay-msg L1 'inc-master z3) 'dec-master z3) 1 = 1)          ; dec undoes inc; floored at 1
      ((lay-msg L1 'dec-master z3) 1 = 1)
      ; Full places only the focus, whole rect
      (: f <(lay-tile full r z3) ((#f = 1) && (<f = '(10 0 0 800 600))))
      ; MirrorShrink shrinks the FOCUSED slave (focus 20, the first slave)
      (: zs (zfocdn z3) M (lay-msg L1 'mirror-shrink zs) h20 (<><(lay-tile M r zs) 4)
         ((h20 < 300) && h20 > 250))
      ; ||| holds the layouts; NextLayout cycles the index (tall -> full -> tall)
      (: C (choice [L1 full])
         ((#<(ch-tile C r z3) = 3)                                         ; index 0 = ResizableTall
          && (#<(ch-tile (lay-msg C 'next-layout z3) r z3) = 1)            ; index 1 = Full
          && ((lay-msg (lay-msg C 'next-layout z3) 'next-layout z3) 1 = 0)))     ; wraps back to 0
      ; a message ||| doesn't know (shrink) delegates to the active sublayout
      ((ch-cur (lay-msg (choice [L1 full]) 'shrink z3)) 2 = 0.48)))

; --- the tiling LEDGER (rtwinsrow, test/uulay.l: proved of the model in uu,
; --- certified by Rocq + Lean; here the same law watched on the metal):
; --- rt-tile's placements name the row EXACTLY -- every window placed once,
; --- none invented, row order kept -- whatever the master count ---
(: (wincol ps) (two? ps ? (<<ps >< wincol >ps) ())
   r (rct 0 0 800 600) ws '(10 20 30 40 50) z (zfrom ws)
   (assert
      (wincol <(lay-tile (resizable-tall 1 0.02 0.5) r z) = ws)
      (wincol <(lay-tile (resizable-tall 3 0.02 0.5) r z) = ws)
      (wincol <(lay-tile (tall 9) r z) = ws)
      !(wincol <(lay-tile (tall 2) r (zfrom ())))))

; --- the layout modifiers (crew/lux/layout.l rung 4): each wraps a layout ---
(: (area p) (p 3 * p 4)
   (sumarea ps) (foldl (\ a p (a + area p)) 0 ps)
   r (rct 0 0 800 600) z3 (zfrom '(10 20 30)) z1 (zfrom '(10))
   L1 (resizable-tall 1 0.02 0.5)
   base (choice [L1 (dwindle 1.618)])
   stk (minimize (smart-borders (toggle-full (mirror base))))                    ; the config's t (l1 ||| l2) shape
   (assert
      ; MIRROR transposes: master moves to the TOP, the partition is preserved
      (: m (lay-msg (mirror L1) 'mirror z3) p <(lay-tile m r z3)                 ; toggle MIRROR on
         ((<p = '(10 0 0 800 300)) && (sumarea p = 480000)))                      ; master owns the top strip
      (lay-tile (mirror L1) r z3 = [<(lay-tile L1 r z3) (mirror L1)])            ; MIRROR off is a pass-through
      ; Dwindle is a golden spiral: the focus takes phi/(1+phi) of the width, still a partition
      (: p <(lay-tile (dwindle 1.618) r z3)
         ((<p 3 = int ((1.618 / 2.618) * 800)) && (sumarea p = 480000) && (#p = 3)))
      ; toggleLayouts Full: off = inner, on = the focus fullscreen
      (#<(lay-tile (toggle-full L1) r z3) = 3)
      (<<(lay-tile (lay-msg (toggle-full L1) 'toggle-layout z3) r z3) = '(10 0 0 800 600))
      ; smartBorders: a lone window (or a fullscreen one) gets a border-0 hint (a 6th field)
      (: p <(lay-tile (smart-borders L1) r z1) ((#<p = 6) && (<p 5 = 0)))
      (: p <(lay-tile (smart-borders L1) r z3) (all (q \ tally q = 5) p))          ; multi-window: no border hint
      ; minimize filters a window out of the row; the rest re-tile; restore brings it back
      (: p <(lay-tile (lay-msg (minimize L1) '(minimize-window 20) z3) r z3)
         ((#p = 2) && !(afind (q \ q 0 = 20) p)))
      (: mm (lay-msg (lay-msg (minimize L1) '(minimize-window 20) z3) 'restore z3)
         (#<(lay-tile mm r z3) = 3))
      ; windowNavigation geometry: nearest window in a direction
      (: pls <(lay-tile L1 r z3)
         ((nav-target pls 10 'right = 20) && !(nav-target pls 10 'left)         ; master: right -> a slave, nothing left
          && (nav-target pls 20 'left = 10) && (nav-target pls 20 'down = 30)))     ; slave: left -> master, down -> the other slave
      ; the WHOLE config stack composes: message pass-through reaches the base, partition holds
      (sumarea <(lay-tile stk r z3) = 480000)
      (#<(lay-tile (lay-msg stk 'next-layout z3) r z3) = 3)                 ; next-layout reaches the ||| at the bottom
      (: q <(lay-tile (lay-msg stk 'toggle-layout z3) r z3) ((#q = 1) && (<q 5 = 0)))))                 ; toggle-full through the stack -> fullscreen, borderless

; --- the OPEN dispatch lane: a layout whose head is a CLOSURE answers the
; --- 'tile/'msg protocol itself -- user layouts enter as plain data, no table
; --- edit (the lux repl's live-layout hook), and modifiers wrap them like
; --- any builtin ---
(: (q op lay a b) (? (op = 'tile) (: f (zfoc b)                                  ; focus in a fixed corner box; state = (lay 1)
                                     [(f ? [(f >< rct 0 0 100 (lay 1))] ()) lay])
                     (op = 'msg) (a = 'grow ? [<lay (lay 1 + 10)] ())
                     ())
   Q [q 50] r (rct 0 0 800 600) z3 (zfrom '(10 20 30))
   (assert
      (<(lay-tile Q r z3) = '((10 0 0 100 50)))                                  ; the closure places
      (<>(lay-tile Q r z3) = Q)                                                  ; ... and hands its record back
      ((lay-msg Q 'grow z3) 1 = 60)                                         ; a message it takes rewrites its state
      !(lay-msg Q 'shrink z3)                                                    ; one it doesn't reads () -- modifiers fall through
      (: p <(lay-tile (smart-borders Q) r z3)                                    ; a modifier wraps the closure like any builtin
         ((#p = 1) && (#<p = 6)))
      (((lay-msg (smart-borders Q) 'grow z3) 1) 1 = 60)))              ; ... and a delegated message threads the closure's new state

; --- the manageHook DSL (crew/lux/manage.l rung 5): combinators + float actions ---
(: s0 (admit (admit (sheaf '(1 2 3)) 10) 20)                                     ; ws1 tiled (20 10); we manage 20
   c (mg-ctx 0 20 (rct 0 0 800 600) (rct 100 100 300 200))                      ; socket 0 (unused: pure actions/mocks), win 20, screen, geom
   yes (c \ 1) no (c \ 0)                                                        ; mock predicates
   tag (nm \ \ c sh (nm >< sh))                                               ; mock action: cons a tag onto the sheaf (marks it ran)
   (assert
      ; float actions move the (tiled) window into the floating half at the right rect
      (: f (do-full-float c s0) (wsfloating? f 20 && (wsrect f 20 = '(0 0 800 600)) && !(any (x \ x = 20) (wstiled f))))
      (: f (do-float c s0) (wsrect f 20 = '(100 100 300 200)))                  ; do-float keeps the window's own geometry
      (: f (do-center-float c s0) (wsrect f 20 = '(250 200 300 200)))           ; centered: (800-300)/2=250, (600-200)/2=200
      (do-ignore c s0 = evict s0 20)                                            ; ignore un-manages entirely
      ; <||> is predicate OR, and it reads as one: `p <||> q`
      ((yes <||> no) c) !((no <||> no) c) ((no <||> yes) c)
      ((no <||> no <||> yes) c)                                                  ; and chains right, so a run of alternatives is one rule
      ; -?> fires the action iff the predicate matches (a 1-list = matched, () = not)
      (((yes -?> tag 'a) c s0) = [('a >< s0)])
      !((no -?> tag 'a) c s0)
      ; the two bind so a rule needs no inner parens: `p <||> q -?> act`
      (((no <||> yes -?> (tag 'a)) c s0) = [('a >< s0)])
      ; compose-one takes the FIRST matching rule; none -> the sheaf unchanged
      (((compose-one [(no -?> tag 'a) (yes -?> tag 'b) (yes -?> tag 'c)]) c s0) = ('b >< s0))
      (((compose-one [(no -?> tag 'a) (no -?> tag 'b)]) c s0) = s0)
      ; compose-all threads every hook (a plain hook, so wrap or use identity)
      (((compose-all [(\ c sh ('x >< sh)) (\ c sh ('y >< sh))]) c s0) = ('y >< ('x >< s0)))
      ; manage-for is the assembled config hook -- a runnable closure (X-driven; not called here)
      (lit? manage-default) (lit? (manage-for '("a") '("b") '("c")))))

; --- the Go/Swap zipper halves (crew/lux/core.l rung 6): focus/swap by NAME ---
(: z3 (zmk 2 '(1) '(3))                                                          ; row (1 2 3), 2 focused
   (assert
      (zfoc (zfocto 3 z3) = 3) (zrow (zfocto 3 z3) = '(1 2 3))                   ; zfocto moves ONLY the focus
      (zfocto 2 z3 = z3) (zfocto 9 z3 = z3)                                      ; to self / to absent: no-ops
      (zrow (zswapw 1 3 z3) = '(3 2 1)) (zfoc (zswapw 1 3 z3) = 2)               ; zswapw exchanges positions, focus keeps its window
      (zrow (zswapw 2 3 z3) = '(1 3 2)) (zfoc (zswapw 2 3 z3) = 2)               ; ... even when the focus is a swap party
      (zswapw 1 3 (zswapw 1 3 z3) = z3)                                          ; an involution
      (zswapw 1 9 z3 = z3)))                                                     ; a missing party: no-op

; --- the keymap (crew/lux/keys.l rung 6): the table's shape, the union law, nextMatch ---
(: (assert
      ; every binding is (mods keysym action), the action a tagged list
      (all (r \ (#r = 3) && two? (kb-act r) && name? <(kb-act r)) the-keys)
      (kmodmask = (shiftm | (ctrlm | (altm | supm))))
      ; the directional generator: 4 dirs x 2 keysyms x 3 modifier rows
      (#keys-dir = 24)
      (keys-find keys-dir supm 104 = '(go left (-20 0)))                         ; sup+h
      (keys-find keys-dir (supm | shiftm) xk-up = '(swapd up (0 -20)))           ; sup+shift+Up
      (keys-find keys-dir (supm | ctrlm) 106 = '(resize down (0 20)))            ; sup+ctrl+j
      ; the union: custom wins on a collision, stock survives elsewhere
      (#keys-custom = 45) (#keys-def = 39) (#the-keys = 70)       ; 14 stock rows shadowed
      (keys-find the-keys supm xk-ret = '(spawn ("xfce4-terminal")))             ; custom sup+ret over stock swap-master
      (keys-find the-keys supm 109 = '(msg mirror))                              ; custom sup+m over stock focus-master
      (keys-find the-keys (supm | shiftm) xk-ret = '(spawn ("xterm")))           ; the stock terminal survives
      (keys-find the-keys supm 116 = '(sink))                                    ; stock mod+t survives
      (keys-find the-keys supm 49 = '(view 1))                                   ; the digit rows
      (keys-find the-keys (supm | shiftm) 57 = '(shift 9))
      !(keys-find the-keys supm 999)                                             ; an unbound chord reads ()
      ; nextMatch's MRU pick: forward = most recent other, backward = oldest
      (nm-pick '(10 20 30) 10 '(10 20 30) 1 = 20)
      (nm-pick '(10 20 30) 10 '(10 20 30) 0 = 30)
      (nm-pick '(10 20 30) 10 '(10) 1 = 20)                                      ; never-focused windows count as oldest
      (nm-pick '(10 20) 10 '(99 20 10) 1 = 20)                                   ; other-workspace history is ignored
      !(nm-pick '(10) 10 '(10) 1)))                                              ; nowhere to go

; --- EWMH + docks (crew/lux/ewmh.l rung 7): strut arithmetic + wire encodings ---
(: (assert
      ; _NET_WM_STRUT_PARTIAL's 12 values: the strut is the first 4; short = none
      (strut-parse '(0 0 30 0 0 0 0 0 0 0 400 800) = '(0 0 30 0))
      !(strut-parse '(1 2))
      ; struts clamp the rect off each edge; a swallowed axis floors at 0
      (strut-shrink (rct 0 0 800 600) '(0 0 30 0) 800 600 = '(0 30 800 570))     ; a top-30 panel
      (strut-shrink (rct 0 0 800 600) '(0 50 0 20) 800 600 = '(0 0 750 580))     ; right + bottom
      ((strut-shrink (rct 0 0 100 100) '(700 0 0 0) 800 600) 2 = 0)))

(: _ (pin ewst 'struts '((77 (0 0 30 0)) (88 (40 0 0 0)))) _ (pin ewst 'avoid 1)
   r1 (ewmh-rect (rct 0 0 800 600) 800 600)
   _ (ewmh-toggle ()) r2 (ewmh-rect (rct 0 0 800 600) 800 600)                   ; ToggleStruts: the full screen again
   _ (ewmh-toggle ()) _ (ewmh-untrack 88) r3 (ewmh-rect (rct 0 0 800 600) 800 600)
   _ (pin ewst 'struts ())                                                       ; leave the tablet clean
   (assert (r1 = '(40 30 760 570)) (r2 = '(0 0 800 600)) (r3 = '(0 30 800 570))))

; --- the property/window encodings, byte-exact into a jug ---
(: j (jug 0) _ (x-chprop32 j 5 6 7 '(9 10)) b (slurp j)
   (assert (#b = 32) (b 0 = 18) (b 2 = 8)                                   ; opcode, len 6+2
           (b 4 = 5) (b 8 = 6) (b 12 = 7)                                        ; window, property, type
           (b 16 = 32) (b 20 = 2) (b 24 = 9) (b 28 = 10)))                       ; format, count, the two values

(: j (jug 0) _ (x-chprop8 j 5 6 7 "AB") b (slurp j)
   (assert (#b = 28) (b 0 = 18) (b 2 = 7)                                   ; len 6 + 1 (2 chars + pad)
           (b 16 = 8) (b 20 = 2) (b 24 = 65) (b 25 = 66) (b 26 = 0)))            ; format 8, n=2, "AB\0\0"

(: j (jug 0) _ (x-mkwin j 42 7) b (slurp j)
   (assert (#b = 32) (b 0 = 1) (b 4 = 42) (b 8 = 7)                         ; CreateWindow, wid, parent
           (b 12 = 255) (b 13 = 255) (b 16 = 1) (b 22 = 2)))                     ; x=-1, w=1, class InputOnly

; --- ClientMessage field reads (win@4 type@8 data@12,16,20) ---
(: pk ('(33 32 0 0 9 0 0 0 5 0 0 0 2 0 0 0 7 0 0 0 8 0 0 0) + '(0 0 0 0 0 0 0 0))
   (assert (ewmh-cm-win pk = 9) (ewmh-cm-type pk = 5)
           (ewmh-cm-d0 pk = 2) (ewmh-cm-d1 pk = 7) (ewmh-cm-d2 pk = 8)))

; --- the config (crew/lux/config.l rung 8): the transliteration's shape.
;     ⚠ EVERY law here reads an EMPTY tablet, never lux-conf: this gate must mean
;     the same thing on a machine that has written ~/.love/etc/lux.l. ---
(: c0 (tablet 0)
   lay0 (foldr lux-wrap-of (choice (filter two? (map lux-lay-of (conf-roster c0 'layout '((rtall 1 0.02 0.5) (dwindle phi))))))
               (conf-roster c0 'layout-wrap '(minimize smart toggle-full mirror)))
   sm (lay0 2) tf (sm 1) mir (tf 2) ch (mir 2)               ; peel minimize/smart/toggle/mirror down to the |||
   subs (ch 2) rt <subs dw <>subs
   (assert
      (salt-one c0 'border 3 = 3) (conf-tags c0 = 9) (conf-mod c0 = supm)
      (<lay0 = 'minimize) (<sm = 'smart) (<tf = 'toggle-lay)
      (<mir = 'mirror) (<ch = 'choice)
      (<rt = 'rtall) (rt 1 = 1) (rt 2 = 0.5)                           ; ResizableTall 1 (2/100) (1/2)
      (<dw = 'dwindle)
      (: phi (dw 1) ((1.61803 < phi) && (phi < 1.61804)))                   ; fib 32 / fib 31, the golden ratio
      (#(keys-build supm 20 9 () ()) = 70) (lit? (manage-for '("a") '("b") ()))
      (<<(conf-roster c0 'startup '(("setxkbmap"))) = "setxkbmap")))

; --- the config DOOR (lib/salt.l): every setting overrides, and its ABSENCE is
;     the value that used to be hardcoded. the tablet is built by hand -- salt
;     pins a setting's whole TAIL under its head, which is what salt-file writes. ---
(: (st l) (foldl (\ t p (pin t <p >p)) (tablet 0) l)                             ; a settings roster -> the tablet salt would answer
   c1 (st '((border 5) (mod alt) (tags 4) (nudge 7)
            (layout (dwindle 2.0)) (layout-wrap smart)
            (float-class inkscape "blender") (center-class ()) (startup ())))
   lay1 (foldr lux-wrap-of (choice (filter two? (map lux-lay-of (conf-roster c1 'layout '((rtall 1 0.02 0.5))))))
               (conf-roster c1 'layout-wrap '(minimize smart toggle-full mirror)))
   km (keys-build (conf-mod c1) (salt-one c1 'nudge 20) (conf-tags c1)
                  (conf-roster c1 'bind ()) (conf-roster c1 'unbind ()))
   (assert
      (salt-one c1 'border 3 = 5) (conf-tags c1 = 4) (conf-mod c1 = altm)        ; scalars land
      (<lay1 = 'smart) (<(lay1 1) = 'choice)                                ; layout-wrap replaces the whole stack
      (#((lay1 1) 2) = 1)                                         ; one ||| member, the config's dwindle
      (<<((lay1 1) 2) = 'dwindle)
      !(conf-roster c1 'center-class ())                                         ; `(k ())` IS the empty roster...
      (#(conf-roster c1 'float-class '("x")) = 2)                           ; ...and a named one lands whole
      !(conf-roster c1 'startup '(("setxkbmap")))                                ; so a config can spawn nothing at all
      (#(conf-roster c1 'ignore-class '("k")) = 1)                          ; an ABSENT roster keeps the default
      (keys-find km altm 120 = '(kill))                                          ; the whole table moved to the new mod...
      !(keys-find km supm 120)                                                   ; ...and left sup behind
      (keys-find km altm 104 = '(go left (-7 0)))                                ; nudge feeds the directional deltas
      (keys-find km altm 52 = '(view 4)) !(keys-find km altm 53)                 ; digit rows stop at the tag count
      (two? lux-tags) (two? lux-keymap) (lit? lux-manage) (two? lux-layout)))    ; and the LIVE ones built, whatever this machine's file says

; --- key SPECS (crew/lux/keys.l): the surface a config writes chords in ---
(: c2 (foldl (\ t p (pin t <p >p)) (tablet 0)
             '((bind ((sup Return) (spawn "alacritty" "-e" "tmux"))
                     ((sup shift F1) (view 1))
                     ((mod XF86AudioMute) (spawn "amixer" "set" "Master" "toggle"))
                     ((sup 65027) (kill))
                     ((nonsense q) (kill)))
               (unbind (sup p) (sup z) (sup bogus))))
   km (keys-build supm 20 9 (conf-roster c2 'bind ()) (conf-roster c2 'unbind ()))
   (assert
      (kspec supm '(sup e) = '(64 101))                                          ; a one-char key IS its ascii keysym
      (kspec supm '(sup shift Return) = '(65 65293))                             ; masks OR together, names come off the table
      (kspec altm '(mod Tab) = '(8 65289))                                       ; `mod` follows the configured modifier
      (kspec supm '(F7) = '(0 65476))                                            ; ⚠ no modifier is a mask of 0, not a failure
      (kspec supm '(sup ",") = '(64 44))                                         ; a one-char STRING is that character, punctuation and all
      (kspec supm '(sup "1") = '(64 49))                                         ; ... digits the same way
      (kspec supm '(sup 65027) = '(64 65027))                                    ; a bare charm IS the keysym: the table is no ceiling
      !(kspec supm '(sup 1)) !(kspec supm '(sup 0))                              ; ⚠ but not below 32 -- `1` is the digit's trap, not its keysym
      !(kspec supm '(sup nosuchkey)) !(kspec supm '(bogus e))                    ; an unknown field kills the whole spec
      !(kspec supm 'sup+e) !(kspec supm ())                                      ; a spec is a LIST; a bare name is not one
      (keys-find km supm xk-ret = '(spawn ("alacritty" "-e" "tmux")))            ; a config's argv rides in FLAT and lands nested
      (keys-find km (supm | shiftm) 65470 = '(view 1))
      (keys-find km supm 269025042 = '(spawn ("amixer" "set" "Master" "toggle")))
      (keys-find km supm 65027 = '(kill))                                        ; the raw-keysym chord grabs like any other
      !(keys-find km supm 112) !(keys-find km supm 122)                          ; unbind cuts, and cuts LAST
      (keys-find km supm 120 = '(kill))                                          ; ... only what it names
      (#km = 71)))                                                          ; 70 stock, +3 new rows, -2 unbound (1 bind + 1 unbind unreadable)

; --- ~/.Xauthority parsing (crew/lux/wire.l rung 8), on mock bytes ---
(: (be16 n) ((string (>> n 8)) + string (255 & n))                                        ; big-endian u16 as two appended bytes
   (fld t) (be16 #t + t)
   (entry num data) ((((be16 256 + fld "bee") + fld num) + fld "MIT-MAGIC-COOKIE-1") + fld data)
   b (entry "0" "aaaabbbbccccdddd" + entry "7" "0123456789abcdef")
   (assert
      (xauth-parse b "7" = '("MIT-MAGIC-COOKIE-1" "0123456789abcdef"))           ; finds the display's cookie
      (<>(xauth-parse b "0") = "aaaabbbbccccdddd")                               ; ... whichever display
      !(xauth-parse b "9")                                                       ; no entry -> ()
      !(xauth-parse "" "0")))

; --- the rung-8 wire encodings, byte-exact into a jug ---
(: j (jug 0) _ (x-borderpix j 5 77) b (slurp j)                                  ; CWA CWBorderPixel
   (assert (#b = 16) (b 0 = 2) (b 4 = 5) (b 8 = 8) (b 12 = 77)))
(: j (jug 0) _ (x-cursor j 9 41 42) b (slurp j)                                  ; OpenFont + CreateGlyphCursor + CloseFont + CWA CWCursor
   (assert (#b = 76)
           (b 0 = 45) (b 2 = 5) (b 4 = 41) (b 12 = 99)                           ; OpenFont len 5, fid, "cursor"[0]='c'
           (b 20 = 94) (b 24 = 42) (b 36 = 68)                                   ; CreateGlyphCursor, cid, glyph 68
           (b 52 = 46) (b 60 = 2) (b 68 = 0) (b 69 = 64) (b 72 = 42)))           ; CloseFont; CWA mask 0x4000, value cid

; --- minimize's targeted restore (a taskbar activate; crew/lux/layout.l rung 8) ---
(: z3 (zfrom '(10 20 30)) r (rct 0 0 800 600)
   M (lay-msg (minimize (resizable-tall 1 0.02 0.5)) '(minimize-window 20) z3)
   (assert
      (#<(lay-tile M r z3) = 2)                                             ; 20 hidden
      (#<(lay-tile (lay-msg M '(restore-window 20) z3) r z3) = 3)           ; named restore brings it back
      !(lay-msg M '(restore-window 30) z3)))                                     ; not minimized -> unhandled ()

(test_fin ())                                        ; ⚠ no zz-fin here: quit before claiming ok
(say err "crew/lux/law: StackSet zipper + sheaf + floating + layout + modifier + manage + keymap + ewmh + config laws ok
")
