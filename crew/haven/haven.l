; crew/haven/haven.l -- haven, the wayland compositor: a shore the clients
; moor at. speaks wl_shm / wl_compositor / xdg_wm_base / wl_output, TILES
; its toplevels with lux's pure layout math (crew/lux/layout.l over the
; focus zipper -- new toplevel takes the master column, xmonad's way), and
; composites every committed buffer into its tile in haven's own X window
; (the pier's blit lane: px-ship over crew/lux/wire.l). a wayland client
; draws, the layout places, and X shows it. loads after crew/lux/{core,
; wire,layout}.l + crew/quay/pier.l + crew/haven/wire.l:
;
;   love -l crew/lux/core.l -l crew/lux/wire.l -l crew/lux/layout.l \
;      -l crew/quay/pier.l -l crew/haven/wire.l -l crew/haven/haven.l \
;      -e '(haven "/run/user/1000/haven-0" () () 800 600)'
;
; the harbor book hb: 0 the zipper, 1 the layout, 2 the next toplevel key,
; 3 a zero row (the fb wipe), 4 the configure serial, 5 the dirty flag (the
; painter task's cue). tl maps a key to (port toplevel-id xdgsurf-id
; surface-id rect) -- the roster.
;
; the MOOR: a second shore at <path>.moor -- the whole language into the
; running compositor (`ain <path>.moor` is the client). where sway has
; swaymsg's fixed JSON, haven answers with ev.
;
; a body-LESS `:` -- the helpers leak; none shadows a nif.
(:
   ; --- the registry: (name interface version kind), what get_registry sings ---
   hv-globals '((1 "wl_shm" 1 shm) (2 "wl_compositor" 4 compositor)
                (3 "xdg_wm_base" 2 wmbase) (4 "wl_output" 2 output)
                (5 "wl_seat" 1 seat)
                (6 "zwp_text_input_manager_v3" 1 timgr)
                (7 "zwp_input_method_manager_v2" 1 immgr))
   (hv-kind nm) (: (go l) (? (two? l)
                             (? (= nm (cap (cap l))) (nthl (cap l) 3) (go (cup l)))
                             ())
                   (go hv-globals))

   ; --- the compositor's one move: a retained buffer lands in its TILE,
   ;     row by row off the pool's mapping, clipped to the rect. no tile
   ;     (rect ()) draws nowhere, a slipped pool reads ptr 0 and skips.
   ;     PAINTER-ONLY: the one task that blits is the one that unmaps, so
   ;     a mapin can never race an unmap -- and it alone ships the glass
   ;     (two tasks streaming px-ship on one socket interleave mid-
   ;     PutImage and the server reads pixels as opcodes) ---
   (hv-blit x fb buf rect)
     (: rc (nthl buf 1) ptr (peep rc 2 0)
        (? (? (two? rect) (< 0 ptr) 0)
           (: fbw (nthl x 4)
              boff (nthl buf 2) w (nthl buf 3) h (nthl buf 4) stride (nthl buf 5)
              rows (mn2 h (rh rect)) cols (mn2 w (rw rect))
              bytes (* cols 4) rowb (* fbw 4)
              (go y) (? (< y rows)
                        (: _ (mapin fb (+ (* (+ (ry rect) y) rowb) (* (rx rect) 4))
                                    ptr (+ boff (* y stride)) bytes)
                           (go (+ y 1)))
                        0)
              (go 0))
           0))

   ; --- the ledger half of a destructor: the record unpins and the client
   ;     hears delete_id, so the id is truly free to REUSE ---
   (hv-bye c ob obj) (: _ (pin ob obj ()) (wl-ship c 1 1 (L obj) ()))

   ; --- the seat's bookkeeping. hb grows input slots: 6 the keyboard roster
   ;     ((client kbdid)..), 7 the pointer roster, 8 the keyboard-entered key,
   ;     9 the pointer-entered key, 11 the xkb keymap text, 12 the key table
   ;     ((mods keycode action)..), 13 the boot sym->keycode map. a serial
   ;     per notable event, off the shared counter ---
   (hv-srl hb) (: n (peep hb 4 1) _ (pin hb 4 (+ n 1)) n)
   (hv-roster hb slot c f) (map (\ p (? (id? (cap p) c) (f (caup p)) 0)) (peep hb slot ()))
   (hv-unroster hb slot c id)
     (pin hb slot (filter (\ p (! (? (id? (cap p) c) (= (caup p) id) 0))) (peep hb slot ())))

   ; --- the keymap, shipped as libxkbcommon's own text over a memfd: format 1
   ;     when the compiled text rode in (out/lib/haven-keymap.xkb -- xkbcli's
   ;     output, the Makefile bakes it), format 0 when the harbor has none ---
   (hv-loadkeymap u)
     (: p (: e (getenv "HAVEN_KEYMAP") (? (< 1 (tally e)) e "out/lib/haven-keymap.xkb"))
        q (open p "r")
        (? (lit? q) (: t (slurp q) _ (close q) t) ""))
   (hv-keymap hb c kid)
     (: t (peep hb 11 "") n (tally t)
        size (+ n 1)
        fd (memfd size) m (mapfd fd size)
        (? (? (< fd 0) 1 (< m 1)) 0
           (: b (cask size)                               ; text + the NUL (a cask wakes zeroed)
              (go i) (? (< i n) (: _ (pin b i (t i)) (go (+ i 1))) 0)
              _ (go 0)
              _ (mapout m 0 b 0 size)
              _ (wl-ship c kid 0 (L (? (< 0 n) 1 0) size) (L fd))
              _ (unmap m size)
              (fdclose fd))))

   ; --- the IME seam: text-input v3 + input-method v2. hb 15 the text-input
   ;     roster ((client tiid enabled serial)..), hb 16 the input method
   ;     (client imid grabid) -- ONE per harbor, a second hears unavailable;
   ;     hb 17/18 the im's pending commit/preedit. the ACTIVE text-input is
   ;     the focused client's enabled one; enable/disable and the focus
   ;     crossings drive im activate/deactivate, and the im's committed text
   ;     pours back to the active text-input (each im word done-terminated) ---
   (hv-tirows hb c) (filter (\ r (id? (cap r) c)) (peep hb 15 ()))
   (hv-tiedit hb c id f)
     (pin hb 15 (map (\ r (? (? (id? (cap r) c) (= (caup r) id) 0) (f r) r)) (peep hb 15 ())))
   (hv-active hb tl)
     (: fk (peep hb 8 ()) e (? fk (peep tl fk ()) ())
        (? (! (two? e)) ()
           (afind (\ r (? (id? (cap r) (cap e)) (< 0 (nthl r 2)) 0)) (peep hb 15 ()))))
   (hv-im hb op ws)
     (: im (peep hb 16 ())
        (? (two? im) (: _ (wl-ship (cap im) (caup im) op ws ())
                        (wl-ship (cap im) (caup im) 5 () ()))
           0))
   ; ti leave/enter + im deactivate/activate across a focus crossing
   (hv-ticross hb tl old new)
     (: _ (: oe (? old (peep tl old ()) ())
            (? (two? oe)
               (: oc (cap oe)
                  _ (map (\ r (wl-ship oc (caup r) 1 (L (nthl oe 3)) ())) (hv-tirows hb oc))
                  (? (any (\ r (< 0 (nthl r 2))) (hv-tirows hb oc)) (hv-im hb 1 ()) 0))
               0))
        (: ne (? new (peep tl new ()) ())
           (? (two? ne)
              (: nc (cap ne)
                 _ (map (\ r (wl-ship nc (caup r) 0 (L (nthl ne 3)) ())) (hv-tirows hb nc))
                 (? (any (\ r (< 0 (nthl r 2))) (hv-tirows hb nc)) (hv-im hb 0 ()) 0))
              0)))

   ; --- keyboard focus follows the zipper: leave the old key's client, enter
   ;     the new one's (empty keys array -- nothing held across the crossing) ---
   (hv-refocus hb tl)
     (: old (peep hb 8 ()) z (peep hb 0 ())
        new (? (two? z) (zfoc z) ())
        (? (= old new) 0
           (: _ (? old (: e (peep tl old ())
                          (? (two? e)
                             (hv-roster hb 6 (cap e)
                               (\ kid (wl-ship (cap e) kid 2 (L (hv-srl hb) (nthl e 3)) ())))
                             0))
                    0)
              _ (? new (: e (peep tl new ())
                          (? (two? e)
                             (hv-roster hb 6 (cap e)
                               (\ kid (wl-ship (cap e) kid 1 (L (hv-srl hb) (nthl e 3) 0) ())))
                             0))
                    0)
              _ (hv-ticross hb tl old new)
              (pin hb 8 new))))

   ; --- a key from the glass: the key TABLE answers first (hb 12, rows
   ;     (mods keycode action) -- the action is a control-plane closure, run
   ;     on press, the chord swallowed both ways; state masked to the real
   ;     chord mods, lux's kmodmask). everything else pours to the focused
   ;     client's keyboards -- modifiers first (X's state bits ARE xkb's real
   ;     mods for the standard map), then the key, X keycode folded to evdev
   ;     (-8) ---
   (hv-keyev hb tl time kc press state)
     (: hit (afind (\ r (? (= (cap r) (& 77 state)) (= (caup r) kc) 0)) (peep hb 12 ()))
        (? (two? hit)
           (? press (: _ ((nthl hit 2) ()) 0) 0)
           (: im (peep hb 16 ()) g (? (two? im) (nthl im 2) 0)
              (? (< 0 g)                                  ; a grab steals the keys for the im
                 (: _ (wl-ship (cap im) g 2 (L (hv-srl hb) (& 255 state) 0 0 0) ())
                    (wl-ship (cap im) g 1 (L (hv-srl hb) time (- kc 8) press) ()))
                 (: fk (peep hb 8 ()) e (? fk (peep tl fk ()) ())
                    (? (two? e)
                       (hv-roster hb 6 (cap e)
                         (\ kid (: _ (wl-ship (cap e) kid 4 (L (hv-srl hb) (& 255 state) 0 0 0) ())
                                   (wl-ship (cap e) kid 3 (L (hv-srl hb) time (- kc 8) press) ()))))
                       0))))))

   ; --- the pointer: whichever tile holds it hears enters/leaves/motion in
   ;     surface-local 24.8 fixed; a press focuses its tile (click-to-focus) ---
   (hv-atxy hb tl px py)
     (: (go row) (? (two? row)
                    (: e (peep tl (cap row) ()) r (? (two? e) (nthl e 4) ())
                       (? (? (two? r)
                             (? (! (< px (rx r))) (? (< px (+ (rx r) (rw r)))
                                (? (! (< py (ry r))) (< py (+ (ry r) (rh r))) 0) 0) 0)
                             0)
                          (cap row) (go (cup row))))
                    ())
        z (peep hb 0 ())
        (go (? (two? z) (zrow z) ())))
   (hv-ptrmove hb tl time px py)
     (: cur (hv-atxy hb tl px py) old (peep hb 9 ())
        _ (? (= cur old) 0
             (: _ (? old (: e (peep tl old ())
                            (? (two? e)
                               (hv-roster hb 7 (cap e)
                                 (\ pid (wl-ship (cap e) pid 1 (L (hv-srl hb) (nthl e 3)) ())))
                               0))
                      0)
                _ (? cur (: e (peep tl cur ()) r (? (two? e) (nthl e 4) ())
                            (? (two? e)
                               (hv-roster hb 7 (cap e)
                                 (\ pid (wl-ship (cap e) pid 0
                                          (L (hv-srl hb) (nthl e 3)
                                             (<< (- px (rx r)) 8) (<< (- py (ry r)) 8)) ())))
                               0))
                      0)
                (pin hb 9 cur)))
        (? cur (: e (peep tl cur ()) r (? (two? e) (nthl e 4) ())
                  (? (two? e)
                     (hv-roster hb 7 (cap e)
                       (\ pid (wl-ship (cap e) pid 2
                                (L time (<< (- px (rx r)) 8) (<< (- py (ry r)) 8)) ())))
                     0))
           0))
   (hv-ptrbtn hb tl time btn press)
     (: cur (peep hb 9 ())
        code (? (= btn 1) 272 (= btn 2) 274 (= btn 3) 273 (+ 272 btn))
        _ (? (? press cur 0)
             (: z (peep hb 0 ())
                _ (? (two? z) (pin hb 0 (zfocto cur z)) 0)
                (hv-refocus hb tl))
             0)
        (? cur (: e (peep tl cur ())
                  (? (two? e)
                     (hv-roster hb 7 (cap e)
                       (\ pid (wl-ship (cap e) pid 3 (L (hv-srl hb) time code press) ())))
                     0))
           0))

   ; --- a pool slips its moorings: the cell zeroes NOW (no new reads start)
   ;     but the unmap DEFERS to the painter's reap list (hb 19) -- the
   ;     painter is the only blitter, so sequencing the unmaps into its own
   ;     tick means a mapin can never race an unmap (a client task freeing
   ;     a pool mid-composite segfaulted exactly here). rc is the pool's
   ;     shared cell -- 0 live buffers, 1 parting, 2 ptr, 3 size, 4 fd --
   ;     so buffers outlive the pool OBJECT (simple-shm's habit) and a
   ;     resize remaps in one place ---
   (hv-slip hb rc) (: ptr (peep rc 2 0)
                      _ (? (< 0 ptr)
                           (pin hb 19 (link (L ptr (peep rc 3 0) (peep rc 4 0)) (peep hb 19 ())))
                           0)
                      (pin rc 2 0))
   (hv-reap hb) (: q (pull hb 19 ())
                   (map (\ r (: _ (unmap (cap r) (caup r))
                                (? (< (nthl r 2) 0) 0 (fdclose (nthl r 2)))))
                        q))

   ; --- retile: the layout places the row, every toplevel hears its new
   ;     size, unplaced toplevels lose their rects (FULL hides the rest --
   ;     a stale rect would composite a hidden window), the glass wipes,
   ;     and the painter's next composite repaints every placed tile from
   ;     its retained buffer -- a same-size move (a swap) never makes the
   ;     client recommit, so the compositor repaints from what it holds ---
   (hv-retile hb tl x fb)
     (: res (lay-tile (peep hb 1 ()) (rct 0 0 (nthl x 4) (nthl x 5)) (peep hb 0 ()))
        pls (cap res)
        _ (pin hb 1 (caup res))
        rowb (* (nthl x 4) 4) zr (peep hb 3 ())
        (wipe y) (? (< y (nthl x 5)) (: _ (pour fb (* y rowb) zr 0 rowb) (wipe (+ y 1))) 0)
        _ (wipe 0)
        _ (map (\ key (? (any (\ p (= key (cap p))) pls) 0
                         (: e (peep tl key ())
                            (? (two? e)
                               (pin tl key (L (cap e) (nthl e 1) (nthl e 2) (nthl e 3) ()))
                               0))))
               (zrow (peep hb 0 ())))
        _ (pin hb 5 1)                                   ; the painter composites + ships
        _ (map (\ p (: key (cap p) e (peep tl key ())
                       (? (! (two? e)) 0
                          (: c (cap e)
                             _ (pin tl key (L c (nthl e 1) (nthl e 2) (nthl e 3) (cup p)))
                             _ (wl-ship c (nthl e 1) 0 (L (nthl p 3) (nthl p 4) 0) ())
                             srl (peep hb 4 1)
                             _ (pin hb 4 (+ srl 1))
                             (wl-ship c (nthl e 2) 0 (L srl) ())))))
               pls)
        (hv-refocus hb tl))                              ; the zipper may have turned: keyboards follow

   ; --- a parting client: its toplevels leave the row, its seat objects the
   ;     rosters, the rest re-place ---
   (hv-farewell c hb tl x fb)
     (: mine (filter (\ key (: e (peep tl key ()) (? (two? e) (= c (cap e)) 0)))
                     (zrow (peep hb 0 ())))
        _ (map (\ key (: _ (pin hb 0 (zdel key (peep hb 0 ())))
                         _ (pin (peep hb 14 #()) key ())
                         (pin tl key ()))) mine)
        _ (pin hb 6 (filter (\ p (! (id? (cap p) c))) (peep hb 6 ())))
        _ (pin hb 7 (filter (\ p (! (id? (cap p) c))) (peep hb 7 ())))
        _ (pin hb 15 (filter (\ r (! (id? (cap r) c))) (peep hb 15 ())))
        _ (? (: im (peep hb 16 ()) (? (two? im) (id? (cap im) c) 0))
             (: _ (pin hb 16 ()) _ (pin hb 17 ()) (pin hb 18 ()))
             0)
        _ (? (any (\ key (= key (peep hb 9 ()))) mine) (pin hb 9 ()) 0)
        _ (? (any (\ key (= key (peep hb 8 ()))) mine) (pin hb 8 ()) 0)
        (? (two? mine) (hv-retile hb tl x fb) 0))

   ; --- the moor: lux's moor, haven-shaped. reads peep the live harbor
   ;     through (haven-st ()) -- (L hb tl x fb), pinned at boot; a CHANGE is
   ;     (f z lay) -> (L z2 lay2) over the pure half of the state, queued
   ;     through (haven!) and drained by the PAINTER on its 8ms tick -- haven
   ;     is its own server, so a queued change lands within a frame (no wake
   ;     wire needed), the painter stays the single application point (one
   ;     hand on the state like one hand on the wire), and the retile rides
   ;     the existing configure/ack path. the seatbelt: the new row must be
   ;     duplicate-free and every key a real toplevel -- else refused, the
   ;     old state kept. with layout.l's open dispatch lane, (lay! (L my-
   ;     closure state..)) swaps in a layout written at the prompt ---
   haven-cell #()
   (haven-st u) (peep haven-cell 'st ())
   (haven! f) (: _ (pin haven-cell 'q (link f (peep haven-cell 'q ())))          ; (peep+pin -- a drain in the gap can lose a change;
                 'queued)                                                        ;  prompt-scale odds, and the seatbelt guards either way)
   (lay! l)   (haven! (\ z lay (L z l)))
   (msg! m)   (haven! (\ z lay (: l2 (lay-msg lay m z) (L z (? l2 l2 lay)))))
   (focus! w) (haven! (\ z lay (L (zfocto w z) lay)))
   (row u)    (: st (haven-st ()) (? (two? st) (zrow (peep (cap st) 0 ())) ()))
   ; --- the CONTROL PLANE verbs: one vocabulary drives haven from the keys
   ;     AND the moor -- a chord's action IS one of these closures, so every
   ;     key is rebindable live over the socket ((bind! ..), the stumpwm
   ;     move). the pure ones queue through the guard; part!/spawn are one
   ;     atomic effect each (a single wl-ship / a tether), safe from any task ---
   (up! u)     (haven! (\ z lay (L (zfocup z) lay)))
   (dn! u)     (haven! (\ z lay (L (zfocdn z) lay)))
   (swapup! u) (haven! (\ z lay (L (zswapup z) lay)))
   (swapdn! u) (haven! (\ z lay (L (zswapdn z) lay)))
   (grow! u)   (msg! 'grow)
   (shrink! u) (msg! 'shrink)
   (next! u)   (msg! 'next-layout)
   (part! u)   (: st (haven-st ())                       ; ask the focused toplevel to close --
                  (? (! (two? st)) 0                     ; the client parts on its own and the
                     (: hb (cap st) tl (nthl st 1)       ; normal farewell/retile path follows
                        fk (peep hb 8 ()) e (? fk (peep tl fk ()) ())
                        (? (two? e) (wl-ship (cap e) (nthl e 1) 1 () ()) 0))))
   (spawn cmd) (tether (L "sh" "-c" (+ "WAYLAND_DISPLAY=" (+ (peep haven-cell 'path "") (+ " exec " cmd)))))
   (bind! mods sym f)                                    ; sym an X keysym; keycode off the boot map
     (: st (haven-st ())
        (? (! (two? st)) 'no-harbor
           (: hb (cap st) kc (peep (peep hb 13 #()) sym 0)
              (? (! (< 0 kc)) 'no-key
                 (: kt (filter (\ r (! (? (= (cap r) mods) (= (caup r) kc) 0))) (peep hb 12 ()))
                    _ (pin hb 12 (link (L mods kc f) kt))
                    'bound)))))
   (hv-sane z tl) (: ks (zrow z)
                     (? (znodup? ks)
                        (! (any (\ w (! (two? (peep tl w ())))) ks))
                        0))
   (hv-drain q hb tl x fb)
     (: st (foldl (\ acc f
                     (: r (f (cap acc) (caup acc))
                        (? (? (two? r) (? (= 2 (tally r)) (hv-sane (cap r) tl) 0) 0)
                           r
                           (: _ (say out "haven: change refused\n") _ (flush out) acc))))
                  (L (peep hb 0 ()) (peep hb 1 ()))
                  (rev q))
        _ (pin hb 0 (cap st))
        _ (pin hb 1 (caup st))
        (hv-retile hb tl x fb))
   ; a session reads LINES (see parks the TASK -- the reader's park law) and
   ; sounds the TEXT: `sound` walks a charlist, so a socket running dry mid-skip
   ; cannot strand the whole vm in a C parse the way sounding the fd once did
   ; (the trailing newline lured it in; the painter starved). a torn shape
   ; carries to the next line as the RESIDUE the reader hands back, and every
   ; form on a line runs, where only the first used to.
   (hv-session c)
     (: (rdline acc) (: ch (see c)
                        (? (= ch -1) ()
                           (= ch 10) (rev (link 10 acc))
                           (rdline (link ch acc))))
        (go pend) (: cl (rdline ())
                     (? (! (two? cl)) 0
                        ((: (h cl) (: r (sound cl)
                                      (? (two? r) (: v (ev 'ev (cap r))
                                                     _ (say c (+ (show v) "\n"))
                                                     _ (flush c)
                                                     (h (cup r)))
                                         (id? r 'torn) (go cl)
                                         (go ()))))
                         (cat pend cl))))
        (go ()))
   (hv-moor path)
     (: lp (shore path)
        (? (! (lit? lp)) (: _ (say out (+ "haven: no moor at " (+ path "\n"))) (flush out))
           (: _ (say out (+ "haven: moored at " (+ path "\n")))
              _ (flush out)
              (knock _) (: _ (await lp)
                           c (accept lp)
                           _ (? (lit? c) (twirl (\ _ (: _ (hv-session c) (close c))) 0) 0)
                           (knock ()))
              (knock ()))))

   ; --- one request. ob keeps the client's objects (id -> (kind ..)); st the
   ;     seam (0 carry fill, 1 fd queue, 2 the client's pool cells); k the
   ;     carry the offsets index; hb/tl the shared harbor ---
   (hv-on c st ob k x fb hb tl obj op off sz)
     (: rec (peep ob obj ())
        kind (? (two? rec) (cap rec) (? (= obj 1) 'display ()))
        (? (= kind 'display)
             (? (= op 0) (: cb (wl-u32 k off)             ; sync: done, then let go
                            _ (wl-ship c cb 0 (L 0) ())
                            (wl-ship c 1 1 (L cb) ()))
                (= op 1) (: rid (wl-u32 k off)            ; get_registry: the globals
                            _ (pin ob rid (L 'registry))
                            (map (\ g (wl-ship c rid 0
                                        (link (cap g) (cat (wl-strw (nthl g 1)) (L (nthl g 2))))
                                        ()))
                                 hv-globals))
                0)
           (= kind 'registry)                             ; bind: kind by name
             (: nm (wl-u32 k off)
                vo (wl-rskip k (+ off 4))
                vs (wl-u32 k vo)
                nid (wl-u32 k (+ vo 4))
                kd (hv-kind nm)
                _ (pin ob nid (L kd))
                (? (= kd 'shm)                            ; the formats: argb, xrgb
                     (: _ (wl-ship c nid 0 (L 0) ()) (wl-ship c nid 0 (L 1) ()))
                   (= kd 'output)                         ; the glass, sung whole
                     (: _ (wl-ship c nid 0 (cat (L 0 0 0 0 0)
                                           (cat (wl-strw "haven") (cat (wl-strw "glass") (L 0)))) ())
                        _ (wl-ship c nid 1 (L 3 (nthl x 4) (nthl x 5) 60000) ())
                        (? (< vs 2) 0                     ; scale + done arrived at v2
                           (: _ (wl-ship c nid 3 (L 1) ())
                              (wl-ship c nid 2 () ()))))
                   (= kd 'seat)                           ; capabilities: pointer | keyboard
                     (wl-ship c nid 0 (L 3) ())
                   0))
           (= kind 'seat)                                 ; get_pointer / get_keyboard / get_touch
             (? (= op 0) (: nid (wl-u32 k off)
                            _ (pin ob nid (L 'pointer))
                            (pin hb 7 (link (L c nid) (peep hb 7 ()))))
                (= op 1) (: nid (wl-u32 k off)
                            _ (pin ob nid (L 'keyboard))
                            _ (pin hb 6 (link (L c nid) (peep hb 6 ())))
                            _ (hv-keymap hb c nid)
                            (: fk (peep hb 8 ())          ; already focused here? hear enter now
                               e (? fk (peep tl fk ()) ())
                               (? (? (two? e) (id? (cap e) c) 0)
                                  (wl-ship c nid 1 (L (hv-srl hb) (nthl e 3) 0) ())
                                  0)))
                (= op 2) (pin ob (wl-u32 k off) (L 'touch))
                0)
           (= kind 'keyboard)                             ; release (v3 -- defensively honored)
             (? (= op 0) (: _ (hv-unroster hb 6 c obj) (hv-bye c ob obj)) 0)
           (= kind 'pointer)                              ; set_cursor is a courtesy we skip
             (? (= op 1) (: _ (hv-unroster hb 7 c obj) (hv-bye c ob obj)) 0)
           (= kind 'touch)
             (? (= op 0) (hv-bye c ob obj) 0)
           (= kind 'timgr)                                ; get_text_input: join the roster
             (? (= op 0) (hv-bye c ob obj)
                (= op 1) (: nid (wl-u32 k off)
                            _ (pin ob nid (L 'ti))
                            _ (pin hb 15 (link (L c nid 0 0) (peep hb 15 ())))
                            (: fk (peep hb 8 ()) e (? fk (peep tl fk ()) ())
                               (? (? (two? e) (id? (cap e) c) 0)   ; focused here already? enter now
                                  (wl-ship c nid 0 (L (nthl e 3)) ())
                                  0)))
                0)
           (= kind 'ti)                                   ; enable/disable drive the im; commit answers done
             (? (= op 0) (: _ (pin hb 15 (filter (\ r (! (? (id? (cap r) c) (= (caup r) obj) 0)))
                                                 (peep hb 15 ())))
                            (hv-bye c ob obj))
                (= op 1) (: _ (hv-tiedit hb c obj (\ r (L c obj 1 (nthl r 3))))
                            act (hv-active hb tl)
                            (? (? (two? act) (= (caup act) obj) 0) (hv-im hb 0 ()) 0))
                (= op 2) (: was (hv-active hb tl)
                            _ (hv-tiedit hb c obj (\ r (L c obj 0 (nthl r 3))))
                            (? (? (two? was) (= (caup was) obj) 0) (hv-im hb 1 ()) 0))
                (= op 7) (: _ (hv-tiedit hb c obj (\ r (L c obj (nthl r 2) (+ 1 (nthl r 3)))))
                            r2 (afind (\ r (? (id? (cap r) c) (= (caup r) obj) 0)) (peep hb 15 ()))
                            (wl-ship c obj 5 (L (nthl r2 3)) ()))
                0)                                        ; 3..6: surrounding/cause/type/rect -- not held yet
           (= kind 'immgr)                                ; get_input_method(seat, id): one per harbor
             (? (= op 0) (: nid (wl-u32 k (+ off 4))
                            _ (pin ob nid (L 'im))
                            (? (two? (peep hb 16 ()))
                               (wl-ship c nid 6 () ())    ; a second input method: unavailable
                               (: _ (pin hb 16 (L c nid 0))
                                  (? (two? (hv-active hb tl)) (hv-im hb 0 ()) 0))))
                (= op 1) (hv-bye c ob obj)
                0)
           (= kind 'im)                                   ; the words pend; commit flushes to the active ti
             (? (= op 0) (pin hb 17 (wl-rstr k off))
                (= op 1) (pin hb 18 (L (wl-rstr k off)
                                       (wl-i32 k (wl-rskip k off))
                                       (wl-i32 k (+ 4 (wl-rskip k off)))))
                (= op 3) (: act (hv-active hb tl)
                            _ (? (two? act)
                                 (: tc (cap act) ti (caup act)
                                    pp (peep hb 18 ()) pc (peep hb 17 ())
                                    _ (? (two? pp)
                                         (wl-ship tc ti 2 (cat (wl-strw (cap pp)) (L (caup pp) (nthl pp 2))) ())
                                         0)
                                    _ (? pc (wl-ship tc ti 3 (wl-strw pc) ()) 0)
                                    (wl-ship tc ti 5 (L (nthl act 3)) ()))
                                 0)
                            _ (pin hb 17 ())
                            (pin hb 18 ()))
                (= op 5) (: gid (wl-u32 k off)            ; grab_keyboard: the keys leave the clients
                            _ (pin ob gid (L 'imgrab))
                            im (peep hb 16 ())
                            _ (pin hb 16 (L (cap im) (caup im) gid))
                            _ (hv-keymap hb c gid)        ; keymap: the same op-0 shape as wl_keyboard's
                            (wl-ship c gid 3 (L 25 400) ()))
                (= op 6) (: _ (pin hb 16 ()) _ (pin hb 17 ()) _ (pin hb 18 ())
                            (hv-bye c ob obj))
                0)
           (= kind 'imgrab)                               ; release: the keys pour to the clients again
             (? (= op 0) (: im (peep hb 16 ())
                            _ (? (? (two? im) (= obj (nthl im 2)) 0)
                                 (pin hb 16 (L (cap im) (caup im) 0))
                                 0)
                            (hv-bye c ob obj))
                0)
           (= kind 'compositor)                           ; create_surface / create_region
             (? (= op 0) (pin ob (wl-u32 k off) (L 'surface 0 0 ()))
                (= op 1) (pin ob (wl-u32 k off) (L 'region))
                0)
           (= kind 'region)
             (? (= op 0) (hv-bye c ob obj) 0)
           (= kind 'shm)                                  ; create_pool: the fd docks
             (? (= op 0) (: nid (wl-u32 k off) size (wl-i32 k (+ off 4))
                            fd (wl-popfd st)
                            rc (tablet 4)
                            _ (pin rc 0 0) _ (pin rc 1 0)
                            _ (pin rc 2 (? (< fd 0) 0 (mapfd fd size)))
                            _ (pin rc 3 size) _ (pin rc 4 fd)
                            _ (pin st 2 (link rc (peep st 2 ())))
                            (pin ob nid (L 'pool rc)))
                0)
           (= kind 'pool)                                 ; create_buffer / destroy / resize
             (: rc (nthl rec 1)
                (? (= op 0) (: _ (pin rc 0 (+ 1 (peep rc 0 0)))
                               (pin ob (wl-u32 k off)
                                    (L 'buffer rc (wl-i32 k (+ off 4)) (wl-i32 k (+ off 8))
                                       (wl-i32 k (+ off 12)) (wl-i32 k (+ off 16)))))
                   (= op 1) (: _ (? (< 0 (peep rc 0 0)) (pin rc 1 1) (hv-slip hb rc))   ; buffers still sail: part later
                               (hv-bye c ob obj))
                   (= op 2) (: ns (wl-i32 k off)          ; resize: remap -- the OLD mapping reaps in the
                               ;     painter's tick (fd -1: the pool's fd stays open)
                               _ (? (< 0 (peep rc 2 0))
                                    (pin hb 19 (link (L (peep rc 2 0) (peep rc 3 0) -1) (peep hb 19 ())))
                                    0)
                               _ (pin rc 2 (mapfd (peep rc 4 0) ns))
                               (pin rc 3 ns))
                   0))
           (= kind 'buffer)                               ; destroy: the last one slips a parting pool
             (? (= op 0) (: rc (nthl rec 1)
                            n (- (peep rc 0 0) 1)
                            _ (pin rc 0 n)
                            _ (? (? (< n 1) (< 0 (peep rc 1 0)) 0) (hv-slip hb rc) 0)
                            (hv-bye c ob obj))
                0)
           (= kind 'surface)                              ; (surface pending cbid key)
             (? (= op 0) (hv-bye c ob obj)
                (= op 1) (pin ob obj (L 'surface (wl-u32 k off) (nthl rec 2) (nthl rec 3)))
                (= op 3) (pin ob obj (L 'surface (nthl rec 1) (wl-u32 k off) (nthl rec 3)))
                (= op 6) (: pend (nthl rec 1) cb (nthl rec 2) key (nthl rec 3)
                            _ (? (< 0 pend)
                                 (: buf (peep ob pend ())
                                    _ (? (? (two? buf) key 0)
                                         (: _ (pin (peep hb 14 #()) key buf)     ; retain: the PAINTER composites it
                                            (pin hb 5 1))
                                         0)
                                    (wl-ship c pend 0 () ()))                    ; release
                                 0)
                            _ (? (< 0 cb)
                                 (: _ (wl-ship c cb 0 (L 0) ()) (wl-ship c 1 1 (L cb) ()))
                                 0)
                            (pin ob obj (L 'surface 0 0 key)))     ; commit CONSUMES: one release per attach
                0)
           (= kind 'wmbase)                               ; destroy / positioner / get_xdg_surface
             (? (= op 0) (hv-bye c ob obj)
                (= op 1) (pin ob (wl-u32 k off) (L 'positioner))
                (= op 2) (pin ob (wl-u32 k off) (L 'xdgsurf (wl-u32 k (+ off 4))))
                0)
           (= kind 'positioner)
             (? (= op 0) (hv-bye c ob obj) 0)
           (= kind 'xdgsurf)
             (? (= op 0) (hv-bye c ob obj)
                (= op 1) (: nid (wl-u32 k off)            ; get_toplevel: join the row
                            surfid (nthl rec 1)
                            key (peep hb 2 1)
                            _ (pin hb 2 (+ key 1))
                            srec (peep ob surfid ())
                            _ (pin ob surfid (L 'surface (nthl srec 1) (nthl srec 2) key))
                            _ (pin ob nid (L 'toplevel key))
                            _ (pin tl key (L c nid obj surfid ()))
                            _ (pin hb 0 (zins key (peep hb 0 ())))
                            (hv-retile hb tl x fb))
                0)
           (= kind 'toplevel)                             ; destroy: leave the row
             (? (= op 0) (: key (nthl rec 1)
                            e (peep tl key ())
                            _ (hv-bye c ob obj)
                            (? (two? e)
                               (: srec (peep ob (nthl e 3) ())
                                  _ (? (two? srec)      ; the surface stays, keyless
                                       (pin ob (nthl e 3) (L 'surface (nthl srec 1) (nthl srec 2) ()))
                                       0)
                                  _ (pin hb 0 (zdel key (peep hb 0 ())))
                                  _ (pin (peep hb 14 #()) key ())
                                  _ (pin tl key ())
                                  (hv-retile hb tl x fb))
                               0))
                0)
           0))

   ; --- one client, cradle to grave: park, pump, part ---
   (hv-serve c x fb hb tl)
     (: k (cask 65536) sc (cask 65536)
        st (tablet 4) _ (pin st 0 0) _ (pin st 1 ()) _ (pin st 2 ())
        ob (tablet 64)
        (on obj op off sz) (hv-on c st ob k x fb hb tl obj op off sz)
        (drain _) (: r (wl-pump c k sc st on) (? (< 0 r) (drain ()) r))
        (park _) (: _ (await c)
                    r (drain ())
                    (? (< r 0) (park ())
                       (: _ (hv-farewell c hb tl x fb)
                          _ (map (hv-slip hb) (peep st 2 ()))   ; every pool slips with its client
                          (close c))))
        (park ()))

   ; --- the harbor, backend-blind: x carries only the glass size here
   ;     (slots 4/5 -- the X entry threads its socket through the same
   ;     record); (ship fb) pours the composited glass to whatever shows
   ;     it; (attach hb tl) spins the backend's input tasks and pins the
   ;     key table (hb 12) + the sym->keycode map (hb 13) ---
   (hv-harbor path x fb ship attach)
     (: hb (tablet 16)
        _ (pin hb 0 ()) _ (pin hb 1 (mirror (choice (L (tall 1) full)))) _ (pin hb 2 1)
        _ (pin hb 3 (cask (* (nthl x 4) 4))) _ (pin hb 4 1) _ (pin hb 5 0)
        _ (pin hb 6 ()) _ (pin hb 7 ()) _ (pin hb 8 ()) _ (pin hb 9 ())
        _ (pin hb 11 (hv-loadkeymap ()))
        _ (pin hb 14 (tablet 16))                 ; the retained buffers: key -> last committed buf
        _ (pin hb 15 ()) _ (pin hb 16 ())         ; text-inputs; the input method
        _ (pin hb 17 ()) _ (pin hb 18 ())         ; the im's pending commit / preedit
        _ (pin hb 19 ())                          ; the reap list: mappings the painter unmaps
        tl (tablet 64)
        _ (pin haven-cell 'st (L hb tl x fb))
        _ (pin haven-cell 'path path)
        lp (shore path)
        (? (! (lit? lp)) 'no-shore
         (: ; the painter: the ONE task that blits, unmaps, and ships the
            ; glass post-boot. client tasks only retain buffers, queue
            ; slips, and raise the dirty flag; this reaps the slipped
            ; mappings, drains the moor's queue, composites every placed
            ; tile from its retained buffer, and ships on an 8ms clock
            ; (pier's paintloop discipline, made total: blit and unmap in
            ; ONE hand can never race -- a client task freeing a pool
            ; mid-composite was a segfault)
            (paint _) (: _ (hv-reap hb)
                         q (pull haven-cell 'q ())
                         _ (? (two? q) (hv-drain q hb tl x fb) 0)
                         _ (? (peep hb 5 0)
                              (: _ (pin hb 5 0)
                                 bufs (peep hb 14 #())
                                 _ (map (\ key (: e (peep tl key ()) b (peep bufs key ())
                                                  (? (? (two? e) (two? b) 0)
                                                     (hv-blit x fb b (nthl e 4))
                                                     0)))
                                        (zrow (peep hb 0 ())))
                                 (ship fb))
                              0)
                         _ (rest 8)
                         (paint ()))
            _ (twirl paint 0)
            _ (twirl (\ u (hv-moor (+ path ".moor"))) 0)
            _ (attach hb tl)
            (knock _) (: _ (await lp)
                         c (accept lp)
                         _ (? (lit? c) (twirl (\ _ (hv-serve c x fb hb tl)) 0) 0)
                         (knock ()))
            (knock ()))))

   ; --- the default chords, every action a control-plane verb (sup=64,
   ;     sup+shift=65). ks resolves a keysym to a keycode (each backend
   ;     brings its own resolver); a sym it lacks drops its row ---
   (hv-ktab ks)
     (: (bd mods sym f) (: kc (ks sym) (? (< 0 kc) (L (L mods kc f)) ()))
        (cat (bd 64 106 dn!)     (cat (bd 64 107 up!)
        (cat (bd 65 106 swapdn!) (cat (bd 65 107 swapup!)
        (cat (bd 64 104 shrink!) (cat (bd 64 108 grow!)
        (cat (bd 64 32 next!)    (cat (bd 65 113 part!)
        (bd 64 65293 (\ u (: t (getenv "HAVEN_TERM")
                             (? (< 0 (tally t)) (spawn t) 0)))))))))))))

   ; --- the X entry: an X window for the glass, a shore for the door ---
   (haven path dpy auth wpx hpx)
     (: s (x-connect dpy)
        (? (! (lit? s)) 'no-socket
         (: sc (x-setup s auth)
            (? (! (two? sc)) (: _ (close s) 'refused)
             (: win (scr-idbase sc) gc (+ 1 (scr-idbase sc)) depth (scr-depth sc)
                _ (px-mkwin s win (scr-root sc) 0 0 wpx hpx)
                _ (x-cwa-events s win 163919)             ; + KeyRelease, buttons, motion (the seat's senses)
                _ (px-gc s gc win)
                _ (px-title s win "haven")
                _ (x-map s win)
                _ (x-flush s)
                ; the boot keymap: letters + digits + space/return, sym -> keycode
                ; (hb 13 -- bind! resolves live rebinds off it)
                km (: (go a n acc) (? (< n 1) acc (go (+ a 1) (- n 1) (link a acc)))
                      (x-keymap s (scr-mink sc) (scr-maxk sc)
                                (cat (go 97 26 ()) (cat (go 48 10 ()) (L 32 65293)))))
                fb (cask (* wpx (* hpx 4)))
                x (L s win gc depth wpx hpx)
                (ship f) (px-ship s win gc depth f wpx 0 hpx)
                ; the listener: the OTHER pump on the X socket (ain's two-
                ; pump law -- the painter writes, this only reads). every
                ; key/button/motion on the glass becomes seat traffic;
                ; an expose just asks the painter to ship again
                (attach hb tl)
                  (: _ (pin hb 12 (hv-ktab (\ sym (peep km sym 0))))
                     _ (pin hb 13 km)
                     (readx _) (: e (x-next s)
                                  (? (! e) 0
                                     (: code (ev-code e) pkt (ev-raw e)
                                        _ (? (= code 12) (pin hb 5 1)
                                             (= code 2) (hv-keyev hb tl (u32 pkt 4) (ev-detail e) 1 (u16 pkt 28))
                                             (= code 3) (hv-keyev hb tl (u32 pkt 4) (ev-detail e) 0 (u16 pkt 28))
                                             (= code 6) (hv-ptrmove hb tl (u32 pkt 4) (u16 pkt 24) (u16 pkt 26))
                                             (= code 4) (hv-ptrbtn hb tl (u32 pkt 4) (ev-detail e) 1)
                                             (= code 5) (hv-ptrbtn hb tl (u32 pkt 4) (ev-detail e) 0)
                                             0)
                                        (readx ()))))
                     (twirl readx 0))
                r (hv-harbor path x fb ship attach)
                _ (close s)
                r))))))

; a welp-face help so a scare answers a visible value (to the moor client, or
; at a drain/serve raise site) instead of dropping the compositor -- the
; dock's shape: answer ('scare a b), the only bit a help sees.
(hear (\ s a b `('scare a b)))
