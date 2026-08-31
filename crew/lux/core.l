; crew/lux/core.l -- the pure StackSet: the window model, no X and no sockets. This is
; xmonad's StackSet.hs, single-screen: a focus zipper per workspace, a sheaf of
; workspaces with one current, and a floating map (window -> rect) beside the
; tiled stacks. Pure -- so it rides the corpus on every target (crew/lux/law.l asserts
; the invariants), and the driver (crew/lux/lux.l) + the X layers build on it.
;
; tools/wm2uu.l reads this file as data and keys on the post-opfix tree --
; cap/cup chains (the glued accessors factor to them), ></+/!=, and the literal
; shape (cap (cup (cup z))). a rewrite that changes the tree, even value-equal,
; must teach the translator first or test/host/overlay.l's leg 6 stops certifying.
;
; a body-less top-level `:` -- every binding leaks to the global scope, so the
; other crew/lux/ files see this vocabulary once concatenated. names shadow no
; nif; the wm vocabulary is `z*` (the zipper) and `ws*`/admit/evict/shiftto/
; shiftwin (the sheaf).
(:
   ; --- the app's own list utilities ---
   (afind pred l) (? (atom? l) () (pred <l) <l (afind pred >l))    ; first element satisfying pred, else ()
   (rnext ring f d) (: n #ring                              ; the cyclic neighbor: d steps from f around ring
                       (idx i m) (? !(two? m) () (<m = f) i (idx (i + 1) >m))
                       i (idx 1 ring)                              ; 1-based (0 reads false), () when f is absent
                       (? (n = 0) ()
                          !i <ring                                 ; absent (or ()): start at the head
                          (ring ((i - 1 + d + n) % n))))

   ; --- the zipper: a workspace's tiled stack. () or (focus up down), up reversed
   ;     (nearest-first), so focus motion is O(1) at either end. ---
   (zmk f up dn) [f up dn]
   (zfoc z) (two? z ? <z)                                     ; the focused window (() if empty)
   (zrow z) (two? z ? (rev <>z + (<z >< <>>z)))               ; the stack as an ordered row, master first
   (zfrom l) (two? l ? (zmk <l () >l))                        ; a row -> a zipper, head focused
   (zrev z) (two? z ? (zmk <z <>>z <>z))                      ; mirror: up <-> down
   (zfocup z) (? !(two? z) ()                                      ; focus up, wrapping to the bottom
                 (: up <>z
                    (? (two? up) (zmk <up >up (<z >< <>>z))
                       (: all (rev (<z >< <>>z)) (zmk <all >all ())))))
   (zfocdn z) (zrev (zfocup (zrev z)))                             ; focus down = mirrored focus up
   (zswapup z) (? !(two? z) ()                                     ; swap the focused window upward
                  (: up <>z
                     (? (two? up) (zmk <z >up (<up >< <>>z))
                        (zmk <z (rev <>>z) ()))))                  ; at the top: rotate to the bottom
   (zswapdn z) (zrev (zswapup (zrev z)))
   (zmaster z) (!(two? z) ? () (zmk <z () (rev <>z + <>>z)))       ; focused becomes master
   (zins w z) (two? z ? (zmk w <>z (<z >< <>>z)) (zmk w () ()))  ; insert above the focus, focused
   (znub w l) (filter (x \ x != w) l)
   (zdel w z) (? !(two? z) ()                                      ; delete w; focus falls down, then up
                 (: f <z up (znub w <>z) dn (znub w <>>z)
                    (? (f != w) (zmk f up dn)
                       (two? dn) (zmk <dn up >dn)
                       (two? up) (zmk <up >up ())
                       ())))
   (zhas? w z) (any (x \ x = w) (zrow z))
   (zfromrow row f) (: (go up m) (? !(two? m) ()                   ; a row -> a zipper focused at f (() if absent)
                                    (<m = f) (zmk f up >m)
                                    (go (<m >< up) >m))
                       (go () row))
   (zfocto w z) (zhas? w z ? (zfromrow (zrow z) w) z)            ; focus the named window (Go dir's StackSet half)
   (zswapw a b z) (? (zhas? a z && zhas? b z)                      ; exchange a and b in place; focus follows its window
                     (zfromrow (map (x \ (? (x = a) b (x = b) a x)) (zrow z)) (zfoc z)) z)

   ; --- the sheaf: (cur board floating). board = ((tag stack)..), one tag current;
   ;     floating = an alist ((window rect)..), the un-tiled windows (rect = (x y w h)). ---
   (sheaf tags) [<tags (map (t \ [t ()]) tags) ()]
   (wscur sh) <sh
   (wsboard sh) <>sh
   (wsfloat sh) <>>sh
   (wsmake cur board float) [cur board float]
   (wsknown? sh t) (any (p \ <p = t) (wsboard sh))
   (wsget sh t) (: p (assoc t (wsboard sh)) (two? p ? <>p))   ; the tag's zipper
   (wsset sh t z) (wsmake (wscur sh) (map (p \ (<p = t ? [t z] p)) (wsboard sh)) (wsfloat sh))
   (wsview sh t) (wsknown? sh t ? (wsmake t (wsboard sh) (wsfloat sh)) sh)   ; switch current (only the tag turns)
   (wsrow sh) (zrow (wsget sh (wscur sh)))                         ; the current tiled row, ordered
   (wsfoc sh) (zfoc (wsget sh (wscur sh)))                         ; the focused window of the current ws
   (wsmap sh f) (wsset sh (wscur sh) (f (wsget sh (wscur sh))))    ; reshape the current zipper

   ; --- floating (xmonad's W.float / W.sink), a pure alist. An entry is
   ;     (window rect tag): a float lives on a workspace (its home tag), shown
   ;     only there -- xmonad's model, and what keeps a fullscreen float off
   ;     the other workspaces. ---
   (wsunfl w fl) (filter (p \ <p != w) fl)                       ; drop w from the floating alist
   (wsfloating? sh w) (any (p \ <p = w) (wsfloat sh))            ; w is floating?
   (wsrect sh w) (: p (assoc w (wsfloat sh)) (two? p ? <>p))  ; w's float rect, or ()
   (wsftag sh w) (: p (assoc w (wsfloat sh)) (two? p ? <>>p)) ; w's home tag, or ()
   (wsvisfl sh) (filter (p \ <>>p = wscur sh) (wsfloat sh))      ; the floats at home on the current tag
   (wsdel-tiled sh w) (map (p \ [<p (zdel w <>p)]) (wsboard sh))   ; drop w from every workspace's tiled stack
   (wsdofloat sh w r)                                              ; float: tiled -> floating; the home tag is where it
    (: t0 (wsftag sh w) t1 (t0 ? t0 (wstagof sh w)) t (t1 ? t1 (wscur sh))   ; already lives (re-float keeps it; a tiled window's
       (wsmake (wscur sh) (wsdel-tiled sh w) ([w r t] >< wsunfl w (wsfloat sh))))   ; tag; else the current view)
   (wssink sh w) (wsmap (wsmake (wscur sh) (wsboard sh) (wsunfl w (wsfloat sh))) (zins w))   ; sink: floating -> the current tiled stack

   ; --- managing windows across the whole sheaf ---
   (wstiled sh) (foldl (\ a p (a + zrow <>p)) () (wsboard sh))     ; every tiled window, all workspaces
   (wswins sh) (wstiled sh + map cap (wsfloat sh))                 ; every managed window (tiled + floating)
   (wsknownw? sh w) (any (x \ x = w) (wswins sh))
   (wsring sh) (wsrow sh + map cap (wsvisfl sh))                   ; the focus ring: the tiled row, then the floats at home
   (wstagof sh w) (: r (filter (p \ any (x \ x = w) (zrow <>p)) (wsboard sh))
                     (two? r ? <<r (wsftag sh w)))               ; the tag holding w (tiled, else a float's home), or ()
   (admit sh w) (wsknownw? sh w ? sh (wsmap sh (zins w)))        ; manage w into the current ws (a no-op if already managed)
   (evict sh w) (wsmake (wscur sh) (wsdel-tiled sh w) (wsunfl w (wsfloat sh)))   ; unmanage w everywhere (tiled or float)
   (shiftwin sh w t) (? !(wsknown? sh t && wsknownw? sh w && (t != wstagof sh w)) sh
                        (wsfloating? sh w)                         ; a float emigrates by retagging its home (rect kept)
                        (wsmake (wscur sh) (wsboard sh)
                                (map (p \ (<p = w ? [w <>p t] p)) (wsfloat sh)))
                        (: d (evict sh w) (wsset d t (zins w (wsget d t)))))   ; tiled: leave here, arrive focused on t
   (shiftto sh t) (shiftwin sh (wsfoc sh) t)                       ; the focused window emigrates to tag t; we stay
   (wssane? sh) (wsknown? sh (wscur sh) && znodup? (wswins sh))    ; the StackSet invariant
   (znodup? l) (? !(two? l) 1 (any (x \ x = <l) >l) 0 (znodup? >l)))
