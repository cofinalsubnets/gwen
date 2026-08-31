; crew/lux/sigs.l -- lux's overlay: an optional, observe-only type domain for
; the pure StackSet (crew/lux/core.l) + the layout geometry (crew/lux/layout.l).
; The vocabulary as dies: a window is not a tag is not a rect -- nominal kinds
; over values that are all innocent charms/lists at runtime, so the classic
; slip (a tag handed to zins, a window handed to wsview) is flagged at compile
; time and nothing changes at run time (the overlay law: erasable). Gradual:
; an unknown (a cap/cup harvest, a local) rides through; only a known kind in
; the wrong seat clashes. Install with (overlay-set lux-dom lux-sigs), or check
; a form pure with ((overlay 'ov-check1) lux-dom lux-sigs form) -- boot/overlay.l
; gates core.l + layout.l clean against this table, and the deliberate wound
; flagged. This is rung 1 of typing lux; the uu-proved StackSet laws ride
; on top of it (memory: the verified-xmonad ladder).
;
; a body-less `:` -- lux-dom/lux-sigs leak; neither shadows a nif.
(:
   (lux-join a b) (a = b ? a 'top)
   (lux-tr op a b) 'top
   (lux-lit e) 'top
   (lux-okr bd decl) (member? bd '(top bot) ? 1 (bd = decl))
   lux-dom (hash 'dice '(window tag rect zipper sheaf wrow trow fmap lay)
                'ctors ()
                'join lux-join 'tr lux-tr 'litdie lux-lit 'okr lux-okr)

   ; --- the boundaries: (name (param-dies..) result-die). wrow = a row of
   ;     windows, trow = a row of tags, fmap = the floating alist. `top` in a
   ;     result keeps the def gradual (predicates, raw board reads). ---
   lux-sigs '(
     ; the zipper (core.l)
     (zmk      (window wrow wrow)     zipper)
     (zfoc     (zipper)               window)
     (zrow     (zipper)               wrow)
     (zfrom    (wrow)                 zipper)
     (zrev     (zipper)               zipper)
     (zfocup   (zipper)               zipper)
     (zfocdn   (zipper)               zipper)
     (zswapup  (zipper)               zipper)
     (zswapdn  (zipper)               zipper)
     (zmaster  (zipper)               zipper)
     (zins     (window zipper)        zipper)
     (znub     (window wrow)          wrow)
     (zdel     (window zipper)        zipper)
     (zhas?    (window zipper)        top)
     (zfromrow (wrow window)          zipper)
     (zfocto   (window zipper)        zipper)
     (zswapw   (window window zipper) zipper)
     (znodup?  (wrow)                 top)
     ; the sheaf (core.l)
     (sheaf    (trow)                 sheaf)
     (wscur    (sheaf)                tag)
     (wsboard  (sheaf)                top)
     (wsfloat  (sheaf)                fmap)
     (wsmake   (tag a fmap)           sheaf)
     (wsknown? (sheaf tag)            top)
     (wsget    (sheaf tag)            zipper)
     (wsset    (sheaf tag zipper)     sheaf)
     (wsview   (sheaf tag)            sheaf)
     (wsrow    (sheaf)                wrow)
     (wsfoc    (sheaf)                window)
     (wsmap    (sheaf a)              sheaf)
     (wsunfl   (window fmap)          fmap)
     (wsfloating? (sheaf window)      top)
     (wsrect   (sheaf window)         rect)
     (wsdel-tiled (sheaf window)      top)
     (wsdofloat (sheaf window rect)   sheaf)
     (wssink   (sheaf window)         sheaf)
     (wstiled  (sheaf)                wrow)
     (wswins   (sheaf)                wrow)
     (wsknownw? (sheaf window)        top)
     (wstagof  (sheaf window)         tag)
     (admit    (sheaf window)         sheaf)
     (evict    (sheaf window)         sheaf)
     (shiftto  (sheaf tag)            sheaf)
     (shiftwin (sheaf window tag)     sheaf)
     (wsring   (sheaf)                wrow)
     (wssane?  (sheaf)                top)
     ; the geometry (layout.l)
     (rct      (a b c d)              rect)
     (rx       (rect)                 top)
     (ry       (rect)                 top)
     (rw       (rect)                 top)
     (rh       (rect)                 top)
     (splitv-eq (a rect)              top)
     (splitv-w  (a rect)              top)
     (splith-by (a rect)              top)
     (placeat  (wrow a)               top)
     (lay-tile (lay rect zipper)      top)
     (lay-msg  (lay a zipper)         top)
     (resizable-tall (a b c)          lay)
     (tall     (a)                    lay)
     (dwindle  (a)                    lay)))
