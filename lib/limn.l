; lib/limn.l -- to limn is to paint: cb cells onto an outer terminal, as escapes.
; the third piece of the pane story and the one every seat needs -- cb says WHAT is
; in a grid (bytes in, cells out), lib/wharf.l says WHERE the rectangle is, and this
; says a grid's cells at an origin. cells are the currency all three speak.
; `(use 'limn)`.
;
; ⚠ IT TAKES A GRID AND AN ORIGIN, never a plan and never a harbour: the bars come in
; as wharf-bars' LIST, so nothing here knows what a berth or a buffer is. that is what
; lets one painter serve panes of ptys and splits of one process alike.
;
; ⚠ NOTHING IS ERASED, and nothing needs to be -- a caller whose panes and bars TILE
; its wharf (lib/wharf.l's standing law) writes every cell every frame, so no stale
; glyph can survive a layout change. a caller that leaves gaps owns them.
;
; host-only: `glass` and `unfold` are cb's nifs (crew/quay/nif.c), so this never joins
; the corpus -- test/host/limn.l is its gate.
(:
   ; --- the pen: a cell's high bytes ((>> cell 8): fg, bg, font+faces). one SGR per
   ; pen CHANGE, respecified from zero each time -- cheap, and never stale ----------
   (limn-sgr p) (: fg (& 255 p) bg (& 255 (>> p 8)) fa (& 15 (>> p 20))
                 (+ "\e[0"
                  (+ (? (& 1 fa) ";1" "")
                   (+ (? (& 2 fa) ";4" "")
                    (+ (? (& 4 fa) ";7" "")
                     (+ ";38;5;" (+ (show fg) (+ ";48;5;" (+ (show bg) "m")))))))))

   ; glyph bytes go OUT as utf-8: the grid holds cp437 (the fold), the outer terminal
   ; speaks unicode (the unfold), each face rendered once and cached
   limn-glyphs #()
   (limn-utf8 cp) (? (< cp 128) ("" + cp)
                     (< cp 2048) (+ ("" + (| 192 (>> cp 6))) ("" + (| 128 (& 63 cp))))
                     (+ ("" + (| 224 (>> cp 12)))
                        (+ ("" + (| 128 (& 63 (>> cp 6)))) ("" + (| 128 (& 63 cp))))))
   (limn-glyph b) (: g (peep limn-glyphs b ())
                     (? (string? g) g
                      (: cp (? ((<= 32 b) && (< b 127)) b (unfold b))
                         s (limn-utf8 (? (charm? cp && (< 0 cp)) cp 63))
                         _ (pin limn-glyphs b s)
                         s)))

   (limn-go row col) (+ "\e[" (+ (show (row + 1)) (+ ";" (+ (show (col + 1)) "H"))))
   limn-down (limn-utf8 9474)                 ; │
   limn-across (limn-utf8 9472)               ; ─

   ; paint row i cell by cell, threading the pen; a blank cell is a space
   (limn-row j s c i q p) (? (< q c)
                           (: g (glass s (+ (* i c) q))
                              p2 (>> g 8)
                              _ (? (= p p2) 0 (say j (limn-sgr p2)))
                              _ (say j (limn-glyph (: ch (& 255 g) (? (= 0 ch) 32 ch))))
                              (limn-row j s c i (+ q 1) p2))
                           p)

   ; one pane: every grid row addressed at its OWN origin, which is the whole
   ; difference between painting a rectangle and painting a run of full lines.
   ; p is the pen carried in and answered back, so a caller threads it across panes
   (limn-rows j s r c row0 col0 i p)
    (? (< i r)
       (: _ (say j (limn-go (row0 + i) col0))
          p2 (limn-row j s c i 0 p)
          (limn-rows j s r c row0 col0 (i + 1) p2))
       p)

   (limn-run j n s) (? (< 0 n) (: _ (say j s) (limn-run j (n - 1) s)) 0)

   ; the bars between panes, straight off wharf-bars: (dir row col n), 'across or 'down
   (limn-bars j bs)
    (map (\ b (: d <b row <>b col <>>b n <>>>b
                 (? (= d 'across) (: _ (say j (limn-go row col)) (limn-run j n limn-across))
                  (: (down i) (? (< i n)
                                 (: _ (say j (limn-go (row + i) col)) _ (say j limn-down)
                                    (down (i + 1)))
                                 0)
                     (down 0)))))
         bs))
