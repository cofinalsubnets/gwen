; --- test/host/limn.l: the laws of lib/limn.l, the cell painter ---
; NOT in the test/*.l corpus: `glass` and `unfold` are cb's nifs (crew/quay/nif.c)
; and love0 links none of them. it needs the LOADER too, the way its neighbour
; test/host/wharf.l does -- `use` finds lib/limn.l by the filesystem walk.
;
;   cat test/00-init.l test/host/limn.l | out/host/love   ; exits 1 on fail
;
; the vocabulary under test: a grid of cb cells in, escapes out. origins are
; 0-based going in and 1-based on the wire, which is the one conversion here.
(use 'limn)

; `sub?` is each gate's own here (test/host/berth.l carries the same three lines) --
; it SEARCHES from i; `at?` is the exact-position twin, which is what counting wants
(: (at? h n i) (&& ((i + tally n) <= tally h) (n = snip h i (i + tally n)))
   (sub? h n i) (? ((i + tally n) < (tally h + 1))
                   (? (at? h n i) 1 (sub? h n (i + 1)))
                   0)
   (mkscr r c) (screen (cask (screen () r c)) r c)
   (paint s r c row0 col0)                     ; limn one whole grid into a jug
    (: j (jug 0) _ (limn-rows j s r c row0 col0 0 -1) (slurp j))

; --- the address: 0-based in, 1-based out, row then col ---
   _ (assert
      ("\e[1;1H" = limn-go 0 0)
      ("\e[3;12H" = limn-go 2 11)
      ("\e[25;80H" = limn-go 24 79))

; --- utf-8 out, by range: the grid holds cp437, the terminal speaks unicode ---
   _ (assert
      ("A" = limn-utf8 65)
      (1 = tally (limn-utf8 127))                    ; one byte to the ascii edge
      (2 = tally (limn-utf8 233))                    ; é
      (3 = tally (limn-utf8 9474))                   ; │ the box glyph
      (limn-down = limn-utf8 9474)
      (limn-across = limn-utf8 9472))

; --- a glyph byte: ascii through, a high byte UNFOLDED, and the cache is
;     transparent -- asking twice answers the same text ---
   _ (assert
      ("x" = limn-glyph 120)
      (limn-glyph 201 = limn-glyph 201)               ; cached, and equal to itself
      (0 < tally (limn-glyph 201)))

; --- the pen: one SGR per change, respecified from zero so it is never stale ---
   _ (assert
      ("\e[0;38;5;7;48;5;0m" = limn-sgr (| 7 (<< 0 8)))
      ("\e[0;38;5;1;48;5;4m" = limn-sgr (| 1 (<< 4 8)))
      ; the faces ride the pen's high nibble: bold 1, under 2, reverse 4
      (sub? (limn-sgr (| 7 (<< 1 20))) ";1" 0)
      (sub? (limn-sgr (| 7 (<< 2 20))) ";4" 0)
      (sub? (limn-sgr (| 7 (<< 4 20))) ";7" 0))

; --- THE LAW THAT MATTERS: every row is addressed at its OWN origin, which is
;     the whole difference between painting a rectangle and painting full lines ---
   _ (: s (mkscr 3 4)
        _ (scribe s "ab")
        f (paint s 3 4 5 9)
        _ (assert
           (sub? f "\e[6;10H" 0)                      ; row 0 of the grid at (5,9)
           (sub? f "\e[7;10H" 0)                      ; row 1 at the SAME column
           (sub? f "\e[8;10H" 0)                      ; row 2
           (sub? f "ab" 0)                            ; and the glyphs came through
           !(sub? f "\e[1;1H" 0)))                    ; never the home cell

; --- a blank cell is a space, so a pane always writes every cell it owns and no
;     stale glyph can survive underneath it (lib/limn.l's no-erase bargain) ---
   _ (: s (mkscr 1 4)
        _ (scribe s "z")
        f (paint s 1 4 0 0)
        _ (assert (sub? f "z   " 0)))

; --- the pen CARRY: limn-rows answers the pen it ended on, and a run of cells
;     wearing one pen says its SGR ONCE ---
   _ (: s (mkscr 1 6)
        _ (scribe s "aaaaaa")
        j (jug 0)
        p (limn-rows j s 1 6 0 0 0 -1)
        f (slurp j)
        (cnt t sb i n) (? ((tally t) <= i) n
                          (at? t sb i) (cnt t sb (i + 1) (n + 1))
                          (cnt t sb (i + 1) n))
        _ (assert
           (0 <= p)                                   ; a pen came back out
           (1 = cnt f "\e[0;38;5;" 0 0)))             ; and exactly one SGR was said

; --- the bars, straight off wharf-bars' list: (dir row col len) ---
   _ (: j (jug 0)
        _ (limn-bars j '((across 4 0 3) (down 0 7 2)))
        f (slurp j)
        _ (assert
           (sub? f "\e[5;1H" 0)                       ; the across bar's origin
           (sub? f (limn-across + limn-across + limn-across) 0)
           (sub? f "\e[1;8H" 0)                       ; the down bar, row by row
           (sub? f "\e[2;8H" 0)))

; --- an empty bar list paints nothing, and a zero-row pane paints nothing ---
   _ (: j (jug 0) _ (limn-bars j ()) (assert ("" = slurp j)))
   _ (: s (mkscr 1 4) (assert ("" = paint s 0 4 0 0)))

   _ (test_fin ())
   _ (puts "limn: ok") _ (putc 10))
