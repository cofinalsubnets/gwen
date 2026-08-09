; manifest -- the spreadsheet: a ship's manifest for values.
;
; every cell is a love expression, read repl-style (wrapped in parens, so
; infix goes bare: `a1 * 2`). a cell that names other cells compiles as a
; LAMBDA OVER ITS DEPENDENCIES -- b1 holding "a1 * 7" becomes (\ a1 (a1 * 7))
; applied to a1's worth -- computed on demand, memoized, a visiting set
; standing watch for cycles. an empty cell is (), the unit, so it rides
; through any arithmetic the way nothing should. a bare unknown name is its
; own spelling (labels for free); known globals stay free, so a cell may use
; the whole language -- and a cell may HOLD a function: g1 = "(+ 2)" and
; h1 = "(g1 40)" is 42. everything is a function, now with a cursor.
;
; the sheet is a little book: slot 0 the sources (nom -> string), slot 1 the
; worth cache (cleared whole on any edit -- the painter refills only what it
; shows), slot 2 the visiting watch.
;
; run: out/host/love -l crew/manifest/manifest.l -e '(manifest "crew/manifest/cargo.l")'
;      (a bare (manifest ()) sails without a file.)
; keys: arrows/hjkl move . enter edits the cell (= starts fresh) . x clears
;       w stows . q stows and steps ashore . ^C abandons ship
;
; the engine is pure core (ev/sound/tap); the tui rides raw/winsize like
; ink, so manifest never joins the corpus -- boot/manifest.l probes both,
; the tui end to end through a pty with the cb screen as the witness.

(:
 ; --- reading a cell: repl-style, one form, unreadable text is a label ---
 (glean s) (: r (sound (+ "(" (+ s ")")))
              (? (two? r) (: d <r (? (two? d) d ())) ()))

 ; --- the cell-name shape: one letter, then digits (a1 .. z999) ---
 (cell? n) (? (name? n)
             (: s (string n) t (tally s)
                (&& (< 1 t) (< t 5)
                    (<= 97 (s 0)) (<= (s 0) 122)
                    ((: (digs i) (? (< i t)
                                   (? ((<= 48 (s i)) && (<= (s i) 57)) (digs (+ i 1)) 0)
                                   1))
                     1)))
             0)

 ; --- the book of known names, laid once on first use: a name off this
 ; book and off the sheet is a LABEL and binds to its own spelling ---
 known #()
 (knowns _) (: _ (? !$known
                    (: _ (map (\ n (pin known n 1)) (names ()))
                       ; the READER-EMITTED noms ride formulas but sit OFF the book (the egg mops
                       ; them): @(..) reads as (tuple ..), backtick as (list ..), the printer round-
                       ; trips spread/mono. off `known` they'd be collected as deps and the formula
                       ; lambda would SHADOW the real constructor -- which only ever worked because
                       ; feel's fold resolved the head shadow-blind at compile (the glaze compiles
                       ; the honest lexical surface and applied the dep's spelling instead).
                       (map (\ n (pin known n 1)) '(tuple list spread mono)))
                    0)
                 known)
 (label? n) (&& (name? n) !(cell? n) !(peep (knowns ()) n 0)
                (: b ((string n) 0) ((<= 97 b) && (<= b 122))))

 ; --- dependencies: every cell-shaped or label name in the form, deduped,
 ; in first-sight order (quote interiors over-collect; binding is harmless
 ; there -- quoted data keeps its symbols) ---
 (walkdeps f seen acc) (? (two? f) (walkdeps >f seen (walkdeps <f seen acc))
                          ((? (cell? f) 1 (label? f)) && !(peep seen f 0))
                          (: _ (pin seen f 1) (+ acc (L f)))
                          acc)
 (deps f) (walkdeps f (tablet 0) ())

 ; --- the sheet ---
 (sheet _) #(0 (tablet 0) 1 (tablet 0) 2 (tablet 0) 3 0)   ; 3: the whirl count
 none (mint 0)     ; the private absent-marker (() is a real worth)
 whirl (mint 0)    ; the cycle's face: unforgeable, nothing, shown "cycle!"

 (cell! sh n s) (: _ (? (tally s) (pin (sh 0) n s) (pull (sh 0) n 0))
                   _ (sh <- 1 (tablet 0))
                   sh)
 (cell sh n) (peep (sh 0) n ())

 ; --- worth: demand-evaluate, memoize. the formula becomes a lambda over
 ; its deps and is applied to their worths (cells recurse; labels are their
 ; spelling), so nothing global is touched and no missing ever fires ---
 (worth1 sh s) (: f (glean s)
                  (? !(two? f) s                        ; unreadable: the text itself
                   (: ds (deps f)
                      lam (? ds (ev (+ (L (\ \)) (+ ds (L f)))) (ev f))
                      vs (map (\ d (? (cell? d) (worth sh d) (string d))) ds)
                      (? (foldl (\ a v (? (id? v whirl) 1 a)) 0 vs)
                         whirl                          ; a whirl dep taints (it would ride as the unit)
                         (foldl (\ g v (g v)) lam vs)))))
 ; a visiting-hit bumps the whirl count; any cell whose evaluation window saw
 ; a bump answers whirl itself, so the cycle wears its face at EVERY cell on
 ; the loop (a bare whirl is a mint -- the unit -- and would otherwise ride
 ; through the arithmetic and let a circle quietly converge).
 (worth sh n) (: v (peep (sh 1) n none)
                 (? !(id? v none) v
                  (peep (sh 2) n 0) (: _ (sh <- 3 (+ 1 (peep sh 3 0))) whirl)
                  (: s (peep (sh 0) n none)
                     (? (id? s none) ()                 ; an empty cell is the unit
                      (: _ (pin (sh 2) n 1)
                         c0 (peep sh 3 0)
                         v1 (worth1 sh s)
                         v2 (? (< c0 (peep sh 3 0)) whirl v1)
                         _ (pull (sh 2) n 0)
                         _ (pin (sh 1) n v2)
                         v2)))))

 ; --- stow / unstow: the sheet as one readable form ---
 (stow sh path) (: p (open path "w")
                   (? !(lit? p) ()
                    (: _ (say p "(manifest\n")
                       _ (map (\ n (: _ (say p " (")
                                      _ (say p (string n))
                                      _ (say p " ")
                                      _ (say p (show (peep (sh 0) n "")))
                                      (say p ")\n")))
                              (sort (keys (sh 0))))
                       _ (say p ")\n")
                       _ (flush p)
                       _ (close p)
                       1)))
 (unstow sh path) (: p (open path "r")
                     (? !(lit? p) sh
                      (: t (slurp p)
                         r (sound t)
                         d (? (two? r) <r ())
                         _ (close p)
                         _ (? (two? d && id? <d (\ manifest))
                              (map (\ pr (cell! sh <pr <>pr)) >d)
                              0)
                         sh)))

 ; --- faces: how a worth looks in its 9 columns ---
 (padl s w) (? (< (tally s) w) (padl (+ " " s) w) (snip s 0 w))
 (padr s w) (? (< (tally s) w) (padr (+ s " ") w) (snip s 0 w))
 (face v) (? (id? v whirl) "cycle!"
             (id? v ()) ""
             (string? v) v
             (show v))

 ; --- the tui ---
 (upto n f) ((: (go i) (? (< i n) (: _ (f i) (go (+ i 1))) ())) 0)
 (cnom cx cy) (intern (+ ("" + (+ 97 cx)) (show (+ cy 1))))
 (min2 a b) (? (< a b) a b)
 (max2 a b) (? (< a b) b a)

 ; one key, arrows folded to 1000+final ("\e[A".."\e[D"); a lone ESC is 27
 (key _) (: b (see in)
            (? (= b 27)
               (? (cue? 0)
                  (: b2 (see in)
                     (? (= b2 91) (+ 1000 (see in)) 27))
                  27)
               b))

 (drawcell j sh i cy sel) (: v (worth sh (cnom i cy))
                             t (face v)
                             t2 (? (constellation? v) (padl t 9) (padr t 9))
                             _ (? sel (say j "\e[7m") 0)
                             _ (say j t2)
                             _ (? sel (say j "\e[0m") 0)
                             (say j " "))

 (draw sh path r c cx cy top edit) (:
    nr (- r 3)
    nc (min2 26 (// (- c 5) 10))
    cur (cnom cx cy)
    j (jug 0)
    _ (say j "\e[?25l\e[H")
    ; the formula bar: cell name, then the edit line or the stowed source
    src (: s (cell sh cur) (? (id? s ()) "" s))
    bar (+ (string cur) (+ (? (string? edit) " = " ": ")
                           (? (string? edit) edit src)))
    _ (say j (padr bar (- c 1)))
    _ (say j "\e[K\r\n")
    ; the header row: column letters, the cursor's lit
    _ (say j "    ")
    _ (upto nc (\ i (: t (padl (+ ("" + (+ 97 i)) "    ") 7)
                       _ (? (= i cx) (say j "\e[7m") 0)
                       _ (say j (padr t 9))
                       _ (? (= i cx) (say j "\e[0m") 0)
                       (say j " "))))
    _ (say j "\e[K\r\n")
    ; the sheet
    _ (upto nr (\ k (: cy2 (+ top k)
                       _ (say j (padl (show (+ cy2 1)) 3))
                       _ (say j " ")
                       _ (upto nc (\ i (drawcell j sh i cy2 ((= i cx) && (= cy2 cy)))))
                       (say j "\e[K\r\n"))))
    ; the stern: keys, or the editing hint
    _ (say j (padr (? (string? edit)
                      "enter commits . esc steps back"
                      (+ (? (tally path) (+ path " . ") "")
                         "arrows move . enter edits . = fresh . x clears . w stows . q quits"))
                   (- c 1)))
    _ (say j "\e[J")
    ; editing: the terminal cursor rides the formula bar's end
    _ (? (string? edit)
         (say j (+ "\e[1;" (+ (show (+ (tally (string cur)) (+ 4 (tally edit)))) "H\e[?25h")))
         0)
    _ (say out (slurp j))
    (flush out))

 ; the watch: draw, take a key, act, come about
 (go sh path r c cx cy top edit) (:
    nr (- r 3)
    nc (min2 26 (// (- c 5) 10))
    top2 (? (< cy top) cy (>= cy (+ top nr)) (+ (- cy nr) 1) top)
    _ (draw sh path r c cx cy top2 edit)
    k (key ())
    (? (string? edit)
       (? (? (= k 13) 1 (= k 10)) (: _ (cell! sh (cnom cx cy) edit) (go sh path r c cx cy top2 ()))
          (= k 27) (go sh path r c cx cy top2 ())
          (? (= k 127) 1 (= k 8)) (go sh path r c cx cy top2
                                      (? (tally edit) (snip edit 0 (- (tally edit) 1)) edit))
          ((<= 32 k) && (< k 127)) (go sh path r c cx cy top2 (+ edit ("" + k)))
          (go sh path r c cx cy top2 edit))
       (? (? (= k 1065) 1 (= k 107)) (go sh path r c cx (max2 0 (- cy 1)) top2 ())       ; up
          (? (= k 1066) 1 (= k 106)) (go sh path r c cx (min2 98 (+ cy 1)) top2 ())      ; down
          (? (= k 1067) 1 (= k 108)) (go sh path r c (min2 (- nc 1) (+ cx 1)) cy top2 ()); right
          (? (= k 1068) 1 (= k 104)) (go sh path r c (max2 0 (- cx 1)) cy top2 ())       ; left
          (= k 13) (go sh path r c cx cy top2 (: s (cell sh (cnom cx cy)) (? (id? s ()) "" s)))
          (= k 61) (go sh path r c cx cy top2 "")
          (= k 120) (: _ (cell! sh (cnom cx cy) "") (go sh path r c cx cy top2 ()))
          (= k 119) (: _ (? (tally path) (stow sh path) 0) (go sh path r c cx cy top2 ()))
          (= k 113) (? (tally path) (stow sh path) ())                                   ; q: stow, step ashore
          (= k 3) ()                                                                     ; ^C: abandon ship
          (go sh path r c cx cy top2 ()))))

 (manifest path) (: sh (sheet 0)
                    _ (? (tally path) (unstow sh path) 0)
                    ws (winsize 0)                       ; a fresh pty says 0x0: fall back
                    r (? (two? ws && (< 5 <ws)) <ws 24)
                    c (? (two? ws && (< 19 >ws)) >ws 80)
                    _ (raw 1)
                    _ (say out "\e[?1049h\e[2J")
                    _ (flush out)
                    _ (go sh path r c 0 0 0 ())
                    _ (say out "\e[0m\e[?25h\e[?1049l")
                    _ (flush out)
                    _ (raw 0)
                    ()))
