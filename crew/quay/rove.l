; rove -- quay's roguelike engine: delving below decks.
;
; a DECK is one floor of the hold: tiles in a cask (0 rock, 1 floor, 2 the
; hatch down), carved by delve -- rooms lashed by L-corridors, the functional
; rng threaded through so a seed reproduces the deck -- with a seen cask
; remembering what the lantern has shown. fov is line-of-sight over bresenham
; strides (walls light up as line endpoints); the painter says one frame of
; painted cells into any port, so frames gate headless; the helm decodes raw
; keys (hjkl + yubn + arrows). the crawl is the proving game riding it all:
; rats, crabs and morays below decks, grog and gold, the pearl three decks
; down -- pure state in, pure state out, one key per turn.
;
; run: out/host/love -l crew/quay/rove.l -e '(rove ())'
; the engine core is pure love (delve/fov/paint/crawl-* run headless, gated by
; test/host/rove.l); only rove itself ties the tty (raw/winsize/cue? ride
; host/posix.c), so rove never joins the corpus.

(:
 ; --- the deck: tiles in a cask, (x y) indexed; 0 rock, 1 floor, 2 the hatch ---
 (dat d x y) (0 <= x < d 'w && 0 <= y < d 'h ? (peep (d 't) (x + (d 'w) * y) 0) 0)
 (open? d x y) (0 < dat d x y)
 (dput d x y v) (pin (d 't) (x + (d 'w) * y) v)

 ; a spun rng: the functional state in a box, one draw steps it
 (roll sb n) (: p (random (peep sb () 0) n) (pin sb () >p, <p))

 ; --- delve: carve rooms, lash them with corridors ---
 ; a room is [x y rw rh]; its center anchors the corridors. rooms keep a
 ; one-cell gap so no two merge; each new room tunnels to the previous one,
 ; so the whole deck is connected by construction. answers (deck . st').
 (delve w h st) (:
   d {'w w 'h h 't (cask (w * h)) 'seen (cask (w * h)) 'rooms ()}
   sb {st}
   (mid rm) [(peep rm 0 0 + peep rm 2 1 // 2) (peep rm 1 0 + peep rm 3 1 // 2)]
   (carve x0 y0 rw rh) (: (row y x) (x < x0 + rw ? (dput d x y 1, row y (x + 1)))
                          (rows y) (y < y0 + rh ? (row y x0, rows (y + 1)))
                          (rows y0))
   (hallx y xa xb) (carve (xa < xb ? xa xb) y (1 + |(xb - xa)) 1)
   (hally x ya yb) (carve x (ya < yb ? ya yb) 1 (1 + |(yb - ya)))
   (lash [ax ay] [bx by])
    (roll sb 2 ? (hallx ay ax bx, hally bx ay by)
                 (hally ax ay by, hallx by ax bx))
   (apart? rm) (all (o \ 1 + peep rm 0 0 + peep rm 2 1 <= peep o 0 0 ||
                         1 + peep o 0 0 + peep o 2 1 <= peep rm 0 0 ||
                         1 + peep rm 1 0 + peep rm 3 1 <= peep o 1 0 ||
                         1 + peep o 1 0 + peep o 3 1 <= peep rm 1 0))
   (try n) (? (0 < n)
            (: rw (4 + roll sb 7) rh (3 + roll sb 4)
               (? (w <= rw + 2 || h <= rh + 2) (try (n - 1))
                ; the span a room of width rw can start at, keeping a 1-cell border: x is
                ; 1 + roll(w - rw - 1), so the far edge lands at w-2 at worst. (this read `+ 1`
                ; while the additive band folded RIGHT and w - (rw + 1) is the same number --
                ; the hand made the arithmetic say what it means.)
                (: x (1 + roll sb (w - rw - 1)) y (1 + roll sb (h - rh - 1))
                   rm [x y rw rh]
                 (? !(apart? rm (d 'rooms)) (try (n - 1))
                  (carve x y rw rh,
                   d 'rooms ? (lash (mid rm) (mid <(d 'rooms))),
                   pin d 'rooms (rm >< d 'rooms),
                   try (n - 1)))))))
   _ (try ((w * h) // 24))
   ; a cramped deck can come up short: guarantee two moored rooms
   _ (tally (d 'rooms) < 2 ?
      (: r1 '(1 1 3 3) r2 [(w - 4) (h - 4) 3 3]
         (carve 1 1 3 3, carve (w - 4) (h - 4) 3 3,
          lash (mid r1) (mid r2), pin d 'rooms [r2 r1])))
   _ (pins d 'in (mid (last (d 'rooms)))
             'down (mid <(d 'rooms)))
   _ (dput d <(d 'down) <>(d 'down) 2)
   (d >< peep sb () 0))

 ; --- the lantern: line-of-sight fov over bresenham strides ---
 ; a stride is clear when every cell strictly between the ends is open; the
 ; endpoint itself may be rock -- that is how a wall catches the light.
 (clear? d x0 y0 x1 y1) (:
   dx |(x1 - x0) dy (0 - |(y1 - y0))
   sx (? (x0 < x1) 1 -1) sy (? (y0 < y1) 1 -1)
   (go x y e) (? ((x = x1) && (y = y1)) 1
               (: e2 (2 * e)
                  mx (dy <= e2) my (e2 <= dx)
                  nx (? mx (x + sx) x) ny (? my (y + sy) y)
                  ne (e + (mx ? dy) + (my ? dx))
                  (? (nx = x1 && ny = y1) 1
                     (open? d nx ny) (go nx ny ne)
                     0)))
   (go x0 y0 (dx + dy)))

 ; the visible set: a tablet of cell indices, radius-bounded and stride-clear;
 ; each visible cell is also pinned into the deck's seen cask (the memory).
 ; a closing pass glows the rock beside every lit floor cell, so corridors
 ; wear their walls (a bare stride only catches rock end-on)
 (fov d px py r) (:
   vis (tablet 0) w (d 'w) h (d 'h) sn (d 'seen) r2 (1 + r * r)
   (glow x y) (: i (x + w * y) (pin vis i 1, pin sn i 1, ()))
   (cell x y) (2 (x - px) + 2 (y - py) <= r2 && clear? d px py x y ? (glow x y))
   (row y x) (x <= px + r ? (cell x y, row y (x + 1)))
   (rows y) (y <= py + r ? (row y (px - r), rows (y + 1)))
   _ (rows (py - r))
   _ (map (i \ 0 < peep (d 't) i 0 ?
           (: x (i % w) y (i // w)
            (map (\ o (: nx (x + <o) ny (y + <>o)
                         (0 <= nx < w && 0 <= ny < h &&
                          dat d nx ny = 0 ? (glow nx ny))))
                 '((-1 -1) (0 -1) (1 -1) (-1 0)
                   (1 0) (-1 1) (0 1) (1 1)))))
          (keys vis))
   vis)

 ; --- the painter: one frame said into a port as painted cells ---
 ; glyph/hue per cell -- an ent on a lit cell, else loot, else the tile: lit
 ; tiles bright, remembered ones dim, the dark a bare space. one SGR per hue
 ; CHANGE, threaded down the frame; the port is any jug, so frames gate headless.
 pens {}
 (pen n) (: s (peep pens n ())
            (string? s ? s
             (: s2 ("\e[38;5;" + show n + "m") (pin pens n s2, s2))))
 tile-gl "#.>"
 (tile-hue t lit) (? lit (t @ 2 51 1 242 250)
                         (t @ 2 24 1 236 240))
 (loot-face it) (<it @ 'gold '(36 220)
                       'grog '(33 213)
                       '(42 159))                       ; the pearl: *
 (paint j d vis loot ents msg bar) (:
   w (d 'w) h (d 'h) sn (d 'seen)
   spot (foldl (\ t e (pin t (e 'x + w * e 'y) e)) {} ents)
   _ (say j $ "\e[H\e[0m" + (w < tally msg ? (snip msg 0 w) msg) + "\e[K\r\n")
   (cell i lit) (: e (lit ? (peep spot i ()))
                   it (lit ? (peep loot i ()))
                   (? e [(e 'gl) (e 'hue)]
                      it (loot-face it)
                      (peep sn i 0)
                       (: t (peep (d 't) i 0) [(tile-gl t) (tile-hue t lit)])
                      '(32 0)))
   (row y x i p) (x >= w ? p
                  (: gh (cell i (peep vis i 0))
                     p2 <>gh
                   (p != p2 ? (say j (pen p2)),
                    say j (string <gh),
                    row y (x + 1) (i + 1) p2)))
   (rows y p) (y >= h ? p (rows (y + 1) (: p2 (row y 0 (w * y) p) _ (say j "\r\n") p2)))
   (rows 0 -1,
    say j "\e[0m", say j bar, say j "\e[K",
    j))

 ; --- the helm: raw key bytes -> one steer ---
 ; the pure decoder maps a byte (and the two that may trail an escape) to a
 ; steer: a move pair, a plain charm, or a word; helm reads the tty for rove.
 (helm-dec a b c) ; h j k l y u b n
  (a @ 104 '(-1 0)  106 '(0 1)  107 '(0 -1) 108 '(1 0)
       121 '(-1 -1) 117 '(1 -1) 98  '(-1 1) 110 '(1 1) -1 'quit
       27 (b @ 91 (c @ 65 '(0 -1) 66 '(0 1) 67 '(1 0) 68 '(-1 0) 'esc) 'esc)
       a)
 (helm _) (: a (see in)
             (? (a = 27) (: _ (rest 1)                        ; let the tail of a sequence land
                 (? (cue? 0) (: b (see in) (? (cue? 0) (helm-dec a b (see in)) (helm-dec a b -1)))
                  'esc))
              (helm-dec a -1 -1)))

 ; --- the crawl: the proving game. state in a tablet, one key per turn ---
 ; the hold's fauna: a kind is (glyph hue hp bite), deeper decks bare more teeth
 (mob-name k) (peep '("rat" "crab" "moray") k "rat")
 (mob-row k) (peep '((114 137 3 1) (99 173 5 2) (109 71 9 3)) k '(114 137 3 1))
 (mob-kinds lvl) (lvl @ 1 '(0 0 0 1) 2 '(0 1 1 2) '(1 2 2 2))
 (mob-new k x y) (: r (mob-row k)
                   {'x x 'y y 'gl (peep r 0 114) 'hue (peep r 1 137)
                    'hp (peep r 2 3) 'dm (peep r 3 1) 'nm (mob-name k)})

 ; stock a fresh deck: mobs and loot in every room but the player's own;
 ; the last deck trades its hatch for the pearl
 last-deck 3
 (stock g) (:
   d (g 'd) sb (g 'sb) lvl (g 'lvl) loot (tablet 0)
   (drop-at rm) [(peep rm 0 0 + roll sb (peep rm 2 1)) (peep rm 1 0 + roll sb (peep rm 3 1))]
   (mobs rm n acc) (? (0 < n)
                    (: xy (drop-at rm) x <xy y <>xy
                       (? (&& (dat d x y = 1)
                              !(peep loot (x + d 'w * y) ())
                              !(any (o \ x = o 'x && y = o 'y) acc))
                        (mobs rm (n - 1) (mob-new (peep (mob-kinds lvl) (roll sb 4) 0) x y >< acc))
                        (mobs rm (n - 1) acc)))
                    acc)
   (treasure rm) (: xy (drop-at rm) i (<xy + (d 'w) * <>xy)
                    (? (peep (d 't) i 0 != 1) ()
                       (roll sb 10 < 5) (pin loot i ['gold (5 + roll sb 16)])
                       (roll sb 10 < 3) (pin loot i '(grog 0))))
   ms (foldl (\ acc rm (treasure rm, mobs rm (roll sb (2 + lvl)) acc))
             () (init (d 'rooms)))
   _ (lvl = last-deck ?
      (: dn (d 'down)
       (dput d <dn <>dn 1,
        pin loot (<dn + (d 'w) * <>dn) '(pearl 0))))
   (pins g 'ms ms 'loot loot))

 ; a fresh game: deck one delved from the seed state, the player moored at 'in
 (crawl-new w h st) (:
   p0 (delve w h st) d <p0
   st1 >p0 sb {st1}                                 ; the brace boxes; #st1 would count it
   pl {'x <(d 'in) 'y <>(d 'in) 'gl 64 'hue 228 'hp 12 'mx 12 'gold 0}
   g {'d d 'lvl 1 'sb sb 'p pl 'msg "" 'over 0}
   _ (stock g)
   (pin g 'vis (fov d (pl 'x) (pl 'y) 8)))

 (note g s) (pin g 'msg (g 'msg ? (g 'msg + "; " + s) s))
 (mob-at g x y) <(filter (m \ x = m 'x && y = m 'y) (g 'ms))
 (blocked? g x y) (!(open? (g 'd) x y) ||
                   (x = g 'p 'x && y = g 'p 'y) ||
                   mob-at g x y)

 ; the player's stride: bump a mob to strike it, step onto loot to take it
 (strike g m) (: dm (1 + roll (g 'sb) 3)
                 _ (pin m 'hp (m 'hp - dm))
                 (? (0 < m 'hp) (note g $ "you strike the " + m 'nm + " (" + show dm + ")")
                  (pin g 'ms (filter (\ o !(id? o m)) (g 'ms)),
                   note g ("the " + m 'nm + " is done for"))))
 (take g i it)
  (pull (g 'loot) i (),
   <it @ 'gold (pin (g 'p) 'gold (g 'p 'gold + <>it),
                note g ("you pocket " + show <>it + " gold"))
         'grog (: mend (g 'p 'hp + 5 < g 'p 'mx ? 5 (g 'p 'mx - g 'p 'hp))
                (pin (g 'p) 'hp (g 'p 'hp + mend),
                 note g ("you swig the grog (+" + show mend + ")")))
         (pin g 'over 'won,
          note g "the pearl! yours, and the long climb home"))

 (stride g mv) (: p (g 'p) nx (p 'x + <mv) ny (p 'y + <>mv)
                  m (mob-at g nx ny)
                  (? m (strike g m)
                     !(open? (g 'd) nx ny) (note g "the hull holds")
                     (: _ (pin p 'x nx, pin p 'y ny)
                        i (nx + (g 'd 'w) * ny)
                        it (peep (g 'loot) i ())
                        (it ? (take g i it)))))

 ; the hold stirs: a mob that sees you closes in; struck close, it bites
 (sgn n) (? (0 < n) 1 (n < 0) -1 0)
 (bite g m) (: dm (1 + roll (g 'sb) (m 'dm))
               _ (pin (g 'p) 'hp (g 'p 'hp - dm))
               _ (note g ("the " + m 'nm + " bites you (" + show dm + ")"))
               (0 >= g 'p 'hp ? (pin g 'over 'dead, note g "you are done for")))
 (shift g m mv) (: nx (m 'x + <mv) ny (m 'y + <>mv)
                 (blocked? g nx ny ? () (pin m 'x nx, pin m 'y ny)))
 (stir g m) (: p (g 'p) dx ((p 'x) - m 'x) dy ((p 'y) - m 'y)
               (? (|dx <= 1 && |dy <= 1) (bite g m)
                  (dx * dx + dy * dy <= 65 &&
                   clear? (g 'd) (m 'x) (m 'y) (p 'x) (p 'y))
                   (: sx (sgn dx) sy (sgn dy)
                      ox (m 'x) oy (m 'y)
                      _ (shift g m [sx sy])
                      (ox = m 'x && oy = m 'y ?
                       (shift g m [sx 0],
                        ox = m 'x && oy = m 'y ? (shift g m [0 sy]))))
                  (roll (g 'sb) 3 < 1)
                   (shift g m [(roll (g 'sb) 3 - 1) (roll (g 'sb) 3 - 1)])))

 ; down the hatch: a fresh deck, the crew and cargo restocked
 (descend g) (: p0 (delve (g 'd 'w) (g 'd 'h) (peep (g 'sb) () 0))
                d <p0
                _ (pin (g 'sb) () >p0)
                _ (pins g 'd d 'lvl (1 + g 'lvl))
                _ (pins (g 'p) 'x <(d 'in) 'y <>(d 'in))
                _ (stock g)
                (note g ("you climb down to deck " + show (g 'lvl))))

 ; one turn: the player's steer, then every mob stirs, then the lantern
 (crawl-turn g k) (:
   _ (pin g 'msg "")
   _ (? (g 'over || k = 46) ()
      (two? k) (stride g k)
      (k = 62) (dat (g 'd) (g 'p 'x) (g 'p 'y) @ 2 (descend g) (note g "no hatch here")))
   _ (!(g 'over) ? (each (g 'ms) (m \ !(g 'over) ? (stir g m))))
   (pin g 'vis (fov (g 'd) (g 'p 'x) (g 'p 'y) 8)))

 (crawl-bar g) ("deck " + show (g 'lvl) + "/" + show last-deck
                + "  hp " + show (g 'p 'hp) + "/" + show (g 'p 'mx)
                + "  gold " + show (g 'p 'gold))
 (crawl-frame g) (: j (paint (jug 0) (g 'd) (g 'vis) (g 'loot)
                            (g 'p >< g 'ms) (g 'msg) (crawl-bar g))
                    (slurp j))

 ; --- rove: the crawl on the tty. any seed sails; q or esc steps ashore ---
 (rove _) (: ws (winsize 0)
             r (two? ws && 7 < <ws ? (<ws - 2) 22)
             c (two? ws && 19 < <>ws ? <>ws 80)
             g (crawl-new (c < 100 ? c 100) r (wheel (clock 0)))
             _ (raw 1,
                say out "\e[?1049h\e[?25l\e[2J",
                note g "the hold is dark; the pearl waits three decks down")
             (sail _) (: _ (say out (crawl-frame g))
                         (? (g 'over) (id? (g 'over) 'fled ? () (rest 1200))
                          (: k (helm ())
                             (? (k = 113 || id? k 'esc || id? k 'quit)
                              (pin g 'over 'fled, ())
                              (crawl-turn g k, sail ())))))
             (sail (),
              say out "\e[0m\e[?25h\e[?1049l",
              flush out,
              raw 0,
              puts $ g 'over @ 'won  ("the pearl is yours -- and " + show (g 'p 'gold) + " gold besides")
                               'dead ("the hold keeps you, deck " + show (g 'lvl))
                                     ("ashore with " + show (g 'p 'gold) + " gold")),
             (g 'p 'gold)))
