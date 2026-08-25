; --- test/host/rove.l: the rove engine, headless ---
; NOT in the test/*.l corpus (it loads a crew file and leaks its helpers).
; the engine core is pure love, so everything but the tty runner gates here:
; delve (seeded, connected, bordered), fov (strides, walls, memory), the
; painter (a frame into a jug), the helm decoder, and the crawl end to end
; -- combat, loot, grog, the hatch, the pearl, death. the tty side (rove)
; stays a live probe: it is berth-thin over what this file proves.
;
;   cat test/00-init.l test/host/rove.l | out/host/love

(reads (open "crew/quay/rove.l" "r"))

(: (find h n i) (? (i + tally n < 1 + tally h)
                  (? (n = snip h i (i + tally n)) 1 (find h n (i + 1)))
                  0))

; --- delve: a seed reproduces its deck; decks are connected and bordered ---
(: p1 (delve 60 20 (wheel 11))
   p2 (delve 60 20 (wheel 11))
   p3 (delve 60 20 (wheel 12))
   d <p1
   (teq a b) (: (go i ok) (? (i < 1200) (go (i + 1) (&& ok (peep (a 't) i 0 = peep (b 't) i 0)))
                           ok)
                (go 0 1))
   flood (: mk (cask 1200)
            (go stk n) (? !stk n
                        (: xy <stk x <xy y <>xy i (x + 60 * y)
                           (? (|| !(open? d x y) (peep mk i 0)) (go >stk n)
                            (: _ (pin mk i 1)
                               (go (cat [[(x + 1) y] [(x - 1) y] [x (y + 1)] [x (y - 1)]] >stk)
                                   (n + 1))))))
            (go [(d 'in)] 0))
   floors (: (go i n) (? (i < 1200) (go (i + 1) (n + (? (0 < peep (d 't) i 0) 1 0))) n)
             (go 0 0))
   rim (: (go i bad) (? (i < 60)
                       (go (i + 1) (|| bad (dat d i 0) (dat d i 19)))
                       (: (gov j b2) (? (j < 20) (gov (j + 1) (|| b2 (dat d 0 j) (dat d 59 j))) b2)
                          (gov 0 bad)))
          (go 0 0))
 (assert
  (teq <p1 <p2)                                  ; one seed, one deck
  !(teq <p1 <p3)                                 ; another seed, another deck
  (1 < tally (d 'rooms))
  (1 = dat d <(d 'in) <>(d 'in))                 ; you moor on open floor
  (2 = dat d <(d 'down) <>(d 'down))             ; the hatch is cut
  (flood = floors)                               ; every floor cell reachable
  !rim))                                         ; the hull is never breached

; --- fov: strides light what they reach, rock blocks, the seen cask remembers ---
; a hand deck, two cabins split by a bulkhead at x=4:
;   #########
;   #...#...#
;   #...#...#
;   #...#...#
;   #########
(: hd {'w 9 'h 5 't (cask 45) 'seen (cask 45) 'rooms [[1 1 3 3] [5 1 3 3]]}
   _ (map (\ y (map (\ x (? (x = 4) () (dput hd x y 1))) '(1 2 3 4 5 6 7))) '(1 2 3))
   v (fov hd 1 1 8)
 (assert
  (peep v (1 + 9 * 1) 0)                         ; your own cell is lit
  (peep v (3 + 9 * 1) 0)                         ; down the cabin
  (peep v (4 + 9 * 1) 0)                         ; the bulkhead catches the light
  !(peep v (5 + 9 * 1) 0)                        ; ... and keeps the far cabin dark
  !(peep v (7 + 9 * 3) 0)
  (peep v 0 0)                                   ; the corner wall glows (adjacency)
  (peep (hd 'seen) (3 + 9 * 1) 0)                ; the lantern leaves a memory
  !(peep (hd 'seen) (5 + 9 * 1) 0))
 ; step across: the far cabin lights, the seen cask keeps both
 (: v2 (fov hd 5 2 8)
  (assert
   (peep v2 (5 + 9 * 1) 0)
   !(peep v2 (1 + 9 * 1) 0)
   (peep (hd 'seen) (1 + 9 * 1) 0)
   (peep (hd 'seen) (6 + 9 * 2) 0))))

; --- the helm decoder: arrows, the sailor's keys, the words ---
(assert
 ('(0 -1) = helm-dec 27 91 65) ('(0 1) = helm-dec 27 91 66)
 ('(1 0) = helm-dec 27 91 67) ('(-1 0) = helm-dec 27 91 68)
 (id? 'esc (helm-dec 27 -1 -1))
 ('(-1 0) = helm-dec 104 -1 -1) ('(0 1) = helm-dec 106 -1 -1)
 ('(0 -1) = helm-dec 107 -1 -1) ('(1 0) = helm-dec 108 -1 -1)
 ('(-1 -1) = helm-dec 121 -1 -1) ('(1 -1) = helm-dec 117 -1 -1)
 ('(-1 1) = helm-dec 98 -1 -1) ('(1 1) = helm-dec 110 -1 -1)
 (id? 'quit (helm-dec -1 -1 -1))
 (113 = helm-dec 113 -1 -1))

; --- the crawl on a hand deck: every verb of the game, deterministic ---
(: (hand-g _) (: hd {'w 9 'h 5 't (cask 45) 'seen (cask 45) 'rooms [[1 1 3 3] [5 1 3 3]]}
                 _ (map (\ y (map (\ x (? (x = 4) () (dput hd x y 1))) '(1 2 3 4 5 6 7))) '(1 2 3))
                 _ (pin hd 'in [1 1], pin hd 'down [7 3])
                 st (wheel 5) sb {st}
                 pl {'x 1 'y 1 'gl 64 'hue 228 'hp 12 'mx 12 'gold 0}
                 {'d hd 'lvl 1 'sb sb 'p pl 'ms () 'loot (tablet 0) 'msg "" 'over 0}))

; the hull holds: a stride into rock stands you still
(: g (hand-g ())
   _ (crawl-turn g [0 -1])
 (assert (1 = g 'p 'x) (1 = g 'p 'y) (find (g 'msg) "the hull holds" 0)))

; gold weighs in, grog mends, and a full flask tops out at the brim
(: g (hand-g ())
   _ (pin (g 'loot) (2 + 9 * 1) ['gold 7], pin (g 'loot) (3 + 9 * 1) ['grog 0])
   _ (pin (g 'p) 'hp 5)
   _ (crawl-turn g [1 0])
   _ (assert (7 = g 'p 'gold) (find (g 'msg) "pocket" 0) (1 = tally (keys (g 'loot))))
   _ (crawl-turn g [1 0])
 (assert (10 = g 'p 'hp) (find (g 'msg) "grog" 0) !(keys (g 'loot))))

; bump combat: the rat goes down inside its hit points
(: g (hand-g ())
   m (mob-new 0 2 1)
   _ (pin g 'ms [m])
   (fight i) (? (&& (i < 8) (g 'ms)) (crawl-turn g [1 0], fight (i + 1)) i)
   n (fight 0)
 (assert !(g 'ms) (n <= 4) (find (g 'msg) "done for" 0)
         (1 = g 'p 'x)))                         ; you strike in place, never through

; the bite: a cornered sailor with one hp left is done for
(: g (hand-g ())
   _ (pin g 'ms [(mob-new 0 2 1)], pin (g 'p) 'hp 1)
   _ (crawl-turn g 46)
 (assert (id? 'dead (g 'over)) (find (g 'msg) "done for" 0)))

; the hatch: > climbs down only where the hatch is cut, and restocks the deck
(: g (hand-g ())
   _ (crawl-turn g 62)
   _ (assert (1 = g 'lvl) (find (g 'msg) "no hatch" 0))
   _ (dput (g 'd) 1 1 2)
   _ (crawl-turn g 62)
 (assert (2 = g 'lvl) (find (g 'msg) "deck 2" 0)
         (1 = dat (g 'd) (g 'p 'x) (g 'p 'y))    ; moored on the fresh deck's floor
         (tablet? (g 'loot))))

; the pearl ends the crawl
(: g (hand-g ())
   _ (pin (g 'loot) (2 + 9 * 1) ['pearl 0])
   _ (crawl-turn g [1 0])
 (assert (id? 'won (g 'over)) (find (g 'msg) "pearl" 0)))

; --- the whole game, generated: a seeded random walk holds every invariant ---
(: g (crawl-new 60 20 (wheel 3))
   kb {wheel 99}
   dirs [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1] 46 62]
   (turn i ok) (? (&& (i < 300) !(g 'over)) (: k (peep dirs (roll kb 10) 46)
                   _ (crawl-turn g k)
                   p (g 'p)
                   (turn (i + 1)
                         (&& ok (open? (g 'd) (p 'x) (p 'y))
                                ((p 'hp) <= p 'mx)
                                (0 <= p 'gold))))
                ok)
 (assert (turn 0 1)
         (|| (g 'over) (0 < g 'p 'hp))))

; --- the painter: one frame into a jug, glyphs and hues where they belong ---
(: g (crawl-new 60 20 (wheel 3))
   f (crawl-frame g)
 (assert
  (string? f)
  (find f "@" 0)                                 ; you are on the map
  (find f "#" 0) (find f "." 0)                  ; walls and floor in the light
  (find f "deck 1/3" 0) (find f "hp 12/12" 0)    ; the bar tells the tale
  (find f "\e[38;5;228m" 0)                      ; your hue, respecified on change
  (find f "\r\n" 0)))                            ; frames ride \r\n

(test_fin ())
(puts "rove: ok")
