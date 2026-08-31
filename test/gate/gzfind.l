; test/gate/gzfind.l -- lib/gz.l's match finder has two presentations: `gz-find`, which is
; the definition, and the holo IR beside it that says the same thing to a cpu. This gate is
; the differential between them, and it needs nothing outside the tree.
;
; the twin here is love, not C. test/gate/forge.l's kernels shadow C nifs, so a red there
; could be either side; here the twin is the only definition there is, so a disagreement is
; the kernel's. That is also the reason this kernel is worth having: what it beats is an
; interpreted loop, where forge's own three merely draw with gcc.
;
; the guards are most of it. The twin reads its arguments through love -- `(s i)` bounds-
; checks, a cask applied behaves as 0 -- so a torn `n` or a window array of the wrong size is
; a defined, if useless, answer there and a wild read here. Every shape the IR does not say
; must come back through the twin carrying the twin's own answer, and a guard that falls
; through where it should jump reads memory it does not own and says nothing about it.
;
; zero fitted is not a failure here and is in test/gate/forge.l. a build with no glaze
; module answers the twin everywhere, which is the design; what this gate owes is that the
; two agree, and that it says out loud which one it was comparing.
(: _ (use 'gz)
   _ (use "lib/forge.l")
   (die m) (: _ (say err ("gzfind: " + m + "\n")) _ (flush err) (quit 1))
   (safe f) (trap (\ _ (f 0)) () (\ a b ['scared a]))
   (mk n f) (: b (cask n) (go i) (? (n <= i) (string b) (: _ (pin b i (f i)) (go (i + 1)))) (go 0))
   win (32768 * 4)

   nat (gz-finder (gz-floor + 1))
   native (peep (from 'forge 'forge-nat) 'gz-find 0)
   _ (say out ("gzfind: the finder is " + (? native "NATIVE" "the twin")
               + ", arch " + love-arch + "\n"))
   _ (? (forge-why 0) (say out ("  declines: " + show (forge-why 0) + "\n")) 0)

   ; --- the corpora ---------------------------------------------------------------------
   ; chosen for the chain, which is the thing the kernel walks. one byte repeated puts
   ; every position in one bucket and is the only input that reaches gz-chain's limit at
   ; every step -- and the only one whose matches run to gz-maxmatch, so it is also where
   ; the `lim <= l` early return lives. a short cycle is the same shape at real distances.
   ; a spread leaves the buckets nearly empty and exercises the early exits instead. The
   ; sizes differ because the twin is what costs here: `one` walks 32 chains of 258 bytes
   ; at every position, and the point is chain depth, which 3000 positions already has.
   corpora [['one    (mk  3000 (\ i 65))]
            ['cycle  (mk  9000 (\ i (65 + (i % 7))))]
            ['spread (mk 20000 (\ i (((i * 2654435761) >> 13) % 251)))]
            ['runs   (mk 12000 (\ i (? ((i % 600) < 300) 32 (i % 256))))]
            ['text   (: (go k a) (? (k < 1) a
                                    (go (k - 1) (a + "the quick brown fox jumps over the lazy dog. "
                                                   + "pack my box with five dozen liquor jugs. ")))
                        (go 300 ""))]]

   ; --- the differential ----------------------------------------------------------------
   ; the window arrays are built as deflate builds them, every position indexed as it is
   ; passed, because a chain is only interesting once it is deep and a synthetic one would
   ; not have the shape the two can disagree about.
   (walk nom s)
    (: n (tally s)
       head (cask win)   prev (cask win)
       (go i)
        (? ((i + gz-minmatch) > n) (n - gz-minmatch + 1)
           (: a (gz-find s n i head prev)
              b (nat s n i head prev)
              _ (? (= a b) 0
                   (die (show nom + " at " + show i + ": twin " + show a + ", native " + show b)))
              _ (gz-ins s n i head prev)
              (go (i + 1))))
       (go 0))
   seen (foldl (\ a r (a + (walk <r <>r))) 0 corpora)
   _ (say out ("  agree on " + show seen + " positions over "
               + show (tally corpora) + " corpora\n"))

   ; --- the shapes the IR does not say --------------------------------------------------
   ; a real window pair, so that in every row below exactly one thing is odd.
   wn 20000
   ws (mk wn (\ i (65 + (i % 11))))
   wh (cask win)   wp (cask win)
   _ (: (go i) (? (wn <= i) 0 (: _ (gz-ins ws wn i wh wp) (go (i + 1)))) (go 0))
   bads [['s-cask   (cask 64)  wn        100        wh          wp]
         ['s-tray   (iota 40)  wn        100        wh          wp]
         ['s-fix    7          wn        100        wh          wp]
         ['s-nil    ()         wn        100        wh          wp]
         ['s-flo    1.5        wn        100        wh          wp]
         ['n-str    ws         "x"       100        wh          wp]
         ['n-flo    ws         1.5       100        wh          wp]
         ['n-past   ws         (wn + 500) 100       wh          wp]
         ['n-neg    ws         -5        100        wh          wp]
         ['n-zero   ws         0         100        wh          wp]
         ['i-str    ws         wn        "x"        wh          wp]
         ['i-neg    ws         wn        -1         wh          wp]
         ['i-end    ws         wn        (wn - 2)   wh          wp]
         ['i-past   ws         wn        (wn + 9)   wh          wp]
         ['head-str ws         wn        100        "nope"      wp]
         ['head-sml ws         wn        100        (cask 64)   wp]
         ['prev-str ws         wn        100        wh          "nope"]
         ['prev-sml ws         wn        100        wh          (cask 64)]]
   _ (map (\ [nom s n i h p]
             (: a (safe (\ _ (gz-find s n i h p)))
                b (safe (\ _ (nat s n i h p)))
                (? (= a b) 0
                   (die ("deopt " + show nom + ": twin " + show a + ", native " + show b)))))
          bads)
   _ (say out ("  " + show (tally bads) + " shapes the IR declines come back through the twin\n"))

   ; a doctored window. None of these can come out of gz-ins and each is one `pin` away,
   ; and each is a different wrong read: a cell naming a position ahead of i walks the
   ; compare forward off the end of s, and a cell with the top bit set is where `ld4`'s
   ; sign extension and gz-c32's four shifted bytes part company.
   ; `ws` has period 11, so a distance divisible by 11 matches to the limit and one that
   ; is not matches nothing -- which is what makes the last row below discriminating. A
   ; dead first link, then a torn link, then a live one 11 back: the twin walks past the
   ; torn cell to the match, and a kernel that read that cell as -1 would stop at it and
   ; answer no-match. Without the row, dropping `zx4` passes this whole gate.
   (doc h) (: dh (cask win)   dp (cask win)
              _ (map (\ [w k v] (gz-c32! (? w dp dh) k v)) h)
              (? (= (safe (\ _ (gz-find ws wn 100 dh dp))) (safe (\ _ (nat ws wn 100 dh dp))))
                 1 0))
   hs (gz-hash ws 100)
   docs [['ahead     [[0 hs 20000]]]
         ['topbit    [[0 hs 4294967295]]]
         ['prevahead [[0 hs 100] [1 (99 & gz-wmask) 20000]]]
         ; the two sign-extension rows, and they must be two: the head cell and the chain
         ; link are separate loads, and a torn cell only tells them apart where the walk
         ; goes on past it to a match -- so each row parks a live position 11 back at the
         ; bucket the torn one hands the walk to.
         ['headtorn  [[0 hs 4294967295] [1 (4294967294 & gz-wmask) 90]]]
         ['prevtorn  [[0 hs 91] [1 (90 & gz-wmask) 4294967295]
                      [1 (4294967294 & gz-wmask) 90]]]]
   _ (map (\ [nom h] (? (doc h) 0 (die ("a doctored window disagrees: " + show nom))))
          docs)
   _ (say out ("  a doctored window agrees " + show (tally docs) + " ways\n"))

   ; --- and the whole coder, both finders -----------------------------------------------
   ; the finder feeds the greedy parse, so one differing match moves every token after it:
   ; a byte-identical stream over a corpus this size is a much stronger claim than the
   ; per-position walk above, and it is the one a user would notice breaking.
   (defl fn s) (: n (tally s)
                  j (jug 0)   t (tablet 0)
                  head (cask win)   prev (cask win)
                  _ (gz-blocks fn s n j t head prev 0)
                  _ (gz-align j t)
                  ((slurp j) + ""))
   big (foldl (\ a r (a + <>r)) "" corpora)
   dt (defl gz-find big)
   dn (defl nat big)
   _ (? (= dt dn) 0 (die ("the two deflates differ: " + show (tally dt) + " vs " + show (tally dn))))
   r (gz-inflate dn (tally big))
   _ (? (two? r) 0 (die "the native stream will not inflate"))
   _ (? (= <(>r) big) 0 (die "the native stream inflates to something else"))
   _ (say out ("  " + show (tally big) + " bytes deflate byte-identical both ways, and round-trip\n"))

   ; --- the floor ------------------------------------------------------------------------
   ; `use 'forge` costs ~24 ms of parse and eval and a tar member is a few hundred bytes,
   ; so below the floor the twin is the better program and the finder must not reach for
   ; the kernel at all. Above it, having reached once, the answer is cached.
   _ (? (id? (gz-finder (gz-floor - 1)) gz-find) 0 (die "a small input forged anyway"))
   _ (? (id? (gz-finder (gz-floor + 1)) nat) 0 (die "the forged finder is not cached"))
   _ (say out "gzfind: 0 failed\n")
   0)
