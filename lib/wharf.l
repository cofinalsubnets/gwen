; lib/wharf.l -- a wharf is where several vessels berth: the layout algebra under
; quay's panes, and the one piece every seat shares. it answers RECTANGLES IN
; CELLS, never pixels -- berth seats them with \e[r;cH, the kernel console
; multiplies them by the font's width and height, and neither has to know what
; the other does with them. cells are the only currency both speak, and the one
; `gaze` already answers in. `(use 'wharf)`.
;
; a PLAN is a binary tree. a leaf is a berth's id; a node is one split:
;
;   (dir pct a b)   dir 'stack (a above b) or 'beside (a left of b)
;                   pct 1..99 -- a's share of the axis being divided
;
; ⚠ AN ID IS ANY VALUE THAT IS NOT A PAIR: pairs are how a node is told from a
; leaf. charms are the obvious choice and what the laws use. () is reserved --
; it is how wharf-shut says the last berth is gone.
;
; a split SPENDS A CELL on the bar between its halves, so two berths side by side
; in 80 columns get 39 and 40. under three cells there is no room for two, so the
; split does not happen and b lays at zero extent.
;
; ⚠ A ZERO-EXTENT BERTH IS STILL IN THE LIST, wearing (id row col 0 0) -- a berth
; too small to show is not a berth that vanished, and a caller that cannot tell
; those apart will paint one and steer to the other. presence is out of band here
; as everywhere: ask wharf-show?, never whether the rectangle nets nothing.
;
; two laws hold of every lay, and test/host/wharf.l checks them over a spread of
; plans and sizes rather than trusting the arithmetic: every rectangle sits INSIDE
; the wharf, and no two OVERLAP. the spans are half-open, so a zero-extent berth
; overlaps nothing -- including itself -- and neither law has to except it.
(:
   ; --- reading a laid berth: (id row col rows cols), origins 0-based ----------
   (wharf-id b)    <b
   (wharf-row b)   <>b
   (wharf-col b)   <>>b
   (wharf-rows b)  <>>>b
   (wharf-cols b)  <>>>>b
   (wharf-show? b) (&& (0 < (wharf-rows b)) (0 < (wharf-cols b)))

   ; a's share of n cells at pct, the bar's cell already spent. both halves get
   ; at least one; under three there is no split to make and a takes it all.
   (wcut n pct) (? (n < 3) n
                   (: k (// ((n - 1) * pct) 100)
                      (? (k < 1) 1 ((n - 2) < k) (n - 2) k)))
   (wrest n a) (? (n < 3) 0 (n - 1 - a))

   ; --- the lay: every berth's rectangle, in plan order -----------------------
   ; ⚠ a half with no room lays at its PARENT'S corner, not past the bar that was
   ; never spent -- so every rectangle a plan yields sits inside the wharf, and a
   ; caller may do arithmetic on an origin without first asking if it is real.
   (wlay p row col r c)
    (? (two? p)
       (: pct <>p a <>>p b <>>>p
          (? (= <p 'stack)
             (: h (wcut r pct) t (wrest r h)
                (cat (wlay a row col h c)
                     (wlay b (? (0 < t) (row + h + 1) row) col t c)))
             (: w (wcut c pct) t (wrest c w)
                (cat (wlay a row col r w)
                     (wlay b row (? (0 < t) (col + w + 1) col) r t)))))
       (L (L p row col r c)))
   (wharf-lay p r c) (wlay p 0 0 r c)

   ; --- the bars: (dir row col len), 'across spanning cols, 'down spanning rows
   ; a bar rides only where a split actually happened -- both halves showable --
   ; so a wharf too narrow to divide is drawn with no line through it.
   (wbars p row col r c)
    (? (two? p)
       (: pct <>p a <>>p b <>>>p
          (? (= <p 'stack)
             (: h (wcut r pct) t (wrest r h)
                (cat (? (&& (0 < h) (0 < t)) (L (L 'across (row + h) col c)) ())
                     (cat (wbars a row col h c)
                          (wbars b (row + h + 1) col t c))))
             (: w (wcut c pct) t (wrest c w)
                (cat (? (&& (0 < w) (0 < t)) (L (L 'down row (col + w) r)) ())
                     (cat (wbars a row col r w)
                          (wbars b row (col + w + 1) r t))))))
       ())
   (wharf-bars p r c) (wbars p 0 0 r c)

   ; --- the roster and the hit test -------------------------------------------
   (wharf-berths p) (? (two? p) (cat (wharf-berths <>>p) (wharf-berths <>>>p)) (L p))

   ; which berth owns this cell, or () for none -- a zero-extent one owns nothing,
   ; since its half-open span is empty, so the mouse can never land on the unshowable
   (wharf-at bs row col)
    (? (two? bs)
       (: b <bs
          (? (&& (&& ((wharf-row b) <= row) (row < ((wharf-row b) + (wharf-rows b))))
                 (&& ((wharf-col b) <= col) (col < ((wharf-col b) + (wharf-cols b)))))
             (wharf-id b)
             (wharf-at >bs row col)))
       ())

   ; --- steering ---------------------------------------------------------------
   ; focus moves to the nearest showable berth past this one's edge whose OTHER
   ; axis overlaps -- the rule that lets one tall berth be reached from any of the
   ; short ones beside it. nowhere to go answers the id unchanged, so a steer at
   ; the wall is a no-op rather than a condition.
   (wover a1 n1 a2 n2) (&& (a1 < (a2 + n2)) (a2 < (a1 + n1)))
   (wpick bs id) (? (two? bs) (? (= (wharf-id <bs) id) <bs (wpick >bs id)) ())
   (wpast f b d)
    (? (= d 'right) (&& (((wharf-col f) + (wharf-cols f)) <= (wharf-col b))
                        (wover (wharf-row f) (wharf-rows f) (wharf-row b) (wharf-rows b)))
       (= d 'left)  (&& (((wharf-col b) + (wharf-cols b)) <= (wharf-col f))
                        (wover (wharf-row f) (wharf-rows f) (wharf-row b) (wharf-rows b)))
       (= d 'down)  (&& (((wharf-row f) + (wharf-rows f)) <= (wharf-row b))
                        (wover (wharf-col f) (wharf-cols f) (wharf-col b) (wharf-cols b)))
                    (&& (((wharf-row b) + (wharf-rows b)) <= (wharf-row f))
                        (wover (wharf-col f) (wharf-cols f) (wharf-col b) (wharf-cols b))))
   (wgap f b d)
    (? (= d 'right) ((wharf-col b) - ((wharf-col f) + (wharf-cols f)))
       (= d 'left)  ((wharf-col f) - ((wharf-col b) + (wharf-cols b)))
       (= d 'down)  ((wharf-row b) - ((wharf-row f) + (wharf-rows f)))
                    ((wharf-row f) - ((wharf-row b) + (wharf-rows b))))
   ; the tiebreak: how far off the other axis, so a straight move beats a diagonal
   (wskew f b d)
    (: v (? (|| (= d 'right) (= d 'left)) ((wharf-row b) - (wharf-row f))
                                          ((wharf-col b) - (wharf-col f)))
       (? (v < 0) (0 - v) v))
   (wnear f d best b)
    (? (! (two? best)) b
       (: g1 (wgap f b d) g2 (wgap f best d)
          (? (g1 < g2) b (g2 < g1) best
             (? ((wskew f b d) < (wskew f best d)) b best))))
   (wharf-steer bs id d)
    (: f (wpick bs id)
       (? (! (two? f)) id
          (: cs (filter (\ b (&& (wharf-show? b) (wpast f b d))) bs)
             (? (two? cs) (wharf-id (foldl (wnear f d) () cs)) id))))

   ; --- editing the plan --------------------------------------------------------
   ; split the berth named id in two; the new one takes the second half. an id
   ; that is not in the plan changes nothing, which is what a stale focus deserves.
   (wharf-split p id d pct new)
    (? (two? p) (L <p <>p (wharf-split <>>p id d pct new) (wharf-split <>>>p id d pct new))
       (? (= p id) (L d pct p new) p))

   ; shut a berth; its sibling takes the whole of what the pair held, bar and all.
   ; shutting the last one answers () -- a wharf with nothing tied up at it.
   (wharf-shut p id)
    (? (two? p)
       (: a (wharf-shut <>>p id) b (wharf-shut <>>>p id)
          (? (() = a) b (() = b) a (L <p <>p a b)))
       (? (= p id) () p)))
