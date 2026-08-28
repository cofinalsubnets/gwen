; --- test/host/wharf.l: the laws of lib/wharf.l, quay's layout algebra ---
; NOT in the test/*.l corpus, but for a different reason than its neighbours here:
; wharf needs NO nifs at all -- it is arithmetic over lists, and would run happily
; on love0, wasm and the kernel. what it needs is the LOADER: `use` finds lib/wharf.l
; by the filesystem walk, and the corpus also runs where there is no filesystem. when
; a second seat bakes wharf behind its egg, these laws can move to the corpus whole.
;
;   cat test/00-init.l test/host/wharf.l | out/host/love   ; exits 1 on fail
;
; the vocabulary under test is rectangles in CELLS: (id row col rows cols), origins
; 0-based, and a bar is (dir row col len) with dir 'across or 'down.
(use "lib/wharf.l")

; --- the two standing laws, checked rather than trusted ---
; spans are half-open, so a zero-extent berth is inside everything and overlaps
; nothing -- which is why neither law needs to except it.
(: (ovl a1 n1 a2 n2) (&& (a1 < (a2 + n2)) (a2 < (a1 + n1)))
   (hit a b) (&& (ovl (wharf-row a) (wharf-rows a) (wharf-row b) (wharf-rows b))
                 (ovl (wharf-col a) (wharf-cols a) (wharf-col b) (wharf-cols b)))
   (clashes bs) (? (two? bs) ((tally (filter (hit <bs) >bs)) + (clashes >bs)) 0)
   (inside r c b) (&& (&& (0 <= (wharf-row b)) (((wharf-row b) + (wharf-rows b)) <= r))
                      (&& (0 <= (wharf-col b)) (((wharf-col b) + (wharf-cols b)) <= c)))
   (strays r c bs) (tally (filter (\ b (! (inside r c b))) bs))
   ; every plan laid at every size: nothing overlaps, nothing escapes, nobody is lost
   (sound? p r c) (: bs (wharf-lay p r c)
                     (&& (&& (0 = (clashes bs)) (0 = (strays r c bs)))
                         ((tally bs) = (tally (wharf-berths p)))))
   plans '(9
           (beside 50 1 2)
           (stack 50 1 2)
           (beside 25 1 (stack 75 2 3))
           (stack 30 (beside 50 1 2) (beside 50 3 4))
           (beside 50 (stack 50 1 2) (stack 50 3 4))
           (beside 10 1 (beside 10 2 (beside 10 3 4))))
   ; ⚠ <>s, not (cup s): the tail of (24 80) is the LIST (80), whose net is 80 --
   ; so the arithmetic below would run on a list and quietly prove nothing
   sizes '((24 80) (1 1) (2 2) (3 3) (1 80) (80 1) (5 7) (200 300))
 _ (assert
    (0 = (tally (filter (\ p (! (foldl (\ a s (&& a (sound? p <s <>s))) 1 sizes)))
                        plans)))))

; --- 1. a lone berth is the whole wharf, and wears no bar ---
(: _ (assert
      ('((7 0 0 24 80)) = (wharf-lay 7 24 80))
      (() = (wharf-bars 7 24 80))
      ('(7) = (wharf-berths 7))))

; --- 2. a split spends exactly one cell on its bar ---
; 39 + 1 + 40 = 80, and the bar stands in the column neither half was given.
(: bs (wharf-lay '(beside 50 1 2) 24 80)
   br (wharf-bars '(beside 50 1 2) 24 80)
 _ (assert
    ('((1 0 0 24 39) (2 0 40 24 40)) = bs)
    ('((down 0 39 24)) = br)
    (80 = ((wharf-cols <bs) + 1 + (wharf-cols <>bs)))
    (24 = (wharf-rows <bs))))                   ; the undivided axis is untouched

; --- 3. stack divides the rows the same way, and its bar lies across ---
(: _ (assert
      ('((1 0 0 11 80) (2 12 0 12 80)) = (wharf-lay '(stack 50 1 2) 24 80))
      ('((across 11 0 80)) = (wharf-bars '(stack 50 1 2) 24 80))))

; --- 4. pct is a's share of what is left after the bar ---
(: bs (wharf-lay '(beside 25 1 2) 24 80)
 _ (assert (19 = (wharf-cols <bs)) (60 = (wharf-cols <>bs))))

; --- 5. nesting: a plan is a tree, and the halves divide what they were handed ---
(: p '(beside 50 1 (stack 50 2 3))
 _ (assert
    ('((1 0 0 24 39) (2 0 40 11 40) (3 12 40 12 40)) = (wharf-lay p 24 80))
    ('((down 0 39 24) (across 11 40 40)) = (wharf-bars p 24 80))
    ('(1 2 3) = (wharf-berths p))))

; --- 6. the tight end: three cells is the smallest split there is ---
; ⚠ under three, the split does not happen: a takes the axis, b lays at its
; PARENT'S corner with zero extent -- present in the list, absent from the glass.
(: p '(beside 50 1 2)
   b3 (wharf-lay p 24 3)
   b2 (wharf-lay p 24 2)
 _ (assert
    ('((1 0 0 24 1) (2 0 2 24 1)) = b3)         ; 1 + bar + 1
    (two? (wharf-bars p 24 3))
    ('((1 0 0 24 2) (2 0 0 24 0)) = b2)         ; no bar, and b is at the corner
    (() = (wharf-bars p 24 2))
    (wharf-show? <b2)
    (! (wharf-show? <>b2))
    (2 = (tally b2))))                          ; ⚠ and it is STILL IN THE LIST

; --- 7. the hit test: bars belong to nobody ---
(: bs (wharf-lay '(beside 50 1 (stack 50 2 3)) 24 80)
 _ (assert
    (1 = (wharf-at bs 0 0))
    (() = (wharf-at bs 0 39))                   ; the bar's own column
    (2 = (wharf-at bs 0 40))
    (3 = (wharf-at bs 12 40))
    (() = (wharf-at bs 11 40))                  ; the across bar between 2 and 3
    (() = (wharf-at bs 99 99))
    ; a zero-extent berth claims nothing, even the cell it is seated on -- asked
    ; alone, because its showable sibling covers that corner and answers first
    (() = (wharf-at (drop 1 (wharf-lay '(beside 50 1 2) 24 2)) 0 0))))

; --- 8. steering: nearest past the edge, other axis overlapping ---
; the wall is a no-op rather than a condition, and so is a focus that has been shut.
(: bs (wharf-lay '(beside 50 1 (stack 50 2 3)) 24 80)
 _ (assert
    (2 = (wharf-steer bs 1 'right))             ; ties on gap break to the nearer row
    (1 = (wharf-steer bs 2 'left))
    (1 = (wharf-steer bs 3 'left))              ; one tall berth reached from either
    (3 = (wharf-steer bs 2 'down))
    (2 = (wharf-steer bs 3 'up))
    (1 = (wharf-steer bs 1 'left))              ; the wall
    (3 = (wharf-steer bs 3 'right))
    (2 = (wharf-steer bs 2 'up))
    (99 = (wharf-steer bs 99 'left))))          ; a stale id steers nowhere

; --- 8a. an unshowable berth is never steered to ---
(: bs (wharf-lay '(beside 50 1 2) 24 2)
 _ (assert (1 = (wharf-steer bs 1 'right))))

; --- 9. splitting names the new berth, and leaves the rest of the plan alone ---
(: _ (assert
      ('(stack 50 2 5) = (wharf-split 2 2 'stack 50 5))
      ('(beside 50 1 (stack 50 2 5)) = (wharf-split '(beside 50 1 2) 2 'stack 50 5))
      ('(beside 50 1 2) = (wharf-split '(beside 50 1 2) 99 'stack 50 5))))

; --- 10. shutting promotes the sibling: the pair's whole rectangle, bar included ---
(: p (wharf-shut '(beside 50 1 (stack 50 2 3)) 2)
 _ (assert
    ('(beside 50 1 3) = p)
    ('((1 0 0 24 39) (3 0 40 24 40)) = (wharf-lay p 24 80))
    (() = (wharf-shut 7 7))                     ; the last one leaves an empty wharf
    ('(beside 50 1 2) = (wharf-shut '(beside 50 1 2) 99))
    (1 = (wharf-shut '(beside 50 1 2) 2))))

; --- 10a. the empty wharf holds nothing, and is not a berth named () ---
(: _ (assert
      (() = (wharf-lay () 24 80))
      (() = (wharf-bars () 24 80))
      (() = (wharf-berths ()))
      (() = (wharf-at (wharf-lay () 24 80) 0 0))))

; --- 11. split then shut is the identity on the plan ---
(: p '(beside 25 1 (stack 75 2 3))
 _ (assert (p = (wharf-shut (wharf-split p 3 'beside 50 4) 4))))

(test_fin ())
(puts "wharf: ok")
