; --- test/host/manifest.l: the spreadsheet, engine and tui, end to end ---
; NOT in the test/*.l corpus (the tui rides raw/winsize; the pty harness
; rides tether -- all host/*.c). the engine itself is pure core, probed here
; against the loaded app; the tui is driven over a REAL pty and its painted
; screen read back through the cb glass -- the emulator built for berth is
; the spreadsheet's witness:
;
;   cat test/00-init.l test/host/manifest.l | out/host/love

(reads (open "crew/manifest/manifest.l" "r"))

; --- formulas, the unit, labels, functions as cargo ---
(: sh (sheet 0)
 _ (cell! sh (\ a1) "5")
 _ (cell! sh (\ b1) "a1 * 7")             ; infix goes bare, repl-style
 _ (cell! sh (\ c1) "(+ a1 b1)")
 _ (assert
    (= 5 (worth sh (\ a1)))
    (= 35 (worth sh (\ b1)))
    (= 40 (worth sh (\ c1))))
 _ (cell! sh (\ d1) "(+ a1 z9)")          ; z9 is empty: the unit rides
 _ (assert (= 5 (worth sh (\ d1))))
 _ (cell! sh (\ e1) "hello")              ; a bare unknown name is a label
 _ (cell! sh (\ f1) "\"port of call\"")   ; a string is text
 _ (assert
    (= "hello" (worth sh (\ e1)))
    (= "port of call" (worth sh (\ f1))))
 _ (cell! sh (\ g1) "(+ 2)")              ; a cell may HOLD a function
 _ (cell! sh (\ h1) "(g1 40)")            ; ... and a neighbour may apply it
 _ (assert (= 42 (worth sh (\ h1))))
 _ (cell! sh (\ a1) "10")                 ; an edit recomputes the dependents
 _ (assert
    (= 70 (worth sh (\ b1)))
    (= 80 (worth sh (\ c1)))))

; --- the whole language in a cell: arrays, scopes, higher order ---
(: sh (sheet 0)
 _ (cell! sh (\ a1) "3")
 _ (cell! sh (\ i1) "@(1 2 3)")
 _ (cell! sh (\ j1) "net i1")
 _ (cell! sh (\ n1) "(: x 3 (x * a1))")   ; a local x shadows its label
 _ (cell! sh (\ o1) "(map (+ a1) (L 1 2))")
 _ (assert
    (= 6 (worth sh (\ j1)))
    (= 9 (worth sh (\ n1)))
    (= (L 4 5) (worth sh (\ o1)))))

; --- cycles: flagged at every cell on the loop, and taint what reads them
; (a bare whirl is the unit and would otherwise ride quietly through) ---
(: sh (sheet 0)
 _ (cell! sh (\ k1) "l1 + 1")
 _ (cell! sh (\ l1) "k1 + 1")
 _ (cell! sh (\ m1) "(+ k1 5)")
 _ (cell! sh (\ a1) "3")
 _ (assert
    (= "cycle!" (face (worth sh (\ k1))))
    (= "cycle!" (face (worth sh (\ l1))))
    (= "cycle!" (face (worth sh (\ m1))))
    (= 3 (worth sh (\ a1)))               ; clean cargo stays clean
    (= "" (face ()))                      ; an empty cell wears nothing
    (= "5" (face 5)))
 _ (cell! sh (\ l1) "2")                  ; break the circle: all is forgiven
 _ (assert
    (= 3 (worth sh (\ k1)))
    (= 8 (worth sh (\ m1)))))

; --- stow / unstow: the sheet as one readable form, worths surviving ---
(: p "/tmp/manifest-boot-test.l"
 sh (sheet 0)
 _ (cell! sh (\ a1) "5")
 _ (cell! sh (\ b1) "a1 * 7")
 _ (cell! sh (\ e1) "\"line \\e[7m noise\"")   ; escapes must round-trip
 _ (assert (stow sh p))
 sh2 (unstow (sheet 0) p)
 _ (assert
    (= (cell sh2 (\ b1)) (cell sh (\ b1)))
    (= 35 (worth sh2 (\ b1)))
    (= (worth sh (\ e1)) (worth sh2 (\ e1)))))

; --- the tui, over a real pty, the cb screen as witness: type a formula,
; sail right, type another that reads the first, watch the paint ---
(: (findi h n i) (? (< (+ i (tally n)) (+ (tally h) 1))
                    (? (= n (snip h i (+ i (tally n)))) i (findi h n (+ i 1)))
                    -1)
   (find h n i) (! (! (+ 1 (findi h n i))))
   (mkscr r c) (screen (cask (screen () r c)) r c)
   (rowtx s r) (: c (gaze s 2)
                (foldl (\ a i (+ a (: ch (& 255 (glass s (+ (* r c) i))) (? (= 0 ch) 32 ch))))
                       "" (jot c)))
 md (tether `("out/host/love" "-l" "crew/manifest/manifest.l" "-e" "(manifest ())"))
 m (cup md)
 ; a reader task gathers every painted byte into a box, so the test can
 ; HANDSHAKE: keys sent before the child's raw-mode first frame would land
 ; in a COOKED pty (ICRNL eats the \r commit) -- wait for proof, then type
 buf #""
 rd (twirl (\ _ (: (pump x) (: ch (see m)
                              (? (= ch -1) ()
                               (: _ (pin buf () (+ (peep buf () "") ("" + ch)))
                                  (pump ()))))
                  (pump ()))) 0)
 (await s tries) (: (aw t) (? (find (peep buf () "") s 0) 1
                              (< t 1) 0
                              (: _ (rest 30) (aw (- t 1))))
                    (aw tries))
 _ (assert (await "\e[?25l" 300))         ; the first frame: raw mode is up
 _ (say m "=10\r") _ (flush m)            ; a1 = 10
 _ (assert (await "a1: 10" 300))          ; committed: the bar shows the source
 _ (say m "\e[C") _ (flush m)             ; sail right to b1
 _ (assert (await "b1" 300))
 _ (say m "=a1 * 7\r") _ (flush m)        ; b1 = a1 * 7
 _ (assert (await "70" 300))              ; the worth painted
 _ (say m "q") _ (flush m)                ; step ashore
 st (: (wf p2) (: g2 (gather p2) (? (two? g2) g2 (: _ (rest 2) (wf p2)))) (wf (cap md)))
 _ (catch rd)                             ; the reader drains to EOF and rests
 outb (peep buf () "")
 scr (mkscr 24 80)
 ; the tail "\e[?1049l" would CLEAR the one-grid witness (alt-screen leave
 ; is clear-and-restore) -- scribe only the voyage, not the stepping-ashore
 shore (findi outb "\e[?1049l" 0)
 _ (scribe scr (? (< -1 shore) (snip outb 0 shore) outb))
 row1 (rowtx scr 2)                       ; screen line 2 = sheet row 1
 _ (assert
    (two? st) (= 0 (cap st))              ; a clean exit
    (find outb "\e[?1049h" 0)             ; the alt screen was entered
    (find outb "\e[?1049l" 0)             ; ... and left
    (find row1 "10" 0)                    ; a1 painted
    (find row1 "70" 0)                    ; b1 = a1 * 7 painted
    (find (rowtx scr 0) "b1" 0))          ; the formula bar rode the cursor
 (close m))

(puts "manifest: ok")
