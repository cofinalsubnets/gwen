; ink -- quay's screensaver: a chromatophore shimmer for the terminal.
;
; the cuttlefish paints its skin: four drifting sine waves (three linear, one
; radial ripple) sampled over the window grid by galaxy broadcasting -- C does
; every cell -- then quantized onto a 256-color ocean ramp and said to the
; terminal as painted cells. any key returns you to shore.
;
; run: out/host/love -l crew/quay/ink.l -e '(ink ())'
; the shimmer core is pure love (ink-scene builds a frame(t) closure that runs
; headless into a jug -- the wasm console drives it); only the tty runner (ink)
; ties raw / winsize / cue?, which ride src/posix.c, so ink never joins the test
; corpus (love0 links no host nifs) -- probe it live under a pty instead.

(: esc (string 27)

   ; the ocean ramp: 24 chromatophores, deep blue out to pale pink and back,
   ; each pre-rendered as one SGR background cell so a frame is pure lookup
   ramp [17 18 19 20 26 32 38 44 45 51 87 123 159 195 189 183 147 111 75 69 63 57 25 24]
   (cell n) (+ (+ (+ esc "[48;5;") (show n)) "m ")   ; show: decimal digits (string of a charm is its CHAR)
   pal (: (dab t i l) (? l (: _ (pin t i (cell <l)) (dab t (+ i 1) >l)) t)
          (dab {0 (cell 17)} 0 ramp))

   ; the scene at size (r c): the grid, built once, closed into a frame(t) that
   ; runs headless into a jug. (r 1) + (1 c) broadcasts to (r c); cy doubles y so
   ; the ripple is round in cell aspect (a cell is about twice as tall as wide).
   (ink-scene r c) (:
     ycol (star-tray [r 1] (jot r))
     xrow (star-tray [1 c] (jot c))
     y2 (+ (* ycol 1.0) (* xrow 0.0))
     x2 (+ (* ycol 0.0) (* xrow 1.0))
     cx (- x2 (/ c 2.0))
     cy (* (- y2 (/ r 2.0)) 2.0)
     rr ((/ 1 2) (+ (* cx cx) (* cy cy)))

     ; the skin at time t: net of the four waves, a gem galaxy in [-4, 4]
     (field t) (+ (sine (+ (* x2 0.09) t))
                  (+ (sine (- (* y2 0.16) (* t 0.7)))
                     (+ (sine (+ (* (+ x2 y2) 0.06) (* t 0.4)))
                        (sine (- (* rr 0.11) t)))))

     ; one frame: quantize the skin to ramp indices, say every cell into a jug,
     ; slurp it back as one write (home first; reset + newline per row)
     (row j k i q) (? (< q c) (: _ (say j (pal (int (peep k [i q] 0)))) (row j k i (+ q 1))) ())
     (rows j k i) (? (< i r) (: _ (row j k i 0) _ (say j (+ esc "[0m\n")) (rows j k (+ i 1))) ())
     (frame t) (: k (// (* (+ (field t) 4.001) 2.9) 1)
                  j (jug 0)
                  _ (say j (+ esc "[H"))
                  _ (rows j k 0)
                  (slurp j))
     frame)

   ; ink -- the shimmer on the tty. the swim: paint, rest a beat, listen for a
   ; cue from shore. the frame goes out say's bulk lane -- one write(2) per frame.
   (ink _) (: ws (winsize 0)
              rv (? (two? ws) (- <ws 1) 0)          ; spare the last line: no scroll
              cv (? (two? ws) >ws 0)
              r (? (< 1 rv) rv 23)                  ; a sizeless tty paints 23x80
              c (? (< 1 cv) cv 80)
              frame (ink-scene r c)
              (swim tick) (? (cue? 0) ()
                             (: f (frame (* tick 0.06))
                                _ (say out f)
                                _ (rest 33)
                                (swim (+ tick 1))))
              _ (raw 1)
              _ (say out (+ esc (+ "[?1049h" (+ esc (+ "[?25l" (+ esc "[2J"))))))   ; alt screen, no cursor
              _ (swim 0)
              _ (see in)                                                            ; drink the cue
              _ (say out (+ esc (+ "[0m" (+ esc (+ "[?25h" (+ esc "[?1049l"))))))    ; shore
              _ (flush out)
              (raw 0)))
