; --- test/host/berth.l: the berth emulator's moor, end to end over a real pty ---
; NOT in the test/*.l corpus (screen/scribe/tether live in host/*.c; love0 links
; none of them). loads the berth library off disk with reads, berths real
; children, and reads the grid back through the glass:
;
;   cat test/00-init.l test/host/berth.l | out/host/love
;
; interactive berthing (raw tty, keys, repaint) stays a live probe -- this
; verifies the moor: pty -> parser -> grid, replies riding home, exit status.

(reads (open "crew/quay/berth.l" "r"))

(: (mkscr r c) (screen (cask (screen () r c)) r c)
   (chat s i) (& 255 (glass s i))
   (rowtx s r) (: c (gaze s 2)
                (foldl (\ a i (+ a (: ch (chat s (+ (* r c) i)) (? (= 0 ch) 32 ch))))
                       "" (jot c))))

; --- a child's plain output lands on the grid, colours and all ---
(: md (tether `("sh" "-c" "printf 'hi\\033[31mred'; exit 3"))
 _ (assert (two? md))
 s (mkscr 5 20)
 bx #s
 _ (moor bx (cup md) ())
 _ (assert
    (= "hired" (snip (rowtx s 0) 0 5))
    (= 7 (& 255 (>> (glass s 1) 8)))     ; the h: default pen
    (= 1 (& 255 (>> (glass s 2) 8))))    ; the r: red
 st (waitfor (cap md))
 _ (assert (two? st) (= 3 (cap st)))     ; the status pair came home
 (close (cup md)))

; --- the pty's ONLCR: a child's bare \n arrives as \r\n, so columns behave ---
(: md (tether `("sh" "-c" "printf 'a\\nb'"))
 s (mkscr 5 20)
 bx #s
 _ (moor bx (cup md) ())
 _ (assert
    (= 97 (chat s 0))
    (= 98 (chat s 20)))                  ; b at row 1 COLUMN 0, not under the a
 _ (waitfor (cap md))
 (close (cup md)))

; --- the dialogue lane: a DSR answer rides home and the child reads it ---
; the child asks where the cursor is, drinks the 6-byte "\e[1;1R" answer off
; its own stdin, then says it back through cat -v -- proof the moor forwards
; replies to the pty, and IN ORDER. stty raw -echo, as any real full-screen
; guest would: a COOKED slave holds input until a newline (the answer has none)
; and would echo it back onto the grid besides.
; ⚠ the child SAYS what it read rather than discarding it. it used to be
; `head -c 6 >/dev/null`, which passed just as happily while moor wrote the
; answer BACKWARDS -- a reply is a byte stream, and a test that never looks at
; the bytes cannot tell "\e[1;1R" from "R1;1[\e".
(: md (tether `("sh" "-c" "stty raw -echo; printf '\\033[6n'; head -c 6 | cat -v"))
 s (mkscr 5 20)
 bx #s
 _ (moor bx (cup md) ())
 _ (assert (= "^[[1;1R" (snip (rowtx s 0) 0 7)))
 _ (waitfor (cap md))
 (close (cup md)))

; --- a full-screen guest: clear, address, paint, leave ---
(: md (tether `("sh" "-c" "printf '\\033[2J\\033[3;5Hdeep\\033[7msea'"))
 s (mkscr 6 20)
 bx #s
 _ (moor bx (cup md) ())
 _ (assert
    (= "    deepsea" (snip (rowtx s 2) 0 11))
    (= 4 (>> (glass s (+ (* 2 20) 8)) 28)))   ; the s of sea: reverse video
 _ (waitfor (cap md))
 (close (cup md)))

; --- the utf-8 round trip, all the way through the painter: a guest says
; ─ and é; the grid folds them to cp437; full berth (painter and all, on
; its own pty) says them back OUT as the same utf-8 ---
(: (find h n i) (? (< (+ i (tally n)) (+ (tally h) 1))
                  (? (= n (snip h i (+ i (tally n)))) 1 (find h n (+ i 1)))
                  0)
 ; (the octal escapes ride through TWO love readers before sh: hence \\\\)
 md (tether `("out/host/love" "-l" "crew/quay/berth.l"
            "-e" "(berth (L \"sh\" \"-c\" \"printf 'x\\\\342\\\\224\\\\200y\\\\303\\\\251z'; sleep 1\"))"))
 outb (slurp (cup md))
 st (: (wf p) (: g2 (gather p) (? (two? g2) g2 (: _ (rest 2) (wf p)))) (wf (cap md)))
 _ (assert
    (two? st)
    (find outb "x\xe2\x94\x80y" 0)       ; the ─ came back out as utf-8
    (find outb "\xc3\xa9z" 0))           ; ... and the é
 (close (cup md)))

; --- the harbour: many berths on one wharf ---------------------------------
; the multiplexing half. berth's own entry needs a tty (raw/winsize) and blocks
; until the last child leaves, so these drive the harbour directly -- the same
; pieces berth wires up, minus the terminal. a pane is (box master pid).
(: (sub? h n i) (? (< (+ i (tally n)) (+ (tally h) 1))
                   (? (= n (snip h i (+ i (tally n)))) 1 (sub? h n (+ i 1)))
                   0)
   ; berth's own opening, with the tty parts left out
   (mkhb r c argv)
    (: p0 (hopen argv r c)
       panes #()
       _ (pin panes 1 p0)
       hb #()
       _ (pin hb 'plan 1) _ (pin hb 'focus 1) _ (pin hb 'next 2)
       _ (pin hb 'rows r) _ (pin hb 'cols c)
       _ (pin hb 'panes panes) _ (pin hb 'argv argv)
       _ (pin hb 'dirt #1) _ (pin hb 'jobs ()) _ (pin hb 'lay ())
       _ (hrelay hb)
       hb)
   (scrof hb id) ((hbox (hpane hb id)) -> () 0)

 ; --- one berth is the whole wharf, and its frame starts at the home cell ---
 hb (mkhb 10 20 `("sh" "-c" "printf hi"))
 _ (moor (hbox (hpane hb 1)) (hmas (hpane hb 1)) ())
 f (hframe hb)
 _ (assert
    ('((1 0 0 10 20)) = (hb 'lay))
    (20 = (gaze (scrof hb 1) 2))
    (sub? f "\e[1;1H" 0)                  ; row 0 of the only pane
    (sub? f "hi" 0))
 _ (waitfor (hpid (hpane hb 1)))
 _ (close (hmas (hpane hb 1)))

 ; --- a split re-opens BOTH screens to their new rectangles ----------------
 ; 21 columns divide 10 + bar + 10, and every pty is told what it now has.
 hb2 (mkhb 10 21 `("sh" "-c" "sleep 30"))
 _ (hsplit hb2 'beside)
 lay (hb2 'lay)
 _ (assert
    (2 = (tally lay))
    (10 = (wharf-cols <lay)) (10 = (wharf-cols <>lay))
    (0 = (wharf-col <lay))   (11 = (wharf-col <>lay))
    (2 = (hb2 'focus))                    ; the new berth takes the focus
    (10 = (gaze (scrof hb2 1) 2))         ; ⚠ pane 1 was re-opened, not left at 21
    (10 = (gaze (scrof hb2 2) 2))
    (10 = (gaze (scrof hb2 1) 1)))        ; the undivided axis is untouched

 ; --- the frame composites: each pane addressed at its own origin, plus a bar
 f2 (hframe hb2)
 _ (assert
    (sub? f2 "\e[1;1H" 0)                 ; pane 1's first row
    (sub? f2 "\e[1;11H" 0)                ; the bar's own column
    (sub? f2 hdown 0)                     ; ... drawn with it
    ; ⚠ anchor on a row the CURSOR never addresses: it lands on pane 2's origin
    ; too, so "\e[1;12H" alone passes even with the pane painter ignoring it
    (sub? f2 "\e[2;12H" 0)                ; pane 2's second row, one past the bar
    (sub? f2 "\e[10;12H" 0)               ; ... and its last
    (sub? f2 "\e[10;1H" 0))               ; pane 1's last row, at the wharf's edge

 ; --- steering moves the focus and nothing else ---------------------------
 _ (hsteer hb2 'left)
 _ (assert (1 = (hb2 'focus)))
 _ (hsteer hb2 'up)
 _ (assert (1 = (hb2 'focus)))            ; the wall: a no-op, not a condition
 _ (hsteer hb2 'right)
 _ (assert (2 = (hb2 'focus)))

 ; --- shutting promotes the sibling over the bar they shared --------------
 _ (hshut hb2 2)
 _ (assert
    (1 = (hb2 'plan))
    (1 = (hb2 'focus))                    ; the focus follows the survivor
    ('((1 0 0 10 21)) = (hb2 'lay))
    (21 = (gaze (scrof hb2 1) 2)))        ; and it took the bar's column too
 _ (hshut hb2 1)
 _ (assert
    (() = (hb2 'plan))                    ; the empty wharf ends berth's swim
    (() = (wharf-berths (hb2 'plan)))
    (() = (hb2 'focus)))
 (map (\ t (? (landed? t) (catch t) (freeze t))) (hb2 'jobs)))

; --- C-a v, on a real berth, over a real pty -------------------------------
; the prefix intercept is the one piece the harbour laws above cannot reach: it
; lives in the key pump, and a key has to arrive to exercise it. so berth runs
; on its own pty and the split is TYPED -- two bytes onto the master -- and the
; bar it draws comes back out in the frames. ⚠ a fresh pty reports 0x0, so berth
; falls back to 24x80 and the geometry here is deterministic: 39 + bar + 40.
(: (sub? h n i) (? (< (+ i (tally n)) (+ (tally h) 1))
                   (? (= n (snip h i (+ i (tally n)))) 1 (sub? h n (+ i 1)))
                   0)
 md (tether `("out/host/love" "-l" "crew/quay/berth.l"
              "-e" "(berth (L \"sh\" \"-c\" \"sleep 5\"))"))
 _ (assert (two? md))
 m (cup md)
 _ (rest 1200)                            ; let berth come up and moor its first child
 ; ⚠ two puts in sequence, never (map (put m) ..): map walks right to left, and
 ; "v C-a" is not the prefix -- the same trap this file's DSR law now pins down
 _ (put m 1) _ (put m 118) _ (flush m)     ; C-a v: split beside
 outb (slurp m)                           ; ... until both children leave and berth ends
 _ (assert
    (sub? outb "\e[1;40H" 0)              ; the bar's column, 39 zero-based
    (sub? outb hdown 0)                   ; ... drawn with the box glyph
    (sub? outb "\e[2;41H" 0))             ; the new pane's second row, past the bar
 _ (: (wf p) (: g2 (gather p) (? (two? g2) g2 (: _ (rest 2) (wf p)))) (wf (cap md)))
 (close m))

(puts "berth: ok")
