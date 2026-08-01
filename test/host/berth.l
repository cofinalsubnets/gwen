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
; its own stdin, then says OK -- proof the moor forwards replies to the pty.
; stty raw -echo, as any real full-screen guest would: a COOKED slave holds
; input until a newline (the answer has none) and would echo it back onto
; the grid besides.
(: md (tether `("sh" "-c" "stty raw -echo; printf '\\033[6n'; head -c 6 >/dev/null; printf OK"))
 s (mkscr 5 20)
 bx #s
 _ (moor bx (cup md) ())
 _ (assert (= "OK" (snip (rowtx s 0) 0 2)))
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

(puts "berth: ok")
