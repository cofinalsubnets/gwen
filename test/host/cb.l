; --- test/host/cb.l: a vttest-lite for quay's console screen (host/cb.c + crew/quay/quay.c) ---
; NOT in the test/*.l corpus on purpose: the screen nifs (screen/scribe/glass/gaze/reply)
; live in host/cb.c, which the final `love` links but love0 does NOT -- and love0 bakes the
; whole corpus, so a corpus test would read these names as missing and fail love0's
; self-test. So this is verified the host-nif way: standalone, against the built
; binary (the assert macro comes from test/00-init.l):
;
;   cat test/00-init.l test/host/cb.l | out/host/love      ; exits 1 on fail, prints cb: ok
;
; the same struct cb is the kernel's framebuffer console, so every law asserted
; here holds under inle too -- this file is the emulator's executable spec.

; --- the little vocabulary ---
(: (mkscr r c) (screen (cask (screen () r c)) r c)   ; the ctor: size protocol + open
   (chat s i) (& 255 (glass s i))                    ; the char byte of cell i
   (cface s i) (>> (glass s i) 28)                   ; the face nibble
   (cfg s i) (& 255 (>> (glass s i) 8))              ; fg index
   (cbg s i) (& 255 (>> (glass s i) 16))             ; bg index
   (cur s) (gaze s 0)
   (rowtx s r)                                       ; row r's chars, blanks as spaces
    (: c (gaze s 2)
       (foldl (\ a i (+ a (string (: ch (chat s (+ (* r c) i)) (? (= 0 ch) 32 ch))))) "" (jot c))))

; --- 1. the ctor and the size protocol ---
(: n (screen () 4 10)
 _ (assert
    (charm? n)                   ; a non-cask first arg answers the byte count
    (< 40 n)                     ; at least the 160 cell bytes + a header
    (! (screen (cask 8) 4 10))   ; a too-small cask is refused
    (! (screen (cask n) 0 10))   ; silly geometry is refused
    (! (scribe 5 "x"))           ; feeding a non-screen is nothing
    (! (glass (cask 8) 0)))      ; glassing a non-screen is nothing
 s (mkscr 4 10)
 _ (assert
    (= 4 (gaze s 1)) (= 10 (gaze s 2))   ; rows, cols
    (= 0 (gaze s 4)) (= 3 (gaze s 5))    ; the scroll region opens full
    (= 3 (& 3 (gaze s 3)))               ; cursor shown + autowrap on
    (= 0 (cur s))
    (= 0 (chat s 0))
    (! (glass s 40))))                   ; off the grid: nothing

; --- 2. plain text, the byte lanes, and controls ---
(: s (mkscr 4 10)
 _ (scribe s "hi")
 _ (assert
    (= 104 (chat s 0)) (= 105 (chat s 1))
    (= 7 (cfg s 0))                      ; the default pen: 7 on 0
    (= 2 (cur s)))
 _ (scribe s 33)                         ; a bare charm is one byte
 _ (assert (= 33 (chat s 2)))
 b (cask 2)
 _ (pin b 0 63) _ (pin b 1 63)
 _ (scribe s b)                          ; a cask pours its bytes
 _ (assert (= "hi!??" (snip (rowtx s 0) 0 5)))
 _ (scribe s "\r")
 _ (assert (= 0 (cur s)))                ; CR homes the column
 _ (scribe s "ab\x08\x08")               ; (\b is no reader escape: \x08 is the byte)
 _ (assert (= 0 (cur s)))
 _ (scribe s "\x08")
 _ (assert (= 0 (cur s))))               ; BS at column 0 stays (no row crossing)
(: s (mkscr 4 10)
 _ (scribe s "ab\ncd")
 _ (assert
    (= 99 (chat s 12)) (= 100 (chat s 13))   ; bare \n KEEPS the column (LNM off)
    (= 14 (cur s))))

; --- 3. tabs ---
(: s (mkscr 4 20)
 _ (scribe s "\ta")
 _ (assert (= 97 (chat s 8)))             ; tab stops every 8
 _ (scribe s "\tb")
 _ (assert (= 98 (chat s 16)))
 _ (scribe s "\t")
 _ (assert (= 19 (cur s))))               ; ... clamped at the last column

; --- 4. autowrap, deferred (the DECAWM pending-wrap law) ---
(: s (mkscr 4 10)
 _ (scribe s "0123456789")
 _ (assert
    (= 9 (cur s))                         ; ten glyphs: cursor STAYS on the last column
    (= 16 (& 16 (gaze s 3)))              ; ... wrap pending
    (= 57 (chat s 9)))
 _ (scribe s "A")
 _ (assert
    (= 65 (chat s 10))                    ; the eleventh glyph carried to a fresh row
    (= 11 (cur s)))
 _ (scribe s "\e[?7l")                    ; autowrap off
 _ (scribe s "\e[1;10H")
 _ (scribe s "XYZ")
 _ (assert
    (= 90 (chat s 9))                     ; X, Y, Z overwrote in place: Z holds
    (= 9 (cur s))
    (= 65 (chat s 10)))                   ; the next row untouched
 _ (scribe s "\e[?7h"))

; --- 5. absolute addressing + relative moves, clamped ---
(: s (mkscr 4 10)
 _ (scribe s "\e[2;5H")
 _ (assert (= 14 (cur s)))                ; CUP is 1-based
 _ (scribe s "\e[9;99H")
 _ (assert (= 39 (cur s)))                ; ... and clamps to the grid
 _ (scribe s "\e[H")
 _ (assert (= 0 (cur s)))
 _ (scribe s "\e[2B") _ (assert (= 20 (cur s)))    ; CUD
 _ (scribe s "\e[3C") _ (assert (= 23 (cur s)))    ; CUF
 _ (scribe s "\e[2A") _ (assert (= 3 (cur s)))     ; CUU
 _ (scribe s "\e[9A") _ (assert (= 3 (cur s)))     ; clamped at the top
 _ (scribe s "\e[2D") _ (assert (= 1 (cur s)))     ; CUB
 _ (scribe s "\e[9C") _ (assert (= 9 (cur s)))     ; clamped in the row
 _ (scribe s "\e[5G") _ (assert (= 4 (cur s)))     ; CHA: column 5
 _ (scribe s "\e[3d") _ (assert (= 24 (cur s)))    ; VPA: row 3, column kept
 _ (scribe s "\e[E")  _ (assert (= 30 (cur s)))    ; CNL: next row home
 _ (scribe s "\e[2F") _ (assert (= 10 (cur s))))   ; CPL: two up, home

; --- 6. erase: display and line, all three shapes ---
(: s (mkscr 3 10)
 _ (scribe s "0123456789012345678901234567")   ; fill (the last two pend)
 _ (scribe s "\e[2;5H\e[K")
 _ (assert (= "0123      " (rowtx s 1)))       ; EL 0: to end of row
 _ (scribe s "\e[1;5H\e[1K")
 _ (assert (= "     56789" (rowtx s 0)))       ; EL 1: from start, cursor inclusive
 _ (scribe s "\e[3;5H\e[2K")
 _ (assert (= "          " (rowtx s 2))))      ; EL 2: the whole row
(: s (mkscr 3 10)
 _ (scribe s "aaaaaaaaaabbbbbbbbbbcccccccc")
 _ (scribe s "\e[2;5H\e[J")
 _ (assert
    (= "aaaaaaaaaa" (rowtx s 0))
    (= "bbbb      " (rowtx s 1))
    (= "          " (rowtx s 2)))              ; ED 0: cursor to the end
 _ (scribe s "\e[1;5H\e[1J")
 _ (assert (= "     aaaaa" (rowtx s 0)))       ; ED 1: start to cursor
 _ (scribe s "\e[2J")
 _ (assert (= "          " (rowtx s 0))))      ; ED 2: everything

; --- 7. edit: insert/delete characters, erase characters ---
(: s (mkscr 2 10)
 _ (scribe s "abcdefghij")
 _ (scribe s "\e[1;3H\e[2@")
 _ (assert (= "ab  cdefgh" (rowtx s 0)))       ; ICH: room made, tail slides off
 _ (scribe s "\e[2P")
 _ (assert (= "abcdefgh  " (rowtx s 0)))       ; DCH: closed back up
 _ (scribe s "\e[3X")
 _ (assert (= "ab   fgh  " (rowtx s 0))
           (= 2 (cur s))))                     ; ECH blanks in place, cursor still

; --- 8. insert/delete lines ---
(: s (mkscr 4 10)
 _ (scribe s "aaaa\e[2;1Hbbbb\e[3;1Hcccc\e[4;1Hdddd")
 _ (scribe s "\e[2;1H\e[L")
 _ (assert
    (= "aaaa      " (rowtx s 0))
    (= "          " (rowtx s 1))               ; IL opened a blank row
    (= "bbbb      " (rowtx s 2))
    (= "cccc      " (rowtx s 3)))              ; ... and dddd fell off
 _ (scribe s "\e[M")
 _ (assert
    (= "bbbb      " (rowtx s 1))               ; DL took the blank back out
    (= "cccc      " (rowtx s 2))
    (= "          " (rowtx s 3))))

; --- 9. the scroll region: DECSTBM, region-bound feeds, SU/SD, RI ---
(: s (mkscr 5 10)
 _ (scribe s "top\e[2;1Haa\e[3;1Hbb\e[4;1Hcc\e[5;1Hbot")
 _ (scribe s "\e[2;4r")
 _ (assert
    (= 1 (gaze s 4)) (= 3 (gaze s 5))          ; the region took
    (= 0 (cur s)))                             ; ... and DECSTBM homes
 _ (scribe s "\e[4;1H\n")                      ; \n at the region's bottom margin
 _ (assert
    (= "top       " (rowtx s 0))               ; above the region: untouched
    (= "bb        " (rowtx s 1))               ; the region scrolled up
    (= "cc        " (rowtx s 2))
    (= "          " (rowtx s 3))
    (= "bot       " (rowtx s 4)))              ; below the region: untouched
 _ (scribe s "\e[2;1H\eM")                     ; RI at the region's top margin
 _ (assert
    (= "          " (rowtx s 1))               ; the region scrolled down
    (= "bb        " (rowtx s 2))
    (= "top       " (rowtx s 0)))
 _ (scribe s "\e[2S")                          ; SU by two
 _ (assert (= "cc        " (rowtx s 1)))
 _ (scribe s "\e[T")                           ; SD by one
 _ (assert (= "cc        " (rowtx s 2)))
 _ (scribe s "\e[r")                           ; the region opens back up
 _ (assert (= 0 (gaze s 4)) (= 4 (gaze s 5))))

; --- 10. scroll at the screen's own bottom (the classic teletype law) ---
(: s (mkscr 3 10)
 _ (scribe s "one\r\ntwo\r\nthree\r\n")
 _ (assert
    (= "two       " (rowtx s 0))               ; one scrolled off the top
    (= "three     " (rowtx s 1))
    (= "          " (rowtx s 2))
    (= 20 (cur s))))

; --- 11. SGR: colours, faces, resets ---
(: s (mkscr 2 12)
 _ (scribe s "\e[31;44;1mx")
 _ (assert
    (= 1 (cfg s 0)) (= 4 (cbg s 0)) (= 1 (cface s 0)))   ; red on blue, bold
 _ (scribe s "\e[4;7my")
 _ (assert (= 7 (cface s 1)))                            ; + underline + reverse
 _ (scribe s "\e[22;27mz")
 _ (assert (= 2 (cface s 2)))                            ; bold and reverse off, underline holds
 _ (scribe s "\e[mw")
 _ (assert (= 7 (cfg s 3)) (= 0 (cbg s 3)) (= 0 (cface s 3)))   ; SGR 0: the floor
 _ (scribe s "\e[92ma\e[38;5;196mb\e[38;2;255;0;0mc")
 _ (assert
    (= 10 (cfg s 4))                          ; bright green: the high half
    (= 196 (cfg s 5))                         ; 256-colour, straight index
    (= 196 (cfg s 6)))                        ; truecolour red lands on the cube's 196
 _ (scribe s "\e[48;5;238;39mq")
 _ (assert (= 238 (cbg s 7)) (= 7 (cfg s 7))))

; --- 12. save/restore (ESC 7/8 and CSI s/u carry cursor + pen) ---
(: s (mkscr 4 10)
 _ (scribe s "\e[2;3H\e[35m\e7")
 _ (scribe s "\e[4;9H\e[m\e[42m")
 _ (scribe s "\e8x")
 _ (assert
    (= 120 (chat s 12))                       ; restored to 2;3 and stamped there
    (= 5 (cfg s 12))                          ; ... with the saved magenta pen
    (= 0 (cbg s 12)))                         ; ... not the green that came after
 _ (scribe s "\e[1;1H\e[s\e[3;3H\e[u")
 _ (assert (= 0 (cur s))))

; --- 13. modes: cursor visibility, origin ---
(: s (mkscr 6 10)
 _ (assert (= 1 (& 1 (gaze s 3))))
 _ (scribe s "\e[?25l")
 _ (assert (= 0 (& 1 (gaze s 3))))            ; hidden: the flag renderers read
 _ (scribe s "\e[?25h")
 _ (assert (= 1 (& 1 (gaze s 3))))
 _ (scribe s "\e[3;5r\e[?6h")                 ; region rows 3..5, origin on
 _ (assert (= 20 (cur s)))                    ; home = the region's top
 _ (scribe s "\e[2;1H")
 _ (assert (= 30 (cur s)))                    ; row 2 = one under the region top
 _ (scribe s "\e[9;1H")
 _ (assert (= 40 (cur s)))                    ; clamped at the region's bottom
 _ (scribe s "\e[?6l\e[r"))

; --- 14. the one-grid alternate screen ---
(: s (mkscr 3 10)
 _ (scribe s "ground\e[35m")
 _ (scribe s "\e[?1049h")
 _ (assert
    (= 0 (chat s 0))                          ; a clean page for the full-screen guest
    (= 0 (cur s)))
 _ (scribe s "guest")
 _ (scribe s "\e[?1049l")
 _ (assert
    (= 0 (chat s 0))                          ; the ground is NOT repainted (one grid,
    (= 6 (cur s))                             ;  the honest price) -- but the cursor
    (= 5 (cfg s 8))))                         ;  and pen come back as saved

; --- 15. replies: DSR, DA, and the drain-once law ---
(: s (mkscr 4 10)
 _ (scribe s "\e[3;7H\e[6n")
 _ (assert (= [27 91 51 59 55 82] (reply s))     ; "\e[3;7R"
           (! (reply s)))                        ; drained: quiet
 _ (scribe s "\e[c")
 _ (assert (= [27 91 63 54 99] (reply s)))       ; "\e[?6c", a VT102
 _ (scribe s "\eZ")
 _ (assert (= [27 91 63 54 99] (reply s)))
 _ (scribe s "\e[5n")
 _ (assert (= [27 91 48 110] (reply s))))        ; "\e[0n": all well

; --- 15b. the asks a line editor waits on: colour queries, secondary DA ---
; (silence here cost a real second of buffered keystrokes at zsh startup)
(: s (mkscr 4 20)
 _ (scribe s "\e]11;?\x07")
 _ (assert (= [27 93 49 49 59 114 103 98 58 48 48 48 48 47 48 48 48 48 47 48 48 48 48 7]
              (reply s)))                        ; bg: rgb:0000/0000/0000, BEL-terminated
 _ (scribe s "\e]10;?\e\\")                      ; ST-terminated ask, same answer lane
 _ (assert (= [27 93 49 48 59 114 103 98 58 99 48 99 48 47 99 48 99 48 47 99 48 99 48 7]
              (reply s)))                        ; fg: rgb:c0c0/c0c0/c0c0
 _ (scribe s "\e]0;just a title\x07")
 _ (assert (! (reply s)))                        ; a title is not a question
 _ (scribe s "\e[>c")
 _ (assert (= [27 91 62 48 59 48 59 48 99] (reply s)))     ; secondary DA
 _ (scribe s "\e[c")
 _ (assert (= [27 91 63 54 99] (reply s))))      ; primary DA unchanged

; --- 16. what must be swallowed whole ---
(: s (mkscr 2 10)
 _ (scribe s "\e]0;a title\x07x")                ; OSC to BEL (\x07 -- \a is no reader escape)
 _ (assert (= 120 (chat s 0)))
 _ (scribe s "\e]2;more\e\\y")                   ; OSC to ST
 _ (assert (= 121 (chat s 1)))
 _ (scribe s "\e(B\e)0\e=\e>z")                  ; charsets + keypad chatter
 _ (assert (= 122 (chat s 2)))
 _ (scribe s "\e[?2004h\e[>1c\e[0 q!")           ; odd modes, intermediates: junked
 _ (assert (= 33 (chat s 3)))
 _ (scribe s "a\x7fb\e[\x7f1D")                  ; DEL: nothing, anywhere (mid-CSI too)
 _ (assert (= 97 (chat s 4)) (= 98 (chat s 5)) (= 5 (cur s))))

; --- 17. DECALN: the adjustment screen ---
(: s (mkscr 3 8)
 _ (scribe s "\e[2;3r\e#8")
 _ (assert
    (= 69 (chat s 0)) (= 69 (chat s 23))         ; E corner to corner
    (= 0 (gaze s 4)) (= 2 (gaze s 5))            ; region reopened
    (= 0 (cur s))))

; --- 17b. utf-8 folds onto the cp437 the fonts carry ---
(: s (mkscr 4 20)
 _ (scribe s "\xc3\xa9")                          ; é
 _ (assert (= 130 (chat s 0)))                    ; -> cp437 0x82
 _ (scribe s "\xe2\x94\x80\xe2\x94\x82\xe2\x94\x8c\xe2\x95\x90\xe2\x95\x94")   ; ─ │ ┌ ═ ╔
 _ (assert (= 196 (chat s 1)) (= 179 (chat s 2)) (= 218 (chat s 3))
           (= 205 (chat s 4)) (= 201 (chat s 5)))
 _ (scribe s "\xe2\x95\xad")                      ; ╭ rounded aliases the ┌
 _ (assert (= 218 (chat s 6)))
 _ (scribe s "\xe2\x96\x91\xe2\x96\x88")          ; ░ █
 _ (assert (= 176 (chat s 7)) (= 219 (chat s 8)))
 _ (scribe s "\xce\xb1\xe2\x88\x9e\xc2\xb0")      ; α ∞ °
 _ (assert (= 224 (chat s 9)) (= 236 (chat s 10)) (= 248 (chat s 11)))
 _ (scribe s "\xf0\x9f\x98\x80")                  ; an emoji: one ■, one cell
 _ (assert (= 254 (chat s 12)) (= 13 (cur s)))
 _ (scribe s 226) _ (scribe s 148)                ; a sequence split across feeds
 _ (scribe s 144)                                 ; ... still one ┐
 _ (assert (= 191 (chat s 13)))
 _ (scribe s "\x80")                              ; a stray continuation: the ■
 _ (assert (= 254 (chat s 14)))
 _ (scribe s "\xc3\e[Dx")                         ; torn mid-sequence: a ■ lands,
 _ (assert (= 120 (chat s 15))                    ; the ESC rides on -- CUB steps
           (= 16 (cur s))))                       ; back and x overwrites the ■

; --- 18. RIS: back to the floor ---
(: s (mkscr 3 8)
 _ (scribe s "\e[31;44;7mxyz\e[2;2r\e[?25l\e[?7l")
 _ (scribe s "\ec")
 _ (assert
    (= 0 (chat s 0))
    (= 0 (cur s))
    (= 3 (& 3 (gaze s 3)))                       ; shown + wrapping again
    (= 0 (gaze s 4)) (= 2 (gaze s 5)))
 _ (scribe s "w")
 _ (assert (= 7 (cfg s 0)) (= 0 (cface s 0))))

; --- 19. wet: every write names its rows; a read clears the ledger ---
(: s (mkscr 4 10)
 _ (assert (= 15 (& 15 (wet s 0))))       ; fresh: every row dirty
 _ (assert (= 0 (wet s 0)))               ; read-and-cleared
 _ (scribe s "\e[3;1Hx")
 _ (assert (= 4 (wet s 0)))               ; row 2 alone
 _ (scribe s "hi\nyo")
 _ (assert (= 12 (wet s 0)))              ; rows 2 and 3 (\n keeps the column)
 _ (scribe s "\e[2J")
 _ (assert (= 15 (wet s 0)))              ; erase display: everything
 _ (scribe s "\e[1;5r\e[4;1H\n")             ; a region scroll dirties the region
 _ (wet s 0)
 _ (scribe s "\e[2;1H\e[L")
 _ (assert (= 14 (& 14 (wet s 0)))))      ; IL at row 1: rows 1..3

(test_fin ())
(puts "cb: ok")
