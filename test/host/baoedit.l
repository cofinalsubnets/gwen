; --- test/host/baoedit.l: a smoke test for bao's raw-line editor (love/bao.l `edraw`) ---
; edraw lives in love/bao.l, now the BAKED shell core (in the host egg), so edraw is
; a book global -- but it reads love forms (it's the editor), so it can't be a corpus
; test (love0 bakes the corpus and would run edraw on the test stream). edraw reads
; from a PORT ARG (here a (tap ..), not stdin), so the bare stdin runner drives it:
; the piped forms read-eval off stdin (a pipe is the bare filter) while each edraw
; consumes its own sip. Run (assert comes from test/00-init.l):
;
;   cat test/00-init.l test/host/baoedit.l | out/host/love
;
; green dots = pass; the shell's banner/prompts + edraw's ANSI redraws are noise
; around them. Full interactive editing (real keystrokes, real tty) stays manual.

; (keys l) = a sip over the byte list l -> an input port edraw reads keystroke by
; keystroke. event bytes: 13 = Enter; 4 = ^D; 127 = backspace; ESC [ D/C/H/F =
; left/right/home/end. abc = the charlist "abc", the target of every edit.
; eof = the sentinel edraw returns at ^D / port EOF: captured by READING it back
; off an empty port (the sentinel is bao-private now -- the editor no longer leaks
; its `eofsym` global, so we name it by what edraw produces, not by its binding).
(: edraw (from 'bao 'edraw)              ; the editor primitives ride the bao module now
   edln (from 'bao 'edln)
   (keys l) (tap l)
   abc `(97 98 99)
   eof (edraw (keys 0) ""))

; submit, edit, and EOF -- all returning the RAW bytes (no parseall), prompt "".
(: _ (assert
   ; plain: a b c Enter -> "abc"
   (= abc (edraw (keys `(97 98 99 13)) ""))
   ; insert mid-line: a c, LEFT, b, Enter -> "abc"
   (= abc (edraw (keys `(97 99 27 91 68 98 13)) ""))
   ; backspace: a b x <bsp> c Enter -> "abc"
   (= abc (edraw (keys `(97 98 120 127 99 13)) ""))
   ; HOME then insert: b c, HOME, a, Enter -> "abc"
   (= abc (edraw (keys `(98 99 27 91 72 97 13)) ""))
   ; END after HOME: a, HOME, END, b c, Enter -> "abc" (END returns to line tail)
   (= abc (edraw (keys `(97 27 91 72 27 91 70 98 99 13)) ""))
   ; ^D -> the EOF sentinel (bao-private), never a stray line
   (id? eof (edraw (keys `(4)) ""))
   ; port EOF (empty input) -> the same sentinel
   (id? eof (edraw (keys 0) ""))))

; --- edln: edraw PLUS command history. returns (line . nhist), or 'eof at EOF
; (a public symbol, not edraw's private sentinel). same editing primitives; up/
; down (ESC [ A / ESC [ B) recall through hist (past lines, most-recent first).
(: _ (assert
   ; the editing primitives carry over -- the line is now (cap r):
   (= abc (cap (edln (keys `(97 98 99 13)) "" ())))              ; a b c -> "abc"
   (= abc (cap (edln (keys `(97 99 27 91 68 98 13)) "" ())))     ; a c, LEFT, b -> "abc"
   (= abc (cap (edln (keys `(97 98 120 127 99 13)) "" ())))      ; a b x <bsp> c -> "abc"
   (id? 'eof (edln (keys `(4)) "" ()))                           ; ^D -> the 'eof sentinel
   (id? 'eof (edln (keys 0) "" ()))                              ; port EOF -> the same
   ; a submitted line is pushed onto history (most-recent first); blanks dropped:
   (= (L abc) (cup (edln (keys `(97 98 99 13)) "" ())))          ; nhist on empty -> (abc)
   (= (L abc abc) (cup (edln (keys `(97 98 99 13)) "" (L abc)))) ; ... pushed on front
   (nil? (cap (edln (keys `(13)) "" (L abc))))                   ; blank line -> ()
   (= (L abc) (cup (edln (keys `(13)) "" (L abc))))              ; ... NOT recorded
   ; up recalls the most-recent entry; down past the draft is the empty draft:
   (= abc (cap (edln (keys `(27 91 65 13)) "" (L abc))))         ; UP, enter -> "abc"
   (nil? (cap (edln (keys `(27 91 66 13)) "" (L abc))))          ; DOWN (no newer) -> ""
   ; the live draft survives an up/down round-trip:
   (= `(120) (cap (edln (keys `(120 27 91 65 27 91 66 13)) "" (L abc))))))  ; x, UP, DOWN -> "x"

(test_fin ())
(puts "baoedit: ok") (putc 10)
