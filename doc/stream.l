; --- doc/stream.l: the coinductive byte-stream, the PURE HALF, in love ---
; a runnable design companion to doc/io.md: it models the algebra over an
; in-memory charlist producer, so it runs ANYWHERE (no fds, no pty) and pins the
; shape the core session builds against. Run (assert comes from test/00-init.l):
;
;   cat test/00-init.l doc/stream.l | out/host/love
;
; green dots = pass, then `stream: ok`. What it demonstrates:
;   * a stream forces its head ONCE and MEMOIZES it -- the memo IS the lookahead,
;     which is why nothing needs an `ungetc`;
;   * EOF is the empty stream `()`, never a -1 byte -- a 0/NUL byte is a real
;     value, told from end-of-stream by `seof?`, never by its magnitude;
;   * `read : stream -> () | (datum . stream')` as a PURE fold: EOF folds to (),
;     a datum to a pair, and a mid-datum EOF is a REAL scare (not the () floor) --
;     which is the whole more-bit / port-back read protocol, collapsed;
;   * `select`/`ready?` -- the multiplex primitive, trivial here because a memory
;     stream is always ready (over fds it projects onto poll).
;
; surface map to the C design (doc/io.md): shd ~ cap, stl ~ cup, seof? ~ ! (the
; stream tests empty), and (twop s) ~ !(seof? s).

(: forces (tablet 0)                       ; a global counter: how many times ANY
   _ (pin forces 0 0)                       ; stream cell was actually produced
   (bump _) (pin forces 0 (+ 1 (peep forces 0 0)))   ; takes a dummy: love has no nullary calls

   ; --- a stream is a BOX over a producer thunk, with a force-once memo ---
   ; 'p = the producer (\ _ -> () | (head . tail-stream)); 'f = forced? ; 'v = memo.
   (strm p) (: b (tablet 0) _ (pin b 'p p) b)
   (force s) (? (peep s 'f 0) (peep s 'v 0)            ; already forced -> the memo
                (: v ((peep s 'p 0) 0)                  ; else run the thunk ONCE,
                   _ (pin s 'v v) _ (pin s 'f 1) v))    ; memoize, and return it

   ; the projections -- forcing once, reading the memo thereafter. shd/stl are only
   ; valid on a non-empty stream (guard with seof?), exactly like cap/cup on a list.
   ; EOF = the empty stream = (); a non-empty force is a (head . tail) chain. we test
   ; that STRUCTURALLY (two?), not with `!`: a raw chain nets by its BYTES (a (0 .
   ; tail) nets 0, and the tail box is a dotted tail net excludes), so `!` would
   ; misread a 0-byte head as empty. in C this subtlety vanishes -- a `source` hot is
   ; present-by-construction (nets truthy), so there `! source` IS the emptiness test.
   (seof? s) (! (two? (force s)))         ; force is a chain iff the stream has a byte
   (shd s)   (cap (force s))                ; the head byte (a charm; 0 is a real byte)
   (stl s)   (cup (force s))                ; the tail stream (a fresh stream box)

   ; --- producers ---
   ; (sip-l l): a stream over a byte LIST. each tail is a FRESH unforced (sip-l ..),
   ; so the spine is built lazily and `bump` counts only genuine productions.
   (sip-l l) (strm (\ _ (: _ (bump 0)
                           (? (two? l) (link (cap l) (sip-l (cup l))) ()))))
   (chars s)    (map s (jot (tally s)))     ; "ab" -> (97 98) (a string indexes bytes)
   (strm-str s) (sip-l (chars s))           ; a stream straight off a string literal

   ; (drain s): read a stream to EOF, collecting its bytes. terminates on the ()
   ; floor -- no -1 sentinel, no "while ch != EOF". the pump's pure shape.
   (drain s) (? (seof? s) () (link (shd s) (drain (stl s))))

   ; --- the reader, as a pure stream -> () | (datum . stream') fold ---
   ; a tiny grammar (ws-separated non-negative ints + parenthesized lists) -- enough
   ; to show the SHAPE the C reader takes; the full datum surface is the same fold.
   (digit? c) (&& (<= 48 c) (<= c 57))
   (skipws s) (? (seof? s) s (= 32 (shd s)) (skipws (stl s)) s)
   (read-int s acc)
     (? (&& !(seof? s) (digit? (shd s)))
        (read-int (stl s) (+ (* 10 acc) (- (shd s) 48)))
        (link acc s))                        ; (number . stream')
   (read1 s0)
     (: s (skipws s0)
        (? (seof? s)         ()              ; EOF -> the floor, in-band as a value
           (digit? (shd s))  (read-int s 0)
           (= 40 (shd s))    (read-lst (stl s) 0)   ; '(' -> a list
           (scare 'badchar (shd s))))
   (read-lst s acc)                          ; read until ')'; acc is reversed
     (: t (skipws s)
        (? (seof? t)      (scare 'eofmid 0)  ; mid-datum EOF = a REAL scare, NOT ()
           (= 41 (shd t)) (link (rev acc) (stl t))  ; ')' -> (list . stream')
           (: r (read1 t)                    ; r = (datum . stream'')
              (read-lst (cup r) (link (cap r) acc)))))

   ; --- the one genuinely-new primitive (doc/io.md part II, `select`) ---
   ; a memory stream is ALWAYS ready (nothing to block on); over an fd, ready?
   ; projects onto ai_ready and select onto ai_wait_fds. select returns the first
   ; ready (here: non-empty) stream, or () if every stream is at EOF.
   (ready? _) 1
   (select ss) (? (two? ss)
                  (? (seof? (cap ss)) (select (cup ss)) (cap ss))
                  ()))

; --- demos (expr ; value) ---
(drain (strm-str "hi"))                  ; (104 105)   read to the () floor, no -1
(cap (read1 (strm-str "42 x")))          ; 42          a datum + the rest-stream
(cap (read1 (strm-str "(1 2 3)")))       ; (1 2 3)     a list, parsed by the fold
(cap (read1 (strm-str "(1 (2 3) 4)")))   ; (1 (2 3) 4) nested -- read1 recurses
(read1 (strm-str ""))                    ; ()          empty input folds to the floor
(shd (sip-l `(0 65)))                   ; 0           a NUL byte is a real value...
(seof? (sip-l `(0 65)))                 ; 0           ...told from EOF by seof?, not -1
(ready? (strm-str "x"))                  ; 1           a memory stream is always ready

; --- the gate ---
(: _ (assert
   ; drain hits the () floor (no sentinel) and round-trips the bytes
   (= (chars "stream") (drain (strm-str "stream")))
   ; the pure read fold: a number, then a list, both with their rest-stream
   (= 42 (cap (read1 (strm-str "42 rest"))))
   (= `(1 2 3) (cap (read1 (strm-str "(1 2 3)"))))
   (= `(1 `(2 3) 4) (cap (read1 (strm-str "(1 (2 3) 4)"))))
   ; reading the rest: "1 2" -> 1, then the stream' still yields 2
   (= 2 (cap (read1 (cup (read1 (strm-str "1 2"))))))
   ; EOF folds to the floor () -- not a byte, not a raise
   (! (read1 (strm-str "")))
   (! (read1 (strm-str "   ")))           ; all whitespace -> EOF too
   ; a 0/NUL byte is a real head, distinct from end-of-stream
   (= 0 (shd (sip-l `(0 65))))
   (! (seof? (sip-l `(0 65))))
   (seof? (sip-l 0))                      ; the empty list IS the empty stream
   ; select: skip the spent stream, return the ready one; () when all are spent
   (: a (strm-str "x") (id? a (select `((sip-l 0) a))))
   (! (select `((sip-l 0) (strm-str ""))))))

; memoization: force the same stream's head twice -> the producer runs ONCE.
; this is the lookahead -- a held tail is a peeked byte, no ungetc, no re-read.
(: _ (pin forces 0 0)
   s (strm-str "AB")
   h1 (shd s) h2 (shd s)                  ; force the head, then read it again
   (assert (= 65 h1) (= 65 h2) (= 1 (peep forces 0 0))))

; mid-datum EOF is a REAL scare (the () floor is for a CLEAN end only). install a
; help that hands back the condition tag, so an unterminated list surfaces 'eofmid
; instead of terminating; a well-formed read never trips it.
(hear (\ s a b a))
(: _ (assert (id? 'eofmid (read1 (strm-str "(1 2")))   ; "(1 2" with no ')' -> scare
             (= `(1 2) (cap (read1 (strm-str "(1 2)")))))) ; closed -> clean datum
(hear 0)                               ; uninstall (hygiene; back to the floor handler)

(puts "stream: ok") (putc 10)
