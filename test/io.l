; port nifs taking an explicit port: get, unget, end?, put, say, flush -- siblings of the
; static-port (getc/putc/key?) nifs that dispatch via f->io. open/close round out the
; surface for heap-allocated stream ports backed by real fds.

(assert
  ; in and out are heap-shaped (lvm_port_io), so show renders a hex address
  (string? (show in))
  (string? (show out))
  (< 0 (sat (show in)))
  (< 0 (sat (show out)))

  ; unget + get roundtrip on stdin -- one top-level form, or the repl eats the pushback
  (: (rt c) (: _ (unsee in c) (see in))
   (do (= 65  (rt 65))
      (= 32  (rt 32))
      (= 255 (rt 255))))

  ; the same pair once a real read has happened, which the net-zero roundtrip cannot
  ; reach. on a tap, never on `in`: a real read of `in` eats the corpus feeding this
  ; suite. the pushback is the port's (love.c's zungetc), so a tap walks identical code.
  (: t (tap '(120 121))         ; a tap taps a charlist -- (tap "xy") is the empty one
   _ (see t)
   (do (= 99 (unsee t 99))
      (= 99 (see t))
      (= 121 (see t))           ; ..and y is still there, unshifted by the detour
      (= -1 (see t))))          ; ..and the end is still the end

  ; a non-port reads as an already-empty stream: (get x) is -1, not the echoed x, so a
  ; read-until-(-1) loop over an absent port terminates. this is what bounds the file
  ; roundtrip below where `open` is a missing name resolving to a non-port.
  (= -1 (see 42))
  (= -1 (see "notaport"))

  ; port? answers by the head's ap word, so every port shape passes and every non-port
  ; is () -- hot? and lit? alone cannot tell a port from its neighbours in the zoo.
  (port? in)
  (port? out)
  (port? (tap "x"))
  (port? (jug 0))
  (! (port? 42))
  (! (port? "notaport"))
  (! (port? ()))
  (! (port? (\ x x)))
  (! (port? (cask 4)))
  ; slurp is bounded like see: a non-port drains as the empty stream rather than reading
  ; the raw words of whatever it was handed. every drain lands text.
  (= "" (slurp 42))   (string? (slurp 42))
  (= "" (slurp ()))
  (id? (slurp (tap "")) (slurp 42))

  ; file roundtrip: say a string to /tmp, read it back byte-by-byte, compare. the payload
  ; is (show (clock 0)) so a stale file from a previous run cannot pass. gated to hosted
  ; builds -- the wasm shim stubs fd ports to EOF, so a real file cannot round-trip.
  (? word (: txt (show (clock 0))
     wp (open "/tmp/l-io-test" "w")
     _ (say wp txt)
     _ (close wp)
     rp (open "/tmp/l-io-test" "r")
     (slurp acc) (: c (see rp) (? (= c -1) (rev acc) (slurp (link c acc))))
     rd (string (slurp 0))
     _ (close rp)
     (= txt rd)) 1)

  ; --- chug and unchug: the run, and the run put back ---------------------------
  ; unchug rewinds rpos, which is at once the reader's position and what ai_io_pending
  ; counts, so bytes given back are bytes stdin's seek-back sees again.
  ; on a file, never on `in`: a real read of `in` eats the corpus feeding this suite.
  (? word (: pth "/tmp/l-io-unchug"
     wp (open pth "w") _ (say wp "abcdefghij") _ (close wp)
     p (open pth "r")
     c (see p) _ (unsee p c)
     whole (chug p)                     ; the pushback byte, then the whole run
     back4 (unchug p 4)
     tail4 (chug p)                     ; ..and the run is those four, unshifted
     backall (unchug p 99)              ; an over-ask clamps to what was drained
     again (chug p)
     back10 (unchug p 99)
     fwd3 (unchug p -3)                 ; a negative take steps the position forward
     rest7 (chug p)
     _ (close p)
     ; && and never `do` -- do is seqx, so it would answer the last clause alone
     (&& (= "abcdefghij" whole)
         (= 4 back4)    (= "ghij" tail4)
         (= 10 backall) (= "abcdefghij" again)
         (= 10 back10)  (= 3 fwd3)      (= "defghij" rest7))) 1)
  ; nothing to give back is 0, never a scare -- and a short answer is the refusal, so the
  ; count is the only check a caller has. a tap carries the ci vt and never a run.
  (= 0 (unchug (tap '(120 121)) 4))
  (= 0 (unchug 42 4))
  (= 0 (unchug "notaport" 4))
  (= 0 (unchug in 0))                   ; the count refuses before the port is touched
  ; never a nonzero unchug of `in` here: the door is signed, so a negative moves stdin
  ; forward -- and this suite is fed through stdin on one of its doors, where that eats
  ; its own source and reads a comment as code. a run-less port is the safe way to ask.
  (= 0 (unchug (tap '(120 121)) -2))

  ; --- tap: charlist-backed read-only port (the ci vt) ---
  (string? (show (tap 0)))
  (< 0 (sat (show (tap 0))))
  ; each call allocates a fresh port object
  (: p (tap 0) q (tap 0) !(= p q))
  ; a tap is a byte stream, so a charm outside 0..255 lands as its low byte. that is what
  ; stops a -1 in the list forging the end of the stream: readers that stop at -1 lose
  ; the whole list to one element.
  (: p (tap '(300 -1 65))
     (&& (= 44 (see p)) (= 255 (see p)) (= 65 (see p)) (= -1 (see p))))
  ; counted by forcing, never by tally: a flow is lazy past the first run and a tap has
  ; no buffered run, so tally sees one cell and a promise. the law is that both arrive.
  (: (fw cl n) (? (two? cl) (fw (: t (cup cl) (? (lit? t) (t ()) t)) (n + 1)) n)
     (2 = (fw (flow (tap '(-1 65))) 0)))

  ; --- once: the promise ---
  ; a thunk that runs at most once and remembers, fired with the unit per the
  ; zero-operand law -- (th) alone is th unrun. the law is the count, not the agreement:
  ; two firings that agree prove nothing, so the witness is a tablet the body bumps.
  (: c (tablet 0)
     th (once (\ u (: k (peep c 0 0) _ (pin c 0 (k + 1)) k)))
     a (th ()) b (th ()) d (th ())
     (&& (= a b) (= b d) (= 1 (peep c 0 0))))
  ; a promise of nothing still forces once -- a flow's last promise answers () at eof.
  ; presence-as-the-net fails here: a bare pin of () leaves the slot reading exactly like
  ; an unforced one, so f re-runs forever, silently.
  (: c (tablet 0)
     th (once (\ u (: _ (pin c 0 ((peep c 0 0) + 1)) ())))
     _ (th ()) _ (th ())
     (&& (nil? (th ())) (= 1 (peep c 0 0))))

  ; --- flow: a port is a charlist ---
  ; one whose tail is a promise drinking the next gulp, so `sound` reads a port with no
  ; one turning it into text first.
  (: p (tap '(40 49 32 50 41))                          ; "(1 2)"
     r (sound (flow p))
     (&& (two? r) (= '(1 2) <r)))
  ; the gulp is realized and only the join is lazy: one run's cells are ordinary conses,
  ; and a flowing charlist ends in a promise or in (). the run is what the port has in
  ; hand -- a buffered fd port hands over its 4096 the same way (test/front/io.l).
  (: f (flow (tap '(97 98)))
     (&& (= 97 <f) (= 98 <>f) (|| (lit? >>f) (nil? >>f))))
  ; an empty port is the empty charlist, not a promise of one
  (nil? (flow (tap ())))
  ; the memoization law: a position forced twice answers the same thing, so the flow's
  ; tail is a `once` and not a bare (\ u (flow p)) -- p1 re-forces on purpose, and a
  ; re-drinking tail loses the bytes between, a misread rather than a crash. the witness
  ; has to be a port that still has something to give: two forces that both say () prove
  ; nothing, so the unget puts a byte back after the first gulp.
  (: p (tap '(97)) f (flow p) t >f
     _ (unsee p 98)
     x (t ()) y (t ())
     ; x is compared by identity, not by `=`: its own tail is the next promise, so
     ; (= '(98) x) is false -- the printer shows it as (98) because it stops at a tail
     ; that is not a cons, the one place a flow does not look like a list.
     (&& (lit? t) (&& (= 98 <x) (id? x y))))

  ; --- sound: one datum per call, off text ---
  ; sound takes a charlist and answers three ways: the datum consed onto what is left,
  ; () at a clean end, or `torn` where the text ran out inside a shape. presence is the
  ; cons -- ask two?, never the net -- and the residue is the answer, so the caller owns
  ; what is left and nothing is stashed in a port.
  (nil? (sound ()))

  ; single datum: number, symbol, list, quoted form, string literal
  (= 1 (cap (sound '(49))))                             ; "1"
  (= 'a (cap (sound '(97))))                            ; "a"
  (: d (cap (sound '(40 49 32 50 41)))                  ; "(1 2)"
     (&& (two? d) (= 1 (cap d)) (= 2 (caup d))))
  (: d (cap (sound '(39 49)))                           ; "'1" -> (` 1)
     (&& (two? d) (= 1 (caup d))))
  (= (string (link 104 (link 105 0))) (cap (sound '(34 104 105 34))))   ; "hi"

  ; sequential reads thread the residue; the last one is the clean end
  (: a (sound '(49 32 50 32 51))                        ; "1 2 3"
     b (sound >a) c (sound >b)
     (&& (= 1 <a) (= 2 <b) (= 3 <c) (nil? (sound >c))))

  ; whitespace and ;-comments are skipped between datums
  (= 1 (cap (sound '(32 32 49 32 32))))                 ; "  1  "
  (= 2 (cap (sound '(59 32 99 109 116 10 50))))         ; "; cmt\n2"

  ; a bare close-paren at top level is a clean end
  (nil? (sound '(41)))                                  ; ")"

  ; --- torn: the text ran out inside a shape ---
  (id? 'torn (sound '(40)))                             ; unclosed list
  (id? 'torn (sound '(34 97)))                          ; unterminated string
  (id? 'torn (sound '(39)))                             ; dangling quote

  ; torn and the clean end are distinct, and neither is a datum: a datum comes back as a
  ; cons whatever it holds, so reading "()" answers the datum () -- exactly the case a
  ; net test would call nothing.
  (: r (sound '(40 41)) (&& (two? r) (nil? (cap r))))
  !(id? 'torn (sound ()))

  ; a torn read resumes: hand back the same text with the rest appended
  (: r (sound (cat '(40 49) '(32 50 41))) (&& (two? r) (= '(1 2) (cap r))))

  ; mixed sequence, residue threaded: "() 1"
  (: a (sound '(40 41 32 49)) b (sound >a)
     (&& (nil? <a) (= 1 <b) (nil? (sound >b))))

  ; an unforced tail is a charlist not yet drunk, so sound fires it and reads on. without
  ; this arm a residue arriving unforced would read as a clean end -- a short read,
  ; silently. the cost is that a closure handed to sound is called, not refused.
  (: r (sound (once (\ u (map "(1 2)" (jot 5)))))
     (&& (two? r) (= '(1 2) <r)))

  ; misuse: a non-charlist holds no datum, and reads as the clean end
  (nil? (sound 42))
  (nil? (sound 'notaport)))
