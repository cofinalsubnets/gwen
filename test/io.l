; Port nifs that take an explicit port: get, unget, end?, put, say,
; flush. Sibling forms of the static-port (getc/putc/key?) nifs that
; dispatch via f->io. open/close round out the surface for heap-allocated
; stream ports backed by real OS file descriptors.

(assert
  ; The static frontend ports are globally bound to `in` and `out`.
  ; They are heap-shaped (lvm_port_io discriminator), so the default
  ; printer renders them as a hex address — non-empty and string-typed.
  (string? (show in))
  (string? (show out))
  (< 0 (sat (show in)))
  (< 0 (sat (show out)))

  ; unget + get roundtrip on stdin (within a single top-level form,
  ; otherwise the REPL parser would consume the pushed-back byte itself).
  (: (rt c) (: _ (unsee in c) (see in))
   (do (= 65  (rt 65))
      (= 32  (rt 32))
      (= 255 (rt 255))))

  ; the same pair once a REAL read has happened, which the net-zero roundtrip above
  ; cannot reach: unget answers the byte, the byte is next, and the port resumes.
  ; ⚠ ON A TAP, NEVER ON `in` -- a real read of `in` eats a byte of whatever is
  ; feeding this suite, and on the host that is the corpus itself. nothing is given
  ; up by the swap: the pushback is the PORT's, one head word for every kind of port
  ; (love.c's zungetc), so a tap walks the identical code -- and it runs on the
  ; kernel too, where `in` is the keyboard and a quiet console parks forever.
  (: t (tap '(120 121))         ; ⚠ a tap taps a CHARLIST -- (tap "xy") is the empty one
   _ (see t)
   (do (= 99 (unsee t 99))
      (= 99 (see t))
      (= 121 (see t))           ; ..and y is still there, unshifted by the detour
      (= -1 (see t))))          ; ..and the end is still the end

  ; A non-port reads as an already-empty stream: (get x) is -1 (EOF), not the
  ; echoed x, so a read-until-(-1) loop over an absent or misused port terminates
  ; instead of spinning on a value that never equals -1. This is what keeps the
  ; file roundtrip below bounded on a host with no filesystem, where `open` is a
  ; missing name that resolves to a non-port.
  (= -1 (see 42))
  (= -1 (see "notaport"))

  ; The port question itself: `port?` answers by the head's ap word, so every
  ; port shape passes -- the static pair, a tap, a jug -- and every non-port
  ; is (), the closure and the buf included (hot? and lit? alone cannot tell
  ; a port from its neighbours in the zoo).
  (port? in)
  (port? out)
  (port? (tap "x"))
  (port? (jug 0))
  (! (port? 42))
  (! (port? "notaport"))
  (! (port? ()))
  (! (port? (\ x x)))
  (! (port? (cask 4)))
  ; and slurp is bounded like see: a non-port drains as the already-empty
  ; stream, so the zero point -- a missing or misused name -- slurps to the
  ; empty string, the same value an exhausted tap drains to, instead of reading
  ; the raw words of whatever it was handed. every drain lands text.
  (= "" (slurp 42))   (string? (slurp 42))
  (= "" (slurp ()))
  (id? (slurp (tap "")) (slurp 42))

  ; File-I/O roundtrip: write a string to /tmp via say, read it back
  ; byte-by-byte via get, rebuild as a string with str, and compare.
  ; The payload is (show (clock 0)) so the bytes vary every run --
  ; a literal string would let a stale file from a previous run pass.
  ; the wasm shim has no filesystem (fd ports are stubbed to EOF/out-buffer), so
  ; real-file I/O can't round-trip there; gated to the hosted builds. The read
  ; loop is bounded everywhere now (get on a non-port is EOF), so the stubbed
  ; path no longer OOMs. In-memory ports (tap, below) still run on wasm.
  (? word (: txt (show (clock 0))
     wp (open "/tmp/l-io-test" "w")
     _ (say wp txt)
     _ (close wp)
     rp (open "/tmp/l-io-test" "r")
     (slurp acc) (: c (see rp) (? (= c -1) (rev acc) (slurp (link c acc))))
     rd (string (slurp 0))
     _ (close rp)
     (= txt rd)) 1)

  ; --- tap: charlist-backed read-only port (the ci vt) ---
  ; tap returns a port whose show rendering is a non-empty string.
  (string? (show (tap 0)))
  (< 0 (sat (show (tap 0))))
  ; Each call allocates a fresh port object.
  (: p (tap 0) q (tap 0) !(= p q))
  ; ⚠ A TAP IS A BYTE STREAM like every other port, so a charm outside 0..255
  ; lands as its LOW BYTE. that is not a rounding convenience -- it is what stops
  ; a -1 in the list from FORGING the end of the stream. ci_getc used to hand the
  ; raw charm over, and the measured cost was not at `see` (which answered -1 and
  ; carried on) but at every reader that STOPS there: `(flow (tap '(-1 65)))`
  ; came back EMPTY, so sound/reads/slurp lost the whole list to one element.
  (: p (tap '(300 -1 65))
     (&& (= 44 (see p)) (= 255 (see p)) (= 65 (see p)) (= -1 (see p))))
  ; ⚠ COUNTED BY FORCING, never by `tally`: a flow is lazy past the first run and a tap has no
  ; buffered run at all, so tally sees ONE cell and a promise. the law is that both bytes ARRIVE.
  (: (fw cl n) (? (two? cl) (fw (: t (cup cl) (? (lit? t) (t ()) t)) (n + 1)) n)
     (2 = (fw (flow (tap '(-1 65))) 0)))

  ; --- once: the promise ---
  ; a thunk that runs at most once and remembers. fired with the UNIT, per the
  ; zero-operand law -- (th) alone is th unrun.
  ; ⚠ THE LAW IS THE COUNT, not just the agreement: two firings that agree prove
  ; nothing on their own (a pure f agrees anyway), so the witness is a tablet the
  ; body bumps.
  (: c (tablet 0)
     th (once (\ u (: k (peep c 0 0) _ (pin c 0 (k + 1)) k)))
     a (th ()) b (th ()) d (th ())
     (&& (= a b) (= b d) (= 1 (peep c 0 0))))
  ; ⚠ AND A PROMISE OF NOTHING STILL FORCES ONCE. this is the case that matters --
  ; a flow's last promise answers () at eof -- and it is where presence-as-the-net
  ; fails: a bare pin of () leaves the slot reading exactly like an unforced one,
  ; so f re-runs forever, silently.
  (: c (tablet 0)
     th (once (\ u (: _ (pin c 0 ((peep c 0 0) + 1)) ())))
     _ (th ()) _ (th ())
     (&& (nil? (th ())) (= 1 (peep c 0 0))))

  ; --- flow: a PORT IS A CHARLIST ---
  ; one whose tail is a promise that drinks the next gulp, so `sound` reads a port
  ; with no one turning it into text first.
  (: p (tap '(40 49 32 50 41))                          ; "(1 2)"
     r (sound (flow p))
     (&& (two? r) (= '(1 2) <r)))
  ; the gulp is REALIZED and only the join is lazy: the cells of one run are ordinary
  ; conses, and a flowing charlist ends in a promise or in ().
  ; the run is the PORT's, and it is what the port has in hand: a tap's charlist is
  ; already in memory, so both bytes come at once and the promise sits after them. a
  ; buffered fd port hands over its 4096 the same way (test/front/io.l walks a real one).
  (: f (flow (tap '(97 98)))
     (&& (= 97 <f) (= 98 <>f) (|| (lit? >>f) (nil? >>f))))
  ; an empty port is the empty charlist, not a promise of one.
  (nil? (flow (tap ())))
  ; ⚠ THE MEMOIZATION LAW: a position forced twice answers THE SAME THING, so the
  ; flow's tail is a `once` and not a bare (\ u (flow p)). p1 re-forces on purpose
  ; (the `#!` lookahead reads the tail three times in one expression, the \xNN lane
  ; twice), and a re-drinking tail loses the bytes between -- a MISREAD, never a
  ; crash. ⚠ the witness has to be a port that still has something to give: the
  ; unget puts a byte back AFTER the first gulp, so a second drink would swallow it
  ; and answer the clean end. two forces that both say () prove nothing.
  (: p (tap '(97)) f (flow p) t >f
     _ (unsee p 98)
     x (t ()) y (t ())
     ; ⚠ and x is compared by IDENTITY, not by `=`: its own tail is the next promise,
     ; so (= '(98) x) is FALSE -- the printer shows it as (98) because it stops at a
     ; tail that is not a cons, which is the one place a flow does not look like a list.
     (&& (lit? t) (&& (= 98 <x) (id? x y))))

  ; --- sound: one datum per call, off TEXT. ---
  ; sound takes a CHARLIST and answers three ways: the datum CONSED ONTO WHAT IS
  ; LEFT, () at a clean end, or `torn` where the text ran out inside a shape.
  ; presence is the CONS -- ask two?, never the net -- and the residue is the
  ; answer, so the caller owns what is left and nothing is stashed in a port.
  ;
  ; Empty input: a clean end.
  (nil? (sound ()))

  ; Single datum: number, symbol, list, quoted form, string literal.
  (= 1 (cap (sound '(49))))                             ; "1"
  (= 'a (cap (sound '(97))))                            ; "a"
  (: d (cap (sound '(40 49 32 50 41)))                  ; "(1 2)"
     (&& (two? d) (= 1 (cap d)) (= 2 (caup d))))
  (: d (cap (sound '(39 49)))                           ; "'1" -> (` 1)
     (&& (two? d) (= 1 (caup d))))
  (= (string (link 104 (link 105 0))) (cap (sound '(34 104 105 34))))   ; "hi"

  ; Sequential reads: THREAD THE RESIDUE. the last one is the clean end.
  (: a (sound '(49 32 50 32 51))                        ; "1 2 3"
     b (sound >a) c (sound >b)
     (&& (= 1 <a) (= 2 <b) (= 3 <c) (nil? (sound >c))))

  ; Whitespace and ;-comments are skipped between datums.
  (= 1 (cap (sound '(32 32 49 32 32))))                 ; "  1  "
  (= 2 (cap (sound '(59 32 99 109 116 10 50))))         ; "; cmt\n2"

  ; A bare close-paren at top level is a clean end.
  (nil? (sound '(41)))                                  ; ")"

  ; --- torn: the text ran out INSIDE a shape ---
  (id? 'torn (sound '(40)))                             ; unclosed list
  (id? 'torn (sound '(34 97)))                          ; unterminated string
  (id? 'torn (sound '(39)))                             ; dangling quote

  ; ⚠ torn and the clean end are DISTINCT, and NEITHER IS A DATUM: a datum comes
  ; back as a cons whatever it holds, so reading "()" answers the datum () --
  ; which is exactly the case a net test would call nothing.
  (: r (sound '(40 41)) (&& (two? r) (nil? (cap r))))
  !(id? 'torn (sound ()))

  ; A torn read RESUMES: hand back the same text with the rest appended.
  (: r (sound (cat '(40 49) '(32 50 41))) (&& (two? r) (= '(1 2) (cap r))))

  ; Mixed sequence, residue threaded: "() 1"
  (: a (sound '(40 41 32 49)) b (sound >a)
     (&& (nil? <a) (= 1 <b) (nil? (sound >b))))

  ; an unforced tail is a charlist that has not been drunk yet, so sound fires it and reads
  ; on. without this arm a residue arriving unforced would read as a clean end -- a short
  ; read, silently. the cost is that a closure handed to sound is CALLED, not refused.
  (: r (sound (once (\ u (map "(1 2)" (jot 5)))))
     (&& (two? r) (= '(1 2) <r)))

  ; Misuse: a non-charlist holds no datum, and reads as the clean end.
  (nil? (sound 42))
  (nil? (sound 'notaport)))
