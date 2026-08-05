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

  ; unget returns the byte it pushed.
  (: _ (see in)   ; pull and discard whatever's at the head
   (= 99 (unsee in 99)))
  ; ...and the pushed byte is then the next read.
  (= 99 (see in))

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
  ; unit, the same value an exhausted tap drains to, instead of reading the
  ; raw words of whatever it was handed.
  (id? () (slurp 42))
  (id? () (slurp ()))
  (id? (slurp (tap "")) (slurp 42))

  ; File-I/O roundtrip: write a string to /tmp via say, read it back
  ; byte-by-byte via get, rebuild as a string with str, and compare.
  ; The payload is (show (clock 0)) so the bytes vary every run --
  ; a literal string would let a stale file from a previous run pass.
  ; the wasm shim has no filesystem (fd ports are stubbed to EOF/out-buffer), so
  ; real-file I/O can't round-trip there; gated to the hosted builds. The read
  ; loop is bounded everywhere now (get on a non-port is EOF), so the stubbed
  ; path no longer OOMs. In-memory ports (sip, below) still run on wasm.
  (? word (: txt (show (clock 0))
     wp (open "/tmp/l-io-test" "w")
     _ (say wp txt)
     _ (close wp)
     rp (open "/tmp/l-io-test" "r")
     (slurp acc) (: c (see rp) (? (= c -1) (rev acc) (slurp (link c acc))))
     rd (string (slurp 0))
     _ (close rp)
     (= txt rd)) 1)

  ; --- sip: charlist-backed read-only synth port (fd=-4, ci) ---
  ; sip returns a port whose show rendering is a non-empty string.
  (string? (show (tap 0)))
  (< 0 (sat (show (tap 0))))
  ; Each call allocates a fresh port object.
  (: p (tap 0) q (tap 0) !(= p q))
  ; ⚠ A TAP IS A BYTE STREAM like every other port, so a charm outside 0..255
  ; lands as its LOW BYTE. that is not a rounding convenience -- it is what stops
  ; a -1 in the list from FORGING the end of the stream. ci_getc used to hand the
  ; raw charm over, and the measured cost was not at `see` (which answered -1 and
  ; carried on) but at every reader that STOPS there: a drain of (-1 65) came back
  ; EMPTY, so sound/reads/slurp lost the whole list to one element.
  (: p (tap '(300 -1 65))
     (&& (= 44 (see p)) (= 255 (see p)) (= 65 (see p)) (= -1 (see p))))
  (= 2 (tally (drink (tap '(-1 65)))))

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

  ; --- spout: a PORT IS A CHUNK SOURCE ---
  ; a promise of (text . next), () at eof, so `sound` reads a port with no one
  ; turning it into text first and no cell stepped per byte.
  (: p (tap '(40 49 32 50 41))                          ; "(1 2)"
     r (sound (spout p))
     (&& (two? r) (= '(1 2) <r)))
  ; the chunk is REALIZED text and only the join is lazy. ⚠ ITS SIZE IS THE PORT'S,
  ; not a promise the source makes: `chug` answers what is already readable, so a
  ; buffered fd gives its whole pending run and a tap -- which has no buffer at all
  ; -- gives the one pushback byte. both are correct; only the granularity differs.
  (: s (spout (tap '(97 98)))
     g (s ())
     (&& (two? g) (&& (= "a" <g) (lit? >g))))
  ; an empty port's source answers () on its first force -- no chunk, and never an
  ; EMPTY one (a reader would pull on that forever).
  (nil? ((spout (tap ())) ()))
  ; ⚠ THE MEMOIZATION LAW: a source forced twice answers THE SAME THING, so spout's
  ; next is a `once` and not a bare (\ u (spout p)). p1 re-forces on purpose (the
  ; `#!` lookahead reads the tail three times in one expression, the \xNN lane
  ; twice), and a re-drinking source loses the bytes between -- a MISREAD, never a
  ; crash. ⚠ the witness has to be a port that still has something to give: the
  ; unget puts a byte back AFTER the first chunk, so a second drink would swallow it
  ; and answer the clean end. two forces that both say () prove nothing.
  (: p (tap '(97)) s (spout p) g (s ()) t >g
     _ (unsee p 98)
     x (t ()) y (t ())
     (&& (lit? t) (&& (= "b" <x) (id? x y))))
  ; drip: spout's careful twin, one byte per force, so the source never runs ahead
  ; of the port -- what `in` takes, and why a form inside a piped script can read it.
  (: s (drip (tap '(97 98)))
     g (s ())
     (&& (= "a" <g) (= "b" (cap ((>g) ())))))

  ; --- sound: one datum per call, off TEXT. ---
  ; sound takes TEXT and answers three ways: the datum CONSED ONTO WHAT IS LEFT,
  ; () at a clean end, or `torn` where the text ran out inside a shape. presence is
  ; the CONS -- ask two?, never the net -- and the residue is the answer, so the
  ; caller owns what is left and nothing is stashed in a port.
  ; ⚠ THE RESIDUE IS A CURSOR, not text: it names a place in the text it came off,
  ; so threading it copies nothing. `spot` builds one, and past the end it is the
  ; zero point -- which reads as the clean end.
  ;
  ; Empty input: a clean end.
  (nil? (sound ""))
  (nil? (sound ()))

  ; Single datum: number, symbol, list, quoted form, string literal.
  (= 1 (cap (sound "1")))
  (= 'a (cap (sound "a")))
  (: d (cap (sound "(1 2)"))
     (&& (two? d) (= 1 (cap d)) (= 2 (caup d))))
  (: d (cap (sound "'1"))                               ; -> (` 1)
     (&& (two? d) (= 1 (caup d))))
  (= "hi" (cap (sound "\"hi\"")))

  ; Sequential reads: THREAD THE RESIDUE. the last one is the clean end.
  (: a (sound "1 2 3")
     b (sound >a) c (sound >b)
     (&& (= 1 <a) (= 2 <b) (= 3 <c) (nil? (sound >c))))

  ; ... and a `spot` starts one mid-text, which is the residue's own shape.
  (= 2 (cap (sound (spot "1 2 3" 2))))
  (nil? (sound (spot "1" 9)))                           ; past the end: the clean end

  ; Whitespace and ;-comments are skipped between datums.
  (= 1 (cap (sound "  1  ")))
  (= 2 (cap (sound "; cmt\n2")))

  ; A bare close-paren at top level is a clean end.
  (nil? (sound ")"))

  ; --- torn: the text ran out INSIDE a shape ---
  (id? torn (sound "("))                                ; unclosed list
  (id? torn (sound "\"a"))                               ; unterminated string
  (id? torn (sound "'"))                                ; dangling quote

  ; ⚠ torn and the clean end are DISTINCT, and NEITHER IS A DATUM: a datum comes
  ; back as a cons whatever it holds, so reading "()" answers the datum () --
  ; which is exactly the case a net test would call nothing.
  (: r (sound "()") (&& (two? r) (nil? (cap r))))
  !(id? torn (sound ""))

  ; A torn read RESUMES: hand back the same text with the rest appended.
  (: r (sound ("(1" + " 2)")) (&& (two? r) (= '(1 2) (cap r))))

  ; Mixed sequence, residue threaded: "() 1"
  (: a (sound "() 1") b (sound >a)
     (&& (nil? <a) (= 1 <b) (nil? (sound >b))))

  ; Misuse: a non-text holds no datum, and reads as the clean end.
  (nil? (sound 42))
  (nil? (sound 'notaport)))
