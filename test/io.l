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
  (id? torn (sound '(40)))                              ; unclosed list
  (id? torn (sound '(34 97)))                           ; unterminated string
  (id? torn (sound '(39)))                              ; dangling quote

  ; ⚠ torn and the clean end are DISTINCT, and NEITHER IS A DATUM: a datum comes
  ; back as a cons whatever it holds, so reading "()" answers the datum () --
  ; which is exactly the case a net test would call nothing.
  (: r (sound '(40 41)) (&& (two? r) (nil? (cap r))))
  !(id? torn (sound ()))

  ; A torn read RESUMES: hand back the same text with the rest appended.
  (: r (sound (cat '(40 49) '(32 50 41))) (&& (two? r) (= '(1 2) (cap r))))

  ; Mixed sequence, residue threaded: "() 1"
  (: a (sound '(40 41 32 49)) b (sound >a)
     (&& (nil? <a) (= 1 <b) (nil? (sound >b))))

  ; Misuse: a non-charlist holds no datum, and reads as the clean end.
  (nil? (sound 42))
  (nil? (sound 'notaport)))
