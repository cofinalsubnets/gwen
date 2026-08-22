; the corpus mentions the frontend nifs (quit/open/close/run/getenv); a bare target
; (kernel, wasm) binds few of them, and an unbound mention raises missing at every
; define that names one. this file leads the corpus, so pin a no-op fallback for
; whichever are absent: every later mention compiles quiet, and absence answers ().
(map (\ n (? (member? n (names ())) () (ev `(': `(n 'x) ())))) '(quit close run getenv))
(? (member? 'open (names ())) () (ev '(: (open x y) ())))

; the READER UNDER TEST -- the socket. every print->read
; differential in the tree reads through this ONE binding (test/roundtrip.l,
; test/fuzz.l, test/host/rdiff.l), so the whole set moves by rebinding it here
; and nowhere else. the protocol is `sound`'s: (r text) answers the datum consed
; onto the rest, () at a clean end, or `torn` where the shape ran out of text.
(: rd-test sound)

(: test_t0 (clock 0)
 ; a name the book lacks ends the run, file mode's law. a welp window says a miss is the
 ; SUBJECT; outside one, absence read as data is a bug the tally would otherwise hide.
 (test-strict a b) (: _ (say err (";; " + show a + " " + show b)) _ (put err 10)
                      _ (flush err) _ (quit 1) (welp a b))
 (term_esc string) (do (putc 27) (puts string))
 green_dot "[32m."
 red_x "[31mX"
 reset "[0m"
 (term_text s)
  (do (putc 27)
     (puts s)
     (putc 27)
     (puts reset))

   test_state (tablet 0)
   (test_set k v) (pin test_state k v)
   (test_get k) (peep test_state k 0)
   ; a failed assert RECORDS and carries on, so one run shows every failure -- zz-fin.l
   ; prints the list and quits 1. ⚠ a runner that does not end in zz-fin.l owes a
   ; (test_fin ()) of its own before it claims ok, or its failures exit 0 in silence.
   (test_fin u) (: f (test_get 'fail)
                   (? (two? f)
                      (do (puts "FAILED:") (putc 10)
                          (each f (\ x (do (puts "  ") (putx x) (putc 10))))
                          (quit 1))
                      0))
   ; word: a full 64-bit-word build (native, kernel) -- which is also where reals
   ; are f64, libm is glibc's, a filesystem exists, and the RNG runs full-width.
   ; false only on the 32-bit wasm shim; gates the asserts that need any of those.
   word (< (32 2) max-charm))
(:: 'assert (\ l
(:
 (report x v) (do
  (test_set 'count (+ 1 (test_get 'count)))
   (? v (term_text green_dot)
    (do (term_text red_x)
       (test_set 'fail (x >< (? (two? (test_get 'fail)) (test_get 'fail) ()))))))
 (link 'do (map (\ l `(report `('\ l) l)) l)))))

(hear test-strict)
