; the corpus mentions the frontend nifs (quit/open/close/run/getenv); a bare target
; (kernel, wasm) binds few of them, and an unbound mention raises missing at every
; define that names one. this file leads the corpus, so pin a no-op fallback for
; whichever are absent: every later mention compiles quiet, and absence answers ().
(map (\ n (? (member? n (names ())) () (ev `(': `(n 'x) ())))) '(quit close run getenv))
(? (member? 'open (names ())) () (ev '(: (open x y) ())))

; the READER UNDER TEST -- doc/reader.md rung 3's socket. every print->read
; differential in the tree reads through this ONE binding (test/roundtrip.l,
; test/fuzz.l, test/host/rdiff.l), so rung 6 points the whole set at p1 by
; rebinding it here and nowhere else. the protocol is `sound`'s, which is also
; the whole contract p1 owes: (r port eofsym) answers the form, the sentinel at
; eof, or the PORT ITSELF where the shape ran out of text.
(: rd-test sound)

(: test_t0 (clock 0)
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
   ; word: a full 64-bit-word build (native, kernel) -- which is also where reals
   ; are f64, libm is glibc's, a filesystem exists, and the RNG runs full-width.
   ; false only on the 32-bit wasm shim; gates the asserts that need any of those.
   word (< (32 2) max-charm))
(:: 'assert (\ l
(:
 (report x v) (do
  (test_set 'count (+ 1 (test_get 'count)))
   (? v (term_text green_dot)
    (do (term_text red_x) (putc 10)
       (puts "assert failed: ") (putx x) (putc 10) (quit 1))))
 (link 'do (map (\ l `(report `('\ l) l)) l)))))
