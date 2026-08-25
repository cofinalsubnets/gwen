; Subprocess capability (host-only nifs `hark` and `getenv`).
;
; (hark argv) takes a LIST of strings and spawns the program named by its head
; (PATH-resolved via execvp). It returns:
;   (status . output)  -- a PAIR -- when the process ran: status is the exit
;                         code (or 128+signal if killed), output is the child's
;                         captured stdout as one string;
;   a positive fixnum  -- the OS errno -- when the program couldn't be spawned;
;   a negative fixnum  -- our-side misuse (empty argv / a non-string element).
; So `two?` is the success/failure discriminator. stderr is inherited, not
; captured. (getenv name) -> string, or nil when unset.
;
; These children are chosen so none reads stdin: under the test harness the
; child inherits l's stdin (the test-corpus pipe), and a reader would eat it.

; hark/getenv are host-only (subprocess + environment); the browser shim has
; neither, so skip the whole file there (word = the hosted builds).
(? word (assert
  ; --- ran successfully: a chain, exit 0, and (here) no output ---
  (two? (hark ["true"]))
  (= 0 (cap (hark ["true"])))
  (= 0 (sat (cup (hark ["true"]))))             ; `true` writes nothing

  ; --- a nonzero exit code is reported as-is ---
  (= 1 (cap (hark ["false"])))

  ; --- stdout is captured (echo appends a newline) ---
  (: r (hark ["echo" "hi"]) (&& (= 0 (cap r)) (= "hi\n" (cup r))))

  ; --- multiple args are marshalled in order, with no shell in between ---
  (: r (hark ["printf" "%s-%s" "ab" "cd"]) (= "ab-cd" (cup r)))

  ; --- a program that can't be spawned -> positive errno fixnum, NOT a chain ---
  (: r (hark ["/no/such/prog-xyzzy"]) (&& (charm? r) (< 0 r)))    ; abs path: ENOENT
  (: r (hark ["no-such-prog-xyzzy"])  (&& (charm? r) (< 0 r)))    ; PATH miss: ENOENT

  ; --- our-side misuse -> -1 (a negative fixnum, distinct from any errno) ---
  (= -1 (hark 0))                                ; empty argv (nil)
  (= -1 (hark ["echo" 5]))                      ; 5 is not a string

  ; --- output larger than the initial buffer exercises the grow path ---
  (: r (hark ["seq" "20000"]) (&& (= 0 (cap r)) (< 65536 (sat (cup r)))))

  ; --- getenv: PATH is set (a string); a bogus name is unset -> () (the zero point) ---
  (string? (getenv "PATH"))
  (nil? (getenv "GWEN_DEFINITELY_UNSET_12345"))) 0)
