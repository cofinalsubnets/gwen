#!/usr/bin/env -S love -l
; tools/waits.l -- WHO WAITS. the device floor's invariant, made structural: the
; only code in the tree that blocks is the scheduler, plus a short roster of named
; exceptions. a wait that comes back therefore comes back LOUDLY, at build time,
; instead of arriving as a hang in somebody's test six months later.
;
; the hooks it watches are the four that actually stop the world -- ai_sleep,
; ai_wait_fds, ai_fd_drain, and love.c's own wait_one shim. every call to one of
; them, in every tracked .c file, must name a function on the roster below.
;
; ⚠ IT READS THE C AS WRITTEN, not the binary, and that is the whole design.
; every io_* and z* function in love.c is `static`, so the compiler inlines them
; and their names are gone from the ELF; a call graph read off the image would
; answer GREEN because it could not find the caller, which is the worst answer a
; gate can give. vmret can disassemble because lvm_* aps are addressed and survive.
;
; the scan is one line at a time: an identifier immediately followed by `(` is a
; call, and the enclosing function is the nearest definition line at column 0
; (love's C puts every definition there and indents every body). two edges of that
; rule are deliberate --
;
;  * `Ap(lvm_yield_sw, g)` names `Ap`, never lvm_yield_sw. a tail-jump is NOT a
;    call: the op returns to the trampoline first and the scheduler then waits in
;    its own frame, which is exactly the shape this gate exists to permit.
;  * a `//` comment is stripped before the scan, so prose about a wait cannot
;    redden the gate -- a gate that reddens on writing about it teaches people to
;    stop writing about it. block comments are not stripped; nothing needs it.
;
; ⚠ WHAT IT DOES NOT SEE, so nobody reads it as more than it is: the four hooks by
; NAME, never blocking in general. a raw read(2) in love.c, a fresh primitive, or a
; hook reached through a function pointer all walk past it. it says "the named waits
; are where we put them", which is the thing that regresses.
;
; Usage: love tools/waits.l FILE ..   (paths as the roster spells them: repo-relative)
; Exit:  0 = the waits are exactly the roster, 1 = they are not, 2 = usage error.

(: (die m) (: _ (say err "waits: ") _ (say err m) _ (say err "\n") (quit 2))

   ; --- what counts as a wait -------------------------------------------------
   hooks `("ai_sleep" "ai_wait_fds" "ai_fd_drain" "wait_one")
   (hook? nm) (any (\ h (h = nm)) hooks)
   ; the three love.h CONTRACT symbols, whose bodies are the wait itself: a
   ; frontend's `ai_wait_fds` reaching ai_sleep is what a wait hook IS, so the
   ; family's own implementations are exempt and only callers from outside it
   ; have to answer. love.c's wait_one is NOT one of these -- it is ordinary
   ; code that happens to wait, so it answers on the roster like anyone else.
   (impl? nm) (any (\ h (h = nm)) `("ai_sleep" "ai_wait_fds" "ai_fd_drain"))

   ; --- THE ROSTER: (file function hook why) ----------------------------------
   ; every wait in the tree, and the sentence that earns it.
   roster
   `(`("core/love.c" "wait_one" "ai_wait_fds"
       "the scheduler's one-fd shim -- an lvm_ frame may hold no scratch")
     `("core/love.c" "lvm_yield_sw_mono" "wait_one"
       "the monotask scheduler, parked on one fd")
     `("core/love.c" "lvm_yield_sw_mono" "ai_sleep"
       "the monotask scheduler, a timer and no fd")
     `("core/love.c" "yield_sw_wait" "ai_wait_fds"
       "THE wait: every parked task and the nearest timer, one call")
     `("core/love.c" "io_close" "ai_fd_drain"
       "⚠ the finalizer: a dying port's unsent run. runs inside gc, where nothing can park -- the write side's one remaining unbounded wait (doc/io.md)")
     `("port/teensy41/main.c" "main" "ai_sleep"
       "the panic blink after a fatal shell exit -- nothing runs after it"))

   ; --- character classes ------------------------------------------------------
   (idc? c) (|| (&& (c >= 97) (c <= 122))
             (|| (&& (c >= 65) (c <= 90))
             (|| (&& (c >= 48) (c <= 57)) (c = 95))))
   (blank? c) (|| (c = 32) (c = 9))
   (inch? s c) (: n (tally s) (go k) (? (n <= k) () ((s k) = c) 1 (go (k + 1))) (go 0))

   ; --- line helpers -----------------------------------------------------------
   (lines s) (: n (tally s)
                (go start i acc)
                 (? (n <= i) (rev (? (start < i) (link (snip s start i) acc) acc))
                    ((s i) = 10) (go (i + 1) (i + 1) (link (snip s start i) acc))
                    (go start (i + 1) acc))
                (go 0 0 ()))
   (rtrim s) (: (go e) (? (&& (e > 0) (blank? (s (e - 1)))) (go (e - 1)) e)
                (snip s 0 (go (tally s))))
   (uncomment s) (: n (tally s)
                    (go i) (? ((i + 1) >= n) s
                              (&& ((s i) = 47) ((s (i + 1)) = 47)) (snip s 0 i)
                              (go (i + 1)))
                    (go 0))

   ; every identifier in s that is immediately followed by `(`
   (calls s) (: n (tally s)
                (end k) (? (&& (k < n) (idc? (s k))) (end (k + 1)) k)
                (go i acc)
                 (? (n <= i) (rev acc)
                    (idc? (s i))
                     (: j (end i)
                        (go j (? (&& (j < n) ((s j) = 40)) (link (snip s i j) acc) acc)))
                    (go (i + 1) acc))
                (go 0 ()))

   ; --- naming the enclosing function ------------------------------------------
   ; a top-level definition line: column 0, has a paren, is not a preprocessor
   ; line / comment / brace, and either opens a body or does not end in `;` --
   ; ⚠ the `{` half is load-bearing. love.c puts the first statement on the
   ; header line (`static lvm(lvm_yield_sw_mono) { uintptr_t my_wake = ..;`), so
   ; a bare "ends in a semicolon means prototype" test throws away the scheduler.
   (header? s) (: n (tally s)
                  c (? (n < 1) 32 (s 0))
                  (&& (n > 0)
                   (&& (! (blank? c))
                    (&& (! (any (\ x (x = c)) `(35 47 42 125 41)))
                     (&& (inch? s 40)
                         (|| (inch? s 123) (! ((s (n - 1)) = 59))))))))
   ; a name that is a qualifier or a keyword, never the function being defined
   nonfn `("__attribute__" "if" "while" "for" "switch" "return" "sizeof" "defined")
   (ident-from s i) (: n (tally s)
                       (go k) (? (&& (k < n) (idc? (s k))) (go (k + 1)) (snip s i k))
                       (go i))
   (at? s i p) (: m (tally p)
                  (go k) (? (m <= k) 1 ((s (i + k)) = (p k)) (go (k + 1)) ())
                  (&& ((i + m) <= (tally s)) (go 0)))
   (find s p) (: n (tally s)
                 (go i) (? (n <= i) () (at? s i p) i (go (i + 1)))
                 (go 0))
   ; the function a definition line names: the first call-shaped identifier that
   ; is not a qualifier -- except for `lvm(name)`, love.h:70's definition macro,
   ; where the name is INSIDE the parens and every VM op is spelled that way.
   (hdr-name s)
    (: j (find s "lvm(")
       cs (filter (\ x (! (any (\ q (q = x)) nonfn))) (calls s))
       (? (! (() = j)) (ident-from s (j + 4))
          (two? cs) <cs
          (rtrim s)))

   ; --- the scan ---------------------------------------------------------------
   ; -> ((file fn hook line) ..), one entry per distinct wait; `line` is where it
   ; was FIRST seen, which is where a reader should go look. the key is the first
   ; three -- two calls to the same hook from the same function are one fact.
   (seen? l k) (any (\ e (e = k)) l)
   (key e) (+ <e (+ " " (+ (el e 1) (+ " " (el e 2)))))
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   (scan path)
    (: port (open path "r")
       _ (? port 0 (die (+ "cannot open " path)))
       src (slurp port)
       _ (close port)
       (go ls n fn acc)
        (? (! (two? ls)) (rev acc)
           (: line (uncomment (rtrim <ls))
              hdr (header? line)
              fn2 (? hdr (hdr-name line) fn)
              ; a definition line names its own function, and that is not a call
              hits (? (impl? fn2) ()
                      (filter (\ c (&& (hook? c) (! (&& hdr (c = fn2))))) (calls line)))
              acc2 (foldl (\ a c (: e `(path fn2 c n)
                                    (? (seen? (map key a) (key e)) a (link e a))))
                          acc hits)
              (go >ls (n + 1) fn2 acc2)))
       (go (lines src) 1 "" ()))

   ; --- the report --------------------------------------------------------------
   (show2 e) (: _ (say out "  ") _ (say out (el e 1)) _ (say out " -> ")
                _ (say out (el e 2)) (put out 10))
   ; a found wait points AT its line; a roster entry has no line, it has a reason
   (showx e) (: _ (say out "  ") _ (say out <e) _ (put out 58)
                _ (putbn out (el e 3) 10) _ (say out ":") _ (show2 e) 0)
   (show3 r) (: _ (say out "  ") _ (say out <r) _ (show2 r)
                _ (say out "      ") _ (say out (el r 3)) (put out 10))
   (llen l) (foldl (\ n _ (n + 1)) 0 l)

   paths >argv
   _ (? (two? paths) 0 (die "usage: waits.l FILE .."))
   found (foldl (\ a p (+ a (scan p))) () paths)
   fkeys (map key found)
   rkeys (map key roster)
   extra (filter (\ e (! (seen? rkeys (key e)))) found)
   stale (filter (\ r (! (seen? fkeys (key r)))) roster)
   (? (|| (two? extra) (two? stale))
      (: _ (? (two? extra)
              (: _ (say out "waits: ") _ (putbn out (llen extra) 10)
                 _ (say out " call site(s) reach a wait hook and are NOT on the roster:\n")
                 _ (each extra showx)
                 (say out "  -- if this wait is right, put it on the roster in tools/waits.l with its reason.\n"))
              0)
         _ (? (two? stale)
              (: _ (say out "waits: ") _ (putbn out (llen stale) 10)
                 _ (say out " roster entry(s) have no call site (stale -- delete them):\n")
                 (each stale show3))
              0)
         (quit 1))
      (: _ (say out "waits: all ") _ (putbn out (llen found) 10)
         _ (say out " waits in ") _ (putbn out (llen paths) 10)
         _ (say out " files are the roster\n")
         (quit 0))))
