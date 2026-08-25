; verbs -- the registry behind `love mooncc ..`, `love cook ..`, and a `sed` symlink onto the
; artifact. one tablet, symbol-keyed, and MUTABLE IN PLACE on purpose: a verb table is read at
; RUN off a command-line string, so nothing in it folds and sealing it would buy nothing. that
; is the whole difference from a module's members, which are compile-time definitions -- `pin`
; is the door and (from 'verbs 'tab) hands the real tablet, not a copy. rng.l needs a box for
; the same reason this one does not: rng rebinds a VALUE, this rebinds a CONTAINER's contents,
; and a binding folded into a closure still reaches the pins.
; ⚠ registered, never spliced (holo's shape -- the boot unsplices): tab/word/seat are not names
; to reach bare, and `get` would shadow half the tree.
; ⚠ an empty tab is nil? like every other empty -- ask it by peep, never by testing the tablet.
;
; two front doors, because the two dispatch lanes carry different shadow rules:
;   (word w)  a leading POSITIONAL -- love/cli.l's rail. refuses a `/` or a `.l` so a verb can
;             never shadow a local script; `love ./up` and `love -- up` stay the file. with
;             fifty applets registered that rule is what keeps `make`, `env`, `true` and `cmp`
;             from eating a script of the same name.
;   (seat p)  argv[0] -- src/main.c's door, which fires before the cli runs at all (a bare
;             `seed` at argc 1 wants its usage). always a path, so the basename decides and no
;             shadow rule applies.
; a verb takes the args AFTER its name and ANSWERS A STATUS CHARM; the rail quits with it.
; that is kore's convention and it is why `love kore sed ..` nests.
(module '(verbs any? bn dash fire get prime scan seat seatof tab unprime word)
(: tab (tablet 0)
   (get k)   (peep tab k 0)
   (bn p)    (: n (tally p)
                (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
                (go 0 0))
   (dotl? w) (: n (tally w) ?((2 < n) && (snip w (n - 2) n = ".l")))   ; ⚠ glued ?( : && answers () for a false LEFT
   (sl? w)   (: n (tally w) (go i) (? (n <= i) 0 (w i = 47) 1 (go (i + 1))) (go 0))
   (word w)  (? ((sl? w) || (dotl? w)) () (get (intern w)))
   (seat p)  (get (intern (bn p)))
   ; the PRIME verbs. wake and bake are parsed in C before any love runs, and by physics, not
   ; habit: wake decides WHICH heap (image_load precedes ai_ini), and bake must snapshot before
   ; run_program pushes the session layer -- `leave` is mopped, so a love-level bake could not
   ; pop its own layer and would bake a layered heap. they hold rows here anyway so one manifest
   ; answers for help and the shadow rule, and so a misplaced one is an honest error instead of
   ; falling through to "run the file named bake".
   (prime nm) (\ as (: _ (say err ("love: " + string nm + " must lead the command line"
                                   + " -- it is read before the session exists"))
                       _ (put err 10) 2))
   _ (pin tab 'wake (prime 'wake))
   _ (pin tab 'bake (prime 'bake))
   ; `love verbs` -- the table naming itself, one per line for a pipe where -h's blob is for
   ; a human. sorted: a tablet's key order is its own business and would read like a change.
   (vsort ws) (? (atom? ws) ws
                 (: p <ws r (vsort >ws)
                    (ins x l) (? (atom? l) [x] (string x < string <l) (x >< l) (<l >< ins x >l))
                    (ins p r)))
   _ (pin tab 'verbs (\ as (: _ (map (\ k (: _ (puts (string k)) (putc 10))) (vsort (keys tab)))
                              _ (flush out) 0)))

   ; --- the SEAT SCAN, the third door ---
   ; a file that IS the program seats itself: `love crew/moon/moon.l foo.c`, the installed
   ; shebang's `-l`, a catted script under its own name. That is not the registry's two
   ; doors -- no name was typed, a PATH was -- and it is why six apps each wrote this walk
   ; out for themselves. one walk now; the spellings stay with the app, which is the only
   ; thing that ever knew them.
   ;   (fire ws)      -> (SEAT-WORD . args-after) over THIS invocation's line, else ()
   ;   (scan ws a)    the same over a list handed in -- kore's in-image door, which runs
   ;                  an arg list that is not the one this process was started with.
   ;   (seatof ws a)  the raw scan under both, without the prime step.
   ; ⚠ THE LINE IS READ HERE, and the guard is the whole licence. This module compiles
   ; into the egg, where a bare global read once folded to the baking session's value and
   ; every caller then scanned the line that baked the image -- so a bake pins no `cmdline`
   ; at all now (src/main.c), which leaves the read a lookup. The absence IS the bake's
   ; honest answer: nothing was typed, nothing seats, and the feet go quiet without scaring.
   ; the seat WORD rides back because a multi-name app reads it: lush's login dash, an
   ; applet symlink. ⚠ () means "not seated", the presence answer -- ask it with two?.
   (dash w)    (? ((0 < tally w) && (w 0 = 45)) (snip w 1 (tally w)) w)   ; -sh: login(1)'s mark
   (any? w ws) (? (atom? ws) 0 ((<ws = w) || (any? w >ws)))
   (mine? ws w) (any? (dash (bn w)) ws)
   ; the primes lead, and they are the COMMAND LINE's word, never the program's -- step
   ; over them so `love wake IMAGE app ..` seats app. wake always carries its path; bake
   ; may, and a bake with none seats nobody anyway (it exits before a session exists).
   (unprime a) (? !(two? a) a
                  ((<a = "wake") || (<a = "bake")) (? (two? >a) >>a ())
                  a)
   (seatof ws a) (? !(two? a) ()
                    (<a = "-l") (? (two? >a)                     ; a preload NAMED for me seats
                                   (? (mine? ws <>a) (<>a >< >>a) (seatof ws >>a))   ; me -- the shebang's lane
                                   ())
                    (mine? ws <a) (<a >< >a)
                    ())                                          ; seats someone else: not me
   (scan ws a) (seatof ws (unprime a))
   (fire ws)   (scan ws (? (member? 'cmdline (names ())) >cmdline ())))

) ; module verbs
