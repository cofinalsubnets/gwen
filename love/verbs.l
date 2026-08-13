; verbs -- the registry behind `love mooncc ..`, `love up ..`, and a `sed` symlink onto the
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
;   (seat p)  argv[0] -- host/main.c's door, which fires before the cli runs at all (a bare
;             `seed` at argc 1 wants its usage). always a path, so the basename decides and no
;             shadow rule applies.
; a verb takes the args AFTER its name and ANSWERS A STATUS CHARM; the rail quits with it.
; that is kore's convention and it is why `love kore sed ..` nests.
(: tab (tablet 0)
   (get k)   (peep tab k 0)
   (bn p)    (: n (tally p)
                (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
                (go 0 0))
   (dotl? w) (: n (tally w) ((2 < n) && (snip w (n - 2) n = ".l")))
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
   _ (pin tab 'bake (prime 'bake)))
