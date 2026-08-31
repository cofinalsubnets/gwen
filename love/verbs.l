; verbs -- the registry behind `love mooncc ..`, `love cook ..`, and a `sed` symlink onto
; the artifact. one symbol-keyed tablet, mutated in place: (from 'verbs 'tab) hands the
; real tablet, not a copy, and `pin` is the door. registered, never spliced -- tab/word/
; seat are not names to reach bare, and `get` would shadow half the tree.
;
; two front doors, because the two dispatch lanes carry different shadow rules:
;   (word w)  a leading positional -- love/cli.l's rail. refuses a `/` or a `.l`, so with
;             fifty applets registered `make`, `env`, `true` and `cmp` cannot eat a local
;             script of the same name; `love ./up` and `love -- up` stay the file.
;   (seat p)  argv[0] -- src/main.c's door, which fires before the cli runs at all (a bare
;             `seed` at argc 1 wants its usage). always a path, so the basename decides.
; a verb takes the args after its name and answers a status charm; the rail quits with it.
; that is kore's convention, and it is why `love kore sed ..` nests.
(module '(verbs any? bn dash fire get prime scan seat seatof tab unprime word)
(: tab {}   ; nil? when empty, like every empty -- ask it with peep, not by testing it
   get tab
   (bn p)    (: n #p
                (go k r) (k < n ? (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
                (go 0 0))
   (dotl? w) (: n #w (2 < n && snip w (n - 2) n = ".l"))
   (sl? w)   (: n #w (go i) (? (n <= i) 0 (w i = 47) 1 (go (i + 1))) (go 0))
   (word w)  (sl? w || dotl? w ? () (get (name w)))
   seat  (get * name * bn)
   ; wake and bake are parsed in C before any love runs; they hold rows here so help and the
   ; shadow rule see them, and a misplaced one errors instead of running a file named bake.
   (prime nm) (\ as (: _ (say err ("love: " + string nm + " must lead the command line"
                                   + " -- it is read before the session exists\n"))
                       2))
   _ (pins tab 'wake (prime 'wake) 'bake (prime 'bake))
   ; `love verbs` -- one name per line, for a pipe. sorted: a tablet's key order is its own
   ; business and would read like a change.
   (vsort ws) (? (atom? ws) ws
                 (: p <ws r (vsort >ws)
                    (ins x l) (? (atom? l) [x] (string x < string <l) (x . l) (<l . ins x >l))
                    (ins p r)))
   (putln k) (puts (string k), putc 10)
   _ (pin tab 'verbs (\ as (map putln * vsort $ keys tab,
                            flush out,
                            0)))

   ; --- the seat scan, the third door ---
   ; a file that is the program seats itself: `love crew/moon/moon.l foo.c`, the installed
   ; shebang's `-l`, a catted script under its own name. no name was typed, a path was.
   ;   (fire ws)      -> (seat-word . args-after) over this invocation's line, else ()
   ;   (scan ws a)    the same over a list handed in -- kore's in-image door
   ;   (seatof ws a)  the raw scan under both, without the prime step
   ; a bake pins no `cmdline` (src/main.c), which leaves the read below a lookup: nothing
   ; typed, nothing seats. () means "not seated", the presence answer -- ask it with two?.
   (dash w)    (0 < tally w && w 0 = 45 ? (snip w 1 (tally w)) w)   ; -sh: login(1)'s mark
   (any? w (a . u)) (a = w || any? w u)
   (mine? ws w) (any? (dash (bn w)) ws)
   ; the primes lead and are the command line's word, never the program's -- step over them
   ; so `love wake IMAGE app ..` seats app.
   (unprime a) (? (atom? a) a
                  (<a = "wake" || <a = "bake") >>a
                  a)
   ; no arm answers () -- seats someone else, not me
   (seatof ws a) (? (atom? a) ()
                    (<a = "-l") (two? >a ?                     ; a preload named for me seats
                                 (mine? ws <>a ? (<>a . >>a) (seatof ws >>a)))  ; me -- the shebang's lane
                    (mine? ws <a) (<a . >a))
   (scan ws a) (seatof ws (unprime a))
   (fire ws)   (scan ws (? (member? 'cmdline (names ())) >cmdline ())))

) ; module verbs
