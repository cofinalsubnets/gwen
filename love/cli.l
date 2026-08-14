; host CLI driver -- parse argv (after the program-name head) left to right:
;   -l/--load <file>    preload a file (read-eval it) before the program
;   -e/--eval <string>  eval the string as ONE expression (the repl's line law: many
;                       datums fold to one application, so infix works; one datum as-is)
;   -v/--version        print the version and exit
;   -h/--help           print usage and exit
;   --                  end of options: the next arg is the program, the rest are its args
;   <file>              load the file (a program)
; with no program (no -e, no file), read-eval stdin to EOF. Loaded by main.c when args are
; present.
;
; the VERB RAIL: the registry is the `verbs` MODULE (love/verbs.l), so it rides the image and
; needs no global -- a frontend that never registers it simply has none, and the presence guard
; below is that answer. (V 'word h) turns a first positional NAME into its FN, applying the
; shadow rule in one place: a verb wins over a same-named file, but a `/` or a `.l` is always
; the file, so `love -- up` and `love ./up` still read one. the table's OTHER door -- argv[0]
; naming a verb (a `seed` symlink onto the artifact) -- lives in main.c's run_program: it must
; fire even at argc 1, where this file never runs.
;
; `pre` handles leading -l preloads + -v/-h and returns (kind . rest); the global
; argv is then rebound (top-level (: argv ..) DEFGLOB -- book is mopped post-egg, so a
; pin/peep on it is a no-op; defglob writes g->book directly) to the program's view
; (program . args). every lane that evals this file has the prel up already -- the build-tool
; lane loads prel0 ahead of it, the woken lane carries it in the image -- so prel fns, the
; macros (|| list) and the bracket ctors are all in hand. `pat` too: the build-tool lane was
; the one holdout and now says (use 'pat) like every other, so `@` reads here.
;
; the helpers (load1 eval1 bye version usage need vfind pre) are CLOSURE-PRIVATE in the inner
; body-having `:`, OFF the global book -- only `parsed` and `argv` (the kept publics; argv is
; read by test/run.l + host/main.c) are defglob'd out, the cli sibling of the repl->bao
; consolidation. The triple holder is bound to `_`, NOT a name: a top-level body-less `:`
; defglobs every NAMED binding to the book, and this form RE-RUNS at startup (so an egg mop
; can't catch it), but `_` never leaks -- the intermediate stays off (names ()). The reads
; below pick it up by the sequence law. The inner body returns (parsed program-view-argv run),
; where `run` is the dispatch THUNK. Order matters: we defglob `argv` to the program's view
; FIRST, THEN call `(run ())` -- the loaded program reads the global `argv`, so it must already
; be rebound before the load fires. (-v/-h still exit inside `pre`, before the rebind, as before.)
(: _
 (: (load1 p) (: q (open p "r")
       (? q (reads q) (say err ("love: cannot open " + p), quit 1)))
    (eval1 s) (: m (nom 0)                           ; a fresh nom: unfinished-shape, id?-safe
       (rdl x acc) (sound x @ 'torn      m           ; every top-level datum off the string, in order
                               (a >< b)  (rdl b (a >< acc))
                               (rev acc))            ; 'torn is an unfinished shape
       ds (rdl s ())
       (? (id? ds m) (say err "love: -e: unfinished form", quit 2)
          (two? ds)  (ev 'ev (two? >ds ? ds <ds))    ; MANY datums fold to the ONE application (infix factors,
          ()))                                       ; like a repl line); ONE stays as-is. none: nothing to run
    (bye s) (puts s, putc 10, flush out, quit 0)     ; print a line to out, flush, exit 0
    (version _) (bye $ "love " + love-version)       ; _ arg: love has no nullary calls
    ; the verb line comes off the TABLE, never a written-down list -- the registry is the
    ; one manifest, so a binary carries the help its own image earns (a plain love says
    ; `bake wake`, the dist artifact names its whole crew). the {'tab tab} pattern needs no
    ; tablet? guard the way vfind below does: a hash pattern EMITS one, and an unregistered
    ; registry then falls out twice over -- no arm fits, and `+` rides the unit through.
    (vwords ks) (? (atom? ks) "" (atom? >ks) (string <ks) (string <ks + " " + vwords >ks))
    (usage _) (bye $ "usage: love [wake IMAGE] [-l FILE] [-e EXPR] [-n] [-m SIZE] [-v] [-h] [--] [VERB|FILE] [ARG ...]\n"
                   + "  -l, --load FILE   preload FILE (read-eval) before the program\n"
                   + "  -e, --eval EXPR   eval EXPR as one expression (infix works), print the answer\n"
                   + "  -n, --silent      with -e: do not print the answer\n"
                   + "  -m  --memory N    max heap in {k,m,g} default = 0m = unlimited\n"
                   + "  -v, --version     print the version and exit\n"
                   + "  -h, --help        print this help and exit\n"
                   + "  --                end of options; the rest are files\n"
                   + (from 'verbs @ {'tab tab} ("verbs: " + vwords (keys tab) + "\n"))
                   )
    (need h t) (atom? t ? (say err ("love: " + h + " needs an argument"), quit 2))
    ; --- -m, the footprint cap: slot 0 of `tune` over a fresh read, so the other three knobs
    ; ride unchanged and the last -m on the line wins. 0 is the unbounded spelling, which is
    ; why `size` takes a floor of 0 where a count would want 1. ⚠ it lands at the NEXT
    ; collection: the pool a session opens with was sized long before argv was read.
    (wordsz _) (charm? 1073741824 ? 8 4)             ; 2^30 is a charm only where a fixnum is 62-bit
    ; ⚠ validate HERE. a bad value reaching `tune` writes a non-Z tray, which the nif refuses
    ; whole and in silence -- the flag would then read as accepted and do nothing.
    (size h s) (: n (tally s)                        ; ⚠ v is BOUND, not inlined: a bare infix `//` in
                  u (peep s (n - 1) 0 @ (107 | 75) 1024 (109 | 77) 1048576 (103 | 71) 1073741824 ())
                  d (u ? (snip s 0 (n - 1)) s)                       ; the `:` body slot folds into the
                  v <(sound d)                                       ; binding chain and eats the body
                  _ ((charm? v && v >= 0) ? 0 (say err ("love: " + h + ": not a size: " + s), quit 2))
                  ((v * (u ? u 1048576)) // wordsz ()))
    ; ⚠ tablet?, never a bare truth test: an unregistered module reads () and (() 'word) is the
    ; church const 1, so an unguarded (V 'word h) would answer h -- the string itself, truthy,
    ; and the rail would call a command word as a function.
    (vfind h) (: V (from 'verbs) (? (tablet? V) (V 'word h) ()))
    (pre a) (a @ (h >< t)            ; -> (kind . rest): 'eval code.args / 'verb fn.args / 'file prog.args / 'stdin
      (h @ ("-l" | "--load")    (need h t, load1 <t, pre >t)
           ("-v" | "--version") (version ())
           ("-h" | "--help")    (usage ())
           ("-e" | "--eval")    (need h t, 'eval >< t)
           ("-n" | "--silent")  (pre t @ ('eval >< r) ('evalq >< r) r r)
           ("-m" | "--memory")  (need h t, tune (pin (tune ()) 0 (size h <t)), pre >t)
           "--"                 (two? t ? ('file >< t) '(stdin))
           (: f (vfind h)       ; a positional: a registered verb first, else the program file
              (f ? ('verb >< f >< a) ('file >< a))))
      '(stdin))
    p (pre >argv)                    ; runs -l preloads + -v/-h + -m; the program kind
    k <p                             ; ..and the kind alone, read six times below
    av (k @ ('eval | 'evalq | 'verb) >>p >p)   ; the program's view of argv
    run (\ _
      (k @ 'eval  (print out (eval1 <>p), put out 10)  ; -e ANSWERS OUT LOUD, a repl line printing like
           'evalq (eval1 <>p)         ; one: `print`, the repl's face, not dot's raw one. -n mutes.
           'verb  (: r (<>p >>>p)     ; FN takes the args after the name and ANSWERS a status;
                     (quit (charm? r ? r 0)))   ;   one that quits internally never returns here
           'file  (load1 <>p)
           (reads in)))
    [p av run])                      ; the body: return (parsed program-argv run)
 parsed <_                           ; DEFGLOB the kept publics out of the triple (the prev `_`)
 argv   <>_                          ; rebind argv to the program's view, BEFORE the run
 _ (<>>_ ()))                        ; (run ()): dispatch now that argv is the program's view (the unit -- run ignores its arg)
