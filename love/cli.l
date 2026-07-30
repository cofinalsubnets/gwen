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
; the VERB RAIL: `verbs` -- ((NAME FN) ..), pinned () by every boot and rebound
; with the table by an artifact's bake (the dist lane's cat, crew/seed/up.l) --
; turns a first positional NAME into (FN args). a verb wins over a same-named
; file; `love -- up` and `love ./up` still read the file. the table's OTHER door
; -- argv[0] naming a verb (a `seed` symlink onto the artifact) -- lives in
; main.c's run_program: it must fire even at argc 1, where this file never runs.
;
; `pre` handles leading -l preloads + -v/-h and returns (kind . rest); the global
; argv is then rebound (top-level (: argv ..) DEFGLOB -- book is mopped post-egg, so a
; pin/peep on it is a no-op; defglob writes g->book directly) to the program's view
; (program . args). cli.l is compiled by c0 BEFORE -l loads prel, so the always-run path
; (pre/load1) uses only nifs/special-forms, NOT prel fns (two? not atom?; |, the
; bitwise-or nif, not the || macro; link not the L macro for the export list).
;
; the helpers (load1 eval1 bye version usage need pre) are CLOSURE-PRIVATE in the inner
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
       (? q (reads q) (: _ (say err (+ "love: cannot open " p)) (quit 1))))
    (eval1 s) (: q (tap (map s (jot (tally s))))     ; a string is a fn over its bytes -> a charlist port
       e (nom 0) m (nom 0)                           ; fresh noms: eof / unfinished-shape, id?-safe
       (rdl _) (: r (sound q e)                      ; every top-level datum off the string, in order
          (? (id? r q) m                             ; the port back: an unfinished shape
             (id? r e) ()
             (: t (rdl ()) (? (id? t m) m (link r t)))))
       ds (rdl ())
       (? (id? ds m)       (: _ (say err "love: -e: unfinished form") (quit 2))
          (two? ds)
          (? (two? (cup ds)) (ev 'ev ds)             ; MANY datums: the ONE application -- infix factors, like a repl line
             (ev 'ev (cap ds)))                      ; ONE datum: as-is (a leading monadic stays fused)
          ()))                                       ; no datums: nothing to run
    (bye s) (: _ (puts s) _ (putc 10) _ (flush out) (quit 0))   ; print a line to out, flush, exit 0
    (version _) (bye (+ "love " love-version))                       ; _ arg: love has no nullary calls
    (usage _) (bye (+ "usage: love [-l FILE] [-e EXPR] [-n] [-v] [-h] [--] [FILE ...]\n"
               (+ "  -l, --load FILE   preload FILE (read-eval) before the program\n"
               (+ "  -e, --eval EXPR   eval EXPR as one expression (infix works), print the answer\n"
               (+ "  -n, --silent      with -e: do not print the answer\n"
               (+ "  -v, --version     print the version and exit\n"
               (+ "  -h, --help        print this help and exit\n"
               (+ "  --                end of options; the rest are files\n"
                  "with no program (no -e, no FILE), read-eval stdin to EOF."))))))))
    (need h t) (? (two? t) 0 (: _ (say err (+ "love: " (+ h " needs an argument"))) (quit 2)))
    (vfind h t) (? (two? t)                          ; the verb table walk: NAME -> FN, () on a miss
                   (? (= h (cap (cap t))) (cap (cup (cap t))) (vfind h (cup t)))
                   ())
    (pre a) (? (two? a)                              ; -> (kind . rest): 'eval code.args / 'verb fn.args / 'file prog.args / 'stdin
     (: h (cap a) t (cup a) (?
      (| (= h "-l") (= h "--load"))    (: _ (need h t) _ (load1 (cap t)) (pre (cup t)))
      (| (= h "-v") (= h "--version")) (version 0)
      (| (= h "-h") (= h "--help"))    (usage 0)
      (| (= h "-e") (= h "--eval"))    (: _ (need h t) (link 'eval t))
      (| (= h "-n") (= h "--silent"))  (: r (pre t) (? (= (cap r) 'eval) (link 'evalq (cup r)) r))
      (= h "--")                        (? (two? t) (link 'file t) (link 'stdin ()))
      (: f (vfind h verbs)              ; a positional: a baked verb first, else the program file
         (? f (link 'verb (link f a)) (link 'file a)))))
     (link 'stdin ()))
    p (pre (cup argv))                  ; runs -l preloads + -v/-h; the program kind
    av (? (| (= (cap p) 'eval) (| (= (cap p) 'evalq) (= (cap p) 'verb))) (cup (cup p)) (cup p))  ; the program's view of argv
    run (\ _ (? (= (cap p) 'eval) (: v (eval1 (caup p)) _ (print out v) (put out 10))  ; -e ANSWERS OUT LOUD, a repl line
              (= (cap p) 'evalq) (eval1 (caup p))   ; printing like one: `print`, the repl's face, not dot's raw one. -n mutes.
              (= (cap p) 'verb) (: _ ((caup p) (cup (cuup p))) (quit 0))   ; FN takes the args after the name
              (= (cap p) 'file) (load1 (caup p))
              (reads in)))
    (link p (link av (link run ()))))   ; the body: return (parsed program-argv run)
 parsed (cap _)                         ; DEFGLOB the kept publics out of the triple (the prev `_`)
 argv   (cap (cup _))                   ; rebind argv to the program's view, BEFORE the run
 _ ((caup (cup _)) ()))                 ; (run ()): dispatch now that argv is the program's view (the unit -- run ignores its arg)
