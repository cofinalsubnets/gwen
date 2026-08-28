; cli -- the host command line, read left to right after the program-name head. the file
; defines and does not run: a body-less top-level `:`, so the names defglob out.
;   (cli-rail ln)     the walk over ln = (program-name . args)
;   (cli-line ln tty) a frontend's whole tail: the argv[0] door, the rail, the repl, stdin
; src/main.c calls the second, and so does a forked lush child -- a word whose PATH winner
; is this binary need not exec, and what it would have exec'd is (cli-line its-line) on a
; warm heap. the shadow rule lives in one place, love/verbs.l's `word`. prel and pat must
; be up before this compiles: @ reads here, and a macro reaches a reader only once its
; layer is spliced.
;
; the defglobs go through `ev` of a built (: nm 'v), and the inner quote is load-bearing --
; without it ev takes the value apart as a form to run. argv reaches the program's view
; before (run ()) fires, which a loaded program reads.
;
; the exit door: every leave -- a bad -m, a missing operand, an unopenable file, a
; torn -e, a verb's own status -- scares 'cli-leave, and the trap handler is the only
; `quit`. that lets a caller in the same image put its own trap on and get a charm where a
; spawn hands it a dead process. the handler is written twice because a leave lands in
; either of two spans, `pre` and `run`, and the defglob has to sit between them.
(: (cli-rail ln)
 (: (charm0 x) (charm? x ? x 0)
    cli2
 (trap (\ x
 (: (cli-leave c) (scare 'cli-leave c)
    (load1 p) (open p "r" @ '() (say err $ "love: cannot open " + p, cli-leave 1)
                            x (reads x))
    (eval1 s) (: m (nom 0)                           ; a fresh nom: the unfinished shape, id?-safe
       (rdl x acc) (sound x @ 'torn      m           ; every top-level datum off the string, in order
                               (a . b)  (rdl b (a . acc))
                               (rev acc))
       (rdl s () @ ^m (say err "love: -e: unfinished form", cli-leave 2)
                   (d @ (a . u)) (ev $ atom? u ? a d))) ; many datums fold to one application
    (say1 s) (puts s, putc 10)             ; -v and -h each print and keep walking argv
    (version _) ("love " + love-version)   ; _ arg: love has no nullary calls
    (usage _)
    ("usage: love [wake IMAGE] [-l FILE] [-e EXPR] [-q] [-m SIZE] [-v] [-h] [--] [VERB|FILE] [ARG ...]\n" +
     "  -l, --load FILE   preload FILE (read-eval) before the program\n" +
     "  -e, --eval EXPR   eval EXPR as one expression (infix works), print the answer\n" +
     "  -q, --quiet       with -e: do not print the answer\n" +
     "  -m  --memory N    max heap in {k,m,g} default = 0m = unlimited\n" +
     "  -v, --version     print the version\n" +
     "  -h, --help        print this help\n" +
     "  --                end of options; the rest are files, and none of them is stdin\n" +
     "with no arguments at all love reads stdin; with arguments and no program it stops\n" +
     "when the command line runs out, so -v and -h compose and neither is terminal.\n" +
     "`love verbs` names the verbs this binary carries.\n")
    (need h '()) (say err $ "love: " + h + " needs an argument", cli-leave 2)
    K 1024 M 1048576 G 1073741824
    wordsz (charm? G ? 8 4)             ; 2^30 is a charm only where a fixnum is 62-bit
    ; validate here: a bad value reaching `tune` writes a non-Z tray, which the nif refuses
    ; whole and in silence, so the flag would read as accepted and do nothing. 0 is the
    ; unbounded spelling, which is why this floors at 0 rather than 1.
    (size h s) (: n #s
                  u (peep s (n - 1) 0 @ (107 || 75) K
                                        (109 || 77) M
                                        (103 || 71) G)
                  d (u && snip s 0 (n - 1) || s)
                  v <(sound d)                   ; bound, not inlined: a bare infix `//` in the
                                                 ; `:` body slot folds into the binding chain
                  (charm? v && v >= 0 ?
                   (v * (u || M) // wordsz)
                   (say err $ "love: " + h + ": not a size: " + s,
                    cli-leave 2)))
    ; -> (kind . rest): 'eval code.args / 'verb fn.args / 'file prog.args / 'stdin / 'none.
    ; 'none is the command line running out with no program named: -v and -h are not
    ; terminal, so `love -v -h` prints both, reaches the end, and leaves 0.
    (pre a) (a @ (h . t)
      (h @ ("-l" || "--load")    (need h t, load1 <t, pre >t)
           ("-v" || "--version") (say1 (version 0), pre t)
           ("-h" || "--help")    (say1 (usage 0), pre t)
           ("-e" || "--eval")    (need h t, 'eval . t)
           ("-q" || "--quiet")   (pre t @ ('eval . r) ('evalq . r) r r)
           ; slot 0 of a fresh `tune` read, so the other knobs ride and the last -m wins. it
           ; lands at the next collection: the opening pool was sized before argv was read.
           ("-m" || "--memory")  (need h t, tune (pin (tune ()) 0 (size h <t)), pre >t)
           "--"                 (t @ (_ . _) ('file . t) '(stdin))
           (from 'verbs 'word h @ '() ('file . a) f ('verb . f . a)))
      '(none))
    ; stdin is the answer to an EMPTY command line, or to `--` with nothing after it --
    ; never to a line that carried flags and named no program.
    p (two? >ln ? (pre >ln) '(stdin))   ; runs -l preloads + -v/-h + -m; the program kind
    k <p
    av (k @ ('eval || 'evalq || 'verb) >>p >p)   ; the program's view of argv
    run (\ _
      (k @ 'eval  (puts * show $ eval1 <>p, putc 10)  ; -e answers out loud through `show`, the
           'evalq (eval1 <>p)         ; repl's face, not dot's raw one; -q mutes
           'verb  (: r (<>p >>>p)     ; a verb takes the args after its name and answers a status,
                     (cli-leave $ charm0 r))  ; out the one door
           'file  (load1 <>p)
           'none  ()                  ; the flags did their printing; leaving is the answer
           (reads in)))
    [av run]))
       () (\ a b (a == 'cli-leave ? (quit $ charm0 b) (scare a b))))
 (ev [': 'argv ['\ <cli2]], ; the program's view, before the run
  trap (\ x (<>cli2 ()))    ; (run ()): dispatch, argv now the program's view
   () (\ a b (a == 'cli-leave ? (quit $ charm0 b) (scare a b)))))

 (cli-line ln tty)
   (: (charm0 x) (charm? x ? x 0)
      V  (from 'verbs)
      up (V 'unprime)
      av (up ? (<ln . up >ln) ln)   ; the program's view: argv[0], then past the primes
      _  (ev [': 'cmdline ['\ ln] 'argv ['\ av] ])
      f  (V 'seat <ln)
      (? f          (quit * charm0 $ f >ln)
         (two? >av) (cli-rail av)
         tty        (from 'bao 'bao 0)   ; the baked shell core is define-only; this fires it
         (reads in))))                     ; non-tty stdin: bao's stream face drinks the in port
