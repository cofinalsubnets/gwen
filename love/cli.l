; host CLI driver -- one command line, read left to right after the program-name head:
;   -l/--load <file>    preload a file (read-eval it) before the program
;   -e/--eval <string>  eval the string as one expression
;   -v/--version        print the version and exit
;   -h/--help           print usage and exit
;   --                  end of options: the next arg is the program, the rest are its args
;   <file>              load the file (a program)
; with no program (no -e, no file), read-eval stdin to EOF.
;
; the file defines and does not run -- a body-less top-level `:`, so both names defglob out:
;   (cli-rail ln)      the walk above over ln = (program-name . args)
;   (cli-line ln tty)  a frontend's whole tail: the argv[0] door, the rail, the repl, stdin
; src/main.c calls the second and so does a forked lush child: a word whose PATH winner is
; this binary need not exec, and what it would have exec'd is (cli-line its-line) on a warm heap.
;
; the shadow rule lives in one place, love/verbs.l's `word`: a verb wins over a same-named
; file, but a `/` or a `.l` is always the file, so `love -- up` and `love ./up` read one.
; prel and pat must be up before this file compiles -- @ reads here, and a macro reaches a
; reader only once its layer is spliced.
;
; the defglobs go through `ev` of a built `(: nm 'v)`, since a `:` inside a function body is
; a local binding. the inner quote is load-bearing: without it ev takes the value apart as a
; form to run. argv reaches the program's view before (run ()) fires, which a loaded program reads.
;
; the exit door, kore's two faces (crew/kore/u.l's uleave/urun): every leave -- -v, -h, a bad
; -m, a missing operand, an unopenable file, a torn -e, a verb's own status -- scares
; 'cli-leave, and the trap handler is the only `quit`. that is what lets a caller in the same
; image put its own trap on and get a charm where a spawn hands it a dead process.
; the handler is written out twice because a leave lands in either of two spans, `pre` and
; `run`, and the defglob has to sit between them.
; the line is `ln`, not `a`: `a` is the handler's first parameter.
(: (cli-rail ln)
 (: cli2
 (trap (\ x
 (: (cli-leave c) (scare 'cli-leave c)
    (load1 p) (: q (open p "r")
       (? q (reads q) (say err ("love: cannot open " + p), cli-leave 1)))
    (eval1 s) (: m (nom 0)                           ; a fresh nom: the unfinished shape, id?-safe
       (rdl x acc) (sound x @ 'torn      m           ; every top-level datum off the string, in order
                               (a >< b)  (rdl b (a >< acc))
                               (rev acc))
       ds (rdl s ())
       (? (id? ds m) (say err "love: -e: unfinished form", cli-leave 2)
          (two? ds)  (ev 'ev (two? >ds ? ds <ds))    ; many datums fold to one application, a repl
          ()))                                       ; line's law; one stays as-is; none runs nothing
    (bye s) (puts s, putc 10, flush out, cli-leave 0)
    (version _) (bye $ "love " + love-version)       ; _ arg: love has no nullary calls
    ; the {'tab tab} pattern needs no tablet? guard the way vfind does: a hash pattern emits
    ; one, so an unregistered registry falls out twice over and `+` rides the unit through.
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
    (need h t) (atom? t ? (say err ("love: " + h + " needs an argument"), cli-leave 2))
    ; -m, the footprint cap: slot 0 of a fresh `tune` read, so the other knobs ride and the
    ; last -m on the line wins. 0 is the unbounded spelling, which is why `size` floors at 0.
    ; it lands at the next collection: the pool a session opens with was sized before argv was read.
    (wordsz _) (charm? 1073741824 ? 8 4)             ; 2^30 is a charm only where a fixnum is 62-bit
    ; validate here: a bad value reaching `tune` writes a non-Z tray, which the nif refuses
    ; whole and in silence, so the flag would read as accepted and do nothing.
    (size h s) (: n (tally s)                        ; v is bound, not inlined: a bare infix `//` in
                  u (peep s (n - 1) 0 @ (107 || 75) 1024 (109 || 77) 1048576 (103 || 71) 1073741824 ())
                  d (u ? (snip s 0 (n - 1)) s)                       ; the `:` body slot folds into the
                  v <(sound d)                                       ; binding chain and eats the body
                  _ ((charm? v && v >= 0) ? 0 (say err ("love: " + h + ": not a size: " + s), cli-leave 2))
                  ((v * (u ? u 1048576)) // wordsz ()))
    ; tablet?, never a bare truth test: an unregistered module reads () and (() 'word) is the
    ; church const 1, which would answer h itself and call a command word as a function.
    (vfind h) (: V (from 'verbs) (? (tablet? V) (V 'word h) ()))
    (pre a) (a @ (h >< t)            ; -> (kind . rest): 'eval code.args / 'verb fn.args / 'file prog.args / 'stdin
      (h @ ("-l" || "--load")    (need h t, load1 <t, pre >t)
           ("-v" || "--version") (version ())
           ("-h" || "--help")    (usage ())
           ("-e" || "--eval")    (need h t, 'eval >< t)
           ("-n" || "--silent")  (pre t @ ('eval >< r) ('evalq >< r) r r)
           ("-m" || "--memory")  (need h t, tune (pin (tune ()) 0 (size h <t)), pre >t)
           "--"                 (two? t ? ('file >< t) '(stdin))
           (: f (vfind h)       ; a positional: a registered verb first, else the program file
              (f ? ('verb >< f >< a) ('file >< a))))
      '(stdin))
    p (pre >ln)                      ; runs -l preloads + -v/-h + -m; the program kind
    k <p
    av (k @ ('eval || 'evalq || 'verb) >>p >p)   ; the program's view of argv
    run (\ _
      (k @ 'eval  (print out (eval1 <>p), put out 10)  ; -e answers out loud through `print`, the
           'evalq (eval1 <>p)         ; repl's face, not dot's raw one; -n mutes
           'verb  (: r (<>p >>>p)     ; a verb takes the args after its name and answers a status,
                     (cli-leave (charm? r ? r 0)))  ; out the one door
           'file  (load1 <>p)
           (reads in)))
    [av run]))
       () (\ a b (id? a 'cli-leave ? (quit (charm? b ? b 0)) (scare a b))))
 _ (ev (list ': 'argv (list 'quote <cli2)))   ; the program's view, before the run
 (trap (\ x (<>cli2 ()))                      ; (run ()): dispatch, argv now the program's view
     () (\ a b (id? a 'cli-leave ? (quit (charm? b ? b 0)) (scare a b)))))

 ; a frontend's whole tail: the argv[0] verb, the rail, the repl, the stdin drink, in that
 ; order. `tty` is the caller's isatty answer, C's to know -- a forked lush child says 0,
 ; having a shell of its own already.
 ; cmdline and argv are defglob'd here rather than read: a forked child arrives with both
 ; still naming the shell's invocation.
 ; ln is the whole invocation, `wake IMAGE` included, and the program's view comes off it
 ; through the registry's own `unprime` -- the two words src/main.c counts as `skip`.
 ; the argv[0] door fires even at argc 1, where a bare `seed` symlink wants its usage; a verb
 ; that quits internally never returns. tablet?, never a bare truth test: (() 'seat) is the
 ; church const 1, which would answer argv[0] itself and dispatch it.
 (cli-line ln tty)
   (: V  (from 'verbs)
      up (? (tablet? V) (V 'unprime) ())
      av (? up (cap ln >< (up (cup ln))) ln)   ; the program's view: argv[0], then past the primes
      _  (ev (list ': 'cmdline (list 'quote ln)))
      _  (ev (list ': 'argv (list 'quote av)))
      f  (? (tablet? V) (V 'seat (cap ln)) ())
      (? f         (: r (f (cup ln)) (quit (? (charm? r) r 0)))
         (two? >av) (cli-rail av)
         tty        ((from 'bao 'bao) 0)   ; the baked shell core is define-only; this fires it
         (reads in))))                     ; non-tty stdin: bao's stream face drinks the in port
