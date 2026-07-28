; --- lush 🐚 -- the love shell ---
; a cowrie who lives on the quay among lux and the rest of the pulchritude:
; a real (Unix) command shell in love. reads a line, runs real programs via
; spawn + wait -- inherited stdio, so output is LIVE and the command owns the
; terminal (not captured like `run`). this is also the shell init execs on the
; distro console (mk/distro.mk cats the parts to /lib/sh.l): the pid1 story
; end to end (init -> lush -> real programs). it rides the host/posix.c
; job-control nifs (spawn/spawnmap/wait/chdir/cwd) + getenv, fdopen for its
; own pipe ends, and bao's edln for the line.
;
; the parts, in cat order: job.l (state + job control), lex.l (tokens),
; gram.l (the AST), glob.l (fnmatch + pathnames), word.l (expansion),
; eval.l (the engine), line.l (the repl loops), main.l (this file: the entry
; + the seat). run it:
;   out/host/lush                    ; interactive (or cooked when stdin is a pipe)
;   out/host/lush -c "echo hi"       ; one command (multi-line strings fine)
;   out/host/lush script.sh          ; a script file, continuations gathered
;   out/host/love -l lib/lush.l ..   ; the module road to the same seat
; the gate: test/host/sh.l reads the parts bare (cat order) and asserts on the
; sh-* names; the interactive loop is probed live under script(1).

(: ; the script lane: gather lines through the 'more protocol, eval each
   ; complete buffer; `exit` quits from inside, the tail quit carries $? out
   (sh-lines port buf)
     (: line (sh-getline port)
        (? (id? line 'eof)
           (? (tally buf)
              (: _ (say err "lush: unexpected eof\n") (pin sh-status () 2))
              0)
           (: full (? (tally buf) (+ buf (+ "\n" line)) line)
              r (sh-line full)
              (? (two? r) (sh-lines port full) (sh-lines port "")))))
   (sh-file path _args)
     (? (two? (stat path))
        (: p (open path "r") _ (sh-lines p "") _ (close p) (quit (sh-status ())))
        (: _ (say err (+ (+ "lush: " path) ": not found\n")) (quit 127)))
   (sh-c cmd)
     (: r (sh-line cmd)
        _ (? (two? r) (: _ (say err "lush: unexpected eof\n") (pin sh-status () 2)) 0)
        (quit (sh-status ())))
   (sh-main args)
     (? !args (sh 0)
        (: a (cap args)
           (? (= a "-c") (? (cup args) (sh-c (caup args))
                            (: _ (say err "lush: -c needs a command\n") (quit 2)))
              (sh-file a (cup args)))))
   ; the interactive entry: ignore the tty's job signals (children reset them),
   ; then probe with (raw 1): () = a tty (interactive), an errno = not a tty
   ; (the cooked fallback). interactively: install lush-help as the floor IF no
   ; help is present (the bao door -- peek the book, define through ev so a
   ; later (: help 0) uninstall is seen), source ~/.lushrc cooked, seed the
   ; recall from ~/.lush_history. ^D / `exit` ends the shell.
   (sh _) (: _ (signal sig-int 1) _ (signal sig-quit 1) _ (signal sig-tstp 1)
             _ (signal sig-ttou 1) _ (signal sig-ttin 1)
             ok (raw 1)
             (? (nil? ok)
                (: _ (? !(member? 'help (names ())) (ev `(': 'help lush-help)))
                   _ (pin sh-tty () 1)
                   rc (+ (+ "" (getenv "HOME")) "/.lushrc")
                   _ (? (two? (stat rc))
                        (: p (open rc "r") _ (raw 0) _ (sh-lines p "") _ (close p) (raw 1))
                        0)
                   (sh-loop "" (sh-histload 0)))
                (sh-cooked ""))))

; --- the SEAT (kiosko's): fire only when this file IS the program -- the catted
; bin (`lush ..`), an sh-named symlink to it, or a -l load of the assembly.
; `cmdline` is the raw C argv and is never rebound, so the read is
; order-independent inside any cat; init/boot.l's `-l /lib/sh.l -e "(sh 0)"`
; lane stays quiet here ("sh.l" is deliberately NOT a me? name) and fires
; through its -e instead.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "lush") (b = "lush.l") (b = "sh")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a) (? (me? <>a) (link 1 >>a) (seat >>a)) ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) (sh-main >s) ()))
