; --- lush 🐚 -- the love shell ---
; a cowrie who lives on the quay among lux and the rest of the pulchritude:
; a real (Unix) command shell in love. reads a line, runs real programs via
; spawn + wait -- inherited stdio, so output is LIVE and the command owns the
; terminal (not captured like `run`). this is also the shell init execs on the
; distro console (mk/distro.mk cats the parts to /lib/sh.l): the pid1 story
; end to end (init -> lush -> real programs). it rides the host/posix.c
; job-control nifs (spawn/wait/chdir/cwd) + getenv, plus the v2 plumbing
; (pipe/openfd/spawnio/fdclose) for | < > >>.
;
; the parts, in cat order: job.l (state + job control), eval.l (tokenize/
; parse/execute), line.l (the repl loops), main.l (this file: the entry + the
; seat). run it:
;   out/host/lush                    ; interactive (or cooked when stdin is a pipe)
;   out/host/lush -c "echo hi"       ; one command
;   out/host/lush script.sh          ; a file of lines, # comments skipped
;   out/host/love -l lib/lush.l ..   ; the module road to the same seat
; the gate: test/host/sh.l reads the parts bare (cat order) and asserts on the
; sh-* names; the interactive loop is probed live under script(1).

(: ; a line whose first non-blank byte is # is a comment (script/-c lanes only;
   ; in-word # stays a word byte -- real comment lexing is the rung-1 lexer's)
   (sh-cmt? s i) (? (= i (tally s)) 0
                    (: b (s i) (? (sh-ws? b) (sh-cmt? s (+ i 1)) (= b 35))))
   ; the script lane: run a port line by line; `exit` quits from inside sh-run,
   ; a bare `exit` answers 'done and the tail quit carries $? out
   (sh-lines port)
     (: line (sh-getline port)
        (? (id? line 'eof) 0
           (: words (? (sh-cmt? line 0) () (sh-words line))
              (? !words (sh-lines port)
                 (: r (sh-run words) (? (id? r 'done) 0 (sh-lines port)))))))
   (sh-file path _args)
     (? (two? (stat path))
        (: p (open path "r") _ (sh-lines p) _ (close p) (quit (sh-status ())))
        (: _ (say err (+ (+ "lush: " path) ": not found\n")) (quit 127)))
   (sh-c cmd)
     (: words (? (sh-cmt? cmd 0) () (sh-words cmd))
        _ (? !words 0 (sh-run words))
        (quit (sh-status ())))
   (sh-main args)
     (? !args (sh 0)
        (: a (cap args)
           (? (= a "-c") (? (cup args) (sh-c (caup args))
                            (: _ (say err "lush: -c needs a command\n") (quit 2)))
              (sh-file a (cup args)))))
   ; the interactive entry: ignore the tty's job signals (children reset them),
   ; then probe with (raw 1): () = a tty (interactive), an errno = not a tty
   ; (the cooked fallback). ^D / `exit` ends the shell.
   (sh _) (: _ (signal sig-int 1) _ (signal sig-quit 1) _ (signal sig-tstp 1)
             _ (signal sig-ttou 1) _ (signal sig-ttin 1)
             ok (raw 1)
             (? (nil? ok) (: _ (pin sh-tty () 1) (sh-loop ())) (sh-cooked 0))))

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
