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
;   out/host/lush --login ..         ; a login shell: /etc/profile + ~/.profile first
;   out/host/love -l lib/lush.l ..   ; the module road to the same seat
; the gate: test/host/sh.l reads the parts bare (cat order) and asserts on the
; sh-* names; the interactive loop is probed live under script(1).

(: ; source a file through the line machinery, quietly absent (the rc/profile floor)
   (sh-src path)
     (? (two? (stat path))
        (: p (open path "r") _ (sh-lines p "") _ (close p) 0)
        0)
   ; a LOGIN shell (--login, or a dash-led seat name) reads /etc/profile then
   ; ~/.profile, whatever mode it runs in; interactive shells layer $ENV (else
   ; ~/.lushrc) on top, in (sh _) below. the chsh story lives in doc/lush.md.
   (sh-profiles _)
     (? (sh-login ())
        (: _ (sh-src "/etc/profile") (sh-src (+ (+ "" (getenv "HOME")) "/.profile")))
        0)
   ; the script lane rides eval.l's sh-lines ('more gathers continuations,
   ; `exit` quits from inside); $0 and the positionals land before the run
   (sh-file path args)
     (? (two? (stat path))
        (: _ (pin sh-arg0 () path)
           _ (pin sh-pos () args)
           _ (sh-profiles 0)
           p (open path "r") _ (sh-lines p "") _ (close p) (quit (sh-status ())))
        (: _ (say err (+ (+ "lush: " path) ": not found\n")) (quit 127)))
   (sh-c cmd rest)                              ; -c CMD [name [args..]], POSIX
     (: _ (pin sh-arg0 () (? (two? rest) <rest "lush"))
        _ (pin sh-pos () (? (two? rest) >rest ()))
        _ (sh-profiles 0)
        r (sh-line cmd)
        _ (? (two? r) (: _ (say err "lush: unexpected eof\n") (pin sh-status () 2)) 0)
        (quit (sh-status ())))
   ; the flag word: c e u x, bundled or apart. `sh -ec CMD` is what a Makefile's
   ; .SHELLFLAGS writes and what every shell takes, so a bundle is not a courtesy --
   ; without it lush cannot BE the SHELL of a make. answers (1 c? rest) with e/u/x
   ; already pinned, or () when a letter is not ours (then the word is not a flag).
   (sh-flagw a rest)
     (: n (tally a)
        (go k c?) (? (<= n k) (L 1 c? rest)
                     (: ch (a k)
                        (? (= ch 99) (go (+ k 1) 1)
                           (= ch 101) (: _ (pin sh-eflag () 1) (go (+ k 1) c?))
                           (= ch 117) (: _ (pin sh-uflag () 1) (go (+ k 1) c?))
                           (= ch 120) (: _ (pin sh-xflag () 1) (go (+ k 1) c?))
                           ())))
        (go 1 0))
   (sh-main args)
     (? !args (sh 0)
        (: a <args
           (? (= a "--login") (: _ (pin sh-login () 1) (sh-main >args))
              (&& (< 1 (tally a)) (= (a 0) 45))
                (: fw (sh-flagw a >args)
                   (? !(two? fw) (sh-file a >args)          ; a letter not ours: a name
                      (sh-nth fw 1)
                        (: r (sh-nth fw 2)
                           (? r (sh-c <r >r)
                              (: _ (say err "lush: -c needs a command\n") (quit 2))))
                      (sh-main (sh-nth fw 2))))            ; flags only: read on
              (sh-file a >args))))
   ; the interactive entry: ignore the tty's job signals (children reset them),
   ; then probe with (raw 1): () = a tty (interactive), an errno = not a tty
   ; (the cooked fallback). interactively: hear lush-help as the floor IF no
   ; help is heard yet (the bao door), source the profiles (login only) then
   ; $ENV else ~/.lushrc cooked -- AFTER the floor lands, so a broken rc scares
   ; back to the prompt instead of killing the shell -- and seed the recall from
   ; ~/.lush_history. ^D / `exit` ends the shell.
   (sh _) (: _ (signal sig-int 1) _ (signal sig-quit 1) _ (signal sig-tstp 1)
             _ (signal sig-ttou 1) _ (signal sig-ttin 1)
             ok (raw 1)
             (? (nil? ok)
                (: _ (? (nil? (heard ())) (hear lush-help))
                   _ (pin sh-tty () 1)
                   erc (+ "" (getenv "ENV"))
                   rc (? (tally erc) erc (+ (+ "" (getenv "HOME")) "/.lushrc"))
                   _ (raw 0) _ (sh-profiles 0) _ (sh-src rc) _ (raw 1)
                   (sh-loop "" (sh-histload 0)))
                (: _ (sh-profiles 0) (sh-cooked "")))))

; --- the SEAT (kiosko's): fire only when this file IS the program -- the catted
; bin (`lush ..`), an sh-named symlink to it, or a -l load of the assembly.
; `cmdline` is the raw C argv and is never rebound, so the read is
; order-independent inside any cat; init/boot.l's `-l /lib/sh.l -e "(sh 0)"`
; lane stays quiet here ("sh.l" is deliberately NOT a me? name) and fires
; through its -e instead.
; a dash-led basename (-lush, -sh: login(1)'s mark) seats us too and makes a LOGIN
; shell -- the scan strips the dash to match and hands the RAW word back, which is
; the whole reason it hands one back. (the env -S shebang usually eats the dash, so
; --login is the reliable door, doc/lush.md.)
(: _ (pins (from 'verbs 'tab) 'sh sh-main 'lush sh-main)
   (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   s ((from 'verbs 'fire) ["lush" "lush.l" "sh"])
   (? (two? s) (: w (bn <s)
                  _ (? (tally w && (w 0 = 45)) (pin sh-login () 1) 0)
                  (sh-main >s))
      ()))
