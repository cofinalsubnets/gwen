; --- sh: a real (Unix) command shell in love ---
; reads a line, runs real programs via spawn + wait -- inherited stdio, so output is
; LIVE and the command owns the terminal (not captured like `run`). this is the shell
; init execs on the console: the pid1 story end to end (init -> sh -> real programs).
; it rides the host/posix.c job-control nifs (spawn/wait/chdir/cwd) + getenv, plus the
; v2 plumbing (pipe/openfd/spawnio/fdclose) for | < > >>. run:
;   out/host/love -l init/sh.l -e "(sh 0)"
; line editing on a real tty is bao's own editor (`edln`, the baked shell core):
; in-line editing + up/down command history. `-l/-e` pass argv so main.c skips raw
; mode -- sh raws the tty itself (edln owns echo) and drops back to cooked around
; each child (programs want a normal tty), exactly the readline dance. piped /
; non-tty input falls back to the cooked-tty line reader (no editing, no history).
; EOF (^D) ends the shell; the loop is tail-recursive (no stack growth across commands).
;
; v2: '...' / "..." quoting, pipes (a | b | c), redirects (< > >>). operators are
; SPACE-SEPARATED (`a | b`, `cmd > file`); glued lexing (`a>b`) is a later pass. a
; missing command shows as the child's exit 127 (spawn forks then execs); pipes wire
; fds in C (spawnio's child dup2s + closes the pipe ends, so a downstream reader EOFs).
;
; v3 (doc/posix.md step 4): JOB CONTROL. interactively, every job leads its own
; process group and the first stage takes the terminal (spawnio's pg/fg args), so
; ^C and ^Z reach exactly the foreground job -- the shell ignores INT/QUIT/TSTP/
; TTOU/TTIN (children reset them before exec) and reclaims the tty after each job
; (ttyfg). a ^Z stop comes back from `wait` as 256+sig and parks in the job table;
; jobs / fg [n] / bg [n] are builtins (SIGCONT rides bao's `still`), a trailing &
; backgrounds the line, and finished background jobs report at the next prompt
; (hear-drain). ^C at the PROMPT abandons the line (edln's -12). gated headless in
; boot/sh.l; the pty story (stop/fg/interrupt) probed live under script(1).
;
; step 5 (env + status): the tokenizer expands $NAME and $? in unquoted words and
; inside ".." ('..' stays literal; an unset var is the unit, so its word drops away);
; $? is the last foreground status (a stop folds to 128+sig); export NAME=VAL rides
; the setenv nif, exit [n] the quit nif, and cd sets $?. L0's staging is complete.

(: (sh-rev l) (foldl (\ a x (link x a)) () l)
   (sh-nth l i) (? (= i 0) (cap l) (sh-nth (cup l) (- i 1)))

   ; --- v3: job control (doc/posix.md step 4) -- the shell IGNORES the tty's job
   ; signals (installed in (sh _) below; spawn's child side resets them to default),
   ; so ^C/^Z reach only the foreground command. a ^Z stop comes back from `wait`
   ; as 256+sig (above every exit status) and the job parks in `sh-jobs`:
   ; n -> (pids state cmd), state 'stopped | 'running. builtins: jobs / fg / bg;
   ; a trailing & runs the pipeline unwaited as a 'running job. signal numbers are
   ; the Linux/x86 charms (the repo's home ABI; signalfd is Linux-only already).
   sig-int 2 sig-quit 3 sig-tstp 20 sig-cont 18 sig-ttou 22 sig-ttin 21
   sh-jobs (tablet 0)
   sh-njob #0
   sh-tty #0                                   ; 1 = interactive: jobs get their own pgrp + the tty
   ; interactive spawns lead a fresh process group (pg 0) and the first stage takes
   ; the terminal (fg 1) -- a job in its OWN group is what makes ^Z real (a stop to
   ; an orphaned group is discarded, and the shell's group is orphaned under a
   ; nested session). the cooked/non-tty lane keeps the legacy in-shell pgrp (-1).
   (sh-pgm _) (? (sh-tty ()) 0 (0 - 1))
   (sh-fgm _) (? (sh-tty ()) 1 0)

   ; --- step 5: $ expansion + $? -- the tokenizer expands $NAME (alnum/_ run) and
   ; $? in UNQUOTED words and inside "..."; '...' stays literal. an unset var is the
   ; unit, so it concatenates away, and a bare unset word never starts (bash's
   ; empty-removal for free). $? is the last foreground status: the last stage's
   ; exit, a stop folded to 128+sig (the shell convention).
   sh-status #0
   (sh-name? b) (? (&& (<= 48 b) (< b 58)) 1
                   (&& (<= 65 b) (< b 91)) 1
                   (&& (<= 97 b) (< b 123)) 1
                   (= b 95))
   (sh-vname s i n cur)                                ; the $NAME run -> (name . i')
     (? (= i n) (link cur i)
        (: b (s i) (? (sh-name? b) (sh-vname s (+ i 1) n (+ cur b)) (link cur i))))
   (sh-dollar s i n cur started q acc)                 ; just past the $; resume mode q (0 | 34)
     (? (&& (< i n) (= (s i) 63))
        (sh-resume s (+ i 1) n (+ cur (show (sh-status ()))) 1 q acc)
        (: nm (sh-vname s i n "") name (cap nm) i2 (cup nm)
           (? !(tally name)
              (sh-resume s i2 n (+ cur "$") 1 q acc)   ; a lone $ is literal
              (: v (getenv name)
                 (sh-resume s i2 n (+ cur v) (? started 1 (!! (tally v))) q acc)))))
   (sh-resume s i n cur started q acc)
     (? (= q 34) (sh-quoted s i n cur 34 acc)
        (sh-sc s i n cur started acc))
   (sh-unwords ws) (foldl (\ a w (? (tally a) (+ (+ a " ") w) w)) "" ws)
   (sh-addjob pids state cmd)
     (: n (+ 1 (sh-njob ())) _ (pin sh-njob () n) _ (pin sh-jobs n (L pids state cmd)) n)
   (sh-jobline n j)
     (+ "[" (+ (show n) (+ "] " (+ (show (sh-nth j 1)) (+ "  " (+ (sh-nth j 2) "\n"))))))
   (sh-stopped? sts) (foldl (\ a x (? a a (< 255 x))) 0 sts)
   ; wait a foreground job; a stop registers + reports it instead of hanging the prompt
   (sh-waitfg pids cmd)
     (: sts (map (\ p (wait p)) pids)
        _   (? (sh-tty ()) (ttyfg 0) 0)                ; the shell takes the terminal back
        _   (pin sh-status ()                          ; $?: the last stage; a stop folds to 128+sig
             (: st (last sts) (? (&& (charm? st) (< 255 st)) (- st 128) st)))
        (? (sh-stopped? sts)
           (: n (sh-addjob pids 'stopped cmd)
              _ (puts (sh-jobline n (sh-jobs n))) 0)
           0))
   ; background bookkeeping: drain hear at each prompt; a reaped pid leaves its job,
   ; an emptied job prints done and goes
   (sh-unpid l p) (? !l () (? (= (cap l) p) (cup l) (link (cap l) (sh-unpid (cup l) p))))
   (sh-bgnote pid)
     (each (keys sh-jobs)
       (\ n (: j (sh-jobs n)
               (? !j 0
                  (: ps (sh-unpid (sh-nth j 0) pid)
                     (? (= ps (sh-nth j 0)) 0
                        !ps (: _ (puts (+ "[" (+ (show n) (+ "] done  " (+ (sh-nth j 2) "\n")))))
                               (pull sh-jobs n 0))
                        (pin sh-jobs n (L ps (sh-nth j 1) (sh-nth j 2)))))))))
   (sh-reapbg _)
     (: r (hear 0) (? (two? r) (: _ (sh-bgnote (cap r)) (sh-reapbg 0)) 0))
   ; job addressing: "fg 2" by number, bare "fg" the latest
   (sh-int s) (sh-intl s 0 0)
   (sh-intl s i a) (? (= i (tally s)) a (sh-intl s (+ i 1) (+ (* a 10) (- (s i) 48))))
   (sh-lastjob _) (foldl (\ a k (? (< a k) k a)) 0 (keys sh-jobs))
   (sh-jobn argv) (? (cup argv) (sh-int (caup argv)) (sh-lastjob 0))
   ; export NAME=VAL -> setenv; a word without '=' is a quiet no-op (we have no
   ; local-vs-env split to promote from)
   (sh-eqpos s i) (? (= i (tally s)) () (? (= (s i) 61) i (sh-eqpos s (+ i 1))))
   (sh-export w)
     (: p (sh-eqpos w 0)
        (? (nil? p) 0 (setenv (snip w 0 p) (snip w (+ p 1) (tally w)))))
   (sh-fg n)
     (: j (sh-jobs n)
        (? !j (: _ (puts "fg: no such job\n") 0)
           (: pids (sh-nth j 0) cmd (sh-nth j 2)
              _ (pull sh-jobs n 0)
              _ (? (sh-tty ()) (ttyfg (cap pids)) 0)   ; the job's group gets the terminal first
              _ (each pids (\ p (still p sig-cont)))
              (sh-waitfg pids cmd))))
   (sh-bg n)
     (: j (sh-jobs n)
        (? !j (: _ (puts "bg: no such job\n") 0)
           (: _ (each (sh-nth j 0) (\ p (still p sig-cont)))
              _ (pin sh-jobs n (L (sh-nth j 0) 'running (sh-nth j 2)))
              _ (puts (sh-jobline n (sh-jobs n)))
              0)))
   ; a trailing "&" (its own word, like the other operators) backgrounds the line
   (sh-amp? ws) (? !ws 0 (? (two? (cup ws)) (sh-amp? (cup ws)) (= "&" (cap ws))))
   (sh-drop-amp ws) (? (two? (cup ws)) (link (cap ws) (sh-drop-amp (cup ws))) ())

   ; --- read one line (cooked tty): bytes until \n; 'eof at immediate EOF.
   (sh-getline port) (: c (see port) (? (< c 0) 'eof (= c 10) "" (+ (+ "" c) (sh-rest port))))
   (sh-rest port)    (: c (see port) (? (< c 0) ""   (= c 10) "" (+ (+ "" c) (sh-rest port))))

   ; --- tokenize: whitespace split, with '...' / "..." quoting (literal contents).
   ; operators (| < > >>) are ordinary space-separated word tokens, parsed below.
   (sh-ws? b) (? (= b 32) 1 (? (= b 9) 1 (= b 10)))
   (sh-words s) (sh-sc s 0 (tally s) "" 0 ())
   (sh-sc s i n cur started acc)
     (? (= i n) (sh-rev (? started (link cur acc) acc))
        (: b (s i)
           (? (sh-ws? b) (sh-sc s (+ i 1) n "" 0 (? started (link cur acc) acc))
              (= b 34)   (sh-quoted s (+ i 1) n cur 34 acc)        ; "
              (= b 39)   (sh-quoted s (+ i 1) n cur 39 acc)        ; '
              (= b 36)   (sh-dollar s (+ i 1) n cur started 0 acc) ; $ expands
              (sh-sc s (+ i 1) n (+ cur b) 1 acc))))
   (sh-quoted s i n cur q acc)                                     ; literal until the matching quote
     (? (= i n) (sh-rev (link cur acc))
        (: b (s i) (? (= b q) (sh-sc s (+ i 1) n cur 1 acc)
                      (&& (= b 36) (= q 34)) (sh-dollar s (+ i 1) n cur 1 34 acc)   ; $ expands in "..", not '..
                      (sh-quoted s (+ i 1) n (+ cur b) q acc))))

   ; --- parse: split words on "|" into stages; pull < > >> + their files from each.
   (sh-split words) (sh-sp words () ())
   (sh-sp words cur acc)
     (? !words (sh-rev (link (sh-rev cur) acc))
        (: w (cap words) (? (= w "|") (sh-sp (cup words) () (link (sh-rev cur) acc))
                            (sh-sp (cup words) (link w cur) acc))))
   (sh-redirs words) (sh-rd words () () () 0)                      ; -> (argv infile outfile append?)
   (sh-rd words argv in out app)
     (? !words (L (sh-rev argv) in out app)
        (: w (cap words)
           (? (= w "<")  (sh-rd (cuup words) argv (caup words) out app)
              (= w ">")  (sh-rd (cuup words) argv in (caup words) 0)
              (= w ">>") (sh-rd (cuup words) argv in (caup words) 1)
              (sh-rd (cup words) (link w argv) in out app))))

   ; --- execute --------------------------------------------------------------------
   (sh-makepipes n) (? (< n 1) () (link (pipe 0) (sh-makepipes (- n 1))))
   (sh-flat ps) (? !ps () (link (cap (cap ps)) (link (cup (cap ps)) (sh-flat (cup ps)))))  ; (r w r w ..)

   ; one pipeline stage: wire in/out from the pipes (a file redirect overrides), spawn.
   ; pg/fg ride through to spawnio: stage 0 may LEAD a group and take the tty; later
   ; stages JOIN the leader's group (sh-stages threads the first pid back in as pg).
   (sh-stage stage pipes allfds i last pg fg)
     (: rd (sh-redirs stage) argv (sh-nth rd 0) inf (sh-nth rd 1) outf (sh-nth rd 2) app (sh-nth rd 3)
        pin  (? (= i 0)    (0 - 1) (cap (sh-nth pipes (- i 1))))   ; read prev pipe
        pout (? (= i last) (0 - 1) (cup (sh-nth pipes i)))         ; write this pipe
        ifd  (? inf  (openfd inf 0)            pin)
        ofd  (? outf (openfd outf (? app 2 1)) pout)
        pid  (spawnio argv ifd ofd (0 - 1) allfds pg fg)
        _    (? inf  (fdclose ifd) 0)  _ (? outf (fdclose ofd) 0)    ; parent drops the file fds
        pid)
   (sh-stages stages pipes allfds i last pg fg)
     (? !stages ()
        (: p (sh-stage (cap stages) pipes allfds i last pg (? (= i 0) fg 0))
           (link p (sh-stages (cup stages) pipes allfds (+ i 1) last
                              (? (< pg 0) pg (? (= i 0) p pg)) fg))))

   ; single stage, no pipe: builtins, else a file-redirected spawn + wait.
   (sh-single stage)
     (: rd (sh-redirs stage) argv (sh-nth rd 0) inf (sh-nth rd 1) outf (sh-nth rd 2) app (sh-nth rd 3)
        (? !argv 0
           (: cmd (cap argv)
              (? (= cmd "exit") (? (cup argv) (quit (sh-int (caup argv))) 'done)
                 (= cmd "cd")   (: r (chdir (? (cup argv) (caup argv) (getenv "HOME")))
                                   _ (pin sh-status () (? (nil? r) 0 1)) 0)
                 (= cmd "export") (: _ (each (cup argv) sh-export) 0)
                 (= cmd "pwd")  (: _ (puts (+ (cwd 0) "\n")) 0)
                 (= cmd "jobs") (: _ (each (sort (keys sh-jobs))
                                           (\ n (puts (sh-jobline n (sh-jobs n))))) 0)
                 (= cmd "fg")   (sh-fg (sh-jobn argv))
                 (= cmd "bg")   (sh-bg (sh-jobn argv))
                 (: ifd (? inf  (openfd inf 0)            (0 - 1))
                    ofd (? outf (openfd outf (? app 2 1)) (0 - 1))
                    pid (spawnio argv ifd ofd (0 - 1) () (sh-pgm 0) (sh-fgm 0))
                    _   (? inf  (fdclose ifd) 0)  _ (? outf (fdclose ofd) 0)
                    _   (sh-waitfg (L pid) (sh-unwords argv))
                    0)))))

   (sh-run words)
     (? (sh-amp? words) (sh-runbg (sh-drop-amp words)) (sh-runfg words))
   (sh-runbg words)                                               ; spawn unwaited, job it
     (: stages (sh-split words)
        n      (- (tally stages) 1)
        pipes  (sh-makepipes n)
        allfds (sh-flat pipes)
        pids   (sh-stages stages pipes allfds 0 n (sh-pgm 0) 0)
        _      (each allfds (\ fd (fdclose fd)))
        jn     (sh-addjob pids 'running (+ (sh-unwords words) " &"))
        _      (puts (+ "[" (+ (show jn) (+ "] " (+ (sh-unwords (map show pids)) "\n")))))
        0)
   (sh-runfg words)
     (: stages (sh-split words)
        (? (< (tally stages) 2) (sh-single (cap stages))
           (: n      (- (tally stages) 1)
              pipes  (sh-makepipes n)
              allfds (sh-flat pipes)
              pids   (sh-stages stages pipes allfds 0 n (sh-pgm 0) (sh-fgm 0))
              _      (each allfds (\ fd (fdclose fd)))             ; parent closes every pipe fd
              _      (sh-waitfg pids (sh-unwords words))          ; the whole pipeline, ^Z-aware
              0)))

   ; --- the REPL: a real tty gets edln (in-line editing + up/down history);
   ; piped / non-tty input gets the cooked-tty line reader. (sh _) probes with
   ; (raw 1): () = a tty (interactive), an errno = not a tty (cooked fallback).
   ; both loops are tail-recursive; ^D / `exit` ends the shell.
   (sh-cooked _)
     (: _    (sh-reapbg 0)
        _    (say out (+ (cwd 0) "$ "))  _ (flush out)
        line (sh-getline in)
        (? (id? line 'eof) (puts "\n")
           (: words (sh-words line)
              (? !words (sh-cooked 0)
                 (: r (sh-run words) (? (id? r 'done) 0 (sh-cooked 0)))))))
   ; interactive: edln owns echo, so raw the tty for the prompt and drop to cooked
   ; around each child. edln returns (line . nhist) -- the raw charlist + the grown
   ; history -- or 'eof; thread nhist back in so up/down recall across commands.
   (sh-loop hist)
     (: _ (sh-reapbg 0)
        r (edln in (+ (cwd 0) "$ ") hist)
        (? (id? r 'eof) (: _ (raw 0) (puts "\n"))                 ; cooked, newline, end
           (: line (cap r) nhist (cup r)
              words (sh-words (slurp (tap line)))                 ; charlist -> string for the tokenizer
              (? !words (sh-loop nhist)
                 (: _  (raw 0)                                    ; cooked for the child
                    rr (sh-run words)
                    (? (id? rr 'done) (raw 0)
                       (: _ (raw 1) (sh-loop nhist))))))))         ; re-raw for the next prompt
   (sh _) (: _ (signal sig-int 1) _ (signal sig-quit 1) _ (signal sig-tstp 1)
             _ (signal sig-ttou 1) _ (signal sig-ttin 1)
             ok (raw 1)
             (? (nil? ok) (: _ (pin sh-tty () 1) (sh-loop ())) (sh-cooked 0))))
