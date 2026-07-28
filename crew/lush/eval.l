; --- lush 🐚 tokenize / parse / execute -- the v2 engine ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; v2: '...' / "..." quoting, pipes (a | b | c), redirects (< > >>). operators are
; SPACE-SEPARATED (`a | b`, `cmd > file`); glued lexing (`a>b`) is a later pass. a
; missing command shows as the child's exit 127 (spawn forks then execs); pipes wire
; fds in C (spawnio's child dup2s + closes the pipe ends, so a downstream reader EOFs).
;
; step 5 (env + status): the tokenizer expands $NAME and $? in unquoted words and
; inside ".." ('..' stays literal; an unset var is the unit, so its word drops away);
; $? is the last foreground status (a stop folds to 128+sig); export NAME=VAL rides
; the setenv nif, exit [n] the quit nif, and cd sets $?.

(: ; --- step 5: $ expansion + $? -- the tokenizer expands $NAME (alnum/_ run) and
   ; $? in UNQUOTED words and inside "..."; '...' stays literal. an unset var is the
   ; unit, so it concatenates away, and a bare unset word never starts (bash's
   ; empty-removal for free). $? is the last foreground status: the last stage's
   ; exit, a stop folded to 128+sig (the shell convention).
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
   ; export NAME=VAL -> setenv; a word without '=' is a quiet no-op (we have no
   ; local-vs-env split to promote from)
   (sh-eqpos s i) (? (= i (tally s)) () (? (= (s i) 61) i (sh-eqpos s (+ i 1))))
   (sh-export w)
     (: p (sh-eqpos w 0)
        (? (nil? p) 0 (setenv (snip w 0 p) (snip w (+ p 1) (tally w)))))

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
              0))))
