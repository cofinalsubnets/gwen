; --- test/host/sh.l: lush's gate -- the lexer / grammar / expansion / glob pure
; functions, the job-control nifs (signal / wait's stop lane / sigfd's signal
; list: doc/posix.md step 4, signals arriving as conditions), and the BUILT
; shell driven end to end through out/host/love (never env's PATH love). the
; interactive loop needs a pty (probed live, the berth pattern); everything
; beneath it gates headless here. the parts read bare in cat order; main.l's
; seat stays quiet under the corpus (no lush on cmdline).
; run: cat test/00-init.l test/host/sh.l | out/host/love
(reads (open "crew/lush/job.l" "r"))
(reads (open "crew/lush/lex.l" "r"))
(reads (open "crew/lush/gram.l" "r"))
(reads (open "crew/lush/glob.l" "r"))
(reads (open "crew/lush/word.l" "r"))
(reads (open "crew/lush/eval.l" "r"))
(reads (open "crew/lush/line.l" "r"))
(reads (open "crew/lush/main.l" "r"))

; the lexer: glued operators, io-numbers out of band ((), or (n) attached),
; quotes as segments, # comments, the 'more protocol on an unfinished shape.
; (the reader has no dotted-pair syntax, so expecteds are BUILT, not quoted.)
(: lw (\ s (link 'w (L (link 'lit s))))
   rdt (\ ion o (link 'rd (link ion o)))
   opt (\ o (link 'op o))
   (assert
    (= (L 1 (lw "a") (lw "b")) (sh-lex " a  b "))
    (= (L 1 (link 'w (L (link 'sq "x  y")))) (sh-lex "'x  y'"))
    (= (L 1 (link 'w (L (link 'lit "a") (link 'dq (L (link 'lit "b c"))) (link 'lit "d"))))
       (sh-lex "a\"b c\"d"))
    (= (L 1 (lw "a") (rdt () ">") (lw "b")) (sh-lex "a>b"))
    (= (L 1 (lw "x") (rdt (L 2) ">&") (lw "1")) (sh-lex "x 2>&1"))
    (= (L 1 (lw "a") (opt "&&") (lw "b")) (sh-lex "a&&b"))
    (= (L 1 (lw "a") (opt ";") (lw "b")) (sh-lex "a; b"))
    (= (L 1 (lw "a")) (sh-lex "a # rest dies"))
    (= (L 1 (link 'w (L (link 'var "HOME")))) (sh-lex "$HOME"))
    (= (L 1 (link 'w (L (link 'dq (L (link 'lit "v=") (link 'var "V")))))) (sh-lex "\"v=$V\""))
    (= (L 1 (link 'w (L (link 'sub "echo hi")))) (sh-lex "$(echo hi)"))
    (= (L 1 (link 'w (L (link 'sub "date")))) (sh-lex "`date`"))
    (id? (cap (sh-lex "echo 'x")) 'more)
    (id? (cap (sh-lex "echo \\")) 'more)))

; the grammar: pipelines under && under ; -- and & backgrounds only its own
; and_or; a dangling && is parse-'more (the PS2 lane); compounds refuse politely
(: pr (sh-parse (cup (sh-lex "a | b && c ; d &")))
   a (cup pr)
   (assert (id? (cap pr) 'ok) (id? (cap a) 'seq)
           (id? (cap (sh-nth a 1)) 'and)
           (id? (cap (sh-nth (sh-nth a 1) 1)) 'pipe)
           (id? (cap (sh-nth a 2)) 'bg)))
(: pr (sh-parse (cup (sh-lex "a | b")))
   a (cup pr)
   (assert (id? (cap a) 'pipe) (= 2 (tally (sh-nth a 1)))))
(: pr (sh-parse (cup (sh-lex "! false")))
   (assert (id? (cap (cup pr)) 'not)))
(assert (id? (cap (sh-parse (cup (sh-lex "a &&")))) 'more)
        (id? (cap (sh-parse (cup (sh-lex "( x )")))) 'err))

; expansion: POSIX order -- splitting only over expansion output, quotes
; shield, an unset var alone is NO field, a quoted empty IS one, glob last
(: _ (setenv "LUSH_T1" "quay")
   _ (setenv "LUSH_T2" "a  b")
   v (\ nm (link 'var nm))
   (assert
    (= '("pre-quay") (sh-fields (L (link 'lit "pre-") (v "LUSH_T1"))))
    (= '("a" "b") (sh-fields (L (v "LUSH_T2"))))
    (= '("a  b") (sh-fields (L (link 'dq (L (v "LUSH_T2"))))))
    (= () (sh-fields (L (v "LUSH_NIL_ZZ"))))
    (= '("") (sh-fields (L (link 'sq ""))))
    (= '("quay") (sh-fields (L (link 'sub "echo quay"))))
    (= '("crew/lush/job.l") (sh-fields (L (link 'lit "crew/lush/j*.l"))))
    (= '("crew/zz*qq") (sh-fields (L (link 'lit "crew/zz*qq"))))
    (= '("crew/lush/j*.l") (sh-fields (L (link 'sq "crew/lush/j*.l"))))))
(: _ (pin sh-status () 7)
   (assert (= '("7") (sh-fields (L (link 'var "?"))))))

; fnmatch + the glob walk: classes, ranges, negation, the mask law
(assert (sh-match "*.l" "job.l") (! (sh-match "*.l" "job.c"))
        (sh-match "j??.l" "job.l") (sh-match "[a-j]ob.l" "job.l")
        (! (sh-match "[!a-j]ob.l" "job.l"))
        (= '("crew/lush/job.l") (sh-glob "crew/lush/j[m-p]b.l" (sh-ones 19))))

; (signal sig 1) ignores: a self-INT that would kill us just fizzles; restore after
(: me (getpid 0)
   _ (assert (id? (signal sig-int 1) ()))
   _ (still me sig-int)
   _ (assert 1)                                     ; still here: the ignore held
   _ (assert (id? (signal sig-int 0) ())))

; wait's stop lane: a SIGSTOPped child answers 256+sig (a charm past every exit
; status -- the shell's "park it as a job" signal), then CONT + KILL ends it 137
(: p (spawn (L "sleep" "5"))
   _ (still p 19)                                   ; SIGSTOP
   (assert (= (+ 256 19) (wait p))
           (: _ (still p sig-cont) _ (still p 9) (= 137 (wait p)))))

; sigfd takes a signal LIST now: SIGUSR1 blocked + queued, kill self, take the
; record -- the signal is DATA, (signo . pid)
(: sp (sigfd (L 10))
   me (getpid 0)
   _  (still me 10)
   ev (sigtake sp)
   (assert (hot? sp) (two? ev) (= 10 (cap ev)) (= me (cup ev))))

; ... and a taken signal re-raised through the condition system IS the doc's
; "signals arrive as conditions": scare routes it to the installed help policy
(: help (\ s a b (L 'gotsig a b)))
(assert (= (L 'gotsig 'sigusr1 (getpid 0)) (scare 'sigusr1 (getpid 0))))
(: help 0)

; env nifs: set, read back through environ's raw K=V, absence unsets
(assert (id? (setenv "LOVE_SH_T5" "quay") ())
        (= "quay" (getenv "LOVE_SH_T5"))
        (foldl (\ a e (? a a (= e "LOVE_SH_T5=quay"))) 0 (environ 0)))
(assert (id? (setenv "LOVE_SH_T5" ()) ())                              ; absence unsets
        (id? (getenv "LOVE_SH_T5") ()))
; $? and export, through the shell's own line entry
(: _ (sh-line "sh -c 'exit 7'")
   (assert (= 7 (sh-status ())) (= '("7") (sh-fields (L (link 'var "?"))))))
(: _ (sh-line "export LOVE_SH_T6=pier")
   (assert (= "pier" (getenv "LOVE_SH_T6"))))

; the BUILT shell, end to end: pipes, lists, cmdsub, glob, 2>&1 -- driven
; through out/host/love so the tree's own nifs serve (env's love is the nest's)
(: r (run (L "out/host/love" "out/host/lush" "-c" "echo a b | tr a-z A-Z"))
   (assert (= 0 (cap r)) (= "A B\n" (cup r))))
(: r (run (L "out/host/love" "out/host/lush" "-c" "false && echo no || echo yes"))
   (assert (= "yes\n" (cup r))))
(: r (run (L "out/host/love" "out/host/lush" "-c" "echo $(echo hi | tr h H) crew/lush/j*.l"))
   (assert (= "Hi crew/lush/job.l\n" (cup r))))
(: r (run (L "out/host/love" "out/host/lush" "-c" "sh -c 'echo e >&2' 2>&1 | tr e E"))
   (assert (= "E\n" (cup r))))
(: r (run (L "out/host/love" "out/host/lush" "-c" "exit 9"))
   (assert (= 9 (cap r))))

(puts "sh: ok") (putc 10)
