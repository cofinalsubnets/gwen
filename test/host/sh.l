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
        (id? (cap (sh-parse (cup (sh-lex "( x )")))) 'ok))

; rung 3: compounds parse (and stay 'more while open), ${..} lexes as vop,
; heredocs rewrite to <h with the body as the target word
(: pr (sh-parse (cup (sh-lex "if a; then b; fi")))
   (assert (id? (cap pr) 'ok) (id? (cap (cup pr)) 'if)))
(: pr (sh-parse (cup (sh-lex "while a; do b; done < f")))
   (assert (id? (cap pr) 'ok) (id? (cap (cup pr)) 'rdr)))
(: pr (sh-parse (cup (sh-lex "case x in a|b) c;; esac")))
   (assert (id? (cap pr) 'ok) (id? (cap (cup pr)) 'case)))
(: pr (sh-parse (cup (sh-lex "f() { x; }")))
   (assert (id? (cap pr) 'ok) (id? (cap (cup pr)) 'fun)))
(assert (id? (cap (sh-parse (cup (sh-lex "if a; then b")))) 'more)
        (id? (cap (sh-parse (cup (sh-lex "for f in")))) 'more)
        (= '(1 (w (vop "x" ":-" "y"))) (: r (sh-lex "${x:-y}") r)))
(: lx (sh-lex "cat <<EOF\nbody\nEOF")
   t (cup lx)
   (assert (= 1 (cap lx))
           (id? (cap (caup t)) 'rd)                      ; (rd () . "<h")
           (= "<h" (cup (cup (caup t))))))

; rung 3, end to end through the BUILT shell: control flow, functions,
; positionals, ${}, heredocs, subshells, set -u
(: r (hark (L "out/host/love" "out/host/lush" "-c" "for f in a b; do echo f=$f; done"))
   (assert (= "f=a\nf=b\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "f() { echo n=$# one=$1; return 3; }; f x y; echo rc=$?"))
   (assert (= "n=2 one=x\nrc=3\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "case q.l in *.c) echo c;; *.l) echo l;; esac"))
   (assert (= "l\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "p=a/b.l; echo ${p%.l} ${p#a/} ${U:-dflt} ${#p}"))
   (assert (= "a/b b.l dflt 5\n" (cup r))))
(: _ (setenv "LUSH_HD" "quay")
   r (hark (L "out/host/love" "out/host/lush" "-c" "cat <<EOF | tr a-z A-Z\nhd $LUSH_HD\nEOF"))
   (assert (= "HD QUAY\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "( exit 5 ); echo sub=$?"))
   (assert (= "sub=5\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "set -u; echo $LUSH_UNDEF_QQ; echo no"))
   (assert (! (= 0 (cap r)))))

; the engine plans: (sh-plan ast) reads the AST once and answers a THUNK, so
; firing it twice runs twice. the function book holds that plan -- a closure, so
; two? can never see it, and absence is the sh-nofn mint (peep's total test)
(: pr (sh-parse (cup (sh-lex "LUSH_PL=$(expr ${LUSH_PL:-0} + 1)")))
   p (sh-plan (cup pr))
   _ (p ()) _ (p ())
   (assert (= '("2") (sh-fields (L (link 'var "LUSH_PL"))))))
(: _ (sh-line "lush_pf() { echo x; }")
   (assert (sh-fn? "lush_pf") (! (sh-fn? "lush_pq_zz"))
           (! (two? (sh-fnof "lush_pf")))))
; break/continue count their levels through the planned loops
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "for a in 1 2; do for b in x y; do echo $a$b; break 2; done; done; echo end"))
   (assert (= "1x\nend\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "for a in 1 2 3; do [ $a = 2 ] && continue; echo n=$a; done"))
   (assert (= "n=1\nn=3\n" (cup r))))

; errexit's guard is a DYNAMIC EXTENT, which is why sh-noe is ambient: the
; suspension inside a condition reaches a FORKED pipeline stage and an `eval`,
; and outside one it reaches neither. bash agrees on all five.
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; { false; echo r; } | cat; echo after"))
   (assert (= "after\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; if { false; echo r; } | cat; then echo t; fi"))
   (assert (= "r\nt\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; if eval \"false; echo x\"; then echo t; fi"))
   (assert (= "x\nt\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; eval \"false; echo x\"; echo after"))
   (assert (= 1 (cap r)) (= "" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "set -e; false && echo a; echo survived"))
   (assert (= "survived\n" (cup r))))
; ... but a $( ) body is guarded, so under set -e it runs to its end and hands
; back what it wrote -- the two capture lanes (a seq reaped per step, a compound
; in one forked child) answer the same, and bash agrees with both
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; v=$( { false; echo r; } ); echo v=[$v] after"))
   (assert (= "v=[r] after\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; v=$(false; echo r); echo v=[$v] after"))
   (assert (= "v=[r] after\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; f(){ false; echo fr; }; v=$(f); echo v=[$v] after"))
   (assert (= "v=[fr] after\n" (cup r))))
; a command with NO command name answers the last $( ) performed over its
; assignments, in order (POSIX) -- none ran is a clean 0, a command name of its
; own wins, and under set -e that status is a real trigger unless it is guarded
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=$(exit 3); echo $?"))
   (assert (= "3\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=1; echo $?"))
   (assert (= "0\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "a=$(exit 3) b=$(exit 4); echo $?"))
   (assert (= "4\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=$(exit 5)x$(exit 6); echo $?"))
   (assert (= "6\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=$(false) true; echo $?"))
   (assert (= "0\n" (cup r))))
; ⚠ the WORDS expand before the assignments, so a $( ) that vanishes to no field
; still counts, and still counts FIRST
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=1 $(exit 3); echo $?"))
   (assert (= "3\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=$(exit 2) $(exit 3); echo $?"))
   (assert (= "2\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "$(exit 6); echo $?"))
   (assert (= "6\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "set -e; v=$(false); echo never"))
   (assert (= 1 (cap r)) (= "" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c"
             "set -e; if v=$(false); then echo t; else echo e; fi"))
   (assert (= "e\n" (cup r))))

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

; completion (rung 2): (insert . candidates) -- one path candidate lands its
; tail (+ " ", a directory its "/"), the common prefix otherwise; the helpers
(: r (sh-complete (sh-expl "cat crew/lush/jo"))
   (assert (two? r) (= "b.l " (slurp (tap (cap r)))) (= '("crew/lush/job.l") (cup r))))
(: r (sh-complete (sh-expl "cat crew/lus"))
   (assert (= "h/" (slurp (tap (cap r))))))              ; one DIR candidate: its /
(: r (sh-complete (sh-expl "cat crew/lu"))
   (assert (= '("crew/lush" "crew/lux") (cup r))))       ; two ways forward, listed
(assert (= "ab" (sh-common '("abc" "abd" "ab")))
        (= "~" (sh-homely (+ "" (getenv "HOME")))))

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
(hear (\ a b (L 'gotsig a b)))
(assert (= (L 'gotsig 'sigusr1 (getpid 0)) (scare 'sigusr1 (getpid 0))))
(hear 0)

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
(: r (hark (L "out/host/love" "out/host/lush" "-c" "echo a b | tr a-z A-Z"))
   (assert (= 0 (cap r)) (= "A B\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "false && echo no || echo yes"))
   (assert (= "yes\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "echo $(echo hi | tr h H) crew/lush/j*.l"))
   (assert (= "Hi crew/lush/job.l\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "sh -c 'echo e >&2' 2>&1 | tr e E"))
   (assert (= "E\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "exit 9"))
   (assert (= 9 (cap r))))

; rung 4: --login reads $HOME/.profile (a scratch HOME keeps the suite
; hermetic); without the flag the profile stays unread. `return` in a
; .-sourced file stops that file with its status (the POSIX dot-return).
(: home0 (+ "" (getenv "HOME"))
   d "out/host/.lush-t4"
   _ (mkdir d 493)
   p (open (+ d "/.profile") "w") _ (say p "LUSH_P4=prof\nexport LUSH_P4\n") _ (close p)
   p (open (+ d "/rr.sh") "w") _ (say p "echo one\nreturn 5\necho two\n") _ (close p)
   _ (setenv "HOME" d)
   r1 (hark (L "out/host/love" "out/host/lush" "--login" "-c" "echo p=$LUSH_P4"))
   r2 (hark (L "out/host/love" "out/host/lush" "-c" "echo p=[$LUSH_P4]"))
   _ (setenv "HOME" home0)
   r3 (hark (L "out/host/love" "out/host/lush" "-c" ". out/host/.lush-t4/rr.sh; echo rc=$?"))
   (assert (= "p=prof\n" (cup r1))
           (= "p=[]\n" (cup r2))
           (= "one\nrc=5\n" (cup r3))))
; a comment ends at ITS newline, never the input's end (the multi-line -c /
; buffered-script shape every /etc/profile.d file has)
(: r (hark (L "out/host/love" "out/host/lush" "-c" "# lead\necho a # tail\n# mid\necho b"))
   (assert (= "a\nb\n" (cup r))))
; a closer on its OWN LINE: the multi-line subshell in cmdsub, the case item's ;;
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=$(\n(\necho deep\n)\n)\necho v=$v"))
   (assert (= "v=deep\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "case x in\na) echo no\n;;\nx) echo yes\n;;\nesac"))
   (assert (= "yes\n" (cup r))))
; ${x:-word}: word is QUOTE-AWARE (sh-wordize) -- inner quotes remove, and a
; quoted pattern char in ${v#pat} is inert (the mask law through the vop)
(: r (hark (L "out/host/love" "out/host/lush" "-c" "echo \"${LUSH_U4:-\"a b\"}\" ${LUSH_U4:-'c d'}"))
   (assert (= "a b c d\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v='x*y'; echo ${v#\"x*\"} ${v#x?}"))
   (assert (= "y y\n" (cup r))))

; the FORKLESS $( ), lane one: a no-effect builtin says its piece into a jug this
; task wears (love.h's hook 6) -- no pipe, no fd, no child. the answer is the one
; the pipe gave, the status included, and a function of the same name takes the
; name back (to lane two, below: a body that may spawn needs an fd)
(: r (hark (L "out/host/love" "out/host/lush" "-c" "cd /; echo \"[$(pwd)][$(pwd)]\""))
   (assert (= "[/][/]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "x=$(false); echo $?; y=$(true); echo $?"))
   (assert (= "1\n0\n" (cup r))))

; ⚠ A FAILED cd MUST SAY SO. chdir answers () on success and the ERRNO on failure, and
; an errno is NEGATIVE -- so it is red, so a truth test reads the failure as a success:
; `cd nowhere` returned 0 with no word said, and `cd build && make` ran make wherever you
; already stood. The kind is what separates them (charm?, never !).
(: r (hark (L "out/host/love" "out/host/lush" "-c" "cd /definitely-not-here; echo $?"))
   (assert (= "1\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "cd /definitely-not-here && echo moved; echo done"))
   (assert (= "done\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "pwd() { echo shadow; }; echo $(pwd)"))
   (assert (= "shadow\n" (cup r))))

; lane two: the body runs HERE, down a pipe a TASK drains, and the state a
; subshell would have taken by copy is put back by hand (sh-subsave's roster).
(: r (hark (L "out/host/love" "out/host/lush" "-c" "f() { echo a; echo b; }; x=$(f); echo \"[$x]\""))
   (assert (= "[a\nb]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "cd /tmp; echo [$(cd /; pwd)][$(pwd)]"))
   (assert (= "[/][/tmp]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "v=out; echo [$(v=in; echo $v)][$v]"))
   (assert (= "[in][out]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "export E=out; echo [$(export E=in; echo $E)][$E]"))
   (assert (= "[in][out]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "g() { echo G; }; f() { g() { echo I; }; }; echo [$(f; g)][$(g)]"))
   (assert (= "[I][G]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "set -- keep it; f() { echo $1; }; echo [$(f one)][$1 $2]"))
   (assert (= "[one][keep it]\n" (cup r))))
; ..and the roster rides the RAISED way out too -- the one exit a driven shell
; cannot reach from here, since nothing a script can type raises a condition. a
; scare crossing the boundary used to leave the body's cwd (and its vars, funcs,
; positionals and flags with it) standing in the shell that ran it.
; ⚠ the cwd is read and PUT BACK before the assert, never inside it: a failure
; here would otherwise leave every later test reading from the wrong directory.
(: start (cwd 0)
   r (trap (\ _ (sh-subhere (\ _ (: _ (chdir "/") (scare 'boom 0))))) () (\ a b a))
   here (cwd 0)
   _ (chdir start)
   (assert (id? r 'boom)                       ; the condition still rides outward
           (start = here)                      ; ..and the subshell's chdir did not
           !(start = "/")))                    ; ..(and the law was not vacuous)
; ⚠ `exit` in a forkless body may not reach `quit` -- it folds to ('xit . st),
; which nothing catches but the capture. a FORK under one puts the depth back,
; so a subshell's exit is a real one again
(: r (hark (L "out/host/love" "out/host/lush" "-c" "echo [$(echo x; exit 7; echo y)]; z=$(exit 7); echo $?"))
   (assert (= "[x]\n7\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "f() { exit 9; }; (f); echo after $?"))
   (assert (= "after 9\n" (cup r))))
; ⚠ a FORK under a forkless body inherits the DRAINER, so it has to shut it down
; before it runs a step -- else the child reads its parent's capture and stalls
; on a pipe only its parent can end. a stage that is a function is such a fork; a
; `( )` is not, since under a capture it runs in-process instead
(: r (hark (L "out/host/love" "out/host/lush" "-c" "f() { echo hi; }; echo [$(f | tr a-z A-Z)]"))
   (assert (= "[HI]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "cd /tmp; echo [$( (cd /; pwd) )][$(pwd)]"))
   (assert (= "[/][/tmp]\n" (cup r))))
(: r (hark (L "out/host/love" "out/host/lush" "-c" "x=$( (exit 4); echo tail); echo [$x]"))
   (assert (= "[tail]\n" (cup r))))
; the drainer earns its keep: a body saying more than one pipe buffer would stop
; forever against a reader that is us. a builtin's buffered bytes come first
(: r (hark (L "out/host/love" "out/host/lush" "-c" "f() { pwd; cat love.c; cat love.c; }; x=$(f); echo ${#x}"))
   t (cup r)
   (assert (< 800000 (sh-int (snip t 0 (- (tally t) 1))))))

(test_fin ())
(puts "sh: ok") (putc 10)
