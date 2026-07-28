; --- lush 🐚 the grammar -- tokens to the AST ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-parse tokens) -> ('ok . ast) | ('more . why) | ('err . msg) -- 'more is
; the parse-level PS2 protocol: a dangling && || |, or a compound still open
; (if without fi, a case mid-item) wants another line. the AST:
;   ('cmd words redirs)     words each a segment list; redirs (ion op target)
;   ('pipe cmds)            two or more stages (any command kind)
;   ('and a b) ('or a b) ('seq a b) ('bg a) ('not a)
;   ('if clauses else)      clauses ((cond . body)..); else () when absent
;   ('while cond body) ('until cond body)
;   ('for name words body)  words absent -> the one word $@
;   ('case word items)      items ((patterns . body)..), patterns word-segs
;   ('group body) ('subsh body) ('fun name body) ('rdr ast redirs)
; reserved words live only at COMMAND POSITION: pcomm answers ('stop . ts) for
; a closer (then/fi/do/done/esac/else/elif/}), and the list machinery lifts it
; into "the list so far ends here" -- the compound parsers then expect their
; own closer by name. an empty rest where a compound needs more is 'more.

(: (tk-op? t o) (&& (id? (cap t) 'op) (= (cup t) o))
   (tk-w? t) (id? (cap t) 'w)
   (tk-rd? t) (id? (cap t) 'rd)
   (tk-nl? t) (tk-op? t "nl")
   (skipnl ts) (? (&& ts (tk-nl? (cap ts))) (skipnl (cup ts)) ts)
   (skipsep ts) (? (&& (two? ts) (|| (tk-op? (cap ts) ";") (tk-nl? (cap ts))))
                   (skipsep (cup ts)) ts)
   (bad? r) (! (two? (cap r)))
   ; the bare text of a plain unquoted one-segment word, "" otherwise
   (litw t) (? (&& (two? t) (tk-w? t) (two? (cup t)) (! (cup (cup t)))
                  (id? (cap (cap (cup t))) 'lit))
               (+ "" (cup (cap (cup t)))) "")
   (rw? t w) (&& (tally (litw t)) (= w (litw t)))
   stopw '("then" "else" "elif" "fi" "do" "done" "esac" "}")

   (sh-parse ts)
     (: ts2 (skipnl ts)
        (? !ts2 (link 'ok ())
           (: r (plist ts2)
              (? (id? (cap r) 'stop) (link 'err (+ "unexpected " (litw (cap (cup r)))))
                 (bad? r) r
                 (cup r) (link 'err "trailing tokens")
                 (link 'ok (cap r))))))

   ; list: & backgrounds ONLY its own and_or; seq folds right by recursion
   (plist ts) (: r (pao ts) (? (bad? r) r (plrest (cap r) (cup r))))
   (plrest a ts)
     (? (! (two? ts)) (link a ())
        (: t (cap ts)
           (? (tk-op? t "&") (plnext (L 'bg a) (cup ts))
              (|| (tk-op? t ";") (tk-nl? t)) (plnext a (cup ts))
              (link a ts))))
   (plnext a ts)
     (: ts2 (skipnl ts)
        (? (! (two? ts2)) (link a ())
           (: r (plist ts2)
              (? (id? (cap r) 'stop) (link a (cup r))    ; a closer: the list ends here
                 (bad? r) r
                 (link (L 'seq a (cap r)) (cup r))))))

   ; and_or: && / ||, left-assoc; newlines after the op continue the line
   (pao ts) (: r (ppipe ts) (? (bad? r) r (paomore (cap r) (cup r))))
   (paomore a ts)
     (? (! (two? ts)) (link a ())
        (: t (cap ts)
           (? (tk-op? t "&&") (paorhs 'and a (cup ts))
              (tk-op? t "||") (paorhs 'or a (cup ts))
              (link a ts))))
   (paorhs k a ts)
     (: ts2 (skipnl ts)
        (? (! (two? ts2)) (link 'more 'op)
           (: r (ppipe ts2)
              (? (bad? r) r (paomore (L k a (cap r)) (cup r))))))

   ; pipeline: a leading bare ! word negates the whole thing
   (ppipe ts)
     (? (rw? (cap ts) "!")
        (: r (ppipe1 (cup ts)) (? (bad? r) r (link (L 'not (cap r)) (cup r))))
        (ppipe1 ts))
   (ppipe1 ts) (: r (pcomm ts) (? (bad? r) r (ppmore (L (cap r)) (cup r))))
   (ppmore cmds ts)
     (? (&& ts (tk-op? (cap ts) "|"))
        (: ts2 (skipnl (cup ts))
           (? (! (two? ts2)) (link 'more 'op)
              (: r (pcomm ts2) (? (bad? r) r (ppmore (link (cap r) cmds) (cup r))))))
        (link (? (cup cmds) (L 'pipe (sh-rev cmds)) (cap cmds)) ts))

   ; command: a compound by its reserved word, a function def, else simple
   (pcomm ts)
     (? (! (two? ts)) (link 'more 'compound)
        (: t (cap ts) w (litw t)
           (? (sh-smem? w stopw) (link 'stop ts)
              (= w "if") (pif (cup ts))
              (= w "while") (ploop 'while (cup ts))
              (= w "until") (ploop 'until (cup ts))
              (= w "for") (pfor (cup ts))
              (= w "case") (pcase (cup ts))
              (= w "{") (pgroup (cup ts))
              (tk-op? t "(") (psubsh (cup ts))
              (fundef? ts) (pfun ts)
              (pcmd ts))))
   (fundef? ts) (&& (tally (litw (cap ts)))
                    (two? (cup ts)) (tk-op? (caup ts) "(")
                    (two? (cuup ts)) (tk-op? (cap (cuup ts)) ")"))

   ; expect the closer w (separators skipped) -> (1 . rest) | 'more | 'err
   (expw ts w msg)
     (: ts2 (skipsep ts)
        (? (! (two? ts2)) (link 'more 'compound)
           (rw? (cap ts2) w) (link 1 (cup ts2))
           (link 'err msg)))
   ; trailing redirects on a compound: done < file, { .. } 2>&1
   (predir ast ts) (prd ast ts ())
   (prd ast ts rs)
     (? (&& (two? ts) (tk-rd? (cap ts)))
        (: t (cap ts) ts2 (cup ts)
           (? (&& (two? ts2) (tk-w? (cap ts2)))
              (prd ast (cup ts2) (link (L (cap (cup t)) (cup (cup t)) (cup (cap ts2))) rs))
              (link 'err "redirect wants a target")))
        (? (two? rs) (link (L 'rdr ast (sh-rev rs)) ts) (link ast ts)))

   (pif ts)
     (: c (plist (skipnl ts))
        (? (id? (cap c) 'stop) (link 'err "if: empty condition")
           (bad? c) c
           (: t1 (expw (cup c) "then" "if wants `then`")
              (? (! (charm? (cap t1))) t1
                 (: b (plist (skipnl (cup t1)))
                    (? (id? (cap b) 'stop) (link 'err "if: empty body")
                       (bad? b) b
                       (pifrest (L (link (cap c) (cap b))) (cup b))))))))
   (pifrest clauses ts)
     (: ts2 (skipsep ts)
        (? (! (two? ts2)) (link 'more 'compound)
           (: w (litw (cap ts2))
              (? (= w "fi") (predir (L 'if (sh-rev clauses) ()) (cup ts2))
                 (= w "elif")
                   (: c (plist (skipnl (cup ts2)))
                      (? (bad? c) c
                         (: t1 (expw (cup c) "then" "elif wants `then`")
                            (? (! (charm? (cap t1))) t1
                               (: b (plist (skipnl (cup t1)))
                                  (? (bad? b) b
                                     (pifrest (link (link (cap c) (cap b)) clauses) (cup b))))))))
                 (= w "else")
                   (: b (plist (skipnl (cup ts2)))
                      (? (bad? b) b
                         (: f (expw (cup b) "fi" "else wants `fi`")
                            (? (! (charm? (cap f))) f
                               (predir (L 'if (sh-rev clauses) (cap b)) (cup f))))))
                 (link 'err "if wants `fi`")))))
   (ploop k ts)
     (: c (plist (skipnl ts))
        (? (id? (cap c) 'stop) (link 'err "loop: empty condition")
           (bad? c) c
           (: d (expw (cup c) "do" "loop wants `do`")
              (? (! (charm? (cap d))) d
                 (: b (plist (skipnl (cup d)))
                    (? (bad? b) b
                       (: e (expw (cup b) "done" "loop wants `done`")
                          (? (! (charm? (cap e))) e
                             (predir (L k (cap c) (cap b)) (cup e))))))))))
   (pfor ts)
     (? (! (two? ts)) (link 'more 'compound)
        (: nm (litw (cap ts))
           (? (! (tally nm)) (link 'err "for wants a name")
              (: ts2 (skipsep (cup ts))
                 (? (! (two? ts2)) (link 'more 'compound)
                    (rw? (cap ts2) "in")
                      (: (go t acc) (? (&& (two? t) (tk-w? (cap t)))
                                       (go (cup t) (link (cup (cap t)) acc))
                                       (link (sh-rev acc) t))
                         r (go (cup ts2) ())
                         (pfordo nm (cap r) (cup r)))
                    (pfordo nm (L (L (link 'var "@"))) ts2))))))
   (pfordo nm ws ts)
     (: d (expw ts "do" "for wants `do`")
        (? (! (charm? (cap d))) d
           (: b (plist (skipnl (cup d)))
              (? (bad? b) b
                 (: e (expw (cup b) "done" "for wants `done`")
                    (? (! (charm? (cap e))) e (predir (L 'for nm ws (cap b)) (cup e))))))))
   (pcase ts)
     (? (! (two? ts)) (link 'more 'compound)
        (: wt (cap ts)
           (? (! (tk-w? wt)) (link 'err "case wants a word")
              (: i (expw (cup ts) "in" "case wants `in`")
                 (? (! (charm? (cap i))) i (pcitems (cup wt) () (cup i)))))))
   (pcitems w items ts)
     (: ts2 (skipnl ts)
        (? (! (two? ts2)) (link 'more 'compound)
           (rw? (cap ts2) "esac") (predir (L 'case w (sh-rev items)) (cup ts2))
           (: ts3 (? (tk-op? (cap ts2) "(") (cup ts2) ts2)
              (go t acc)                          ; patterns: word (| word)*
                (? (! (two? t)) (link 'more 'compound)
                   (! (tk-w? (cap t))) (link 'err "case wants a pattern")
                   (: acc2 (link (cup (cap t)) acc) t2 (cup t)
                      (? (&& (two? t2) (tk-op? (cap t2) "|")) (go (cup t2) acc2)
                         (&& (two? t2) (tk-op? (cap t2) ")")) (link (sh-rev acc2) (cup t2))
                         (! (two? t2)) (link 'more 'compound)
                         (link 'err "case pattern wants `)`"))))
              r (go ts3 ())
              (? (! (two? (cap r))) r
                 (pcbody w (cap r) (cup r) items)))))
   (pcbody w pats ts items)
     (: ts2 (skipnl ts)
        (? (! (two? ts2)) (link 'more 'compound)
           (tk-op? (cap ts2) ";;") (pcitems w (link (link pats ()) items) (cup ts2))
           (rw? (cap ts2) "esac") (pcitems w (link (link pats ()) items) ts2)
           (: b (plist ts2)
              (? (bad? b) b
                 (: ts3 (skipnl (cup b))
                    (? (! (two? ts3)) (link 'more 'compound)
                       (tk-op? (cap ts3) ";;")
                         (pcitems w (link (link pats (cap b)) items) (cup ts3))
                       (rw? (cap ts3) "esac")
                         (pcitems w (link (link pats (cap b)) items) ts3)
                       (link 'err "case wants `;;` or `esac`")))))))
   (pgroup ts)
     (: b (plist (skipnl ts))
        (? (id? (cap b) 'stop) (pgend () (cup b))    ; { } empty? no: the closer ends it
           (bad? b) b
           (pgend (cap b) (cup b))))
   (pgend body ts)
     (: e (expw ts "}" "{ wants `}`")
        (? (! (charm? (cap e))) e (predir (L 'group body) (cup e))))
   (psubsh ts)
     (: b (plist (skipnl ts))
        (? (bad? b) b
           (: ts2 (skipnl (cup b))
              (? (! (two? ts2)) (link 'more 'compound)
                 (tk-op? (cap ts2) ")") (predir (L 'subsh (cap b)) (cup ts2))
                 (link 'err "( wants `)`")))))
   (pfun ts)                                     ; NAME ( ) [nl] command
     (: nm (litw (cap ts))
        ts3 (skipnl (cuup (cup ts)))
        (? (! (two? ts3)) (link 'more 'compound)
           (: b (pcomm ts3)
              (? (bad? b) b (link (L 'fun nm (cap b)) (cup b))))))

   ; simple command: words + redirects, any order; a redirect eats its target
   (pcmd ts) (pcm ts () ())
   (pcm ts ws rs)
     (? (! (two? ts)) (pcfin ts ws rs)
        (: t (cap ts)
           (? (tk-w? t) (pcm (cup ts) (link (cup t) ws) rs)
              (tk-rd? t)
                (: ts2 (cup ts)
                   (? (&& (two? ts2) (tk-w? (cap ts2)))
                      (pcm (cup ts2) ws (link (L (cap (cup t)) (cup (cup t)) (cup (cap ts2))) rs))
                      (! (two? ts2)) (link 'err "redirect wants a target")
                      (link 'err "redirect wants a word")))
              (tk-op? t "(") (link 'err "unexpected parenthesis")
              (pcfin ts ws rs))))
   (pcfin ts ws rs)
     (? (&& !ws !rs) (link 'err "expected a command")
        (link (L 'cmd (sh-rev ws) (sh-rev rs)) ts)))
