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

(: (tk-op? t o) (id? <t 'op && (>t = o))
   (tk-w? t) (id? <t 'w)
   (tk-rd? t) (id? <t 'rd)
   (tk-nl? t) (tk-op? t "nl")
   (skipnl ts) (? (ts && tk-nl? <ts) (skipnl >ts) ts)
   (skipsep ts) (? (two? ts && (tk-op? <ts ";" || tk-nl? <ts))
                   (skipsep >ts) ts)
   (bad? r) !(two? <r)
   ; the bare text of a plain unquoted one-segment word, "" otherwise
   (litw t) (? (&& (two? t) (tk-w? t) (two? >t) !>>t
                  (id? <<>t 'lit))
               ("" + ><>t) "")
   (rw? t w) (tally (litw t) && (w = (litw t)))
   stopw '("then" "else" "elif" "fi" "do" "done" "esac" "}")

   (sh-parse ts)
     (: ts2 (skipnl ts)
        (? !ts2 ('ok >< ())
           (: r (plist ts2)
              (? (id? <r 'stop)
                   (: t <>r w (litw t)
                      ('err >< ("unexpected " + (? (tally w) w ("" + >t)))))
                 (bad? r) r
                 >r ('err >< "trailing tokens")
                 ('ok >< <r)))))

   ; list: & backgrounds ONLY its own and_or; seq folds right by recursion
   (plist ts) (: r (pao ts) (? (bad? r) r (plrest <r >r)))
   (plrest a ts)
     (? !(two? ts) (a >< ())
        (: t <ts
           (? (tk-op? t "&") (plnext ['bg a] >ts)
              (tk-op? t ";" || tk-nl? t) (plnext a >ts)
              (a >< ts))))
   (plnext a ts)
     (: ts2 (skipnl ts)
        (? !(two? ts2) (a >< ())
           (: r (plist ts2)
              (? (id? <r 'stop) (a >< >r)                ; a closer: the list ends here
                 (bad? r) r
                 ['seq a <r >< >r]))))

   ; and_or: && / ||, left-assoc; newlines after the op continue the line
   (pao ts) (: r (ppipe ts) (? (bad? r) r (paomore <r >r)))
   (paomore a ts)
     (? !(two? ts) (a >< ())
        (: t <ts
           (? (tk-op? t "&&") (paorhs 'and a >ts)
              (tk-op? t "||") (paorhs 'or a >ts)
              (a >< ts))))
   (paorhs k a ts)
     (: ts2 (skipnl ts)
        (? !(two? ts2) ('more >< 'op)
           (: r (ppipe ts2)
              (? (bad? r) r (paomore [k a <r] >r)))))

   ; pipeline: a leading bare ! word negates the whole thing
   (ppipe ts)
     (? (rw? <ts "!")
        (: r (ppipe1 >ts) (? (bad? r) r ['not <r >< >r]))
        (ppipe1 ts))
   (ppipe1 ts) (: r (pcomm ts) (? (bad? r) r (ppmore [<r] >r)))
   (ppmore cmds ts)
     (? (ts && tk-op? <ts "|")
        (: ts2 (skipnl >ts)
           (? !(two? ts2) ('more >< 'op)
              (: r (pcomm ts2) (? (bad? r) r (ppmore (<r >< cmds) >r)))))
        ((? >cmds ['pipe (sh-rev cmds)] <cmds) >< ts))

   ; command: a compound by its reserved word, a function def, else simple
   (pcomm ts)
     (? !(two? ts) ('more >< 'compound)
        (: t <ts w (litw t)
           (? (sh-smem? w stopw) ('stop >< ts)
              ; a closer OP at command position stops the list too -- it is how a
              ; multi-line subshell's `)` (or a case item's `;;`) lands on its own line
              (tk-op? t ")" || tk-op? t ";;") ('stop >< ts)
              (w = "if") (pif >ts)
              (w = "while") (ploop 'while >ts)
              (w = "until") (ploop 'until >ts)
              (w = "for") (pfor >ts)
              (w = "case") (pcase >ts)
              (w = "{") (pgroup >ts)
              (tk-op? t "(") (psubsh >ts)
              (fundef? ts) (pfun ts)
              (pcmd ts))))
   (fundef? ts) (&& (tally (litw <ts))
                    (two? >ts) (tk-op? <>ts "(")
                    (two? >>ts) (tk-op? <>>ts ")"))

   ; expect the closer w (separators skipped) -> (1 . rest) | 'more | 'err
   (expw ts w msg)
     (: ts2 (skipsep ts)
        (? !(two? ts2) ('more >< 'compound)
           (rw? <ts2 w) (1 >< >ts2)
           ('err >< msg)))
   ; trailing redirects on a compound: done < file, { .. } 2>&1
   (predir ast ts) (prd ast ts ())
   (prd ast ts rs)
     (? (two? ts && tk-rd? <ts)
        (: t <ts ts2 >ts
           (? (two? ts2 && tk-w? <ts2)
              (prd ast >ts2 [<>t >>t ><ts2 >< rs])
              ('err >< "redirect wants a target")))
        (? (two? rs) ['rdr ast (sh-rev rs) >< ts] (ast >< ts)))

   (pif ts)
     (: c (plist (skipnl ts))
        (? (id? <c 'stop) ('err >< "if: empty condition")
           (bad? c) c
           (: t1 (expw >c "then" "if wants `then`")
              (? !(charm? <t1) t1
                 (: b (plist (skipnl >t1))
                    (? (id? <b 'stop) ('err >< "if: empty body")
                       (bad? b) b
                       (pifrest [(<c >< <b)] >b)))))))
   (pifrest clauses ts)
     (: ts2 (skipsep ts)
        (? !(two? ts2) ('more >< 'compound)
           (: w (litw <ts2)
              (? (w = "fi") (predir ['if (sh-rev clauses) ()] >ts2)
                 (w = "elif")
                   (: c (plist (skipnl >ts2))
                      (? (bad? c) c
                         (: t1 (expw >c "then" "elif wants `then`")
                            (? !(charm? <t1) t1
                               (: b (plist (skipnl >t1))
                                  (? (bad? b) b
                                     (pifrest ((<c >< <b) >< clauses) >b)))))))
                 (w = "else")
                   (: b (plist (skipnl >ts2))
                      (? (bad? b) b
                         (: f (expw >b "fi" "else wants `fi`")
                            (? !(charm? <f) f
                               (predir ['if (sh-rev clauses) <b] >f)))))
                 ('err >< "if wants `fi`")))))
   (ploop k ts)
     (: c (plist (skipnl ts))
        (? (id? <c 'stop) ('err >< "loop: empty condition")
           (bad? c) c
           (: d (expw >c "do" "loop wants `do`")
              (? !(charm? <d) d
                 (: b (plist (skipnl >d))
                    (? (bad? b) b
                       (: e (expw >b "done" "loop wants `done`")
                          (? !(charm? <e) e
                             (predir [k <c <b] >e)))))))))
   (pfor ts)
     (? !(two? ts) ('more >< 'compound)
        (: nm (litw <ts)
           (? !(tally nm) ('err >< "for wants a name")
              (: ts2 (skipsep >ts)
                 (? !(two? ts2) ('more >< 'compound)
                    (rw? <ts2 "in")
                      (: (go t acc) (? (two? t && tk-w? <t)
                                       (go >t (><t >< acc))
                                       (sh-rev acc >< t))
                         r (go >ts2 ())
                         (pfordo nm <r >r))
                    (pfordo nm [[('var >< "@")]] ts2))))))
   (pfordo nm ws ts)
     (: d (expw ts "do" "for wants `do`")
        (? !(charm? <d) d
           (: b (plist (skipnl >d))
              (? (bad? b) b
                 (: e (expw >b "done" "for wants `done`")
                    (? !(charm? <e) e (predir ['for nm ws <b] >e)))))))
   (pcase ts)
     (? !(two? ts) ('more >< 'compound)
        (: wt <ts
           (? !(tk-w? wt) ('err >< "case wants a word")
              (: i (expw >ts "in" "case wants `in`")
                 (? !(charm? <i) i (pcitems >wt () >i))))))
   (pcitems w items ts)
     (: ts2 (skipnl ts)
        (? !(two? ts2) ('more >< 'compound)
           (rw? <ts2 "esac") (predir ['case w (sh-rev items)] >ts2)
           (: ts3 (? (tk-op? <ts2 "(") >ts2 ts2)
              (go t acc)                          ; patterns: word (| word)*
                (? !(two? t) ('more >< 'compound)
                   !(tk-w? <t) ('err >< "case wants a pattern")
                   (: acc2 (><t >< acc) t2 >t
                      (? (two? t2 && tk-op? <t2 "|") (go >t2 acc2)
                         (two? t2 && tk-op? <t2 ")") (sh-rev acc2 >< >t2)
                         !(two? t2) ('more >< 'compound)
                         ('err >< "case pattern wants `)`"))))
              r (go ts3 ())
              (? !(two? <r) r
                 (pcbody w <r >r items)))))
   (pcbody w pats ts items)
     (: ts2 (skipnl ts)
        (? !(two? ts2) ('more >< 'compound)
           (tk-op? <ts2 ";;") (pcitems w ((pats >< ()) >< items) >ts2)
           (rw? <ts2 "esac") (pcitems w ((pats >< ()) >< items) ts2)
           (: b (plist ts2)
              (? (bad? b) b
                 (: ts3 (skipnl >b)
                    (? !(two? ts3) ('more >< 'compound)
                       (tk-op? <ts3 ";;")
                         (pcitems w ((pats >< <b) >< items) >ts3)
                       (rw? <ts3 "esac")
                         (pcitems w ((pats >< <b) >< items) ts3)
                       ('err >< "case wants `;;` or `esac`")))))))
   (pgroup ts)
     (: b (plist (skipnl ts))
        (? (id? <b 'stop) (pgend () >b)              ; { } empty? no: the closer ends it
           (bad? b) b
           (pgend <b >b)))
   (pgend body ts)
     (: e (expw ts "}" "{ wants `}`")
        (? !(charm? <e) e (predir ['group body] >e)))
   (psubsh ts)
     (: b (plist (skipnl ts))
        (? (bad? b) b
           (: ts2 (skipnl >b)
              (? !(two? ts2) ('more >< 'compound)
                 (tk-op? <ts2 ")") (predir ['subsh <b] >ts2)
                 ('err >< "( wants `)`")))))
   (pfun ts)                                     ; NAME ( ) [nl] command
     (: nm (litw <ts)
        ts3 (skipnl >>>ts)
        (? !(two? ts3) ('more >< 'compound)
           (: b (pcomm ts3)
              (? (bad? b) b ['fun nm <b >< >b]))))

   ; simple command: words + redirects, any order; a redirect eats its target
   (pcmd ts) (pcm ts () ())
   (pcm ts ws rs)
     (? !(two? ts) (pcfin ts ws rs)
        (: t <ts
           (? (tk-w? t) (pcm >ts (>t >< ws) rs)
              (tk-rd? t)
                (: ts2 >ts
                   (? (two? ts2 && tk-w? <ts2)
                      (pcm >ts2 ws [<>t >>t ><ts2 >< rs])
                      !(two? ts2) ('err >< "redirect wants a target")
                      ('err >< "redirect wants a word")))
              (tk-op? t "(") ('err >< "unexpected parenthesis")
              (pcfin ts ws rs))))
   (pcfin ts ws rs)
     (? (!ws && !rs) ('err >< "expected a command")
        ['cmd (sh-rev ws) (sh-rev rs) >< ts]))
