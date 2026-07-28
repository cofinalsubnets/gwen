; --- lush 🐚 the grammar -- tokens to the AST ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-parse tokens) -> ('ok . ast) | ('more . why) | ('err . msg) -- 'more is
; the parse-level PS2 protocol (a dangling && || | wants another line; the
; lexer's 'more already covered unfinished words). the AST:
;   ('cmd words redirs)   words: each a segment list; redirs: (ion op target)
;   ('pipe cmds)          two or more stages
;   ('and a b) ('or a b) ('seq a b) ('bg a) ('not a)
; the grammar, POSIX cut to rung 1 (compounds land at rung 3):
;   list     := and_or ((';' | '&' | nl) and_or)*  [trailing ; & nl fine]
;   and_or   := pipeline (('&&' | '||') pipeline)*   [newlines skip after the op]
;   pipeline := ['!'] cmd ('|' cmd)*
;   cmd      := (word | redirect)+
; a parser answers (ast . rest) -- ast is always a pair, so a symbol cap
; ('more / 'err) IS the failure lane, out of band (the presence law).

(: (tk-op? t o) (&& (id? (cap t) 'op) (= (cup t) o))
   (tk-w? t) (id? (cap t) 'w)
   (tk-rd? t) (id? (cap t) 'rd)
   (tk-nl? t) (tk-op? t "nl")
   (skipnl ts) (? (&& ts (tk-nl? (cap ts))) (skipnl (cup ts)) ts)
   (bad? r) (! (two? (cap r)))

   (sh-parse ts)
     (: ts2 (skipnl ts)
        (? !ts2 (link 'ok ())
           (: r (plist ts2)
              (? (bad? r) r
                 (cup r) (link 'err "trailing tokens")
                 (link 'ok (cap r))))))

   ; list: & backgrounds ONLY its own and_or (a ; b & backgrounds b alone);
   ; seq folds right by recursion, which evals left-to-right all the same
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
              (? (bad? r) r (link (L 'seq a (cap r)) (cup r))))))

   ; and_or: && / ||, left-assoc; newlines after the op continue the line
   (pao ts) (: r (ppipe ts) (? (bad? r) r (paomore (cap r) (cup r))))
   (paomore a ts)
     (? !ts (link a ())
        (: t (cap ts)
           (? (tk-op? t "&&") (paorhs 'and a (cup ts))
              (tk-op? t "||") (paorhs 'or a (cup ts))
              (link a ts))))
   (paorhs k a ts)
     (: ts2 (skipnl ts)
        (? !ts2 (link 'more 'op)
           (: r (ppipe ts2)
              (? (bad? r) r (paomore (L k a (cap r)) (cup r))))))

   ; pipeline: a leading bare ! word negates the whole thing
   (bangw? t)
     (&& (tk-w? t)
         (: sg (cup t)
            (&& (two? sg) (! (cup sg)) (id? (cap (cap sg)) 'lit) (= (cup (cap sg)) "!"))))
   (ppipe ts)
     (? (bangw? (cap ts))
        (: r (ppipe1 (cup ts)) (? (bad? r) r (link (L 'not (cap r)) (cup r))))
        (ppipe1 ts))
   (ppipe1 ts) (: r (pcmd ts) (? (bad? r) r (ppmore (L (cap r)) (cup r))))
   (ppmore cmds ts)
     (? (&& ts (tk-op? (cap ts) "|"))
        (: ts2 (skipnl (cup ts))
           (? !ts2 (link 'more 'op)
              (: r (pcmd ts2) (? (bad? r) r (ppmore (link (cap r) cmds) (cup r))))))
        (link (? (cup cmds) (L 'pipe (sh-rev cmds)) (cap cmds)) ts))

   ; simple command: words + redirects, any order; a redirect eats its target word
   (pcmd ts) (pcm ts () ())
   (pcm ts ws rs)
     (? !ts (pcfin ts ws rs)
        (: t (cap ts)
           (? (tk-w? t) (pcm (cup ts) (link (cup t) ws) rs)
              (tk-rd? t)
                (: ion (cap (cup t)) op (cup (cup t))
                   (? (= op "<<") (link 'err "<<: heredocs land at rung 3")
                      (: ts2 (cup ts)
                         (? (&& ts2 (tk-w? (cap ts2)))
                            (pcm (cup ts2) ws (link (L ion op (cup (cap ts2))) rs))
                            !ts2 (link 'err "redirect wants a target")
                            (link 'err "redirect wants a word")))))
              (|| (tk-op? t "(") (tk-op? t ")")) (link 'err "subshells land at rung 3")
              (tk-op? t ";;") (link 'err ";; belongs to case (rung 3)")
              (pcfin ts ws rs))))
   (pcfin ts ws rs)
     (? (&& !ws !rs) (link 'err "expected a command")
        (link (L 'cmd (sh-rev ws) (sh-rev rs)) ts)))
