; crew/kore/expr.l -- expr, the little arithmetic-and-matching language POSIX hands a
; shell script. its own file because it rides re.l's BRE engine for `:`, and a body
; captures its free names at its define -- so it loads after re.l, exactly as sed.l and
; awk.l do. a toolbox of one: an expr-main, no seat, kore.l is its door.
;
; the grammar, loosest binding first:
;   |   &   < <= = == != >= >   + -   * / %   :
; then the primaries: ( E ), length S, substr S pos len, index S chars, match S re,
; `+ TOKEN` (the quote that makes a keyword an ordinary word), and a bare word.
;
; every VALUE is A TEXT. a text that reads as a whole number is a number wherever one
; is wanted and a text everywhere else, and that single rule is the whole of expr's type
; system -- `2 < 10` is 1 and `2 < 10a` is 0, because the second pair has no number in it.
; the exit code is expr's own third channel: 0 the answer is neither "" nor "0", 1 it is,
; 2 the expression is malformed or the arithmetic will not do.
;
; a body-less top-level `:` -- expr-main leaks, the rest is the parser.
(module 'kore
(:
   ; the operators by level, loosest first; the walk below indexes this list by depth
   ex-ops '(("|") ("&") ("<" "<=" "=" "==" "!=" ">=" ">") ("+" "-") ("*" "/" "%") (":"))
   (ex-die m) (udie 2 ("expr: " + m))
   ; a whole-number text -> (1 n) | (). a leading - belongs to the number and nothing
   ; else does: expr has no unary minus, so "-3" is a numeral and "- 3" is a syntax error
   (ex-num s)
    (: n (tally s)
       neg (? (0 < n) (s 0 = 45) 0)
       st (? neg 1 0)
       (go k acc) (? (n <= k) [1 acc]
                     (: c (s k) (? ((c < 48) || (57 < c)) ()
                                   (go (k + 1) (((* 10 acc) + c) - 48)))))
       (? (n <= st) () (: r (go st 0) (? (two? r) [1 (? neg (0 - <>r) <>r)] ()))))
   (ex-true? v) (!(v = "") && !(v = "0"))
   (ex-bool b) (? b "1" "0")
   ; the division truncates toward zero and the remainder wears the dividend's sign,
   ; which is C's rule and expr's -- love's // floors, so the sign is taken out and put
   ; back rather than divided with
   (ex-arith op a b)
    (: x (ex-num a) y (ex-num b)
       _ (? ((two? x) && two? y) () (ex-die "non-integer argument"))
       p <>x q <>y
       _ (? (((op = "/") || (op = "%")) && (q = 0)) (ex-die "division by zero") ())
       tq (? (op = "+") 0 (op = "-") 0 (op = "*") 0
             (: m (// (abs p) (abs q)) (? ((p < 0) = (q < 0)) m (0 - m))))
       (show (? (op = "+") (p + q) (op = "-") (p - q) (op = "*") (* p q)
                (op = "/") tq (p - (* tq q)))))
   (ex-cmp op a b)
    (: x (ex-num a) y (ex-num b)
       both ((two? x) && two? y)
       lt (? both (<>x < <>y) (a < b))
       eq (? both (<>x = <>y) (a = b))
       (ex-bool (? (op = "<") lt
                   (op = "<=") (lt || eq)
                   ((op = "=") || (op = "==")) eq
                   (op = "!=") !eq
                   (op = ">=") !lt
                   (!lt && !eq))))
   ; S : re -- the BRE anchored at the head. a pattern carrying \( \) answers group 1's
   ; text (empty on a miss), a pattern without one answers the match's length ("0" on a
   ; miss). anchored through refindp's predicate rather than a leading (bol) node: the
   ; predicate rides inside the backtracker, so a start it refuses cannot be walked past
   ; with a shorter match still to try at it.
   (ex-match a b)
    (: r (rebre b)
       _ (? (two? r) () (ex-die ("invalid regular expression: " + b)))
       nds <>r ng <>>r
       m (refindp nds a 0 (\ st en (st = 0)))
       (? ng (? (two? m) (: e (assoc 1 (m 3))
                            (? (two? e) (snip a <>e <>>e) ""))
                "")
          (? (two? m) (show (<>>m - <>m)) "0")))
   ; index S chars: the 1-based seat of the first charm of S that chars also holds, or 0
   (ex-index s cs)
    (: n (tally s) m (tally cs)
       (has c) (: (hit j) (? (m <= j) 0 (cs j = c) 1 (hit (j + 1)))
                  (hit 0))
       (go i) (? (n <= i) "0" (has (s i)) (show (i + 1)) (go (i + 1)))
       (go 0))
   ; substr S pos len: 1-based, and every way of falling outside answers the empty text
   (ex-substr s ps ls)
    (: n (tally s)
       x (ex-num ps) y (ex-num ls)
       p (? (two? x) <>x 0) l (? (two? y) <>y 0)
       st (p - 1)
       en (st + l)
       (? ((!(two? x) || !(two? y)) || ((l < 1) || (st < 0))) ""
          (n <= st) ""
          (snip s st (? (n < en) n en))))
   (ex-app op a b)
    (? (op = "|") (? (ex-true? a) a (? (ex-true? b) b "0"))
       (op = "&") (? ((ex-true? a) && (ex-true? b)) a "0")
       (op = ":") (ex-match a b)
       (member? op '("+" "-" "*" "/" "%")) (ex-arith op a b)
       (ex-cmp op a b))
   ; the whole descent is one function taking its own depth, so the primary can call
   ; back into the loosest level for a parenthesis without any two names needing to
   ; know each other -- a body sees its own name, and nothing after it
   (ex-parse lv ts)
    (: (prim m)
        (? !(two? m) (ex-die "syntax error: missing operand")
           (: w <m
              (? (w = "(")
                 (: r (ex-parse 0 >m)
                    t2 <>r
                    _ (? ((two? t2) && (<t2 = ")")) () (ex-die "syntax error: expecting )"))
                    [<r >t2])
                 (w = "+") (? (two? >m) [<>m >>m]
                              (ex-die "syntax error: missing argument after '+'"))
                 (w = "length") (? (two? >m) [(show (tally <>m)) >>m]
                                   (ex-die "syntax error: missing argument after 'length'"))
                 (w = "index") (? (two? >>m) [(ex-index <>m <>>m) >>>m]
                                  (ex-die "syntax error: missing argument after 'index'"))
                 (w = "match") (? (two? >>m) [(ex-match <>m <>>m) >>>m]
                                  (ex-die "syntax error: missing argument after 'match'"))
                 (w = "substr") (? (two? >>>m) [(ex-substr <>m <>>m <>>>m) >>>>m]
                                   (ex-die "syntax error: missing argument after 'substr'"))
                 [w >m])))
       (? (5 < lv) (prim ts)
          (: ops (ex-ops lv)
             r (ex-parse (lv + 1) ts)
             (go v m) (? ((two? m) && (member? <m ops))
                         (: r2 (ex-parse (lv + 1) >m)
                            (go (ex-app <m v <r2) <>r2))
                         [v m])
             (go <r <>r))))
   (expr-main as)
    (? !(two? as) (ex-die "syntax error: missing operand")
       (: r (ex-parse 0 as)
          rest <>r
          _ (? (two? rest) (ex-die ("syntax error: unexpected argument '" + <rest + "'")) ())
          v <r
          _ (say out v) _ (put out 10) _ (flush out)
          (? (ex-true? v) 0 1))))
) ; module 'kore
