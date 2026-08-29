; crew/kore/awk.l -- awk: a pattern-action language over records and fields. a
; toolbox file, no seat -- kore (crew/kore/kore.l) is its door. it rides the
; u-floor (udie/uleave, uread, uatoi), text.l's lines, and re.l's ERE engine
; (reere, refind, rehas) for every regex it speaks.
;
;   awk [-F FS] [-v var=value].. 'PROGRAM' [FILE..]
;   awk [-F FS] [-v var=value].. -f PROGFILE.. [FILE..]
;
; the dialect is POSIX awk: BEGIN/END, pattern-action items, ranges, fields and
; $0 rebuilding, the special variables (NR NF FNR FS OFS ORS FILENAME SUBSEP RSTART
; RLENGTH CONVFMT OFMT), arrays with `in` and `delete`, user functions with array
; parameters by reference, and the builtins length substr index split sub gsub match
; sprintf sin cos atan2 exp log sqrt int rand srand tolower toupper system close.
;
; out of dialect, deliberately -- each is a rung, not an oversight:
; ⚠ getline in every spelling. it is the one construct that makes the record
;   loop re-entrant, and half a getline is worse than none.
; ⚠ output pipes (`print | "cmd"`). plain `>` and `>>` to a FILE are here; a pipe
;   wants a port pair we do not open.
; ⚠ RS is the newline, always. a paragraph or regex RS re-splits the input and
;   the record loop is written for lines.
; ⚠ ARGV/ARGC and ENVIRON. the arguments are walked, not published, and a program
;   that rewrites ARGV mid-run is a second record loop wearing an array.
; ⚠ printf's `*` width and the `#` flag.
;
; ⚠ NUMBER FORMATTING IS OURS, NOT THE HOST'S. `show` prints a double round-trip
; exact ("0.3333333333333333"); awk owes %.6g ("0.333333"), so aw-fixed/aw-expo/
; aw-gen do the digits by hand off a normalised mantissa. they round half away
; from zero on the scaled value where C rounds half to even on the exact binary
; one -- the two differ only on an exact tie, which a decimal literal can reach
; and a computed double essentially never does.
(module 'kore
(:
   ; -- values --------------------------------------------------------------
   ; a value is () uninitialised, a number, a string, or ('sn n s) -- a STRNUM,
   ; the thing that came off input looking like a number: it must COMPARE as a
   ; number and PRINT as the text it arrived in ("007" is 7 and prints 007)
   ; ⚠ `nil?` IS A TRUTH TEST, not a type one -- it answers 1 for 0, for -4 and
   ; for "" as readily as for (). awk leans on the difference every line (an
   ; uninitialised value is () and prints "", the number 0 prints "0"), so the
   ; empty question is asked by identity and never by truth
   (aw-nil? ()) 1
   (aw-sn? ('sn . _)) 1
   (aw-numeric? v) !(string? v)          ; () and strnum both compare numeric
   (aw-blank? (32 || 9 || 10)) 1
   (aw-dig? c) (48 <= c <= 57)
   ; the numeric span of s inside [a,b): (past-the-end) -- a if there is no number
   (aw-nspan s a b)
    (: (run k) (k < b && aw-dig? (s k) ? (run (k + 1)) k)
       i1 (a < b && (s a @ (45 || 43) 1) ? (a + 1) a)
       i2 (run i1)
       i3 (i2 < b && s i2 = 46 ? (run (i2 + 1)) i2)
       (? (i2 = i1 && i3 = i2) a                       ; no digits at all
          (i3 < b && (s i3 = 101 || s i3 = 69))
           (: k (? (i3 + 1 < b && (s (i3 + 1) = 45 || s (i3 + 1) = 43))
                   (i3 + 2) (i3 + 1))
              k2 (run k)
              (k2 = k ? i3 k2))                          ; a bare e is not an exponent
          i3))
   ; the text of a number -> the number (the span is already known good)
   (aw-parse s a b)
    (: neg (s a = 45)
       i1 (s a = 45 || s a = 43 ? (a + 1) a)
       (mant k acc sc dot)
        (? (b <= k) [acc sc]
           (: c (s k)
              (? (c = 46) (mant (k + 1) acc sc 1)
                 (aw-dig? c) (mant (k + 1) (10 * acc + c - 48) (dot ? (sc * 10) sc) dot)
                 [acc sc])))
       (fnd k) (? (b <= k) b (s k = 101 || s k = 69) k (fnd (k + 1)))
       e (fnd i1)
       m (mant i1 0 1 0)
       base (<m / <>m)
       ex (? (e = b) 0
             (: t (snip s (e + 1) b)
                (? (0 < #t && t 0 = 45) -(uatoi (snip t 1 #t))
                   (0 < #t && t 0 = 43) (uatoi (snip t 1 #t))
                   (uatoi t))))
       v (ex = 0 ? base (base * 10 ** ex))
       (neg ? -v v))
   ; awk's string->number: the longest numeric PREFIX, blanks first, 0 for none
   (aw-s2n s)
    (: n #s
       (sp k) (k < n && aw-blank? (s k) ? (sp (k + 1)) k)
       a (sp 0)
       e (aw-nspan s a n)
       (e = a ? 0 (aw-parse s a e)))
   ; does the WHOLE text (blanks aside) read as a number -- the strnum question
   (aw-looks-num? s)
    (: n #s
       (sp k) (k < n && aw-blank? (s k) ? (sp (k + 1)) k)
       (bk k) (0 < k && aw-blank? (s (k - 1)) ? (bk (k - 1)) k)
       a (sp 0) b (bk n)
       (a >= b ? 0 (aw-nspan s a b = b)))
   ; a text off the input -> the value it should behave as
   (aw-mk s) (? (aw-looks-num? s) ['sn (aw-s2n s) s] s)
   (aw-numv v) (? (aw-nil? v) 0 (string? v) (aw-s2n v) (aw-sn? v) <>v v)
   (aw-bool v) (? (aw-nil? v) 0
                  (string? v) (0 < #v)
                  (aw-sn? v) (<>v != 0)
                  (v != 0))

   ; -- the decimal engine: C's %f, %e and %g by hand --------------------------
   (aw-trunc x) (-9.0e18 < x < 9.0e18 ? (int x) x)
   ; ⚠ AN INTEGRAL DOUBLE PAST A CHARM STILL OWES ITS DIGITS. awk prints 1e20 as
   ; its twenty-one figures, and `int` overflows to 0 there. every double at that
   ; size is m*2^k for an exact m under 2^53, so scale down to m, take it as a
   ; charm, and scale back up through love's bigints -- exact, and glibc-identical
   ; even where the double is not the round number it was written as (1e23 is
   ; 99999999999999991611392 to both)
   (aw-bigint x)
    (: a (abs x)
       (k j) (a / 2 ** j < 9.0e15 ? j (k (j + 1)))
       j (k 0)
       (int (x / 2 ** j) * 2 ** j))
   (aw-intstr x)
    (-9.0e18 < x < 9.0e18 ? (show (int x)) (show (aw-bigint x)))
   (aw-zeros k) (k < 1 ? "" ("0" + (aw-zeros (k - 1))))
   (aw-spaces k) (k < 1 ? "" (" " + (aw-spaces (k - 1))))
   (aw-lpad w s) (#s < w ? (aw-lpad w ("0" + s)) s)
   ; |x| > 0 -> the decimal exponent e with 1 <= |x|/10^e < 10
   (aw-exp10 a)
    (: e0 (aw-trunc (log a / log 10))
       (up e) (a / 10 ** e >= 10 ? (up (e + 1)) e)
       (dn e) (a / 10 ** e < 1 ? (dn (e - 1)) e)
       (dn (up e0)))
   ; %.<nd>f
   (aw-fixed x nd)
    (: neg (x < 0)
       a (abs x)
       y (a * (** 10 nd))
       ; past a charm the value is integral already, so its exact digits plus nd
       ; zeros ARE the fixed spelling
       (? !(y < 9.0e18)
          (: s ((neg ? "-" "") + aw-intstr a)
             (? (nd < 1) s (s + "." + aw-zeros nd)))
          (: r (int (y + 0.5))
             s (show r)
             body (nd < 1 ? s
                     (: p (aw-lpad (nd + 1) s)
                        m #p
                        (snip p 0 (m - nd) + "." + snip p (m - nd) m)))
             (? neg ("-" + body) body))))
   ; %.<nd>e
   (aw-expo x nd)
    (: neg (x < 0)
       a (abs x)
       sg (? neg "-" "")
       (tl e) (: d (show (e < 0 ? -e e))
                 ("e" + (e < 0 ? "-" "+") + (#d < 2 ? ("0" + d) d)))
       (a = 0 ? (sg + aw-fixed 0 nd + tl 0)
          (: e (aw-exp10 a)
             m (a / 10 ** e)
             carry (m >= 10 - 0.5 / 10 ** nd)   ; the round can carry to 10.0
             e2 (? carry (e + 1) e)
             ms (aw-fixed (? carry (a / 10 ** e2) m) nd)
             (sg + ms + tl e2))))
   ; drop the trailing zeros of a fraction, then a bare trailing point
   (aw-strip s)
    (: n #s
       (dot k) (? (n <= k) -1 (s k = 46) k (dot (k + 1)))
       d (dot 0)
       (? (d < 0) s
          (: (bk k) (? (d < k && s (k - 1) = 48) (bk (k - 1)) k)
             b (bk n)
             (snip s 0 (b - 1 = d ? d b)))))
   ; %.<np>g -- e-form outside [-4, np), f-form inside, trailing zeros dropped
   (aw-gen x np0)
    (: np (np0 < 1 || np0)
       neg (x < 0)
       a (abs x)
       (? (a = 0) "0"
          (: e (aw-exp10 a)
             (? ((e < -4) || !(e < np))
                (: s (aw-expo x (np - 1))
                   n #s
                   (ep k) (? (n <= k) n (s k = 101 || s k = 69) k (ep (k + 1)))
                   i (ep 0)
                   (aw-strip (snip s 0 i) + snip s i n))
                (aw-strip (aw-fixed x (np - 1 - e)))))))
   ; a number as awk writes it: an integral value is its digits, whatever CONVFMT
   (aw-numstr x fmt)
    (x = aw-trunc x ? (aw-intstr x) (aw-sprintf fmt [x] "%.6g"))
   (aw-str v cf) (? (aw-nil? v) ""
                    (string? v) v
                    (aw-sn? v) <>>v
                    (aw-numstr v cf))
   (aw-map-case s up)
    (: n #s o (jug 0)
       (go k) (n <= k ? (slurp o)
               (: c (s k)
                (put o (? (up && 97 <= c <= 122) (c - 32)
                          (!up && 65 <= c <= 90) (c + 32)
                          c),
                 go (k + 1))))
       (go 0))
   (aw-up s) (aw-map-case s 1)
   (aw-down s) (aw-map-case s 0)

   ; -- sprintf --------------------------------------------------------------
   (aw-sp-int x) (aw-intstr x)
   (aw-base n b caps)
    (: dg (? caps "0123456789ABCDEF" "0123456789abcdef")
       (go v acc) (? (v = 0) acc (go (// v b) ((string (dg (% v b))) + acc)))
       (? (n = 0) "0" (go n "")))
   ; a conversion's flags, width and precision -> (minus zero plus space w prec k)
   (aw-spec f n k0)
    (: (fl k m z p sp)
        (? (n <= k) [m z p sp 0 -1 k]
           (: c (f k)
              (? (c = 45) (fl (k + 1) 1 z p sp)
                 (c = 48) (fl (k + 1) m 1 p sp)
                 (c = 43) (fl (k + 1) m z 1 sp)
                 (c = 32) (fl (k + 1) m z p 1)
                 (: (num j acc got) (j < n && aw-dig? (f j) ?
                                       (num (j + 1) (10 * acc + f j - 48) 1)
                                       [acc got j])
                    w (num k 0 0)
                    k2 <>>w
                    pr (k2 < n && f k2 = 46 ? (num (k2 + 1) 0 1) [0 0 k2])
                    [m z p sp <w (<>pr ? <pr -1) <>>pr]))))
       (fl k0 0 0 0 0))
   (aw-pad m z w s)
    (: t #s
       (? (t >= w) s
          m (s + aw-spaces (w - t))
          z (0 < t && (s 0 = 45 || s 0 = 43) ?
               (string (s 0) + aw-zeros (w - t) + snip s 1 t)
               (aw-zeros (w - t) + s))
          (aw-spaces (w - t) + s)))
   (aw-sgn p sp s) (? (0 < #s && s 0 = 45) s  p ("+" + s)  sp (" " + s)  s)
   ; ONE conversion at f k (which is a %) -> (next-k args-left), the text said into o
   (aw-conv o f n k l cf)
    (aw-spec f n (k + 1) @ [m z pl spc w pr j]
     (? (n <= j) (: _ (put o 37) [(k + 1) l])
        (: d (f j)
           v (two? l && <l)
           rest (two? l ? >l l)
           num (aw-numv v)
           done [(j + 1) rest]
           (? (d = 37) (put o 37, [(j + 1) l])
              (d = 99)
               (: t (? (aw-nil? v) "" (string? v) v (aw-sn? v) <>>v ())
                  s (string? t ? (0 < #t ? (string (t 0)) "")
                       (string (aw-trunc num)))
                  (say o (aw-pad m 0 w s), done))
              (d = 115)
               (: s0 (aw-str v cf)
                  s (pr < 0 ? s0 (snip s0 0 (#s0 < pr ? #s0 pr)))
                  (say o (aw-pad m 0 w s), done))
              (d = 100 || d = 105)
               (: s (aw-sgn pl spc (aw-sp-int num))
                  (say o (aw-pad m z w s), done))
              (d = 111 || d = 120 || d = 88 || d = 117)
               (: x (aw-trunc num)
                  y (abs x)
                  s0 (? (d = 111) (aw-base y 8 0)
                        (d = 117) (show y)
                        (aw-base y 16 (d = 88)))
                  s (x < 0 ? ("-" + s0) s0)
                  (say o (aw-pad m z w s), done))
              (d = 102 || d = 70)
               (: s (aw-sgn pl spc (aw-fixed num (pr < 0 ? 6 pr)))
                  (say o (aw-pad m z w s), done))
              (d = 101 || d = 69)
               (: s0 (aw-expo num (pr < 0 ? 6 pr))
                  s (aw-sgn pl spc (d = 69 ? (aw-up s0) s0))
                  (say o (aw-pad m z w s), done))
              (d = 103 || d = 71)
               (: s0 (aw-gen num (pr < 0 ? 6 pr))
                  s (aw-sgn pl spc (d = 71 ? (aw-up s0) s0))
                  (say o (aw-pad m z w s), done))
              (put o 37, [(k + 1) l]))))
     [(k + 1) l])
   (aw-sprintf f as cf)
    (: n #f o (jug 0)
       (go k l)
        (n <= k ? (slurp o)
         (: c (f k)
          (c = 37 && k + 1 < n ?
             (aw-conv o f n k l cf @ [nk rest] (go nk rest) (slurp o))
             (put o c, go (k + 1) l))))
       (go 0 as))

   ; -- the lexer -----------------------------------------------------------
   ; every refusal from here down leaves through one scare, caught in awk-main:
   ; a syntax error is not a value any caller could do anything with
   (aw-err m) (scare 'awk-err m)
   (aw-alp? c) (97 <= c <= 122 || 65 <= c <= 90 || c = 95)
   (aw-aln? c) (aw-alp? c || aw-dig? c)
   (aw-kw? w) (member? w '("BEGIN" "END" "function" "func" "if" "else" "while"
                           "for" "do" "break" "continue" "next" "exit" "return"
                           "delete" "in" "print" "printf" "getline"))
   ; ⚠ the / is TWO tokens depending on what came before it: division after a
   ; value, and the opening of a regex everywhere else. this is the whole reason
   ; the lexer carries the previous token
   (aw-div? t)
    (aw-nil? t ? 0
     (<t @ ('num || 'str || 'name || 'ere) 1
           'op (member? <>t '(")" "]" "++" "--"))
           0))
   aw-ops '("**=" "+=" "-=" "*=" "/=" "%=" "^=" "==" "!=" "<=" ">=" "&&" "||"
            "++" "--" "!~" ">>" "**" "{" "}" "(" ")" "[" "]" ";" "," "+" "-"
            "*" "/" "%" "^" "!" ">" "<" "|" "?" ":" "~" "$" "=")
   (aw-op-at s n k)                              ; -> [text next] | ()
    (: (has t) (: m #t (k + m <= n && t = snip s k (k + m)))
       (try (w . u)) (has w ? (: nm (? (w = "**") "^" (w = "**=") "^=" w)
                                 [nm (k + #w)])
                      (try u))
       (try aw-ops))
   (aw-esc c) (c @ 110 10  116 9  114 13  92 92  34 34
                   47 47  97 7  98 8  102 12  118 11  c)
   ; a "..." body from i (past the quote) -> [text next]; the escapes are eaten here
   (aw-strlit s n i)
    (: o (jug 0)
       (go k) (n <= k ? (aw-err "unterminated string")
               (: c (s k)
                (? (c = 34) [(slurp o) (k + 1)]
                   (c = 92 && k + 1 < n)
                    (: d (s (k + 1))
                       (48 <= d <= 55 ?
                          (: (oct j v cnt)
                              (cnt < 3 && j < n && 48 <= s j <= 55 ?
                                 (oct (j + 1) (8 * v + s j - 48) (cnt + 1))
                                 [v j])
                             (oct (k + 1) 0 0 @ [v j] (put o v, go j) ()))
                          (put o (aw-esc d), go (k + 2))))
                   (put o c, go (k + 1)))))
       (go i))
   ; a /.../ body from i -> [text next]; only \/ is eaten, the rest is the engine's
   (aw-erelit s n i)
    (: o (jug 0)
       (go k) (n <= k ? (aw-err "unterminated regex")
               (: c (s k)
                (? (c = 47) [(slurp o) (k + 1)]
                   (c = 10) (aw-err "newline in regex")
                   (c = 92 && k + 1 < n)
                    (: d (s (k + 1))
                       (d = 47 ? (put o 47, go (k + 2))
                        (put o 92, put o d, go (k + 2))))
                   (put o c, go (k + 1)))))
       (go i))
   (aw-lex src)
    (: n (tally src)
       (eol j) (j < n && src j != 10 ? (eol (j + 1)) j)
       (wd j) (j < n && aw-aln? (src j) ? (wd (j + 1)) j)
       (go k prev acc)
        (n <= k ? (rev (['eof ""] . acc))
         (: c (src k)
          (? (c = 32 || c = 9 || c = 13) (go (k + 1) prev acc)
             (c = 35) (go (eol k) prev acc)
             ; a backslash-newline is a continuation: neither token nor break
             (c = 92) (go (k + 1 < n && src (k + 1) = 10 ? (k + 2) (k + 1)) prev acc)
             (c = 10) (: t ['nl ""] (go (k + 1) t (t . acc)))
             (c = 34) (aw-strlit src n (k + 1) @ [s j]
                        (: t ['str s] (go j t (t . acc))) ())
             (c = 47 && !(aw-div? prev))
              (aw-erelit src n (k + 1) @ [s j]
                (: t ['ere s] (go j t (t . acc))) ())
             (aw-dig? c || (c = 46 && k + 1 < n && aw-dig? (src (k + 1))))
              (: e (aw-nspan src k n)
                 t ['num (aw-parse src k e)]
                 (go e t (t . acc)))
             (aw-alp? c)
              (: e (wd k)
                 w (snip src k e)
                 ; a ( GLUED to the name is a call; spaced, it is concatenation
                 t (? (aw-kw? w) ['kw w]
                      (e < n && src e = 40) ['fname w]
                      ['name w])
                 (go e t (t . acc)))
             (aw-op-at src n k @ [w j]
               (: t ['op w] (go j t (t . acc)))
               (aw-err ("unexpected character " + string c))))))
       (go 0 () ()))

   ; -- the parser ----------------------------------------------------------
   ; every door answers [node rest]; a refusal scares, so nothing here carries a
   ; failure shape and the arms stay readable
   (aw-k? ts k) (two? ts && <<ts == k)
   (aw-op? ts v) (two? ts && <<ts == 'op && <><ts = v)
   (aw-kwd? ts v) (two? ts && <<ts == 'kw && <><ts = v)
   (aw-eof? ts) (two? ts ? (aw-k? ts 'eof) 1)
   (aw-nl ts) (aw-k? ts 'nl ? (aw-nl >ts) ts)             ; newlines only
   (aw-term ts) (aw-k? ts 'nl || aw-op? ts ";" ? (aw-term >ts) ts)
   (aw-want ts v m) (aw-op? ts v ? >ts (aw-err m))
   (aw-lval? e) (<e @ ('var || 'field || 'idx) 1 0)
   aw-assigns '("=" "+=" "-=" "*=" "/=" "%=" "^=")
   aw-rels '("<" "<=" "==" "!=" ">=" ">")
   aw-bis '("length" "substr" "index" "split" "sub" "gsub" "match" "sprintf"
            "sin" "cos" "atan2" "exp" "log" "sqrt" "int" "rand" "srand"
            "tolower" "toupper" "system" "close")
   ; can this token open an operand -- the whole of concatenation's grammar
   (aw-starts? ts)
    (two? ts &&
     (<<ts @ ('num || 'str || 'ere || 'name || 'fname) 1
             'op (member? <><ts '("$" "(" "!" "++" "--"))
             0))
   ; a comma-separated expression list, closed by the caller
   (aw-elist ts ng acc)
    (aw-e-assign ts ng @ [e r]
      (aw-op? r "," ? (aw-elist (aw-nl >r) ng (e . acc)) [(rev (e . acc)) r])
      ())
   (aw-subs ts)                                  ; [ e , e .. ] -> [subs rest]
    (aw-elist ts 0 () @ [es r]
      [es (aw-want r "]" "expected ] after a subscript")] ())
   (aw-e-prim ts ng)
    (? !(two? ts) (aw-err "unexpected end of program")
       (: t <ts k <t v <>t r >ts
          (? (id? k 'num) [['num v] r]
             (id? k 'str) [['str v] r]
             (id? k 'ere) [['ere v] r]
             (id? k 'name)
              (? (aw-op? r "[") (aw-subs >r @ [es r2] [['idx v es] r2] ())
                 (member? v aw-bis) [['bi v ()] r]
                 [['var v] r])
             (id? k 'fname)
              (: r2 (aw-want r "(" "expected (")
                 (? (aw-op? r2 ")")
                    [(? (member? v aw-bis) ['bi v ()] ['call v ()]) >r2]
                    (aw-elist (aw-nl r2) 0 () @ [es r3]
                      (: r4 (aw-want r3 ")" "expected ) after the arguments")
                         [(? (member? v aw-bis) ['bi v es] ['call v es]) r4])
                      ())))
             (id? k 'kw) (aw-err ("`" + v + "' is not an expression"
                                  + (? (v = "getline") " (getline is out of dialect)" "")))
             (aw-op? ts "$") (aw-e-prim r ng @ [e r2] [['field e] r2] ())
             (aw-op? ts "!") (aw-e-un r ng @ [e r2] [['not e] r2] ())
             ((aw-op? ts "++") || (aw-op? ts "--"))
              (aw-e-prim r ng @ [e r2]
                (? (aw-lval? e) [['pre v e] r2] (aw-err "++ wants a variable")) ())
             (aw-op? ts "(")
              (aw-elist (aw-nl r) 0 () @ [es r2]
                (: r3 (aw-want r2 ")" "expected )")
                   (? (two? >es) [['glist es] r3] [['grp <es] r3]))
                ())
             (aw-err ("unexpected `" + (? (string? v) v "") + "'")))))
   (aw-e-post ts ng)
    (aw-e-prim ts ng @ [e r]
      ((aw-op? r "++" || aw-op? r "--") && aw-lval? e ?
         [['post <><r e] >r] [e r])
      ())
   (aw-e-pow ts ng)
    (aw-e-post ts ng @ [a r]
      (aw-op? r "^" ? (aw-e-un >r ng @ [b r2] [['pow a b] r2] ()) [a r])
      ())
   (aw-e-un ts ng)
    (? (aw-op? ts "!") (aw-e-un >ts ng @ [e r] [['not e] r] ())
       (aw-op? ts "-") (aw-e-un >ts ng @ [e r] [['neg e] r] ())
       (aw-op? ts "+") (aw-e-un >ts ng @ [e r] [['pos e] r] ())
       (aw-e-pow ts ng))
   (aw-e-mul ts ng)
    (aw-e-un ts ng @ [a r]
      (: (go n m)
          (aw-op? m "*" || aw-op? m "/" || aw-op? m "%" ?
             (aw-e-un >m ng @ [b m2] (go ['bin <><m n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-add ts ng)
    (aw-e-mul ts ng @ [a r]
      (: (go n m)
          (aw-op? m "+" || aw-op? m "-" ?
             (aw-e-mul >m ng @ [b m2] (go ['bin <><m n b] m2) ())
             [n m])
         (go a r))
      ())
   ; concatenation is the operator with no spelling: two operands side by side
   (aw-e-cat ts ng)
    (aw-e-add ts ng @ [a r]
      (: (go n m)
          (aw-starts? m ? (aw-e-add m ng @ [b m2] (go ['cat n b] m2) ()) [n m])
         (go a r))
      ())
   ; ⚠ NON-ASSOCIATIVE, like awk: a < b < c is a parse error there and here too,
   ; which is why this takes one relop and stops
   (aw-e-rel ts ng)
    (aw-e-cat ts ng @ [a r]
      (aw-k? r 'op && member? <><r (ng ? '("<" "<=" "==" "!=" ">=") aw-rels) ?
         (aw-e-cat >r ng @ [b r2] [['cmp <><r a b] r2] ())
         [a r])
      ())
   (aw-e-match ts ng)
    (aw-e-rel ts ng @ [a r]
      (: (go n m)
          (aw-op? m "~" || aw-op? m "!~" ?
             (aw-e-rel >m ng @ [b m2] (go ['match (<><m = "!~") n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-in ts ng)
    (aw-e-match ts ng @ [a r]
      (: (go n m)
          (aw-kwd? m "in" ?
             (aw-k? >m 'name ?
                (go ['inarr (<n == 'glist ? <>n [n]) <><>m] >>m)
                (aw-err "in wants an array name"))
             [n m])
         (go a r))
      ())
   (aw-e-and ts ng)
    (aw-e-in ts ng @ [a r]
      (: (go n m)
          (aw-op? m "&&" ? (aw-e-in (aw-nl >m) ng @ [b m2] (go ['and n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-or ts ng)
    (aw-e-and ts ng @ [a r]
      (: (go n m)
          (aw-op? m "||" ? (aw-e-and (aw-nl >m) ng @ [b m2] (go ['or n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-tern ts ng)
    (aw-e-or ts ng @ [c r]
      (aw-op? r "?" ?
         (aw-e-tern (aw-nl >r) ng @ [a r2]
           (: r3 (aw-op? r2 ":" ? (aw-nl >r2) (aw-err "expected : in ?:"))
              (aw-e-tern r3 ng @ [b r4] [['cond c a b] r4] ()))
           ())
         [c r])
      ())
   (aw-e-assign ts ng)
    (aw-e-tern ts ng @ [n r]
      (aw-k? r 'op && member? <><r aw-assigns && aw-lval? n ?
         (aw-e-assign (aw-nl >r) ng @ [v r2] [['assign <><r n v] r2] ())
         [n r])
      ())
   (aw-expr ts) (aw-e-assign ts 0)

   ; -- statements ----------------------------------------------------------
   (aw-body ts)                                  ; the statement a control form owns
    (aw-stmt (aw-nl ts))
   (aw-block ts)                                 ; { .. } -> [stmts rest]
    (: r (aw-want ts "{" "expected {")
       (go m acc)
        (: m2 (aw-term m)
           (? (aw-op? m2 "}") [(rev acc) >m2]
              (aw-eof? m2) (aw-err "unexpected end of program: a { is unclosed")
              (aw-stmt m2 @ [s r2] (go r2 (s . acc)) ())))
       (go r ()))
   (aw-redir ts)                                 ; -> [redir rest]
    (? (aw-op? ts ">" || aw-op? ts ">>")
       (: mode <><ts
          (aw-e-cat >ts 1 @ [e r] [[mode e] r] ()))
       (aw-op? ts "|") (aw-err "output pipes are out of dialect")
       [() ts])
   (aw-prargs ts)                                ; print's list -> [args redir rest]
    (? (aw-k? ts 'nl || aw-op? ts ";" || aw-op? ts "}" || aw-eof? ts
        || aw-op? ts ">" || aw-op? ts ">>")
       (aw-redir ts @ [rd r] [() rd r] ())
       (aw-elist ts 1 () @ [es r]
         ; `print (a, b)` is the whole list in brackets, not one grouped operand
         (: es2 (!(two? >es) && <<es == 'glist ? <><es es)
            (aw-redir r @ [rd r2] [es2 rd r2] ()))
         ()))
   (aw-simple ts)                                ; a statement with no body of its own
    (: t <ts k <t v <>t r >ts
       (? (aw-kwd? ts "print")
           (aw-prargs r @ [es rd r2] [['print es rd] r2] ())
          (aw-kwd? ts "printf")
           (aw-prargs r @ [es rd r2]
             (? (two? es) [['printf es rd] r2] (aw-err "printf wants a format")) ())
          (aw-kwd? ts "next") [['next] r]
          (aw-kwd? ts "break") [['break] r]
          (aw-kwd? ts "continue") [['cont] r]
          (aw-kwd? ts "exit")
           (aw-starts? r ? (aw-expr r @ [e r2] [['exit e] r2] ()) [['exit ()] r])
          (aw-kwd? ts "return")
           (aw-starts? r ? (aw-expr r @ [e r2] [['ret e] r2] ()) [['ret ()] r])
          (aw-kwd? ts "delete")
           (? !(aw-k? r 'name) (aw-err "delete wants an array")
              (aw-op? >r "[")
               (aw-subs >>r @ [es r2] [['del <><r es] r2] ())
              [['delall <><r] >r])
          (aw-expr ts @ [e r2] [['expr e] r2] ())))
   (aw-stmt ts)
    (? (aw-op? ts "{") (aw-block ts @ [ss r] [['blk ss] r] ())
       (aw-op? ts ";") [['nop] >ts]
       (aw-kwd? ts "if")
        (: r (aw-want >ts "(" "expected ( after if")
           (aw-expr (aw-nl r) @ [c r2]
             (: r3 (aw-want r2 ")" "expected ) after the if condition")
                (aw-body r3 @ [a r4]
                  (: r5 (aw-term r4)
                     (? (aw-kwd? r5 "else")
                        (aw-body >r5 @ [b r6] [['if c a b] r6] ())
                        [['if c a ()] r4]))
                  ()))
             ()))
       (aw-kwd? ts "while")
        (: r (aw-want >ts "(" "expected ( after while")
           (aw-expr (aw-nl r) @ [c r2]
             (: r3 (aw-want r2 ")" "expected ) after the while condition")
                (aw-body r3 @ [b r4] [['while c b] r4] ()))
             ()))
       (aw-kwd? ts "do")
        (aw-body >ts @ [b r]
          (: r2 (aw-term r)
             r3 (aw-kwd? r2 "while" ? (aw-want >r2 "(" "expected ( after while")
                   (aw-err "expected while after a do body"))
             (aw-expr (aw-nl r3) @ [c r4]
               [['do b c] (aw-want r4 ")" "expected ) after the do condition")] ()))
          ())
       (aw-kwd? ts "for") (aw-for >ts)
       (aw-simple ts))
   ; the two for shapes: (v in arr) and the three-part one
   (aw-for ts)
    (: r (aw-want ts "(" "expected ( after for")
       (aw-k? r 'name && aw-kwd? >r "in" ?
          (? !(aw-k? >>r 'name) (aw-err "for .. in wants an array")
             (: nm <><r arr <><>>r
                r2 (aw-want >>>r ")" "expected ) after for .. in")
                (aw-body r2 @ [b r3] [['forin nm arr b] r3] ())))
          (: ini (aw-op? r ";" ? [() r] (aw-simple r))
             r2 (aw-want <>ini ";" "expected ; in for")
             cnd (aw-op? r2 ";" ? [() r2] (aw-expr (aw-nl r2)))
             r3 (aw-want <>cnd ";" "expected ; in for")
             stp (aw-op? (aw-nl r3) ")" ? [() (aw-nl r3)] (aw-simple (aw-nl r3)))
             r4 (aw-want <>stp ")" "expected ) after the for clauses")
             (aw-body r4 @ [b r5] [['for <ini <cnd <stp b] r5] ()))))

   ; -- the program ---------------------------------------------------------
   ; items answer as ('begin body) ('end body) ('item pat body) ('range p1 p2 body)
   (aw-params ts acc)
    (? (aw-op? ts ")") [(rev acc) >ts]
       !(aw-k? ts 'name) (aw-err "a parameter must be a name")
       (: nm <><ts r (aw-nl >ts)
          (aw-op? r "," ? (aw-params (aw-nl >r) (nm . acc))
             (aw-params r (nm . acc)))))
   (aw-program ts fns acc)
    (: t (aw-term ts)
       (? (aw-eof? t) (rev acc)
          (aw-kwd? t "function" || aw-kwd? t "func")
           (? !(aw-k? >t 'name || aw-k? >t 'fname)
              (aw-err "function wants a name")
              (: nm <><>t
                 r (aw-want (aw-nl >>t) "(" "expected ( after a function name")
                 (aw-params (aw-nl r) () @ [ps r2]
                   (aw-block (aw-nl r2) @ [b r3]
                     (pin fns nm [ps b], aw-program r3 fns acc) ())
                   ())))
          (aw-kwd? t "BEGIN")
           (aw-block (aw-nl >t) @ [b r] (aw-program r fns (['begin b] . acc)) ())
          (aw-kwd? t "END")
           (aw-block (aw-nl >t) @ [b r] (aw-program r fns (['end b] . acc)) ())
          (aw-op? t "{")
           (aw-block t @ [b r] (aw-program r fns (['item () b] . acc)) ())
          (aw-expr t @ [p r]
            (? (aw-op? r ",")
               (aw-expr (aw-nl >r) @ [p2 r2]
                 (aw-op? r2 "{" ?
                    (aw-block r2 @ [b r3] (aw-program r3 fns (['range p p2 b] . acc)) ())
                    (aw-program r2 fns (['range p p2 ()] . acc)))
                 ())
               (aw-op? r "{")
                (aw-block r @ [b r2] (aw-program r2 fns (['item p b] . acc)) ())
               (aw-program r fns (['item p ()] . acc)))
            ())))

   ; -- the runtime ---------------------------------------------------------
   ; E carries the whole world: 'g the globals, 'loc the current frame or (),
   ; 'fn the functions, 'rec the record with 'raws its split and 'flds the slots
   ; a program has touched, 'fs the separator it arrived under, 're the regex
   ; cache, 'ports the open redirections, 'ranges the pattern ranges that are on
   (aw-new fns)
    (: E (tablet 0) g (tablet 0)
       _ (pins E 'g g  'fn fns  'loc ()
                 'flds 'none  'raws 'none  'fs " "  'rec ""  're (tablet 0)
                 'ports (tablet 0)  'ranges (tablet 0)  'ph 'begin)
       (pins g "FS" " "  "OFS" " "  "ORS" "\n"
               "NR" 0  "NF" 0  "FNR" 0
               "SUBSEP" (string 28)  "CONVFMT" "%.6g"  "OFMT" "%.6g"
               "FILENAME" ""  "RSTART" 0  "RLENGTH" -1,
        E))
   (aw-g E) (E 'g)
   (aw-sv E v) (aw-str v (peep (aw-g E) "CONVFMT" "%.6g"))
   (aw-pv E v) (? (string? v) v
                  (aw-nil? v) ""
                  (aw-sn? v) <>>v
                  (aw-numstr v (peep (aw-g E) "OFMT" "%.6g")))
   (aw-nrn E nm) (aw-trunc (aw-numv (peep (aw-g E) nm 0)))
   (aw-nth l k) (two? l && (k = 0 ? <l (aw-nth >l (k - 1))))
   (aw-local? E nm) (: l (E 'loc) (tablet? l && peep l nm 'nope != 'nope))
   (aw-get E nm)
    (: l (E 'loc)
       (? (tablet? l) (: v (peep l nm 'nope) (v == 'nope ? (aw-g E nm) v))
          (aw-g E nm)))
   (aw-put E nm v)
    (aw-local? E nm ? (pin (E 'loc) nm v, v)
       (nm = "NF" ?
          (: _ (aw-raws E)                       ; the split it is about to resize
             _ (pin (aw-g E) nm v)
             (aw-setnf E (aw-trunc (aw-numv v)), v))
          (pin (aw-g E) nm v, v)))
   (aw-arr E nm)
    (: v (aw-get E nm) (tablet? v ? v (: t (tablet 0) (aw-put E nm t, t))))
   ; a deleted element stays in the tablet wearing 'nope -- there is no unpin, and
   ; `delete a` inside a function must empty the CALLER's array, which a fresh
   ; tablet would not do
   (aw-akeys a)
    (: (go l acc) (l @ (k . u) (go u (peep a k 'nope == 'nope ? acc (k . acc)))
                       (rev acc))
       (go (keys a) ()))
   (aw-key E subs)
    (: ss (aw-sv E (peep (aw-g E) "SUBSEP" (string 28)))
       (go l acc first) (l @ (s . u) (go u (first ? (aw-sv E s) (acc + ss + aw-sv E s)) 0)
                              acc)
       (go subs "" 1))
   (aw-recomp E pat)
    (: c (E 're)
       r (peep c pat 'none)
       (r !== 'none ? r
        (: nds (aw-re pat) (pin c pat nds, nds))))
   (aw-re pat)
    (: p (reere pat)
       (two? p ? <>p (aw-err ("bad regular expression: /" + pat + "/"))))

   ; -- fields ---------------------------------------------------------------
   (aw-splitw t)                                 ; the default FS: runs of blank
    (: n #t
       (go i st acc)
        (n <= i ? (rev (st < 0 ? acc ((snip t st i) . acc)))
         (: c (t i)
          (aw-blank? c ? (go (i + 1) -1 (st < 0 ? acc ((snip t st i) . acc)))
             (go (i + 1) (st < 0 ? i st) acc))))
       (go 0 -1 ()))
   (aw-splitc t c)                               ; a single charm, literally
    (: n #t
       (n = 0 ? ()
        (: (go from k acc) (? (n <= k) (rev ((snip t from k) . acc))
                              (t k = c) (go (k + 1) (k + 1) ((snip t from k) . acc))
                              (go from (k + 1) acc))
           (go 0 0 ()))))
   (aw-splitre E t pat)
    (: n #t nds (aw-recomp E pat)
       (n = 0 ? ()
        (: (go i acc)
            (refind nds t i @ [_ st en _]
              (en <= st ? (rev ((snip t i n) . acc))          ; an empty match cannot walk
                 (go en ((snip t i st) . acc)))
              (rev ((snip t i n) . acc)))
           (go 0 ()))))
   (aw-fields E t fs)
    (? (fs = " ") (aw-splitw t)
       (#fs = 1) (aw-splitc t (fs 0))
       (aw-splitre E t fs))
   ; a record is not split until something asks for a field, and a field is not
   ; read as a number until something reads it -- an untouched field costs the
   ; walk that found its edges and nothing else. FS is sampled when the record
   ; arrives, so changing FS mid-record still splits by the separator it came in on
   (aw-setrec E s)
    (pins E 'rec s  'raws 'none  'flds 'none
            'fs (aw-sv E (peep (aw-g E) "FS" " ")),
     s)
   (aw-raws E)
    (: r (E 'raws)
       (r !== 'none ? r
        (: fs (aw-fields E (peep E 'rec "") (peep E 'fs " "))
           _ (pin E 'raws fs)
           _ (pin (aw-g E) "NF" #fs)
           fs)))
   ; the tablet holds only the slots a program has read or written
   (aw-fl E)
    (: fl (E 'flds)
       (fl !== 'none ? fl (: t (tablet 0) (pin E 'flds t, t))))
   (aw-nthf l k) (l @ (a . u) (k = 0 ? a (aw-nthf u (k - 1))) ())
   (aw-nf E) (: _ (aw-raws E) (aw-trunc (aw-numv (peep (aw-g E) "NF" 0))))
   ; the value at k as it stands, cooked or raw -- what $0 is rebuilt from
   (aw-fldv E k)
    (: fl (E 'flds)
       v (fl == 'none ? 'nope (peep fl k 'nope))
       (v !== 'nope ? v (aw-nthf (aw-raws E) (k - 1))))
   (aw-rebuild E)
    (: ofs (aw-sv E (peep (aw-g E) "OFS" " "))
       n (aw-nf E) o (jug 0)
       (go k) (n < k ? (slurp o)
               (1 < k && say o ofs,
                say o (aw-sv E (aw-fldv E k)),
                go (k + 1)))
       s (go 1)
       (pin E 'rec s, s))
   (aw-grow E from to)
    (: fl (aw-fl E)
       (fill j) (j <= to && (pin fl j (peep fl j ""), fill (j + 1)))
       (fill from))
   (aw-setnf E k)
    (: n (aw-nf E)
       _ (n < k && aw-grow E (n + 1) k)
       (pin (aw-g E) "NF" k,
        aw-rebuild E))
   (aw-getfld E k)
    (? (k < 0) (aw-err "a field index cannot be negative")
       (k = 0) (aw-mk (peep E 'rec ""))
       (k <= aw-nf E ?
          (: fl (E 'flds)
             v (fl == 'none ? 'nope (peep fl k 'nope))
             (v !== 'nope ? v
                (: c (aw-mk (aw-nthf (aw-raws E) (k - 1)))
                   (pin (aw-fl E) k c, c))))))
   ; $0 as text: aw-str and aw-pv both answer a strnum's own spelling, so the
   ; record needs no cooking where only its letters are wanted
   (aw-rec E) (peep E 'rec "")
   (aw-setfld E k v)
    (? (k < 0) (aw-err "a field index cannot be negative")
       (k = 0) (aw-setrec E (aw-sv E v), v)
       (: n (aw-nf E)
          _ (n < k && (aw-grow E (n + 1) k, pin (aw-g E) "NF" k))
          (pin (aw-fl E) k v,
           aw-rebuild E,
           v)))

   ; -- comparison ------------------------------------------------------------
   ; ⚠ THE STRNUM RULE: two numeric-ish values compare as NUMBERS, anything else
   ; as text. this is why a field carries both faces -- "10" from input is less
   ; than "9" as text and greater as a number, and awk owes the number
   (aw-slt a b)
    (: na #a nb #b
       (go k) (? (na <= k && nb <= k) 0
                 (na <= k) 1
                 (nb <= k) 0
                 (a k < b k) 1
                 (b k < a k) 0
                 (go (k + 1)))
       (go 0))
   ; -- the analyser ---------------------------------------------------------
   ; a node becomes a lambda of E, once, before the run. the tag walk, the
   ; operator spelling and the accessor chains are spent here; what is left over
   ; the records is a call
   (aw-an e)
    (<e @ ('num || 'str) (: v <>e (E \ v))
          'grp (aw-an <>e)
          'var (aw-anvar <>e)
          'field (aw-anfld <>e)
          ; a bare /re/ in an expression is `$0 ~ /re/`, not a value
          'ere (: nds (aw-re <>e)
                  (E \ (rehas nds (aw-rec E) ? 1 0)))
          ; mentioning a[i] creates it -- awk's rule, and `if (a[k])` then leaves
          ; k in the array for a later `in`
          'idx (: nm <>e k (aw-ankey <>>e) (E \ (aw-idx E nm (k E))))
          'cat (: a (aw-an <>e) b (aw-an <>>e)
                  (E \ (aw-sv E (a E) + aw-sv E (b E))))
          'and (: a (aw-an <>e) b (aw-an <>>e)
                  (E \ (aw-bool (a E) && aw-bool (b E) ? 1 0)))
          'or (: a (aw-an <>e) b (aw-an <>>e)
                 (E \ (aw-bool (a E) || aw-bool (b E) ? 1 0)))
          'not (: a (aw-an <>e) (E \ !(aw-bool (a E))))
          'neg (: a (aw-an <>e) (E \ -(aw-numv (a E))))
          'pos (: a (aw-an <>e) (E \ (aw-numv (a E))))
          'pow (: a (aw-an <>e) b (aw-an <>>e)
                  (E \ (aw-numv (a E) ** aw-numv (b E))))
          'cond (: c (aw-an <>e) a (aw-an <>>e) b (aw-an <>>>e)
                   (E \ (aw-bool (c E) ? (a E) (b E))))
          'cmp (aw-ancmp <>e (aw-an <>>e) (aw-an <>>>e))
          'bin (aw-anbin <>e (aw-an <>>e) (aw-an <>>>e))
          'match (aw-anmatch <>e (aw-an <>>e) <>>>e)
          'inarr (: nm <>>e k (aw-ankey <>e)
                    (E \ (peep (aw-arr E nm) (k E) 'nope != 'nope ? 1 0)))
          'assign (aw-anassign <>e (aw-anl <>>e) (aw-an <>>>e))
          'pre (aw-anstep <>e (aw-anl <>>e) 0)
          'post (aw-anstep <>e (aw-anl <>>e) 1)
          'bi (aw-anbi <>e <>>e)
          'call (aw-ancall <>e <>>e)
          'glist (aw-err "a parenthesised list is only usable before `in'")
          (aw-err "internal: an expression shape with no analyser"))
   ; NF is owed by the lazy split, so reading it opens the record first
   (aw-anvar nm)
    (nm = "NF" ? (E \ (aw-raws E, aw-get E nm)) (E \ (aw-get E nm)))
   (aw-idx E nm key)
    (: a (aw-arr E nm) v (peep a key 'nope)
       (v == 'nope ? (pin a key (), ()) v))
   ; a field index: a literal one is folded, so $1 costs a tablet read and no more
   (aw-anfldk ke)
    (<ke @ 'num (: k (aw-trunc <>ke) (E \ k))
            (: f (aw-an ke) (E \ (aw-trunc (aw-numv (f E))))))
   (aw-anfld ke)
    (<ke @ 'num (: k (aw-trunc <>ke) (E \ (aw-getfld E k)))
            (: f (aw-an ke) (E \ (aw-getfld E (aw-trunc (aw-numv (f E)))))))
   ; the subscripts of a[i,j] -> one lambda answering the SUBSEP-joined key
   (aw-ankey es)
    (: fs (map aw-an es)
       (!(two? >fs) ? (: f <fs (E \ (aw-sv E (f E))))
          (E \ (aw-key E (map (\ f (f E)) fs)))))
   ; an lvalue -> [read write], both lambdas of E
   (aw-anl lv)
    (<lv @ 'var (: nm <>lv [(aw-anvar nm) (\ E v (aw-put E nm v))])
           'field (: kf (aw-anfldk <>lv)
                     [(E \ (aw-getfld E (kf E))) (\ E v (aw-setfld E (kf E) v))])
           (: nm <>lv k (aw-ankey <>>lv)
              [(E \ (aw-idx E nm (k E)))
               (\ E v (pin (aw-arr E nm) (k E) v, v))]))
   (aw-anbin op a b)
    (: f (op @ "+" (\ x y (x + y))
               "-" (\ x y (x - y))
               "*" (\ x y (x * y))
               "/" (\ x y (y = 0 ? (aw-err "division by zero") (x / y)))
                   (\ x y (y = 0 ? (aw-err "division by zero in %")
                                   (x - y * aw-trunc (x / y)))))
       (E \ (f (aw-numv (a E)) (aw-numv (b E)))))
   ; the strnum rule: two numeric-ish values compare as numbers, anything else as
   ; text -- so each op is analysed into its pair of faces
   (aw-ancmp op a b)
    (: p (op @ "<" [(\ x y (x < y)) (\ x y (aw-slt x y))]
               "<=" [(\ x y (x <= y)) (\ x y !(aw-slt y x))]
               ">" [(\ x y (y < x)) (\ x y (aw-slt y x))]
               ">=" [(\ x y (y <= x)) (\ x y !(aw-slt x y))]
               "==" [(\ x y (x = y)) (\ x y (x = y))]
                    [(\ x y (x != y)) (\ x y (x != y))])
       n <p t <>p
       (E \ (: u (a E) v (b E)
               (aw-numeric? u && aw-numeric? v ? (n (aw-numv u) (aw-numv v))
                  (t (aw-sv E u) (aw-sv E v))))))
   ; a pattern node -> a lambda answering the compiled regex; a literal /re/ is
   ; compiled here and never looked up again
   (aw-anpat pe)
    (<pe @ 'ere (: nds (aw-re <>pe) (E \ nds))
            (: f (aw-an pe) (E \ (aw-recomp E (aw-sv E (f E))))))
   (aw-anmatch neg a pe)
    (: g (aw-anpat pe)
       (neg ? (E \ (rehas (g E) (aw-sv E (a E)) ? 0 1))
          (E \ (rehas (g E) (aw-sv E (a E)) ? 1 0))))
   (aw-anassign op lp rhs)
    (: get <lp put <>lp
       (? (op = "=") (\ E (put E (rhs E)))
          (: f (op @ "+=" (\ x y (x + y))
                     "-=" (\ x y (x - y))
                     "*=" (\ x y (x * y))
                     "/=" (\ x y (y = 0 ? (aw-err "division by zero") (x / y)))
                     "%=" (\ x y (y = 0 ? (aw-err "division by zero in %")
                                          (x - y * aw-trunc (x / y))))
                        (\ x y (x ** y)))
             (\ E (: y (aw-numv (rhs E)) (put E (f (aw-numv (get E)) y)))))))
   (aw-anstep op lp post)
    (: get <lp put <>lp d (op = "++" ? 1 -1)
       (post ? (\ E (: old (aw-numv (get E)) (put E (old + d), old)))
          (\ E (: nv (aw-numv (get E) + d) (put E nv)))))

   ; -- the builtins ----------------------------------------------------------
   ; ⚠ a start under 1 CLAMPS and the length is not docked for it -- gnu's reading,
   ; not POSIX's: substr("hello",0,3) is "hel" there and "he" by the standard
   (aw-substr s m n has)
    (: len #s
       st (? (m < 1) 1 m)
       en0 (? has (st + n) (len + 1))
       en (? (len + 1 < en0) (len + 1) en0)
       (? (en <= st) "" (snip s (st - 1) (en - 1))))
   (aw-index s t)
    (: n #s m #t
       (go k) (? (n - m < k) 0
                 (snip s k (k + m) = t) (k + 1)
                 (go (k + 1)))
       (m = 0 ? (n != 0) (go 0)))
   ; & in a replacement is the matched text; \& is a literal &
   (aw-repl rep matched)
    (: n (tally rep) o (jug 0)
       (go k) (n <= k ? (slurp o)
               (: c (rep k)
                (? (c = 92 && k + 1 < n)
                   (: d (rep (k + 1))
                      (? (d = 38) (put o 38, go (k + 2))
                         (d = 92) (put o 92, go (k + 2))
                         (put o 92, go (k + 1))))
                   (c = 38) (say o matched, go (k + 1))
                   (put o c, go (k + 1)))))
       (go 0))
   (aw-subst E nds s rep glob)                   ; -> [text count]
    (: n (tally s) o (jug 0)
       (go i cnt)
        (? (n < i) [(slurp o) cnt]
           (refind nds s i @ [_ st en _]
             (: _ (say o (snip s i st))
                _ (say o (aw-repl rep (snip s st en)))
                (? !glob (: _ (say o (snip s en n)) [(slurp o) (cnt + 1)])
                   (en = st) (: _ (? (st < n) (put o (s st)) 0)
                                (go (st + 1) (cnt + 1)))
                   (go en (cnt + 1))))
             (: _ (say o (snip s i n)) [(slurp o) cnt])))
       (go 0 0))
   (aw-anbi nm as)
    (: cnt #as
       (an k) (: nd (aw-nth as k) (two? nd ? (aw-an nd) (E \ ())))
       a0 (an 0) a1 (an 1) a2 (an 2)
       (s0 E) (aw-sv E (a0 E))
       (s1 E) (aw-sv E (a1 E))
       (x0 E) (aw-numv (a0 E))
       (x1 E) (aw-numv (a1 E))
       (nm @ "length"
           (? (cnt = 0) (E \ #(aw-rec E))
              (: nd (aw-nth as 0)
                 vnm (<nd == 'var ? <>nd ())
                 (E \ #(: cur (aw-nil? vnm ? () (aw-get E vnm))
                          (tablet? cur ? (aw-akeys cur) (s0 E))))))
          "substr"
           (? (cnt < 2) (aw-err "substr wants two arguments")
              (: has (2 < cnt)
                 (E \ (aw-substr (s0 E) (aw-trunc (x1 E))
                                 (aw-trunc (aw-numv (a2 E))) has))))
          "index" (E \ (aw-index (s0 E) (s1 E)))
          "sprintf"
           (: rest (map aw-an >as)
              (E \ (aw-sprintf (s0 E) (map (\ f (f E)) rest)
                               (peep (aw-g E) "CONVFMT" "%.6g"))))
          "split"
           (? (cnt < 2) (aw-err "split wants a string and an array")
              (: nd (aw-nth as 1)
                 _ (<nd != 'var ? (aw-err "split wants an array name"))
                 anm <>nd
                 pf (? (2 < cnt) (aw-anfs (aw-nth as 2))
                       (E \ (aw-sv E (peep (aw-g E) "FS" " "))))
                 (E \ (: a (aw-arr E anm)
                         _ (: (clr l) (two? l ? (pin a <l 'nope, clr >l)) (clr (keys a)))
                         ps (aw-fields E (s0 E) (pf E))
                         (go l k) (two? l ? (pin a (show k) (aw-mk <l), go >l (k + 1))
                                     (k - 1))
                         (go ps 1)))))
          ("sub" || "gsub")
           (? (cnt < 2) (aw-err (nm + " wants a pattern and a replacement"))
              (: lv (? (2 < cnt) (aw-nth as 2) ['field ['num 0]])
                 _ (? (aw-lval? lv) 0 (aw-err (nm + " wants a variable to change")))
                 lp (aw-anl lv) get <lp put <>lp
                 pf (aw-anpat (aw-nth as 0))
                 glob (nm = "gsub" ? 1 0)
                 (E \ (: r (aw-subst E (pf E) (aw-sv E (get E)) (s1 E) glob)
                         _ (? (0 < <>r) (put E <r) 0)
                         <>r))))
          "match"
           (: pf (aw-anpat (aw-nth as 1))
              (E \ (refind (pf E) (s0 E) 0 @ [_ st en _]
                     (pin (aw-g E) "RSTART" (st + 1),
                      pin (aw-g E) "RLENGTH" (en - st),
                      st + 1)
                     (pin (aw-g E) "RSTART" 0,
                      pin (aw-g E) "RLENGTH" -1,
                      0))))
          "int" (E \ (aw-trunc (x0 E)))
          "sqrt" (E \ ((1 / 2) (x0 E)))
          "exp" (E \ (** e (x0 E)))
          "log" (E \ (log (x0 E)))
          "sin" (E \ (sine (x0 E)))
          "cos" (E \ (cosine (x0 E)))
          "atan2" (E \ (aw-atan2 (x0 E) (x1 E)))
          "rand" (E \ (randf ()))
          "srand" (E \ (: old (peep E 'seed 0)
                          v (? (0 < cnt) (aw-trunc (x0 E)) 0)
                          (pin E 'seed v, rng-set v, old)))
          "tolower" (E \ (aw-down (s0 E)))
          "toupper" (E \ (aw-up (s0 E)))
          "system" (E \ (: _ (flush out) _ (flush err)
                           p (spawn ["/bin/sh" "-c" (s0 E)])
                           (p < 0 ? 127 (wait p))))
          "close"
           (E \ (: ps (E 'ports) nm2 (s0 E) q (peep ps nm2 'none)
                   (q == 'none ? -1 (close q, pin ps nm2 'none, 0))))
          (aw-err ("no such builtin " + nm))))
   ; split's third argument: a literal /re/ is its own text, anything else a value
   (aw-anfs nd)
    (<nd @ 'ere (: p <>nd (E \ p)) (: f (aw-an nd) (E \ (aw-sv E (f E)))))
   ; atan2 off the arc tangent love carries, quadrant by quadrant
   (aw-atan2 y x)
    (: at (\ t (: s (abs t)
                  a (aw-atan1 s)
                  (t < 0 ? -a a)))
       (? (0 < x) (at (y / x))
          (x < 0) (y < 0 ? (at (y / x) - pi) (at (y / x) + pi))
          (0 < y) (pi / 2)
          (y < 0) (-pi / 2)
          0))
   ; arctan on [0,inf) by bisecting tangent -- love carries tangent, not its inverse
   (aw-atan1 t)
    (: (go lo hi k)
        (? (k < 1) ((lo + hi) / 2)
           (: m ((lo + hi) / 2)
              (tangent m < t ? (go m hi (k - 1)) (go lo m (k - 1)))))
       (go 0 (pi / 2) 60))
   ; an array argument passes its tablet, which is awk's one aliasing rule: the
   ; callee's delete and its assignments are the caller's too. a scalar is
   ; evaluated and copied, so only a name already holding an array aliases
   (aw-anarg nd)
    (<nd @ 'var (: nm <>nd f (aw-anvar nm)
                   (E \ (: cur (aw-get E nm) (tablet? cur ? cur (f E)))))
            (aw-an nd))
   (aw-ancall nm as)
    (: gs (map aw-anarg as)
       vs (map (\ nd (<nd == 'var ? <>nd ())) as)
       (E \ (aw-call E nm gs vs)))
   (aw-call E nm gs vs)
    (: fns (peep E 'fn ())
       f (peep fns nm 'none)
       _ (? (id? f 'none) (aw-err ("calling an undefined function " + nm)) 0)
       ps <f body <>f
       loc (tablet 0)
       (bind l a)
        (? !(two? l) 0
           (: v (two? a ? (<a E) ())
              _ (pin loc <l v)
              (bind >l (two? a ? >a a))))
       _ (bind ps gs)
       old (peep E 'loc ())
       _ (pin E 'loc loc)
       r (trap (\ _ (body E, ()))
               ()
               (\ a b (a == 'awk-ret ? b (pin E 'loc old, scare a b))))
       _ (pin E 'loc old)
       ; an untyped name handed in (g(A) with A never touched) becomes an array
       ; iff the callee subscripted it -- awk decides late, so the answer only
       ; exists now. carry the tablet back to the caller's name
       _ (: (back l a)
             (!(two? l) || !(two? a) ? 0
                (: vnm <a
                   v (loc <l)
                   (!(aw-nil? vnm) && tablet? v && !(tablet? (aw-get E vnm))
                    ? (aw-put E vnm v),
                    back >l >a)))
            (back ps vs))
       r)

   ; -- statements ------------------------------------------------------------
   (aw-anport rd)
    (aw-nil? rd ? (E \ out)
       (: m (<rd = ">>" ? "a" "w") f (aw-an <>rd)
          (E \ (aw-port E (aw-sv E (f E)) m))))
   (aw-port E nm m)
    (: ps (peep E 'ports ())
       p (peep ps nm 'none)
       (p !== 'none ? p
        (: q (open nm m)
           (q || aw-err ("cannot open " + nm + " for writing"),
            pin ps nm q,
            q))))
   (aw-do-print E vs p)
    (: ofs (aw-sv E (peep (aw-g E) "OFS" " "))
       ors (aw-sv E (peep (aw-g E) "ORS" "\n"))
       (go (a . u) first) (first || say p ofs, say p (aw-pv E a), go u 0)
       (go vs 1,
        say p ors,
        0))
   (aw-anprint es rd)
    (: pf (aw-anport rd)
       (two? es ? (: fs (map aw-an es)
                     (E \ (aw-do-print E (map (\ f (f E)) fs) (pf E))))
          (E \ (aw-do-print E [(aw-rec E)] (pf E)))))
   (aw-anprintf es rd)
    (: pf (aw-anport rd) f0 (aw-an <es) rest (map aw-an >es)
       (E \ (: _ (say (pf E) (aw-sprintf (aw-sv E (f0 E)) (map (\ g (g E)) rest)
                                         (peep (aw-g E) "CONVFMT" "%.6g")))
               0)))
   (aw-anlist ss)
    (: fs (map aw-ax ss)
       (? !(two? fs) (E \ 0)
          !(two? >fs) <fs
          (E \ (: (go (f . u)) (f E, go u) (go fs)))))
   ; one turn of a loop body: 1 keep going, 0 the break came through
   (aw-iter E b)
    (trap (_ \ (b E, 1))
          ()
          (\ a c (a @ 'awk-break  0 'awk-cont 1 (scare a c))))
   (aw-loop E c b st)
    (aw-bool (c E) && aw-iter E b && (st E, aw-loop E c b st))
   (aw-forin E nm arr b)
    (: a (aw-arr E arr)
       (go (al . ul))
        (aw-put E nm (aw-mk al),
         aw-iter E b && go ul)
       (go (aw-akeys a)))
   (aw-ax s)
    (<s @ 'blk (aw-anlist <>s)
          'nop (E \ 0)
          'expr (: f (aw-an <>s) (E \ (f E, 0)))
          'print (aw-anprint <>s <>>s)
          'printf (aw-anprintf <>s <>>s)
          'if (: c (aw-an <>s) a (aw-ax <>>s)
                 (aw-nil? <>>>s ? (E \ (aw-bool (c E) ? (a E) 0))
                    (: b (aw-ax <>>>s) (E \ (aw-bool (c E) ? (a E) (b E))))))
          'while (: c (aw-an <>s) b (aw-ax <>>s) (E \ (aw-loop E c b (\ _ 0))))
          'do (: b (aw-ax <>s) c (aw-an <>>s)
                 (E \ (: (again _) (aw-iter E b && aw-bool (c E) && again 0)
                         (again 0))))
          'for (: ini (aw-nil? <>s ? (\ _ 0) (aw-ax <>s))
                  c (aw-nil? <>>s ? (\ _ 1) (aw-an <>>s))
                  st (aw-nil? <>>>s ? (\ _ 0) (aw-ax <>>>s))
                  b (aw-ax <>>>>s)
                  (E \ (ini E, aw-loop E c b st)))
          'forin (: nm <>s arr <>>s b (aw-ax <>>>s) (E \ (aw-forin E nm arr b)))
          'next (E \ (scare 'awk-next ()))
          'break (E \ (scare 'awk-break ()))
          'cont (E \ (scare 'awk-cont ()))
          'exit (aw-nil? <>s ? (E \ (scare 'awk-exit ()))
                   (: f (aw-an <>s)
                      (E \ (scare 'awk-exit (aw-trunc (aw-numv (f E)))))))
          'ret (aw-nil? <>s ? (E \ (scare 'awk-ret ()))
                  (: f (aw-an <>s) (E \ (scare 'awk-ret (f E)))))
          'del (: nm <>s k (aw-ankey <>>s)
                  (E \ (pin (aw-arr E nm) (k E) 'nope, 0)))
          'delall (: nm <>s
                     (E \ (: a (aw-arr E nm)
                             (go l) (? (two? l) (: _ (pin a <l 'nope) (go >l)) 0)
                             (go (keys a))))))

   ; -- the record loop -------------------------------------------------------
   ; an item is analysed once: ('begin f) ('end f) ('item pf af) ('range pf qf af)
   (aw-anact b) (aw-nil? b ? (E \ (aw-do-print E [(aw-rec E)] out)) (aw-anlist b))
   (aw-anitem it)
    (<it @ 'begin ['begin (aw-anlist <>it)]
            'end ['end (aw-anlist <>it)]
            'item ['item (aw-nil? <>it ? () (aw-an <>it)) (aw-anact <>>it)]
            ['range (aw-an <>it) (aw-an <>>it) (aw-anact <>>>it)])
   (aw-anfns fns)
    (: (go l) (l @ (nm . u)
                (: f (peep fns nm ())
                   _ (pin fns nm [<f (aw-anlist <>f)])
                   (go u))
                0)
       (go (keys fns)))
   (aw-range E it i)
    (: rs (E 'ranges)
       b <>>>it
       (? (peep rs i 0)
          (aw-bool (<>>it E) && pin rs i 0, b E)
          (aw-bool (<>it E) ?
             (aw-bool (<>>it E) || pin rs i 1, b E)
             0)))
   (aw-record E items line)
    (: _ (pins (aw-g E) "NR" (aw-nrn E "NR" + 1)
                       "FNR" (aw-nrn E "FNR" + 1),
          aw-setrec E line)
       (go (it . u) i)
        (it @ ['item a b]  ((aw-nil? a || aw-bool (a E)) && b E)
              ('range . _) (aw-range E it i),
         go u (i + 1))
       (go items 0))
   (aw-feed E items line)
    (trap (\ _ (: _ (aw-record E items line) 0))
          ()
          (\ a b (a == 'awk-next ? 0 (scare a b))))
   (aw-cmdas? a)
    (: n #a
       (go k) (? (n <= k) 0
                 (a k = 61) (0 < k)
                 (aw-aln? (a k)) (k = 0 && aw-dig? (a k) ? 0 (go (k + 1)))
                 0)
       (go 0))
   (aw-unesc s)
    (: n #s o (jug 0)
       (go k) (n <= k ? (slurp o)
               (: c (s k)
                  (c = 92 && k + 1 < n ? (put o (aw-esc (s (k + 1))), go (k + 2))
                   (put o c, go (k + 1)))))
       (go 0))
   (aw-cmdas E a)
    (: n #a
       (fnd k) (a k = 61 ? k (fnd (k + 1)))
       i (fnd 0)
       (aw-put E (snip a 0 i) (aw-mk (aw-unesc (snip a (i + 1) n)))))
   (aw-input E items as)
    (: (one f)
        (: t (? (f = "-") (slurp in)
              (uread f @ [_ r] r (aw-err ("cannot open " + f))))
           _ (pin (aw-g E) "FILENAME" (? (f = "-") "" f))
           _ (pin (aw-g E) "FNR" 0)
           (go l) (? (two? l) (: _ (aw-feed E items <l) (go >l)) 0)
           (go (lines t)))
       (go l any)
        (? (atom? l) (? any 0 (one "-"))
           (aw-cmdas? <l) (aw-cmdas E <l, go >l any)
           (one <l, go >l 1))
       (go as 0))
   (aw-phase E items which)
    (: (go (a . u)) (<a == which && <>a E, go u)
       (go items))
   (aw-needs? items)
    (: (go (a . u)) (<a != 'begin || go u) (go items))

   ; -- the tool --------------------------------------------------------------
   (aw-flags a fs vs pfs)
    (? !(two? a) [fs (rev vs) (rev pfs) a]
       (: w <a n (tally w)
          glued (2 < n ? (snip w 2 n))
          (? (w = "--") [fs (rev vs) (rev pfs) >a]
             (w = "-F") (two? >a ? (aw-flags >>a <>a vs pfs) (udie 2 "awk: -F wants a separator"))
             (w = "-v") (two? >a ? (aw-flags >>a fs (<>a . vs) pfs) (udie 2 "awk: -v wants var=value"))
             (w = "-f") (two? >a ? (aw-flags >>a fs vs (<>a . pfs)) (udie 2 "awk: -f wants a file"))
             (2 < n && w 0 = 45 && w 1 = 70) (aw-flags >a glued vs pfs)
             (2 < n && w 0 = 45 && w 1 = 118) (aw-flags >a fs (glued . vs) pfs)
             (2 < n && w 0 = 45 && w 1 = 102) (aw-flags >a fs vs (glued . pfs))
             [fs (rev vs) (rev pfs) a])))
   (awk-main as)
    (aw-flags as () () () @ [fs vs pfs rest]
     (: fns (tablet 0)
        src (? (two? pfs)
               (: (cat l acc) (? !(two? l) acc
                                 (: r (uread <l)
                                    (? (two? r) (cat >l (acc + <>r + "\n"))
                                       (udie 2 ("awk: cannot open " + <l)))))
                  (cat pfs ""))
               (? (two? rest) <rest
                  (udie 2 "usage: awk [-F FS] [-v var=val].. 'PROGRAM' [FILE..]")))
        files (? (two? pfs) rest (? (two? rest) >rest ()))
        E (aw-new fns)
        (blow m) (: _ (say err ("awk: " + m)) _ (put err 10) (uleave 2))
        items (trap (\ _ (: is (aw-program (aw-lex src) fns ())
                             _ (aw-anfns fns)
                             (map aw-anitem is)))
                    ()
                    (\ a b (? (id? a 'awk-err) (blow b) (scare a b))))
        _ (? (aw-nil? fs) 0 (pin (aw-g E) "FS" (aw-unesc fs)))
        _ (map (\ v (aw-cmdas E v)) vs)
        ; ⚠ exit outside END still runs the END rules -- POSIX, and the whole
        ; reason the phase is tracked; an exit INSIDE them is the end of it
        (ending b)
         (: _ (? (id? (peep E 'ph 'end) 'end) 0
                 (: _ (pin E 'ph 'end)
                    (trap (\ _ (aw-phase E items 'end))
                          ()
                          (\ a2 b2 (? (id? a2 'awk-exit) 0
                                      (id? a2 'awk-err) (blow b2)
                                      (scare a2 b2))))))
            (? (aw-nil? b) 0 b))
        code
         (trap
          (\ _ (: _ (aw-phase E items 'begin)
                  _ (? (aw-needs? items)
                       (: _ (pin E 'ph 'main) (aw-input E items files)) 0)
                  _ (pin E 'ph 'end)
                  _ (aw-phase E items 'end)
                  0))
          ()
          (\ a b (? (id? a 'awk-err) (blow b)
                    (id? a 'awk-exit) (ending b)
                    (scare a b))))
        _ (: (shut l) (? (two? l) (: p (peep (peep E 'ports ()) <l 'none)
                                     _ (? (id? p 'none) 0 (close p))
                                     (shut >l))
                          0)
             (shut (keys (peep E 'ports ()))))
        _ (flush out)
        code)
     2))
) ; module 'kore
