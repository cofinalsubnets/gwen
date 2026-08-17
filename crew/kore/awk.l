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
(:
   ; -- values --------------------------------------------------------------
   ; a value is () uninitialised, a number, a string, or ('sn n s) -- a STRNUM,
   ; the thing that came off input looking like a number: it must COMPARE as a
   ; number and PRINT as the text it arrived in ("007" is 7 and prints 007)
   ; ⚠ `nil?` IS A TRUTH TEST, not a type one -- it answers 1 for 0, for -4 and
   ; for "" as readily as for (). awk leans on the difference every line (an
   ; uninitialised value is () and prints "", the number 0 prints "0"), so the
   ; empty question is asked by identity and never by truth
   (aw-nil? v) (id? v ())
   (aw-sn? v) ((two? v) && (id? <v 'sn))
   (aw-numeric? v) (? (string? v) 0 1)          ; () and strnum both compare numeric
   (aw-blank? c) ((c = 32) || (c = 9) || (c = 10))
   (aw-dig? c) (48 <= c <= 57)
   ; the numeric span of s inside [a,b): (past-the-end) -- a if there is no number
   (aw-nspan s a b)
    (: (run k) (? ((k < b) && aw-dig? (s k)) (run (k + 1)) k)
       i1 (? ((a < b) && ((s a = 45) || (s a = 43))) (a + 1) a)
       i2 (run i1)
       i3 (? ((i2 < b) && (s i2 = 46)) (run (i2 + 1)) i2)
       (? ((i2 = i1) && (i3 = i2)) a                       ; no digits at all
          ((i3 < b) && ((s i3 = 101) || (s i3 = 69)))
           (: k (? (((i3 + 1) < b) && ((s (i3 + 1) = 45) || (s (i3 + 1) = 43)))
                   (i3 + 2) (i3 + 1))
              k2 (run k)
              (? (k2 = k) i3 k2))                          ; a bare e is not an exponent
          i3))
   ; the text of a number -> the number (the span is already known good)
   (aw-parse s a b)
    (: neg (s a = 45)
       i1 (? ((s a = 45) || (s a = 43)) (a + 1) a)
       (mant k acc sc dot)
        (? (b <= k) [acc sc]
           (: c (s k)
              (? (c = 46) (mant (k + 1) acc sc 1)
                 (aw-dig? c) (mant (k + 1) ((* 10 acc) + (c - 48)) (? dot (sc * 10) sc) dot)
                 [acc sc])))
       (fnd k) (? (b <= k) b ((s k = 101) || (s k = 69)) k (fnd (k + 1)))
       e (fnd i1)
       m (mant i1 0 1 0)
       base (/ <m <>m)
       ex (? (e = b) 0
             (: t (snip s (e + 1) b)
                (? ((0 < tally t) && (t 0 = 45)) (0 - uatoi (snip t 1 (tally t)))
                   ((0 < tally t) && (t 0 = 43)) (uatoi (snip t 1 (tally t)))
                   (uatoi t))))
       v (? (ex = 0) base (base * (** 10 ex)))
       (? neg (0 - v) v))
   ; awk's string->number: the longest numeric PREFIX, blanks first, 0 for none
   (aw-s2n s)
    (: n (tally s)
       (sp k) (? ((k < n) && aw-blank? (s k)) (sp (k + 1)) k)
       a (sp 0)
       e (aw-nspan s a n)
       (? (e = a) 0 (aw-parse s a e)))
   ; does the WHOLE text (blanks aside) read as a number -- the strnum question
   (aw-looks-num? s)
    (: n (tally s)
       (sp k) (? ((k < n) && aw-blank? (s k)) (sp (k + 1)) k)
       (bk k) (? ((0 < k) && aw-blank? (s (k - 1))) (bk (k - 1)) k)
       a (sp 0) b (bk n)
       (? !(a < b) 0 ((aw-nspan s a b) = b)))
   ; a text off the input -> the value it should behave as
   (aw-mk s) (? (aw-looks-num? s) ['sn (aw-s2n s) s] s)
   (aw-numv v) (? (aw-nil? v) 0 (string? v) (aw-s2n v) (aw-sn? v) <>v v)
   (aw-bool v) (? (aw-nil? v) 0
                  (string? v) (? (0 < tally v) 1 0)
                  (aw-sn? v) (? (<>v = 0) 0 1)
                  (? (v = 0) 0 1))

   ; -- the decimal engine: C's %f, %e and %g by hand --------------------------
   (aw-trunc x) (? (((0 - 9.0e18) < x) && (x < 9.0e18)) (int x) x)
   ; ⚠ AN INTEGRAL DOUBLE PAST A CHARM STILL OWES ITS DIGITS. awk prints 1e20 as
   ; its twenty-one figures, and `int` overflows to 0 there. every double at that
   ; size is m*2^k for an exact m under 2^53, so scale down to m, take it as a
   ; charm, and scale back up through love's bigints -- exact, and glibc-identical
   ; even where the double is not the round number it was written as (1e23 is
   ; 99999999999999991611392 to both)
   (aw-bigint x)
    (: a (? (x < 0) (0 - x) x)
       (k j) (? ((/ a (** 2 j)) < 9.0e15) j (k (j + 1)))
       j (k 0)
       (* (int (/ x (** 2 j))) (** 2 j)))
   (aw-intstr x)
    (? (((0 - 9.0e18) < x) && (x < 9.0e18)) (show (int x)) (show (aw-bigint x)))
   (aw-zeros k) (? (k < 1) "" ("0" + (aw-zeros (k - 1))))
   (aw-spaces k) (? (k < 1) "" (" " + (aw-spaces (k - 1))))
   (aw-lpad w s) (? (tally s < w) (aw-lpad w ("0" + s)) s)
   ; |x| > 0 -> the decimal exponent e with 1 <= |x|/10^e < 10
   (aw-exp10 a)
    (: e0 (aw-trunc (/ (log a) (log 10)))
       (up e) (? !((/ a (** 10 e)) < 10) (up (e + 1)) e)
       (dn e) (? ((/ a (** 10 e)) < 1) (dn (e - 1)) e)
       (dn (up e0)))
   ; %.<nd>f
   (aw-fixed x nd)
    (: neg (x < 0)
       a (? neg (0 - x) x)
       y (a * (** 10 nd))
       ; past a charm the value is integral already, so its exact digits plus nd
       ; zeros ARE the fixed spelling
       (? !(y < 9.0e18)
          (: s ((? neg "-" "") + (aw-intstr a))
             (? (nd < 1) s (s + "." + (aw-zeros nd))))
          (: r (int (y + 0.5))
             s (show r)
             body (? (nd < 1) s
                     (: p (aw-lpad (nd + 1) s)
                        m (tally p)
                        ((snip p 0 (m - nd)) + "." + (snip p (m - nd) m))))
             (? neg ("-" + body) body))))
   ; %.<nd>e
   (aw-expo x nd)
    (: neg (x < 0)
       a (? neg (0 - x) x)
       sg (? neg "-" "")
       (tl e) (: d (show (? (e < 0) (0 - e) e))
                 ("e" + (? (e < 0) "-" "+") + (? (tally d < 2) ("0" + d) d)))
       (? (a = 0) (sg + (aw-fixed 0 nd) + (tl 0))
          (: e (aw-exp10 a)
             m (/ a (** 10 e))
             carry !(m < (10 - (0.5 / (** 10 nd))))   ; the round can carry to 10.0
             e2 (? carry (e + 1) e)
             ms (aw-fixed (? carry (/ a (** 10 e2)) m) nd)
             (sg + ms + (tl e2)))))
   ; drop the trailing zeros of a fraction, then a bare trailing point
   (aw-strip s)
    (: n (tally s)
       (dot k) (? (n <= k) -1 (s k = 46) k (dot (k + 1)))
       d (dot 0)
       (? (d < 0) s
          (: (bk k) (? ((d < k) && (s (k - 1) = 48)) (bk (k - 1)) k)
             b (bk n)
             (snip s 0 (? ((b - 1) = d) d b)))))
   ; %.<np>g -- e-form outside [-4, np), f-form inside, trailing zeros dropped
   (aw-gen x np0)
    (: np (? (np0 < 1) 1 np0)
       neg (x < 0)
       a (? neg (0 - x) x)
       (? (a = 0) "0"
          (: e (aw-exp10 a)
             (? ((e < -4) || !(e < np))
                (: s (aw-expo x (np - 1))
                   n (tally s)
                   (ep k) (? (n <= k) n ((s k = 101) || (s k = 69)) k (ep (k + 1)))
                   i (ep 0)
                   ((aw-strip (snip s 0 i)) + (snip s i n)))
                (aw-strip (aw-fixed x (np - 1 - e)))))))
   ; a number as awk writes it: an integral value is its digits, whatever CONVFMT
   (aw-numstr x fmt)
    (? (x = (aw-trunc x)) (aw-intstr x) (aw-sprintf fmt [x] "%.6g"))
   (aw-str v cf) (? (aw-nil? v) ""
                    (string? v) v
                    (aw-sn? v) <>>v
                    (aw-numstr v cf))
   (aw-map-case s up)
    (: n (tally s) o (jug 0)
       (go k) (? (n <= k) (slurp o)
                 (: c (s k)
                    _ (put o (? (up && (97 <= c <= 122)) (c - 32)
                                (!up && (65 <= c <= 90)) (c + 32)
                                c))
                    (go (k + 1))))
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
                 (: (num j acc got) (? ((j < n) && aw-dig? (f j))
                                       (num (j + 1) ((* 10 acc) + (f j - 48)) 1)
                                       [acc got j])
                    w (num k 0 0)
                    k2 <>>w
                    pr (? ((k2 < n) && (f k2 = 46)) (num (k2 + 1) 0 1) [0 0 k2])
                    [m z p sp <w (? <>pr <pr -1) <>>pr]))))
       (fl k0 0 0 0 0))
   (aw-pad m z w s)
    (: t (tally s)
       (? !(t < w) s
          m (s + (aw-spaces (w - t)))
          z (? ((0 < t) && ((s 0 = 45) || (s 0 = 43)))
               ((string (s 0)) + (aw-zeros (w - t)) + (snip s 1 t))
               ((aw-zeros (w - t)) + s))
          ((aw-spaces (w - t)) + s)))
   (aw-sgn p sp s) (? ((0 < tally s) && (s 0 = 45)) s  p ("+" + s)  sp (" " + s)  s)
   ; ONE conversion at f k (which is a %) -> (next-k args-left), the text said into o
   (aw-conv o f n k l cf)
    (aw-spec f n (k + 1) @ [m z pl spc w pr j]
     (? (n <= j) (: _ (put o 37) [(k + 1) l])
        (: d (f j)
           v (? (two? l) <l ())
           rest (? (two? l) >l l)
           num (aw-numv v)
           done [(j + 1) rest]
           (? (d = 37) (: _ (put o 37) [(j + 1) l])
              (d = 99)
               (: t (? (aw-nil? v) "" (string? v) v (aw-sn? v) <>>v ())
                  s (? (string? t) (? (0 < tally t) (string (t 0)) "")
                       (string (aw-trunc num)))
                  _ (say o (aw-pad m 0 w s)) done)
              (d = 115)
               (: s0 (aw-str v cf)
                  s (? (pr < 0) s0 (snip s0 0 (? (tally s0 < pr) (tally s0) pr)))
                  _ (say o (aw-pad m 0 w s)) done)
              ((d = 100) || (d = 105))
               (: s (aw-sgn pl spc (aw-sp-int num))
                  _ (say o (aw-pad m z w s)) done)
              ((d = 111) || (d = 120) || (d = 88) || (d = 117))
               (: x (aw-trunc num)
                  y (? (x < 0) (0 - x) x)
                  s0 (? (d = 111) (aw-base y 8 0)
                        (d = 117) (show y)
                        (aw-base y 16 (? (d = 88) 1 0)))
                  s (? (x < 0) ("-" + s0) s0)
                  _ (say o (aw-pad m z w s)) done)
              ((d = 102) || (d = 70))
               (: s (aw-sgn pl spc (aw-fixed num (? (pr < 0) 6 pr)))
                  _ (say o (aw-pad m z w s)) done)
              ((d = 101) || (d = 69))
               (: s0 (aw-expo num (? (pr < 0) 6 pr))
                  s (aw-sgn pl spc (? (d = 69) (aw-up s0) s0))
                  _ (say o (aw-pad m z w s)) done)
              ((d = 103) || (d = 71))
               (: s0 (aw-gen num (? (pr < 0) 6 pr))
                  s (aw-sgn pl spc (? (d = 71) (aw-up s0) s0))
                  _ (say o (aw-pad m z w s)) done)
              (: _ (put o 37) [(k + 1) l]))))
     [(k + 1) l])
   (aw-sprintf f as cf)
    (: n (tally f) o (jug 0)
       (go k l)
        (? (n <= k) (slurp o)
           (: c (f k)
              (? ((c = 37) && ((k + 1) < n))
                 (aw-conv o f n k l cf @ [nk rest] (go nk rest) (slurp o))
                 (: _ (put o c) (go (k + 1) l)))))
       (go 0 as))

   ; -- the lexer -----------------------------------------------------------
   ; every refusal from here down leaves through one scare, caught in awk-main:
   ; a syntax error is not a value any caller could do anything with
   (aw-err m) (scare 'awk-err m)
   (aw-alp? c) ((97 <= c <= 122) || (65 <= c <= 90) || (c = 95))
   (aw-aln? c) ((aw-alp? c) || (aw-dig? c))
   (aw-kw? w) (member? w '("BEGIN" "END" "function" "func" "if" "else" "while"
                           "for" "do" "break" "continue" "next" "exit" "return"
                           "delete" "in" "print" "printf" "getline"))
   ; ⚠ the / is TWO tokens depending on what came before it: division after a
   ; value, and the opening of a regex everywhere else. this is the whole reason
   ; the lexer carries the previous token
   (aw-div? t)
    (? (aw-nil? t) 0
       (: k <t
          (? ((id? k 'num) || (id? k 'str) || (id? k 'name) || (id? k 'ere)) 1
             (id? k 'op) (? (member? <>t '(")" "]" "++" "--")) 1 0)
             0)))
   aw-ops '("**=" "+=" "-=" "*=" "/=" "%=" "^=" "==" "!=" "<=" ">=" "&&" "||"
            "++" "--" "!~" ">>" "**" "{" "}" "(" ")" "[" "]" ";" "," "+" "-"
            "*" "/" "%" "^" "!" ">" "<" "|" "?" ":" "~" "$" "=")
   (aw-op-at s n k)                              ; -> [text next] | ()
    (: (has t) (: m (tally t) (((k + m) <= n) && (t = (snip s k (k + m)))))
       (try l) (? !(two? l) ()
                  (has <l) (: w <l
                              nm (? (w = "**") "^" (? (w = "**=") "^=" w))
                              [nm (k + (tally w))])
                  (try >l))
       (try aw-ops))
   (aw-esc c) (? (c = 110) 10 (c = 116) 9 (c = 114) 13 (c = 92) 92 (c = 34) 34
                 (c = 47) 47 (c = 97) 7 (c = 98) 8 (c = 102) 12 (c = 118) 11 c)
   ; a "..." body from i (past the quote) -> [text next]; the escapes are eaten here
   (aw-strlit s n i)
    (: o (jug 0)
       (go k) (? (n <= k) (aw-err "unterminated string")
                 (: c (s k)
                    (? (c = 34) [(slurp o) (k + 1)]
                       ((c = 92) && ((k + 1) < n))
                        (: d (s (k + 1))
                           (? (48 <= d <= 55)
                              (: (oct j v cnt)
                                  (? ((cnt < 3) && (j < n) && (48 <= (s j) <= 55))
                                     (oct (j + 1) ((* 8 v) + ((s j) - 48)) (cnt + 1))
                                     [v j])
                                 (oct (k + 1) 0 0 @ [v j] (: _ (put o v) (go j)) ()))
                              (: _ (put o (aw-esc d)) (go (k + 2)))))
                       (: _ (put o c) (go (k + 1))))))
       (go i))
   ; a /.../ body from i -> [text next]; only \/ is eaten, the rest is the engine's
   (aw-erelit s n i)
    (: o (jug 0)
       (go k) (? (n <= k) (aw-err "unterminated regex")
                 (: c (s k)
                    (? (c = 47) [(slurp o) (k + 1)]
                       (c = 10) (aw-err "newline in regex")
                       ((c = 92) && ((k + 1) < n))
                        (: d (s (k + 1))
                           (? (d = 47) (: _ (put o 47) (go (k + 2)))
                              (: _ (put o 92) _ (put o d) (go (k + 2)))))
                       (: _ (put o c) (go (k + 1))))))
       (go i))
   (aw-lex src)
    (: n (tally src)
       (eol j) (? ((j < n) && !((src j) = 10)) (eol (j + 1)) j)
       (wd j) (? ((j < n) && (aw-aln? (src j))) (wd (j + 1)) j)
       (go k prev acc)
        (? (n <= k) (rev (['eof ""] >< acc))
           (: c (src k)
              (? ((c = 32) || (c = 9) || (c = 13)) (go (k + 1) prev acc)
                 (c = 35) (go (eol k) prev acc)
                 ; a backslash-newline is a continuation: neither token nor break
                 (c = 92) (go (? (((k + 1) < n) && ((src (k + 1)) = 10)) (k + 2) (k + 1))
                              prev acc)
                 (c = 10) (: t ['nl ""] (go (k + 1) t (t >< acc)))
                 (c = 34) (aw-strlit src n (k + 1) @ [s j]
                            (: t ['str s] (go j t (t >< acc))) ())
                 ((c = 47) && !(aw-div? prev))
                  (aw-erelit src n (k + 1) @ [s j]
                    (: t ['ere s] (go j t (t >< acc))) ())
                 ((aw-dig? c) || ((c = 46) && ((k + 1) < n) && (aw-dig? (src (k + 1)))))
                  (: e (aw-nspan src k n)
                     t ['num (aw-parse src k e)]
                     (go e t (t >< acc)))
                 (aw-alp? c)
                  (: e (wd k)
                     w (snip src k e)
                     ; a ( GLUED to the name is a call; spaced, it is concatenation
                     t (? (aw-kw? w) ['kw w]
                          ((e < n) && ((src e) = 40)) ['fname w]
                          ['name w])
                     (go e t (t >< acc)))
                 (aw-op-at src n k @ [w j]
                   (: t ['op w] (go j t (t >< acc)))
                   (aw-err ("unexpected character " + (string c)))))))
       (go 0 () ()))

   ; -- the parser ----------------------------------------------------------
   ; every door answers [node rest]; a refusal scares, so nothing here carries a
   ; failure shape and the arms stay readable
   (aw-k? ts k) ((two? ts) && (id? <<ts k))
   (aw-op? ts v) ((two? ts) && (id? <<ts 'op) && (<><ts = v))
   (aw-kwd? ts v) ((two? ts) && (id? <<ts 'kw) && (<><ts = v))
   (aw-eof? ts) (? (two? ts) (aw-k? ts 'eof) 1)
   (aw-nl ts) (? (aw-k? ts 'nl) (aw-nl >ts) ts)           ; newlines only
   (aw-term ts) (? ((aw-k? ts 'nl) || (aw-op? ts ";")) (aw-term >ts) ts)
   (aw-want ts v m) (? (aw-op? ts v) >ts (aw-err m))
   (aw-lval? e) (: k <e (? ((id? k 'var) || (id? k 'field) || (id? k 'idx)) 1 0))
   aw-assigns '("=" "+=" "-=" "*=" "/=" "%=" "^=")
   aw-rels '("<" "<=" "==" "!=" ">=" ">")
   aw-bis '("length" "substr" "index" "split" "sub" "gsub" "match" "sprintf"
            "sin" "cos" "atan2" "exp" "log" "sqrt" "int" "rand" "srand"
            "tolower" "toupper" "system" "close")
   ; can this token open an operand -- the whole of concatenation's grammar
   (aw-starts? ts)
    (? !(two? ts) 0
       (: t <ts k <t v <>t
          (? ((id? k 'num) || (id? k 'str) || (id? k 'ere)
              || (id? k 'name) || (id? k 'fname)) 1
             (id? k 'op) (? (member? v '("$" "(" "!" "++" "--")) 1 0)
             0)))
   ; a comma-separated expression list, closed by the caller
   (aw-elist ts ng acc)
    (aw-e-assign ts ng @ [e r]
      (? (aw-op? r ",") (aw-elist (aw-nl >r) ng (e >< acc)) [(rev (e >< acc)) r])
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
      (? (((aw-op? r "++") || (aw-op? r "--")) && (aw-lval? e))
         [['post <><r e] >r] [e r])
      ())
   (aw-e-pow ts ng)
    (aw-e-post ts ng @ [a r]
      (? (aw-op? r "^") (aw-e-un >r ng @ [b r2] [['pow a b] r2] ()) [a r])
      ())
   (aw-e-un ts ng)
    (? (aw-op? ts "!") (aw-e-un >ts ng @ [e r] [['not e] r] ())
       (aw-op? ts "-") (aw-e-un >ts ng @ [e r] [['neg e] r] ())
       (aw-op? ts "+") (aw-e-un >ts ng @ [e r] [['pos e] r] ())
       (aw-e-pow ts ng))
   (aw-e-mul ts ng)
    (aw-e-un ts ng @ [a r]
      (: (go n m)
          (? ((aw-op? m "*") || (aw-op? m "/") || (aw-op? m "%"))
             (aw-e-un >m ng @ [b m2] (go ['bin <><m n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-add ts ng)
    (aw-e-mul ts ng @ [a r]
      (: (go n m)
          (? ((aw-op? m "+") || (aw-op? m "-"))
             (aw-e-mul >m ng @ [b m2] (go ['bin <><m n b] m2) ())
             [n m])
         (go a r))
      ())
   ; concatenation is the operator with no spelling: two operands side by side
   (aw-e-cat ts ng)
    (aw-e-add ts ng @ [a r]
      (: (go n m)
          (? (aw-starts? m) (aw-e-add m ng @ [b m2] (go ['cat n b] m2) ()) [n m])
         (go a r))
      ())
   ; ⚠ NON-ASSOCIATIVE, like awk: a < b < c is a parse error there and here too,
   ; which is why this takes one relop and stops
   (aw-e-rel ts ng)
    (aw-e-cat ts ng @ [a r]
      (? ((aw-k? r 'op) && (member? <><r (? ng '("<" "<=" "==" "!=" ">=") aw-rels)))
         (aw-e-cat >r ng @ [b r2] [['cmp <><r a b] r2] ())
         [a r])
      ())
   (aw-e-match ts ng)
    (aw-e-rel ts ng @ [a r]
      (: (go n m)
          (? ((aw-op? m "~") || (aw-op? m "!~"))
             (aw-e-rel >m ng @ [b m2] (go ['match (? (<><m = "!~") 1 0) n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-in ts ng)
    (aw-e-match ts ng @ [a r]
      (: (go n m)
          (? (aw-kwd? m "in")
             (? (aw-k? >m 'name)
                (go ['inarr (? (id? <n 'glist) <>n [n]) (<>(<>m))] >>m)
                (aw-err "in wants an array name"))
             [n m])
         (go a r))
      ())
   (aw-e-and ts ng)
    (aw-e-in ts ng @ [a r]
      (: (go n m)
          (? (aw-op? m "&&") (aw-e-in (aw-nl >m) ng @ [b m2] (go ['and n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-or ts ng)
    (aw-e-and ts ng @ [a r]
      (: (go n m)
          (? (aw-op? m "||") (aw-e-and (aw-nl >m) ng @ [b m2] (go ['or n b] m2) ())
             [n m])
         (go a r))
      ())
   (aw-e-tern ts ng)
    (aw-e-or ts ng @ [c r]
      (? (aw-op? r "?")
         (aw-e-tern (aw-nl >r) ng @ [a r2]
           (: r3 (? (aw-op? r2 ":") (aw-nl >r2) (aw-err "expected : in ?:"))
              (aw-e-tern r3 ng @ [b r4] [['cond c a b] r4] ()))
           ())
         [c r])
      ())
   (aw-e-assign ts ng)
    (aw-e-tern ts ng @ [n r]
      (? ((aw-k? r 'op) && (member? <><r aw-assigns) && (aw-lval? n))
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
              (aw-stmt m2 @ [s r2] (go r2 (s >< acc)) ())))
       (go r ()))
   (aw-redir ts)                                 ; -> [redir rest]
    (? ((aw-op? ts ">") || (aw-op? ts ">>"))
       (: mode <><ts
          (aw-e-cat >ts 1 @ [e r] [[mode e] r] ()))
       (aw-op? ts "|") (aw-err "output pipes are out of dialect")
       [() ts])
   (aw-prargs ts)                                ; print's list -> [args redir rest]
    (? ((aw-k? ts 'nl) || (aw-op? ts ";") || (aw-op? ts "}") || (aw-eof? ts)
        || (aw-op? ts ">") || (aw-op? ts ">>"))
       (aw-redir ts @ [rd r] [() rd r] ())
       (aw-elist ts 1 () @ [es r]
         ; `print (a, b)` is the whole list in brackets, not one grouped operand
         (: es2 (? ((!(two? >es)) && (id? <<es 'glist)) (<>(<es)) es)
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
           (? (aw-starts? r) (aw-expr r @ [e r2] [['exit e] r2] ()) [['exit ()] r])
          (aw-kwd? ts "return")
           (? (aw-starts? r) (aw-expr r @ [e r2] [['ret e] r2] ()) [['ret ()] r])
          (aw-kwd? ts "delete")
           (? !(aw-k? r 'name) (aw-err "delete wants an array")
              (aw-op? >r "[")
               (aw-subs >>r @ [es r2] [['del <>(<r) es] r2] ())
              [['delall <>(<r)] >r])
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
             r3 (? (aw-kwd? r2 "while") (aw-want >r2 "(" "expected ( after while")
                   (aw-err "expected while after a do body"))
             (aw-expr (aw-nl r3) @ [c r4]
               [['do b c] (aw-want r4 ")" "expected ) after the do condition")] ()))
          ())
       (aw-kwd? ts "for") (aw-for >ts)
       (aw-simple ts))
   ; the two for shapes: (v in arr) and the three-part one
   (aw-for ts)
    (: r (aw-want ts "(" "expected ( after for")
       (? ((aw-k? r 'name) && (aw-kwd? >r "in"))
          (? !(aw-k? >>r 'name) (aw-err "for .. in wants an array")
             (: nm <>(<r) arr (<>(<(>>r)))
                r2 (aw-want >>>r ")" "expected ) after for .. in")
                (aw-body r2 @ [b r3] [['forin nm arr b] r3] ())))
          (: ini (? (aw-op? r ";") [() r] (aw-simple r))
             r2 (aw-want <>ini ";" "expected ; in for")
             cnd (? (aw-op? r2 ";") [() r2] (aw-expr (aw-nl r2)))
             r3 (aw-want <>cnd ";" "expected ; in for")
             stp (? (aw-op? (aw-nl r3) ")") [() (aw-nl r3)] (aw-simple (aw-nl r3)))
             r4 (aw-want <>stp ")" "expected ) after the for clauses")
             (aw-body r4 @ [b r5] [['for <ini <cnd <stp b] r5] ()))))

   ; -- the program ---------------------------------------------------------
   ; items answer as ('begin body) ('end body) ('item pat body) ('range p1 p2 body)
   (aw-params ts acc)
    (? (aw-op? ts ")") [(rev acc) >ts]
       !(aw-k? ts 'name) (aw-err "a parameter must be a name")
       (: nm <>(<ts) r (aw-nl >ts)
          (? (aw-op? r ",") (aw-params (aw-nl >r) (nm >< acc))
             (aw-params r (nm >< acc)))))
   (aw-program ts fns acc)
    (: t (aw-term ts)
       (? (aw-eof? t) (rev acc)
          ((aw-kwd? t "function") || (aw-kwd? t "func"))
           (? !((aw-k? >t 'name) || (aw-k? >t 'fname))
              (aw-err "function wants a name")
              (: nm <>(<(>t))
                 r (aw-want (aw-nl >>t) "(" "expected ( after a function name")
                 (aw-params (aw-nl r) () @ [ps r2]
                   (aw-block (aw-nl r2) @ [b r3]
                     (: _ (pin fns nm [ps b]) (aw-program r3 fns acc)) ())
                   ())))
          (aw-kwd? t "BEGIN")
           (aw-block (aw-nl >t) @ [b r] (aw-program r fns (['begin b] >< acc)) ())
          (aw-kwd? t "END")
           (aw-block (aw-nl >t) @ [b r] (aw-program r fns (['end b] >< acc)) ())
          (aw-op? t "{")
           (aw-block t @ [b r] (aw-program r fns (['item () b] >< acc)) ())
          (aw-expr t @ [p r]
            (? (aw-op? r ",")
               (aw-expr (aw-nl >r) @ [p2 r2]
                 (? (aw-op? r2 "{")
                    (aw-block r2 @ [b r3] (aw-program r3 fns (['range p p2 b] >< acc)) ())
                    (aw-program r2 fns (['range p p2 ()] >< acc)))
                 ())
               (aw-op? r "{")
                (aw-block r @ [b r2] (aw-program r2 fns (['item p b] >< acc)) ())
               (aw-program r fns (['item p ()] >< acc)))
            ())))

   ; -- the runtime ---------------------------------------------------------
   ; E carries the whole world: 'g the globals, 'loc the current frame or (),
   ; 'fn the functions, 'rec and 'flds the record and its fields, 're the regex
   ; cache, 'ports the open redirections, 'ranges the pattern ranges that are on
   (aw-new fns)
    (: E (tablet 0) g (tablet 0)
       _ (pin E 'g g) _ (pin E 'fn fns) _ (pin E 'loc ())
       _ (pin E 'flds (tablet 0)) _ (pin E 'rec "") _ (pin E 're (tablet 0))
       _ (pin E 'ports (tablet 0)) _ (pin E 'ranges (tablet 0)) _ (pin E 'ph 'begin)
       _ (pin g "FS" " ") _ (pin g "OFS" " ") _ (pin g "ORS" "\n")
       _ (pin g "NR" 0) _ (pin g "NF" 0) _ (pin g "FNR" 0)
       _ (pin g "SUBSEP" (string 28)) _ (pin g "CONVFMT" "%.6g") _ (pin g "OFMT" "%.6g")
       _ (pin g "FILENAME" "") _ (pin g "RSTART" 0) _ (pin g "RLENGTH" -1)
       E)
   (aw-g E) (peep E 'g ())
   (aw-sv E v) (aw-str v (peep (aw-g E) "CONVFMT" "%.6g"))
   (aw-pv E v) (? (string? v) v
                  (aw-nil? v) ""
                  (aw-sn? v) <>>v
                  (aw-numstr v (peep (aw-g E) "OFMT" "%.6g")))
   (aw-nrn E nm) (aw-trunc (aw-numv (peep (aw-g E) nm 0)))
   (aw-nth l k) (? (k = 0) (? (two? l) <l ()) (? (two? l) (aw-nth >l (k - 1)) ()))
   (aw-local? E nm) (: l (peep E 'loc ())
                       (? (tablet? l) (? (id? (peep l nm 'nope) 'nope) 0 1) 0))
   (aw-get E nm)
    (: l (peep E 'loc ())
       (? (tablet? l) (: v (peep l nm 'nope) (? (id? v 'nope) (peep (aw-g E) nm ()) v))
          (peep (aw-g E) nm ())))
   (aw-put E nm v)
    (? (aw-local? E nm) (: _ (pin (peep E 'loc ()) nm v) v)
       (: _ (pin (aw-g E) nm v)
          _ (? (nm = "NF") (aw-setnf E (aw-trunc (aw-numv v))) 0)
          v))
   (aw-arr E nm)
    (: v (aw-get E nm) (? (tablet? v) v (: t (tablet 0) _ (aw-put E nm t) t)))
   ; a deleted element stays in the tablet wearing 'nope -- there is no unpin, and
   ; `delete a` inside a function must empty the CALLER's array, which a fresh
   ; tablet would not do
   (aw-akeys a)
    (: (go l acc) (? !(two? l) (rev acc)
                     (? (id? (peep a <l 'nope) 'nope) (go >l acc) (go >l (<l >< acc))))
       (go (keys a) ()))
   (aw-key E subs)
    (: ss (aw-sv E (peep (aw-g E) "SUBSEP" (string 28)))
       (go l acc first) (? !(two? l) acc
                           (go >l (? first (aw-sv E <l) (acc + ss + (aw-sv E <l))) 0))
       (go subs "" 1))
   (aw-recomp E pat)
    (: c (peep E 're ())
       r (peep c pat 'none)
       (? (id? r 'none)
          (: p (reere pat)
             nds (? (two? p) <>p (aw-err ("bad regular expression: /" + pat + "/")))
             _ (pin c pat nds)
             nds)
          r))

   ; -- fields ---------------------------------------------------------------
   (aw-splitw t)                                 ; the default FS: runs of blank
    (: n (tally t)
       (go i st acc)
        (? (n <= i) (rev (? (st < 0) acc ((snip t st i) >< acc)))
           (: c (t i)
              (? (aw-blank? c) (go (i + 1) -1 (? (st < 0) acc ((snip t st i) >< acc)))
                 (go (i + 1) (? (st < 0) i st) acc))))
       (go 0 -1 ()))
   (aw-splitc t c)                               ; a single charm, literally
    (: n (tally t)
       (? (n = 0) ()
          (: (go from k acc) (? (n <= k) (rev ((snip t from k) >< acc))
                                ((t k) = c) (go (k + 1) (k + 1) ((snip t from k) >< acc))
                                (go from (k + 1) acc))
             (go 0 0 ()))))
   (aw-splitre E t pat)
    (: n (tally t) nds (aw-recomp E pat)
       (? (n = 0) ()
          (: (go i acc)
              (refind nds t i @ [_ st en _]
                (? (en <= st) (rev ((snip t i n) >< acc))     ; an empty match cannot walk
                   (go en ((snip t i st) >< acc)))
                (rev ((snip t i n) >< acc)))
             (go 0 ()))))
   (aw-fields E t fs)
    (? (fs = " ") (aw-splitw t)
       ((tally fs) = 1) (aw-splitc t (fs 0))
       (aw-splitre E t fs))
   (aw-nf E) (aw-trunc (aw-numv (peep (aw-g E) "NF" 0)))
   (aw-setrec E s)
    (: fl (tablet 0)
       fs (aw-fields E s (aw-sv E (peep (aw-g E) "FS" " ")))
       _ (pin E 'rec s) _ (pin E 'flds fl)
       (go l k) (? (two? l) (: _ (pin fl k (aw-mk <l)) (go >l (k + 1))) (k - 1))
       _ (pin (aw-g E) "NF" (go fs 1))
       s)
   (aw-rebuild E)
    (: ofs (aw-sv E (peep (aw-g E) "OFS" " "))
       fl (peep E 'flds ()) n (aw-nf E) o (jug 0)
       (go k) (? (n < k) (slurp o)
                 (: _ (? (1 < k) (say o ofs) 0)
                    _ (say o (aw-sv E (peep fl k ())))
                    (go (k + 1))))
       s (go 1)
       _ (pin E 'rec s)
       s)
   (aw-grow E fl from to)
    (: (fill j) (? (j <= to) (: _ (pin fl j (peep fl j "")) (fill (j + 1))) 0) (fill from))
   (aw-setnf E k)
    (: fl (peep E 'flds ()) n (aw-nf E)
       _ (? (n < k) (aw-grow E fl (n + 1) k) 0)
       _ (pin (aw-g E) "NF" k)
       (aw-rebuild E))
   (aw-getfld E k)
    (? (k < 0) (aw-err "a field index cannot be negative")
       (k = 0) (aw-mk (peep E 'rec ""))
       (? (k <= (aw-nf E)) (peep (peep E 'flds ()) k ()) ()))
   (aw-setfld E k v)
    (? (k < 0) (aw-err "a field index cannot be negative")
       (k = 0) (: _ (aw-setrec E (aw-sv E v)) v)
       (: fl (peep E 'flds ()) n (aw-nf E)
          _ (? (n < k) (: _ (aw-grow E fl (n + 1) k) (pin (aw-g E) "NF" k)) 0)
          _ (pin fl k v)
          _ (aw-rebuild E)
          v))

   ; -- comparison ------------------------------------------------------------
   ; ⚠ THE STRNUM RULE: two numeric-ish values compare as NUMBERS, anything else
   ; as text. this is why a field carries both faces -- "10" from input is less
   ; than "9" as text and greater as a number, and awk owes the number
   (aw-slt a b)
    (: na (tally a) nb (tally b)
       (go k) (? ((na <= k) && (nb <= k)) 0
                 (na <= k) 1
                 (nb <= k) 0
                 ((a k) < (b k)) 1
                 ((b k) < (a k)) 0
                 (go (k + 1)))
       (go 0))
   (aw-cmpv E op a b)
    (? ((aw-numeric? a) && (aw-numeric? b))
       (: x (aw-numv a) y (aw-numv b)
          (? (op = "<") (? (x < y) 1 0)
             (op = "<=") (? (y < x) 0 1)
             (op = ">") (? (y < x) 1 0)
             (op = ">=") (? (x < y) 0 1)
             (op = "==") (? (x = y) 1 0)
             (? (x = y) 0 1)))
       (: x (aw-sv E a) y (aw-sv E b)
          (? (op = "<") (? (aw-slt x y) 1 0)
             (op = "<=") (? (aw-slt y x) 0 1)
             (op = ">") (? (aw-slt y x) 1 0)
             (op = ">=") (? (aw-slt x y) 0 1)
             (op = "==") (? (x = y) 1 0)
             (? (x = y) 0 1))))

   ; -- expressions -----------------------------------------------------------
   (aw-repat E e) (? (id? <e 'ere) <>e (aw-sv E (aw-ev E e)))
   (aw-arith E op a b)
    (: x (aw-numv a) y (aw-numv b)
       (? (op = "+") (x + y)
          (op = "-") (x - y)
          (op = "*") (x * y)
          (op = "/") (? (y = 0) (aw-err "division by zero") (/ x y))
          (? (y = 0) (aw-err "division by zero in %") (x - (y * (aw-trunc (/ x y)))))))
   (aw-lget E lv)
    (: k <lv
       (? (id? k 'var) (aw-get E <>lv)
          (id? k 'field) (aw-getfld E (aw-trunc (aw-numv (aw-ev E <>lv))))
          (aw-ev E lv)))
   (aw-lput E lv v)
    (: k <lv
       (? (id? k 'var) (aw-put E <>lv v)
          (id? k 'field) (aw-setfld E (aw-trunc (aw-numv (aw-ev E <>lv))) v)
          (: a (aw-arr E <>lv)
             key (aw-key E (map (\ x (aw-ev E x)) <>>lv))
             _ (pin a key v)
             v)))
   (aw-assign E op lv rhs)
    (: v (aw-ev E rhs)
       nv (? (op = "=") v
             (: old (aw-numv (aw-lget E lv)) y (aw-numv v)
                (? (op = "+=") (old + y)
                   (op = "-=") (old - y)
                   (op = "*=") (old * y)
                   (op = "/=") (? (y = 0) (aw-err "division by zero") (/ old y))
                   (op = "%=") (? (y = 0) (aw-err "division by zero in %")
                                   (old - (y * (aw-trunc (/ old y)))))
                   (** old y))))
       (aw-lput E lv nv))
   (aw-step E op lv post)
    (: old (aw-numv (aw-lget E lv))
       nv (? (op = "++") (old + 1) (old - 1))
       _ (aw-lput E lv nv)
       (? post old nv))
   (aw-ev E e)
    (: k <e
       (? (id? k 'num) <>e
          (id? k 'str) <>e
          (id? k 'grp) (aw-ev E <>e)
          (id? k 'var) (aw-get E <>e)
          (id? k 'field) (aw-getfld E (aw-trunc (aw-numv (aw-ev E <>e))))
          ; ⚠ a bare /re/ in an expression is `$0 ~ /re/`, not a value
          (id? k 'ere) (? (rehas (aw-recomp E <>e) (aw-sv E (aw-getfld E 0))) 1 0)
          ; ⚠ MENTIONING a[i] CREATES it -- awk's rule, and `if (a[k])` then leaves
          ; k in the array for a later `in`. that is not a leak, it is the language
          (id? k 'idx)
           (: a (aw-arr E <>e)
              key (aw-key E (map (\ x (aw-ev E x)) <>>e))
              v (peep a key 'nope)
              (? (id? v 'nope) (: _ (pin a key ()) ()) v))
          (id? k 'cat) ((aw-sv E (aw-ev E <>e)) + (aw-sv E (aw-ev E <>>e)))
          (id? k 'and) (? (aw-bool (aw-ev E <>e)) (? (aw-bool (aw-ev E <>>e)) 1 0) 0)
          (id? k 'or) (? (aw-bool (aw-ev E <>e)) 1 (? (aw-bool (aw-ev E <>>e)) 1 0))
          (id? k 'not) (? (aw-bool (aw-ev E <>e)) 0 1)
          (id? k 'neg) (0 - (aw-numv (aw-ev E <>e)))
          (id? k 'pos) (aw-numv (aw-ev E <>e))
          (id? k 'pow) (** (aw-numv (aw-ev E <>e)) (aw-numv (aw-ev E <>>e)))
          (id? k 'cond) (? (aw-bool (aw-ev E <>e)) (aw-ev E <>>e) (aw-ev E <>>>e))
          (id? k 'cmp) (aw-cmpv E <>e (aw-ev E <>>e) (aw-ev E <>>>e))
          (id? k 'bin) (aw-arith E <>e (aw-ev E <>>e) (aw-ev E <>>>e))
          (id? k 'match)
           (: h (? (rehas (aw-recomp E (aw-repat E <>>>e)) (aw-sv E (aw-ev E <>>e))) 1 0)
              (? <>e (? h 0 1) h))
          (id? k 'inarr)
           (: a (aw-arr E <>>e)
              key (aw-key E (map (\ x (aw-ev E x)) <>e))
              (? (id? (peep a key 'nope) 'nope) 0 1))
          (id? k 'assign) (aw-assign E <>e <>>e <>>>e)
          (id? k 'pre) (aw-step E <>e <>>e 0)
          (id? k 'post) (aw-step E <>e <>>e 1)
          (id? k 'bi) (aw-bi E <>e <>>e)
          (id? k 'call) (aw-call E <>e <>>e)
          (id? k 'glist) (aw-err "a parenthesised list is only usable before `in'")
          (aw-err "internal: an expression shape with no evaluator")))

   ; -- the builtins ----------------------------------------------------------
   ; ⚠ a start under 1 CLAMPS and the length is not docked for it -- gnu's reading,
   ; not POSIX's: substr("hello",0,3) is "hel" there and "he" by the standard
   (aw-substr s m n has)
    (: len (tally s)
       st (? (m < 1) 1 m)
       en0 (? has (st + n) (len + 1))
       en (? ((len + 1) < en0) (len + 1) en0)
       (? (en <= st) "" (snip s (st - 1) (en - 1))))
   (aw-index s t)
    (: n (tally s) m (tally t)
       (go k) (? ((n - m) < k) 0
                 ((snip s k (k + m)) = t) (k + 1)
                 (go (k + 1)))
       (? (m = 0) (? (n = 0) 0 1) (go 0)))
   ; & in a replacement is the matched text; \& is a literal &
   (aw-repl rep matched)
    (: n (tally rep) o (jug 0)
       (go k) (? (n <= k) (slurp o)
                 (: c (rep k)
                    (? ((c = 92) && ((k + 1) < n))
                       (: d (rep (k + 1))
                          (? (d = 38) (: _ (put o 38) (go (k + 2)))
                             (d = 92) (: _ (put o 92) (go (k + 2)))
                             (: _ (put o 92) (go (k + 1)))))
                       (c = 38) (: _ (say o matched) (go (k + 1)))
                       (: _ (put o c) (go (k + 1))))))
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
   (aw-bi E nm as)
    (: (v k) (: nd (aw-nth as k) (? (aw-nil? nd) () (aw-ev E nd)))
       (s k) (aw-sv E (v k))
       (x k) (aw-numv (v k))
       cnt (tally as)
       (? (nm = "length")
           (? (cnt = 0) (tally (aw-sv E (aw-getfld E 0)))
              (: nd (aw-nth as 0)
                 cur (? (id? <nd 'var) (aw-get E <>nd) ())
                 (? (tablet? cur) (tally (aw-akeys cur)) (tally (s 0)))))
          (nm = "substr")
           (? (cnt < 2) (aw-err "substr wants two arguments")
              (aw-substr (s 0) (aw-trunc (x 1)) (aw-trunc (x 2)) (? (2 < cnt) 1 0)))
          (nm = "index") (aw-index (s 0) (s 1))
          (nm = "sprintf") (aw-sprintf (s 0) (map (\ nd (aw-ev E nd)) (? (two? as) >as ()))
                                       (peep (aw-g E) "CONVFMT" "%.6g"))
          (nm = "split")
           (? (cnt < 2) (aw-err "split wants a string and an array")
              (: nd (aw-nth as 1)
                 _ (? (id? <nd 'var) 0 (aw-err "split wants an array name"))
                 a (aw-arr E <>nd)
                 _ (: (clr l) (? (two? l) (: _ (pin a <l 'nope) (clr >l)) 0) (clr (keys a)))
                 fs (? (2 < cnt)
                       (: nd2 (aw-nth as 2)
                          (? (id? <nd2 'ere) <>nd2 (aw-sv E (aw-ev E nd2))))
                       (aw-sv E (peep (aw-g E) "FS" " ")))
                 ps (aw-fields E (s 0) fs)
                 (go l k) (? (two? l) (: _ (pin a (show k) (aw-mk <l)) (go >l (k + 1))) (k - 1))
                 (go ps 1)))
          ((nm = "sub") || (nm = "gsub"))
           (? (cnt < 2) (aw-err (nm + " wants a pattern and a replacement"))
              (: lv (? (2 < cnt) (aw-nth as 2) ['field ['num 0]])
                 _ (? (aw-lval? lv) 0 (aw-err (nm + " wants a variable to change")))
                 nds (aw-recomp E (aw-repat E (aw-nth as 0)))
                 r (aw-subst E nds (aw-sv E (aw-lget E lv)) (s 1) (? (nm = "gsub") 1 0))
                 _ (? (0 < <>r) (aw-lput E lv <r) 0)
                 <>r))
          (nm = "match")
           (: nds (aw-recomp E (aw-repat E (aw-nth as 1)))
              t (s 0)
              (refind nds t 0 @ [_ st en _]
                (: _ (pin (aw-g E) "RSTART" (st + 1))
                   _ (pin (aw-g E) "RLENGTH" (en - st))
                   (st + 1))
                (: _ (pin (aw-g E) "RSTART" 0) _ (pin (aw-g E) "RLENGTH" -1) 0)))
          (nm = "int") (aw-trunc (x 0))
          (nm = "sqrt") ((1 / 2) (x 0))
          (nm = "exp") (** e (x 0))
          (nm = "log") (log (x 0))
          (nm = "sin") (sine (x 0))
          (nm = "cos") (cosine (x 0))
          (nm = "atan2") (aw-atan2 (x 0) (x 1))
          (nm = "rand") (randf ())
          (nm = "srand") (: old (peep E 'seed 0)
                            v (? (0 < cnt) (aw-trunc (x 0)) 0)
                            _ (pin E 'seed v)
                            _ (rng-set v)
                            old)
          (nm = "tolower") (aw-down (s 0))
          (nm = "toupper") (aw-up (s 0))
          (nm = "system") (: _ (flush out) _ (flush err)
                             p (spawn ["/bin/sh" "-c" (s 0)])
                             (? (p < 0) 127 (wait p)))
          (nm = "close")
           (: ps (peep E 'ports ()) nm2 (s 0) q (peep ps nm2 'none)
              (? (id? q 'none) -1 (: _ (close q) _ (pin ps nm2 'none) 0)))
          (aw-err ("no such builtin " + nm))))
   ; atan2 off the arc tangent love carries, quadrant by quadrant
   (aw-atan2 y x)
    (: at (\ t (: s (? (t < 0) (0 - t) t)
                  a (aw-atan1 s)
                  (? (t < 0) (0 - a) a)))
       (? (0 < x) (at (/ y x))
          (x < 0) (? (y < 0) ((at (/ y x)) - pi) ((at (/ y x)) + pi))
          (0 < y) (pi / 2)
          (y < 0) (0 - (pi / 2))
          0))
   ; arctan on [0,inf) by bisecting tangent -- love carries tangent, not its inverse
   (aw-atan1 t)
    (: (go lo hi k)
        (? (k < 1) ((lo + hi) / 2)
           (: m ((lo + hi) / 2)
              (? ((tangent m) < t) (go m hi (k - 1)) (go lo m (k - 1)))))
       (go 0 (pi / 2) 60))
   (aw-call E nm as)
    (: fns (peep E 'fn ())
       f (peep fns nm 'none)
       _ (? (id? f 'none) (aw-err ("calling an undefined function " + nm)) 0)
       ps <f body <>f
       loc (tablet 0)
       ; ⚠ AN ARRAY ARGUMENT PASSES ITS TABLET, which is awk's one aliasing rule:
       ; the callee's delete and its assignments are the caller's too. a scalar
       ; is evaluated and copied, so only a name already holding an array aliases
       (bind l a)
        (? !(two? l) 0
           (: v (? (two? a)
                   (: nd <a
                      (? (id? <nd 'var)
                         (: cur (aw-get E <>nd) (? (tablet? cur) cur (aw-ev E nd)))
                         (aw-ev E nd)))
                   ())
              _ (pin loc <l v)
              (bind >l (? (two? a) >a a))))
       _ (bind ps as)
       old (peep E 'loc ())
       _ (pin E 'loc loc)
       r (trap (\ _ (: _ (aw-exec-list E body) ()))
               ()
               (\ a b (? (id? a 'awk-ret) b (: _ (pin E 'loc old) (scare a b)))))
       _ (pin E 'loc old)
       ; ⚠ an UNTYPED name handed in (g(A) with A never touched) becomes an array
       ; iff the callee subscripted it -- awk decides late, so the answer only
       ; exists now. carry the tablet back to the caller's name
       _ (: (back l a)
             (? (!(two? l) || !(two? a)) 0
                (: nd <a
                   v (peep loc <l ())
                   _ (? ((id? <nd 'var) && (tablet? v) && !(tablet? (aw-get E <>nd)))
                        (aw-put E <>nd v) 0)
                   (back >l >a)))
            (back ps as))
       r)

   ; -- statements ------------------------------------------------------------
   (aw-port E rd)
    (? (aw-nil? rd) out
       (: mode <rd
          nm (aw-sv E (aw-ev E <>rd))
          ps (peep E 'ports ())
          p (peep ps nm 'none)
          (? (id? p 'none)
             (: q (open nm (? (mode = ">>") "a" "w"))
                _ (? q 0 (aw-err ("cannot open " + nm + " for writing")))
                _ (pin ps nm q)
                q)
             p)))
   (aw-do-print E es rd)
    (: p (aw-port E rd)
       ofs (aw-sv E (peep (aw-g E) "OFS" " "))
       ors (aw-sv E (peep (aw-g E) "ORS" "\n"))
       vs (? (two? es) (map (\ x (aw-ev E x)) es) [(aw-getfld E 0)])
       (go l first) (? !(two? l) 0
                       (: _ (? first 0 (say p ofs))
                          _ (say p (aw-pv E <l))
                          (go >l 0)))
       _ (go vs 1)
       _ (say p ors)
       0)
   (aw-do-printf E es rd)
    (: p (aw-port E rd)
       f (aw-sv E (aw-ev E <es))
       vs (map (\ x (aw-ev E x)) >es)
       _ (say p (aw-sprintf f vs (peep (aw-g E) "CONVFMT" "%.6g")))
       0)
   (aw-exec-list E ss) (: (go l) (? (two? l) (: _ (aw-exec E <l) (go >l)) 0) (go ss))
   ; one turn of a loop body: 1 keep going, 0 the break came through
   (aw-iter E body)
    (trap (\ _ (: _ (aw-exec E body) 1))
          ()
          (\ a b (? (id? a 'awk-break) 0 (id? a 'awk-cont) 1 (scare a b))))
   (aw-loop E c body step)
    (? (aw-bool (aw-ev E c))
       (? (aw-iter E body)
          (: _ (? (aw-nil? step) 0 (aw-exec E step)) (aw-loop E c body step))
          0)
       0)
   (aw-forin E nm arr body)
    (: a (aw-arr E arr)
       (go l) (? !(two? l) 0
                 (: _ (aw-put E nm (aw-mk <l))
                    (? (aw-iter E body) (go >l) 0)))
       (go (aw-akeys a)))
   (aw-exec E s)
    (: k <s
       (? (id? k 'blk) (aw-exec-list E <>s)
          (id? k 'nop) 0
          (id? k 'expr) (: _ (aw-ev E <>s) 0)
          (id? k 'print) (aw-do-print E <>s <>>s)
          (id? k 'printf) (aw-do-printf E <>s <>>s)
          (id? k 'if) (? (aw-bool (aw-ev E <>s)) (aw-exec E <>>s)
                         (? (aw-nil? <>>>s) 0 (aw-exec E <>>>s)))
          (id? k 'while) (aw-loop E <>s <>>s ())
          (id? k 'do) (: (again _) (? (aw-iter E <>s)
                                      (? (aw-bool (aw-ev E <>>s)) (again 0) 0) 0)
                         (again 0))
          (id? k 'for) (: _ (? (aw-nil? <>s) 0 (aw-exec E <>s))
                           (aw-loop E (? (aw-nil? <>>s) ['num 1] <>>s)
                                    (<>(>>>s)) <>>>s))
          (id? k 'forin) (aw-forin E <>s <>>s <>>>s)
          (id? k 'next) (scare 'awk-next ())
          (id? k 'break) (scare 'awk-break ())
          (id? k 'cont) (scare 'awk-cont ())
          (id? k 'exit) (scare 'awk-exit (? (aw-nil? <>s) () (aw-trunc (aw-numv (aw-ev E <>s)))))
          (id? k 'ret) (scare 'awk-ret (? (aw-nil? <>s) () (aw-ev E <>s)))
          (id? k 'del) (: a (aw-arr E <>s)
                          key (aw-key E (map (\ x (aw-ev E x)) <>>s))
                          _ (pin a key 'nope) 0)
          (id? k 'delall) (: a (aw-arr E <>s)
                             (go l) (? (two? l) (: _ (pin a <l 'nope) (go >l)) 0)
                             (go (keys a)))
          0))

   ; -- the record loop -------------------------------------------------------
   (aw-action E body) (? (aw-nil? body) (aw-do-print E () ()) (aw-exec-list E body))
   (aw-range E it i)
    (: rs (peep E 'ranges ())
       body (<>(>>it))
       (? (peep rs i 0)
          (: _ (? (aw-bool (aw-ev E <>>it)) (pin rs i 0) 0) (aw-action E body))
          (? (aw-bool (aw-ev E <>it))
             (: _ (? (aw-bool (aw-ev E <>>it)) 0 (pin rs i 1)) (aw-action E body))
             0)))
   (aw-record E items line)
    (: _ (pin (aw-g E) "NR" ((aw-nrn E "NR") + 1))
       _ (pin (aw-g E) "FNR" ((aw-nrn E "FNR") + 1))
       _ (aw-setrec E line)
       (go l i)
        (? !(two? l) 0
           (: it <l
              _ (? (id? <it 'item)
                   (? ((aw-nil? <>it) || (aw-bool (aw-ev E <>it))) (aw-action E <>>it) 0)
                   (id? <it 'range) (aw-range E it i)
                   0)
              (go >l (i + 1))))
       (go items 0))
   (aw-feed E items line)
    (trap (\ _ (: _ (aw-record E items line) 0))
          ()
          (\ a b (? (id? a 'awk-next) 0 (scare a b))))
   (aw-cmdas? a)
    (: n (tally a)
       (go k) (? (n <= k) 0
                 ((a k) = 61) (0 < k)
                 (aw-aln? (a k)) (? ((k = 0) && (aw-dig? (a k))) 0 (go (k + 1)))
                 0)
       (go 0))
   (aw-unesc s)
    (: n (tally s) o (jug 0)
       (go k) (? (n <= k) (slurp o)
                 (: c (s k)
                    (? ((c = 92) && ((k + 1) < n))
                       (: _ (put o (aw-esc (s (k + 1)))) (go (k + 2)))
                       (: _ (put o c) (go (k + 1))))))
       (go 0))
   (aw-cmdas E a)
    (: n (tally a)
       (fnd k) (? ((a k) = 61) k (fnd (k + 1)))
       i (fnd 0)
       (aw-put E (snip a 0 i) (aw-mk (aw-unesc (snip a (i + 1) n)))))
   (aw-input E items as)
    (: (one f)
        (: t (? (f = "-") (slurp in)
                (: r (uread f) (? (two? r) <>r (aw-err ("cannot open " + f)))))
           _ (pin (aw-g E) "FILENAME" (? (f = "-") "" f))
           _ (pin (aw-g E) "FNR" 0)
           (go l) (? (two? l) (: _ (aw-feed E items <l) (go >l)) 0)
           (go (lines t)))
       (go l any)
        (? !(two? l) (? any 0 (one "-"))
           (aw-cmdas? <l) (: _ (aw-cmdas E <l) (go >l any))
           (: _ (one <l) (go >l 1)))
       (go as 0))
   (aw-phase E items which)
    (: (go l) (? !(two? l) 0
                 (: _ (? (id? <(<l) which) (aw-exec-list E <>(<l)) 0) (go >l)))
       (go items))
   (aw-needs? items)
    (: (go l) (? !(two? l) 0 (? (id? <(<l) 'begin) (go >l) 1)) (go items))

   ; -- the tool --------------------------------------------------------------
   (aw-flags a fs vs pfs)
    (? !(two? a) [fs (rev vs) (rev pfs) a]
       (: w <a n (tally w)
          glued (? (2 < n) (snip w 2 n) ())
          (? (w = "--") [fs (rev vs) (rev pfs) >a]
             (w = "-F") (? (two? >a) (aw-flags >>a <>a vs pfs) (udie 2 "awk: -F wants a separator"))
             (w = "-v") (? (two? >a) (aw-flags >>a fs (<>a >< vs) pfs) (udie 2 "awk: -v wants var=value"))
             (w = "-f") (? (two? >a) (aw-flags >>a fs vs (<>a >< pfs)) (udie 2 "awk: -f wants a file"))
             ((2 < n) && ((w 0) = 45) && ((w 1) = 70)) (aw-flags >a glued vs pfs)
             ((2 < n) && ((w 0) = 45) && ((w 1) = 118)) (aw-flags >a fs (glued >< vs) pfs)
             ((2 < n) && ((w 0) = 45) && ((w 1) = 102)) (aw-flags >a fs vs (glued >< pfs))
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
        items (trap (\ _ (aw-program (aw-lex src) fns ()))
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
