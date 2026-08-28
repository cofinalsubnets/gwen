; --- lush 🐚 the word -- expansion: segments to fields ---
; no brackets here: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-fields segs) -> the fields a word becomes, POSIX order: tilde, then
; parameter/command substitution, then IFS field splitting -- only over
; expansion output, never literal text -- then glob, then quote removal (free:
; quotes never entered the text). a field grows as a run list ((text . live?)..)
; so quoting survives to the glob (glob.l's mask law); `seen` carries the
; empty-word law out of band. parameters resolve shell vars (sh-vars) over the
; environment; the positionals and specials ($@ $* $# $? $$ $0-9) live here,
; and "$@" in quotes fields one positional each (the passthrough law).
; ${x:-y} and kin ride ('vop . (name op text)) -- text re-enters through
; sh-wordize (quote-aware) at use. ('sub . text) runs through the box sh-subeval -- eval.l
; pins the evaluator there at load (word compiles before eval exists).

(module 'lush
(: sh-subeval {0}
   (sh-ifs? b) (|| (b = 32) (b = 9) (b = 10))
   (sh-chomp t)
     (: (go k) (? ((0 < k) && ((t (k - 1)) = 10)) (go (k - 1)) (snip t 0 k))
        (go #t))
   (sh-runsub x) (sh-chomp ("" + ((sh-subeval ()) x)))

   ; --- the love lane: a form's answer, and that answer as shell text -----------
   ; lush is love, so the evaluator is aboard and nothing is spawned.
   ; (sh-loveval src) -> (1 . value) | ('err . (name . value)), cli.l's -e law:
   ; every top-level datum off the string, many folding to one application (so
   ; infix reads) and one staying as it is; 'torn is an unfinished shape. the run
   ; sits under a trap in urun's posture, so a bad form is a condition handed back
   ; and never a dead shell -- the torn case wears one too, so both render at one
   ; door. a raise is what a trap catches: a form calling `quit` still ends the
   ; process, exactly as `exit` does.
   (sh-loveval src)
     (: m (nom 0)                                 ; a fresh nom: 'torn, id?-safe
        (rdl x acc) (sound x @ 'torn       m
                               (d . rest) (rdl rest (d . acc))
                               _           (sh-rev acc))
        ; ^m, the pin: a bare `m` in a pattern would bind and take every answer.
        ; the nom is read from outside and the arm asks for that value alone.
        ; [] is the empty chain, exactly; a `()` arm would fit anything (pat.l)
        (trap (\ _ (rdl src () @ ^m ('err . ('unfinished-form . src))
                                 [] (1 . ())
                                 ds (1 . (ev 'ev (two? >ds ? ds <ds)))))
              ()
              (\ a b ('err . (a . b)))))
   (sh-lovesay c)                                 ; a love condition, on err, status 1
     (: _ (say err "lush: love: ") _ (print err <c) _ (say err " ")
        _ (print err >c) _ (put err 10) _ (flush err) 1)
   ; a love value as shell text: a string is its bytes, anything else prints in
   ; love notation -- so a string round-trips usable and a list reads back sayable
   (sh-lovetext v) (? (string? v) v (show v))
   ; ${(FORM)} at expansion: the text, or "" and a word on err. a bad form is an
   ; empty expansion, never a torn word -- the seg is already lexed by the time we
   ; are here, so there is no line to fail and the shell has to stay sayable
   (sh-lovseg src)
     (sh-loveval src @ ('err . c) (: _ (sh-lovesay c) "")
                      (_ . v)     (sh-lovetext v)
                      _            "")

   ; --- parameters: shell vars over env; specials; presence out of band --------
   ; (sh-vget nm) -> () unset | (1 . value) -- a tablet hit is a string (maybe
   ; ""), the miss the (); string? tells them apart where net cannot
   (sh-vget nm)
     (? (nm = "?") (1 . show (sh-status ()))
        (nm = "#") (1 . show #(sh-pos ()))
        (nm = "$") (1 . show (getpid 0))
        (nm = "!") (1 . show (sh-lastbg ()))
        (nm = "0") (1 . ("" + (sh-arg0 ())))
        ((nm = "@") || (nm = "*")) (1 . sh-unwords (sh-pos ()))
        (sh-digits? nm)
          (: (go l k)
               (? !(two? l) ()
                  (k = 1) (1 . <l)
                  (go >l (k - 1)))
             (go (sh-pos ()) (sh-int nm)))
        (: v (peep sh-vars nm ())
           (? (string? v) (1 . v)
              (: e (getenv nm) (? (string? e) (1 . e) ())))))
   (sh-vval nm)
     (: g (sh-vget nm)
        (? (two? g) >g
           (: _ (? (sh-uflag ())
                   (: _ (say err ("lush: " + (nm + ": unbound\n")))
                      (? (sh-tty ()) 0 (sh-leave 2)))
                   0)
              "")))
   (sh-setvar nm v)                             ; an exported name stays exported
     (? (string? (getenv nm)) (setenv nm v) (pin sh-vars nm v))

   ; --- $(( .. )): POSIX shell arithmetic --------------------------------------
   ; signed integer expressions over shell variables, with C's operators and C's
   ; precedence. one precedence-climbing walk (ar-bin asks itself for the level
   ; above) rather than a function per rung. Every step answers (value . index
   ; past) or () for "not that", so a bad expression unwinds without a raise and
   ; exactly one message is said.
   ; A NAME'S VALUE is arithmetic TEXT, not a number: `x=1+1` makes $((x*2))
   ; four. Hence the depth cap -- `a=b; b=a` would otherwise never land.
   ; and nothing here asks ? of A VALUE. love's truth is the sign, so -1 is
   ; false and `(? b ..)` would read a negative divisor as a zero one. Every test
   ; below is written against `= 0`, and that is not a style choice.
   arerr (tablet 0)                             ; a said complaint: the top stays quiet
   arlive (tablet 0)                            ; 0 inside a branch && || ?: did not take
   (ar-bad msg) (: _ (pin arerr 0 1)
                    _ (? (peep arlive 0 1) (say err ("lush: arithmetic: " + msg + "\n")) 0)
                    ())
   ; a runtime complaint -- a value only this evaluation could know is wrong. In a
   ; branch that did not take, there is no such value: `x != 0 && 100 / x` must
   ; not divide. a syntax complaint still fires there, because bash reads both
   ; sides even where it runs one.
   (ar-rt msg) (peep arlive 0 1 ? (ar-bad msg) 0)
   (ar-dead th) (: old (peep arlive 0 1) _ (pin arlive 0 0)
                   v (th ()) _ (pin arlive 0 old) v)
   (ar-ws s n i) (i < n && (s i = 32 || s i = 9 || s i = 10) ? (ar-ws s n (1 + i)) i)
   (ar-at s n i t) (: m #t ((i + m) <= n && (snip s i (i + m)) = t))
   (ar-nth l k) (? !(two? l) () (k = 0) <l (ar-nth >l (k - 1)))
   (ar-has? l t) (any (\ x (x = t)) l)
   ; --- numbers: 0x hex, leading-0 octal, else decimal
   (ar-hexv b) (? (sh-digit? b) (b - 48)
                  (97 <= b < 103) (b - 87)
                  (65 <= b < 71) (b - 55)
                  ())
   (ar-dig? b base) (: v (ar-hexv b) (charm? v && v < base))
   (ar-digs s n i base acc any?)
     (? (i < n && (ar-dig? (s i) base))
        (ar-digs s n (1 + i) base ((acc * base) + (ar-hexv (s i))) 1)
        (any? ? (acc . i) ()))
   (ar-num s n i)
     (? ((1 + i) < n && s i = 48 && (s (1 + i) = 120 || s (1 + i) = 88))
          (ar-digs s n (2 + i) 16 0 0)
        (s i = 48 && (1 + i) < n && (ar-dig? (s (1 + i)) 8))
          (ar-digs s n (1 + i) 8 0 0)
        (ar-digs s n i 10 0 0))
   (ar-name s n i)
     (? (i < n && sh-name1? (s i))
        (: (go j) (j < n && sh-name? (s j) ? (go (1 + j)) j)
           e (go i) ((snip s i e) . e))
        ())
   ; a name's value, as a number. unset or empty is 0 -- which is what makes the
   ; `n=$((n + 1))` tally work on its first pass.
   (ar-vval nm d)
     (: g (sh-vget nm) t (? (two? g) >g "")
        (? (0 = tally t) 0 (d = 0) (ar-bad "name nesting too deep") (ar-eval t (d - 1))))
   (ar-store nm v) (: _ (sh-setvar nm (show v)) v)
   ; --- the operators. love truncates // toward zero, which is C's division; >>
   ; is C's arithmetic shift and wants the floor, so it is spelled out.
   (ar-fdiv a b) (: q (a // b) (q * b = a ? q (a < 0 ? (q - 1) q)))
   (ar-apply o a b)
     (? (o = "+") (a + b)   (o = "-") (a - b)   (o = "*") (a * b)
        (o = "/") (b = 0 ? (ar-rt "division by zero") (a // b))
        (o = "%") (b = 0 ? (ar-rt "division by zero") (a - (b * (a // b))))
        (o = "<<") (b < 0 ? (ar-rt "negative shift") (a * (b 2)))
        (o = ">>") (b < 0 ? (ar-rt "negative shift") (ar-fdiv a (b 2)))
        (o = "<") (a < b ? 1 0)     (o = "<=") (a <= b ? 1 0)
        (o = ">") (a > b ? 1 0)     (o = ">=") (a >= b ? 1 0)
        (o = "==") (a = b ? 1 0)    (o = "!=") (a = b ? 0 1)
        (o = "&") (a & b) (o = "^") (a ^ b) (o = "|") (a | b)
        (o = "**") (b < 0 ? (ar-rt "negative exponent") (b a))
        (ar-bad ("unknown operator " + o)))
   ; tightest last, and the master list is longest-first so `<<` is never read as
   ; a `<` at the comparison rung.
   ; ** is the one rung that associates right (2**3**2 is 512, not 64), and the
   ; only non-POSIX operator here -- bash and ksh both have it, and leaving it out
   ; would make a working script a syntax error rather than a portable one.
   arlv '(("||") ("&&") ("|") ("^") ("&") ("==" "!=") ("<=" ">=" "<" ">")
          ("<<" ">>") ("+" "-") ("*" "/" "%") ("**"))
   arops '("**" "<<" ">>" "<=" ">=" "==" "!=" "&&" "||" "+" "-" "*" "/" "%"
           "<" ">" "&" "^" "|")
   (ar-op s n i) (foldl (\ a t (? (string? a) a (ar-at s n i t) t a)) () arops)
   ; the assignment operators, longest first. a lone = only when the next byte
   ; is not one, or `a == b` would assign.
   arasn '("<<=" ">>=" "*=" "/=" "%=" "+=" "-=" "&=" "^=" "|=")
   (ar-asnop s n i)
     (: r (foldl (\ a t (? (string? a) a (ar-at s n i t) t a)) () arasn)
        (? (string? r) (r . (i + tally r))
           (i < n && s i = 61 && !((1 + i) < n && s (1 + i) = 61)) ("=" . (1 + i))
           ()))
   ; --- the walk
   (ar-expr s n i d)
     (: i0 (ar-ws s n i)
        nv (ar-name s n i0)
        (? !(two? nv) (ar-cond s n i0 d)
           (: j (ar-ws s n >nv)
              op (ar-asnop s n j)
              (? !(two? op) (ar-cond s n i0 d)
                 (: r (ar-expr s n >op d)
                    (? !(two? r) ()
                       (<op = "=") ((ar-store <nv <r) . >r)
                       (: cur (ar-vval <nv d)
                          (? !(charm? cur) ()
                             (: v (ar-apply (snip <op 0 (#<op - 1)) cur <r)
                                (charm? v ? ((ar-store <nv v) . >r) ()))))))))))
   (ar-cond s n i d)
     (: r (ar-bin s n i 0 d)
        (? !(two? r) ()
           (: j (ar-ws s n >r)
              (? !(j < n && s j = 63) r
                 (: c !(<r = 0)
                    a (c ? (ar-expr s n (1 + j) d) (ar-dead (\ _ (ar-expr s n (1 + j) d))))
                    (? !(two? a) ()
                       (: j2 (ar-ws s n >a)
                          (? !(j2 < n && s j2 = 58) (ar-bad "? without :")
                             (: b (c ? (ar-dead (\ _ (ar-cond s n (1 + j2) d)))
                                       (ar-cond s n (1 + j2) d))
                                (? !(two? b) () ((c ? <a <b) . >b)))))))))))
   (ar-bin s n i k d)
     (? (k >= tally arlv) (ar-un s n i d)
        (: r (ar-bin s n i (1 + k) d) (? !(two? r) () (ar-more s n r k d))))
   ; one more operator at this rung, left-associating. && and || do not evaluate
   ; the side they do not need -- `x = 0 || 1 / x` is the whole reason.
   (ar-more s n r k d)
     (: j (ar-ws s n >r)
        o (ar-op s n j)
        (? !(string? o) r
           !(ar-has? (ar-nth arlv k) o) r
           (: j2 (j + tally o)
              sc (o = "&&" || o = "||")
              rt? (o = "**")                    ; the one right-associating rung
              dead? (sc && (o = "&&" ? (<r = 0) !(<r = 0)))
              nk (rt? ? k (1 + k))
              rr (dead? ? (ar-dead (\ _ (ar-bin s n j2 nk d)))
                          (ar-bin s n j2 nk d))
              (? !(two? rr) ()
                 (: v (? !sc (ar-apply o <r <rr)
                         dead? (o = "&&" ? 0 1)
                         (<rr = 0 ? 0 1))
                    (? !(charm? v) ()
                       rt? (v . >rr)           ; the RHS already took the whole tail
                       (ar-more s n (v . >rr) k d)))))))
   (ar-un s n i d)
     (: i2 (ar-ws s n i)
        (? (n <= i2) ()
           (ar-at s n i2 "++") (ar-step s n (2 + i2) 1 d)
           (ar-at s n i2 "--") (ar-step s n (2 + i2) (0 - 1) d)
           (: b (s i2)
              neg (\ f (: r (ar-un s n (1 + i2) d) (two? r ? ((f <r) . >r) ())))
              (? (b = 45) (neg (\ v (0 - v)))
                 (b = 43) (ar-un s n (1 + i2) d)
                 (b = 33) (neg (\ v (v = 0 ? 1 0)))
                 (b = 126) (neg (\ v ((0 - v) - 1)))
                 (ar-prim s n i2 d)))))
   ; ++name / --name: the new value. the postfix pair is in ar-prim, and answers
   ; the old one -- the only difference between them.
   (ar-step s n i by d)
     (: nv (ar-name s n (ar-ws s n i))
        (? !(two? nv) (ar-bad "++ wants a name")
           (: cur (ar-vval <nv d)
              (charm? cur ? ((ar-store <nv (cur + by)) . >nv) ()))))
   (ar-prim s n i d)
     (: i2 (ar-ws s n i)
        (? (n <= i2) (ar-bad "expression ends early")
           (s i2 = 40)
             (: r (ar-expr s n (1 + i2) d)
                (? !(two? r) ()
                   (: j (ar-ws s n >r)
                      (? (j < n && s j = 41) (<r . (1 + j)) (ar-bad "( without )")))))
           (sh-digit? (s i2)) (: r (ar-num s n i2) (two? r ? r (ar-bad "bad number")))
           (sh-name1? (s i2))
             (: nv (ar-name s n i2)
                j (ar-ws s n >nv)
                (? (ar-at s n j "++") (ar-post <nv 1 d (2 + j))
                   (ar-at s n j "--") (ar-post <nv (0 - 1) d (2 + j))
                   (: v (ar-vval <nv d) (charm? v ? (v . >nv) ()))))
           (ar-bad ("unexpected " + (string (s i2))))))
   (ar-post nm by d j)
     (: cur (ar-vval nm d)
        (? !(charm? cur) () (: _ (ar-store nm (cur + by)) (cur . j))))
   ; the whole text, and nothing left over
   (ar-eval src d)
     (: n #src
        r (ar-expr src n 0 d)
        (? !(two? r) () (: j (ar-ws src n >r) (j < n ? (ar-bad "trailing text") <r))))
   ; ('ari . text) -> the value as shell text. a bad expression says so and stands
   ; for nothing: "" carries the mistake into the next command, and an empty word
   ; is at least a word a `[ ]` will complain about. 0 would just be wrong quietly.
   ; the TEXT expands first (POSIX): parameter and command substitution run over
   ; it before a single operator is read, so `$(( $(wc -c < f) % 512 ))` is one
   ; expression and not a syntax error. a bare name is left alone by that pass and
   ; reaches ar-vval as itself, which is how `$((n * 2))` still means n.
   (sh-arith src)
     (: _ (pin arerr 0 0) _ (pin arlive 0 1)
        v (ar-eval (sh-voptext src) 24)
        (? (charm? v) (show v)
           (: _ (? (peep arerr 0 0) 0
                   (say err ("lush: arithmetic: " + src + ": syntax error\n")))
              "")))

   ; --- ${name<op>text}: the word text re-enters through sh-wordize ------------
   (sh-textof segs)                             ; segs -> one string (no split/glob)
     (foldl (\ a sg (: k <sg
                       (+ a (? (id? k 'lit) >sg
                               (id? k 'sq) >sg
                               (id? k 'dq) (sh-textof >sg)
                               (id? k 'var) (sh-vval >sg)
                               (id? k 'vop) (sh-vopseg sg)
                               (id? k 'sub) (sh-runsub >sg)
                               (id? k 'ari) (sh-arith >sg)
                               (id? k 'lov) (sh-lovseg >sg)
                               ""))))
            "" segs)
   (sh-voptext text) (sh-textof (sh-wordize text))
   (sh-vopseg sg) (: p >sg (sh-vop (sh-nth p 0) (sh-nth p 1) (sh-nth p 2)))
   (sh-vop nm op text)
     (? (op = "len") (show #(sh-vval nm))
        (: g (sh-vget nm)
           set? (two? g)
           v (? set? >g "")
           colon (tally op && ((op 0) = 58))
           use? (? colon (set? && tally v) set?)
           o (? colon (snip op 1 #op) op)
           (? (o = "-") (? use? v (sh-voptext text))
              (o = "=") (? use? v (: t (sh-voptext text) _ (sh-setvar nm t) t))
              (o = "?") (? use? v
                           (: _ (say err ("lush: " + (nm + (": " + (? #text text "parameter not set")))))
                              _ (put err 10)
                              (? (sh-tty ()) "" (sh-leave 2))))
              (o = "+") (? use? (sh-voptext text) "")
              (sh-vstrip v o text))))
   ; ${v#pat} ${v##pat} ${v%pat} ${v%%pat}: shortest/longest prefix/suffix strip
   (sh-patmask segs)                            ; segs -> (text . mask), quotes inert
     (foldl (\ a sg (: k <sg
                       t (? (id? k 'lit) >sg
                            (id? k 'sq) >sg
                            (id? k 'dq) (sh-textof >sg)
                            (id? k 'var) (sh-vval >sg)
                            (id? k 'vop) (sh-vopseg sg)
                            (id? k 'sub) (sh-runsub >sg)
                            (id? k 'ari) (sh-arith >sg)
                            (id? k 'lov) (sh-lovseg >sg)
                            "")
                       live (? (id? k 'sq || id? k 'dq || id? k 'lov) "0" "1")
                       ((<a + t) . (>a + (sh-mrep #t live)))))
            ("" . "") segs)
   (sh-vstrip v o text)
     (: pw (sh-patmask (sh-wordize text))
        pt <pw pm >pw n #v
        (? (o = "#")  (: (go i) (? (n < i) v
                                   (? (sh-fmatch pt pm (snip v 0 i)) (snip v i n) (go (i + 1))))
                         (go 0))
           (o = "##") (: (go i) (? (i < 0) v
                                   (? (sh-fmatch pt pm (snip v 0 i)) (snip v i n) (go (i - 1))))
                         (go n))
           (o = "%")  (: (go k) (? (n < k) v
                                   (? (sh-fmatch pt pm (snip v (n - k) n)) (snip v 0 (n - k)) (go (k + 1))))
                         (go 0))
           (o = "%%") (: (go k) (? (k < 0) v
                                   (? (sh-fmatch pt pm (snip v (n - k) n)) (snip v 0 (n - k)) (go (k - 1))))
                         (go n))
           v))

   ; --- the field machinery -----------------------------------------------------
   ; field state: (done runs seen) -- done finished fields (rev), runs the
   ; field in progress (rev run list), seen "this field exists"
   (addrun runs t a) (? #t ((t . a) . runs) runs)
   (closef done runs seen) (? seen (runs . done) done)
   (skifs t i n) (? ((i < n) && sh-ifs? (t i)) (skifs t (i + 1) n) i)
   (chunk t i n) (? ((i < n) && !(sh-ifs? (t i))) (chunk t (i + 1) n) i)
   ; split expansion output on IFS: chunks join the field, runs of IFS close it
   (spl t i n done runs seen)
     (? (i = n) [done runs seen]
        (? (sh-ifs? (t i))
           (spl t (skifs t i n) n (closef done runs seen) () 0)
           (: j (chunk t i n)
              (spl t j n done (addrun runs (snip t i j) 1) 1))))

   ; ".." with the $@ law: each positional its own field, edges glue
   (dqat inner st)
     (? !(two? inner) st
        (: sg <inner k <sg
           d (sh-nth st 0) r (sh-nth st 1)
           (? (id? k 'var && (>sg = "@"))
              (: ps (sh-pos ())
                 st2 (? !(two? ps) st
                        (foldl (\ a p (: dd (sh-nth a 0) rr (sh-nth a 1) sn2 (sh-nth a 2)
                                         (? sn2 [(rr . dd) (addrun () p 0) 1]
                                            [dd (addrun () p 0) 1])))
                               [d (addrun r (? (two? ps) <ps "") 0) 1]
                               >ps))
                 (dqat >inner st2))
              (dqat >inner
                    [d (addrun r (? (id? k 'lit) >sg
                                      (id? k 'var) (sh-vval >sg)
                                      (id? k 'vop) (sh-vopseg sg)
                                      (id? k 'sub) (sh-runsub >sg)
                                      (id? k 'ari) (sh-arith >sg)
                                      (id? k 'lov) (sh-lovseg >sg)
                                      "")
                                 0)
                       1]))))
   (wseg sg st)
     (: d (sh-nth st 0) r (sh-nth st 1) sn (sh-nth st 2)
        k <sg v >sg
        (? (id? k 'lit) [d (addrun r v 1) 1]
           (id? k 'sq)  [d (addrun r v 0) 1]
           ; ${(..)} is one field, quiet like "${x}": a love answer is a value,
           ; not a chomped byte stream, so nothing here splits it on IFS or reads
           ; it for glob chars. splitting can be bought later; unsplitting cannot.
           (id? k 'lov) [d (addrun r (sh-lovseg v) 0) 1]
           ; "": a real (seen) field even empty -- except a lone "$@" over zero
           ; positionals, which contributes nothing (dqat's glue lane sets seen)
           (id? k 'dq)  (dqat v [d r (? (two? v) sn 1)])
           (id? k 'var) (: t (sh-vval v) (spl t 0 #t d r sn))
           (id? k 'vop) (: t (sh-vopseg sg) (spl t 0 #t d r sn))
           (id? k 'sub) (: t (sh-runsub v) (spl t 0 #t d r sn))
           (id? k 'ari) (: t (sh-arith v) (spl t 0 #t d r sn))
           st))
   (wsegs segs st) (? !segs st (wsegs >segs (wseg <segs st)))

   ; ~ / ~/rest at the word's head -> $HOME, shielded (never split, never glob);
   ; the rest of the run stays live so ~/*.l still globs
   (sh-tilde segs)
     (? (&& (two? segs) (id? <<segs 'lit)
            #><segs ((><segs 0) = 126))
        (: t ><segs h ("" + (getenv "HOME"))
           (? (t = "~") (('sq . h) . >segs)
              ((t 1) = 47) (('sq . h) . (('lit . snip t 1 #t) . >segs))
              segs))
        segs)

   ; a finished field: join the runs; a live glob char sends it to sh-glob,
   ; no match keeps the word (the POSIX default)
   (sh-mrep k c) (? (k < 1) "" ((sh-mrep (k - 1) c) + c))
   (sh-field1 runs)
     (: rs (sh-rev runs)
        t (foldl (\ a r (a + <r)) "" rs)
        gb (foldl (\ a r (? a a (>r && sh-anyglob <r))) 0 rs)
        (? gb
           (: m (foldl (\ a r (a + (sh-mrep #<r (? >r "1" "0")))) "" rs)
              g (sh-glob t m)
              (? g g [t]))
           [t]))
   (sh-fields segs)
     (: st (wsegs (sh-tilde segs) [() () 0])
        fs (sh-rev (closef (sh-nth st 0) (sh-nth st 1) (sh-nth st 2)))
        (catmap sh-field1 fs)))
) ; module 'lush
