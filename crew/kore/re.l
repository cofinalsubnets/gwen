; crew/kore/re.l -- the regex engine of kore (a POSIX-BRE dialect), and grep
; over it. the engine is pure and lawed (crew/kore/law.l holds hand laws plus a
; seeded differential fuzz against an independent Brzozowski-derivative oracle);
; grep is smoked byte-identical against GNU in `make test_kore`. a toolbox file,
; no seat -- kore (crew/kore/kore.l) is its door. loads after core.l in the cat and
; rides the u-floor (uread, udie) and text.l's lines.
;
; the dialect: literals, `.`, `*`, `^` (anchor at the head only), `$` (anchor at
; the tail only), [..] classes with ranges and ^-negation (a first `]` is
; literal, an edge `-` is literal), \-escaped literals, \( \) groups, and the
; GNU extensions \+ and \?. like GNU, a repeat with nothing to repeat (`*` at
; the head or after `^`, and \+/\? likewise) falls back to the literal charm.
; out of scope, refused by the parser: backrefs (\1..\9) and intervals \{ \}.
; alternation \| is not in POSIX BRE and is likewise refused (as a stray \).
;
; shapes: (rebre p) answers (1 nodes ngroups) or () on a malformed pattern --
; uread's success shape, since the empty pattern's nodes are legitimately ().
; a node is ('chr c) | ('any) | ('cls neg ((lo hi)..)) | ('bol) | ('eol)
; | ('grp idx nodes) | ('rep atom lo hi), hi () for unbounded; groups number
; in \( order from 1. (rehas nodes s) answers the boolean face; (refind nodes
; s i) the leftmost match at-or-after i as (1 start end spans), spans an
; ((idx start end)..) with the newest pin of each group first -- \(..\)*'s \1
; is the LAST iteration, like GNU; read it with assoc. matching is greedy
; backtracking -- for a boolean or a leftmost span over this alternation-free
; dialect that is the whole truth.
(:
   ; -- the parser: pattern text -> (1 nodes) | () --
   (rebre p)
    (: n (tally p)
       ; a class body at i (past [ and any ^): `first` keeps a leading ] literal
       (citems i first acc)
        (? (n <= i) ()
           (: c (p i)
              (? ((c = 93) && !first) `(acc (i + 1))
                 (&& ((i + 2) < n) (p (i + 1) = 45) !(p (i + 2) = 93))
                  (citems (i + 3) 0 (`(c (p (i + 2))) >< acc))
                 (citems (i + 1) 0 (`(c c) >< acc)))))
       ; can a repeat land? nothing repeats an anchor or an empty head
       (repable acc) (two? acc && !(member? <<acc '(bol eol)))
       ; the sequence walk: acc reversed; lvl counts open groups, gi groups
       ; seen (their \( order numbers them). answers (nodes j gi) with j past
       ; the closing \) (or at the end), or () malformed
       (pseq i acc lvl gi)
        (? (n <= i) (? (0 < lvl) () `((rev acc) i gi))
           (: c (p i)
              (? (c = 42) (? (repable acc)
                             (pseq (i + 1) (`('rep <acc 0 ()) >< >acc) lvl gi)
                             (pseq (i + 1) ('(chr 42) >< acc) lvl gi))
                 (c = 46) (pseq (i + 1) ('(any) >< acc) lvl gi)
                 ((c = 94) && (i = 0)) (pseq 1 ('(bol) >< acc) lvl gi)
                 ((c = 36) && ((i + 1) = n)) (pseq (i + 1) ('(eol) >< acc) lvl gi)
                 (c = 91)
                  (: j (i + 1)
                     neg ((j < n) && (p j = 94))
                     r (citems (? neg (j + 1) j) 1 ())
                     (? (two? r)
                        (pseq <>r (`('cls (? neg 1 0) <r) >< acc) lvl gi)
                        ()))
                 (c = 92)
                  (? (n <= (i + 1)) ()
                     (: d (p (i + 1))
                        (? (d = 40)
                            (: g2 (gi + 1)
                               r (pseq (i + 2) () (lvl + 1) g2)
                               (? (two? r)
                                  (pseq <>r (`('grp g2 <r) >< acc) lvl <>>r)
                                  ()))
                           (d = 41) (? (0 < lvl) `((rev acc) (i + 2) gi) ())
                           (d = 43) (? (repable acc)
                                       (pseq (i + 2) (`('rep <acc 1 ()) >< >acc) lvl gi)
                                       (pseq (i + 2) ('(chr 43) >< acc) lvl gi))
                           (d = 63) (? (repable acc)
                                       (pseq (i + 2) (`('rep <acc 0 1) >< >acc) lvl gi)
                                       (pseq (i + 2) ('(chr 63) >< acc) lvl gi))
                           ((49 <= d) && (d <= 57)) ()
                           (|| (d = 123) (d = 125) (d = 124)) ()
                           (pseq (i + 2) (`('chr d) >< acc) lvl gi))))
                 (pseq (i + 1) (`('chr c) >< acc) lvl gi))))
       r (pseq 0 () 0 0)
       (? (two? r) `(1 <r <>>r) ()))

   ; -- the matcher: greedy backtracking in continuation style --
   ; does charm c sit in the class node?
   (rcls nd c)
    (: hit (any (\ r ((<r <= c) && (c <= <>r))) <>>nd)
       (? <>nd !hit hit))
   ; match the node list ns against s from i; sp carries the group spans won so
   ; far; k rides (end-index spans) onward on the winning path
   (rem ns s i sp k)
    (? !(two? ns) (k i sp)
       (: nd <ns rest >ns t <nd n (tally s)
          (? (t = 'chr) (? ((i < n) && (s i = <>nd)) (rem rest s (i + 1) sp k) ())
             (t = 'any) (? (i < n) (rem rest s (i + 1) sp k) ())
             (t = 'cls) (? ((i < n) && rcls nd (s i)) (rem rest s (i + 1) sp k) ())
             (t = 'bol) (? (i = 0) (rem rest s i sp k) ())
             (t = 'eol) (? (i = n) (rem rest s i sp k) ())
             (t = 'grp) (: gi <>nd
                           (rem <>>nd s i sp
                                (\ j sp2 (rem rest s j (`(gi i j) >< sp2) k))))
             (: a <>nd b >>nd
                (rrep a <b <>b rest s i sp k 0)))))
   ; the repeat: try one more (progress required), else the tail once fed
   (rrep a lo hi rest s i sp k cnt)
    (: more (? (!hi || (cnt < hi))
               (rem `(a) s i sp
                    (\ j sp2 (? (j = i) () (rrep a lo hi rest s j sp2 k (cnt + 1)))))
               ())
       (? more more
          (cnt < lo) ()
          (rem rest s i sp k)))
   ; the boolean face: does s contain a match anywhere. the continuation answers
   ; (1 j), never (j): a match ending at 0 would net blue and read as absence
   (rehas ns s)
    (: n (tally s)
       (from i) (? (rem ns s i () (\ j sp `(1 j)))
                   1
                   (i < n) (from (i + 1))
                   ())
       (from 0))
   ; the leftmost span at-or-after i: (1 start end spans) | () -- uread's
   ; success shape, since the empty match at 0 is the span (0 0), blue by
   ; measure. greedy end: the span the substitution tools take; spans feed \1
   (refind ns s i)
    (: n (tally s)
       (from j) (: r (rem ns s j () (\ e sp `(1 e sp)))
                   (? (two? r) `(1 j <>r <>>r)
                      (j < n) (from (j + 1))
                      ()))
       (from i))

   ; -- grep: the lines that match [-n number, -v invert, -c count, -l names] --
   (grep-main as)
    (: (fl a n? v? c? l?) (? !(two? a) `(n? v? c? l? a)
                             (<a = "-n") (fl >a 1 v? c? l?)
                             (<a = "-v") (fl >a n? 1 c? l?)
                             (<a = "-c") (fl >a n? v? 1 l?)
                             (<a = "-l") (fl >a n? v? c? 1)
                             `(n? v? c? l? a))
       p (fl as 0 0 0 0)
       n? <p v? <>p
       p >>p
       c? <p l? <>p rest <>>p
       _ (? (two? rest) () (udie 2 "usage: grep [-n] [-v] [-c] [-l] PATTERN [FILE..]"))
       r (rebre <rest)
       _ (? (two? r) () (udie 2 ("grep: invalid pattern: " + <rest)))
       ast <>r
       fs >rest
       many (two? fs && two? >fs)
       (sel ln) (: m (rehas ast ln) (? v? !m m))
       ; one text through the chosen face; answers the hit flag onward
       (dof label t hit)
        (: ls (lines t)
           (? l? (: (anyl xs) (? !(two? xs) () (sel <xs) 1 (anyl >xs))
                    (? (anyl ls) (: _ (say out label) _ (put out 10) 1) hit))
              c? (: (cnt xs k) (? (two? xs) (cnt >xs (? (sel <xs) (k + 1) k)) k)
                    k (cnt ls 0)
                    _ (? many (: _ (say out label) (put out 58)) ())
                    _ (say out (show k)) _ (put out 10)
                    (? (0 < k) 1 hit))
              (: (lp xs no h)
                  (? !(two? xs) h
                     (: m (sel <xs)
                        _ (? m (: _ (? many (: _ (say out label) (put out 58)) ())
                                  _ (? n? (: _ (say out (show no)) (put out 58)) ())
                                  _ (say out <xs) (put out 10))
                               ())
                        (lp >xs (no + 1) (? m 1 h))))
                 (lp ls 1 hit))))
       ; `bad`, not `err`: err is the PORT, and a shadow here would say into a charm
       (gow a hit bad)
        (? (two? a)
           (: f <a
              lab (? (f = "-") "(standard input)" f)
              r2 (? (f = "-") `(1 (slurp in)) (uread f))
              (? (two? r2) (gow >a (dof lab <>r2 hit) bad)
                 (: _ (say err ("grep: " + f + ": No such file or directory"))
                    _ (put err 10)
                    (gow >a hit 1))))
           (: _ (flush out) _ (flush err) (quit (? bad 2 hit 0 1))))
       (? (two? fs) (gow fs 0 0)
          (: hit (dof "(standard input)" (slurp in) 0)
             _ (flush out)
             (quit (? hit 0 1))))))
