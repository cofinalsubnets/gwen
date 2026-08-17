; crew/kore/re.l -- the regex engine of kore (POSIX BRE and ERE), and grep over
; it. the engine is pure and lawed (crew/kore/law.l holds hand laws plus a
; seeded differential fuzz against an independent Brzozowski-derivative oracle);
; grep is smoked byte-identical against GNU in `make test_kore`. a toolbox file,
; no seat -- kore (crew/kore/kore.l) is its door. loads after core.l in the cat and
; rides the u-floor (uread, udie) and text.l's lines.
;
; TWO DIALECTS, ONE PARSER: BRE and ERE differ only in which sigils wear a
; backslash, so `reparse` takes the dialect as a flag and `esc?` is the whole
; seam -- ( ) | { } + ? are bare in ERE and \-escaped in BRE, while * . [ ^ $
; are bare in both. (rebre p) and (reere p) are its two doors.
;
; the surface: literals, `.`, `*`, `+`, `?`, `{n,m}` intervals, `|` alternation,
; `^` and `$` (in BRE anchors only at the head and the tail, in ERE anywhere),
; ( ) groups, and [..] classes with ranges, ^-negation, and the twelve POSIX
; [:name:] classes (a first `]` is literal, an edge `-` is literal). out of
; scope, refused by the parser in both dialects: backrefs (\1..\9).
;
; ⚠ THE RAGGED EDGES WERE READ OFF GNU, NOT GUESSED, because the differential is
; what says so: a repeat with nothing to repeat is a LITERAL in BRE (`\{2\}` is
; the text {2}) and DROPPED in ERE (`-E '*a'` is `a`, GNU warns and carries on);
; a brace that never closes and a `)` with nothing open are literals in ERE and
; malformed in BRE. an empty alternative branch is legal in both and matches the
; empty text. an inverted interval (`{3,2}`) is malformed in both.
;
; shapes: (rebre p) / (reere p) answer (1 nodes ngroups) or () on a malformed
; pattern -- uread's success shape, since the empty pattern's nodes are
; legitimately (). a node is ['chr c] | ['any] | ['cls neg ([lo hi]..)] | ['bol]
; | ['eol] | ['grp idx nodes] | ['rep atom lo hi] | ['alt (nodes..)], hi () for
; unbounded; groups number in ( order from 1. (rehas nodes s) answers the
; boolean face; (refind nodes s i) the leftmost match at-or-after i as
; (1 start end spans), spans an ([idx start end]..) with the newest pin of each
; group first -- (..)*'s \1 is the LAST iteration, like GNU; read it with assoc.
; matching is greedy backtracking in continuation style, so alternation is just
; one more node that tries its branches in order on the winning path.
(module 'kore
(:
   ; -- the twelve POSIX [:name:] classes, each written once as its ranges --
   cnames '(("alnum" (48 57) (65 90) (97 122))  ("alpha" (65 90) (97 122))
            ("blank" (9 9) (32 32))             ("cntrl" (0 31) (127 127))
            ("digit" (48 57))                   ("graph" (33 126))
            ("lower" (97 122))                  ("print" (32 126))
            ("punct" (33 47) (58 64) (91 96) (123 126))
            ("space" (9 13) (32 32))            ("upper" (65 90))
            ("xdigit" (48 57) (65 70) (97 102)))
   ; the `:` of a closing `:]` at-or-after j -> (1 j) | (). the presence wrapper,
   ; not a bare index: a `:]` can legitimately sit at 0 and 0 nets blue
   (recend p n j)
    (? (n <= (j + 1)) ()
       (p j = 58 && p (j + 1) = 93) [1 j]
       (recend p n (j + 1)))
   ; a class body at i (past [ and any ^): `first` keeps a leading ] literal.
   ; answers (items j) | (), items an ([lo hi]..). ONE scanner, both dialects --
   ; POSIX spells the bracket expression the same in BRE and ERE
   (recitems p n i first acc)
    (? (n <= i) ()
       (: c (p i)
          (? (c = 93 && !first) [acc (i + 1)]
             (c = 91 && (i + 1) < n && p (i + 1) = 58)
              (recend p n (i + 2) @ [_ e]
                (assoc (snip p (i + 2) e) cnames @ (_ >< rs)
                  (recitems p n (e + 2) 0 (cat rs acc))
                  ())
                ())
             ((i + 2) < n && p (i + 1) = 45 && !(p (i + 2) = 93))
              (recitems p n (i + 3) 0 ([c (p (i + 2))] >< acc))
             (recitems p n (i + 1) 0 ([c c] >< acc)))))

   ; -- the parser, both dialects: pattern text -> (1 nodes ngroups) | () --
   (reparse p ere?)
    (: n (tally p)
       ; THE ONE SEAM between the dialects: ( ) | { } + ? are bare in ERE and
       ; \-escaped in BRE, so they are asked for here and nowhere else. * . [ ^ $
       ; are bare in both and are read straight off the byte.
       bw (? ere? 1 2)
       (esc? i ch) (? ere? (i < n && p i = ch)
                       ((i + 1) < n && p i = 92 && p (i + 1) = ch))
       (dig? c) (48 <= c <= 57)
       (rnum i v got) (? (i < n && dig? (p i)) (rnum (i + 1) ((v * 10) + (p i - 48)) 1)
                         got [1 v i]
                         ())
       ; the interval body at i (past the opening brace) -> (lo hi j) | (), j past
       ; the close. {n} is (n n), {n,} unbounded, {n,m} both ends
       (rivl i)
        (: (clo j) (esc? j 125)
           (rnum i 0 0 @ [_ lo j]
             (? (clo j) [lo lo (j + bw)]
                (j < n && p j = 44)
                 (rnum (j + 1) 0 0 @ [_ hi j2] (? (clo j2) [lo hi (j2 + bw)] ())
                                    (? (clo (j + 1)) [lo () (j + 1 + bw)] ()))
                ())
             ()))
       ; can a repeat land? nothing repeats an anchor or an empty head
       (repable acc) (two? acc && !(member? <<acc '(bol eol)))
       ; where it cannot: BRE keeps the sigil as a literal charm, ERE drops it
       (nofit i acc lvl gi c w)
        (? ere? (pseq (i + w) acc lvl gi)
           (pseq (i + w) (['chr c] >< acc) lvl gi))
       ; a brace that never closes, and a `)` with nothing open: literal in ERE,
       ; malformed in BRE
       (loose i acc lvl gi c w)
        (? ere? (pseq (i + w) (['chr c] >< acc) lvl gi) ())
       ; one alternative: stops at the bar, the group close, or the end. acc is
       ; reversed; lvl counts open groups, gi the groups seen (their ( order
       ; numbers them). answers (nodes j gi why), why = end | close | bar
       (pseq i acc lvl gi)
        (? (n <= i) [(rev acc) i gi 'end]
           (esc? i 124) [(rev acc) (i + bw) gi 'bar]
           (esc? i 41) (? (0 < lvl) [(rev acc) (i + bw) gi 'close]
                          (loose i acc lvl gi 41 bw))
           (esc? i 40)
            (: g2 (gi + 1)
               (palt (i + bw) (lvl + 1) g2 @ [nds j g 'close]
                 (pseq j (['grp g2 nds] >< acc) lvl g)
                 ()))
           (esc? i 43) (? (repable acc)
                          (pseq (i + bw) (['rep <acc 1 ()] >< >acc) lvl gi)
                          (nofit i acc lvl gi 43 bw))
           (esc? i 63) (? (repable acc)
                          (pseq (i + bw) (['rep <acc 0 1] >< >acc) lvl gi)
                          (nofit i acc lvl gi 63 bw))
           ; ⚠ the ERE drop is of the WHOLE {n,m}, which is why rivl runs first even
           ; when nothing can be repeated: skipping just the brace leaves `2}` behind
           ; as literals, and `-E '{2}'` would match nothing where GNU matches all
           (esc? i 123)
            (rivl (i + bw) @ [lo hi j]
              (? (hi && lo > hi) ()                    ; {3,2}: malformed in both
                 (repable acc) (pseq j (['rep <acc lo hi] >< >acc) lvl gi)
                 ere? (pseq j acc lvl gi)
                 (nofit i acc lvl gi 123 bw))
              (loose i acc lvl gi 123 bw))
           (: c (p i)
              (? (c = 42) (? (repable acc)
                             (pseq (i + 1) (['rep <acc 0 ()] >< >acc) lvl gi)
                             (nofit i acc lvl gi 42 1))
                 (c = 46) (pseq (i + 1) ('(any) >< acc) lvl gi)
                 (c = 94 && (ere? || i = 0)) (pseq (i + 1) ('(bol) >< acc) lvl gi)
                 (c = 36 && (ere? || (i + 1) = n)) (pseq (i + 1) ('(eol) >< acc) lvl gi)
                 (c = 91)
                  (: j (i + 1)
                     neg ((j < n) && (p j = 94))
                     (recitems p n (? neg (j + 1) j) 1 () @ [items j2]
                       (pseq j2 (['cls (? neg 1 0) items] >< acc) lvl gi)
                       ()))
                 (c = 92)
                  (? (n <= (i + 1)) ()
                     (: d (p (i + 1))
                        (? (49 <= d <= 57) ()          ; backrefs: refused, both dialects
                           (pseq (i + 2) (['chr d] >< acc) lvl gi))))
                 (pseq (i + 1) (['chr c] >< acc) lvl gi))))
       ; the alternation above the sequence: branches joined by the bar, kept FLAT
       ; in one ['alt ..] node so a three-way reads as three branches
       (palt i lvl gi)
        (: (go j g acc)
            (pseq j () lvl g @ [nds j2 g2 why]
              (: acc2 (nds >< acc)
                 (? (why = 'bar) (go j2 g2 acc2)
                    (: brs (rev acc2)
                       [(? (two? >brs) [['alt brs]] <brs) j2 g2 why])))
              ())
           (go i gi ()))
       (palt 0 0 0 @ [nds _ g 'end] [1 nds g] ()))
   (rebre p) (reparse p ())
   (reere p) (reparse p 1)

   ; -- case folding, as an AST pass: -i never reaches the matcher --
   ; the other case of a letter, or () for anything else
   (recase c) (? (65 <= c <= 90) (c + 32) (97 <= c <= 122) (c - 32) ())
   ; the part of [lo hi] lying in [a b], shifted by d -- how a range gains its mirror
   (rovl lo hi a b d)
    (: x (? (lo < a) a lo) y (? (b < hi) b hi)
       (? (x <= y) [[(x + d) (y + d)]] ()))
   (refoldr rs)
    (catmap (\ [lo hi] (cat (rovl lo hi 97 122 -32) (rovl lo hi 65 90 32))) rs)
   ; a literal becomes the two-charm class, a class gains its mirror ranges, and
   ; everything with children recurses. the matcher is untouched by -i
   (refold nds)
    (map (\ nd
           ; ⚠ NOT (recase c @ () .. m ..): pat.l reads () as the form at zero, which
           ; asks nothing and so fits everything. presence here is a plain test
           (nd @ ['chr c]      (: m (recase c) (? m ['cls 0 [[c c] [m m]]] nd))
                 ['cls neg rs] ['cls neg (cat rs (refoldr rs))]
                 ['grp gi sub] ['grp gi (refold sub)]
                 ['alt brs]    ['alt (map refold brs)]
                 ['rep a lo hi] ['rep <(refold [a]) lo hi]
                 nd))
         nds)

   ; -- the matcher: greedy backtracking in continuation style --
   ; does charm c sit in a class of these ranges, negated or not?
   (rcls neg rs c)
    (: hit (any (\ [lo hi] (lo <= c <= hi)) rs) (? neg !hit hit))
   ; match the node list ns against s from i; sp carries the group spans won so
   ; far; k rides (end-index spans) onward on the winning path. THE NODE
   ; VOCABULARY IS THE ARM LIST -- a shape with no arm here cannot be built above
   (rem ns s i sp k)
    (ns @ (nd >< rest)
       (: n (tally s)
          (step j) (rem rest s j sp k)          ; the tail, one charm on
          (nd @ ['chr c]      (? (i < n && s i = c) (step (i + 1)) ())
                ['any]        (? (i < n) (step (i + 1)) ())
                ['cls neg rs] (? (i < n && rcls neg rs (s i)) (step (i + 1)) ())
                ['bol]        (? (i = 0) (step i) ())
                ['eol]        (? (i = n) (step i) ())
                ['grp gi sub] (rem sub s i sp
                                   (\ j sp2 (rem rest s j ([gi i j] >< sp2) k)))
                ; alternation: the branches in order on the winning path. the CPS
                ; is the whole implementation -- try one with the tail as its
                ; onward, and a branch that fails answers () so the next is asked
                ['alt brs] (: (try bs)
                               (bs @ (b >< more)
                                 (: r (rem b s i sp (\ j sp2 (rem rest s j sp2 k)))
                                    (? r r (try more)))
                                 ())
                              (try brs))
                ['rep a lo hi] (rrep a lo hi rest s i sp k 0)))
       (k i sp))
   ; the repeat: try one more (progress required), else the tail once fed.
   ; ⚠ AN EMPTY ITERATION CANNOT REPEAT -- that guard is what stops `\(\)*` spinning
   ; -- BUT IT STILL SATISFIES THE MINIMUM, all at once. without that second half
   ; `a*\+\+` demands an `a`, because the inner a* matches empty, the empty is
   ; refused, and the outer's lo=1 is never met: GNU matches every line there and
   ; we matched only the ones with an a. (the seeded fuzz found it, not a human.)
   (rrep a lo hi rest s i sp k cnt)
    (: more (? (!hi || (cnt < hi))
               (rem [a] s i sp
                    (\ j sp2 (? (j = i) (? (cnt < lo) (rem rest s j sp2 k) ())
                                 (rrep a lo hi rest s j sp2 k (cnt + 1)))))
               ())
       (? more more
          (cnt < lo) ()
          (rem rest s i sp k)))
   ; the boolean face: does s contain a match anywhere. the continuation answers
   ; (1 j), never (j): a match ending at 0 would net blue and read as absence
   (rehas ns s)
    (: n (tally s)
       (from i) (? (rem ns s i () (\ j sp [1 j]))
                   1
                   (i < n) (from (i + 1))
                   ())
       (from 0))
   ; the leftmost span at-or-after i: (1 start end spans) | () -- uread's success
   ; shape, since the empty match at 0 is the span (0 0), blue by measure. greedy
   ; end: the span the substitution tools take; spans feed \1
   ; the leftmost span at-or-after i whose (start end) PASSES p. ⚠ p rides INSIDE
   ; the continuation, which is the whole trick: a span it rejects makes `rem`
   ; answer (), and the greedy backtracker then tries the OTHER lengths at that
   ; start before the start moves on. a filter applied to refind's answer cannot
   ; do that -- it only ever sees the greedy span. -w is where it shows: `a*.` on
   ; "a._c_" is word-bounded at "a" and not at the greedy "a.", and the outside
   ; filter calls that line a miss where GNU calls it a hit
   (refindp ns s i p)
    (: n (tally s)
       (from j) (rem ns s j () (\ e sp (? (p j e) [1 e sp] ())) @ [_ e sp] [1 j e sp]
                  (? (j < n) (from (j + 1)) ()))
       (from i))
   (refind ns s i) (refindp ns s i (\ a b 1))

   ; -- grep --------------------------------------------------------------------
   ; flags ride a tablet keyed by the option charm (u.l's uopts fills it), so a
   ; new one is a row here and not another leg on a tuple every caller re-spells
   (gflag t c) (peep t c 0)
   ; a pattern text -> its node list, by the dialect and the -F/-i flags. a
   ; NEWLINE SPLITS: GNU reads one pattern per line, which is also how -e stacks
   (gnodes t txt)
    ; ⚠ THE PARSER'S SHAPE IS (1 nodes ngroups), so -F answers the same three.
    ; ⚠ AND -F WALKS BY INDEX: `two?` is false for a string, so (map f s) is () --
    ; which reads as the empty pattern and matches every line
    (: (one s) (? (gflag t 70) [1 (map (\ i ['chr (s i)]) (jot (tally s))) 0]
                  ((? (gflag t 69) reere rebre) s))
       (wrap nds) (: a (? (gflag t 120) (cat ('(bol) >< nds) '((eol))) nds)
                     (? (gflag t 105) (refold a) a))
       (go ls acc)
        (ls @ (l >< more)
           (one l @ [_ nds _] (go more ((wrap nds) >< acc)) ())
           [1 (rev acc)])
       ; ⚠ the empty pattern is ONE empty pattern, not none: `lines ""` is (), and
       ; a walk over () would answer no patterns at all -- so `grep ''`, which
       ; matches every line in GNU, would match nothing here
       ls (lines txt)
       (go (? (two? ls) ls [""]) ()))
   ; -w: the span must not sit inside a word. checked at the SPAN, not the
   ; pattern, because a word boundary is not a node this engine can hold
   (gword? c) ((48 <= c <= 57) || (65 <= c <= 90) || (97 <= c <= 122) || (c = 95))
   ; the leftmost match of any of nss in ln, word-bounded if w? -> (1 st en) | ()
   (gfind nss ln w? i)
    (: n (tally ln)
       (bnd st en) (? !w? 1
                      ((st = 0 || !(gword? (ln (st - 1)))) &&
                       (en = n || !(gword? (ln en)))))
       (best acc nds)
        (refindp nds ln i bnd @ [_ st en _]
          (? !(two? acc) [1 st en] (st < <>acc) [1 st en] acc)
          acc)
       (foldl best () nss))
   (grep-main as)
    (: t (tablet 0)
       o (uopts as t '(101 109) ())              ; -e PATTERN (stacks), -m COUNT
       _ (? (two? o) () (udie 2 "grep: bad option"))
       eps <>o rest <>>o
       ; with -e the whole tail is files; without it the first word is the pattern
       _ (? (two? eps || two? rest) ()
            (udie 2 "usage: grep [-nvclqhaixwFEo] [-e PAT] [-m N] PATTERN [FILE..]"))
       pt (? (two? eps) (ujoinc 10 eps) <rest)
       fs (? (two? eps) rest >rest)
       nss (gnodes t pt)
       _ (? (two? nss) () (udie 2 ("grep: invalid pattern: " + pt)))
       ast <>nss
       n? (gflag t 110) v? (gflag t 118) c? (gflag t 99) l? (gflag t 108)
       q? (gflag t 113) h? (gflag t 104) o? (gflag t 111) w? (gflag t 119)
       mx (: v (uoptv t 109) (? v (uatoi v) ()))
       many (!h? && (two? fs && two? >fs))
       (sel ln) (: m (? (two? (gfind ast ln w? 0)) 1 0) (? v? (? m 0 1) m))
       (pfx label) (? many (: _ (say out label) (put out 58)) ())
       ; -o: every match on the line, each on its own row. -v has nothing to show
       (emit1 label no ln)
        (: (go i)
            (gfind ast ln w? i @ [_ st en]
              ; ⚠ a ZERO-LENGTH match prints NOTHING (GNU's rule, and `-o '[a-z]*'`
              ; is how you meet it) -- but it still advances, or the walk sits still
              (: _ (? (st < en)
                      (: _ (pfx label)
                         _ (? n? (: _ (say out (show no)) (put out 58)) ())
                         _ (say out (snip ln st en)) (put out 10))
                      ())
                 (? (en < tally ln) (go (? (en = st) (en + 1) en)) ()))
              ())
           (go 0))
       ; one text through the chosen face; answers the hit flag onward
       (dof label t2 hit)
        (: ls (lines t2)
           (? q? (: (anyl xs) (xs @ (x >< more) (? (sel x) 1 (anyl more)) ())
                    (? (anyl ls) 1 hit))
              l? (: (anyl xs) (xs @ (x >< more) (? (sel x) 1 (anyl more)) ())
                    (? (anyl ls) (: _ (say out label) _ (put out 10) 1) hit))
              c? (: (cnt xs k) (? (mx && (mx <= k)) k
                                  (xs @ (x >< more) (cnt more (? (sel x) (k + 1) k)) k))
                    k (cnt ls 0)
                    _ (pfx label)
                    _ (say out (show k)) _ (put out 10)
                    (? (0 < k) 1 hit))
              (: (lp xs no h seen)
                  (? (mx && (mx <= seen)) h
                     (xs @ (x >< more)
                       (: m (sel x)
                          _ (? (m && o? && !v?) (emit1 label no x)
                               m (: _ (pfx label)
                                     _ (? n? (: _ (say out (show no)) (put out 58)) ())
                                     _ (say out x) (put out 10))
                               ())
                          (lp more (no + 1) (? m 1 h) (? m (seen + 1) seen)))
                       h))
                 (lp ls 1 hit 0))))
       ; `bad`, not `err`: err is the PORT, and a shadow here would say into a charm
       (gow a hit bad)
        (a @ (f >< more)
           (: lab (? (f = "-") "(standard input)" f)
              src (? (f = "-") [1 (slurp in)] (uread f))
              (src @ [_ txt]
                (gow more (dof lab txt hit) bad)
                (: _ (say err ("grep: " + f + ": No such file or directory"))
                   _ (put err 10)
                   (gow more hit 1))))
           (: _ (flush out) _ (flush err) (? bad 2 hit 0 1)))
       (? (two? fs) (gow fs 0 0)
          (: hit (dof "(standard input)" (slurp in) 0)
             _ (flush out)
             (? hit 0 1)))))
) ; module 'kore
