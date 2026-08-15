; crew/kore/law.l -- the kore laws, held by assertion: the myers/patience projections
; (the script spells both sides), MINIMALITY against an O(nm) LCS oracle, the u-floor and
; field/fs floors to their GNU faces, the regex floor (hand BRE laws + a seeded fuzz
; against an independent Brzozowski-derivative oracle), and a seeded fuzz over the diffs.
; reads the leaked globals from the crew/kore/ toolboxes (the `make test_kore` cat).

; --- the classic: Myers' own worked example, D = 5 ---
(: A '(a b c a b b a) B '(c b a b a c) s (myers A B)
   (assert
      (= (dold s) A) (= (dnew s) B)                         ; the script spells both sides
      (= (dcost s) 5)))                                     ; the paper's shortest path

; --- the edges: empties, equal, disjoint, a lone keep ---
(: s1 (myers () '(1 2)) s2 (myers '(1 2) ()) s3 (myers '(1 2 3) '(1 2 3)) s4 (myers '(1 2) '(3 4))
   (assert
      !(myers () ())                                        ; nothing to nothing is nothing
      (= (dnew s1) '(1 2)) !(dold s1) (= (dcost s1) 2)      ; all adds
      (= (dold s2) '(1 2)) !(dnew s2) (= (dcost s2) 2)      ; all dels
      (= (dold s3) '(1 2 3)) (= (dnew s3) '(1 2 3)) (= (dcost s3) 0)   ; equal: all keeps
      (= (dcost s4) 4)                                      ; disjoint: total replacement
      (= (myers '(1) '(1)) '((keep 1)))
      (= (dcost (myers '(a) '(b))) 2)
      (= (dcost (myers '(x a b c) '(a b c y))) 2)))         ; one del + one add around the shared run

; --- the oracle + the seeded fuzz: dcost must equal n + m - 2*lcs on random lists, and
; the projections must spell the inputs back -- minimal AND valid, every draw ---
(: (lcslen a b)
    (: ax (dindex a) bx (dindex b) n (tally a) m (tally b)
       t (tablet 0)
       (get i j) (? ((< i 1) || (< j 1)) 0 (peep t (+ (* i 4096) j) 0))
       (goj i j) (? (< m j) ()
                    (: v (? (= (ax (- i 1)) (bx (- j 1))) (+ 1 (get (- i 1) (- j 1)))
                            (: u (get (- i 1) j) w (get i (- j 1)) (? (< u w) w u)))
                       _ (pin t (+ (* i 4096) j) v)
                       (goj i (+ j 1))))
       (goi i) (? (< n i) (get n m) (: _ (goj i 1) (goi (+ i 1))))
       (goi 1))
   (rlist i st acc) (? (< i 1) (L acc st)                   ; a random list over the alphabet 1..4; answers (list state)
                       (: r (random st 4) (rlist (- i 1) >r ((+ 1 <r) >< acc))))
   (fz i st)
    (? (< i 1) 'ok
       (: r1 (random st 8) r2 (random >r1 8)
          la (rlist <r1 >r2 ())
          lb (rlist <r2 <>la ())
          a <la b <lb
          s (myers a b)
          ok (&& (= (dold s) a) (= (dnew s) b)
                 (= (dcost s) (- (+ (tally a) (tally b)) (* 2 (lcslen a b)))))
          (? ok (fz (- i 1) <>lb) (L 'broke a b))))
   r (fz 100 (wheel 7))
   (assert (= r 'ok)))

; --- patience: the same script contract, anchored -- the projections hold verbatim,
; minimality is traded (never below myers), and the unique-in-both lines land as keeps ---
(: (keeps s) (map caup (filter (\ o (= <o 'keep)) s))
   A '(a b c a b b a) B '(c b a b a c)
   s (patience A B)                                             ; no unique-both line here ...
   mv (patience '(h1 b1 b2 h2 c1 c2) '(h2 c1 c2 h1 b1 b2))      ; a moved block, all lines unique
   x (patience '(u1 u2) '(u2 u1))                               ; crossing anchors: LIS keeps one
   st (patience '("a" "b" "c") '("b" "c" "d"))                  ; string lines census structurally
   tr (patience '((1 2) (3 4)) '((3 4) (5 6)))                  ; ... and whole trees do too
   eq (patience '(1 2 3) '(1 2 3))
   (assert
      (= (dold s) A) (= (dnew s) B) (= (dcost s) 5)             ; ... so patience = the myers floor
      (= (dold mv) '(h1 b1 b2 h2 c1 c2)) (= (dnew mv) '(h2 c1 c2 h1 b1 b2))
      (= (tally (keeps mv)) 3) (= (dcost mv) 6)                 ; one block anchors whole, the other moves
      (= (dcost x) 2) (= (dold x) '(u1 u2)) (= (dnew x) '(u2 u1))
      (= (dold st) '("a" "b" "c")) (= (dnew st) '("b" "c" "d")) (= (dcost st) 2)
      (= (keeps tr) '((3 4))) (= (dcost tr) 2)
      (= (dcost eq) 0) (= (keeps eq) '(1 2 3))
      !(patience () ()) (= (dnew (patience () '(1 2))) '(1 2))
      (= (dold (patience '(1 2) ())) '(1 2))))

; --- the patience fuzz: a wider alphabet (so anchors actually occur), projections per
; draw, and the trade law dcost(patience) >= dcost(myers) held against the myers core ---
(: (rlist2 i st acc) (? (< i 1) (L acc st)
                        (: r (random st 12) (rlist2 (- i 1) >r ((+ 1 <r) >< acc))))
   (fzp i st)
    (? (< i 1) 'ok
       (: r1 (random st 11) r2 (random >r1 11)
          la (rlist2 <r1 >r2 ())
          lb (rlist2 <r2 <>la ())
          a <la b <lb
          sp (patience a b)
          ok (&& (= (dold sp) a) (= (dnew sp) b)
                 !(< (dcost sp) (dcost (myers a b))))
          (? ok (fzp (- i 1) <>lb) (L 'broke a b))))
   r (fzp 100 (wheel 11))
   (assert (= r 'ok)))

; --- lines/unlines: the text bridge -- split at 10, terminate every line, the round
; trip exact on newline-terminated texts (an unterminated tail gains its newline) ---
(: t "a\nb\n" u "a\n\nb\n"
   (assert
      (= (lines t) '("a" "b")) (= (unlines '("a" "b")) t)
      (= (unlines (lines t)) t) (= (unlines (lines u)) u)         ; round trips, empty line included
      !(lines "") !(unlines ())                                   ; nothing bridges to nothing
      (= (tally (lines "\n")) 1) (= (unlines (lines "\n")) "\n")  ; one empty line, kept
      (= (tally (lines u)) 3)
      (= (lines "a\nb") '("a" "b")) (= (unlines (lines "a\nb")) t)))   ; the one normalization

; --- the unified surface: worked hunks, exact to the byte -- context merges through a
; narrow gap, splits at a wide one, and the empty side wears the -0,0 face ---
(: at "a\nb\nc\nd\ne\nf\ng\nh\ni\n"
   d1 (udiff "old" "new" at "a\nb\nC\nd\ne\nf\ng\nH\ni\n")       ; two changes, gap 4 <= 2*3: ONE hunk
   x1 "--- old\n+++ new\n@@ -1,9 +1,9 @@\n a\n b\n-c\n+C\n d\n e\n f\n g\n-h\n+H\n i\n"
   s2 (uhunks (patience (lines at) (lines "a\nb\nC\nd\ne\nf\nG\nh\ni\n")) 1)   ; gap 3 > 2*1: TWO hunks
   x2 "@@ -2,3 +2,3 @@\n b\n-c\n+C\n d\n@@ -6,3 +6,3 @@\n f\n-g\n+G\n h\n"
   (assert
      (= d1 x1) (= s2 x2)
      !(udiff "a" "b" at at)                                      ; agreement is nothing
      (= (udiff "a" "b" "" "x\n") "--- a\n+++ b\n@@ -0,0 +1 @@\n+x\n")
      (= (udiff "a" "b" "x\n" "") "--- a\n+++ b\n@@ -1 +0,0 @@\n-x\n")))

; --- the u-floor (crew/kore/u.l): the pure helpers under the line tools, each
; held to its GNU face -- the clips, the counter, the two path clips ---
(: (assert
      (= (uatoi "42") 42) (= (uatoi "-7") -7) (= (uatoi "0") 0)
      (= (upad 7 "2") "      2") (= (upad 1 "34") "34")            ; a wider count never shrinks
      (= (ujoin '("a" "b" "c")) "a b c") (= (ujoin ()) "")
      (= (uhead 2 "a\nb\nc\n") "a\nb\n") (= (uhead 9 "a\nb\n") "a\nb\n")
      !(uhead 0 "a\n")
      (= (utail 2 "a\nb\nc\n") "b\nc\n") (= (utail 9 "a\nb\n") "a\nb\n")
      (= (ucount "a b\nc\n") '(2 3 6)) (= (ucount "") '(0 0 0))    ; (lines words bytes)
      (= (ucount "  a  b  ") '(0 2 8))                             ; a word is a maximal ink run
      (= (ubase "/a/b.txt" "") "b.txt") (= (ubase "/a/b.txt" ".txt") "b")
      (= (ubase "/a/b/" "") "b") (= (ubase "///" "") "/")
      (= (ubase "b" "b") "b")                                      ; the suffix never eats the whole name
      (= (udir "/a/b") "/a") (= (udir "a") ".") (= (udir "/") "/")
      (= (udir "a/") ".") (= (udir "/a/b/") "/a")))

; --- the field-tool floor: usplit keeps every piece (cut's face, where lines drops
; the empty tail), uspec/upick read cut's LIST, uset expands tr's ranges + escapes,
; urev walks a text backwards ---
(: (assert
      (= (usplit 58 "a:b:c") '("a" "b" "c"))
      (= (usplit 58 "a:") '("a" ""))                               ; the kept empty tail
      (= (usplit 58 "") '(""))
      (= (ujoinc 58 '("a" "" "c")) "a::c")
      (= (ujoinc 58 '("a" "b")) (+ "a" (+ ":" "b")))
      (= (uspec "1,3-5,7-,-2") '((1 1) (3 5) (7 536870911) (1 2)))
      (upick '((2 4)) 3) (upick '((2 4)) 2) (upick '((2 4)) 4)
      !(upick '((2 4)) 5) !(upick '((2 4)) 1) !(upick () 1)
      (= (uset "a-c") '(97 98 99))
      (= (uset "abc") '(97 98 99))
      (= (uset "x-") '(120 45)) (= (uset "-x") '(45 120))          ; an edge dash stays literal
      (= (uset "\\n\\t") '(10 9))                                  ; the escapes
      (= (urev "abc") "cba") !(urev "")))

; --- the fs-tool floor (crew/kore/fs.l): uoct reads chmod's octal ---
(: (assert
      (= (uoct "755") 493) (= (uoct "644") 420) (= (uoct "0") 0) (= (uoct "7") 7)))

; --- the regex floor (crew/kore/re.l): the BRE faces, held by hand ---
(: (ok p s) (: r (rebre p) (two? r && rehas <>r s))
   (no p s) (: r (rebre p) (two? r && !(rehas <>r s)))
   (bad p) !(two? (rebre p))
   (assert
      (ok "" "anything") (ok "" "")                            ; the empty pattern matches every line
      (ok "a" "bab") (no "a" "xyz")
      (ok "b.c" "abzcd") (no "b.c" "bc")                       ; . wants one charm
      (ok "^ab" "abc") (no "^b" "abc")                         ; ^ anchors the head ...
      (ok "c$" "abc") (no "b$" "abc")                          ; ... $ the tail
      (ok "^abc$" "abc") (no "^abc$" "abcd") (ok "a^b" "a^b")  ; elsewhere they are ink
      (ok "ab*c" "ac") (ok "ab*c" "abbbc") (no "ab*c" "adc")
      (ok "ab\\+c" "abc") (no "ab\\+c" "ac")                   ; the GNU \+ and \?
      (ok "ab\\?c" "ac") (ok "ab\\?c" "abc") (no "ab\\?c" "abbc")
      (ok "[abc]x" "acx") (no "[abc]" "xyz")
      (ok "[^abc]" "ab ") (ok "[a-c]" "b") (no "[a-c]" "d")
      (ok "[]a]" "]") (ok "[^]]" "a") (no "[^]]" "]")          ; a first ] is literal
      (ok "[a-]" "-") (ok "[-a]" "-")                          ; an edge - is literal
      (ok "*x" "*x") (ok "^*" "*a") (no "^*" "a*")             ; a repeat with no atom is ink
      (ok "\\+" "a+b") (no "\\+" "ab")
      (ok "a\\.b" "a.b") (no "a\\.b" "axb") (ok "a\\\\b" "a\\b")
      (ok "\\(ab\\)*c" "ababc") (ok "\\(ab\\)*c" "c") (no "x\\(ab\\)c" "xac")
      (ok "\\(a\\(b\\)*\\)*x" "abbax") (ok "\\(\\)*x" "ax")    ; nesting; an empty repeat rests
      (bad "\\(ab") (bad "ab\\)") (bad "[ab") (bad "a\\")      ; malformed answers ()
      (bad "a\\1") (bad "a\\{2\\}") (bad "a\\|b")              ; out-of-dialect refused
      (= (refind <>(rebre "b*") "abbc" 0) '(1 0 0 ()))         ; leftmost, before longest
      (= (refind <>(rebre "bb") "abbc" 0) '(1 1 3 ()))
      (= (refind <>(rebre "b*") "abbc" 1) '(1 1 3 ()))         ; greedy at its own start
      !(refind <>(rebre "zz") "abbc" 0)
      ; the group spans (\1's food): numbered in \( order, newest pin first,
      ; so a repeated group reads as its LAST iteration -- GNU's \1
      (= (refind <>(rebre "\\(b*\\)c") "abbc" 0) '(1 1 4 ((1 1 3))))
      (= (refind <>(rebre "\\(a\\(b\\)\\)") "ab" 0) '(1 0 2 ((1 0 2) (2 1 2))))
      (= (refind <>(rebre "\\(ab\\)*x") "ababx" 0) '(1 0 5 ((1 2 4) (1 0 2))))
      (= <>>(rebre "\\(a\\)\\(b\\)") 2)                        ; rebre counts its groups
      (= <>>(rebre "ab") 0)))

; --- the regex fuzz: an independent Brzozowski-derivative oracle (alternative
; SETS of node lists, no backtracking anywhere) against the engine's greedy
; backtracker, over seeded random patterns and subjects. anchors sit out (a
; derivative has no position); the hand laws above hold them instead ---
(: (dnul nd) (: t <nd
                (? (= t 'grp) (dnulq <>>nd)
                   (= t 'rep) ((< <>>nd 1) || dnul <>nd)
                   ()))
   (dnulq ns) (? (two? ns) (dnul <ns && dnulq >ns) 1)
   (dhit nd c) (: h (any (\ r ((<= <r c) && (<= c <>r))) <>>nd)
                  (? <>nd !h h))
   ; one node's derivative by charm c -> a list of alternative node lists
   (dn nd c) (: t <nd
                (? (= t 'chr) (? (= c <>nd) '(()) ())
                   (= t 'any) '(())
                   (= t 'cls) (? (dhit nd c) '(()) ())
                   (= t 'grp) (dl <>>nd c)
                   (: a <>nd lo <>>nd hi <>>>nd
                      (? (hi && (< hi 1)) ()
                         (: lo2 (? (< 0 lo) (- lo 1) 0) hi2 (? hi (- hi 1) ())
                            (map (\ u (u + (L (L 'rep a lo2 hi2)))) (dn a c)))))))
   ; a node LIST's derivative: the head's, and past a nullable head the tail's
   (dl ns c) (? !(two? ns) ()
                (: d1 (map (\ u (u + >ns)) (dn <ns c))
                   (? (dnul <ns) (d1 + (dl >ns c)) d1)))
   (dstep alts c) (: (dd xs acc) (? !(two? xs) acc
                                    (member? <xs acc) (dd >xs acc)
                                    (dd >xs (<xs >< acc)))
                     (dd (catmap (\ u (dl u c)) alts) ()))
   ; contains-match by derivatives: from every start, hit as soon as some
   ; alternative goes nullable (the prefix face rehas wears)
   (dhas ns s)
    (: n (tally s)
       (walk alts j) (? (any dnulq alts) 1
                        (<= n j) ()
                        (: a2 (dstep alts (s j))
                           (? (two? a2) (walk a2 (+ j 1)) ())))
       (from i) (? (walk (L ns) i) 1 (< i n) (from (+ i 1)) ())
       (from 0))
   ; the draws: 1..4 tokens make a pattern, 0..6 charms of abc a subject
   toks '("a" "b" "c" "." "a*" ".*" "[ab]" "[^a]" "[a-c]" "b\\+" "c\\?" "\\(ab\\)*" "\\(a\\|b\\)")
   ntok 12                                                     ; \(a\|b\) is bait: parses to () and the draw skips it
   (rpat k st acc) (? (< k 1) (L acc st)
                      (: r (random st ntok) (rpat (- k 1) >r (+ acc ((toks) <r)))))
   (rsub k st acc) (? (< k 1) (L acc st)
                      (: r (random st 3) (rsub (- k 1) >r (+ acc ((L "a" "b" "c") <r)))))
   (fzr i st)
    (? (< i 1) 'ok
       (: r1 (random st 4) r2 (random >r1 7)
          pp (rpat (+ 1 <r1) >r2 "")
          ss (rsub <r2 <>pp "")
          r (rebre <pp)
          (? !(two? r) (fzr i <>ss)                            ; an out-of-dialect draw redraws
             (: e (? (rehas <>r <ss) 1 0)
                o (? (dhas <>r <ss) 1 0)
                (? (= e o) (fzr (- i 1) <>ss)
                   (L 'broke <pp <ss))))))
   r (fzr 200 (wheel 13))
   (assert (= r 'ok)))

; --- the sed floor (crew/kore/sed.l): the substitution walk to its GNU face,
; and the script parser's shapes -- what parses, what refuses ---
(: (sub p r s g?) (: b (rebre p) <>(usub <>b r s g?))
   (assert
      (= (sub "b" "X" "abc" ()) "aXc") (= (sub "z" "X" "abc" ()) "abc")
      <(usub <>(rebre "b") "X" "abc" ())                         ; the changed bit ...
      !<(usub <>(rebre "z") "X" "abc" ())                        ; ... and its absence
      (= (sub "b" "X" "abab" 1) "aXaX")                          ; g takes every match
      (= (sub "b" "X" "abab" ()) "aXab")                         ; bare s takes the first
      (= (sub "x*" "-" "abc" 1) "-a-b-c-")                       ; the empty-match step rule
      (= (sub "x*" "-" "xbz" 1) "-b-z-")                         ; ... and the discard after a real match
      (= (sub "x*" "-" "xx" 1) "-")
      (= (sub "b" "[&]" "abc" ()) "a[b]c")                       ; & is the whole match
      (= (sub "\\(b*\\)c" "[\\1]" "xbbc" ()) "x[bb]")            ; \1 reads the group span
      (= (sub "a\\(b\\)\\?c" "<\\1>" "ac" ()) "<>")              ; an unmatched group is ""
      (= (sub "b" "\\&\\\\" "abc" ()) "a&\\c")                   ; \& and \\ stay ink
      (= (sub "^a" "X" "aaa" 1) "Xaa")                           ; the anchor can't re-fire
      (two? (sparse "p")) (two? (sparse "2,4d")) (two? (sparse "/x/,$d"))
      (two? (sparse "s/a/b/g; 3p")) (two? (sparse "2 p")) (two? (sparse "s|/|_|"))
      (= (tally <>(sparse "p;p;p")) 3)
      !(sparse "s/a") !(sparse ",")                              ; unterminated s; a legless ,
      !(sparse "2,3q") !(sparse "x") !(sparse "s/a/b/x")            ; q takes one address; unknown verb/flag
      !(sparse "s/a\\{/b/")))                                    ; the regex dialect refuses through

; --- the process floor (crew/kore/proc.l): durations to milliseconds, and
; the quote-blind word split under xargs ---
(: (assert
      (= (udur "2") 2000) (= (udur "0") 0) (= (udur "1s") 1000)
      (= (udur "1.5") 1500) (= (udur ".5") 500) (= (udur "1.") 1000)
      (= (udur "2m") 120000) (= (udur "1h") 3600000) (= (udur "1d") 86400000)
      (= (udur "1.5m") 90000)
      !(udur "") !(udur "x") !(udur "5x") !(udur "1.2.3") !(udur ".")
      (= (uwords " a  bb\tc\n") '("a" "bb" "c"))
      (= (uwords "x") '("x"))
      !(uwords "  \n") !(uwords "")))

(test_fin ())                                        ; ⚠ no zz-fin here: quit before claiming ok
(say err "crew/kore/law: myers + patience + lines/unlines + unified hunks + the u-floor + the regex floor -- projections + minimality (lcs oracle) + anchors + seeded fuzz (lcs + brzozowski oracles) ok
")
