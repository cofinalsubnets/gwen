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
      ; ⚠ A MISSING FINAL NEWLINE IS DATA, and both clips answer to it: a clip that
      ; reaches the source's last line ends as the source did, one that CUT before it
      ; ends on a line the text terminated itself.
      (nl-end? "a\n") !(nl-end? "a") !(nl-end? "")
      (= (chomp "a\n") "a") (= (chomp "a") "a") (= (chomp "") "")
      (= (uhead 9 "a\nb") "a\nb") (= (uhead 1 "a\nb") "a\n")
      (= (utail 9 "a\nb") "a\nb") (= (utail 1 "a\nb") "b")
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

; --- the calendar floor (crew/kore/core.l): civil-from-days and its inverse. exact by
; construction (no month table, no float), and the ROUND TRIP is what holds it: stat's
; %y and date both read them, and a leap-year slip would only show on one day a year ---
(: (rt z) (: c (ucivil z) (= (udays <c <>c <>>c) z))
   (assert
      (= (ucivil 0) '(1970 1 1)) (= (ucivil 1) '(1970 1 2)) (= (ucivil -1) '(1969 12 31))
      (= (ucivil 19675) '(2023 11 14)) (= (ucivil 20682) '(2026 8 17))
      (= (ucivil 11016) '(2000 2 29))                   ; a century divisible by 400 IS leap
      (= (ucivil 11017) '(2000 3 1))
      (= (ucivil -25508) '(1900 3 1))                   ; ..and 1900 is not
      (= (udays 1970 1 1) 0) (= (udays 2000 2 29) 11016) (= (udays 1900 3 1) -25508)
      (rt 0) (rt 1) (rt -1) (rt 19675) (rt 11016) (rt 20682) (rt 40000) (rt -25508)))

; --- the record floor (crew/kore/core.l): paste's delimiter list, split's suffix and
; size words, join's field split, and od's three readings ---
(: (assert
      (= (udels ":|") '(":" "|"))
      (= (udels "\\t") '("\t")) (= (udels "\\0") '(""))   ; POSIX's \0 IS the empty one
      (= (udels "") '(""))
      (= (usfx 0 2 0) "aa") (= (usfx 1 2 0) "ab") (= (usfx 26 2 0) "ba")
      (= (usfx 675 2 0) "zz") (= (usfx 0 3 0) "aaa")
      (= (usfx 0 2 1) "00") (= (usfx 9 2 1) "09") (= (usfx 10 2 1) "10")
      (= (usize "1000") 1000) (= (usize "2K") 2048) (= (usize "1M") 1048576)
      (= (ujfs () "  a   1  ") '("a" "1"))                ; blank runs, the edges dropped
      (= (ujfs () "") ()) (= (ujfs 58 "a:b:") '("a" "b" ""))
      (= (unth '("a" "b") 2) "b") !(unth '("a" "b") 3) !(unth '("a") 0)
      (= (uzpad 3 "7") "007") (= (uzpad 1 "77") "77")
      (= (uradix 255 16) "ff") (= (uradix 255 8) "377")
      (= (uodesc 10) "\\n") (= (uodesc 0) "\\0") (= (uodesc 92) "\\") (= (uodesc 65) "A")
      (= (uodesc 1) "001") (= (uodesc 255) "377") (= (uodesc 127) "177")
      (= (uodname 10) "nl") (= (uodname 32) "sp") (= (uodname 127) "del")
      (= (uodname 255) "del") (= (uodname 65) "A")        ; -ta masks the high bit
      (= (uodtype "x1") '(x 1 2)) (= (uodtype "o2") '(o 2 6)) (= (uodtype "d4") '(d 4 11))
      (= (uodtype "u1") '(u 1 3)) (= (uodtype "c") '(c 1 3)) !(uodtype "q")))

; --- the report floor (crew/kore/fs.l): stat's permission face and timestamp, du's
; three-figure human face (rounded UP, which is GNU's rule and not a rounding) ---
(: (assert
      (= (ust-perm 33188) "-rw-r--r--") (= (ust-perm 16877) "drwxr-xr-x")
      (= (ust-perm 41471) "lrwxrwxrwx") (= (ust-perm 36333) "-rwsr-sr-x")
      (= (ust-perm 33261) "-rwxr-xr-x") (= (ust-perm 17407) "drwxrwxrwt")
      (= (ust-kind 33188 10) "regular file") (= (ust-kind 33188 0) "regular empty file")
      (= (ust-kind 16877 4) "directory") (= (ust-kind 41471 2) "symbolic link")
      (= (ustamp 0) "1970-01-01 00:00:00.000000000 +0000")
      (= (ustamp 1700000000073610912) "2023-11-14 22:13:20.073610912 +0000")
      (= (uhuman 0) "0") (= (uhuman 1023) "1023")
      (= (uhuman 1024) "1.0K") (= (uhuman 4096) "4.0K") (= (uhuman 12288) "12K")
      (= (uhuman 102400) "100K") (= (uhuman 1500160) "1.5M") (= (uhuman 3000000000) "2.8G")))

; --- the expr floor (crew/kore/expr.l): every value is a TEXT, a text that reads as a
; whole number is a number where one is wanted, and the division TRUNCATES ---
(: (e as) (<(ex-parse 0 as))
   (assert
      (= (ex-num "42") '(1 42)) (= (ex-num "-7") '(1 -7)) !(ex-num "12a") !(ex-num "")
      !(ex-num "-") (= (ex-num "0") '(1 0))
      (ex-true? "1") (ex-true? "a") !(ex-true? "0") !(ex-true? "")
      (= (ex-arith "/" "-7" "2") "-3") (= (ex-arith "%" "-7" "2") "-1")   ; C's truncation
      (= (ex-arith "/" "7" "-2") "-3") (= (ex-arith "%" "7" "-2") "1")
      (= (ex-cmp "<" "2" "10") "1") (= (ex-cmp "<" "2" "10a") "0")        ; text, not number
      (= (ex-cmp "=" "abc" "abc") "1") (= (ex-cmp ">=" "3" "3") "1")
      (= (ex-substr "abcdef" "2" "3") "bcd") (= (ex-substr "abcdef" "0" "3") "")
      (= (ex-substr "abcdef" "5" "99") "ef")
      (= (ex-index "abcdef" "cd") "3") (= (ex-index "abcdef" "z") "0")
      (= (ex-match "abc" "a.c") "3") (= (ex-match "abc" "a\\(.\\)c") "b")
      (= (ex-match "abcd" "a.c") "3")                                     ; anchored at the head
      (= (ex-match "abc" "x") "0") (= (ex-match "abc" "a\\(x\\)c") "")
      (= (e '("1" "+" "2")) "3") (= (e '("1" "+" "2" "*" "3")) "7")
      (= (e '("(" "1" "+" "2" ")" "*" "3")) "9")
      (= (e '("0" "|" "3")) "3") (= (e '("0" "&" "2")) "0") (= (e '("1" "&" "2")) "1")
      (= (e '("length" "abcde")) "5") (= (e '("+" "length")) "length")))

; --- the patch floor (crew/kore/patch.l): the hunk header, the body's counts and its
; `\ No newline` mark, the -p walk, and the file<->pairs bridge ---
(: p1 "--- a/x\n+++ b/x\n@@ -1,3 +1,3 @@\n one\n-two\n+TWO\n three\n"
   secs (pt-parse (lines p1))
   sec <secs
   h (<(at sec 2))
   (assert
      (= (pt-hdr "@@ -1,8 +1,8 @@") '(1 8 1 8))
      (= (pt-hdr "@@ -3 +4,2 @@ fn()") '(3 1 4 2))                        ; a bare seat is one line
      (= (pt-hdr "@@ -0,0 +1,2 @@") '(0 0 1 2))
      !(pt-hdr "@@ nope")
      (= (pt-hname "--- a/sub/f.txt\t2026-08-17 09:46:51") "a/sub/f.txt")
      (= (pt-hname "+++ b/x") "b/x")
      (= (pt-strip "a/b/c.txt" 1) "b/c.txt") (= (pt-strip "a/b/c.txt" 2) "c.txt")
      (= (pt-strip "a/b/c.txt" ()) "c.txt")                               ; unsaid drops them all
      (= (pt-strip "c.txt" 3) "c.txt")
      (= (tally secs) 1) (= <sec "a/x") (= <>sec "b/x")
      (= h '(1 1 ((32 "one" 1) (45 "two" 1) (43 "TWO" 1) (32 "three" 1))))
      (= (pt-old h) '("one" "two" "three")) (= (pt-new h) '(("one" 1) ("TWO" 1) ("three" 1)))
      ; the `\` line marks the entry BEFORE it, and it arrives after the counts are spent
      (= (<(pt-body (lines "-c\n\\ No newline at end of file\n+C\n\\ No newline at end of file\n")
                    1 1 ()))
         '((45 "c" 0) (43 "C" 0)))
      (= (pt-render (pt-pairs "a\nb\n")) "a\nb\n")
      (= (pt-render (pt-pairs "a\nb")) "a\nb")                            ; the end is data
      (= (pt-seek (pt-pairs "x\na\nb\n") '("a" "b") 0) '(1 1))            ; found one line down
      (= (pt-seek (pt-pairs "") () -1) '(1 0))                            ; a create's -0,0
      !(pt-seek (pt-pairs "q\n") '("a") 0)))

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
      (bad "a\\1") (bad "a\\{2") (bad "a\\{3,2\\}")            ; backref, unclosed, inverted
      ; --- alternation, intervals and the POSIX classes ---
      (ok "a\\|b" "b") (ok "a\\|b" "a") (no "a\\|b" "c")       ; BRE's GNU \|
      (ok "ab\\|cd" "zcdz") (no "ab\\|cd" "acbd")              ; the branches are WHOLE
      (ok "\\(a\\|b\\)c" "bc") (no "\\(a\\|b\\)c" "cc")        ; grouped, and it still groups
      (ok "a\\|b\\|c" "c") (no "a\\|b\\|c" "d")                ; three-way, kept flat
      (ok "a\\|" "") (ok "\\|a" "z")                           ; an empty branch fits the empty text
      (ok "ab\\{2\\}c" "abbc") (no "ab\\{2\\}c" "abc")         ; {n}
      (ok "ab\\{2,\\}c" "abbbc") (no "ab\\{2,\\}c" "abc")      ; {n,}
      (ok "ab\\{1,2\\}c" "abc") (no "ab\\{1,2\\}c" "abbbc")    ; {n,m}
      (ok "\\{2\\}" "x{2}y") (ok "\\+q" "+q")                  ; nothing to repeat: BRE ink
      (ok "[[:digit:]]" "a1") (no "[[:digit:]]" "ab")
      (ok "[[:alpha:][:digit:]]" "1") (ok "[^[:digit:]]" "a")  ; two in one bracket, negated
      (ok "[[:space:]]" "a b") (ok "[[:upper:]]" "aB")
      (bad "[[:nope:]]")                                       ; an unknown class is malformed
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

; --- the ERE door: THE SAME NODES, the backslashes moved. these laws are the
; claim that one parser serves two dialects -- each pair here has a BRE twin
; above wearing different ink, and the two must agree on the tree ---
(: (eok p s) (: r (reere p) (two? r && rehas <>r s))
   (eno p s) (: r (reere p) (two? r && !(rehas <>r s)))
   (ebad p) !(two? (reere p))
   (assert
      (eok "a|b" "b") (eno "a|b" "c") (eok "(a|x)b" "xb")
      (eok "ab?c" "ac") (eok "a+" "aa") (eno "a+" "b")
      (eok "a{2}" "aa") (eno "a{2}" "ab") (eok "(a|b)+z" "abz")
      (eok "[[:digit:]]{2}" "a12") (eno "[[:digit:]]{2}" "a1b")
      (eok "*r" "r") (eok "+q" "q") (eok "{2}" "anything")     ; ERE DROPS a loose repeat
      (eok "a{" "a{") (eok "a)" "a)")                          ; ..and takes these as ink
      (ebad "a{3,2}") (ebad "(a") (ebad "a\\1")
      ; THE TWO DIALECTS AGREE ON THE TREE, which is the whole reason for one parser
      (= (reere "(a|b)+c") (rebre "\\(a\\|b\\)\\+c"))
      (= (reere "a{2,3}[[:alpha:]]") (rebre "a\\{2,3\\}[[:alpha:]]"))
      (= (reere "^(ab|c)*$") (rebre "^\\(ab\\|c\\)*$"))))

; --- the regex fuzz: an independent Brzozowski-derivative oracle (alternative
; SETS of node lists, no backtracking anywhere) against the engine's greedy
; backtracker, over seeded random patterns and subjects. anchors sit out (a
; derivative has no position); the hand laws above hold them instead ---
(: (dnul nd) (: t <nd
                (? (= t 'grp) (dnulq <>>nd)
                   (= t 'alt) (any dnulq <>nd)      ; nullable if ANY branch is
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
                   ; the derivative of an alternation is the UNION of its branches'
                   ; -- the one place the set-of-alternatives oracle costs nothing,
                   ; where the backtracker had to order them
                   (= t 'alt) (catmap (\ b (dl b c)) <>nd)
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
   ; ⚠ EVERY TOKEN HERE MUST PARSE and the oracle must know its node -- an entry
   ; the draw cannot build is dead weight, and one the oracle cannot take is a
   ; false alarm. alternation and the intervals joined when the engine grew them
   toks '("a" "b" "c" "." "a*" ".*" "[ab]" "[^a]" "[a-c]" "b\\+" "c\\?" "\\(ab\\)*"
          "\\(a\\|b\\)" "a\\|b" "\\(a\\|bc\\)*" "b\\{2\\}" "a\\{1,2\\}" "[[:alpha:]]")
   ntok 18
   (rpat k st acc) (? (< k 1) (L acc st)
                      (: r (random st ntok) (rpat (- k 1) >r (+ acc (toks <r)))))
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
   (sp s) (sparse s ())                                          ; the BRE door, the default
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
      (two? (sp "p")) (two? (sp "2,4d")) (two? (sp "/x/,$d"))
      (two? (sp "s/a/b/g; 3p")) (two? (sp "2 p")) (two? (sp "s|/|_|"))
      (= (tally <>(sp "p;p;p")) 3)
      !(sp "s/a") !(sp ",")                                      ; unterminated s; a legless ,
      !(sp "2,3q") !(sp "x") !(sp "s/a/b/x")                     ; q takes one address; unknown verb/flag
      !(sp "s/a\\{/b/")                                          ; the regex dialect refuses through
      ; ⚠ THE DIALECT REACHES THE SCRIPT, in an address and in an s alike -- the
      ; -E flag would be a lie if sparse only passed it to one of the two
      (two? (sp "s/\\(a\\|b\\)/x/")) (two? (sparse "s/(a|b)/x/" 1))
      (two? (sparse "/a|b/d" 1))
      (= (sp "s/a\\{2\\}/x/") (sparse "s/a{2}/x/" 1))            ; one tree, two inks
      (= (sp "/\\(a\\|b\\)c/p") (sparse "/(a|b)c/p" 1))))

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

; --- the find floor (crew/kore/find.l): the path join, the argument split and
; the expression grammar. the walk itself is smoked against the system find ---
(: (assert
      (= (fi-join "a" "b") "a/b") (= (fi-join "a/" "b") "a/b") (= (fi-join "/" "b") "/b")
      ; the paths run out at the first flag, bracket or bang
      (= (fi-paths '("a" "b" "-name" "x") ()) ['("a" "b") '("-name" "x")])
      (= (fi-paths '("-type" "f") ()) [() '("-type" "f")])
      (= (fi-paths '("a") ()) ['("a") ()])
      (= (fi-paths '("a" "!" "-type" "d") ()) ['("a") '("!" "-type" "d")])
      ; the depths come OUT of the word list wherever they sat
      (= (fi-depths '("-maxdepth" "2" "-print") 99 0 ()) ['("-print") 2 0])
      (= (fi-depths '("-print" "-mindepth" "1") 99 0 ()) ['("-print") 99 1])
      ; -a is implicit, and binds tighter than -o
      (= (fi-or '("-name" "a" "-o" "-name" "b" "-type" "f"))
         [['or ['name "a"] ['and ['name "b"] ['type "f"]]] ()])
      (= (fi-or '("!" "-type" "d")) [['not ['type "d"]] ()])
      (= (fi-or '("(" "-name" "a" "-o" "-name" "b" ")" "-print"))
         [['and ['or ['name "a"] ['name "b"]] ['print]] ()])
      (= (fi-or '("-exec" "echo" "{}" ";")) [['exec '("echo" "{}")] ()])
      !(fi-or '("-name"))                                ; a primary with no argument
      !(fi-or '("(" "-print"))                           ; a bracket that never closes
      !(fi-or '("-exec" "echo"))                         ; an -exec with no ;
      ; only -print and -exec ACT; anything else earns the added -print
      (fi-acts? ['print]) (fi-acts? ['and ['name "a"] ['print]])
      !(fi-acts? ['and ['name "a"] ['type "f"]]) !(fi-acts? ['prune])))

; --- the awk floor (crew/kore/awk.l): the number faces C's printf owes, the
; strnum question, and the grammar's shapes. the language is smoked against gawk ---
(: (assert
      ; %f %e %g by hand -- `show` spells a double round-trip exact, awk owes %.6g
      (= (aw-fixed 3.14159 2) "3.14") (= (aw-fixed -1.005 2) "-1.00")
      (= (aw-fixed 2.5 0) "3")                 ; ⚠ half away from zero; C says "2"
      (= (aw-expo 1234.5 3) "1.235e+03") (= (aw-expo 0.000123 2) "1.23e-04")
      (= (aw-gen (/ 1 3) 6) "0.333333") (= (aw-gen 100000 6) "100000")
      (= (aw-gen 1000000 6) "1e+06") (= (aw-gen 0.0001 6) "0.0001")
      (= (aw-gen 0.00001 6) "1e-05") (= (aw-gen 0 6) "0")
      (= (aw-numstr 42 "%.6g") "42") (= (aw-numstr 3.5 "%.6g") "3.5")
      (= (aw-sprintf "%d|%5.2f|%-4s|%x" (list 42 3.14159 "ab" 255) "%.6g") "42| 3.14|ab  |ff")
      (= (aw-sprintf "%05d|%+d|%c" (list 42 7 65) "%.6g") "00042|+7|A")
      ; the strnum question is asked of the WHOLE text, blanks aside
      (aw-looks-num? " 12.5 ") (aw-looks-num? "-3") (aw-looks-num? "1e3")
      !(aw-looks-num? "12a") !(aw-looks-num? "") !(aw-looks-num? "e3") !(aw-looks-num? "1e")
      (= (aw-s2n "  12.5xyz") 12.5) (= (aw-s2n "1e3") 1000) (= (aw-s2n "abc") 0)
      ; substr clamps its start and does NOT dock the length for it -- gnu's reading
      (= (aw-substr "hello" 2 3 1) "ell") (= (aw-substr "hello" 0 3 1) "hel")
      (= (aw-substr "hello" 4 0 0) "lo") (= (aw-substr "hello" 2 -1 1) "")
      (= (aw-index "hello" "ll") 3) (= (aw-index "hello" "z") 0)
      (= (aw-repl "[&]" "b") "[b]") (= (aw-repl "\\&" "b") "&")
      (aw-slt "ab" "b") !(aw-slt "b" "ab") !(aw-slt "a" "a") (aw-slt "a" "ab")
      ; ⚠ the lexer's one context: / after a VALUE divides, / anywhere else opens a regex
      (= (aw-lex "a/b") '((name "a") (op "/") (name "b") (eof "")))
      (= (aw-lex "/a/") '((ere "a") (eof "")))
      (= (aw-lex "f(") '((fname "f") (op "(") (eof "")))       ; a glued ( makes a call
      ; and the shapes the parser owes the evaluator
      (= (aw-program (aw-lex "{print}") (tablet 0) ()) '((item () ((print () ())))))
      (= (aw-program (aw-lex "BEGIN{x=1}") (tablet 0) ())
         '((begin ((expr (assign "=" (var "x") (num 1)))))))
      (= (aw-program (aw-lex "/a/,/b/{}") (tablet 0) ()) '((range (ere "a") (ere "b") ())))))

(test_fin ())                                        ; ⚠ no zz-fin here: quit before claiming ok
(say err "crew/kore/law: myers + patience + lines/unlines + unified hunks + the u-floor + the regex floor + the find and awk floors -- projections + minimality (lcs oracle) + anchors + seeded fuzz (lcs + brzozowski oracles) ok
")
