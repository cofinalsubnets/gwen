; crew/vi/law.l -- the editor's laws, held by assertion: every key sequence
; below drives the PURE engine (vstep, one byte at a time) and reads the
; tablet back -- no tty, no port, no file. the cat (`make test_vi`):
;
;   cat test/00-init.l crew/kore/text.l crew/kore/core.l crew/kore/re.l \
;       crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/law.l | out/host/love
;
; (vgo lines keys) births a buffer, feeds the TEXT's bytes (27 spelled \e via
; the 27-element list shape), and answers the state; the helpers read it back.
(: (bytes s) (: n (tally s) (go i acc) (? (< i 0) acc (go (- i 1) (link (s i) acc))) (go (- n 1) ()))
   (vgo ls ks) (vfeed (vnew ls "t") (bytes ks))
   (vgob ls bs) (vfeed (vnew ls "t") bs)
   BS 127 CR 13
   (lns st) (st 'lines) (at st) (L (st 'row) (st 'col))
   (st? st k) (st k)

; --- motion: hjkl, 0 ^ $, counts, the goal column, G/gg ---
   _ (assert
      (= (at (vgo '("abc" "de") "ll")) '(0 2))
      (= (at (vgo '("abc" "de") "lll")) '(0 2))                ; the col fence
      (= (at (vgo '("abc" "de") "llh")) '(0 1))
      (= (at (vgo '("abc" "de") "j")) '(1 0))
      (= (at (vgo '("abc" "de") "jj")) '(1 0))                 ; the row fence
      (= (at (vgo '("abc" "de") "jk")) '(0 0))
      (= (at (vgo '("abc") "$")) '(0 2))
      (= (at (vgo '("abc") "$0")) '(0 0))
      (= (at (vgo '("  xy") "^")) '(0 2))
      (= (at (vgo '("abcdef") "3l")) '(0 3))
      (= (at (vgo '("abcdef" "ab") "$j")) '(1 1))              ; clamp on the short line ...
      (= (at (vgo '("abcdef" "ab" "abcdef") "$jj")) '(2 5))    ; ... but the GOAL survives
      (= (at (vgo '("a" "b" "c") "G")) '(2 0))
      (= (at (vgo '("a" "b" "c") "2G")) '(1 0))
      (= (at (vgo '("a" "b" "c") "Ggg")) '(0 0))
      (= (at (vgo '("a" "b" "c") "3gg")) '(2 0)))

; --- words: w e b across lines ---
   _ (assert
      (= (at (vgo '("foo bar baz") "w")) '(0 4))
      (= (at (vgo '("foo bar baz") "2w")) '(0 8))
      (= (at (vgo '("foo.bar") "w")) '(0 3))                   ; punct is its own word
      (= (at (vgo '("foo" "bar") "w")) '(1 0))                 ; w crosses the line
      (= (at (vgo '("foo bar") "e")) '(0 2))
      (= (at (vgo '("foo bar") "ee")) '(0 6))
      (= (at (vgo '("foo bar") "$b")) '(0 4))
      (= (at (vgo '("foo bar") "$bb")) '(0 0))
      (= (at (vgo '("foo" "bar") "jb")) '(0 0)))               ; b crosses back

; --- f F t T ---
   _ (assert
      (= (at (vgo '("abcabc") "fc")) '(0 2))
      (= (at (vgo '("abcabc") "2fc")) '(0 5))
      (= (at (vgo '("abcabc") "tc")) '(0 1))
      (= (at (vgo '("abcabc") "$Fa")) '(0 3))
      (= (at (vgo '("abcabc") "$Ta")) '(0 4))
      (= (at (vgo '("abc") "fz")) '(0 0)))                     ; a miss stands still

; --- insert: i a I A o O, enter, backspace, escape ---
   _ (assert
      (= (lns (vgo '("bc") "iaX")) '("aXbc"))                  ; i, then a and X insert
      (= (lns (vgob '("ac") (L 105 98 27 88))) '("bac"))       ; i b ESC, then X at col 0 bites nothing
      (= (lns (vgo '("") "ihello")) '("hello"))
      (= (lns (vgo '("xy") "aZ")) '("xZy"))
      (= (lns (vgo '("xy") "AZ")) '("xyZ"))
      (= (lns (vgo '("  xy") "IZ")) '("  Zxy"))
      (= (lns (vgo '("ab") "ocd")) '("ab" "cd"))
      (= (at (vgo '("ab") "ocd")) '(1 2))
      (= (lns (vgo '("ab") "Ocd")) '("cd" "ab"))
      (= (lns (vgo '("ab" "cd") "jOx")) '("ab" "x" "cd")))
   _ (assert
      (= (lns (vgob '("abcd") (L 108 108 105 13))) '("ab" "cd"))          ; ll i CR splits
      (= (at (vgob '("abcd") (L 108 108 105 13))) '(1 0))
      (= (lns (vgob '("abcd") (L 65 120 BS BS 121))) '("abcy"))           ; A x bsp bsp y
      (= (lns (vgob '("ab" "cd") (L 106 105 BS))) '("abcd"))              ; bsp at col 0 joins up
      (= (at (vgob '("ab" "cd") (L 106 105 BS))) '(0 2))
      (= (at (vgob '("abc") (L 65 27))) '(0 2))                           ; esc steps left, fenced
      (= (st? (vgob '("a") (L 105 120 27)) 'mode) 'n))

; --- operators: x X D C s r J, dd yy cc, d+motion, spans ---
   _ (assert
      (= (lns (vgo '("abc") "x")) '("bc"))
      (= (lns (vgo '("abc") "2x")) '("c"))
      (= (lns (vgo '("abc") "lx")) '("ac"))
      (= (lns (vgo '("abc") "$X")) '("ac"))
      (= (lns (vgo '("abcdef") "llD")) '("ab"))
      (= (at (vgo '("abcdef") "llD")) '(0 1))
      (= (lns (vgo '("abc" "def") "dd")) '("def"))
      (= (lns (vgo '("abc" "def") "jdd")) '("abc"))
      (= (lns (vgo '("a" "b" "c") "2dd")) '("c"))
      (= (lns (vgo '("only") "dd")) '(""))                     ; the vi invariant: one line stays
      (= (lns (vgo '("foo bar") "dw")) '("bar"))
      (= (lns (vgo '("foo bar") "d$")) '(""))
      (= (lns (vgo '("foo bar") "wdb")) '("bar"))
      (= (lns (vgo '("abcabc") "dfb")) '("cabc"))
      (= (lns (vgo '("abcabc") "dtb")) '("bcabc"))
      (= (lns (vgo '("a" "b" "c") "dj")) '("c"))               ; d+linewise takes both rows
      (= (lns (vgo '("a" "b" "c") "jdk")) '("c"))
      (= (lns (vgo '("a" "b" "c") "dG")) '(""))
      (= (lns (vgo '("abc") "rz")) '("zbc"))
      (= (lns (vgo '("abcd") "2rz")) '("zzcd"))
      (= (at (vgo '("abcd") "2rz")) '(0 1))
      (= (lns (vgo '("ab" "  cd") "J")) '("ab cd"))
      (= (at (vgo '("ab" "  cd") "J")) '(0 2))
      (= (lns (vgo '("a" "b" "c") "3J")) '("a b c")))
   _ (assert
      (= (lns (vgo '("foo bar") "cwxy")) '("xy bar"))          ; c enters insert
      (= (st? (vgo '("foo bar") "cw") 'mode) 'i)
      (= (lns (vgo '("abc") "sZ")) '("Zbc"))
      (= (lns (vgo '("abcdef") "llCxy")) '("abxy"))
      (= (lns (vgo '("aa" "bb") "ccX")) '("X" "bb")))          ; cc keeps a fresh line

; --- yank + paste, both kinds ---
   _ (assert
      (= (lns (vgo '("ab" "cd") "yyp")) '("ab" "ab" "cd"))
      (= (at (vgo '("ab" "cd") "yyp")) '(1 0))
      (= (lns (vgo '("ab" "cd") "yyP")) '("ab" "ab" "cd"))
      (= (at (vgo '("ab" "cd") "yyP")) '(0 0))
      (= (lns (vgo '("ab" "cd") "ddp")) '("cd" "ab"))          ; the line swap idiom
      (= (lns (vgo '("abc") "ylp")) '("aabc"))                 ; charwise p lands past the cursor
      (= (lns (vgo '("foo bar") "ywP")) '("foo foo bar"))
      (= (lns (vgo '("ab" "cd") "2yyjp")) '("ab" "cd" "ab" "cd")))

; --- undo / redo ---
   _ (assert
      (= (lns (vgo '("abc") "xu")) '("abc"))
      (= (lns (vgob '("abc") (L 105 122 27 117))) '("abc"))    ; the whole insert, one undo
      (= (lns (vgob '("abc") (L 120 117 18))) '("bc"))        ; x u ^R
      (= (lns (vgo '("a" "b") "dduu")) '("a" "b"))
      (= (at (vgo '("abc") "lxu")) '(0 1)))

; --- search over re.l: / ? n N, wrap ---
   (skeys s) (cat (bytes "/") (cat (bytes s) (L CR)))
   _ (assert
      (= (at (vgob '("foo" "bar" "foobar") (skeys "bar"))) '(1 0))
      (= (at (vgob '("foo" "bar" "foobar") (cat (skeys "bar") (bytes "n")))) '(2 3))
      (= (at (vgob '("foo" "bar" "foobar") (cat (skeys "bar") (bytes "nn")))) '(1 0))   ; wraps
      (= (at (vgob '("foo" "bar" "foobar") (cat (skeys "bar") (bytes "nN")))) '(1 0))
      (= (at (vgob '("ab" "cd") (cat (bytes "j$?ab") (L CR)))) '(0 0))
      (= (at (vgob '("xax" "b") (skeys "q"))) '(0 0)))         ; a miss stands still

; --- the ex line: :N, :$, :q with dirt, :q!, :w's request ---
   _ (assert
      (= (at (vgob '("a" "b" "c") (cat (bytes ":3") (L CR)))) '(2 0))
      (= (at (vgob '("a" "b" "c") (cat (bytes ":$") (L CR)))) '(2 0))
      (= (st? (vgob '("a") (cat (bytes ":q") (L CR))) 'quit) 1)
      (! (st? (vgob '("a") (cat (bytes "x:q") (L CR))) 'quit)) ; dirty q refuses
      (= (st? (vgob '("a") (cat (bytes "x:q!") (L CR))) 'quit) 1)
      (= (st? (vgob '("a") (cat (bytes ":w") (L CR))) 'dow) '("t"))
      (= (st? (vgob '("a") (cat (bytes ":w other") (L CR))) 'dow) '("other"))
      (: q (vgob '("a") (cat (bytes "x:wq") (L CR)))
         (&& (= (q 'dow) '("t")) (= (q 'quit) 1)))
      (: q (vgob '("a") (bytes "xZZ"))
         (&& (= (q 'dow) '("t")) (= (q 'quit) 1))))

; --- the frame: escapes exact on a tiny screen ---
   (mini ls ks) (: st (vgo ls ks) _ (pin st 'w 8) _ (pin st 'h 4) (vframe st))
   E$ (+ "" 27)
   _ (assert
      (= (mini '("ab") "")
         (+ E$ (+ "[H" (+ "ab" (+ E$ (+ "[K\r\n" (+ "~" (+ E$ (+ "[K\r\n" (+ "~" (+ E$ (+ "[K\r\n"
            (+ "t  1,1" (+ E$ (+ "[K" (+ E$ "[1;1H"))))))))))))))))
      ; a long line clips at w; the cursor never leaves the view
      (= (mini '("abcdefghij") "$")
         (+ E$ (+ "[H" (+ "abcdefgh" (+ E$ (+ "[K\r\n" (+ "~" (+ E$ (+ "[K\r\n" (+ "~" (+ E$ (+ "[K\r\n"
            (+ "t  1,10" (+ E$ (+ "[K" (+ E$ "[1;8H"))))))))))))))))
      ; scrolling: the cursor's row stays inside the 3 text rows
      (: st (vgo '("a" "b" "c" "d" "e") "G") _ (pin st 'w 8) _ (pin st 'h 4)
         f (vframe st)
         (&& (= (st 'top) 2) (two? (L f)))))

; --- :e and :o -- the request the face performs, and the buffer swap it calls -
   _ (assert
      (= (st? (vgob '("a") (cat (bytes ":e other") (L CR))) 'doe) '("other"))
      (= (st? (vgob '("a") (cat (bytes ":o other") (L CR))) 'doe) '("other"))   ; the same door
      (= (st? (vgob '("a") (cat (bytes ":e") (L CR))) 'doe) '("t"))             ; bare :e reloads
      (! (st? (vgob '("a") (cat (bytes "x:e other") (L CR))) 'doe))             ; dirty refuses ...
      (= (st? (vgob '("a") (cat (bytes "x:e! other") (L CR))) 'doe) '("other")) ; ... and ! overrides
      (= (st? (vgob '("a") (cat (bytes "x:e!") (L CR))) 'doe) '("t"))
      (! (st? (vgob '("a") (cat (bytes ":e ") (L CR))) 'doe)))                  ; no name, no request
   _ (assert
      ; the swap starts the file over -- and re-decides the paint from the NEW name
      (: q (vgo '("a" "b") "jx") _ (vload q '("x" "y" "z") "n.l")
         (&& (= (lns q) '("x" "y" "z"))
             (&& (= (at q) '(0 0))
                 (&& (! (q 'dirty)) (&& (! (q 'undo)) (q 'hl))))))
      (: q (vgo '("a") "") _ (pin q 'hl 1) _ (vload q '("z") "n.md") (! (q 'hl)))
      (: q (vgo '("a") "") _ (vload q () "e.l") (= (lns q) '(""))))             ; an empty read is one empty line

; --- the cursor sits where you are TYPING: on the status line in ex and search -
   _ (assert
      (: f (mini '("ab") ":") (= (snip f ((tally f) - 6) (tally f)) (E$ + "[4;2H")))
      (: f (mini '("ab") ":wq") (= (snip f ((tally f) - 6) (tally f)) (E$ + "[4;4H")))
      (: f (mini '("ab") "/x") (= (snip f ((tally f) - 6) (tally f)) (E$ + "[4;3H")))
      (: f (mini '("ab") "") (= (snip f ((tally f) - 6) (tally f)) (E$ + "[1;1H"))))

; --- the syntax paint (crew/vi/hue.l): classes, the carry, and the one law
; that keeps every exact-frame assertion above honest --------------------------
   hbk (hue-book ())
   (klass s) (map (\ r <r) <(hue-lex hbk s 0 1))
   (runs s in) <(hue-lex hbk s in 1)
   ; strip SGR and ONLY SGR: \e[<digits;>m goes, \e[H and \e[K and \e[r;cH stay,
   ; because those are the frame's own geometry and the whole point is that the
   ; paint did not touch it
   (bare s)
    (: n (tally s)
       (dig? c) (|| (&& (48 <= c) (c <= 57)) (c = 59))
       (go i acc)
        (? (n <= i) acc
           (&& ((s i) = 27) (&& ((i + 1) < n) ((s (i + 1)) = 91)))
            (: (sk j) (? (&& (j < n) (dig? (s j))) (sk (j + 1)) j)
               k (sk (i + 2))
               (? (&& (k < n) ((s k) = 109)) (go (k + 1) acc)
                  (go (i + 1) (acc + (string (s i))))))
           (go (i + 1) (acc + (string (s i)))))
       (go 0 ""))
   (hlon st) (: _ (pin st 'pens (hue-pens hue-theme ())) _ (pin st 'hl 1) st)

; --- one class per token, table order: a word lane beats the punct lanes, the
;     number lane beats a leading sigil, and punct INSIDE a name is the name ---
   _ (assert
      (= (klass "; hi") '(comment))
      (= (klass "#!/x") '(comment))
      (= (klass "\"a ; b\"") '(string))               ; a ; inside a string is no comment
      (= (klass "'foo") '(atom))
      (= (klass "`(1)") '(ctor paren number paren))
      (= (klass "map") '(book))
      (= (klass "zzznope") ())                        ; not in the book: plain, and that is the tell
      (= (klass "zzz?") ())
      (= (klass ":") '(form))
      (= (klass "&&") '(macro))
      (= (klass "tau") '(const))
      (= (klass "$x") '(mono))                        ; the valence law, in colour
      (= (klass "a $ b") '(dyad))
      (= (klass "+") '(dyad))                         ; a bare operator is NOT a book name
      (= (klass "-3") '(number))
      (= (klass ".5") '(number))
      (= (klass "1.5e-3") '(number))
      (= (klass "1e10") '(number)))

; --- the carry: a string spans lines (papel.l's css is forty of them), so a
;     line's class depends on where the line BEFORE it ended -----------------
   _ (assert
      (= (hue-carry hbk '("(: css \"a" "b\" 1)" "x")) '(0 1 0))
      (= (runs "a ; b\" c" 1) '((string 0 6)))        ; carried in: the ; is still string
      (= (runs "a ; b" 1) '((string 0 5)))            ; and it stays open to the end
      ; keep () is the SAME walk with its answers dropped -- one lexer, provably
      (: a (hue-lex hbk "\"x" 0 1) b (hue-lex hbk "\"x" 0 ())
         (&& (<>a = <>b) (! (two? <b)))))

; --- the word lanes are ASKED, not listed. only the three special forms are
;     written out, and they are the grammar rather than the book ---------------
   _ (assert
      (: ls (map hue-arg (hue-tagged 'word))
         (&& (= (tally (filter nom? ls)) 3)                ; macros values names
             (= (tally (filter two? ls)) 1)))              ; the forms, and nothing else
      (0 < (tally (hue-words 'macros)))
      (0 < (tally (hue-words 'values)))
      (0 < (tally (hue-words 'names)))
      ; the probes answer what they claim to: a known macro, a known constant, a
      ; known verb -- each in ITS lane and no other
      (member? "assert" (hue-words 'macros))
      (member? "&&" (hue-words 'macros))                   ; NOT filtered: a sigil macro is a macro
      (member? "tau" (hue-words 'values))
      (member? "map" (hue-words 'names))
      (! (member? "+" (hue-words 'names)))                 ; a bare operator belongs to the punct lanes
      (! (member? "$" (hue-words 'values)))
      ; sorted, because the generator freezes them into a diff-gated file
      (: w (hue-words 'names) (= w (sort w)))
      ; ⚠ THE STABILITY LAW. both book lanes are POSITIVE tests -- a name DOES
      ; something or IS something -- so session data (argv, cmdline, `parsed`,
      ; which is bound only when love was handed a file) falls out of BOTH, and
      ; the generated syntax file cannot depend on how the generator was started.
      (all (\ w (lit? (ev (intern w)))) (hue-words 'names))
      (all (\ w (! (lit? (ev (intern w))))) (hue-words 'values))
      (! (member? "argv" (hue-words 'names)))
      (! (member? "argv" (hue-words 'values)))
      (! (member? "cmdline" (hue-words 'names)))
      ; earlier row wins in the one tablet, exactly as the table reads top-down
      (: b (hue-book ()) (&& (id? (b "assert") 'macro) (&& (id? (b "tau") 'const)
                                                           (id? (b "map") 'book)))))

; --- the pens: molokayo, and every class has one -----------------------------
   _ (assert
      (= (hue-pen '(Comment 59 "#75715E" ()) ()) "\e[38;5;59m")
      (= (hue-pen '(Statement 161 "#F92672" (bold)) ()) "\e[1;38;5;161m")
      (= (hue-pen '(Statement 161 "#F92672" (bold)) 1) "\e[1;38;2;249;38;114m")
      (: ps (hue-pens hue-theme ()) (all (\ c (string? (ps (hue-cls c)))) hue-classes))
      (all (\ r (any (\ c (id? (hue-grp c) <r)) hue-classes)) hue-theme))

; --- PAINT INVARIANCE: colour adds colour and moves NOTHING. this is what keeps
;     every byte-exact frame law above meaningful once the paint exists -------
   (twin ls ks) (: a (vgo ls ks) b (vgo ls ks)
                   _ (pin a 'w 30) _ (pin a 'h 5) _ (pin b 'w 30) _ (pin b 'h 5)
                   _ (hlon b)
                   ((bare (vframe b)) = (vframe a)))
   _ (assert
      (twin '("(map (+ 1) '(1 2 3)) ; hi" "(: x 2)") "")
      (twin '("(: css \"a" "b\" 1)") "j")
      (twin '("(a really quite long line that clips at the view edge)") "$")
      (twin '("") ""))

; --- born on for .l, off for everything else (which is why "t" above is plain),
;     and :hl flips it ------------------------------------------------------
   _ (assert
      (! (st? (vnew '("a") "t") 'hl))
      (! (st? (vnew '("a") "") 'hl))
      (! (st? (vnew '("a") "l") 'hl))
      (st? (vnew '("a") "x.l") 'hl)
      (st? (vnew '("a") "crew/vi/hue.l") 'hl)
      (st? (vgob '("a") (cat (bytes ":hl") (L CR))) 'hl)
      (! (st? (vgob '("a") (cat (bytes ":hl") (cat (L CR) (cat (bytes ":hl") (L CR))))) 'hl)))

   _ (say err "crew/vi/law: motions + words + finds + insert + operators + registers + undo + search + ex + frames + hue ok") _ (put err 10) 0)
