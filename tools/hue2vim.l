; tools/hue2vim.l -- write the vim syntax for .l out of crew/vi/hue.l's class table
; and the vocabulary the running binary answers to. `make syntax` runs it, into
; out/host/syntax.vim (crew/build.mk), and mk/install.mk installs that:
;
;   cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l | out/host/love
;
; the file is BUILT and never checked in, which is the point of the whole tool:
; the syntax used to carry a hand-curated copy of the vocabulary, with a comment
; asking a human to re-probe it on every rename. a promise only prose keeps is
; a promise already breaking -- and so is one that needs a human to re-run a
; command. `make -C tools test_hue` asks the one thing a build cannot: whether
; this ran to the end and meant it.
;
; ⚠ THE ONE PLACE THE TWO INTERPRETERS DIFFER. hue-lex takes the FIRST row that
; claims a token, so the table reads top-down. vim takes the LAST match that
; claims a character (its keywords beat all of it either way). so the syn lines
; go out in REVERSE table order and vim reproduces the table's own priority --
; sigils before numbers before atoms before strings before comments, exactly as
; the hand-written file used to arrange by hand and explain in a paragraph.
(use 'vi) ; hue's table rides module 'vi -- the cat registered it just above

(: (line s) (: _ (say out s) (put out 10))
   (blank x) (line "")
   (bytes s) (: n (tally s) (go i acc) (? (i < 0) acc (go (i - 1) (link (s i) acc)))
                (go (n - 1) ()))
   (join l sep) (foldl (\ a w (? (0 < (tally a)) (a + sep + w) w)) "" l)
   (pad t n) (: (sp j) (? (j < 1) "" (" " + (sp (j - 1))))
                k (n - (tally t))
                (t + (sp k)))
   (pre? t p) (: m (tally p)
                 (go i) (? (m <= i) 1 ((tally t) <= i) () ((t i) = (p i)) (go (i + 1)) ())
                 (go 0))
   (esc? c set) (: b (bytes set) (member? c b))
   ; a keyword as vim reads one: | and \ carry meaning in a syn keyword line
   (kwesc t) (foldl (\ a c (a + (? (esc? c "|\\") ("\\" + (string c)) (string c))))
                    "" (bytes t))
   ; a byte inside a [...] collection, and inside the "..." that delimits it
   (clesc t) (foldl (\ a c (a + (? (esc? c "]^-\\\"") ("\\" + (string c)) (string c))))
                    "" (bytes t))

   ; --- the groups: Love<Class>, straight off the table ------------------------
   (cap1 t) (: c (t 0) ((string (? (&& (97 <= c) (c <= 122)) (c - 32) c)) + (snip t 1 (tally t))))
   (grp r) ("Love" + (cap1 (string (hue-cls r))))

   ; --- the vocabulary ---------------------------------------------------------
   ; every word lane asks crew/vi/hue.l's probes -- there is no list here either.
   ; a lane CLAIMS its words and no later lane re-declares them: vim would accept
   ; the duplicate, but the file would be lying about which lane owns the name,
   ; and the painter (one tablet, earlier row wins) does not lie about it.
   ;
   ; the hue-* bindings are dropped: they are in the book only because this
   ; generator loads hue.l to read its table. they are the tool, not the language.
   ; ⚠ THE BOOK IS ONLY THE LANGUAGE ONCE IT HAS BEEN SEALED, and `book` is the
   ; tell: src/main.c pulls it -- with nif/nifx -- at the end of EVERY boot, so a
   ; run that can still see it is one where the seal did not happen. writing the
   ; compiler's own internals into the syntax file would be freezing BUILD STATE
   ; and calling it the language, so this dies loudly rather than emit a file that
   ; looks fine and is wrong. unreachable while the seal holds, which is the point:
   ; it is what notices if the seal ever goes lane-dependent again.
   _ (? (member? 'book (names ()))
         (: _ (say err "hue2vim: `book` is still on the book -- the seal did not run, and that is build state, not the language")
            _ (put err 10)
            (quit 1))
        0)
   wtab (: seen (tablet ())
           (go l t)
            (? (! (two? l)) t
               (: r <l
                  ws (filter (\ w (&& (! (seen w)) (! (pre? w "hue-")))) (hue-words (hue-arg r)))
                  _ (each ws (\ w (pin seen w 1)))
                  _ (pin t (hue-cls r) ws)
                  (go >l t)))
           (go (hue-tagged 'word) (tablet ())))
   (kwlines g ws) (? (! (two? ws)) ()
                     (: _ (line ("syn keyword " + g + " " + (join (map kwesc (take 8 ws)) " ")))
                        (kwlines g (drop 8 ws))))

   ; --- one tag, one vim rendering ---------------------------------------------
   ; the tag set is closed at eight, so this dispatch is the whole other half of
   ; crew/vi/hue.l's lexer. a class wanting a shape neither can say means a new
   ; TAG, in both -- never a hand-patch here.
   stop (" \\t\\n" + (clesc hue-stop))         ; what ends a token, as a collection
   sig  (clesc hue-sigils)
   (vim r)
    (: g (grp r) tag (hue-tag r) a (hue-arg r)
       (? (id? tag 'eol)
           (each a (\ o (line ("syn match " + g + " \"" + o + ".*$\" contains=LoveTodo"))))
          (id? tag 'span)
           (: q (string a) e (string (hue-nth (hue-rule r) 2))
              (line ("syn region " + g + " start='" + q + "' skip='" + e + e + e + e
                     + "\\|" + e + e + q + "' end='" + q + "'")))
          (id? tag 'mark)
           (line ("syn match " + g + " \"" + (string a) + "[^" + stop + "]*\""))
          ; sep: one byte standing alone, and a tag of its own because both
          ; readings reach a tag through its FIRST row, so a second row under
          ; one tag would be read by neither
          (id? tag 'sep)
           (line ("syn match " + g + " \"" + (string a) + "\""))
          (id? tag 'num)
           ; hex FIRST: `0x1f` would otherwise take the plain-integer match on its
           ; leading 0 and leave the rest bare, which is a worse lie than no colour
           (: _ (line ("syn match " + g + " \"\\<-\\?0[xX]\\x\\+\\>\""))
              _ (line ("syn match " + g + " \"\\<-\\?\\d\\+\\>\""))
              _ (line ("syn match " + g + " \"\\<-\\?\\d\\+\\.\\d*\\([eE][-+]\\?\\d\\+\\)\\?\\>\""))
              _ (line ("syn match " + g + " \"\\<-\\?\\.\\d\\+\\([eE][-+]\\?\\d\\+\\)\\?\\>\""))
              (line ("syn match " + g + " \"\\<-\\?\\d\\+[eE][-+]\\?\\d\\+\\>\"")))
          (id? tag 'word) (kwlines g (wtab (hue-cls r)))
          (id? tag 'punct)
           ; the valence law as vim reads it: a punct run LEADS a token (the
           ; lookbehind), and it is monadic when the full run touches a datum
           ; start. the excluded set carries the sigil chars themselves so a
           ; greedy run cannot backtrack and split itself.
           (? (id? a 'glued)
               (line ("syn match " + g + " \"[A-Za-z0-9_]\\@<![" + sig
                      + "]\\+\\ze[^ \\t),;" + sig + "]\""))
              (line ("syn match " + g + " \"[A-Za-z0-9_]\\@<![" + sig + "]\\+\"")))
          ; the delimiters ride the list regions at the foot of the file, where
          ; the rainbow levels need them; nothing to emit here
          ()))

   ; --- the rainbow levels: vim-only presentation, dark then light -------------
   levels '((red         red1          red          red3)
            (yellow      orange1       darkyellow   orangered3)
            (green       yellow1       darkgreen    orange2)
            (cyan        greenyellow   blue         yellow3)
            (magenta     green1        darkmagenta  olivedrab4)
            (red         springgreen1  red          green4)
            (yellow      cyan1         darkyellow   paleturquoise3)
            (green       slateblue1    darkgreen    deepskyblue4)
            (cyan        magenta1      blue         darkslateblue)
            (magenta     purple1       darkmagenta  darkviolet))
   (levline i l k)
    (line ("    hi def LoveLevel" + (show i) + " ctermfg=" + (string (hue-nth l k))
           + (: p (13 - (tally (string (hue-nth l k)))) (: (sp j) (? (j < 1) "" (" " + (sp (j - 1)))) (sp p)))
           + "guifg=" + (string (hue-nth l (k + 1)))))
   (levels-of k) (: (go l i) (? (! (two? l)) () (: _ (levline i <l k) (go >l (i + 1)))) (go levels 0))

   pgroup (grp <(hue-tagged 'delim))
   cluster (join ((map grp (filter (\ r (! (id? (hue-tag r) 'delim))) hue-classes))
                  + '("LoveTodo")) ",")

   ; the delim row's bytes read as PAIRS -- (0,1) then (2,3) then (4,5) -- and one
   ; region takes them ALL, so a level is one region however its list is bracketed.
   ; a region per pair instead would need the rainbow's contains= to name three
   ; regions a level, and buys only the mis-colouring of `(a]`, which is libra's
   ; to say and not a highlighter's. ⚠ `[` is the one byte vim's regex would read
   ; as a collection; `(`, `{` and `}` are literal under default magic.
   (vim-brk c) (? (c = 91) "\\[" (string c))
   brkpairs
    (: (go j k a) (? (tally hue-brk <= j) a
                     (go (j + 2) k (a + ((" " + k) + ("=\"" + (vim-brk (hue-brk j)) + "\"")))))
       ((go 0 "start" "") + (go 1 "end" "")))

; --- the file ------------------------------------------------------------------
   _ (line "\" vim syntax for love (.l) -- GENERATED. do not edit.")
   _ (line "\"")
   _ (line "\"   make syntax   --   cat crew/vi/config.l crew/vi/hue.l tools/hue2vim.l | out/host/love")
   _ (line "\"")
   _ (line "\" the source is crew/vi/hue.l's class table -- the same table pulchritude's own")
   _ (line "\" painter reads, so the editor in the tree and the editor outside it agree by")
   _ (line "\" construction -- plus the vocabulary the running binary answers to. this file is")
   _ (line "\" BUILT and never checked in, so a rename that moves (names ()) is picked up by")
   _ (line "\" make rather than by a human remembering to re-run something.")
   _ (line "\" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>")
   _ (line "if exists(\"b:current_syntax\")")
   _ (line "  finish")
   _ (line "endif")
   _ (blank ())
   _ (line "\" symbol-constituent chars (the reader ends a token only on whitespace and")
   _ (line "\" ( ) \" ' , # ; ). operators @ # $ are excluded so they highlight standalone.")
   _ (line "syn iskeyword @,33,37,38,42-43,45-47,48-57,58,60-63,92,94-96,124,126")
   _ (blank ())
   _ (line "\" vim's one nicety with no counterpart in the painter: TODO words inside a")
   _ (line "\" comment. contained, so it fires only inside the comment lane.")
   _ (line "syn match LoveTodo /\\<\\(TODO\\|FIXME\\|NOTE\\|XXX\\|HACK\\)\\>/ contained")
   _ (blank ())
   _ (line "\" the classes, in REVERSE table order: vim's last match wins, hue-lex's first")
   _ (line "\" row wins, and reversing makes the two agree.")
   _ (each (rev hue-classes) (\ r (vim r)))
   _ (blank ())
   _ (line "\" a close paren with nothing open is an error -- vim-only; lib/lint.l is where")
   _ (line "\" the tree actually answers that question.")
   _ (line "syn match LoveParenError \")\"")
   _ (blank ())
   _ (line "syn sync lines=100")
   _ (blank ())
   _ (each hue-classes (\ r (line ("hi def link " + (pad (grp r) 14) + " " + (string (hue-grp r))))))
   _ (line ("hi def link " + (pad "LoveTodo" 14) + " Todo"))
   _ (line ("hi def link " + (pad "LoveParenError" 14) + " Error"))
   _ (blank ())
   _ (line "\" Rainbow parentheses -- each nesting level gets its own colour, level 9 wraps")
   _ (line "\" to 0. Toggle with g:love_rainbow (default: off, one flat Delimiter).")
   _ (line ("syn cluster LoveListCluster contains=" + cluster))
   _ (blank ())
   _ (line "if !exists(\"g:love_rainbow\")")
   _ (line "  let g:love_rainbow = 0")
   _ (line "endif")
   _ (blank ())
   _ (line "if g:love_rainbow")
   _ (: (go i) (? (9 < i) ()
                  (: _ (line ("  syn region LoveList" + (show i) + " matchgroup=LoveLevel" + (show i)
                              + brkpairs + " contains=@LoveListCluster,LoveList"
                              + (show (? (i < 9) (i + 1) 0))))
                     (go (i + 1))))
        (go 0))
   _ (blank ())
   _ (line "  if &background ==# \"dark\"")
   _ (levels-of 0)
   _ (line "  else")
   _ (levels-of 2)
   _ (line "  endif")
   _ (line "else")
   _ (line ("  syn region LoveList matchgroup=" + pgroup
            + brkpairs + " contains=@LoveListCluster,LoveList"))
   _ (line "endif")
   _ (blank ())
   _ (line "let b:current_syntax = \"love\"")
   0)
