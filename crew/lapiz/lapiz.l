; lapiz -- the document lens: two surfaces over one middle. markdown <-> html today.
;
; a markdown file and an html file can say the same thing; a LENS is the pair of maps that
; carries each to the other and back without drift. lapiz gets that by refusing to translate
; directly: there is ONE middle -- a plain love AST -- and a READER and a SHOWER per surface,
;
;     markdown --mdread--> AST --htshow--> html
;     html     --htread--> AST --mdshow--> markdown
;
; so md->ht is just (htshow (mdread s)) and ht->md is (mdshow (htread s)), and a THIRD surface
; is its own reader/shower pair and nothing else -- it never learns about the other two, and it
; arrives already able to convert both ways with everything present. that is the whole shape of
; a pandoc-ish converter, and where this one is headed.
;
; both readers are built from the PARSER COMBINATORS in the post-egg layer love/post.l (a parser
; is (\ s y n): a string s, a success continuation y = (rest payload), a failure continuation
; n = rest; payloads are lists that `seq` concatenates -- love has no regex, so the leaves are
; `lit`, `one` and `span`). cook and sat/dimacs share that same set; lapiz adds the handful it
; lacked -- `notp` (NEGATIVE LOOKAHEAD, the thing a block grammar needs: "a paragraph line is one
; that starts no other block"), `nope`/`alts`, `many1`, and the two that build a NODE out of a
; flat payload -- `pack` (fold a whole payload down to one element) and `tag` (..and make it one).
;
; --- the lens laws (test/host/lapiz.l proves them) ---
;   1. the AST is the FIXED POINT of each surface:  (mdread (mdshow a)) = a
;                                                   (htread (htshow a)) = a
;   2. so each surface round-trips through the other, up to NORMALIZATION:
;        (ht->md (md->ht s)) = s   for s already normal,   and both norms are IDEMPOTENT.
; law 1 is the strong one -- it says neither shower loses anything its reader wants, which is
; exactly "the two surfaces are views of one value"; law 2 falls out of it. NORMAL means what a
; shower emits: paragraphs on one line, `-` bullets, ``` fences, one blank line between blocks.
; reading is therefore a RETRACTION (many markdowns, one AST) and showing a SECTION of it -- a
; lens, not an isomorphism, and the laws say which half is which. a new surface owes law 1 and
; nothing more; that is what keeps its arrival from touching any other.
;
; --- the middle ---
; a doc is a list of BLOCKS:
;   ('head N inl)  ('para inl)  ('fence lang text)  ('quote doc)
;   ('bullets (inl ..))  ('numbers (inl ..))  ('rule)
; an inl is a list of SPANS:
;   ('text s)  ('em inl)  ('strong inl)  ('code s)  ('link inl url)
; an inl is NORMAL when no two texts are adjacent (the readers merge as they go, `mrg`) --
; without that, law 1 fails on the AST even though both surfaces read the same. and every list
; here ends in the UNIT: a 0 tail is a fossil, and `=` tells the two terminators apart, so a
; single fossil fails law 1 on nothing but the nil.

; the module scope opens here: every binding below lands in this layer, mutually visible by
; bare name; the (leave ()) at the foot of this file closes it, which REGISTERS it as the module
; `lapiz` -- a consumer either (use 'lapiz) and reads bare, or reaches one member through the accessor:
; (from 'lapiz 'md->ht). the six faces are mdread/mdshow/htread/htshow/md->ht/ht->md; everything
; else is machinery, reachable for a probe ((from 'lapiz 'keys)) but not meant for the eye.
(enter 'lapiz)

; the combinators come from the `parse` MODULE book (love/post.l) -- bind the shared set. lapiz
; does NOT rebind the global `parse` the way cook and sat/dimacs do (they take the STRICT runner
; under that name; lapiz only ever wants the lenient `runp`), so the book stays whole. ⚠ it does
; READ that book, so a cat putting cook or dimacs FIRST hands lapiz the strict runner instead.
(: P parse
   eps (P 'eps)  lit (P 'lit)  one (P 'one)  span (P 'span)  alt (P 'alt)
   seq (P 'seq)  seqs (P 'seqs) opt (P 'opt)  skip (P 'skip)  pmap (P 'pmap)
   many (P 'many) runp (P 'runp))

; --- little utils ---
(: (nthl l k)   (? (k = 0) <l (nthl >l (k - 1)))
   (strcat l)   (foldl (+) "" l)
   (joins l sp) (? (! (two? l)) "" (! (two? >l)) <l (<l + sp + joins >l sp))
   (rep s n)    (? (n < 1) "" (s + rep s (n - 1)))
   (splitnl s)  ((: (g i st acc) (? (i < tally s) (? ((s i) = 10) (g (i + 1) (i + 1) (link (snip s st i) acc))
                                                      (g (i + 1) st acc))
                                    (st < i) (rev (link (snip s st i) acc))
                                    (rev acc)))
                 0 0 ())
   (endnl s)    (? (&& (0 < tally s) ((s (tally s - 1)) = 10)) s (s + "\n")))   ; one trailing newline, always

; --- the combinators post.l's set lacked ---
(: nope       (\ s y n (n s))                                  ; the always-failing parser: alts' unit
   (notp a)   (\ s y n (a s (\ _ _ (n s)) (\ _ (y s ()))))     ; NEGATIVE LOOKAHEAD -- consumes nothing either way
   (many1 a)  (seq a (many a))
   (alts l)   (foldr alt nope l)
   (pack a)   (pmap (\ p `(p)) a)                              ; a whole payload folded to ONE element
   (tag f a)  (pmap (\ p `((f p))) a)                          ; ..and that element built into a node
   (dflt a v) (alt a (pmap (\ _ `(v)) eps)))                   ; a always yields ONE element, v when absent

; --- character classes ---
(: (chr? c)  1
   (nonl? c) (! (c = 10))
   (sp? c)   (|| (c = 32) (c = 9))
   (ws? c)   (|| (sp? c) (|| (c = 10) (c = 13)))
   (dig? c)  (&& (47 < c) (c < 58))
   (hash? c) (c = 35)
   (spec? c) (member? c '(92 42 96 91 93))                     ; \ * ` [ ] -- where an inline span can start
   (ord? c)  (! (spec? c)))

; --- normalizing an inl: adjacent texts are ONE text (the AST is the fixed point, so this matters) ---
(: (mrg l) (? (! (two? l)) ()
              (! (two? >l)) (link <l ())
              (: a <l b <>l
                 (? (&& (id? <a 'text) (id? <b 'text)) (mrg (link `('text (<>a + <>b)) >>l))
                    (link a (mrg >l)))))
   (plain l) (strcat (map (\ x <>x) l)))                       ; an inl of pure texts back to a string

; ---------------------------------------------------------------- markdown, read
; inline. the text leaf stops at every sigil, so a construct is never swallowed; a sigil that
; starts nothing falls through to `mdchar` as one-char text and `mrg` glues it back on.
(: (mdesc s y n)    ((tag (\ p `('text (string (nthl p 1)))) (seq (lit "\\") (one chr?))) s y n)
   (mdcode s y n)   ((tag (\ p `('code (nthl p 1))) (seqs `((lit "`") (span (\ c (! (c = 96)))) (lit "`")))) s y n)
   (mdstrong s y n) ((tag (\ p `('strong <p)) (seqs `((skip (lit "**")) (pack (mdinls "**")) (skip (lit "**"))))) s y n)
   (mdem s y n)     ((tag (\ p `('em <p)) (seqs `((skip (lit "*")) (pack (mdinls "*")) (skip (lit "*"))))) s y n)
   (mdlink s y n)   ((tag (\ p `('link <p <>p))
                          (seqs `((skip (lit "[")) (pack (mdinls "]")) (skip (lit "]"))
                                  (skip (lit "(")) (span (\ c (! (c = 41)))) (skip (lit ")"))))) s y n)
   (mdtext s y n)   ((tag (\ p `('text (strcat p))) (seq (one ord?) (span ord?))) s y n)
   (mdchar s y n)   ((tag (\ p `('text (string <p))) (one chr?)) s y n)
   (mdspan s y n)   ((alts `(mdesc mdcode mdstrong mdem mdlink mdtext mdchar)) s y n)
   (mdinls stop s y n) ((pmap mrg (many (seq (notp (lit stop)) mdspan))) s y n)   ; spans up to a closer
   (mdinl s)        (runp (pmap mrg (many mdspan)) s))

; blocks. every one of these is entered at the START OF A LINE and eats its own newline.
(: eol       (skip (opt (lit "\n")))
   blankline (skip (seqs `((span sp?) (lit "\n"))))
   headmark  (seqs `((lit "#") (span hash?) (lit " ")))
   nummark   (seqs `((one dig?) (span dig?) (lit ". ")))
   starter   (alts `(headmark nummark (lit "```") (lit ">") (lit "- ") (lit "---")))   ; what a paragraph must not be

   (b-rule s y n)  ((tag (\ _ `('rule)) (seqs `((lit "---") (skip (span sp?)) (lit "\n")))) s y n)
   (b-head s y n)  ((tag (\ p `('head (1 + tally (nthl p 1)) (mdinl (nthl p 3))))
                         (seqs `((lit "#") (span hash?) (lit " ") (span nonl?) eol))) s y n)
   (f-line s y n)  ((seqs `((notp (lit "```")) (span nonl?) (lit "\n"))) s y n)       ; ..a fenced line keeps its newline
   (b-fence s y n) ((tag (\ p `('fence <p (strcat <>p)))
                         (seqs `((skip (lit "```")) (span nonl?) eol (pack (many f-line))
                                 (skip (lit "```")) (skip (span nonl?)) eol))) s y n)
   (q-line s y n)  ((seqs `((skip (lit ">")) (skip (opt (lit " "))) (span nonl?) (lit "\n"))) s y n)
   (b-quote s y n) ((tag (\ p `('quote (mdread (strcat <p)))) (pack (many1 q-line))) s y n)   ; the doc reads ITSELF
   (u-item s y n)  ((seqs `((skip (lit "- ")) (span nonl?) eol)) s y n)
   (o-item s y n)  ((seqs `((skip (one dig?)) (skip (span dig?)) (skip (lit ". ")) (span nonl?) eol)) s y n)
   (b-bullets s y n) ((tag (\ p `('bullets (map mdinl p))) (many1 u-item)) s y n)
   (b-numbers s y n) ((tag (\ p `('numbers (map mdinl p))) (many1 o-item)) s y n)
   ; a paragraph line must be neither blank nor a block opener -- and must eat A CHARACTER
   ; (`one nonl?` before the span), else `many` spins forever on the empty tail.
   (p-line s y n)  ((tag (\ p (strcat p))                      ; ..`tag` does the wrapping; f yields the ELEMENT
                         (seqs `((notp blankline) (notp starter) (one nonl?) (span nonl?) eol))) s y n)
   (b-para s y n)  ((tag (\ p `('para (mdinl (joins p " ")))) (many1 p-line)) s y n)
   (mdblock s y n) ((seqs `((skip (many blankline))
                            (alts `(b-rule b-head b-fence b-quote b-bullets b-numbers b-para)))) s y n)
   (mdread t) (runp (seqs `((many mdblock) (skip (many blankline)))) (endnl t)))

; ---------------------------------------------------------------- markdown, show
(: (mq s) ((: (g i acc) (? (i < tally s) (: c (s i) (g (i + 1) (acc + (? (spec? c) "\\" "") + (string c)))) acc)) 0 "")
   (mdi l)  (strcat (map mdi1 l))
   (mdi1 x) (: t <x
               (? (id? t 'text)   (mq <>x)
                  (id? t 'code)   ("`" + <>x + "`")
                  (id? t 'strong) ("**" + mdi <>x + "**")
                  (id? t 'em)     ("*" + mdi <>x + "*")
                  (id? t 'link)   ("[" + mdi <>x + "](" + nthl x 2 + ")")
                  ""))
   ; a paragraph whose first characters would re-read as another block wears ONE backslash --
   ; the escape leaf takes it back off, so law 1 survives a paragraph that starts "- " or "# ".
   (shield s) (? (two? (runp starter (endnl s))) ("\\" + s) s)
   (qpre s)   (strcat (map (\ ln (? (0 < tally ln) ("> " + ln + "\n") ">\n")) (splitnl s)))
   (nlist l i) (? (! (two? l)) "" ((show i) + ". " + mdi <l + "\n" + nlist >l (i + 1)))
   (mdb x) (: t <x
              (? (id? t 'head)    (rep "#" <>x + " " + mdi (nthl x 2) + "\n")
                 (id? t 'para)    (shield (mdi <>x) + "\n")
                 (id? t 'fence)   ("```" + <>x + "\n" + nthl x 2 + "```\n")
                 (id? t 'quote)   (qpre (mdshow <>x))
                 (id? t 'bullets) (strcat (map (\ i ("- " + mdi i + "\n")) <>x))
                 (id? t 'numbers) (nlist <>x 1)
                 (id? t 'rule)    "---\n"
                 ""))
   (mdshow d) (joins (map mdb d) "\n"))                        ; each block ends in \n, so \n between = a blank line

; ---------------------------------------------------------------- html, show
(: (hq s) ((: (g i acc) (? (i < tally s)
                           (: c (s i) (g (i + 1) (acc + (? (c = 38) "&amp;" (c = 60) "&lt;" (c = 62) "&gt;" (string c)))))
                           acc)) 0 "")
   (hti l)  (strcat (map hti1 l))
   (hti1 x) (: t <x
               (? (id? t 'text)   (hq <>x)
                  (id? t 'code)   ("<code>" + hq <>x + "</code>")
                  (id? t 'strong) ("<strong>" + hti <>x + "</strong>")
                  (id? t 'em)     ("<em>" + hti <>x + "</em>")
                  (id? t 'link)   ("<a href=\"" + hq (nthl x 2) + "\">" + hti <>x + "</a>")
                  ""))
   (hitems l t) ((joins (map (\ i ("<li>" + hti i + "</li>")) l) "\n") + "\n</" + t + ">")
   (htb x) (: t <x  h (? (id? t 'head) ("h" + show <>x) "")
              (? (id? t 'head)    ("<" + h + ">" + hti (nthl x 2) + "</" + h + ">")
                 (id? t 'para)    ("<p>" + hti <>x + "</p>")
                 (id? t 'fence)   ("<pre><code" + (? (0 < tally <>x) (" class=\"language-" + <>x + "\"") "")
                                    + ">" + hq (nthl x 2) + "</code></pre>")
                 (id? t 'quote)   ("<blockquote>\n" + htshow <>x + "\n</blockquote>")
                 (id? t 'bullets) ("<ul>\n" + hitems <>x "ul")
                 (id? t 'numbers) ("<ol>\n" + hitems <>x "ol")
                 (id? t 'rule)    "<hr>"
                 ""))
   (htshow d) (joins (map htb d) "\n"))

; ---------------------------------------------------------------- html, read
(: (otag t)    (lit ("<" + t + ">"))
   (ctag t)    (lit ("</" + t + ">"))
   hws         (skip (span ws?))
   (wrapt t a) (seqs `((skip (otag t)) a hws (skip (ctag t))))
   (entof nm)  (? (nm = "amp") "&" (nm = "lt") "<" (nm = "gt") ">" (nm = "quot") "\"" ("&" + nm + ";"))
   (hraw? c)   (&& (! (c = 60)) (! (c = 38)))                  ; ordinary text runs to the next < or &

   (hent s y n)    ((tag (\ p `('text (entof <p))) (seqs `((skip (lit "&")) (span (\ c (! (c = 59)))) (skip (lit ";"))))) s y n)
   (htext s y n)   ((tag (\ p `('text (strcat p))) (seq (one hraw?) (span hraw?))) s y n)
   (hcode s y n)   ((tag (\ p `('code (plain <p))) (wrapt "code" (pack (many (alts `(hent htext)))))) s y n)
   (hstrong s y n) ((tag (\ p `('strong <p)) (wrapt "strong" (pack htinl))) s y n)
   (hem s y n)     ((tag (\ p `('em <p)) (wrapt "em" (pack htinl))) s y n)
   ; ..the href is entity-decoded by running the same two text leaves over it
   (hlink s y n)   ((tag (\ p `('link <>p (plain (runp (many (alts `(hent htext))) <p))))
                         (seqs `((skip (lit "<a href=\"")) (span (\ c (! (c = 34)))) (skip (lit "\">"))
                                 (pack htinl) (skip (lit "</a>"))))) s y n)
   (htinl s y n)   ((pmap mrg (many (alts `(hent hcode hstrong hem hlink htext)))) s y n)

   (hlev n)  (tag (\ p `('head n <p)) (wrapt ("h" + show n) (pack htinl)))
   hlang     (seqs `((skip (lit " class=\"language-")) (span (\ c (! (c = 34)))) (skip (lit "\""))))
   (h-rule s y n)  ((tag (\ _ `('rule)) (lit "<hr>")) s y n)
   (h-head s y n)  ((alts (map hlev '(1 2 3 4 5 6))) s y n)
   (h-para s y n)  ((tag (\ p `('para <p)) (wrapt "p" (pack htinl))) s y n)
   (h-fence s y n) ((tag (\ p `('fence <p (plain <>p)))
                         (seqs `((skip (lit "<pre><code")) (dflt hlang "") (skip (lit ">"))
                                 (pack (many (alts `(hent htext)))) (skip (lit "</code></pre>"))))) s y n)
   (h-quote s y n) ((tag (\ p `('quote <p)) (wrapt "blockquote" (pack (many h-block)))) s y n)
   (h-li s y n)    ((seqs `(hws (wrapt "li" (pack htinl)))) s y n)
   (h-ul s y n)    ((tag (\ p `('bullets p)) (wrapt "ul" (many h-li))) s y n)
   (h-ol s y n)    ((tag (\ p `('numbers p)) (wrapt "ol" (many h-li))) s y n)
   (h-block s y n) ((seqs `(hws (alts `(h-rule h-head h-fence h-quote h-ul h-ol h-para)))) s y n)
   (htread t) (runp (seqs `((many h-block) hws)) t))

; ---------------------------------------------------------------- the lens
(: (md->ht s) (htshow (mdread s))
   (ht->md s) (mdshow (htread s)))

; the module scope closes here -- the layer is REGISTERED as `lapiz`. a later surface joins the
; sealed module the runtime way: (enter ()) (use 'lapiz) <surface.l> (leave ()).
(leave ())
