; lapiz -- the document lens: SURFACES over one middle. markdown, html and roff(man).
;
; a markdown file and an html file can say the same thing; a LENS is the pair of maps that
; carries each to the other and back without drift. lapiz gets that by refusing to translate
; directly: there is ONE middle -- a plain love AST -- and a READER and a SHOWER per surface,
;
;     markdown --mdread--> AST --htshow--> html      (and every other pairing, from the
;     html     --htread--> AST --rfshow--> roff       same six functions -- see `convert`)
;     roff     --rfread--> AST --mdshow--> markdown
;
; so md->ht is just (htshow (mdread s)). three surfaces make SIX conversions and there is no
; pairwise code for any of them: a conversion is one surface's reader and another's shower, so
; the `surfaces` table is the whole of it and a fourth adds a row. roff was added third and
; touched neither of the others -- except where it SHOULD, by pressing two nodes the middle was
; missing (meta, deflist), which then made markdown and html richer too. that is the trade a
; surface is allowed to ask for; anything else it must do alone.
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
;   ('meta ((key value) ..))  ('head N inl)  ('para inl)  ('fence lang text)  ('quote doc)
;   ('bullets (inl ..))  ('numbers (inl ..))  ('deflist ((term-inl body-inl) ..))  ('rule)
; an inl is a list of SPANS:
;   ('text s)  ('em inl)  ('strong inl)  ('code s)  ('link inl url)
; ('meta ..) is document-level and lives at the HEAD of the doc (markdown spells it as front
; matter, and a `---` anywhere else is a rule); its keys are the canonical five, always all
; five, absent ones empty -- see `mnorm`.
; a doc is NORMAL when no two texts are adjacent in an inl (`mrg`) and no two lists of one kind
; are adjacent in the doc (`bmrg`); every reader lands on that. without it law 1 fails on an AST
; that reads the same through every surface but is spelled two ways. and every list here ends in
; the UNIT: a 0 tail is a fossil, and `=` tells the two terminators apart, so a single fossil
; fails law 1 on nothing but the nil.
;
; --- the roff DOMAIN ---
; roff's man macros are a SMALLER surface, and saying where is part of the job. law 1 holds on
; man-page-shaped docs: meta, head 1-2, para, fence with an empty language, quote, bullets,
; numbers, deflist, and text/strong/em/code inline. outside that rfshow stays total and rfread
; normalizes in four ways, each one asserted by the gate rather than left to be found:
;   a head at 3+ lands at 2 (roff has .SH and .SS and no more), a fence language is dropped
;   (.EX/.EE has no slot), a rule vanishes (man pages have none), and a link flattens to its
;   text with the url trailing in \(la..\(ra -- the man idiom for one.

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

; --- the meta record: document-level fields, one CANONICAL shape ---
; every reader runs `mnorm`, so a meta block always carries the same five keys in the same
; order, absent ones empty. That is what lets a shower emit only the fields that are FILLED
; and still round-trip: the reader fills the rest back in with "". Values are single-line.
; ⚠ the key is INTERNED, not `nom`ed: `nom` mints a FRESH symbol that is neither `id?` nor
; `=` to the literal of the same spelling, so a meta read from one surface would compare
; unequal to the same meta read from another -- law 1 failing on the key, not the value.
(: metakeys '(title section date source manual)
   (mget kvs k) (? (! (two? kvs)) "" (: p <kvs (? (id? <p k) <>p (mget >kvs k))))
   (mnorm kvs)  (map (\ k `(k (mget kvs k))) metakeys)
   (mfilled kvs) (filter (\ p (0 < tally <>p)) kvs))

; --- the roff glyph table, read BOTH ways: rfread decodes \(em to the character, rq encodes
; it back. so it must be a BIJECTION -- no two names may share a character, which is why the
; three quote escapes (\(aq \(oq \(cq, all an apostrophe) are absent. encoding matters: groff
; reads its input as latin-1 unless preconv ran, so a raw UTF-8 character in the source
; renders as MOJIBAKE, not as itself.
(: rglyphs '(("em" "—") ("en" "–") ("bu" "•") ("la" "⟨") ("ra" "⟩") ("mi" "−"))
   (glyph n)   ((: (g l) (? (! (two? l)) ("\\(" + n) ((<<l) = n) (<>(<l)) (g >l))) rglyphs)
   (rqat s i)  ((: (g l) (? (! (two? l)) ()                    ; does a glyph start at i? -> (name len)
                            (: gl (<>(<l)) n (tally gl)
                               (? (&& ((i + n) <= tally s) (gl = (snip s i (i + n)))) `((<(<l)) n) (g >l)))))
                rglyphs))

; --- normalizing an inl: adjacent texts are ONE text (the AST is the fixed point, so this matters) ---
(: (mrg l) (? (! (two? l)) ()
              (: h <l (&& (id? <h 'text) (0 = tally <>h))) (mrg >l)   ; a \& contributes an EMPTY text: drop it
              (! (two? >l)) (link <l ())
              (: a <l b <>l
                 (? (&& (id? <a 'text) (id? <b 'text)) (mrg (link `('text (<>a + <>b)) >>l))
                    (link a (mrg >l)))))
   ; ..and the same at BLOCK level: two adjacent lists of one kind are ONE list. markdown's
   ; own loose-list rule says so (`- a` blank `- b` is one list), html would spell them as two
   ; <ul>s, and roff cannot separate two `.TP` runs at all -- so the normal form is "no two
   ; adjacent bullets/numbers/deflists" and every reader lands on it. Without this the roff
   ; surface silently merges a pair that the markdown surface kept apart, and law 1 fails on
   ; a document that looks right in all three.
   (bmrg l) (? (! (two? l)) ()
               (! (two? >l)) (link <l ())
               (: a <l b <>l
                  (? (&& (id? <a <b) (member? <a '(bullets numbers deflist)))
                     (bmrg (link `(<a (cat <>a <>b)) >>l))
                     (link a (bmrg >l)))))
   (plain l) (strcat (map (\ x <>x) l))                        ; an inl of pure texts back to a string
   (dpair l) (? (! (two? l)) () (! (two? >l)) () (link `(<l <>l) (dpair >>l)))   ; a flat t/b payload into pairs
   (zipm ks vs) (? (! (two? ks)) () (! (two? vs)) () (link `(<ks <vs) (zipm >ks >vs))))

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
   defmark   (lit ":   ")                                                        ; the definition-list body marker
   starter   (alts `(headmark nummark defmark (lit "```") (lit ">") (lit "- ") (lit "---")))   ; what a paragraph must not be

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
   ; a definition-list item: a TERM line (itself no block opener), then its `:   ` body line.
   ; tried before b-para -- a term line reads as a paragraph line until the body line proves
   ; otherwise, and the failure backtracks, so b-para picks up an ordinary paragraph. blocks
   ; are always blank-line separated by every shower, so a paragraph never swallows a term.
   (d-term s y n)  ((tag (\ p (strcat p)) (seqs `((notp blankline) (notp starter) (one nonl?) (span nonl?) eol))) s y n)
   (d-body s y n)  ((seqs `((skip defmark) (span nonl?) eol)) s y n)
   (d-item s y n)  ((tag (\ p `((mdinl <p) (mdinl <>p))) (seq d-term d-body)) s y n)
   (b-deflist s y n) ((tag (\ p `('deflist p)) (many1 d-item)) s y n)
   ; a paragraph line must be neither blank nor a block opener -- and must eat A CHARACTER
   ; (`one nonl?` before the span), else `many` spins forever on the empty tail.
   (p-line s y n)  ((tag (\ p (strcat p))                      ; ..`tag` does the wrapping; f yields the ELEMENT
                         (seqs `((notp blankline) (notp starter) (one nonl?) (span nonl?) eol))) s y n)
   (b-para s y n)  ((tag (\ p `('para (mdinl (joins p " ")))) (many1 p-line)) s y n)
   (mdblock s y n) ((seqs `((skip (many blankline))
                            (alts `(b-rule b-head b-fence b-quote b-bullets b-numbers b-deflist b-para)))) s y n)
   ; YAML-ish FRONT MATTER, the markdown face of the meta block: head-only (a `---` fence
   ; anywhere else is a rule), so it is read before the block loop and shown before it.
   (kvline s y n)  ((tag (\ p `((intern <p) (nthl p 1)))
                         (seqs `((span (\ c (&& (nonl? c) (! (c = 58))))) (skip (lit ": ")) (span nonl?) eol))) s y n)
   (b-meta s y n)  ((tag (\ p `('meta (mnorm p)))
                         (seqs `((skip (lit "---")) (skip (span sp?)) (skip (lit "\n"))
                                 (many kvline) (skip (lit "---")) (skip (span sp?)) (skip (lit "\n"))))) s y n)
   (mdread t) (bmrg (runp (seqs `((opt b-meta) (many mdblock) (skip (many blankline)))) (endnl t))))

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
   (mfront kvs) ("---\n" + (strcat (map (\ p ((string <p) + ": " + <>p + "\n")) (mfilled kvs))) + "---\n")
   (mdefs l)    (strcat (map (\ it (mdi <it + "\n:   " + mdi <>it + "\n")) l))
   (mdb x) (: t <x
              (? (id? t 'head)    (rep "#" <>x + " " + mdi (nthl x 2) + "\n")
                 (id? t 'para)    (shield (mdi <>x) + "\n")
                 (id? t 'fence)   ("```" + <>x + "\n" + nthl x 2 + "```\n")
                 (id? t 'quote)   (qpre (mdshow <>x))
                 (id? t 'bullets) (strcat (map (\ i ("- " + mdi i + "\n")) <>x))
                 (id? t 'numbers) (nlist <>x 1)
                 (id? t 'deflist) (mdefs <>x)
                 (id? t 'meta)    (mfront <>x)
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
   (hmeta kvs)  (joins (map (\ p ("<meta name=\"" + (string <p) + "\" content=\"" + hq <>p + "\">")) (mfilled kvs)) "\n")
   (hdefs l)    ("<dl>\n" + (joins (map (\ it ("<dt>" + hti <it + "</dt>\n<dd>" + hti <>it + "</dd>")) l) "\n") + "\n</dl>")
   (htb x) (: t <x  h (? (id? t 'head) ("h" + show <>x) "")
              (? (id? t 'head)    ("<" + h + ">" + hti (nthl x 2) + "</" + h + ">")
                 (id? t 'para)    ("<p>" + hti <>x + "</p>")
                 (id? t 'fence)   ("<pre><code" + (? (0 < tally <>x) (" class=\"language-" + <>x + "\"") "")
                                    + ">" + hq (nthl x 2) + "</code></pre>")
                 (id? t 'quote)   ("<blockquote>\n" + htshow <>x + "\n</blockquote>")
                 (id? t 'bullets) ("<ul>\n" + hitems <>x "ul")
                 (id? t 'numbers) ("<ol>\n" + hitems <>x "ol")
                 (id? t 'deflist) (hdefs <>x)
                 (id? t 'meta)    (hmeta <>x)
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
   (h-meta1 s y n) ((tag (\ p `((intern <p) (plain (runp (many (alts `(hent htext))) <>p))))
                          (seqs `(hws (skip (lit "<meta name=\"")) (span (\ c (! (c = 34))))
                                  (skip (lit "\" content=\"")) (span (\ c (! (c = 34)))) (skip (lit "\">"))))) s y n)
   (h-meta s y n)  ((tag (\ p `('meta (mnorm p))) (many1 h-meta1)) s y n)
   (h-dt s y n)    ((seqs `(hws (wrapt "dt" (pack htinl)) hws (wrapt "dd" (pack htinl)))) s y n)
   (h-dl s y n)    ((tag (\ p `('deflist (dpair p))) (wrapt "dl" (many h-dt))) s y n)
   (h-block s y n) ((seqs `(hws (alts `(h-rule h-head h-fence h-quote h-ul h-ol h-dl h-meta h-para)))) s y n)
   (htread t) (bmrg (runp (seqs `((many h-block) hws)) t)))

; ---------------------------------------------------------------- roff (man), show
; the third surface, and a SMALLER one: roff's man macros carry sections two deep, literal
; blocks with no language, and no rule or hyperlink at all. so the ROFF DOMAIN is the
; man-page-shaped docs -- meta, head 1-2, para, fence with empty lang, quote, bullets,
; numbers, deflist; text/strong/em/code inline. Law 1 holds THERE. Outside it rfshow stays
; total and rfread normalizes in four stated ways, each asserted by the gate rather than
; left to be discovered: head >=3 lands at 2, a fence language is dropped, a rule vanishes,
; and a link flattens to its text with the url in \(la..\(ra (the man idiom for one).
(: (rq s) ((: (g i acc)
              (? (! (i < tally s)) acc
                 (: hit (rqat s i)
                    (? (two? hit) (g (i + (nthl hit 1)) (acc + "\\(" + <hit))
                       (: c (s i) (g (i + 1) (acc + (? (c = 92) "\\e" (c = 45) "\\-" (string c)))))))))
           0 "")
   (rline s) (? (&& (0 < tally s) (|| ((s 0) = 46) ((s 0) = 39))) ("\\&" + s) s)   ; a leading . or ' is text
   ; roff DOUBLE-SPACES after a period that ends an input line, so a paragraph emitted as one
   ; long line loses its sentence spacing. break it one sentence per line -- the idiomatic man
   ; source shape, and lossless: the reader fills the lines back with a single space.
   (rsent s) ((: (g i st acc)
                 (? (! (i < tally s)) (revcat (link (rline (snip s st i)) acc) ())
                    (&& ((s i) = 46) (&& ((i + 1) < tally s) ((s (i + 1)) = 32)))
                      (g (i + 2) (i + 2) (link (rline (snip s st (i + 1))) acc))
                    (g (i + 1) st acc))) 0 0 ())
   (rfill s) (strcat (map (\ ln (ln + "\n")) (rsent s)))
   (rfi l)  (strcat (map rfi1 l))
   (rfi1 x) (: t <x
               (? (id? t 'text)   (rq <>x)
                  (id? t 'code)   ("\\f(CR" + rq <>x + "\\fR")     ; CR, not the deprecated CW: groff warns on that
                  (id? t 'strong) ("\\fB" + rfi <>x + "\\fR")
                  (id? t 'em)     ("\\fI" + rfi <>x + "\\fR")
                  (id? t 'link)   (rfi <>x + " \\(la" + rq (nthl x 2) + "\\(ra")
                  ""))
   (rth kvs)   (".TH" + (strcat (map (\ k (" \"" + (mget kvs k) + "\"")) metakeys)) + "\n")
   (rnums l i) (? (! (two? l)) "" (".IP \"" + (show i) + ".\" 4\n" + (rline (rfi <l)) + "\n" + rnums >l (i + 1)))
   (rdefs l)   (strcat (map (\ it (".TP\n" + (rline (rfi <it)) + "\n" + (rfill (rfi <>it)))) l))
   (rfb x) (: t <x
              (? (id? t 'meta)    (rth <>x)
                 (id? t 'head)    ((<>x < 2 ? ".SH " ".SS ") + rfi (nthl x 2) + "\n")
                 (id? t 'para)    (".PP\n" + (rfill (rfi <>x)))
                 (id? t 'fence)   (".EX\n" + (strcat (map (\ ln ((rline (rq ln)) + "\n")) (splitnl (nthl x 2)))) + ".EE\n")
                 (id? t 'quote)   (".PP\n.RS\n" + rfshow <>x + ".RE\n")   ; the .PP spaces the display
                 (id? t 'bullets) (strcat (map (\ i (".IP \\(bu 2\n" + (rline (rfi i)) + "\n")) <>x))
                 (id? t 'numbers) (rnums <>x 1)
                 (id? t 'deflist) (rdefs <>x)
                 ""))                                        ; a rule has no roff form: it vanishes
   (rfshow d) (strcat (map rfb d)))

; ---------------------------------------------------------------- roff (man), read
; roff FILLS, and that shapes this whole half: consecutive input lines join with a SPACE,
; a font macro's arguments join with NOTHING (the alternating .BR/.RB/.IR/.RI) or with a
; space (.B/.I) -- all three probed under groff, not remembered. so a PARAGRAPH is a RUN of
; lines, each contributing an inl fragment, and the block macros are what end a run. that is
; why this reader is line-oriented where the html one is tag-oriented: the surface is. it
; reads the MACRO forms a hand-written page uses (.B/.I/.BR/.RB/.IR/.RI), not just the inline
; escapes rfshow emits -- doc/*.md were converted from hand-written pages by this reader.
(: rfesc? (\ c (! (c = 92)))                                    ; ordinary inline text runs to the next \
   rclose    (alts `((lit "\\fR") (lit "\\fP")))                ; \fP is "previous font" -- same thing here
   (rgl s y n)     ((tag (\ p `('text (glyph ((string <p) + (string <>p)))))
                         (seqs `((skip (lit "\\(")) (one chr?) (one chr?)))) s y n)
   (resc s y n)    ((alts `((tag (\ _ `('text "\\")) (lit "\\e"))
                            (tag (\ _ `('text "-")) (lit "\\-"))
                            (tag (\ _ `('text "")) (lit "\\&"))     ; zero-width: mrg drops it
                            (tag (\ _ `('text " ")) (lit "\\ "))
                            rgl)) s y n)
   (rtext s y n)   ((tag (\ p `('text (strcat p))) (seq (one rfesc?) (span rfesc?))) s y n)
   ; ..a \ that starts none of the above is an escape we do not model: it reads as literal
   ; text one char at a time and `mrg` glues it back on -- the same fallback the markdown
   ; surface gives an unclaimed sigil. Without it the reader stops dead at the first one.
   (rchar s y n)   ((tag (\ p `('text (string <p))) (one chr?)) s y n)
   (rfont o t s y n) ((tag (\ p `(t <p)) (seqs `((skip (lit o)) (pack (rinls)) (skip rclose)))) s y n)
   (rstrong s y n) ((rfont "\\fB" 'strong) s y n)
   (rem s y n)     ((rfont "\\fI" 'em) s y n)
   (rcode s y n)   ((tag (\ p `('code (plain <p)))
                         (seqs `((skip (alts `((lit "\\f(CR") (lit "\\f(CW"))))   ; CW is groff-deprecated, still read
                                 (pack (many (seq (notp rclose) (alts `(resc rtext rchar))))) (skip rclose)))) s y n)
   (rspan s y n)   ((alts `(resc rcode rstrong rem rtext rchar)) s y n)
   (rinls s y n)   ((pmap mrg (many (seq (notp rclose) rspan))) s y n)
   (rfinl s)       (mrg (runp (many rspan) s))
   (rplain s)      (plain (mrg (runp (many (alts `(resc rtext rchar))) s))))   ; escapes only, no fonts

; the font MACROS: which fonts to cycle through, and whether the arguments take a space.
(: (rargs s)                                                  ; a macro's arguments, bare or "quoted"
   ((: (g i acc)
       (? (! (i < tally s)) (rev acc)
          ((s i) = 32) (g (i + 1) acc)
          ((s i) = 34) (: j ((: (h k) (? (&& (k < tally s) (! ((s k) = 34))) (h (k + 1)) k)) (i + 1))
                          (g (j + 1) (link (snip s (i + 1) j) acc)))
          (: j ((: (h k) (? (&& (k < tally s) (! ((s k) = 32))) (h (k + 1)) k)) i)
             (g j (link (snip s i j) acc))))) 0 ())
   fontmacs '((".BR" (strong text) 0) (".RB" (text strong) 0) (".IR" (em text) 0)
              (".RI" (text em) 0) (".SB" (strong) 1) (".SM" (text) 1) (".B" (strong) 1) (".I" (em) 1))
   (macinfo m) ((: (g l) (? (! (two? l)) () ((<<l) = m) (>(<l)) (g >l))) fontmacs)
   (wrapf f l)   (? (id? f 'strong) `(`('strong l)) (id? f 'em) `(`('em l)) l)
   (allplain? l) (? (! (two? l)) 1 (id? <<l 'text) (allplain? >l) 0)
   (argl a f)    (: l (rfinl a) (? (allplain? l) (wrapf f l) l))     ; an explicit \f inside the arg wins
   (argls as fs i sp) (? (! (two? as)) ()
                         (: h (argl <as (nthl fs (i % tally fs)))
                            (? (! (two? >as)) h
                               (cat h (cat (? sp `(`('text " ")) ()) (argls >as fs (i + 1) sp))))))
   (imacl line) (: a (rargs line) m (? (two? a) <a "") info (macinfo m)
                   (? (two? info) (mrg (argls (rargs (snip line (tally m) (tally line))) <info (nthl info 1) 0))
                      (rfinl line)))
   (linl line)  (? (&& (0 < tally line) ((line 0) = 46)) (imacl line) (rfinl line)))

; the block layer: a run of fill lines is a paragraph, a block macro ends it.
(: blockmac (alts `((lit ".TH") (lit ".SH") (lit ".SS") (lit ".PP") (lit ".LP") (lit ".TP")
                    (lit ".RS") (lit ".RE") (lit ".EX") (lit ".EE") (lit ".IP") (lit ".br") (lit ".nf") (lit ".fi")))
   rfline    (seqs `((span nonl?) (skip (lit "\n"))))
   (rmacl m) (seqs `((skip (lit m)) (skip (span sp?)) (span nonl?) (skip (lit "\n"))))
   (r-fline s y n)  ((tag (\ p (linl (strcat p)))
                          (seqs `((notp blockmac) (one nonl?) (span nonl?) (skip (lit "\n"))))) s y n)
   (joinfr l) (? (! (two? l)) () (! (two? >l)) <l (cat <l (cat `(`('text " ")) (joinfr >l))))
   (r-filled s y n) ((tag (\ p (mrg (joinfr p))) (many1 r-fline)) s y n)

   (r-th s y n)   ((tag (\ p `('meta (mnorm (zipm metakeys (rargs <p))))) (rmacl ".TH")) s y n)
   (r-sh s y n)   ((tag (\ p `('head 1 (rfinl (joins (rargs <p) " ")))) (rmacl ".SH")) s y n)
   (r-ss s y n)   ((tag (\ p `('head 2 (rfinl (joins (rargs <p) " ")))) (rmacl ".SS")) s y n)
   ; a .PP with no filled run behind it is an EMPTY paragraph (the pages use one to space a
   ; display); `opt` gives it an empty payload, so it contributes no block at all.
   (r-pp s y n)   ((seqs `((skip (alts `((lit ".PP") (lit ".LP")))) (skip (span nonl?)) (skip (lit "\n"))
                           (opt (tag (\ p `('para <p)) r-filled)))) s y n)
   (r-para s y n) ((tag (\ p `('para <p)) r-filled) s y n)      ; text with no .PP -- after a .SH
   (r-tp s y n)   ((tag (\ p `(<p <>p))
                        (seqs `((skip (lit ".TP")) (skip (span nonl?)) (skip (lit "\n")) r-fline r-filled))) s y n)
   (r-dl s y n)   ((tag (\ p `('deflist p)) (many1 r-tp)) s y n)
   (r-bu s y n)   ((seqs `((skip (lit ".IP \\(bu")) (skip (span nonl?)) (skip (lit "\n")) r-filled)) s y n)
   (r-ul s y n)   ((tag (\ p `('bullets p)) (many1 r-bu)) s y n)
   (r-no s y n)   ((seqs `((skip (lit ".IP")) (skip (span nonl?)) (skip (lit "\n")) r-filled)) s y n)
   (r-ol s y n)   ((tag (\ p `('numbers p)) (many1 r-no)) s y n)
   (r-ex s y n)   ((tag (\ p `('fence "" (strcat (map (\ ln ((rplain ln) + "\n")) p))))
                         (seqs `((skip (alts `((lit ".EX") (lit ".nf")))) (skip (span nonl?)) (skip (lit "\n"))
                                 (many (seq (notp (alts `((lit ".EE") (lit ".fi")))) rfline))
                                 (skip (alts `((lit ".EE") (lit ".fi")))) (skip (span nonl?)) (skip (lit "\n"))))) s y n)
   (r-rs s y n)   ((tag (\ p `('quote p))
                         (seqs `((skip (lit ".RS")) (skip (span nonl?)) (skip (lit "\n"))
                                 (many r-block)
                                 (skip (lit ".RE")) (skip (span nonl?)) (skip (lit "\n"))))) s y n)
   (r-br s y n)    ((skip (seqs `((lit ".br") (span nonl?) (lit "\n")))) s y n)   ; a break ends the run
   (r-blank s y n) ((skip (seqs `((span sp?) (lit "\n")))) s y n)
   (r-block s y n) ((alts `(r-blank r-br r-th r-sh r-ss r-pp r-ex r-rs r-ul r-ol r-dl r-para)) s y n)
   (rfread t) (bmrg (runp (many r-block) t)))

; ---------------------------------------------------------------- the lens
; three surfaces, six conversions -- but no pairwise code: a conversion is a reader from
; one and a shower from the other, so the table is the whole of it. a fourth surface adds
; a row and nothing else.
(: surfaces `(`('md mdread mdshow) `('ht htread htshow) `('rf rfread rfshow))
   (srow k)   ((: (g l) (? (! (two? l)) () (id? <<l k) <l (g >l))) surfaces)
   (reader-of k) (<>(srow k))
   (shower-of k) (nthl (srow k) 2)
   (convert from to s) ((shower-of to) ((reader-of from) s))
   (md->ht s) (htshow (mdread s))
   (ht->md s) (mdshow (htread s))
   (md->rf s) (rfshow (mdread s))
   (rf->md s) (mdshow (rfread s)))

; the module scope closes here -- the layer is REGISTERED as `lapiz`. a later surface joins the
; sealed module the runtime way: (enter ()) (use 'lapiz) <surface.l> (leave ()).
(leave ())
