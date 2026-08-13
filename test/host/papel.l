; --- test/host/papel.l: the static site, exactly ---
; NOT in the test/*.l corpus (it loads three crew files and splices their modules,
; and it writes real files under /tmp).
;
; papel neither parses markdown nor decides what is stale -- lapiz owns the first
; and cook the second, and both are proved by their own gates. so what is left to
; prove here is exactly what papel adds, the SITE: the facts about a page that are
; only true because of the other pages.
;   ANCHORS   every href="#x" papel emits has an id="x" on the same page. one parse
;             feeds the toc and the body, and this is the assert that says so; it is
;             the one invariant a second pass over the text would eventually break.
;   SLUGS     stable, and deduped when two headings spell the same one.
;   RELINK    a .md link becomes .html iff that page is IN the site. absolute,
;             anchored and outside-the-site links are left exactly as written --
;             papel may not invent a dead link markdown did not already have.
;   TITLE     head-1, else the meta `title`, else the file name -- in that order.
;   FRESH     a second build writes NOTHING; touching one source writes exactly two
;             files (that page and the index). that is cook's law, but papel is the
;             one that has to declare the recipes right for it to hold.
;
; run:  cat test/00-init.l test/host/papel.l | out/host/love
; (⚠ through STDIN, as the gate does -- `love test/host/papel.l` would seat a file
;  whose basename is papel.l, and papel's own seat would fire on it.)

(use 'papel)                                ; the loader: lib/papel.l -- papel wants lapiz + cook itself

; --- the module seam: three modules, and papel adds nothing to the book ----------
(assert
 (member? 'papel (from ()))
 (member? 'lapiz (from ()))
 (member? 'cook (from ()))
 (member? 'build (keys (from 'papel)))
 (lit? (from 'papel 'papel-main))
 (! (from 'papyrus))                          ; a missing module answers ()
 ; the combinators are a MODULE nobody splices at boot: lapiz and cook each said (use 'peg),
 ; so peg is loaded and reachable BY KEY -- and `parse` is not a name in the book at all.
 (member? 'peg (from ()))
 (lit? (from 'peg 'seq))
 (! (member? 'parse (names ()))))

(: dir "/tmp/papeltest"
   (put-file f s) (: q (open (dir + "/" + f) "w") _ (say q s) (close q))
   (get-file f)   (: q (open (dir + "/" + f) "r") (? q (slurp q) ""))
   ; every attribute value introduced by `pre` and closed by a quote: the way to ask
   ; the emitted html what it actually said, without re-parsing it.
   (finds s pre)  (: m (tally pre) n (tally s)
                     ((: (g i acc)
                         (? ((i + m) <= n)
                            (? ((snip s i (i + m)) = pre)
                               (: j ((: (f k) (? (&& (k < n) (! ((s k) = 34))) (f (k + 1)) k)) (i + m))
                                  (g j (link (snip s (i + m) j) acc)))
                               (g (i + 1) acc))
                            (rev acc))) 0 ()))
   (has? l x)     (member? x l)
   (uniq? l)      ((: (g l) (? (! (two? l)) 1 (member? <l >l) 0 (g >l))) l)

   ; from a CLEAN slate: the freshness clauses below count files written, so a left-over
   ; site from a previous run would make the first build a no-op and every count wrong.
   _ (hark `("rm" "-rf" dir))
   _ (mkdir dir 493)
   _ (mkdir (dir + "/src") 493)

   ; --- the fixture: small, and every clause below points at one line of it -------
   _ (put-file "src/one.md" "# One

The first page, whose opening paragraph becomes its line on the index.

## Shared Name

a

## Shared Name

b

See [two](two.md), [the archive](archive/old.md), [the web](https://example.com/x.md).
")
   _ (put-file "src/two.md" "---
title: Two From Meta
---

Only a paragraph, so the title has to come from the meta block.
")
   _ (put-file "src/three.md" "Neither a head-1 nor a meta block, so the file name is the title.
")

   conf (: t (tablet 0)
           _ (pin t 'out (dir + "/site"))
           _ (pin t 'title "Fixture")
           _ (pin t 'src `((dir + "/src")))
           _ (pin t 'self "crew/papel/papel.l")
           _ (pin t 'quiet 1)
           t)

   ; --- the build, and FRESHNESS ---------------------------------------------------
   ; 3 pages + the stylesheet + the index = 5 files; then nothing; then exactly the
   ; touched page and the index, which depends on every source's title.
   n1 (build conf)
   n2 (build conf)
   ; rewrite one source byte-for-byte, to advance its mtime. ⚠ READ IT FIRST: `open "w"`
   ; truncates, so reading inside the `say` would write the empty file back.
   ; ⚠ AND LET THE COARSE CLOCK TICK FIRST (rest 5): the kernel stamps files off a
   ; ~ms-granular clock, so a rewrite microseconds after the build lands in the SAME
   ; tick as the outputs and a strict-older mtime compare (cook's, exactly make's)
   ; reads it fresh. the fork-free stat nif made cook fast enough to hit this; the
   ; old shell-out date was ~ms per file, an accidental cushion.
   _ (rest 5)
   _ (: s (get-file "src/one.md") q (open (dir + "/src/one.md") "w") _ (say q s) (close q))
   n3 (build conf)
   _ (assert (n1 = 5) (n2 = 0) (n3 = 2))

   ; --- and the dev-server gate over it -------------------------------------------
   ; `refresh` is what the server calls per request: a source fingerprint, and `build`
   ; only when it moved. it must never answer work for an untouched tree (that was the
   ; 1.5s page load), and must never MISS a real edit (that would be a dev server that
   ; ignores you). the second is the one that matters, so it is checked both ways.
   box (tablet 0)
   r1 (refresh conf box)                                 ; first look -- nothing remembered yet
   r2 (refresh conf box)
   _ (rest 5)                                       ; same coarse-clock care as one.md above
   _ (: s (get-file "src/two.md") q (open (dir + "/src/two.md") "w") _ (say q s) (close q))
   r3 (refresh conf box)
   r4 (refresh conf box)
   _ (assert (0 = r1) (0 = r2) (2 = r3) (0 = r4))

   ; ..and re-registering must not GROW the card. `build` declares its recipes every call,
   ; and cook's merge used to append prerequisites each time -- so a dev server that
   ; rebuilds per source change climbed 28 ingredients a rebuild, forever, and `prep`
   ; walked all of them. unbounded memory and quadratic time out of a server sitting idle.
   cards (from 'cook 'cards)
   (icount _) (tally (cap (peep cards (dir + "/site/index.html") ())))
   n0 (icount ())
   _ ((: (again k) (? (k < 6) (: _ (build conf) (again (k + 1))) 0)) 0)
   _ (assert (0 < n0) (n0 = (icount ())))

   one (get-file "site/one.html")
   two (get-file "site/two.html")
   three (get-file "site/three.html")
   idx (get-file "site/index.html")

   ; --- ANCHORS: every toc target exists as an id on the page ----------------------
   ; the property that says "one parse, two uses". it holds for the fixture and for
   ; every page of this tree's own docs, built below.
   (anchored? h) (: ids (finds h "id=\"")
                    tgts (finds h "href=\"#")
                    (? (! (two? tgts)) 1 (: bad (filter (\ t (! (has? ids t))) tgts) (! (two? bad)))))
   _ (assert
      (anchored? one) (anchored? two) (anchored? three)
      ; and it is not vacuous: page one really has a contents, with the head-1 left
      ; out of it (it became the title) and both duplicate headings in.
      ((tally (finds one "href=\"#")) = 2)
      ; SLUGS: deduped, stable, and never repeated on a page
      (has? (finds one "id=\"") "one")
      (has? (finds one "id=\"") "shared-name")
      (has? (finds one "id=\"") "shared-name-1")
      (uniq? (finds one "id=\"")))

   ; --- RELINK: rewritten inside the site, untouched outside ------------------------
   hrefs (finds one "href=\"")
   _ (assert
      (has? hrefs "two.html")                    ; in the site -> .html
      (has? hrefs "archive/old.md")              ; not built -> left alone
      (has? hrefs "https://example.com/x.md")    ; absolute -> left alone
      (! (has? hrefs "two.md")))

   ; --- TITLE: the three sources, in order ------------------------------------------
   _ (assert
      (0 < tally (finds one "<h1 id=\""))        ; from the head-1
      (has? (finds two "content=\"") "Two From Meta")
      ((from 'papel 'ptitle) ((from 'lapiz 'mdread) (get-file "src/two.md")) "x") = "Two From Meta"
      ((from 'papel 'ptitle) ((from 'lapiz 'mdread) (get-file "src/three.md")) "three.md") = "three.md")

   ; --- the INDEX lists every page, each with its blurb ------------------------------
   _ (: ih (finds idx "href=\"")
        _ (assert (has? ih "one.html") (has? ih "two.html") (has? ih "three.html")
                  ; the blurb is the first paragraph, clipped on a word boundary
                  (0 < tally ((from 'papel 'blurb) ((from 'lapiz 'mdread) (get-file "src/one.md"))))))

   ; --- the slug is a function of the text, and nothing else -------------------------
   _ (: slug (from 'papel 'slug)
        (assert ((slug "Hello, World!") = "hello-world")
                ((slug "  --- ") = "section")            ; nothing left -> a name, never ""
                ((slug "A") = (slug "a"))))

   ; --- the real thing: this tree's own docs -----------------------------------------
   ; 28 markdown files through the whole pipe, and the anchor property has to hold on
   ; every page of it. README.md is deliberately in: it is the widest markdown here.
   rconf (: t (tablet 0)
            _ (pin t 'out (dir + "/love"))
            _ (pin t 'title "love")
            _ (pin t 'src '("README.md" "doc"))
            _ (pin t 'self "crew/papel/papel.l")
            _ (pin t 'quiet 1)
            t)
   rn (build rconf)
   _ (assert
      (10 < rn)                                   ; it built a real site
      (0 = (build rconf)))                        ; ..and is at once up to date
   _ (: (page f) (: q (open (dir + "/love/" + f) "r") (? q (slurp q) ""))
        _ (each (filter (\ f ((from 'papel 'ends?) f ".html")) (readdir (dir + "/love")))
                (\ f (: h (page f)
                        (assert (0 < tally h)
                                ((from 'papel 'ends?) h "</html>\n")
                                (anchored? h)))))
        0)

   _ (puts "crew/papel: ok -- the static site (anchors, slugs, relink, titles, freshness)
")
   0)
