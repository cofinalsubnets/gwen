#!/usr/bin/env -S love -l
; papel -- the static site: a tree of markdown becomes a tree of html, and neither
; the rendering nor the rebuilding is papel's own idea.
;
;   lapiz  reads each page into the AST and shows it as html      (crew/lapiz/lapiz.l)
;   cook   decides which pages are stale and runs only those      (crew/cook/cook.l)
;   papel  is what is left: the SITE -- titles, anchors, contents, cross-links, the page
;          template -- the things that are true of a page only because of the other pages.
;
; that split is the whole design. papel declares one cook RECIPE per page (out/site/x.html
; from doc/x.md) and hands the lot to cook, so an incremental site build is not a feature
; papel implements -- it is cook's ordinary job, arriving for free. and papel never parses
; markdown or emits a paragraph: it asks lapiz. what it does own is the SITE-LEVEL reading
; of the AST, and each piece of that is a small walk over the middle:
;
;   TITLE     the first head-1, else the meta `title`, else the file name.
;   ANCHORS   every head gets a stable id (`slug`, deduped), so a heading is linkable.
;   CONTENTS  the same heads become the page's toc -- one parse, two uses.
;   BLURB     the first paragraph, clipped, becomes the page's line on the index.
;   RELINK    a `[x](hatch.md)` link is rewritten to hatch.html -- but ONLY when that page
;             is in the site. a link out of the site is left exactly as written, so papel
;             cannot invent a dead link that markdown did not already have.
;
; the index page is built the same way as any other: papel makes an AST (a head and a
; deflist of link/blurb pairs) and hands it to lapiz to show. there is no html in papel
; except the page TEMPLATE and one line -- the anchored heading -- which is the single
; node papel renders itself, because lapiz has nowhere to put an id. everything else is
; `htb`, lapiz's own block shower.
;
; usage:  papel [-o OUT] [-t TITLE] [SRC ..]        (with the `papel` script on PATH)
;     or: love -l crew/papel/papel.l [-o OUT] [SRC ..]
; SRC is a .md file or a directory of them; with none, this tree's own docs (README.md
; and doc/). OUT defaults to out/site. papel writes OUT/<name>.html per source, OUT/
; papel.css, and OUT/index.html -- the last only when no source is itself an index.md.

; NO BRACKETS HERE: the runtime supplies the layer (a module consumer says
; (use 'papel); a -l load rides the session layer, where the seat reads bare).

; --- the modules papel stands on ---------------------------------------------------------
; papel loads its siblings ITSELF rather than being handed them: two tool files cannot both
; be -l'd, since each one's seat would fire on the other's command line, so the `papel`
; script -l's this file alone. the crew root comes off our own seat word, which means an
; installed script pointing at the source tree finds its siblings wherever that tree is --
; `want` hands the loader that PATH ((use "crew/lapiz/lapiz.l")), which loads it as a
; module and splices it. a module already registered (the gate, a host embedding) is
; left alone. `root` and `bn` are exactly the kind of name a leak makes dangerous, and
; they stay wherever this file's layer is -- never orth.
(: (bn p)   (: n (tally p)
               (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
               (go 0 0))
   (dirof p) (: n (tally p)
                (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p 0 r))
                (go 0 0))
   (findme a) (? (! (two? a)) ()                     ; the word that named this file, if any
                 (: b (bn <a) (|| (b = "papel") (b = "papel.l"))) <a
                 (findme >a))
   (up p)     (: n (tally p) (? (0 < n) (dirof (snip p 0 (n - 1))) p))   ; "a/b/" -> "a/"
   ; ..and follow a SYMLINK to it. `papel` installs the way cook does, as a link on PATH
   ; pointing at this source file, so the name we were invoked by lives in ~/.local/bin
   ; and the siblings do not. readlink is what turns the link back into the source tree.
   root (: me (findme >cmdline)
             tgt (? me (: r (readlink me) (? r r me)) ())
             (? tgt (up (dirof tgt)) "crew/"))                           ; crew/papel/papel.l -> crew/
   ; want: the sibling module, wherever this papel lives -- beside this file in
   ; the source tree (root came off our own seat), else by NAME through the
   ; loader's walk (an installed love's nest: ~/.love/lib/love/<nm>.l). the tree
   ; lane probes first, so a tree papel running under an installed love reads
   ; its OWN siblings, never the nest's older copies.
   (want nm f) (? (from nm) 1
                  (: q (open (root + f) "r")
                     (? q (: _ (close q) _ (use (root + f)) 1)
                        (: _ (use nm) 1))))
   _ (want 'lapiz "lapiz/lapiz.l")
   _ (want 'cook "cook/cook.l"))

; --- the two seams -------------------------------------------------------------------
; from lapiz, the DOCUMENT surface (read a page, show a page) plus the three pieces the
; template needs to render one node itself: `hq` (escape), `hti` (an inl), `htb` (a block).
; papel renders exactly ONE node -- an anchored head -- and defers every other to `htb`.
; from cook, the two verbs a build is made of.
(: L (from 'lapiz)
   mdread (L 'mdread)  htshow (L 'htshow)  htb (L 'htb)  hti (L 'hti)  hq (L 'hq)  mget (L 'mget)
   C (from 'cook)
   recipe (C 'recipe)  prepped (C 'prepped))

; --- little utils (papel's own: it leans on lapiz for DOCUMENTS, not for lists) --------
(: (nth l k)     (? (k = 0) <l (nth >l (k - 1)))
   (strcat l)    (foldl (+) "" l)
   (joins l sp)  (? (! (two? l)) "" (! (two? >l)) <l (<l + sp + joins >l sp))
   (ends? s t)   (: n (tally s) m (tally t) (&& (m <= n) ((snip s (n - m) n) = t)))
   (sub? s t)    (: n (tally s) m (tally t)
                    ((: (g i) (? ((i + m) <= n) (? ((snip s i (i + m)) = t) 1 (g (i + 1))) 0)) 0))
   (base p)      (: n (tally p)
                    (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
                    (go 0 0))
   (numof s)     ((: (g i acc) (? (&& (i < tally s) (&& (48 <= (s i)) ((s i) <= 57)))
                                  (g (i + 1) (acc * 10 + ((s i) - 48))) acc)) 0 0)
   (slurpf f)    (: q (open f "r") (? q (slurp q) ""))
   (spitf f s)   (: q (open f "w") _ (? q (: _ (say q s) (close q)) 0) (? q 1 0))
   ; a directory answers a list from readdir, a file answers nothing -- that IS the test,
   ; and it needs no mode bits (love has no bitwise and to pick S_IFDIR out of stat's mode).
   (dir? p)      (two? (readdir p))
   ; mkdir every prefix, ignoring an already-there (mkdir answers an errno, and we do not
   ; care which -- a real failure surfaces at the first open for writing). it walks the
   ; path rather than splitting it, so a LEADING SLASH survives: splitting on / and
   ; rejoining quietly turns /tmp/x into tmp/x, and the whole build then writes nowhere.
   (mkdirp p)    (: n (tally p)
                    (go i) (? (i < n) (: _ (? (&& (0 < i) ((p i) = 47)) (mkdir (snip p 0 i) 493) 0)
                                          (go (i + 1)))
                              (? (0 < n) (mkdir p 493) 0))
                    (go 0)))

; --- slugs: a heading becomes an id ----------------------------------------------------
; lowercase, runs of anything-else collapsed to one dash, the ends trimmed. two headings
; that spell the same slug are deduped by a counter, so an anchor is stable for a given
; page even when its text is not unique.
(: (down c)   (? (&& (65 <= c) (c <= 90)) (c + 32) c)
   (alnum? c) (|| (&& (48 <= c) (c <= 57)) (|| (&& (65 <= c) (c <= 90)) (&& (97 <= c) (c <= 122))))
   (squash s) ((: (g i acc prev) (? (i < tally s)
                                    (: c (s i)
                                       (? (&& (c = 45) (prev = 45)) (g (i + 1) acc c)
                                          (g (i + 1) (acc + string c) c)))
                                    acc)) 0 "" 0)
   (trimd s)  (: n (tally s)
                 a ((: (f i) (? (&& (i < n) ((s i) = 45)) (f (i + 1)) i)) 0)
                 b ((: (f i) (? (&& (a < i) ((s (i - 1)) = 45)) (f (i - 1)) i)) n)
                 (snip s a b))
   (slug s)   (: raw ((: (g i acc) (? (i < tally s)
                                      (: c (s i) (g (i + 1) (acc + (? (alnum? c) (string (down c)) "-"))))
                                      acc)) 0 "")
                 out (trimd (squash raw))
                 (? (0 < tally out) out "section")))

; --- reading the middle: the site-level facts of one page ------------------------------
; every one of these is a walk over the AST, which is the point of having one.
(: (itext l) (strcat (map (\ x (: t <x
                                  (? (id? t 'text) <>x
                                     (id? t 'code) <>x
                                     (|| (id? t 'strong) (|| (id? t 'em) (id? t 'link))) (itext <>x)
                                     ""))) l))
   (head? b)  (id? <b 'head)
   (pmeta d)  (: m (filter (\ b (id? <b 'meta)) d) (? (two? m) (<>(<m)) ()))
   (pbody d)  (filter (\ b (! (id? <b 'meta))) d)
   ; the heads, each with its deduped slug: ((level slug label) ..). ONE parse feeds both
   ; the anchors in the body and the toc beside it, so the two cannot drift apart.
   (pheads d) ((: (g l seen acc)
                  (? (! (two? l)) (rev acc)
                     (: b <l
                        (? (head? b)
                           (: lab (itext (nth b 2))
                              s0 (slug lab)
                              k (peep seen s0 0)
                              sn (? (0 < k) (s0 + "-" + show k) s0)
                              _ (pin seen s0 (k + 1))
                              (g >l seen (link `((<>b) sn lab) acc)))
                           (g >l seen acc)))))
               d (tablet 0) ())
   ; the title: the first head-1, else the meta `title`, else the caller's fallback.
   (ptitle d dflt) (: h1 (filter (\ b (&& (head? b) ((<>b) = 1))) d)
                      mt (mget (pmeta d) 'title)
                      (? (two? h1) (itext (nth <h1 2))
                         (0 < tally mt) mt
                         dflt))
   ; the blurb: the first paragraph, clipped on a word boundary. () when the page has none.
   (clip s n) (: t (tally s)
                 (? (t <= n) s
                    (: b ((: (f i) (? (&& (0 < i) (! ((s (i - 1)) = 32))) (f (i - 1)) i)) n)
                       ((snip s 0 (? (0 < b) (b - 1) n)) + "..."))))
   (blurb d)  (: ps (filter (\ b (id? <b 'para)) d)
                 (? (two? ps) `(`('text (clip (itext (<>(<ps))) 160))) ())))

; --- cross-links: .md becomes .html, but only inside the site --------------------------
; a url is rewritten iff it is a bare relative `*.md` naming a page the site actually
; builds. anything absolute, anchored, or pointing outside stays exactly as written --
; papel will not manufacture a dead link that the markdown did not already have.
(: (mdurl known u)
   (? (sub? u "://") u
      (! (ends? u ".md")) u
      (: b (base u)
         (? (peep known b 0) ((snip u 0 (tally u - 3)) + ".html") u)))
   (rli f l)  (map (\ x (: t <x
                           (? (id? t 'link) `('link (rli f <>x) (f (nth x 2)))
                              (|| (id? t 'strong) (id? t 'em)) `(t (rli f <>x))
                              x))) l)
   (rlb f b)  (: t <b
                 (? (id? t 'para)    `('para (rli f <>b))
                    (id? t 'head)    `('head (<>b) (rli f (nth b 2)))
                    (id? t 'bullets) `('bullets (map (\ i (rli f i)) <>b))
                    (id? t 'numbers) `('numbers (map (\ i (rli f i)) <>b))
                    (id? t 'deflist) `('deflist (map (\ it `((rli f <it) (rli f <>it))) <>b))
                    (id? t 'quote)   `('quote (map (\ x (rlb f x)) <>b))
                    b))
   (relink f d) (map (\ b (rlb f b)) d))

; --- showing: lapiz shows every block but one ------------------------------------------
; the anchored head is the ONE node papel emits itself, because lapiz's html has nowhere
; to put an id and inventing a node for it would push a site's concern into the lens. the
; heads arrive already slugged, in document order, so the walk just consumes them in step.
(: (hhead lev sl inl) (: h ("h" + show lev)
                         ("<" + h + " id=\"" + hq sl + "\">" + hti inl + "</" + h + ">"))
   (pshow d hs) ((: (g l hs acc)
                    (? (! (two? l)) (joins (rev acc) "\n")
                       (: b <l
                          (? (head? b) (g >l >hs (link (hhead <(<hs) (<>(<hs)) (nth b 2)) acc))
                             (g >l hs (link (htb b) acc))))))
                 d hs ())
   ; the toc: the same heads, minus the one that became the title. a page with fewer than
   ; two of them gets none -- a one-item contents is furniture, not navigation.
   (ptoc d hs) (: rest (? (&& (two? hs) (1 = <(<hs))) >hs hs)
                  (? (! (two? >rest)) ""
                     ("<ul>\n" + strcat (map (\ h ("<li class=\"l" + show <h + "\"><a href=\"#"
                                                   + hq (<>h) + "\">" + hq (nth h 2) + "</a></li>\n")) rest)
                      + "</ul>")))
   (pmetah kvs) (? (two? kvs) (htb `('meta kvs)) ""))

; --- the page template ------------------------------------------------------------------
(: (wrap conf ttl metah toch body)
   (: site (peep conf 'title "papel")
      ("<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
       + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       + "<title>" + hq ttl + (? (ttl = site) "" (" &middot; " + hq site)) + "</title>\n"
       + "<link rel=\"stylesheet\" href=\"papel.css\">\n"
       + (? (0 < tally metah) (metah + "\n") "")
       + "</head>\n<body>\n"
       + "<header><a href=\"index.html\">" + hq site + "</a></header>\n"
       + "<main>\n"
       + (? (0 < tally toch) ("<nav class=\"toc\">\n" + toch + "\n</nav>\n") "")
       + "<article>\n" + body + "\n</article>\n</main>\n"
       + "<footer>markdown through <b>lapiz</b>, built by <b>cook</b>, laid out by <b>papel</b></footer>\n"
       + "</body>\n</html>\n")))

; --- the stylesheet ---------------------------------------------------------------------
; one file, linked by every page, written by the same cook recipe machinery as the pages
; (so editing this string rebuilds the site). it answers to the reader's theme.
(: css
   ":root{--bg:#fffdf9;--fg:#1f1c19;--dim:#6f6862;--rule:#e6e0d6;--link:#8a4b2a;--pre:#f4f0e9}
@media (prefers-color-scheme:dark){:root{--bg:#16151a;--fg:#e8e4dd;--dim:#9a938b;--rule:#2e2b33;--link:#e0a06a;--pre:#1e1d24}}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.65 ui-serif,Georgia,serif}
header{border-bottom:1px solid var(--rule);padding:1rem 1.25rem;font-family:ui-sans-serif,system-ui,sans-serif}
header a{color:var(--fg);text-decoration:none;font-weight:600;letter-spacing:.02em}
main{display:flex;gap:2.5rem;align-items:flex-start;max-width:64rem;margin:0 auto;padding:2rem 1.25rem}
article{min-width:0;flex:1}
footer{border-top:1px solid var(--rule);padding:1.25rem;color:var(--dim);font-size:.85rem;text-align:center}
a{color:var(--link)}
h1,h2,h3,h4,h5,h6{font-family:ui-sans-serif,system-ui,sans-serif;line-height:1.25;margin:2rem 0 .6rem}
h1{margin-top:0;font-size:1.9rem}
h2{font-size:1.35rem;border-bottom:1px solid var(--rule);padding-bottom:.3rem}
h3{font-size:1.1rem}
p,ul,ol,dl,blockquote,pre{margin:0 0 1rem}
code{background:var(--pre);padding:.1em .35em;border-radius:3px;font:.88em/1.5 ui-monospace,Menlo,monospace}
pre{background:var(--pre);padding:.9rem 1rem;border-radius:6px;overflow-x:auto}
pre code{background:none;padding:0}
blockquote{border-left:3px solid var(--rule);margin-left:0;padding-left:1rem;color:var(--dim)}
hr{border:0;border-top:1px solid var(--rule);margin:2rem 0}
dt{font-weight:600;margin-top:.8rem}
dd{margin:0 0 .2rem 1.2rem;color:var(--dim)}
table{border-collapse:collapse}
.toc{position:sticky;top:1.5rem;flex:0 0 14rem;font-family:ui-sans-serif,system-ui,sans-serif;font-size:.85rem;line-height:1.5;border-left:1px solid var(--rule);padding-left:1rem}
.toc ul{list-style:none;margin:0;padding:0}
.toc li{margin:.15rem 0}
.toc a{color:var(--dim);text-decoration:none}
.toc a:hover{color:var(--link)}
.toc .l3{padding-left:.9rem}
.toc .l4,.toc .l5,.toc .l6{padding-left:1.8rem}
@media (max-width:52rem){main{display:block}.toc{position:static;border-left:0;border-bottom:1px solid var(--rule);padding:0 0 1rem;margin-bottom:1.5rem}}
")

; --- one page, and the index ------------------------------------------------------------
(: (outname s) (: b (base s) ((snip b 0 (tally b - 3)) + ".html"))
   (render conf known src dst)
   (: d0 (mdread (slurpf src))
      d  (relink (\ u (mdurl known u)) d0)
      body (pbody d)
      hs (pheads body)
      ttl (ptitle body (base src))
      _ (spitf dst (wrap conf ttl (pmetah (pmeta d)) (ptoc body hs) (pshow body hs)))
      dst)
   ; the index is a page like any other: papel builds an AST -- a head, then a deflist of
   ; link/blurb pairs -- and lapiz shows it. no second template, no html here at all.
   (index-doc conf srcs)
   (: site (peep conf 'title "papel")
      rows (map (\ s (: d (mdread (slurpf s))
                        `(`(`('link `(`('text (ptitle d (base s)))) (outname s))) (blurb d)))) srcs)
      `(`('head 1 `(`('text site))) `('deflist rows)))
   (windex conf srcs dst)
   (: _ (spitf dst (wrap conf (peep conf 'title "papel") "" "" (htshow (index-doc conf srcs)))) dst))

; --- the sources, and the build -----------------------------------------------------------
(: (mdsof p) (? (dir? p) (map (\ f (p + "/" + f)) (sort (filter (\ f (ends? f ".md")) (readdir p))))
                (ends? p ".md") `(p)
                ())
   (sources conf) (foldl (+) () (map mdsof (peep conf 'src ())))
   (conf0 _) (: t (tablet 0)
                _ (pin t 'out "out/site")
                _ (pin t 'title "papel")
                _ (pin t 'src ())
                _ (pin t 'self "crew/papel/papel.l")
                t)
   ; the BUILD is a declaration, not a loop: one recipe per page, one for the stylesheet,
   ; one for the index, and a phony `site` over the lot -- then cook decides what is stale.
   ; every output also depends on THIS FILE, since the template and the stylesheet live in
   ; it; edit either and the whole site is correctly out of date. (only when the file is
   ; really there -- cook quits on an ingredient that exists neither on disk nor as a rule.)
   ; the DEV-SERVER gate. before calling `build` at all, take a cheap fingerprint of
   ; the sources -- one `stat` nif call each -- and only call it when that changed:
   ; a no-op `build` still walks the whole recipe graph, and a page should serve in
   ; the sub-ms its serving actually costs. answers what `build` answers, 0 when
   ; nothing did. (cook's date is the same stat nif now; the fingerprint keeps the
   ; per-request path down to the sources alone.)
   ;
   ; comparing against a REMEMBERED fingerprint rather than against the outputs is what
   ; makes this safe to be coarse. the nif's mtime is milliseconds, so an edit landing in
   ; the same millisecond as our reading could be missed once -- but the very next
   ; request compares against that same reading and sees it. and `build` itself is
   ; untouched, so the decision that actually matters stays cook's, at cook's resolution.
   ; mtime AND size, because an edit can preserve either one alone.
   (fprint conf) (foldl (+) 0 (map (\ s (: st (stat s) (? (two? st) ((<>st) + <st) 0)))
                                   (sources conf)))
   (refresh conf box) (: f (fprint conf)
                         (? (f = (peep box 'f -1)) 0 (: _ (pin box 'f f) (build conf))))

   (build conf)
   (: out (peep conf 'out "out/site")
      srcs (sources conf)
      _ (? (two? srcs) 0 (scare 'papel "papel: no markdown sources"))
      _ (mkdirp out)
      known (: t (tablet 0) _ (each srcs (\ s (pin t (base s) 1))) t)
      self (peep conf 'self "")
      dep (? (stat self) `(self) ())
      dsts (map (\ s (out + "/" + outname s)) srcs)
      _ (each srcs (\ s (recipe (out + "/" + outname s) (link s dep)
                                `((\ _ (render conf known s (out + "/" + outname s)))))))
      sheet (out + "/papel.css")
      _ (recipe sheet dep `((\ _ (spitf sheet css))))
      idx (out + "/index.html")
      own? (peep known "index.md" 0)                 ; a real index.md wins; papel adds none
      _ (? own? 0 (recipe idx (srcs + dep) `((\ _ (windex conf srcs idx)))))
      _ (recipe 'site (dsts + `(sheet) + (? own? () `(idx))) ())
      ; cook's own `cook` prints its verdict; papel wants the NUMBER, so it takes the
      ; quiet entry. $ of a map is its key count, and cook marks an item shipped only when
      ; the recipe really advanced its mtime -- so this is exactly "files written".
      n $(prepped 'site)
      _ (? (peep conf 'quiet 0) 0
           (: _ (puts (? (0 < n) ("papel: " + show n + " written to " + out + "/")
                           ("papel: " + out + "/ is up to date")))
              (putc 10)))
      n))

; --- the command line ---------------------------------------------------------------------
(: _ (:
   (usage _) (: _ (puts "papel -- the static site (markdown -> html, through lapiz, built by cook)
usage: papel [-o OUT] [-t TITLE] [SRC ..]
  -o, --out DIR     where the site is written (default out/site)
  -t, --title NAME  the site name, used in the header and on the index (default papel)
  -q, --quiet       write the site and say nothing
  -s, --serve PORT  build, then hand OUT to kiosko and serve it on PORT
  SRC               a .md file or a directory of them; with none, README.md and doc/
")
                (quit 0))
   (need h r) (? (two? r) 0 (: _ (say err ("papel: " + h + " needs an argument")) _ (put err 10) (quit 2)))
   (papel-main as)
   (: _ (? (lit? (from 'papel)) (use 'papel) 0)   ; splice my REGISTERED self if I am one (cook-main's
                                                  ; guard); on a -l load my names already walk
      c (conf0 ())
      (loop ws) (? (! (two? ws)) 0
                   (: w <ws r >ws
                      (? (|| (w = "-o") (w = "--out"))   (: _ (need w r) _ (pin c 'out <r) (loop >r))
                         (|| (w = "-t") (w = "--title")) (: _ (need w r) _ (pin c 'title <r) (loop >r))
                         (|| (w = "-q") (w = "--quiet")) (: _ (pin c 'quiet 1) (loop r))
                         (|| (w = "-s") (w = "--serve")) (: _ (need w r) _ (pin c 'serve (numof <r)) (loop >r))
                         (|| (w = "-h") (w = "--help"))  (usage 0)
                         (: _ (pin c 'src ((peep c 'src ()) + `(w))) (loop r)))))
      _ (loop as)
      _ (? (two? (peep c 'src ())) 0 (pin c 'src '("README.md" "doc")))
      _ (build c)
      ; --serve: hand the finished directory to kiosko, which is a separate crew member and
      ; stays one -- papel does not know what a socket is, and kiosko does not know what
      ; markdown is. the whole integration is a directory, a port, and a thunk.
      ;
      ; the thunk is what makes it a DEV SERVER rather than a viewer: kiosko runs it before
      ; each client, and it is `refresh` -- a fingerprint of the sources, and `build` only
      ; when that moved. so an unchanged page load costs nothing and an edited one
      ; re-renders exactly what changed, between hitting save and hitting reload. quiet,
      ; because a rebuild per request is not news. the box carries the last fingerprint,
      ; seeded from the build we just did so the first request is not a false alarm.
      port (peep c 'serve 0)
      _ (? port
           (: _ (want 'kiosko "kiosko/kiosko.l")
              q (peep c 'quiet 0)
              _ (pin c 'quiet 1)
              box (: t (tablet 0) _ (pin t 'f (fprint c)) t)
              _ (? ((from 'kiosko 'serve) (peep c 'out "out/site") port q (\ _ (refresh c box))) 0
                   (: _ (say err ("papel: cannot listen on :" + show port)) _ (put err 10) (quit 1))))
           0)
      (quit 0))
   papel-main)

   papel-main _)

; the SEAT (cook's shape): fire papel-main iff this file -- or the installed `papel`
; script -- sits in the program seat of the real command line, on the args after it.
; body-having, so the machinery leaks nothing; papel-main reads BARE -- whichever
; layer holds this file's binds is still the head here.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "papel") (b = "papel.l")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a) (? (me? <>a) (link 1 >>a) (seat >>a)) ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) (papel-main >s) ()))
