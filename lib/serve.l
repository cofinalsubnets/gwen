; lib/serve.l -- `love serve`: the tree, in a browser, painted on the way out.
;
;   love serve [-p PORT] [DIR]      DIR defaults to the working directory
;
; kiosko already serves a directory and lists what it holds. what this adds is one
; HOOK, and three answers through it:
;
;   *.l        the PAINTED source -- syntax coloured, one span per line, every
;              line linkable (lib/hueweb.l)
;   *.md       the RENDERED document -- markdown through lapiz's html surface
;   /hue.css   the stylesheet that colours both
;
; everything else is kiosko's ordinary static serving, directory listings and all,
; which is where the tree structure comes from. nothing is written to disk and
; nothing is generated ahead of time.
;
; ⚠ NO papel HERE, so a rendered .md is the DOCUMENT and not a site page: no
; title, no anchors, no contents nav, no cross-link rewriting -- those are papel's
; site-level readings and papel is not in the artifact. `make site` is still what
; builds a site. this is what a browser wants of a tree it is reading.
;
; ⚠ NOTHING IS CHECKED IN AND NOTHING IS CACHED TO DISK. a painted file is built
; at request time, ~35 ms for the biggest crew file and under 10 for a typical
; one, and then held in a box keyed by the file's mtime and size -- so the second
; request is free and an edit invalidates its own page. a tree of painted html in
; git would be a copy that rots; this is the same bytes, made when asked.
;
; ⚠ IT READS kiosko AND hueweb BARE, for the reason lib/hueweb.l reads hue bare:
; in the artifact they are CATTED, not registered as modules, so an accessor would
; answer () -- and a () applied to operands is 1, which is truthy. bare resolves
; down the splice in both worlds.
;
; ⚠ IT IS THE ARTIFACT'S OWN ERRAND. hue.l, config.l and kiosko are already in
; crew/build.mk's distfiles, so the seed carries everything this needs -- which is
; why the painter (lib/hueweb.l) borrows no template from papel, which is not in
; there. `love serve` on a seed binary with nothing else on the box is the point.
(:
   (sv-nth l k) (? (k = 0) <l (sv-nth >l (k - 1)))
   (sv-ends? s t) (: n (tally s) m (tally t) ((m <= n) && (snip s (n - m) n = t)))
   (sv-rdf p) (: q (open p "r") (? q (: s (slurp q) _ (close q) (? (string? s) `(1 s) ())) ()))
   ; the directory a path sits in, as a link back up: "/a/b/c.l" -> "/a/b/"
   (sv-up p) (: n (tally p)
                (g k r) (? (k < n) (g (k + 1) (? (p k = 47) (k + 1) r)) (snip p 0 r))
                (g 0 0))

   ; the stamp a cache entry is keyed by: an edit that changes neither mtime nor
   ; size is an edit nobody can see anyway (papel's fingerprint takes the same bet,
   ; and for the same reason -- the alternative is re-reading the file to decide
   ; whether to re-read the file).
   (sv-stamp p) (: st (stat p) (? (two? st) ((<>st) + (<st) * 7) 0))

   ; ..and the box it lives in: `(stamp html) per path, replaced when the stamp moves.
   (sv-cached box p make)
   (: k (sv-stamp p)
      e (peep box p ())
      (? (&& (two? e) (<e = k)) (<>e)
         (: h (make ())
            _ (pin box p `(k h))
            h)))

   ; the page for one source file: a line back to its directory, then the paint.
   (sv-page path body)
   (hw-page path '("/hue.css") "/"
            ("<p class=\"srcnav\"><a href=\"" + sv-up path + "\">"
             + sv-up path + "</a> &middot; " + path + "</p>\n" + body))

   ; the page for one markdown file: lapiz reads it and shows it as html, and the
   ; same small template holds it. ⚠ `mdread`/`htshow` BARE, for the reason kiosko
   ; and hueweb are bare -- in the artifact lapiz is catted, not registered.
   (sv-doc path src)
   (hw-page path '("/hue.css") "/"
            ("<p class=\"srcnav\"><a href=\"" + sv-up path + "\">"
             + sv-up path + "</a> &middot; " + path + "</p>\n" + htshow (mdread src)))

   ; THE HOOK. `(ctype body) to serve, () to say "not mine" -- and everything that
   ; is not a .l, a .md or the stylesheet is exactly that.
   (sv-make root box p)
   (: md? (sv-ends? p ".md")
      (? (p = "/hue.css") `("text/css; charset=utf-8" (hw-css 0))
         (! (md? || (sv-ends? p ".l"))) ()
         (: f (root + p)
            r (sv-rdf f)
            (? !(two? r) ()                       ; not there: let kiosko say 404
               `("text/html; charset=utf-8"
                 (sv-cached box f (\ _ (? md? (sv-doc p (<>r))
                                           (sv-page p (hw-body p (<>r)))))))))))

   (sv-usage _)
   (: _ (say err "love serve -- the tree in a browser, painted on the way out
usage: love serve [-p PORT] [DIR]
  -p, --port PORT   the port to listen on (default 8080)
  DIR               the directory to serve (default .)
a .l is painted and a .md rendered at request time; /hue.css colours both.
")
      (quit 0))

   (sv-num s) ((: (g i acc) (? ((i < tally s) && (48 <= s i) && (s i <= 57))
                               (g (i + 1) (acc * 10 + (s i - 48))) acc)) 0 0)

   (serve-main as)
   (: box (tablet 0)
      (loop ws port dir)
       (? !(two? ws) `(port dir)
          (: w <ws r >ws
             (? ((w = "-p") || (w = "--port"))
                 (? (two? r) (loop >r (sv-num <r) dir) (sv-usage 0))
                ((w = "-h") || (w = "--help")) (sv-usage 0)
                (loop r port w))))
      c (loop as 8080 ".")
      port <c
      dir (<>c)
      ; ⚠ BARE, never (from 'kiosko ..). in the artifact kiosko is CATTED, not
      ; registered, so the accessor answers () -- and () applied to five operands is
      ; 1, which is truthy, so the miss would read as a server that started and the
      ; command would just exit 0. bare resolves down the splice either way.
      _ (? (serve5 dir port 0 () (\ p (sv-make dir box p))) 0
           (: _ (say err ("serve: cannot listen on :" + show port)) _ (put err 10) (quit 1)))
      (quit 0))

   ; the verb, and the seat: `love serve ..` on the artifact, or this file named on
   ; a command line. kiosko's own shape, and the reason `serve` needs no script.
   _ (pins (from 'verbs 'tab) 'serve serve-main)
   s ((from 'verbs 'fire) ["serve" "serve.l"])
   _ (? (two? s) (serve-main >s) ()))
