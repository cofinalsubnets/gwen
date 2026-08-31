; --- test/host/serve.l: `love serve`'s hook (lib/serve.l) ---
; not in the test/*.l corpus (it loads crew modules and splices them).
;
; the server half is kiosko's and gated there (test/host/kiosko.l drives a real
; socket, the maker hook included). what is gated here is the maker: the four
; things `love serve` answers for and the ones it refuses, driven directly --
; no port, no socket, no timing, because none of that is this file's claim.
;
;   *.l        the painted source
;   *.l.md     that file's header comment, as a document
;   *.md       the rendered document
;   /hue.css   the stylesheet that colours both
;   anything else -> (), and kiosko does what it always did
; run:
;   cat test/00-init.l test/host/serve.l | out/host/love

(use "crew/vi/config.l")                    ; the theme: hue-theme
(use "crew/vi/hue.l")                       ; the class table, read bare by hueweb
(use 'lapiz)                                ; the document lens, read bare by serve
(use 'libra)                                ; ..and the header lifter, likewise
(use 'hueweb)
(use 'kiosko)
(use 'serve)

; --- the seam -----------------------------------------------------------------
(assert
 (member? 'serve (from ()))
 (member? 'serve-main (keys (from 'serve)))
 ; the verb is registered, which is what makes `love serve` a word on the artifact
 (lit? (peep (from 'verbs 'tab) 'serve ())))

; a scratch tree to answer for. the maker takes a root and a path and joins them
; itself, so the fixture is a real directory and the test asks for real paths.
(: dir "/tmp/servetest"
   _ (mkdir dir 493)
   (spit p s) (: q (open (dir + p) "w") _ (say q s) (close q))
   _ (spit "/x.l" "; a comment\n(: x 1)\n")
   _ (spit "/d.md" "# a head\n\nsome *prose*.\n")
   _ (spit "/plain.txt" "not mine\n")
   box (tablet 0)
   (mk p) (sv-make dir box p)
   (sub? s t) (: n (tally s) m (tally t)
                 ((: (g i) (? ((i + m) <= n) (? ((snip s i (i + m)) = t) 1 (g (i + 1))) 0)) 0))

   ; --- the three it answers for ----------------------------------------------
   _ (: r (mk "/x.l")
        _ (assert
           (two? r)
           (<r = "text/html; charset=utf-8")
           (sub? <>r "class=\"l-comment\"")        ; painted, by hue's own table
           (sub? <>r "id=\"L1\"")                  ; and every line is linkable
           (sub? <>r "/hue.css"))                  ; ..with the stylesheet that colours it
        0)
   _ (: r (mk "/d.md")
        _ (assert
           (two? r)
           (<r = "text/html; charset=utf-8")
           (sub? <>r "<h1>a head</h1>")            ; rendered, through lapiz
           (sub? <>r "<em>prose</em>"))
        0)
   ; `.l.md` is claimed before `.md`, and reads the .l with the suffix off. the
   ; two pages link to each other, which is the whole reason the nav line exists.
   _ (: r (mk "/x.l.md")
        _ (assert
           (two? r)
           (sub? <>r "<h1>/x.l</h1>")              ; the file names the document
           (sub? <>r "a comment")                  ; ..and its header is the document
           (sub? <>r "href=\"/x.l\""))            ; ..with the way back to the source
        0)
   ; a file with no header says so, rather than serving a blank page
   _ (: _ (spit "/bare.l" "(: x 1)\n")
        r (mk "/bare.l.md")
        _ (assert (two? r) (sub? <>r "no header comment"))
        0)
   _ (: r (mk "/hue.css")
        _ (assert
           (two? r)
           (<r = "text/css; charset=utf-8")
           (sub? <>r ".hue .l-comment{color:#"))   ; the theme join, not a guess
        0)

   ; --- and the ones it refuses -----------------------------------------------
   ; () is the whole of "not mine": kiosko then serves the file, or says 404, and
   ; neither answer is this file's to give. a maker that answered for everything
   ; would take the static server away.
   _ (assert
      (() = (mk "/plain.txt"))                       ; a file it does not claim
      (() = (mk "/"))                                ; the listing stays kiosko's
      (() = (mk "/nothing.l")))                      ; ..and a .l that is not there

   ; --- the cache --------------------------------------------------------------
   ; keyed by the file's mtime and size, in memory, never on disk. the second ask
   ; is the same value, and rewriting the file answers the new one.
   _ (: a (mk "/x.l")
        b (mk "/x.l")
        _ (assert (<>a = <>b))
        0)
   0)

(: _ (test_fin ())
   _ (puts "lib/serve: ok -- the maker (painted .l, its header, a rendered .md, the css, and ())
")
   0)
