; lib/hueweb.l -- the .l syntax as a stylesheet and as painted html, out of
; crew/vi/hue.l's class table and crew/vi/config.l's theme. the third reader of that
; one table: the painter in crew/vi/core.l's vframe, the vim syntax (tools/hue2vim.l),
; and this. nothing here knows what a comment or a sigil is -- it asks hue, the way the
; other two do, so a class added to that table arrives in all three unannounced.
;
;   (hw-css)             the stylesheet: one rule per class, the frame, and just
;                        enough prose styling for a rendered document -- `love
;                        serve` has no papel.css to lean on
;   (hw-body path src)   that source as a <pre>, one span per line
;   (hw-page ttl hrefs home body)   ..in a page, with its stylesheets and a home link
;
; the split the theme already makes is what makes this cheap. hue.l says "a comment is
; a Comment"; crew/vi/config.l says "in molokayo a Comment is #75715E". the vim
; generator uses the first half and leaves the colour to the reader's colorscheme; a
; web page has no colorscheme, so this joins both and the page wears the editor's
; theme by construction.
;
; the runs are sparse: hue-lex answers only the spans it claims, so the emitter fills
; the gaps with plain text rather than assuming they tile the line. and it carries
; `in`, the string-open state, from each line to the next, or a multi-line literal
; (papel's forty-line css) paints as code from its second line on.
;
; no papel here, and that is what makes `love serve` possible: hue.l, config.l and
; kiosko are already in the artifact (the Makefile's distfiles) and papel is not. so
; the page template is this file's own, small and self-contained, and the seed can
; paint its own source with nothing on the box.
;
; the vocabulary is the host's: the word lanes ask `names`, `macros` and `values` of
; the running binary, so under LOVE_NO_IMAGE (the egg boot) the answer is the
; compiler's own internals rather than the language that shipped -- a generator
; writing files must clear it, the way the syntax rule does.
;
; hue rides module 'vi. the images that cat crew/vi/{config,hue}.l registered it; a
; tool outside those images says (use "crew/vi/config.l") and (use "crew/vi/hue.l")
; first. the use below sits inside this module, so it splices vi for this file's
; compile alone: at a catted file's top level it would hand the session vi's ninety
; -odd names.
(module (hueweb hw-css hw-body hw-page)
(use 'vi)
(:
   (hw-cat l)   (foldl (+) "" l)
   ; the three characters html cannot hear otherwise
   (hw-q s) (:- (g 0 "")
                (g i acc) (? (i < #s)
                             (: c (s i)
                                (g (i + 1) (acc + (? (c = 38) "&amp;" (c = 60) "&lt;"
                                                     (c = 62) "&gt;" (string c)))))
                             acc))
   ; the final newline ends the last line, it does not open another: a file of n lines
   ; that ends in one would otherwise paint an n+1'th empty row.
   (hw-lines s) (:- (g 0 0 ())
                    (g i st acc) (? !(i < #s) (st < i ? (rev (snip s st i . acc)) (rev acc))
                                    (s i = 10) (g (i + 1) (i + 1) (snip s st i . acc))
                                    (g (i + 1) st acc)))

   ; --- the stylesheet -------------------------------------------------------
   ; one rule per class, coloured by the group that class names. a class whose
   ; group the theme does not carry gets no rule at all rather than a guess.
   (hw-row g) (:- (f hue-theme) (f l) (? (atom? l) () (id? <<l g) <l (f >l)))
   (hw-faces l) (hw-cat (map (f \ ? (id? f 'bold) ";font-weight:700"
                                    (id? f 'italic) ";font-style:italic" "") l))
   (hw-rule r) (: t (hw-row (hue-grp r))
                  (? !(two? t) ""
                     (".hue .l-" + string (hue-cls r) + "{color:" + (t 2)
                      + hw-faces (t 3) + "}\n")))
   ; the frame: a slab to sit on, and line numbers out of a css counter -- so the
   ; markup carries one span per line and no number, and selecting the code copies
   ; the code alone. the page's own colours answer to the reader's theme; the
   ; syntax colours are molokayo either way, which is what the editor shows.
   hw-frame
   ":root{--bg:#fffdf9;--fg:#1f1c19;--dim:#6f6862;--rule:#e6e0d6;--link:#8a4b2a;--pre:#f4f0e9}
@media (prefers-color-scheme:dark){:root{--bg:#16151a;--fg:#e8e4dd;--dim:#9a938b;--rule:#2e2b33;--link:#e0a06a;--pre:#1e1d24}}
.hue{background:var(--pre);padding:1rem 0;border-radius:6px;overflow-x:auto;counter-reset:ln;
  font:.82rem/1.55 ui-monospace,Menlo,monospace}
.hue .ln{display:block;padding:0 1rem 0 0;white-space:pre}
.hue .ln::before{counter-increment:ln;content:counter(ln);display:inline-block;width:3.4rem;
  padding-right:1rem;text-align:right;color:var(--dim);opacity:.55;-webkit-user-select:none;user-select:none}
.hue .ln:target{background:rgba(127,127,127,.16)}
.srcnav{font-size:.85rem;margin:0 0 1rem;color:var(--dim)}
.srcnav a{color:var(--link)}
body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.65 ui-serif,Georgia,serif}
header{border-bottom:1px solid var(--rule);padding:1rem 1.25rem;font-family:ui-sans-serif,system-ui,sans-serif}
header a{color:var(--fg);text-decoration:none;font-weight:600}
main{max-width:64rem;margin:0 auto;padding:2rem 1.25rem}
a{color:var(--link)}
h1,h2,h3,h4,h5,h6{font-family:ui-sans-serif,system-ui,sans-serif;line-height:1.25;margin:2rem 0 .6rem}
h1{margin-top:0;font-size:1.7rem}
h2{font-size:1.3rem;border-bottom:1px solid var(--rule);padding-bottom:.3rem}
p,ul,ol,dl,blockquote,pre{margin:0 0 1rem}
code{background:var(--pre);padding:.1em .35em;border-radius:3px;font:.88em/1.5 ui-monospace,Menlo,monospace}
pre{background:var(--pre);padding:.9rem 1rem;border-radius:6px;overflow-x:auto}
pre code,.hue code{background:none;padding:0}
blockquote{border-left:3px solid var(--rule);margin-left:0;padding-left:1rem;color:var(--dim)}
dt{font-weight:600;margin-top:.8rem}
dd{margin:0 0 .2rem 1.2rem;color:var(--dim)}
hr{border:0;border-top:1px solid var(--rule);margin:2rem 0}
"
   (hw-css _) (hw-frame + hw-cat (map hw-rule hue-classes))

   ; --- one file, painted ----------------------------------------------------
   ; a line becomes its runs, in order, with the unclaimed gaps between them kept
   ; as plain text. `in` rides from line to line for the string carry.
   (hw-span c a b s) ("<span class=\"l-" + string c + "\">" + hw-q (snip s a b) + "</span>")
   (hw-paint bk s in)
   (: r (hue-lex bk s in 1)
      (go rs i acc)
       (? !(two? rs) [(acc + hw-q (snip s i #s)) <>r]
          (<rs @ [c a b]
           (go >rs b (acc + hw-q (snip s i a) + hw-span c a b s))))
      (go <r 0 ""))
   (hw-body path src)
   (: bk (hue-book 0)
      (go ls k in acc)
       (? !(two? ls) acc
          (: p (hw-paint bk <ls in)
             (go >ls (k + 1) <>p
                 (acc + "<span class=\"ln\" id=\"L" + show k + "\">" + <p + "\n</span>"))))
      ("<pre class=\"hue\">" + (go (hw-lines src) 1 0 "") + "</pre>"))

   ; --- the page -------------------------------------------------------------
   ; small on purpose: a title, whatever stylesheets the caller wants linked, a
   ; home link, and the body. the site passes papel.css so a painted page and a
   ; doc page are one design; `love serve` passes its own and nothing else.
   (hw-page ttl hrefs home body)
   ("<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
    + "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    + "<title>" + hw-q ttl + "</title>\n"
    + hw-cat (map (h \ "<link rel=\"stylesheet\" href=\"" + hw-q h + "\">\n") hrefs)
    + "</head>\n<body>\n"
    + (0 < #home ? ("<header><a href=\"" + hw-q home + "\">love</a></header>\n") "")
    + "<main>\n<article>\n" + body + "\n</article>\n</main>\n</body>\n</html>\n"))
) ; module hueweb
