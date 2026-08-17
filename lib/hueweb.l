; lib/hueweb.l -- the .l syntax as a STYLESHEET and as painted html, out of
; crew/vi/hue.l's class table and crew/vi/config.l's theme. the THIRD reader of
; that one table: the painter in crew/vi/core.l's vframe, the vim syntax
; (mk/tools/hue2vim.l), and this. nothing here knows what a comment or a sigil is
; -- it asks hue, the way the other two do, so a class added to that table arrives
; in all three without anyone being told.
;
;   (hw-css)             the stylesheet: one rule per class, the frame, and just
;                        enough prose styling for a rendered document -- `love
;                        serve` has no papel.css to lean on
;   (hw-body path src)   that source as a <pre>, one span per line
;   (hw-page ttl hrefs home body)   ..in a page, with its stylesheets and a home link
;
; the SPLIT the theme already makes is what makes this cheap. hue.l says "a
; comment is a Comment"; crew/vi/config.l says "in molokayo a Comment is
; #75715E". the vim generator uses the first half and leaves the colour to the
; reader's colorscheme; a web page has no colorscheme, so this joins both and the
; page wears the editor's theme by construction.
;
; ⚠ THE RUNS ARE SPARSE. hue-lex answers only the spans it CLAIMS -- a name in no
; vocabulary is in no run at all -- so the emitter fills the gaps with plain text
; rather than assuming the runs tile the line. and it carries `in`, the
; string-open state, from each line to the next, or a multi-line literal (papel's
; forty-line css) paints as code from its second line on.
;
; ⚠ NO PAPEL HERE, and that is what makes `love serve` possible: hue.l, config.l
; and kiosko are already in the artifact (crew/build.mk's distfiles) and papel is
; not. so the page template is this file's own, small and self-contained, rather
; than borrowed from the site generator -- and the seed can paint its own source
; with nothing on the box.
;
; ⚠ AND THE VOCABULARY IS THE HOST'S. the word lanes ask `names`, `macros` and
; `values` of the running binary, so under LOVE_NO_IMAGE (the egg boot) the answer
; is the compiler's own internals rather than the language that shipped. a
; generator writing files must clear it, the way the syntax rule does.
; ⚠ hue rides MODULE 'vi. the images that cat crew/vi/{config,hue}.l registered
; it (the files' own wraps); a tool outside those images says
; (use "crew/vi/config.l") and (use "crew/vi/hue.l") first -- the wraps register
; the same module -- and the use below splices it for this file's own compile.
(use 'vi)
(:
   (hw-nth l k) (? (k = 0) <l (hw-nth >l (k - 1)))
   (hw-cat l)   (foldl (+) "" l)
   ; the three characters html cannot hear otherwise
   (hw-q s) ((: (g i acc) (? (i < tally s)
                              (: c (s i)
                                 (g (i + 1) (acc + (? (c = 38) "&amp;" (c = 60) "&lt;"
                                                      (c = 62) "&gt;" (string c)))))
                              acc)) 0 "")
   ; ⚠ the final newline ENDS the last line, it does not open another: a file of N
   ; lines that ends in one would otherwise paint an N+1'th empty row, with a line
   ; number under the last line of the file.
   (hw-lines s) ((: (g i st acc) (? !(i < tally s) (? (st < i) (rev (snip s st i >< acc)) (rev acc))
                                    (s i = 10)     (g (i + 1) (i + 1) (snip s st i >< acc))
                                    (g (i + 1) st acc)))
                 0 0 ())

   ; --- the stylesheet -------------------------------------------------------
   ; one rule per CLASS, coloured by the GROUP that class names. a class whose
   ; group the theme does not carry gets no rule at all rather than a guess.
   (hw-row g) ((: (f l) (? !(two? l) () (id? <(<l) g) <l (f >l))) hue-theme)
   (hw-faces l) (hw-cat (map (\ f (? (id? f 'bold) ";font-weight:700"
                                     (id? f 'italic) ";font-style:italic" "")) l))
   (hw-rule r) (: t (hw-row (hue-grp r))
                  (? !(two? t) ""
                     (".hue .l-" + (string (hue-cls r)) + "{color:" + (hw-nth t 2)
                      + (hw-faces (hw-nth t 3)) + "}\n")))
   ; the frame: a slab to sit on, and LINE NUMBERS out of a css counter -- so the
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
   ; a line becomes its runs, in order, with the UNCLAIMED gaps between them kept
   ; as plain text. `in` rides from line to line for the string carry.
   (hw-span c a b s) ("<span class=\"l-" + (string c) + "\">" + hw-q (snip s a b) + "</span>")
   (hw-paint bk s in)
   (: r (hue-lex bk s in 1)
      (go rs i acc)
       (? !(two? rs) `((acc + hw-q (snip s i (tally s))) <>r)
          (: u <rs c <u a <>u b <>>u
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
    + hw-cat (map (\ h ("<link rel=\"stylesheet\" href=\"" + hw-q h + "\">\n")) hrefs)
    + "</head>\n<body>\n"
    + (? (0 < tally home) ("<header><a href=\"" + hw-q home + "\">love</a></header>\n") "")
    + "<main>\n<article>\n" + body + "\n</article>\n</main>\n</body>\n</html>\n"))
