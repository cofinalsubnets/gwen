; --- test/host/lapiz.l: the document lens, exactly ---
; NOT in the test/*.l corpus (it loads a crew file and splices its module).
; the corpus is the LENS LAWS over a markdown corpus wide enough to touch every
; block and every span:
;   LAW 1  the AST is the FIXED POINT of each surface -- (mdread (mdshow a)) = a
;          and (htread (htshow a)) = a. the strong one: neither shower loses
;          anything its reader wants, so the two surfaces are views of ONE value.
;   LAW 2  falls out -- each surface round-trips through the other up to
;          normalization, and both normalizers are idempotent. reading is a
;          RETRACTION (many markdowns, one AST), showing its SECTION.
; a new surface owes law 1 and nothing more; that is what keeps its arrival from
; touching any other. run:
;   cat test/00-init.l test/host/lapiz.l | out/host/love

(reads (open "crew/lapiz/lapiz.l" "r"))     ; ..which seals itself at its foot
(use 'lapiz)

; --- the module seam: lapiz registered, the splice + accessor both reach -------
(assert
 (member? 'lapiz (from ()))                   ; the registry lists it
 (member? 'md->ht (from 'lapiz 'keys))        ; 'keys introspects the layer
 ("<h1>hi</h1>" = ((from 'lapiz 'md->ht) "# hi"))   ; the opaque accessor reaches a face
 (lit? (from 'lapiz))                         ; presence: the accessor is a closure
 (! (from 'lapis)))                           ; a missing module answers ()

; --- the corpus: every block, every span, and the edges around them ------------
(: corpus
   (L "" "# a title" "###### deep" "just a paragraph"
      "a wrapped paragraph
continued on a second line"
      "hello *world* and `code` and **bold** here"
      "**bold with *em* inside** and [a *link*](http://x.y/p?q=1&r=2)"
      "sigils that start nothing: a - b, 2 * 3, x[1], a \\* literal star"
      "\\- not a bullet"  "\\# not a head"  "\\--- not a rule"
      "5 < 6 & 7 > 2 -- the html escapes"
      "- one
- two **bold**"
      "1. first
2. second
3. third"
      "> quoted *text*
> and more
>
> a second quoted block"
      "```love
(+ 1 2)
```"
      "```
no language tag
```"
      "---"
      "# everything

a paragraph, then a list:

- alpha
- beta

> a quote with `code`

```c
int main(void) { return 0; }
```

---

the [end](e).")

   (nrm s) (mdshow (mdread s))                 ; the markdown normalizer: read then show
   (hrm s) (htshow (htread s))                 ; ..and the html one
   ; the 0 nil-tail fossil would fail law 1 on nothing but the terminator, and
   ; `show` prints the two alike -- so probe the tail itself, everywhere in the AST.
   (t0? x) (? (! (two? x)) (0 = x) (|| (t0? <x) (t0? >x)))
   _ (map (\ s
      (: d (mdread s)  m (nrm s)  h (md->ht s)
         (assert
            (d = (mdread (mdshow d)))          ; LAW 1, the markdown surface
            (d = (htread (htshow d)))          ; LAW 1, the html surface
            (m = (ht->md (md->ht s)))          ; LAW 2: the lens closes, up to normalization
            (m = (nrm m))                      ; ..and a normalizer is idempotent
            (h = (md->ht (ht->md h)))
            (h = (hrm h))
            (! (t0? d))                        ; NO 0 TAIL anywhere in the middle
            (! (t0? (htread h)))))) corpus)

 ; --- the shapes, spelled out: the middle is plain love data -------------------
 _ (assert
    ((mdread "# hi") = '((head 1 ((text "hi")))))
    ((mdread "*a*b") = '((para ((em ((text "a"))) (text "b")))))
    ((mdread "- x\n- y\n") = '((bullets (((text "x")) ((text "y"))))))
    ((mdread "> q\n") = '((quote ((para ((text "q")))))))
    ((mdread "```c\nx\n```\n") = '((fence "c" "x\n")))
    ((mdread "[t](u)") = '((para ((link ((text "t")) "u")))))
    ((mdread "---\n") = '((rule)))
    ; a paragraph that would re-read as a block wears ONE backslash; the escape leaf takes it off
    ((nrm "- a") = "- a\n")
    ((nrm "\\- a") = "\\- a\n")
    ((mdread "\\- a") = '((para ((text "- a")))))
    ; the two faces of one middle
    ((md->ht "# hi") = "<h1>hi</h1>")
    ((md->ht "a *b* `c`") = "<p>a <em>b</em> <code>c</code></p>")
    ((ht->md "<p><em>x</em></p>") = "*x*\n")
    ((ht->md "<ul>\n<li>a</li>\n<li>b</li>\n</ul>") = "- a\n- b\n")
    ((ht->md "<h2>t</h2>\n<hr>") = "## t\n\n---\n")
    ; entities decode on the way in and re-encode on the way out
    ((ht->md "<p>5 &lt; 6 &amp; 7</p>") = "5 < 6 & 7\n")
    ((md->ht "5 < 6 & 7") = "<p>5 &lt; 6 &amp; 7</p>"))

 ; --- a real document: README.md through the lens, laws and all ----------------
 _ (: src (: q (open "README.md" "r") (slurp q))
      d (mdread src)
      (assert
         (0 < tally d)                         ; it parsed
         (d = (mdread (mdshow d)))             ; LAW 1 on 13KB of real markdown
         (d = (htread (htshow d)))
         (! (t0? d))))

 _ (puts "crew/lapiz: ok -- the document lens (markdown <-> html), laws 1 + 2
")
 0)
