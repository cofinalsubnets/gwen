; crew/kore/sum.l -- the checksum tools: cksum, md5sum, sha256sum. one shape three
; times -- read a file whole, digest its bytes, say a line -- so what is actually here
; is the two GNU faces (cksum's "CRC BYTES NAME", the digest tools' "DIGEST  NAME")
; and the -c lane that reads those lines back. the digests are src/hash.c; there is
; no love statement of any of the three, so an image carrying no host nif (the
; kernel's, which compiles none) says which one is missing and leaves with 2.
; loads after fs.l for udirp. a body-less top-level `:` -- the three mains leak,
; kore.l registers them.
(module 'kore
(:
   ; --- the digest door ------------------------------------------------------
   ; the probe is asked at first call and kept, never at load: this file is baked
   ; into the artifact by a love that has the nifs, so a load-time answer would be the
   ; baking love's and not the running one's. lib/gz.l's gz-nif, the same reasoning --
   ; and the same reason each probe is a lambda: an unbound global scares when it is
   ; referenced, not when the function around it is compiled.
   sum-door (tablet 0)
   sum-ckp (\ _ (cksum ""))
   sum-md5p (\ _ (md5 ""))
   sum-shap (\ _ (sha256 ""))
   (sum-nif k f)
    (: v (peep sum-door k ())
       (? (charm? v) v
          (: w (trap (\ _ (: _ (f 0) 1)) () (\ a b 0)) _ (pin sum-door k w) w)))
   (sum-have who nm k probe) (? (sum-nif k probe) 0 (udie 2 (who + ": this image carries no " + nm)))

   ; the triples live inside lambdas for the reason the probes do: an unbound global
   ; scares when it is referenced, and this file is baked by a love that has the nifs
   ; into an image that may not. [state-width init feed done].
   (sum-shad _) [105 sha256-init sha256-feed sha256-done]
   (sum-md5d _) [89  md5-init    md5-feed    md5-done]
   (sum-ckd _)  [12  cksum-init  cksum-feed  cksum-done]

   ; one operand digested as a stream -> [digest bytes] | 'dir | (). "-" is stdin, as
   ; it is everywhere in kore. sha256sum of a 100 MB file used to hold all 100 MB to
   ; digest it; the state is a cask of tens of bytes and the file goes past a gulp at
   ; a time. The byte count rides out because cksum's line says it.
   ; 'dir is its own answer because a directory opens and reads empty -- digested, it
   ; would be the digest of nothing, a plausible wrong number rather than a miss.
   (sum-run mk f)
    (? (udirp f) 'dir
       (: p (uin (\ _ 0) f)
          (? (! p) ()
             (: d (mk 0)
                b (cask <d)
                _ (<>d b)
                rd (urd <p)
                (go n) (: h (urd-gulp rd) k (tally h)
                          (? k (: _ (<>>d b h) (go (n + k))) n))
                n (go 0)
                _ (? <>p (close <p) 0)
                v (<>>>d b)
                (? (v = ()) () [v n])))))
   ; the -c list is read whole, and should be: it is a list of digests, never the thing
   ; digested. one line a file, and nobody writes a gigabyte of them.
   (sum-list f)
    (? (f = "-") [1 ((slurp in) + "")]
       (udirp f) 'dir
       (: r (uread f) (? (two? r) [1 (<>r + "")] ())))
   (sum-nope who f r)
    (: _ (say err (who + ": " + f + (? (id? r 'dir) ": is a directory" ": cannot open")))
       (put err 10))

   ; --- the two output faces -------------------------------------------------
   ; cksum says the byte count too, and says no name for stdin (GNU's face exactly)
   (sum-ckline v n f) ((show v) + " " + (show n) + (? (tally f) (" " + f) "") + "\n")
   (sum-hexline v n f) (v + "  " + f + "\n")

   ; --- the digest lane ------------------------------------------------------
   ; every operand onto out; sn names stdin in the no-operand lane. one unreadable
   ; file complains and costs the status, the rest still run.
   (sum-say who line mk sn as)
    (: (row f nm ok)
        (: r (sum-run mk f)
           (? (two? r) (: _ (say out (line <r <>r nm)) ok)
              (: _ (sum-nope who f r) 0)))
       (go a ok) (? (two? a) (go >a (row <a <a ok)) (: _ (flush out) (? ok 0 1)))
       (? (two? as) (go as 1)
          (: _ (row "-" sn 1) _ (flush out) 0)))

   ; --- the -c lane ----------------------------------------------------------
   ; a line is n hex digits, two characters (two spaces, or " *" where the writer
   ; called it binary), then the name. the shape is asked in full -- the digits
   ; counted and read -- because a looser split takes any line with a space in it for
   ; a checksum line, and a file of prose then reads as a list of failures instead of
   ; the "no checksum lines here" it is. an upper-case list is a list.
   (sum-lower s)
    (: n (tally s)
       o (jug 0)
       (go k) (? (n <= k) (slurp o)
                 (: c (s k) _ (put o (? ((64 < c) && (c < 71)) (c + 32) c)) (go (k + 1))))
       (go 0))
   (sum-hex? l n)
    (: (go k) (? (n <= k) 1
                 (: c (l k)
                    (? (((47 < c) && (c < 58)) || ((96 < c) && (c < 103)) || ((64 < c) && (c < 71)))
                       (go (k + 1)) 0)))
       (go 0))
   (sum-split l n)
    (: t (tally l)
       c (? ((n + 1) < t) (l (n + 1)) 0)
       (? ((n + 2) < t) (? (((l n) = 32) && ((c = 32) || (c = 42)) && (sum-hex? l n))
                           [(sum-lower (snip l 0 n)) (snip l (n + 2) t)] ())
          ()))
   ; the lists read back. the state is [bad unread seen sour]: lines that did not match,
   ; files named in a line that could not be read, lines that were checksum lines at
   ; all, and lists that were no good. sour is its own count because it costs the
   ; status and says nothing: the two warnings below are each a claim about lines, and
   ; a list nobody could read has none to make a claim about.
   (sum-check who mk n as)
    (: (row l st)
        (: p (sum-split l n)
           bad <st unread <>st seen <>>st sour <>>>st
           (? !(two? p) st
              (: said <p f <>p r (sum-run mk f)
                 (? !(two? r) (: _ (sum-nope who f r) _ (say out (f + ": FAILED open or read\n"))
                                 [bad (unread + 1) (seen + 1) sour])
                    (<r = said) (: _ (say out (f + ": OK\n")) [bad unread (seen + 1) sour])
                    (: _ (say out (f + ": FAILED\n")) [(bad + 1) unread (seen + 1) sour])))))
       (over ls st) (? (two? ls) (over >ls (row <ls st)) st)
       ; one list, and GNU's complaint where nothing in it was a checksum line
       (list1 nm text st)
        (: was <>>st
           s2 (over (lines text) st)
           (? (was < <>>s2) s2
              (: _ (say err (who + ": " + nm + ": no properly formatted checksum lines found"))
                 _ (put err 10)
                 [<s2 <>s2 <>>s2 (<>>>s2 + 1)])))
       (walk a st)
        (? !(two? a) st
           (: r (sum-list <a)
              (? (two? r) (walk >a (list1 <a <>r st))
                 (: _ (sum-nope who <a r) (walk >a [<st <>st <>>st (<>>>st + 1)])))))
       st (? (two? as) (walk as [0 0 0 0]) (list1 "standard input" ((slurp in) + "") [0 0 0 0]))
       bad <st unread <>st sour <>>>st
       _ (flush out)
       _ (? (0 < unread) (sum-warn who unread "listed file" " could not be read") 0)
       _ (? (0 < bad) (sum-warn who bad "computed checksum" " did NOT match") 0)
       _ (flush err)
       (? ((0 < bad) || (0 < unread) || (0 < sour)) 1 0))
   ; "1 listed file could not be read", "2 listed files could not be read" -- the noun
   ; takes the s and the verb does not, which holds for both warnings GNU says here
   (sum-warn who n what tail)
    (: _ (say err (who + ": WARNING: " + (show n) + " " + what + (? (n = 1) "" "s") + tail))
       (put err 10))

   ; --- the three mains ------------------------------------------------------
   ; the digest pair share a front: -c reads a list back, anything else digests operands
   (sum-front who nm k probe mk n as)
    (: _ (sum-have who nm k probe)
       (? ((two? as) && ((<as = "-c") || (<as = "--check"))) (sum-check who mk n >as)
          (sum-say who sum-hexline mk "-" as)))
   (cksum-main as) (: _ (sum-have "cksum" "cksum" 0 sum-ckp)
                       (sum-say "cksum" sum-ckline sum-ckd "" as))
   (md5sum-main as) (sum-front "md5sum" "md5" 1 sum-md5p sum-md5d 32 as)
   (sha256sum-main as) (sum-front "sha256sum" "sha256" 2 sum-shap sum-shad 64 as))
) ; module 'kore
