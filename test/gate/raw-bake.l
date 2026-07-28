; test/gate/raw-bake.l -- the WAKE half of the gcc-free image cycle, as a PROGRAM
; rather than a make recipe. mooncc could never bake before (an EXEC's low load
; address collides the codec's pointer/fixnum index range); a -pie ET_DYN loads high
; and clears it. Reuses test_raw's objects, links them PIE, bakes the post-warm image
; into the binary's OWN .image (host/image.c self-patch), then WAKES that image and
; runs the corpus over the woken heap. This is the ONLY gate that exercises
; ai_image_load on a mooncc binary -- the seam where an odd-addressed lvm_* ap
; mis-encodes as a fixnum, invisible to every egg-boot gate.
;
; The SPLIT: make owns the dependency graph and the file lists (its $(filter-out) drops
; glaze.l -- the bake pulls the nif/nifx seam off the book, so (lit? nif) cannot hold on
; a woken heap; the gcc build fails it identically, so it is not a mooncc regression).
; This owns the procedure, where a shell recipe owned it before -- `run` answers
; (status . output) in one call, which in shell is a subshell, a redirect, a $? capture
; and a grep. No shell is spawned anywhere here: `run` is execvp.
;
; usage: love test/gate/raw-bake.l OUTDIR CORPUS.l ..
(:
   nm "test_raw_bake"
   ; -- the gate vocabulary. it lives here until a SECOND gate wants it; then it
   ; moves to test/gate/gate.l and this file leads with a load of it.
   (fail why) (: _ (say err ("FAIL " + nm + ": " + why + "\n")) (quit 1))
   (note s) (say out (s + "\n"))
   (ends? s p) (: n (tally s) m (tally p) (&& (m <= n) ((snip s (n - m) n) = p)))
   (has? s p) (: n (tally s) m (tally p)              ; grep -q, one substring
                 (go i) (? ((n - i) < m) () ((snip s i (i + m)) = p) 1 (go (i + 1)))
                 (go 0))
   (lastln s) (: e (: n (tally s) (? (&& (0 < n) ((s (n - 1)) = 10)) (n - 1) n))
                 (b j) (? (j < 1) 0 ((s (j - 1)) = 10) j (b (j - 1)))
                 (snip s (b e) e))                    ; tail -1, newline already off
   ; PRESENCE IS THE WRAPPER: a 0-byte file reads as "" and "" is nil, so the answer
   ; carries its own presence and the caller tests two?, never the text.
   (slurpf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))
   (readf p) (: r (slurpf p) (? (two? r) <>r (fail ("cannot read " + p))))
   (spit p t) (: q (open p "w")
                 (? q (: _ (say q t) _ (close q) 1) (fail ("cannot write " + p))))
   ; one step: run it, answer its output, or fail NAMING the step. this is the whole
   ; of what `|| { echo FAIL ..; exit 1; }` was doing, once per line.
   (step why av) (: r (run av) (? (0 = <r) >r (fail (why + " -- exit " + (show <r)))))

   args >>cmdline                                     ; drop the binary and this script
   ho <args
   corpus >args
   arch (lastln (step "uname" `("uname" "-m")))
   ; the whole lane is x86-64: mksys/nolibc/math are x64, and a skip is not a failure
   (? !(arch = "x86_64") (note (nm + ": x86-64 only, skipped on " + arch))
      (: raw (ho + "/raw")
         objs (map (\ f (raw + "/" + f)) (filter (\ f (ends? f ".o")) (readdir raw)))
         _ (? (two? objs) 0 (fail ("no objects in " + raw + " -- run test_raw first")))
         pie (ho + "/love-raw-pie")
         baked (ho + "/love-raw-baked")
         one (ho + "/.raw-bake-corpus.l")
         _ (note ("BAKE-WAKE " + pie))
         _ (step "-pie link" ((`((ho + "/mooncc") "-pie") + objs) + `("-o" pie)))
         _ (step "copy the pie binary" `("cp" pie baked))   ; bake a COPY: pie stays unbaked
         _ (step "--bake (the mooncc-PIE binary refused to snapshot its own image)"
                 `(baked "--bake"))
         ; the corpus runs CONCATENATED in one global scope, so it goes in as one file
         _ (spit one (foldl (\ a f (a + (readf f) + "\n")) "" corpus))
         r (run `(baked one))
         last (lastln >r)
         _ (? last (note last) 0)                     ; the summary line, pass or fail -- if there was one
         _ (? (&& (0 = <r) (has? >r "tests pass")) 0
              (fail ("the woken corpus (exit " + (show <r) + ") -- ai_image_load desync?")))
         (note (nm + ": the mooncc-PIE binary bakes its own image and WAKES it"
                   + " -- corpus passes on the woken heap")))))
