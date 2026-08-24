; lcatfs: bake a file TREE into .rodata -- one `{"path", "bytes", len, ms},` row per
; file, where lcatv bakes one file into one literal. The kernel's ramfs
; (free/kmain.c) serves reads straight off these rows, so a file nobody opens
; costs a row and not one word of the bounded heap. Bytes are VERBATIM, lcatv's
; escaping and not lcat's show-reprint: the reader must read them back the way the
; host reads the file. argv = (this-file path..); each path is baked under the
; spelling it was given, so the caller decides what the tree looks like from inside.
; ms is the SOURCE file's mtime on the (clock t) scale, which is what makes the
; kernel's `stat` answer a date instead of a zero -- the bake is the only place that
; date exists, since the initrd carries no directory of its own.
; `-s SECONDS` pins every row's ms to one stamp instead (selfpack's own mtime
; law): the ARTIFACT's bake must be a function of the tree's bytes alone, or
; the seed fixpoint reads the checkout's clock. the test kernel keeps real dates.
(: (eb c) (? (= c 10) (say out "\\n")
              (| (= c 92) (= c 34)) (: _ (put out 92) (put out c))
              (| (< c 32) (> c 126)) (: _ (put out 92)
                 _ (put out (+ 48 (% (// c 64) 8)))
                 _ (put out (+ 48 (% (// c 8) 8)))
                 (put out (+ 48 (% c 8))))
              (put out c))
   (ebs s) ((: (g i) (? (< i (tally s)) (: _ (eb (peep s i 0)) (g (+ 1 i))))) 0)
   (row pin ms0 p) (: h (open p "r")
              _ (? h 0 (: _ (say err (+ "lcatfs: cannot open " p)) (quit 1)))
              s (slurp h)
              st (stat p)                       ; (size ms mode ns); presence is the cons
              _ (say out "{\"") _ (ebs p)
              _ (say out "\", \"") _ (ebs s)
              _ (say out "\", ") _ (say out (show (tally s)))
              _ (say out ", ") _ (say out (show (? pin ms0 (? (two? st) <>st 0))))
              _ (say out "},")
              (put out 10))
   as (cup argv)
   pin (? (two? as) (= "-s" (cap as)) 0)
   ms0 (? pin (* 1000 (cap (sound (cap (cup as))))) 0)
   ps (? pin (cup (cup as)) as)
   ((: (g l) (? (two? l) (: _ (row pin ms0 (cap l)) (g (cup l))))) ps))
