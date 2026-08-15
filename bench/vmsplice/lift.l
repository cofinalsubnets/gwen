; lift.l -- pull one relocation-free function body out of a .o and write it as love
; source pinning `jitcode`, the bytes as a cask: exactly what `nif` installs.
;
; the reader is holo's OWN (crew/holo/link.l's ld-read, in the baked image since the
; linker half landed) -- no objcopy, no foreign ELF walk.
;
;   love lift.l <obj> <symbol> <out.l>
;
; ⚠ REFUSES a body carrying a relocation. A lifted body runs at an address the linker
; never saw, so a pc-relative reference to lvm_chain or a call to ai_please would land
; wherever the JIT happened to map it. Binding those to the LIVE process's addresses is
; a linking step this does not do -- see doc/archive/moon-regalloc.md's splice-client section.
(: (nth l i) (? (= i 0) (cap l) (nth (cup l) (- i 1)))
   (die m) (: _ (say err ("lift: " m "\n")) (quit 1))
   as (cuup cmdline)                                   ; drop argv[0] and this script
   _ (? ((tally as) >= 3) 0 (die "usage: love lift.l <obj> <symbol> <out.l>"))
   obj (nth as 0) nom (string (nth as 1)) dst (nth as 2)   ; ⚠ never `out`: that is the stdout PORT
   q (open obj "r")
   _ (? q 0 (die ("cannot open " obj)))
   s (slurp q) _ (close q)
   ob ((from 'holo 'ld-read) 'lift s 62)               ; 62 = EM_X86_64
   secs (peep ob 'secs 0) syms (peep ob 'syms 0)
   ; the .text section: index -> (lane off size align name)
   tx (: (go ks) (? (atom? ks) ()
                    (: v (peep secs (cap ks) 0)
                       (? (id? (nth v 0) 'text) (link (cap ks) v) (go (cup ks)))))
         (go (keys secs)))
   _ (? tx 0 (die "no .text"))
   tix (cap tx) toff (nth (cup tx) 1) tsz (nth (cup tx) 2)
   ; the symbol, by name: (name bind typ shndx value size)
   sy (: (go ks) (? (atom? ks) ()
                    (: v (peep syms (cap ks) 0)
                       (? ((tally v) && ((nth v 0) = nom)) v (go (cup ks)))))
         (go (keys syms)))
   _ (? sy 0 (die ("no symbol " nom)))
   at (nth sy 4)                                       ; st_value, the offset in .text
   ; the EXTENT: mooncc leaves st_size 0, so the body runs to the next .text symbol
   ; above it, or to the end of the section.
   end (: (go ks acc)
           (? (atom? ks) acc
              (: v (peep syms (cap ks) 0)
                 w (nth v 4)
                 (go (cup ks) (? (((nth v 3) = tix) && (w > at) && (w < acc)) w acc))))
         (go (keys syms) tsz))
   n (end - at)
   _ (? (> n 0) 0 (die (nom " has no bytes")))
   ; ⚠ the refusal: any relocation landing inside [at, end) makes this unliftable
   nrel (: (inseg es acc) (? (atom? es) acc
                             (: o (cap (cap es))
                                (inseg (cup es) (? ((o >= at) && (o < end)) (+ acc 1) acc))))
           (go rs acc) (? (atom? rs) acc
                          (: r (cap rs)
                             (go (cup rs) (? ((cap r) = tix) (inseg (cup r) acc) acc))))
         (go (peep ob 'relas ()) 0))
   _ (? (= nrel 0) 0
        (die (nom " carries " (show nrel) " relocation(s) -- not liftable")))
   ; the bytes, as a love source file pinning `jitcode`
   o (open dst "w")
   _ (? o 0 (die ("cannot write " dst)))
   _ (say o ("; lifted by bench/vmsplice/lift.l -- " nom ", " (show n) " bytes, no relocations\n"))
   _ (say o "(: jitcode (cask '(")
   _ (: (go i) (? (< i n) (: _ (say o (show (peep s (+ toff (+ at i)) 0))) _ (say o " ") (go (+ i 1))) 0)
        (go 0))
   _ (say o ")))\n")
   _ (close o)
   _ (say out ("lift: " nom " " (show n) " bytes -> " dst "\n"))
   (quit 0))
