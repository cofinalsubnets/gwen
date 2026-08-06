; test/gate/ld32.l -- read a REAL ELF32/REL object back through the front half of
; holo's linker (ld-read) and check what it found. every link the tree does proves
; the 64-bit rows of link.l's field table; this is the only thing that exercises
; the 32-bit ones, and unexercised rows rot in silence.
; test_mps2_t1 binds a whole v6-M image through the back half; this names what a
; miss IS -- the thumb bit on a FUNC symbol, the two kinds, the empty addend slot.
; run: cat crew/holo/{thumb1,elf,obj,link}.l + the kore lay, then (ld32-check "<o>").
(: (ld32-check p)
   (: r (uread p)
      _ (? (two? r) 0 (scare 'ld32-no-object p))
      s <>r
      _ (? ((s 4) = 1) 0 (scare 'ld32-not-elf32 p))
      ob (ld-read p s 40)                                  ; EM_ARM
      secs (peep ob 'secs 0)
      tx (peep (peep ob 'lanes 0) 'text ())
      _ (? (two? tx) 0 (scare 'ld32-no-text p))
      _ (? (0 < (at (peep secs <tx ()) 2)) 0 (scare 'ld32-empty-text p))
      syms (peep ob 'syms 0) nsym (peep ob 'nsym 0)
      ; ⚠ a defined FUNC's st_value carries the THUMB BIT. lose it and the caller
      ; enters ARM state, which an M-profile part does not have: an instant fault.
      (swalk i nf nu)
       (? (nsym <= i) `(nf nu)
          (: v (peep syms i ()) nm (at v 0) ty (at v 2) sx (at v 3) val (at v 4)
             fn? (&& (ty = 2) (0 < sx))
             _ (? fn? (? (1 = (val & 1)) 0 (scare 'ld32-thumb-bit nm)) 0)
             (swalk (i + 1) (nf + (? fn? 1 0))
                            (nu + (? (&& (sx = 0) (0 < (tally nm))) 1 0)))))
      w (swalk 0 0 0)
      _ (? (0 < <w) 0 (scare 'ld32-no-funcs p))
      _ (? (0 < <>w) 0 (scare 'ld32-no-undefs p))          ; am.c calls __aeabi_*
      ; the only two kinds a thumb1 object mints -- and the addend slot EMPTY on
      ; every one, because a REL keeps its addend in the field. a kind outside the
      ; set means the writer grew one the reader was never told about.
      (each ts m)
       (? !(two? ts) m
          (: t <ts k (at t 1) ad (at t 3)
             _ (? (member? k '(2 10)) 0 (scare 'ld32-reloc-kind k))
             _ (? (two? ad) (scare 'ld32-rel-addend k) 0)
             (each >ts (m + 1))))
      (rwalk rs n) (? !(two? rs) n (rwalk >rs (each >(<rs) n)))
      nr (rwalk (peep ob 'relas 0) 0)
      _ (? (0 < nr) 0 (scare 'ld32-no-relocs p))
      _ (puts ("ld32: " + ((show nsym) + (" syms, " + ((show <w)
              + (" thumb funcs, " + ((show <>w) + (" undefs, " + ((show nr)
              + (" REL relocs -- ok\n"))))))))))
      (flush out)))
