; tools/mx.l -- THE TABLE. love.c's `+` and `*` dispatch matrices as love data, the
; KIND LATTICE they are indexed by, and the three derivations that read it: the C
; (mx.h and kinds.h, through clay) and the Rocq model (proof/rocq/mx.v, through
; tools/mx2coq.l). doc/clay.md rungs 2 and 2b.
;
; the point is which way the arrow runs. the tables used to BE C -- eleven #defines
; that were #undef'd after use, because C cannot say "the same row for every numeric
; kind" any other way -- and proof/rocq/mx.v reached them through tools/mxdump.c, a
; 47-line translation unit that #include'd love.c WHOLE to read its own statics back
; out, naming each lane by comparing function pointers and printing UNKNOWN when it
; could not. that is doc/verify.md's bridge 3, a differential against a dump.
;
; now the love table is the source and both the C and the model are generated from
; it. that is bridge 1 -- shared source, one text two checkers, the discipline
; proof/rocq/gen.v already lives under -- and it costs the dumper, the pointer
; comparison, and the UNKNOWN case, all three of which simply stop existing: here the
; lane's SYMBOL is the datum.
;
; ⚠ EDIT THIS FILE, NOT mx.h / kinds.h. `make test_clay` regenerates both and fails
; on drift.
(:
   ; --- the kinds. this roster IS `enum q` -- love.h includes the kinds.h laid from
   ; it -- so the order here is the dispatch lattice itself, and each lane is a
   ; contiguous block because the roster puts it in one. why THIS order, band by
   ; band, is love.h's narrative over the include; changing it is that comment's
   ; business and this list's together, which is the point of there being one list.
   mx-kinds '(KMint KNom KCharm KWide KFlo KCplx KBig KVec KArrZ KArrR KArrC KArrO
              KString KChain KMap KHot)
   ; the arithmetic lane: every numeric kind routes identically -- the scalar gem
   ; tower, the vec sentinel, and the parallel array tower. this is the group the C
   ; needed a macro (NUMK) to say.
   mx-numk '(KCharm KWide KFlo KCplx KBig KVec KArrZ KArrR KArrC KArrO)

   ; --- the rows. one row per (name numk KMint KNom KString KChain KMap KHot): the
   ; numeric lane, then the six kinds that are named one at a time. ten plus six is
   ; sixteen, so every row is total and no cell can come up NULL.
   ; precedence, high to low: lambda > map > chain > text > number (incl. array).
   mx-rows
    '(; `+`: numbers add, lists and text concatenate, lambdas and maps Church-add.
      ;      () -- and every bare mint -- is the IDENTITY in every lane: it nets 0,
      ;      nothing adjoins nothing, so () + x = x + () = x. the dispatchers early-out
      ;      on a mint before reaching the table; the table says the same thing, so
      ;      the fast path is a fast path and never load-bearing.
      (ADD_MINT  lvm_bin_unit   lvm_bin_unit lvm_bin_unit   lvm_bin_unit   lvm_bin_unit lvm_bin_unit lvm_bin_unit)
      (ADD_NUM   lvm_addn       lvm_bin_unit lvm_add_string lvm_add_string lvm_add_seq  lvm_addh     lvm_addh)
      ; a NAMED symbol inherits the STRING algebra under +: nom+nom cats spellings and
      ; re-interns, nom+str demotes to a string, nom+num rides the byte law. but
      ; nom+chain stays add_seq -- a symbol ADJOINS to a list, foo stays foo.
      (ADD_STR   lvm_add_string lvm_bin_unit lvm_add_string lvm_add_string lvm_add_seq  lvm_addh     lvm_addh)
      (ADD_NOM   lvm_add_string lvm_bin_unit lvm_add_string lvm_add_string lvm_add_seq  lvm_addh     lvm_addh)
      (ADD_TWO   lvm_add_seq    lvm_bin_unit lvm_add_seq    lvm_add_seq    lvm_add_seq  lvm_addh     lvm_addh)
      (ADD_H     lvm_addh       lvm_bin_unit lvm_addh       lvm_addh       lvm_addh     lvm_addh     lvm_addh)
      ; `*`: the semiring product whose `+` is the lane above. numbers multiply,
      ;      sequence * count repeats, lambdas and maps compose (Church mul).
      (MUL_MINT  lvm_bin_unit   lvm_bin_unit lvm_bin_unit   lvm_bin_unit   lvm_bin_unit lvm_bin_unit lvm_bin_unit)
      (MUL_NUM   lvm_muln       lvm_bin_unit lvm_mul_rep    lvm_mul_rep    lvm_mul_rep  lvm_mulh     lvm_mulh)
      ; string*string and sym*sym stay nil (lvm_0, the undefined lane -> the unit that
      ; vanishes); a NUMBER against either repeats it.
      (MUL_REP   lvm_mul_rep    lvm_bin_unit lvm_0          lvm_0          lvm_0        lvm_mulh     lvm_mulh)
      ; the chain row is MUL_REP with one cell changed: chain*chain is the CARTESIAN
      ; product, which is what makes the list lane a semiring rather than a monoid.
      (MUL_CHAIN lvm_mul_rep    lvm_bin_unit lvm_0          lvm_0          lvm_mul_cart lvm_mulh     lvm_mulh)
      (MUL_H     lvm_mulh       lvm_bin_unit lvm_mulh       lvm_mulh       lvm_mulh     lvm_mulh     lvm_mulh))

   ; --- which row each kind takes. a KMap or KHot operand wins over everything, which
   ; is why their rows AND columns are the Church lanes: a map IS a lookup lambda for
   ; +/*, kept deliberately so its rung shares the lanes.
   mx-add-of '((ADD_MINT KMint) (ADD_NOM KNom)
               (ADD_NUM KCharm KWide KFlo KCplx KBig KVec KArrZ KArrR KArrC KArrO)
               (ADD_STR KString) (ADD_TWO KChain) (ADD_H KMap KHot))
   mx-mul-of '((MUL_MINT KMint) (MUL_REP KNom KString)
               (MUL_NUM KCharm KWide KFlo KCplx KBig KVec KArrZ KArrR KArrC KArrO)
               (MUL_CHAIN KChain) (MUL_H KMap KHot))

   ; --- expanding the table ------------------------------------------------------
   (mx-el l i) (? (i < 1) <l (mx-el >l (i - 1)))
   (mx-rowof asg k)                                   ; which row does kind k take?
    (: (go l) (? (! (two? l)) ()
                 (: r <l (? (member? k >r) <r (go >l))))
       (go asg))
   (mx-lane rw c)                                     ; the lane at (row rw, column c)
    (: r (: (go l) (? (! (two? l)) () (: e <l (? (id? <e rw) >e (go >l)))))
       v (r mx-rows)
       (? (member? c mx-numk) (mx-el v 0)
          (id? c 'KMint) (mx-el v 1) (id? c 'KNom) (mx-el v 2) (id? c 'KString) (mx-el v 3)
          (id? c 'KChain) (mx-el v 4) (id? c 'KMap) (mx-el v 5) (mx-el v 6)))
   ; the whole square, 16 rows of 16 lane noms in enum order -- the shape mx2coq.l
   ; derives the band partition from, and the shape the C lays out.
   (mx-grid asg) (map (\ k (map (\ c (mx-lane (mx-rowof asg k) c)) mx-kinds)) mx-kinds)
   mx-add (mx-grid mx-add-of)
   mx-mul (mx-grid mx-mul-of)

   ; --- the C, through clay ------------------------------------------------------
   _ (use 'clay)
   mx-show (from 'clay 'clay-show)
   mx-ok? (from 'clay 'clay-ok?)
   ; lvm_t *const <nm>[KN][KN] -- an array of arrays of CONST POINTERS to the lane ap,
   ; which is a different type from a const array of pointers and the reason clay has
   ; to place `const` on the declarator rather than the base.
   mx-ty '(arr (arr (const (ptr (named "lvm_t"))) KN) KN)
   (mx-clay1 nm g)
    (: rows (map (\ p `('didx <p `('init (map (\ q `('didx <q `('var (string <>q)))) (zip mx-kinds <>p)))))
                 (zip mx-kinds g))
       `('gdecl `(`((string nm) mx-ty `('init rows))) '(static)))
   (zip a b) (? (! (two? a)) () (link `(<a <b) (zip >a >b)))
   ; the whole header. the banner is not decoration: a hand edit here is lost the next
   ; time the gate runs, so the file has to say where its source is.
   ; ⚠ a plain BINDING, not a nullary helper: `(mx-h)` is `mx-h` unrun (CLAUDE.md's
   ; zero-operand law), and this file exists to be read by two other programs.
   mx-h
    (: nl2 (string 10)
       (join (map mx-show
        `(`('note ("mx.h -- GENERATED by tools/mx.l. do not edit; edit the table there." + nl2
                   + "the +/* dispatch matrices, and love.c's only generated region. one datum" + nl2
                   + "feeds this and proof/rocq/mx.v, so the theorem and the code cannot drift." + (string 10)
                   + "doc/clay.md rung 2; the drift check is `make test_clay`."))
          '(note "")
          '(note "`+`: the rows are NAMED-index, so inserting a kind cannot silently shift a")
          ; the count is READ off the roster, so the sentence cannot outlive it
          `('note ("column, and every row names all " + (show (tally mx-kinds))
                   + " -- an unnamed entry would be NULL, a crash."))
          (mx-clay1 "ai_add_mx" mx-add)
          '(note "")
          '(note "`*`: the semiring product whose `+` is the lane above.")
          (mx-clay1 "ai_mul_mx" mx-mul)))))
   ; --- the same table's OTHER consumer: the enum the matrices are indexed BY ------
   ; love.h spelled this roster out by hand and mx-kinds was a transcription of it,
   ; kept honest by a comment saying the order "is not free to change here alone" --
   ; which is a promise, not a check. now the transcription is the SOURCE: `enum q`
   ; is laid from the same list the grid is, so a kind cannot enter the lattice
   ; without entering both, and the ⚠ that comment carried simply stops existing.
   ; KN rides at the end and is not a kind: it is the roster's own LENGTH, which is
   ; what lets mx-ty say [KN][KN] and mean the grid it actually laid.
   kinds-clay `('edef "q" (mx-kinds + '(KN)))
   kinds-h
    (: nl2 (string 10)
       (join (map mx-show
        `(`('note ("kinds.h -- GENERATED by tools/mx.l. do not edit; edit the table there." + nl2
                   + "the kind lattice: the dispatch order love.h's narrative reads, and the row" + nl2
                   + "and column index of the +/* matrices in mx.h -- ONE roster, so the enum and" + nl2
                   + "the grid it dimensions cannot drift apart. KN is the count, not a kind." + nl2
                   + "doc/clay.md rung 2b; the drift check is `make test_clay`."))
          kinds-clay))))
   ; a shape check before anything is written: gen.l and the C compiler both trust
   ; this, and a malformed tree would arrive as a codegen error naming nothing.
   mx-ok
    (: a (mx-ok? (mx-clay1 "ai_add_mx" mx-add))
       b (mx-ok? (mx-clay1 "ai_mul_mx" mx-mul))
       c (mx-ok? kinds-clay)
       (&& (id? a 1) (&& (id? b 1) (id? c 1)))))
