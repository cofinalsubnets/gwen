; mx.l -- love.c's `+` and `*` dispatch matrices as love data, the kind lattice they
; are indexed by, and the derivations that read it: the C (mx.h and kinds.h, through
; clay), the host link's love_data.ld, and the Rocq model (test/proof/rocq/mx.v,
; through tools/mx2coq.l). doc/misc/clay.md rungs 2 and 2b.
;
; edit this file, not mx.h / kinds.h / love_data.ld -- make relays all three from
; here, off the mx_gen roster in test/test.mk. `make mx` refreshes, test_clay diffs.
(:
   ; --- the kinds. this roster IS `enum q`, so the order here is the dispatch lattice
   ; itself and each lane a contiguous block; love.h's narrative over the include is
   ; where that order is argued. dispatch only -- every entry is a kind `ai_kind` can
   ; answer, so a tray enters as its four tiers and never as the bare rep.
   mx-kinds '(KMint KNom KCharm KSun KGem KTwin KBig KTrayZ KTrayR KTrayC KTrayO
              KString KChain KTablet KHot)
   ; the arithmetic lane: every numeric kind routes identically, the scalar gem tower
   ; and the parallel tray tower alike.
   mx-numk '(KCharm KSun KGem KTwin KBig KTrayZ KTrayR KTrayC KTrayO)

   ; --- the reps: what a heap object's hot says it is, a different question from which
   ; lane it dispatches down. `ai_typ` answers these nine and nothing else, so a switch
   ; over them is exhaustive and -Wswitch says so. the tray is the one that differs:
   ; one rep, refined by element tier into four dispatch kinds.
   mx-reps '(DMint DNom DSun DGem DTwin DBig DTray DString DChain)
   ; each rep's dispatch kind. DTray's entry is the FLOOR of the tier ladder and is
   ; never read -- ai_kind branches on the tray first and adds the tier itself.
   mx-repq '((DMint KMint) (DNom KNom) (DSun KSun) (DGem KGem) (DTwin KTwin)
             (DBig KBig) (DTray KTrayZ) (DString KString) (DChain KChain))

   ; --- the rows. one row per (name numk KMint KNom KString KChain KTablet KHot): the
   ; numeric lane, then the six kinds named one at a time -- fifteen in all, so every
   ; row is total and no cell can come up NULL.
   ; precedence, high to low: lambda > tablet > chain > text > number (incl. tray).
   mx-rows
    '(; `+`: numbers add, lists and text concatenate, lambdas and maps Church-add.
      ;      () and every bare mint is the identity in every lane, so () + x = x. the
      ;      dispatchers early-out on a mint before the table and must say the same.
      ;      mixed bands degenerate: a number against a symbol, string or chain arrives
      ;      as that band's unit and the other operand answers whole. the band order is
      ;      NUM < NOM < STR < CHAIN, and nom+str keeps the spelling hom.
      (ADD_MINT  lvm_bin_unit   lvm_bin_unit lvm_bin_unit   lvm_bin_unit   lvm_bin_unit lvm_bin_unit lvm_bin_unit)
      (ADD_NUM   lvm_addn       lvm_bin_unit lvm_bin_b      lvm_bin_b      lvm_bin_b    lvm_addh     lvm_addh)
      ; a symbol takes the string algebra under +: spellings cat and re-intern, nom+str
      ; demotes to a string. nom+chain still adjoins -- foo stays foo.
      (ADD_STR   lvm_bin_a      lvm_bin_unit lvm_add_string lvm_add_string lvm_add_seq  lvm_addh     lvm_addh)
      (ADD_NOM   lvm_bin_a      lvm_bin_unit lvm_add_string lvm_add_string lvm_add_seq  lvm_addh     lvm_addh)
      (ADD_TWO   lvm_bin_a      lvm_bin_unit lvm_add_seq    lvm_add_seq    lvm_add_seq  lvm_addh     lvm_addh)
      (ADD_H     lvm_addh       lvm_bin_unit lvm_addh       lvm_addh       lvm_addh     lvm_addh     lvm_addh)
      ; `*`: the semiring product whose `+` is the lane above. numbers multiply,
      ;      sequence * count repeats, lambdas and maps compose (Church mul). () is the
      ;      zero and annihilates, so a collapse to lvm_0 propagates instead of
      ;      vanishing. 1 is already the identity, numeral being function.
      (MUL_MINT  lvm_0          lvm_0        lvm_0          lvm_0          lvm_0        lvm_0        lvm_0)
      (MUL_NUM   lvm_muln       lvm_0        lvm_mul_rep    lvm_mul_rep    lvm_mul_rep  lvm_mulh     lvm_mulh)
      ; str*str and nom*nom stay lvm_0, the absorbing zero; a number against either
      ; repeats it.
      (MUL_REP   lvm_mul_rep    lvm_0        lvm_0          lvm_0          lvm_0        lvm_mulh     lvm_mulh)
      ; MUL_REP with one cell changed: chain*chain is the cartesian product, which is
      ; what makes the list lane a semiring rather than a monoid.
      (MUL_CHAIN lvm_mul_rep    lvm_0        lvm_0          lvm_0          lvm_mul_cart lvm_mulh     lvm_mulh)
      (MUL_H     lvm_mulh       lvm_0        lvm_mulh       lvm_mulh       lvm_mulh     lvm_mulh     lvm_mulh))

   ; --- which row each kind takes. a KTablet or KHot operand wins over everything, so
   ; their rows and columns are both the Church lanes: a tablet is a lookup lambda here.
   mx-add-of '((ADD_MINT KMint) (ADD_NOM KNom)
               (ADD_NUM KCharm KSun KGem KTwin KBig KTrayZ KTrayR KTrayC KTrayO)
               (ADD_STR KString) (ADD_TWO KChain) (ADD_H KTablet KHot))
   mx-mul-of '((MUL_MINT KMint) (MUL_REP KNom KString)
               (MUL_NUM KCharm KSun KGem KTwin KBig KTrayZ KTrayR KTrayC KTrayO)
               (MUL_CHAIN KChain) (MUL_H KTablet KHot))

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
          (id? c 'KChain) (mx-el v 4) (id? c 'KTablet) (mx-el v 5) (mx-el v 6)))
   ; the whole square of lane noms, in enum order: what mx2coq.l derives the band
   ; partition from, and what the C lays out.
   (mx-grid asg) (map (\ k (map (\ c (mx-lane (mx-rowof asg k) c)) mx-kinds)) mx-kinds)
   mx-add (mx-grid mx-add-of)
   mx-mul (mx-grid mx-mul-of)

   ; --- the C, through clay ------------------------------------------------------
   _ (use 'clay)
   ; lvm_t *const <nm>[KN][KN] -- arrays of const POINTERS, a different type from a
   ; const array of pointers, so clay places the const on the declarator not the base.
   mx-ty '(arr (arr (const (ptr (named "lvm_t"))) KN) KN)
   (mx-clay1 nm g)
    (: rows (map (\ p ['didx <p ['init (map (\ q ['didx <q ['var (string <>q)]]) (zip mx-kinds <>p))]])
                 (zip mx-kinds g))
       ['gdecl [[(string nm) mx-ty ['init rows]]] '(static)])
   (zip a b) (? (! (two? a)) () (link [<a <b] (zip >a >b)))

   ; --- the enums the matrices are indexed by -------------------------------------
   ; KN rides at the end of `enum q` and is not a kind: it is the roster's own length,
   ; which is what lets mx-ty say [KN][KN] and mean the grid it laid. enum d gets no
   ; such tail -- it is the one enum the C switches over, and a count riding in it is a
   ; case -Wswitch would demand for a value ai_typ can never answer.
   kinds-clay ['edef "q" (mx-kinds + '(KN))]
   reps-clay ['edef "d" mx-reps]
   ; rep -> the kind it dispatches as, and the only crossing between the two rosters:
   ; clay's edef has no `= n` slot, so the enums share no values. it rides mx.h rather
   ; than kinds.h, which reaches every frontend where a static table would sit unread
   ; under -Werror. unsized -- C reads the bound off the designators.
   repq-clay
    ['gdecl [["ai_kind_of_d" '(arr (const (named "enum q")) 0)
                 ['init (map (\ p ['didx <p ['var (string <>p)]]) mx-repq)]]]
      '(static)]

   ; --- the same roster, laid for the LINKER ---------------------------------------
   ; enum d is a slot index: love.c lays each sentinel in its own love.data.N, love.h
   ; reads a value's rep as arithmetic on the ap, and ld has to be told that tiling.
   ; the stride is the seat's, not the tree's -- a script tiling tighter than its seat
   ; believes answers slot 0 for every value, in silence. tiling looser is loud: a body
   ; past its slot drives ld's location counter backwards and it refuses, so the `. =`
   ; per slot is a check as well as a placement.
   ; one row per seat class, saying both numbers at once: the stride the C compiles
   ; under and the stride ld is told. it has only to clear the fattest sentinel body
   ; the backend emits, which measures call lowering rather than code -- arm64 4, x64
   ; 8, thumb2 60, riscv64 88 -- so lean's 16 is 2x x64's measured 8 with room for an
   ; `endbr64`, and the two fat backends spill their params to the frame.
   mx-strides '((lean 16  ("__x86_64__" "__aarch64__"))
                (fat  128 ()))              ; no predicate: the #else, and there is one
   ; and whether a seat lays the sections at all: 0 asks the sentinels by name and
   ; answers the same enum d -- wasm has none to lay, love0 opts out. a layout never
   ; crosses between two binaries: the heap image carries an ap as its index in
   ; image_extra_aps.
   mx-sections '((byname 0 ("__wasm__"))
                 (tiled  1 ()))
   (mx-stride cls) (: (go l) (atom? l ? (scare 'mx-stride cls)
                              (: r <l (id? <r cls ? <>r (go >l))))
                      (go mx-strides))
   ; --- and the same classes laid as C, through clay's rung 2 ---------------------
   ; a knob: `#ifndef NAME` over an `#if defined(..) || .. / #elif / #else` on the
   ; classes, so the rows above are a default and the ifndef is the -D door love0
   ; takes ai_data_section=0 through. the linker block reads this same roster.
   (mx-defd n) ['call ['var "defined"] [['var n]]]
   (mx-ors l) (atom? >l ? (mx-defd <l) ['lor (mx-defd <l) (mx-ors >l)])
   (mx-knob nm rows)
    (: (arms l) (two? l ?
                   (: r <l (two? (el r 2) ? ([(mx-ors (el r 2)) [['cpp-def nm <>r]]] >< arms >l)
                            (arms >l))))
       (els l) (atom? l ? (scare 'mx-knob nm)
                  (: r <l (two? (el r 2) ? (els >l) [['cpp-def nm <>r]])))
       ['cpp-if [[['un 'not (mx-defd nm)] [['cpp-if (arms rows) (els rows)]]]]])
   mx-nl (string 10)
   (mx-tile base stride p)                        ; one KEEP per rep, each slot pinned
    (: (go i) (? (i = tally mx-reps) ""
                 (p + "KEEP(*(love.data." + show i + "))  . = " + base + " + "
                    + (show (stride * (i + 1))) + ";" + mx-nl + go (i + 1)))
       (go 0))
   ; the host link's -T fragment, laid whole
   mx-ld
    (: s (mx-stride 'lean)
       ("/* love_data.ld -- generated by mx.l. do not edit; edit the table there." + mx-nl
        + "   the data sentinels' tiling for the host link (love.c's DSENT, love.h's" + mx-nl
        + "   ai_typ). INSERT augments the default script instead of replacing it, so this" + mx-nl
        + "   drops onto a host link as a plain -T fragment. the output name leads with a" + mx-nl
        + "   dot, so it is no C identifier and ld defines no bounds for it -- the two" + mx-nl
        + "   symbols here are the ASSERT's. holo needs no script at all: it lays each" + mx-nl
        + "   section on the grain the object declares, which is this same tiling. */" + mx-nl
        + "SECTIONS {" + mx-nl
        + "  .love.data : ALIGN(" + (show s) + ") {" + mx-nl
        + "    __start_love_data = .;" + mx-nl
        + (mx-tile "__start_love_data" s "    ")
        + "    __stop_love_data = .;" + mx-nl + "  }" + mx-nl + "} INSERT AFTER .text;" + mx-nl
        + "ASSERT(__stop_love_data - __start_love_data == "
        + (show (tally mx-reps)) + " * " + (show s) + "," + mx-nl
        + "       \"love_data: slot count moved -- sync love.h's ai_data_n and mx.l\")" + mx-nl))

   mx-h
    (: nl2 (string 10)
       (cats (map clay-show
        [['note "mx.h -- +/* dispatch matrices. generated by mx.l, do not edit"]
         (mx-clay1 "ai_add_mx" mx-add)
         '(note "")
         (mx-clay1 "ai_mul_mx" mx-mul)
         repq-clay])))
   ; the slot layout rides kinds.h, beside the roster it is a layout of
   slots-clay
    (: nl (string 10)
       [['note ("the data slot layout. the sentinels tile one section at ai_data_stride in" + nl
                 + "enum d order, so a value's rep is arithmetic on its ap -- love.c's DSENT," + nl
                 + "love.h's ai_typ, and src/love_data.ld laid from this same roster." + nl
                 + "each #if here is a default; the guard over it is the -D door.")]
         (mx-knob "ai_data_section" mx-sections)
         (mx-knob "ai_data_stride" mx-strides)
         ['cpp-def "ai_data_n" (tally mx-reps)]])
   kinds-h
    (: nl2 (string 10)
       ((cats (map clay-show
         [['note ("kinds.h -- generated by mx.l. do not edit; edit the table there." + nl2)]
          kinds-clay
          reps-clay
          '(note "")]))
        + cats (map clay-show slots-clay)))
   ; a shape check first: a malformed tree arrives as a codegen error naming nothing.
   mx-ok
    (: a (clay-ok? (mx-clay1 "ai_add_mx" mx-add))
       b (clay-ok? (mx-clay1 "ai_mul_mx" mx-mul))
       c (clay-ok? kinds-clay)
       d (clay-ok? reps-clay)
       e (clay-ok? repq-clay)
       f (foldl (\ a x (a && id? (clay-ok? x) 1)) 1 slots-clay)
       (a = b = c = d = e = f = 1)))
