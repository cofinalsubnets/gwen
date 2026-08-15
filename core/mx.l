; mx.l -- THE TABLE. love.c's `+` and `*` dispatch matrices as love data, the
; KIND LATTICE they are indexed by, and the three derivations that read it: the C
; (mx.h and kinds.h, through clay) and the Rocq model (test/proof/rocq/mx.v, through
; tools/mx2coq.l). doc/clay.md rungs 2 and 2b.
;
; the point is which way the arrow runs. the tables used to BE C -- eleven #defines
; that were #undef'd after use, because C cannot say "the same row for every numeric
; kind" any other way -- and test/proof/rocq/mx.v reached them through tools/mxdump.c, a
; 47-line translation unit that #include'd love.c WHOLE to read its own statics back
; out, naming each lane by comparing function pointers and printing UNKNOWN when it
; could not. that is's bridge 3, a differential against a dump.
;
; now the love table is the source and both the C and the model are generated from
; it. that is bridge 1 -- shared source, one text two checkers, the discipline
; test/proof/rocq/gen.v already lives under -- and it costs the dumper, the pointer
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
   ; ⚠ DISPATCH only: every entry is a kind `ai_kind` can actually ANSWER, so a tray
   ; enters as its four TIERS and never as the bare rep -- that one lives in the
   ; roster below.
   mx-kinds '(KMint KNom KCharm KSun KGem KTwin KBig KTrayZ KTrayR KTrayC KTrayO
              KString KChain KTablet KHot)
   ; the arithmetic lane: every numeric kind routes identically -- the scalar gem
   ; tower and the parallel tray tower. this is the group the C needed a macro
   ; (NUMK) to say.
   mx-numk '(KCharm KSun KGem KTwin KBig KTrayZ KTrayR KTrayC KTrayO)

   ; --- the REPS: what a heap object's hot says it is, which is a different question
   ; from which lane it dispatches down. `ai_typ` compares the ap against the data
   ; sentinels and can answer these NINE and nothing else, so a switch over them is
   ; exhaustive and -Wswitch says so -- that is the whole reason the two rosters are
   ; two. the tray is the one that differs: ONE rep, refined by element tier into
   ; four dispatch kinds. reps carry no order and no lanes; only the map below reads
   ; this list's shape.
   ; ⚠ the APPLY action is not a table here (each sentinel tail-jumps to its own handler in
   ; love.c), but it has a DEFAULT and the roster is where to say so: const-1, what an operand
   ; gets when its kind has no lane of its own. a point answers it at every operand; the two
   ; sequences answer it at everything that is neither an index nor their own kind. it is the
   ; zero point's own action, so it reads as "nothing to look up here" rather than as an error --
   ; subject to change, and the reason a dead lane can be retired by falling back to it.
   mx-reps '(DMint DNom DSun DGem DTwin DBig DTray DString DChain)
   ; each rep's dispatch kind. DTray's entry is the FLOOR of the tier ladder and is
   ; never read -- ai_kind branches on the tray first and adds the tier itself.
   mx-repq '((DMint KMint) (DNom KNom) (DSun KSun) (DGem KGem) (DTwin KTwin)
             (DBig KBig) (DTray KTrayZ) (DString KString) (DChain KChain))

   ; --- the rows. one row per (name numk KMint KNom KString KChain KTablet KHot): the
   ; numeric lane, then the six kinds that are named one at a time. nine plus six is
   ; fifteen, so every row is total and no cell can come up NULL.
   ; precedence, high to low: lambda > tablet > chain > text > number (incl. tray).
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

   ; --- which row each kind takes. a KTablet or KHot operand wins over everything,
   ; which is why their rows AND columns are the Church lanes: a tablet IS a lookup
   ; lambda for +/*, kept deliberately so its rung shares the lanes.
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
   ; the whole square, 16 rows of 16 lane noms in enum order -- the shape mx2coq.l
   ; derives the band partition from, and the shape the C lays out.
   (mx-grid asg) (map (\ k (map (\ c (mx-lane (mx-rowof asg k) c)) mx-kinds)) mx-kinds)
   mx-add (mx-grid mx-add-of)
   mx-mul (mx-grid mx-mul-of)

   ; --- the C, through clay ------------------------------------------------------
   ; clay is a module: `use` splices it, so clay-show / clay-ok? read bare. no accessor.
   _ (use 'clay)
   ; lvm_t *const <nm>[KN][KN] -- an array of arrays of CONST POINTERS to the lane ap,
   ; which is a different type from a const array of pointers and the reason clay has
   ; to place `const` on the declarator rather than the base.
   mx-ty '(arr (arr (const (ptr (named "lvm_t"))) KN) KN)
   (mx-clay1 nm g)
    (: rows (map (\ p `('didx <p `('init (map (\ q `('didx <q `('var (string <>q)))) (zip mx-kinds <>p)))))
                 (zip mx-kinds g))
       `('gdecl `(`((string nm) mx-ty `('init rows))) '(static)))
   (zip a b) (? (! (two? a)) () (link `(<a <b) (zip >a >b)))

   ; --- the same table's OTHER consumer: the enums the matrices are indexed BY ------
   ; love.h spelled this roster out by hand and mx-kinds was a transcription of it,
   ; kept honest by a comment saying the order "is not free to change here alone" --
   ; which is a promise, not a check. now the transcription is the SOURCE: `enum q`
   ; is laid from the same list the grid is, so a kind cannot enter the lattice
   ; without entering both, and the ⚠ that comment carried simply stops existing.
   ; KN rides at the end and is not a kind: it is the roster's own LENGTH, which is
   ; what lets mx-ty say [KN][KN] and mean the grid it actually laid. ⚠ enum d gets
   ; NO such tail: it is the one enum the C SWITCHES over, and a count riding in it
   ; is a case -Wswitch would demand for a value ai_typ can never answer. the table
   ; below wants no bound anyway.
   kinds-clay `('edef "q" (mx-kinds + '(KN)))
   reps-clay `('edef "d" mx-reps)
   ; the one bridge between the two rosters: rep -> the kind it dispatches as. it is
   ; LAID, not written, because a hand-written map is exactly the drift the split was
   ; meant to end -- a rep can no more miss its kind than a kind can miss its column.
   ; ⚠ clay's edef has no `= n` slot on purpose, so the two enums do NOT share values
   ; and this table is the only crossing. it rides mx.h, not kinds.h: kinds.h is
   ; love.h's and reaches every frontend, where a static table would sit unread under
   ; -Werror -- the tables are love.c's alone. UNSIZED on purpose: C reads the bound
   ; off the designators, so it cannot disagree with the roster that laid them.
   repq-clay
    `('gdecl `(`("ai_kind_of_d" '(arr (const (named "enum q")) 0)
                 `('init (map (\ p `('didx <p `('var (string <>p)))) mx-repq))))
      '(static))

   ; --- the same roster, laid for the LINKER ---------------------------------------
   ; enum d is a SLOT INDEX before it is anything else: love.c lays each data sentinel
   ; in its own love_data.N and love.h reads a value's rep as arithmetic on the ap. ld
   ; has to be TOLD that tiling -- it cannot read ai_data_stride -- so six linker
   ; scripts carried nine hand-copied lines apiece. they are laid from the roster now,
   ; and a rep entering it enters every script.
   ; ⚠ the stride is the SEAT's, not the tree's: love.h's #if picks it off the arch and
   ; the row below says the same number. a script tiling TIGHTER than its seat believes
   ; answers slot 0 for every value, in silence -- the one drift in this scheme that
   ; does not announce itself, which is why the table owns it. the loud direction is
   ; ld's own: a body past its slot drives the location counter backwards and ld
   ; refuses, so `. =` per slot is a check and not only a placement.
   ; --- the seat classes. ONE row says both numbers a seat has: the stride the C
   ; compiles under and the stride ld is told, which is why the drift love.h's ⚠ used
   ; to warn about can no longer be written down. the last row carries no predicate
   ; and is the #else; there is exactly one.
   ; the stride has only to clear the fattest body the backend building THIS text
   ; emits, and they differ by an order of magnitude. the sentinels already SHARE
   ; their handlers (each is one tail jump to a data_*_apply), so what it measures is
   ; CALL LOWERING, not code: arm64 4, x64 8, thumb2 60, riscv64 88. the two fat ones
   ; spill their params to the frame and read them straight back; the IR sweeps that
   ; fold exactly that (stld, copyprop) run on x64 alone -- gen.l's build says so and
   ; says why. they port, and pay (measured: riscv64 76, thumb2 48), but they land
   ; inside the regen's own ranking, where they cost arm64 its param homing. a rung
   ; of its own. lean's 16 buys x64 exactly 2x its measured 8; 8 would buy none, and
   ; the next `endbr64` (a seat built without -fcf-protection=none) is +4. aligned()
   ; does lower a section below -falign-functions, so the floor is margin, not the cc.
   mx-strides '((lean 16  ("__x86_64__" "__aarch64__"))
                (fat  128 ()))
   ; and whether a seat lays the sections at all: 0 asks the sentinels BY NAME and
   ; answers the same enum d. wasm has no sections to lay, mach-o spells them
   ; `segment,section`, and love0 opts out (host/build.mk -D) so the bootstrap owes no
   ; linker script. safe in every direction -- the heap image carries an ap as its
   ; INDEX in image_extra_aps, so a layout never crosses between two binaries.
   mx-sections '((byname 0 ("__wasm__" "__APPLE__"))
                 (tiled  1 ()))
   ; the scripts, keyed to a class. the third column is the script's indent, so the
   ; block keeps its neighbours' shape. a seat leaves this roster when its LINK
   ; becomes ours: holo lays each love_data.N whole and in order, on the grain the
   ; object declares, which IS this tiling -- so a holo-linked board owes no script
   ; at all. the three thumb seats left that way; the two kernel ones stay while
   ; KLINK=lld remains the comparison lane and ld.lld still needs telling.
   mx-seats '(("free/x86_64/x86_64.lds"    lean 8)
              ("free/aarch64/aarch64.lds"  lean 8))
   (mx-stride cls) (: (go l) (? (! (two? l)) (scare 'mx-stride cls)
                                 (: r <l (? (id? <r cls) <>r (go >l))))
                      (go mx-strides))
   ; --- and the same classes laid as C, through clay's rung 2 ---------------------
   ; a knob: `#ifndef NAME` over an `#if defined(..) || .. / #elif / #else` on the
   ; classes. the ifndef is the -D DOOR -- a build that knows better says so, which
   ; is how love0 takes ai_data_section=0 -- so the rows here are only the default.
   ; ⚠ this and the linker blocks read ONE roster: a class's stride cannot say two
   ; things any more, which was the last hand-kept crossing in the scheme.
   (mx-defd n) `('call `('var "defined") `(`('var n)))
   (mx-ors l) (? (! (two? >l)) (mx-defd <l) `('lor (mx-defd <l) (mx-ors >l)))
   (mx-knob nm rows)
    (: (arms l) (? (! (two? l)) ()
                   (: r <l (? (two? (el r 2)) (link `((mx-ors (el r 2)) `(`('cpp-def nm <>r)))
                                                    (arms >l))
                              (arms >l))))
       (els l) (? (! (two? l)) (scare 'mx-knob nm)
                  (: r <l (? (two? (el r 2)) (els >l) `(`('cpp-def nm <>r)))))
       `('cpp-if `(`(`('un 'not (mx-defd nm)) `(`('cpp-if (arms rows) (els rows)))))))
   ; the two lines a script keeps by hand; everything between them is laid.
   mx-ld-open "/* love_data: the sentinel tiling, laid by mx.l -- edit the table there. */"
   mx-ld-shut "/* end love_data */"

   mx-nl (string 10)
   (mx-cat l) (foldl (\ a s (a + s + mx-nl)) "" l)
   (mx-tile base stride p)                        ; one KEEP per rep, each slot pinned
    (: (go i) (? (i = (tally mx-reps)) ""
                 (p + "KEEP(*(love_data." + (show i) + "))  . = " + base + " + "
                    + (show (stride * (i + 1))) + ";" + mx-nl + (go (i + 1))))
       (go 0))
   (mx-lds-block stride pad)
    (: p (" " * pad)
       (p + ". = ALIGN(" + (show stride) + ");" + mx-nl
          + p + "__love_data = .;" + mx-nl + (mx-tile "__love_data" stride p)))

   ; the host link's -T fragment, laid WHOLE: it exists for this and nothing else.
   mx-ld
    (: s (mx-stride 'lean)
       ("/* love_data.ld -- GENERATED by mx.l. do not edit; edit the table there." + mx-nl
        + "   the data sentinels' tiling for the host link (love.c's DSENT, love.h's" + mx-nl
        + "   ai_typ). INSERT augments the default script instead of replacing it, so this" + mx-nl
        + "   drops onto a host link as a plain -T fragment. the output name leads with a" + mx-nl
        + "   dot, so it is no C identifier and ld defines no bounds for it -- the two" + mx-nl
        + "   symbols here are the ASSERT's. holo needs no script at all: it lays each" + mx-nl
        + "   section on the grain the object declares, which is this same tiling. */" + mx-nl
        + "SECTIONS {" + mx-nl
        + "  .love_data : ALIGN(" + (show s) + ") {" + mx-nl
        + "    __start_love_data = .;" + mx-nl
        + (mx-tile "__start_love_data" s "    ")
        + "    __stop_love_data = .;" + mx-nl + "  }" + mx-nl + "} INSERT AFTER .text;" + mx-nl
        + "ASSERT(__stop_love_data - __start_love_data == "
        + (show (tally mx-reps)) + " * " + (show s) + "," + mx-nl
        + "       \"love_data: slot count moved -- sync love.h's ai_data_n and mx.l\")" + mx-nl))

   ; --- and the seats with a full script of their own: splice, don't lay -----------
   ; those scripts are the board's memory map and belong to the board. mx.l takes the
   ; text and answers it with the marked block relaid, so the recipe's IO is a pipe.
   (mx-seat path) (: (go l) (? (! (two? l)) (scare 'mx-seat path)
                                (: r <l (? (<r = path) >r (go >l))))
                     (go mx-seats))
   (mx-lines s) (: n (tally s)
                   (go i j) (? (j = n) (? (i = j) () `((snip s i j)))
                               ((peep s j 0) = 10) (link (snip s i j) (go (j + 1) (j + 1)))
                               (go i (j + 1)))
                   (go 0 0))
   (mx-trim s) (: n (tally s)
                  (go i) (? (i = n) "" ((peep s i 0) = 32) (go (i + 1)) (snip s i n))
                  (go 0))
   (mx-lds path text)
    (: st  (mx-seat path)
       blk (mx-lds-block (mx-stride <st) <>st)
       ; a seat missing either marker is a mistake, not an empty edit -- scare, so the
       ; recipe leaves the committed file alone rather than writing it back unchanged.
       (skip l) (? (! (two? l)) (scare 'mx-lds-shut path)
                   ((mx-trim <l) = mx-ld-shut) (mx-cat l)
                   (skip >l))
       (walk l) (? (! (two? l)) (scare 'mx-lds-open path)
                   ((mx-trim <l) = mx-ld-open) (<l + mx-nl + blk + (skip >l))
                   (<l + mx-nl + (walk >l)))
       (walk (mx-lines text)))

   ; the whole header. the banner is not decoration: a hand edit here is lost the next
   ; time the gate runs, so the file has to say where its source is.
   ; ⚠ a plain BINDING, not a nullary helper: `(mx-h)` is `mx-h` unrun (CLAUDE.md's
   ; zero-operand law), and this file exists to be read by two other programs.
   mx-h
    (: nl2 (string 10)
       (join (map clay-show
        `(`('note ("mx.h -- +/* dispatch matrices. generated by mx.l, do not edit"))
          (mx-clay1 "ai_add_mx" mx-add)
          '(note "")
          (mx-clay1 "ai_mul_mx" mx-mul)
          repq-clay))))
   ; the slot LAYOUT rides kinds.h beside the roster it is a layout of. love.h used
   ; to spell it and asserted its count against enum d; one roster lays both now, so
   ; there is nothing left for that assert to catch.
   slots-clay
    (: nl (string 10)
       `(`('note ("THE DATA SLOT LAYOUT. the sentinels tile one section at ai_data_stride in" + nl
                  + "enum d order, so a value's rep is arithmetic on its ap -- love.c's DSENT," + nl
                  + "love.h's ai_typ, and the six linker scripts laid from this same roster." + nl
                  + "each #if here is a DEFAULT; the guard over it is the -D door."))
         (mx-knob "ai_data_section" mx-sections)
         (mx-knob "ai_data_stride" mx-strides)
         `('cpp-def "ai_data_n" (tally mx-reps))))
   kinds-h
    (: nl2 (string 10)
       ((join (map clay-show
         `(`('note ("kinds.h -- GENERATED by mx.l. do not edit; edit the table there." + nl2))
           kinds-clay
           reps-clay
           '(note ""))))
        + (join (map clay-show slots-clay))))
   ; a shape check before anything is written: gen.l and the C compiler both trust
   ; this, and a malformed tree would arrive as a codegen error naming nothing.
   mx-ok
    (: a (clay-ok? (mx-clay1 "ai_add_mx" mx-add))
       b (clay-ok? (mx-clay1 "ai_mul_mx" mx-mul))
       c (clay-ok? kinds-clay)
       d (clay-ok? reps-clay)
       e (clay-ok? repq-clay)
       f (foldl (\ a x (&& a (id? (clay-ok? x) 1))) 1 slots-clay)
       (&& f (&& (id? a 1) (&& (id? b 1) (&& (id? c 1) (&& (id? d 1) (id? e 1))))))))
