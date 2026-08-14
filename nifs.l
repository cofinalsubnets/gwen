; nifs.l -- THE REGISTRY. love.c's nif roster and its instruction roster as love data,
; and the C laid from them: nifs.h, through clay. doc/clay.md rung 4.
;
; it lives HERE, beside love.c and the header it lays, because that is what it is:
; love.c's definitions, written as data rather than as text. tools/ is for tools.
;
; the rosters used to BE C -- five #defines minting the little stream a nif runs, an
; X-macro per roster, and three CONSUMER macros handed to them, because C cannot say
; "the same list, a different reader" any other way. two sites expanded them, and the
; consumer macros existed for nothing else.
;
; a love loop says it once. the roster is the source, the stream table and the def1
; table are both derivations, so a nif cannot enter one and miss the other -- which is
; the arrangement those five macros were making by hand. this is doc/clay.md's route
; (b), the one place a macro genuinely goes.
;
; ⚠ EDIT THIS FILE, NOT nifs.h. `make test_clay` regenerates it and fails on drift.
; ⚠ nifs.h is COMMITTED and no make rule depends on this file: a new row builds clean and
; gates GREEN while its nom is still missing from the book. `make mx` is the refresh.
(:
   ; clay is a module: `use` splices it, so clay-show / clay-ok? -- and the el/join it
   ; stands on -- all read bare. no accessor, and it has to come first.
   _ (use 'clay)

   ; --- the nifs. one row per (ARRAY name arity IMPL): the C symbol the row is known
   ; by, the love name it binds in the book, how many operands it takes, and the lvm_
   ; that does the work. arity IS the body -- one runs the op straight, and two through
   ; five curry on lvm_cur first -- so the column is a shape, never a number the C reads.
   ; `+` and `*` of two functions are prel's stack/compose, reached from
   ; lvm_addh/lvm_mulh through g->hot_stack / g->hot_compose.
   nif-rows
    '(
      (nif_clock         "clock"         1 lvm_clock)
      (nif_nclock        "nclock"        1 lvm_nclock)
      (nif_please        "please"        1 lvm_please)
      (nif_gauge         "gauge"         1 lvm_gauge)
      (nif_tune          "tune"          1 lvm_tune)
      (nif_apof          "apof"          1 lvm_apof)
      (nif_seal          "seal-hook"     2 lvm_seal)
      (nif_heard         "heard"         1 lvm_heard)
      (nif_worn          "worn"          1 lvm_worn)
      (nif_myself        "myself"        1 lvm_myself)
      (nif_books         "books"         1 lvm_books)
      (nif_setbooks      "setbooks"      1 lvm_setbooks)
      (nif_mods          "mods"          1 lvm_mods)
      (nif_lib           "lib"           1 lvm_lib)
      (nif_add           "+"             2 lvm_add)
      (nif_sub           "-"             2 lvm_sub)
      (nif_mul           "*"             2 lvm_mul)
      (nif_quot          "/"             2 lvm_quot)
      (nif_fquot         "//"            2 lvm_fquot)
      (nif_rem           "%"             2 lvm_rem)
      (nif_lt            "<"             2 lvm_lt)
      (nif_le            "<="            2 lvm_le)
      (nif_eq            "="             2 lvm_eq)
      (nif_ge            ">="            2 lvm_ge)
      (nif_gt            ">"             2 lvm_gt)
      (nif_same          "id?"           2 lvm_same)
      (nif_bsl           "<<"            2 lvm_bsl)
      (nif_bsr           ">>"            2 lvm_bsr)
      (nif_band          "&"             2 lvm_band)
      (nif_bor           "|"             2 lvm_bor)
      (nif_bxor          "^"             2 lvm_bxor)
      (nif_link          "link"          2 lvm_link)
      (nif_car           "cap"           1 lvm_cap)
      (nif_cdr           "cup"           1 lvm_cup)
      (nif_sort          "sort"          1 lvm_sort)
      (nif_tally         "tally"         1 lvm_tally)
      (nif_longp         "long?"         2 lvm_longp)
      (nif_snip          "snip"          3 lvm_snip)
      (nif_sound0        "sound0"        1 lvm_sound0)
      (nif_string        "string"        1 lvm_string)
      (nif_intern        "name"          1 lvm_intern)
      (nif_mint          "mint"          1 lvm_mint)
      (nif_nomctor       "nom"           1 lvm_nomctor)
      (nif_spin          "spin"          1 lvm_spin)
      (nif_peek          "peek"          2 lvm_peek)
      (nif_lamsrc        "lamsrc"        1 lvm_lamsrc)
      (nif_nifnom        "nifnom"        1 lvm_nifnom)
      (nif_poke          "poke"          3 lvm_poke)
      (nif_trim          "trim"          1 lvm_trim)
      (nif_seek          "seek"          2 lvm_seek)
      (nif_saturate      "saturate"      1 lvm_saturate)
      (nif_peep          "peep"          3 lvm_peep)
      (nif_pin           "pin"           3 lvm_pin)
      (nif_pull          "pull"          3 lvm_pull)
      (nif_table         "tablet"        1 lvm_tablet)
      (nif_keys          "keys"          1 lvm_keys)
      (nif_dig           "dig"           1 lvm_dig)
      (nif_casknew       "cask"          1 lvm_casknew)
      (nif_bcopy         "pour"          5 lvm_bcopy)
      (nif_chainp        "link?"         1 lvm_chainp)
      (nif_strp          "string?"       1 lvm_strp)
      (nif_gem           "gem"           1 lvm_gem)
      (nif_gemp          "gem?"          1 lvm_gemp)
      (nif_sin           "sine"          1 lvm_sin)
      (nif_cos           "cosine"        1 lvm_cos)
      (nif_log           "log"           1 lvm_log)
      (nif_pow           "power"         2 lvm_pow)
      (nif_twin          "twin"          2 lvm_twin)
      (nif_twinp         "twin?"         1 lvm_twinp)
      (nif_re            "re"            1 lvm_re)
      (nif_im            "im"            1 lvm_im)
      (nif_conj          "conj"          1 lvm_conj)
      (nif_abs           "abs"           1 lvm_abs)
      (nif_arg           "arg"           1 lvm_carg)
      (nif_tray          "tray"          3 lvm_trayctor)
      (nif_iota          "iota"          1 lvm_iota)
      (nif_nif           "nif"           4 lvm_nif)
      (nif_nifx          "nifx"          5 lvm_nifx)
      (nif_rank          "rank"          1 lvm_rank)
      (nif_alen          "alen"          1 lvm_alen)
      (nif_shape         "shape"         1 lvm_shape)
      (nif_atype         "atype"         1 lvm_atype)
      (nif_asum          "asum"          1 lvm_asum)
      (nif_aprod         "aprod"         1 lvm_aprod)
      (nif_max           "max"           1 lvm_max)
      (nif_min           "min"           1 lvm_min)
      (nif_aall          "aall"          1 lvm_aall)
      (nif_inner         "inner"         2 lvm_inner)
      (nif_outer         "outer"         2 lvm_outer)
      (nif_packp         "packp"         1 lvm_packp)
      (nif_bigp          "big?"          1 lvm_bigp)
      (nif_sunp          "sun?"          1 lvm_sunp)
      (nif_setp          "tray?"         1 lvm_setp)
      (nif_intf          "int"           1 lvm_intf)
      (nif_nomp          "nom?"          1 lvm_nomp)
      (nif_namep         "name?"         1 lvm_namep)
      (nif_mintp         "mint?"         1 lvm_mintp)
      (nif_tabp          "tablet?"       1 lvm_tabp)
      (nif_charmp        "charm?"        1 lvm_charmp)
      (nif_litp          "lit?"          1 lvm_litp)
      (nif_hotp          "hot?"          1 lvm_hotp)
      (nif_nilp          "nil?"          1 lvm_nilp)
      (nif_ev            "ev"            1 lvm_eval)
      (nif_callk         "call-cc"       1 lvm_callk)
      (nif_scare         "scare"         2 lvm_scare)
      (nif_yield         "yield"         1 lvm_yield_nif)
      (nif_spawn         "twirl"         2 lvm_spawn)
      (nif_wait          "catch"         1 lvm_wait)
      (nif_sleep         "rest"          1 lvm_sleep)
      (nif_donep         "landed?"       1 lvm_donep)
      (nif_scoop         "scoop"         1 lvm_scoop)
      (nif_hush          "freeze"        1 lvm_hush)
      (nif_key           "cue?"          1 lvm_key)
      (nif_fputbn        "putbn"         3 lvm_fputbn)
      (nif_await         "await"         1 lvm_await)
      (nif_fgetc         "see"           1 lvm_fgetc)
      (nif_fungetc       "unsee"         2 lvm_fungetc)
      (nif_chug          "chug"          1 lvm_chug)
      (nif_unchug        "unchug"        2 lvm_unchug)
      (nif_inhand        "inhand"        1 lvm_inhand)
      (nif_fputc         "put"           2 lvm_fputc)
      (nif_fputs         "say"           2 lvm_fputs)
      (nif_fflush        "flush"         1 lvm_fflush)
      (nif_wheel         "wheel"         1 lvm_wheel)
      (nif_turn          "turn"          1 lvm_turn)
      (nif_turnf         "turnf"         1 lvm_turnf)
      (nif_coinmk        "coin"          2 lvm_coinmk)
      (nif_load          "load"          1 lvm_load)
      (nif_dieof         "die-of"        1 lvm_dieof)
      (nif_coinp         "coin?"         1 lvm_coinp)
      (nif_calloutdrive  "calloutdrive"  1 lvm_calloutdrive)
      (nif_calloutresume "calloutresume" 1 lvm_calloutresume)
      )

   ; --- the instructions: the ops the codegen emits. they bind by NAME so the
   ; compiler can name one while it emits, and the egg pulls every lvm_ off the book
   ; at birth (love/egg.l) -- scaffolding, never a user's vocabulary. no name column
   ; and no arity: an instruction is jumped to, not applied, so its C symbol is the
   ; only spelling it has.
   nif-insts
    '(
      lvm_unc lvm_index lvm_ret lvm_ap lvm_tap lvm_apn lvm_tapn lvm_jump lvm_cond lvm_arg
      lvm_quote lvm_defglob lvm_argap lvm_quoteap lvm_argtap lvm_arg0 lvm_arg1 lvm_arg2
      lvm_arg3 lvm_quo0 lvm_quo1 lvm_quo2 lvm_quo3 lvm_quom1 lvm_quom2 lvm_aa lvm_aq lvm_qa
      lvm_qq lvm_aap lvm_aqp lvm_qap lvm_qqp lvm_argcap lvm_argcup lvm_argtwo lvm_argcond
      lvm_argtwocond
      )

   ; --- the C, through clay ------------------------------------------------------
   ; ONE table, not 126 arrays: a nif's stream is a RUN inside it, and def1 carries the
   ; run's address. the runs are two shapes -- {impl, ret0} at one operand, and
   ; {cur, count, impl, ret0} above that -- so they are spliced in flat, and the seam
   ; between them lives here rather than in the C.
   (vr s) `('var s)
   (cell e) `('init `(e))          ; one `union u`: braced, the ap being its first member
   (catl l) (foldl (+) () l)
   (nif-run a im)                  ; the cells of one nif's run
    (: end `((cell (vr (string im))) (cell (vr "lvm_ret0")))
       (? (a < 2) end
          (`((cell (vr "lvm_cur"))
             (cell `('dfield "x" `('call (vr "putcharm") `(`('num a))))))
           + end)))
   (nif-wide? r) (1 < (el r 2))    ; a curried run is four cells, a plain one two
   nif-table
    `('gdecl `(`("nifs" '(arr (const (named "union u")) 0)
                 `('init (catl (map (\ r (nif-run (el r 2) (el r 3))) nif-rows)))))
      '(static))

   ; where each nif's run STARTS. a running sum over the roster, which is the whole
   ; reason the table can be one object: the offsets are the loop's business, and no
   ; hand ever has to keep 126 of them straight.
   nif-ats
    (: (go l n) (? (! (two? l)) () (link n (go >l (n + (? (nif-wide? <l) 4 2)))))
       (go nif-rows 0))

   ; the book table: each row is (love name, the value as a word). a nif carries its
   ; run's address, an instruction carries itself, and ai_defn reads the pair. UNSIZED
   ; on purpose -- C counts the rows, so countof cannot disagree with the roster.
   ; ⚠ `init`, not `irow`: a def1 row is a struct and owns its braces. it is already
   ; one line, because its two cells are scalars and the flat rule keeps them whole.
   (def-row nm v) `('init `(`('str nm) `('cast '(named "intptr_t") v)))
   (nif-at n) (? (n < 1) (vr "nifs") `('bin '+ (vr "nifs") `('num n)))
   (zip a b) (? (! (two? a)) () (link `(<a <b) (zip >a >b)))
   nif-def1
    `('gdecl `(`("def1" '(arr (const (struct "ai_def")) 0)
                 `('init ((map (\ p (def-row (el <p 1) (nif-at <>p))) (zip nif-rows nif-ats))
                          + (map (\ i (def-row (string i) (vr (string i)))) nif-insts)))))
      '(static))

   ; the whole header. the banner is not decoration: a hand edit here is lost the next
   ; time the gate runs, so the file has to say where its source is.
   ; ⚠ a plain BINDING, not a nullary helper: `(nifs-h)` is `nifs-h` unrun.
   nifs-h
    (join (map clay-show
      `('(note "nifs.h -- the nif + instruction registry. generated by nifs.l, do not edit")
        nif-table '(note "") nif-def1)))
   ; a shape check before anything is written: gen.l and the C compiler both trust
   ; this, and a malformed tree would arrive as a codegen error naming nothing.
   nifs-ok
    (: bad (any (\ c (! (id? (clay-ok? c) 1))) `(nif-table nif-def1))
       (? bad 0 1)))
