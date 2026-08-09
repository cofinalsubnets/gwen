; crew/moon/clay.l -- CLAY: C as love data. the AST moon has produced all along,
; given a NAME, a VALIDATOR and a SHOWER, so that a piece of C can be GENERATED from
; a love table rather than transcribed by hand -- holo's relationship to assembly,
; one level up. the plan, the ranked slate and the gates are doc/clay.md.
;
; a registered module: (use 'clay), then clay-show / clay-ok? / clay-tags. nothing
; here depends on the rest of moon -- a shower is pure AST -> text -- so a generator
; can take clay without dragging in the lexer, the preprocessor and the parser. the
; G1 gate (test/host/clay.l) is the one consumer that wants both halves.
;
; --- ⚠ THE ONE THING TO KNOW: CLAY IS A SUPERSET, AND IT HAS TO BE ---------------
; cparse's AST is not a complete C representation, and this is not a small caveat.
; a top-level DECLARATION mostly does not survive it:
;
;   typedef struct P { int x; } P;   ->  (tdef)          the name and the shape both gone
;   int f(int a);                    ->  (proto "f")     the signature gone
;   long g(int a) { .. }             ->  (fn "g" (("a" int)) (blk ..))   no RETURN TYPE
;   static int const t[2] = ..       ->  (gdecl (("t" (arr int 2) ..)))  no static, no const
;
; that information is real, it is just not in the TREE -- it lives in the side
; tables cparse answers alongside it (stag, sigs) and in the parse state's static
; and alias tablets. gen.l is fine with that because it is HANDED those tables. a
; SHOWER is not: it has only the tree.
;
; so clay's grammar is cparse's tags PLUS the slots that carry what cparse drops,
; and the extra slots are always APPENDED -- the lexer's glued-paren flag set that
; precedent, and it is what keeps clay acceptable to (cgen-obj ..) unchanged:
;
;   (fn name params body RET ATTRS)    the return type, absent = int; then attribute
;                                      SPELLINGS ("ai_noinline"), love.h's macros call-shaped
;   (proto name PARAMS RET ATTRS)      the same three slots on a forward declaration, which
;                                      cparse keeps the NAME of and nothing else
;   (ret e PRE)                        a prefix spelling: `ai_musttail return ..`
;   (gdecl decls STOR)                 (static) / (extern) / (), absent = ()
;   (restrict t)                       the qualifier beside (const t) -- pointers only
;   (sassert e "msg")                  a _Static_assert, which cparse reduces to (tdef)
;   (tdef NAME TY)                     a real typedef, which cparse can never emit
;   (sdef TAG FIELDS)                  a struct/union DEFINITION, which it can never emit
;   (named "lvm_t")   (const t)        type forms with no cparse counterpart
;   (arr t EXPR)                       a symbolic bound -- cparse folds these to numbers,
;   (didx EXPR val)                    a GENERATOR wants to say [KCharm], not [2]
;   (chr 92)                           a character literal, which cparse folds to a number
;   (note "text")                      a comment, authored, never captured
;   (cpp-def NAME [BODY])              the preprocessor, likewise never captured
;   (cpp-undef NAME)   (cpp-if ARMS [ELSE])
;
; a clay term that came out of cparse is therefore a SUBSET term, and the lossy
; forms show as the best C the tree can justify: an absent return type prints
; `int`, and `(proto "f")` prints `int f();` -- a reconstruction, not a
; reproduction. `(tdef)` alone is the one form that REFUSES, because it carries
; nothing at all and a `;` in its place would be pretending. that refusal is what
; G1 counts.
;
; --- comments -------------------------------------------------------------------
; `note` is EMIT-ONLY. cparse cannot capture a comment (lex.l has no comment token
; and cpp.l runs first), so notes are AUTHORED in the generator, exactly as
; port/inle/mkvec.l carries its narrative in the love that lays the assembly.
; doc/clay.md's "comments, in two halves" prices the capture side; it is not built.
;
; --- the preprocessor -------------------------------------------------------------
; emit-only by construction, and more firmly than `note` is: cpp runs BEFORE the lexer,
; so a directive is gone by the time cparse could see one. love.c is ONE TEXT compiled
; for many targets and test_fixpoint wants the mooncc rebuild byte-identical, so the
; architecture conditionals cannot be folded away by generating per seat -- a generator
; has to emit the `#if` itself. doc/clay.md's rung 2.
;   (cpp-def NAME [BODY])  NAME a SPELLING, carrying its own parameter list where it has
;                          one ("countof(_)"), which is what cpp reads. BODY absent is a
;                          bare `#define` -- presence is the arity, never the value.
;   (cpp-if ARMS [ELSE])   ARMS is ((cond items)..): the first arm `#if`, the rest
;                          `#elif`, ELSE the `#else` items. items are TOP-LEVEL forms at
;                          column 0, so a cpp-if nests inside one as love.h's do.
; ⚠ a #define's body is TOKENS, not an expression. a spelling rides through verbatim; a
; clay expression renders at the TIGHTEST context, so anything composite parenthesizes
; itself -- a macro body cannot see where it is substituted.
; ⚠ `#ifdef X` is `#if defined(X)`: (call (var "defined") ((var "X"))), no node owed --
; `defined` wears call syntax in C's own grammar.
(:
   ; --- the small vocabulary this file stands on ---------------------------------
   ; el/join/sepby/gripe are two lines each, and defining them here is what lets
   ; clay load with no moon behind it. lex.l's gripe trio is the same shape; the
   ; two copies live in different module layers and cannot clobber each other.
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   (join l) (foldl (+) "" l)
   (sepby s l) (? !(two? l) "" (foldl (\ a x (a + s + x)) <l >l))
   nl (string 10)
   (gripe msg) `('gripe () () () msg)
   (gripe? x) (two? x && id? <x 'gripe)
   ; look answers a row's FIRST value (the precedence tables' shape); lookr answers
   ; the whole tail, which is what an arity RANGE needs.
   (look tbl k d) (: (go l) (? !(two? l) d
                               (: e <l (? (id? <e k) <>e (go >l))))
                     (go tbl))
   (lookr tbl k) (: (go l) (? !(two? l) ()
                              (: e <l (? (id? <e k) >e (go >l))))
                    (go tbl))

   ; --- the node grammar, as data ------------------------------------------------
   ; (tag lo hi): the operand count clay-ok? admits, hi -1 meaning any. the RANGES
   ; are where the superset lives -- `fn 3 4` is cparse's three plus clay's return
   ; type, `tdef 0 2` is cparse's empty marker and clay's real typedef.
   clay-tags
    ; ⚠ `fn` is OVERLOADED in moon's grammar: (fn name params body [ret]) is a
    ; definition and (fn ret) is the function TYPE under a pointer, so the range has
    ; to admit both and cannot catch a truncated definition. the only alternative is
    ; renaming one of them in parse.l, which is not clay's edit to make.
    ; ⚠ `edef` is EMIT-ONLY, and cannot be otherwise: cparse reduces a top-level
    ; `enum q { .. };` to the bare `(tdef)` marker (parse.l:1509), folding the
    ; constants into its `enums` tablet and keeping neither the tag nor the names.
    ; so an edef only ever comes from a generator, never back from a parse -- the
    ; same standing `note` has, and for the same reason.
    ; ⚠ `sdef` is emit-only for the same reason, and its FIELDS take the shape
    ; parse's own pmembers answers -- ((name ty)..) -- so the day parse.l fills
    ; (tdef) the member list is already the one it hands back. the 3rd slot is
    ; 'union: a union DEFINES with its own keyword but REFERS as ('struct tag),
    ; since moon's tag namespace holds both (parse.l:10) -- not clay's choice.
    ; ⚠ `sassert` is emit-only like edef/sdef: parse reduces a _Static_assert to the
    ; bare (tdef) marker, so one only ever comes from a generator.
    ; ⚠ the cpp trio is emit-only more firmly than any of those: cpp runs before the
    ; lexer, so a directive never reaches a parse at all. see the header.
    '((prog 1 1) (fn 1 5) (proto 1 4) (gdecl 1 2) (xdecl 1 2) (tdef 0 2) (note 1 1)
      (edef 2 2) (sdef 2 3) (sassert 2 2) (cpp-def 1 2) (cpp-undef 1 1) (cpp-if 1 2)
      (blk 1 1) (decl 1 1) (sdecl 1 1) (ret 1 2) (if 2 3) (while 2 2) (for 4 4)
      (do 2 2) (switch 2 2) (case 1 1) (dflt 0 0) (brk 0 0) (cont 0 0) (goto 1 1)
      (lbl 1 1) (expr 1 1) (nop 0 0) (asm 4 4)
      (num 1 1) (flo 1 1) (str 1 1) (chr 1 1) (var 1 1) (bin 3 3) (un 2 2) (asn 2 2)
      ; ('rmw op lv rhs) -- `lv op= rhs` kept whole, because the desugar to
      ; (asn lv (bin op lv rhs)) would evaluate lv twice (parse.l's calm?)
      (rmw 3 3)
      (post 2 2) (cond 3 3) (comma 2 2) (call 2 2) (deref 1 1) (addr 1 1)
      (dot 2 2) (cast 2 2) (szof 1 1) (init 1 1) (dfield 2 2) (didx 2 2)
      (clit 2 2) (land 2 2) (lor 2 2) (vastart 1 2) (vaarg 2 2) (vaend 1 1)
      ; type forms -- validated by the same table, since a type IS a clay node
      ; ('varr elt dim-expr [idx]) -- the optional third is gen's VLA-local slot
      ; index (gen.l:2071), bookkeeping the shower has nothing to say about
      (ptr 1 1) (arr 2 2) (varr 2 3) (struct 1 1) (named 1 1) (const 1 1) (restrict 1 1))

   ; --- clay-ok? -----------------------------------------------------------------
   ; gen.l TRUSTS its input; a generator that hands it a malformed tree gets a
   ; codegen crash naming nothing. so: walk the tree, check every head is a known
   ; tag at an admitted arity, answer 1 or a gripe naming the first offender. this
   ; is a SHAPE check, not a type check -- it cannot know that (bin + x y) is
   ; well-typed, only that `bin` takes three.
   (clay-ok? c)
    (: (bad m) (gripe ("clay: " + m))
       (go x)
        (? !(two? x) ()                             ; a leaf: a name, a number, a string
           !(name? <x) (bad "a form whose head is not a tag")
           (: h <x n (tally >x) r (lookr clay-tags h)
              (? !(two? r) (bad ("unknown tag " + string h))
                 (n < <r) (bad (string h + " wants at least " + show <r + " operands"))
                 ((-1 < <>r) && (<>r < n)) (bad (string h + " takes at most " + show <>r + " operands"))
                 ; ⚠ a gdecl/xdecl's STORAGE slot is a list of bare keywords, not
                 ; nodes -- `(static)` walked as a form would read `static` as a tag.
                 ; so the walk takes the declarators and stops. an edef is the same
                 ; hazard whole: a bare constant is a name and a valued one is
                 ; `(nom expr)`, whose head would read as a tag, so it has no kids at
                 ; all -- the price of letting a constant name itself. a full fn's or
                 ; proto's ATTRS slot is a list of spellings, so the walk stops before it.
                 (: (kids l) (? !(two? l) ()
                                (: e <l
                                   (? (two? e) (: v (? (name? <e) (go e) (kids e)) (? v v (kids >l)))
                                      (kids >l))))
                    (kids (? (member? h '(gdecl xdecl)) `(<>x)
                             (id? h 'edef) ()
                             (id? h 'fn && (4 < n)) `((el x 2) (el x 3) (el x 4))
                             (id? h 'proto && (3 < n)) `((el x 2) (el x 3))
                             >x))))))
       v (go c)
       (? v v 1))

   ; --- types --------------------------------------------------------------------
   ; a C declaration is a BASE plus a DECLARATOR wrapped around the name, and the
   ; two halves have to be built separately -- `lvm_t *const m[KN][KN]` is one type
   ; whose text brackets its own name. cty-base peels to the innermost specifier;
   ; cty-dtor grows the declarator outward, carrying `p` = "what I have built so far
   ; is a pointer", which is exactly when an array or function suffix must
   ; parenthesize it (`int (*fp)()`, not `int *fp()`).
   ; the unsigned scalars canonicalize to ONE symbol at parse (`unsigned long` ->
   ; 'ulong, parse.l:99), so the base name has to expand back out -- `ulong` is
   ; moon's word for the type, not C's.
   spelling '((uchar "unsigned char") (ushort "unsigned short") (uint "unsigned int")
              (ulong "unsigned long") (ullong "unsigned long long") (llong "long long"))
   (cptr? t) (two? t && id? <t 'ptr)
   ; a tag spells as itself, string or symbol. cident? is a FIRST-character check and
   ; wants to be no more: a tag reaches here from a C lexer or from a generator, and
   ; the one non-identifier in that set is parse's anonymous `.anon0` (parse.l:614).
   (ctag a) (? (string? a) a (string a))
   (cident? a) (: s (ctag a) c (? (tally s) (s 0) 0)
                  (((96 < c) && (c < 123)) || ((64 < c) && (c < 91)) || (c = 95)))
   (cty-base t)
    (? (name? t) (look spelling t (string t))
       !(two? t) "int"
       (: h <t a <>t
          (? (id? h 'ptr) (cty-base a)
             (id? h 'arr) (cty-base a)
             (id? h 'fn) (cty-base a)
             (id? h 'named) a
             (id? h 'struct) ("struct " + ctag a)
             ; const binds to the DECLARATOR when the thing under it is a pointer
             ; (`lvm_t *const`), to the base otherwise (`size_t const`). restrict
             ; qualifies a pointer or nothing, so the base only ever peels it.
             (id? h 'const) (? (cptr? a) (cty-base a) (cty-base a + " const"))
             (id? h 'restrict) (cty-base a)
             "int")))
   (cty-dtor t s p)
    (? !(two? t) s
       (: h <t a <>t
          (? (id? h 'ptr) (cty-dtor a ("*" + s) 1)
             (id? h 'const) (? (cptr? a) (cty-dtor <>a ("*const " + s) 1) (cty-dtor a s p))
             (id? h 'restrict) (? (cptr? a) (cty-dtor <>a ("*restrict " + s) 1) (cty-dtor a s p))
             (id? h 'arr) (cty-dtor a ((? p ("(" + s + ")") s) + "[" + cdim <>>t + "]") 0)
             ; a VLA: the bound is an expression, never a fold
             (id? h 'varr) (cty-dtor a ((? p ("(" + s + ")") s) + "[" + ce <>>t 0 + "]") 0)
             (id? h 'fn) (cty-dtor a ((? p ("(" + s + ")") s) + "()") 0)
             s)))
   ; an array bound: a charm prints, 0 is the UNSIZED `[]` (a flexible member, or a
   ; bound cparse inferred from an initializer and clay is free to leave open), and
   ; anything else is a clay expression -- the generator's `[KN]`.
   (cdim n) (? (charm? n) (? (n = 0) "" (show n)) (ce n 0))
   ; a full declaration: type wrapped around s. s empty gives the ABSTRACT
   ; declarator a cast wants.
   (cty t s) (: d (cty-dtor t s 0) b (cty-base t) (? !(tally d) b (b + " " + d)))

   ; --- literals -----------------------------------------------------------------
   ; ⚠ a NEGATIVE (num n) has no C literal spelling: `-1` is a unary minus applied
   ; to 1, and only a constant context folds it back. so a negative number round-
   ; trips where cparse folded it (a global initializer, an array bound) and does
   ; not where it did not. that is a fact about C, not about the shower.
   (cnum n) (show n)
   ; a float literal must LOOK like one to a C lexer, and love's shortest-roundtrip
   ; printer says `7` for 7.0 and `1e308` for the big one -- so `.0` is owed to the
   ; first and would corrupt the second. anything already carrying a `.` or an
   ; exponent is a float literal as it stands.
   ; an infinity or a nan prints as `ieee-inf` / `ieee-nan` -- love's spelling, and C
   ; has no literal at all for either. so a float literal is sayable exactly when its
   ; printed form is made of the characters a C float literal is made of.
   (cfinite? x) (: s (show x)
                   !(any (\ i !(member? (s i) '(46 69 101 43 45 48 49 50 51 52 53 54 55 56 57)))
                         (jot (tally s))))
   (cflo x) (: s (show x)
               b (map s (jot (tally s)))
               (? (member? 46 b || member? 101 b || member? 69 b) s (s + ".0")))
   ; ⚠ `//`, not `/`: love's `/` is exact, so (1 / 64) is 0.015625 and the octal
   ; digit comes out a float
   (oct3 b) (string (48 + ((b // 64) % 8)) + string (48 + ((b // 8) % 8)) + string (48 + (b % 8)))
   (cesc b)
    (? (b = 34) "\\\"" (b = 92) "\\\\" (b = 10) "\\n" (b = 9) "\\t" (b = 13) "\\r"
       ((31 < b) && (b < 127)) (string b)
       ("\\" + oct3 b))
   (cstrlit s) ("\"" + join (map (\ i (cesc (s i))) (jot (tally s))) + "\"")
   ; a character literal. cparse folds one to its number -- it is compiling -- so this
   ; is emit-only, and it exists for the sites where the number is the wrong thing to
   ; read: `n->bytes[0] == '\\'` says what 92 only encodes. the two quotes swap roles
   ; against a string's, and nothing else about the escapes changes.
   (cchr b) ("'" + (? (b = 39) "\\'" (b = 34) "\"" (cesc b)) + "'")

   ; --- expressions --------------------------------------------------------------
   ; precedence-aware rather than fully parenthesized: this C is CHECKED INTO GIT and
   ; read by people, and G1's round-trip is what keeps the table honest -- a wrong
   ; row shows up as a tree that came back different, which is the whole reason the
   ; gate compares ASTs and never strings.
   binprec '((* 80) (/ 80) (% 80) (+ 75) (- 75) (shl 70) (shr 70)
             (lt 65) (gt 65) (le 65) (ge 65) (eq 60) (ne 60)
             (& 55) (^ 50) (| 45))
   binop '((* "*") (/ "/") (% "%") (+ "+") (- "-") (shl "<<") (shr ">>")
           (lt "<") (gt ">") (le "<=") (ge ">=") (eq "==") (ne "!=")
           (& "&") (^ "^") (| "|"))
   unop '((neg "-") (not "!") (bnot "~"))
   ; `a[i]` is (deref (bin + a i)) -- cparse desugars indexing -- and it binds like the
   ; POSTFIX it is written as, not like the `*` it is written from. that is the whole
   ; difference between `k[0].ap` and `(k + 0)->ap`, which are the same tree.
   (cidx? e) (two? e && id? <e 'deref &&
              (: t (el e 1) (two? t && id? <t 'bin && id? (el t 1) '+)))
   (cprec e)
    (? !(two? e) 100
       (: h <e
          (? (id? h 'bin) (look binprec <>e 100)
             (id? h 'land) 40 (id? h 'lor) 35 (id? h 'cond) 30
             (id? h 'asn) 25 (id? h 'rmw) 25 (id? h 'comma) 20
             (id? h 'un) 90 (id? h 'deref) (? (cidx? e) 100 90) (id? h 'addr) 90
             (id? h 'cast) 90 (id? h 'clit) 90
             100)))
   ; ce: render e, parenthesized when it binds looser than the context wants.
   (ce e p) (: q (cprec e) t (craw e) (? (q < p) ("(" + t + ")") t))
   (craw e)
    (? !(two? e) (? (string? e) e (show e))
       (: h <e a (el e 1)
          (? (id? h 'num) (cnum a)
             (id? h 'flo) (cflo a)
             (id? h 'str) (cstrlit a)
             (id? h 'chr) (cchr a)
             (id? h 'var) a
             (id? h 'bin) (: p (look binprec a 100)
                              (ce (el e 2) p + " " + look binop a "?" + " " + ce (el e 3) (p + 1)))
             (id? h 'land) (ce a 40 + " && " + ce (el e 2) 41)
             (id? h 'lor) (ce a 35 + " || " + ce (el e 2) 36)
             ; ?: and = are RIGHT-associative, so the tight side flips
             (id? h 'cond) (ce a 31 + " ? " + ce (el e 2) 31 + " : " + ce (el e 3) 30)
             (id? h 'asn) (ce a 90 + " = " + ce (el e 2) 25)
             (id? h 'rmw) (ce (el e 2) 90 + " " + look binop a "?" + "= " + ce (el e 3) 25)
             (id? h 'comma) (ce a 21 + ", " + ce (el e 2) 20)
             ; ⚠ a space between the operator and an operand that starts with the
             ; SAME character: `- -x`, never `--x`, which the lexer would read as a
             ; predecrement. (-(-x) is real -- it is how the folder leaves a double
             ; negation it could not fold.)
             (id? h 'un) (: o (look unop a "?") r (ce (el e 2) 90)
                             (o + (? (tally r && (r 0 = o 0)) " " "") + r))
             ; printing the star back would be faithful but unreadable. the sugar
             ; re-desugars on the way in, so G1 holds either way; this is the one place
             ; clay chooses the nicer of two forms that mean the same tree.
             (id? h 'deref) (: t a
                               (? (cidx? e) (ce (el t 2) 100 + "[" + ce (el t 3) 0 + "]")
                                  ("*" + ce t 90)))
             (id? h 'addr) ("&" + ce a 90)
             (id? h 'post) (ce a 95 + (? (0 < el e 2) "++" "--"))
             ; a uint/ulong cast around a bare literal prints as the SUFFIX: spelled as a
             ; cast, the reparse ladder (C11 6.4.4.1) wraps a big value in its own cast
             ; and doubles this one. same tree, so G1 holds -- like the deref sugar above.
             (id? h 'cast)
              (: t2 (el e 2)
                 sfx (? !(two? t2 && id? <t2 'num && (0 <= el t2 1)) ()
                        (id? a 'uint && (el t2 1 <= 4294967295)) "u"
                        (id? a 'ulong) "ul" ())
                 (? sfx (cnum (el t2 1) + sfx)
                    ("(" + cty a "" + ") " + ce t2 90)))
             (id? h 'szof) ("sizeof(" + (? (ctype? a) (cty a "") (ce a 0)) + ")")
             ; a call's arguments sit ABOVE the comma operator, hence 21
             (id? h 'call) (ce a 100 + "(" + sepby ", " (map (\ x (ce x 21)) (el e 2)) + ")")
             ; (dot (deref x) "f") is what `x->f` parses to, so print the arrow back --
             ; unless the deref is an INDEX, where `a[i].f` is the arrow already spent
             (id? h 'dot) (: t a
                              (? (two? t && id? <t 'deref && !(cidx? t))
                                 (ce <>t 100 + "->" + el e 2)
                                 (ce t 100 + "." + el e 2)))
             (id? h 'init) (cinit e 0)
             (id? h 'clit) ("(" + cty a "" + ") " + craw (el e 2))
             (id? h 'vastart) ("__builtin_va_start(" + ce a 21 + ", " + ce (el e 2) 21 + ")")       ; clay's 2-slot form; the 1-slot one never reaches here (clay-unsaid)
             (id? h 'vaarg) ("__builtin_va_arg(" + ce a 21 + ", " + cty (el e 2) "" + ")")
             (id? h 'vaend) ("__builtin_va_end(" + ce a 21 + ")")
             "/*?*/")))
   ; a type shows up in operand position for sizeof; tell the two apart by the head
   (ctype? t) (? (name? t) (member? t '(char short int long uint void double float))
                 (two? t && member? <t '(ptr arr struct named const restrict fn)))
   ; a brace initializer, and the ONE layout rule the shower has: a NESTED initializer
   ; breaks the line, a flat one does not. so a 16x16 dispatch matrix comes out one
   ; ROW per line -- which is the unit you read a matrix in -- while the row itself
   ; stays whole. no threshold to tune, and it falls out of the data's own shape.
   (cinitv x) (? (two? x && member? <x '(dfield didx)) (el x 2) x)
   (cnested? l) (any (\ x (: v (cinitv x) (two? v && id? <v 'init))) l)
   (cinit x ind)
    (? !(two? x) (ce x 21)
       (id? <x 'dfield) ("." + el x 1 + " = " + cinit (el x 2) ind)
       (id? <x 'didx) ("[" + (? (charm? (el x 1)) (show (el x 1)) (ce (el x 1) 0)) + "] = " + cinit (el x 2) ind)
       (id? <x 'init)
        (: l (el x 1)
           (? (cnested? l)
              ("{" + nl + join (map (\ i (pad (ind + 1) + cinit i (ind + 1) + "," + nl)) l)
               + pad ind + "}")
              ("{ " + sepby ", " (map (\ i (cinit i ind)) l) + " }")))
       (ce x 21))

   ; --- declarations -------------------------------------------------------------
   ; one `decl` is one C declaration and may carry SEVERAL declarators (`int a, *b;`),
   ; so they share a base and each grows its own -- splitting them into two
   ; statements would come back as two nodes.
   (cstor st) (? !(two? st) "" (sepby " " (map string st) + " "))
   (cdtor1 d) (: nm <d t (el d 1) v (el d 2)
                 (cty-dtor t nm 0 + (? !(two? v) "" (" = " + cinit v 0))))
   (cdecls ds) (: b (cty-base (el <ds 1))
                  (b + " " + sepby ", " (map cdtor1 ds)))

   ; --- statements ---------------------------------------------------------------
   ; one space per level, love.c's own indent.
   (pad n) (? (n < 1) "" (" " + pad (n - 1)))
   (line n s) (pad n + s + nl)
   ; a `note` is the only node that prints as prose. multi-line text becomes one
   ; `//` per line, so an authored paragraph keeps its shape.
   (cnote n txt)
    (: (one1 s) (? (tally s) ("// " + s) "//")     ; no trailing space on a blank line
       (go acc cur l)
        (? !(two? l) (acc + line n (one1 cur))
           (: c <l (? (c = 10) (go (acc + line n (one1 cur)) "" >l)
                      (go acc (cur + string c) >l))))
       (go "" "" (map txt (jot (tally txt)))))
   ; --- braces, K&R, the way love.c writes them ----------------------------------
   ; copen lays a header line and the body under it: `while (c) {` + the items, or a
   ; bare header + one indented statement when the body is not a block. cclose lays
   ; the closer, and its `tail` is what lets `}` keep company -- ` else` and
   ; ` while (c);` both hang off the same brace. a non-block body has no brace, so
   ; the tail falls to its own line, which is still correct C.
   (blk? s) (two? s && id? <s 'blk)
   (copen n pre hdr s)
    (? (blk? s) (line n (pre + hdr + " {") + join (map (\ x (cstmt x (n + 1))) (el s 1)))
       (line n (pre + hdr) + cstmt s (n + 1)))
   (cclose n s tail)
    (? (blk? s) (line n ("}" + tail))
       (? (tally tail) (line n tail) ""))

   ; a `case`/`dflt` marker sits INLINE in a switch's flat item list (fallthrough is
   ; the list), so it outdents by one against the statements it labels.
   (cstmt s n)
    (? !(two? s) ""
       (: h <s a (el s 1)
          (? (id? h 'blk) (line n "{" + join (map (\ x (cstmt x (n + 1))) a) + line n "}")
             (id? h 'note) (cnote n a)
             (id? h 'decl) (line n (cdecls a + ";"))
             (id? h 'sdecl) (line n ("static " + cdecls a + ";"))
             ; ret's PRE slot is a spelling -- `ai_musttail return ..`, the tail-call
             ; discipline vmret checks. presence is the ARITY, never the value.
             (id? h 'ret) (: pre (? (1 < tally >s) (ctag (el s 2) + " ") "")
                             (line n (pre + (? (two? a) ("return " + ce a 0 + ";") "return;"))))
             (id? h 'expr) (line n (ce a 0 + ";"))
             (id? h 'nop) (line n ";")
             (id? h 'brk) (line n "break;")
             (id? h 'cont) (line n "continue;")
             (id? h 'goto) (line n ("goto " + a + ";"))
             (id? h 'lbl) (line (? (n < 1) 0 (n - 1)) (a + ":"))
             (id? h 'case) (line (? (n < 1) 0 (n - 1)) ("case " + (? (charm? a) (show a) (ce a 0)) + ":"))
             (id? h 'dflt) (line (? (n < 1) 0 (n - 1)) "default:")
             (id? h 'if) (cif s n "")
             (id? h 'while) (: b (el s 2) (copen n "" ("while (" + ce a 0 + ")") b + cclose n b ""))
             (id? h 'do) (copen n "" "do" a + cclose n a (" while (" + ce (el s 2) 0 + ");"))
             ; all three clauses empty is `for (;;)`, C's own spelling of forever --
             ; the general join would lay `for (; ; )`, which reads as three omissions
             (id? h 'for) (: b (el s 4)
                             i (cfor a) c (? (two? (el s 2)) (ce (el s 2) 0) "")
                             p (? (two? (el s 3)) (ce (el s 3) 0) "")
                             hdr (? !(tally (i + c + p)) "for (;;)"
                                    ("for (" + i + "; " + c + "; " + p + ")"))
                             (copen n "" hdr b + cclose n b ""))
             (id? h 'switch) (line n ("switch (" + ce a 0 + ") {")
                              + join (map (\ x (cstmt x (n + 1))) (el s 2)) + line n "}")
             (id? h 'asm) (line n ("__asm__ (" + cstrlit a + casmop (el s 2) + casmop (el s 3)
                                   + (? (two? (el s 4)) (" : " + sepby ", " (map cstrlit (el s 4))) "") + ");"))
             (line n (ce s 0 + ";")))))
   ; if/else, and the else-if CHAIN kept flat: an else whose whole body is another
   ; `if` prints as `} else if (..) {` rather than opening a fresh level, which is
   ; what C source looks like and what the nesting in the tree actually means.
   (cif s n pre)
    (: c (el s 1) th (el s 2) e (el s 3) hdr (pre + "if (" + ce c 0 + ")")
       (? !(two? e) (copen n "" hdr th + cclose n th "")
          (id? <e 'if) (copen n "" hdr th + cif e n (? (blk? th) "} else " "else "))
          (copen n "" hdr th + copen n (? (blk? th) "} " "") "else" e + cclose n e "")))
   ; a for's first clause is a declaration or an expression, and it carries no `;`
   ; ⚠ the init clause arrives WRAPPED -- `(decl ..)` or `(expr ..)`, statement nodes,
   ; not a bare expression -- so both have to be unwrapped by hand. an empty clause
   ; is (), which prints as nothing and reads back as ().
   (cfor a) (? !(two? a) ""
               (id? <a 'decl) (cdecls (el a 1))
               (id? <a 'expr) (ce (el a 1) 0)
               (ce a 0))
   (casmop l) (? !(two? l) " :"
                 (" : " + sepby ", " (map (\ o (cstrlit <o + "(" + ce (el o 1) 0 + ")")) l)))

   ; --- top level ----------------------------------------------------------------
   (cparam p) (cty-dtor (el p 1) <p 0)
   (cparams ps) (: b (map (\ p (cty-base (el p 1) + " " + cparam p)) ps)
                   (? (two? ps) (sepby ", " b) "void"))
   ; a signature: attribute spellings, then the return type wrapped around
   ; `name(params)`. cfn opens a body under it, a proto ends it with `;` -- one
   ; spelling, so a forward declaration cannot drift from the definition it announces.
   (csig nm at r ps)
    (join (map (\ a (ctag a + " ")) at) + cty r (nm + "(" + cparams ps + ")"))
   ; one enum constant: a bare name takes the position C gives it, and `(nom expr)`
   ; names its own value -- which is the only way to say the anonymous enum a
   ; compile-time constant lives in (`enum { nf_maxcap = 64 };`).
   (cconst k) (? (two? k) (string <k + " = " + ce (el k 1) 21) (string k))
   (ctop t)
    (? !(two? t) ""
       (: h <t a (el t 1)
          (? (id? h 'note) (cnote 0 a)
             (id? h 'gdecl) (line 0 (cstor (? (1 < tally >t) (el t 2) ()) + cdecls a + ";"))
             (id? h 'xdecl) (line 0 ("extern " + cdecls a + ";"))
             ; a forward declaration. cparse keeps the NAME and nothing else, so a bare
             ; (proto n) is the same reconstruction a return-type-less fn is; the
             ; appended slots are what a generator says, and what a mutual recursion
             ; between two generated functions has no other way to spell.
             (id? h 'proto)
              (? !(two? >>t) (line 0 ("int " + ctag a + "();"))
                 (line 0 (csig (ctag a) (? (3 < tally >t) (el t 4) ())
                               (? (2 < tally >t) (el t 3) 'int) (el t 2) + ";")))
             (id? h 'tdef) (? (two? >t) (line 0 ("typedef " + cty (el t 2) a + ";"))
                              (gripe "(tdef) carries nothing -- a typedef cparse could not keep"))
             ; an enum definition, one line. a constant takes the position C already
             ; gives it -- a roster that had to say its own indices would not be one --
             ; and `(nom expr)` is for the enum that is not a lattice at all: an
             ; ANONYMOUS one, C's spelling for a lone compile-time constant, where a
             ; position means nothing and the value is the whole point.
             (id? h 'edef)
              (line 0 ("enum " + (? (string? a || name? a) (ctag a + " ") "") + "{ "
                       + sepby ", " (map cconst (el t 2)) + " };"))
             ; a struct or union definition, one member per line. a FIELD is parse's
             ; member pair, so its type is a clay node and brackets its own name the
             ; way every other declaration here does -- `lvm_t *m[KN]` as a member is
             ; the same cty call it is as a global.
             (id? h 'sdef)
              (: kw (? (id? (el t 3) 'union) "union" "struct")
                 (line 0 (kw + " " + ctag a + " {")
                  + join (map (\ f (line 1 (cty (el f 1) <f + ";"))) (el t 2))
                  + line 0 "};"))
             (id? h 'sassert)
              (line 0 ("_Static_assert(" + ce a 21 + ", " + cstrlit (ctag (el t 2)) + ");"))
             ; the preprocessor. a directive owns its line and starts at column 0,
             ; nested or not -- love.h's own style, and the one cpp reads fastest.
             (id? h 'cpp-def)
              (line 0 ("#define " + ctag a
                       + (? (1 < tally >t) (" " + ce (el t 2) 100) "")))
             (id? h 'cpp-undef) (line 0 ("#undef " + ctag a))
             (id? h 'cpp-if) (ccpp a (? (1 < tally >t) (el t 2) ()) (1 < tally >t))
             (gripe ("cannot show top-level " + string h)))))
   ; #if / #elif / #else / #endif over top-level items. the arms carry their own
   ; conditions and the else is a SLOT, so an else out of place has no spelling.
   (ccpp arms els has)
    (: (go l first)
        (? !(two? l) (? has (cseq (line 0 "#else") (citems els)) "")
           (: r <l
              (cseq (cseq (line 0 ((? first "#if " "#elif ") + ce <r 0)) (citems (el r 1)))
                    (go >l 0))))
       (cseq (go arms 1) (line 0 "#endif")))
   ; ⚠ a gripe from a nested item has to come OUT rather than be concatenated into
   ; the text, which is what `+` on a form and a string would quietly do.
   (cseq a b) (? (gripe? a) a (? (gripe? b) b (a + b)))
   (citems l) (: (go m) (? !(two? m) "" (cseq (ctop1 <m) (go >m)))
                 (go l))
   ; the top-level dispatch, shared by clay-show and by a cpp arm's body: a `fn` with
   ; a body goes to cfn, everything else to ctop.
   (ctop1 x) (? (two? x && id? <x 'fn && (2 < tally >x)) (cfn x) (ctop x))

   ; --- what cannot be said ------------------------------------------------------
   ; the forms that carry strictly less than C needs, found in ONE pass up front so
   ; that every renderer below can just return text. the first two are cparse's
   ; degenerate shapes, and both have a faithful clay counterpart waiting for the day
   ; parse.l fills them:
   ;   (tdef)          a typedef / struct definition / _Static_assert, name and shape gone
   ;   (decl ())       a block-scope declaration that declared no OBJECT -- a bare
   ;                   `struct S {..};` or a function declaration, whose whole content
   ;                   parse kept elsewhere (the tag table, the sig table). `int ;` is
   ;                   what printing it anyway looks like, and it is not C.
   ;   (vastart ap)    va_start's last named parameter, dropped at parse (:733)
   ;   (struct .anon0) an ANONYMOUS tag, which is not a C identifier and so cannot be
   ;                   written down at all. a NAMED tag is sayable: `sdef` defines the
   ;                   shape, and a reference standing without one is either
   ;                   incomplete BY DESIGN (`struct S *p` needs no definition) or
   ;                   resolved by the header the generated region sits inside, which
   ;                   is how mx.h rides love.c.
   ; the alternative -- printing `;` for the first and a made-up second argument for
   ; the second -- would round-trip green and emit C that does not mean the program.
   ; a gate that can be passed by pretending is worth nothing.
   ; ⚠ and that is exactly why the named tag can be let through without weakening G1:
   ; a file that DEFINES the struct it names still carries the (tdef) that refuses, so
   ; a tag clay prints and does not define is one the source never defined either.
   (clay-unsaid c)
    (: (go x)
        (? !(two? x) ()
           (id? <x 'tdef && !(two? >x))
            (gripe "(tdef) carries nothing -- a typedef, struct or _Static_assert cparse could not keep")
           (member? <x '(decl sdecl) && !(two? <>x))
            (gripe ("(" + string <x + " ()) declared no object -- the tag or the function name is not in the node"))
           (id? <x 'vastart && !(two? >>x))
            (gripe "(vastart ap) lost va_start's last named parameter at parse")
           (id? <x 'struct && !(cident? <>x))
            (gripe ("(struct " + show <>x + ") -- an anonymous tag is not a C identifier"))
           (id? <x 'flo && !(cfinite? <>x))
            (gripe ("(flo " + show <>x + ") -- C has no literal for an infinity or a nan"))
           ; C has no empty enum, so `enum q { };` is not a thin way to say a lattice
           ; with no kinds -- it is a syntax error.
           (id? <x 'edef && !(two? (el x 2)))
            (gripe ("(edef " + show <>x + " ()) has no constants -- C has no empty enum"))
           ; ...and no empty struct either, for the same reason: a shape with no
           ; members is a syntax error, not a thin way to say an empty one.
           (id? <x 'sdef && !(two? (el x 2)))
            (gripe ("(sdef " + show <>x + " ()) has no members -- C has no empty struct"))
           (id? <x 'sdef && !(cident? <>x))
            (gripe ("(sdef " + show <>x + ") -- an anonymous tag is not a C identifier"))
           ; a macro is NAMED by an identifier, and `#if` with no arm is not a thin
           ; way to say a group that is always off -- both are syntax errors.
           (member? <x '(cpp-def cpp-undef) && !(cident? <>x))
            (gripe ("(" + string <x + " " + show <>x + ") -- a macro name is an identifier"))
           (id? <x 'cpp-if && !(two? <>x))
            (gripe "(cpp-if ()) has no arms -- C has no #if without a condition")
           ; restrict qualifies a pointer or nothing; anywhere else the shower would
           ; have to drop it in silence, which is pretending.
           (id? <x 'restrict && !(cptr? <>x))
            (gripe "(restrict t) -- C restricts only a pointer")
           (: (kids l) (? !(two? l) ()
                          (: e <l v (? (two? e) (? (name? <e) (go e) (kids e)) ())
                             (? v v (kids >l))))
              (kids >x)))
       (go c))

   ; --- the door -----------------------------------------------------------------
   ; (clay-show c) -> C text, or a gripe. `prog` is the whole translation unit;
   ; anything else shows on its own, which is what a generator laying one table
   ; wants.
   (clay-show c)
    (: u (clay-unsaid c)
       (? u u
          !(two? c) (gripe "clay-show wants a form")
          (id? <c 'prog) (citems (el c 1))
          (ctop1 c)))
   ; a function definition: signature line, then the body block at column 0. the
   ; RETURN TYPE is clay's 4th slot; absent -- which is every fn cparse ever built --
   ; `int` is the best the tree can justify. the 5th is attribute SPELLINGS laid
   ; before it (`ai_noinline ai_noicf struct ai *lvm_add(..)`). see the header.
   (cfn t)
    (: nm (ctag (el t 1))
       r (? (3 < tally >t) (el t 4) 'int)
       at (? (4 < tally >t) (el t 5) ())
       b (el t 3)
       (copen 0 "" (csig nm at r (el t 2)) b + cclose 0 b "")))
