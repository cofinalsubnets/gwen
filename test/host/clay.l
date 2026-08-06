; --- test/host/clay.l: CLAY, the C shower (crew/moon/clay.l) ----------------
; the AUTHORING direction, which is the half G1 structurally cannot reach.
; test/clay.l round-trips clay that came OUT of cparse, so it exercises exactly
; the subset cparse can emit -- and every form clay adds on top (a return type, a
; storage class, `const`, a typedef'd name, a symbolic array bound, a designated
; index that is not a number, a comment) is by definition absent from that subset.
; those forms are what a GENERATOR writes, so they are checked here, against the
; text they must produce.
;
; comparing generated C to expected C text is right here and wrong in test/clay.l,
; and the difference is what is being proven: there, that the shower and the parser
; agree about a TREE; here, that the shower emits the C a human meant to write.
;
; run standalone from the repo root:
;   cat test/00-init.l test/host/clay.l | out/host/love

(use 'clay)

(: cs (from 'clay 'clay-show)
   cok (from 'clay 'clay-ok?)
   nl (string 10)
   (one c) (cs c)                      ; a single top-level item, no (prog ..) wrapper
   (gripe? x) (&& (two? x) (id? <x 'gripe)))

; --- types: the base and the declarator are built separately, because a C
; declaration brackets its own name. these are the shapes love.c actually writes.
(assert
 ((one '(gdecl (("x" int (num 5))))) = ("int x = 5;" + nl))
 ((one '(gdecl (("p" (ptr char) ())))) = ("char *p;" + nl))
 ((one '(gdecl (("t" (arr (ptr char) 2) ())))) = ("char *t[2];" + nl))
 ((one '(gdecl (("m" (arr (arr int 2) 3) ())))) = ("int m[3][2];" + nl))
 ; a pointer to an ARRAY and an array of POINTERS differ by exactly the parens
 ((one '(gdecl (("q" (ptr (arr int 3)) ())))) = ("int (*q)[3];" + nl))
 ((one '(gdecl (("f" (ptr (fn int)) ())))) = ("int (*f)();" + nl))
 ; the unsigned scalars canonicalize to one symbol at parse; the spelling comes back
 ((one '(gdecl (("u" ulong ())))) = ("unsigned long u;" + nl))
 ; an unsized bound is the flexible member, and prints as the empty bracket
 ((one '(gdecl (("v" (arr char 0) ())))) = ("char v[];" + nl)))

; --- const binds to the DECLARATOR over a pointer and to the BASE otherwise.
; `lvm_t *const m[16][16]` is the shape the dispatch matrices need, and getting it
; wrong is the difference between a const table and a table of consts.
(assert
 ((one '(gdecl (("a" (const int) (num 1))))) = ("int const a = 1;" + nl))
 ((one '(gdecl (("b" (const (ptr char)) ())))) = ("char *const b;" + nl))
 ((one '(gdecl (("t" (const (named "size_t")) ())))) = ("size_t const t;" + nl))
 ((one '(gdecl (("mx" (arr (arr (const (ptr (named "lvm_t"))) KN) KN) ())) (static)))
  = ("static lvm_t *const mx[KN][KN];" + nl)))

; --- a SYMBOLIC bound and a symbolic designated index. cparse folds both to
; numbers -- it has to, it is compiling -- and a generator wants neither folded:
; the whole point of laying a table from love is that the C says [KCharm], the name
; the enum gave it, and stays readable.
(: cl-di '(gdecl (("v" (arr int 2) (init ((didx KMint (var "lvm_0")) (didx KNom (var "lvm_1")))))))
   cl-df '(gdecl (("s" (named "S") (init ((dfield "x" (num 1)) (num 2))))))
   (assert
    ((one '(gdecl (("v" (arr int KN) ())))) = ("int v[KN];" + nl))
    ((one cl-di) = ("int v[2] = { [KMint] = lvm_0, [KNom] = lvm_1 };" + nl))
    ((one cl-df) = ("S s = { .x = 1, 2 };" + nl))))

; --- the return type is clay's 4th slot on `fn`. cparse never fills it, so every
; function it hands back prints `int`; an AUTHORED one says what it means.
(assert
 ((one '(fn "f" (("a" int)) (blk ((ret (var "a")))))) = ("int f(int a) {" + nl + " return a;" + nl + "}" + nl))
 ((one '(fn "g" () (blk ((ret ()))) void)) = ("void g(void) {" + nl + " return;" + nl + "}" + nl)))

; --- a NOTE is emit-only: authored in the generator, never captured from C. one
; `//` per line, so a paragraph keeps its shape and generated love.c arrives with
; the prose the tree runs on rather than stripped of it.
(assert
 ((one '(note "the lane table")) = ("// the lane table" + nl))
 ((one '(note "two lines
here")) = ("// two lines" + nl + "// here" + nl)))

; --- a real typedef, the form cparse cannot emit --------------------------------
(: cl-vas '(prog ((fn "f" () (blk ((expr (vastart (var "ap"))))))))
   cl-inf `('gdecl `(`("d" 'double `('flo (1.0 / 0.0)))))
   (assert
    ((one '(tdef "lvm_t" (fn void))) = ("typedef void lvm_t();" + nl))
    ; ...and the empty marker it emits INSTEAD refuses, rather than printing a `;`
    ; that would round-trip green while saying nothing. the same for va_start's lost
    ; parameter and for a struct shape the tree does not carry.
    (gripe? (one '(tdef)))
    (gripe? (one cl-vas))
    ; C has no literal for an infinity, so it is not sayable
    (gripe? (one cl-inf))))

; --- a struct or union DEFINITION, the other form cparse cannot emit ------------
; a FIELD is parse's own member pair ((name ty)..), so a member type is a clay node
; and brackets its own name exactly as a global does. the 3rd slot is 'union, which
; changes the DEFINING keyword only -- moon's tag namespace holds both, so a union
; is still REFERRED to as ('struct tag).
(: cl-sd '(sdef "P" (("x" int) ("next" (ptr (struct "P"))) ("row" (arr (named "lvm_t") KN))))
   cl-un '(sdef "pun" (("d" double) ("u" ullong)) union)
   (assert
    ((one cl-sd) = ("struct P {" + nl + " int x;" + nl + " struct P *next;" + nl
                    + " lvm_t row[KN];" + nl + "};" + nl))
    ((one cl-un) = ("union pun {" + nl + " double d;" + nl + " unsigned long long u;" + nl + "};" + nl))))

; --- and the composition that was unreachable before it: define the shape, name
; it, then USE it. a named tag is sayable on its own, because a reference standing
; without a definition is either incomplete BY DESIGN (`struct S *p`) or resolved by
; the header the generated region sits inside -- which is how mx.h rides love.c.
(: cl-tds '(prog ((sdef "P" (("x" int))) (tdef "P" (struct "P")) (gdecl (("v" (struct "P") ())))))
   (assert
    ((one cl-tds) = ("struct P {" + nl + " int x;" + nl + "};" + nl
                     + "typedef struct P P;" + nl + "struct P v;" + nl))
    ((one '(gdecl (("p" (ptr (struct "S")) ())))) = ("struct S *p;" + nl))
    ; the ANONYMOUS tag is the one that stays unsayable, and it is not a judgement
    ; call: parse mints it as ".anon0", which no C compiler will read back.
    (gripe? (one '(sdef ".anon0" (("x" int)))))
    (gripe? (one '(gdecl (("v" (ptr (struct ".anon0")) ())))))
    ; C has no empty struct, the same way it has no empty enum
    (gripe? (one '(sdef "E" ())))))

; --- the five shapes of doc/clay.md's rung 1, ~750 sites in love.c --------------
; attributes ride fn's 5th slot as SPELLINGS -- love.h's macros used call-shaped,
; so clay says "ai_noinline" and never what it expands to. emit-only by necessity:
; parse.l balance-skips a trailing attribute run, so none can come back through G1.
(assert
 ((one '(fn "ai_sleep" (("ticks" (named "uintptr_t"))) (blk ((ret ()))) void
            ("__attribute__((weak))" "ai_noinline")))
  = ("__attribute__((weak)) ai_noinline void ai_sleep(uintptr_t ticks) {" + nl
     + " return;" + nl + "}" + nl))
 ; restrict binds to the declarator, the shape every lvm_ parameter wears
 ((one '(gdecl (("g" (restrict (ptr (struct "ai"))) ())))) = ("struct ai *restrict g;" + nl))
 ; ...and qualifies a pointer or nothing -- dropping it in silence would pretend
 (gripe? (one '(gdecl (("x" (restrict int) ())))))
 ; the ret prefix is love.h's tail-call discipline, the one vmret disassembles for
 ((one '(fn "f" () (blk ((ret (call (var "Continue") ()) ai_musttail)))))
  = ("int f(void) {" + nl + " ai_musttail return Continue();" + nl + "}" + nl))
 ; _Static_assert, which cparse reduces to the bare (tdef) marker
 ((one '(sassert (bin eq (bin shr (un neg (num 1)) (num 1)) (un neg (num 1))) "sign extended shift"))
  = ("_Static_assert(-1 >> 1 == -1, \"sign extended shift\");" + nl))
 ; the flexible member composes: (arr t 0) is the unsized bound inside an sdef too
 ((one '(sdef "blob" (("n" (named "uintptr_t")) ("bytes" (arr char 0)))))
  = ("struct blob {" + nl + " uintptr_t n;" + nl + " char bytes[];" + nl + "};" + nl))
 ; the wide scalar needs no node at all -- (named ..) already says it
 ((one '(gdecl (("w" (named "unsigned __int128") ())))) = ("unsigned __int128 w;" + nl)))

; --- and the composition the five buy: an lvm_ definition, whole ----------------
; `lvm(lvm_add)`'s expansion at love.c's 184 definition sites -- attributes, the
; restrict-qualified params, the return type, the musttail return.
(: cl-lvm '(fn "lvm_add"
            (("g" (restrict (ptr (struct "ai")))) ("Ip" (ptr (named "union u")))
             ("Hp" (ptr (named "ai_word"))) ("Sp" (restrict (ptr (named "ai_word")))))
            (blk ((ret (call (var "Continue") ()) "ai_musttail")))
            (ptr (struct "ai"))
            ("ai_noinline" "ai_noicf"))
   (assert
    ((one cl-lvm)
     = ("ai_noinline ai_noicf struct ai *lvm_add(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp) {"
        + nl + " ai_musttail return Continue();" + nl + "}" + nl))))

; --- the preprocessor, doc/clay.md's rung 2 ------------------------------------
; emit-only more firmly than a note is: cpp runs before the lexer, so no directive
; survives to be parsed back. a BODY that is a clay expression renders at the
; tightest context, which is macro hygiene falling out of the precedence table --
; the body cannot see where it is substituted, so it parenthesizes itself.
(assert
 ((one '(cpp-def "ai_data_n" 9)) = ("#define ai_data_n 9" + nl))
 ((one '(cpp-def "countof(_)" (bin / (szof (var "_")) (szof (deref (var "_"))))))
  = ("#define countof(_) (sizeof(_) / sizeof(*_))" + nl))
 ; ...and a body that is TOKENS rather than an expression rides through as a
 ; spelling, the standing that `fn`'s attributes and `ret`'s prefix already have
 ((one '(cpp-def "ai_noicf" "__attribute__((noipa))")) = ("#define ai_noicf __attribute__((noipa))" + nl))
 ; presence is the ARITY: no body slot is a bare #define, the feature-test flag
 ((one '(cpp-def "AI_FLAG")) = ("#define AI_FLAG" + nl))
 ((one '(cpp-undef "AI_FLAG")) = ("#undef AI_FLAG" + nl))
 ; a macro is NAMED by an identifier -- a spelling that is not one would emit C
 ; that no compiler reads
 (gripe? (one '(cpp-def "9bad" 1)))
 (gripe? (one '(cpp-undef "(x)"))))

; --- #if / #elif / #else, over TOP-LEVEL items ----------------------------------
; the arms are ((cond items)..) and the else is its own slot, so an else out of
; place has no spelling at all. `#ifdef X` needs no node: `defined` wears call
; syntax in C's own grammar, so it is a plain (call (var "defined") ..).
(: cl-def (\ n `('call `('var "defined") `(`('var n))))
   cl-if `('cpp-if `(`(`('lor (cl-def "__wasm__") (cl-def "__APPLE__"))
                        `(`('cpp-def "ai_data_section" 0))))
                   `(`('cpp-def "ai_data_section" 1)))
   ; the shape love.h actually wants: a guard with a conditional nested inside it,
   ; every directive at column 0 whatever the depth -- love.h's own style
   cl-nest `('cpp-if `(`(`('un 'not (cl-def "ai_data_stride"))
                          `(cl-if))))
   (assert
    ((one cl-if) = ("#if defined(__wasm__) || defined(__APPLE__)" + nl
                    + "#define ai_data_section 0" + nl + "#else" + nl
                    + "#define ai_data_section 1" + nl + "#endif" + nl))
    ((one cl-nest) = ("#if !defined(ai_data_stride)" + nl
                      + "#if defined(__wasm__) || defined(__APPLE__)" + nl
                      + "#define ai_data_section 0" + nl + "#else" + nl
                      + "#define ai_data_section 1" + nl + "#endif" + nl + "#endif" + nl))
    ; an #elif arm, and an arm's body carrying more than a directive
    ((one '(cpp-if (((var "A") ((note "a"))) ((var "B") ((note "b")))) ((note "c"))))
     = ("#if A" + nl + "// a" + nl + "#elif B" + nl + "// b" + nl
        + "#else" + nl + "// c" + nl + "#endif" + nl))
    ; no arms is a syntax error, not a thin way to say a group that is always off
    (gripe? (one '(cpp-if ())))
    ; ⚠ a refusal from INSIDE an arm has to come out as a gripe, never be
    ; concatenated into the text -- which is what `+` on a form would quietly do
    (gripe? (one '(cpp-if ((1 ((tdef)))))))
    (gripe? (one '(cpp-if ((1 ((blk ())))))))))

; --- clay-ok? is a SHAPE check, because gen.l trusts its input ------------------
(assert
 (1 = (cok '(bin + (num 1) (num 2))))
 (gripe? (cok '(bin + (num 1))))                  ; bin takes three
 (gripe? (cok '(nosuchtag 1)))
 (gripe? (cok '(blk ((ret (num 1) x y)))))        ; ret takes at most two: value + prefix
 ; the check reaches INSIDE a declarator list, which is where a generator's
 ; mistakes actually land
 (gripe? (cok '(gdecl (("x" int (bin + (num 1)))))))
 ; the ATTRS slot holds spellings, not nodes, so the walk must not read them as tags
 (1 = (cok '(fn "f" () (blk ((ret (num 1) ai_musttail))) void (ai_noinline ai_noicf))))
 (1 = (cok '(sassert (bin eq (num 1) (num 1)) "m")))
 (gripe? (cok '(sassert (num 1))))                 ; wants the message
 ; a cpp arm's CONDITION and its body are both walked, three list levels down
 (1 = (cok '(cpp-if (((var "A") ((cpp-def "X" 1)))))))
 (gripe? (cok '(cpp-if (((var "A") ((bin + (num 1))))))))
 (gripe? (cok '(cpp-def))))                        ; wants a name

; --- precedence: the shower is not fully parenthesized, so the table has to be
; right. these are the pairs that go wrong when it is not.
(: (e x) (cs `('gdecl `(`("r" 'int x))))
   (assert
    ((e '(bin * (bin + (var "a") (var "b")) (var "c"))) = ("int r = (a + b) * c;" + nl))
    ((e '(bin + (bin * (var "a") (var "b")) (var "c"))) = ("int r = a * b + c;" + nl))
    ; - is LEFT associative: a - (b - c) needs the parens back
    ((e '(bin - (var "a") (bin - (var "b") (var "c")))) = ("int r = a - (b - c);" + nl))
    ((e '(bin - (bin - (var "a") (var "b")) (var "c"))) = ("int r = a - b - c;" + nl))
    ; shifts bind TIGHTER than the comparisons, looser than the arithmetic
    ((e '(bin shl (bin + (var "a") (var "b")) (num 1))) = ("int r = a + b << 1;" + nl))
    ((e '(bin eq (bin shr (var "a") (num 3)) (num 0))) = ("int r = a >> 3 == 0;" + nl))
    ; ?: is right-associative and binds looser than ||
    ((e '(cond (lor (var "a") (var "b")) (num 1) (cond (var "c") (num 2) (num 3))))
     = ("int r = a || b ? 1 : c ? 2 : 3;" + nl))
    ; ⚠ - applied to a negation needs a SPACE, or the lexer reads a predecrement
    ((e '(un neg (un neg (var "a")))) = ("int r = - -a;" + nl))
    ; a[i] is (deref (bin + a i)) -- cparse desugars indexing, and the sugar comes back
    ((e '(deref (bin + (var "a") (num 2)))) = ("int r = a[2];" + nl))
    ; x->f is (dot (deref x) f), and prints as the arrow it came from
    ((e '(dot (deref (var "p")) "f")) = ("int r = p->f;" + nl))))

; --- strings: the escapes C needs, and an octal fallback for the rest -----------
(: cl-str `('gdecl `(`("s" '(ptr char) `('str ("a" + (string 10) + "\"b\\" + (string 1))))))
   (assert
    ((one cl-str) = ("char *s = \"a\\n\\\"b\\\\\\001\";" + nl))))

(puts "clay: ok")
(putc 10)
