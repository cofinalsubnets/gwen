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
    (gripe? (one '(gdecl (("v" (struct "S") ())))))
    ; C has no literal for an infinity, so it is not sayable
    (gripe? (one cl-inf))))

; --- clay-ok? is a SHAPE check, because gen.l trusts its input ------------------
(assert
 (1 = (cok '(bin + (num 1) (num 2))))
 (gripe? (cok '(bin + (num 1))))                  ; bin takes three
 (gripe? (cok '(nosuchtag 1)))
 (gripe? (cok '(blk ((ret (num 1) (num 2))))))    ; ret takes one
 ; the check reaches INSIDE a declarator list, which is where a generator's
 ; mistakes actually land
 (gripe? (cok '(gdecl (("x" int (bin + (num 1))))))))

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
