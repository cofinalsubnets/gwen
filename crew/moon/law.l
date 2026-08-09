; crew/moon/law.l -- the compiler's laws (stage 0), held by assertion over the
; pure pipeline: lexer goldens (kinds, values, lines, maximal munch), parser
; AST goldens, and the WHOLE IR for return-42 compared form by form. the cat
; (`make test_moon`):
;
;   cat test/00-init.l crew/moon/lex.l crew/moon/parse.l crew/moon/gen.l crew/moon/law.l | out/host/love
;
; the end-to-end (elf64, the run, the gcc differential) lives in the Makefile
; smoke through the real `mooncc`.
;
; ⚠ a MID-LETREC assert must bind to `_` -- a bare one sits in binding position,
; becomes define-sugar, and NEVER RUNS (it shadows a name and the pairing shifts).
; when in doubt, falsify one assert and watch the gate red.

; A REFUSAL, however it says so. A stage answers a bare () when it refused
; without a cause it could name, and a gripe when it could -- and since rung 4
; (doc/moon-diag.md) every codegen refusal names SOMETHING, if only the function
; it gave up in. So a law that means "this source is refused" says `refused?`;
; the laws that mean "refused, and here is exactly what it said" pin `gripe?`
; and the message, in the diagnostics sections below.
(: (refused? x) (!x || gripe? x))

; --- the lexer: kinds, values, lines, comments, maximal munch ---
(: (tn ts k) (? (< k 1) <ts (tn >ts (- k 1)))
(tn3 ts k) (: t (tn ts k) `(<t <>t <>>t))   ; kind/value/line: not the slot-4 spelling
   ; kind/value/line alone: a numeric or string token also carries its SOURCE
   ; SPELLING (cpp's sof, what # owes), which these laws are not about
   (tn3 ts k) (: t (tn ts k) `(<t <>t <>>t))   ; lists don't index by application
   r (clex "int main() { return 42; }")
   ts <>r
   (assert
      (two? r) (= (tally ts) 9)
      (= (tn ts 0) (L 'kw 'int 1))
      (= (tn ts 1) (L 'id "main" 1))
      (= (tn ts 2) (L 'p "(" 1 1))              ; a '(' GLUED to `main` carries the 4th field (function-macro paren marker)
      (= (tn ts 3) (L 'p ")" 1))
      (= (tn ts 4) (L 'p "{" 1))
      (= (tn ts 5) (L 'kw 'return 1))
      (= (tn3 ts 6) (L 'num 42 1))
      (= (tn ts 7) (L 'p ";" 1))
      (= (tn ts 8) (L 'p "}" 1))))
(: (tn ts k) (? (< k 1) <ts (tn >ts (- k 1)))
(tn3 ts k) (: t (tn ts k) `(<t <>t <>>t))   ; kind/value/line: not the slot-4 spelling
   ; kind/value/line alone: a numeric or string token also carries its SOURCE
   ; SPELLING (cpp's sof, what # owes), which these laws are not about
   (tn3 ts k) (: t (tn ts k) `(<t <>t <>>t))
   (spl r k) (: t (tn <>r k) (? (4 < tally t) <>>>>t ()))          ; the token's slot-4 spelling
   c1 <>(clex "a // x\nb")
   c2 <>(clex "a /* x\ny */ b")
   c3 <>(clex "a+++b")
   c4 <>(clex "x <<= 1; y >>= 2;")
   (assert
      (= (tally c1) 2) (= (tn c1 1) (L 'id "b" 2))          ; // eats the line, lines count
      (= (tally c2) 2) (= (tn c2 1) (L 'id "b" 2))          ; /* */ spans, lines count
      (= (tally c3) 4)                                       ; maximal munch: a ++ + b
      (= (tn c3 1) (L 'p "++" 1)) (= (tn c3 2) (L 'p "+" 1))
      (= (tn c4 1) (L 'p "<<=" 1)) (= (tn c4 5) (L 'p ">>=" 1))
      (= (tn3 <>(clex "'a' '\\n' '\\''") 0) (L 'num 97 1))        ; char literals are numbers
      (= (tn3 <>(clex "'a' '\\n' '\\''") 1) (L 'num 10 1))
      (= (tn3 <>(clex "'a' '\\n' '\\''") 2) (L 'num 39 1))
      ; a multi-char constant packs left to right into an int, gcc's rule (0x6162);
      ; an UNTERMINATED one still refuses.
      (= (tn3 <>(clex "'ab'") 0) (L 'num 24930 1))
      !(clex "'")                                            ; malformed literals refuse
      (= (tn3 <>(clex "\"hi\"") 0) (L 'str "hi" 1))
      (= (tn3 <>(clex "\"a\\nb\"") 0) (L 'str "a\nb" 1))
      (= (tn3 <>(clex "\"ab\\\ncd\"") 0) (L 'str "abcd" 1))        ; \<newline> inside a string SPLICES (the pair vanishes) -- tar's --help strings
      !(clex "\"open")                                       ; unterminated string refuses
      ; the SOURCE SPELLING beside the value, slot 4: what a value cannot answer
      ; back (0777 and 511 are one value) and what # owes. cpp's sof reads it.
      (= (spl (clex "0777") 0) "0777") (= (spl (clex "0777") 0) "0777")
      (= (spl (clex "1u") 0) "1u") (= (spl (clex "1.20") 0) "1.20")
      (= (spl (clex "0x1F") 0) "0x1F") (= (spl (clex "'a'") 0) "'a'")
      (= (spl (clex "\"a\\nb\"") 0) "\"a\\nb\"")               ; the escape, not the byte
      ; wide/prefixed literals: L (wchar_t) / u / U / u8 -- we carry no distinct wide type,
      ; so the prefix DROPS. a char value is identical; a wide string reads narrow.
      (= (tn3 <>(clex "L'a'") 0) (L 'num 97 1))               ; L'a' == 'a'
      (= (tn3 <>(clex "u'B'") 0) (L 'num 66 1))               ; u'B'
      (= (tn3 <>(clex "U'C'") 0) (L 'num 67 1))               ; U'C'
      (= (tn3 <>(clex "L'\\n'") 0) (L 'num 10 1))             ; an escape rides through the prefix
      (= (tn3 <>(clex "L\"hi\"") 0) (L 'str "hi" 1))          ; a wide string reads narrow
      (= (tn3 <>(clex "u8\"hi\"") 0) (L 'str "hi" 1))         ; u8 (two-char prefix)
      (= (tn <>(clex "Long") 0) (L 'id "Long" 1))            ; an id STARTING with L/u/U is NOT a prefix (no quote follows)
      (= (tally <>(clex "L L'a'")) 2)                        ; bare L stays an id; L'a' a number
      !(clex "int @")                                        ; not C: the lex error rides up
      (= (tally <>(clex "abc")) 3)                        ; form-feed + vertical-tab are WHITESPACE (GNU page breaks); real FF/VT bytes here
      !(clex "a /* open")                                    ; unterminated comment refused
      (= (tn3 <>(clex "0644") 0) (L 'num 420 1))            ; octal integer literal (0644 = 420); gzip's 0777/0666 modes
      (= (tn3 <>(clex "0777") 0) (L 'num 511 1))
      (= (tn3 <>(clex "0") 0) (L 'num 0 1))                 ; a plain 0 stays 0
      (= (tn3 <>(clex "08") 0) (L 'num 8 1))                ; 8/9 aren't octal digits -> falls to decimal
      (= (tn3 <>(clex "0x1F") 0) (L 'num 31 1))              ; 7c-ii: hex literals
      (= (tn3 <>(clex "0xFF & 1") 0) (L 'num 255 1))
      (= (tn3 <>(clex "255L") 0) (L 'num 255 1))             ; an l/L suffix is consumed outright
      (= (tn3 <>(clex "100U") 0) (L 'unum 100 1))            ; a u/U marks the token UNSIGNED (neg/div/shift go unsigned):
      (= (tn3 <>(clex "100UL") 0) (L 'ulnum 100 1))          ; bare u an unsigned INT, u+l/L (or a >32-bit value) an unsigned LONG
      (= (tn3 <>(clex "0x8ULL") 0) (L 'ullnum 8 1))          ; the LL pair is its own kind (t32 types it llong)
      (= (tn3 <>(clex "'\\f'") 0) (L 'num 12 1))             ; \f form feed, \a bell, \v vtab
      (= (tn3 <>(clex "'\\a'") 0) (L 'num 7 1))
      (= (tn3 <>(clex "'\\v'") 0) (L 'num 11 1))
      (= <>(clex "") ())))                                   ; empty text, empty tokens

; --- the parser: precedence, associativity, desugarings, statement shapes ---
(: (cast src) <>(cparse <>(clex src))
   (cexp src) (: ast (cast (+ "int main() { return " (+ src "; }")))
                 fn <<>ast
                 items <><>>>fn
                 <><items)
   (assert
      (= (cast "int main() { return 42; }")
         '(prog ((fn "main" () (blk ((ret (num 42))))))))
      (= (cast "int add(int a, int b) { return a+b; }")
         '(prog ((fn "add" (("a" int) ("b" int)) (blk ((ret (bin + (var "a") (var "b")))))))))
      (= (cast "long f(char c, short s) { return c; }")
         '(prog ((fn "f" (("c" char) ("s" short)) (blk ((ret (var "c"))))))))
      (= (cast "int f(void); int main() { return 0; }")
         '(prog ((proto "f") (fn "main" () (blk ((ret (num 0))))))))
      ; a call's HEAD is an expression now (function pointers) -- (call headexpr args)
      (= (cexp "f(1, 2+3)") '(call (var "f") ((num 1) (bin + (num 2) (num 3)))))
      (= (cexp "f()") '(call (var "f") ()))
      (= (cexp "f(g(1))") '(call (var "f") ((call (var "g") ((num 1))))))
      ; a call through a dereferenced function pointer
      (= (cexp "(*fp)(7)") '(call (deref (var "fp")) ((num 7))))
      (= (cexp "2+3*4") '(bin + (num 2) (bin * (num 3) (num 4))))
      (= (cexp "10-3-2") '(bin - (bin - (num 10) (num 3)) (num 2)))
      (= (cexp "(2+3)*4") '(bin * (bin + (num 2) (num 3)) (num 4)))
      (= (cexp "1<<3") '(bin shl (num 1) (num 3)))
      (= (cexp "a<b==c") '(bin eq (bin lt (var "a") (var "b")) (var "c")))
      (= (cexp "-x") '(un neg (var "x")))
      (= (cexp "!~x") '(un not (un bnot (var "x"))))
      (= (cexp "x=y=1") '(asn (var "x") (asn (var "y") (num 1))))    ; right assoc
      (= (cexp "x+=2") '(asn (var "x") (bin + (var "x") (num 2))))   ; the desugar
      (= (cexp "++x") '(asn (var "x") (bin + (var "x") (num 1))))
      ; a CALM lvalue duplicates; a stepping one is kept whole (rmw) or taken once (post)
      (= (cexp "a[i]+=2") '(asn (deref (bin + (var "a") (var "i")))
                                (bin + (deref (bin + (var "a") (var "i"))) (num 2))))
      (= (cexp "*p++|=2") '(rmw | (deref (post (var "p") 1)) (num 2)))
      (= (cexp "a[i++]-=2") '(rmw - (deref (bin + (var "a") (post (var "i") 1))) (num 2)))
      (= (cexp "*f()=2") '(asn (deref (call (var "f") ())) (num 2)))   ; plain = never duplicates
      (= (cexp "++*p++") '(bin + (post (deref (post (var "p") 1)) 1) (num 1)))
      (= (cexp "x--") '(post (var "x") -1))
      (= (cexp "a?1:2") '(cond (var "a") (num 1) (num 2)))
      (= (cexp "1,2") '(comma (num 1) (num 2)))
      (= (cexp "a&&b||c") '(lor (land (var "a") (var "b")) (var "c")))
      (= (cexp "*p") '(deref (var "p")))
      (= (cexp "&x") '(addr (var "x")))
      (= (cexp "a[2]") '(deref (bin + (var "a") (num 2))))
      (= (cexp "*p++") '(deref (post (var "p") 1)))
      (= (cexp "sizeof(int*)") '(num 8))
      (= (cexp "(char*)x") '(cast (ptr char) (var "x")))
      (= (cexp "(const char*)x") '(cast (ptr char) (var "x")))    ; a qualifier opens the cast type too -- quals drop, like the decl sites
      (= (cexp "sizeof(const int)") '(num 4))
      (= (cast "int main() { int *p; int a[4]; return 0; }")
         '(prog ((fn "main" () (blk ((decl (("p" (ptr int) ())))
                                     (decl (("a" (arr int 4) ())))
                                     (ret (num 0))))))))
      (= (cast "long g = 5; int main() { return 0; }")
         '(prog ((gdecl (("g" long (num 5))))
                 (fn "main" () (blk ((ret (num 0))))))))
      (okv? (cparse <>(clex "int main() { int *a[3]; return 0; }")))        ; an array of pointers parses (4d)
      (= (cast "int main() { int x=1; if (x) return 2; return 3; }")
         '(prog ((fn "main" () (blk ((decl (("x" int (num 1))))
                                     (if (var "x") (ret (num 2)) ())
                                     (ret (num 3))))))))
      ; a label is a flat MARKER in a statement list, but a labeled statement is ONE statement:
      ; as an unbraced ARM it takes what it labels WITH it, into a block. left flat, the arm
      ; would be the marker alone and the statement would land past the arm's join -- the then
      ; arm falling into the else (`else pass: switch` in love.c's gvzprintf printed "\%z").
      (= (cast "int main() { lab: return 2; }")
         '(prog ((fn "main" () (blk ((lbl "lab") (ret (num 2))))))))
      (= (cast "int main() { if (1) return 1; else e0: return 2; }")
         '(prog ((fn "main" () (blk ((if (num 1) (ret (num 1))
                                         (blk ((lbl "e0") (ret (num 2)))))))))))
      (= (cast "int main() { if (0) x0: return 1; return 3; }")
         '(prog ((fn "main" () (blk ((if (num 0) (blk ((lbl "x0") (ret (num 1)))) ())
                                     (ret (num 3))))))))
      (= (cast "int main() { char c; long a=1, b; return 0; }")
         '(prog ((fn "main" () (blk ((decl (("c" char ())))
                                     (decl (("a" long (num 1)) ("b" long ())))
                                     (ret (num 0))))))))
      (= (cexp "(char)300") '(cast char (num 300)))
      (= (cexp "(long)(int)x") '(cast long (cast int (var "x"))))
      (= (cexp "sizeof(int)") '(num 4))
      (= (cexp "sizeof(char)+sizeof(short)+sizeof(long)") '(bin + (bin + (num 1) (num 2)) (num 8)))
      (= (cast "int main() { while (1) break; return 0; }")
         '(prog ((fn "main" () (blk ((while (num 1) (brk)) (ret (num 0))))))))
      ; musttail comes back through a parse now -- the ret's PRE slot (clay's (ret e PRE));
      ; every other statement attribute still skips
      (= (cast "int main() { __attribute__((musttail)) return main(); }")
         '(prog ((fn "main" () (blk ((ret (call (var "main") ()) "__attribute__((musttail))")))))))
      (gripe? (cparse <>(clex "int main() { return (1+2; }")))            ; the lost )
      (gripe? (cparse <>(clex "int main() { if 1 return 2; }")))          ; if wants parens
      (gripe? (cparse <>(clex "int main() { return 42 }")))
      (= <>(cparse ()) '(prog ()))))          ; an empty token list is a VALID empty TU (crypt.c), not a parse failure

; --- the generator: one full golden, and the refusals that guard the seams ---
(: (cast src) <>(cparse <>(clex src))
   (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   ir <>(cgn "int main() { return 42; }")
   (assert
      ; slot -8 keeps r3 (rbx, SysV callee-saved) in any fn that TOUCHES r3;
      ; here nothing does, so the copy fold proves the epilogue reload a
      ; self-load and the save/restore pair sweeps away entirely. the default
      ; `return 0` tail is DEAD after the explicit return and sweeps too.
      ; and main is a slotless LEAF, so the frame link itself elides: no
      ; push/mov/sub at entry, a bare ret at the exit.
      (= ir '((call main) (mov r6 r0) (li r0 60) (sys)
              (align 2) (label main) (li r0 42) (ret)))
      (two? (cgn "int main() { int x=1; while (x<5) x++; return x; }"))
      ; a RET-POSITION call tail-JUMPS (stage 8): the epilogue then (jmp F), no
      ; (call F) -- so deep tail recursion runs flat and the lvm shape Continues
      ; by jmp at ai_tco=1. a fn whose frame ESCAPES (& a local) keeps its calls.
      (member? '(jmp rec) <>(cgn "int rec(int n){ if(n==0) return 5; return rec(n-1); } int main(){ return rec(3); }"))
      !(member? '(call rec) <>(cgn "int rec(int n){ if(n==0) return 5; return rec(n-1); } int main(){ return rec(3); }"))
      ; a frame-escaping fn keeps the plain call (the address outlives the frame)
      (member? '(call g) <>(cgn "int g(int*p){ return *p; } int h(int n){ int x=n; return g(&x); } int main(){ return h(3); }"))
      ; a STATEMENT-position attribute run skips (fallthrough..) -- except musttail, which
      ; is OWED now: the annotation rides the ret's PRE slot, marks the call, and sibs
      ; spells the jump or the compile refuses -- never a silent fallback to a plain call
      (member? '(jmp rec) <>(cgn "int rec(int n){ if(n==0) return 5; __attribute__((musttail)) return rec(n-1); } int main(){ return rec(3); }"))
      (two? (cgn "int main(int c, char **v) { switch (c) { case 1: c++; __attribute__((fallthrough)); default: c++; } return c; }"))
      (gripe? (cgn "int main() { __attribute__((musttail)) return 5; }"))   ; no call to mark: refused, named
      (refused? (cgn "int f(int n) { return n; } int main() { int c; __attribute__((musttail)) c = f(1); return c; }"))   ; not a return: refused at parse
      ; a musttail call is owed AS a call: the splice that would swallow a tiny callee
      ; declines, and the kept call leaves as the jmp
      (member? '(jmp f) <>(cgn "int f(int n) { return n + 1; } int h(int n) { __attribute__((musttail)) return f(n); } int main() { return h(3); }"))
      (refused? (cgn "int f() { return 1; }"))                      ; no main, no program
      (gripe? (cgn "int main() { return y; }"))              ; undeclared name -- a NAMED refusal now (rung 1, doc/moon-diag.md)
      (refused? (cgn "int main() { return 5=3; }"))                 ; not an lvalue
      (refused? (cgn "int main() { break; }"))                      ; no loop to leave
      (two? (cgn "int main() { int a=1; int b=2; return a<<b; }"))  ; variable shifts ride shlv now
      (two? (cgn "int f(int a) { return a; } int main() { return f(1); }"))
      (gripe? (cgn "int main() { return f(1,2,3,4,5,6,7); }"))     ; an unknown callee still refuses, now naming it (f undeclared) ...
      (two? (cgn "int f(int a,int b,int c,int d,int e,int x,int y) { return y; } int main() { return f(1,2,3,4,5,6,7); }"))   ; ... but args 7+ ride the caller stack now, both sides
      ; a direct call must AGREE WITH ITS OWN PROTOTYPE -- a short one otherwise
      ; reads whatever the argument register held, in silence.
      (gripe? (cgn "int f(int a, int b); int f(int a,int b){return a+b;} int main() { return f(1); }"))
      (gripe? (cgn "int f(int a); int f(int a){return a;} int main() { return f(1,2); }"))
      (two? (cgn "int f(int a, int b); int f(int a,int b){return a+b;} int main() { return f(1,2); }"))
      ; the named parameters are a FLOOR when the callee is variadic, not a count
      (two? (cgn "int f(int a, ...); int f(int a,...){return a;} int main() { return f(1,2,3); }"))
      (gripe? (cgn "int f(int a, int b, ...); int f(int a,int b,...){return a;} int main() { return f(1); }"))
      ; ⚠ an EMPTY parameter list is exempt, and has to be: `int f()` (unspecified,
      ; the old style) and `int f(void)` (none) park the same empty row in `sigs`,
      ; so refusing the first would refuse musl's headers.
      (two? (cgn "int f(); int main() { return f(1,2,3); }"))
      (two? (cgn "int f(void); int f(void){return 1;} int main() { return f(); }"))
      (two? (cgn "int g; int main() { int *p = &g; *p = 3; return g; }"))
      (refused? (cgn "int main() { int *p; int *q; return (long)(p+q); }"))   ; ptr+ptr refused
      (refused? (cgn "int main() { int x; return *x; }"))                     ; deref wants a pointer
      (refused? (cgn "int main() { int a[3]; int b[3]; a = b; return 0; }"))  ; arrays are not assignable
      (two? (cgn "int g = 1+2; int main() { return g; }"))             ; a global init FOLDS to its constant
      (refused? (cgn "int x; int g = x; int main() { return g; }"))           ; ... but stays refused when genuinely non-constant
      (two? (cgn "char *s = \"hi\"; int main() { return 0; }"))       ; a label-valued global: abs64 (4d)
      (refused? (cgn "int main() { int a[2] = 5; return 0; }"))))          ; a scalar cannot initialize an array

; --- stage 4a: aggregates -- layout to the C rules, members, the new statements ---
(: (cast src) <>(cparse <>(clex src))
   (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (cexpp pre e) (: ast (cast (+ pre (+ " int main() { return " (+ e "; }"))))
                    (last l) (? (two? >l) (last >l) <l)
                    fn (last <>ast)
                    items <><>>>fn
                    <><items)
   (citems src) (: ast (cast src)                       ; the block items of the LAST top-level fn (a sizeof over a typed local folds)
                   (last l) (? (two? >l) (last >l) <l)
                   <><>>>(last <>ast))
   (assert
      (= (cexpp "struct S { char c; int i; char d; };" "sizeof(struct S)") '(num 12))
      (= (cexpp "struct T { char c; long l; };" "sizeof(struct T)") '(num 16))
      (= (cexpp "struct P { int a; }; struct Q { struct P p; char c; };" "sizeof(struct Q)") '(num 8))
      (= (cexpp "union U { char c; long l; int i; };" "sizeof(union U)") '(num 8))
      (= (cexpp "enum { X=3, Y };" "Y") '(num 4))
      (= (cexpp "enum { X=3, Y };" "X") '(num 3))
      (= (cexpp "typedef long word;" "sizeof(word)") '(num 8))
      (= (cexpp "typedef long word;" "sizeof(word*)") '(num 8))
      (= (cexpp "" "s.v") '(dot (var "s") "v"))
      (= (cexpp "" "p->v") '(dot (deref (var "p")) "v"))
      (= (cexpp "" "p->a.b") '(dot (dot (deref (var "p")) "a") "b"))
      ; sizeof over a TYPED local/member folds to a constant at PARSE (an array
      ; bound needs it -- gen's szof lane only sizes at codegen). the ptr member
      ; `h->p` sizes to its undecayed array span (155), and `sizeof buf` reads
      ; buf's own local type (155) -- both foldable, so the bound + the return fold.
      (= (citems "struct H { char n[100]; char p[155]; }; int f(struct H *h) { char buf[sizeof h->p]; return sizeof buf; }")
         '((decl (("buf" (arr char 155) ()))) (ret (num 155))))
      (= (citems "int f(int a[7]) { char buf[sizeof a[0]]; return sizeof buf; }")   ; a[i] element sizeof (a param decays to a pointer: sizeof *ptr = 4)
         '((decl (("buf" (arr char 4) ()))) (ret (num 4))))
      (two? (cgn "struct N { long v; struct N *n; }; int main() { struct N a; a.v = 7; a.n = &a; return a.n->v; }"))
      (two? (cgn "int main() { int x = 2; switch (x) { case 1: return 9; default: return 7; } }"))
      (two? (cgn "int main() { int i = 0; do i++; while (i < 3); return i; }"))
      (two? (cgn "int main() { int i = 0; top: i++; if (i < 3) goto top; return i; }"))
      (refused? (cgn "struct S { int x; }; int main() { struct S s; return s; }"))                       ; no struct VALUES
      (refused? (cgn "struct S { int x; }; int main() { struct S s; return s.y; }"))                      ; no such member
      ; --- bitfields: LSB-first packing into a declared-type unit (SysV/gcc x86);
      ; the member's stored (unit-offset bitoff width) drives a 2-shift load and a
      ; read-modify-write store. layout folds at parse; &bitfield is illegal. ---
      (= (cexpp "struct S { unsigned a:1; unsigned b:1; unsigned c:1; };" "sizeof(struct S)") '(num 4))   ; three :1 pack into one int unit
      (= (cexpp "struct M { char pre; unsigned x:3; unsigned y:5; int post; };" "sizeof(struct M)") '(num 8))   ; the run rides pre's tail, post int-aligns
      (= (cexpp "struct U { unsigned a:4; unsigned b:4; };" "sizeof(struct U)") '(num 4))            ; one byte of bits, int-aligned
      (= (cexpp "struct Z { int a; int :0; int b; };" "sizeof(struct Z)") '(num 8))                 ; `:0` forces the next unit to align
      (two? (cgn "struct b { unsigned a:1; unsigned n:5; int s:4; }; int main() { struct b x; x.a=1; x.n=21; x.s=-3; return x.a + x.n + (x.s < 0); }"))  ; read/write, signed sign-extends
      (refused? (cgn "struct b { unsigned a:1; }; int main() { struct b x; unsigned *p = &x.a; return *p; }"))   ; &bitfield is illegal -- refuse
      ; --- stage 4b: whole-struct assignment (a block copy) and function pointers ---
      (two? (cgn "struct S { int x; int y; }; int main() { struct S a; struct S b; a.x = 3; a.y = 4; b = a; return b.x + b.y; }"))
      (two? (cgn "struct S { long a; long b; }; int f(struct S *p, struct S *q) { *p = *q; return p->a; } int main() { return 0; }"))
      (two? (cgn "int inc(int x) { return x + 1; } int main() { int (*fp)(int); fp = inc; return fp(41); }"))
      (two? (cgn "int inc(int x) { return x+1; } int main() { int (*fp)(int) = 0; fp = inc; return (*fp)(41); }"))
      ; pointer to ARRAY -- (*p)[i] derefs to the array then indexes; &a of an array is one
      (two? (cgn "int a[3] = {1,2,3}; int main() { int (*p)[3] = &a; return (*p)[2]; }"))
      (two? (cgn "int main() { int a[3]; int (*p)[3] = &a; (*p)[1] = 9; return (*p)[1]; }"))
      (two? (cgn "int f(int (*p)[3]) { return (*p)[0]; } int main() { int a[3] = {7,8,9}; return f(&a); }"))
      ; a FUNCTION returning a function pointer -- int (*g(void))(void) : g is a function
      ; whose return type is a pointer-to-function. definition, prototype, and inner params bound.
      (two? (cgn "int f(void){return 7;} int (*g(void))(void){ return f; } int main(){ return g()(); }"))
      (two? (cgn "int f0(int x){return x;} int f1(int x){return x+1;} int (*pick(int k))(int){ return k ? f1 : f0; } int main(){ return pick(1)(9); }"))
      (two? (cgn "int f(void){return 7;} int (*g(void))(void); int (*g(void))(void){ return f; } int main(){ return g()(); }"))
      ; a dispatch table -- an array of function pointers, THE love.c pattern
      (two? (cgn "int a(int x){return x+1;} int b(int x){return x*2;} int main(){ int (*t[2])(int); t[0]=a; t[1]=b; return t[0](10)+t[1](10); }"))
      ; --- stage 4c: brace/designated initializers, string inits, [] inference, flexible arrays ---
      (= (cast "int main() { int a[3] = {1,2,3}; return a[0]; }")
         '(prog ((fn "main" () (blk ((decl (("a" (arr int 3) (init ((num 1) (num 2) (num 3)))))) (ret (deref (bin + (var "a") (num 0))))))))))
      (= (cast "struct S { int x; int y; }; int main() { struct S s = {.y=5,.x=2}; return s.x; }")
         '(prog ((tdef) (fn "main" () (blk ((decl (("s" (struct "S") (init ((dfield "y" (num 5)) (dfield "x" (num 2))))))) (ret (dot (var "s") "x"))))))))
      (= (cast "int g[] = {10,20,30}; int main() { return 0; }")   ; [] size inferred from the brace list
         '(prog ((gdecl (("g" (arr int 3) (init ((num 10) (num 20) (num 30)))))) (fn "main" () (blk ((ret (num 0))))))))
      (okv? (cparse <>(clex "struct T { int n; char d[]; }; int main() { return sizeof(struct T); }")))        ; a flexible array member parses
      (two? (cgn "int main() { int a[4] = {5}; return a[0] + a[3]; }"))            ; partial init zero-fills the rest
      (two? (cgn "struct P { int a; int b; }; int main() { struct P t[2] = {{1,2},{3,4}}; return t[1].a; }"))  ; nested array-of-struct
      ; brace elision (C 6.7.9): a flat run fills nested subaggregates whose braces were
      ; omitted (a scalar item on an aggregate element was a codegen error before). global +
      ; local, arrays + structs, deep nesting; the fully-braced form still lays identically.
      (two? (cgn "int a[2][2] = {1,2,3,4}; int main(){ return a[1][1]; }"))
      (two? (cgn "int main(){ int a[2][2] = {1,2,3,4}; return a[1][0]; }"))
      (two? (cgn "struct P{int x,y;}; struct P p[2] = {1,2,3,4}; int main(){ return p[1].x; }"))
      (two? (cgn "int a[2][2][2] = {1,2,3,4,5,6,7,8}; int main(){ return a[1][1][1]; }"))
      (two? (cgn "char b[8] = \"hi\"; int main() { return b[0]; }"))               ; a global char array from a string
      (two? (cgn "int t[3] = {1,2,3}; int main() { return t[2]; }"))               ; a global array constant image
      (two? (cgn "int main() { char s[4] = \"ab\"; return s[0]; }"))               ; a local char array from a string
      ; --- stage 4d: label-valued global images through the abs64 data relocation ---
      (two? (cgn "char *s = \"hi\"; int main() { return 0; }"))                    ; char* -> a rodata string's address
      (two? (cgn "int q = 5; int *p = &q; int main() { return *p; }"))             ; &global
      (two? (cgn "int a[2] = {1,2}; int *p = a; int main() { return p[1]; }"))     ; an array decays to its address
      (two? (cgn "char *m[3] = {\"a\",\"bb\",\"ccc\"}; int main() { return m[0][0]; }"))   ; an array of string pointers
      (two? (cgn "char *m[] = {\"x\",\"y\"}; int main() { return 0; }"))           ; ... with [] size inference
      ; THE love.c pattern: a static table of (name, function-pointer) records
      (two? (cgn "struct N { char *nm; int (*fn)(int); }; int f(int x){return x;} struct N t[1] = {{\"f\", f}}; int main() { return t[0].fn(9); }"))
      ; a static initializer is any CONSTANT ADDRESS: label + a compile-time byte
      ; offset (the offset rides the abs64 addend). &arr[i], decayed pointer
      ; arithmetic, a const-expr index, &struct.member, &arr[i].member (nested).
      (two? (cgn "int a[4]; int *p = &a[2]; int main() { return 0; }"))                 ; &arr[i]
      (two? (cgn "int a[4]; int *p = a + 3; int main() { return 0; }"))                 ; decay + const (scaled)
      (two? (cgn "enum{K=2}; int a[8]; int *p = &a[K+1]; int main() { return 0; }"))    ; a const-expr subscript
      (two? (cgn "struct P{int x,y;}; struct P s; int *p = &s.y; int main() { return 0; }"))      ; &member
      (two? (cgn "struct P{int x,y,z;}; struct P a[3]; int *p = &a[1].z; int main() { return 0; }")) ; &arr[i].member
      (two? (cgn "char *p = \"ok\" + 1; int main() { return 0; }"))                     ; a string label + offset
      ; a NON-constant address still refuses (gcc: initializer not constant)
      (refused? (cgn "int a[4]; int i; int *p = &a[i]; int main() { return 0; }"))
      ; the TOP-LEVEL fn-pointer fence FELL (rung D): ptop's first declarator
      ; routes through pdtor now -- `T (*name)(args)` (love.c's ai_image_absguard)
      ; AND the array form both parse, and gen's &f-in-images lane lays them
      (two? (cgn "int a(int x){return x;} int (*t[1])(int) = {a}; int main() { return t[0](3); }"))
      (two? (cgn "long (*gp)(long) = 0; int main() { return gp ? 1 : 0; }"))))

; --- the double lane (stage 6a, the gem): float literals lex, `double` types,
; the xmm value path, int<->double conversions, and the calling convention. ---
(: (cast src) <>(cparse <>(clex src))
   (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (tn ts k) (? (< k 1) <ts (tn >ts (- k 1)))
   (tn3 ts k) (: t (tn ts k) `(<t <>t <>>t))   ; kind/value/line: not the slot-4 spelling
   irbits <>(cgn "int main() { double d = 1.0; return (int)d; }")
   (has op forms) (: (go l) (? !(two? l) () (two? <l && (= <<l op)) 1 (go >l)) (go forms))
   (assert
      ; the lexer: a float literal is a 'flo token carrying a gem
      (= (tn3 <>(clex "3.5") 0) (L 'flo 3.5 1))
      (= (tn3 <>(clex ".25") 0) (L 'flo 0.25 1))
      (= (tn3 <>(clex "1e3") 0) (L 'flo 1000.0 1))
      (= (tn3 <>(clex "2.") 0) (L 'flo 2.0 1))
      ; the imaginary suffix (i/j, either order with f/l) is its own kind
      (= (tn3 <>(clex "1.0fi") 0) (L 'iflo 1.0 1))
      (= (tn3 <>(clex "2.5if") 0) (L 'iflo 2.5 1))
      (= (tn3 <>(clex ".5i") 0) (L 'iflo 0.5 1))
      (= (tn3 <>(clex "42") 0) (L 'num 42 1))               ; an integer stays 'num
      ; the parser: a double type and a 'flo primary
      (= (cast "int main() { double d = 1.5; return 0; }")
         '(prog ((fn "main" () (blk ((decl (("d" double (flo 1.5)))) (ret (num 0))))))))
      ; the 1.0 constant loads its IEEE-754 bits then movqxr into f0
      (has 'movqxr irbits)
      (has 'li irbits)
      ; the value path compiles: locals, arithmetic, mixed int/double, compare, cast
      (two? (cgn "int main() { double a = 1.5; double b = 2.5; return (int)(a + b); }"))
      (two? (cgn "int main() { double a = 3.0; int n = 2; return (int)(a * n); }"))     ; mixed -> double
      (two? (cgn "int main() { double a = 1.0; double b = 2.0; return a < b; }"))       ; compare -> int
      (two? (cgn "int main() { double a = 5.5; return (int)(-a); }"))                   ; unary minus
      (two? (cgn "int main() { double a = 0.0; return !a; }"))                          ; logical not on a double
      (two? (cgn "double g = 2.5; int main() { return (int)g; }"))                      ; a double global image
      (two? (cgn "int main() { double a[2] = {1.5, 2.5}; return (int)(a[0] + a[1]); }")); a double array + element load
      (two? (cgn "struct P { double x; int n; }; int main() { struct P p = {1.5, 2}; return (int)p.x + p.n; }"))
      ; the xmm CALLING convention (6b): double params/returns/args ride f0..f7
      (two? (cgn "int f(double x) { return (int)x; } int main() { return 0; }"))       ; a double parameter
      (two? (cgn "double f() { return 1.5; } int main() { return (int)f(); }"))        ; a double return + read
      (two? (cgn "double add(double a, double b) { return a + b; } int main() { return (int)add(1.5, 2.5); }"))
      (two? (cgn "int f(int x) { return x; } int main() { return f(2.5); }"))          ; a double arg -> an int param converts
      (two? (cgn "int main() { double d = 3.5; return d; }"))                          ; a double value, an int return truncates
      (two? (cgn "int mix(int a, double b, int c) { return a + (int)b + c; } int main() { return mix(1, 2.5, 3); }"))
      (two? (cgn "double add(double a, double b) { return a + b; } int main() { int i = add(2, 3); return i; }"))  ; a double result into an int slot truncates
      (two? (cgn "int main() { int i = 3.5; return i; }"))                             ; a double literal into an int local
      (two? (cgn "int main() { double d = 3.0; int n = 0; while (d) { d = d - 1.0; n++; } return n; }"))  ; a bare double condition (d != 0.0)
      (two? (cgn "int main() { double d = 2.5; return d ? 9 : 4; }"))                  ; a double ternary condition
      (refused? (cgn "int main() { double a = 1.0; return ~a; }"))))                          ; ~ on a double is still invalid

; --- variadic functions + _Static_assert (stage 6c). the `...` is recorded as
; the signature's variadic flag (cc's simplified all-stack call ABI -- doc/moon.md);
; the three __builtin_va_* forms parse to (vastart|vaend v) / (vaarg v type);
; _Static_assert folds its constant at parse and vanishes to a (tdef) no-op,
; refusing the whole compile when the constant is false. ---
(: (cast src) <>(cparse <>(clex src))
   (csig src name) (: r (cparse <>(clex src)) (peep <>>>r name 'none))
   (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (assert
      ; the `...` leaves the fn node clean; the variadic bit lives in the signature
      (= (cast "int f(int n, ...) { return n; }")
         '(prog ((fn "f" (("n" int)) (blk ((ret (var "n"))))))))
      (= (csig "int f(int n, ...) { return n; }" "f") '(int (int) 1))    ; rettype, params, variadic=1
      (= (csig "int g(int a, int b) { return a; }" "g") '(int (int int) ()))  ; a fixed fn, variadic=()
      (= (csig "int p(int n, ...);" "p") '(int (int) 1))                 ; a variadic prototype records too
      ; the three va builtins parse to their marker forms
      (= (cast "int f(int n, ...) { char *ap; __builtin_va_start(ap, n); int x = __builtin_va_arg(ap, int); __builtin_va_end(ap); return x; }")
         '(prog ((fn "f" (("n" int)) (blk ((decl (("ap" (ptr char) ())))
                                           (expr (vastart (var "ap")))
                                           (decl (("x" int (vaarg (var "ap") int))))
                                           (expr (vaend (var "ap")))
                                           (ret (var "x"))))))))
      ; _Static_assert folds and vanishes to a (tdef) no-op, at file and block scope
      (= (cast "int main() { _Static_assert(1, \"ok\"); return 0; }")
         '(prog ((fn "main" () (blk ((tdef) (ret (num 0))))))))
      (= (cast "_Static_assert(sizeof(int) == 4, \"w\"); int main() { return 0; }")
         '(prog ((tdef) (fn "main" () (blk ((ret (num 0))))))))
      ; cfold folds 64-bit machine PATTERNS: >> under an unsigned LEFT is LOGICAL
      ; ((uintptr_t)-1 >> 2 is max-charm, not -1 -- the arithmetic fold pinned
      ; max-charm -1 and flipped spec.l's `word` on the cc-built love); a signed >>
      ; stays arithmetic; a NARROW cast masks to width, sign-extending the signed
      (= (cast "long mx = (long)((unsigned long)-1 >> 2); int main() { return 0; }")
         '(prog ((gdecl (("mx" long (num 4611686018427387903)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "long mc = (long)(((unsigned long)(long)((long)((unsigned long)-1 >> 2))<<1)|1); int main() { return 0; }")
         '(prog ((gdecl (("mc" long (num 9223372036854775807)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "long a = -8 >> 2; int main() { return 0; }")
         '(prog ((gdecl (("a" long (num -2)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "unsigned int u = (unsigned int)-1; int main() { return 0; }")
         '(prog ((gdecl (("u" uint (num 4294967295)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "int s = (short)100000; int main() { return 0; }")
         '(prog ((gdecl (("s" int (num -31072)))) (fn "main" () (blk ((ret (num 0))))))))
      ; the generator: a variadic fn and a variadic call both compile
      (two? (cgn "int f(int n, ...) { return n; } int main() { return f(1); }"))
      (two? (cgn "int s(int n, ...) { char *ap; __builtin_va_start(ap,n); int t=0; int i=0; while(i<n){ t=t+__builtin_va_arg(ap,int); i++; } __builtin_va_end(ap); return t; } int main() { return s(2, 3, 4); }"))
      ; a parameter of ARRAY type decays to a pointer (the C rule -- so a va_list,
      ; which is an array typedef, rides as a pointer and can be handed on)
      (= (cast "int f(int a[], int n) { return n; }")
         '(prog ((fn "f" (("a" (ptr int)) ("n" int)) (blk ((ret (var "n"))))))))
      (two? (cgn "int sum(int a[], int n) { int s = 0; for (int i = 0; i < n; i++) s += a[i]; return s; } int main() { int x[2]; x[0] = 40; x[1] = 2; return sum(x, 2); }"))
      ; a true _Static_assert compiles; a false one refuses the whole program
      (two? (cgn "int main() { _Static_assert(sizeof(int) == 4, \"w\"); return 0; }"))
      (two? (cgn "_Static_assert(1 + 2 * 3 == 7, \"folds\"); int main() { return 0; }"))
      (refused? (cgn "int main() { _Static_assert(sizeof(int) == 5, \"bad\"); return 0; }"))
      (refused? (cgn "_Static_assert(0, \"nope\"); int main() { return 0; }"))
      ; ⚠ EVERY declarator in a declaration infers its own [] bound, not just the
      ; first. a later one has its dims parsed before the initializer is seen, and
      ; that path used to skip the inference -- so `char a[]={1}, b[]={2,2};` left
      ; b INCOMPLETE and sizeof(b) answered 0 in SILENCE. the bytes were always
      ; laid from the initializer; only the type was wrong, which is the worse
      ; half: a countof() over b read zero and every loop over it did nothing.
      (= (cast "char a[]={1}, b[]={2,2}, c[]={3,3,3};")
         '(prog ((gdecl (("a" (arr char 1) (init ((num 1))))
                         ("b" (arr char 2) (init ((num 2) (num 2))))
                         ("c" (arr char 3) (init ((num 3) (num 3) (num 3)))))))))
      (= (cast "char *p, q[]={1,2};")
         '(prog ((gdecl (("p" (ptr char) ()) ("q" (arr char 2) (init ((num 1) (num 2)))))))))
      (two? (cgn "static const char a[]={1,2,3}, b[]={4,5,6}; _Static_assert(sizeof(b) == 3, \"m\"); int main() { return a[0]; }"))
      (refused? (cgn "static const char a[]={1,2,3}, b[]={4,5,6}; _Static_assert(sizeof(b) == 0, \"m\"); int main() { return a[0]; }"))
      ; ..and the MESSAGE is optional (C23), which is how the kernel writes it.
      ; the assertion was always the point; the string only ever worded the
      ; diagnostic. a false one still refuses -- the short form is not a weaker one.
      (two? (cgn "_Static_assert(sizeof(int) == 4); int main() { return 0; }"))
      (two? (cgn "int main() { _Static_assert(1); return 0; }"))
      (refused? (cgn "_Static_assert(sizeof(int) == 5); int main() { return 0; }"))
      (refused? (cgn "int main() { _Static_assert(0); return 0; }"))
      ; cfold: ?: folds the SELECTED arm (both directions); &&/|| fold != 0 (a negative is true).
      (= (cast "int g = 1?2:3; int main() { return 0; }")
         '(prog ((gdecl (("g" int (num 2)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "int g = 0?2:3; int main() { return 0; }")
         '(prog ((gdecl (("g" int (num 3)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "int g = -1 && 1; int main() { return 0; }")
         '(prog ((gdecl (("g" int (num 1)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "int g = 0 && 5; int main() { return 0; }")
         '(prog ((gdecl (("g" int (num 0)))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "int g = 0 || 5; int main() { return 0; }")
         '(prog ((gdecl (("g" int (num 1)))) (fn "main" () (blk ((ret (num 0))))))))
      ; the dead ternary arm need not be constant (or defined): 1 ? 1 : 1/0 is a valid const expr
      (two? (cgn "_Static_assert(1 ? 1 : 1/0, \"dead div0 unevaluated\"); int main() { return 0; }"))
      ; false-folding asserts now FIRE (were silently let by -- the whole point of the fix)
      (refused? (cgn "_Static_assert(1 ? 0 : 0, \"fires\"); int main() { return 0; }"))
      (refused? (cgn "_Static_assert(1 && 0, \"fires\"); int main() { return 0; }"))
      (refused? (cgn "_Static_assert(0 || 0, \"fires\"); int main() { return 0; }"))
      ; a negative && is TRUE (would fire under the old (0 < a) truthiness) -- must NOT fire
      (two? (cgn "_Static_assert(-1 && 1, \"negative true\"); int main() { return 0; }"))
      ; a GENUINELY non-constant assert is still let by (cfold partial by design, policy unchanged)
      (two? (cgn "int nc; _Static_assert(nc, \"let by\"); int main() { return 0; }"))))

; --- the `float` scalar + the compound literal (stage 6d). float is 4 bytes in
; memory but shares the double register lane (a load widens, a store narrows);
; sizeof(float) is 4. a compound literal (type){init} parses to ('clit ty init),
; an unnamed object whose ADDRESS is an lvalue. ---
(: (cast src) <>(cparse <>(clex src))
   (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (cexp src) (: ast (cast (+ "int main() { return " (+ src "; }")))
                 fn <<>ast
                 items <><>>>fn
                 <><items)
   (assert
      ; float parses as a scalar type; sizeof(float) is 4
      (= (cast "int main() { float f = 1.5; return 0; }")
         '(prog ((fn "main" () (blk ((decl (("f" float (flo 1.5)))) (ret (num 0))))))))
      (= (cexp "sizeof(float)") '(num 4))
      (= (cexp "sizeof(float*)") '(num 8))
      ; a compound literal parses to ('clit ty init), address-takeable
      (= (cexp "(int){7}") '(clit int (init ((num 7)))))
      (= (cexp "&(int){7}") '(addr (clit int (init ((num 7))))))
      ; the generator: the float value path -- locals, arithmetic, compare, the
      ; calling convention, arrays, struct members, and a single-precision global
      (two? (cgn "int main() { float a = 1.5; float b = 2.25; return (int)(a + b); }"))
      (two? (cgn "int main() { float a = 3.0; return a < 5.0; }"))
      (two? (cgn "float sc(float x, float k) { return x * k; } int main() { return (int)sc(2.5, 3.0); }"))
      (two? (cgn "int main() { float a[2] = {1.5, 2.5}; return (int)(a[0] + a[1]); }"))
      (two? (cgn "struct V { float x; int n; }; int main() { struct V v; v.x = 4.5; v.n = 2; return (int)v.x + v.n; }"))
      (two? (cgn "float g = 6.25; int main() { return (int)g; }"))                 ; a float global -> a 4-byte single image
      (refused? (cgn "int main() { float a = 1.0; return ~a; }"))                          ; ~ on a float is still invalid
      ; the compound literal: a struct one address-taken and read, a scalar one
      (two? (cgn "struct P { int a; int b; }; int f(struct P *p) { return p->a + p->b; } int main() { return f(&(struct P){10, 32}); }"))
      (two? (cgn "int main() { int *p = &(int){42}; return *p; }"))
      (two? (cgn "struct P { int a; int b; }; int main() { return (&(struct P){3, 4})->a; }"))))

; --- the object view (stage 7a): cgen-obj splits the program into TEXT (functions,
; no _start) and DATA (globals + strings), and names the DEFINED functions and
; global variables for the symbol table. crew/holo/obj.l turns that into a
; relocatable .o (the ELF bytes are gated end-to-end by the make .o link/interop
; smoke, which needs the holo book + the system linker). ---
(: (cob src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen-obj <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (el l i) (? (< i 1) <l (el >l (- i 1)))
   ob (cob "int f(int x) { return x; } int g = 5; int main() { return f(g); }")
   (assert
      (two? ob)                                             ; (1 text data funcnames globnames)
      (= (el ob 3) '("f" "main"))                           ; the defined functions -> global FUNC symbols
      (= (el ob 4) '("g"))                                  ; the global variables -> global OBJECT symbols
      (member? '(label f) (el ob 1))                        ; a function label lives in TEXT
      (member? '(label g) (el ob 2))                        ; a global label lives in DATA
      !(member? '(call main) (el ob 1))                     ; NO _start/crt0 in an object (the linker's crt calls main)
      (two? (cob "int only(int x) { return x + 1; }"))      ; a LIBRARY TU (no main) still emits an object
      (gripe? (cob "int main() { return nope; }"))))        ; an undeclared name refuses, now naming it (rung 1)

; --- __attribute__((section(NAME))): a named section is a STREAM of its own.
; three names have a meaning the object writer knows -- ai_nifs (bracketed by
; the linker for main()'s boot drain), .image (laid last, for the bake) and
; .data (where an unattributed global goes). every OTHER name rides out as its
; own section, because WHERE those bytes land is the point: the kernel's
; .limine_requests is SCANNED for by a bootloader, and a request block quietly
; sitting in .data is a kernel that boots with an empty memmap. cob passes no
; xtra, so this needs the variant that carries the parser's section table. ---
(: (cobx src)
    (: r (cparse <>(clex src))
       (? (okv? r) (cgen-obj (el r 1) (el r 2) (el r 3) (el r 4)
                             `((el r 5) (el r 6) (el r 7) (el r 8) (el r 9)) 'x64) ()))
   (el l i) (? (< i 1) <l (el >l (- i 1)))
   ob (cobx "__attribute__((used, section(\".limine_requests\"))) static volatile long m = 7;
             __attribute__((section(\".limine_requests_end\"))) static volatile long e = 1;
             long plain = 3; int main() { return (int) (m + e + plain); }")
   xs (el ob 8)
   (assert
      (= (tally xs) 2)                                   ; two extra sections, one per NAME
      ; ..in DECLARATION order: the three limine sections bracket a scan, so the
      ; order is the protocol's and not the compiler's to choose
      (= <<xs ".limine_requests")
      (= <<>xs ".limine_requests_end")
      (member? '(label m) <><xs)                          ; the global's bytes land THERE
      (member? '(label e) <><>xs)
      !(member? '(label m) (el ob 2))                     ; ..and NOT in .data
      !(member? '(label e) (el ob 2))
      (member? '(label plain) (el ob 2))                  ; an unattributed global still does
      ; the two SPECIAL names keep their own streams and stay out of the extras
      (= (el (cobx "__attribute__((section(\"ai_nifs\"))) static long n = 1; int main() { return 0; }") 8) ())
      (= (el (cobx "__attribute__((section(\".image\"))) static long i = 1; int main() { return 0; }") 8) ())))

; --- the preprocessor (stage 5): object/function macros, rescan, hidesets,
; # stringize, ## paste, variadics, and #if/#ifdef conditional const-expr eval.
; pps runs cpp (no includes) and joins the output tokens' spellings to a string. --
(: (spell1 t) (: k <t
                 (? (= k 'str) (+ "\"" (+ <>t "\""))
                    ((= k 'num) || (= k 'kw)) (show <>t)
                    <>t))
   (pps src) (: r (cpp <>(clex src) () "<law>")
                (? !(okv? r) 'FAIL
                   (: (go l) (? !(two? l) "" (+ (spell1 <l) (+ (? (two? >l) " " "") (go >l))))
                      (go (fsbody <>r)))))        ; peel cpp's leading spans marker (parse.l fsbody)
   ; the refusal side of the same door: the message a cpp gripe carries, and the
   ; line it names.
   (cppno src) (: r (cpp <>(clex src) () "<law>") (? (gripe? r) <>>>>r 'FAIL))
   (cppline src) (: r (cpp <>(clex src) () "<law>") (? (gripe? r) <>>r 'FAIL))
   (assert
      (= (pps "#define N 42\nN + 1") "42 + 1")                         ; object macro
      (= (pps "#define A B\n#define B 7\nA") "7")                      ; rescan to fixpoint
      (= (pps "#define ADD(a,b) a+b\nADD(1,2)") "1 + 2")               ; function macro
      (= (pps "#define EOF (-1)\nEOF") "( - 1 )")                      ; 7c-iii: a SPACE before ( = an object macro whose body is parenthesized
      (= (pps "#define M(x, ...) f(x, ##__VA_ARGS__)\nM(a)") "f ( a )")        ; GNU `, ##__VA_ARGS__` elides the comma at zero args
      (= (pps "#define M(x, ...) f(x, ##__VA_ARGS__)\nM(a, b, c)") "f ( a , b , c )")
      (= (pps "#define SQ(x) ((x)*(x))\nSQ(SQ(2))")                    ; nested / re-expansion
         "( ( ( ( 2 ) * ( 2 ) ) ) * ( ( ( 2 ) * ( 2 ) ) ) )")
      (= (pps "#define S(x) #x\nS(a b)") "\"a b\"")                    ; # stringize
      ; # owes the argument's SPELLING, which its value cannot answer: 0777 and 511
      ; are one value, 1u and 1 are one value, and a float's value is not text at all
      (= (pps "#define S(x) #x\nS(0777)") "\"0777\"")                  ; ..not the octal's decimal
      (= (pps "#define S(x) #x\nS(00)") "\"00\"")                      ; ..leading zeros kept
      (= (pps "#define S(x) #x\nS(0x1F)") "\"0x1F\"")                  ; ..hex stays hex
      (= (pps "#define S(x) #x\nS(1u)") "\"1u\"")                      ; ..suffix kept
      (= (pps "#define S(x) #x\nS(1.20)") "\"1.20\"")                  ; ..a float is text, not a value
      (= (pps "#define S(x) #x\nS(\"hi\")") "\"\"hi\"\"")              ; ..a string keeps its quotes
      (= (pps "#define C(a,b) a##b\nC(foo,bar)") "foobar")             ; ## paste
      (= (pps "#define P(...) f(__VA_ARGS__)\nP(1,2,3)") "f ( 1 , 2 , 3 )")   ; variadic
      (= (pps "#define F F\nF") "F")                                   ; blue paint: self-ref stops
      (= (pps "#define X\n#ifdef X\ny\n#endif") "y")                   ; #ifdef taken
      (= (pps "#ifndef Z\nn\n#endif") "n")                             ; #ifndef taken
      (= (pps "#if 2+3*4 == 14\nok\n#endif") "ok")                     ; const-expr precedence
      (= (pps "#if 0\na\n#elif 1\nb\n#else\nc\n#endif") "b")           ; #elif chain
      (= (pps "#if (1 << 4) > 10 && !0\ny\n#endif") "y")               ; shift / logical / unary
      (= (pps "#define G 1\n#if defined(G)\nd\n#endif") "d")           ; defined() operator
      (= (pps "#define M(x) x\nno M here") "no M here")                ; a fn macro name not called stays literal
      (= (pps "__LINE__") "1")                                         ; __LINE__ predefined -> the line number
      (= (pps "a\nb\n__LINE__") "a b 3")                               ; ... tracked down the file
      (= (pps "__FILE__") "\"<law>\"")                                 ; __FILE__ predefined -> the TU path, a string
      (= (pps "#define M a /* one \\\n   two */ b\nM") "a b")          ; a \<nl> INSIDE a comment splices (phase 2 before 3): the directive holds together (m4 regex.c)
      (okv? (cpp <>(clex "#ifdef NOPE\nx\n#endif") () "<law>"))        ; a fully-skipped file is a VALID empty TU (crypt.c), not a failure
      (= (pps "#define E\nE + E") "+")                                 ; empty macro body vanishes
      ; ⚠ a refusal is a GRIPE, and a gripe is a PAIR -- so these ask okv?/cppno,
      ; where a `!` would read every one of them as success.
      (= (cppno "#if 1\nx") "unterminated #if -- no #endif")           ; unterminated #if refuses
      (= (cppno "#endif") "#endif without #if")                        ; ..and so does a stray closer
      (= (cppno "#error boom") "#error: boom")                         ; #error is FATAL, and says the program's own words
      (= (cppno "#cmakedefine X 1")                                    ; an unimplemented directive REFUSES (C11 6.10p1)
         "invalid preprocessing directive #cmakedefine")
      (= (cppno "#include_next <x.h>")                                 ; include_next among them: dropping it loses a header in silence
         "invalid preprocessing directive #include_next")
      (= (cppline "a\nb\n#error late") 3)                              ; the gripe carries the DIRECTIVE's line
      (= (cppline "#if 1\na\nb") 1)                                    ; ..and an unterminated one carries its OPENER's
      (= (cppline "#if 0\na\n#else\nb") 1)                             ; ..which an #else does not reset
      (okv? (cpp <>(clex "#\n#pragma x\n# 4 \"f.c\"\nz") () "<law>"))))        ; null, pragma and gcc's line marker all pass

; --- diagnostics: a parse refusal names where it stood (doc/moon-diag.md rung 2) ---
; the gripe carrier is ('gripe file line col msg); file () means the TU itself,
; which the driver (moon.l ccdie) fills the source path in for.
(: (inc txt) (\ n s `(1 txt))
   (ppi src txt) (: r (cpp <>(clex src) (inc txt) "<law>") (? (two? r) <>r 'FAIL))
   (pgi src txt) (cparse (ppi src txt))
   (gfile g) <>g (gline g) <>>g
   hdr "int a(void);\nint b(void);\nint c(void);\n"
   (assert
      ; a bad form inside a HEADER names the HEADER and the header's own line.
      ; this is the thing a line number alone can never say: an #include
      ; splices the header's numbering into the one flat stream.
      (: g (pgi "int a(void);\n#include \"h.h\"\n" "int ok(void);\nint bad(void) { return ( ; }\n")
         (gripe? g && (gfile g = "h.h") && (gline g = 2)))
      ; ... and symmetrically the TU's own lines do NOT skew by the lines an
      ; earlier include spliced in front of them
      (: g (pgi "#include \"h.h\"\nint main(void)\n{\n  return (1 + ;\n}\n" hdr)
         (gripe? g && !(gfile g) && (gline g = 4)))
      ; the report site is the FURTHEST token the parse reached, NOT the failing
      ; form's first: a body that dies on line 5 must not point at line 2
      (: g (cparse <>(clex "int ok(void) { return 1; }\nint bad(void)\n{\n  int x = 1;\n  x = x + ;\n  return x;\n}\n"))
         (gripe? g && (gline g = 5)))
      ; adjacent string literals JOIN ACROSS an include boundary -- host/main.c
      ; splices the egg exactly so ("(" then #include egg.h then "'(" ..), so
      ; the file marks must stay invisible to phase 6
      (= <>(pgi "char *s = \"a\"\n#include \"h.h\"\n\"c\";\n" "\"b\"\n")
         '(prog ((gdecl (("s" (ptr char) (str "abc")))))))
      ; a clean parse is still a plain result, never mistaken for a gripe
      (okv? (pgi "int main(void) { return 0; }\n" hdr))))

; --- diagnostics: a DELIBERATE refusal names the feature it has no lane for
; (doc/moon-diag.md rung 4). the whole point is the DISTINCTION: a `'bad` meaning
; "I decline, take another lane" (an inline decline, a policy deopt) must stay
; SILENT, and only "there is no lane for this, anywhere" speaks. so every law
; below pins BOTH directions -- the refusal names itself, AND the same source on
; a target that HAS the lane compiles clean. a message golden is worth its churn
; here: the string IS the deliverable.
; cgen-obj, not cgen: the OBJECT view compiles a TU as it stands, where cgen
; also insists on a `main` to start -- and a lane's refusal has nothing to do
; with whether the file happens to be a program.
(: (cgt src tgt)
    (: r (cparse-t <>(clex src) tgt)
       (? (okv? r) (cgen-obj <>r <>>r <>>>r <>>>>r () tgt) ()))
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   (gmsg g) (el g 4)
   vla "int s(int n){ int a[n]; a[0]=1; return a[0]; }"
   big "struct b { long a,b,c,d,e; };\n"
   (assert
      ; the C99 VLA: x64 and arm64 carry the lane, riscv64 does not -- and says which
      (okv? (cgt vla 'x64))
      (okv? (cgt vla 'arm64))
      (: g (cgt vla 'riscv64)
         (gripe? g && (gmsg g = "no lane for a variable-length array on riscv64 (in s)")))
      ; a >16-byte struct by value. the two halves refuse SEPARATELY and each
      ; names which half it is -- passing and returning are different lanes, and
      ; "codegen error" could never tell you which one you had
      (: g (cgt (big + "int t(struct b f){ return (int)f.a; }") 'arm64)
         (gripe? g && (gmsg g = "no lane for passing this 40-byte struct by value on arm64 (in t)")))
      (: g (cgt (big + "struct b m(long x){ struct b r; r.a=x; return r; }") 'arm64)
         (gripe? g && (gmsg g = "no lane for returning this 40-byte struct by value on arm64 (in m)")))
      (okv? (cgt (big + "int t(struct b f){ return (int)f.a; }") 'x64))
      ; C99 complex arithmetic is an x64 lane only
      (: g (cgt "double _Complex f(double _Complex a, double _Complex b){ return a*b; }" 'arm64)
         (gripe? g && (gmsg g = "no lane for C99 complex arithmetic on arm64 (in f)")))
      ; a refusal with NO named cause is still ONE gripe, never a note on one
      ; channel and an error on another -- the whole message reaches whichever
      ; stream a build log kept
      (: g (cgt "int f(void){ return nope; }" 'x64)
         (gripe? g && (gmsg g = "undeclared 'nope' (in f)")))
      ; the static-data lane refuses through the same door: a cause pinned by
      ; globs, a gripe minted at gfns's tail (cgdata's callers thread a bare
      ; 'bad through a dozen joins, and a gripe is not one)
      (: g (cgt "int g = nope;\n" 'x64)
         (gripe? g && (gmsg g = "cannot lay the initializer of 'g' ((var \"nope\"))")))))

; -- 7c-i: freestanding-header surface. cc keeps no linkage/qualifier state, so
; storage/qualifier keywords (static const volatile restrict register inline) and
; __attribute__((..)) are SKIPPED wherever a type may begin or a declarator carry
; them -- the AST equals the unqualified source. an unnamed prototype parameter
; parses as an abstract declarator (the form the system headers' protos use).
(: (cast src) <>(cparse <>(clex src))
   (assert
      (= (cast "static const int x = 5;")  (cast "int x = 5;"))
      (= (cast "int f(int const a, volatile int b){return a;}")
         (cast "int f(int a, int b){return a;}"))
      (= (cast "int g(char * restrict p){return *p;}")
         (cast "int g(char *p){return *p;}"))
      (= (cast "__attribute__((unused)) static int h(void){return 1;}")
         (cast "int h(void){return 1;}"))
      ; TRAILING __attribute__((..)) / __asm__(..) on a declarator (glibc's prototypes:
      ; `int f(void) __attribute__((__nothrow__));`) skip the same -- proto, fn def, data, multi
      (= (cast "int f(int n) __attribute__((__nothrow__ , __leaf__));")
         (cast "int f(int n);"))
      (= (cast "int g(void) __attribute__((__nothrow__)){return 0;}")
         (cast "int g(void){return 0;}"))
      (= (cast "int v __attribute__((aligned(8)));")  (cast "int v;"))
      ; ..an ARRAY declarator carries the trailing run too (the AST is unchanged --
      ; the aligned(N) itself is HONORED out of band, parse's 'aligns tablet ->
      ; cgdata's ('align n); the loader's page tables stand on it)
      (= (cast "int a[16] __attribute__((aligned(64)));") (cast "int a[16];"))
      (= (cast "int a[2][3] __attribute__((aligned(16))) = {{1,2,3},{4,5,6}};")
         (cast "int a[2][3] = {{1,2,3},{4,5,6}};"))
      (= (cast "int a __attribute__((unused)), b;")   (cast "int a, b;"))
      (= (cast "extern int x __asm__(\"realx\");")     (cast "extern int x;"))
      (= (cast "int main(void){register int a=3; const int b=4; return a+b;}")
         (cast "int main(void){int a=3; int b=4; return a+b;}"))
      (= (cast "int f(void const *p){return 0;}")             ; a qual between type and star keeps the pointer
         (cast "int f(void *p){return 0;}"))
      (= (cast "int f(int, long); int main(void){return 0;}") ; unnamed proto params -> abstract declarators
         '(prog ((proto "f") (fn "main" () (blk ((ret (num 0))))))))))

; -- 7c-ii: a type-specifier RUN canonicalizes to one type symbol. the unsigned
; scalars are 'uchar 'ushort 'uint 'ulong (gen: zero-extend loads, logical >>,
; unsigned compare/divide); `signed` folds away, `long long` is 'long.
(: (cast src) <>(cparse <>(clex src))
   (ty1 src)  <<><<><<>(cast (+ src " int main(void){return 0;}"))
   (assert
      (= (ty1 "unsigned long g;") 'ulong)
      (= (ty1 "unsigned char b;") 'uchar)
      (= (ty1 "unsigned short h;") 'ushort)
      (= (ty1 "unsigned u;")      'uint)
      (= (ty1 "signed char c;")   'char)     ; signed folds away
      (= (ty1 "long long q;")     'long)     ; long long -> long
      (= (ty1 "int i;")           'int)))

; -- 7c-iii: the C-feature tail cc needs to read love.c -- function-type typedefs,
; multi-declarator typedefs, sizeof EXPR (a node gen sizes from the type), a bare
; void return, a folded case label, the function-pointer cast type, an incomplete
; array. the object-vs-function macro distinction rides in the cpp section above.
(: (cast src) <>(cparse <>(clex src))
   (cexp src) (: ast (cast (+ "int main() { return " (+ src "; }")))
                 fn <<>ast
                 items <><>>>fn
                 <><items)
   (assert
      (= (cast "void f(void){return;}") '(prog ((fn "f" () (blk ((ret ())))))))   ; bare void return
      (= (cexp "sizeof x") '(szof (var "x")))                                       ; sizeof EXPR -> a node (not folded)
      (= (cexp "(int (*)(int)) p") '(cast (ptr (fn int)) (var "p")))                ; a function-pointer cast type (return rides)
      (= (cexp "sizeof(int)") '(num 4))                                             ; sizeof TYPE folds to a num literal
      (= (cast "typedef long a, b; int main(void){ b x; return x; }")                ; a multi-declarator typedef binds BOTH names as `long`
         '(prog ((tdef) (fn "main" () (blk ((decl (("x" long ()))) (ret (var "x"))))))))
      (= (cast "typedef int F(int); int main(void){return 0;}") '(prog ((tdef) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "extern int v[]; int main(void){return 0;}")                         ; an incomplete array: extern DECLARES, no storage
         '(prog ((xdecl (("v" (arr int 0) ()))) (fn "main" () (blk ((ret (num 0))))))))))

; -- 7c-iii part 2: multidimensional arrays. [3][4] of T reads as a RUN of bracket
; suffixes folded outer..inner into (arr (arr T 4) 3); a[i][j] desugars to the plain
; *(*(a+i)+j) -- gen does the type-directed work (the stride on a+i is the ROW size,
; and a deref whose type is still an array keeps the address). a global 2D table takes
; a nested-brace initializer. only the OUTERMOST dim may be [] (size from init).
(: (cast src) <>(cparse <>(clex src))
   (dty src)  (: ast (cast (+ src " int main(void){return 0;}"))    ; the first global decl's type
                 <><<><<>ast)
   (assert
      (= (dty "int a[3][4];") '(arr (arr int 4) 3))                 ; row-major nesting, outer dim first
      (= (dty "char m[2][3][4];") '(arr (arr (arr char 4) 3) 2))    ; three dims fold right
      (= (dty "int a[5];") '(arr int 5))                            ; a lone dim is unchanged
      (= (dty "int g[][3] = {{1,2,3},{4,5,6}};") '(arr (arr int 3) 2))   ; outer [] inferred from the init count
      ; brace elision (C 6.7.9): a flat run fills nested rows -- the outer [] divides by
      ; the row's scalar-leaf count (int[2]=2, int[2][3]=6), not the raw item count
      (= (dty "int a[][2] = {1,2,3,4};") '(arr (arr int 2) 2))          ; elided: 4 items / 2 = 2 rows
      (= (dty "int a[][2] = {1,2,3,4,5,6};") '(arr (arr int 2) 3))      ; ... /2 = 3 rows
      (= (dty "int a[][2][3] = {1,2,3,4,5,6,7,8,9,10,11,12};") '(arr (arr (arr int 3) 2) 2))  ; 12 / 6 = 2 outer
      ; a PARENTHESIZED declarator (*p) then a trailing suffix: [dims] -> pointer to ARRAY,
      ; (params) -> pointer to FUNCTION, nothing -> a plain pointer. an inner [n] on the name
      ; makes it an ARRAY of those pointers. (the type the suffix binds outward around the *.)
      (= (dty "int (*p)[3];") '(ptr (arr int 3)))                   ; pointer to array[3] of int
      (= (dty "int (*p)[2][3];") '(ptr (arr (arr int 3) 2)))        ; pointer to a 2D array
      (= (dty "int (*p[2])[3];") '(arr (ptr (arr int 3)) 2))        ; array[2] of pointer-to-array
      (= (dty "int (*p)(void);") '(ptr (fn int)))                   ; pointer to function (unregressed)
      (= (dty "int (*fp[4])(void);") '(arr (ptr (fn int)) 4))       ; dispatch table (unregressed)
      (= (cast "enum q { A, B, KN }; int m[KN][KN]; int main(void){return 0;}")   ; an ENUM CONSTANT dim folds (KN = 2)
         '(prog ((tdef) (gdecl (("m" (arr (arr int 2) 2) ()))) (fn "main" () (blk ((ret (num 0))))))))))

; -- 7c-iii part 3: the last of love.c's parse tail. a hex FLOAT literal (0x1.0p-53,
; a base-2 exponent after `p`). an array dimension is any integer CONSTANT EXPRESSION
; (2*3, 64/limb_bits) parsed by pexpr + cfold -- subsuming the enum-constant dim. a
; DESIGNATED array initializer `[i]=v` takes a const-expr (enum) index. and BLOCK-SCOPED
; typedef shadowing: a local variable whose name is a typedef (love.h makes `num`/`word`
; typedefs) hides it for the rest of the block, so `num + num` reads as an expression
; (not a cast) -- restored at block exit so a sibling function still sees the type.
(: (cast src) <>(cparse <>(clex src))
   (cexp src) (: ast (cast (+ "int main() { return " (+ src "; }")))
                 fn <<>ast items <><>>>fn <><items)
   (dty src)  (: ast (cast (+ src " int main(void){return 0;}")) <><<><<>ast)
   (assert
      (= (cexp "0x1p4") '(flo 16.0))                               ; 0x1p4 = 1 * 2^4
      (= (cexp "0xff") '(num 255))                                 ; a plain hex integer is unchanged
      (= (dty "int m[2 * 3];") '(arr int 6))                       ; a const-EXPRESSION dimension folds
      (= (cast "enum e { A, B, C }; int t[3] = { [A]=10, [C]=30 }; int main(void){return 0;}")   ; designated init, enum index
         '(prog ((tdef) (gdecl (("t" (arr int 3) (init ((didx 0 (num 10)) (didx 2 (num 30))))))) (fn "main" () (blk ((ret (num 0))))))))
      (= (cast "typedef long num; int main(void){ long num = 1; return (int)(num + num); }")   ; a local shadows the `num` typedef
         '(prog ((tdef) (fn "main" () (blk ((decl (("num" long (num 1)))) (ret (cast int (bin + (var "num") (var "num"))))))))))))

; -- block-scoped ENUM-CONSTANT shadowing, the typedef shadow's sibling: a local
; named after an enum constant is a VARIABLE for the rest of the block (not a
; folded number -- which read wrong and refused as an lvalue), and the constant
; comes back at block exit.
(: (cast src) <>(cparse <>(clex src))
   (assert
      (= (cast "enum { K = 5 }; int main(void){ int K = 1; K *= 2; return K; }")
         '(prog ((tdef) (fn "main" () (blk ((decl (("K" int (num 1))))
                                            (expr (asn (var "K") (bin * (var "K") (num 2))))
                                            (ret (var "K"))))))))
      (= (cast "enum { K = 5 }; int f(void){ { int K = 1; } return K; }")   ; restored at `}`
         '(prog ((tdef) (fn "f" () (blk ((blk ((decl (("K" int (num 1))))))
                                         (ret (num 5))))))))))

; -- struct layout: a multi-declarator member line lays its names IN ORDER
; (`double re, im;` is re at 0, im at 8 -- a double rev once flipped it, an ABI
; break against gcc that only bit multi-declarator lines). and the by-value
; struct rep: a <=16-byte struct rides calls by its SysV eightbyte classes -- one
; eightbyte in a gp/xmm reg, two eightbytes (int/sse in any mix) in a register
; pair; returned, passed, copied. a >16-byte one is MEMORY class: it rides the
; overflow block whole, and returns through the caller's hidden pointer.
(: (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (stg src) (: r (cparse <>(clex src)) (peep <>>r "S" ()))
   (assert
      (= (stg "struct S { double re, im; }; int main(){return 0;}")
         '((("re" 0 double) ("im" 8 double)) 16 8))
      (= (stg "struct S { char a, b; int c, d; }; int main(){return 0;}")
         '((("a" 0 char) ("b" 1 char) ("c" 4 int) ("d" 8 int)) 12 4))
      (two? (cgn "struct S { double re, im; }; static struct S mk(double a, double b){ struct S z = {a,b}; return z; } static double re(struct S z){ return z.re; } int main(){ return (int)re(mk(41.0, 1.0)) + 1; }"))
      (two? (cgn "struct T { int x, y, z; } t; struct T id(struct T a); int main(){ return id(t).x; }"))   ; a 9..16B two-eightbyte struct (int:int) now rides calls
      (two? (cgn "struct B { long a, b, c; } b; long f(struct B x); int main(){ return (int)f(b); }"))       ; a >16B MEMORY struct passes on the stack
      (two? (cgn "struct B { long a, b, c; }; struct B mk(long k); struct B g(long k){ return mk(k); } int main(){ return (int)g(1).a; }"))   ; ..and returns through the hidden pointer
      (two? (cgn "struct B { long a, b, c; } b; long f(int q, struct B x, int r); int main(){ return (int)f(1, b, 2); }"))))   ; ..behind register args

; -- the gen tail: every name a comma-list prototype registers is callable (the
; sig sweep pins mproto siblings, which have no proto form of their own), and &f
; is a function's address exactly like the bare use -- as a value in code and as
; a label in a global image. an undeclared &name still refuses.
(: (cgn src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (assert
      (two? (cgn "int one(int), two(int); int one(int x){return x+1;} int two(int x){return x+2;} int main(){return one(1)+two(2);}"))
      (two? (cgn "int one(int), two(int); int one(int x){return x+1;} int main(){ int (*p)(int) = two; return p != 0; }"))
      (two? (cgn "int f(int x){return x;} int main(){ int (*p)(int) = &f; return p(1); }"))
      (two? (cgn "int f(int x){return x;} typedef int (*fp)(int); fp t[1] = { &f }; int main(){ return t[0](1); }"))
      (refused? (cgn "int main(){ return (long)&nope; }"))))

; -- inline asm (the statement): the template is holo neutral TEXT, %N the
; operands (outputs first), adjacent template strings concatenate, and the
; body assembles at cg time into ONE opaque ('raw ..) between the staging
; forms. the picks come off asmpick front-first (r10 the first), so the
; one-output golden's bytes are exactly (li r10 40) assembled. constraint
; refusals: a letter outside r/i/rN, a duplicate pin, x64's frame r4, a
; float operand -- each () out of cgen, never fudged bytes.
(: (cast src) <>(cparse <>(clex src))
   (cgn src) (: r (cparse <>(clex src))
               (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   hb (from 'holo 'holo-bytes)
   g1 (cgn "int main() { long v; asm(\"li %0, 40\" : \"=r\"(v)); return v + 2; }")
   want (hb 'x64 (asm-text "li r10, 40"))
   (hasraw fs) (? !(two? fs) ()
                  (: f2 <fs
                     (? (two? f2 && (= <f2 'raw) && (= <>f2 want)) 1 (hasraw >fs))))
   g2 (cgn "void o(unsigned char v, unsigned short p) { asm volatile(\"outb\" : : \"r0\"(v), \"r2\"(p)); } int main() { o(1, 2); return 0; }")
   want2 (hb 'x64 (asm-text "outb"))
   (hasraw2 fs) (? !(two? fs) ()
                   (: f2 <fs
                      (? (two? f2 && (= <f2 'raw) && (= <>f2 want2)) 1 (hasraw2 >fs))))
   (assert
      (= (cast "void f() { asm(\"sys\"); }")
         '(prog ((fn "f" () (blk ((asm "sys" () () ())))))))
      ; every GNU spelling of the head and of volatile reads the SAME statement. the
      ; underscored ones are also patt's decoration names, and patt runs first -- so
      ; each of these once lost its head and refused on the stranded `volatile`.
      (= (cast "void f() { __asm__ volatile(\"sys\"); }")
         (cast "void f() { asm(\"sys\"); }"))
      (= (cast "void f() { __asm volatile(\"sys\"); }")
         (cast "void f() { asm(\"sys\"); }"))
      (= (cast "void f() { __asm__ __volatile__(\"sys\"); }")
         (cast "void f() { asm(\"sys\"); }"))
      (= (cast "void f() { __asm(\"sys\"); }")
         (cast "void f() { asm(\"sys\"); }"))
      (= (cast "void f() { long v; asm volatile(\"li %0, 4\" \"0\" : \"=r\"(v) : \"r\"(v), \"i\"(3) : \"memory\"); }")
         '(prog ((fn "f" () (blk ((decl (("v" long ())))
                                  (asm "li %0, 40" (("=r" (var "v")))
                                       (("r" (var "v")) ("i" (num 3)))
                                       ("memory"))))))))
      (two? g1) (hasraw <>g1)
      ; the PORT I/O ops (musl's bits/io.h rides them): register-contracted
      ; like `sys`, so the template is bare and the operands only pin -- the
      ; assembled body is exactly the opcode bytes
      (two? g2) (hasraw2 <>g2)
      (refused? (cgn "int main() { long v; asm(\"li %0, 1\" : \"=q\"(v)); return 0; }"))
      (refused? (cgn "int main() { asm(\"sys\" : : \"r0\"(1), \"r0\"(2)); return 0; }"))
      (refused? (cgn "int main() { asm(\"sys\" : : \"r4\"(1)); return 0; }"))
      (refused? (cgn "int main() { double d; asm(\"sys\" : : \"r\"(d)); return 0; }"))))

; -- the alive pass (per-call wrap filtering): a non-leaf fn's homed params
; spill/reload ONLY around calls they are live across. `wraps` counts stores
; of home-pool registers to the frame (the spill side): the summed-then-dead
; pair takes ZERO wraps, the read-after-the-call pair keeps its two; a param
; SHADOWED by an inner local conflates two vars under one name, so it deopts
; to the blanket (its spill survives the call it is dead across -- and the
; unshadowed sibling still drops its own, per-var within ONE call); a goto
; reads the whole universe (the escape edge), so its fn keeps both.
(: (cgn src) (: r (cparse <>(clex src))
               (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   ; counts stores of the PARAM-home registers to the frame (the spill side).
   ; the set is exactly the two-param homes (r6 r5): the vmap's direct slot
   ; stores ride the OPERAND pool (r9 r10 r7 r8) and are not spills. the base
   ; is r4 -- or sp, once frame elision (calls ride it now) re-addressed the
   ; slots; these fns carry no spush cells, so an sp store IS a wrap.
   (wraps fs) (: (go l n) (? !(two? l) n
                             (: f2 <l
                                (go >l (? (two? f2 &&
                                           (<f2 = 'st) &&
                                           member? <>f2 '(r4 sp) &&
                                           member? <>>>f2 '(r5 r6))
                                          (n + 1) n))))
                 (go fs 0))
   ; the fn's OWN forms, label to the next align seam: the inliner copies the body into main,
   ; where a folded store may read a home register legitimately -- main's copy is not a wrap
   (fnf nm fs) (: (seek l) (? !(two? l) ()
                              (= <l `('label nm)) >l
                              (seek >l))
                  (take l) (? !(two? l) ()
                              (= <l '(align 2)) ()
                              (<l >< take >l))
                  (take (seek fs)))
   pre9 "long g(long x); "
   irD (cgn (pre9 + "long f(long a, long b) { long s = a+b+a*b+a-b; g(1); return s+1; } int main() { return f(20, 22); }"))
   irL (cgn (pre9 + "long h(long a, long b) { long s = a+b+a*b; g(1); return s+a-b+a*b; } int main() { return h(20, 22); }"))
   irS (cgn (pre9 + "long q(long a, long b) { long s = a+b+a*b+a-b; g(1); { long a = s; s = a+1; } return s+2; } int main() { return q(20, 22); }"))
   irG (cgn (pre9 + "long w(long a, long b) { long s = a+b+a*b+a-b; g(1); if (s > 9) goto out; s = s+1; out: return s+1; } int main() { return w(20, 22); }"))
   _ (assert
      (two? irD) (two? irL) (two? irS) (two? irG)
      (= 0 (wraps (fnf 'f <>irD)))                   ; dead across the call: no spills at all
      (= 2 (wraps (fnf 'h <>irL)))                   ; live across: both homes keep their wrap
      (= 1 (wraps (fnf 'q <>irS)))                   ; the shadowed a's blanket; b's own still drops
      (member? '(st sp 32 r6) (fnf 'q <>irS))        ; ...and it is a's (r6, the first gp home; q elides, slots speak sp)
      (= 2 (wraps (fnf 'w <>irG))))                  ; a goto fn wraps everything
   ; -- callee-saved locals (the cs lane): a local touched under NESTED loops (depth >= 2)
   ; homes on r11..r14 (machine r12-r15) even while it CROSSES calls -- the callee preserves
   ; the register, so the home takes no wrap; the price is one save/restore pair in the
   ; owner's frame, and only compounding iteration counts repay it. here s and the inner j
   ; ride r11/r12 through g(j) unwrapped; the depth-1 i (and irS's loop-free s above) stay
   ; slotted -- their touches never earn the pair.
   irQ (cgn (pre9 + "long ql(long a, long n) { long s = 0; long i = 0; while (i < n) { long j = 0; while (j < n) { s = s + g(j) + a; j = j + 1; } i = i + 1; } return s; } int main() { return ql(3, 4); }"))
   qf (fnf 'ql <>irQ)
   (occ f2) (: (go l n) (? !(two? l) n (go >l (? (= <l f2) (n + 1) n))) (go qf 0))
   _ (assert
      (two? irQ)
      (= 1 (occ '(st sp 56 r11)))                    ; s's save, ONCE (prologue) -- the calls never wrap it
      (= 1 (occ '(ld r11 sp 56)))                    ; s's restore, ONCE (epilogue)
      (= 1 (occ '(st sp 64 r12)))                    ; j's pair likewise
      (= 1 (occ '(ld r12 sp 64)))
      (member? '(add r12 r12 1) qf)                  ; the inner step is register-only
      (member? '(st sp 24 r9) qf))
   ; -- slot-load elision: loadval honors the want hint (a decl/asn init from a member/deref
   ; lands straight in the hinted register, so the vmap pin fires and re-reads are zero forms),
   ; and stld rides a whole straight-line run (a bound slot's reload forwards from the register
   ; until a redef, an overlapping store, a label, or a bar).
   irE (cgn (pre9 + "long f2(long *p) { long a = p[0]; if (a & 1) return a; return g(a); } int main() { return f2((long*)0); }"))
   irF (cgn (pre9 + "long q2(long *p) { long a = p[0]; g(a); return a + (a >> 3); } int main() { return q2((long*)0); }"))
   (splds fs) (: (go l n) (? !(two? l) n
                            (go >l (? (two? <l && id? <<l 'ld && id? <>><l 'sp) (n + 1) n)))
                 (go fs 0))
   _ (assert
      (two? irE) (two? irF)
      ; the pinned deref: a lands in r5 and rides it end to end -- slot-free, frameless
      (member? '(ld r5 r6 0) (fnf 'f2 <>irE))
      (member? '(test r5 1) (fnf 'f2 <>irE))
      (= 0 (splds (fnf 'f2 <>irE)))
      ; a crosses the call (one wrap reload), then the second read FORWARDS: one sp load total
      (= 1 (splds (fnf 'q2 <>irF))))
   ; -- the wide pair park: a d128 binop's LEFT rides two pool registers and the combine
   ; reads the r-twins (a128r/mw128r/..) -- the machine-stack cell never opens. a callish
   ; right keeps the cell: it must hold the pair THROUGH the call.
   irW (cgn (pre9 + "unsigned long w(unsigned long a, unsigned long b, unsigned long c) { unsigned __int128 s = (unsigned __int128) a * b + c; return (unsigned long)(s >> 64) ^ (unsigned long) s; } int main() { return (int) w(3,4,5); }"))
   irX (cgn (pre9 + "unsigned long v(unsigned long a) { unsigned __int128 s = (unsigned __int128) a * a + (unsigned __int128) g(1); return (unsigned long) s; } int main() { return (int) v(3); }"))
   (occh l h9) (: (go l2 n) (? !(two? l2) n (go >l2 (? (two? <l2 && id? <<l2 h9) (n + 1) n))) (go l 0))
   _ (assert
      (two? irW) (two? irX)
      (= 1 (occh (fnf 'w <>irW) 'mw128r))            ; the widening mul reads its parked register
      (= 1 (occh (fnf 'w <>irW) 'a128r))             ; the add combines through the pair
      (= 0 (occh (fnf 'w <>irW) 'mw128))             ; the cell forms are gone from this shape
      (= 0 (occh (fnf 'w <>irW) 'a128))
      (= 1 (occh (fnf 'v <>irX) 'mw128r))            ; the mul's right is callish-free: parked
      (= 1 (occh (fnf 'v <>irX) 'a128)))             ; the add's right CALLS: the cell holds through it
   ; -- the leaf cs overflow: a call-free fn whose param homes drank the operand pool
   ; (five homable params here) overflows its depth>=2 locals onto the cs pool -- the
   ; save/restore pair rides the leaf lane's regen too (the ride regen passes the cs regs
   ; through), and the loop nest runs register-only.
   irY (cgn "long lf(long *r, long *a, long na, long *b, long nb) { long s = 0; for (long i = 0; i < na; i++) { for (long j = 0; j < nb; j++) { s = s + a[i] * b[j] + r[i+j]; } } return s; } int main() { return 0; }")
   yf (fnf 'lf <>irY)
   _ (assert
      (two? irY)
      (member? '(st sp 32 r11) yf)                   ; s saves (prologue), homes r11
      (member? '(ld r11 sp 32) yf)                   ; ...and restores (epilogue)
      (member? '(add r12 r12 1) yf)                  ; i steps on its home, register-only
      (member? '(add r13 r13 1) yf)                  ; j likewise
      (= 3 (splds yf)))                              ; the ONLY sp loads are the three restores
   ; -- canonical int homing: an int/uint LOCAL homes (lhomable?), and the home always holds
   ; the EXTENDED value -- every def re-narrows (sx4/zx4), so reads are zero forms and 32-bit
   ; wrap holds. an int PARAM homes only by RIDING its arrival (one entry cvt, no pool
   ; register): here na rides r2; nb's ride is dropped by the soundness re-read (r7 turns
   ; scratch) and it keeps its SLOT -- never a pool home.
   irZ (cgn "long g2(long *r, long *a, int na, long *b, int nb) { long s = 0; for (int i = 0; i < na; i++) { for (int j = 0; j < nb; j++) { s = s + a[i] * b[j]; } } return s; } int main() { return 0; }")
   zf (fnf 'g2 <>irZ)
   _ (assert
      (two? irZ)
      (member? '(sx4 r2) zf)                         ; na rides its arrival, canonicalized once
      (member? '(cmp r11 r2) zf)                     ; ...and the bound compare reads it register-register
      (member? '(st sp 32 r7) zf)                    ; nb's dropped ride lands in its slot
      (member? '(sx4 r11) zf)                        ; i's defs re-narrow: the home stays canonical
      (member? '(add r11 r11 1) zf)                  ; i on its cs home
      (= 2 (splds zf)))                              ; full-word sp loads: only the two cs restores
   ; the guard law: with a call remaining there is no ride, so an int param KEEPS ITS SLOT
   ; (a pool home would starve the locals and pay wraps -- the measured anti-lever)
   irZp (cgn "long ext(long); long h3(int n, long *a) { long s = 0; for (int i = 0; i < n; i++) { s = s + ext(a[i]) + n; } return s; } int main() { return 0; }")
   zpf (fnf 'h3 <>irZp)
   _ (assert
      (two? irZp)
      (member? '(st sp 48 r6) zpf)                   ; n spills at entry...
      (member? '(ld4 r0 sp 48) zpf))                 ; ...and every read re-extends from the slot
   ; -- priced int-param homes (the leaf lane): an int param whose slot touches pay the
   ; entry cvt takes its POSITIONAL pool seat -- after the locals pick, and only while the
   ; pool stays deep (>= 2 free after the take). f4: n homes its own arrival seat (one sx4,
   ; no mov, the slot never opens -- the fn runs bare); f5: four pointer homes + the picked
   ; locals leave the pool at two, so the hot n keeps its slot (the mag_mul discipline:
   ; the unpriced version drank the pool and reopened the d128 stack cell); f6: a read-free
   ; param earns nothing.
   irIH (cgn "long f4(long *a, int n) { return a[n] + a[0]; } int main() { return 0; }")
   ihf (fnf 'f4 <>irIH)
   irIP (cgn "long f5(long *a, long *b, long *c, long *d, int n) { long s = 0; for (int i = 0; i < n; i++) { s = s + a[i] + b[i] + c[i] + d[i]; } return s; } int main() { return 0; }")
   ipf (fnf 'f5 <>irIP)
   irID (cgn "long f6(long *a, int n) { return a[0]; } int main() { return 0; }")
   idf (fnf 'f6 <>irID)
   _ (assert
      (two? irIH) (two? irIP) (two? irID)
      (member? '(sx4 r5) ihf)                        ; n canonicalizes ON its seat (home = arrival)...
      (member? '(ldx r0 r6 r5 8 0) ihf)              ; ...and indexes register-resident
      (= 0 (splds ihf))                              ; no slot traffic at all: f4 runs bare
      (member? '(st sp 16 r7) ipf)                   ; f5: n spills -- the pool ends at two...
      (member? '(ld4 r0 sp 16) ipf)                  ; ...and the loop re-reads the slot
      !(member? '(sx4 r5) idf))                      ; a read-free param earns nothing
   ; -- d128 LOCALS home as CS PAIRS: a loop-carried __int128 local takes (lo hi) off the
   ; cs pool (callee-saved: call-safe, no wraps, and no operand-pool pressure -- the wpk
   ; park's registers stay free). reads/writes are mov pairs on the r0:r2 protocol; the
   ; slot never opens. &-taken and ++ shapes deopt through clval.
   irV (cgn "long w2(long *a, int n) { unsigned __int128 c = 0; for (int i = 0; i < n; i++) { for (int k = 0; k < 2; k++) { c = c + (unsigned __int128) a[i] * 3; a[i] = (long) c; } } return (long)(c >> 64); } int main() { return 0; }")
   vf (fnf 'w2 <>irV)
   (assert
      (two? irV)
      (member? '(mov r11 r0) vf)                     ; c's write lands its lo home...
      (member? '(mov r12 r2) vf)                     ; ...and its hi
      (member? '(a128r r10 r7) vf)                   ; the combine reads the parked twins
      (= 2 (splds vf))))                             ; full-word sp loads: only the two cs restores

; -- the callee-saved contract (cskeep): every cgn above already compiles under it --
; a breach REFUSES at emission -- so these laws probe the checker itself. the dirty
; walk is path-aware: a written reg is dirty until its restore, labels join by union,
; and every exit must leave the set empty -- so an early exit whose path never wrote
; (the stld-folded restore) passes, and the cs9 shape (written bare) scares. the
; breach probes ride a counting help; the checker RESUMES through it, so one run
; logs every scare it meets.
(: ck9 (tablet 0) _ (pin ck9 'tgt 'x64)
   kf1 '((label f9) (push r4) (mov r4 sp) (sub sp sp 32) (st r4 -8 r3) (st r4 -16 r11)
         (mov r11 r6) (mov r3 r11) (ld r3 r4 -8) (ld r11 r4 -16) (mov sp r4) (pop r4) (ret))
   kf2 '((label f9) (sub sp sp 16) (st sp 8 r12)                ; the early exit's path never
         (cmp r6 r5) (br lt .k9) (add sp sp 16) (ret)           ; writes r12: its folded-away
         (label .k9) (mov r12 r6) (mov r0 r12) (ld r12 sp 8) (add sp sp 16) (ret))   ; restore is ADMITTED
   kf3 '((label f9) (sub sp sp 16) (st sp 8 r13) (li r13 0)     ; the back edge keeps r13 dirty
         (label .c9) (add r13 r13 1) (cmp r13 r5) (br lt .c9)   ; around the loop; the restore
         (mov r0 r13) (ld r13 sp 8) (add sp sp 16) (ret))       ; cleanses before the one exit
   _ (assert
      (= kf1 (cskeep ck9 kf1)) (= kf2 (cskeep ck9 kf2)) (= kf3 (cskeep ck9 kf3))
      (= '(r12) (csdefs '(mov r12 r0)))
      (= '(r11) (csdefs '(ld r11 sp 80)))
      (= '(r12) (csdefs '(div r12 r0 r5)))           ; idiv writes r0 + its dst, never r3 (self-saved r2)
      !(two? (csdefs '(a128r r11 r12)))              ; the d128 family only speaks r0:r2
      !(two? (csdefs '(st sp 80 r11)))               ; a save writes memory, not the register
      (id? 'dark (csdefs '(frob r0))))               ; an op on neither roster is dark -> refuses
   klog (tablet 0)
   _ (hear (\ p e (: _ (pin klog p (+ 1 (peep klog p 0))) 0)))
   _ (cskeep ck9 '((label f9) (mov r12 r6) (ret)))                 ; written bare: the cs9 shape
   _ (cskeep ck9 '((label f9) (sub sp sp 16) (st sp 8 r11) (mov r11 r6)
                   (cmp r6 r5) (br lt .k9) (ret)                   ; one path exits dirty
                   (label .k9) (ld r11 sp 8) (ret)))
   _ (cskeep ck9 '((label f9) (sub sp sp 16) (st sp 8 r11) (mov r11 r6) (jmp helper9)))   ; a sibcall is an exit too
   _ (cskeep ck9 '((label f9) (frob r0) (ret)))
   _ (cskeep ck9 '((label f9) (sub sp sp 16) (mov r11 r6) (st sp 8 r11) (ld r11 sp 8) (ret)))
   _ (hear ())
   (assert
      (= 1 (peep klog 'cs-unsaved 0))
      (= 3 (peep klog 'cs-unrestored 0))             ; the bare write's exit + the dirty branch + the sibcall
      (= 1 (peep klog 'cs-dark-op 0))
      (= 1 (peep klog 'cs-save-late 0))))            ; a save AFTER the write stores the clobber

; -- the musttail contract (sibs): a marked ret-position call -- (call F must) -- leaves as
; the tail jmp and the marker dies with the rewrite; a shape that cannot tail SCARES
; (musttail-not-a-tail), and a fn sibcall declines whole (fesc) scares musttail-escape.
; the breach probes ride the counting help, like the cs breaches above.
(: mg9 (tablet 0) _ (pin mg9 'tgt 'x64) _ (pin mg9 'fn "f9")
   mepi '((ld r3 r4 -8) (mov sp r4) (pop r4) (ret))
   _ (assert
      (member? '(jmp g9) (sibcall mg9 ('((label f9) (call g9 must)) + mepi)))
      !(member? '(call g9 must) (sibcall mg9 ('((label f9) (call g9 must)) + mepi))))
   mlog (tablet 0)
   _ (hear (\ p e (: _ (pin mlog p (+ 1 (peep mlog p 0))) 0)))
   _ (sibcall mg9 ('((label f9) (call g9 must) (mov r0 r5)) + mepi))   ; a live insn after the call: no tail
   _ (: t9 (tablet 0) _ (pin t9 'tgt 'x64) _ (pin t9 'fesc 1) _ (pin t9 'fn "f9")
        (sibcall t9 '((label f9) (call g9 must) (ret))))               ; the whole-fn decline
   _ (hear ())
   (assert
      (= 1 (peep mlog 'musttail-not-a-tail 0))
      (= 1 (peep mlog 'musttail-escape 0))))

; -- free locals (zero-crossing, stage B1): a local never live across any
; call homes even in a CALL-HAVING fn -- the call-then-hot-loop shape runs
; its loop body all-register. s rides r5 (hpool front past the reserve) and
; i r9; the loop's imul/adds are 3-address on the homes, no slot traffic.
(: (cgn src) (: r (cparse <>(clex src))
               (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   irB (cgn "long g(long x); long f(long n) { g(n); long s = 0; long i = 0; while (i < n) { s = s + i*i; i = i + 1; } return s; } int main() { return f(4); }")
   (assert
      (two? irB)
      (member? '(imul r10 r9 r9) <>irB)               ; i*i on the home, 3-address
      (member? '(add r5 r5 r10) <>irB)                ; s accumulates in its home
      (member? '(add r9 r9 1) <>irB)))                ; i++ is ONE add on the home

; -- arm64 homing (stage D): the same alive machinery rides AAPCS64. homes come
; off r10..r14 (caller-saved, never an argument register -- every home takes a
; prologue mov), wraps say fp (a4ize), an overflow param (the 9th long) reads
; at fp+32 -- ABOVE the lr+fp push cells (the x64 base 16 was stage D's first
; bug), and a name BORN in a call-bearing statement joins that statement's
; record (m = g(a), n = g(b): m crosses the second call INSIDE one tick --
; statement-grain liveness can't order intra-statement events, so the kill
; rides the wrapset and such a local keeps its frame slot; stage D's second
; bug, x64 only masked it because the folds erase intra-statement slot traffic
; from the ranking).
(: (cgna src) (: r (cparse <>(clex src))
                (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'arm64) ()))
   ; the two-param homes only (r10 r11) -- the vmap's direct slot stores ride
   ; the remaining pool (r12 r13 r14) and are not spills (wraps' twin above)
   (wrapsa fs) (: (go l n) (? !(two? l) n
                              (: f2 <l
                                 (go >l (? (two? f2 &&
                                            (<f2 = 'st) &&
                                            (<>f2 = 'fp) &&
                                            member? <>>>f2 '(r10 r11))
                                           (n + 1) n))))
                  (go fs 0))
   (ldat fs b k) (? !(two? fs) ()
                    (: f2 <fs
                       (? (two? f2 && (<f2 = 'ld) && (<>>f2 = b) && (<>>>f2 = k)) 1
                          (ldat >fs b k))))
   (hmovs fs) (: (go l n) (? !(two? l) n
                             (: f2 <l
                                (go >l (? (two? f2 &&
                                           (<f2 = 'mov) &&
                                           member? <>f2 '(r5 r6 r7 r8 r9))
                                          (n + 1) n))))
                 (go fs 0))
   pre9 "long g(long x); "
   irDa (cgna (pre9 + "long f(long a, long b) { long s = a+b+a*b+a-b; g(1); return s+1; } int main() { return f(20, 22); }"))
   irLa (cgna (pre9 + "long h(long a, long b) { long s = a+b+a*b; g(1); return s+a-b+a*b; } int main() { return h(20, 22); }"))
   irOa (cgna "static long f9(long a, long b, long c, long d, long e, long f, long g2, long h, long i) { return a+h+i; } long q(long a) { return f9(a,2,3,4,5,6,7,8,9) - 17; } int main() { return (int)(q(1) + 24); }")
   irKa (cgna (pre9 + "long k(long a, long b) { long m = g(a), n = g(b), p = m*n; return p; } int main() { return (int)(k(2,3) - 2); }"))
   irBa (cgna "long g(long x); long p(long n) { g(1); long s = 0; long i; for (i = 0; i < n; i++) s = s + i*i; return s; } int main() { return (int)(p(4) + 28); }")
   (assert
      (two? irDa) (two? irLa) (two? irOa) (two? irKa) (two? irBa)
      (member? '(mov r10 r0) <>irDa)                 ; a homes off the pool, one prologue mov
      (member? '(mov r11 r1) <>irDa)
      (= 0 (wrapsa <>irDa))                          ; dead across the call: no spills
      (= 2 (wrapsa <>irLa))                          ; live across: both homes wrap
      ; f9 is a LEAF, so the frame elision re-addressed it off sp (K=32,
      ; K9=64): the 9th long reads above the whole reservation, where the
      ; caller stack sits -- fp+32 through the off+K map, never x64's +16
      (ldat <>irOa 'sp 64)
      !(ldat <>irOa 'sp 48)
      !(ldat <>irOa 'fp 32)                          ; no fp read survives in a frameless leaf
      (= 0 (hmovs <>irKa))                           ; born-at-a-call locals keep their slots
      (member? '(mov r6 r0) <>irBa)                  ; the free local s homes...
      (member? '(add r6 r6 r9) <>irBa)               ; ...and accumulates 3-address (i*i in r9)
      (member? '(add r5 r5 1) <>irBa)                ; the loop counter i ALSO homes (r5): its
      (= 0 (wrapsa <>irBa))))                        ; ++ no longer leans its own slot. zero wraps

; -- register-bound branch splices: a call-free cbinl predicate reads its args from
; REGISTERS. a var arg over a pinned/homed local ALIASES that register (zero forms);
; an expression arg PARKS in a pool reg -- only while the pool stays deep (the shallow
; park is the measured displacement anti-lever). the loop body runs slot-free.
(: (cgn src) (: r (cparse <>(clex src))
               (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (fnf nm fs) (: (seek l) (? !(two? l) () (? (= <l `('label nm)) >l (seek >l)))
                  (take l) (? !(two? l) ()
                              (= <l '(align 2)) ()
                              (<l >< take >l))
                  (take (seek fs)))
   (splds fs) (: (go l n) (? !(two? l) n
                            (go >l (? (two? <l && id? <<l 'ld && id? <>><l 'sp) (n + 1) n)))
                 (go fs 0))
   pre9 "static long inr(long x, long n) { return x >= 2 && x <= n; }\n"
   irU (cgn (pre9 + "long wq(long a, long n) { long s = 0; long i = 0; while (i < n) { if (inr(a, n)) s = s + a; if (inr(s, a)) s = s + 1; i = i + 1; } return s; } int main() { return wq(3, 5); }"))
   uf (fnf 'wq <>irU)
   _ (assert
      (two? irU)
      (member? '(cmp r6 2) uf)                       ; x aliases a's ride: the compare reads r6
      (member? '(cmp r6 r5) uf)                      ; ...and n's ride r5 -- no temp anywhere
      (member? '(cmp r10 2) uf)                      ; the second splice aliases s's home
      (member? '(cmp r10 r6) uf)
      (= 0 (splds uf)))                              ; the whole fn runs slot-free
   irP (cgn (pre9 + "long pk(long a, long n) { long s = 0; for (long i = 0; i < n; i++) { if (inr(a + i, n)) s += i; } return s; } int main() { return pk(1, 6); }"))
   pf (fnf 'pk <>irP)
   _ (assert
      (two? irP)
      (member? '(add r7 r6 r10) pf)                  ; the expression arg lands its park r7...
      (member? '(cmp r7 r5) pf)                      ; ...and the body compares park vs alias
      (= 0 (splds pf)))
   ; the VALUE splice aliases too (its multi-return body reads lo straight from n's ride);
   ; its low-read expression arg stays a slot -- a value-splice park spans the whole body
   ; (the measured anti-lever), so it must EARN the seat: weighted body reads >= 8
   irQ2 (cgn "static long clamp9(long x, long lo) { if (x < lo) return lo; if (x > 99) return 99; return x; }\nlong vq(long a, long n) { long s = 0; for (long i = 0; i < n; i++) { s = s + clamp9(a + i, n); } return s; } int main() { return vq(1, 5); }")
   q2f (fnf 'vq <>irQ2)
   _ (assert
      (two? irQ2)
      (member? '(cmp r7 r5) q2f)                     ; x < lo compares against the ALIAS, no stage
      (= 0 (splds q2f)))
   ; ⚠ a body that ASSIGNS its param cannot alias: the register holds the CALLER's live
   ; value and the splice's own p++ would walk it. that param takes a slot (value splice) or
   ; a park (branch splice); a read-only sibling in the same splice still aliases.
   irW (cgn "static long walk9(long p, long lim) { for (;; p++) { if (p >= lim) return p; } }\nlong wv(long a, long n) { long s = 0; for (long i = 0; i < n; i++) { s = s + walk9(a, n); } return s; } int main() { return wv(1, 5); }")
   wf (fnf 'wv <>irW)
   _ (assert
      (two? irW)
      (member? '(st sp 16 r6) wf)                    ; the written param p spills off a's ride...
      !(member? '(add r6 r6 1) wf)                   ; ...and a's ride is never walked
      (member? '(cmp r7 r5) wf))                     ; lim, never written, still aliases n's ride
   irX (cgn "static int p9(long a, long b) { return (a += b) > 100; }\nlong run(long x, long y) { long s = 0; for (long i = 0; i < 4; i++) { if (p9(x, y)) s = s + 1; s = s + x; } return s; } int main() { return run(3, 5); }")
   xf (fnf 'run <>irX)
   _ (assert
      (two? irX)
      (member? '(mov r7 r6) xf)                      ; a += b parks off x's ride...
      !(member? '(add r6 r6 r5) xf)                  ; ...and x's ride never takes the add
      (member? '(add r7 r7 r5) xf))                  ; b, never written, still aliases y's ride
   ; a value-splice park that EARNS the seat: a read inside the body's loop weighs x8 (the
   ; slot bind would reload it every iteration), so the expression arg rides a pool reg
   ; for the splice's whole span and the loop runs off the register
   ; ...and the body's own DECLS earn seats the same way (regen attempts only): s binds a
   ; pool reg, so the loop's accumulate is register-register and only i still slots
   irG (cgn "static long sum9(long p, long n) { long s = 0; for (long i = 0; i < n; i++) { s = s + p; } return s; }\nlong gq(long a, long n) { return sum9(a + 1, n); }\nint main() { return gq(3, 5); }")
   gf (fnf 'gq <>irG)
   _ (assert
      (two? irG)
      (member? '(mov r10 r9) gf)                     ; a + 1 lands its park r10...
      (member? '(add r9 r9 r10) gf)                  ; ...and the loop reads it, no reload
      (member? '(mov r9 r0) gf)                      ; s's decl bind: init lands the register
      (= 1 (splds gf)))                              ; the one surviving slot read is i's
   ; ⚠ a park under the CALLER's loop resets from its source every iteration -- that mov
   ; wears the entry-home shape (mov H A), and unhome renaming it away erases the reset
   ; (strpbrk walked its set forward and never rewound; test_libc was the only catch).
   ; pears stops at the first body label, so the reset stands and the walk stays on the park.
   irH (cgn "static char *sch9(char const *s, int c) { for (;; s++) { if (*s == (char) c) return (char *) s; if (!*s) return 0; } }\nchar *pb9(char const *s, char const *set) {\n  for (; *s; s++) if (sch9(set, *s)) return (char *) s;\n  return 0; }\nint main() { char const *s = \"hello, world\"; return pb9(s, \"xyzw\") ? 7 : 255; }")
   hf (fnf 'pb9 <>irH)
   _ (assert
      (two? irH)
      (member? '(mov r9 r5) hf)                      ; the reset copies set anew each iteration...
      (member? '(add r9 r9 1) hf)                    ; ...the walk rides the park
      !(member? '(add r5 r5 1) hf))                  ; ...and the arrival never walks
   ; a splice whose one decl earns a seat runs REGISTER-ONLY: the caller goes slot-free
   ; and frameless -- the body-decl bind's end-to-end shape
   irJ (cgn "static long cnt9(long n) { long i = 0; while (i < n) { i++; } return i; }\nlong gq2(long a, long n) { return cnt9(n) + a; }\nint main() { return gq2(3, 5); }")
   jf (fnf 'gq2 <>irJ)
   (assert
      (two? irJ)
      (member? '(cmp r9 r5) jf)                      ; i rides its seat against n's ride...
      (member? '(add r9 r9 1) jf)                    ; ...and steps in place
      (= 0 (splds jf))))                             ; the whole fn runs slot-free

; -- the shuttle skip + slot residency: a right side whose forms never write r1 (no call, no
; label, no control) runs with the left PARKED IN r1 -- one mov, no push/pop memory round-trip
; (lvm_eq's compare staging); and stld carries a slot's binding through reg-reg movs, so a
; forwarded value survives its SOURCE register's death and the reload (then the dead store,
; via deadst) drops.
(: (cgn src) (: r (cparse <>(clex src))
               (? (okv? r) (cgen <>r <>>r <>>>r <>>>>r () 'x64) ()))
   (fnf nm fs) (: (seek l) (? !(two? l) () (? (= <l `('label nm)) >l (seek >l)))
                  (take l) (? !(two? l) ()
                              (= <l '(align 2)) ()
                              (<l >< take >l))
                  (take (seek fs)))
   (splds fs) (: (go l n) (? !(two? l) n
                            (go >l (? (two? <l && id? <<l 'ld && id? <>><l 'sp) (n + 1) n)))
                 (go fs 0))
   (spsts fs) (: (go l n) (? !(two? l) n
                            (go >l (? (two? <l && id? <<l 'st && id? <><l 'sp) (n + 1) n)))
                 (go fs 0))
   ; r1safe? is the license: pure r0-landing forms pass; a call, a label, an r1-write refuse
   _ (assert
      (r1safe? '((la r0 s) (cmp r9 r0)))
      !(r1safe? '((call f9)))
      !(r1safe? '((ld r1 r4 8)))
      !(r1safe? '((label .x))))
   ; the divide always shuttles: a leaf right turns the push/pop pair into one mov -- and
   ; dehusk reads the park straight through the staging (no r0 round-trip at all)
   irD (cgn "long dv(long a, long b) { return a / b; } int main() { return (int)dv(10, 2); }")
   df (fnf 'dv <>irD)
   _ (assert
      (two? irD)
      (member? '(mov r1 r6) df)
      !(member? '(mov r1 r0) df)
      (= 0 (splds df))
      (= 0 (spsts df)))
   ; stld: a slot binding rides a mov and survives its source's death (the reload elides);
   ; when every carrier dies the reload stands
   sq '((st r4 40 r0) (mov r9 r0) (la r0 s) (cmp r9 r0) (br eq .t7) (ld r9 r4 40) (la r0 s2) (cmp r9 r0))
   _ (assert
      !(member? '(ld r9 r4 40) (stld sq))
      (member? '(ld r9 r4 40) (stld '((st r4 40 r0) (mov r9 r0) (la r0 s) (la r9 s2) (ld r9 r4 40)))))
   ; end to end: the pointp shape (the vm's hottest compare chain) runs slot-free -- p rides
   ; its park through both compares and the dead store sweeps
   irE (cgn "typedef long word; typedef struct ai { long x; } ai_t; typedef void lvm_t(ai_t *); typedef struct cell { lvm_t *ap; } cell_t; static void lvm_sym(ai_t *g) { g->x = 1; } static void lvm_nom(ai_t *g) { g->x = 2; } static inline int lamp(word x) { return !(x & 1) && x != 0; } static inline cell_t *cell(word x) { return (cell_t *)x; } static inline int pointp(word x) { lvm_t *p; return lamp(x) && ((p = cell(x)->ap) == lvm_sym || p == lvm_nom); } long probe(word a, word b) { if (((a & b & 1)) || pointp(a) || pointp(b)) return a == b; return 7; } int main() { return (int)probe(3, 5); }")
   pbf (fnf 'probe <>irE)
   (assert
      (two? irE)
      (member? '(la r0 lvm_nom) pbf)
      (= 0 (splds pbf))
      (= 0 (spsts pbf))))

; -- the mov-husk fold: adjacent reg-reg movs squeeze (dehusk -- a back-copy or duplicate
; drops, a feed reads through and strands its middle def for deaddef), and cfoldir drops a
; mov whose source chases to the dest's own value at any straight-line distance. the sp
; family never folds: sp moves through the cell cursor, so its copy records outlive it.
(: _ (assert
      ; dehusk units: feed read-through, back-copy, duplicate, self-mov; r3 and a label bar
      (= (dehusk '((mov r0 r6) (mov r1 r0) (mov r0 r5) (div r1 r1 r0)))
         '((mov r0 r6) (mov r1 r6) (mov r0 r5) (div r1 r1 r0)))
      (= (dehusk '((mov r0 r9) (mov r9 r0) (cmp r9 r0))) '((mov r0 r9) (cmp r9 r0)))
      (= (dehusk '((mov r0 r9) (mov r0 r9) (ret))) '((mov r0 r9) (ret)))
      (= (dehusk '((mov r0 r0) (mov r3 r0) (mov sp r4))) '((mov r3 r0) (mov sp r4)))
      (= (dehusk '((mov r0 r6) (label .x9) (mov r1 r0))) '((mov r0 r6) (label .x9) (mov r1 r0))))
   ; an ALU/compare consumer reads through the same way -- but only where the mov STRANDS
   ; (dies? past the consumer), never into its own defs (the alu dance), and never for the
   ; protocol ops (div rides fixed registers)
   _ (assert
      (= (dehusk '((mov r0 r11) (add r2 r2 r0) (mov r0 r5)))
         '((mov r0 r11) (add r2 r2 r11) (mov r0 r5)))
      (= (dehusk '((mov r9 r0) (test r9 1) (br ne .x9))) '((mov r9 r0) (test r9 1) (br ne .x9)))
      (= (dehusk '((mov r9 r0) (add r9 r9 1) (mov r9 r0))) '((mov r9 r0) (add r9 r9 1) (mov r9 r0)))
      (= (dehusk '((mov r9 r5) (add r5 r5 r9) (mov r9 r0))) '((mov r9 r5) (add r5 r5 r9) (mov r9 r0)))
      (= (dehusk '((mov r0 r5) (div r1 r1 r0) (mov r0 r9))) '((mov r0 r5) (div r1 r1 r0) (mov r0 r9))))
   ; cfoldir's root-restore drop rides past reads; the epilogue sp restore stands
   _ (assert
      (= (cfoldir '((mov r9 r6) (test r9 1) (mov r6 r9) (ret))) '((mov r9 r6) (test r9 1) (ret)))
      (member? '(mov sp r4) (cfoldir '((mov r4 sp) (sub sp sp 16) (mov sp r4) (ret)))))
   ; the rename sandwich: (mov X Y) M.. (mov Y X) with X dead after and Y untouched between
   ; runs as well in Y alone -- X renames away, both movs drop. declines: X live after,
   ; a middle touching Y, a 'la symbol spelled like X
   _ (assert
      (= (dehusk '((mov r1 r0) (add r1 r1 65536) (mov r0 r1) (sub r0 r0 1) (ret)))
         '((add r0 r0 65536) (sub r0 r0 1) (ret)))
      (= (dehusk '((mov r1 r0) (ld r5 r1 8) (add r1 r1 24) (mov r0 r1) (ret)))
         '((ld r5 r0 8) (add r0 r0 24) (ret)))
      (= (dehusk '((mov r1 r0) (mov r0 r1) (ret))) '((ret)))
      (= (dehusk '((mov r1 r0) (add r1 r1 8) (mov r0 r1) (mov r5 r1) (ret)))
         '((mov r1 r0) (add r1 r1 8) (mov r0 r1) (mov r5 r1) (ret)))
      (= (dehusk '((mov r1 r0) (add r1 r1 r0) (mov r0 r1) (ret)))
         '((mov r1 r0) (add r1 r1 r0) (mov r0 r1) (ret)))
      (= (dehusk '((mov r1 r0) (li r0 5) (mov r0 r1) (ret)))
         '((mov r1 r0) (li r0 5) (mov r0 r1) (ret)))
      (= (dehusk '((mov r1 r0) (la r5 r1) (mov r0 r1) (ret)))
         '((mov r1 r0) (la r5 r1) (mov r0 r1) (ret))))
   ; the quiet-window back-copy: the restore drops across defs-free forms and conditional
   ; brs (the drop lives only on the fall-through); a label or a call still bars
   (assert
      (= (dehusk '((mov r1 r0) (test r1 1) (br ne .x9) (mov r0 r1) (ret)))
         '((mov r1 r0) (test r1 1) (br ne .x9) (ret)))
      (= (dehusk '((mov r1 r0) (label .x9) (mov r0 r1) (ret)))
         '((mov r1 r0) (label .x9) (mov r0 r1) (ret)))
      (= (dehusk '((mov r1 r0) (call foo) (mov r0 r1) (ret)))
         '((mov r1 r0) (call foo) (mov r0 r1) (ret)))))

(say err "crew/moon/law: lexer (kinds/lines/munch) + parser (precedence/desugar/shapes + refusals) + gen (golden + seams) + aggregates (layout/members/switch/bitfields) + cpp (macros/rescan/hidesets/stringize/paste/variadic/conditionals) + diagnostics (a parse refusal's file + line: the furthest token reached, header attribution, strings joining across an include) + doubles (float literals/xmm value path/conversions/the calling convention) + varargs & _Static_assert (the ... signature bit / va builtins / SysV register save area / array-param decay / const-fold-or-refuse) + float (4-byte scalar, widen/narrow, single-precision globals) & compound literals + objects (text/data split, symbols, no crt0) + freestanding headers (ignored quals/attributes, unnamed proto params) + unsigned & hex (hex/suffix/escape lexing, the specifier-soup canonicalizer, logical >>/unsigned compare/divide) + the C tail (object-vs-function macros, GNU `,##__VA_ARGS__`, function-type/multi-declarator typedefs, sizeof EXPR vs TYPE (a typed local/member folds at parse -- an array bound), bare void return, folded case labels, function-pointer cast types, incomplete arrays, multidimensional arrays with const-expr/enum dims, hex float literals, designated array initializers, block-scoped typedef shadowing) + the gen tail (mproto siblings pinned, &f a function's address in code and images) + inline asm (holo-text templates, %N operands, one opaque raw, constraint refusals) + the alive pass (per-call wraps: dead-across drops, live-across keeps, shadowed/goto blanket; free locals home through calls; a born-at-a-call kill rides its statement's record) + arm64 homing (r10..r14 homes, fp wraps, fp+32 overflow, free locals off the pool) + splice register binds (alias/park/deep-pool, an assigned param never aliases) + the shuttle skip (a leaf right parks the left in r1; stld slot residency through movs) + the mov-husk fold (dehusk's adjacent squeeze, the ALU read-through where the mov strands; cfoldir's root-restore drop; the rename sandwich and the quiet-window back-copy) + the callee-saved contract (cskeep's dirty walk holds every emitted fn; path-aware restores admitted, the cs9 shape refused) ok")
(put err 10)
