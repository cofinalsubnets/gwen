; crew/moon/parse.l -- the C parser (rung 3 stage 4a). tokens -> (1 ast stbl) |
; (). recursive descent + a precedence ladder; the cursor a token list
; threaded as (value rest) answers, () the parse error riding up. the parser
; now carries STATE (ps, a tablet) -- C cannot be parsed without it: typedef
; names ('types), enum constants ('enums, folded to numbers AT PARSE), and
; the struct/union tag table ('stag: tag -> (fields size align), fields
; ((name off ty)..) with C layout -- members aligned to their own alignment,
; the whole rounded to the widest). the stag table rides out with the AST
; for gen's sizing. types: 'char 'short 'int 'long 'void | ('ptr t)
; | ('arr t n) | ('struct tag) -- unions flatten into 'struct entries whose
; offsets are all zero. anonymous struct/union MEMBERS splice their fields
; into the parent (love.h's habit). the AST as before, plus: ('dot e name)
; (e->x desugars to (dot (deref e) x)), ('switch e items) with ('case v) and
; ('dflt) markers inline (fallthrough is the flat list), ('do body cond),
; ('goto name) ('lbl name), ('tdef) (a typedef, gen skips it). stage 4b adds
; the function-pointer declarator ('ptr ('fn ret), a table ('arr ('ptr ('fn ret)) n))
; and generalizes a call HEAD to any postfix expr ('call headexpr args).
; stage 4c adds brace/designated initializers ('init (item..), item plain |
; ('dfield name val) | ('didx n val), nesting), the flexible array member
; ('arr t 0 from []), and [] size inference from the initializer (a string's
; bytes+NUL, or the highest brace index). stage 4d lifts the star-array fence:
; a declarator T *name[n] is an array of pointers ('arr ('ptr t) n), so a
; string table char *m[] parses. still fenced: unsigned, struct params/returns
; BY VALUE (struct ASSIGNMENT walks), case labels past constants and enum
; names, local typedefs, and a bare TOP-LEVEL fn-pointer array (ptop lays its
; own declarators, does not route through pdtor). stage 6d adds the `float`
; scalar (4-byte) and the compound literal ('clit ty init) -- an unnamed object
; from a parenthesized type followed by a brace initializer, (type){ init }.
(:
   ; token readers: t = (kind val line)
   (tkk t) <t (tkv t) <>t
   (tp? t s) (&& (two? t) (&& ((tkk t) = 'p) ((tkv t) = s)))
   (tkw? t k) (&& (two? t) (&& ((tkk t) = 'kw) ((tkv t) = k)))
   ; -- where a refusal stood (doc/moon-diag.md rung 2) --
   ; cpp hands the SPANS -- ((index file)..), which file each stretch of the
   ; flat stream came from -- on a marker token at the stream's head, laid by
   ; the pass that was already rebuilding the spine. peel it in O(1); a caller
   ; feeding raw lexer output has none and reads ().
   ; ⚠ the file has to be carried at all because an #include splices the
   ; header's own line numbering into the one stream: a line number alone
   ; can't say which file it means, and lines run BACKWARDS at a splice.
   (fspans ts) (? (&& (two? ts) ((tkk <ts) = 'fspans)) (tkv <ts) ())
   (fsbody ts) (? (&& (two? ts) ((tkk <ts) = 'fspans)) >ts ts)
   (filat spans i)                                  ; the file in force at stream index i
    (: (go l cur) (? (! (two? l)) cur (i < (cap (cap l))) cur (go >l (caup (cap l))))
       (go spans ()))
   ; a token the failing form's RE-PARSE stamped: (kind val line seq file).
   ; slots 4 and 5 are free -- the cpp hideset and the lexer's glued-paren
   ; flag both sit in slot 4 and are DEAD once parse holds the stream (parse
   ; reads kind and val and nothing else).
   (stampf ts i spans)
    (? (! (two? ts)) ()
       (: t <ts
          (link `((tkk t) (tkv t) <>>t i (filat spans i)) (stampf >ts (i + 1) spans))))
   (stamped? t) (two? >>>>t)                       ; a 5th cell -- probed, never counted: mark runs on EVERY lookahead of every parse
   (seqof t) (? (stamped? t) <>>>t ())
   (filof t) (? (stamped? t) <>>>>t ())
   ; the watermark. a recursive-descent refusal answers a bare () and unwinds
   ; to the top carrying no position, so we remember the deepest token any
   ; lookahead stood on and read the position back off THAT.
   ; ⚠ THE SUCCESSFUL PARSE MUST NOT PAY FOR THIS. there is no cheap mutable
   ; scalar here -- a tablet peep+pin per lookahead cost +25% on love.c -- so
   ; only the RE-PARSE of an already-failing form runs on stamped tokens, and
   ; an unstamped one (every token of every parse that is going to succeed)
   ; costs mark a length test and nothing else. diagnostics are paid for out
   ; of the failure path, where there is all the time in the world.
   far (tablet 0)
   (mark ts)
    (: _ (? (! (two? ts)) ()
            (: t <ts s (seqof t)
               (? (&& (charm? s) ((peep far 'seq -1) < s))
                  (: _ (pin far 'seq s) (pin far 'tok t))
                  ())))
       ts)
   (want ts s) (: _ (mark ts) (? (&& (two? ts) (tp? <ts s)) >ts ()))
   (peekp ts s) (: _ (mark ts) (&& (two? ts) (tp? <ts s)))
   asnops `(`("=" ()) `("+=" '+) `("-=" '-) `("*=" '*) `("/=" '/) `("%=" '%)
             `("&=" '&) `("|=" '|) `("^=" '^) `("<<=" 'shl) `(">>=" 'shr))
   scalars '(char short int long void double float)
   ; a type-specifier RUN (char short int long void double float signed unsigned)
   ; canonicalizes to one type symbol -- `unsigned long` -> 'ulong, `signed char`
   ; -> 'char. the unsigned scalars are 'uchar 'ushort 'uint 'ulong (gen gives
   ; them zero-extend loads + unsigned >> / compare / divide). `long long` folds
   ; to 'long on the 64-bit targets (same width); on the 32-bit thumb ABI it is
   ; its own 8-byte type, 'llong/'ullong -- the register pair.
   tyspec '(char short int long void double float signed unsigned _Complex)
   (canon ps specs)
    (? (member? '_Complex specs)                   ; C99 complex: a SEEDED two-member struct
        (? (member? 'float specs) '(struct "__cplx_float") '(struct "__cplx_double"))
       (member? 'void specs) 'void
       (member? 'double specs) 'double
       (member? 'float specs) 'float
       (: uns (member? 'unsigned specs)
          ll (&& (pst32? ps) (: (n2 l k) (? (! (two? l)) k (n2 >l (? (id? <l 'long) (k + 1) k)))
                                (1 < (n2 specs 0))))                        ; TWO longs on t32
          base (? (member? 'char specs) 'char (member? 'short specs) 'short
                  ll 'llong (member? 'long specs) 'long 'int)
          (? (! uns) base
             (base = 'char) 'uchar (base = 'short) 'ushort
             (base = 'llong) 'ullong (base = 'long) 'ulong 'uint)))
   ; ignorable specifiers: storage/qualifier keywords + gcc attribute ids. cc
   ; keeps no linkage/qualifier state, so these are SKIPPED wherever a type may
   ; begin or a declarator may carry them -- __attribute__((..)) / _Alignas(..)
   ; balance-skip their parens; _Noreturn / __inline stand alone.
   qualkw '(const volatile restrict register auto inline extern static)
   qualid '("__attribute__" "__inline" "__inline__" "__restrict" "__restrict__"
            "__volatile__" "__const" "_Noreturn" "_Alignas" "__alignof__")
   (pquals ts)
    (? (! (two? ts)) ts
       (: t <ts
          (? (&& ((tkk t) = 'kw) (member? (tkv t) qualkw)) (pquals >ts)
             (&& ((tkk t) = 'id) (member? (tkv t) qualid))
              (: r >ts (? (peekp r "(") (pquals (skipbal >r 1)) (pquals r)))
             ts)))
   ; skip a TRAILING run of __attribute__((..)) / __asm__(..) -- the decoration
   ; glibc hangs off a declarator's tail (`int f(void) __attribute__((__nothrow__));`,
   ; `extern int x __asm__("y");`). balance-skip the parens; the codegen owes nothing.
   (patt ts)
    (? (! (two? ts)) ts
       (&& ((tkk <ts) = 'id) (member? (tkv <ts) `("__attribute__" "__asm__" "__asm")))
        (: r >ts (? (peekp r "(") (patt (skipbal >r 1)) (patt r)))
       ts)
   ; does the leading declaration-specifier run carry `static`? scans the front
   ; run of qualifier + type-specifier keywords (and balance-skips attribute ids),
   ; the same tokens pquals drops, and flags the `static` storage class -- so a
   ; BLOCK-SCOPE `static` local is laid in .bss (a real global), not a stack slot
   ; (a stack `static` is uninitialised garbage every call -- the ai_pagesize bug).
   (leadstatic? ts)
    (? (! (two? ts)) ()
       (: t <ts
          (? (&& ((tkk t) = 'kw) ((tkv t) = 'static)) 1
             (&& ((tkk t) = 'kw) (|| (member? (tkv t) qualkw) (member? (tkv t) tyspec))) (leadstatic? >ts)
             (&& ((tkk t) = 'id) (member? (tkv t) qualid))
              (: r >ts (? (peekp r "(") (leadstatic? (skipbal >r 1)) (leadstatic? r)))
             ())))
   ; a leading __attribute__((..)) whose balance names `weak` -> the tokens after
   ; the run; anything else (no attribute, or one without weak) -> ()
   (weakat? ts)
    (? (! (two? ts)) ()
       (! (&& ((tkk <ts) = 'id) ((tkv <ts) = "__attribute__"))) ()
       (: r >ts
          (? (! (peekp r "(")) ()
             (: (wscan s d hit)
                 (? (d < 1) (? hit s ())
                    (! (two? s)) ()
                    (peekp s "(") (wscan >s (d + 1) hit)
                    (peekp s ")") (wscan >s (d - 1) hit)
                    (wscan >s d (|| hit (&& ((tkk <s) = 'id)
                                                 (|| ((tkv <s) = "weak") ((tkv <s) = "__weak__"))))))
                (wscan >r 1 ())))))
   ; the declaration's ALIAS attribute: `alias ( "TARGET" )` inside an
   ; __attribute__ run, anywhere in the form (musl's weak_alias trails it:
   ; `extern __typeof(old) new __attribute__((__weak__, __alias__("old")))`).
   ; -> (TARGET weak?) | (). scans to the first depth-0 `;` or `{`, like
   ; secat?, noting a `weak` in the SAME run -- binding matters: musl's
   ; exit.c defines __stdio_exit weakly so __stdio_exit.c's real one wins.
   (aliasat? ts)
    (: (go s d tgt wk)
        (? (! (two? s)) (? tgt `(tgt wk) ())
           (: t <s
              (? (&& (d < 1) (|| (tp? t ";") (tp? t "{"))) (? tgt `(tgt wk) ())
                 (tp? t "(") (go >s (d + 1) tgt wk)
                 (tp? t ")") (go >s (d - 1) tgt wk)
                 (&& ((tkk t) = 'id)
                     (|| ((tkv t) = "weak") ((tkv t) = "__weak__")))
                  (go >s d tgt 1)
                 (&& ((tkk t) = 'id)
                     (|| ((tkv t) = "alias") ((tkv t) = "__alias__")))
                  (: r >s
                     (? (! (peekp r "(")) (go >s d tgt wk)
                        (: r2 >r
                           (? (&& (two? r2) ((tkk <r2) = 'str))
                              (go >s d (tkv <r2) wk) (go >s d tgt wk)))))
                 (go >s d tgt wk))))
       (go ts 0 () ()))
   ; a `weak` attribute ANYWHERE in the declaration -- weakat? reads only a
   ; LEADING one, but musl writes `extern weak hidden const size_t _DYNAMIC[];`
   ; (its `weak` macro sits after the storage class). the binding matters even
   ; for a declaration that defines nothing: a weak UNDEFINED symbol resolves
   ; to zero at static link, which is how __init_tls tells static from dynamic.
   (wkany? ts)
    (: (go s d) (? (! (two? s)) ()
                   (: t <s
                      (? (&& (d < 1) (|| (tp? t ";") (tp? t "{"))) ()
                         (tp? t "(") (go >s (d + 1))
                         (tp? t ")") (go >s (d - 1))
                         (&& ((tkk t) = 'id)
                             (|| ((tkv t) = "weak") ((tkv t) = "__weak__"))) 1
                         (go >s d))))
       (go ts 0))
   ; the declaration's section attribute, if any: scan this form's tokens (to
   ; the first depth-0 `;` or `{`) for `section ( "NAME" )` -- inside a
   ; declaration that shape only occurs in an __attribute__. -> NAME | ()
   (secat? ts)
    (: (go s d) (? (! (two? s)) ()
                   (: t <s
                      (? (&& (d < 1) (|| (tp? t ";") (tp? t "{"))) ()
                         (tp? t "(") (go >s (d + 1))
                         (tp? t ")") (go >s (d - 1))
                         (&& ((tkk t) = 'id) ((tkv t) = "section"))
                          (? (&& (peekp >s "(") (&& (two? >>s) ((tkk <>>s) = 'str)))
                             (tkv <>>s)
                             (go >s d))
                         (go >s d))))
       (go ts 0))
   (anyat? ts)                                       ; ANY leading __attribute__((..)): the rest | ()
    (? (! (two? ts)) ()
       (! (&& ((tkk <ts) = 'id) ((tkv <ts) = "__attribute__"))) ()
       (: r >ts
          (? (! (peekp r "(")) ()
             (: (ascan s d) (? (d < 1) s
                               (! (two? s)) ()
                               (peekp s "(") (ascan >s (d + 1))
                               (peekp s ")") (ascan >s (d - 1))
                               (ascan >s d))
                (ascan >r 1)))))
   (passoc s tbl) (? (! (two? tbl)) ()
                     ((cap (cap tbl)) = s) <tbl
                     (passoc s >tbl))

   ; -- the parse state --
   (psnew tgt) (: t (tablet 0)
                _ (pin t 'tgt tgt)                  ; the codegen target: 32-bit widths (thumb1/thumb2) vs 64-bit
                _ (pin t 'types (tablet 0))         ; typedef name -> ty
                _ (pin t 'weaks ())                 ; __attribute__((weak)) function names
                _ (pin t 'enums (tablet 0))         ; enum constant -> value
                _ (pin t 'stag (tablet 0))          ; tag -> (fields size align)
                _ (pin t 'sigs (tablet 0))          ; fn/proto name -> (rettype (paramty..)) for the call ABI
                _ (pin t 'anon 0)                   ; the anonymous-tag counter
                _ (pin t 'sfns (tablet 0))          ; top-level `static` fn names: LOCAL symbols in the .o
                _ (pin t 'svars (tablet 0))         ; top-level `static` globals: internal linkage, never exported
                _ (pin t 'sects (tablet 0))         ; global name -> its __attribute__((section "..")) string
                _ (pin t 'locals (tablet 0))        ; local/param name -> its declared type (for parse-time sizeof folding)
                _ (pin t 'globals (tablet 0))       ; file-scope name -> ty, so sizeof over a GLOBAL settles at parse too
                ; gcc's builtin va_list, seeded so `typedef __builtin_va_list va_list;`
                ; (musl's alltypes.h) parses: the SAME per-target tag stdarg.h lays --
                ; gen bakes the va offsets, so only size/align/decay ride the type here
                _ (pin (t 'stag) "__va_list_tag"
                       (? (member? tgt '(thumb2 thumb2sp thumb1)) '((("__ap" 0 (ptr void))) 4 4)
                          (id? tgt 'riscv64) '((("__ap" 0 (ptr void))) 8 8)   ; LP64: one walking pointer, 8-byte slots
                          (id? tgt 'arm64)
                           '((("__stack" 0 (ptr void)) ("__gr_top" 8 (ptr void)) ("__vr_top" 16 (ptr void))
                              ("__gr_offs" 24 int) ("__vr_offs" 28 int)) 32 8)
                          '((("gp_offset" 0 int) ("fp_offset" 4 int) ("overflow_arg_area" 8 (ptr void))
                             ("reg_save_area" 16 (ptr void))) 24 8)))
                _ (pin (t 'types) "__builtin_va_list" '(arr (struct "__va_list_tag") 1))
                ; C99 _Complex, seeded as two-member structs (canon maps the
                ; specifier run onto these tags): layout, copy, union membership
                ; and the SysV (sse sse) / one-sse ABI all ride the struct lanes
                _ (pin (t 'stag) "__cplx_double" '((("re" 0 double) ("im" 8 double)) 16 8))
                _ (pin (t 'stag) "__cplx_float" '((("re" 0 float) ("im" 4 float)) 8 4))
                _ (pin t 'vlan 0)                   ; the VLA decl-site counter (stamps each site's slot index)
                _ (pin t 'alias (tablet 0))         ; __attribute__((alias("t"))) : name -> its target (musl's weak_alias)
                t)
   ; does a type begin here?
   (tkbty? ps t)
    (? (! (two? t)) ()
       ((tkk t) = 'kw) (member? (tkv t) '(char short int long void double float signed unsigned _Complex struct union enum))
       ((tkk t) = 'id) (? (member? (tkv t) `("__typeof" "__typeof__")) 1
                          (peep (ps 'types) (tkv t) 0) 1 ())
       ())

   ; -- the type floor (parse side; gen re-derives with the same stag) --
   (ptr? ty) (&& (two? ty) (<ty = 'ptr))
   (arr? ty) (&& (two? ty) (<ty = 'arr))
   (fn? ty) (&& (two? ty) (<ty = 'fn))
   (stru? ty) (&& (two? ty) (<ty = 'struct))
   (cplxty? ty) (&& (stru? ty) (member? <>ty `("__cplx_double" "__cplx_float")))   ; a seeded complex
   (bitf? ty) (&& (two? ty) (<ty = 'bitfield))     ; a bitfield member: (bitfield uty bitoff width)
   ; the machine WORD (bytes): a pointer and a gp scalar (long) ride it. 4 on the 32-bit thumb ABI,
   ; 8 otherwise. MUST match gen's (wsize g) (gen.l) -- a divergence mislays every pointer-bearing
   ; struct SILENTLY, since playout bakes offsets here at parse and gen trusts the stag table.
   (pst32? ps) (member? (peep ps 'tgt ()) '(thumb2 thumb2sp thumb1))
   (psword ps) (? (pst32? ps) 4 8)
   (tsz ps ty) (? (ptr? ty) (psword ps)
                  (arr? ty) (* <>>ty (tsz ps <>ty))
                  (stru? ty) (caup (peep (ps 'stag) <>ty 0))
                  (bitf? ty) (tsz ps <>ty)          ; the unit's size (a bitfield rides its declared type)
                  (|| (ty = 'char) (ty = 'uchar)) 1 (|| (ty = 'short) (ty = 'ushort)) 2
                  (|| (ty = 'int) (|| (ty = 'uint) (ty = 'float))) 4
                  (|| (ty = 'llong) (|| (ty = 'ullong) (ty = 'double))) 8   ; 8-byte on EVERY target
                  (psword ps))
   (talign ps ty) (? (ptr? ty) (psword ps)
                     (arr? ty) (talign ps <>ty)
                     (stru? ty) (cauup (peep (ps 'stag) <>ty 0))
                     (bitf? ty) (talign ps <>ty)
                     (tsz ps ty))
   (aup n a) (& (n + (a - 1)) (0 - a))              ; round n up to alignment a

   ; -- parse-time expr typer (for sizeof folding) -- the UNDECAYED type of an
   ; lvalue/pointer expr, | () when unknown (the caller keeps a deferred 'szof
   ; node then, and gen sizes it). only the forms whose type parse can settle
   ; from ps 'locals + ps 'stag: a local/param var, a ->/. member, a *deref /
   ; a[i] element, a cast. arrays stay UNDECAYED (sizeof of an array is its span,
   ; not a pointer) -- gen's szof lane reads the same undecayed type via clval.
   (fldty ps sty nm) (: e (peep (ps 'stag) <>sty ())     ; the type of member nm off a struct type
                        (? (! (two? e)) ()
                           (: f (assoc nm <e) (? (two? f) <>>f ()))))
   (fmof ps sty nm) (? (! (stru? sty)) ()               ; a member's (byte-offset ty) off a struct type
                       (: e (peep (ps 'stag) <>sty ())
                          (? (! (two? e)) ()
                             (: f (assoc nm <e) (? (two? f) `(<>f <>>f) ())))))
   ; &((T*)0)->fld (and a.b chains over it) folds to the member's byte offset --
   ; the offsetof idiom, which sqlite carries as its own macro. the chain must
   ; GROUND in a deref of a null pointer cast; anything else stays an address.
   (nulloff ps e)
    (: (ground b)                                    ; -> (byte-offset struct-ty) | ()
        (? (! (two? b)) ()
           (<b = 'deref)
            (: c <>b
               (? (! (&& (two? c) (<c = 'cast))) ()
                  (: ty <>c v (cfold <>>c)
                     (? (&& (ptr? ty) (&& (charm? v) (v = 0))) `(0 <>ty) ()))))
           (<b = 'dot)
            (: gr (ground <>b)
               (? (! (two? gr)) ()
                  (: f (fmof ps <>gr <>>b)
                     (? (two? f) `((<gr + <f) <>f) ()))))
           ())
       ; a null-grounded pointer VALUE -> (byte-address stride) | (): the cast
       ; ((T*)K), and + arithmetic over it scaled by the pointee (sqlite's
       ; SQLITE_INT_TO_PTR is &((char*)0)[X]). anything not grounded in a
       ; constant null cast stays an address.
       (pval x)
        (? (! (two? x)) ()
           (&& (<x = 'cast) (ptr? <>x))
            (: ty <>x
               v (cfold <>>x)
               (? (charm? v) `(v (tsz ps <>ty)) ()))
           (&& (<x = 'bin) (id? <>x '+))
            (: pa (pval <>>x)
               (? (two? pa)
                  (: n (cfold <>>>x) (? (charm? n) `((<pa + (* n <>pa)) <>pa) ()))
                  (: pb (pval <>>>x)
                     (? (! (two? pb)) ()
                        (: n (cfold <>>x) (? (charm? n) `((<pb + (* n <>pb)) <>pb) ()))))))
           ; a null-grounded ARRAY member decays to its (constant) address with
           ; the element stride -- &((T*)0)->ts[i] (musl's ioctl compat table)
           (<x = 'dot)
            (: gr (ground x)
               (? (! (two? gr)) ()
                  (: ty <>gr
                     (? (arr? ty) `(<gr (tsz ps <>ty)) ()))))
           ())
       (? (&& (two? e) (<e = 'deref))                ; &*(ptr-expr): the pointer's own value
           (: pv (pval <>e) (? (two? pv) <pv ()))
          (! (&& (two? e) (<e = 'dot))) ()
          (: gr (ground <>e)
             (? (! (two? gr)) ()
                (: f (fmof ps <>gr <>>e)
                   (? (two? f) (<gr + <f) ()))))))
   (ptrarg ps e) (? (! (&& (two? e) (<e = 'bin))) ()     ; the ptr/arr operand of a `bin + ptr idx` (a[i] desugars so)
                    (: a (ptype ps <>>e) b (ptype ps <>>>e)
                       (? (|| (ptr? a) (arr? a)) a (|| (ptr? b) (arr? b)) b ())))
   (ptype ps e)
    (? (! (two? e)) ()
       (: k <e
          (? (k = 'var) (: lt (peep (ps 'locals) <>e ())   ; locals shadow; a file-scope name falls through
                           (? lt lt
                              (: gt (peep (ps 'globals) <>e ())
                                 (? gt gt
                                    ; a bare FUNCTION name: its 'sigs row as ('fn rty)
                                    ; (__typeof(open) -- musl's weak_alias declares through it)
                                    (: sg (peep (ps 'sigs) <>e ())
                                       (? (two? sg) `('fn <sg) ()))))))
             (k = 'dot) (: bt (ptype ps <>e) (? (stru? bt) (fldty ps bt <>>e) ()))
             (k = 'deref) (: it (ptype ps <>e)
                             (? (ptr? it) <>it (arr? it) <>it
                                (: pt (ptrarg ps <>e) (? (ptr? pt) <>pt (arr? pt) <>pt ()))))
             (k = 'cast) <>e
             (k = 'clit) <>e
             ; a call through a named function: its 'sigs return type (the
             ; complex promotion below needs clog's answer to type complex)
             (k = 'call) (? (&& (two? <>e) (id? (cap <>e) 'var))
                            (: sg (peep (ps 'sigs) (caup <>e) ())
                               (? (two? sg) <sg ()))
                            ())
             (k = 'str) `('arr 'char ((tally <>e) + 1))   ; a literal IS its array (cpp pasted phase 6; the lexer decoded escapes) -- sizeof("ab") folds to 3, an array bound
             ; a literal: sizeof('T') is sizeof(int) (musl's _IOW ioctl codes);
             ; a plain decimal past int32 is a long (suffixed forms arrive cast)
             (k = 'num) (? (&& (-2147483648 <= <>e) (<>e <= 2147483647)) 'int 'long)
             (k = 'flo) 'double
             ())))

   ; -- declarators: stars XOR one array suffix, PLUS the function-pointer
   ; dragon -- T (*name)(..) and the dispatch-table T (*name[n])(..). the
   ; parameter list is BALANCE-SKIPPED (args classify by their own type); a
   ; function pointer is ('ptr ('fn ret)) -- ret so an indirect call knows a
   ; double result -- a table of them ('arr ('ptr ('fn ret)) n).
   (skipbal s d) (? (d < 1) s
                    (! (two? s)) ()
                    (peekp s "(") (skipbal >s (d + 1))
                    (peekp s ")") (skipbal >s (d - 1))
                    (skipbal >s d))
   ; a RUN of [const-expr] / [] suffixes -> (dims rest) | (). the dimension is any
   ; integer CONSTANT expression -- a literal, an enum constant, `64 / limb_bits` --
   ; parsed by pexpr and folded by cfold. only the first (outer) dim may be [] (size 0).
   ; a parameter's [static n] / [const n] (C99 6.7.6.3) drops its quals -- the
   ; parameter decays to a pointer, so the promise costs nothing.
   ; a dim that will not fold is a RUNTIME dim (C99 VLA), carried as ('vdim expr)
   ; -- the consumers judge: a local decl allocates, a pointee scales at runtime,
   ; sizeof multiplies; anywhere else the varr type refuses downstream.
   (adims ps ts2 acc) (? (! (peekp ts2 "[")) `((rev acc) ts2)
                         (: t2 (pquals >ts2)
                            (? (peekp t2 "]") (adims ps >t2 (link 0 acc))
                               (: er (pexpr ps t2)
                                  (? (! (two? er)) ()
                                     (: dv (cfold <er)
                                        rest (want <>er "]")
                                        (? (! (two? rest)) ()
                                           (charm? dv) (adims ps rest (link dv acc))
                                           (adims ps rest (link `('vdim <er) acc)))))))))
   ; fold dims (outer..inner) into a nested arr: [3][4] of T -> (arr (arr T 4) 3);
   ; a 'vdim dim lays ('varr elt expr) instead
   (mkarr ety dims) (? (! (two? dims)) ety
                       (: d <dims
                          (? (&& (two? d) (id? <d 'vdim)) `('varr (mkarr ety >dims) <>d)
                             `('arr (mkarr ety >dims) d))))
   (varr? ty) (&& (two? ty) (<ty = 'varr))
   ; the (params)/[dims] suffix run on a direct declarator. each suffix wraps
   ; INSIDE w (the wrap-so-far: eventual-pointee -> declared type), so the type
   ; grows inside-out; the name's OWN (params) ride out as dat (a fn DEF binds
   ; them). bare = still just a (paren-wrapped) name, cleared by any suffix.
   ; answers ((name w dat bare) rest).
   (psufs ps ts name w dat bare)
    (? (peekp ts "(")
        (: rp (pparams ps >ts ())
           (? (! (two? rp)) () (psufs ps <>rp name (\ t (w `('fn t))) (? bare rp dat) ())))
       (peekp ts "[")
        (: ar (adims ps ts ())
           (? (! (two? ar)) () (psufs ps <>ar name (\ t (w (mkarr t <ar))) dat ())))
       `(`(name w dat bare) ts))
   ; direct declarator, RECURSIVE: name | ( *.. dtor ) , then suffixes -- covers
   ; (*name)(params), the redundant (name), (*const p) (quals ride pquals), and
   ; nested forms (`void (*(*x)(a))(b)`: x a ptr to fn(a) returning a ptr to
   ; fn(b) returning void). a paren declarator's pointee is what TRAILS its `)`.
   ; the head may also be EMPTY (abstract: `(*)(params)`). -> ((name w dat bare) rest) | ()
   (pdirect ps ts)
    (: (nstars s k) (: s2 (pquals s) (? (peekp s2 "*") (nstars >s2 (k + 1)) `(k s2)))
       (ptrk k t) (? (k < 1) t (ptrk (k - 1) `('ptr t)))
       (? (&& (two? ts) ((tkk <ts) = 'id)) (psufs ps >ts (tkv <ts) (\ t t) () 1)
          (! (peekp ts "(")) (psufs ps ts () (\ t t) () ())
          (: sr (nstars >ts 0)
             k <sr t2 <>sr
             dr (pdirect ps t2)
             (? (! (two? dr)) ()
                (: e <dr
                   r (want <>dr ")")
                   (? (! (two? r)) ()
                      (psufs ps r <e (\ t (<>e (ptrk k t))) <>>e (&& <>>>e (k = 0)))))))))
   (pdtor ps ts base)                               ; ps threads to adims for const-expr array dims; -> ((name ty) rest) | ()
    (: (stars ts2 ty) (: s (pquals ts2) (? (peekp s "*") (stars >s `('ptr ty)) `(ty s)))
       sr (stars ts base)
       ty <sr ts2 (pquals <>sr)           ; attrs may sit before the name
       (? (peekp ts2 "(")                            ; a parenthesized declarator -- pdirect recurses,
           ; its wrap lays the declared type over ty; dat (the name's own params)
           ; rides out as the THIRD result element when the declarator IS a fn
           (: dr (pdirect ps ts2)
              (? (! (two? dr)) ()
                 (: e <dr ty2 (<>e ty) dat <>>e
                    `((? (two? dat) `(<e ty2 dat) `(<e ty2)) <>dr))))
          (! (&& (two? ts2) ((tkk <ts2) = 'id)))    ; no name: an ABSTRACT declarator (unnamed proto param)
           (? (peekp ts2 "[")                          ; abstract ARRAY `T [n]..` (e.g. `struct timespec const[2]`)
               (: dr (adims ps ts2 ())                 ; -> nested arr of ty; pparams decays it to a pointer
                  (? (! (two? dr)) () `(`(() (mkarr ty <dr)) <>dr)))
              (|| (peekp ts2 ")") (peekp ts2 ",")) `(`(() ty) ts2)
              ())
          (: name (tkv <ts2)
             ts3 >ts2
             (? (peekp ts3 "(")                       ; name(params) -- a function declarator; type ('fn), the params balance-skipped (cc calls untyped)
                 (: t2 (skipbal >ts3 1)
                    (? (two? t2) `(`(name `('fn ty)) t2) ()))
                (! (peekp ts3 "[")) `(`(name ty) ts3)
                (: dr (adims ps ts3 ())               ; one or more [n]/[] suffixes -> a nested arr of ty
                   (? (! (two? dr)) ()
                      `(`(name (mkarr ty <dr)) <>dr)))))))

   ; -- base types: scalars, struct/union (with bodies), enum, typedef names --
   ; struct bodies REGISTER into ps 'stag; answers (ty rest) | ()
   ; ": CONST-EXPR" bitfield width (ts starts AT the ":") -> (width rest) | ()
   (pbits ps ts) (? (! (peekp ts ":")) ()
                    (: er (pasn ps >ts)
                       (? (! (two? er)) ()
                          (: cv (cfold <er) (? (charm? cv) `(cv <>er) ())))))
   (pmembers ps ts acc)                             ; member pairs ((name ty)..); () name = anonymous splice; (name ty width) = a bitfield
    (? (! (two? ts)) ()
       (peekp ts "}") `((rev acc) >ts)
       (: br (pbty ps ts)
          (? (! (two? br)) ()
             (: bty <br ts2 <>br
                (? (peekp ts2 ";")                   ; an anonymous struct/union member
                   (? (stru? bty) (pmembers ps >ts2 (link `(() bty) acc)) ())
                   (peekp ts2 ":")                   ; an anonymous bitfield (padding / `:0` aligner): () name + width
                   (: bw (pbits ps ts2)
                      (? (! (two? bw)) ()
                         (: rest (want <>bw ";")
                            (? (two? rest) (pmembers ps rest (link `(() bty <bw) acc)) ()))))
                   (: (dfin mem2 toks acc2)          ; after a declarator (+ optional bitfield): , or ;
                       (? (peekp toks ",") (dts (cup toks) (link mem2 acc2))
                          (: rest (want toks ";")
                             (? (two? rest) `((link mem2 acc2) rest) ())))
                      (dts ts3 acc2)                 ; dtor [: width] (, dtor [: width])* ;
                       (: dr (pdtor ps ts3 bty)
                          (? (! (two? dr)) ()
                             (peekp <>dr ":")        ; a bitfield: append the width to (name ty)
                              (: bw (pbits ps <>dr)
                                 (? (! (two? bw)) () (dfin (<dr + `(<bw)) <>bw acc2)))
                             (dfin <dr <>dr acc2)))
                      r (dts ts2 ())
                      ; (cap r) is REVERSED (dts links) -- and acc accumulates reversed
                      ; too (pmembers revs at "}"), so prepend it AS IS: a second rev
                      ; here laid `double re, im;` as im-then-re (an ABI break vs gcc)
                      (? (two? r) (pmembers ps <>r (<r + acc)) ())))))))
   ; lay a member list out; a union flattens every offset to 0. the cursor is a
   ; BIT position from the struct start: normal members byte-align to their own
   ; alignment; a bitfield sits at the cursor, only bumped to the next declared-
   ; type unit boundary to avoid straddling it -- the SysV/gcc x86 packing. its
   ; stored (unit-byte-offset bitoff) let gen load the aligned word and extract.
   (bfmem? m) (two? (cup (cup m)))                  ; a member pair carrying a width: (name ty width)
   ; a trailing __attribute__((..)) run on a struct BODY: consume it, reading
   ; whether `packed` rides inside (x86_64 epoll_event -- the kernel ABI needs
   ; the packed layout, so skipping the attribute would mislay `data` at 8)
   (pattpk ts)
    (? (! (&& (two? ts) (&& ((tkk <ts) = 'id) ((tkv <ts) = "__attribute__")))) `(0 ts)
       (: r >ts
          (? (! (peekp r "(")) `(0 ts)
             (: (scan s d pk)
                 (? (|| (! (two? s)) (d < 1)) `(pk s)
                    (peekp s "(") (scan >s (d + 1) pk)
                    (peekp s ")") (scan >s (d - 1) pk)
                    (&& ((tkk <s) = 'id) (member? (tkv <s) `("packed" "__packed__"))) (scan >s d 1)
                    (scan >s d pk))
                r2 (scan >r 1 0)
                nx (pattpk <>r2)
                `((| <r2 <nx) <>nx)))))
   (playout ps ms uni pk)                           ; -> (fields size align); pk: packed, every member aligns 1
    (: (go l bit mx acc)
        (? (! (two? l))
           (: a2 (? (mx < 1) 1 mx)
              `((rev acc) (aup (>> (bit + 7) 3) a2) a2))
           (: m <l name <m ty <>m
              (? (bfmem? m)                          ; a bitfield member (named or anonymous): (name ty width)
                 (: w <>>m
                    usz (tsz ps ty) ubits (usz * 8) a (talign ps ty)
                    nmx (? (mx < a) a mx)
                    (? (= w 0) (go >l (aup bit ubits) nmx acc)        ; `:0` -- align the cursor to the unit, no field
                       uni (: fld (? name `(name 0 `('bitfield ty 0 w)) ())
                              (go >l (? (bit < ubits) ubits bit) nmx (? name (link fld acc) acc)))
                       (: bit2 (? (((& bit (ubits - 1)) + w) > ubits) (aup bit ubits) bit)   ; bump to avoid straddling the unit
                          uoff (& (>> bit2 3) (0 - usz))              ; the declared-type-aligned word holding these bits
                          bitoff (bit2 - (uoff * 8))
                          fld (? name `(name uoff `('bitfield ty bitoff w)) ())   ; anonymous -> reserve bits, no field
                          (go >l (bit2 + w) nmx (? name (link fld acc) acc)))))
                 (! name)                            ; the anonymous struct/union splice (name absent, no width)
                 (: inner (peep (ps 'stag) <>ty 0)
                    ia (? pk 1 <>>inner)
                    isz <>inner
                    sbit (? uni 0 (aup bit (ia * 8)))
                    at (>> sbit 3)
                    (spl fs acc2) (? (! (two? fs)) acc2
                                     (: f <fs
                                        (spl >fs (link `(<f (at + <>f) <>>f) acc2))))
                    nmx (? (mx < ia) ia mx)
                    nbit (? uni (? (bit < (isz * 8)) (isz * 8) bit) (sbit + (isz * 8)))
                    (go >l nbit nmx (spl <inner acc)))
                 (: a (? pk 1 (talign ps ty))
                    sz (tsz ps ty)
                    abit (? uni 0 (aup bit (a * 8)))
                    at (>> abit 3)
                    nmx (? (mx < a) a mx)
                    nbit (? uni (? (bit < (sz * 8)) (sz * 8) bit) (abit + (sz * 8)))
                    (go >l nbit nmx (link `(name at ty) acc))))))
       (go ms 0 0 ()))
   (panon ps) (: k (peep ps 'anon 0) _ (pin ps 'anon (k + 1)) (".anon" + (show k)))
   (pbty ps ts0)
    (: ts (pquals ts0)
     (? (! (two? ts)) ()
       (: t <ts
          (? (&& ((tkk t) = 'kw) (member? (tkv t) tyspec))     ; a run of type specifiers -> one canonical type
              (: (grab l acc) (? (&& (two? l) (&& ((tkk <l) = 'kw) (member? (tkv <l) tyspec)))
                                 (grab >l (link (tkv <l) acc))
                                 ; a qualifier INTERLEAVED in the run (`unsigned const char`):
                                 ; C's declaration specifiers are order-free -- skip and continue
                                 (: l2 (pquals l)
                                    (? (&& (two? l2) (&& ((tkk <l2) = 'kw) (member? (tkv <l2) tyspec)))
                                       (grab l2 acc)
                                       `((rev acc) l))))
                 r (grab ts ())
                 `((canon ps <r) <>r))
             (|| (tkw? t 'struct) (tkw? t 'union))
              (: uni (tkw? t 'union)
                 ts2 >ts
                 tag (? (&& (two? ts2) ((tkk <ts2) = 'id)) (tkv <ts2) ())
                 ts3 (? tag >ts2 ts2)
                 (? (peekp ts3 "{")
                    (: tag2 (? tag tag (panon ps))
                       mr (pmembers ps >ts3 ())
                       (? (! (two? mr)) ()
                          (: pa (pattpk <>mr)          ; a trailing __attribute__((packed)) lays align-1
                             lay (playout ps <mr uni <pa)
                             _ (pin (ps 'stag) tag2 lay)
                             `(`('struct tag2) <>pa))))
                    (? tag `(`('struct tag) ts3) ())))
             (tkw? t 'enum)
              (: ts2 >ts
                 ts3 (? (&& (two? ts2) ((tkk <ts2) = 'id)) >ts2 ts2)
                 (? (peekp ts3 "{")
                    (: (consts ts4 v)
                        (? (peekp ts4 "}") `('int >ts4)    ; a trailing comma (or empty body): the enum ends
                           (! (&& (two? ts4) ((tkk <ts4) = 'id))) ()
                           (: name (tkv <ts4)
                              ts5 >ts4
                              vr (? (peekp ts5 "=")
                                    (: er (pasn ps >ts5)              ; = CONST-EXPR: fold it (enum consts, 1<<n, A+1) -- not just a literal
                                       (? (! (two? er)) ()
                                          (: cv (cfold <er)
                                             (? (charm? cv) `(cv <>er) ()))))
                                    `(v ts5))
                              (? (! (two? vr)) ()
                                 (: _ (pin (ps 'enums) name <vr)
                                    ts6 <>vr
                                    (? (peekp ts6 ",") (consts >ts6 (<vr + 1))
                                       (: rest (want ts6 "}")
                                          (? (two? rest) `('int rest) ())))))))
                       (consts >ts3 0))
                    `('int ts3)))
             (&& ((tkk t) = 'id) (? (peep (ps 'types) (tkv t) 0) 1 ()))
              `((peep (ps 'types) (tkv t) 0) >ts)
             ; __typeof(expr) / __typeof__(type-name) (gcc): the operand's parse-time
             ; type -- a type-name reads pcty, an expression ptype (whose 'sigs lane
             ; carries musl's weak_alias: `extern __typeof(old) new __attribute__(..)`)
             (&& ((tkk t) = 'id) (member? (tkv t) `("__typeof" "__typeof__")))
              (: r1 (want >ts "(")
                 (? (! (two? r1)) ()
                    (tkbty? ps <r1)
                     (: tr (pcty ps r1)
                        rest (? (two? tr) (want <>tr ")") ())
                        (? (two? rest) `(<tr rest) ()))
                    (: er (pasn ps r1)
                       (? (! (two? er)) ()
                          (: ty (ptype ps <er)
                             rest (want <>er ")")
                             (? (&& ty (two? rest)) `(ty rest) ()))))))
             ()))))
   ; a type-name (casts, sizeof): base + an ABSTRACT declarator -- pdtor covers
   ; stars, arrays, AND the function-pointer form `T (*)(params)` (name = ()).
   (pcty ps ts)
    (: br (pbty ps ts)
       (? (! (two? br)) ()
          (: dr (pdtor ps <>br <br)
             (? (two? dr) `((caup (cap dr)) <>dr) ()))))

   ; complex usual-arithmetic conversion (C99 6.3.1.8): exactly one side
   ; complex, the other arithmetic-real -> the real side rises to {x, 0}
   ; (a clit, single-eval), so gen's complex lane only meets complex operands
   (cxwrap ty x) `('clit ty `('init `(x `('flo 0.0))))
   (mkbin ps op l r)
    (: tl (ptype ps l) tr (ptype ps r)
       cl (cplxty? tl) cr (cplxty? tr)
       (? (&& cl (&& (! cr) (&& tr (! (|| (ptr? tr) (|| (arr? tr) (stru? tr)))))))
           `('bin op l (cxwrap tl r))
          (&& cr (&& (! cl) (&& tl (! (|| (ptr? tl) (|| (arr? tl) (stru? tl)))))))
           `('bin op (cxwrap tr l) r)
          `('bin op l r)))
   ; one left-associative binary level
   (plevel ps sub ops ts)
    (: r (sub ps ts)
       (? (! (two? r)) ()
          (: (more l ts2)
              (: t (? (two? ts2) <ts2 ())
                 hit (? (&& (two? t) ((tkk t) = 'p)) (passoc (tkv t) ops) ())
                 (? (! (two? hit)) `(l ts2)
                    (: r2 (sub ps >ts2)
                       (? (two? r2)
                          (more (mkbin ps <>hit l <r2) <>r2)
                          ()))))
             (more <r <>r))))

   ; primary := num | str | id (an enum constant folds here) | ( expr )
   (pprim ps ts)
    (? (! (two? (mark ts))) ()      ; the bottom of the expression ladder: mark here and a bad OPERAND reports itself, not the operator before it
       (: t <ts
          (? ((tkk t) = 'num) `(`('num (tkv t)) >ts)
             ((tkk t) = 'unum)                        ; a u-suffixed literal wears its unsignedness (neg/div/shift go unsigned):
              `(`('cast 'uint `('num (tkv t))) >ts)     ; 1u an unsigned INT (-1u wraps at 32 bits),
             ((tkk t) = 'ulnum)                                 ; 1UL an unsigned LONG -- and on t32, a
              (: v (tkv t)                                      ; value past the 32-bit word promotes to
                 uty (? (&& (pst32? ps) (|| (v < 0) (4294967295 < v))) 'ullong 'ulong)   ; ullong (C99 6.4.4.1)
                 `(`('cast uty `('num v)) >ts))
             ((tkk t) = 'ullnum)                                ; 1ULL: the suffix FORCES 64 bits on t32
              `(`('cast (? (pst32? ps) 'ullong 'ulong) `('num (tkv t))) >ts)
             ((tkk t) = 'llnum)                                 ; 1LL: llong on t32, plain elsewhere
              (? (pst32? ps) `(`('cast 'llong `('num (tkv t))) >ts) `(`('num (tkv t)) >ts))
             ((tkk t) = 'flo) `(`('flo (tkv t)) >ts)
             ; an imaginary literal (1.0fi, _Complex_I's spelling) is the complex
             ; constant {0, v} -- complex-double: complex FLOATS compute as
             ; doubles too (the float model), narrowing only at typed stores
             ((tkk t) = 'iflo)
              `(`('clit '(struct "__cplx_double") `('init `(`('flo 0.0) `('flo (tkv t))))) >ts)
             ((tkk t) = 'str) `(`('str (tkv t)) >ts)
             ; the varargs builtins (my stdarg.h maps va_* onto these): va_arg's
             ; second operand is a TYPE, so they can't ride the ordinary call path
             (&& ((tkk t) = 'id) ((tkv t) = "__builtin_va_arg"))
              (: r1 (want >ts "(")
                 ra (? (two? r1) (pasn ps r1) ())
                 r2 (? (two? ra) (want <>ra ",") ())
                 tr (? (two? r2) (pcty ps r2) ())
                 rest (? (two? tr) (want <>tr ")") ())
                 (? (two? rest) `(`('vaarg <ra <tr) rest) ()))
             (&& ((tkk t) = 'id) ((tkv t) = "__builtin_va_start"))
              (: r1 (want >ts "(")
                 ra (? (two? r1) (pasn ps r1) ())
                 r2 (? (two? ra) (want <>ra ",") ())
                 rb (? (two? r2) (pasn ps r2) ())         ; the last named param (unused)
                 rest (? (two? rb) (want <>rb ")") ())
                 (? (two? rest) `(`('vastart <ra) rest) ()))
             (&& ((tkk t) = 'id) ((tkv t) = "__builtin_va_end"))
              (: r1 (want >ts "(")
                 ra (? (two? r1) (pasn ps r1) ())
                 rest (? (two? ra) (want <>ra ")") ())
                 (? (two? rest) `(`('vaend <ra) rest) ()))
             ; va_copy: a va_list is an array of one tag, so the copy IS `*dst = *src`
             (&& ((tkk t) = 'id) ((tkv t) = "__builtin_va_copy"))
              (: r1 (want >ts "(")
                 ra (? (two? r1) (pasn ps r1) ())
                 r2 (? (two? ra) (want <>ra ",") ())
                 rb (? (two? r2) (pasn ps r2) ())
                 rest (? (two? rb) (want <>rb ")") ())
                 (? (two? rest) `(`('asn `('deref <ra) `('deref <rb)) rest) ()))
             ((tkk t) = 'id)
              (: ev (peep (ps 'enums) (tkv t) ())
                 (? (charm? ev) `(`('num ev) >ts)
                    `(`('var (tkv t)) >ts)))
             (tp? t "(") (: r (pexpr ps >ts)
                            (? (! (two? r)) ()
                               (: rest (want <>r ")")
                                  (? (two? rest) `(<r rest) ()))))
             ())))
   ; postfix: calls, indexing, members, ++/--
   (pargs ps ts acc)
    (? (peekp ts ")") `((rev acc) >ts)
       (: r (pasn ps ts)
          (? (! (two? r)) ()
             (peekp <>r ",") (pargs ps (cup (caup r)) (link <r acc))
             (: rest (want <>r ")")
                (? (two? rest) `((rev (link <r acc)) rest) ())))))
   ; the suffix run from a given seed -- ppost seeds with a primary; punary's
   ; compound literal seeds with the clit ((struct kstat){0}.st_atime_sec,
   ; (size_t [3]){..}[i] -- musl indexes and members straight off the literal)
   (ppostfrom ps e0 ts0)
    (: (more e ts2)
              (? (peekp ts2 "(")                     ; a call: the HEAD is any postfix expr (fn pointers)
                  (: ra (pargs ps >ts2 ())
                     (? (two? ra) (more `('call e <ra) <>ra) `(() ())))
                 (peekp ts2 "[")
                  (: ri (pexpr ps >ts2)
                     (? (! (two? ri)) `(() ())
                        (: rest (want <>ri "]")
                           (? (two? rest)
                              (more `('deref `('bin '+ e <ri)) rest)
                              `(() ())))))
                 (peekp ts2 ".")
                  (: t2 >ts2
                     (? (&& (two? t2) ((tkk <t2) = 'id))
                        (more `('dot e (tkv <t2)) >t2)
                        `(() ())))
                 (peekp ts2 "->")
                  (: t2 >ts2
                     (? (&& (two? t2) ((tkk <t2) = 'id))
                        (more `('dot `('deref e) (tkv <t2)) >t2)
                        `(() ())))
                 (peekp ts2 "++") (more `('post e 1) >ts2)
                 (peekp ts2 "--") (more `('post e -1) >ts2)
                 `(e ts2))
       m (more e0 ts0)
       (? (two? <m) m ()))
   (ppost ps ts)
    (: r (pprim ps ts)
       (? (! (two? r)) () (ppostfrom ps <r <>r)))
   ; unary (casts and sizeof(TYPE) fold here)
   (punary ps ts)
    (? (! (two? ts)) ()
       (: t <ts
          (? (&& (tp? t "(") (tkbty? ps (cap (pquals >ts))))    ; skip quals: (const T*)x is a cast, like the decl sites
              (: tr (pcty ps >ts)
                 rest (? (two? tr) (want <>tr ")") ())
                 (? (! (two? rest)) ()
                    (peekp rest "{")                    ; a compound literal (type){ init } -- an unnamed object,
                                                        ; and a postfix HEAD (member/index ride ppostfrom)
                     (: iv (pinit ps rest)
                        (? (two? iv) (ppostfrom ps `('clit <tr <iv) <>iv) ()))
                    (: r (punary ps rest)
                       (? (! (two? r)) ()
                          ; the complex cast lanes (C99 6.3.1.7, musl's creal/__CIMAG):
                          ; same complex passes through; a width flip converts
                          ; componentwise; real -> complex is {x, 0}; complex -> real
                          ; reads .re. (a non-lvalue operand evaluates once per
                          ; component read -- musl only casts vars and params.)
                          (: cty <tr
                             sty (ptype ps <r)
                             (? (cplxty? cty)
                                 (? (&& (cplxty? sty) ((caup sty) = (caup cty))) `(<r <>r)
                                    (cplxty? sty)
                                     `(`('clit cty `('init `(`('dot <r "re") `('dot <r "im")))) <>r)
                                    `(`('clit cty `('init `(<r `('flo 0.0)))) <>r))
                                (&& (cplxty? sty) (! (stru? cty)))
                                 `(`('cast cty `('dot <r "re")) <>r)
                                `(`('cast cty <r) <>r)))))))
             (tkw? t 'sizeof)
              (: r1 >ts
                 (? (&& (peekp r1 "(") (tkbty? ps (cap (pquals >r1))))    ; sizeof(const T) too
                     (: tr (pcty ps >r1)           ; sizeof ( type-name ) -- folds to a constant now
                        rest (? (two? tr) (want <>tr ")") ())
                        (? (! (two? rest)) ()
                           ; sizeof of a VLA TYPE (struct T[n+1], if_nameindex's malloc):
                           ; the runtime multiply dim * sizeof(elt)
                           (varr? <tr) `(`('bin '* (cauup (cap tr)) `('num (tsz ps (caup (cap tr))))) rest)
                           `(`('num (tsz ps <tr)) rest)))
                    (: r (punary ps r1)                 ; sizeof unary-expr | sizeof(expr) -- gen sizes from the type
                       (? (two? r) (: ty (ptype ps <r)
                                      (? (&& ty (! (varr? ty)))   ; a VLA's size is RUNTIME: defer to gen's szof
                                         `(`('num (tsz ps ty)) <>r) `(`('szof <r) <>r))) ()))))
             (tp? t "*") (: r (punary ps >ts)
                            (? (two? r) `(`('deref <r) <>r) ()))
             (tp? t "&") (: r (punary ps >ts)
                            (? (! (two? r)) ()
                               (: n (nulloff ps <r)   ; the offsetof idiom folds to a constant
                                  (? (charm? n) `(`('num n) <>r) `(`('addr <r) <>r)))))
             (tp? t "+") (punary ps >ts)
             (tp? t "-") (: r (punary ps >ts)
                            (? (two? r) `(`('un 'neg <r) <>r) ()))
             (tp? t "!") (: r (punary ps >ts)
                            (? (two? r) `(`('un 'not <r) <>r) ()))
             (tp? t "~") (: r (punary ps >ts)
                            (? (two? r) `(`('un 'bnot <r) <>r) ()))
             (tp? t "++") (: r (punary ps >ts)
                             (? (two? r) `(`('asn <r `('bin '+ <r `('num 1))) <>r) ()))
             (tp? t "--") (: r (punary ps >ts)
                             (? (two? r) `(`('asn <r `('bin '- <r `('num 1))) <>r) ()))
             (ppost ps ts))))
   ; the ladder
   (pmul ps ts) (plevel ps punary `(`("*" '*) `("/" '/) `("%" '%)) ts)
   (padd ps ts) (plevel ps pmul `(`("+" '+) `("-" '-)) ts)
   (pshift ps ts) (plevel ps padd `(`("<<" 'shl) `(">>" 'shr)) ts)
   (prel ps ts) (plevel ps pshift `(`("<" 'lt) `("<=" 'le) `(">" 'gt) `(">=" 'ge)) ts)
   (peq ps ts) (plevel ps prel `(`("==" 'eq) `("!=" 'ne)) ts)
   (pband ps ts) (plevel ps peq `(`("&" '&)) ts)
   (pbxor ps ts) (plevel ps pband `(`("^" '^)) ts)
   (pbor ps ts) (plevel ps pbxor `(`("|" '|)) ts)
   (pland ps ts)
    (: r (pbor ps ts)
       (? (! (two? r)) ()
          (: (more l ts2) (? (peekp ts2 "&&")
                             (: r2 (pbor ps >ts2)
                                (? (two? r2) (more `('land l <r2) <>r2) ()))
                             `(l ts2))
             (more <r <>r))))
   (plor ps ts)
    (: r (pland ps ts)
       (? (! (two? r)) ()
          (: (more l ts2) (? (peekp ts2 "||")
                             (: r2 (pland ps >ts2)
                                (? (two? r2) (more `('lor l <r2) <>r2) ()))
                             `(l ts2))
             (more <r <>r))))
   (pcond ps ts)
    (: r (plor ps ts)
       (? (! (two? r)) ()
          (peekp <>r "?")
           (: rt (pexpr ps (cup (caup r)))
              (? (! (two? rt)) ()
                 (: ts2 (want <>rt ":")
                    (? (! (two? ts2)) ()
                       (: re (pcond ps ts2)
                          (? (two? re)
                             `(`('cond <r <rt <re) <>re)
                             ()))))))
          r))
   (pasn ps ts)
    (: r (pcond ps ts)
       (? (! (two? r)) ()
          (: t (? (two? <>r) (cap (caup r)) ())
             hit (? (&& (two? t) ((tkk t) = 'p)) (passoc (tkv t) asnops) ())
             (? (! (two? hit)) r
                (: r2 (pasn ps (cup (caup r)))
                   (? (! (two? r2)) ()
                      (: lv <r op <>hit
                         `(`('asn lv (? op `('bin op lv <r2) <r2))
                            <>r2))))))))
   (pexpr ps ts)
    (: r (pasn ps ts)
       (? (! (two? r)) ()
          (: (more l ts2) (? (peekp ts2 ",")
                             (: r2 (pasn ps >ts2)
                                (? (two? r2) (more `('comma l <r2) <>r2) ()))
                             `(l ts2))
             (more <r <>r))))

   ; an initializer is an assignment-expr OR a brace list. a brace list is
   ; ('init (item..)); an item is a plain initializer, ('dfield name init),
   ; or ('didx n init) -- a designator CHAIN (.a.b = v, .a[i] = v, C99
   ; 6.7.9) parses as a path and dbuild nests it, coalescing a consecutive
   ; run that shares its head into ONE sub-init (musl's `.lim[0] = a,
   ; .lim[1] = b` fills one member's two elements -- split items would
   ; image the member twice, each zero-padding over the other). an
   ; initializer nests (array of struct, struct of array). trailing comma ok.
   (pinit ps ts)
    (? (peekp ts "{")
        (: (items ts2 acc)
            (? (peekp ts2 "}") `(`('init (dbuild (rev acc))) >ts2)
               (: it (pinititem ps ts2)
                  (? (! (two? it)) ()
                     (: elem <it ts3 <>it
                        (? (peekp ts3 ",") (items >ts3 (link elem acc))
                           (peekp ts3 "}") `(`('init (dbuild (rev (link elem acc)))) >ts3)
                           ())))))
           (items >ts ()))
       ; a scalar initializer that FOLDS is its number -- `(Bits>>3)`-style macro
       ; values image as constants (globals demand it; a local loses nothing)
       (: r (pasn ps ts)
          (? (! (two? r)) ()
             (: e <r v (cfold e)
                (? (cint? v) `(`('num v) <>r)            ; cint?: a sun-sized fold (max-charm's image) is still a number
                   (&& (<e = 'un)
                       (&& (<>e = 'neg) ((cap (cauup e)) = 'flo)))
                    `(`('flo (0.0 - (caup (cauup e)))) <>r)   ; -1.0 in a global image: fold the neg into the literal
                   r)))))   ; cint?: a sun-sized fold (max-charm's image) is still a number
   ; how many array elements an initializer implies (for [] size inference):
   ; a string is its bytes + NUL; a brace list is the highest index reached
   ; (designated [k] sets the cursor to k). () if it implies nothing. `elt` is the
   ; element type: when it's an array and the items are a plain elided RUN (no braces,
   ; no designators), the run fills rows of `arrleaves elt` scalars each -- int a[][2] =
   ; {1,2,3,4} is 2 rows, not 4 -- so divide (C 6.7.9).
   (arrleaves ty) (? (arr? ty) (* <>>ty (arrleaves <>ty)) 1)   ; scalar leaves of a (nested) array
   (allplain? items) (foldl (\ ok it (&& ok (! (member? <it '(init didx dfield))))) 1 items)
   (initcount elt ini)
    (? (&& (two? ini) (<ini = 'str)) ((tally <>ini) + 1)
       (&& (two? ini) (<ini = 'init))
        (: items <>ini
           ; { "s" } sizing a char array (6.7.9p14): the string's own count,
           ; not one item (a lone str on a POINTER elt stays one: char *p[])
           (? (&& (two? items)
                  (&& (! (two? >items))
                      (&& (two? <items) (&& ((cap <items) = 'str) (! (ptr? elt))))))
               ((tally (caup <items)) + 1)
              (&& (arr? elt) (&& (two? items) (allplain? items)))
               (: L (arrleaves elt) (? (< 0 L) (// ((tally items) + (L - 1)) L) (tally items)))
               (: (go l i mx)
                   (? (! (two? l)) mx
                      (: it <l
                         ci (? (<it = 'didx) <>it i)
                         (go >l (ci + 1) (? (mx < (ci + 1)) (ci + 1) mx))))
                  (go items 0 0))))
       ())
   ; a designator path: (.name | [const-expr])+ then `=` -> ((('f name)|('i n)..) rest) | ()
   (pdesigs ps ts acc)
    (? (peekp ts ".")
        (: t2 >ts
           (? (! (&& (two? t2) ((tkk <t2) = 'id))) ()
              (pdesigs ps >t2 (link `('f (tkv <t2)) acc))))
       (peekp ts "[")
        (: er (pexpr ps >ts)                         ; a designator index: any const-expr ([KMint]=.., [2]=..)
           (? (! (two? er)) ()
              (: n (cfold <er)
                 r1 (want <>er "]")
                 (? (&& (charm? n) (two? r1)) (pdesigs ps r1 (link `('i n) acc)) ()))))
       (! (two? acc)) ()
       (: r (want ts "=")
          (? (two? r) `((rev acc) r) ())))
   ; the item stream carries (path value) pairs (path () = a plain item); dbuild
   ; folds them into 'dfield/'didx items, one nested init per consecutive
   ; same-head run, the value DIRECT when a lone path exhausts (today's shape)
   (dbuild prs)
    (? (! (two? prs)) ()
       (: pv <prs path (cap pv) v (caup pv)
          (? (! (two? path)) (link v (dbuild >prs))
             (: h <path
                (grab l acc)                         ; consecutive pairs whose head = h, stripped
                 (? (! (two? l)) `((rev acc) ())
                    (: p2 (cap <l)
                       (? (&& (two? p2) (= <p2 h))
                          (grab >l (link `((cup p2) (caup <l)) acc))
                          `((rev acc) l))))
                gr (grab >prs `(`(>path v)))
                subs <gr
                sub (dbuild subs)
                node (? (&& (! (two? >subs)) (! (two? (cap <subs)))) <sub `('init sub))
                (link `((? (id? <h 'f) 'dfield 'didx) <>h node) (dbuild <>gr))))))
   (pinititem ps ts)                                 ; -> ((path value) rest) | ()
    (? (|| (peekp ts ".") (peekp ts "["))
        (: dr (pdesigs ps ts ())
           (? (! (two? dr)) ()
              (: v (pinit ps <>dr)
                 (? (two? v) `(`(<dr <v) <>v) ()))))
       (: v (pinit ps ts)
          (? (two? v) `(`(() <v) <>v) ())))

   ; a constant-expression folder (for _Static_assert): an integer | () if the
   ; expression is not a compile-time constant. sizeof/enum already folded to num.
   ; values are 64-bit machine PATTERNS (love's shifts and bitops wrap at the word,
   ; so charms/suns fold like the hardware); a NARROW cast masks to its width
   ; (sign-extending the signed ones), and >> under an unsigned LEFT goes LOGICAL
   ; via mask -- (uintptr_t)-1 >> 2 is max-charm, not -1 (the arithmetic-shift
   ; fold pinned max-charm -1 and flipped spec.l's `word`). uext? is the
   ; parse-level unsignedness read: an unsigned cast, or a bin riding one on its
   ; left -- an approximation, honest for the putcharm-style word math images use.
   (cint? x) (|| (charm? x) (|| (sun? x) (big? x)))
   (uext? e) (? (! (two? e)) ()
                (<e = 'cast) (member? <>e '(uchar ushort uint ulong ullong))
                (<e = 'bin) (uext? <>>e)
                ())
   ; the folder carries no types, so it reads them off the CASTS the literal
   ; lane already emits (`1u` parses as ('cast 'uint ..)). that is enough to
   ; answer the one question that matters here: is this expression's C type
   ; `unsigned int`, whose `+ - * <<` must WRAP at 32 bits where a love integer
   ; would just keep growing? it is, when a uint reaches the root and nothing
   ; 64-bit does. everything else folds unbounded exactly as before -- signed
   ; overflow is UB, and a 64-bit result already IS the word pattern.
   (w64c? e) (? (! (two? e)) ()
                (<e = 'cast) (member? <>e '(long ulong llong ullong))
                (<e = 'bin) (|| (w64c? <>>e) (w64c? <>>>e))
                (<e = 'un) (w64c? <>>e)
                ())
   (uic? e) (? (! (two? e)) ()
               (<e = 'cast) (id? <>e 'uint)
               (<e = 'bin) (|| (uic? <>>e) (uic? <>>>e))
               (<e = 'un) (uic? <>>e)
               ())
   (u32w e v) (? (&& (uic? e) (! (w64c? e))) (& v 4294967295) v)
   (cfold e)
    (? (! (two? e)) ()
       (<e = 'num) <>e
       (<e = 'bin)
        (: op <>e a (cfold <>>e) b (cfold <>>>e)
           (? (|| (! (cint? a)) (! (cint? b))) ()
              (op = '+) (u32w e (a + b)) (op = '-) (u32w e (a - b)) (op = '*) (u32w e (* a b))
              (op = '/) (? (b = 0) () (// a b)) (op = '%) (? (b = 0) () (a - (* (// a b) b)))
              (op = 'eq) (? (a = b) 1 0) (op = 'ne) (? (a = b) 0 1)
              (op = 'lt) (? (a < b) 1 0) (op = 'le) (? (a <= b) 1 0)
              (op = 'gt) (? (b < a) 1 0) (op = 'ge) (? (b <= a) 1 0)
              (op = '&) (& a b) (op = '|) (| a b) (op = '^) (^ a b)
              (op = 'shl) (u32w e (<< a b))
              (op = 'shr) (? (uext? <>>e) (& (>> a b) ((<< 1 (64 - b)) - 1)) (>> a b))
              ()))
       (<e = 'un)
        (: v (cfold <>>e)
           (? (! (cint? v)) () (<>e = 'neg) (0 - v)
              (<>e = 'not) (? (v = 0) 1 0) (<>e = 'bnot) ((0 - v) - 1) ()))
       (<e = 'cast)
        (: v (cfold <>>e) ty <>e
           (? (! (cint? v)) v                       ; non-integer / non-const: fold through
              (ty = 'uchar) (& v 255)
              (ty = 'ushort) (& v 65535)
              (ty = 'uint) (& v 4294967295)
              (member? ty '(char short int))
               (: w (? (ty = 'char) 8 (ty = 'short) 16 32)
                  r (& v ((<< 1 w) - 1))
                  (? (r < (<< 1 (w - 1))) r (r - (<< 1 w))))
              v))                                   ; word-sized: the pattern is the value
       ; land/lor are 3-element ('land l r), so l=<>e r=<>>e (NOT the 4-element bin accessors);
       ; truthiness is != 0, not > 0 -- a negative operand is true in C (-1 && 1 == 1).
       (<e = 'land) (: a (cfold <>e) b (cfold <>>e)
                            (? (|| (! (cint? a)) (! (cint? b))) () (? (&& (! (a = 0)) (! (b = 0))) 1 0)))
       (<e = 'lor) (: a (cfold <>e) b (cfold <>>e)
                           (? (|| (! (cint? a)) (! (cint? b))) () (? (|| (! (a = 0)) (! (b = 0))) 1 0)))
       ; ternary ('cond c a b): fold only the SELECTED arm (C11 6.6) -- the dead arm need not be
       ; constant (or even defined: 1 ? 0 : 1/0 is a valid constant expression).
       (<e = 'cond) (: c (cfold <>e)
                            (? (! (cint? c)) ()
                               (: v (? (c = 0) (cfold <>>>e) (cfold <>>e))
                                  (? (cint? v) v ()))))
       ())
   ; _Static_assert ( const-expr , "message" ) ;  -> (('tdef) rest) | () -- an
   ; assertion that FOLDS to false fails the compile; a non-constant one is let by.
   (pstatic ps ts)
    (: r1 (want >ts "(")
       re (? (two? r1) (pcond ps r1) ())
       r2 (? (two? re) (want <>re ",") ())
       r3 (? (&& (two? r2) ((tkk <r2) = 'str)) >r2 ())
       r4 (? (two? r3) (want r3 ")") ())
       rest (? (two? r4) (want r4 ";") ())
       (? (! (two? rest)) ()
          (: v (cfold <re)
             (? (&& (charm? v) (v <= 0)) () `(`('tdef) rest)))))
   (static? ts) (&& (two? ts) (&& ((tkk <ts) = 'id) ((tkv <ts) = "_Static_assert")))

   ; decl := TYPE [dtor [= asn] (, dtor [= asn])*] ;
   (pdecl ps ts)
    (: br (pbty ps ts)
       stat (leadstatic? ts)                          ; block-scope `static` -> ('sdecl ..), laid in .bss
       (? (! (two? br)) ()
          (: base <br ts0 <>br
             (? (peekp ts0 ";") `(`('decl ()) >ts0)   ; a bare definition (struct S {..};)
                (: (one ts2)
                    (: dr (pdtor ps ts2 base)
                       (? (! (two? dr)) ()
                          (: name (cap (cap dr)) ty0 (caup (cap dr))
                             ; a VLA local: stamp the decl SITE's slot index onto the
                             ; type (gen's prescan keys its ptr/size slots by it); an
                             ; initializer or a `static` VLA refuses (invalid C)
                             ty (? (! (varr? ty0)) ty0
                                   (: ix (peep ps 'vlan 0)
                                      _ (pin ps 'vlan (ix + 1))
                                      (ty0 + `(ix))))
                             ts3 <>dr
                             (? (&& (varr? ty) (|| stat (peekp ts3 "="))) ()
                                (peekp ts3 "=")
                                (: r (pinit ps >ts3)
                                   (? (! (two? r)) ()
                                      (: ini <r
                                         ty2 (? (&& (arr? ty) (&& (<>>ty = 0) (charm? (initcount <>ty ini))))
                                                `('arr <>ty (initcount <>ty ini)) ty)   ; [] local: infer size
                                         `(`(name ty2 ini) <>r))))
                                `(`(name ty ()) ts3)))))
                   (more ts2 acc)
                    (: r (one ts2)
                       (? (! (two? r)) ()
                          (peekp <>r ",") (more (cup (caup r)) (link <r acc))
                          (: rest (want <>r ";")
                             (? (two? rest) `(`((? stat 'sdecl 'decl) (rev (link <r acc))) rest) ()))))
                   (more ts0 ()))))))

   ; asm [volatile] ( "template" [: outputs [: inputs [: clobbers]]] ) ; -- the
   ; GNU STATEMENT form (basic and extended), the template in holo's neutral
   ; TEXT (crew/holo/text.l; gen assembles it). adjacent template strings
   ; concatenate (multi-line templates). operands ride as ((constraint expr)..),
   ; clobbers as (string..) -- the constraint grammar is gen's business.
   ; -> (('asm tmpl outs ins clobs) rest) | ()
   (asmid? t) (&& (two? t) (&& ((tkk t) = 'id)
                                (member? (tkv t) `("asm" "__asm__" "__asm"))))
   (pasm ps ts)
    (: r >ts
       t1 (? (&& (two? r) (|| (tkw? <r 'volatile)
                              (&& ((tkk <r) = 'id) ((tkv <r) = "__volatile__"))))
             >r r)
       r0 (want t1 "(")
       (? (! (&& (two? r0) ((tkk <r0) = 'str))) ()
          (: (tmpl ts2 acc) (? (&& (two? ts2) ((tkk <ts2) = 'str))
                               (tmpl >ts2 (acc + (tkv <ts2)))
                               `(acc ts2))
             ; an operand run: "constraint" ( expr ) , .. -- empty before : or )
             (ops ts2 acc)
              (? (! (&& (two? ts2) ((tkk <ts2) = 'str))) `((rev acc) ts2)
                 (: c (tkv <ts2)
                    r1 (want >ts2 "(")
                    re (? (two? r1) (pexpr ps r1) ())
                    r2 (? (two? re) (want <>re ")") ())
                    (? (! (two? r2)) ()
                       (peekp r2 ",") (ops >r2 (link `(c <re) acc))
                       `((rev (link `(c <re) acc)) r2))))
             (clobs ts2 acc)
              (? (! (&& (two? ts2) ((tkk <ts2) = 'str))) `((rev acc) ts2)
                 (: c (tkv <ts2)
                    (? (peekp >ts2 ",") (clobs >>ts2 (link c acc))
                       `((rev (link c acc)) >ts2))))
             tr (tmpl r0 "")
             s1 (? (peekp <>tr ":") (ops (cup (caup tr)) ()) `(() <>tr))
             (? (! (two? s1)) ()
                (: s2 (? (peekp <>s1 ":") (ops (cup (caup s1)) ()) `(() <>s1))
                   (? (! (two? s2)) ()
                      (: s3 (? (peekp <>s2 ":") (clobs (cup (caup s2)) ()) `(() <>s2))
                         (? (! (two? s3)) ()
                            (: r9 (want <>s3 ")")
                               rest (? (two? r9) (want r9 ";") ())
                               (? (two? rest) `(`('asm <tr <s1 <s2 <s3) rest) ()))))))))))

   (pstmt ps ts)
    (? (! (two? ts)) ()
       (static? ts) (pstatic ps ts)
       (: t <ts
          (? (tkw? t 'return)
              (? (peekp >ts ";") `(`('ret ()) >>ts)    ; bare `return;` -- a void return
                 (: r (pexpr ps >ts)
                    (? (! (two? r)) ()
                       (: rest (want <>r ";")
                          (? (two? rest) `(`('ret <r) rest) ())))))
             (tkw? t 'if)
              (: r1 (want >ts "(")
                 rc (? (two? r1) (pexpr ps r1) ())
                 r2 (? (two? rc) (want <>rc ")") ())
                 rt (? (two? r2) (pstmt ps r2) ())
                 (? (! (two? rt)) ()
                    (? (&& (two? <>rt) (tkw? (cap (caup rt)) 'else))
                       (: re (pstmt ps (cup (caup rt)))
                          (? (two? re)
                             `(`('if <rc <rt <re) <>re)
                             ()))
                       `(`('if <rc <rt ()) <>rt))))
             (tkw? t 'while)
              (: r1 (want >ts "(")
                 rc (? (two? r1) (pexpr ps r1) ())
                 r2 (? (two? rc) (want <>rc ")") ())
                 rb (? (two? r2) (pstmt ps r2) ())
                 (? (two? rb) `(`('while <rc <rb) <>rb) ()))
             (tkw? t 'do)
              (: rb (pstmt ps >ts)
                 (? (! (two? rb)) ()
                    (! (&& (two? <>rb) (tkw? (cap (caup rb)) 'while))) ()
                    (: r1 (want (cup (caup rb)) "(")
                       rc (? (two? r1) (pexpr ps r1) ())
                       r2 (? (two? rc) (want <>rc ")") ())
                       rest (? (two? r2) (want r2 ";") ())
                       (? (two? rest) `(`('do <rb <rc) rest) ()))))
             (tkw? t 'for)
              (: r1 (want >ts "(")
                 (? (! (two? r1)) ()
                    (: ri (? (tkbty? ps (cap (pquals r1))) (pdecl ps r1)   ; the decl gate skips quals, like a block's (`for (const char *x; ..`)
                             (peekp r1 ";") `(() >r1)
                             (: r (pexpr ps r1)
                                (? (two? r)
                                   (: rest (want <>r ";")
                                      (? (two? rest) `(`('expr <r) rest) ()))
                                   ())))
                       (? (! (two? ri)) ()
                          (: rc (? (peekp <>ri ";") `(() (cup (caup ri)))
                                   (: r (pexpr ps <>ri)
                                      (? (two? r)
                                         (: rest (want <>r ";")
                                            (? (two? rest) `(<r rest) ()))
                                         ())))
                             (? (! (two? rc)) ()
                                (: rs (? (peekp <>rc ")") `(() (cup (caup rc)))
                                         (: r (pexpr ps <>rc)
                                            (? (two? r)
                                               (: rest (want <>r ")")
                                                  (? (two? rest) `(<r rest) ()))
                                               ())))
                                   (? (! (two? rs)) ()
                                      (: rb (pstmt ps <>rs)
                                         (? (two? rb)
                                            `(`('for <ri <rc <rs <rb) <>rb)
                                            ()))))))))))
             (tkw? t 'switch)
              (: r1 (want >ts "(")
                 rc (? (two? r1) (pexpr ps r1) ())
                 r2 (? (two? rc) (want <>rc ")") ())
                 r3 (? (two? r2) (want r2 "{") ())
                 (? (! (two? r3)) ()
                    (: (items ts2 acc)
                        (? (! (two? ts2)) ()
                           (tp? <ts2 "}") `((rev acc) >ts2)
                           (tkw? <ts2 'case)               ; case CONST-EXPR : -- parse + fold (EOF -> (-1), enum consts, 1<<4)
                            (: rv (pexpr ps >ts2)
                               (? (! (two? rv)) ()
                                  (: v (cfold <rv)
                                     (? (! (charm? v)) ()
                                        (: rest (want <>rv ":")
                                           (? (two? rest) (items rest (link `('case v) acc)) ()))))))
                           (tkw? <ts2 'default)
                            (: rest (want >ts2 ":")
                               (? (two? rest) (items rest (link `('dflt) acc)) ()))
                           (: r (? (tkbty? ps (cap (pquals ts2))) (pdecl ps ts2) (pstmt ps ts2))
                              (? (two? r) (items <>r (link <r acc)) ())))
                       ir (items r3 ())
                       (? (two? ir) `(`('switch <rc <ir) <>ir) ()))))
             (tkw? t 'goto)
              (: t2 >ts
                 (? (! (&& (two? t2) ((tkk <t2) = 'id))) ()
                    (: rest (want >t2 ";")
                       (? (two? rest) `(`('goto (tkv <t2)) rest) ()))))
             (tkw? t 'break)
              (: rest (want >ts ";") (? (two? rest) `(`('brk) rest) ()))
             (tkw? t 'continue)
              (: rest (want >ts ";") (? (two? rest) `(`('cont) rest) ()))
             ; case/default are LABELS -- legal at any statement depth, not just as a
             ; direct child of the switch body (the switch handler above catches the
             ; top-level ones; these catch the nested coroutine-style ones -- bzip2's
             ; GET_BITS drops `case lll:` inside a while, Duff's-device style). gen ties
             ; each back to its nearest enclosing switch by case VALUE (g 'cases).
             (tkw? t 'case)
              (: rv (pexpr ps >ts)
                 (? (! (two? rv)) ()
                    (: v (cfold <rv)
                       (? (! (charm? v)) ()
                          (: rest (want <>rv ":")
                             (? (two? rest) `(`('case v) rest) ()))))))
             (tkw? t 'default)
              (: rest (want >ts ":") (? (two? rest) `(`('dflt) rest) ()))
             (asmid? t) (pasm ps ts)     ; before the call/label fallthroughs: asm( is a statement
             (tp? t "{") (pblock ps ts)
             (tp? t ";") `(`('nop) >ts)
             ; a label: id ':' (a ternary's id is followed by '?', never ':')
             (&& ((tkk t) = 'id) (tp? (? (two? >ts) <>ts ()) ":"))
              `(`('lbl (tkv t)) >>ts)
             (: r (pexpr ps ts)
                (? (! (two? r)) ()
                   (: rest (want <>r ";")
                      (? (two? rest) `(`('expr <r) rest) ())))))))
   ; block-scoped shadowing (C scope): a local variable whose NAME is a typedef or
   ; an ENUM CONSTANT hides it for the rest of the block -- so a later `name op name`
   ; reads as an expression (not a cast), and a read of the local stays a var (not a
   ; folded number, which would also refuse as an lvalue). record (table name old);
   ; a typedef pins to 0 (not-a-type), an enum constant is PULLED (the fold site
   ; tests charm?, and 0 is a charm); restore on block exit. love.h makes `num`/`word`
   ; typedefs and love.c reuses `N` as a local over `enum { N = 13 }`.
   (declnames form) (? (&& (two? form) (<form = 'decl)) (map cap <>form) ())
   (shadow1 ps nm shadows)
    (: s1 (: old (peep (ps 'types) nm 'none)
             (? (old = 'none) shadows
                (: _ (pin (ps 'types) nm 0) (link `('types nm old) shadows))))
       olde (peep (ps 'enums) nm 'none)
       (? (olde = 'none) s1
          (: _ (pull (ps 'enums) nm 0) (link `('enums nm olde) s1))))
   (shadowdecl ps names shadows) (? (! (two? names)) shadows
                                     (shadowdecl ps >names (shadow1 ps <names shadows)))
   (unshadow ps shadows) (? (! (two? shadows)) ()
                            (: e <shadows
                               _ (? (<>>e = 'none) (pull (ps <e) <>e 0) (pin (ps <e) <>e <>>e))   ; 'none = the name was ABSENT (a fresh local): remove it, don't pin the sentinel
                               (unshadow ps >shadows)))
   ; record a local/param name -> type into ps 'locals, saving the old binding
   ; ('none if fresh) on the block's shadow list so unshadow restores C scope.
   ; ent is (name ty ..) -- a decl entry (name ty ini) or a param (name ty).
   (bindloc ps ent shadows) (: nm <ent
                                old (peep (ps 'locals) nm 'none)
                                _ (pin (ps 'locals) nm <>ent)
                                (link `('locals nm old) shadows))
   (bindlocs ps ents shadows) (? (! (two? ents)) shadows (bindlocs ps >ents (bindloc ps <ents shadows)))
   (loctypes form) (? (&& (two? form) (member? <form '(decl sdecl))) <>form ())   ; a block decl's entries, () otherwise
   ; a BLOCK-scope typedef (sqlite's LOGFUNC_t): pin each declarator into ps
   ; 'types like tdeflist, but SAVE the old binding on the block's shadow list
   ; ('none = fresh) so unshadow restores C scope. -> (rest sh2) | (); the
   ; typedef is a pure parse-time binding, no AST form.
   (btdefs ps base ts sh)
    (: dr (pdtor ps ts base)
       (? (! (two? dr)) ()
          (: nm (cap (cap dr))
             old (peep (ps 'types) nm 'none)
             _ (pin (ps 'types) nm (caup (cap dr)))
             sh2 (link `('types nm old) sh)
             af <>dr
             (? (peekp af ",") (btdefs ps base >af sh2)
                (peekp af ";") `(>af sh2)
                ()))))
   (pblock ps ts)
    (: ts0 (want ts "{")
       (? (! (two? ts0)) ()
          (: (items ts2 acc sh)
              (? (! (two? ts2)) (: _ (unshadow ps sh) ())
                 (tp? <ts2 "}") (: _ (unshadow ps sh) `(`('blk (rev acc)) >ts2))
                 (tkw? <ts2 'typedef)
                  (: br (pbty ps >ts2)
                     r (? (two? br) (btdefs ps <br <>br sh) ())
                     (? (! (two? r)) (: _ (unshadow ps sh) ())
                        (items <r acc <>r)))
                 (: r (? (tkbty? ps (cap (pquals ts2))) (pdecl ps ts2) (pstmt ps ts2))
                    (? (! (two? r)) (: _ (unshadow ps sh) ())
                       (items <>r (link <r acc) (bindlocs ps (loctypes <r) (shadowdecl ps (declnames <r) sh))))))
             (items ts0 () ()))))

   ; params := void | TYPE '*'* id (, ..)* [, ...]   -> (entries rest variadic?)
   (pparams ps ts acc)
    (? (peekp ts ")") `((rev acc) >ts ())
       (&& (two? ts) (&& (tkw? <ts 'void) (peekp >ts ")"))) `(() >>ts ())
       (! (tkbty? ps (cap (pquals ts)))) ()
       (: br (pbty ps ts)
          (? (! (two? br)) ()
             (: dr (pdtor ps <>br <br)
                (? (! (two? dr)) ()
                   ; a parameter of ARRAY type decays to a pointer (the C rule) --
                   ; so `int a[]`, and a va_list (an array typedef), ride as pointers
                   (: e0 <dr ty0 <>e0
                      entry (? (arr? ty0) `(<e0 `('ptr <>ty0)) e0)
                      ts3 <>dr
                      (? (peekp ts3 ",")
                          (? (peekp >ts3 "...")              ; ... after this param -> variadic
                              (: rest (want >>ts3 ")")
                                 (? (two? rest) `((rev (link entry acc)) rest 1) ()))
                             (pparams ps >ts3 (link entry acc)))
                         (: rest (want ts3 ")")
                            (? (two? rest) `((rev (link entry acc)) rest ()) ())))))))))

   ; -- K&R (old-style) definitions: `f(a, b) int a; char *b; { .. }`. the parens
   ; hold a bare IDENTIFIER list; the parameter TYPES follow as declarations before
   ; the `{` (an undeclared id defaults to int, C's rule). answers the SAME
   ; (entries rest variadic?) shape pparams does, with rest AT the `{` -- so the fn
   ; path flows straight into pblock. protos stay ANSI (OF() expands with types), so
   ; a bare non-type id after `(` is the sole K&R tell.
   (krhead? ps ts) (&& (two? ts) (&& ((tkk <ts) = 'id) (! (tkbty? ps <ts))))
   (kids ts acc)                                     ; the identifier list -> (names rest-after-`)`)
    (? (&& (two? ts) ((tkk <ts) = 'id))
        (: nm (tkv <ts) r >ts
           (? (peekp r ",") (kids >r (link nm acc))
              (peekp r ")") `((rev (link nm acc)) >r)
              ()))
       ())
   (klook nm l) (? (! (two? l)) 'int ((cap (cap l)) = nm) (caup (cap l)) (klook nm >l))
   (kdtors ps ts base acc)                           ; `dtor (, dtor)* ;` -> (name ty) pairs, K&R arrays decay to ptr
    (: dr (pdtor ps ts base)
       (? (! (two? dr)) ()
          (: nm (cap (cap dr)) ty0 (caup (cap dr)) rest <>dr
             ty (? (arr? ty0) `('ptr <>ty0) ty0)
             acc2 (link `(nm ty) acc)
             (? (peekp rest ",") (kdtors ps >rest base acc2)
                (peekp rest ";") (kdecls ps >rest acc2)
                ()))))
   (kdecls ps ts acc)                                ; the run of parameter declarations up to `{` -> (tmap rest)
    (? (peekp ts "{") `(acc ts)
       (: br (pbty ps ts)
          (? (! (two? br)) () (kdtors ps <>br <br acc))))
   (pkr ps ts)                                       ; ts after `(` -> (entries rest-at-`{` ()) | ()
    (: kr (kids ts ())
       (? (! (two? kr)) ()
          (: names <kr afterp <>kr
             dr (kdecls ps afterp ())
             (? (! (two? dr)) ()
                (: tmap <dr rest <>dr
                   `((map (\ nm `(nm (klook nm tmap))) names) rest ()))))))

   ; a comma-list of `*.. name (params)` function prototypes sharing one base type.
   ; each sibling's OWN stars ride into its sig's return slot -- dropping them read
   ; `struct ai *ai_strof(..)` as a struct-by-value return, and every host-TU call
   ; to an mproto sibling refused (invisible from love.c: the DEFINITIONS re-pin).
   ; registers each sig; answers (L rest-after-`;`) | ().
   (mproto ps base ts)
    (: (stars s ty) (: s2 (pquals s) (? (peekp s2 "*") (stars >s2 `('ptr ty)) `(ty s2)))
       sr (stars ts base)
       rty <sr s0 <>sr
       (? (! (&& (two? s0) ((tkk <s0) = 'id))) ()
          (: nm (tkv <s0) s1 >s0
             (? (! (peekp s1 "(")) ()
                (: rp (pparams ps >s1 ())
                   (? (! (two? rp)) ()
                      (: _ (pin (ps 'sigs) nm `(rty (map caup <rp) <>>rp))
                         af <>rp
                         (? (peekp af ",") (mproto ps base >af)
                            ; answer WRAPPED (L rest): a bare rest conflates the
                            ; empty remainder (a proto list at EOF) with refusal
                            (peekp af ";") `(>af)
                            ()))))))))

   ; a comma-list of typedef declarators sharing one base type (`typedef T a, b;`);
   ; pins each name -> its declared type. answers ('tdef) rest | ().
   (tdeflist ps base ts)
    (: dr (pdtor ps ts base)
       (? (! (two? dr)) ()
          (: _ (pin (ps 'types) (cap (cap dr)) (caup (cap dr)))
             af <>dr
             (? (peekp af ",") (tdeflist ps base >af)
                ; peek for the `;`: the remainder may be EMPTY (a typedef at EOF)
                (peekp af ";") `(`('tdef) >af)
                ()))))

   ; the top level: typedefs, functions, globals, bare type definitions.
   ; staticness + the section attribute are noted at ENTRY (the attribute-skip
   ; lanes recurse, so the outermost sighting wins) and pinned per name by tops.
   (ptop ps ts)
    (: _ (? (peep ps 'curstat ()) () (? (leadstatic? ts) (pin ps 'curstat 1) ()))
       _ (? (peep ps 'cursect ()) () (: sc (secat? ts) (? sc (pin ps 'cursect sc) ())))
       _ (? (peep ps 'curalias ()) () (: al (aliasat? ts) (? al (pin ps 'curalias al) ())))
       _ (? (peep ps 'curweak ()) () (? (wkany? ts) (pin ps 'curweak 1) ()))
       (ptop9 ps ts))
   (ptop9 ps ts)
    (? (peekp ts ";") `(`('tdef) >ts)          ; a stray top-level `;` -- an empty declaration
       (static? ts) (pstatic ps ts)
       ; a leading __attribute__((weak)): parse past it and record the function's
       ; name -- the .o emitter binds its symbol WEAK (a host override wins at link)
       (two? (weakat? ts))
        (: r (ptop ps (weakat? ts))
           (? (! (two? r)) ()
              (: _ (? ((cap (cap r)) = 'fn) (pin ps 'weaks (link (caup (cap r)) (ps 'weaks))) ())
                 r)))
       ; any OTHER leading __attribute__((..)) (noinline, section, ..): a hint
       ; the codegen owes nothing -- skip it and parse the declaration proper
       (two? (anyat? ts)) (ptop ps (anyat? ts))
       ; `extern` data DECLARES, never defines: an init-less gdecl under a leading
       ; extern becomes ('xdecl pairs) -- addressable, no storage laid (the linker
       ; binds the defining object's symbol). protos/typedefs pass through untouched.
       (tkw? <ts 'extern)
        (: r (ptop ps >ts)
           (? (! (two? r)) ()
              (! ((cap (cap r)) = 'gdecl)) r
              (: (initless? ps2) (? (! (two? ps2)) 1
                                    (two? (cauup (cap ps2))) ()
                                    (initless? >ps2))
                 (? (initless? (caup (cap r))) `(`('xdecl (caup (cap r))) <>r) r))))
       (tkw? <ts 'typedef)
        (: br (pbty ps >ts)
           (? (! (two? br)) () (tdeflist ps <br <>br)))
       (: br (pbty ps ts)
          (? (! (two? br)) ()
             (: base <br ts0 <>br
                (? (peekp ts0 ";") `(`('tdef) >ts0)    ; a bare struct/enum definition
                   (: (stars ts2 ty) (: s (pquals ts2) (? (peekp s "*") (stars >s `('ptr ty)) `(ty s)))
                      sr (stars ts0 base)
                      rty <sr ts1 (pquals <>sr)
                      ; the first declarator: a plain name, or a paren fn-pointer
                      ; `(*name)(params)` (love.c's ai_image_absguard) -- routed
                      ; through pdtor like the comma declarators below; the
                      ; leading stars already rode into rty. -> (name rest ty),
                      ; ty () on the plain path (the declarator's type is rty)
                      fdr (? (&& (two? ts1) ((tkk <ts1) = 'id)) `((tkv <ts1) >ts1 () ())
                             (peekp ts1 "(")
                              (: dr (pdtor ps ts1 rty)
                                 (? (! (two? dr)) ()
                                    (! (cap (cap dr))) ()             ; abstract at top level: refuse
                                    `((cap (cap dr)) <>dr (caup (cap dr)) (cauup (cap dr)))))   ; 4th = a fn declarator's parsed params
                             ())
                      (? (! (two? fdr)) ()
                         (: name <fdr
                            ts2 <>fdr
                            fpty (cauup fdr)
                            fprm <>>>fdr                              ; the inner params when the declarator IS a function
                            (? (&& (! (two? fpty)) (peekp ts2 "("))   ; a function (the return type rides the sig table)
                               (: rp (? (krhead? ps >ts2) (pkr ps >ts2) (pparams ps >ts2 ()))
                                  (? (! (two? rp)) ()
                                     (: _ (pin (ps 'sigs) name `(rty (map caup <rp) <>>rp))
                                        aft (patt <>rp)                     ; a trailing __attribute__((..)) after the proto/fn declarator
                                        (? (peekp aft ";") `(`('proto name) (cup aft))
                                           ; more comma-separated declarators SHARING base -- each `* nm (params)`
                                           ; is another prototype (register its sig); ends at `;`. (`void *malloc
                                           ; (size_t), free(void*)` / love.h's `struct ai *ai_ini(void), *ai_ini_m(..)`)
                                           (peekp aft ",")
                                            (: rest (mproto ps base (cup aft))
                                               (? (two? rest) `(`('proto name) <rest) ()))
                                           (: psh (bindlocs ps <rp ())   ; params typed for sizeof folding; restore on fn exit
                                              b (pblock ps aft)
                                              _ (unshadow ps psh)
                                              (? (two? b) `(`('fn name <rp <b) <>b) ()))))))
                               (fn? fpty)                                ; the declarator IS a function (returns a fn-ptr):
                                                                         ; the return type is <>fpty, the params rode fprm out of pdtor
                                (: _ (pin (ps 'sigs) name `(<>fpty (map caup <fprm) <>>fprm))
                                   aft (patt ts2)
                                   (? (peekp aft ";") `(`('proto name) (cup aft))
                                      (: psh (bindlocs ps <fprm ())
                                         b (pblock ps aft)
                                         _ (unshadow ps psh)
                                         (? (two? b) `(`('fn name <fprm <b) <>b) ()))))
                               ; a global declaration; the first declarator's stars came off `sr`
                               (: (one ts3 nm t2)                        ; t2 is the element type (stars included)
                                   (? (peekp ts3 "[")
                                      (: dr (adims ps ts3 ())  ; one or more [n]/[] dims (enum consts fold)
                                         (? (! (two? dr)) ()
                                            (: dims <dr rest0 <>dr
                                               (? (&& (! (two? >dims)) (<dims = 0))   ; a LONE [] -- size from init, or 0 (extern)
                                                   (? (! (peekp rest0 "=")) `(`(nm `('arr t2 0) ()) rest0)   ; no init: incomplete, size 0
                                                      (: ir (pinit ps >rest0)
                                                         (? (! (two? ir)) ()
                                                            (: n (initcount t2 <ir)
                                                               (? (charm? n)
                                                                  `(`(nm `('arr t2 n) <ir) <>ir)
                                                                  ())))))
                                                  ; multi-dim (or a single sized dim): nested arr, optional init.
                                                  ; an outer [] ([][3]) infers its count from the init's top level.
                                                  (: aty0 (mkarr t2 dims)
                                                     (? (peekp rest0 "=")     ; T a[n].. = init
                                                         (: ir (pinit ps >rest0)
                                                            (? (! (two? ir)) ()
                                                               (: aty (? (<>>aty0 = 0) `('arr <>aty0 (initcount <>aty0 <ir)) aty0)
                                                                  `(`(nm aty <ir) <>ir))))
                                                        `(`(nm aty0 ()) rest0)))))))
                                      (peekp ts3 "=")
                                       (: r (pinit ps >ts3)
                                          (? (two? r) `(`(nm t2 <r) <>r) ()))
                                      `(`(nm t2 ()) ts3))
                                  first (one (patt ts2) name (? (two? fpty) fpty rty))
                                  (? (! (two? first)) ()
                                     (: (more ts3 acc)
                                         (? (peekp ts3 ",")
                                            (: dr (pdtor ps >ts3 base)
                                               (? (! (two? dr)) ()
                                                  (: r (one (patt <>dr) (cap (cap dr)) (caup (cap dr)))
                                                     (? (two? r) (more <>r (link <r acc)) ()))))
                                            ; a declarator whose type is a FUNCTION type (a function-type
                                            ; typedef: love.h's `lvm_t lvm_ret0, lvm_cur, ..;`) DECLARES a
                                            ; function -- register its sig (the typedef carries the return),
                                            ; lay NO storage (a gdecl here laid duplicate .data symbols)
                                            ; `;` ends the declaration -- PEEK for it: the remainder may
                                            ; be EMPTY (a global at EOF), which want's () would conflate
                                            ; with the missing-semicolon refusal
                                            (? (! (peekp ts3 ";")) ()
                                               (: rest >ts3
                                                  (fnp? pr) (: t <>pr (? (two? t) (<t = 'fn) ()))
                                                  _ (: (regs l) (? (! (two? l)) ()
                                                                   (: p2 <l
                                                                      _ (? (fnp? p2)
                                                                           (pin (ps 'sigs) <p2
                                                                                `((? (two? (cup (caup p2))) (caup (caup p2)) 'long) () ()))
                                                                           ())
                                                                      ; an ALIAS declared through a function TYPE (musl's
                                                                      ; weak_alias: `extern __typeof(old) new __attribute__
                                                                      ; ((alias("old")))`) answers a nameless 'tdef, so the
                                                                      ; name is recorded HERE, where it is still in hand
                                                                      _ (: al (peep ps 'curalias ())
                                                                           (? (! (two? al)) ()
                                                                              (: _ (pin (ps 'alias) <p2 <al)
                                                                                 (? (caup al) (pin ps 'weaks (link <p2 (ps 'weaks))) ()))))
                                                                      (regs >l)))
                                                       (regs acc))
                                                  vps (: (go2 l a2) (? (! (two? l)) a2
                                                                       (go2 >l (? (fnp? <l) a2 (link <l a2))))
                                                         (go2 acc ()))
                                                  (? (two? vps) `(`('gdecl vps) rest)
                                                     `(`('tdef) rest)))))
                                        (more <>first `(<first))))))))))))))
   ; a parse refusal names where it stood. the first pass ran unstamped and so
   ; left no trail -- so stamp JUST this form and run it through ptop again,
   ; which is cheap (one form) and now marks as it goes. the FURTHEST token
   ; that re-parse reaches is the report site; the form's own first token is
   ; the fallback, for a form that dies before any lookahead marks (and for
   ; the re-parse that unexpectedly SUCCEEDS -- ps carries the first attempt's
   ; half-registered typedefs, so that is possible, and its watermark would be
   ; at the form's end, which would read as a nonsense position).
   ; ONE position and no more: the driver deskews a gripe's line for the
   ; prepended -D lines, and a second line number baked into the message text
   ; would ride out uncorrected.
   (pfail ps ts2 spans toks)
    (: i0 ((tally toks) - (tally ts2))   ; the failing form's index -- an O(n) walk, on the path that is already dying
       ft (? (two? ts2) <ts2 ())
       _ (pin far 'seq -1)
       _ (pin far 'tok ())
       r (ptop ps (stampf ts2 i0 spans))
       dt (? (two? r) () (peep far 'tok ()))
       t (? (two? dt) dt ft)
       (? (! (two? t)) (gripe () () () "parse error at end of input")
          (gripe (? (two? dt) (filof t) (filat spans i0)) <>>t ()
                 ("parse error near " + (spell t)))))
   (cparse ts) (cparse-t ts 'x64)          ; the law tests + any bare caller: 64-bit widths (unchanged behavior)
   (cparse-t ts tgt)
    (: ps (psnew tgt)
       toks (fsbody ts) spans (fspans ts)
       ; note the completed form's staticness/section per declared name, then
       ; clear the stashes for the next form
       (note f)
        (: _ (? (member? <f '(gdecl xdecl))         ; register file-scope names -> types (ptype's global fallback)
                (: (gogl prs) (? (! (two? prs)) 1
                                 (: _ (pin (ps 'globals) (cap (cap prs)) (caup (cap prs))) (gogl >prs)))
                   (gogl <>f))
                ())
           st (peep ps 'curstat ()) sc (peep ps 'cursect ())
           _ (? (&& st (member? <f '(fn proto))) (pin (ps 'sfns) <>f 1) ())
           _ (? (&& (<f = 'gdecl) (|| st sc))
                (: (go2 prs) (? (! (two? prs)) 1
                                (: nm (cap (cap prs))
                                   _ (? st (pin (ps 'svars) nm 1) ())
                                   _ (? sc (pin (ps 'sects) nm sc) ())
                                   (go2 >prs)))
                   (go2 <>f))
                ())
           ; an ALIAS declaration names no storage of its own: it is another
           ; name for a symbol defined in this TU (musl's weak_alias, 189
           ; files). record name -> target; cgen lays the extra label.
           _ (: al (peep ps 'curalias ())
                (? (! (two? al)) ()
                   (: nms (? (<f = 'gdecl) (map cap <>f)
                             (<f = 'proto) `(<>f)
                             (<f = 'fn) `(<>f) ())
                      (go3 l) (? (! (two? l)) 1
                                 (: _ (pin (ps 'alias) <l <al)
                                    _ (? (caup al) (pin ps 'weaks (link <l (ps 'weaks))) ())
                                    (go3 >l)))
                      (go3 nms))))
           _ (: wk (peep ps 'curweak ())
                (? (! wk) ()
                   (: nms (? (member? <f '(gdecl xdecl)) (map cap <>f)
                             (member? <f '(proto fn)) `(<>f) ())
                      (go4 l) (? (! (two? l)) 1
                                 (: _ (pin ps 'weaks (link <l (ps 'weaks))) (go4 >l)))
                      (go4 nms))))
           _ (pin ps 'curstat ())
           _ (pin ps 'curalias ())
           _ (pin ps 'curweak ())
           (pin ps 'cursect ()))
       (tops ts2 acc)
        (? (! (two? ts2)) `(1 `('prog (rev acc)) (ps 'stag) (ps 'sigs) (ps 'weaks)   ; empty acc = a valid EMPTY TU (crypt.c), not a parse failure
                              (ps 'sfns) (ps 'svars) (ps 'sects) (ps 'alias))
           (: r (ptop ps ts2)
              (? (two? r) (: _ (note <r) (tops <>r (link <r acc))) (pfail ps ts2 spans toks))))
       (tops toks ())))
