; crew/moon/lex.l -- the C lexer (rung 3 stage 0; doc/misc/moon.md is the plan). text ->
; (1 tokens) | () on a lex error. a token is (kind val line): ('kw nom), ('id
; text), ('num charm), ('p text) -- the punctuator by maximal munch. comments
; both kinds; line numbers carried for diagnostics. char literals 'a' (with
; \n \t \r \0 \\ \') read as ('num code); string literals (same escapes,
; plus \") as ('str text). not yet (staged): hex/octal/float literals,
; adjacent-string pasting, the preprocessor's # lines.
(module 'moon
(use 'kore)
(:
   ; a diagnostic carrier: a stage may answer this instead of
   ; a bare bad, naming its own cause -- file/line/col (any () when unknown) and a
   ; message. gripe? guards it; the driver (moon.l ccdie) formats it, else falls
   ; back to today's generic line so unconverted stages don't regress. a gripe is
   ; a chain, so okv? -- a real stage result -- excludes it explicitly. it lives
   ; here, the first moon file in both load orders (crew/build.mk, test/test.mk),
   ; so every later stage can answer one.
   (gripe file line col msg) ['gripe file line col msg]
   (gripe? ('gripe >< _)) 1
   (okv? x) (two? x && !(gripe? x))
   ; clexat: lex with the first line numbered ln0 -- the driver's door. its -D
   ; text prepends to the TU, so those lines get 1-k..0 and the source itself
   ; starts at 1: every stamped line (gripes, __LINE__) is true with no
   ; pay-back pass. clex is the plain 1-based face every other caller reads.
   ; UTF-8 -> code points, the decode half of the wide-literal codec (the encode half,
   ; utf8put, lives in clexat): a prefixed literal's token value is canonical UTF-8,
   ; and parse asks this for the elements. tolerant: an invalid or truncated sequence
   ; decodes as its first byte and moves one, so a re-encoded high escape (L"\xE4")
   ; survives the round trip as the one element C says it is.
   (utf8dec s)
    (: m (tally s)
       (cb? c) (128 <= c < 192)
       (go i acc)
        (? (m <= i) (rev acc)
           (: c (s i)
              (? (194 <= c < 224 && i + 1 < m && cb? (s (i + 1)))
                  (go (i + 2) ((((c - 192) << 6) | (s (i + 1) & 63)) >< acc))
                 (224 <= c < 240 && (i + 2 < m) && cb? (s (i + 1)) && cb? (s (i + 2)))
                  (go (i + 3) ((((c - 224) << 12) | ((s (i + 1) & 63) << 6) | (s (i + 2) & 63)) >< acc))
                 (240 <= c < 245 && (i + 3 < m) && cb? (s (i + 1)) && cb? (s (i + 2)) && cb? (s (i + 3)))
                  (go (i + 4) ((((c - 240) << 18) | ((s (i + 1) & 63) << 12) | ((s (i + 2) & 63) << 6) | (s (i + 3) & 63)) >< acc))
                 (go (i + 1) (c >< acc)))))
       (go 0 ()))
   ; -- a float literal's VALUE, deferred --------------------------------------
   ; the lexer hands back the pieces and this folds them, because the fold is the
   ; expensive half and most literals never need it: every macro body is lexed
   ; where it is defined, so the predefine table and <float.h> used to pay a
   ; correctly-rounded decimal->binary walk per literal in every TU that expanded
   ; none of them. the pieces ride the VALUE slot, not the spelling slot, so the
   ; rebuilds that drop a 5th cell (cpp's relex, parse's stampf) carry them anyway.
   ; a number passes straight through -- cpp mints 'flo tokens with real values.
   (cfipow b p) (: (go k a) (? (k < 1) a (go (k - 1) (* a b))) (go p 1))
   ; never (/ mant (ipow 10 k)) in floats: that rounds twice, and 10^310 is an
   ; infinity before the divide ever runs -- 1e-310 folded to 0, 1e-308 to -inf,
   ; 1e-307 came out a ulp light, every one of them silent. hand the exact digits
   ; to the reader: am_strtod under `sound` is the tree's one correctly-rounded
   ; decimal->binary, the same one love's own literals ride, and it carries the
   ; whole range including the subnormals. the `e` keeps the answer a gem however
   ; integral the digits look.
   (cfdec mant dxp) <(sound (show mant + "e" + show dxp))
   (cflo v)
    (? !(two? v) v
       (id? <v 'dec) (cfdec <>v <>>v)
       ; mant * 2^bexp is exact in decimal either way -- a negative binary exponent
       ; is mant * 5^k over 10^k -- so the same reader takes this lane too, and
       ; 0x1p-1074 stops needing a 2^1074 that no double can hold.
       (id? <v 'hex) (: m <>v b <>>v
                        (? (b < 0) (cfdec (* m (cfipow 5 (0 - b))) b)
                           (cfdec (* m (cfipow 2 b)) 0)))
       v)

   (clex src) (clexat src 1)
   (clexat src ln0)
    (: n (tally src)
       kws '(auto break case char const continue default do double else enum
             extern float for goto if inline int long register restrict return
             short signed sizeof static struct switch typedef union unsigned
             void volatile while _Bool _Complex __int128)
       p3 '("<<=" ">>=" "...")
       p2 '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>" "+=" "-=" "*=" "/=" "%="
            "&=" "|=" "^=" "->" "++" "--" "##")
       ; the multi-char punctuators bucketed by first char. firstat walked all
       ; 21 of them on every punctuator token, each `at?` minting a closure and a
       ; tally; a `(` now looks at none of them and a `<` at three.
       pmap (: t {}
               (add l) (? !(two? l) t
                          (: pat <l  h (pat 0)
                             _ (pin t h (peep t h () + [pat]))
                             (add >l)))
               (add (p3 + p2)))
       kwt (: t {} (go l) (? !(two? l) t (: _ (pin t <l 1) (go >l))) (go kws))
       p1 "()[]{};,.:?~!+-*/%<>=&|^#"
       ; --- the class table: one 256-byte string, a bitmask per char ------
       ; a membership test becomes an index and an `&` where it was a scan
       ; (in1? walked 25 chars, idc? called two predicates -- both per char).
       ; bit 0 is left unused: a string indexed past its end answers the charm
       ; 1, so a char outside 0..255 would match whichever class held bit 0;
       ; with it empty, (& 1 m) is 0 for every mask here. `oct?` rides dig?
       ; rather than taking the eighth bit.
       ;   2 ws  4 dig  8 idh  16 idc  32 hexd  64 suf  128 punct1
       (cbit c) ((c = 32 || c = 9 || c = 13 || c = 12 || c = 11 ? 2 0) +
                 (48 <= c <= 57 ? 4 0) +
                 ; a byte past 127 is an identifier char (a utf-8 `Å` is two of them, C11
                 ; 6.4.2.1's extended set); a stray one outside a literal now interns, not scares
                 (97 <= c <= 122 || 65 <= c <= 90 || c = 95 || 128 <= c ? 8 0) +
                 (48 <= c <= 57 || 97 <= c <= 122 || 65 <= c <= 90 || c = 95 || 128 <= c ? 16 0) +
                 (48 <= c <= 57 || 97 <= c <= 102 || 65 <= c <= 70 ? 32 0) +
                 (c = 117 || c = 85 || c = 108 || c = 76 ? 64 0) +
                 (member? c '(40 41 91 93 123 125 59 44 46 58 63 126 33 43 45 42 47 37 60 62 61 38 124 94 35) ? 128 0))
       ctbl (: (go k acc) (? (k < 0) (string acc) (go (k - 1) (cbit k >< acc))) (go 255 ()))
       (ws? c) (ctbl c & 2)
       (dig? c) (ctbl c & 4)
       (hexd? c) (ctbl c & 32)
       (hexv c) (? (dig? c) (c - 48) (97 <= c <= 102) (c - 87) (c - 55))         ; a=10 A=10
       (suf? c) (ctbl c & 64)
       (skipsuf j) (? (j < n && suf? (src j)) (skipsuf (j + 1)) j)                      ; eat an int-literal suffix run
       ; a wide/prefixed literal marker before ' or " : L (C89 wchar_t), u/U (C11 char16/32),
       ; u8 (C11 UTF-8 string, byte semantics). -> prefix length (1|2), else 0; the
       ; prefixed lane below picks the token kind.
       (wpfx i) (: c (src i)
                   (? !((c = 76) || (c = 117) || (c = 85)) 0
                      (: pl (? (c = 117 && i + 1 < n && src (i + 1) = 56) 2 1)       ; u8 -> 2
                         nc (? ((i + pl) < n) (src (i + pl)) 0)
                         (? ((nc = 39) || (nc = 34)) pl 0))))
       ; does the literal spanning [i, j) carry a u/U? an l/L? (neither is a hex
       ; digit, so the whole-span scan is safe for decimal and hex integers alike)
       (hasu? i j) (: (go k) (? (j <= k) () ((src k = 117) || (src k = 85)) 1 (go (k + 1))) (go i))
       (hasl? i j) (: (go k) (? (j <= k) () ((src k = 108) || (src k = 76)) 1 (go (k + 1))) (go i))
       ; a doubled l/L (the ll/LL suffix): its own kinds -- on the 32-bit thumb targets
       ; `long long` is a distinct 8-byte type and 1ull<<40 must ride it (parse types them).
       (hasll? i j) (: (l? c) ((c = 108) || (c = 76))
                       (go k) (? ((j - 1) <= k) () (l? (src k) && l? (src (k + 1))) 1 (go (k + 1)))
                       (go i))
       ; the token kind of an integer literal: u alone reads unsigned INT (-1u
       ; wraps at 32 bits), u with l/L -- or a value past 32 bits -- unsigned long;
       ; the ll/ll pairs get their own kinds (ullnum/llnum). an unsuffixed one climbs
       ; C11 6.4.4.1's ladder, where hex/octal/binary take the unsigned rungs --
       ; 0xffffffff is a uint, 0xffffffffffffffff a ulong -- and a decimal past
       ; long takes ulong too (gcc's reading; C is out of types there). dec marks
       ; the decimal caller.
       (ukind i j v dec)
        (? !(hasu? i j)
            (? (hasll? i j) 'llnum
               (!dec && !(hasl? i j) && (2147483648 <= v) && (v <= 4294967295)) 'unum
               (9223372036854775808 <= v <= 18446744073709551615) 'ulnum
               (hasl? i j) 'lnum                                 ; 1L is a long, not an int that fits
               'num)
           (hasll? i j) 'ullnum
           (hasl? i j || (4294967296 <= v)) 'ulnum
           'unum)
       ; C11 6.4.3p2: a ucn may not name a basic-set character (short identifier under
       ; 00a0, bar $ @ `), nor a surrogate, nor anything past the last code point. So
       ; a ucn naming 'A' is a constraint violation, not a long way to spell it --
       ; gcc 13 refuses it where newer ones take c23's relaxation.
       (ucnok? v) ?((v = 36) || (v = 64) || (v = 96) ||
                    (160 <= v <= 1114111 && !(55296 <= v <= 57343)))
       (ucn? k) ((k < n) && (src k = 92) && ((k + 1) < n) && ((src (k + 1) = 117) || (src (k + 1) = 85)))
       (escv d) (? (d = 110) 10 (d = 116) 9 (d = 114) 13 (d = 97) 7 (d = 98) 8            ; \n \t \r \a \b
                   (d = 102) 12 (d = 118) 11 (d = 48) 0 (d = 92) 92                       ; \f \v \0 backslash
                   (d = 34) 34 (d = 39) 39 (d = 63) 63 (d = 101) 27 ())                   ; \" \' \? \e
       (oct? c) ((ctbl c & 4) && c <= 55)                                            ; an octal digit 0-7
       (escseq k) (: d (src k)                     ; k = index of the char after the backslash -> (value nextpos) or ()
                     (? (oct? d)                                                          ; \ooo -- 1..3 octal digits
                         (: (oc m v cnt) (? (cnt < 3 && m < n && oct? (src m))
                                            (oc (m + 1) (+ (* v 8) (src m - 48)) (cnt + 1))
                                            [v m])
                            (oc k 0 0))
                        (d = 120)                                                         ; \xhh -- 1+ hex digits
                         (? (((k + 1) < n) && hexd? (src (k + 1)))
                            (: (hx m v) (? (m < n && hexd? (src m))
                                           (hx (m + 1) (+ (* v 16) (hexv (src m))))
                                           [v m])
                               (hx (k + 1) 0))
                            ())
                        ; \uxxxx / \uxxxxxxxx (C99 6.4.3): exactly 4 or 8 hex digits, a short
                        ; run refusing rather than taking what it found. the third element
                        ; marks it a ucn -- it names a code point, so a narrow string owes the
                        ; utf-8 encoding of it where \xhh lays the one byte it named.
                        ((d = 117) || (d = 85))
                         (: w (? (d = 117) 4 8)
                            (uc m v cnt) (? (cnt = w) (? (ucnok? v) [v m 1] ())
                                            (m < n && hexd? (src m))
                                             (uc (m + 1) (+ (* v 16) (hexv (src m))) (cnt + 1))
                                            ())
                            (uc (k + 1) 0 0))
                        (: v (escv d) (? (charm? v) [v (k + 1)] ()))))
       ; the body of a string-shaped literal, from qi (after the opening quote) to the
       ; closing `term` -> (content endpos) | (). one lane under the narrow, wide and
       ; wide-char faces; `wide` re-encodes escape values as UTF-8 (utf8put below).
       ; the body lands a run at a time, never a byte: `say` of a snip is one
       ; memcpy, where an accumulator grown per char is quadratic -- the corpus
       ; literal baked into the kernel is 910 KB and cost 51s of a 64s build.
       (strbody qi term wide)
        (: sink (jug 0)
           (run k) (? (n <= k) k                      ; how far the ordinary bytes reach
                      (: c2 (src k)
                         (? (c2 = term || c2 = 92 || c2 = 10) k (run (k + 1)))))
           (eat j)
            (: e (run j)
               _ (? (j < e) (say sink (snip src j e)) ())
               (? (n <= e) ()
                  (: c2 (src e)
                     (? (c2 = term) [(slurp sink) (e + 1)]
                        (c2 = 92 && e + 1 < n && src (e + 1) = 10) (eat (e + 2))       ; \<nl> splice: the pair vanishes, the string reads on
                        (c2 = 92 && e + 2 < n && src (e + 1) = 13 && src (e + 2) = 10) (eat (e + 3))           ; \<cr><nl> splice
                        (c2 = 10) ()
                        (c2 = 92 && e + 1 < n)
                         (: r (escseq (e + 1))
                            ; a ucn (two? >>r) encodes even in a narrow string: gcc lays
                            ; "ä" as two utf-8 bytes, where "\xe4" is the one byte e4
                            (? (two? r) (: _ (? (wide || two? >>r) (utf8put sink <r) (put sink <r)) (eat <>r)) ()))
                        ; a stop `run` made that none of the above claims: one byte,
                        ; then on. the trailing \ lands here, and so would any future
                        ; stop -- which is what keeps a widened `run` from looping.
                        (: _ (put sink c2) (eat (e + 1)))))))
           (eat qi))
       ; the encode half of the wide-literal codec (utf8dec above is the decode):
       ; an escape value re-encodes, keeping the token's value canonical UTF-8
       (utf8put sink v)
        (? (v < 128) (put sink v)
           (v < 2048) (put sink (192 + (v >> 6)), put sink (128 + (v & 63)))
           (v < 65536) (put sink (224 + (v >> 12)),
                        put sink (128 + (v >> 6 & 63)),
                        put sink (128 + (v & 63)))
           (put sink (240 + (v >> 18)),
            put sink (128 + (v >> 12 & 63)),
            put sink (128 + (v >> 6 & 63)),
            put sink (128 + (v & 63))))
       (idh? c) (ctbl c & 8)
       (idc? c) (ctbl c & 16)
       (at? i pat) (: m (tally pat)                 ; does pat sit at i?
                      (go k) (? (m <= k) 1
                                (n <= (i + k)) ()
                                (src (i + k) = pat k) (go (k + 1))
                                ())
                      (go 0))
       (firstat i ps) (? !(two? ps) ()
                         (at? i <ps) <ps
                         (firstat i >ps))
       (in1? c) (ctbl c & 128)
       ; -- float literals: a run with a '.' or an exponent (12.5, .5, 2., 1e9,
       ; 3.14e-2, a trailing f/F/l/L consumed). digrun scans a decimal run ->
       ; (endj value ndigits); flolit -> (endj isflo? pieces-or-int), the pieces
       ; folded by cflo above only where somebody reads the constant. --
       (ipow b p) (: (go k a) (? (k < 1) a (go (k - 1) (* a b))) (go p 1))
       (digrun j) (: (go k v nd) (? (k < n && dig? (src k))
                                    (go (k + 1) (10 * v + (src k - 48)) (nd + 1)) [k v nd])
                     (go j 0 0))
       (flolit i)
        (: ir (digrun i) j0 <ir iv <>ir
           dot (j0 < n && src j0 = 46)
           fr (? dot (digrun (j0 + 1)) [j0 0 0])
           j1 <fr fv <>fr fd <>>fr
           mant ((* iv (ipow 10 fd)) + fv)
           hase (j1 < n && (src j1 = 101 || src j1 = 69))
           ee (? hase (: k (j1 + 1)
                         neg (k < n && src k = 45)
                         k2 (? (neg || (k < n && src k = 43)) (k + 1) k)
                         er (digrun k2)
                         [<er (? neg -<>er <>er)])
                      [j1 0])
           j2 <ee exp <>ee
           isflo (dot || hase)
           j3 (? (isflo && (j2 < n) && member? (src j2) '(102 70 108 76)) (j2 + 1) j2)
           ; gcc's imaginary suffix (i/I/j/J, riding after or before f/l): 1.0fi
           ; is _Complex_I's spelling (musl complex.h) -- the flag rises to 'imag
           im (isflo && (j3 < n) && member? (src j3) '(105 73 106 74))
           j4 (? !im j3
                 (: k (j3 + 1)
                    (? (k < n && member? (src k) '(102 70 108 76)) (k + 1) k)))
           dxp (exp - fd)
           ; C11 6.4.4.2: an f/F suffix makes the constant a float, not a double kept
           ; at 53 bits. the 4th element reports it; the caller picks the kind.
           fsu (? (isflo && (j2 < n) && member? (src j2) '(102 70)) 1 0)
           val (? isflo ['dec mant dxp] iv)          ; cflo folds it, if anyone asks
           [j4 (? im 'imag isflo) val fsu])
       ; -- hex literals: 0x integer (0xff), or a hex float 0x H.H p [+-] D (a binary
       ; exponent, base 2): 0x1.0p-53, 0x1p4, 0x.8p1. i points at the '0'. -> (endj isflo? value). --
       (hxrun j v nd) (? (j < n && hexd? (src j)) (hxrun (j + 1) (16 * v + hexv (src j)) (nd + 1)) [j v nd])
       ; -- octal literal: a leading 0 then octal digits (0777, 0644). i points at the 0
       ; (it folds to 0). -> (endj value); a plain `0`, `0.5`, `08`, `0x..` never reach here. --
       (octd? c) ((ctbl c & 4) && c <= 55)
       (octlit i) (: (go k v) (? (k < n && octd? (src k)) (go (k + 1) (8 * v + (src k - 48))) [k v]) (go i 0))
       ; 0b/0B binary (gcc's extension, C23's standard spelling) -- one lane beside
       ; the octal one, suffix run and all. a Playdate sprite mask is written this way.
       (binlit i) (: (go k v) (? ((k < n) && ((src k = 48) || (src k = 49)))
                                 (go (k + 1) ((* 2 v) + (src k - 48))) [k v])
                     (go (i + 2) 0))
       (hexlit i)
        (: ir (hxrun (i + 2) 0 0) j0 <ir im <>ir
           dot (j0 < n && src j0 = 46)
           fr (? dot (hxrun (j0 + 1) im 0) [j0 im 0])
           j1 <fr mant <>fr fd <>>fr          ; mant folds the fractional hex digits in; fd counts them
           hasp ((j1 < n) && ((src j1 = 112) || (src j1 = 80)))        ; p / P -- the (required) binary exponent
           (? !(dot || hasp) [(skipsuf j0) () im]          ; a plain hex integer -- eat its suffix
              (: ee (? hasp (: k (j1 + 1)
                               neg (k < n && src k = 45)
                               k2 (? (neg || (k < n && src k = 43)) (k + 1) k)
                               er (digrun k2)
                               [<er (? neg -<>er <>er)])
                            [j1 0])
                 j2 <ee exp <>ee
                 bexp (exp - (* 4 fd))                        ; each fractional hex digit is 2^-4
                 val ['hex mant bexp]                    ; cflo folds it, if anyone asks
                 j3 (? ((j2 < n) && member? (src j2) '(102 70 108 76)) (j2 + 1) j2)
                 [j3 1 val (? ((j2 < n) && member? (src j2) '(102 70)) 1 0)])))
       (go i line acc)
        (? (n <= i) [1 (rev acc)]
           (: c (src i)
              (? (c = 10) (go (i + 1) (line + 1) acc)
                 ((c = 92) && ((i + 1) < n) && (src (i + 1) = 10))         ; \<nl> splice: one logical line
                  (go (i + 2) line acc)                                    ; (line unadvanced so a directive stays grouped)
                 ((c = 92) && ((i + 2) < n) && (src (i + 1) = 13) && (src (i + 2) = 10))          ; \<cr><nl>
                  (go (i + 3) line acc)
                 (ws? c) (go (i + 1) line acc)
                 ; both comment probes sit behind one (= c 47) gate (two at? calls
                 ; per character otherwise)
                 ((c = 47) && at? i "//")
                  (: (eat j) (? ((n <= j) || (src j = 10)) j (eat (j + 1)))
                     (go (eat i) line acc))
                 ((c = 47) && at? i "/*")
                  (: ; a \<nl> inside a comment is a splice (C phase 2 runs before
                     ; comments), so it stays on the logical line -- a #define whose
                     ; body carries a spliced multi-line comment keeps its grouping
                     ; (m4-1.4 regex.c PUSH_FAILURE_POINT)
                     (spliced? j) ((0 < j) && ((src (j - 1) = 92) ||
                                               ((1 < j) && (src (j - 1) = 13) && (src (j - 2) = 92))))
                     (eat j l2) (? (n <= (j + 1)) [() l2]   ; unterminated
                                              (at? j "*/") [(j + 2) l2]
                                              (eat (j + 1) (? ((src j = 10) && !(spliced? j)) (l2 + 1) l2)))
                                r (eat (i + 2) line)
                                (? (charm? <r) (go <r <>r acc) ()))
                 (c = 34) (: r (strbody (i + 1) 34 ())          ; a string literal
                             (? (two? r) (go <>r line (['str <r line () (snip src i <>r)] >< acc)) ()))
                 ; a character constant: chars and escapes up to the closing quote.
                 ; multi-character ones are ordinary C (implementation-defined, gcc
                 ; packs left to right into an int) and they turn up in text the
                 ; program never means to compile -- pdclib's `#error .. 'long' ..`
                 ; sits in a dead #if, and the whole file lexes before cpp runs.
                 (c = 39) (: (mc k v cnt)
                             (? (n <= k) ()
                                (src k = 39) (? (cnt = 0) () [k v cnt])
                                (src k = 92)
                                 (: r (escseq (k + 1))
                                    (? (two? r) (mc <>r (| (v << 8) (<r & 255)) (cnt + 1)) ()))
                                (mc (k + 1) (| (v << 8) (& (src k) 255)) (cnt + 1)))
                             r (mc (i + 1) 0 0)
                             (? !(two? r) ()
                                (: j (+ <r 1)
                                   ; one char keeps the raw byte it always had; a wider one
                                   ; is int-wide and signed, so '\xff\xff\xff\xff' is -1.
                                   v (? (<>>r = 1) <>r
                                        (: w (<>r & 4294967295)
                                           (? (0 < (w & 2147483648)) (w - 4294967296) w)))
                                   (go j line (['num v line () (snip src i j)] >< acc)))))
                 ((c = 48) && ((i + 1) < n) && ((src (i + 1) = 120) || (src (i + 1) = 88)))        ; 0x / 0X hex (int or float)
                  (: r (hexlit i)
                     k (? <>r (? <>>>r 'flof 'flo) (ukind i <r <>>r ()))   ; a u/U in the suffix run marks it unsigned (1U / 1ULL)
                     (go <r line ([k <>>r line () (snip src i <r)] >< acc)))
                 ((c = 48) && ((i + 2) < n) && ((src (i + 1) = 98) || (src (i + 1) = 66)) &&
                  ((src (i + 2) = 48) || (src (i + 2) = 49)))                                            ; 0b / 0B binary
                  (: r (binlit i)
                     j2 (skipsuf <r)
                     k (ukind i j2 <>r ())
                     (go j2 line ([k <>r line () (snip src i j2)] >< acc)))
                 ((c = 48) && ((i + 1) < n) && octd? (src (i + 1)))       ; 0nnn octal integer (0644, 0777)
                  (: r (octlit i)
                     j2 (skipsuf <r)
                     k (ukind i j2 <>r ())
                     (go j2 line ([k <>r line () (snip src i j2)] >< acc)))
                 (dig? c || ((c = 46) && ((i + 1) < n) && dig? (src (i + 1))))
                  (: r (flolit i)                        ; a digit run, or .digit -- integer or float
                     j2 (? <>r <r (skipsuf <r))    ; an integer eats its u/U/l/L suffix
                     k (? (id? <>r 'imag) 'iflo    ; the imaginary float (1.0fi): its own kind
                          <>r (? <>>>r 'flof 'flo) (ukind i j2 <>>r 1))
                     (go j2 line ([k <>>r line () (snip src i j2)] >< acc)))
                 ; a prefixed literal: the kind carries the width (L -> 'wstr, u -> 'str16,
                 ; U -> 'str32; u8 stays narrow), the value is canonical UTF-8, the
                 ; spelling keeps the prefix. a wide char constant decodes to its last
                 ; code point (L'ä' is 0xE4, not the packed bytes; L'ab' is 'b' --
                 ; implementation-defined, gcc's reading) and stays 'num.
                 (< 0 (wpfx i))
                  (: pl (wpfx i)
                     c2 (src (i + pl))
                     k (? ((c = 117) && (pl = 1)) 'str16 (c = 85) 'str32 (c = 76) 'wstr 'str)
                     (? (c2 = 34)
                         (: r (strbody ((i + pl) + 1) 34 !(k = 'str))
                            (? (two? r) (go <>r line ([k <r line () (snip src i <>r)] >< acc)) ()))
                        (k = 'str) (go (i + pl) line acc)     ; u8'': lex narrow, byte semantics
                        (: r (strbody ((i + pl) + 1) 39 1)
                           cps (? (two? r) (utf8dec <r) ())
                           (lastc l) (? (two? >l) (lastc >l) <l)
                           (? (two? cps) (go <>r line (['num (lastc cps) line () (snip src i <>r)] >< acc)) ()))))
                 ; an identifier may be spelled with ucns (C99 6.4.2.1). a ucn names a code
                 ; point, so `Å` must intern as the utf-8 `Å` does -- one name, built by
                 ; the escape lane where the plain run is a snip. a malformed one lex-errors.
                 ((idh? c) || (ucn? i))
                  (: (idr j) (? ((j < n) && idc? (src j)) (idr (j + 1))
                                (ucn? j) (: r (escseq (j + 1)) (? (two? r) (idr <>r) j))
                                j)
                     j0 (idr i)
                     (esc? k) (? (j0 <= k) () (src k = 92) 1 (esc? (k + 1)))
                     (? (j0 <= i) ()
                        (: t (? (esc? i)
                                (: sink (jug 0)
                                   (bld k) (? (j0 <= k) (slurp sink)
                                              (ucn? k) (: r (escseq (k + 1))
                                                          _ (utf8put sink <r)
                                                          (bld <>r))
                                              (: _ (put sink (src k)) (bld (k + 1))))
                                   (bld i))
                                (snip src i j0))
                           s (intern t)
                           (go j0 line ((? (peep kwt s ()) ['kw s line] ['id t line]) >< acc)))))
                 (: hit (firstat i (peep pmap c ()))
                    (? hit (go (i + tally hit) line (['p hit line] >< acc))
                       (in1? c)                        ; a '(' GLUED to a preceding id (no space) carries a 4th field: the function-macro paren
                        (: gl ((c = 40) && (0 < i) && idc? (src (i - 1)))
                           (go (i + 1) line ((? gl ['p (string c) line 1] ['p (string c) line]) >< acc)))
                       ())))))
       (go 0 ln0 ())))
) ; module 'moon
