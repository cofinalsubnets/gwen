; crew/moon/floor.l -- THE C TYPE FLOOR: the laws that are neither syntax nor codegen,
; spelled once and read from both sides. pure over the shared type vocabulary
; ('char 'short 'int 'long .. | ('ptr t) | ('arr t n) | ('struct tag) | ('bitfield ty off w)):
; the machine WORD and the struct-tag table arrive as PARAMETERS, which is the whole point --
; parse threads them off ps, gen off g, and a front that is NEITHER stands on this file alone.
; ⚠ nothing here may name a token, a parse state, a register or a target: the moment it does,
; a second front has to drag cparse in to size a struct.
(:
   ; -- the type shapes --
   (ptr? ('ptr)) 1
   (arr? ('arr)) 1
   (fn? ('fn)) 1                     ; ('fn ret?) -- a function; decays to its address
   (stru? ('struct)) 1
   (bitf? ('bitfield)) 1             ; (bitfield uty bitoff width); its lvalue is a UNIT address
   (cplxty? ty) (stru? ty && member? <>ty '("__cplx_double" "__cplx_float"))   ; a seeded complex

   ; -- sizes and alignment. w is the machine word in BYTES (4 on the 32-bit thumb ABI, 8
   ; otherwise), stag the tag table: tag -> (fields size align).
   ; ⚠ ONE body on purpose: parse BAKES member offsets into stag at playout and gen TRUSTS
   ; them, so a divergence mislays every pointer-bearing struct SILENTLY.
   ; ⚠ the widths spell their members out rather than reaching for gen's `llt?`/`d128?` -- those
   ; two name a register pair and a protocol, which is gen's business and no law of C. that is
   ; the admission rule here: a name earns the floor by being about C, not about a machine.
   (tysz w stag ty)
    (? (ptr? ty) w
       (arr? ty) (* <>>ty (tysz w stag <>ty))
       (stru? ty) <>(peep stag <>ty 0)
       (bitf? ty) (tysz w stag <>ty)       ; the unit's size (a bitfield rides its declared type)
       ((ty = 'char) || (ty = 'uchar) || (ty = 'bool)) 1
       ((ty = 'short) || (ty = 'ushort)) 2
       ((ty = 'int) || (ty = 'uint) || (ty = 'float)) 4
       ((ty = 'llong) || (ty = 'ullong) || (ty = 'double)) 8   ; 8-byte on EVERY target (t32's word is 4)
       ((ty = 'i128) || (ty = 'u128)) 16
       w)
   (tyalign w stag ty)
    (? (ptr? ty) w
       (arr? ty) (tyalign w stag <>ty)
       (stru? ty) <>>(peep stag <>ty 0)
       (bitf? ty) (tyalign w stag <>ty)
       (tysz w stag ty))
   (aup n a) (& (n + (a - 1)) (0 - a))     ; round n up to alignment a

   ; -- the typing door: C's promotion and conversion law spelled ONCE -- integer promotions
   ; (C11 6.3.1.1) and the usual arithmetic conversions (6.3.1.8). parse's arithmetic lanes
   ; read it, gen's cmpu (compare/divide signedness) CONSUMES it. w64 is the one seam: whether
   ; long is 8 bytes -- on ILP32 a long cannot hold every uint, so long meets uint at ULONG
   ; where LP64 answers long.
   (parith? ty) (? (bitf? ty) 1
                   (member? ty '(bool char uchar short ushort int uint long ulong
                                 llong ullong i128 u128 float double)))
   (trank ty) (? (member? ty '(i128 u128)) 4
                 ((ty = 'llong) || (ty = 'ullong)) 3
                 ((ty = 'long) || (ty = 'ulong)) 2 1)
   (tusg? ty) (member? ty '(uchar ushort uint ulong ullong u128))
   (tusg ty) (? (ty = 'int) 'uint (ty = 'long) 'ulong
                (ty = 'llong) 'ullong (ty = 'i128) 'u128 ty)
   (tw w64 ty) (: r (trank ty) (? (r = 1) 4 (r = 2) (? w64 8 4) (r = 3) 8 16))
   (pprom ty) (? (bitf? ty) (pprom <>ty)              ; a bitfield promotes as its unit
                 (member? ty '(bool char uchar short ushort)) 'int
                 ty)
   (puac w64 ta tb)
    (? ((ta = 'double) || (tb = 'double)) 'double
       ((ta = 'float) || (tb = 'float)) 'float
       (: a (pprom ta) b (pprom tb)
          (? (a = b) a
             (: ua (tusg? a) ub (tusg? b)
                (? ((ua && ub) || (!ua && !ub)) (? (trank a < trank b) b a)   ; same signedness: higher rank
                   (: u (? ua a b) s (? ua b a)     ; one of each: the unsigned one, the signed one
                      (? (trank s <= trank u) u     ; unsigned rank >= signed: unsigned wins
                         (tw w64 u < tw w64 s) s    ; signed strictly wider: it represents all of u
                         (tusg s))))))))            ; same width: the signed one's unsigned twin
   (tdecay ty) (? (arr? ty) ['ptr <>ty] ty))
