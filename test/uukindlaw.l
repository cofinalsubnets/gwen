; test/uukindlaw.l -- the LAWS of the kinds-lattice join, proved of the GENERATED
; term (test/uukind.l, lifted from doc/proto/kinds.l by tools/kinds2uu.l). the
; abstract domain is FINITE -- seven codes, bot..top plus the incomparable str --
; so the semilattice laws are DECIDABLE by exhausting the carrier: we evaluate the
; generated `kjoin` in the kernel (vof) over every pairing/tripling of uk-codes and
; check idempotence, commutativity, associativity and the bot/top bounds. a check
; here is thus a COMPLETE proof over the carrier, and drift-gated: a kinds.l edit
; that breaks a law re-emits a kjoin that fails an assert at `make test`. the four
; kinds.l driver cases ride below as uu idpath lemmas -- the bridge to Rocq + Lean.
; guard: the uu kernel (test/uu.l -- vof/defn/kjoin/...) MUST load before these laws. $t front-loads
; it, but a locale-sorted corpus glob puts uukind* first and the laws would run against an unloaded
; kernel (a cryptic ;; missing vof -> BADJJ). Fail loudly and by name instead if that ever recurs.
(assert (member? 'vof (names ())))
(:
   (uk-jj a b) (vof 0 (L 'kjoin a b))                        ; kjoin a b, run in the kernel
   (uk-all f)  (foldl (\ acc x (&& acc (f x))) 1 uk-codes)   ; forall over the carrier
   (uk-all2 f) (uk-all (\ a (uk-all (\ b (f a b)))))
   (uk-all3 f) (uk-all (\ a (uk-all (\ b (uk-all (\ c (f a b c)))))))
   uk-bot (cap uk-codes)                                     ; the floor  (bot, code 0)
   uk-top (cap (cup (rev uk-codes))))                        ; the ceiling (top, below str)

(assert
 ; --- a bounded join-semilattice, exhaustively over uk-codes ---
 (uk-all  (\ a     (id? (uk-jj a a) a)))                              ; idempotent
 (uk-all2 (\ a b   (id? (uk-jj a b) (uk-jj b a))))                    ; commutative
 (uk-all3 (\ a b c (id? (uk-jj (uk-jj a b) c) (uk-jj a (uk-jj b c))))) ; associative
 (uk-all  (\ a (&& (id? (uk-jj uk-bot a) a) (id? (uk-jj a uk-bot) a)))) ; bot the identity
 (uk-all  (\ a (&& (id? (uk-jj uk-top a) uk-top) (id? (uk-jj a uk-top) uk-top)))) ; top absorbs
 ; --- str (the last code) is the incomparable branch: it joins UP to top against
 ;     any numeric, so it is NOT the spine's max (that is the whole point of str) ---
 (id? (vof 0 '(numq 6)) (vof 0 'false))            ; str is not numeric
 (id? (uk-jj 6 1) uk-top))                         ; str `join` charm = top, not str

; --- the bridge: kinds.l's four driver cases as uu idpath lemmas. these are real
;     kernel proof terms (paths over a computing join), so they export axiom-free
;     through tools/uu2coq.l + uu2lean.l to Rocq AND Lean alongside the uu corpus. ---
(assert
 (defq 'kjoin-tier-wins  '(paths nat (kjoin 1 4) 4) '(idpath 4))   ; A. higher tier wins
 (defq 'kjoin-bot-unit   '(paths nat (kjoin 0 3) 3) '(idpath 3))   ; B. bot is the unit
 (defq 'kjoin-top-cap    '(paths nat (kjoin 4 5) 5) '(idpath 5))   ; C. top caps the spine
 (defq 'kjoin-str-widens '(paths nat (kjoin 6 1) 5) '(idpath 5)))  ; D. str widens to top
